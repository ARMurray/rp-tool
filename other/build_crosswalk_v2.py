#!/usr/bin/env python3
"""
build_crosswalk_v2.py

Adds five explicit-semantics columns to the PR RP Tool crosswalk:

    rp_method        direct | projected
                     'direct'    = compare observed statistic to criterion, no MF,
                                   no dilution ratio (pH / temperature precedent)
                     'projected' = TSD 3.3.2 multiplying-factor projection

    rp_operator      > | <
                     VIOLATION-direction. The operator IS the RP test:
                         RP == TRUE  iff  observed <rp_operator> CRITERION_VALUE
                     '>' = ceiling  (RP if observed exceeds criterion)
                     '<' = floor    (RP if observed falls below criterion)
                     Strict inequality: a value exactly equal to the criterion
                     is COMPLIANT, per PRWQS wording ("shall not exceed",
                     "shall not contain less than", "outside the range of").

    statistic        DAILY_MAX | DAILY_MIN | GEOMEAN | P90 | AVG
                     Semantic token. Maps to ICIS
                     (statistical_base_type_code, statistical_base_short_desc)
                     pairs via STAT_LOOKUP (emitted as a separate file).
                     Drives BOTH which DMR records are extracted and which
                     value is compared.

    criterion_form   numeric | formula | narrative | none
                     'formula'   = computed at runtime (TAN, hardness metals)
                     'narrative' = non-numeric WQS ("shall not be altered")
                     'none'      = no WQS criterion exists -> Needs Attention

    criterion_label  Display override. Blank = fall back to NPDES_Pollutant.
                     Exists so display names never touch NPDES_Pollutant,
                     which is the join key to NPDES_Forms_Pollutants_1.csv.

The legacy `Method` column is RETAINED unchanged so the current app.R keeps
working during migration. It can be dropped once app.R reads the new columns.

Input : crosswalk.csv   (545 rows)
Output: crosswalk_v2.csv (545 rows, +5 columns)
        statistic_lookup.csv
"""

import csv
import sys
from pathlib import Path

SRC = Path(sys.argv[1] if len(sys.argv) > 1 else "/mnt/project/crosswalk.csv")
OUT_DIR = Path(sys.argv[2] if len(sys.argv) > 2 else "/mnt/user-data/outputs")
OUT_DIR.mkdir(parents=True, exist_ok=True)

NEW_COLS = ["rp_method", "rp_operator", "statistic", "criterion_form", "criterion_label"]

# --------------------------------------------------------------------------
# Semantic statistic token -> acceptable ICIS (type_code, short_desc) pairs.
# Kept OUT of the crosswalk so new short_desc spellings are a one-line edit
# here rather than a 545-row migration. short_desc matched case-insensitively
# after stripping whitespace.
# --------------------------------------------------------------------------
STAT_LOOKUP = [
    ("DAILY_MAX", "MAX", "DAILY MX"),
    ("DAILY_MAX", "MAX", "MAXIMUM"),
    ("DAILY_MAX", "MAX", "MO MAX"),
    ("DAILY_MAX", "MAX", "48HR MX"),
    ("DAILY_MIN", "MIN", "INST MIN"),
    ("DAILY_MIN", "MIN", "MINIMUM"),
    ("GEOMEAN",   "AVG", "GEO MEAN"),
    ("GEOMEAN",   "AVG", "MO GEO"),
    ("GEOMEAN",   "AVG", "30DA GEO"),
    ("P90",       "MAX", "90TH %"),
    ("AVG",       "AVG", "MO AVG"),
    ("AVG",       "AVG", "WKLY AVG"),
    ("AVG",       "AVG", "DAILY AV"),
]

# --------------------------------------------------------------------------
# Per-parameter overrides. Anything not listed takes the Method-derived
# default below. Keyed by parameter_code; value is either a flat dict
# (applies to every row for that code) or a dict of
# CRITERION_VALUE -> dict (to split rows that share a parameter_code).
# --------------------------------------------------------------------------
OVERRIDES = {
    # ---- Dissolved oxygen: floor, observed minimum, no MF -----------------
    # PRWQS SB & SD: "shall not contain less than 5(.0) mg/L".
    # Was Method='Concentration' -> indistinguishable from a ceiling, and
    # its MIN records were being discarded by the MAX-only fetch filter.
    "00300": {
        "rp_method": "direct",
        "rp_operator": "<",
        "statistic": "DAILY_MIN",
        "criterion_form": "numeric",
        "criterion_label": "Dissolved Oxygen",
    },

    # ---- Enterococci: two independent criteria, two data series ----------
    # PRWQS SB & SD: geometric mean <= 35/100mL over any 90-day interval,
    # AND 90th percentile <= 130/100mL over the same interval.
    # Both projected (RWC calculated), split by statistic.
    "61211": {
        "35": {
            "rp_method": "projected",
            "rp_operator": ">",
            "statistic": "GEOMEAN",
            "criterion_form": "numeric",
            "criterion_label": "Enterococci (geometric mean)",
        },
        "130": {
            "rp_method": "projected",
            "rp_operator": ">",
            "statistic": "P90",
            "criterion_form": "numeric",
            "criterion_label": "Enterococci (90th percentile)",
        },
    },

    # ---- pH: existing direct treatment, now declared rather than hardcoded
    # NPDES_Pollutant deliberately NOT changed -- it is the forms join key
    # and NPDES_Forms_Pollutants_1.csv was not available to verify against.
    "00400": {
        "MAXROW": {
            "rp_method": "direct",
            "rp_operator": ">",
            "statistic": "DAILY_MAX",
            "criterion_form": "numeric",
            "criterion_label": "pH (maximum)",
        },
        "MINROW": {
            "rp_method": "direct",
            "rp_operator": "<",
            "statistic": "DAILY_MIN",
            "criterion_form": "numeric",
            "criterion_label": "pH (minimum)",
        },
    },

    # ---- Display-name fixes for rows with no NPDES_Pollutant -------------
    "00080": {"criterion_label": "Color"},
    "00070": {"criterion_label": "Turbidity"},
    "39516": {"criterion_label": "Polychlorinated Biphenyls (PCBs)"},
    "00010": {"criterion_label": "Temperature"},

    # ---- Phosphorus naming (display-only; join key untouched) ------------
    "00665": {"criterion_label": "Total Phosphorus"},

    # ---- Total ammonia nitrogen -----------------------------------------
    "00610": {"criterion_label": "Total Ammonia Nitrogen"},
}


def default_from_method(method, use_class):
    """Legacy Method column -> new column defaults."""
    if method == "Max":
        return dict(rp_method="direct", rp_operator=">", statistic="DAILY_MAX",
                    criterion_form="numeric")
    if method == "Min":
        return dict(rp_method="direct", rp_operator="<", statistic="DAILY_MIN",
                    criterion_form="numeric")
    if method == "Special":
        # TAN + hardness-dependent metals: criterion computed at runtime.
        return dict(rp_method="projected", rp_operator=">", statistic="DAILY_MAX",
                    criterion_form="formula")
    if method == "NA" or use_class == "NA":
        # NPDES-side rows with no matching WQS criterion (BOD5, PCB congeners,
        # etc). No criterion to compare against -> route to Needs Attention.
        return dict(rp_method="", rp_operator="", statistic="",
                    criterion_form="none")
    # Method == 'Concentration' -- the 460-row default.
    return dict(rp_method="projected", rp_operator=">", statistic="DAILY_MAX",
                criterion_form="numeric")


def main():
    with SRC.open(newline="", encoding="utf-8-sig") as fh:
        rows = list(csv.DictReader(fh))
    fieldnames = list(rows[0].keys()) + NEW_COLS

    changes = []
    for r in rows:
        code = r["parameter_code"].strip()
        method = r["Method"].strip()
        use_class = r["USE_CLASS_NAME_LOCATION_ETC"].strip()
        crit = r["CRITERION_VALUE"].strip()

        vals = default_from_method(method, use_class)
        vals["criterion_label"] = ""

        ov = OVERRIDES.get(code)
        if ov:
            if code == "00400":
                key = "MAXROW" if method == "Max" else "MINROW"
                vals.update(ov[key])
            elif code == "61211":
                if crit in ov:
                    vals.update(ov[crit])
            else:
                vals.update(ov)

        before = (r.get("Method"), "")
        r.update(vals)

        # Record semantically meaningful departures from the legacy Method
        if code in ("00300", "61211"):
            changes.append((code, r["POLLUTANT_NAME"], use_class, crit,
                            method, vals["rp_method"], vals["rp_operator"],
                            vals["statistic"]))

    out = OUT_DIR / "crosswalk_v2.csv"
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(rows)

    lut = OUT_DIR / "statistic_lookup.csv"
    with lut.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["statistic", "statistical_base_type_code",
                    "statistical_base_short_desc"])
        w.writerows(STAT_LOOKUP)

    # ---- verification summary ----
    import collections
    print(f"rows in : {len(rows)}")
    print(f"rows out: {sum(1 for _ in csv.DictReader(out.open(newline='')))}")
    for c in NEW_COLS[:-1]:
        print(f"\n{c}: {dict(collections.Counter(r[c] for r in rows))}")
    n_lab = sum(1 for r in rows if r['criterion_label'])
    print(f"\ncriterion_label populated: {n_lab} rows")
    print("\nBehaviour changes vs legacy Method:")
    for ch in changes:
        print("   ", ch)


if __name__ == "__main__":
    main()

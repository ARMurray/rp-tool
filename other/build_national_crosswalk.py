#!/usr/bin/env python3
"""
build_national_crosswalk.py

Builds the national WQS -> ICIS-NPDES parameter-code crosswalk from:

    other/national_wqs.csv      EPA criteria database (all states/territories/tribes)
    other/PR_joined.csv         Puerto Rico criteria already matched to ICIS codes
    other/wqs_matched.csv       earlier name-matching pass (EXACT_PR / NORM_PR / FUZZY_*)
    other/REF_Parameter.csv     ICIS-NPDES parameter reference list

Run from the repository root:

    python other/build_national_crosswalk.py

Outputs (other/national_crosswalk/):

    national_name_matches.csv      One row per (pollutant name, ICIS code). THIS is the table a
                                   resolver tool edits. Unresolved names have one row with a
                                   blank parameter_code.
    national_name_suggestions.csv  Candidate codes for unresolved names. Suggestions only; nothing
                                   here is accepted.
    national_crosswalk.csv         One row per (criterion, ICIS code), built by joining the
                                   name table back onto every criterion. Unresolved criteria
                                   keep one row with a blank parameter_code.
    build_summary.txt              Counts by match type and rule.

Match tiers, in order of trust:

    PR_CROSSWALK     Puerto Rico criteria, using the codes already assigned (all codes kept).
    EXACT_PR         Name identical to a Puerto Rico pollutant name; takes all of that name's PR codes.
    NORM_PR          Name matched to a PR name after normalisation in the earlier pass, except
                     the entries in NORM_PR_DEMOTED, which changed the chemistry.
    NORM_VALENCE     Name equals a PR name once a valence in parentheses is kept as part of the
                     name (so "chromium (vi)" matches "chromium vi", not "chromium").
    CAS_PR           CAS number appears in the PR criteria and maps to exactly one ICIS code.
    ICIS_DESC_EXACT  Name (or an alias in its parentheses) equals exactly one active ICIS
                     parameter description. Skipped when the CAS maps to a different or to
                     several PR codes.

Everything else is left blank. Fuzzy matches from the earlier pass are NOT accepted; they are
listed in the suggestions file.
"""

import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

import pandas as pd
from rapidfuzz import fuzz, process

ROOT = Path(".")
OUT = ROOT / "other" / "national_crosswalk"
OUT.mkdir(parents=True, exist_ok=True)

STATES = set(
    "AL AK AZ AR CA CO CT DE DC FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN MS MO MT NE NV NH "
    "NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WI WY".split()
)
TERRITORIES = set("PR VI GU AS MP".split())

# Matches from the earlier pass that normalisation got wrong (a species, valence or isomer was
# dropped, or a different compound was matched). They go back to unresolved.
NORM_PR_DEMOTED = {
    "chromium (iii)": "valence dropped; PR matched total chromium (re-matched by NORM_VALENCE)",
    "chromium (vi)": "valence dropped; PR matched total chromium (re-matched by NORM_VALENCE)",
    "arsenic (iii)": "valence dropped; PR matched total arsenic",
    "arsenic (v)": "valence dropped; PR matched total arsenic",
    "selenium (v)": "valence dropped; PR matched total selenium",
    "mercury (ii)": "valence dropped; PR matched total mercury",
    "benzo(e) pyrene": "different isomer; matched to benzo[a]pyrene",
    "benzo(j) fluoranthene": "different isomer; matched to benzo[k]fluoranthene",
    "bis(2-chloroisopropyl) ether": "different compound; matched to bis[chloromethyl] ether",
}

# Names the CAS_PR and ICIS_DESC_EXACT rules would have matched but that a review of the output
# showed to be wrong or not a single compound. They stay unresolved; the code the rule would
# have given is listed in the suggestions file as RULE_DEMOTED.
RULE_DEMOTED = {
    "p,p'-dichlorodiphenyldichloroethane": "source CAS 72559 is DDE; the name is DDD (CAS swapped in source)",
    "p,p'-dichlorodiphenyldichloroethylene": "source CAS 72548 is DDD; the name is DDE (CAS swapped in source)",
    "dichlorobenzenes": "group of isomers; CAS is for the 1,3 isomer only",
    "dichloropropenes": "group of isomers; CAS is for 1,3-dichloropropene only",
    "dichloropropanes": "group of isomers; CAS is for 1,2-dichloropropane only",
    "dinitrotoluenes": "group of isomers; CAS is for 2,4-dinitrotoluene only",
    "total trihalomethanes": "sum of four compounds; CAS is chloroform only",
    "trihalomethanes (total)": "sum of four compounds; CAS is chloroform only",
    "trihalomethanes (t)": "sum of four compounds; CAS is chloroform only",
    "ddt and metabolites": "sum of DDT, DDD and DDE; CAS is 4,4'-DDT only",
    "4,4'-ddt and derivatives": "sum of DDT, DDD and DDE; CAS is 4,4'-DDT only",
    "chlorine produced oxidant": "saltwater oxidant measure; not identical to total residual chlorine",
    "sodium": "ICIS description 'Sodium, %' is percent sodium, not concentration",
    "4,4'methylene-bis-(2-chloroaniline)": "alias in parentheses matched 2-chloroaniline, a different compound",
    "trichlorobenzene ( 1,3,5-)": "isomer is specified; ICIS 'Trichlorobenzene' is unspecified",
    "chlorinated benzenes (except di)": "qualified group; ICIS 'Chlorinated benzenes' has no exclusion",
}

# Words inside parentheses that qualify the measurement. Dropping them changes what is measured,
# so the parenthetical is never stripped to form an alias.
QUALIFIER_WORDS = {
    "total", "dissolved", "free", "ionic", "ionized", "unionized", "un-ionized", "inorganic",
    "organic", "filtered", "recoverable", "soluble", "acid", "as", "expressed", "fraction",
    "undissociated", "dissociated", "elemental", "particulate",
}
VALENCE_RE = re.compile(r"^(?:[ivx]+|[+-]?\d+[+-]?)$", re.I)


def key(name):
    return re.sub(r"\s+", " ", str(name).strip().lower())


def pad_code(c):
    if pd.isna(c) or str(c).strip() == "":
        return ""
    s = str(c).strip()
    return s.zfill(5) if s.isdigit() else s


def tokens(s):
    s = str(s).lower()
    s = re.sub(r"\.(alpha|beta|gamma|delta)\.", r"\1", s)
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()


def valence_name(name):
    """Lower-case name with parentheses removed ONLY around valence tokens, e.g.
    'chromium (vi)' -> 'chromium vi'; other parenthetical text is dropped."""
    s = key(name)

    def repl(m):
        inner = m.group(1).strip()
        return " " + inner + " " if VALENCE_RE.match(inner) else " "

    s = re.sub(r"\(([^)]*)\)", repl, s)
    return re.sub(r"\s+", " ", s).strip()


def aliases(name):
    """Candidate names to test against ICIS descriptions: the full name, the name with a
    non-qualifier parenthetical removed, and each comma-separated item in that parenthetical."""
    out = [name]
    m = re.search(r"\(([^)]*)\)", name)
    if m:
        inner = m.group(1).strip()
        words = set(re.findall(r"[a-z\-]+", inner.lower()))
        if not (VALENCE_RE.match(inner) or words & QUALIFIER_WORDS):
            base = re.sub(r"\([^)]*\)", " ", name)
            out.append(base)
            out.extend(p.strip() for p in inner.split(",") if len(p.strip()) >= 4)
    return out


def main():
    nat = pd.read_csv(ROOT / "other/national_wqs.csv", dtype=str, encoding="utf-8-sig", low_memory=False)
    nat["nkey"] = nat.POLLUTANT_NAME.map(key)
    prj = pd.read_csv(ROOT / "other/PR_joined.csv", dtype=str)
    prj = prj.dropna(subset=["parameter_code"])
    prj["parameter_code"] = prj.parameter_code.map(pad_code)
    old = pd.read_csv(ROOT / "other/wqs_matched.csv", dtype=str, low_memory=False)
    old["parameter_code"] = old.parameter_code.map(pad_code)
    old["nkey"] = old.POLLUTANT_NAME.map(key)
    ref = pd.read_csv(ROOT / "other/REF_Parameter.csv", dtype=str, low_memory=False)
    ref["parameter_code"] = ref.PARAMETER_CODE.map(pad_code)
    icis_desc = dict(zip(ref.parameter_code, ref.PARAMETER_DESC))
    active = ref[ref.STATUS_FLAG == "A"].copy()
    active["tok"] = active.PARAMETER_DESC.map(tokens)
    by_tok = defaultdict(list)
    for code, tok in zip(active.parameter_code, active.tok):
        by_tok[tok].append(code)

    # ---- Puerto Rico: criterion-level codes and name -> codes map ---------------------------
    pr_nat = nat[nat.ENTITY_ABBR == "PR"][["CRITERION_ID", "nkey", "CAS_NO"]]
    pr_codes = prj[["CRITERION_ID", "parameter_code"]].merge(pr_nat, on="CRITERION_ID", how="inner")
    name_to_pr = pr_codes.groupby("nkey").parameter_code.agg(lambda s: sorted(set(s))).to_dict()
    valname_to_pr = defaultdict(set)
    for nk, codes in name_to_pr.items():
        valname_to_pr[valence_name(nk)].update(codes)
    cas_to_pr = (
        pr_codes.dropna(subset=["CAS_NO"]).groupby("CAS_NO").parameter_code.agg(lambda s: sorted(set(s))).to_dict()
    )

    # ---- name-level table -------------------------------------------------------------------
    names = (
        nat[nat.ENTITY_ABBR != "PR"]
        .groupby("nkey")
        .agg(
            POLLUTANT_NAME=("POLLUTANT_NAME", lambda s: Counter(s).most_common(1)[0][0]),
            CAS_NO=("CAS_NO", "first"),
            n_criteria=("CRITERION_ID", "size"),
            n_entities=("ENTITY_ABBR", "nunique"),
        )
        .reset_index()
    )
    old_by_name = old.drop_duplicates("nkey").set_index("nkey")

    rows, sugg = [], []

    def add(nk, codes, mtype, rule, note=""):
        for c in codes:
            rows.append((nk, c, icis_desc.get(c, ""), mtype, rule, note))

    for r in names.itertuples(index=False):
        nk, cas = r.nkey, r.CAS_NO if isinstance(r.CAS_NO, str) else None
        prev = old_by_name.loc[nk] if nk in old_by_name.index else None
        prev_type = prev.match_type if prev is not None else None
        pr_cas_codes = cas_to_pr.get(cas, []) if cas else []
        note = ""

        # 1. EXACT_PR: identical to a PR pollutant name (all PR codes)
        if nk in name_to_pr:
            add(nk, name_to_pr[nk], "EXACT_PR", "name identical to PR pollutant")
            continue

        # 2. NORM_PR from the earlier pass, minus demoted entries
        if prev_type == "NORM_PR":
            if nk in NORM_PR_DEMOTED:
                note = "demoted NORM_PR: " + NORM_PR_DEMOTED[nk]
                sugg.append((nk, "PRIOR_NORM_DEMOTED", prev.parameter_code, icis_desc.get(prev.parameter_code, ""), ""))
            else:
                add(nk, [prev.parameter_code], "NORM_PR", "normalised name matched to PR name (earlier pass)")
                continue

        # 3. NORM_VALENCE: valence kept as part of the name
        vn = valence_name(nk)
        if vn != nk and vn in valname_to_pr and re.search(r"\([^)]*\)", nk) and VALENCE_RE.match(
            re.search(r"\(([^)]*)\)", nk).group(1).strip()
        ):
            add(nk, sorted(valname_to_pr[vn]), "NORM_VALENCE", "valence kept: " + vn)
            continue

        # 4/5. CAS_PR and ICIS_DESC_EXACT
        icis_hits = []
        for a in aliases(r.POLLUTANT_NAME):
            hits = by_tok.get(tokens(a), [])
            if len(hits) == 1:
                icis_hits.append(hits[0])
        icis_hits = sorted(set(icis_hits))

        if nk in RULE_DEMOTED:
            note = "held after review: " + RULE_DEMOTED[nk]
            for c in pr_cas_codes or icis_hits:
                sugg.append((nk, "RULE_DEMOTED", c, icis_desc.get(c, ""), ""))
        elif len(pr_cas_codes) == 1:
            code = pr_cas_codes[0]
            if icis_hits and icis_hits != [code]:
                note = "CAS maps to PR code %s but ICIS description matches %s" % (code, ",".join(icis_hits))
            else:
                rule = "CAS in PR criteria maps to one code" + (" and ICIS description agrees" if icis_hits else "")
                add(nk, [code], "CAS_PR", rule)
                continue
        elif len(pr_cas_codes) > 1:
            note = "CAS maps to several PR codes: " + ",".join(pr_cas_codes)
        elif len(icis_hits) == 1:
            add(nk, icis_hits, "ICIS_DESC_EXACT", "name or alias equals one active ICIS description")
            continue
        elif len(icis_hits) > 1:
            note = "aliases match different ICIS descriptions: " + ",".join(icis_hits)

        # ---- unresolved: blank row + suggestions ------------------------------------------
        rows.append((nk, "", "", "UNRESOLVED", "", note))
        for c in pr_cas_codes:
            sugg.append((nk, "CAS_PR", c, icis_desc.get(c, ""), ""))
        if prev is not None and isinstance(prev.parameter_code, str) and prev.match_type.startswith("FUZZY"):
            sugg.append((nk, "PRIOR_" + prev.match_type, prev.parameter_code, icis_desc.get(prev.parameter_code, ""), prev.match_score))
        q = tokens(r.POLLUTANT_NAME)
        for desc_tok, score, idx in process.extract(q, active.tok.tolist(), scorer=fuzz.token_sort_ratio, limit=5):
            sugg.append((nk, "NAME_SIMILARITY", active.parameter_code.iloc[idx], active.PARAMETER_DESC.iloc[idx], round(score / 100, 3)))

    nm = pd.DataFrame(rows, columns=["name_key", "parameter_code", "parameter_desc", "match_type", "match_rule", "note"])
    nm = nm.merge(names[["nkey", "POLLUTANT_NAME", "CAS_NO", "n_criteria", "n_entities"]], left_on="name_key", right_on="nkey").drop(columns="nkey")
    nm["match_status"] = nm.match_type.map(lambda t: "UNRESOLVED" if t == "UNRESOLVED" else "MATCHED")
    nm["icis_status"] = nm.parameter_code.map(dict(zip(ref.parameter_code, ref.STATUS_FLAG))).fillna("")
    nm = nm[["name_key", "POLLUTANT_NAME", "CAS_NO", "n_criteria", "n_entities", "parameter_code", "parameter_desc",
             "icis_status", "match_status", "match_type", "match_rule", "note"]].sort_values(["match_status", "n_criteria"], ascending=[False, False])
    nm.to_csv(OUT / "national_name_matches.csv", index=False, encoding="utf-8")

    sg = pd.DataFrame(sugg, columns=["name_key", "source", "parameter_code", "parameter_desc", "score"])
    sg = sg.drop_duplicates(["name_key", "source", "parameter_code"])
    sg.to_csv(OUT / "national_name_suggestions.csv", index=False, encoding="utf-8")

    # ---- criterion-level file ---------------------------------------------------------------
    # PR criteria use the codes already assigned; everything else joins on the name table.
    keep = [
        "ENTITY_ABBR", "ENTITY_NAME", "REGION", "CRITERION_ID", "POLLUTANT_NAME", "CAS_NO",
        "CRITERION_VALUE", "UNIT_NAME", "CRITERIATYPEAQUAHUMHLTH", "CRITERIATYPEFRESHSALTWATER",
        "CRITERIATYPE_ACUTECHRONIC", "CRITERIATYPE_WATERORG", "USE_CLASS_NAME_LOCATION_ETC_ID",
        "USE_CLASS_NAME_LOCATION_ETC", "EFFECTIVE_DATE",
    ]
    nat["entity_type"] = nat.ENTITY_ABBR.map(lambda e: "state" if e in STATES else "territory" if e in TERRITORIES else "tribe_or_other")
    base = nat[keep + ["entity_type", "nkey"]]
    nonpr = base[base.ENTITY_ABBR != "PR"].merge(
        nm[["name_key", "parameter_code", "parameter_desc", "match_status", "match_type", "match_rule"]],
        left_on="nkey", right_on="name_key", how="left",
    ).drop(columns=["name_key"])
    prr = base[base.ENTITY_ABBR == "PR"].merge(
        prj[["CRITERION_ID", "parameter_code"]].drop_duplicates(), on="CRITERION_ID", how="left"
    )
    prr["parameter_desc"] = prr.parameter_code.map(icis_desc).fillna("")
    prr["match_status"] = prr.parameter_code.map(lambda c: "MATCHED" if isinstance(c, str) and c else "UNRESOLVED")
    prr["match_type"] = prr.match_status.map(lambda s: "PR_CROSSWALK" if s == "MATCHED" else "UNRESOLVED")
    prr["match_rule"] = prr.match_status.map(lambda s: "code assigned in the Puerto Rico crosswalk" if s == "MATCHED" else "")
    full = pd.concat([nonpr, prr], ignore_index=True).drop(columns=["nkey"])
    full["parameter_code"] = full.parameter_code.fillna("")
    full["parameter_desc"] = full.parameter_desc.fillna("")
    # ICIS status of the code: 'I' (inactive) codes cannot be assumed reportable on a DMR.
    status = dict(zip(ref.parameter_code, ref.STATUS_FLAG))
    full["icis_status"] = full.parameter_code.map(status).fillna("")
    full = full[["ENTITY_ABBR", "ENTITY_NAME", "REGION", "entity_type"] + keep[3:] + ["parameter_code", "parameter_desc", "icis_status", "match_status", "match_type", "match_rule"]]
    full = full.sort_values(["REGION", "ENTITY_ABBR", "CRITERION_ID"], key=lambda s: pd.to_numeric(s, errors="coerce") if s.name in ("REGION", "CRITERION_ID") else s)
    full.to_csv(OUT / "national_crosswalk.csv", index=False, encoding="utf-8")

    # ---- summary ----------------------------------------------------------------------------
    crit = full.drop_duplicates(["ENTITY_ABBR", "CRITERION_ID"])
    L = []
    L.append("criteria (all entities): %d" % len(crit))
    L.append("criteria with at least one code: %d (%.1f%%)" % ((crit.match_status == "MATCHED").sum(), 100 * (crit.match_status == "MATCHED").mean()))
    sc = crit[crit.entity_type != "tribe_or_other"]
    L.append("states + territories: %d criteria, %.1f%% matched" % (len(sc), 100 * (sc.match_status == "MATCHED").mean()))
    L.append("")
    L.append("criteria by match_type:\n" + crit.match_type.value_counts().to_string())
    nm1 = nm.drop_duplicates("name_key")
    L.append("criteria rows using an INACTIVE ICIS code (mostly inherited from the PR crosswalk): %d" % (full.icis_status == "I").sum())
    L.append("\ndistinct names (excluding PR-only): %d" % len(nm1))
    L.append("names by match_type:\n" + nm1.match_type.value_counts().to_string())
    L.append("\nunresolved names by rows affected (top 20):")
    L.append(nm1[nm1.match_status == "UNRESOLVED"].sort_values("n_criteria", ascending=False).head(20)[["POLLUTANT_NAME", "n_criteria", "CAS_NO"]].to_string(index=False))
    (OUT / "build_summary.txt").write_text("\n".join(L), encoding="utf-8")
    print("\n".join(L))


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""
make_coverage_graphics.py

Slide-ready charts of how much of the national WQS crosswalk is matched to ICIS-NPDES
parameter codes. Reads other/national_crosswalk/national_crosswalk.csv and writes PNG (and SVG)
files to docs/graphics/, plus the numbers behind them.

Run from the repository root:   python other/make_coverage_graphics.py

Scope: the 50 states, DC and the territories (PR, VI, GU, AS). The 62 tribal and other
entities in the source file are left out. "Solved" means a criterion has at least one ICIS code.
"""

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle
import pandas as pd
from pathlib import Path

SRC = Path("other/national_crosswalk/national_crosswalk.csv")
OUT = Path("docs/graphics")
OUT.mkdir(parents=True, exist_ok=True)
(OUT / "data").mkdir(exist_ok=True)

# ---- palette: reference data-viz palette, light surface -----------------------------------
SURFACE = "#ffffff"
INK, INK2, MUTED = "#0b0b0b", "#52514e", "#8a8985"
SOLVED = "#2a78d6"          # blue, categorical slot 1 / sequential step 450
OPEN = "#c9c8c1"            # neutral gray for "not yet matched"
GRID = "#e4e3de"
ORDINAL = ["#104281", "#256abf", "#3987e5", "#86b6ef"]  # validated ordinal blues, dark to light
SEQ = ["#cde2fb", "#b7d3f6", "#9ec5f4", "#86b6ef", "#6da7ec", "#5598e7", "#3987e5", "#2a78d6",
       "#256abf", "#1c5cab", "#184f95", "#104281", "#0d366b"]

plt.rcParams.update({
    "font.family": ["Segoe UI", "Arial", "DejaVu Sans"],
    "figure.facecolor": SURFACE, "axes.facecolor": SURFACE, "savefig.facecolor": SURFACE,
    "text.color": INK, "axes.labelcolor": INK2, "xtick.color": INK2, "ytick.color": INK2,
    "svg.fonttype": "path", "axes.spines.top": False, "axes.spines.right": False,
})
FIG = (13.333, 7.5)   # 16:9
DPI = 180
FOOT = "Source: EPA national WQS criteria matched to ICIS-NPDES parameter codes. States, DC and territories; tribal entities excluded."


def save(fig, name):
    fig.savefig(OUT / f"{name}.png", dpi=DPI)
    fig.savefig(OUT / f"{name}.svg")
    plt.close(fig)


def titles(fig, title, sub):
    fig.text(0.05, 0.935, title, fontsize=26, fontweight="bold", ha="left", va="center")
    fig.text(0.05, 0.875, sub, fontsize=14.5, color=INK2, ha="left", va="center")
    fig.text(0.05, 0.035, FOOT, fontsize=9.5, color=MUTED, ha="left", va="center")


# ---- data ---------------------------------------------------------------------------------
df = pd.read_csv(SRC, dtype=str, keep_default_na=False)
df = df[df.entity_type.isin(["state", "territory"])]
crit = df.groupby(["ENTITY_ABBR", "CRITERION_ID"]).agg(
    region=("REGION", "first"), name=("POLLUTANT_NAME", "first"),
    solved=("match_status", lambda s: (s == "MATCHED").any()),
    mtype=("match_type", "first"),
).reset_index()
crit["region"] = crit.region.astype(float).astype(int)

by_state = crit.groupby(["region", "ENTITY_ABBR"]).agg(
    criteria=("solved", "size"), solved=("solved", "sum")).reset_index()
by_state["open"] = by_state.criteria - by_state.solved
by_state["pct"] = by_state.solved / by_state.criteria
# distinct pollutant names (parameters) per state, for the data file
nm = crit.assign(key=crit.name.str.lower().str.strip())
pol = nm.groupby("ENTITY_ABBR").apply(
    lambda g: pd.Series({"pollutants": g.key.nunique(), "pollutants_solved": g[g.solved].key.nunique()}),
    include_groups=False).reset_index()
by_state = by_state.merge(pol, on="ENTITY_ABBR")
by_state.to_csv(OUT / "data" / "solved_by_state.csv", index=False)

by_region = by_state.groupby("region").agg(
    jurisdictions=("ENTITY_ABBR", "size"), criteria=("criteria", "sum"), solved=("solved", "sum")).reset_index()
by_region["open"] = by_region.criteria - by_region.solved
by_region["pct"] = by_region.solved / by_region.criteria
by_region.to_csv(OUT / "data" / "solved_by_region.csv", index=False)

total, solved_n = len(crit), int(crit.solved.sum())

# ---- 1. overall: one segmented bar by how each criterion was matched ----------------------
def overall():
    grp = {
        "PR_CROSSWALK": "Puerto Rico names", "EXACT_PR": "Puerto Rico names",
        "CAS_PR": "CAS number", "ICIS_DESC_EXACT": "ICIS description",
        "NORM_PR": "Name variants", "NORM_VALENCE": "Name variants", "UNRESOLVED": "Not yet matched",
    }
    c = crit.assign(g=crit.mtype.map(grp)).g.value_counts()
    order = ["Puerto Rico names", "ICIS description", "CAS number", "Name variants", "Not yet matched"]
    cols = ORDINAL + [OPEN]
    fig = plt.figure(figsize=FIG)
    titles(fig, "How much of the state standards link to ICIS parameters",
           "All 50 states, DC and the territories: 25,978 water quality criteria")
    fig.text(0.05, 0.68, f"{solved_n/total:.1%}", fontsize=84, fontweight="bold", color=SOLVED, ha="left", va="center")
    fig.text(0.40, 0.69, f"{solved_n:,} of {total:,} criteria\nalready have an ICIS-NPDES parameter code",
             fontsize=20, color=INK, ha="left", va="center", linespacing=1.3)
    ax = fig.add_axes([0.05, 0.30, 0.90, 0.17])
    ax.set_xlim(0, total); ax.set_ylim(0, 1); ax.axis("off")
    x = 0
    for g, col in zip(order, cols):
        n = int(c.get(g, 0))
        ax.add_patch(Rectangle((x + 8, 0), n - 16, 1, color=col, lw=0))
        if n / total > 0.05:
            ax.text(x + n / 2, 0.5, f"{n:,}\n{n/total:.0%}", ha="center", va="center", fontsize=15,
                    fontweight="bold", color="#ffffff" if g not in ("ICIS description", "Name variants", "Not yet matched") else INK)
        x += n
    # legend
    x0 = 0.05
    for g, col in zip(order, cols):
        n = int(c.get(g, 0))
        fig.patches.append(Rectangle((x0, 0.215), 0.014, 0.025, transform=fig.transFigure, color=col))
        fig.text(x0 + 0.02, 0.2275, g if g == "Not yet matched" else g, fontsize=12.5, color=INK2, va="center")
        x0 += 0.02 + 0.0078 * len(g) + 0.04
    fig.text(0.05, 0.14, "Names the Puerto Rico work already matched carry almost everything. The rest were matched by an exact\n"
             "ICIS description or CAS number, or are name variants. Unmatched criteria are left blank for review.",
             fontsize=12.5, color=INK2, va="center", linespacing=1.45)
    save(fig, "01_overall_solved")
    pd.DataFrame({"group": order, "criteria": [int(c.get(g, 0)) for g in order]}).to_csv(OUT / "data" / "overall_by_method.csv", index=False)


# ---- 2. by region: stacked horizontal bars -------------------------------------------------
STATE_LISTS = by_state.groupby("region").ENTITY_ABBR.apply(lambda s: ", ".join(sorted(s)))

def region_chart():
    fig = plt.figure(figsize=FIG)
    titles(fig, "Criteria matched to ICIS parameter codes, by EPA region",
           "Share of each region's state water quality criteria that already have a code")
    ax = fig.add_axes([0.20, 0.13, 0.70, 0.68])
    d = by_region.sort_values("region", ascending=False)
    y = list(range(len(d)))
    ax.barh(y, d.solved, color=SOLVED, height=0.60)
    ax.barh(y, d.open, left=d.solved, color=OPEN, height=0.60)
    for yi, r in zip(y, d.itertuples()):
        ax.text(r.solved - 30, yi, f"{r.solved:,}", ha="right", va="center", color="#ffffff", fontsize=12.5, fontweight="bold")
        ax.text(r.criteria + 40, yi, f"{r.pct:.0%}", ha="left", va="center", color=INK, fontsize=16, fontweight="bold")
        ax.text(-0.015, yi + 0.12, f"Region {r.region}", transform=ax.get_yaxis_transform(), ha="right", va="center", fontsize=14.5, fontweight="bold", color=INK)
        ax.text(-0.015, yi - 0.20, STATE_LISTS[r.region].replace(", ", " "), transform=ax.get_yaxis_transform(), ha="right", va="center", fontsize=9.5, color=MUTED)
    ax.set_yticks(y)
    ax.set_yticklabels([])
    ax.set_ylim(-0.6, len(d) - 0.4)
    ax.set_xlim(0, d.criteria.max() * 1.12)
    ax.set_xticks([0, 1000, 2000, 3000])
    ax.set_xticklabels(["0", "1,000", "2,000", "3,000"])
    ax.xaxis.grid(True, color=GRID, lw=0.8)
    ax.set_axisbelow(True)
    ax.spines["left"].set_visible(False)
    ax.spines["bottom"].set_visible(False)
    ax.tick_params(axis="y", length=0)
    ax.tick_params(axis="x", length=0, labelsize=11, pad=6)
    fig.text(0.90, 0.07, "criteria", fontsize=11, color=INK2, ha="right", va="center")
    fig.patches.append(Rectangle((0.665, 0.866), 0.012, 0.022, transform=fig.transFigure, color=SOLVED))
    fig.text(0.683, 0.877, "Matched", fontsize=12.5, color=INK2, va="center")
    fig.patches.append(Rectangle((0.765, 0.866), 0.012, 0.022, transform=fig.transFigure, color=OPEN))
    fig.text(0.783, 0.877, "Not yet matched", fontsize=12.5, color=INK2, va="center")
    save(fig, "02_by_region")


# ---- 3. by state: US tile map --------------------------------------------------------------
TILES = {
    "AK": (0, 0), "ME": (10, 0), "VT": (9, 1), "NH": (10, 1),
    "WA": (0, 2), "ID": (1, 2), "MT": (2, 2), "ND": (3, 2), "MN": (4, 2), "IL": (5, 2), "WI": (6, 2), "MI": (7, 2), "NY": (8, 2), "RI": (9, 2), "MA": (10, 2),
    "OR": (0, 3), "NV": (1, 3), "WY": (2, 3), "SD": (3, 3), "IA": (4, 3), "IN": (5, 3), "OH": (6, 3), "PA": (7, 3), "NJ": (8, 3), "CT": (9, 3),
    "CA": (0, 4), "UT": (1, 4), "CO": (2, 4), "NE": (3, 4), "MO": (4, 4), "KY": (5, 4), "WV": (6, 4), "VA": (7, 4), "MD": (8, 4), "DE": (9, 4),
    "AZ": (1, 5), "NM": (2, 5), "KS": (3, 5), "AR": (4, 5), "TN": (5, 5), "NC": (6, 5), "SC": (7, 5), "DC": (8, 5),
    "OK": (3, 6), "LA": (4, 6), "MS": (5, 6), "AL": (6, 6), "GA": (7, 6),
    "HI": (0, 7), "TX": (3, 7), "FL": (8, 7),
    "GU": (0, 8.4), "AS": (1, 8.4), "PR": (9, 8.4), "VI": (10, 8.4),
}

def tile_map():
    fig = plt.figure(figsize=FIG)
    titles(fig, "Criteria matched to ICIS parameter codes, by state",
           "Share of each jurisdiction's water quality criteria that already have a code. Small number is the EPA region.")
    ax = fig.add_axes([0.05, 0.09, 0.72, 0.76])
    ax.set_xlim(-0.1, 11.1); ax.set_ylim(9.5, -0.1); ax.set_aspect("equal"); ax.axis("off")
    lo, hi = 0.60, 0.95
    for r in by_state.itertuples():
        if r.ENTITY_ABBR not in TILES:
            continue
        cx, cy = TILES[r.ENTITY_ABBR]
        t = min(max((r.pct - lo) / (hi - lo), 0), 1)
        col = SEQ[int(round(t * (len(SEQ) - 1)))]
        dark = int(round(t * (len(SEQ) - 1))) >= 6
        ax.add_patch(FancyBboxPatch((cx + 0.04, cy + 0.04), 0.92, 0.92, boxstyle="round,pad=0,rounding_size=0.08", fc=col, ec="none"))
        tc = "#ffffff" if dark else INK
        ax.text(cx + 0.5, cy + 0.40, r.ENTITY_ABBR, ha="center", va="center", fontsize=14, fontweight="bold", color=tc)
        ax.text(cx + 0.5, cy + 0.72, f"{r.pct:.0%}", ha="center", va="center", fontsize=10.5, color=tc)
        ax.text(cx + 0.88, cy + 0.17, str(r.region), ha="right", va="center", fontsize=7, color=tc, alpha=0.85)
    ax.text(0.0, 8.15, "Territories", fontsize=10, color=MUTED, ha="left", va="center")
    # color scale
    cax = fig.add_axes([0.82, 0.55, 0.025, 0.25])
    cax.imshow([[i] for i in range(len(SEQ) - 1, -1, -1)], cmap=matplotlib.colors.ListedColormap(SEQ), aspect="auto")
    cax.set_xticks([]); cax.yaxis.tick_right()
    cax.set_yticks([0, len(SEQ) - 1]); cax.set_yticklabels(["95% or more", "60% or less"], fontsize=11)
    for s in cax.spines.values(): s.set_visible(False)
    cax.tick_params(length=0)
    fig.text(0.82, 0.83, "Criteria matched", fontsize=12, color=INK2, ha="left", va="center")
    lowest = by_state.sort_values("pct").head(3)
    fig.text(0.82, 0.43, "Lowest three", fontsize=12, color=INK2, ha="left", va="center")
    for i, r in enumerate(lowest.itertuples()):
        fig.text(0.82, 0.385 - i * 0.045, f"{r.ENTITY_ABBR}  {r.pct:.0%}", fontsize=13, color=INK, fontweight="bold", ha="left", va="center")
    save(fig, "03_by_state_map")


# ---- 4. by state, grouped by region: small multiples ---------------------------------------
def state_panels():
    fig, axes = plt.subplots(2, 5, figsize=FIG, gridspec_kw={"left": 0.045, "right": 0.985, "top": 0.80, "bottom": 0.09, "wspace": 0.35, "hspace": 0.30})
    titles(fig, "Criteria matched to ICIS parameter codes, state by state",
           "Each panel is one EPA region, with its overall share. Each bar is one state: blue matched, gray not yet matched.")
    for ax, reg in zip(axes.flat, range(1, 11)):
        d = by_state[by_state.region == reg].sort_values("pct", ascending=False)
        y = list(range(len(d)))
        ax.barh(y, d.pct, color=SOLVED, height=0.62)
        ax.barh(y, 1 - d.pct, left=d.pct, color=OPEN, height=0.62)
        ax.set_yticks(y)
        ax.set_yticklabels(d.ENTITY_ABBR, fontsize=11, fontweight="bold")
        for yi, r in zip(y, d.itertuples()):
            ax.text(1.04, yi, f"{r.pct:.0%}", va="center", ha="left", fontsize=10.5, color=INK, fontweight="bold")
        ax.set_xlim(0, 1.32)
        ax.set_ylim(7.6, -0.6)
        ax.set_xticks([])
        for s_ in ("left", "bottom"):
            ax.spines[s_].set_visible(False)
        ax.tick_params(axis="y", length=0, pad=3)
        rp = by_region[by_region.region == reg].pct.iloc[0]
        ax.set_title(f"Region {reg}  \u00b7  {rp:.0%}", fontsize=13, fontweight="bold", loc="left", pad=8)
    save(fig, "04_by_state_by_region")


# ---- 5. what is left: coverage curve of the unresolved names --------------------------------
def remaining_curve():
    u = crit[~crit.solved]
    vc = u.assign(k=u.name.str.lower().str.strip()).groupby("k").size().sort_values(ascending=False)
    cum = (vc.cumsum() / vc.sum()).values
    nopen = len(u)
    fig = plt.figure(figsize=FIG)
    titles(fig, "Reviewing a few hundred names closes most of what is left",
           f"{len(vc)} pollutant names account for the {nopen:,} criteria not yet matched")
    ax = fig.add_axes([0.09, 0.15, 0.62, 0.66])
    xs = list(range(1, len(cum) + 1))
    ax.plot(xs, cum * 100, color=SOLVED, lw=3)
    marks = [(25, None), (50, None), (100, None), (300, None)]
    for n, _ in marks:
        v = cum[n - 1] * 100
        ax.plot([n], [v], "o", ms=9, color=SOLVED, mec=SURFACE, mew=2)
        ax.text(n + 7, v - 4.0, f"{n} names\n{v:.0f}%", fontsize=13, fontweight="bold", color=INK, va="top",
                bbox=dict(fc=SURFACE, ec="none", pad=2))
    ax.set_xlim(0, len(cum)); ax.set_ylim(0, 102)
    ax.yaxis.grid(True, color=GRID, lw=0.8); ax.set_axisbelow(True)
    ax.set_yticks([0, 25, 50, 75, 100]); ax.set_yticklabels(["0%", "25%", "50%", "75%", "100%"], fontsize=12)
    ax.tick_params(axis="x", labelsize=12, length=0, pad=10); ax.tick_params(axis="y", length=0, pad=8)
    ax.spines["left"].set_visible(False); ax.spines["bottom"].set_color(GRID)
    ax.set_xlabel("Unmatched pollutant names reviewed, most-used first", fontsize=12.5, labelpad=8)
    ax.set_ylabel("Share of unmatched criteria resolved", fontsize=12.5, labelpad=8)
    top = vc.head(8)
    fig.text(0.75, 0.78, "Largest unmatched names", fontsize=13, color=INK2, fontweight="bold", va="center")
    for i, (k, n) in enumerate(top.items()):
        lab = k.split("(")[0].strip() if "(" in k else k
        fig.text(0.75, 0.72 - i * 0.055, lab if len(lab) < 34 else lab[:32] + "\u2026", fontsize=12, color=INK, va="center")
        fig.text(0.965, 0.72 - i * 0.055, f"{n}", fontsize=12.5, color=INK2, va="center", ha="right", fontweight="bold")
    fig.text(0.75, 0.26, "Number is criteria affected.\nMost need a choice between\nseveral ICIS codes, such as total\nvs dissolved, not a name match.",
             fontsize=11.5, color=MUTED, va="top", linespacing=1.4)
    save(fig, "05_whats_left")
    pd.DataFrame({"names_reviewed": range(1, len(cum) + 1), "share_of_unmatched_criteria": cum}).to_csv(OUT / "data" / "unmatched_coverage_curve.csv", index=False)


if __name__ == "__main__":
    overall(); region_chart(); tile_map(); state_panels(); remaining_curve()
    print(by_region.to_string(index=False))
    print(by_state.sort_values("pct").head(10)[["ENTITY_ABBR", "region", "criteria", "solved", "pct"]].to_string(index=False))

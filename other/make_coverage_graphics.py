#!/usr/bin/env python3
"""
make_coverage_graphics.py

Slide-ready charts of how much of the national WQS crosswalk is matched to ICIS-NPDES parameter codes. Reads
other/national_crosswalk/national_crosswalk.csv and writes PNG (and SVG) files to docs/graphics/, plus the numbers
behind them.

Run from the repository root:   python other/make_coverage_graphics.py

Scope: every criterion in the national file (all 120 entities). Entities are grouped as:
  state       the 50 states and DC
  territory   PR, VI, GU, AS and the Northern Mariana Islands (CNMI, tagged "tribe_or_other" in the source file)
  tribe       the 52 tribes and pueblos with their own standards
  other       the 9 California regional water board plans (CA1-CA9) and 3 federal rules (EPA 304(a), NTR, CTR)
"Solved" means a criterion has at least one ICIS code.
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
FOOT = "Source: EPA national WQS criteria (all 120 entities) matched to ICIS-NPDES parameter codes."


def save(fig, name):
    fig.savefig(OUT / f"{name}.png", dpi=DPI)
    fig.savefig(OUT / f"{name}.svg")
    plt.close(fig)


def titles(fig, title, sub, foot=FOOT):
    fig.text(0.05, 0.935, title, fontsize=26, fontweight="bold", ha="left", va="center")
    fig.text(0.05, 0.875, sub, fontsize=14.5, color=INK2, ha="left", va="center")
    fig.text(0.05, 0.035, foot, fontsize=9.5, color=MUTED, ha="left", va="center")


# ---- data ---------------------------------------------------------------------------------
TERRITORIES = {"PR", "VI", "GU", "AS", "CNMI"}
OTHER = {f"CA{i}" for i in range(1, 10)} | {"304A", "NTR", "CTR"}


def group_of(abbr, etype):
    if abbr in TERRITORIES:
        return "territory"
    if abbr in OTHER:
        return "other"
    return "state" if etype == "state" else "tribe"


df = pd.read_csv(SRC, dtype=str, keep_default_na=False)
crit = df.groupby(["ENTITY_ABBR", "CRITERION_ID"]).agg(
    name_ent=("ENTITY_NAME", "first"), etype=("entity_type", "first"), region=("REGION", "first"),
    name=("POLLUTANT_NAME", "first"), solved=("match_status", lambda s: (s == "MATCHED").any()),
    mtype=("match_type", "first"),
).reset_index()
crit["group"] = [group_of(a, e) for a, e in zip(crit.ENTITY_ABBR, crit.etype)]
crit["region"] = pd.to_numeric(crit.region, errors="coerce")           # NaN for the three federal rules

by_entity = crit.groupby(["group", "ENTITY_ABBR"]).agg(
    entity=("name_ent", "first"), region=("region", "first"), criteria=("solved", "size"), solved=("solved", "sum")).reset_index()
by_entity["open"] = by_entity.criteria - by_entity.solved
by_entity["pct"] = by_entity.solved / by_entity.criteria
by_entity.to_csv(OUT / "data" / "solved_by_entity.csv", index=False)

# states + DC + territories (tile map and state panels)
by_state = by_entity[by_entity.group.isin(["state", "territory"])].copy()
by_state["region"] = by_state.region.astype(int)
by_state.drop(columns=["entity"]).to_csv(OUT / "data" / "solved_by_state.csv", index=False)

by_group = crit.groupby("group").agg(entities=("ENTITY_ABBR", "nunique"), criteria=("solved", "size"), solved=("solved", "sum")).reset_index()
by_group["pct"] = by_group.solved / by_group.criteria
by_group.to_csv(OUT / "data" / "solved_by_group.csv", index=False)

# region x group table (federal rules have no region and are left out of the region tables)
reg = crit[crit.region.notna()].assign(set=lambda d: d.group.map({"state": "state_territory", "territory": "state_territory", "tribe": "tribe", "other": "other"}))
by_region = reg.groupby(["region", "set"]).agg(entities=("ENTITY_ABBR", "nunique"), criteria=("solved", "size"), solved=("solved", "sum")).reset_index()
by_region["region"] = by_region.region.astype(int)
by_region["open"] = by_region.criteria - by_region.solved
by_region["pct"] = by_region.solved / by_region.criteria
by_region.to_csv(OUT / "data" / "solved_by_region.csv", index=False)

total, solved_n = len(crit), int(crit.solved.sum())
G = by_group.set_index("group")


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
    titles(fig, "How much of the water quality standards link to ICIS parameters",
           f"{total:,} criteria from {crit.ENTITY_ABBR.nunique()} entities: states, territories, tribes and federal rules")
    fig.text(0.05, 0.72, f"{solved_n/total:.1%}", fontsize=84, fontweight="bold", color=SOLVED, ha="left", va="center")
    fig.text(0.40, 0.73, f"{solved_n:,} of {total:,} criteria\nalready have an ICIS-NPDES parameter code",
             fontsize=20, color=INK, ha="left", va="center", linespacing=1.3)
    ax = fig.add_axes([0.05, 0.41, 0.90, 0.15])
    ax.set_xlim(0, total); ax.set_ylim(0, 1); ax.axis("off")
    x = 0
    for g, col in zip(order, cols):
        n = int(c.get(g, 0))
        ax.add_patch(Rectangle((x + 8, 0), n - 16, 1, color=col, lw=0))
        if n / total > 0.05:
            ax.text(x + n / 2, 0.5, f"{n:,}\n{n/total:.0%}", ha="center", va="center", fontsize=15,
                    fontweight="bold", color="#ffffff" if g not in ("ICIS description", "Name variants", "Not yet matched") else INK)
        x += n
    x0 = 0.05
    for g, col in zip(order, cols):
        fig.patches.append(Rectangle((x0, 0.345), 0.014, 0.025, transform=fig.transFigure, color=col))
        fig.text(x0 + 0.02, 0.3575, g, fontsize=12.5, color=INK2, va="center")
        x0 += 0.02 + 0.0078 * len(g) + 0.04
    # the four kinds of entity
    fig.text(0.05, 0.265, "By kind of entity", fontsize=13, color=INK2, fontweight="bold", va="center")
    blocks = [("States and DC", "state"), ("Territories", "territory"), ("Tribes", "tribe"),
              ("California regional boards and federal rules", "other")]
    for i, (lab, key) in enumerate(blocks):
        r = G.loc[key]
        x = 0.05 + i * 0.235
        fig.text(x, 0.215, f"{r.pct:.0%}", fontsize=28, fontweight="bold", color=SOLVED, va="center")
        fig.text(x + 0.078, 0.228, lab if len(lab) < 24 else "CA regional boards,\nfederal rules", fontsize=12, color=INK, va="center", linespacing=1.15)
        fig.text(x + 0.078, 0.175, f"{int(r.solved):,} of {int(r.criteria):,}", fontsize=10.5, color=INK2, va="center")
        fig.text(x + 0.078, 0.150, f"{int(r.entities)} entities", fontsize=10.5, color=INK2, va="center")
    fig.text(0.05, 0.088, "Names the Puerto Rico work already matched carry almost everything. The rest were matched by an exact ICIS description or "
             "CAS number, or are name variants.\nUnmatched criteria are left blank for review.",
             fontsize=12, color=INK2, va="center", linespacing=1.45)
    save(fig, "01_overall_solved")
    pd.DataFrame({"group": order, "criteria": [int(c.get(g, 0)) for g in order]}).to_csv(OUT / "data" / "overall_by_method.csv", index=False)


# ---- 2. by region: states/territories beside tribes ----------------------------------------
STATE_LISTS = by_state.groupby("region").ENTITY_ABBR.apply(lambda s: " ".join(sorted(s)))


def region_chart():
    fig = plt.figure(figsize=FIG)
    o = G.loc["other"]
    titles(fig, "Criteria matched to ICIS parameter codes, by EPA region",
           "States and territories on the left, tribes on the right, on the same scale.",
           foot=f"Not shown: 9 California regional board plans and 3 federal rules, {int(o.criteria):,} criteria, "
                f"{o.pct:.0%} matched. Northern Mariana Islands counted as a territory.")
    xmax = by_region[by_region.set != "other"].criteria.max() * 1.18
    regs = list(range(10, 0, -1))
    panels = [("state_territory", "States, DC and territories", [0.17, 0.11, 0.35, 0.68]),
              ("tribe", "Tribes", [0.60, 0.11, 0.35, 0.68])]
    for key, label, rect in panels:
        ax = fig.add_axes(rect)
        d = by_region[by_region.set == key].set_index("region")
        tot = G.loc["state"] if False else None
        for yi, rg in enumerate(regs):
            if rg in d.index:
                r = d.loc[rg]
                ax.barh(yi, r.solved, color=SOLVED, height=0.60)
                ax.barh(yi, r.open, left=r.solved, color=OPEN, height=0.60)
                if r.solved > 0.16 * xmax:
                    ax.text(r.solved - xmax * 0.012, yi, f"{int(r.solved):,}", ha="right", va="center", color="#ffffff", fontsize=11.5, fontweight="bold")
                ax.text(r.criteria + xmax * 0.012, yi, f"{r.pct:.0%}", ha="left", va="center", color=INK, fontsize=14, fontweight="bold")
            else:
                ax.text(xmax * 0.01, yi, "none in the file", ha="left", va="center", color=MUTED, fontsize=11, style="italic")
        ax.set_ylim(-0.6, len(regs) - 0.4)
        ax.set_xlim(0, xmax)
        ax.set_yticks(range(len(regs)))
        ax.set_yticklabels([])
        ax.set_xticks([0, 2000, 4000]); ax.set_xticklabels(["0", "2,000", "4,000"])
        ax.xaxis.grid(True, color=GRID, lw=0.8); ax.set_axisbelow(True)
        ax.spines["left"].set_visible(False); ax.spines["bottom"].set_visible(False)
        ax.tick_params(axis="y", length=0); ax.tick_params(axis="x", length=0, labelsize=10.5, pad=6)
        sub = by_region[by_region.set == key]
        ax.set_title(f"{label}  ·  {sub.solved.sum()/sub.criteria.sum():.0%} of {int(sub.criteria.sum()):,}", fontsize=14, fontweight="bold", loc="left", pad=10)
        if key == "state_territory":
            for yi, rg in enumerate(regs):
                ax.text(-0.02, yi + 0.13, f"Region {rg}", transform=ax.get_yaxis_transform(), ha="right", va="center", fontsize=14, fontweight="bold")
                ax.text(-0.02, yi - 0.22, STATE_LISTS.get(rg, ""), transform=ax.get_yaxis_transform(), ha="right", va="center", fontsize=8.5, color=MUTED)
        else:
            for yi, rg in enumerate(regs):
                n = int(d.loc[rg].entities) if rg in d.index else 0
                if n:
                    ax.text(-0.02, yi, f"{n} tribe" + ("s" if n > 1 else ""), transform=ax.get_yaxis_transform(), ha="right", va="center", fontsize=10.5, color=MUTED)
    fig.text(0.95, 0.065, "criteria", fontsize=11, color=INK2, ha="right", va="center")
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
    "GU": (0, 8.4), "AS": (1, 8.4), "CNMI": (2, 8.4), "PR": (9, 8.4), "VI": (10, 8.4),
}


# Fill colors of the EPA regional map (epa.gov regional offices map), sampled from the image
EPA_REGION_FILL = {1: "#ccccff", 2: "#ffff99", 3: "#b1e6fe", 4: "#ccff99", 5: "#ffbb77",
                   6: "#cccc66", 7: "#99ccff", 8: "#99cc99", 9: "#99ffcc", 10: "#fee89a"}


def region_outline_color(rg):
    """The map's own fill color, made more saturated and a little darker so a thin outline stays visible on white."""
    import colorsys
    r, g, b = matplotlib.colors.to_rgb(EPA_REGION_FILL[rg])
    h, s, v = colorsys.rgb_to_hsv(r, g, b)
    return colorsys.hsv_to_rgb(h, min(1.0, s * 1.35), min(v, 0.82))


def draw_region_outlines(ax):
    """Outline-only boxes around each EPA region's tiles. Regions that are not contiguous on the grid
    (AK, HI, the territories, CA/NV/AZ) get one box per piece."""
    from shapely.geometry import box
    from shapely.ops import unary_union
    for rg, grp in by_state.groupby("region"):
        cells = [TILES[a] for a in grp.ENTITY_ABBR if a in TILES]
        comps, left = [], list(cells)
        while left:                                   # connected pieces (4-neighbours on the tile grid)
            comp, stack = [], [left.pop()]
            while stack:
                c = stack.pop(); comp.append(c)
                for n in [n for n in left if (abs(n[0] - c[0]) == 1 and n[1] == c[1]) or (n[0] == c[0] and abs(n[1] - c[1]) == 1)]:
                    left.remove(n); stack.append(n)
            comps.append(comp)
        col = region_outline_color(rg)
        lum = 0.2126 * col[0] + 0.7152 * col[1] + 0.0722 * col[2]
        for comp in comps:
            geom = unary_union([box(cx, cy, cx + 1, cy + 1) for cx, cy in comp]).buffer(-0.09, join_style=1).buffer(0.06, join_style=1)
            for poly in (geom.geoms if hasattr(geom, "geoms") else [geom]):
                x, y = poly.exterior.xy
                ax.plot(x, y, color=col, lw=2.6, solid_joinstyle="round", zorder=3, clip_on=False)
            top = min(comp, key=lambda c: (c[1], c[0]))      # numbered badge on the top-left corner of each box
            ax.add_patch(plt.Circle((top[0] + 0.03, top[1] + 0.03), 0.22, color=col, zorder=4, lw=0, clip_on=False))
            ax.text(top[0] + 0.03, top[1] + 0.03, str(rg), ha="center", va="center_baseline", fontsize=9.5 if rg < 10 else 8, fontweight="bold",
                    color="#ffffff" if lum < 0.62 else INK, zorder=5, clip_on=False)


def tile_map():
    fig = plt.figure(figsize=FIG)
    titles(fig, "Criteria matched to ICIS parameter codes, by state",
           "Share of each state's and territory's criteria that already have a code. Outlines and numbers mark the EPA regions.",
           foot=FOOT + " Tribes are on the next slide.")
    ax = fig.add_axes([0.05, 0.09, 0.72, 0.76])
    ax.set_xlim(-0.1, 11.1); ax.set_ylim(9.5, -0.2); ax.set_aspect("equal"); ax.axis("off")
    lo, hi = 0.60, 0.95
    for r in by_state.itertuples():
        if r.ENTITY_ABBR not in TILES:
            continue
        cx, cy = TILES[r.ENTITY_ABBR]
        t = min(max((r.pct - lo) / (hi - lo), 0), 1)
        col = SEQ[int(round(t * (len(SEQ) - 1)))]
        dark = int(round(t * (len(SEQ) - 1))) >= 6
        ax.add_patch(FancyBboxPatch((cx + 0.11, cy + 0.11), 0.78, 0.78, boxstyle="round,pad=0,rounding_size=0.07", fc=col, ec="none"))
        tc = "#ffffff" if dark else INK
        ax.text(cx + 0.5, cy + 0.41, r.ENTITY_ABBR, ha="center", va="center", fontsize=13.5 if len(r.ENTITY_ABBR) < 4 else 11, fontweight="bold", color=tc)
        ax.text(cx + 0.5, cy + 0.70, f"{r.pct:.0%}", ha="center", va="center", fontsize=10, color=tc)
    draw_region_outlines(ax)
    ax.text(0.34, 8.17, "Territories", fontsize=10, color=MUTED, ha="left", va="center")
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
def region_panels(group_keys, name, title, sub, empty_regions_note=None):
    fig, axes = plt.subplots(2, 5, figsize=FIG, gridspec_kw={"left": 0.045, "right": 0.985, "top": 0.80, "bottom": 0.09, "wspace": 0.35, "hspace": 0.30})
    titles(fig, title, sub, foot=FOOT)
    sel = by_entity[by_entity.group.isin(group_keys)]
    for ax, rg in zip(axes.flat, range(1, 11)):
        d = sel[sel.region == rg].sort_values("pct", ascending=False)
        ax.set_xticks([])
        for s_ in ("left", "bottom"):
            ax.spines[s_].set_visible(False)
        tot = by_region[(by_region.region == rg) & (by_region.set.isin(["state_territory"] if "state" in group_keys else ["tribe"]))]
        head = f"Region {rg}"
        if len(d):
            head += f"  ·  {d.solved.sum()/d.criteria.sum():.0%}"
        ax.set_title(head, fontsize=13, fontweight="bold", loc="left", pad=8)
        if not len(d):
            ax.set_yticks([]); ax.text(0.0, 0.5, "none in the file", transform=ax.transAxes, color=MUTED, fontsize=11, style="italic")
            continue
        y = list(range(len(d)))
        ax.barh(y, d.pct, color=SOLVED, height=0.62)
        ax.barh(y, 1 - d.pct, left=d.pct, color=OPEN, height=0.62)
        ax.set_yticks(y)
        ax.set_yticklabels([short(r) for r in d.itertuples()], fontsize=10.5 if "state" in group_keys else 8.5, fontweight="bold" if "state" in group_keys else "normal")
        for yi, r in zip(y, d.itertuples()):
            ax.text(1.04, yi, f"{r.pct:.0%}", va="center", ha="left", fontsize=10 if "state" in group_keys else 8.5, color=INK, fontweight="bold")
        ax.set_xlim(0, 1.32)
        ax.set_ylim(len(d) - 0.4 if len(d) > 7 or "state" not in group_keys else 7.6, -0.6)
        ax.tick_params(axis="y", length=0, pad=3)
    save(fig, name)


def short(r):
    n = r.entity
    for a, b in (("Confederated Tribes of the ", ""), ("Confederated Tribes of ", ""), ("Pueblo of ", ""), ("California Region ", "CA "),
                 (" Indian Reservation", ""), (" Reservation", ""), (" Indian Tribe", ""), (" Tribe", ""), (" Indian Community", ""),
                 (" Tribal Community", ""), (" Band of Lake Superior Chippewa", " Chippewa"), (" Band of Minnesota Chippewa", " Chippewa"),
                 (" Band of Mission Indians", " Mission Indians"), ("Assiniboine and Sioux Tribes of the Fort Peck", "Fort Peck (Assiniboine, Sioux)"),
                 ("EPA 304(a) Recommended Criteria", "EPA 304(a)"), ("CTR - California Toxics Rule", "California Toxics Rule"),
                 ("NTR - National Toxics Rule", "National Toxics Rule")):
        n = n.replace(a, b)
    if r.group in ("state", "territory"):
        return r.ENTITY_ABBR
    return TRIBE_LABELS.get(r.ENTITY_ABBR, n if len(n) <= 26 else n[:24] + "…")


# readable short labels for names that would otherwise be cut off
TRIBE_LABELS = {
    "EBCI": "Eastern Band of Cherokee", "SKT": "Salish and Kootenai", "ASFP": "Fort Peck (Assiniboine, Sioux)",
    "KDWN": "Kletsel Dehe Wintun", "RBLMI": "Rincon (Luiseño)", "TWNIN": "Twenty-Nine Palms",
    "BPP": "Big Pine Paiute", "CLUSI": "Coos, Lower Umpqua, Siuslaw", "SWINO": "Swinomish", "OHKA": "Ohkay Owingeh",
    "UMU": "Ute Mountain Ute", "LLBO": "Leech Lake Ojibwe", "LACDU": "Lac du Flambeau", "SRM": "Saint Regis Mohawk",
    "CA7": "CA 7 - Colorado River", "HOOP": "Hoopa Valley", "FDL": "Fond du Lac", "BRBCHP": "Bad River", "GPCHMN": "Grand Portage",
    "KEWE": "Keweenaw Bay", "WST": "Warm Springs", "UMA": "Umatilla", "COL": "Colville", "WMA": "White Mountain Apache",
}


def state_panels():
    region_panels(["state", "territory"], "04_by_state_by_region",
                  "Criteria matched to ICIS parameter codes, state by state",
                  "Each panel is one EPA region, with its overall share. Each bar is one state or territory: blue matched, gray not yet matched.")


def tribe_panels():
    # one panel per region that has tribes, plus a panel for the California regional boards and federal rules
    fig, axes = plt.subplots(2, 4, figsize=FIG, gridspec_kw={"left": 0.14, "right": 0.96, "top": 0.80, "bottom": 0.07, "wspace": 1.05, "hspace": 0.28})
    titles(fig, "Criteria matched to ICIS parameter codes, tribe by tribe",
           f"{int(G.loc['tribe'].entities)} tribes with their own standards, by EPA region. Blue matched, gray not yet matched.",
           foot="Other = the nine California regional board plans and three federal rules (EPA 304(a), NTR, CTR). " + FOOT)
    t = by_entity[by_entity.group == "tribe"]
    regions = sorted(t.region.dropna().unique().astype(int))
    plots = [(f"Region {r}", t[t.region == r]) for r in regions] + [("Other", by_entity[by_entity.group == "other"])]
    for ax, (label, d) in zip(axes.flat, plots):
        d = d.sort_values("pct", ascending=False)
        y = list(range(len(d)))
        ax.barh(y, d.pct, color=SOLVED, height=0.62)
        ax.barh(y, 1 - d.pct, left=d.pct, color=OPEN, height=0.62)
        ax.set_yticks(y)
        ax.set_yticklabels([short(r) for r in d.itertuples()], fontsize=8.5)
        for yi, r in zip(y, d.itertuples()):
            ax.text(1.04, yi, f"{r.pct:.0%}", va="center", ha="left", fontsize=8.5, color=INK, fontweight="bold")
        ax.set_xlim(0, 1.3); ax.set_ylim(11.6, -0.6); ax.set_xticks([])
        for s_ in ("left", "bottom"):
            ax.spines[s_].set_visible(False)
        ax.tick_params(axis="y", length=0, pad=3)
        ax.set_title(f"{label}  ·  {d.solved.sum()/d.criteria.sum():.0%}", fontsize=12.5, fontweight="bold", loc="left", pad=8)
    for ax in list(axes.flat)[len(plots):]:
        ax.axis("off")
    save(fig, "04b_by_tribe")


# ---- 5. what is left: coverage curve of the unresolved names --------------------------------
def remaining_curve():
    u = crit[~crit.solved]
    vc = u.assign(k=u.name.str.lower().str.strip()).groupby("k").size().sort_values(ascending=False)
    cum = (vc.cumsum() / vc.sum()).values
    nopen = len(u)
    marks = [n for n in (25, 50, 100, 300) if n <= len(cum)]
    fig = plt.figure(figsize=FIG)
    titles(fig, "Reviewing a few hundred names closes most of what is left",
           f"{len(vc)} pollutant names account for the {nopen:,} criteria not yet matched, across all entities")
    ax = fig.add_axes([0.09, 0.15, 0.62, 0.66])
    xs = list(range(1, len(cum) + 1))
    ax.plot(xs, cum * 100, color=SOLVED, lw=3)
    for n in marks:
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
        fig.text(0.75, 0.72 - i * 0.055, lab if len(lab) < 34 else lab[:32] + "…", fontsize=12, color=INK, va="center")
        fig.text(0.965, 0.72 - i * 0.055, f"{n}", fontsize=12.5, color=INK2, va="center", ha="right", fontweight="bold")
    fig.text(0.75, 0.26, "Number is criteria affected.\nMost need a choice between\nseveral ICIS codes, such as total\nvs dissolved, not a name match.",
             fontsize=11.5, color=MUTED, va="top", linespacing=1.4)
    save(fig, "05_whats_left")
    pd.DataFrame({"names_reviewed": range(1, len(cum) + 1), "share_of_unmatched_criteria": cum}).to_csv(OUT / "data" / "unmatched_coverage_curve.csv", index=False)


if __name__ == "__main__":
    overall(); region_chart(); tile_map(); state_panels(); tribe_panels(); remaining_curve()
    print(by_group.to_string(index=False))
    print(by_region.to_string(index=False))
    print(by_state.sort_values("pct").head(6)[["ENTITY_ABBR", "region", "criteria", "solved", "pct"]].to_string(index=False))
    print(by_entity[by_entity.group == "tribe"].sort_values("pct").head(6)[["ENTITY_ABBR", "region", "criteria", "solved", "pct"]].to_string(index=False))

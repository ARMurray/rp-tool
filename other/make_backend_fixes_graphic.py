#!/usr/bin/env python3
"""
make_backend_fixes_graphic.py

Slide-ready graphic of the data fixes the RP tool makes behind the scenes before any reasonable-potential
math runs. Writes docs/graphics/06_backend_data_fixes.png and .svg.

Run from the repository root:   python other/make_backend_fixes_graphic.py

The record counts are for one example permit (PR0001031, class SB + surface-water standards, the app's
default five-year window starting 2021-11-18). They were computed from the ECHO DMR exports in other/dmr/
by approximating the app's rules, so they can differ slightly from the app's database snapshot.
Each fix cites the code that does it:
  non-detects, units, temperature, ammonia -> app.R DMR loader (fetch_dmr observer)
  unit rules                               -> R/functions.R get_unit_conversion()
  statistic tagging                        -> R/functions.R tag_statistic() + data/statistic_lookup.csv
  flagging                                 -> R/functions.R flag_unmatched_params()
"""

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse, FancyArrowPatch, FancyBboxPatch
from pathlib import Path

OUT = Path("docs/graphics")
OUT.mkdir(parents=True, exist_ok=True)

SURFACE = "#ffffff"
INK, INK2, MUTED = "#0b0b0b", "#52514e", "#8a8985"
BLUE = "#2a78d6"
BLUE_SOFT = "#cde2fb"
GRAY_SOFT = "#ececE7"
GRID = "#e4e3de"

plt.rcParams.update({
    "font.family": ["Segoe UI", "Arial", "DejaVu Sans"],
    "figure.facecolor": SURFACE, "axes.facecolor": SURFACE, "savefig.facecolor": SURFACE,
    "text.color": INK, "svg.fonttype": "path",
})
FIG = (13.333, 7.5)
DPI = 180

# title, plain-language problem/fix (two lines), before chip, after chip, count (big), count caption
ROWS = [
    ("Non-detects get a value and a unit",
     "ICIS leaves them blank. The tool sets them to 0 and borrows\nthe parameter's usual unit, so they still count as samples.",
     "blank value\nblank unit", "0\nin usual unit", "365", "records"),
    ("Every result in the standard's unit",
     "Permittees report mg/L, ug/L, ng/L or ppm; each standard uses one.\nConverted record by record (zinc and chlorine here).",
     "0.028 mg/L\nzinc", "28 ug/L", "153", "records converted"),
    ("Fahrenheit becomes Celsius, duplicates go",
     "Temperature arrives under two codes (°C and °F). One series\nis kept, in °C, so nothing is counted twice.",
     "99.4 °F\ncode 00011", "37.4 °C\ncode 00010", "44", "°F records converted"),
    ("Same pollutant, different code",
     "Ammonia is reported under code 82230 in most PR permits but the\nstandard sits on 00610. Recoded so the two meet.",
     "ammonia\ncode 82230", "ammonia\ncode 00610", "150 of 184", "PR permits affected"),
    ("Loads and flows set aside, not errors",
     "kg/d, gal/d and MGD cannot be compared to a concentration\nstandard. Kept in the download, kept out of the math.",
     "kg/d, gal/d,\nMGD", "set aside\n(not an error)", "485", "records"),
    ("Only the statistic the standard asks for",
     "Each record is tagged (daily max, min, average...). A criterion\nneeds one of them, so averages and the rest are left out.",
     "max, min,\naverage", "the one the\ncriterion needs", "454", "records left out"),
    ("Nothing is silently dropped",
     "Parameters with no standard in the chosen class are listed with the\nreason and a hint (free chlorine 50064 → look at 50060).",
     "no standard\nfound", "flagged, with\nreason + hint", "8", "parameters flagged"),
]


def draw():
    fig = plt.figure(figsize=FIG)
    ax = fig.add_axes([0, 0, 1, 1])
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.axis("off")

    ax.text(0.05, 0.935, "What the tool fixes before any math runs", fontsize=26, fontweight="bold", va="center")
    ax.text(0.05, 0.875,
            "One example permit: 4,089 raw DMR records go in, about 1,270 comparable records reach the RP math. "
            "Every fix below is automatic.",
            fontsize=14, color=INK2, va="center")

    top, bottom = 0.815, 0.095
    h = (top - bottom) / len(ROWS)
    for i, (title, body, before, after, big, cap) in enumerate(ROWS):
        yc = top - h * (i + 0.5)
        if i:
            ax.plot([0.05, 0.95], [top - h * i] * 2, color=GRID, lw=1)
        # number badge
        bw = 0.030  # badge width in axes units; height is scaled so it draws as a circle on a 16:9 canvas
        ax.add_patch(Ellipse((0.066, yc), bw, bw * FIG[0] / FIG[1], color=BLUE, ec="none"))
        ax.text(0.066, yc, str(i + 1), color="#ffffff", fontsize=15, fontweight="bold", ha="center", va="center_baseline")
        # title + body
        ax.text(0.097, yc + 0.024, title, fontsize=15.5, fontweight="bold", va="center")
        ax.text(0.097, yc - 0.022, body, fontsize=10.8, color=INK2, va="center", linespacing=1.35)
        # before -> after chips
        for x0, txt, fc, ec in ((0.545, before, GRAY_SOFT, "none"), (0.735, after, BLUE_SOFT, "none")):
            ax.add_patch(FancyBboxPatch((x0, yc - 0.034), 0.115, 0.068, boxstyle="round,pad=0,rounding_size=0.008",
                                        fc=fc, ec=ec, transform=ax.transData))
            ax.text(x0 + 0.0575, yc, txt, fontsize=10.8, ha="center", va="center", linespacing=1.25)
        ax.add_patch(FancyArrowPatch((0.665, yc), (0.725, yc), arrowstyle="-|>", mutation_scale=16, color=MUTED, lw=1.6))
        # count
        ax.text(0.95, yc + 0.012, big, fontsize=21 if len(big) < 6 else 16.5, fontweight="bold", ha="right", va="center")
        ax.text(0.95, yc - 0.032, cap, fontsize=9.8, color=INK2, ha="right", va="center")

    ax.text(0.05, 0.053,
            "Also dropped: 1,221 records with no reported value (for example no discharge). Standards written as formulas "
            "(hardness-based metals, ammonia from pH and temperature) are solved at run time.",
            fontsize=9.5, color=MUTED, va="center")
    ax.text(0.05, 0.027,
            "Example permit PR0001031, class SB + surface-water standards, window from 2021-11-18. Counts approximated from the ECHO "
            "DMR export; the app's database snapshot may differ slightly.",
            fontsize=9.5, color=MUTED, va="center")
    for ext in ("png", "svg"):
        fig.savefig(OUT / f"06_backend_data_fixes.{ext}", dpi=DPI)
    plt.close(fig)


if __name__ == "__main__":
    draw()
    print("wrote", OUT / "06_backend_data_fixes.png")

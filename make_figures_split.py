"""
Two separate figures from the primary-vs-augmented comparison (OHDSI, 15 studies):
  1) figure_sections.pdf  -> 4 per-section accuracy panels (SP, TAR, PS, OM)
  2) figure_overall.pdf   -> single Overall accuracy panel
Style mirrors Figure 3: solid blue = Primary, hatched = value-augmented.
"""
import json
import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

# Read the committed pa_data.json next to this script (regenerate via build_pa_data.py);
# fall back to /tmp for backward compatibility.
_HERE = os.path.dirname(os.path.abspath(__file__))
_REPO = os.path.join(_HERE, "pa_data.json")
DATA = json.load(open(_REPO if os.path.exists(_REPO) else "/tmp/pa_data.json"))
MODELS = ["GPT-5.5", "GPT-5.4-mini", "Gemini-3.1-Pro", "Gemini-3.1-Flash-Lite",
          "Claude-opus-4.8", "Claude-haiku-4.5", "DeepSeek-V4-Pro", "DeepSeek-V4-Flash"]

C_PRIMARY = "#2c7fb8"
C_AUG_FACE = "#a6cee3"
C_AUG_EDGE = "#08519c"

plt.rcParams.update({"font.family": "DejaVu Sans", "axes.linewidth": 1.0, "font.size": 10})

x = np.arange(len(MODELS))
w = 0.40

LEGEND = [
    Patch(facecolor=C_PRIMARY, edgecolor="black", label="Primary (original published values)"),
    Patch(facecolor=C_AUG_FACE, edgecolor=C_AUG_EDGE, hatch="////", label="Augmented (modified values)"),
]


def draw(ax, key, title=None, letter=None, ylab="Accuracy"):
    prim = [DATA[m]["primary"][key] for m in MODELS]
    aug = [DATA[m]["augmented"][key] for m in MODELS]
    ax.bar(x - w/2, prim, w, color=C_PRIMARY, edgecolor="black", linewidth=0.8, zorder=3)
    ax.bar(x + w/2, aug, w, facecolor=C_AUG_FACE, edgecolor=C_AUG_EDGE, linewidth=0.8,
           hatch="////", zorder=3)
    if title:
        ax.set_title(title, fontsize=12, fontweight="bold", loc="center", pad=8)
    if letter:
        ax.set_title(letter, fontsize=15, fontweight="bold", loc="left", pad=8)
    ax.set_ylabel(ylab, fontsize=11)
    ax.set_ylim(0, 1.05)
    ax.set_yticks(np.arange(0, 1.01, 0.2))
    ax.set_xticks(x)
    ax.set_xticklabels(MODELS, rotation=45, ha="right", fontsize=9)
    ax.yaxis.grid(True, color="#dddddd", linewidth=0.7, zorder=0)
    ax.set_axisbelow(True)


# ---- Figure 1: 4 per-section panels (2x2), labelled (A)-(D) ----
SECTIONS = [("SP", "Study period (SP)", "(A)"), ("TAR", "Time-at-risk (TAR)", "(B)"),
            ("PS", "Propensity score (PS)", "(C)"), ("OM", "Outcome model (OM)", "(D)")]
fig1, axes1 = plt.subplots(2, 2, figsize=(12, 9))
for ax, (k, t, lab) in zip(axes1.ravel(), SECTIONS):
    draw(ax, k, title=t, letter=lab)
fig1.legend(handles=LEGEND, loc="lower center", ncol=2, fontsize=12, frameon=False,
            handlelength=2.2, bbox_to_anchor=(0.5, -0.005))
fig1.tight_layout(rect=[0, 0.05, 1, 1])
fig1.savefig("figure_sections.pdf", bbox_inches="tight")
fig1.savefig("figure_sections.png", dpi=200, bbox_inches="tight")

# ---- Figure 2: single Overall panel (no in-image title) ----
fig2, ax2 = plt.subplots(figsize=(9, 5.5))
draw(ax2, "Overall")
fig2.legend(handles=LEGEND, loc="lower center", ncol=2, fontsize=11, frameon=False,
            handlelength=2.2, bbox_to_anchor=(0.5, -0.02))
fig2.tight_layout(rect=[0, 0.06, 1, 1])
fig2.savefig("figure_overall.pdf", bbox_inches="tight")
fig2.savefig("figure_overall.png", dpi=200, bbox_inches="tight")

print("wrote figure_sections.pdf/.png and figure_overall.pdf/.png")

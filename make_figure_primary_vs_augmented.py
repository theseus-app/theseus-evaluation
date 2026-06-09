"""
Figure: OHDSI primary vs. value-augmented protocols.
Shows that LLM extraction accuracy holds on modified (augmented) values,
arguing against the "models just memorized the published studies" explanation.
Style mirrors Figure 3 (grouped blue bars: solid = primary, hatched = augmented).
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

# (key, title, ylabel, ylim)
PANELS = [
    ("SP",          "Study period (SP)",        "Accuracy",       (0, 1.05)),
    ("TAR",         "Time-at-risk (TAR)",       "Accuracy",       (0, 1.05)),
    ("PS",          "Propensity score (PS)",    "Accuracy",       (0, 1.05)),
    ("OM",          "Outcome model (OM)",       "Accuracy",       (0, 1.05)),
    ("Overall",     "Overall accuracy (SP/TAR/PS/OM)", "Accuracy", (0, 1.05)),
    ("Precision",   "Precision (field-level)",  "Precision",      (0, 1.05)),
    ("Sensitivity", "Sensitivity (field-level)","Sensitivity",    (0, 1.05)),
    ("FPperStudy",  "False positives per study","FP per study",   None),
]

C_PRIMARY = "#2c7fb8"
C_AUG_FACE = "#a6cee3"
C_AUG_EDGE = "#08519c"

plt.rcParams.update({
    "font.family": "DejaVu Sans",
    "axes.linewidth": 1.0,
    "font.size": 10,
})

fig, axes = plt.subplots(2, 4, figsize=(17, 8.2))
axes = axes.ravel()
x = np.arange(len(MODELS))
w = 0.40

for ax, (key, title, ylab, ylim) in zip(axes, PANELS):
    prim = [DATA[m]["primary"][key] for m in MODELS]
    aug = [DATA[m]["augmented"][key] for m in MODELS]
    ax.bar(x - w/2, prim, w, color=C_PRIMARY, edgecolor="black", linewidth=0.8,
           label="Primary (original published values)", zorder=3)
    ax.bar(x + w/2, aug, w, facecolor=C_AUG_FACE, edgecolor=C_AUG_EDGE, linewidth=0.8,
           hatch="////", label="Augmented (modified values)", zorder=3)
    ax.set_title(title, fontsize=11, fontweight="bold", pad=8)
    ax.set_ylabel(ylab, fontsize=10)
    if ylim:
        ax.set_ylim(*ylim)
        ax.set_yticks(np.arange(0, 1.01, 0.2))
    else:
        top = max(max(prim), max(aug)) * 1.25 + 1e-9
        ax.set_ylim(0, max(top, 0.2))
    ax.set_xticks(x)
    ax.set_xticklabels(MODELS, rotation=45, ha="right", fontsize=8.5)
    ax.yaxis.grid(True, color="#dddddd", linewidth=0.7, zorder=0)
    ax.set_axisbelow(True)
    for s in ax.spines.values():
        s.set_visible(True)

legend_handles = [
    Patch(facecolor=C_PRIMARY, edgecolor="black", label="Primary (original published values)"),
    Patch(facecolor=C_AUG_FACE, edgecolor=C_AUG_EDGE, hatch="////", label="Augmented (modified values)"),
]
fig.legend(handles=legend_handles, loc="lower center", ncol=2, fontsize=12, frameon=False,
           handlelength=2.2, bbox_to_anchor=(0.5, -0.01))

fig.suptitle("Primary vs value-augmented protocols [15 OHDSI studies]",
             fontsize=15, fontweight="bold", y=0.995)
fig.tight_layout(rect=[0, 0.04, 1, 0.97])
fig.savefig("figure_primary_vs_augmented.pdf", bbox_inches="tight")
fig.savefig("figure_primary_vs_augmented.png", dpi=200, bbox_inches="tight")
print("wrote figure_primary_vs_augmented.pdf / .png")

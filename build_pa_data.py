"""
Rebuild pa_data.json (Primary vs value-Augmented, OHDSI) from public/results.

Source of truth:
  public/results/ohdsi/primary/<vendor>_<size>/_summary.index.json            -> "primary"
  public/results/ohdsi/primary_augmented/<vendor>_<size>/_summary.index.json  -> "augmented"

Each model entry carries the section accuracies (SP/TAR/PS/OM), the Overall =
mean(SP,TAR,PS,OM), the field-level Precision/Sensitivity/FPperStudy, and N.

Output is written to BOTH:
  ./pa_data.json    (committed, reproducible)
  /tmp/pa_data.json (consumed by make_figures_split.py / make_figure_primary_vs_augmented.py)

Run:  python3 build_pa_data.py
"""
import json
import os

ROOT = os.path.dirname(os.path.abspath(__file__))

# Display label -> (vendor_dir, size_dir). Order matches the figure scripts' MODELS list.
MODELS = [
    ("GPT-5.5",                "openai",   "flagship"),
    ("GPT-5.4-mini",           "openai",   "light"),
    ("Gemini-3.1-Pro",         "gemini",   "flagship"),
    ("Gemini-3.1-Flash-Lite",  "gemini",   "light"),
    ("Claude-opus-4.8",        "claude",   "flagship"),
    ("Claude-haiku-4.5",       "claude",   "light"),
    ("DeepSeek-V4-Pro",        "deepseek", "flagship"),
    ("DeepSeek-V4-Flash",      "deepseek", "light"),
]

# section key in _summary.index.json -> short figure key
SECTIONS = [
    ("studyPeriods",               "SP"),
    ("timeAtRisks",                "TAR"),
    ("propensityScoreAdjustment",  "PS"),
    ("outcomeModels",              "OM"),
]

CONDITIONS = [
    ("primary",   "primary"),            # figure key, results subdir
    ("augmented", "primary_augmented"),
]


def extract(idx_path):
    """Pull the figure fields out of one _summary.index.json."""
    d = json.load(open(idx_path))
    sec = d["sectionAccuracySummary"]
    fld = d["fieldLevelMetrics"]

    out = {}
    accs = []
    for long_key, short in SECTIONS:
        a = sec[long_key]["accuracy"]
        out[short] = a
        accs.append(a)

    # Overall = mean of the 4 section accuracies (same convention as results_summary.html).
    out["Overall"] = sum(accs) / len(accs)
    out["Precision"] = fld["fieldPrecision"]
    out["Sensitivity"] = fld["fieldSensitivity"]
    out["FPperStudy"] = fld["fieldFPperCase"]
    out["N"] = d["totalCases"]
    return out


def main():
    data = {}
    missing = []
    for label, vendor, size in MODELS:
        entry = {}
        for fig_key, subdir in CONDITIONS:
            idx = os.path.join(
                ROOT, "public", "results", "ohdsi", subdir,
                f"{vendor}_{size}", "_summary.index.json",
            )
            if not os.path.exists(idx):
                missing.append(idx)
                continue
            entry[fig_key] = extract(idx)
        data[label] = entry

    if missing:
        print("[WARN] missing summary files:")
        for m in missing:
            print("   ", m)

    repo_out = os.path.join(ROOT, "pa_data.json")
    tmp_out = "/tmp/pa_data.json"
    for path in (repo_out, tmp_out):
        with open(path, "w") as f:
            json.dump(data, f)
    print(f"wrote {repo_out}")
    print(f"wrote {tmp_out}")

    # quick sanity echo
    g = data["GPT-5.5"]["primary"]
    print(f"check GPT-5.5 primary: SP={g['SP']:.3f} TAR={g['TAR']:.3f} "
          f"PS={g['PS']:.3f} OM={g['OM']:.3f} Overall={g['Overall']:.4f} N={g['N']}")


if __name__ == "__main__":
    main()

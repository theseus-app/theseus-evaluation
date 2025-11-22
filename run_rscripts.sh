#!/usr/bin/env bash
set -uo pipefail

usage() {
  cat <<'USAGE'
Usage: run_rscripts.sh [--vendor=NAME] [--size=NAME] [--runner=R|Rscript]

Runs each R script inside public/RScripts_<VENDOR>_<SIZE> directories and checks
whether public/<script-base-name>/ was created after a successful run.
Logs results and generates summary.txt under /log/RScripts_<VENDOR>_<SIZE>.

Examples:
  ./run_rscripts.sh --vendor=OPENAI --size=FLAGSHIP
  ./run_rscripts.sh --vendor=OPENAI
  ./run_rscripts.sh --size=LIGHT
USAGE
}

vendor=""
size=""
runner="Rscript"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --vendor=*) vendor="${1#*=}" ;;
    --vendor) shift; vendor="${1:-}";;
    --size=*) size="${1#*=}" ;;
    --size) shift; size="${1:-}";;
    --runner=*) runner="${1#*=}" ;;
    --runner) shift; runner="${1:-}";;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
  shift
done

to_upper() { printf '%s' "$1" | tr '[:lower:]' '[:upper:]'; }
vendor_filt=$(to_upper "$vendor")
size_filt=$(to_upper "$size")

# RScripts_* 들은 public 밑에 있다고 가정
ROOT_DIR="public"

if [[ ! -d "$ROOT_DIR" ]]; then
  echo "Directory '$ROOT_DIR' not found; expected root for RScripts_* and outputs." >&2
  exit 1
fi

declare -a targets=()

collect_targets() {
  local pattern=$1
  while IFS= read -r line; do
    targets+=("$line")
  done < <(find "$ROOT_DIR" -maxdepth 1 -type d -name "$pattern" | sort)
}

# 대상 디렉토리 선택 로직
if [[ -n "$vendor_filt" && -n "$size_filt" ]]; then
  targets=("${ROOT_DIR}/RScripts_${vendor_filt}_${size_filt}")
elif [[ -n "$vendor_filt" ]]; then
  collect_targets "RScripts_${vendor_filt}_*"
elif [[ -n "$size_filt" ]]; then
  collect_targets "RScripts_*_${size_filt}"
else
  collect_targets "RScripts_*"
fi

if [[ ${#targets[@]} -eq 0 ]]; then
  echo "No matching public/RScripts_<VENDOR>_<SIZE> directories found." >&2
  exit 1
fi

overall_rc=0
success_count=0
fail_count=0
declare -a failed_scripts=()

for dir in "${targets[@]}"; do
  # dir 는 public/RScripts_... 형태
  dir_clean=$(basename "$dir")        # RScripts_OPENAI_FLAGSHIP
  log_dir="log/${dir_clean}"          # log/RScripts_OPENAI_FLAGSHIP
  mkdir -p "$log_dir"

  echo "==> Running scripts in $dir"
  echo "Log directory: $log_dir"

  scripts=()
  while IFS= read -r script_path; do
    scripts+=("$script_path")
  done < <(find "$dir" -maxdepth 1 -type f -name '*.R' | sort)

  if [[ ${#scripts[@]} -eq 0 ]]; then
    echo "  No .R files found in $dir"
    overall_rc=1
    continue
  fi

  for script in "${scripts[@]}"; do
    base=$(basename "$script" .R)
    # ✅ R 스크립트가 생성해야 하는 디렉토리: public/<base>
    expected_dir="${ROOT_DIR}/${base}"

    log_file="${log_dir}/${base}.log"

    echo "  → Running $(basename "$script") | log: $log_file"

    if [[ "$runner" == "R" ]]; then
      cmd_output=$(R --no-save --quiet --slave -f "$script" 2>&1)
      cmd_status=$?
    else
      cmd_output=$("$runner" "$script" 2>&1)
      cmd_status=$?
    fi

    echo "$cmd_output" > "$log_file"

    if [[ $cmd_status -eq 0 ]]; then
      if [[ -d "$expected_dir" ]]; then
        ((success_count++))
        echo "    ✔ Success: $base" | tee -a "$log_file"
      else
        ((fail_count++))
        failed_scripts+=("$base")
        echo "    ⚠ Script succeeded but $expected_dir not found" | tee -a "$log_file"
        overall_rc=1
      fi
    else
      ((fail_count++))
      failed_scripts+=("$base")
      echo "    ✖ Runner command exited with an error (runner: $runner)" | tee -a "$log_file"
      overall_rc=1
    fi
  done
done

total=$((success_count + fail_count))
accuracy=$(awk "BEGIN { if ($total > 0) printf \"%.2f\", ($success_count / $total) * 100; else print 0 }")

summary_dir="log/RScripts_${vendor_filt:-ALL}_${size_filt:-ALL}"
mkdir -p "$summary_dir"
summary_file="${summary_dir}/summary.txt"

{
  echo "===== R Script Execution Summary ====="
  echo "Vendor     : ${vendor_filt:-ALL}"
  echo "Size       : ${size_filt:-ALL}"
  echo "Runner     : ${runner}"
  echo "--------------------------------------"
  echo "Total scripts : $total"
  echo "Success count : $success_count"
  echo "Fail count    : $fail_count"
  echo "Accuracy      : ${accuracy}%"
  echo
  if [[ ${#failed_scripts[@]} -gt 0 ]]; then
    echo "❌ Failed Scripts:"
    for f in "${failed_scripts[@]}"; do
      echo "  - $f"
    done
  else
    echo "✅ All scripts executed successfully."
  fi
  echo "--------------------------------------"
  echo "Logs saved under: $summary_dir"
} | tee "$summary_file"

echo
echo "=== Summary ==="
cat "$summary_file"

exit $overall_rc

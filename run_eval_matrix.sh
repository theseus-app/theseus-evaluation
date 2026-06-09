#!/usr/bin/env bash
# 40-shell eval matrix: 30 non-Gemini in parallel + 10 Gemini sequential.
# Datasets×types (5): ohdsi{DEFAULT,PRIMARY,PRIMARY_AUGMENTED} + non-ohdsi{DEFAULT,PRIMARY}
# Models: OPENAI/CLAUDE/DEEPSEEK (parallel) + GEMINI (sequential) × FLAGSHIP/LIGHT
set -u
cd /Users/minseongkim/Desktop/youlab/theseus/theseus-evaluation

TS=$(date +%Y%m%d_%H%M%S)
LOGDIR="batch_logs_$TS"
mkdir -p "$LOGDIR"
echo "LOGDIR=$LOGDIR"

run_shell () {
  local gold="$1" vendor="$2" size="$3" type="$4" tag="$5"
  local log="$LOGDIR/${tag}.log"
  local start=$(date +%s)
  if [ -n "$gold" ]; then
    GOLD_STANDARD_DIR="$gold" npx tsx batchEvaluate.ts --vendor="$vendor" --size="$size" --type="$type" >"$log" 2>&1
  else
    npx tsx batchEvaluate.ts --vendor="$vendor" --size="$size" --type="$type" >"$log" 2>&1
  fi
  local code=$?
  local dur=$(( $(date +%s) - start ))
  echo "[done ${dur}s exit=${code}] $tag"
}

# "GOLD_DIR|datasetLabel|TYPE"  (empty GOLD = ohdsi default)
COMBOS=(
  "|ohdsi|DEFAULT"
  "|ohdsi|PRIMARY"
  "|ohdsi|PRIMARY_AUGMENTED"
  "public/goldStandardTest|non-ohdsi|DEFAULT"
  "public/goldStandardTest|non-ohdsi|PRIMARY"
)
NONGEM=(OPENAI CLAUDE DEEPSEEK)
SIZES=(FLAGSHIP LIGHT)

echo "== launching 30 non-Gemini shells in parallel =="
pids=()
for c in "${COMBOS[@]}"; do
  IFS='|' read -r gold ds type <<< "$c"
  for v in "${NONGEM[@]}"; do
    for s in "${SIZES[@]}"; do
      run_shell "$gold" "$v" "$s" "$type" "${ds}_${type}_${v}_${s}" &
      pids+=($!)
    done
  done
done

echo "== launching 10 Gemini shells sequentially (background chain) =="
(
  for c in "${COMBOS[@]}"; do
    IFS='|' read -r gold ds type <<< "$c"
    for s in "${SIZES[@]}"; do
      run_shell "$gold" GEMINI "$s" "$type" "${ds}_${type}_GEMINI_${s}"
    done
  done
) &
gem_pid=$!

for p in "${pids[@]}"; do wait "$p"; done
wait "$gem_pid"
echo "ALL DONE :: $LOGDIR"

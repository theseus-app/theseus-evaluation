#!/usr/bin/env bash
# Re-run incomplete shells: 9 ENOSPC-truncated non-Gemini (parallel) +
# 5 Gemini LIGHT (free key) + 5 Gemini FLAGSHIP (paid key via core's
# GOOGLE_API_KEY_PAID, auto-selected for size=FLAGSHIP).
# Light & flagship gemini run as two separate sequential chains (distinct quotas).
# Each target dir is wiped first so stale prior-run files don't masquerade as fresh.
# Usage: bash run_eval_rerun.sh dry | bash run_eval_rerun.sh
set -u
cd /Users/minseongkim/Desktop/youlab/theseus/theseus-evaluation
MODE="${1:-run}"
TSX="./node_modules/.bin/tsx"
TS=$(date +%Y%m%d_%H%M%S)
LOGDIR="rerun_logs_$TS"
[ "$MODE" = run ] && mkdir -p "$LOGDIR"
echo "MODE=$MODE LOGDIR=$LOGDIR TSX=$TSX"
[ "$MODE" = run ] && { [ -x "$TSX" ] || { echo "FATAL: $TSX 없음 (pnpm install 필요)"; exit 1; }; }

# "GOLD|datasetLabel|TYPE|VENDOR|SIZE"  (GOLD empty => ohdsi)
NONGEM=(
  "|ohdsi|DEFAULT|OPENAI|FLAGSHIP"
  "|ohdsi|PRIMARY_AUGMENTED|CLAUDE|FLAGSHIP"
  "|ohdsi|PRIMARY_AUGMENTED|DEEPSEEK|FLAGSHIP"
  "|ohdsi|PRIMARY_AUGMENTED|DEEPSEEK|LIGHT"
  "|ohdsi|PRIMARY_AUGMENTED|OPENAI|FLAGSHIP"
  "public/goldStandardTest|non-ohdsi|DEFAULT|DEEPSEEK|FLAGSHIP"
  "public/goldStandardTest|non-ohdsi|DEFAULT|OPENAI|FLAGSHIP"
  "public/goldStandardTest|non-ohdsi|PRIMARY|DEEPSEEK|FLAGSHIP"
  "public/goldStandardTest|non-ohdsi|PRIMARY|OPENAI|FLAGSHIP"
)
GEMLIGHT=(
  "|ohdsi|DEFAULT|GEMINI|LIGHT"
  "|ohdsi|PRIMARY|GEMINI|LIGHT"
  "|ohdsi|PRIMARY_AUGMENTED|GEMINI|LIGHT"
  "public/goldStandardTest|non-ohdsi|DEFAULT|GEMINI|LIGHT"
  "public/goldStandardTest|non-ohdsi|PRIMARY|GEMINI|LIGHT"
)
GEMFLAG=(
  "|ohdsi|DEFAULT|GEMINI|FLAGSHIP"
  "|ohdsi|PRIMARY|GEMINI|FLAGSHIP"
  "|ohdsi|PRIMARY_AUGMENTED|GEMINI|FLAGSHIP"
  "public/goldStandardTest|non-ohdsi|DEFAULT|GEMINI|FLAGSHIP"
  "public/goldStandardTest|non-ohdsi|PRIMARY|GEMINI|FLAGSHIP"
)

outdir () {
  local ds="$1" type="$2" v="$3" s="$4"
  echo "public/results/$ds/$(echo "$type"|tr A-Z a-z)/$(echo "$v"|tr A-Z a-z)_$(echo "$s"|tr A-Z a-z)"
}
run_one () {
  local gold="$1" ds="$2" type="$3" v="$4" s="$5"
  local tag="${ds}_${type}_${v}_${s}" dir; dir=$(outdir "$ds" "$type" "$v" "$s")
  if [ "$MODE" = dry ]; then echo "  [$tag] -> $dir (GOLD='${gold:-<ohdsi>}')"; return; fi
  rm -f "$dir"/*.json 2>/dev/null
  local log="$LOGDIR/${tag}.log" start; start=$(date +%s)
  if [ -n "$gold" ]; then
    GOLD_STANDARD_DIR="$gold" "$TSX" batchEvaluate.ts --vendor="$v" --size="$s" --type="$type" >"$log" 2>&1
  else
    "$TSX" batchEvaluate.ts --vendor="$v" --size="$s" --type="$type" >"$log" 2>&1
  fi
  echo "[done $(( $(date +%s)-start ))s exit=$? ok=$(grep -c '\[OK\]' "$log")] $tag"
}
chain () { for e in "$@"; do IFS='|' read -r g d t v s <<< "$e"; run_one "$g" "$d" "$t" "$v" "$s"; done; }

echo "== ${#NONGEM[@]} non-Gemini (parallel) + Gemini LIGHT(${#GEMLIGHT[@]}) + Gemini FLAGSHIP(${#GEMFLAG[@]}) chains =="
pids=()
for e in "${NONGEM[@]}"; do IFS='|' read -r g d t v s <<< "$e"; run_one "$g" "$d" "$t" "$v" "$s" & pids+=($!); done
( chain "${GEMLIGHT[@]}" ) & pids+=($!)
( chain "${GEMFLAG[@]}" ) & pids+=($!)
for p in "${pids[@]}"; do wait "$p"; done
echo "ALL DONE :: $LOGDIR"

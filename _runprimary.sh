#!/usr/bin/env bash
set -u
cd "$(dirname "$0")"
LOG=_primary_run.log
: > "$LOG"
VENDORS=(OPENAI GEMINI CLAUDE DEEPSEEK)
SIZES=(FLAGSHIP LIGHT)

run() {
  local type=$1 golddir=$2 v=$3 s=$4
  echo "=== $(date +%H:%M:%S) START type=$type vendor=$v size=$s ===" >>"$LOG"
  if [ -n "$golddir" ]; then
    GOLD_STANDARD_DIR="$golddir" npx tsx batchEvaluate.ts --vendor="$v" --size="$s" --type="$type" >>"$LOG" 2>&1
  else
    npx tsx batchEvaluate.ts --vendor="$v" --size="$s" --type="$type" >>"$LOG" 2>&1
  fi
  echo "=== $(date +%H:%M:%S) END   type=$type vendor=$v size=$s exit=$? ===" >>"$LOG"
}

for v in "${VENDORS[@]}"; do
  for s in "${SIZES[@]}"; do
    run PRIMARY           public/goldStandardTest "$v" "$s"
    run PRIMARY_AUGMENTED ""                      "$v" "$s"
  done
done
echo "=== $(date +%H:%M:%S) ALL DONE ===" >>"$LOG"

#!/usr/bin/env bash
# 40-run eval matrix: OHDSI (goldStandard) DEFAULT/PRIMARY/PRIMARY_AUGMENTED = 24,
# non-OHDSI (goldStandardTest) DEFAULT/PRIMARY = 16.
# Parallelism: one stream per (vendor,size) = 8 concurrent; each stream runs its 5 jobs sequentially.
set -u
cd "$(dirname "$0")"
LOGDIR=_run40_logs
mkdir -p "$LOGDIR"
VENDORS=(OPENAI GEMINI CLAUDE DEEPSEEK)
SIZES=(FLAGSHIP LIGHT)

run() {
  local type=$1 golddir=$2 v=$3 s=$4 log=$5
  echo "=== $(date +%H:%M:%S) START type=$type vendor=$v size=$s gold=$golddir ===" >>"$log"
  GOLD_STANDARD_DIR="$golddir" npx tsx batchEvaluate.ts --vendor="$v" --size="$s" --type="$type" >>"$log" 2>&1
  echo "=== $(date +%H:%M:%S) END   type=$type vendor=$v size=$s exit=$? ===" >>"$log"
}

stream() {
  local v=$1 s=$2
  local log="$LOGDIR/${v}_${s}.log"
  : > "$log"
  # OHDSI -> public/goldStandard  (results/<type>/...)
  run DEFAULT           public/goldStandard     "$v" "$s" "$log"
  run PRIMARY           public/goldStandard     "$v" "$s" "$log"
  run PRIMARY_AUGMENTED public/goldStandard     "$v" "$s" "$log"
  # non-OHDSI -> public/goldStandardTest  (results/test/<type>/...)
  run DEFAULT           public/goldStandardTest "$v" "$s" "$log"
  run PRIMARY           public/goldStandardTest "$v" "$s" "$log"
  echo "=== $(date +%H:%M:%S) STREAM ${v}_${s} DONE ===" >>"$log"
}

echo "START $(date)"
for v in "${VENDORS[@]}"; do
  for s in "${SIZES[@]}"; do
    stream "$v" "$s" &
  done
done
wait
echo "ALL DONE $(date)"

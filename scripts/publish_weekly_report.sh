#!/usr/bin/env bash
# Generate weekly report and publish only the report block to reports/week_WW_YYYY.txt
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT_DIR"

mkdir -p reports
TMP_REPORT="reports/_tmp_weekly_report.txt"

# Run the R script and capture output
Rscript weekly-report.r > "$TMP_REPORT" 2>&1

# Find the last block that starts with a line of === (the weekly report header)
START_LINE=$(awk '/^=+$/ {line=NR} END{print line+0}' "$TMP_REPORT")
if [ -z "$START_LINE" ] || [ "$START_LINE" -eq 0 ]; then
  echo "Unable to locate report header in $TMP_REPORT" >&2
  exit 1
fi

# Extract from that line to EOF (assumes the weekly report is the final block)
TMP_BLOCK="reports/_tmp_weekly_block.txt"
sed -n "${START_LINE},$p" "$TMP_REPORT" > "$TMP_BLOCK"

# Parse week and year from the report header line (e.g. "ISO Week 3 (Jan 12 - Jan 18, 2026)")
ISO_LINE=$(grep -m1 "ISO Week" "$TMP_BLOCK" || true)
if [ -z "$ISO_LINE" ]; then
  echo "Could not find ISO Week line in report block" >&2
  exit 1
fi

# Extract week number and year
WEEK=$(echo "$ISO_LINE" | sed -E 's/.*ISO Week *([0-9]{1,2}).*/\1/')
YEAR=$(echo "$ISO_LINE" | sed -E 's/.*\, *([0-9]{4})\).*/\1/')

if [ -z "$WEEK" ] || [ -z "$YEAR" ]; then
  echo "Failed to parse week/year from: $ISO_LINE" >&2
  exit 1
fi

OUT_FILE="reports/week_${WEEK}_${YEAR}.txt"
mv "$TMP_BLOCK" "$OUT_FILE"
rm -f "$TMP_REPORT"

echo "Published weekly report to $OUT_FILE"
exit 0

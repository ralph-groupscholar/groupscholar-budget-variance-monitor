#!/bin/sh
set -eu

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
OUT_FILE="$ROOT_DIR/out/test_output.txt"

cd "$ROOT_DIR"
./bin/gs-budget-variance-monitor data/sample_awards.csv config/default.cfg > "$OUT_FILE"

diff -u "$ROOT_DIR/tests/expected.txt" "$OUT_FILE"

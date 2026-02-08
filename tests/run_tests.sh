#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT_DIR"

make build >/dev/null

bin/gs-budget-variance-monitor data/sample_awards.csv config/default.cfg > tests/out.txt

diff -u tests/expected.txt tests/out.txt

./tests/run_alerts.sh

echo "All tests passed."

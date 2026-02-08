#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT_DIR"

make build >/dev/null

bin/gs-budget-variance-monitor data/sample_awards_alerts.csv config/alerts.cfg > tests/out_alerts.txt

diff -u tests/expected_alerts.txt tests/out_alerts.txt

echo "Alert scenario test: OK"

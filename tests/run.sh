#!/bin/sh
set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

./bin/gs-budget-variance-monitor data/sample_awards.csv config/default.cfg > tests/out.txt

diff -u tests/expected.txt tests/out.txt

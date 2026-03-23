#!/bin/bash
# Run verification script
cd "$(dirname "$0")/.."
python src/run_checks.py "$@"

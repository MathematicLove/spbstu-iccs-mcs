#!/bin/bash
# Connect to test server (localhost:9001, plaintext)
# Usage: ./connect_test.sh [-u USER] [-p PASSWORD]
cd "$(dirname "$0")/.."
python src/fortest/client.py "$@"

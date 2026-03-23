#!/bin/bash
# Connect to main server (localhost:9000, encrypted)
# Usage: ./connect.sh [-u USER] [-p PASSWORD]
cd "$(dirname "$0")/.."
python src/client.py -e --key-file confdata/key.txt "$@"

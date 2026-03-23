#!/bin/bash
# Main server with encryption (ChaCha20), port 9000
cd "$(dirname "$0")/.."
python src/server.py -e --key-file confdata/key.txt

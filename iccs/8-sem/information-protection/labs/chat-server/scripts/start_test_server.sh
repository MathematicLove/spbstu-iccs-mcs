#!/bin/bash
# Test server without encryption (plaintext), port 9001
cd "$(dirname "$0")/.."
python src/fortest/server.py

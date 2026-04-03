#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p bin
mpicc -O3 -std=c99 -lm -o bin/cofactor_mpi src/cofactor_mpi.c
echo "Built bin/cofactor_mpi"

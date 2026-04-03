#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
CUDA_ARCH="${CUDA_ARCH:-sm_35}"
mkdir -p bin
nvcc -ccbin g++ -O3 -arch="$CUDA_ARCH" -o bin/cofactor_cuda src/cofactor_cuda.cu
echo "Built bin/cofactor_cuda (arch=$CUDA_ARCH)"

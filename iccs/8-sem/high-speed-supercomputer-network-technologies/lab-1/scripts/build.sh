#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

mkdir -p bin results

CUDA_ARCH="${CUDA_ARCH:-sm_35}"

module purge
module load compiler/gcc/11
module load nvidia/cuda/11.6u2

nvcc \
    -ccbin g++ \
    -O3 \
    -std=c++14 \
    -lineinfo \
    -Wno-deprecated-gpu-targets \
    -arch="${CUDA_ARCH}" \
    -o bin/linpack_cuda \
    src/main.cu

echo "Built: $ROOT_DIR/bin/linpack_cuda"

#!/usr/bin/env bash
#SBATCH --job-name=task1-cuda
#SBATCH --partition=tornado-k40
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:20:00
#SBATCH --output=results/%x-%j.out
#SBATCH --error=results/%x-%j.err

set -euo pipefail

cd "${SLURM_SUBMIT_DIR}"
ROOT_DIR="${SLURM_SUBMIT_DIR}"

module purge
module load compiler/gcc/11
module load nvidia/cuda/11.6u2

mkdir -p results bin

./scripts/build.sh

echo "===== account info ====="
whoami
hostname
date

echo
echo "===== slurm info ====="
echo "SLURM_JOB_ID=${SLURM_JOB_ID:-unknown}"
echo "SLURM_JOB_NAME=${SLURM_JOB_NAME:-unknown}"
echo "SLURM_JOB_PARTITION=${SLURM_JOB_PARTITION:-unknown}"
echo "SLURM_JOB_NUM_NODES=${SLURM_JOB_NUM_NODES:-unknown}"
echo "SLURM_NODELIST=${SLURM_NODELIST:-unknown}"
echo "CUDA_VISIBLE_DEVICES=${CUDA_VISIBLE_DEVICES:-unset}"
scontrol show job "${SLURM_JOB_ID}" || true

echo
echo "===== node config ====="
lscpu | sed -n '1,20p'
if [ -n "${SLURMD_NODENAME:-}" ]; then
    scontrol show node "${SLURMD_NODENAME}" || true
fi
nvidia-smi -L || true
nvidia-smi || true

echo
echo "===== benchmark ====="
./bin/linpack_cuda \
    --start 1000 \
    --step 500 \
    --count 6 \
    --eps 1e-6 \
    --max-iters 15000 \
    --threads 256 \
    --repeat 3 \
    --warmup 1 \
    --csv "results/task1-cuda-${SLURM_JOB_ID}.csv"

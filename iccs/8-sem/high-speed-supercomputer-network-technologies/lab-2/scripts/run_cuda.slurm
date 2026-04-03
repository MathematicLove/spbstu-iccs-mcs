#!/usr/bin/env bash
#SBATCH --job-name=task2-cofactor-cuda
#SBATCH --partition=tornado-k40
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --output=results/%x-%j.out
#SBATCH --error=results/%x-%j.err

set -euo pipefail

cd "${SLURM_SUBMIT_DIR}"

module purge
module load compiler/gcc/11
module load nvidia/cuda/11.6u2

mkdir -p results bin

./scripts/build_cuda.sh

echo "===== account info ====="
whoami; hostname; date

echo
echo "===== slurm info ====="
echo "SLURM_JOB_ID=${SLURM_JOB_ID:-unknown}"
echo "SLURM_JOB_PARTITION=${SLURM_JOB_PARTITION:-unknown}"
echo "SLURM_NODELIST=${SLURM_NODELIST:-unknown}"
scontrol show job "${SLURM_JOB_ID}" || true

echo
echo "===== node config ====="
lscpu | head -20
nvidia-smi -L || true
nvidia-smi || true

CSV="results/task2-cuda-${SLURM_JOB_ID}.csv"
echo "n,impl,time_ms,verify_err,rel_err" > "$CSV"

echo
echo "===== benchmark ====="
for N in 10 20 30 50 70 100; do
    echo "--- n=$N ---"
    ./bin/cofactor_cuda "$N" 256 "$CSV"
done

echo
echo "===== done ====="

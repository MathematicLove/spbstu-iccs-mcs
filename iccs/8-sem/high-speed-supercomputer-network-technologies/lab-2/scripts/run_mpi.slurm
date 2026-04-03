#!/usr/bin/env bash
#SBATCH --job-name=task2-cofactor-mpi
#SBATCH --partition=tornado
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=56
#SBATCH --time=00:30:00
#SBATCH --output=results/%x-%j.out
#SBATCH --error=results/%x-%j.err

set -euo pipefail

cd "${SLURM_SUBMIT_DIR}"

module purge
module load compiler/gcc/11
module load mpi/openmpi

mkdir -p results bin

./scripts/build_mpi.sh

RANKS=${SLURM_JOB_NUM_NODES}

echo "===== account info ====="
whoami; hostname; date

echo
echo "===== slurm info ====="
echo "SLURM_JOB_ID=${SLURM_JOB_ID:-unknown}"
echo "SLURM_JOB_PARTITION=${SLURM_JOB_PARTITION:-unknown}"
echo "SLURM_JOB_NUM_NODES=${SLURM_JOB_NUM_NODES:-unknown}"
echo "SLURM_NODELIST=${SLURM_NODELIST:-unknown}"
echo "RANKS=${RANKS}"
scontrol show job "${SLURM_JOB_ID}" || true

echo
echo "===== node config ====="
lscpu | head -20
if [ -n "${SLURMD_NODENAME:-}" ]; then
    scontrol show node "${SLURMD_NODENAME}" || true
fi

CSV="results/task2-mpi-${RANKS}n-${SLURM_JOB_ID}.csv"
echo "n,procs,time_ms,verify_err,rel_err" > "$CSV"

echo
echo "===== benchmark (${RANKS} nodes / ${RANKS} ranks) ====="
for N in 10 20 30 50 70 100; do
    echo "--- n=$N ---"
    mpirun -np "${RANKS}" --map-by ppr:1:node --bind-to none \
        ./bin/cofactor_mpi "$N" "$CSV"
done

echo
echo "===== done ====="

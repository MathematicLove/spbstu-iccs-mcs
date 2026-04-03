#!/usr/bin/env bash
#SBATCH --job-name=task1-intel-linpack
#SBATCH --partition=tornado
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=56
#SBATCH --time=00:20:00
#SBATCH --output=stdio/%x-%j.out
#SBATCH --error=stdio/%x-%j.err

set -euo pipefail

TASK1_DIR="${SLURM_SUBMIT_DIR:-$PWD}"
module purge

LINPACK_DIR="${LINPACK_DIR:-$HOME/LINPACK}"
LINPACK_INPUT="${LINPACK_INPUT:-$TASK1_DIR/intel/lininput_report_xeon64}"

if [ ! -d "${LINPACK_DIR}" ]; then
    echo "LINPACK directory not found: ${LINPACK_DIR}"
    echo "Prepare Intel LINPACK in your home directory first."
    echo "Example:"
    echo "  mkdir -p \$HOME/LINPACK"
    echo "  tar -xzf \$HOME/linpack.tgz -C \$HOME/LINPACK"
    exit 1
fi

resolve_linpack_dir() {
    if [ -x "${LINPACK_DIR}/xlinpack_xeon64" ]; then
        printf '%s\n' "${LINPACK_DIR}"
        return 0
    fi

    local found
    found="$(find "${LINPACK_DIR}" -name xlinpack_xeon64 2>/dev/null | head -n 1 || true)"
    if [ -n "${found}" ]; then
        dirname "${found}"
        return 0
    fi

    return 1
}

if ! LINPACK_DIR="$(resolve_linpack_dir)"; then
    echo "Intel LINPACK binary not found under: ${LINPACK_DIR}"
    echo "Check archive contents with:"
    echo "  find ${LINPACK_DIR} -name xlinpack_xeon64 2>/dev/null"
    exit 1
fi

cd "${LINPACK_DIR}"

chmod +x ./* || true
chmod -x ./*.* || true
mkdir -p stdio

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
echo "OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-56}"
scontrol show job "${SLURM_JOB_ID}" || true

echo
echo "===== node config ====="
lscpu | sed -n '1,20p'
if [ -n "${SLURMD_NODENAME:-}" ]; then
    scontrol show node "${SLURMD_NODENAME}" || true
fi

echo
echo "===== intel linpack ====="
echo "LINPACK_DIR=${LINPACK_DIR}"
echo "LINPACK_INPUT=${LINPACK_INPUT}"
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK:-56}"
export MKL_NUM_THREADS="${SLURM_CPUS_PER_TASK:-56}"

srun ./xlinpack_xeon64 "${LINPACK_INPUT}"

#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
BIN="${ROOT_DIR}/build/process"
HOSTS_FILE="${ROOT_DIR}/hosts.txt"
MPI_PROCS=${MPI_PROCS:-4}
LOG_DIR=${LOG_DIR:-${ROOT_DIR}/logs}
mkdir -p "${LOG_DIR}"

export BATCH_SIZE=${BATCH_SIZE:-20000}
export GPU_FRACTION=${GPU_FRACTION:-0.8}
export WORKER_THREADS=${WORKER_THREADS:-8}
export LOG_DIR

echo "Starting mpirun with:"
echo "  MPI_PROCS=${MPI_PROCS}"
echo "  BATCH_SIZE=${BATCH_SIZE}"
echo "  GPU_FRACTION=${GPU_FRACTION}"
echo "  WORKER_THREADS=${WORKER_THREADS}"
echo "  LOG_DIR=${LOG_DIR}"

if [ ! -f "${BIN}" ]; then
  echo "Binary ${BIN} not found. Run 'make' first."
  exit 1
fi

if [ -f "${HOSTS_FILE}" ]; then
  mpirun -np ${MPI_PROCS} --hostfile "${HOSTS_FILE}" "${BIN}"
else
  mpirun -np ${MPI_PROCS} "${BIN}"
fi

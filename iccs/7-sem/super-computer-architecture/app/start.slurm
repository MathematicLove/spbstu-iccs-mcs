#!/bin/bash
#SBATCH --job-name=credit_mpi_gpu
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --gres=gpu:1
#SBATCH --time=02:00:00
#SBATCH --output=slurm-%j.out
#SBATCH --error=slurm-%j.err

export BATCH_SIZE=${BATCH_SIZE:-20000}
export GPU_FRACTION=${GPU_FRACTION:-0.8}
export WORKER_THREADS=${WORKER_THREADS:-8}
export LOG_DIR=${LOG_DIR:-./logs}

srun --mpi=pmix_v3 $(pwd)/run.sh

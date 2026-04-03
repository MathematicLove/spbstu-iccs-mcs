#!/usr/bin/env python3
"""
Строит график зависимости времени вычисления матрицы алгебраических
дополнений от размера матрицы для MPI (1, 2, 4 узла) и CUDA.

Использование:
    python3 plot_task2_results.py \
        --mpi1 results/task2-mpi-1n-XXXXX.csv \
        --mpi2 results/task2-mpi-2n-XXXXX.csv \
        --mpi4 results/task2-mpi-4n-XXXXX.csv \
        --cuda results/task2-cuda-XXXXX.csv \
        -o report/img/task2-time-comparison.png
"""
import argparse
import csv
from pathlib import Path

import matplotlib.pyplot as plt


def read_mpi_csv(path: str) -> tuple[list[int], list[float]]:
    sizes, times = [], []
    with open(path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            sizes.append(int(row["n"]))
            times.append(float(row["time_ms"]))
    return sizes, times


def read_cuda_csv(path: str) -> tuple[list[int], list[float]]:
    sizes, times = [], []
    with open(path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            sizes.append(int(row["n"]))
            times.append(float(row["time_ms"]))
    return sizes, times


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mpi1", required=True, help="CSV for MPI 1 node")
    parser.add_argument("--mpi2", required=True, help="CSV for MPI 2 nodes")
    parser.add_argument("--mpi4", required=True, help="CSV for MPI 4 nodes")
    parser.add_argument("--cuda", required=True, help="CSV for CUDA")
    parser.add_argument("-o", "--output",
                        default="task2-time-comparison.png")
    args = parser.parse_args()

    plt.style.use("seaborn-v0_8-whitegrid")
    fig, ax = plt.subplots(figsize=(10, 6))

    for label, path in [
        ("MPI 1 node", args.mpi1),
        ("MPI 2 nodes", args.mpi2),
        ("MPI 4 nodes", args.mpi4),
        ("CUDA", args.cuda),
    ]:
        reader = read_mpi_csv if "mpi" in label.lower() else read_cuda_csv
        sizes, times = reader(path)
        ax.plot(sizes, times, marker="o", linewidth=2.2, label=label)

    ax.set_xlabel("Размер матрицы n")
    ax.set_ylabel("Время, мс")
    ax.set_title("Зависимость времени вычисления матрицы дополнений от n")
    ax.legend()
    ax.grid(True, alpha=0.3)

    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.output, dpi=150, bbox_inches="tight")
    print(f"Saved: {args.output}")


if __name__ == "__main__":
    main()

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt


N_VALUES = [1000, 1500, 2000, 2500, 3000, 3500]

CUDA_TIME_MS = [5.0083, 7.4931, 8.3563, 10.4837, 12.6709, 14.8861]
INTEL_TIME_S = [0.010, 0.020, 0.028, 0.042, 0.069, 0.100]
INTEL_TIME_MS = [value * 1000.0 for value in INTEL_TIME_S]

CUDA_GFLOPS = [133.114, 300.277, 638.244, 993.608, 1420.573, 1920.138]
INTEL_GFLOPS = [67.331, 114.360, 193.276, 250.731, 260.451, 286.264]


def configure_plot() -> None:
    plt.style.use("seaborn-v0_8-whitegrid")
    plt.rcParams["figure.figsize"] = (8.4, 5.2)
    plt.rcParams["figure.dpi"] = 140
    plt.rcParams["savefig.dpi"] = 220
    plt.rcParams["font.size"] = 11
    plt.rcParams["axes.labelsize"] = 11
    plt.rcParams["axes.titlesize"] = 12
    plt.rcParams["legend.fontsize"] = 10


def plot_time(output_path: Path) -> None:
    plt.figure()
    plt.plot(
        N_VALUES,
        INTEL_TIME_MS,
        marker="o",
        linewidth=2.2,
        color="#1f77b4",
        label="Intel LINPACK",
    )
    plt.plot(
        N_VALUES,
        CUDA_TIME_MS,
        marker="s",
        linewidth=2.2,
        color="#d62728",
        label="Собственная реализация",
    )
    plt.xlabel("Размер матрицы N")
    plt.ylabel("Время решения, мс")
    plt.legend(loc="upper left")
    plt.tight_layout()
    plt.savefig(output_path, bbox_inches="tight")
    plt.close()


def plot_gflops(output_path: Path) -> None:
    plt.figure()
    plt.plot(
        N_VALUES,
        INTEL_GFLOPS,
        marker="o",
        linewidth=2.2,
        color="#1f77b4",
        label="Intel LINPACK",
    )
    plt.plot(
        N_VALUES,
        CUDA_GFLOPS,
        marker="s",
        linewidth=2.2,
        color="#d62728",
        label="Собственная реализация",
    )
    plt.xlabel("Размер матрицы N")
    plt.ylabel("Производительность, GFLOPS")
    plt.legend(loc="upper left")
    plt.tight_layout()
    plt.savefig(output_path, bbox_inches="tight")
    plt.close()


def main() -> None:
    configure_plot()

    output_dir = Path(__file__).resolve().parents[2] / "report" / "img"
    output_dir.mkdir(parents=True, exist_ok=True)

    plot_time(output_dir / "task1-time-comparison.png")
    plot_gflops(output_dir / "task1-gflops-comparison.png")


if __name__ == "__main__":
    main()

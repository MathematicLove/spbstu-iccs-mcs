import subprocess
import re
import sys

try:
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
except ImportError:
    print("Установите matplotlib: pip install matplotlib")
    sys.exit(1)

RUN_CRACK = "/Users/practice2/bin/run_crack"


# Run external run_crack for given login and length, parse output.
# Returns (attempts, time_sec) or (None, None) on error/timeout.
def run_crack(login: str, length: int, timeout_sec: int | None = None):
    if timeout_sec is None:
        timeout_sec = 8 * 3600 + 60
    try:
        out = subprocess.run(
            [RUN_CRACK, login, str(length)],
            capture_output=True,
            text=True,
            timeout=timeout_sec,
            cwd=None,
        )
        text = out.stdout + "\n" + out.stderr
    except FileNotFoundError:
        print(f"Не найден {RUN_CRACK}. Запустите скрипт там, где установлен run_crack.", file=sys.stderr)
        return None, None
    except subprocess.TimeoutExpired:
        # Timeout reached: assume it ran timeout_sec seconds
        return None, float(timeout_sec)

    attempts = None
    time_sec = None

    # Count of attempts: 123 or Not found with 456 attempts
    m = re.search(r"Count of attempts:\s*(\d+)", text)
    if m:
        attempts = int(m.group(1))
    else:
        m = re.search(r"Not found with\s+(\d+)\s+attempts", text)
        if m:
            attempts = int(m.group(1))

    # Time spended: 12.34 s
    m = re.search(r"Time spended:\s*([\d.]+)\s*s", text)
    if m:
        time_sec = float(m.group(1))

    return attempts, time_sec


def main():
    # Series 1: by password lengths — u1..u5 with lengths 1..5
    series1 = [
        ("u1", 1),
        ("u2", 2),
        ("u3", 3),
        ("u4", 4),
    ]
    labels1 = [f"u{k}" for k in range(1, 5)]
    results1 = []
    for login, length in series1:
        print(f"Запуск crack {login} {length}...", flush=True)
        a, t = run_crack(login, length)
        results1.append((a, t))
        if a is not None and t is not None:
            print(f"  итераций: {a}, время: {t:.2f} с")
        else:
            print("  не удалось получить данные")

    # Plot 1: comparison by password lengths
    x1 = list(range(1, 5))
    iter1 = [r[0] if r[0] is not None else 0 for r in results1]
    time1 = [r[1] if r[1] is not None else 0.0 for r in results1]

    fig1, ax1 = plt.subplots(figsize=(8, 5))
    ax1.set_xlabel("Длина пароля")
    ax1.set_ylabel("Количество итераций", color="tab:blue")
    ax1.set_title("Сравнение по длинам пароля")
    ax1.bar([i - 0.2 for i in x1], iter1, width=0.35, label="Итерации", color="tab:blue", align="center")
    ax1.tick_params(axis="y", labelcolor="tab:blue")
    ax1.set_xticks(x1)
    ax1.set_xticklabels(labels1)

    ax2 = ax1.twinx()
    ax2.set_ylabel("Время, с", color="tab:orange")
    ax2.plot(x1, time1, "o-", color="tab:orange", linewidth=2, markersize=8, label="Время")
    ax2.tick_params(axis="y", labelcolor="tab:orange")

    fig1.legend(loc="upper left", bbox_to_anchor=(0.1, 0.9))
    fig1.tight_layout()
    fig1.savefig("crack_by_length.png", dpi=150, bbox_inches="tight")
    plt.close(fig1)
    print("Сохранён график: crack_by_length.png")

    # Series 2: by password values — abc3, u8, onlychar, sym
    series2 = [
        ("abc3", 3),
        ("onlychar", 4),
        ("sym", 4),
    ]
    labels2 = ["abc3", "onlychar", "sym"]
    results2 = []
    for login, length in series2:
        print(f"Запуск crack {login} {length}...", flush=True)
        a, t = run_crack(login, length)
        results2.append((a, t))
        if a is not None and t is not None:
            print(f"  итераций: {a}, время: {t:.2f} с")
        else:
            print("  не удалось получить данные")

    # Plot 2: comparison by password values
    x2 = list(range(len(labels2)))
    iter2 = [r[0] if r[0] is not None else 0 for r in results2]
    time2 = [r[1] if r[1] is not None else 0.0 for r in results2]

    fig2, ax1 = plt.subplots(figsize=(8, 5))
    ax1.set_xlabel("Пароль (пользователь)")
    ax1.set_ylabel("Количество итераций", color="tab:blue")
    ax1.set_title("Сравнение по значениям пароля")
    ax1.bar([i - 0.2 for i in x2], iter2, width=0.35, label="Итерации", color="tab:blue", align="center")
    ax1.tick_params(axis="y", labelcolor="tab:blue")
    ax1.set_xticks(x2)
    ax1.set_xticklabels(labels2, rotation=15, ha="right")

    ax2 = ax1.twinx()
    ax2.set_ylabel("Время, с", color="tab:orange")
    ax2.plot(x2, time2, "o-", color="tab:orange", linewidth=2, markersize=8, label="Время")
    ax2.tick_params(axis="y", labelcolor="tab:orange")

    fig2.legend(loc="upper left", bbox_to_anchor=(0.1, 0.9))
    fig2.tight_layout()
    fig2.savefig("crack_by_password.png", dpi=150, bbox_inches="tight")
    plt.close(fig2)
    print("Сохранён график: crack_by_password.png")


if __name__ == "__main__":
    main()

import subprocess
import sys
import platform
import time
from pathlib import Path

def run_tcpdump(interface: str = None, count: int = 20) -> str:
    cmd = ["tcpdump", "-c", str(count), "-n"]
    if interface:
        cmd.extend(["-i", interface])
    try:
        r = subprocess.run(cmd, capture_output=True, text=True, timeout=count + 5)
        return r.stdout + r.stderr
    except FileNotFoundError:
        return "tcpdump not found."
    except subprocess.TimeoutExpired:
        return "Timeout"
    except Exception as e:
        return str(e)


def run_tcpdump_live(interface: str = None, duration: int = 10):
    cmd = ["tcpdump", "-n", "-l"]
    if interface:
        cmd.extend(["-i", interface])
    try:
        proc = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
        )
        start = time.time()
        while time.time() - start < duration and proc.poll() is None:
            line = proc.stdout.readline()
            if line:
                print(line, end="")
        proc.terminate()
        proc.wait(timeout=2)
    except FileNotFoundError:
        print("tcpdump not found.")
    except KeyboardInterrupt:
        proc.terminate()


def get_default_interface() -> str:
    if platform.system() == "Darwin":
        r = subprocess.run(
            ["route", "-n", "get", "default"],
            capture_output=True,
            text=True,
        )
        for line in (r.stdout or "").split("\n"):
            if "interface:" in line:
                return line.split(":")[-1].strip()
        return "en0"
    r = subprocess.run(
        ["ip", "route", "show", "default"],
        capture_output=True,
        text=True,
    )
    parts = (r.stdout or "").split()
    for i, p in enumerate(parts):
        if p == "dev" and i + 1 < len(parts):
            return parts[i + 1]
    return "eth0"


def main():
    import argparse
    parser = argparse.ArgumentParser(description="Monitoring packets tcpdump")
    parser.add_argument("-i", "--interface", help="Network interface")
    parser.add_argument("-c", "--count", type=int, default=20, help="Number of packets")
    parser.add_argument("-t", "--time", type=int, default=0, help="Time in seconds (0 = count)")
    args = parser.parse_args()

    iface = args.interface or get_default_interface()
    print(f"Interface: {iface}")

    if args.time > 0:
        print(f"Capture on {args.time} sec. (Ctrl+C to stop)")
        run_tcpdump_live(iface, args.time)
    else:
        print(f"Capture {args.count} packets...")
        out = run_tcpdump(iface, args.count)
        print(out)


if __name__ == "__main__":
    main()

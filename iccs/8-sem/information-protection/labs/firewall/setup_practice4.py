import os
import sys
import subprocess
from pathlib import Path

ROOT = Path(__file__).parent
sys.path.insert(0, str(ROOT))
try:
    from config import ALLOWED_PING_FROM_IP
except ImportError:
    ALLOWED_PING_FROM_IP = os.environ.get("ALLOWED_PING_FROM_IP", "")

ANCHOR_FILE = "/etc/pf.anchors/practice4"
PF_CONF = "/etc/pf.conf"
ANCHOR_MARKER = 'anchor "practice4"'
LOAD_MARKER = 'load anchor "practice4" from "/etc/pf.anchors/practice4"'

from firewall_rules import get_rules


def ensure_anchor_in_pf_conf() -> bool:
    try:
        content = Path(PF_CONF).read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        print(f"Error reading {PF_CONF}: {e}")
        return False

    if ANCHOR_MARKER in content and LOAD_MARKER in content:
        return True

    try:
        with open(PF_CONF, "a", encoding="utf-8") as f:
            f.write("\n# practice4 anchor\n")
            f.write(ANCHOR_MARKER + "\n")
            f.write(LOAD_MARKER + "\n")
        print(f"Anchor practice4 added in {PF_CONF}")
        return True
    except OSError as e:
        print(f"Error writing {PF_CONF}: {e}")
        return False


def write_anchor(rules: str) -> bool:
    try:
        Path(ANCHOR_FILE).write_text(rules, encoding="utf-8")
        return True
    except OSError as e:
        print(f"Error writing {ANCHOR_FILE}: {e}")
        print("Run with sudo.")
        return False


def run_pfctl(args: list[str]) -> tuple[int, str]:
    try:
        r = subprocess.run(["pfctl"] + args, capture_output=True, text=True, timeout=10)
        return r.returncode, (r.stdout or "") + (r.stderr or "")
    except Exception as e:
        return -1, str(e)


def main() -> int:
    if os.geteuid() != 0:
        print("Run with sudo: sudo python3 setup_practice4.py")
        return 1

    if sys.platform != "darwin":
        print("Error")
        return 1

    if not ensure_anchor_in_pf_conf():
        return 1

    if not write_anchor(get_rules()):
        return 1

    code, out = run_pfctl(["-nf", PF_CONF])
    if code != 0:
        print("Error syntax")
        print(out)
        return 1

    run_pfctl(["-e"])

    code, out = run_pfctl(["-F", "all", "-q", "-f", PF_CONF])
    if code != 0:
        print("Error loading pf")
        print(out)
        return 1

    print("Rules applied. ALLOWED_PING_FROM_IP =", ALLOWED_PING_FROM_IP)
    return 0


if __name__ == "__main__":
    sys.exit(main())

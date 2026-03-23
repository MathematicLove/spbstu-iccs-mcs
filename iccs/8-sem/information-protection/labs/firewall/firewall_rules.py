import os
import sys
import subprocess
from pathlib import Path

ROOT = Path(__file__).parent
sys.path.insert(0, str(ROOT))
try:
    from config import ALLOWED_PING_FROM_IP
except ImportError:
    ALLOWED_PING_FROM_IP = os.environ.get("ALLOWED_PING_FROM_IP", "192.168.1.100")

ANCHOR_FILE = "/etc/pf.anchors/practice4"
PF_CONF = "/etc/pf.conf"

def get_rules() -> str:
    return f"""
# ALLOWED_PING_FROM_IP = {ALLOWED_PING_FROM_IP}

set block-policy drop
scrub in all

block all

pass quick on lo0

pass out quick proto udp to any port 53 keep state
pass out quick proto tcp to any port 53 keep state

pass out quick inet proto icmp all icmp-type 8 code 0 keep state

pass in quick inet proto icmp all icmp-type 0 code 0

pass in quick inet proto icmp from {ALLOWED_PING_FROM_IP} icmp-type 8 code 0 keep state

pass out quick proto tcp to any port {{ 80 443 }} keep state
"""


def get_flush_rules() -> str:
    return """# Flush all traffic
pass all
"""

def run_pfctl(args: list[str]) -> tuple[int, str]:
    try:
        r = subprocess.run(["pfctl"] + args, capture_output=True, text=True, timeout=10)
        return r.returncode, (r.stdout or "") + (r.stderr or "")
    except Exception as e:
        return -1, str(e)

def apply_rules() -> int:
    if os.geteuid() != 0:
        print("Run with sudo.")
        return 1

    try:
        Path(ANCHOR_FILE).write_text(get_rules(), encoding="utf-8")
    except OSError as e:
        print(f"Error {ANCHOR_FILE}: {e}")
        return 1

    code, out = run_pfctl(["-nf", PF_CONF])
    if code != 0:
        print("Error syntax")
        print(out)
        return 1

    run_pfctl(["-e"])
    code, out = run_pfctl(["-F", "all", "-q", "-f", PF_CONF])
    if code != 0:
        print("Error")
        print(out)
        return 1

    print("Rules applied. ALLOWED_PING_FROM_IP =", ALLOWED_PING_FROM_IP)
    return 0

def flush_rules() -> int:
    if os.geteuid() != 0:
        print("Run with sudo.")
        return 1

    try:
        Path(ANCHOR_FILE).write_text(get_flush_rules(), encoding="utf-8")
    except OSError as e:
        print(f"Error {ANCHOR_FILE}: {e}")
        return 1

    run_pfctl(["-f", PF_CONF])
    print("Rules flushed (pass all).")
    return 0

def show_rules() -> int:
    code, out = run_pfctl(["-sr"])
    print(out)
    return 0 if code == 0 else 1

def main() -> int:
    action = (sys.argv[1] if len(sys.argv) > 1 else "apply").lower()

    if action == "apply":
        return apply_rules()
    elif action == "flush":
        return flush_rules()
    elif action == "show":
        return show_rules()
    else:
        print("Usage: sudo python3 firewall_rules.py apply|flush|show")
        return 1

if __name__ == "__main__":
    sys.exit(main())

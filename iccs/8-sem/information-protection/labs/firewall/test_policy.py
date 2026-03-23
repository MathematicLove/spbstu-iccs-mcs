import subprocess
import sys
import platform
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))


def run_cmd(cmd: list[str], timeout: int = 10) -> tuple[bool, str]:
    try:
        r = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
        out = (r.stdout or "") + (r.stderr or "")
        return r.returncode == 0, out
    except subprocess.TimeoutExpired:
        return False, "Timeout"
    except Exception as e:
        return False, str(e)


def test_loopback():
    ok, out = run_cmd(["ping", "-c", "2", "127.0.0.1"])
    return ok, "Loopback (127.0.0.1)", out


def test_dns():
    for prog in ["nslookup", "dig", "host"]:
        ok, out = run_cmd([prog, "google.com"], timeout=5)
        if ok or "connection" not in out.lower():
            return ok, f"DNS ({prog})", out
    return False, "DNS", "nslookup/dig/host not found"


def test_ping_outgoing():
    ok, out = run_cmd(["ping", "-c", "2", "8.8.8.8"], timeout=5)
    return ok, "Ping outgoing (8.8.8.8)", out


def test_http():
    try:
        import urllib.request
        urllib.request.urlopen("http://httpforever.com", timeout=5)
        return True, "HTTP", "OK"
    except Exception as e:
        return False, "HTTP", str(e)


def test_https():
    try:
        import urllib.request
        import ssl
        ctx = ssl.create_default_context()
        urllib.request.urlopen("https://github.com", timeout=5, context=ctx)
        return True, "HTTPS", "OK"
    except Exception as e:
        return False, "HTTPS", str(e)


def test_blocked_traffic():
    cmd = ["nc", "-zv", "8.8.8.8", "22"]
    if platform.system() == "Darwin":
        cmd = ["nc", "-zv", "-G", "2", "8.8.8.8", "22"]
    ok, out = run_cmd(cmd, timeout=5)
    out_lower = out.lower()
    blocked = "timed out" in out_lower or "timeout" in out_lower or not ok
    return blocked, "Blocking (port 22)", out

def main():
    tests = [
        ("Loopback", test_loopback),
        ("DNS", test_dns),
        ("Ping outgoing", test_ping_outgoing),
        ("HTTP", test_http),
        ("HTTPS", test_https),
        ("Blocking (port 22)", test_blocked_traffic),
    ]

    results = []
    for name, fn in tests:
        ok, desc, out = fn()
        results.append((name, ok, out[:200] if out else ""))
        status = "OK" if ok else "FAIL"
        print(f"\n{name}: {status}")
        if out and len(out) < 300:
            print("  ", out.strip().replace("\n", " "))

    ip = "YOUR_IP"
    if platform.system() == "Darwin":
        for iface in ["en0", "en1", "en2"]:
            r = subprocess.run(["ipconfig", "getifaddr", iface], capture_output=True, text=True)
            if r.returncode == 0 and r.stdout and r.stdout.strip():
                ip = r.stdout.strip()
                break
    else:
        r = subprocess.run(["hostname", "-I"], capture_output=True, text=True)
        if r.returncode == 0 and r.stdout:
            ip = r.stdout.split()[0]


    return 0 if all(r[1] for r in results) else 1

if __name__ == "__main__":
    sys.exit(main())

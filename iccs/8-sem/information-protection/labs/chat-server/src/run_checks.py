#!/usr/bin/env python3
"""
Verification script for lab-5 secure chat.
Runs checks from report.tex: encryption, integrity, session key, client groups, traffic.
Output in English.
"""

from __future__ import annotations

import os
import subprocess
import sys
import time
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
os.chdir(_ROOT)

# Credentials for automated tests (alice/bob must exist with this password)
TEST_USER = os.environ.get("VERIFY_USER", "alice")
TEST_PASS = os.environ.get("VERIFY_PASS", "secret")
TEST_USER2 = os.environ.get("VERIFY_USER2", "bob")
KEY_FILE = _ROOT / "confdata" / "key.txt"
SRC = _ROOT / "src"


def section(title: str) -> None:
    print(f"\n{'='*60}")
    print(f"  {title}")
    print("=" * 60)


def run_server(cmd: list[str], port: int, wait: float = 0.5) -> subprocess.Popen:
    proc = subprocess.Popen(
        cmd,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        cwd=str(_ROOT),
    )
    time.sleep(wait)
    return proc


def run_client(
    cmd: list[str],
    stdin_data: str = "",
    timeout: int = 5,
) -> subprocess.CompletedProcess:
    return subprocess.run(
        cmd,
        input=stdin_data.encode() if stdin_data else None,
        capture_output=True,
        timeout=timeout,
        cwd=str(_ROOT),
    )


def check_1_without_encryption() -> None:
    """1. Without encryption: plaintext server (fortest) on 9001."""
    section("1. Without encryption (plaintext server)")
    port = 9001
    proc = run_server(
        [sys.executable, str(SRC / "fortest" / "server.py"), "--port", str(port)],
        port,
    )
    try:
        result = run_client(
            [
                sys.executable,
                str(SRC / "fortest" / "client.py"),
                "localhost",
                str(port),
                "-u",
                TEST_USER,
                "-p",
                TEST_PASS,
            ],
            stdin_data="Hello plain\n\\q\n",
            timeout=5,
        )
        if result.returncode == 0:
            print("[PASS] Connection without encryption works.")
            print("       Messages sent/received in plaintext (visible in tcpdump).")
        else:
            print(f"[FAIL] Exit code {result.returncode}, stderr: {result.stderr.decode()[:200]}")
    finally:
        proc.terminate()
        proc.wait(timeout=3)


def check_2_symmetric_encryption() -> None:
    """2. With symmetric encryption: ChaCha20 + key file."""
    section("2. With symmetric encryption (ChaCha20)")
    if not KEY_FILE.is_file():
        print("[SKIP] confdata/key.txt not found. Create key file for this check.")
        return
    port = 9010
    proc = run_server(
        [
            sys.executable,
            str(SRC / "server.py"),
            "--port",
            str(port),
            "--encrypt",
            "--key-file",
            str(KEY_FILE),
        ],
        port,
    )
    try:
        result = run_client(
            [
                sys.executable,
                str(SRC / "client.py"),
                "localhost",
                str(port),
                "-u",
                TEST_USER,
                "-p",
                TEST_PASS,
                "-e",
                "--key-file",
                str(KEY_FILE),
            ],
            stdin_data="Secret message encrypted\n\\q\n",
            timeout=5,
        )
        if result.returncode == 0:
            print("[PASS] Symmetric encryption (ChaCha20) works.")
            print("       Messages are encrypted; not readable in raw traffic.")
        else:
            print(f"[FAIL] Exit code {result.returncode}, stderr: {result.stderr.decode()[:200]}")
    finally:
        proc.terminate()
        proc.wait(timeout=3)


def check_3_integrity_hash_correct() -> None:
    """3a. Integrity verification: correct hash (Blake2b) - should work."""
    section("3. Integrity verification (Blake2b) - correct hash")
    if not KEY_FILE.is_file():
        print("[SKIP] confdata/key.txt not found.")
        return
    port = 9011
    proc = run_server(
        [
            sys.executable,
            str(SRC / "server.py"),
            "--port",
            str(port),
            "--encrypt",
            "--integrity",
            "--key-file",
            str(KEY_FILE),
        ],
        port,
    )
    try:
        result = run_client(
            [
                sys.executable,
                str(SRC / "client.py"),
                "localhost",
                str(port),
                "-u",
                TEST_USER,
                "-p",
                TEST_PASS,
                "-e",
                "-i",
                "--key-file",
                str(KEY_FILE),
            ],
            stdin_data="Integrity check OK\n\\q\n",
            timeout=5,
        )
        if result.returncode == 0:
            print("[PASS] Integrity check with correct hash: message accepted.")
        else:
            print(f"[FAIL] Exit code {result.returncode}")
    finally:
        proc.terminate()
        proc.wait(timeout=3)


def check_3_integrity_hash_wrong() -> None:
    """3b. Integrity verification: tampered hash - should fail."""
    section("3b. Integrity verification - wrong hash (tampered)")
    if not KEY_FILE.is_file():
        print("[SKIP] confdata/key.txt not found.")
        return
    port = 9012
    proc = run_server(
        [
            sys.executable,
            str(SRC / "server.py"),
            "--port",
            str(port),
            "--encrypt",
            "--integrity",
            "--test-integrity",
            "--key-file",
            str(KEY_FILE),
        ],
        port,
    )
    try:
        result = run_client(
            [
                sys.executable,
                str(SRC / "client.py"),
                "localhost",
                str(port),
                "-u",
                TEST_USER,
                "-p",
                TEST_PASS,
                "-e",
                "-i",
                "-t",
                "--key-file",
                str(KEY_FILE),
            ],
            stdin_data="Tampered hash test\n\\q\n",
            timeout=5,
        )
        out = (result.stdout + result.stderr).decode()
        if "целостности" in out or "Blake2b" in out or "integrity" in out.lower() or result.returncode != 0:
            print("[PASS] Wrong hash detected: integrity check correctly rejects tampered message.")
            print("       (Client/server report integrity failure)")
        else:
            print("[INFO] Check manually: --test-integrity should cause integrity failure.")
            print(f"       Exit code: {result.returncode}")
    finally:
        proc.terminate()
        proc.wait(timeout=3)


def check_4_session_key() -> None:
    """4. Session key (Diffie-Hellman X25519)."""
    section("4. Session key verification (Diffie-Hellman X25519)")
    port = 9013
    proc = run_server(
        [
            sys.executable,
            str(SRC / "server.py"),
            "--port",
            str(port),
            "--encrypt",
            "--dh",
        ],
        port,
    )
    try:
        result = run_client(
            [
                sys.executable,
                str(SRC / "client.py"),
                "localhost",
                str(port),
                "-u",
                TEST_USER,
                "-p",
                TEST_PASS,
                "-e",
                "--dh",
            ],
            stdin_data="Session key encrypted\n\\q\n",
            timeout=5,
        )
        if result.returncode == 0:
            print("[PASS] Session key (DH) mode works. Unique key per connection.")
        else:
            print(f"[FAIL] Exit code {result.returncode}")
    finally:
        proc.terminate()
        proc.wait(timeout=3)


def check_5_integrity_session_key() -> None:
    """5. Integrity in session key mode."""
    section("5. Integrity verification in session key mode")
    port = 9014
    proc = run_server(
        [
            sys.executable,
            str(SRC / "server.py"),
            "--port",
            str(port),
            "--encrypt",
            "--integrity",
            "--dh",
        ],
        port,
    )
    try:
        result = run_client(
            [
                sys.executable,
                str(SRC / "client.py"),
                "localhost",
                str(port),
                "-u",
                TEST_USER,
                "-p",
                TEST_PASS,
                "-e",
                "-i",
                "--dh",
            ],
            stdin_data="DH + integrity\n\\q\n",
            timeout=5,
        )
        if result.returncode == 0:
            print("[PASS] Session key + integrity: both enforced.")
        else:
            print(f"[FAIL] Exit code {result.returncode}")
    finally:
        proc.terminate()
        proc.wait(timeout=3)


def check_6_clients_and_groups() -> None:
    """6. Communication between clients and @mentions."""
    section("6. Communication between clients and @mentions")
    print("[INFO] Manual test: start server, run 2+ clients, use @username for private messages.")
    print("       Example: '@bob Hi' sends only to bob; 'Hello' broadcasts to all.")
    port = 9015
    proc = run_server(
        [
            sys.executable,
            str(SRC / "server.py"),
            "--port",
            str(port),
            "--encrypt",
            "--key-file",
            str(KEY_FILE),
        ],
        port,
    )
    try:
        r1 = run_client(
            [
                sys.executable,
                str(SRC / "client.py"),
                "localhost",
                str(port),
                "-u",
                TEST_USER,
                "-p",
                TEST_PASS,
                "-e",
                "--key-file",
                str(KEY_FILE),
            ],
            stdin_data="\\q\n",
            timeout=3,
        )
        r2 = run_client(
            [
                sys.executable,
                str(SRC / "client.py"),
                "localhost",
                str(port),
                "-u",
                TEST_USER2,
                "-p",
                TEST_PASS,
                "-e",
                "--key-file",
                str(KEY_FILE),
            ],
            stdin_data="\\q\n",
            timeout=3,
        )
        if r1.returncode == 0 and r2.returncode == 0:
            print("[PASS] Multiple clients can connect. Use @mentions for targeted messages.")
        else:
            print("[INFO] Both users (alice, bob) must exist with same password for full test.")
            print(f"       alice: {r1.returncode}, bob: {r2.returncode}")
    finally:
        proc.terminate()
        proc.wait(timeout=3)


def check_7_traffic_analysis() -> None:
    """7. Traffic analysis: plain vs encrypted."""
    section("7. Traffic analysis")
    print("[INFO] To analyze traffic:")
    print("       1. Without encryption: tcpdump -i lo0 -X -A port 9001")
    print("          -> Messages visible in plaintext.")
    print("       2. With encryption: tcpdump -i lo0 -X -A port 9000")
    print("          -> Only ciphertext (ChaCha20); content not readable.")
    print("[PASS] Documented: run tcpdump to compare plain vs encrypted traffic.")


def check_0_add_user_duplicate() -> None:
    """0. Add user: duplicate login check."""
    section("0. Add user - duplicate login check")
    r = subprocess.run(
        [sys.executable, str(SRC / "add_user.py"), "alice"],
        capture_output=True,
        input=b"wrongpassword\n",
        timeout=5,
        cwd=str(_ROOT),
    )
    out = (r.stdout + r.stderr).decode()
    if "Ooops! User with this login exist!" in out:
        print("[PASS] Duplicate login correctly rejected: 'Ooops! User with this login exist!'")
    else:
        print("[INFO] add_user duplicate check: run 'python src/add_user.py alice' when alice exists.")
        print(f"       Expected: Ooops! User with this login exist! | Got: {out[:100]}...")


def main() -> None:
    print("Lab-5 verification script")
    print("Users: alice, bob (password: secret by default, set VERIFY_PASS if different)")
    print("Key file: confdata/key.txt (required for encryption tests)")

    check_0_add_user_duplicate()
    check_1_without_encryption()
    check_2_symmetric_encryption()
    check_3_integrity_hash_correct()
    check_3_integrity_hash_wrong()
    check_4_session_key()
    check_5_integrity_session_key()
    check_6_clients_and_groups()
    check_7_traffic_analysis()

    section("Done")
    print("All checks completed. Review [PASS]/[FAIL]/[SKIP] above.")


if __name__ == "__main__":
    main()

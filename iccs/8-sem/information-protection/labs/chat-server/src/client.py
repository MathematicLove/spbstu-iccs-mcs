#!/usr/bin/env python3
from __future__ import annotations

import argparse
import errno
import getpass
import json
import os
import socket
import sys
import threading

from crypto import (
    dh_client,
    load_master_key,
    pack_message,
    parse_message,
    read_frame,
    write_frame,
)


def read_line(sock: socket.socket) -> str:
    buf = bytearray()
    while True:
        chunk = sock.recv(1)
        if not chunk:
            raise ConnectionError("connection broken")
        if chunk == b"\n":
            break
        buf.extend(chunk)
        if len(buf) > 4096:
            raise ValueError("too long string")
    return buf.decode("utf-8")


def _reader(sock: socket.socket, master_key: bytes) -> None:
    try:
        while True:
            body = read_frame(sock)
            payload = parse_message(body, master_key=master_key)
            print(payload.text, flush=True)
    except OSError as e:
        if e.errno not in (errno.EBADF, errno.ENOTCONN, errno.ECONNRESET):
            print(f"[chat] {e}", flush=True)
    except ValueError as e:
        err_str = str(e)
        print(file=sys.stderr)
        print("=" * 60, file=sys.stderr)
        print("!!! INTEGRITY CHECK FAILED !!!", file=sys.stderr)
        print(err_str, file=sys.stderr)
        print("Disconnected due to integrity violation.", file=sys.stderr)
        print("=" * 60, file=sys.stderr)
        sys.stderr.flush()
        try:
            sock.shutdown(socket.SHUT_RDWR)
            sock.close()
        except OSError:
            pass
        os._exit(1)
    except (ConnectionError, UnicodeError) as e:
        print(f"[chat] {e}", flush=True)


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("host", nargs="?", default="localhost")
    p.add_argument("port", type=int, nargs="?", default=9000)
    p.add_argument("--user", "-u", default=None)
    p.add_argument("--password", "-p", default=None)
    p.add_argument("--key-file", type=str, default=None)
    p.add_argument("--encrypt", "-e", action="store_true")
    p.add_argument("--integrity", "-i", action="store_true")
    p.add_argument("--test-integrity", "-t", action="store_true")
    p.add_argument("--dh", action="store_true")
    args = p.parse_args()

    if args.encrypt:
        if args.dh and args.key_file:
            raise SystemExit("Either --dh or --key-file")
        if not args.dh and not args.key_file:
            raise SystemExit("For --encrypt specify --key-file or --dh")
    if args.key_file and args.dh:
        raise SystemExit("Cannot use --key-file and --dh together")
    if args.test_integrity and not args.integrity:
        raise SystemExit("--test-integrity only with --integrity")

    if args.encrypt and args.key_file:
        from pathlib import Path

        master_key = load_master_key(Path(args.key_file))
    else:
        master_key = b"\x00" * 32

    user = args.user or input("Login: ")
    password = args.password
    if password is None:
        password = getpass.getpass("Password: ")

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((args.host, args.port))

    auth = json.dumps({"user": user, "pass": password}, ensure_ascii=False)
    sock.sendall(auth.encode("utf-8") + b"\n")

    resp = read_line(sock)
    if resp != "OK":
        if resp == "BUSY":
            print(
                "BUSY: Login already in chat. Close another client or use another user.",
                file=sys.stderr,
            )
        else:
            print(resp, file=sys.stderr)
        sys.exit(1)

    if args.dh:
        master_key = dh_client(sock)

    t = threading.Thread(target=_reader, args=(sock, master_key), daemon=True)
    t.start()

    try:
        for line in sys.stdin:
            text = line.rstrip("\n\r")
            if text == r"\q":
                body = pack_message(
                r"\q",
                master_key=master_key,
                use_encrypt=args.encrypt,
                use_integrity=args.integrity,
                test_bad_hash=args.test_integrity,
            )
                write_frame(sock, body)
                break
            body = pack_message(
                text,
                master_key=master_key,
                use_encrypt=args.encrypt,
                use_integrity=args.integrity,
                test_bad_hash=args.test_integrity,
            )
            write_frame(sock, body)
    except (BrokenPipeError, OSError):
        pass
    finally:
        try:
            sock.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass
        t.join(timeout=5.0)
        try:
            sock.close()
        except OSError:
            pass


if __name__ == "__main__":
    main()

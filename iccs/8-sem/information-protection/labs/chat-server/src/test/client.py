#!/usr/bin/env python3
"""
Клиент к fortest/server.py: те же кадры (длина + UTF-8), без шифрования.
"""

from __future__ import annotations

import argparse
import errno
import getpass
import json
import socket
import sys
import threading
from pathlib import Path

_FORTEST_DIR = Path(__file__).resolve().parent
if str(_FORTEST_DIR) not in sys.path:
    sys.path.insert(0, str(_FORTEST_DIR))
from plain_io import read_frame, write_frame

MAX_MESSAGE_CHARS = 80


def read_line(sock: socket.socket) -> str:
    buf = bytearray()
    while True:
        chunk = sock.recv(1)
        if not chunk:
            raise ConnectionError("обрыв")
        if chunk == b"\n":
            break
        buf.extend(chunk)
        if len(buf) > 4096:
            raise ValueError("слишком длинная строка")
    return buf.decode("utf-8")


def _reader(sock: socket.socket) -> None:
    try:
        while True:
            text = read_frame(sock).decode("utf-8")
            print(text, flush=True)
    except OSError as e:
        if e.errno not in (errno.EBADF, errno.ENOTCONN, errno.ECONNRESET):
            print(f"[чат] {e}", flush=True)
    except (ConnectionError, ValueError, UnicodeError) as e:
        print(f"[чат] {e}", flush=True)


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("host", nargs="?", default="localhost")
    p.add_argument("port", type=int, nargs="?", default=9001)
    p.add_argument("--user", "-u", default=None)
    p.add_argument("--password", "-p", default=None)
    args = p.parse_args()

    user = args.user or input("Логин: ")
    password = args.password
    if password is None:
        password = getpass.getpass("Пароль: ")

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((args.host, args.port))

    sock.sendall(json.dumps({"user": user, "pass": password}, ensure_ascii=False).encode("utf-8") + b"\n")

    resp = read_line(sock)
    if resp != "OK":
        if resp == "BUSY":
            print(
                "BUSY: логин уже занят.",
                file=sys.stderr,
            )
        else:
            print(resp, file=sys.stderr)
        sys.exit(1)

    t = threading.Thread(target=_reader, args=(sock,), daemon=True)
    t.start()

    try:
        for line in sys.stdin:
            text = line.rstrip("\n\r")
            if len(text) > MAX_MESSAGE_CHARS:
                print(f"Не более {MAX_MESSAGE_CHARS} символов", file=sys.stderr)
                continue
            write_frame(sock, text.encode("utf-8"))
            if text == r"\q":
                break
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

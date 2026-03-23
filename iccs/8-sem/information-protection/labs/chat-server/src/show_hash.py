#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import secrets
import socket
import sys
from pathlib import Path

BLAKE2B_DIGEST = 32
SALT_SIZE = 16


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


def main() -> None:
    p = argparse.ArgumentParser(description="Show session hash (Blake2b with salt)")
    p.add_argument("host", nargs="?", default="localhost")
    p.add_argument("port", type=int, nargs="?", default=9000)
    p.add_argument("--user", "-u", default=None)
    p.add_argument("--password", "-p", default=None)
    p.add_argument("--key-file", type=str, default=None)
    p.add_argument("--dh", action="store_true")
    args = p.parse_args()

    if args.dh and args.key_file:
        sys.exit("Cannot use --key-file and --dh together")

    if args.key_file:
        from crypto import load_master_key

        master_key = load_master_key(Path(args.key_file))
    else:
        master_key = b"\x00" * 32

    user = args.user or input("Login: ")
    if args.password is not None:
        password = args.password
    else:
        import getpass

        password = getpass.getpass("Password: ")

    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect((args.host, args.port))
    except (socket.error, OSError) as e:
        print("Ooops! Looks like someone forget to run server... (", file=sys.stderr)
        sys.exit(1)

    try:
        auth = json.dumps({"user": user, "pass": password}, ensure_ascii=False)
        sock.sendall(auth.encode("utf-8") + b"\n")

        resp = read_line(sock)
        if resp != "OK":
            if resp == "BUSY":
                print("BUSY: Login already in chat.", file=sys.stderr)
            else:
                print(resp, file=sys.stderr)
            sys.exit(1)

        if args.dh:
            from crypto import dh_client, read_frame, write_frame

            master_key = dh_client(sock)

        salt = secrets.token_bytes(SALT_SIZE)
        h = hashlib.blake2b(master_key, salt=salt, digest_size=BLAKE2B_DIGEST).digest()
        print(f"Your hash: {salt.hex()}${h.hex()}")
    finally:
        sock.close()


if __name__ == "__main__":
    main()

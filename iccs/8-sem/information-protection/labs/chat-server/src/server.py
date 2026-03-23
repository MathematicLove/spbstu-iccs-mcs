#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import socket
import threading
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent

from auth import load_passwd, verify_password
from crypto import (
    dh_server,
    load_master_key,
    pack_message,
    parse_message,
    read_frame,
    write_frame,
)

clients_lock = threading.Lock()
clients: dict[str, socket.socket] = {}
# separate key for connection (different when --dh; same when key from file)
client_keys: dict[str, bytes] = {}

_MENTION = re.compile(r"@([A-Za-z0-9_]+)")


def _mentioned(text: str) -> list[str]:
    return _MENTION.findall(text)


def _has_mentions(text: str) -> bool:
    return bool(_MENTION.search(text))


def _chat_line(username: str, text: str) -> str:
    return f"{username} : {text}"


def read_line(sock: socket.socket) -> str:
    buf = bytearray()
    while True:
        chunk = sock.recv(1)
        if not chunk:
            raise ConnectionError("connection broken while reading line")
        if chunk == b"\n":
            break
        buf.extend(chunk)
        if len(buf) > 4096:
            raise ValueError("too long string")
    return buf.decode("utf-8")


def send_line(sock: socket.socket, text: str) -> None:
    sock.sendall(text.encode("utf-8"))


def send_frame_safe(
    sock: socket.socket,
    text: str,
    *,
    master_key: bytes,
    encrypt: bool,
    integrity: bool,
    test_bad: bool,
) -> None:
    body = pack_message(
        text,
        master_key=master_key,
        use_encrypt=encrypt,
        use_integrity=integrity,
        test_bad_hash=test_bad,
    )
    write_frame(sock, body)


def broadcast_system(
    line: str,
    *,
    encrypt: bool,
    integrity: bool,
    test_bad: bool,
    exclude: set[str] | None = None,
) -> None:
    exclude = exclude or set()
    with clients_lock:
        items = [(u, clients[u], client_keys[u]) for u in clients if u not in exclude]
    for _u, sock, key in items:
        try:
            send_frame_safe(sock, line, master_key=key, encrypt=encrypt, integrity=integrity, test_bad=test_bad)
        except OSError:
            pass


def send_to_users(
    names: set[str],
    line: str,
    *,
    encrypt: bool,
    integrity: bool,
    test_bad: bool,
) -> None:
    with clients_lock:
        items = [(u, clients[u], client_keys[u]) for u in names if u in clients]
    for _u, sock, key in items:
        try:
            send_frame_safe(sock, line, master_key=key, encrypt=encrypt, integrity=integrity, test_bad=test_bad)
        except OSError:
            pass


def handle_client(
    conn: socket.socket,
    addr: tuple,
    passwd_path: Path,
    file_master_key: bytes,
    encrypt: bool,
    integrity: bool,
    test_bad: bool,
    use_dh: bool,
) -> None:
    username: str | None = None
    left_announced = False
    try:
        raw = read_line(conn)
        try:
            auth = json.loads(raw)
            user = str(auth.get("user", "")).strip()
            password = str(auth.get("pass", ""))
        except (json.JSONDecodeError, TypeError):
            print(f"[{addr}] auth FAIL: invalid JSON")
            send_line(conn, "FAIL\n")
            return

        db = load_passwd(passwd_path)
        if not user or not verify_password(user, password, db):
            print(f"[{addr}] auth FAIL: user={user!r}")
            send_line(conn, "FAIL\n")
            return

        with clients_lock:
            if user in clients:
                print(f"[{addr}] auth BUSY: {user} already in chat")
                send_line(conn, "BUSY\n")
                return

        print(f"[{addr}] auth OK: {user} logged in")
        send_line(conn, "OK\n")

        if use_dh:
            session_key = dh_server(conn)
        else:
            session_key = file_master_key

        with clients_lock:
            clients[user] = conn
            client_keys[user] = session_key
        username = user

        print(f"[{user}] joined chat")

        while True:
            body = read_frame(conn)
            payload = parse_message(body, master_key=client_keys[user])
            text = payload.text.strip()
            if text == r"\q":
                print(f"[{user}] left chat")
                left_announced = True
                return

            line_out = _chat_line(user, payload.text)
            with clients_lock:
                online = set(clients.keys())
            if _has_mentions(payload.text):
                # only mentioned @User (if they are online)
                targets = set(_mentioned(payload.text)) & online
                print(f"[{user}] -> {', '.join(f'@{t}' for t in targets)}: {payload.text[:50]!r}")
            else:
                # broadcast
                targets = online
                print(f"[{user}] broadcast: {payload.text[:50]!r}")

            send_to_users(targets, line_out, encrypt=encrypt, integrity=integrity, test_bad=test_bad)

    except ValueError as e:
        who = username or addr
        err_msg = str(e)
        print(f"[{who}] {err_msg}")
        if username:
            m = re.search(r"Corrupted data received: (.+)$", err_msg)
            corrupt_part = m.group(1)[:50] if m else err_msg[:50]
            notice = (f"!!! Integrity FAIL from {username}: {corrupt_part}")[:76]
            disconnect_msg = (f"{username} disconnected (integrity error)")[:76]
            with clients_lock:
                all_clients = set(clients.keys())
            if all_clients:
                send_to_users(all_clients, notice, encrypt=encrypt, integrity=integrity, test_bad=False)
                send_to_users(all_clients, disconnect_msg, encrypt=encrypt, integrity=integrity, test_bad=False)
    except (ConnectionError, OSError, UnicodeError) as e:
        who = username or addr
        print(f"[{who}] error/disconnect: {e}")
    finally:
        if username:
            with clients_lock:
                clients.pop(username, None)
                client_keys.pop(username, None)
            if not left_announced:
                print(f"[{username}] disconnected (no \\q)")
        try:
            conn.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass
        conn.close()


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("--port", "-P", type=int, default=9000)
    p.add_argument("--passwd", type=Path, default=_ROOT / "confdata" / "passwd")
    p.add_argument("--key-file", type=Path, default=None)
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
        file_master_key = load_master_key(args.key_file)
    else:
        file_master_key = b"\x00" * 32

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(("0.0.0.0", args.port))
    sock.listen(50)
    print(f"Yaaay! 0.0.0.0:{args.port}", flush=True)

    try:
        while True:
            conn, addr = sock.accept()
            print(f"[server] new connection from {addr}")
            t = threading.Thread(
                target=handle_client,
                args=(conn, addr, args.passwd, file_master_key, args.encrypt, args.integrity, args.test_integrity, args.dh),
                daemon=True,
            )
            t.start()
    except KeyboardInterrupt:
        print("\nStop.")
    finally:
        sock.close()


if __name__ == "__main__":
    main()

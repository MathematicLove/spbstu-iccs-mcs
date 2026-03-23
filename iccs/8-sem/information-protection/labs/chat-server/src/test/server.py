#!/usr/bin/env python3
"""
Тестовый сервер: та же логика чата, что и в корневом server.py, но без ChaCha20/Blake2b/DH.
Кадр после авторизации: 4 байта длины + UTF-8 строка (в tcpdump -X виден открытый текст).
"""

from __future__ import annotations

import argparse
import json
import re
import socket
import sys
import threading
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent.parent
_SRC = Path(__file__).resolve().parent.parent
_FORTEST = Path(__file__).resolve().parent
if str(_SRC) not in sys.path:
    sys.path.insert(0, str(_SRC))
if str(_FORTEST) not in sys.path:
    sys.path.insert(0, str(_FORTEST))

from auth import load_passwd, verify_password
from plain_io import read_frame, write_frame

# read_line локально — не из plain_io
clients_lock = threading.Lock()
clients: dict[str, socket.socket] = {}

_MENTION = re.compile(r"@([A-Za-z0-9_]+)")
MAX_MESSAGE_CHARS = 80


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
            raise ConnectionError("обрыв при чтении строки")
        if chunk == b"\n":
            break
        buf.extend(chunk)
        if len(buf) > 4096:
            raise ValueError("слишком длинная строка")
    return buf.decode("utf-8")


def send_line(sock: socket.socket, text: str) -> None:
    sock.sendall(text.encode("utf-8"))


def broadcast_system(line: str, exclude: set[str] | None = None) -> None:
    exclude = exclude or set()
    data = line.encode("utf-8")
    with clients_lock:
        items = [(u, clients[u]) for u in clients if u not in exclude]
    for _u, sock in items:
        try:
            write_frame(sock, data)
        except OSError:
            pass


def send_to_users(names: set[str], line: str) -> None:
    data = line.encode("utf-8")
    with clients_lock:
        socks = [(u, clients[u]) for u in names if u in clients]
    for _u, sock in socks:
        try:
            write_frame(sock, data)
        except OSError:
            pass


def handle_client(conn: socket.socket, addr: tuple, passwd_path: Path) -> None:
    username: str | None = None
    left_announced = False
    try:
        raw = read_line(conn)
        try:
            auth = json.loads(raw)
            user = str(auth.get("user", "")).strip()
            password = str(auth.get("pass", ""))
        except (json.JSONDecodeError, TypeError):
            send_line(conn, "FAIL\n")
            return

        db = load_passwd(passwd_path)
        if not user or not verify_password(user, password, db):
            send_line(conn, "FAIL\n")
            return

        with clients_lock:
            if user in clients:
                send_line(conn, "BUSY\n")
                return

        send_line(conn, "OK\n")

        with clients_lock:
            clients[user] = conn
        username = user

        broadcast_system(f"{user} joined chat")

        while True:
            body = read_frame(conn)
            payload_text = body.decode("utf-8")
            if len(payload_text) > MAX_MESSAGE_CHARS:
                raise ValueError(f"сообщение длиннее {MAX_MESSAGE_CHARS} символов")
            text = payload_text.strip()
            if text == r"\q":
                broadcast_system(f"{user} go out from chat", exclude={user})
                left_announced = True
                return

            line_out = _chat_line(user, payload_text)
            with clients_lock:
                online = set(clients.keys())
            if _has_mentions(payload_text):
                targets = set(_mentioned(payload_text)) & online
            else:
                targets = online

            send_to_users(targets, line_out)

    except (ConnectionError, OSError, ValueError, UnicodeError) as e:
        print(f"[{addr}] {e}")
    finally:
        if username:
            with clients_lock:
                clients.pop(username, None)
            if not left_announced:
                broadcast_system(f"{username} go out from chat")
        try:
            conn.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass
        conn.close()


def main() -> None:
    p = argparse.ArgumentParser(description="Чат без шифрования (для tcpdump).")
    p.add_argument("--port", "-P", type=int, default=9001)
    p.add_argument(
        "--passwd",
        type=Path,
        default=_ROOT / "confdata" / "passwd",
        help="Путь к passwd (по умолчанию confdata/passwd от корня lab-5)",
    )
    args = p.parse_args()

    passwd_path = args.passwd
    if not passwd_path.is_file():
        raise SystemExit(f"Нет файла паролей: {passwd_path}")

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(("0.0.0.0", args.port))
    sock.listen(50)
    print(
        f"fortest: plaintext TCP 0.0.0.0:{args.port}  passwd={passwd_path}",
        flush=True,
    )

    try:
        while True:
            conn, addr = sock.accept()
            t = threading.Thread(target=handle_client, args=(conn, addr, passwd_path), daemon=True)
            t.start()
    except KeyboardInterrupt:
        print("\nСтоп.")
    finally:
        sock.close()


if __name__ == "__main__":
    main()

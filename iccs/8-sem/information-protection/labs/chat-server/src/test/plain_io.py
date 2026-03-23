"""Кадр TCP: 4 байта длины (BE) + сырой UTF-8 текст. Без шифрования — удобно для tcpdump."""

from __future__ import annotations

import struct


def read_exact(sock, n: int) -> bytes:
    buf = bytearray()
    while len(buf) < n:
        chunk = sock.recv(n - len(buf))
        if not chunk:
            raise ConnectionError("соединение закрыто")
        buf.extend(chunk)
    return bytes(buf)


def read_frame(sock) -> bytes:
    (length,) = struct.unpack("!I", read_exact(sock, 4))
    if length > 0x100000:
        raise ValueError("некорректная длина кадра")
    return read_exact(sock, length)


def write_frame(sock, body: bytes) -> None:
    sock.sendall(struct.pack("!I", len(body)) + body)


def send_text(sock, text: str) -> None:
    write_frame(sock, text.encode("utf-8"))


def read_text(sock) -> str:
    return read_frame(sock).decode("utf-8")

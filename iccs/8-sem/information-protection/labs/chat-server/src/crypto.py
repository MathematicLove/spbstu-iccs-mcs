from __future__ import annotations

import hashlib
import os
import stat
import struct
from pathlib import Path
from typing import NamedTuple

from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import x25519
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms
from cryptography.hazmat.primitives.kdf.hkdf import HKDF

from cryptography.hazmat.primitives import serialization

MAX_MESSAGE_CHARS = 80
BLAKE2B_DIGEST = 32
CHACHA_KEY_SIZE = 32
CHACHA_NONCE_SIZE = 16
SALT_SIZE = 16
MIN_ENCRYPT_BLOB = SALT_SIZE + CHACHA_NONCE_SIZE + 1

FLAG_ENCRYPT = 1 << 0
FLAG_INTEGRITY = 1 << 1
FLAG_TEST_BAD_HASH = 1 << 2

KIND_DH_CLIENT = 0xC1
KIND_DH_SERVER = 0xC2
KIND_USER = 0x01


class UserPayload(NamedTuple):
    text: str
    encrypt: bool
    integrity: bool
    test_bad_hash: bool


def _check_key_file_permissions(path: Path) -> None:
    if os.name == "nt":
        return
    st = path.stat()
    mode = stat.S_IMODE(st.st_mode)
    if st.st_uid != os.geteuid():
        raise PermissionError(f"Key file {path} must belong to the current user")
    if mode & 0o077:
        raise PermissionError(f"Key file {path}: chmod 400 for owner")
    if not (mode & stat.S_IRUSR):
        raise PermissionError(f"Key file {path}: no read for owner")


def load_master_key(path: Path) -> bytes:
    _check_key_file_permissions(path)
    raw = path.read_text(encoding="utf-8").strip()
    if len(raw) < 32:
        raise ValueError("Key in file must be at least 32 characters")
    return raw.encode("utf-8")


def derive_session_key_from_shared_secret(shared_secret: bytes) -> bytes:
    hkdf = HKDF(algorithm=hashes.SHA256(), length=CHACHA_KEY_SIZE, salt=None, info=b"lab5-x25519-session")
    return hkdf.derive(shared_secret)


def derive_message_key(master_key: bytes, salt: bytes) -> bytes:
    hkdf = HKDF(
        algorithm=hashes.SHA256(),
        length=CHACHA_KEY_SIZE,
        salt=salt,
        info=b"lab5-chacha20-msg",
    )
    return hkdf.derive(master_key)


def chacha20_encrypt(plaintext: bytes, master_key: bytes) -> bytes:
    if len(plaintext) > MAX_MESSAGE_CHARS * 4:
        raise ValueError("too long message")
    salt = os.urandom(SALT_SIZE)
    msg_key = derive_message_key(master_key, salt)
    nonce = os.urandom(CHACHA_NONCE_SIZE)
    cipher = Cipher(algorithms.ChaCha20(msg_key, nonce), mode=None)
    enc = cipher.encryptor()
    return salt + nonce + enc.update(plaintext) + enc.finalize()


def chacha20_decrypt(blob: bytes, master_key: bytes) -> bytes:
    if len(blob) < SALT_SIZE + CHACHA_NONCE_SIZE:
        raise ValueError("invalid packet")
    salt = blob[:SALT_SIZE]
    nonce = blob[SALT_SIZE : SALT_SIZE + CHACHA_NONCE_SIZE]
    ciphertext = blob[SALT_SIZE + CHACHA_NONCE_SIZE :]
    msg_key = derive_message_key(master_key, salt)
    cipher = Cipher(algorithms.ChaCha20(msg_key, nonce), mode=None)
    dec = cipher.decryptor()
    return dec.update(ciphertext) + dec.finalize()


def blake2b_digest(data: bytes) -> bytes:
    return hashlib.blake2b(data, digest_size=BLAKE2B_DIGEST).digest()


def verify_message_text(text: str) -> None:
    if len(text) > MAX_MESSAGE_CHARS:
        raise ValueError(f"No more than {MAX_MESSAGE_CHARS} characters")


def read_exact(sock, n: int) -> bytes:
    buf = bytearray()
    while len(buf) < n:
        chunk = sock.recv(n - len(buf))
        if not chunk:
            raise ConnectionError("connection closed")
        buf.extend(chunk)
    return bytes(buf)


def write_frame(sock, body: bytes) -> None:
    sock.sendall(struct.pack("!I", len(body)) + body)


def read_frame(sock) -> bytes:
    (length,) = struct.unpack("!I", read_exact(sock, 4))
    if length > 0x100000:
        raise ValueError("invalid frame length")
    return read_exact(sock, length)


def _raw_pub(key: x25519.X25519PublicKey) -> bytes:
    return key.public_bytes(
        encoding=serialization.Encoding.Raw,
        format=serialization.PublicFormat.Raw,
    )

def dh_client(sock) -> bytes:
    priv = x25519.X25519PrivateKey.generate()
    write_frame(sock, bytes([KIND_DH_CLIENT]) + _raw_pub(priv.public_key()))
    body = read_frame(sock)
    if len(body) != 33 or body[0] != KIND_DH_SERVER:
        raise ValueError("Expected DH server")
    peer = x25519.X25519PublicKey.from_public_bytes(body[1:33])
    return derive_session_key_from_shared_secret(priv.exchange(peer))


def dh_server(sock) -> bytes:
    body = read_frame(sock)
    if len(body) != 33 or body[0] != KIND_DH_CLIENT:
        raise ValueError("Expected DH client")
    peer = x25519.X25519PublicKey.from_public_bytes(body[1:33])
    priv = x25519.X25519PrivateKey.generate()
    write_frame(sock, bytes([KIND_DH_SERVER]) + _raw_pub(priv.public_key()))
    return derive_session_key_from_shared_secret(priv.exchange(peer))

def pack_message(
    text: str,
    *,
    master_key: bytes,
    use_encrypt: bool,
    use_integrity: bool,
    test_bad_hash: bool,
) -> bytes:
    verify_message_text(text)
    pt = text.encode("utf-8")
    flags = 0
    if use_encrypt:
        flags |= FLAG_ENCRYPT
    if use_integrity:
        flags |= FLAG_INTEGRITY
    if test_bad_hash:
        flags |= FLAG_TEST_BAD_HASH

    if test_bad_hash and len(pt) > 0:
        pt_corrupted = bytearray(pt)
        idx = len(pt_corrupted) // 2
        pt_corrupted[idx] = (pt_corrupted[idx] + 1) % 256
        pt_corrupted = bytes(pt_corrupted)
    else:
        pt_corrupted = pt

    inner = bytearray([flags])
    if use_encrypt:
        inner.extend(chacha20_encrypt(pt_corrupted, master_key))
    else:
        inner.append(len(pt_corrupted))
        inner.extend(pt_corrupted)

    if use_integrity:
        h = blake2b_digest(pt)
        inner.extend(h)

    return bytes([KIND_USER]) + bytes(inner)


def parse_message(body: bytes, *, master_key: bytes) -> UserPayload:
    if not body or body[0] != KIND_USER:
        raise ValueError("not user packet")
    pos = 1
    flags = body[pos]
    pos += 1
    encrypt = bool(flags & FLAG_ENCRYPT)
    integrity = bool(flags & FLAG_INTEGRITY)
    test_sent = bool(flags & FLAG_TEST_BAD_HASH)
    recv_hash: bytes | None = None

    if encrypt:
        blob = body[pos:]
        if integrity:
            if len(blob) < MIN_ENCRYPT_BLOB + BLAKE2B_DIGEST:
                raise ValueError("short packet")
            ct_part = blob[:-BLAKE2B_DIGEST]
            recv_hash = blob[-BLAKE2B_DIGEST:]
        else:
            if len(blob) < MIN_ENCRYPT_BLOB:
                raise ValueError("short packet")
            ct_part = blob
        pt = chacha20_decrypt(ct_part, master_key)
        try:
            text = pt.decode("utf-8")
        except UnicodeDecodeError:
            text = pt.decode("utf-8", errors="replace")
    else:
        ln = body[pos]
        pos += 1
        if integrity:
            end = len(body) - BLAKE2B_DIGEST
            recv_hash = body[end : end + BLAKE2B_DIGEST]
            pt = body[pos:end]
        else:
            pt = body[pos : pos + ln]
        text = pt.decode("utf-8")

    if integrity:
        expected = blake2b_digest(pt)
        if recv_hash is None or recv_hash != expected:
            raise ValueError(
                f"Integrity check failed: invalid Blake2b. Corrupted data received: {text!r}"
            )

    verify_message_text(text)
    return UserPayload(text=text, encrypt=encrypt, integrity=integrity, test_bad_hash=test_sent)

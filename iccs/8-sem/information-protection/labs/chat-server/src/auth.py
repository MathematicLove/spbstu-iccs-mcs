from __future__ import annotations

import hashlib
import secrets
from pathlib import Path


def _parse_line(line: str) -> tuple[str, bytes, bytes] | None:
    line = line.strip()
    if not line or line.startswith("#"):
        return None
    if ":" not in line:
        return None
    login, rest = line.split(":", 1)
    parts = rest.split("$")
    if len(parts) != 3 or parts[0] != "100000":
        return None
    try:
        salt = bytes.fromhex(parts[1])
        digest = bytes.fromhex(parts[2])
    except ValueError:
        return None
    return login.strip(), salt, digest


def load_passwd(path: Path) -> dict[str, tuple[bytes, bytes]]:
    out: dict[str, tuple[bytes, bytes]] = {}
    if not path.is_file():
        return out
    for line in path.read_text(encoding="utf-8").splitlines():
        parsed = _parse_line(line)
        if parsed:
            login, salt, digest = parsed
            out[login] = (salt, digest)
    return out


def verify_password(login: str, password: str, db: dict[str, tuple[bytes, bytes]]) -> bool:
    if login not in db:
        return False
    salt, expected = db[login]
    got = hashlib.pbkdf2_hmac("sha256", password.encode("utf-8"), salt, 100000)
    return secrets.compare_digest(got, expected)


def hash_line(login: str, password: str) -> str:
    salt = secrets.token_bytes(16)
    digest = hashlib.pbkdf2_hmac("sha256", password.encode("utf-8"), salt, 100000)
    return f"{login}:100000${salt.hex()}${digest.hex()}"

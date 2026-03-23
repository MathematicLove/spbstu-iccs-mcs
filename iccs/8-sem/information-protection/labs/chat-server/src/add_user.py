#!/usr/bin/env python3
from __future__ import annotations

import argparse
import getpass
import sys
from pathlib import Path

_ROOT = Path(__file__).resolve().parent.parent
if str(_ROOT / "src") not in sys.path:
    sys.path.insert(0, str(_ROOT / "src"))

from auth import hash_line, load_passwd  # noqa: E402


def main() -> None:
    p = argparse.ArgumentParser()
    default_passwd = _ROOT / "confdata" / "passwd"
    p.add_argument("--passwd", type=Path, default=default_passwd)
    p.add_argument("login")
    args = p.parse_args()
    passwd_path = args.passwd.resolve()
    login = args.login.strip()
    if not login:
        print("Login cannot be empty.", file=sys.stderr)
        sys.exit(1)
    db = load_passwd(passwd_path)
    if login in db:
        print("Ooops! User with this login exists!", file=sys.stderr)
        sys.exit(1)
    pw = getpass.getpass("Password: ")
    passwd_path.parent.mkdir(parents=True, exist_ok=True)
    line = hash_line(login, pw) + "\n"
    with passwd_path.open("a", encoding="utf-8") as f:
        f.write(line)
    print(f"Added to {passwd_path}")


if __name__ == "__main__":
    main()

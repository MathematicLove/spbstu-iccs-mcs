import os
import sys
import time
import hashlib
import itertools
from config_MAC import PASSWD_FILE, HASH_ALGO, PASSWORD_ALPHABET, FIRST_CHAR_ALPHABET

# 8 hours
MAX_BRUTE_TIME_SEC = 8 * 3600

_SAVED_EUID = None


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
    return h.hexdigest()


# passwd format (MAC): login:hash:uid:security_level:fullname (elevate/drop to access 0600 root)
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
                login, pwd_hash, uid, sec_level, fullname = parts
                users[login] = pwd_hash
    finally:
        drop()
    return users


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


# from 1-8 to not kill my poor mac :((((
def generate_passwords(max_length):
    for length in range(1, max_length + 1):
        if length == 1:
            for ch in FIRST_CHAR_ALPHABET:
                yield ch
        else:
            for first in FIRST_CHAR_ALPHABET:
                for rest in itertools.product(PASSWORD_ALPHABET, repeat=length - 1):
                    yield first + ''.join(rest)


def brute_force(login, target_hash, max_length):
    total_attempts = 0
    start_time = time.time()
    for password in generate_passwords(max_length):
        if time.time() - start_time > MAX_BRUTE_TIME_SEC:
            elapsed = time.time() - start_time
            return None, total_attempts, elapsed, True
        total_attempts += 1
        if hash_password(password) == target_hash:
            elapsed = time.time() - start_time
            return password, total_attempts, elapsed, False
    elapsed = time.time() - start_time
    return None, total_attempts, elapsed, False


def main():
    if len(sys.argv) < 2:
        print("use:")
        print("crack_MAC.py LOGIN LENGTH - Permutation")
        print("crack_MAC.py --max-iterations - max iterations for 3-8")
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
        print("use: crack_MAC.py <login> <length>")
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
        print("Interrupt: more than 8 hours")
    if found:
        print(f"Yes! Found password: {found}")
        print(f"Count of attempts: {attempts}")
        print(f"Time spent: {elapsed:.2f} s ({elapsed / 3600:.2f} H)")
    else:
        print(f"Not found with {attempts} attempts")
        print(f"Time spent: {elapsed:.2f} s ({elapsed / 3600:.2f} H)")

if __name__ == "__main__":
    main()

#!/usr/bin/env python3
import os
import sys
import getpass
import hashlib
import logging
from config import PASSWD_FILE, AUTH_LOG, LOG_DIR, HASH_ALGO, PASSWORD_ALPHABET, FIRST_CHAR_ALPHABET

# Logging for Authorization 
logging.basicConfig(filename=AUTH_LOG, level=logging.INFO,
                    format='%(asctime)s - %(levelname)s - %(message)s')

def hash_password(password):
    h = hashlib.new(HASH_ALGO)
    h.update(password.encode('utf-8'))
    return h.hexdigest() # return hash 

# Correct password (char + ASCII)
def validate_password(password):
    if len(password) < 1:
        return False, "Cant be empty!!!"
    first = password[0]
    if first not in FIRST_CHAR_ALPHABET:
        return False, "First letter should be Letter"
    for ch in password:
        if ch not in PASSWORD_ALPHABET:
            return False, f"Cant use: {ch}, only UTF-8"
    return True, "OK"

# Read file passwd and return LIST(MAP(USERS)) [{Str:Data}, {}, ...]
def read_passwd():
    users = []
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
            login, pwd_hash, uid, perms, fullname = parts
            try:
                uid_int = int(uid)
            except ValueError:
                continue
            users.append({
                'login': login,
                'hash': pwd_hash,
                'uid': uid_int,
                'perms': perms,
                'fullname': fullname
            })
    return users

# Append list(map(users)) to passwd - ROOT!
def write_passwd(users):
    with open(PASSWD_FILE, 'w') as f:
        for u in users:
            f.write(f"{u['login']}:{u['hash']}:{u['uid']}:{u['perms']}:{u['fullname']}\n")
    try:
        os.chmod(PASSWD_FILE, 0o600)
    except OSError:
        pass

# UID : 1000 + 1 + 1 + ... + 1
def get_next_uid(users):
    if not users:
        return 1000
    return max(u['uid'] for u in users) + 1

# New user
def add_user():
    print("Adding new user")
    login = input("Login: ").strip()
    if not login:
        print("Cant be empty!")
        return
    users = read_passwd()
    if any(u['login'] == login for u in users):
        print("User with this login already exists.")
        a = input("Edit this user? (y/n): ").strip().lower()
        if a == 'y':
            _edit_user_by_login(login)
        return

    fullname = input("LnFnSn: ").strip()
    if not fullname:
        print("LnFnSn cant be empty")
        return

    perms = input("Permissions: (r - read, w - write, d - delete (can combine)): ").strip()
    if not all(ch in 'rwd' for ch in perms):
        print(f"Oops! Only: r, w, d")
        return

    password = getpass.getpass("Password: ")
    password_confirm = getpass.getpass("Confirm password: ")
    if password != password_confirm:
        print("Oops! Isn't same")
        return

    valid, msg = validate_password(password)
    if not valid:
        print(f"Invalid pass!!: {msg}")
        return

    pwd_hash = hash_password(password)
    uid = get_next_uid(users)
    users.append({
        'login': login,
        'hash': pwd_hash,
        'uid': uid,
        'perms': perms,
        'fullname': fullname
    })
    write_passwd(users)
    logging.info(f"Added {login} (UID={uid})")
    print("User added")

# Edit user (internal: login already known)
def _edit_user_by_login(login):
    users = read_passwd()
    user = next((u for u in users if u['login'] == login), None)
    if not user:
        print("Oops! Not found")
        return

    print("Keep empty to make no change")
    new_fullname = input(f"LFS(ФИО) ({user['fullname']}): ").strip()
    if new_fullname:
        user['fullname'] = new_fullname

    new_perms = input(f"Permissions: ({user['perms']}): ").strip()
    if new_perms:
        if not all(ch in 'rwd' for ch in new_perms):
            print("Oops! Perms only: r, w, d")
            return
        user['perms'] = new_perms

    change_password = input("Change password? (y/n): ").strip().lower()
    if change_password == 'y':
        password = getpass.getpass("New password: ")
        password_confirm = getpass.getpass("Confirm password: ")
        if password != password_confirm:
            print("Oops! Not the same")
            return
        valid, msg = validate_password(password)
        if not valid:
            print(f"Invalid pass: {msg}")
            return
        user['hash'] = hash_password(password)

    write_passwd(users)
    logging.info(f"Changed user {login}")
    print("User changed")


def edit_user():
    login = input("Enter login of user: ").strip()
    if not login:
        print("Cant be empty!")
        return
    _edit_user_by_login(login)


# Delete
def delete_user():
    login = input("Enter user login: ").strip()
    users = read_passwd()
    user = next((u for u in users if u['login'] == login), None)
    if not user:
        print("User not found")
        return

    confirm = input(f"Are u sure to delete bro: {login}? (y/n): ").strip().lower()
    if confirm != 'y':
        print("Canceled")
        return

    uid = user['uid']
    users.remove(user)
    write_passwd(users)
    logging.info(f"User deleted {login} (UID={uid})")
    print("Deleted user")

def main():
    try:
        if os.geteuid() != 0:
            print("Oops! You should be root. Or use sudo python3 user_manager.py ...")
            sys.exit(1)
        os.makedirs(LOG_DIR, mode=0o700, exist_ok=True)
        if os.path.exists(AUTH_LOG):
            os.chmod(AUTH_LOG, 0o600)
    except (PermissionError, OSError) as e:
        if getattr(e, 'errno', None) == 13 or isinstance(e, PermissionError):
            print("Oops! You should be root. Use with sudo.")
        else:
            print(f"Error: {e}")
        sys.exit(1)

    if len(sys.argv) < 2:
        print("user_manager.py {add|edit|delete}")
        sys.exit(1)

    command = sys.argv[1].lower()
    if command == 'add':
        add_user()
    elif command == 'edit':
        edit_user()
    elif command == 'delete':
        delete_user()
    else:
        print(f"Unknow command: {command}")
        sys.exit(1)

if __name__ == "__main__":
    main()
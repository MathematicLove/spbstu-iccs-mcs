#!/usr/bin/env python3
import os
import sys
import getpass
import hashlib
import logging
import json
from config_DAC import (PASSWD_FILE, AUTH_LOG, LOG_DIR, HASH_ALGO,
                        PASSWORD_ALPHABET, FIRST_CHAR_ALPHABET,
                        ACL_FILE, CONFDATA_DIR)

logging.basicConfig(filename=AUTH_LOG, level=logging.INFO,
                    format='%(asctime)s - %(levelname)s - %(message)s')

def hash_password(password):
    h = hashlib.new(HASH_ALGO)
    h.update(password.encode('utf-8'))
    return h.hexdigest()


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


# passwd format (DAC): login:hash:uid:fullname
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
            login, pwd_hash, uid, sec_level, fullname = parts
            try:
                uid_int = int(uid)
            except ValueError:
                continue
            users.append({
                'login': login,
                'hash': pwd_hash,
                'uid': uid_int,
                'security_level': sec_level,
                'fullname': fullname
            })
    return users


def write_passwd(users):
    with open(PASSWD_FILE, 'w') as f:
        for u in users:
            f.write(f"{u['login']}:{u['hash']}:{u['uid']}:{u['security_level']}:{u['fullname']}\n")
    try:
        os.chmod(PASSWD_FILE, 0o600)
    except OSError:
        pass


def get_next_uid(users):
    if not users:
        return 1000
    return max(u['uid'] for u in users) + 1


def add_user():
    print("Adding new user (DAC)")
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
        'security_level': '0',
        'fullname': fullname
    })
    write_passwd(users)
    logging.info(f"Added {login} (UID={uid})")
    print("User added")

def _edit_user_by_login(login):
    users = read_passwd()
    user = next((u for u in users if u['login'] == login), None)
    if not user:
        print("Oops! Not found")
        return

    print("Keep empty to make no change")
    new_fullname = input(f"LFS ({user['fullname']}): ").strip()
    if new_fullname:
        user['fullname'] = new_fullname

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


def list_users():
    users = read_passwd()
    if not users:
        print("No users found")
        return
    print(f"{'Login':<15} {'UID':<8} {'LnFnSn'}")
    print("-" * 40)
    for u in users:
        print(f"{u['login']:<15} {u['uid']:<8} {u['fullname']}")


def read_acl():
    if not os.path.exists(ACL_FILE):
        return {}
    try:
        with open(ACL_FILE, 'r') as f:
            return json.load(f)
    except (json.JSONDecodeError, IOError):
        return {}


def show_matrix():
    users = read_passwd()
    acl_data = read_acl()

    if not users:
        print("No users found")
        return

    logins = [u['login'] for u in users]

    files = []
    if os.path.exists(CONFDATA_DIR):
        files = sorted(f for f in os.listdir(CONFDATA_DIR)
                       if os.path.isfile(os.path.join(CONFDATA_DIR, f)))
    if not files:
        print("No files in confdata/")
        return

    col_w = max(max(len(f) for f in files), 6) + 2
    login_w = max(max(len(l) for l in logins), 10) + 2

    header = "Subject".ljust(login_w)
    for f in files:
        owner = acl_data.get(f, {}).get('owner', '?')
        label = f"{f}({owner})"
        header += label.ljust(col_w + len(owner) + 2)
    print("-" * 60)
    print("DAC: Subj, Obj")
    print("-" * 60)

    col_w_real = col_w + 8
    header = "Subject".ljust(login_w)
    for f in files:
        owner = acl_data.get(f, {}).get('owner', '?')
        header += f"{f}[{owner}]".ljust(col_w_real)
    print(header)
    print("-" * (login_w + col_w_real * len(files)))

    for login in logins:
        row = login.ljust(login_w)
        for f in files:
            file_acl = acl_data.get(f, {})
            perms = file_acl.get('acl', {}).get(login, '-')
            row += perms.ljust(col_w_real)
        print(row)

    print()


def main():
    try:
        if os.geteuid() != 0:
            print("Oops! You should be root. Or use sudo python3 user_manager_DAC.py ...")
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
        print("user_manager_DAC.py {add|edit|delete|list|matrix}")
        sys.exit(1)

    command = sys.argv[1].lower()
    if command == 'add':
        add_user()
    elif command == 'edit':
        edit_user()
    elif command == 'delete':
        delete_user()
    elif command == 'list':
        list_users()
    elif command == 'matrix':
        show_matrix()
    else:
        print(f"Unknown command: {command}")
        sys.exit(1)

if __name__ == "__main__":
    main()

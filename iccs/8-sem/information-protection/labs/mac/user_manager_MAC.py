import os
import sys
import getpass
import hashlib
import logging
import json
from config_MAC import (PASSWD_FILE, AUTH_LOG, LOG_DIR, HASH_ALGO,
                        PASSWORD_ALPHABET, FIRST_CHAR_ALPHABET,
                        SECURITY_LEVELS, ACL_FILE, FILE_LABELS_FILE,
                        CONFDATA_DIR)
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

# passwd format (MAC): login:hash:uid:security_level:fullname
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
                sec_int = int(sec_level)
            except ValueError:
                continue
            users.append({
                'login': login,
                'hash': pwd_hash,
                'uid': uid_int,
                'security_level': sec_int,
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
    print("Adding new user (MAC)")
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

    print("Security levels (MAC):")
    for k, v in SECURITY_LEVELS.items():
        print(f"  {k} - {v}")
    sec_input = input("Security level (0/1/2): ").strip()
    try:
        sec_level = int(sec_input)
        if sec_level not in SECURITY_LEVELS:
            print("Invalid security level!")
            return
    except ValueError:
        print("Security level must be a number!")
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
        'security_level': sec_level,
        'fullname': fullname
    })
    write_passwd(users)
    logging.info(f"Added {login} (UID={uid}, security={SECURITY_LEVELS[sec_level]})")
    print(f"User added with security level: {SECURITY_LEVELS[sec_level]}")


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

    cur_sec = user['security_level']
    print(f"Current security level: {cur_sec} ({SECURITY_LEVELS[cur_sec]})")
    print("Security levels (MAC):")
    for k, v in SECURITY_LEVELS.items():
        print(f"  {k} - {v}")
    new_sec = input(f"Security level [{cur_sec}]: ").strip()
    if new_sec:
        try:
            sec = int(new_sec)
            if sec not in SECURITY_LEVELS:
                print("Invalid security level!")
                return
            user['security_level'] = sec
        except ValueError:
            print("Security level must be a number!")
            return

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
    print(f"{'Login':<15} {'UID':<8} {'Security':<20} {'LnFnSn'}")
    print("-" * 60)
    for u in users:
        sec_name = SECURITY_LEVELS.get(u['security_level'], 'unknown')
        print(f"{u['login']:<15} {u['uid']:<8} {sec_name:<20} {u['fullname']}")


def read_acl():
    if not os.path.exists(ACL_FILE):
        return {}
    try:
        with open(ACL_FILE, 'r') as f:
            return json.load(f)
    except (json.JSONDecodeError, IOError):
        return {}


def read_file_labels():
    if not os.path.exists(FILE_LABELS_FILE):
        return {}
    try:
        with open(FILE_LABELS_FILE, 'r') as f:
            return json.load(f)
    except (json.JSONDecodeError, IOError):
        return {}


def show_matrix():
    users = read_passwd()
    acl_data = read_acl()
    labels = read_file_labels()

    if not users:
        print("No users found")
        return

    logins = [u['login'] for u in users]
    user_levels = {u['login']: u['security_level'] for u in users}

    files = []
    if os.path.exists(CONFDATA_DIR):
        files = sorted(f for f in os.listdir(CONFDATA_DIR)
                       if os.path.isfile(os.path.join(CONFDATA_DIR, f)))
    if not files:
        print("No files in confdata/")
        return

    sec_short = {0: "U", 1: "D", 2: "S"}

    print("-" * 70)
    print("macdac):  Subj x Obj")
    print("NRU = No Read Up, NWD = No Write Down")
    print("U = unclassified, D = DSP, S = secret")
    print("-" * 70)

    col_w = 18
    login_w = 16

    header = "Subject(lvl)".ljust(login_w)
    for f in files:
        file_level = labels.get(f, 0)
        sl = sec_short.get(file_level, '?')
        header += f"{f}[{sl}]".ljust(col_w)
    print(header)
    print("-" * (login_w + col_w * len(files)))

    for login in logins:
        u_lvl = user_levels[login]
        sl = sec_short.get(u_lvl, '?')
        row = f"{login}({sl})".ljust(login_w)
        for f in files:
            file_acl = acl_data.get(f, {})
            dac_perms = file_acl.get('acl', {}).get(login, '-')
            file_level = labels.get(f, 0)
            can_r = u_lvl >= file_level
            can_w = u_lvl <= file_level
            mac_flags = ""
            if not can_r:
                mac_flags += "!R"
            if not can_w:
                mac_flags += "!W"
            cell = dac_perms
            if mac_flags:
                cell += f" {mac_flags}"
            row += cell.ljust(col_w)
        print(row)

    print()
    print("Legend: !R = MAC blocks read (NRU), !W = MAC blocks write (NWD)")
    print()


def main():
    try:
        if os.geteuid() != 0:
            print("Oops! You should be root. Or use sudo python3 user_manager_MAC.py ...")
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
        print("user_manager_MAC.py {add|edit|delete|list|matrix}")
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

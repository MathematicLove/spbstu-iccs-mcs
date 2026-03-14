import os
import sys
import getpass
import hashlib
import logging
import json
from config_MAC import (PASSWD_FILE, CONFDATA_DIR, ACCESS_LOG, LOG_DIR,
                        HASH_ALGO, PASSWORD_ALPHABET, FIRST_CHAR_ALPHABET,
                        ACL_FILE, FILE_LABELS_FILE,
                        SECURITY_LEVELS)

_SAVED_EUID = None


# Temporarily raise EUID to the owner of PASSWD_FILE (root in lab setup).
# Real UID (UID) stays unchanged.
def elevate():
    global _SAVED_EUID
    if _SAVED_EUID is not None:
        return
    try:
        st = os.stat(PASSWD_FILE)
    except OSError:
        return
    target_euid = st.st_uid
    current_euid = os.geteuid()
    if current_euid == target_euid:
        return
    _SAVED_EUID = current_euid
    try:
        os.seteuid(target_euid)
    except PermissionError:
        _SAVED_EUID = None


# Drop temporary elevated EUID back to the original one.
def drop():
    global _SAVED_EUID
    if _SAVED_EUID is None:
        return
    try:
        os.seteuid(_SAVED_EUID)
    finally:
        _SAVED_EUID = None


# Return EUID to the real user (after setuid login).
def drop_to_real():
    if os.geteuid() != os.getuid():
        try:
            os.seteuid(os.getuid())
        except (PermissionError, OSError):
            pass


def _check_passwd_access():
    uid, euid = os.getuid(), os.geteuid()
    path = PASSWD_FILE
    exists = os.path.exists(path)
    err_no_elevate = None
    try:
        with open(path, "r") as f:
            f.read(1)
    except Exception as e:
        err_no_elevate = e
    log_lines = [
        "", "-",
        f"  uid={uid}  euid={euid}  (нужен euid=0 чтобы читать файл root)",
        f"  passwd={path}  exists={exists}",
    ]
    if err_no_elevate:
        log_lines.append(f"  ошибка при открытии: {type(err_no_elevate).__name__}: {err_no_elevate}")
        log_lines.append("  С sudo: euid=0. Без sudo: euid=твой uid, файл 0600 root — доступ запрещён.")
    else:
        log_lines.append("+")
    log_lines.append("---")
    text = "\n".join(log_lines) + "\n"
    print(text, file=sys.stderr, flush=True)
    try:
        with open("/tmp/access_debug.log", "a") as f:
            f.write(text)
    except Exception:
        pass


def hash_password(password):
    h = hashlib.new(HASH_ALGO)
    h.update(password.encode('utf-8'))
    return h.hexdigest()

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
                try:
                    uid_int = int(uid)
                    sec_int = int(sec_level)
                except ValueError:
                    continue
                users[login] = {
                    'hash': pwd_hash,
                    'uid': uid_int,
                    'security_level': sec_int,
                    'fullname': fullname
                }
    except Exception:
        pass
    finally:
        drop()
    return users


def write_passwd(users):
    elevate()
    try:
        with open(PASSWD_FILE, 'w') as f:
            for login, u in users.items():
                f.write(f"{login}:{u['hash']}:{u['uid']}:{u['security_level']}:{u['fullname']}\n")
        try:
            os.chmod(PASSWD_FILE, 0o600)
        except OSError:
            pass
    finally:
        drop()

def validate_password(password):
    if len(password) < 1:
        return False, "Cannot be empty"
    if password[0] not in FIRST_CHAR_ALPHABET:
        return False, "First character must be a letter"
    for ch in password:
        if ch not in PASSWORD_ALPHABET:
            return False, f"Invalid character: {ch}"
    return True, "OK"


# acl 
def read_acl():
    if not os.path.exists(ACL_FILE):
        return {}
    try:
        with open(ACL_FILE, 'r') as f:
            return json.load(f)
    except (json.JSONDecodeError, IOError):
        return {}

def write_acl(acl_data):
    with open(ACL_FILE, 'w') as f:
        json.dump(acl_data, f, indent=2, ensure_ascii=False)
    try:
        os.chmod(ACL_FILE, 0o600)
    except OSError:
        pass

# labels
def read_file_labels():
    if not os.path.exists(FILE_LABELS_FILE):
        return {}
    try:
        with open(FILE_LABELS_FILE, 'r') as f:
            return json.load(f)
    except (json.JSONDecodeError, IOError):
        return {}

def write_file_labels(labels):
    with open(FILE_LABELS_FILE, 'w') as f:
        json.dump(labels, f, indent=2, ensure_ascii=False)
    try:
        os.chmod(FILE_LABELS_FILE, 0o600)
    except OSError:
        pass

def authenticate(users):
    while True:
        login = input("Login: ").strip()
        if not login:
            continue
        password = getpass.getpass("Password: ")
        user = users.get(login)
        if user and user['hash'] == hash_password(password):
            return user, login
        print("Oops! Wrong login or password. Try again:")


# Bell-Lapadula
def check_mac_read(subject_level, object_level):
    return subject_level >= object_level


def check_mac_write(subject_level, object_level):
    return subject_level <= object_level

# DAC utils
def check_dac(acl_data, filename, login, required_perm):
    file_acl = acl_data.get(filename)
    if not file_acl:
        return False
    user_perms = file_acl.get('acl', {}).get(login, '')
    return all(p in user_perms for p in required_perm)

def is_owner(acl_data, filename, login):
    file_acl = acl_data.get(filename)
    if not file_acl:
        return False
    return file_acl.get('owner') == login

def cmd_create(args, login, user_data):
    filename = args[0] if args else None

    while True:
        if not filename:
            filename = input("Enter filename (with extension, ugabuga.txt): ").strip()

        if not filename:
            print("Filename cannot be empty!")
            filename = None
            continue

        name_part, _, ext_part = filename.rpartition('.')
        if not name_part or not ext_part:
            print("Ooops! You must specify an extension (.txt)")
            print("Try again:")
            filename = None
            continue

        break

    if os.path.isabs(filename) or '..' in filename.split(os.sep):
        print("Invalid filename!")
        return

    filepath = os.path.join(CONFDATA_DIR, filename)
    if os.path.exists(filepath):
        print(f"File {filename} already exists!")
        return

    try:
        with open(filepath, 'w') as f:
            f.write("")
    except Exception as e:
        print(f"Error creating file: {e}")
        return

    acl_data = read_acl()
    acl_data[filename] = {
        'owner': login,
        'acl': {login: 'rwd'}
    }
    write_acl(acl_data)

    labels = read_file_labels()
    labels[filename] = user_data['security_level']
    write_file_labels(labels)

    sec_name = SECURITY_LEVELS.get(user_data['security_level'], '?')
    print(f"File '{filename}' created!")
    print(f"Owner: {login} (permissions: rwd)")
    print(f"Security label: {sec_name}")
    logging.info(f"User {login} created file {filename} (security: {sec_name})")


def cmd_read(args, login, user_data):
    if not args:
        print("Usage: read <filename>")
        return
    filename = args[0]

    if os.path.isabs(filename) or '..' in filename.split(os.sep):
        print("Invalid filename!")
        return

    filepath = os.path.join(CONFDATA_DIR, filename)
    if not os.path.exists(filepath):
        print(f"File {filename} not found")
        return

    acl_data = read_acl()
    labels = read_file_labels()

    if not check_dac(acl_data, filename, login, 'r'):
        print("ACCESS DENIED (DAC): You don't have read permission")
        logging.warning(f"DAC DENIED: {login} tried to read {filename}")
        return

    file_level = labels.get(filename, 0)
    user_level = user_data['security_level']
    if not check_mac_read(user_level, file_level):
        file_label = SECURITY_LEVELS.get(file_level, '?')
        user_label = SECURITY_LEVELS.get(user_level, '?')
        print(f"ACCESS DENIED (MAC): No Read Up - your level ({user_label}) < file level ({file_label})")
        logging.warning(f"MAC NRU DENIED: {login} (lvl {user_level}) -> {filename} (lvl {file_level})")
        return

    try:
        with open(filepath, 'r') as f:
            content = f.read()
        print(content if content else "(file is empty)")
        logging.info(f"User {login} read file {filename}")
    except Exception as e:
        print(f"Error: {e}")


def cmd_write(args, login, user_data):
    if not args:
        print("Usage: write <filename>")
        return
    filename = args[0]

    if os.path.isabs(filename) or '..' in filename.split(os.sep):
        print("Invalid filename!")
        return

    filepath = os.path.join(CONFDATA_DIR, filename)
    if not os.path.exists(filepath):
        print(f"File {filename} not found")
        return

    acl_data = read_acl()
    labels = read_file_labels()

    if not check_dac(acl_data, filename, login, 'w'):
        print("ACCESS DENIED (DAC): You don't have write permission")
        logging.warning(f"DAC DENIED: {login} tried to write {filename}")
        return

    file_level = labels.get(filename, 0)
    user_level = user_data['security_level']
    if not check_mac_write(user_level, file_level):
        file_label = SECURITY_LEVELS.get(file_level, '?')
        user_label = SECURITY_LEVELS.get(user_level, '?')
        print(f"ACCESS DENIED (MAC): No Write Down - your level ({user_label}) > file level ({file_label})")
        logging.warning(f"MAC NWD DENIED: {login} (lvl {user_level}) -> {filename} (lvl {file_level})")
        return

    print("Enter text (empty line to save):")
    lines = []
    while True:
        line = input()
        if line == "":
            break
        lines.append(line)

    if not lines:
        print("Nothing to write")
        return

    try:
        with open(filepath, 'a') as f:
            for line in lines:
                f.write(line + '\n')
        print("Data written!")
        logging.info(f"User {login} wrote to file {filename}")
    except Exception as e:
        print(f"Error: {e}")


def cmd_delete(args, login, user_data):
    if not args:
        print("Usage: delete <filename>")
        return
    filename = args[0]

    if os.path.isabs(filename) or '..' in filename.split(os.sep):
        print("Invalid filename!")
        return

    filepath = os.path.join(CONFDATA_DIR, filename)
    if not os.path.exists(filepath):
        print(f"File {filename} not found")
        return

    acl_data = read_acl()

    if not check_dac(acl_data, filename, login, 'd'):
        print("ACCESS DENIED (DAC): You don't have delete permission")
        logging.warning(f"DAC DENIED: {login} tried to delete {filename}")
        return

    try:
        os.remove(filepath)
        if filename in acl_data:
            del acl_data[filename]
            write_acl(acl_data)
        labels = read_file_labels()
        if filename in labels:
            del labels[filename]
            write_file_labels(labels)
        print(f"File {filename} deleted")
        logging.info(f"User {login} deleted file {filename}")
    except Exception as e:
        print(f"Error: {e}")


def cmd_grant(args, login, user_data):
    if len(args) < 3:
        print("Usage: grant <filename> <login> <perms>")
        print("perms: combination of r, w, d")
        return

    filename, target_login, perms = args[0], args[1], args[2]

    if not all(ch in 'rwd' for ch in perms):
        print("Permissions must be combination of: r, w, d")
        return

    acl_data = read_acl()

    if not is_owner(acl_data, filename, login):
        print("ACCESS DENIED: Only the file owner can grant permissions")
        return

    users = read_passwd()
    if target_login not in users:
        print(f"User {target_login} not found")
        return

    acl_data[filename]['acl'][target_login] = perms
    write_acl(acl_data)
    print(f"Granted '{perms}' on '{filename}' to {target_login}")
    logging.info(f"User {login} granted '{perms}' on {filename} to {target_login}")


def cmd_revoke(args, login, user_data):
    if len(args) < 2:
        print("Usage: revoke <filename> <login>")
        return

    filename, target_login = args[0], args[1]

    acl_data = read_acl()

    if not is_owner(acl_data, filename, login):
        print("ACCESS DENIED: Only the file owner can revoke permissions")
        return

    if target_login == login:
        print("Cannot revoke your own permissions (you are the owner)")
        return

    file_acl = acl_data.get(filename, {}).get('acl', {})
    if target_login not in file_acl:
        print(f"User {target_login} has no permissions on {filename}")
        return

    del acl_data[filename]['acl'][target_login]
    write_acl(acl_data)
    print(f"Revoked permissions for {target_login} on {filename}")
    logging.info(f"User {login} revoked permissions for {target_login} on {filename}")


def cmd_list(args, login, user_data):
    acl_data = read_acl()
    labels = read_file_labels()

    if not os.path.exists(CONFDATA_DIR):
        print("confdata/ does not exist")
        return

    files = [f for f in os.listdir(CONFDATA_DIR)
             if os.path.isfile(os.path.join(CONFDATA_DIR, f))]
    if not files:
        print("No files in confdata/")
        return

    user_level = user_data['security_level']
    print(f"{'File':<25} {'Owner':<12} {'Your Perms':<12} {'Security':<15} {'MAC-R':<8} {'MAC-W'}")
    print("-" * 85)
    for f in sorted(files):
        file_acl = acl_data.get(f, {})
        owner = file_acl.get('owner', '?')
        my_perms = file_acl.get('acl', {}).get(login, '-')
        file_level = labels.get(f, 0)
        sec_name = SECURITY_LEVELS.get(file_level, '?')
        can_r = "YES" if check_mac_read(user_level, file_level) else "NO"
        can_w = "YES" if check_mac_write(user_level, file_level) else "NO"
        print(f"{f:<25} {owner:<12} {my_perms:<12} {sec_name:<15} {can_r:<8} {can_w}")


def cmd_acl(args, login, user_data):
    if not args:
        print("Usage: acl <filename>")
        return
    filename = args[0]

    acl_data = read_acl()
    if filename not in acl_data:
        print(f"No ACL data for {filename}")
        return

    if not is_owner(acl_data, filename, login):
        print("ACCESS DENIED: Only the file owner can view the ACL")
        return

    file_acl = acl_data[filename]
    labels = read_file_labels()
    file_level = labels.get(filename, 0)

    print(f"File: {filename}")
    print(f"Owner: {file_acl['owner']}")
    print(f"Security label: {SECURITY_LEVELS.get(file_level, '?')}")
    print("Access Control List:")
    for user, perms in file_acl.get('acl', {}).items():
        print(f"  {user}: {perms}")


def cmd_info(args, login, user_data):
    sec_name = SECURITY_LEVELS.get(user_data['security_level'], '?')
    print(f"Login: {login}")
    print(f"Full name: {user_data['fullname']}")
    print(f"UID: {user_data['uid']}")
    print(f"Security level: {user_data['security_level']} ({sec_name})")


def cmd_edit_my_info(login, user_data):
    users = read_passwd()
    if login not in users:
        print("User not found.")
        return user_data['fullname']
    u = users[login]
    print("Edit your info (/ press enter).")
    new_fullname = input(f"Full name [{user_data['fullname']}]: ").strip()
    if new_fullname:
        u['fullname'] = new_fullname
        user_data['fullname'] = new_fullname
    change_pwd = input("Change password? (y/n): ").strip().lower()
    if change_pwd == 'y':
        password = getpass.getpass("New password: ")
        password_confirm = getpass.getpass("Confirm password: ")
        if password != password_confirm:
            print("Passwords do not match.")
            return user_data['fullname']
        valid, msg = validate_password(password)
        if not valid:
            print(f"Invalid password: {msg}")
            return user_data['fullname']
        u['hash'] = hash_password(password)
        print("Password updated.")
    write_passwd(users)
    logging.info(f"User {login} updated their info")
    print("Info updated.")
    return user_data['fullname']


def cmd_help(args):
    print("We have here:")
    print()
    print("create <filename>")
    print("read <filename> (No Read Up)")
    print("write <filename> (No Write Down)")
    print("delete <filename>")
    print("grant <file> <user> <perms> (owner only)")
    print("revoke <file> <user> (owner only)")
    print("list (files with access info)")
    print("acl <filename> (owner only)")
    print("info (user)")
    print("edit my info")
    print("help")
    print("exit")

def _setup_logging():
    elevate()
    try:
        logging.basicConfig(filename=ACCESS_LOG, level=logging.INFO,
                            format='%(asctime)s - %(levelname)s - %(message)s')
        os.makedirs(LOG_DIR, mode=0o700, exist_ok=True)
        if os.path.exists(ACCESS_LOG):
            os.chmod(ACCESS_LOG, 0o600)
    except (PermissionError, OSError):
        logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s',
                            stream=sys.stderr, force=True)
    # not drop EUID here; otherwise read_passwd() 


def main():
    _setup_logging()
    try:
        if not os.path.exists(CONFDATA_DIR):
            elevate()
            try:
                os.makedirs(CONFDATA_DIR, mode=0o700, exist_ok=True)
            finally:
                drop_to_real()
    except (PermissionError, OSError) as e:
        print(f"Error: {e}")
        sys.exit(1)

    users = read_passwd()
    if not users:
        _check_passwd_access()
        print("Not initialized. No users.")
        sys.exit(1)

    try:
        user, login = authenticate(users)
    except KeyboardInterrupt:
        print("\nExited by ^C")
        sys.exit(0)

    sec_name = SECURITY_LEVELS.get(user['security_level'], '?')
    print(f"\nInfo-Protection Lab3 (MAC), {user['fullname']}")
    print(f"Security level: {sec_name}")
    print("Type 'help' for commands\n")

    while True:
        try:
            cmd_line = input(f"{login}> ").strip()
            if not cmd_line:
                continue
            parts = cmd_line.split()
            cmd = parts[0].lower()
            args = parts[1:]

            if cmd == 'exit':
                break
            elif cmd == 'help':
                cmd_help(args)
            elif cmd == 'create':
                cmd_create(args, login, user)
            elif cmd == 'read':
                cmd_read(args, login, user)
            elif cmd == 'write':
                cmd_write(args, login, user)
            elif cmd == 'delete':
                cmd_delete(args, login, user)
            elif cmd == 'grant':
                cmd_grant(args, login, user)
            elif cmd == 'revoke':
                cmd_revoke(args, login, user)
            elif cmd == 'list':
                cmd_list(args, login, user)
            elif cmd == 'acl':
                cmd_acl(args, login, user)
            elif cmd == 'info':
                cmd_info(args, login, user)
            elif cmd == 'edit' and len(args) >= 2 and args[0] == 'my' and args[1] == 'info':
                cmd_edit_my_info(login, user)
            else:
                print(f"Unknown command: {cmd}. Type 'help'")
        except KeyboardInterrupt:
            print("\n^C exited")
            break
        except Exception as e:
            print(f"Error: {e}")

if __name__ == "__main__":
    main()

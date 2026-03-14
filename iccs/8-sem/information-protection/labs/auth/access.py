#!/usr/bin/env python3
import os
import sys
import getpass
import hashlib
import logging
import shutil
from config import PASSWD_FILE, CONFDATA_DIR, ACCESS_LOG, LOG_DIR, HASH_ALGO, PASSWORD_ALPHABET, FIRST_CHAR_ALPHABET

_SAVED_EUID = None


# Temporarily raise EUID to the owner of PASSWD_FILE (root in lab setup).
# Real UID (UID) is not changed, only EUID.
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


def hash_password(password):
    h = hashlib.new(HASH_ALGO)
    h.update(password.encode('utf-8'))
    return h.hexdigest() # hash!!!!

# Check access to passwd file and explain why it's empty.
# With sudo, euid=0 (root) so the file can be read.
# Without sudo, euid is normal user and file is 0600 root, access denied.
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
        "",
        f"  uid={uid}  euid={euid}  (нужен euid=0 чтобы читать файл root)",
        f"  passwd={path}  exists={exists}",
    ]
    if err_no_elevate:
        log_lines.append(f"  ошибка при открытии: {type(err_no_elevate).__name__}: {err_no_elevate}")
        log_lines.append("  600")
    else:
        log_lines.append(" yeeeeeesssss")
    log_lines.append("---")
    text = "\n".join(log_lines) + "\n"
    print(text, file=sys.stderr, flush=True)
    try:
        with open("/tmp/access_debug.log", "a") as f:
            f.write(text)
    except Exception:
        pass


# Read file passwd and return {login: data}
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
                login, pwd_hash, uid, perms, fullname = parts
                try:
                    uid_int = int(uid)
                except ValueError:
                    continue
                users[login] = {
                    'hash': pwd_hash,
                    'uid': uid_int,
                    'perms': perms,
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
                f.write(f"{login}:{u['hash']}:{u['uid']}:{u['perms']}:{u['fullname']}\n")
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

# Asking for login and pass. Return user data, login | none, none 
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

def check_perms(user_perms, required):
    return all(r in user_perms for r in required) # Check user have perms

def cmd_read(args, user_login, user_perms, user_fullname):
    if not args:
        print("read <filename>")
        return
    filename = args[0]
    # Secure (to not out from confdata/)
    if os.path.isabs(filename) or '..' in filename.split(os.sep):
        print("Wrong name!")
        return
    filepath = os.path.join(CONFDATA_DIR, filename)
    if not os.path.exists(filepath):
        print(f"Not found: {filename}")
        return
    if not check_perms(user_perms, 'r'):
        print("U have not permission to read ((")
        logging.warning(f"User {user_login} try to read {filename} without r-perms")
        return
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        print(content)
        logging.info(f"User {user_login} readed file {filename}")
    except Exception as e:
        print(f"Error: {e}")

def cmd_append(args, user_login, user_perms, user_fullname):
    if not args:
        print("append <filename>")
        return
    filename = args[0]
    if os.path.isabs(filename) or '..' in filename.split(os.sep):
        print("Oops! Wrong name")
        return
    filepath = os.path.join(CONFDATA_DIR, filename)
    if not os.path.exists(filepath):
        print(f"File {filename} not exist")
        return
    if not check_perms(user_perms, 'w'):
        print("U have no perms for write ((")
        logging.warning(f"User {user_login} try to write(append) in {filename} without perms!")
        return
    print("Enter your text (Empty enter to save):")
    lines = []
    while True:
        line = input()
        if line == "":
            break
        lines.append(line)
    if not lines:
        print("No data to add!")
        return
    try:
        with open(filepath, 'a') as f:
            for line in lines:
                f.write(line + '\n')
        print("Added !")
        logging.info(f"User {user_login} added data to file {filename}")
    except Exception as e:
        print(f"Error: {e}")

def cmd_copy(args, user_login, user_perms, user_fullname):
    if len(args) != 2:
        print("copy <source> <dest>")
        return
    src, dst = args
    if '..' in src.split(os.sep) or '..' in dst.split(os.sep):
        print("Wrong file name")
        return
    if not check_perms(user_perms, 'rw'):
        print("You should have 'read' and 'write' perms to copy (create)")
        logging.warning(f"User {user_login} try copy without perms")
        return

    src_path = src if os.path.isabs(src) else os.path.join(CONFDATA_DIR, src)
    dst_path = dst if os.path.isabs(dst) else os.path.join(CONFDATA_DIR, dst)

    if not os.path.exists(src_path):
        print(f"File {src} not exists")
        return
    if os.path.exists(dst_path):
        print(f"File {dst} already exists. No perms to rewrite!!!")
        return

    src_in_conf = os.path.abspath(src_path).startswith(os.path.abspath(CONFDATA_DIR))
    dst_in_conf = os.path.abspath(dst_path).startswith(os.path.abspath(CONFDATA_DIR))

    if not dst_in_conf:
        print("Error!!! Out of confdata/ folder")
        return

    try:
        shutil.copy2(src_path, dst_path)
        print(f"File copyed to {dst}")
        logging.info(f"User {user_login} copyed {src} to {dst}")
    except Exception as e:
        print(f"Error: {e}")

def cmd_remove(args, user_login, user_perms, user_fullname):
    if not args:
        print("remove <filename>")
        return
    filename = args[0]
    if os.path.isabs(filename) or '..' in filename.split(os.sep):
        print("Wrong file name")
        return
    filepath = os.path.join(CONFDATA_DIR, filename)
    if not os.path.exists(filepath):
        print(f"File {filename} not exists")
        return
    if not check_perms(user_perms, 'd'):
        print("U have not perms to delete files ((")
        logging.warning(f"User {user_login} try to delete {filename} without permissions")
        return
    try:
        os.remove(filepath)
        print(f"File {filename} deleted")
        logging.info(f"User {user_login} deleted file {filename}")
    except Exception as e:
        print(f"Error: {e}")

def cmd_perm(args, user_login, user_perms, user_fullname):
    desc = []
    if 'r' in user_perms:
        desc.append("r (read)")
    if 'w' in user_perms:
        desc.append("w (write/append)")
    if 'd' in user_perms:
        desc.append("d (delete)")
    if not desc:
        desc.append("none")
    print("Your permissions:", ", ".join(desc))
    logging.info(f"User {user_login} viewed their permissions: {user_perms}")

def cmd_edit_my_info(login, user_fullname):
    users = read_passwd()
    if login not in users:
        print("User not found.")
        return user_fullname
    u = users[login]
    print("Edit your info (press Enter to keep current).")
    new_fullname = input(f"Full name (ФИО) [{user_fullname}]: ").strip()
    if new_fullname:
        u['fullname'] = new_fullname
        user_fullname = new_fullname
    change_pwd = input("Change password? (y/n): ").strip().lower()
    if change_pwd == 'y':
        password = getpass.getpass("New password: ")
        password_confirm = getpass.getpass("Confirm password: ")
        if password != password_confirm:
            print("Passwords do not match.")
            return user_fullname
        valid, msg = validate_password(password)
        if not valid:
            print(f"Invalid password: {msg}")
            return user_fullname
        u['hash'] = hash_password(password)
        print("Password updated.")
    write_passwd(users)
    logging.info(f"User {login} updated their info (password and/or fullname)")
    print("Info updated. Changes are applied everywhere.")
    return user_fullname

def cmd_help(args):
    print("We have here:")
    print()
    print("read <filename>")
    print("append <filename>")
    print("copy <source> <dest>")
    print("remove <filename>")
    print("perm - permissions")
    print("edit my info")
    print("exit")
    print("help")

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
    # not drop EUID here it will be dropped after read_passwd()


def main():
    # sanity check: when running via setuid binary, uid should be user and euid should be 0
    u, e = os.getuid(), os.geteuid()
    print(f"uid={u} euid={e}", file=sys.stderr, flush=True)
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
        print("Not init. No users.")
        sys.exit(1)

    try:
        user, login = authenticate(users)
    except KeyboardInterrupt:
        print("\n Exited by ^C")
        sys.exit(0)

    user_perms = user['perms']
    user_fullname = user['fullname']

    print(f"Info-Protection Lab2, {user_fullname}")
    print("use help")

    while True:
        try:
            cmd_line = input("> ").strip()
            if not cmd_line:
                continue
            parts = cmd_line.split()
            cmd = parts[0].lower()
            args = parts[1:]

            if cmd == 'exit':
                break
            elif cmd == 'help':
                cmd_help(args)
            elif cmd == 'read':
                cmd_read(args, login, user_perms, user_fullname)
            elif cmd == 'append':
                cmd_append(args, login, user_perms, user_fullname)
            elif cmd == 'copy':
                cmd_copy(args, login, user_perms, user_fullname)
            elif cmd == 'remove':
                cmd_remove(args, login, user_perms, user_fullname)
            elif cmd == 'perm':
                cmd_perm(args, login, user_perms, user_fullname)
            elif cmd == 'edit' and len(args) >= 2 and args[0] == 'my' and args[1] == 'info':
                user_fullname = cmd_edit_my_info(login, user_fullname)
            else:
                print(f"Unknown command: {cmd}")
        except KeyboardInterrupt:
            print("\n ^C exited")
            break
        except Exception as e:
            print(f"Error: {e}")

if __name__ == "__main__":
    main()
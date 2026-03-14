import os

# base tree
BASE_DIR = "/Users/practice3"
PASSWD_FILE = os.path.join(BASE_DIR, "etc", "passwd")
ACL_FILE = os.path.join(BASE_DIR, "etc", "acl.json")
FILE_LABELS_FILE = os.path.join(BASE_DIR, "etc", "file_labels.json")
CONFDATA_DIR = os.path.join(BASE_DIR, "confdata")
BIN_DIR = os.path.join(BASE_DIR, "bin")
LOG_DIR = os.path.join(BASE_DIR, "log")
AUTH_LOG = os.path.join(LOG_DIR, "auth.log")
ACCESS_LOG = os.path.join(LOG_DIR, "access.log")

# Algohash
HASH_ALGO = 'sha256'

# ASCII + 1 is char
PASSWORD_ALPHABET = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()"
FIRST_CHAR_ALPHABET = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

# security levels
SECURITY_LEVELS = {
    0: "unclassified",
    1: "DSP",
    2: "secret"
}
SECURITY_LEVEL_NAMES = {v: k for k, v in SECURITY_LEVELS.items()}

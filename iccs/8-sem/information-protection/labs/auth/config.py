import os

# base tree
BASE_DIR = "/Users/practice2"
PASSWD_FILE = os.path.join(BASE_DIR, "etc", "passwd")
CONFDATA_DIR = os.path.join(BASE_DIR, "confdata")
BIN_DIR = os.path.join(BASE_DIR, "bin")
LOG_DIR = os.path.join(BASE_DIR, "log")
AUTH_LOG = os.path.join(LOG_DIR, "auth.log")
ACCESS_LOG = os.path.join(LOG_DIR, "access.log")

# Algohash 
HASH_ALGO = 'blake2b'

# ASCII + 1 is char
PASSWORD_ALPHABET = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()"
FIRST_CHAR_ALPHABET = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

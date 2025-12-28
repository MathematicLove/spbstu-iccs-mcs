#!/bin/zsh

set -xe

SRC=( daemon.c conf.c prog-args.c )
OUT=daemon

clang -std=c99 -Wall -Wextra -O2 -g -D_XOPEN_SOURCE=700 -I. $SRC -o $OUT

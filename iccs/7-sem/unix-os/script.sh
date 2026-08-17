#!/usr/bin/env bash

set -o nounset

print_usage() {
    cat <<'EOF'
    Usage:
        deep.sh [args] <dir> <comand> [args]
    Options: 
        -h, --help -- show help message
        -D <N> -- limit traversal (deep)
        -m <GLOB> -- include only files that mathc pattern
        -e <GLOB> -- exclude files matching the pattern
EOF
}

for arg in "$@"; do
    if [[ "$arg" == "--help" ]]; then
        print_usage
        exit 0
    fi
done

MAXDEPTH=""
INCLUDES=()
EXCLUDES=()

while getopts ":hD:m:e:" opt; do
    case "$opt" in
        h) print_usage; exit 0 ;;
        D)
            if [[ ! "$OPTARG" =~ ^[0-9]+$ ]]; then
                echo "Error: -D must be greater than 0, but get: 'OPTARG'" >&2
                exit 2
            fi
            MAXDEPTH="$OPTARG"
             ;;
        m) INCLUDES+=("$OPTARG") ;;
        e) EXCLUDES+=("$OPTARG") ;;
        \?) echo "Unknow argument: -$OPTARG, use --help" >&2; exit 2 ;;
        :) echo "Option -$OPTARG need arguments" >&2; exit 2 ;;
    esac
done
shift $((OPTIND-1))

if [[ $# -lt 2 ]]; then
    echo "Error: need to write correct <DIRECTORY> and <COMMAND> [ARGS]" >&2  
    echo "See: --help or -h" >&2 
    exit 2
fi

DIRECTORY=$1; shift
if [[ ! -d "$DIRECTORY" ]]; then
  echo "Error: Directory '$DIRECTORY' does not exist or is unavailable" >&2
  exit 2
fi
if [[ ! -r "$DIRECTORY" ]]; then
  echo "Error: No permission to read the directory '$DIRECTORY'" >&2
  exit 2
fi

CMD=( "$@" )
if [[ ${#CMD[@]} -eq 0 ]]; then
  echo "Error: command not specified" >&2
  exit 2
fi

if ! command -v "${CMD[0]}" >/dev/null 2>&1; then
  echo "Error: command '${CMD[0]}' not found" >&2
  exit 127
fi

FIND_ARGS=( "$DIRECTORY" -type f )
if [[ -n "$MAXDEPTH" ]]; then
  FIND_ARGS=( "$DIRECTORY" -maxdepth "$MAXDEPTH" -type f )
fi

if [[ ${#INCLUDES[@]} -gt 0 ]]; then
  FIND_ARGS+=( \( )
  for i in "${!INCLUDES[@]}"; do
    (( i > 0 )) && FIND_ARGS+=( -o )
    FIND_ARGS+=( -name "${INCLUDES[$i]}" )
  done
  FIND_ARGS+=( \) )
fi

if [[ ${#EXCLUDES[@]} -gt 0 ]]; then
  FIND_ARGS+=( \! \( )
  for i in "${!EXCLUDES[@]}"; do
    (( i > 0 )) && FIND_ARGS+=( -o )
    FIND_ARGS+=( -name "${EXCLUDES[$i]}" )
  done
  FIND_ARGS+=( \) )
fi

run_on_file() {
  local f="$1"
  "${CMD[@]}" "$f"
}

if ! command -v find >/dev/null 2>&1; then
  echo "Error: 'find' is not available." >&2
  exit 127
fi

TOTAL=0; OK=0; FAIL=0

FIFO_PATH=$(mktemp -u)
mkfifo "$FIFO_PATH" || { echo "Failed to create FIFO" >&2; exit 1; }
( find "${FIND_ARGS[@]}" -print0 > "$FIFO_PATH" ) &
FIND_PID=$!

exec 3<"$FIFO_PATH"
rm -f "$FIFO_PATH"  

while IFS= read -r -d '' FILE <&3; do
  (( TOTAL++ ))
  if run_on_file "$FILE"; then
    (( OK++ ))
  else
    (( FAIL++ ))
    printf 'The command failed with an error for file: %s\n' "$FILE" >&2
  fi
done

wait "$FIND_PID"
exec 3<&-

printf 'Done: files: %d, successful: %d, with error: %d\n' "$TOTAL" "$OK" "$FAIL"
[[ $FAIL -gt 0 ]] && exit 1

exit 0

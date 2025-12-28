#!/usr/bin/env bash

set -o nounset

print_usage() {
  cat <<'EOF'
Нужно так:
  unixScript.sh [арг] <директ> <команд> [арг команды]

Опции:
  -h Показать справку и выйти.
  -D <N> Ограничить глубину обхода (find -maxdepth N).
  -m <GLOB> Включать только файлы, совпадающие с шаблоном.
  -e <GLOB> Исключать файлы по шаблону.
  --help То же самое что -h.

Справка:
  ./unixScript.sh -h
  ./unixScript.sh --help
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
        echo "Ошибка: -D требует неотрицательное целое, получено: '$OPTARG'" >&2
        exit 2
      fi
      MAXDEPTH="$OPTARG"
      ;;
    m) INCLUDES+=("$OPTARG") ;;
    e) EXCLUDES+=("$OPTARG") ;;
    \?) echo "Неизвестная опция: -$OPTARG" >&2; exit 2 ;;
    :)  echo "Опция -$OPTARG требует аргумент" >&2; exit 2 ;;
  esac
done
shift $((OPTIND-1))

if [[ $# -lt 2 ]]; then
  echo "Ошибка: необходимо указать <DIRECTORY> и <COMMAND> [ARGS...]" >&2
  echo "Смотри: -h" >&2
  exit 2
fi

DIRECTORY=$1; shift
if [[ ! -d "$DIRECTORY" ]]; then
  echo "Ошибка: директория '$DIRECTORY' не существует или недоступна" >&2
  exit 2
fi
if [[ ! -r "$DIRECTORY" ]]; then
  echo "Ошибка: нет прав на чтение директории '$DIRECTORY'" >&2
  exit 2
fi

CMD=( "$@" )
if [[ ${#CMD[@]} -eq 0 ]]; then
  echo "Ошибка: не указана команда" >&2
  exit 2
fi

if ! command -v "${CMD[0]}" >/dev/null 2>&1; then
  echo "Ошибка: команда '${CMD[0]}' не найдена" >&2
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
  echo "Ошибка: 'find' недоступен." >&2
  exit 127
fi

TOTAL=0; OK=0; FAIL=0

FIFO_PATH=$(mktemp -u)
mkfifo "$FIFO_PATH" || { echo "Не удалось создать FIFO" >&2; exit 1; }
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
    printf 'Команда завершилась с ошибкой для файла: %s\n' "$FILE" >&2
  fi
done

wait "$FIND_PID"
exec 3<&-

printf 'Сработало: файлов: %d, успешно: %d, с ошибкой: %d\n' "$TOTAL" "$OK" "$FAIL"
[[ $FAIL -gt 0 ]] && exit 1

exit 0
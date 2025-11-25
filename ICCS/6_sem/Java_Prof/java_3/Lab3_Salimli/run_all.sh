#!/usr/bin/env bash
set -euo pipefail

THREADS=400
REQUESTS=10000

THREAD_TYPES=("virtual" "classic")
PARSERS=("own" "gson")

PORTS=(8080 8081 8082 8083)

idx=0
for thread in "${THREAD_TYPES[@]}"; do
  for parser in "${PARSERS[@]}"; do
    port=${PORTS[$idx]}

    echo "-----------------------------------------"
    echo "Starting SERVER: Threads=$thread, Parser=$parser, Port=$port"
    ./gradlew :app:runServer \
      -Dthreads="$thread" \
      -Dparser="$parser" \
      -DserverThreads="$THREADS" \
      -Dport="$port" \
      > "server-${thread}-${parser}.log" 2>&1 &
    SERVER_PID=$!

    sleep 3

    echo "Running test for $thread + $parser on port $port..."
    export JAVA_TOOL_OPTIONS="-Dthreads=$thread -Dparser=$parser"
    ./gradlew :app:run \
      --args="--threads $THREADS --requests $REQUESTS --host localhost --port $port"

    echo "Stopping server (PID $SERVER_PID)..."
    kill "$SERVER_PID"
    wait "$SERVER_PID" 2>/dev/null || true

    echo "Finished $thread + $parser"
    ((idx++))
  done
done

echo "All done!"

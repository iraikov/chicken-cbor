#!/bin/sh
# bench-large.sh - run bench-large.scm cases, one process per case,
# and report the peak resident memory of each case.
#
# Usage: bench-large.sh [MAX_N] [MODES]
#   MAX_N  largest element count to try (default 10000000)
#   MODES  space-separated list of modes (default "file memory")
#
# Environment:
#   CSC     path to csc (default csc on PATH); the benchmark is compiled
#   TMPDIR  directory for temporary output files (default /tmp)

MAX_N=${1:-10000000}
MODES=${2:-"file memory"}
CSC=${CSC:-csc}
DIR=$(dirname "$0")
BENCH=${TMPDIR:-/tmp}/cbor-bench-large.$$
"$CSC" -O3 "$DIR/bench-large.scm" -o "$BENCH" || exit 1
OUT=${TMPDIR:-/tmp}/cbor-bench.$$

run_case () {
    # $1 kind, $2 n, $3 mode, $4 compress
    /usr/bin/time -f "%M" -o "$OUT.rss" "$BENCH" \
        "$1" "$2" "$3" "$4" "$OUT" 2>&1 | grep -v '^	' | head -3 | sed 's/^/  /'
    echo "  -> peak-rss-kb=$(tail -1 "$OUT.rss")"
}

for kind in f32 f64 model; do
    for n in 10000 100000 1000000 10000000 100000000; do
        [ "$n" -gt "$MAX_N" ] && continue
        echo "== $kind n=$n"
        run_case "$kind" "$n" baseline 0
        for mode in $MODES; do
            run_case "$kind" "$n" "$mode" 0
        done
        if [ "$kind" = model ] && [ "$n" -le 10000000 ]; then
            run_case "$kind" "$n" file 1
        fi
    done
done
run_case f32 100000 truncate 0
run_case f64 1000000 truncate 0
run_case model 100000 truncate 1
rm -f "$OUT" "$OUT.rss" "$BENCH"

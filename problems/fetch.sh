#!/bin/bash -ex

MAX_PROBLEM=24
PROBLEM_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

for PROBLEM_ID in $(seq 0 $MAX_PROBLEM); do
  curl \
    -o $PROBLEM_DIR/problem_${PROBLEM_ID}.json \
    http://icfpcontest.org/problems/problem_${PROBLEM_ID}.json
done

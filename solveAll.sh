#!/bin/bash
#
#

DATE=$(date +"%y%m%dT%H%M")
OUTPUT=${1:-solving-${DATE}.json}

# 8 cores, 4G heap suggested, huge allocation area (few GCs)
RTSARGS="-N8 -H4G -A10m"

echo "Solving all problem*json in problems, output to $OUTPUT"

echo " assumed it is built, or else do (cd secretbatcave; cabal build)"
#(cd secretbatcave; cabal build)"

set -x
find problems -name "problem*json" \
    | sed -e "s;problems/; -f problems/;g" \
    | xargs secretbatcave/dist/build/solve/solve +RTS $RTSARGS -RTS \
            > $OUTPUT

echo "Done"

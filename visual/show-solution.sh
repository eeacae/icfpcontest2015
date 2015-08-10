#!/bin/bash -ex

DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
DUMP=$DIR/../secretbatcave/dist/build/dump/dump
FRAMES=$DIR/js/frames.js
INDEX=$DIR/index.html

echo -n "var frames = " > $FRAMES
$DUMP florida $@ >> $FRAMES

if which xdg-open &> /dev/null; then
    xdg-open $INDEX
else
    open $INDEX
fi

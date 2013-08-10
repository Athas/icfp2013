#!/bin/sh

for x in `cat src/sorted1`; do
    ./src/command.py solve $x
done

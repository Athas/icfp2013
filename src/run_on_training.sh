#!/bin/sh

for x in `./src/command.py trainids`; do
    ./src/command.py solve $x
    read
done

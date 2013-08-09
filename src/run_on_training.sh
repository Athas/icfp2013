#!/bin/sh

for x in `./src/command.py trainids`; do
    eval "./src/command.py solve $x &"
    pid=$!
    read a
    kill $pid
done

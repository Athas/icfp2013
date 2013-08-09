#!/usr/bin/env python

import subprocess
import time


def generate_train():
    try:
        xs = subprocess.Popen(['./src/api.py', 'train'],
                              stdout=subprocess.PIPE).communicate()[0].split('\n')
        if xs[0] == 'json':
            return xs[1]
    except Exception:
        return

def generate_trains():
    while True:
        for i in range(0, 5):
            t = generate_train()
            if t is not None:
                print t
        time.sleep(21)

if __name__ == '__main__':
    generate_trains()

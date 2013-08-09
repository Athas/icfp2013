#!/usr/bin/env python

import sys
import random
import subprocess
import api
import json
import numpy


def load_problems(path):
    with open(path) as f:
        probs = json.load(f)
    probs1 = {}
    for row in probs:
        probs1[row['id']] = row
    return probs1

problems = load_problems('src/myproblems.json')
problems_training = load_problems('src/myproblems_training.json')

def get_inputs(ops):
    # if 'tfold' in ops:
    return map(lambda x: x + 2**6 - 1, random.sample(xrange(0, 2**63 - 1), 128)) + random.sample(xrange(0, 2**63 - 1), 128)
    # else:
    #     return random.sample(xrange(0, 256), 64) \
    #         + random.sample(xrange(2**64 - 256, 2**64), 64) \
    #         + random.sample(xrange(256, 2**63), 64) \
    #         + map(lambda x: x + 2**63 - 256, random.sample(xrange(256, 2**63), 64))

def get_problem(id):
    try:
        return problems[id.decode()]
    except KeyError:
        # is a training problem
        return problems_training[id.decode()]

def unhex(t):
    return int(t[2:], 16)
        
def solve(auth, id):
    prob = get_problem(id)
    size = prob['size']
    ops = map(lambda s: s.encode(), prob['operators'])
    arguments = list(map(hex, get_inputs(ops)))

    print 'Getting outputs.'
    outputs = []
    for i in range(0, len(arguments), 256):
        arguments1 = arguments[i:i + 256]
        (t, d) = api.eval(auth, id, *arguments1)
        if t == 'error':
            api.error(d)
            return
        j = json.loads(d)
        if j['status'] == 'error':
            api.error(j['message'])
            return
        elif j['status'] == 'ok':
            outputs.extend(j['outputs'])

    maps = zip(map(unhex, arguments), map(unhex, outputs))
    cmd = ['./src/Main', 'solve', str(size), str(ops).replace("'", '"'), str(maps).replace('L', '')]
    print 'Command:', cmd
    out, err = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()

    out += err
    out = out.strip()
    if out == "No solution found" or out == "Malformed input":
        api.error(out)
        print 'NO SOLUTION FOUND!  TIME IS RUNNING OUT!'
        return
    prog = out.strip()
    print 'Program found: ' + prog
    (t, d) = api.guess(auth, id, prog)
    if t == 'error':
        api.error(d)
        print 'GUESSING WENT WRONG!  TIME IS RUNNING OUT!'
        return
    elif t == 'json':
        print d
        j = json.loads(d)
        if j['status'] == 'win':
            print 'YOU WON! ' * 10
            with open('wins', 'w') as f:
                f.write(id + '\n')

def print_help():
    print '''usage: ./command.py COMMAND [ARGS] -> STATUS

Commands:
  solve ID\
'''

def run_main():
    auth = api.get_authkey()
    if auth is None:
        print_help()
        return

    try:
        args = sys.argv[2:]
        (min_args, f) = {
            'solve': (1, lambda: solve(auth, args[0])),
        }[sys.argv[1]]
        if min_args > len(args):
            raise IndexError
    except IndexError:
        print_help()
        return

    f()

if __name__ == '__main__':
    run_main()

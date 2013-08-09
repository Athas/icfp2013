#!/usr/bin/env python

import time
import os
import sys
import random
import subprocess
import api
import json
from multiprocessing import Process


def load_problems(path):
    with open(path) as f:
        probs = json.load(f)
    probs1 = {}
    for row in probs:
        probs1[row['id']] = row
    return probs1

problems = load_problems('src/myproblems.json')
problems_training = load_problems('src/myproblems_training.json')

def offset(n, xs):
    return map(lambda x: x + n, xs)

def get_inputs(ops):
    if 'tfold' in ops:
        return random.sample(xrange(0, 2**63 - 1), 128) \
            + offset(2**63 - 1, random.sample(xrange(0, 2**63 - 1), 128))
    else:
        return random.sample(xrange(0, 256), 64) \
            + offset(2**64 - 257, random.sample(xrange(0, 256), 64)) \
            + offset(256, random.sample(xrange(0, 2**63 - 1), 128)) \
            + offset(2**63 - 257, random.sample(xrange(0, 2**63 - 1), 128))

def get_problem(id):
    try:
        return problems[id.decode()]
    except KeyError:
        # is a training problem
        return problems_training[id.decode()]

def unhex(t):
    return int(t[2:], 16)

pids = []
def solve(auth, id):
    prob = get_problem(id)
    size = prob['size']
    ops = map(lambda s: s.encode(), prob['operators'])
    print 'ID:', id
    print 'Size:', size
    print 'Ops:', ops
    arguments = list(map(lambda n: hex(n).replace('L', ''), get_inputs(ops)))

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
    print 'All necessary data acquired.  Starting guessing!'

    troels = Process(target=run_troels, args=(auth, id, size, ops, maps))
    genetic = Process(target=run_genetic, args=(auth, id, size, ops, arguments, outputs))
    troels.start()
    genetic.start()

    raw_input()
    for p in pids:
        os.kill(pid, 9)
    troels.terminate()
    genetic.terminate()
    
    # timer = Process(target=timer_func)
    # timer.start()

def timer_func():
    k = 10
    t = 0
    while True:
        time.sleep(k)
        t += k
        print 'TIME', t

def format_c_array(xs):
    return '{' + ', '.join(map(str, xs)) + '}'
    
def run_genetic(auth, id, size, ops, arguments, outputs):
    callback = lambda inp, outp: run_genetic(
        auth, id, size, ops, arguments + [hex(inp)], outputs + [hex(outp)])
    run_genetic1(auth, id, size, ops, arguments, outputs, callback)

def run_genetic1(auth, id, size, ops, arguments, outputs, callback):
    ops = set(ops)
    if 'tfold' in ops:
        ops.discard('fold')
        ops.discard('arg')
        ops.discard('tfold')
        ops.add('acc')
        ops.add('byte')
        extra = '#define TFOLD\n'
    else:
        extra = ''
    if 'fold' in ops:
        ops.add('arg')
        ops.add('acc')
        ops.add('byte')
    ops.add('zero')
    ops.add('one')
    ops.add('arg')
    ops_arr = format_c_array(map(lambda op: op[0].upper() + op[1:], ops))
    values_arr = format_c_array(arguments)
    results_arr = format_c_array(map(lambda x: x.encode(), outputs))
    data = '''\
%s#define PROGSIZE %d
term_t ok[] = %s;
uint64_t test_values[]  = %s;
uint64_t test_results[] = %s;
#define RETRY_TIME 10
    ''' % (extra, size, ops_arr, values_arr, results_arr)
    # print 'data.h:\n'
    # print data
    with open('src/data.h', 'w') as f:
        f.write(data)
    subprocess.Popen(['make', '-C', 'src'])
    p = subprocess.Popen(['./src/genetic'], stdout=subprocess.PIPE)
    pids.append(p.pid)
    out = p.communicate()[0]
    prog = out.strip()
    guess_program(auth, id, prog, 'GENERNE', callback)

def run_troels(auth, id, size, ops, maps):
    callback = lambda inp, outp: run_troels(auth, id, size, ops,
                                           maps + [(inp, outp)])
    run_troels1(auth, id, size, ops, maps, callback)

def run_troels1(auth, id, size, ops, maps, callback):
    cmd = ['./src/Main', 'solve', str(size), str(ops).replace("'", '"'), str(maps).replace('L', '')]
    # print 'Command:', cmd
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    pids.append(p.pid)
    out, err = p.communicate()

    out += err
    out = out.strip()
    if out == "No solution found" or out == "Malformed input":
        api.error(out)
        print 'NO SOLUTION FOUND!  TIME IS RUNNING OUT!'
        return
    prog = out.strip()

    guess_program(auth, id, prog, 'TROELS', callback)

def guess_program(auth, id, prog, source, expand_search):
    print 'Program found: ' + prog
    (t, d) = api.guess(auth, id, prog)
    if t == 'error':
        api.error(d)
        print source + ' GUESSING WENT WRONG!  TIME IS RUNNING OUT!'
        return
    elif t == 'json':
        print d
        j = json.loads(d)
        if j['status'] == 'win':
            print (source + ' WON! ') * 10
            with open('wins', 'w') as f:
                f.write(id + '\n')
            
        elif j['status'] == 'mismatch':
            print 'MISMATCH.  REDOING.'
            inp, chal_res, guess = j['values']
            expand_search(unhex(inp.encode()), unhex(chal_res.encode()))

def trainids():
    with open('src/myproblems_training.json') as f:
        problems = json.load(f)

    for x in problems:
        print x['id']

def print_help():
    print '''usage: ./command.py COMMAND [ARGS] -> STATUS

Commands:
  solve ID\
  trainids
'''

def run_main():
    auth = api.get_authkey()
    if auth is None:
        print_help()
        return

    try:
        args = sys.argv[2:]
        try:
            (min_args, f) = {
                'solve': (1, lambda: solve(auth, args[0])),
                'trainids': (0, trainids),
            }[sys.argv[1]]
        except KeyError:
            raise IndexError
        if min_args > len(args):
            raise IndexError
    except IndexError:
        print_help()
        return

    f()

if __name__ == '__main__':
    run_main()

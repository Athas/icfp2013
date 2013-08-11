# Stupid solution.  Should have used select or something similar.

import time
import os
import sys
import random
import json
import thread
import threading
import subprocess
import shutil
import tempfile

import lib.api as api
from lib.api import error

mismatcher_allowed = None
mismatcher = threading.Lock()
guess_lock = threading.Lock()
finish_event = threading.Event()
exit_code = 0
has_won = False
worst_case_n_losses = 0
n_losses = 0

# PIDs of processes to be killed
pids = []

def get_exit_code():
    return exit_code

def set_exit_code(c):
    global exit_code
    exit_code = c

def run_brute(*args):
    return _run_brute(create_callback(run_brute, *args), *args)

def run_dybber(*args):
    return _run_dybber(create_callback(run_dybber, *args), *args)

def run_polar(*args):
    return _run_polar(create_callback(run_polar, *args), *args)

def run_genetic(*args):
    return _run_genetic(create_callback(run_genetic, *args), *args)

def run_geneticham(*args):
    return _run_geneticham(create_callback(run_geneticham, *args), *args)
        
def with_ki(f):
    def g(*args):
        try:
            f(*args)
        except (KeyboardInterrupt, EOFError):
            exit_all(3)
    return g

solvers = {
    'brute': with_ki(run_brute),
    'dybber': with_ki(run_dybber),
#    'polar': with_ki(run_polar),
    'genetic': with_ki(run_genetic),
    'geneticham': with_ki(run_geneticham),
}

def offset(n, xs):
    return map(lambda x: x + n, xs)

def get_inputs(ops):
    if 'tfold' in ops:
        return random.sample(xrange(0, 2**63 - 1), 128) \
            + offset(2**63 - 1, random.sample(xrange(0, 2**63 - 1), 128))
    else:
        return random.sample(xrange(0, 256), 64) \
            + offset(2**64 - 257, random.sample(xrange(0, 256), 64)) \
            + offset(256, random.sample(xrange(0, 2**63 - 1), 64)) \
            + offset(2**63 - 257, random.sample(xrange(0, 2**63 - 1), 64))

def unhex(s):
    return int(s[2:], 16)

def unlong(s):
    return s.replace('L', '')

def lhex(s):
    return unlong(hex(s))

def exit_all(c=0):
    for pid in pids:
        try:
            os.kill(pid, 9)
        except OSError:
            continue
    set_exit_code(c)
    sys.exit(c)

def solve(*args):
    try:
        return _solve(*args)
    except (KeyboardInterrupt, EOFError):
        exit_all(1)
    
def _solve(auth, path, path2, id, local_solvers):
    global worst_case_n_losses
    worst_case_n_losses = len(local_solvers)
    
    with open(path2) as f:
        probs = json.load(f)
    problems = {}
    for row in probs:
        problems[row['id']] = row

    prob = problems[id.decode()]
    size = prob['size']
    ops = map(lambda s: s.encode(), prob['operators'])
    print
    print 'id:', id, '| size:', size, '| ops:', ops
    inputs = get_inputs(ops)
    arguments = map(lambda n: lhex(n), inputs)
    print 'Starting acquiring data.  Will ask for %d outputs.' % len(arguments)

    outputs = []
    for i in range(0, len(arguments), 256):
        arguments1 = arguments[i:i + 256]
        (t, d) = api.eval(auth, id, *arguments1)
        if t == 'error':
            error(api.response_text(d))
            if d == api.PROBLEM_ALREADY_SOLVED:
                exit_all()
            return
        j = json.loads(d)
        if j['status'] == 'error':
            error(j['message'])
            return
        elif j['status'] == 'ok':
            outputs.extend(map(unhex, j['outputs']))

    with open('data/input-outputs-%s' % id, 'w') as f:
        f.write(str(inputs) + '\n' + str(outputs))

    print 'All necessary data acquired.  Starting guessing!'

    args = [auth, id, size, ops, path, inputs, outputs]

    funcs = []
    for m in local_solvers:
        try:
            func = solvers[m]
        except KeyError:
            continue
        funcs.append((func, m))

    for f, m in funcs:
        threading.Thread(target=f, args=args + [m]).start()
    # t = threading.Thread(target=with_ki(counter))
    # t.start()

    finish_event.wait()

def counter():
    start = time.time()
    while True:
        time.sleep(10)
        print time.time() - start
    
def remove_file_line(path, id):
    with open(path) as f:
        lines = f.read().split('\n')
    try:
        lines.remove(id)
    except ValueError:
        pass
    with open(path, 'w') as f:
        f.write('\n'.join(lines))

def guess_program(auth, id, prog, name, path, expand_search, solver_pid):
    global has_won, n_losses, mismatcher_allowed

    mismatcher.acquire()
    if mismatcher_allowed is None:
        mismatcher_allowed = thread.get_ident()
        kill_other_programs(solver_pid)
    mismatcher.release()
    if not thread.get_ident() == mismatcher_allowed:
        return

    print 'Program found from %s: %s' % (name, prog)
    has_won_now = False
    guess_lock.acquire()
    if not has_won:
        (t, d) = api.guess(auth, id, prog)
        if t == 'json' and json.loads(d)['status'] == 'win':
            has_won = True
            has_won_now = True
    guess_lock.release()
    if has_won and not has_won_now:
        api.error('You have solved this.')
        exit_all()
        return
    if t == 'error':
        error(api.response_text(d))
        if d == api.PROBLEM_ALREADY_SOLVED:
            exit_all()
            return
        n_losses += 1
        if n_losses == worst_case_n_losses:
            set_exit_code(1)
            lost_or_won()
    elif t == 'json':
        print d
        j = json.loads(d)
        if j['status'] == 'win':
            has_won = True
            win_guess = lambda auth, id, prog: ('error', '')
            print((name + ' WON! ') * 10)
            remove_file_line(path, id)
            lost_or_won()
            exit_all()
        elif j['status'] == 'mismatch':
            if mismatcher_allowed == thread.get_ident():
                print 'Mismatch detected on %s.  Killing other programs.  Resolving.' % name
                inp, chal_res, guess = j['values']
                expand_search(unhex(inp.encode()), unhex(chal_res.encode()))

def kill_other_programs(than_pid):
    for pid in pids:
        if pid == than_pid:
            continue
        try:
            os.kill(pid, 9)
        except OSError:
            continue
            
def lost_or_won():
    finish_event.set()


def format_c_array(xs):
    return '{' + ', '.join(map(str, xs)) + '}'

def looks_lambda(s):
    return s.startswith('(')
    
def create_callback(f, auth, id, size, ops, path, inputs, outputs, name):
    return lambda inp, outp: f(auth, id, size, ops, path,
                               inputs + [inp], outputs + [outp], name)


def _run_troels_input(actionName, callback, auth, id, size, ops, path, inputs, outputs, name):
    maps = zip(inputs, outputs)
    cmd = ['./solvers/MiscInterface', actionName,
           str(size), str(ops).replace("'", '"'), unlong(str(maps))]
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    pids.append(p.pid)
    out = p.communicate()[0].strip()
    if looks_lambda(out):
        guess_program(auth, id, out, name, path, callback, p.pid)
    else:
        error('No solution found from %s' % name)
        global n_losses
        n_losses += 1
        if n_losses == worst_case_n_losses:
            set_exit_code(1)
            lost_or_won()
            
def _run_brute(*args):
    return _run_troels_input('solve', *args)

def _run_dybber(*args):
    return _run_troels_input('dybbersolve', *args)
    
def _run_polar(*args):
    return _run_troels_input('polarsolve', *args)

def generate_datah_from_id(id):
    with open('data/input-outputs-%s' % str(id)) as f:
        d = f.read().split('\n', 1)
    inps = eval(d[0])
    outps = eval(d[1])

    with open('data/myproblems.json') as f:
        probs = json.load(f)
    problems = {}
    for row in probs:
        problems[row['id']] = row

    size = problems[id]['size']
    ops = problems[id]['operators']
    data = generate_datah(size, ops, inps, outps)
    print data

def generate_datah(size, ops, inputs, outputs, fit_type='NUMDIFF'):
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
    values_arr = format_c_array(map(lambda x: lhex(x), inputs))
    results_arr = format_c_array(map(lambda x: lhex(x), outputs))
    
    data = '''\
%s#define %s
#define PROGSIZE %d
term_t ok[] = %s;
uint64_t test_values[]  = %s;
uint64_t test_results[] = %s;
#define RETRY_TIME 10
    ''' % (extra, fit_type, size, ops_arr, values_arr, results_arr)
    return data

def _run_genetic(*args):
    _run_genetic_generic('NUMDIFF', *args)

def _run_geneticham(*args):
    _run_genetic_generic('BITDIFF', *args)
    
def _run_genetic_generic(fit_type, callback, auth, id, size, ops, path, inputs, outputs, name):
    data = generate_datah(size, ops, inputs, outputs, fit_type)
    tpath = tempfile.mkdtemp()
    tpath = os.path.join(tpath, 'genetic')
    shutil.copytree('solvers/genetic', tpath)
    with open(os.path.join(tpath, 'data.h'), 'w') as f:
        f.write(data)
    subprocess.Popen(['make', '-C', tpath],
                     stdout=subprocess.PIPE).communicate()
    p = subprocess.Popen([os.path.join(tpath, 'genetic')], stdout=subprocess.PIPE)
    pids.append(p.pid)
    out = p.communicate()[0].strip()
    shutil.rmtree(tpath)
    guess_program(auth, id, out, name, path, callback, p.pid)

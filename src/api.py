#!/usr/bin/env python

import sys
import json
import urllib

base_url = "http://icfpc2013.cloudapp.net/"

def request(auth, path, body=None):
    url = base_url + path + "?auth=" + auth + "vpsH1H"
    r = urllib.urlopen(url, body)
    return (r.getcode(), r.read())

def mark(m, s):
    return m + '\n' + s

def as_json(s):
    return mark('json', s)

def as_error(s):
    return mark('error', s)

default_responses = {
    400: lambda: as_error('bad request'),
    401: lambda: as_error('problem was not requested by the current user'),
    403: lambda: as_error('authorization required'),
    404: lambda: as_error('no such challenge'),
    410: lambda: as_error('problem requested more than 5 minutes ago'),
    412: lambda: as_error('problem was already solved (by current user)'),
    413: lambda: as_error('request too big'),
    429: lambda: as_error('try again later')
}

def json_or_error(auth, path, body=None):
    (code, text) = request(auth, path, body)
    rets = {}
    rets[200] = lambda: as_json(text)
    rets.update(default_responses)
    return rets[code]()

def myproblems(auth):
    return json_or_error(auth, 'myproblems')

def make_eval_json(id, program, *arguments):
    j = {}
    j['id'] = int(id)
    j['program'] = program
    j['arguments'] = json.loads(arguments)
    return json.dumps(j)

def eval(auth, *args):
    return as_error('DISABLED')
#    return json_or_error(auth, 'eval', make_eval_json(*args))

def make_guess_json(id, program):
    j = {}
    j['id'] = int(id)
    j['program'] = program
    return json.dumps(j)

def guess(auth, *args):
    return as_error('DISABLED')
#    return json_or_error(auth, 'guess', make_guess_json(*args))

def make_train_json(size=None, operators=None):
    j = {}
    if size is not None:
        j['size'] = int(size)
    if operators is not None:
        j['operators'] = operators
    return json.dumps(j)

def train(auth, *args):
    return json_or_error(auth, 'train', make_train_json(*args))

def status(auth):
    return json_or_error(auth, 'status')

def error(s):
    print >> sys.stderr, 'error: ' + s

def print_help():
    print '''usage: ./api.py COMMAND [ARGS] -> TYPE\\nDATA
  where TYPE = 'json' | 'error'

Your auth key must reside in the 'authkey' file.

Commands:
  myproblems
  eval ID PROGRAM ARGUMENT...
  guess ID PROGRAM
  train [SIZE [OPERATORS]]
  status\
'''

def run_main():
    try:
        with open('authkey') as f:
            auth = f.read().strip()
    except IOError:
        error("missing 'authkey' file'")
        print_help()
        return

    try:
        args = sys.argv[2:]
        (min_args, f) = {
            'myproblems': (0, lambda: myproblems(auth)),
            'eval': (3, lambda: eval(auth, *args)),
            'guess': (2, lambda: guess(auth, args[0], args[1])),
            'train': (0, lambda: train(auth, *args[0:2])),
            'status': (0, lambda: status(auth))
        }[sys.argv[1]]
        if min_args > len(args):
            raise IndexError
    except IndexError:
        print_help()
        return

    print f()

if __name__ == '__main__':
    run_main()

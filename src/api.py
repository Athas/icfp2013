#!/usr/bin/env python

import sys
import json
import urllib

base_url = "http://icfpc2013.cloudapp.net/"

def request(auth, path, body):
    url = base_url + path + "?auth=" + auth + "vpsH1H"
    r = urllib.urlopen(url, body)
    return (r.getcode(), r.read())

def mark(m, s):
    return m + '\n' + s
    
def as_json(s):
    return mark('json', s)

def as_error(s):
    return mark('error', s)
    
def make_train_json(size=None, operators=None):
    j = {}
    if size:
        j['size'] = int(size)
    if operators:
        j['operators'] = operators
    return json.dumps(j)

def train(auth, size=None, operators=None):
    (code, text) = request(auth, "train", make_train_json(size, operators))
    return {
        200: lambda: as_json(text),
        400: lambda: as_error('bad request'),
        403: lambda: as_error('authorization required'),
        429: lambda: as_error('try again later')
    }[code]()

def error(s):
    print >> sys.stderr, 'error: ' + s
    
def print_help():
    print '''usage: ./api.py COMMAND [ARGS] -> TYPE\\nDATA
  where TYPE = 'json' | 'error'

Your auth key must reside in the 'authkey' file.
    
Commands:
  train [SIZE [OPERATORS]]\
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
            'train': (0, lambda: train(auth, *args[0:2]))
        }[sys.argv[1]]
        if min_args > len(args):
            raise IndexError
    except IndexError:
        print_help()
        return

    print f()
    
if __name__ == '__main__':
    run_main()

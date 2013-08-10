import sys
import json
import urllib
import time


BASE_URL = 'http://icfpc2013.cloudapp.net/'

def request(auth, path, body=None):
    url = BASE_URL + path + '?auth=' + auth + 'vpsH1H'
    r = urllib.urlopen(url, body)
    d = r.read()
    return (r.getcode(), d)

# Response codes
BAD_REQUEST = 400
NOT_REQUESTED_BY_CURRENT = 401
AUTHORIZATION_REQUIRED = 403
NO_SUCH_CHALLENGE = 404
PROBLEM_REQUESTED_5MIN_AGO = 410
PROBLEM_ALREADY_SOLVED = 412
REQUEST_TOO_BIG = 413
TRY_AGAIN = 429

def response_text(r):
    try:
        return {
            BAD_REQUEST: 'bad request',
            NOT_REQUESTED_BY_CURRENT: 'problem was not requested by the current user',
            AUTHORIZATION_REQUIRED: 'authorization required',
            NO_SUCH_CHALLENGE: 'no such challenge',
            PROBLEM_REQUESTED_5MIN_AGO: 'problem requested more than 5 minutes ago',
            PROBLEM_ALREADY_SOLVED: 'problem was already solved (by current user)',
            REQUEST_TOO_BIG: 'request too big',
            TRY_AGAIN: 'try again later',
        }[r]
    except KeyError:
        return

def as_json(s):
    return ('json', s)

def as_error(s):
    return ('error', s)

default_responses = {
    BAD_REQUEST: lambda n: as_error(n),
    NOT_REQUESTED_BY_CURRENT: lambda n: as_error(n),
    AUTHORIZATION_REQUIRED: lambda n: as_error(n),
    NO_SUCH_CHALLENGE: lambda n: as_error(n),
    PROBLEM_REQUESTED_5MIN_AGO: lambda n: as_error(n),
    PROBLEM_ALREADY_SOLVED: lambda n: as_error(n),
    REQUEST_TOO_BIG: lambda n: as_error(n),
    TRY_AGAIN: lambda n: as_error(n),
}

def error(s):
    print >> sys.stderr, 'error:', s


def get_authkey():
    try:
        with open('authkey') as f:
            auth = f.read().strip()
        return auth
    except IOError:
        error('missing file "authkey"')


def json_or_error(auth, path, body=None):
    while True:
        (code, text) = request(auth, path, body)
        if code == TRY_AGAIN:
            print 'Rate limiting.  Trying again in 1 second!'
            time.sleep(1)
        else:
            break
    rets = {}
    rets.update(default_responses)
    rets[200] = lambda n: as_json(text)
    try:
        f = rets[code]
    except KeyError:
        return as_error('unknown return code: %d' % code)
    return f(code)


def myproblems(auth):
    return json_or_error(auth, 'myproblems')


def make_eval_json(id, *arguments):
    j = {}
    j['id'] = id
    j['arguments'] = arguments
    return json.dumps(j)

def eval(auth, *args):
    return json_or_error(auth, 'eval', make_eval_json(*args))


def make_evalprog_json(program, *arguments):
    j = {}
    j['program'] = program
    j['arguments'] = arguments
    return json.dumps(j)

def evalprog(auth, *args):
    return json_or_error(auth, 'eval', make_evalprog_json(*args))


def make_guess_json(id, program):
    j = {}
    j['id'] = id
    j['program'] = program
    return json.dumps(j)

def guess(auth, *args):
    return json_or_error(auth, 'guess', make_guess_json(*args))


def make_train_json(size=None, operators=None):
    j = {}
    if size is not None:
        j['size'] = int(size)
    if operators is not None:
        j['operators'] = json.dumps([operators])
    return json.dumps(j)

def train(auth, *args):
    return json_or_error(auth, 'train', make_train_json(*args))


def status(auth):
    return json_or_error(auth, 'status')

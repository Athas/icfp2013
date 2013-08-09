#!/usr/bin/env python

import sys
from pylab import *


def plot_pairs(name, pairs):
    xs = map(lambda t: t[0], pairs)
    ys = map(lambda t: t[1], pairs)
    plot(xs, ys)
    xlabel('x')
    ylabel('f(x)')
    title(name)
    show()

if __name__ == '__main__':
    name = sys.argv[1]
    pairs = map(eval, sys.argv[2:])
    plot_pairs(name, pairs)

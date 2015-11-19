#!/usr/bin/python
"""
Plots interpolated atmospheric model curve (hopefully) in the middle of the curves
used by innewmarcs for interpolation.

The file containing which files and records innewmarcs chose is assumed to be called
'innewmarcs_explain.txt'
"""

from __future__ import print_function
import argparse
from pyfant import *
import matplotlib.pyplot as plt
import re
import os.path
import numpy as np
from itertools import cycle
from mpl_toolkits.mplot3d import Axes3D  # yes, required

def parse_explain_file(filename):
    """Extracts information from "explain" file created by innewmarcs."""
    tokens = {}
    with open(filename) as h:
        while True:
            s = h.readline().strip()
            if not s:
                break
            m = re.match(r"(\w+):\s*(.*)", s)
            if not m:
                raise RuntimeError('Error parsing file "%s"' % filename)
            token, s_data = m.groups()
            if not token in tokens:
                tokens[token] = []
            tokens[token].append(eval(s_data))
    return tokens


FN_EXPLAIN = 'innewmarcs_explain.txt'
VARS = ['nh', 'teta', 'pe', 'pg', 't5l']


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    parser.add_argument('--inum', type=int, default=1, help='Record number (>= 1)')
    parser.add_argument('--var', type=str, help='Variable to plot', default=VARS[0], choices=VARS)
    parser.add_argument('fn', type=str, help='.mod binary file name created by innewmarcs', default='modeles.mod', nargs='?')

    args = parser.parse_args()


    locatab = parse_explain_file(FN_EXPLAIN)["locatab"]
    records = []  # ModRecord, name
    for fn, inum in locatab:
        m = FileMod()
        m.load(fn)
        record_name = '%s#%s' % (os.path.basename(fn), inum)
        records.append((m.records[inum-1], record_name))

    mod = FileMod()
    mod.load(args.fn)
    rm = mod.records[0]

    print_record = lambda record, name: print(name, '; teff=', record.teff, '; glog=', record.glog, '; asalog=', record.asalog)

    # Figure
    plt.figure()
    for record, name in records:
        print_record(record, name)
        plt.plot(record.__getattribute__(args.var), label=name)
    name = args.fn
    v = rm.__getattribute__(args.var)
    if any(np.isnan(v)):
        print("NaN ALERT!!!")
    print_record(rm, name)
    plt.plot(v, label=name, linewidth=2, linestyle='--')
    plt.xlabel('Layer')
    plt.title(args.var)
    plt.legend(loc=0)

    # Another figure
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    markers = cycle("ov^<>p*+x")
    for r, name in records:
        ax.scatter([r.teff], [r.glog], [r.asalog],
                   c=ax._get_lines.color_cycle.next(), s=60,
                   label=name, marker=markers.next())
    ax.scatter([rm.teff], [rm.glog], [rm.asalog],
               c=ax._get_lines.color_cycle.next(), s=60,
               label=args.fn, marker=markers.next())
    ax.set_xlabel('teff')
    ax.set_ylabel('glog')
    ax.set_zlabel('asalog')
    fig.canvas.set_window_title(args.var)
    plt.legend(loc=0)

    # Another figure
    fig = plt.figure()
    y_var_names = ["teff", "glog", "asalog"]
    for i, y_var_name in enumerate(y_var_names):
        ax = fig.add_subplot(len(y_var_names), 1, i+1, projection='3d')
        x = None
        for record, name in records:
            print_record(record, name)
            v = record.__getattribute__(args.var)
            if x is None:
                x = np.arange(0, len(v))
            y_var = record.__getattribute__(y_var_name)
            y = np.ones((len(x),))*y_var
            ax.plot(x, y, v, label=name)

        name = args.fn
        v = rm.__getattribute__(args.var)
        x = np.arange(0, len(v))
        y_var = rm.__getattribute__(y_var_name)
        y = np.ones((len(x),))*y_var
        print_record(rm, name)
        plt.plot(x, y, v, label=name, linewidth=2, linestyle='--')

        plt.xlabel('Layer')
        plt.ylabel(y_var_name)
        plt.title(args.var)
        plt.legend(loc=0)

    plt.show()


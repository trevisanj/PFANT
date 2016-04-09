#!/usr/bin/python

"""
Model Grid Creator

Collects several files and creates a grid in ".mog" format.

For each ".mod" file, there must exist a ".opa" file
"""

import argparse
from pyfant import *
import logging
import glob
import os
import sys

misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    parser.add_argument('--pattern', type=str, help='file name pattern (with wildcards)',
     nargs="?", default="*.mod")
    parser.add_argument('fn_output', type=str, help='output file name', nargs="?",
     default=FileMog.default_filename)

    args = parser.parse_args()

    print args

    logger = get_python_logger()

    filenames = glob.glob("./"+args.pattern)
    n = len(filenames)
    print "%d file%s matching pattern '%s'" % (n, "s" if n != 1 else "", args.pattern)

    records = []
    for filename in filenames:
        name = os.path.splitext(os.path.basename(filename))[0]
        print "Considering files '%s'+('.mod', '.opa') ..." % name
        try:
            f = FileMarcsMod()
            f.load(filename)

            g = FileMarcsOpa()
            g.load(name+".opa")

            r = MogRecord()
            r.from_marcs_files(f, g)
            records.append(r)

        except:
            logger.exception("Error loading file '%s', skipping..." % filename)

    if len(records) == 0:
        print "No valid models found, nothing to save."
    else:

        records.sort(key=lambda r: r.asalog*1e10+r.teff*100+r.glog)

        g = FileMog()
        g.records = records
        g.save_as(args.fn_output)

        print "Successfully created file '%s'" % args.fn_output

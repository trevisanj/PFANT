#!/usr/bin/python

"""
Model Grid Creator

"Collects" several files in current directory and creates a single file
containing atmospheric model grid.

Working modes (option "-m"):
 "opa" (default mode): looks for MARCS ".mod" and ".opa" text file pairs and
                       creates a *big* binary file containing *all* model
                       information including opacities.
                       Output will be in ".moo" format.

 "modtxt": looks for MARCS ".mod" text files only. Resulting grid will not contain
           opacity information.
           Output will be in binary ".mod" format.


 "modbin": looks for binary-format ".mod" files. Resulting grid will not contain
           opacity information.
           Output will be in binary ".mod" format.

.
.
.
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
    parser.add_argument('--mode', type=str, nargs="?", default="opa",
     choices=["opa", "modtxt", "modbin"],
     help='working mode (see description above)')
    VDOM = "\"grid.moo\" or \"grid.mod\", depending on mode"
    parser.add_argument('fn_output', type=str, help='output file name', nargs="?",
     default=VDOM)

    args = parser.parse_args()

    logger = get_python_logger()

    if args.fn_output == VDOM:
        args.fn_output = "grid.moo" if args.mode == "opa" else "grid.mod"
        logger.info("Setting output filename to '%s'" % args.fn_output)

    filenames = glob.glob("./"+args.pattern)
    n = len(filenames)
    print "%d file%s matching pattern '%s'" % (n, "s" if n != 1 else "", args.pattern)

    records = []
    for filename in filenames:
        if args.mode == "opa":
            name = os.path.splitext(os.path.basename(filename))[0]
            print "Considering files '%s'+('.mod', '.opa') ..." % name
            try:
                f = FileModTxt()
                f.load(filename)
                g = FileOpa()
                g.load(name+".opa")
                r = MooRecord()
                r.from_marcs_files(f, g)
                records.append(r)
            except:
                logger.exception("Error loading file '%s', skipping..." % filename)
        else:
            nameext = os.path.basename(filename)
            print "Considering file '%s'+('.mod', '.opa') ..." % nameext
            try:
                if args.mode == "modtxt":
                    f = FileModTxt()
                    f.load(filename)
                    records.append(f.record)
                else:
                    f = FileModBin()
                    f.load(filename)
                    records.extend(f.records)
            except:
                logger.exception("Error loading file '%s', skipping..." % filename)


    if len(records) == 0:
        print "No valid models found, nothing to save."
        sys.exit()

    records.sort(key=lambda r: r.asalog*1e10+r.teff*100+r.glog)

    if args.mode == "opa":
        g = FileMoo()
    else:
        g = FileModBin()
    g.records = records
    g.save_as(args.fn_output)

    print "Successfully created file '%s'" % args.fn_output

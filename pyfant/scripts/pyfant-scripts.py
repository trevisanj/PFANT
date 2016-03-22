#!/usr/bin/python

"""
Lists scripts in PFANT/pyfant/scripts directory.
"""

#from pyfant.gui import *
from pyfant import SmartFormatter, fmt_ascii_h1
import sys
#from PyQt4.QtGui import *
#from PyQt4.QtCore import *
import argparse
#import logging
import glob
import os.path
import imp
import textwrap

# logging.basicConfig(level=logging.DEBUG)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
    description=__doc__,
    formatter_class=SmartFormatter
    )
    parser.add_argument('--markdown', help='Generates MarkDown output', action="store_true")
    args = parser.parse_args()

    # directory of this script
    base_dir = os.path.dirname(os.path.realpath(sys.argv[0]))
    # gets all scripts in script directory
    ff = glob.glob(os.path.join(base_dir, "*.py"))
    ff.sort()
    
    module_len = max([len(os.path.split(f)[1]) for f in ff])

    if args.markdown:
        mask = "%%-%ds | %%s" % module_len
        print mask % ("Script name", "Purpose")
        print "-"*(module_len+1)+"|"+"-"*10
        for f in ff:
            module_name, _ = os.path.splitext(f)
            _, filename = os.path.split(f)
            try:
                script_ = imp.load_source('script_', f)  # module object
                descr = script_.__doc__.strip()
                descr = descr.split("\n")[0]  # first line of docstring
            except Exception as e:
                descr = "*%s*: %s" % (e.__class__.__name__, str(e))
            print mask % (filename, descr)
    else:
        N = 50
        r = base_dir
        if len(r) > N+3:
            r = "..."+r[-N:]
        s = "Scripts in "+r
        print fmt_ascii_h1(s)
        for f in ff:
            module_name, _ = os.path.splitext(f)
            _, filename = os.path.split(f)
            print filename+" "+("."*(module_len-len(filename))),
            try:
                script_ = imp.load_source('script_', f)  # module object

                descr = script_.__doc__.strip()
                descr = descr.split("\n")[0]  # first line of docstring
                ss = textwrap.wrap(descr, 79-module_len-1)

                print ss[0] if ss and len(ss) > 0 else "no doc"
                for i in range(1, len(ss)):
                    print " "*(module_len+1), ss[i]
            except Exception as e:
                print "*%s*: %s" % (e.__class__.__name__, str(e))


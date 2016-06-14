#!/usr/bin/python

"""
Lists all Fortran/Python programs available with PFANT.
"""

from pyfant import *
import os

if __name__ == "__main__":
    linesp, module_len = get_scripts(flag_header=False)
    linesf = get_fortrans(module_len)

    print fmt_ascii_h1("Fortran")
    print "\n".join(linesf)
    print ""

    print fmt_ascii_h1("Python")
    print "\n".join(linesp)


    print ""
    print "Program directories:"
    print "  Fortran: %s" % os.path.join(get_pfant_dir(), "fortran", "bin")
    print "  Python: %s" % os.path.join(get_pfant_dir(), "pyfant", "scripts")


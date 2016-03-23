#!/usr/bin/python
"""
Cuts molecular lines file to wavelength interval specified.

The interval is [llzero, llfin]
"""

import argparse
from pyfant import SmartFormatter
from pyfant import misc
from pyfant import FileMolecules
import logging

misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    parser.add_argument('llzero', type=float, nargs=1,
     help='lower wavelength boundary (angstrom)')
    parser.add_argument('llfin', type=float, nargs=1,
     help='upper wavelength boundary (angstrom)')
    parser.add_argument('fn_input', type=str, help='input file name', nargs=1)
    parser.add_argument('fn_output', type=str, help='output file name', nargs=1)

    args = parser.parse_args()

    file_molecules = FileMolecules()
    file_molecules.load(args.fn_input[0])

    lmbdam = file_molecules.lmbdam
    m, M = min(lmbdam), max(lmbdam)

    print "Original interval: [%g, %g]" % (m, M)
    print "New interval: [%g, %g]" % (args.llzero[0], args.llfin[0])

    file_molecules.cut(args.llzero[0], args.llfin[0])
    file_molecules.save_as(args.fn_output[0])
    print "Successfully created file '%s'" % args.fn_output[0]

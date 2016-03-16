#!/usr/bin/python
"""
Cuts spectrum file to wavelength interval specified. Saved in 2-column format.

The interval is [llzero, llfin]

"""

import argparse
from pyfant import *
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

    file_sp = load_spectrum(args.fn_input[0])
    if not file_sp:
        print "File '%s' not recognized as a spectrum file." % args.fn_input[0]

    sp = file_sp.spectrum
    m = min(sp.x)
    M = max(sp.x)

    print "Original interval: [%g, %g]" % (m, M)
    print "New interval: [%g, %g]" % (args.llzero[0], args.llfin[0])

    f = FileSpectrumXY()
    f.spectrum = cut_spectrum(sp, args.llzero[0], args.llfin[0])
    f.save_as(args.fn_output[0])
    print "Successfully created file '%s'" % args.fn_output[0]

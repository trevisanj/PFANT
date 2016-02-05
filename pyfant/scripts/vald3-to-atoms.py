#!/usr/bin/python
"""
Converts VALD3 atomic/molecular lines file to PFANT atomic lines file.

Molecular lines are skipped
"""

import argparse
from pyfant import SmartFormatter
from pyfant.from_vald import *
from pyfant import misc
import logging

misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    parser.add_argument('fn_input', type=str, help='input file name', nargs=1)
    parser.add_argument('fn_output', type=str, help='output file name', nargs=1)

    args = parser.parse_args()
    with open(args.fn_input[0], 'r') as file_:
        file_atoms = vald3_to_atoms(file_)
    fn_out = args.fn_output[0]
    file_atoms.save_as(fn_out)
    print "File %s was successfully created." % fn_out
    print "Attention: rm se"

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
import numpy as np

misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    parser.add_argument('fn_input', type=str, help='input file name', nargs=1)
    parser.add_argument('fn_output', type=str, help='output file name', nargs=1)
    parser.add_argument('--min_algf', type=float, nargs='?', default=-7,
     help='minimum algf (log gf)')
    parser.add_argument('--max_kiex', type=float, nargs='?', default=7,
     help='maximum kiex')

    args = parser.parse_args()
    logger = misc.get_python_logger()
    logger.info("Converting file...")
    with open(args.fn_input[0], 'r') as file_:
        file_atoms = vald3_to_atoms(file_)
    logger.info("Number of lines: %d" % file_atoms.num_lines)

    logger.info("Filtering algf >= %g..." % args.min_algf)
    file_atoms.filter(lambda line: line.algf >= args.min_algf)
    logger.info("Filtering kiex <= %g..." % args.max_kiex)
    file_atoms.filter(lambda line: line.kiex <= args.max_kiex)
    logger.info("Number of lines after filtering: %d" % file_atoms.num_lines)

    logger.info("Saving file...")
    fn_out = args.fn_output[0]
    file_atoms.save_as(fn_out)
    print "File %s was successfully created." % fn_out
    zz = file_atoms.zinf
    if len(zz) > 0:
        if np.all(zz == zz[0]):
            logger.warning("All zinf's set to %g. Now they must be fine-tuned with tune-zinf.py." % zz[0])

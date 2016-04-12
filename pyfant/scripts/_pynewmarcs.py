#!/usr/bin/python

#
# NOT YET
#

"""
Interpolation to crCuts atomic lines file to wavelength interval specified.

The interval is [llzero, llfin]
"""

import argparse
from pyfant import *
import logging
import glob

misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    parser.add_argument('fn_main', type=str, help='main configuration file name',
     nargs="?", default=FileMain.default_filename)
    parser.add_argument('fn_modeles', type=str, help='output file name', nargs="?",
    default=FileModBin.default_filename)

    args = parser.parse_args()

    logger = get_python_logger()

    file_main = FileMain()
    file_main.load(args.fn_main)


    files = glob.glob("./*.mod")
    records = []
    for file in files:
        try:
            f = FileModTxt()
            f.load(file)
            records.append(f.record)
        except:
            logger.exception("Error loading file '%s'" % file)

    records.sort(key=lambda r: r.asalog*1e10+r.teff*100+r.glog)

    g = FileModBin()
    g.records = records
    g.save_as("uhu.mod")



    # mentira por enqto
    print "Successfully created file '%s'" % args.fn_modeles

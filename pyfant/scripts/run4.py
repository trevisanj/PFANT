#!/usr/bin/python
"""
Runs the four Fortran binaries in sequence: innewmarcs, hydro2, pfant, nulbad

Any files created during the runs will be saved in a subdirectory named
session-<i>, where i is an integer. Exception: python.log will be saved in
current directory.
"""

import argparse
from pyfant import *

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
    description=__doc__,
    formatter_class=SmartFormatter
    )

    names = Conf().opt.get_names() # option names

    for name in names:
        name = name.replace('_', '-')
        parser.add_argument("--"+name, type=str, help='')

    args = parser.parse_args()


    # Configuration for Python logging messages.
    misc.flag_log_console = True
    misc.flag_log_file = True
    logger = get_python_logger()

    c = Combo()
    c.conf.flag_log_file = True  # Configuration for Fortran messages
    c.conf.flag_log_console = True  # "
    c.conf.flag_output_to_dir = False  # Will generate outputs in current directory

    for name in names:
        x = args.__getattribute__(name)
        if x is not None:
            c.conf.opt.__setattr__(name, x)

    c.run()
    logger.info("Session directory: %s" % c.conf.sid.dir)

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

  names = ExeConf().opt.get_names() # option names

  for name in names:
    parser.add_argument("--"+name, type=str, help='')

  args = parser.parse_args()

  c = Combo()

  for name in names:
    x = args.__getattribute__(name)
    if x is not None:
      c.execonf.opt.__setattr__(name, x)

  c.run()

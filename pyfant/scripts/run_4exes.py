#!/usr/bin/python
"""
Runs the 4 exes
"""

import argparse
from pyfant import *

if __name__ == "__main__":
  parser = argparse.ArgumentParser(
    description='',
    formatter_class=argparse.ArgumentDefaultsHelpFormatter
  )

  nn = ExeConf.option_map  # option names

  for name in nn:
    parser.add_argument("--"+name, type=str, help='')

  args = parser.parse_args()

  c = Combo()

  for name in nn:
    x = args.__getattribute__(name)
    if x is not None:
      c.execonf.__setattr__("opt_"+name, x)

  c.run()

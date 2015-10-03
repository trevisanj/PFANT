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

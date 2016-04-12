#!/usr/bin/python
"""
Plots one record of a binary .mod file (e.g., modeles.mod, newnewm050.mod).
"""

import argparse
from pyfant import *
import matplotlib.pyplot as plt

if __name__ == "__main__":
  parser = argparse.ArgumentParser(
   description=VisModRecord.__doc__,
   formatter_class=SmartFormatter
   )

  parser.add_argument('--inum', type=int, default=1, help='Record number (>= 1)')
  parser.add_argument('fn', type=str, help='.mod binary file name', default='modeles.mod', nargs='?')

  args = parser.parse_args()

  m = FileModBin()
  m.load(args.fn)

  v = VisModRecord()
  v.title = args.fn
  v.inum = args.inum
  v.use(m)
  plt.show()


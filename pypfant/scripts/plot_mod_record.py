#!/usr/bin/python
"""
Plots one record of a binary .mod file (e.g., modeles.mod, newnewm050.mod)
"""

import argparse
from pypfant import *
import matplotlib.pyplot as plt

if __name__ == "__main__":
  parser = argparse.ArgumentParser(
    description=plot_mod_records.__doc__,
    formatter_class=argparse.ArgumentDefaultsHelpFormatter
  )

  parser.add_argument('--inum', type=int, default=1, help='Record number (>= 1)')
  parser.add_argument('fn', type=str, help='.mod binary file name', default='modeles.mod', nargs='?')

  args = parser.parse_args()

  m = FileMod()
  m.load(args.fn)
  r = m.records[args.inum-1]

  plot_mod_record(r, args.fn)
  plt.show()
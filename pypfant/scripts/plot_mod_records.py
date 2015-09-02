#!/usr/bin/python
"""
Opens several windows to show what is inside a NEWMARCS grid file.
"""

import argparse
from pypfant import *
import matplotlib.pyplot as plt

if __name__ == "__main__":
  parser = argparse.ArgumentParser(
    description=plot_mod_records.__doc__,
    formatter_class=argparse.ArgumentDefaultsHelpFormatter
  )

  parser.add_argument('fn', type=str, help='NEWMARCS grid file name')

  args = parser.parse_args()

  m = FileMod()
  m.load(args.fn)


  v = VisModRecords()
  v.title = args.fn
  v.use(m)

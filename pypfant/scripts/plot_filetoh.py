#!/usr/bin/python
"""
Plots hydrogen lines
"""

import argparse
from pypfant import *
import matplotlib.pyplot as plt

if __name__ == "__main__":
  parser = argparse.ArgumentParser(
    description=VisFileToH.__doc__,
    formatter_class=argparse.ArgumentDefaultsHelpFormatter
  )

  parser.add_argument('fn', type=str, help='hydrogen lines file name')

  args = parser.parse_args()

  r = FileToH()
  r.load(args.fn)

  v = VisFileToH()
  v.title = args.fn
  v.use(r)
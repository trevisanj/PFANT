#!/usr/bin/python
"""
Opens several windows to show what is inside a NEWMARCS grid file.
"""

import argparse
from pyfant import *
import matplotlib.pyplot as plt

if __name__ == "__main__":
  parser = argparse.ArgumentParser(
   description=VisModCurves.__doc__,
   formatter_class=SmartFormatter
   )

  parser.add_argument('fn', type=str, help='NEWMARCS grid file name')

  args = parser.parse_args()

  m = FileModBin()
  m.load(args.fn)


  v = VisModCurves()
  v.title = args.fn
  v.use(m)

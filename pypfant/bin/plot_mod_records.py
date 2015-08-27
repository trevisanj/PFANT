#!/usr/bin/python
"""
Runs the 4 exes
"""

import argparse
from pypfant import *

if __name__ == "__main__":
  parser = argparse.ArgumentParser(
    description='',
    formatter_class=argparse.ArgumentDefaultsHelpFormatter
  )

  parser.add_argument('integers', metavar='N', type=int, nargs='+',
                   help='an integer for the accumulator')

  args = parser.parse_args()

  print args

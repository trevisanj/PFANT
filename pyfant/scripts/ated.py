#!/usr/bin/python

"""
ated - ATomic lines file EDitor

See ated.html or press F1 inside application.
"""

from pyfant.gui import *
from pyfant import *
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *
import argparse
import logging

logging.basicConfig(level=logging.DEBUG)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
    description="ATomic lines file EDitor",
    formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument('fn', type=str, help='atoms file name', default='atomgrade.dat', nargs='?')
    args = parser.parse_args()

    m = FileAtoms()
    m.load(args.fn)
    app = QApplication([])
    form = XFileAtoms()
    form.show()
    form.load(m)
    sys.exit(app.exec_())

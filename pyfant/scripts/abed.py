#!/usr/bin/python

"""abed - ABundances file EDitor (deprecated)."""

from pyfant.gui import XFileAbonds
from pyfant import *
import sys
from PyQt4.QtGui import *
import argparse
import logging


misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
    description=__doc__,
    formatter_class=SmartFormatter
    )
    parser.add_argument('fn', type=str, help='abundances file name', default='abonds.dat', nargs='?')
    args = parser.parse_args()

    m = FileAbonds()
    m.load(args.fn)
    app = QApplication([])
    form = XFileAbonds()
    form.show()
    form.load(m)
    sys.exit(app.exec_())

#!/usr/bin/python

"""Main configuration file editor."""

from pyfant.gui import XFileMain
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
    parser.add_argument('fn', type=str, help='main configuration file name', default='main.dat', nargs='?')
    args = parser.parse_args()

    m = FileMain()
    m.load(args.fn)
    app = get_QApplication([])
    form = XFileMain()
    form.load(m)
    form.show()
    sys.exit(app.exec_())

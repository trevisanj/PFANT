#!/usr/bin/python

"""File Explorer - lists all files in directory and provides visualization options"""

from pyfant.gui import XExplorer
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
    parser.add_argument('dir', type=str, help='directory name', default='.', nargs='?')
    args = parser.parse_args()

    app = get_QApplication([])
    form = XExplorer(None, args.dir)
    form.showMaximized()
    sys.exit(app.exec_())

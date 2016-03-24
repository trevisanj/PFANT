#!/usr/bin/python

"""Molecular lines file editor."""

from pyfant.gui import *
from pyfant import *
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *
import argparse
import logging

misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
    description=__doc__,
    formatter_class=SmartFormatter
    )
    parser.add_argument('fn', type=str, help='molecules file name',
                        default=FileMolecules.default_filename, nargs='?')
    args = parser.parse_args()

    m = FileMolecules()
    m.load(args.fn)
    app = get_QApplication([])
    form = XFileMolecules()
    form.show()
    form.load(m)
    sys.exit(app.exec_())

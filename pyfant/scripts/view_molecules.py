#!/usr/bin/python

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
    description=VisModRecord.__doc__,
    formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    parser.add_argument('fn', type=str, help='molecules file name', default='moleculagrade.dat', nargs='?')

    args = parser.parse_args()

    m = FileMolecules()
    m.load(args.fn)


    app = QApplication([])

    form = XFileMolecules()
    #form.showMaximized()
    form.show()
    form.load(m)

    sys.exit(app.exec_())

#!/usr/bin/python

"""FileSpectrumList editor with import/export FileWebsimCube"""

from pyfant import *
from pyfant.gui.pymos import XFileSpectrumList
import sys
import argparse
import logging

misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
    description=__doc__,
    formatter_class=SmartFormatter
    )
    parser.add_argument('fn', type=str, help="file name, supports '%s' only at the moment" % (FileSpectrumList.description,), nargs='?')
    args = parser.parse_args()

    app = get_QApplication([])
    form = XFileSpectrumList()

    if args.fn is not None:
        form.load_filename(args.fn)

    form.show()
    sys.exit(app.exec_())

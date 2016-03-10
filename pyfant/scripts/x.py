#!/usr/bin/python

"""X - Graphical interface for running the PFANT Fortran executable binaries."""

from pyfant.gui import XPFANT, XRunnableManager
from pyfant import *
import sys
from PyQt4.QtGui import *
import argparse
import logging


misc.logging_level = logging.INFO

if __name__ == "__main__":
    app = get_QApplication([])
    form0 = XPFANT()
    rm = RunnableManager()
    form1 = XRunnableManager(form0, rm)
    form1.flag_close_mpl_plots_on_close = False
    form1.flag_close_message = False
    form0.set_manager_form(form1)
    # form1.show()
    # it is good to start the manager as late as possible, otherwise
    # the program will hang if, for example, the form fails to be created.
    rm.start()
    try:
        form0.show()
        app.exec_()
    finally:
        rm.exit()

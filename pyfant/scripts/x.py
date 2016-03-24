#!/usr/bin/python

from pyfant.gui import XMulti, XRunnableManager
from pyfant import *
from pyfant.gui import guiaux
import sys
from PyQt4.QtGui import *
import argparse
import logging


__doc__ = """XM - Graphical interface -- single and multi modes.\n\n
Multi mode
----------
"""+guiaux.DESCR_MULTI+\
"---------\n\n"


misc.logging_level = logging.INFO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    args = parser.parse_args()

    app = get_QApplication([])

    form0 = XMulti()
    # this would start with "multi" tab selected form0.tabWidget.setCurrentIndex(3)

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

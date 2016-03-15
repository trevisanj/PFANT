"""Graphicsl interface for PFANT"""

__all__ = ["XPFANT"]

from PyQt4.QtGui import *
from pyfant import *
from .guiaux import *
from . import XRunnableManager
from pyfant import *
from .a_XMainAbonds import *
import os.path
import matplotlib.pyplot as plt
import traceback
import copy

################################################################################
class XPFANT(XMainAbonds):
    """
    Arguments:
      parent=None -- nevermind
      file_main (optional)-- FileMain instance
    """

    def __init__(self, *args, **kwargs):
        ## State variables
        XMainAbonds.__init__(self, *args, **kwargs)

        # # Central layout

        # ## Main control bar

        l = self.controlLayout
        b = self.buttonSubmit = QPushButton("&Submit job")
        b.clicked.connect(self.on_submit)
        l.addWidget(b)
        # s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)
        l.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # # Final adjustments
        self.setWindowTitle("PFANT launcher")

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Qt override

    def closeEvent(self, event):
        plt.close("all")
        event.accept()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    def on_submit(self):
        flag_ok = True
        errors = []
        if not self.me.f:
            errors.append("Main configuration not set")
        if not self.ae.f:
            errors.append("Abundances not set")
        if not self.me.flag_valid:
            errors.append("Error(s) in main configuration")
        if not self.ae.flag_valid:
            errors.append("Error(s) in abundances")
        if not self.oe.flag_valid:
            errors.append("Error(s) in command-line options")
        if len(errors) == 0:
            try:
                self._manager_form.show()
                self.__submit_job()
            except Exception as e:
                errors.append(str(e))
                traceback.print_exc()
        if len(errors) > 0:
            ShowError("Cannot submit job:\n  - "+("\n  - ".join(errors)))

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for signals emited by pyfant widgets


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Internals

    def __submit_job(self):
        r = Combo()
        r.conf.opt = copy.copy(self.oe.f)
        r.conf.file_main = self.me.f
        r.conf.file_abonds = self.ae.f
        r.conf.file_dissoc = self.ae.f.get_file_dissoc()
        r.conf.flag_output_to_dir = True
        self._rm.add_runnables([r])


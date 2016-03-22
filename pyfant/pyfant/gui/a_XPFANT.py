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
import syntax


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
        w = self.buttonSubmit = QPushButton("&Submit job")
        w.clicked.connect(self.on_submit)
        l.addWidget(w)
        w = self.checkbox_custom_id = QCheckBox("Custom session id")
        w.stateChanged.connect(self.on_checkbox_custom_id_state_changed)
        l.addWidget(w)
        w = self.lineEdit_custom_id = QLineEdit()
        w.setFixedWidth(100)
        l.addWidget(w)
        # s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)
        l.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # # Final adjustments
        self.setWindowTitle("PFANT launcher")
        self.__update_lineEdit_custom_id()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Qt override

    def closeEvent(self, event):
        plt.close("all")
        event.accept()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    def on_submit(self):
        flag_ok = True
        errors = self._check_single_setup()
        if len(errors) == 0:
            # more error checking
            if self.checkbox_custom_id.isChecked():
                if len(self.__get_custom_session_id()) == 0:
                    errors.append("Please inform custom session id.")
        if len(errors) == 0:
            try:
                self._manager_form.show()
                self.__submit_job()
            except Exception as e:
                errors.append(str(e))
                get_python_logger().exception("Cannot submit job")
        if len(errors) > 0:
            ShowError("Cannot submit job:\n  - "+("\n  - ".join(errors)))

    def on_checkbox_custom_id_state_changed(self):
        self.__update_lineEdit_custom_id()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for signals emited by pyfant widgets


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Internals

    def __get_custom_session_id(self):
        return str(self.lineEdit_custom_id.text()).strip()

    def __submit_job(self):
        r = Combo()
        if self.checkbox_custom_id.isChecked():
            r.conf.sid.id = self.__get_custom_session_id()
        r.conf.opt = copy.copy(self.oe.f)
        r.conf.file_main = self.me.f
        r.conf.file_abonds = self.ae.f
        r.conf.file_dissoc = self.ae.f.get_file_dissoc()
        r.conf.flag_output_to_dir = True
        self._rm.add_runnables([r])

    def __update_lineEdit_custom_id(self):
        self.lineEdit_custom_id.setEnabled(self.checkbox_custom_id.isChecked())

"""Graphicsl interface for PFANT - single and multi modes"""

__all__ = ["XMulti"]

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
from .a_XPFANT import *
from .a_WFileAbXFwhm import *


################################################################################
class XMulti(XPFANT):
    """
    Arguments:
      parent=None -- nevermind
      file_main (optional)-- FileMain instance
    """

    def __init__(self, *args, **kwargs):
        ## State variables
        XPFANT.__init__(self, *args, **kwargs)

        # # Overriding previous configuration

        w = self.buttonSubmit.setText("&Submit single job")

        # # Multi tab

        tt = self.tabWidget
        tt.currentChanged.connect(self.on_tabWidget_current_changed)

        wm = self.multi_widget = QWidget()
        tt.addTab(wm, "Multi mode (Alt+&4)")

        # ## Layout with all elements in Multi tab
        l = QVBoxLayout()
        wm.setLayout(l)

        # ### Toolbar
        l1 = self.multiToolbarLayour = QHBoxLayout()
        l.addLayout(l1)
        w = self.buttonSubmit = QPushButton("&Submit multi-job")
        w.clicked.connect(self.on_run_multi)
        l1.addWidget(w)
        w = self.checkbox_multi_custom_id = QCheckBox("Custom multi-session id")
        w.stateChanged.connect(self.on_checkbox_multi_custom_id_state_changed)
        l1.addWidget(w)
        w = self.lineEdit_multi_custom_id = QLineEdit()
        l1.addWidget(w)
        w.setFixedWidth(100)
        # s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # ### Editor for multi setup
        editor = self.multi_editor = WFileAbXFwhm()
        l.addWidget(editor)

        # # Final adjustments
        self.__update_lineEdit_multi_custom_id()
        tt.setCurrentIndex(3)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    def on_run_multi(self):
        errors = self._check_setup()
        if len(errors) == 0:
            # more error checking
            if self.checkbox_multi_custom_id.isChecked():
                if len(self.__get_multi_custom_session_id()) == 0:
                    errors.append("Please inform custom session id.")
        if len(errors) == 0:
            try:
                self.setEnabled(False)
                self.__submit_multi()
                self._manager_form.show()
                print "NOT YET MULTI SUBMIT"
            except Exception as e:
                errors.append(str(e))
                get_python_logger().exception("Cannot submit multi-job")
            finally:
                self.setEnabled(True)

        if len(errors) > 0:
            ShowError("Cannot submit multi-job:\n  - "+("\n  - ".join(errors)))

    def on_checkbox_multi_custom_id_state_changed(self):
        self.__update_lineEdit_multi_custom_id()

    def on_tabWidget_current_changed(self, index):
        self.__toggle_single_toolbar(index != 3)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for signals emited by pyfant widgets


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Internals

    def __get_multi_custom_session_id(self):
        return str(self.lineEdit_multi_custom_id.text()).strip()

    def __submit_multi(self):
        custom_id = None
        if self.checkbox_multi_custom_id.isChecked():
            custom_id = self.__get_multi_custom_session_id()

        r = MultiRunnable(self.me.f, self.ae.f, self.oe.f, self.multi_editor.f)
        if self.checkbox_custom_id.isChecked():
            r.sid.id = self.__get_multi_custom_session_id()
        self._rm.add_runnables([r])

    def __update_lineEdit_multi_custom_id(self):
        self.lineEdit_multi_custom_id.setEnabled(self.checkbox_multi_custom_id.isChecked())

    def __toggle_single_toolbar(self, flag_enabled):
        layout = self.controlLayout
        # http://stackoverflow.com/questions/5150182/loop-over-widgets-in-pyqt-layout
        items = (layout.itemAt(i) for i in range(layout.count()))
        for item in items:
            w = item.widget()
            if w:
                w.setEnabled(flag_enabled)

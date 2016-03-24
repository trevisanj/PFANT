"""Graphicsl interface for PFANT - single and multi modes"""

__all__ = ["XMulti"]

from PyQt4.QtGui import *
from .guiaux import *
from pyfant import *
import os.path
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

        # # Complementing previous configuration

        w = self.buttonSubmit.setText("&Submit single job")
        self.ae.loaded.connect(self.on_file_abonds_loaded)

        # # Multi tab

        tt = self.tabWidget
        tt.currentChanged.connect(self.on_tabWidget_current_changed)

        wm = self.multi_widget = QWidget()
        TAB_TEXT_4 = "Multi mode (Alt+&4)"
        tt.addTab(wm, TAB_TEXT_4)
        tt.setTabToolTip(tt.count()-1, DESCR_MULTI)

        # ## Layout with all elements in Multi tab
        l = QVBoxLayout()
        wm.setLayout(l)

        # ### Row with filename
        l1 = self.c923788 = QHBoxLayout()
        l.addLayout(l1)
        w = QLabel("<b>File:<b>")
        l1.addWidget(w)
        w = self.label_fn_abxfwhm = QLabel()
        l1.addWidget(w)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

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
        editor.edited.connect(self.on_multi_edited)
        l.addWidget(editor)

        # # Final adjustments

        # ## Registers data in a_XMainAbonds lists to automatically perform Load/Save/Save as
        self.tab_texts.append(TAB_TEXT_4)
        self.flags_changed.append(False)
        self.save_as_texts.append("Save abundances X FWHM's configuration as")
        self.open_texts.append("Load abundandex X FWHM's file")
        self.clss.append(FileAbXFwhm)
        self.editors.append(self.multi_editor)
        self.labels_fn.append(self.label_fn_abxfwhm)
        self.wilds.append("*.py")

        self.__update_lineEdit_multi_custom_id()
        # tt.setCurrentIndex(3)
        # ## Loads abxfwhm file
        if os.path.isfile(FileAbXFwhm.default_filename):
            f = FileAbXFwhm()
            f.load()
            self.multi_editor.load(f)
        # ## calls slot to perform cross-check between FileAbonds and FileAbXFwhm
        if self.ae.f:
            self.on_file_abonds_loaded()
        self._update_labels_fn()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    def on_run_multi(self):
        errors = self._check_single_setup()

        # more error checking
        if self.checkbox_multi_custom_id.isChecked():
            if len(self.__get_multi_custom_session_id()) == 0:
                errors.append("Please inform custom session id.")
        if not self.multi_editor.f:
            errors.append("abundances X FWHM's configuration not set")
        else:
            # forces validation because validity is linked to FileAbonds managed in other tab
            self.multi_editor.f.validate()
            if not self.multi_editor.flag_valid:
                errors.append("error(s) in abundances X FWHM's configuration")

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
        if index == 3:
            self.multi_editor.validate()

    def on_abonds_edited(self):
        """Overrides method to update multi_editor.file_abonds."""
        XPFANT.on_abonds_edited(self)
        self.multi_editor.file_abonds = self.ae.f

    def on_multi_edited(self):
        self._on_edited()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for signals emited by pyfant widgets

    def on_file_abonds_loaded(self):
        self.multi_editor.file_abonds = self.ae.f

    def on_abonds_edited(self):
        XPFANT.on_abonds_edited(self)
        self.multi_editor.file_abonds = self.ae.f

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Protected methods

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

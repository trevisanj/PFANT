"""abed -- ABundances file EDitor window."""

__all__ = ["XFileAbonds"]

from PyQt4.QtGui import *
from . import a_WFileAbonds
from pyfant import FileAbonds, FileDissoc
from ._guiaux import *
from .guimisc import *
import os

################################################################################
class XFileAbonds(QMainWindow):
    """
    Arguments:
      parent=None -- nevermind
      file_abonds (optional)-- FileAbonds instance
    """

    def __init__(self, parent=None, file_abonds=None):
        ## State variables
        QMainWindow.__init__(self, parent)
        self.flag_changed = False
        me = self.me = a_WFileAbonds.WFileAbonds()
        me.setFont(MONO_FONT)
        me.edited.connect(self.on_edited)
        if file_abonds is not None:
            self.me.load(file_abonds)
        me.setFocus()
        # self.setWindowTitle(title)
        self.setCentralWidget(me)

        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")
        self.act_save = ac = m.addAction("&Save")
        ac.setShortcut("Ctrl+S")
        ac.triggered.connect(self.on_save)
        self.act_save_as = ac = m.addAction("Save &as...")
        ac.setShortcut("Ctrl+Shift+S")
        ac.triggered.connect(self.on_save_as)
        self.act_export_dissoc = ac = m.addAction("Export &dissoc file...")
        assert isinstance(ac, QAction)
        ac.setStatusTip("Saves dissoc.dat file with matching abundances")
        ac.setShortcut("Ctrl+Shift+D")
        ac.triggered.connect(self.on_export_dissoc)
        m.addSeparator()
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

        rect = QApplication.desktop().screenGeometry()
        self.setGeometry(0, 0, 400, rect.height())
        place_left_top(self)

    def load(self, x):
        assert isinstance(x, FileAbonds)
        self.me.load(x)
        self.update_window_title()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots

    def on_save(self, _):
        self.disable_save_actions()
        try:
            if not self.me.flag_valid:
                ShowError(PARAMS_INVALID)
            else:
                self.save()
        finally:
            self.enable_save_actions()

    def on_save_as(self, _):
        self.disable_save_actions()
        try:
            if self.me.f:
                if not self.me.flag_valid:
                    ShowError(PARAMS_INVALID)
                else:
                    new_filename = QFileDialog.getSaveFileName(self, "Save file", ".", ".dat")
                    if new_filename:
                        self.save_as(new_filename)
        finally:
            self.enable_save_actions()

    def on_export_dissoc(self, _):
        self.disable_save_actions()
        try:
            if self.me.f:
                if not self.me.flag_valid:
                    ShowError(PARAMS_INVALID)
                else:
                    new_filename = QFileDialog.getSaveFileName(self, "Save file",
                     os.path.join(".", FileDissoc.default_filename), ".dat")
                    if new_filename:
                        f = self.me.f.make_dissoc()
                        f.save_as(new_filename)
        finally:
            self.enable_save_actions()

    def on_edited(self):
        self.flag_changed = True
        self.update_window_title()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Internals

    def enable_save_actions(self):
        self.act_save.setEnabled(True)
        self.act_save_as.setEnabled(True)

    def disable_save_actions(self):
        self.act_save.setEnabled(False)
        self.act_save_as.setEnabled(False)

    def save(self):
        if self.me.f:
            self.me.f.save_as()
            self.flag_changed = False
            self.update_window_title()

    def save_as(self, filename):
        if self.me.f:
            self.me.f.save_as(filename)
            self.flag_changed = False
            self.update_window_title()

    def update_window_title(self):
        self.setWindowTitle("abed -- %s%s%s" % (self.me.f.filename,
          "" if not self.flag_changed else " (changed)",
          "" if self.me.flag_valid else " (*invalid*)"))


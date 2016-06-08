"""abed -- ABundances file EDitor window."""

__all__ = ["XFileAbonds"]

from PyQt4.QtGui import *
from . import a_WFileAbonds
from pyfant import FileAbonds, FileDissoc
from .guiaux import *
import os
from .a_XText import *

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
        self.save_dir = "."
        me = self.editor = a_WFileAbonds.WFileAbonds()
        me.setFont(MONO_FONT)
        me.edited.connect(self.on_edited)
        me.setFocus()
        # self.setWindowTitle(title)
        self.setCentralWidget(me)


        # # Status bar
        self.labelStatus = QLabel("")
        sb = self.statusBar()
        sb.insertWidget(0, self.labelStatus, 0)


        # # Menu bar
        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")
        self.act_save = ac = m.addAction("&Save")
        ac.setShortcut("Ctrl+S")
        ac.triggered.connect(self.on_save)
        self.act_save_as = ac = m.addAction("Save &as...")
        ac.setShortcut("Ctrl+Shift+S")
        ac.triggered.connect(self.on_save_as)
        self.act_export_dissoc = ac = m.addAction("Export &dissoc file...")
        ac.setStatusTip("Saves dissoc.dat file")
        ac.setShortcut("Ctrl+Shift+D")
        ac.triggered.connect(self.on_export_dissoc)
        self.act_export_turbospectrum = ac = m.addAction("Generate text for &TurboSpectrum")
        ac.setStatusTip("Opens window containing atomic numbers and abundances to be copy-pasted")
        ac.setShortcut("Ctrl+Shift+T")
        ac.triggered.connect(self.on_export_turbospectrum)
        m.addSeparator()
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

        rect = QApplication.desktop().screenGeometry()
        self.setGeometry(0, 0, 400, rect.height())
        place_left_top(self)

        if file_abonds is not None:
            self.load(file_abonds)

    def load(self, x):
        assert isinstance(x, FileAbonds)
        self.editor.load(x)
        self.update_window_title()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots

    def on_save(self, _):
        self.disable_save_actions()
        try:
            if not self.editor.flag_valid:
                ShowError(PARAMS_INVALID)
            else:
                self.save()
        except Exception as e:
            ShowError(str(e))
            raise
        finally:
            self.enable_save_actions()

    def on_save_as(self, _):
        self.disable_save_actions()
        try:
            if self.editor.f:
                if not self.editor.flag_valid:
                    ShowError(PARAMS_INVALID)
                else:
                    new_filename = QFileDialog.getSaveFileName(self, "Save file",
                     self.save_dir, "*.dat")
                    if new_filename:
                        self.save_dir, _ = os.path.split(str(new_filename))
                        self.save_as(new_filename)
        except Exception as e:
            ShowError(str(e))
            raise
        finally:
            self.enable_save_actions()

    def on_export_dissoc(self, _):
        self.disable_save_actions()
        try:
            if self.editor.f:
                if not self.editor.flag_valid:
                    ShowError(PARAMS_INVALID)
                else:
                    new_filename = QFileDialog.getSaveFileName(self, "Save file",
                     os.path.join(".", FileDissoc.default_filename), "*.dat")
                    if new_filename:
                        f = self.editor.f.get_file_dissoc()
                        f.title = "Created using abed.py"
                        f.save_as(new_filename)
        except Exception as e:
            ShowError(str(e))
            raise
        finally:
            self.enable_save_actions()


    def on_export_turbospectrum(self, _):
        w = XText(self, self.editor.f.get_turbospectrum_str(), "Atomic number & abundance")
        w.show()

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
        if self.editor.f:
            self.editor.f.save_as()
            self.flag_changed = False
            self.update_window_title()

    def save_as(self, filename):
        if self.editor.f:
            self.editor.f.save_as(filename)
            self.flag_changed = False
            self.update_window_title()

    def update_window_title(self):
        self.setWindowTitle("abed -- %s%s%s" % (self.editor.f.filename,
          "" if not self.flag_changed else " (changed)",
          "" if self.editor.flag_valid else " (*invalid*)"))


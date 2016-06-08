"""Main Editor dialog."""

__all__ = ["XFileMain"]

from PyQt4.QtGui import *
from . import a_WFileMain
from pyfant import FileMain
from .guiaux import *
import os.path

################################################################################
class XFileMain(QMainWindow):
    """
    Arguments:
      parent=None -- nevermind
      file_main (optional)-- FileMain instance
    """

    def __init__(self, parent=None, file_main=None):
        QMainWindow.__init__(self, parent)
        self.flag_changed = False
        self.save_dir = "."
        me = self.editor = a_WFileMain.WFileMain()
        me.setFont(MONO_FONT)
        me.edited.connect(self.on_edited)
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
        m.addSeparator()
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

        place_left_top(self, 500, 768)

        if file_main is not None:
            self.load(file_main)

    def load(self, x):
        assert isinstance(x, FileMain)
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
        self.setWindowTitle("mained -- %s%s%s" % (self.editor.f.filename,
          "" if not self.flag_changed else " (changed)",
          "" if self.editor.flag_valid else " (*invalid*)"))


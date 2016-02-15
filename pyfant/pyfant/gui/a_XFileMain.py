"""Main Editor dialog."""

__all__ = ["XFileMain"]

from PyQt4.QtGui import *
from . import a_WMainEditor
from pyfant import FileMain
from ._guiaux import *
from .guimisc import *

################################################################################
class XFileMain(QMainWindow):
    """
    Arguments:
      parent=None -- nevermind
      file_main (optional)-- FileMain instance
    """

    def __init__(self, parent=None, file_main=None, title="Main configuration file editor"):
        ## State variables
        QMainWindow.__init__(self, parent)
        self.flag_changed = False
        me = self.me = a_WMainEditor.WMainEditor()
        me.setFont(MONO_FONT)
        me.edited.connect(self.on_edited)
        if file_main is not None:
            self.me.load(file_main)
        me.setFocus()
        self.setWindowTitle(title)
        self.setCentralWidget(me)

        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")
        self.act_save = ac = m.addAction("&Save")
        ac.setShortcut("Ctrl+S")
        ac.triggered.connect(self.on_save)
        self.act_save_as = ac = m.addAction("Save &as...")
        ac.setShortcut("Ctrl+Alt+S")
        ac.triggered.connect(self.on_save_as)
        m.addSeparator()
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

    def load(self, x):
        assert isinstance(x, FileMain)
        self.me.load(x)


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots

    def on_save(self, _):
        self.disable_save_actions()
        try:
            self.save()
        finally:
            self.enable_save_actions()

    def on_save_as(self, _):
        self.disable_save_actions()
        try:
            if self.me.f:
                new_filename = QFileDialog.getSaveFileName(self, "Save file", ".", ".dat")
                if new_filename:
                    self.save_as(new_filename)
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
        self.setWindowTitle("mained -- %s%s%s" % (self.me.f.filename,
          "" if not self.flag_changed else " (changed)",
          "" if self.me.flag_valid else " (*invalid*)"))


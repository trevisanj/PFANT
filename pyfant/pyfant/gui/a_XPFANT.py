"""Graphicsl interface for PFANT"""

__all__ = ["XPFANT"]

from PyQt4.QtGui import *
from . import a_WFileMain
from pyfant import FileMain
from ._guiaux import *
from .guimisc import *
from . import XRunnableManager
from pyfant import *

################################################################################
class XPFANT(QMainWindow):
    """
    Arguments:
      parent=None -- nevermind
      file_main (optional)-- FileMain instance
    """

    def __init__(self, parent=None, file_main=None):
        ## State variables
        QMainWindow.__init__(self, parent)
        # XRunnableManager instance
        self.__manager_form = None
        # RunnableManager instance
        self.__rm = None
        self.flag_changed = False


        # # Menu bar

        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

        # # Central layout

        cw = self.centralWidget = QWidget()
        self.setCentralWidget(cw)
        la = self.centralLayout = QVBoxLayout(cw)

        # ## Main control bar

        l = self.controlLayout = QHBoxLayout()
        la.addLayout(l)
        b = self.buttonSubmit = QPushButton("&Submit job")
        b.clicked.connect(self.on_submit)
        l.addWidget(b)
        # s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)
        l.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # ## Main configuration file

        l0 = self.c12842 = QVBoxLayout()
        la.addLayout(l0)

        # ### Main configuration file toolbar

        l1 = self.c29378 = QHBoxLayout()
        l0.addLayout(l1)
        b = self.button_file_main_load = QPushButton("Load...")
        l1.addWidget(b)
        b.clicked.connect(self.on_file_main_load)
        b = self.button_file_main_save = QPushButton("Save as...")
        l1.addWidget(b)
        b.clicked.connect(self.on_file_main_save)
        b = self.button_file_main_init = QPushButton("Init default")
        l1.addWidget(b)
        b.clicked.connect(self.on_file_main_init)
        l1.addWidget(b)
        # s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # ### Main file editor widget
        me = self.me = a_WFileMain.WFileMain()
        l0.addWidget(me)
        me.setFont(MONO_FONT)
        me.edited.connect(self.on_main_file_edited)
        if file_main is not None:
            self.me.load(file_main)
        me.setFocus()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Interface

    def set_manager_form(self, x):
        assert isinstance(x, XRunnableManager)
        self.__manager_form = x
        self.__rm = x.rm

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    def on_file_main_load(self):
        print "on_file_main_load"

    def on_file_main_save(self):
        print "on_file_main_save"

    def on_file_main_init(self):
        f = FileMain()
        f.init_default()
        self.me.load(f)

        print "on_file_main_init"

    def on_submit(self):
        self.__manager_form.show()
        self.__submit_job()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for signals emited by pyfant widgets

    def on_main_file_edited(self):
        print "on_main_file_edited  "
        # self.flag_changed = True
        # self.update_window_title()



    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Internals

    def __submit_job(self):
        print "__submit_job"
        r = Pfant()
        r.conf.file_main = self.me.f
        r.conf.flag_output_to_dir = True
        self.__rm.add_runnables([r])



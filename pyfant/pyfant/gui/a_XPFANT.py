"""Graphicsl interface for PFANT"""

__all__ = ["XPFANT"]

from PyQt4.QtGui import *
from .a_WFileMain import *
from .a_WFileAbonds import *
from pyfant import FileMain
from .guiaux import *
from . import XRunnableManager
from pyfant import *

################################################################################
class XPFANT(QMainWindow):
    """
    Arguments:
      parent=None -- nevermind
      file_main (optional)-- FileMain instance
    """

    def __init__(self, parent=None, file_main=None, file_abonds=None):
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

        # ## Tabs

        # tab "File"
        tt = self.tabWidget = QTabWidget(self)
        la.addWidget(tt)
        tt.setFont(MONO_FONT)


        # ### Main configuration tab
        w0 = self.c27272 = QWidget()
        tt.addTab(w0, "Main configuration (Alt+&1")

        # #### Main configuration file toolbar
        l0 = self.c12842 = QVBoxLayout(w0)
        l1 = self.c29378 = QHBoxLayout()
        l0.addLayout(l1)
        b = self.button_main_load = QPushButton("Load...")
        l1.addWidget(b)
        b.clicked.connect(self.on_main_load)
        b = self.button_main_save = QPushButton("Save as...")
        l1.addWidget(b)
        b.clicked.connect(self.on_main_save)
        b = self.button_main_init = QPushButton("Init default")
        b.setToolTip(INITIALIZES_SUN)
        l1.addWidget(b)
        b.clicked.connect(self.on_main_init)
        l1.addWidget(b)
        # s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # ### Main file editor widget
        me = self.me = WFileMain()
        l0.addWidget(me)
        me.setFont(MONO_FONT)
        me.edited.connect(self.on_main_edited)
        if file_main is not None:
            self.me.load(file_main)
        # me.setFocus()


        # ### Abundances tab
        w0 = self.c10101 = QWidget()
        tt.addTab(w0, "Abundances (Alt+&2)")

        # #### Main configuration file toolbar
        l0 = self.c54354 = QVBoxLayout(w0)
        l1 = self.c28384 = QHBoxLayout()
        l0.addLayout(l1)
        b = self.button_abonds_load = QPushButton("Load...")
        l1.addWidget(b)
        b.clicked.connect(self.on_abonds_load)
        b = self.button_abonds_save = QPushButton("Save as...")
        l1.addWidget(b)
        b.clicked.connect(self.on_abonds_save)
        b = self.button_abonds_init = QPushButton("Init default")
        b.setToolTip(INITIALIZES_SUN)
        l1.addWidget(b)
        b.clicked.connect(self.on_abonds_init)
        l1.addWidget(b)
        # s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # ### Abundances editor
        ae = self.ae = WFileAbonds()
        l0.addWidget(ae)
        ae.setFont(MONO_FONT)
        ae.edited.connect(self.on_abonds_edited)
        if file_abonds is not None:
            self.ae.load(file_abonds)



        # ### Final tabs setup
        tt.setCurrentIndex(0)


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Interface

    def set_manager_form(self, x):
        assert isinstance(x, XRunnableManager)
        self.__manager_form = x
        self.__rm = x.rm

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    def on_main_load(self):
        print "on_main_load"

    def on_main_save(self):
        print "on_main_save"

    def on_main_init(self):
        f = FileMain()
        f.init_default()
        self.me.load(f)
        print "on_main_init"

    def on_abonds_load(self):
        print "on_abonds_load"

    def on_abonds_save(self):
        print "on_abonds_save"

    def on_abonds_init(self):
        f = FileAbonds()
        f.init_default()
        self.ae.load(f)
        print "on_abonds_init"


    def on_submit(self):
        self.__manager_form.show()
        self.__submit_job()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for signals emited by pyfant widgets

    def on_main_edited(self):
        print "on_main_edited  "
        # self.flag_changed = True
        # self.update_window_title()

    def on_abonds_edited(self):
        print "on_abonds_edited  "
        # self.flag_changed = True
        # self.update_window_title()




    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Internals

    def __submit_job(self):
        print "__submit_job"
        r = Pfant()
        r.conf.file_main = self.me.f
        r.conf.file_abonds = self.ae.f
        r.conf.flag_output_to_dir = True
        self.__rm.add_runnables([r])



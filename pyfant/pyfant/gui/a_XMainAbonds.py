"""Window to edit both main and abundances"""

__all__ = ["XMainAbonds"]

from PyQt4.QtGui import *
from .a_WFileMain import *
from .a_WFileAbonds import *
from .a_WOptionsEditor import *
from pyfant import *
from .guiaux import *
from . import XRunnableManager
from pyfant import *
import os
import os.path



################################################################################
class XMainAbonds(QMainWindow):
    """
    Arguments:
      parent=None -- nevermind
      file_main (optional)-- FileMain instance
    """

    def __init__(self, parent=None, file_main=None, file_abonds=None):
        ## State variables
        QMainWindow.__init__(self, parent)
        # XRunnableManager instance
        self._manager_form = None
        # RunnableManager instance
        self._rm = None
        self.save_dir = None
        self.load_dir = None

        # # Synchronized sequences
        # Used in generic operations where only certain parameters change
        self.tab_texts =  ["Main configuration (Alt+&1)",
                            "Abundances (Alt+&2)",
                            "Command-line options (Alt+&3)"]
        self.flags_changed = [False, False, None]
        self.save_as_texts = ["Save main configuration as...",
                               "Save abundances as...",
                               None]
        self.open_texts = ["Load main configuration file",
                            "Load abundances file",
                            None]
        self.clss = [FileMain, FileAbonds, Options]
        self.wilds = ["*.dat", "*.dat", None]

        # # Menu bar
        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")

        ac = m.addAction("&Open...")
        ac.setShortcut("Ctrl+O")
        ac.triggered.connect(self.on_open)

        m.addSeparator()

        ac = m.addAction("&Save")
        ac.setShortcut("Ctrl+S")
        ac.triggered.connect(self.on_save)

        ac = m.addAction("Save &as...")
        ac.setShortcut("Ctrl+Shift+S")
        ac.triggered.connect(self.on_save_as)

        ac = m.addAction("Save a&ll...")
        ac.setShortcut("Alt+Shift+S")
        ac.triggered.connect(self.on_save_all)

        m.addSeparator()

        ac = m.addAction("Reset to default")
        ac.setShortcut("Ctrl+R")
        ac.triggered.connect(self.on_reset)

        m.addSeparator()

        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

        m = self.menu_view = b.addMenu("&View")

        ac = m.addAction("Runnables &Manager")
        ac.setShortcut("Ctrl+M")
        ac.triggered.connect(self.on_show_rm)

        # # Central layout

        cw = self.centralWidget = QWidget()
        self.setCentralWidget(cw)
        la = self.centralLayout = QVBoxLayout(cw)

        # ## Main control bar
        # A layout is created and left blank for descendants to add
        # widgets as needed

        l = self.controlLayout = QHBoxLayout()
        la.addLayout(l)

        # ## Tabs

        # tab "File"
        tt = self.tabWidget = QTabWidget(self)
        la.addWidget(tt)
        tt.setFont(MONO_FONT)

        # ### Main configuration tab
        w0 = self.c27272 = QWidget()
        tt.addTab(w0, self.tab_texts[0])
        l0 = self.c12842 = QVBoxLayout(w0)

        # #### Main configuration file toolbar
        l1 = self.c29378 = QHBoxLayout()
        l0.addLayout(l1)
        w = QLabel("<b>File:<b>")
        l1.addWidget(w)
        w = self.label_fn_main = QLabel()
        l1.addWidget(w)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # #### Main file editor widget
        me = self.me = WFileMain()
        l0.addWidget(me)
        me.setFont(MONO_FONT)
        me.edited.connect(self.on_main_edited)
        if file_main is not None:
            self.me.load(file_main)
        # me.setFocus()

        # ### Abundances tab
        w0 = self.c10101 = QWidget()
        tt.addTab(w0, self.tab_texts[1])
        l0 = self.c54354 = QVBoxLayout(w0)

        # #### Abundances file toolbar
        l1 = self.c65478 = QHBoxLayout()
        w = QLabel("<b>File:</b>")
        l0.addLayout(l1)
        l1.addWidget(w)
        w = self.label_fn_abonds = QLabel()
        l1.addWidget(w)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # #### Abundances editor
        ae = self.ae = WFileAbonds()
        l0.addWidget(ae)
        ae.setFont(MONO_FONT)
        ae.edited.connect(self.on_abonds_edited)
        if file_abonds is not None:
            self.ae.load(file_abonds)

        # ### Command-line options tab
        w1 = QWidget()
        l1 = QVBoxLayout(w1)
        w0 = self.oe  = WOptionsEditor()
        l1.addWidget(w0)
        tt.addTab(w1, self.tab_texts[2])
        tt.tabBar()


        # ### Final tabs setup
        tt.setCurrentIndex(0)

        # ### These sequences couldn't be set above because the widgets didn't exist yet
        self.editors = [self.me, self.ae, None]
        self.labels_fn = [self.label_fn_main, self.label_fn_abonds, None]
        assert len(self.tab_texts) == len(self.flags_changed) == \
         len(self.save_as_texts) == len(self.open_texts) == len(self.clss) == \
         len(self.editors) == len(self.labels_fn)

        # # Loads default files
        if os.path.isfile(FileMain.default_filename):
            f = FileMain()
            f.load()
            self.me.load(f)
        if os.path.isfile(FileAbonds.default_filename):
            f = FileAbonds()
            f.load()
            self.ae.load(f)
        self._update_labels_fn()

        # # Initializes command-line options
        # At the moment, there is no way to save or load command-line options
        self.oe.load(Options())


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Interface

    def set_manager_form(self, x):
        assert isinstance(x, XRunnableManager)
        self._manager_form = x
        self._rm = x.rm

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    # Index checks are because descendant classes may add more tabs

    def on_open(self):
        if self.__tab_has_file_operations():
            self.__generic_open()

    def on_save(self):
        if self.__tab_has_file_operations():
            self.__generic_save()

    def on_save_as(self):
        if self.__tab_has_file_operations():
            self.__generic_save_as()

    def on_save_all(self):
        index_save = self.__get_index()
        try:
            for index in range(self.tabWidget.count()):
                self.tabWidget.setCurrentIndex(index)
                if not self.__generic_save():
                    # breaks if user cancels a "save as" operation
                    break
        finally:
            self.tabWidget.setCurrentIndex(index_save)

    def on_reset(self):
        if self.__tab_has_file_operations():
            self.__generic_reset()

    def on_show_rm(self):
        if self._manager_form:
            self._manager_form.show()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for signals emited by pyfant widgets

    def on_main_edited(self):
        self._on_edited()

    def on_abonds_edited(self):
        self._on_edited()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Protected methods to be overriden or used by descendant classes

    def _on_edited(self):
        index = self.__get_index()
        self.flags_changed[index] = True
        self.__update_tab_texts()

    def _check_single_setup(self):
        """Checks if setup parameters are valid.

        The verifications here apply both to single and multi mode.
        """
        errors = []
        if not self.me.f:
            errors.append("main configuration not set")
        if not self.ae.f:
            errors.append("abundances not set")
        if not self.me.flag_valid:
            errors.append("error(s) in main configuration")
        if not self.ae.flag_valid:
            errors.append("error(s) in abundances")
        if not self.oe.flag_valid:
            errors.append("error(s) in command-line options")
        return errors

    def _update_labels_fn(self):
        cwd = os.getcwd()
        for editor, label in zip(self.editors, self.labels_fn):
            if not label:
                continue
            if not editor.f:
                text = "(not loaded)"
            elif editor.f.filename:
                text = os.path.relpath(editor.f.filename, ".")
            else:
                text = "(filename not set)"
            label.setText(text)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Gear

    def __get_index(self):
        return self.tabWidget.currentIndex()

    def __generic_reset(self):
        index = self.__get_index()
        editor, text, cls = self.editors[index], self.open_texts[index], \
                                   self.clss[index]
        f = cls()
        f.init_default()
        editor.load(f)
        self.flags_changed[index] = True
        self.__update_tab_texts()
        self._update_labels_fn()

    def __generic_save(self):
        """Returns False if user has cancelled a "save as" operation, otherwise True."""
        index = self.__get_index()
        editor = self.editors[index]
        f = editor.f
        if not f:
            return True
        if f.filename:
            try:
                f.save_as()
                self.flags_changed[index] = False
                self.__update_tab_texts()
                return True
            except Exception as e:
                ShowError(str(e))
                raise
        else:
            return self.__generic_save_as()

    def __generic_save_as(self):
        """Returns False if user has cancelled operation, otherwise True."""
        index = self.__get_index()
        editor, text, wild = self.editors[index], self.save_as_texts[index], \
         self.wilds[index]
        if not editor.f:
            return True
        if editor.f.filename:
            d = editor.f.filename
        else:
            d = self.save_dir if self.save_dir is not None \
                else self.load_dir if self.load_dir is not None \
                else "."
        new_filename = QFileDialog.getSaveFileName(self, text, d, wild)
        if new_filename:
            self.save_dir, _ = os.path.split(str(new_filename))
            try:
                editor.f.save_as(str(new_filename))
                self.flags_changed[index] = False
                self._update_labels_fn()
                self.__update_tab_texts()
                return True
            except Exception as e:
                ShowError(str(e))
                raise
        return False

    def __generic_open(self):
        index = self.__get_index()
        editor, text, cls, label, wild = self.editors[index], \
         self.open_texts[index], self.clss[index], self.labels_fn[index], \
         self.wilds[index]
        try:
            d = self.load_dir if self.load_dir is not None \
                else self.save_dir if self.save_dir is not None \
                else "."
            new_filename = QFileDialog.getOpenFileName(self, text, d, wild)
            if new_filename:
                self.load_dir, _ = os.path.split(str(new_filename))
                f = cls()
                f.load(str(new_filename))
                editor.load(f)
                self._update_labels_fn()
                self.__update_tab_texts()
        except Exception as e:
            ShowError(str(e))
            raise

    def __tab_has_file_operations(self):
        return self.flags_changed[self.__get_index()] is not None

    def __update_tab_texts(self):
        for index, (text, flag_changed) in enumerate(zip(self.tab_texts, self.flags_changed)):
            self.tabWidget.setTabText(index,
             text+(" (changed)" if flag_changed else ""))
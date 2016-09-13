"""Window to edit both main and abundances"""

__all__ = ["XLogMainWindow", "XLogDialog", "XFileMainWindow", "NullEditor", "WBase"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
import traceback as tb
import datetime
from pyfant import *
from pyfant.gui import *
import os
from matplotlib import pyplot as plt

class _LogPart(object):
    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Interface

    def keep_ref(self, obj):
        """Adds obj to internal list to keep a reference to it.

        When using PyQt, it happens that the Python object gets garbage-collected even
        when a C++ Qt object still exists, causing a mess
        """
        self._refs.append(obj)
        return obj

    def add_log_error(self, x, flag_also_show=False):
        """Sets text of labelError."""
        if len(x) == 0:
            x = "(empty error)"
            tb.print_stack()
        x_ = x
        x = '<span style="color: %s">%s</span>' % (COLOR_ERROR, x)
        self.add_log(x, False)
        if flag_also_show:
            show_error(x_)

    def add_log(self, x, flag_also_show=False):
        """Sets text of labelDescr."""
        if hasattr(self, "label_last_log"):
            self.label_last_log.setText(x)
        if hasattr(self, "textEdit_log"):
            te = self.textEdit_log
            te.append("%s -- %s" % (datetime.datetime.strftime(datetime.datetime.now(), '%Y-%m-%d %H:%M:%S'), x))
        else:
            get_python_logger().info(x)
        if flag_also_show:
            show_error(x)


class XLogMainWindow(QMainWindow, _LogPart):
    def __init__(self, parent=None):
        QMainWindow.__init__(self, parent)
        _LogPart.__init__(self)
        self._refs = []


class XLogDialog(QDialog, _LogPart):
    def __init__(self, parent=None):
        QDialog.__init__(self, parent)
        _LogPart.__init__(self)
        self._refs = []


class XFileMainWindow(XLogMainWindow):
    """Application template with file operations in two tabs: (application area) and (log)"""

    def __init__(self, parent=None):
        def keep_ref(obj):
            self._refs.append(obj)
            return obj

        XLogMainWindow.__init__(self, parent)
        self._refs = []
        # XRunnableManager instance
        self._manager_form = None
        # RunnableManager instance
        self._rm = None
        self.save_dir = None
        self.load_dir = None

        # # Synchronized sequences
        # Replace first element of each attribute at child class
        self.tab_texts =  ["------ (Alt+&1)", "Log (Alt+&2)"]
        self.flags_changed = [False, False]
        self.save_as_texts = [None, None]
        self.open_texts = [None, None]
        self.clss = [None, None]  # save class
        self.clsss = [None, None]  # accepted load classes
        self.wilds = [None, None]  # e.g. '*.fits'
        self.editors = [NullEditor(), NullEditor()]  # editor widgets, must comply ...

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

        ac = m.addAction("Load &default")
        ac.setStatusTip("Loads default")
        ac.setShortcut("Ctrl+D")
        ac.triggered.connect(self.on_reset)

        m.addSeparator()

        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)


        # # Central layout
        ##################
        cw = self.centralWidget = QWidget()
        self.setCentralWidget(cw)
        lantanide = self.centralLayout = QVBoxLayout(cw)
        lantanide.setMargin(1)

        # ## Tabs
        tt = self.tabWidget = QTabWidget(self)
        lantanide.addWidget(tt)
        tt.setFont(MONO_FONT)
        # ### Custom Area tab will have a widget **without** a layout, named "gotting"
        w0 = self.gotting = QWidget()
        tt.addTab(w0, self.tab_texts[0])

        # ### Log tab
        te = self.textEdit_log = keep_ref(QTextEdit())
        te.setReadOnly(True)
        tt.addTab(te, self.tab_texts[1])

        # ### Final tabs setup
        tt.setCurrentIndex(0)

        # ## "Last Log" label
        x = self.label_last_log = QLabel(self)
        lantanide.addWidget(x)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Interface

    def load(self, f):
        index = self._get_tab_index()
        if not isinstance(f, self.clsss[index]):
            raise RuntimeError('Object to load must be in %s (not a %s)' % ([x.__name__ for x in self.clsss[index]], f.__class__.__name__))
        f = self._filter_on_load(f)
        editor = self.editors[index]
        editor.load(f)
        self._update_tab_texts()


    def keep_ref(self, obj):
        """Adds obj to internal list to keep a reference to it.

        WHen using PyQt, it happens that the Python object gets garbage-collected even
        when a C++ Qt object still exists, causing a mess
        """
        self._refs.append(obj)
        return obj

    def add_log_error(self, x, flag_also_show=False):
        """Sets text of labelError."""
        if len(x) == 0:
            x = "(empty error)"
            tb.print_stack()
        x_ = x
        x = '<span style="color: %s">%s</span>' % (COLOR_ERROR, x)
        self.label_last_log.setText(x)
        self.add_log(x, False)
        if flag_also_show:
            show_error(x_)


    def add_log(self, x, flag_also_show=False):
        """Sets text of labelDescr."""
        if hasattr(self, "textEdit_log"):
            te = self.textEdit_log
            te.append("%s -- %s" % (datetime.datetime.strftime(datetime.datetime.now(), '%Y-%m-%d %H:%M:%S'), x))
        else:
            get_python_logger().info(x)
        if flag_also_show:
            show_error(x)

    def set_manager_form(self, x):
        assert isinstance(x, XRunnableManager)
        self._manager_form = x
        self._rm = x.rm


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Qt override

    def closeEvent(self, event):
        flag_exit, ff = True, []
        for ed, flag_changed in zip(self.editors, self.flags_changed):
            if ed and ed.f and flag_changed:
                ff.append(ed.f.description)

        if len(ff) > 0:
            s = "Unsaved changes\n  -" + ("\n  -".join(ff)) + "\n\nAre you sure you want to exit?"
            flag_exit = are_you_sure(True, event, self, "Unsaved changes", s)
        if flag_exit:
            plt.close("all")


    def keyPressEvent(self, evt):
        incr = 0
        if evt.modifiers() == Qt.ControlModifier:
            n = self.tabWidget.count()
            if evt.key() in [Qt.Key_PageUp, Qt.Key_Backtab]:
                incr = -1
            elif evt.key() in [Qt.Key_PageDown, Qt.Key_Tab]:
                incr = 1
            if incr != 0:
                new_index = self._get_tab_index() + incr
                if new_index < 0:
                    new_index = n - 1
                elif new_index >= n:
                    new_index = 0
                self.tabWidget.setCurrentIndex(new_index)


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    # All file operations

    def on_open(self):
        if self._tab_has_file_operations():
            self.__generic_open()

    def on_save(self):
        if self._tab_has_file_operations():
            self.__generic_save()

    def on_save_as(self):
        if self._tab_has_file_operations():
            self.__generic_save_as()

    def on_save_all(self):
        index_save = self._get_tab_index()
        try:
            for index in range(self.tabWidget.count()):
                self.tabWidget.setCurrentIndex(index)
                if not self.__generic_save():
                    # breaks if user cancels a "save as" operation
                    break
        finally:
            self.tabWidget.setCurrentIndex(index_save)

    def on_reset(self):
        if self._tab_has_file_operations():
            idx = self._get_tab_index()
            editor = self.editors[idx]
            flag_ok = True
            if editor.f:
                descr = self._get_tab_description()
                r = QMessageBox.question(self, "Load default", "Current setup "
                                                               "for %s will be overwritten with a 'default' setup.\n\n"
                                                               "Confirm?" % descr, QMessageBox.Yes | QMessageBox.No,
                                         QMessageBox.Yes)
                flag_ok = r == QMessageBox.Yes
            if flag_ok:
                self.__generic_reset()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Protected methods to be overriden or used by descendant classes

    def _on_edited(self):
        print "def %s._on_edited(self):" % self.__class__.__name__
        index = self._get_tab_index()
        self.flags_changed[index] = True
        self._update_tab_texts()



    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Protected methods to override as needed

    def _filter_on_load(self, f):
        """Perform any necessary on-load file type conversions here: should all converge to same class"""
        return f

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Gear

    def _get_tab_index(self):
        """Returns index of current selected tab."""
        return self.tabWidget.currentIndex()

    def _get_tab_description(self):
        """Returns "description" of current tab (tab text without shortcut info)."""
        idx = self._get_tab_index()
        text = self.tab_texts[idx]
        if "(" in text:
            text = text[:text.index("(") - 1]
        text = text[0].lower() + text[1:]
        return text

    def _tab_has_file_operations(self):
        return self.flags_changed[self._get_tab_index()] is not None

    def _update_tab_texts(self):
        for index, (text, flag_changed) in enumerate(zip(self.tab_texts, self.flags_changed)):
            self.tabWidget.setTabText(index,
                                      text + (" (changed)" if flag_changed else ""))

    def __generic_reset(self):
        index = self._get_tab_index()
        editor, text, cls = self.editors[index], self.open_texts[index], \
                            self.clss[index]
        f = cls()
        f.init_default()
        editor.load(f)
        self.flags_changed[index] = True
        self._update_tab_texts()

    def __generic_save(self):
        """Returns False if user has cancelled a "save as" operation, otherwise True."""
        index = self._get_tab_index()
        editor = self.editors[index]
        f = editor.f
        if not f:
            return True
        if not editor.flag_valid:
            show_error("Cannot save, %s has error(s)!" % f.description)
            return True
        if f.filename:
            try:
                f.save_as()
                self.flags_changed[index] = False
                self._update_tab_texts()
                editor.update_gui_label_fn()  # duck typing
                return True
            except Exception as e:
                show_error(str(e))
                raise
        else:
            return self.__generic_save_as()

    def __generic_save_as(self):
        """Returns False if user has cancelled operation, otherwise True."""
        index = self._get_tab_index()
        editor, text, wild = self.editors[index], self.save_as_texts[index], \
                             self.wilds[index]
        if not editor.f:
            return True
        if editor.f.filename:
            d = editor.f.filename
        else:
            d = os.path.join(self.save_dir if self.save_dir is not None \
                                 else self.load_dir if self.load_dir is not None \
                else ".", editor.f.default_filename)
        new_filename = QFileDialog.getSaveFileName(self, text, d, wild)
        if new_filename:
            self.save_dir, _ = os.path.split(str(new_filename))
            try:
                editor.f.save_as(str(new_filename))
                self.flags_changed[index] = False
                self._update_tab_texts()
                editor.update_gui_label_fn()
                return True
            except Exception as e:
                show_error(str(e))
                raise
        return False

    def __generic_open(self):
        index = self._get_tab_index()
        editor, text, cls, wild = self.editors[index], \
                                  self.open_texts[index], self.clss[index], self.wilds[index]
        try:
            d = self.load_dir if self.load_dir is not None \
                else self.save_dir if self.save_dir is not None \
                else "."
            new_filename = QFileDialog.getOpenFileName(self, text, d, wild)
            if new_filename:
                self.load_filename(new_filename)
        except Exception as e:
            show_error(str(e))
            raise

    def load_filename(self, filename, index=None):
        """
        Loads file given filename

        Arguments:
            filename --
            index -- tab index to load file into. If not passed, loads into current tab
        """
        filename = str(filename)  # QString protection
        if index is None:
            index = self._get_tab_index()
        editor, text, cls, wild, possible_classes = self.editors[index], \
                                                    self.open_texts[index], self.clss[index], self.wilds[index], \
                                                    self.clsss[index]
        self.load_dir, _ = os.path.split(filename)
        f = load_with_classes(filename, possible_classes)
        if f is None:
            raise RuntimeError("Could not load '%s'" % filename)

        f = self._filter_on_load(f)

        self.load(f)


class NullEditor(object):
    """Class to fulfill requirement in XFileMainWindow, easiest way out, no bother"""
    f = None



class WBase(QWidget):
    """Widget with 'edited' signal, keep_ref(), logging tools"""
    # Emitted whenever any value changes
    edited = pyqtSignal()

    def __init__(self, parent):
        assert isinstance(parent, (XLogMainWindow, XLogDialog))
        QWidget.__init__(self, parent)
        self._refs = []
        self.parent_form = parent


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Interface

    def keep_ref(self, obj):
        """Adds obj to internal list to keep a reference to it.

        WHen using PyQt, it happens that the Python object gets garbage-collected even
        when a C++ Qt object still exists, causing a mess
        """
        self._refs.append(obj)
        return obj


    def add_log_error(self, x, flag_also_show=False):
        """Delegates to parent form"""
        self.parent_form.add_log_error(x, flag_also_show)

    def add_log(self, x, flag_also_show=False):
        """Delegates to parent form"""
        self.parent_form.add_log(x, flag_also_show)
        
    def update_gui_label_fn(self):
        pass
        
        



"""Explorer window."""
__all__ = ["XExplorer"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
import numpy as np
from .guiaux import *
import os
import glob
import os.path
import time
from pyfant import *
from .a_XText import *
import matplotlib.pyplot as plt

COLOR_LOADED = "#ADFFB4"  # light green
COLOR_LOAD_ERROR = "#FFB5B5"  # light red
COLOR_DIR = "#FFFFB5"  # light yellow

@froze_it
class _FileProps(object):
    def __init__(self):
        # DataFile object or None
        self.f = None
        # string or None
        self.filepath = None
        # Already "scanned" (load attempted)
        self.flag_scanned = False
        # Error loading?
        self.flag_error = False
        # Is text?
        self.flag_text = False
        # Is file (name to match os.path.isfile
        self.isfile = False
        # Error message
        self.error_message = ""
        # Row index in table
        self.row_index = None

    def get_color(self):
        """Returns None if background not to be changed, otherwise a QColor."""
        if not self.isfile:
            return QColor(COLOR_DIR)
        elif self.flag_error:
            return QColor(COLOR_LOAD_ERROR)
        elif not self.flag_scanned:
            return None
        return QColor(COLOR_LOADED)

    def get_summary(self):
        ret = "File not scanned yet"
        if not self.isfile:
            ret = '<span style="color: #B38600">Directory</span>'
        elif self.flag_scanned:
            aa = ["text file" if self.flag_text else "binary file"]
            if self.flag_error:
                emsg = self.error_message or "not recognized as a PFANT data file"
                aa.append("<span style=\"font-weight: bold; color: %s\">%s</span>" %
                          (COLOR_ERROR, emsg))
            else:
                aa.append("<span style=\"color: %s\">PFANT type: <b>%s</b></span>" %
                          ("#006000", self.f.__class__.__name__))
            ret = "<b>Summary</b>: "+", ".join(aa)
        return ret

    def get_info(self):
        ret = ""
        if self.flag_scanned and not self.flag_error:
            ret = str(self.f)
        return ret

class LoadThread(QThread):
    def __init__(self, parent, propss):
        QThread.__init__(self, parent)
        self.propss = propss

    def run(self):
        for props in self.propss:
            if not props.isfile:
                continue
            f = None
            try:
                f = load_any_file(props.filepath)
                props.f = f
            except Exception, e:
                # Suppresses exceptions
                props.error_message = str(e)
            finally:
                props.flag_scanned = True
                props.flag_error = f is None
                props.flag_text = istextfile(props.filepath)

#
# class VisThread(QThread):
#     def __init__(self, parent, vis_class, props):
#         QThread.__init__(self, parent)
#         self.vis_class = vis_class
#         self.props = props
#         self.form = parent
#
#     def run(self):
#         vis_class = self.vis_class
#         props = self.props
#         if vis_class == "txt":
#             w = XText(self.form, open(props.filepath, "r").read(), props.filepath)
#             w.show()
#         else:
#             vis_class().use(props.f)


class XExplorer(QMainWindow):
    """Window to explore files in directory from PFANT point of view.

    Arguments:
      parent=None
      dir_="." -- directory name
    """

    def __init__(self, parent=None, dir_="."):
        QMainWindow.__init__(self, parent)
        self.dir = None
        # Whether to close matplotlib plots when window is closed
        self.flag_close_mpl_plots_on_close = True
        self.__vis_classes = []
        # List of (DataFile objects)/(None if not loaded)/(-1 if error loading)
        self.__propss = []
        # # More internals
        self.__flag_loading = False
        self.__flag_visualizing = False
        self.__load_thread = None

        # # Menu bar
        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")
        ac = m.addAction("Attempt &load")
        ac.setShortcut("Ctrl+L;Ctrl+O")
        ac.triggered.connect(self.on_load)
        m.addSeparator()
        ac = m.addAction("&Refresh")
        ac.setShortcut("Ctrl+R")
        ac.triggered.connect(self.on_refresh)
        m.addSeparator()
        ac = m.addAction("Change &directory...")
        ac.setShortcut("Ctrl+D")
        ac.triggered.connect(self.on_cd)
        m.addSeparator()
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

        # # Status bar
        self.labelStatus = QLabel("Welcome")
        sb = self.statusBar()
        sb.insertWidget(0, self.labelStatus, 0)


        # # Horizontal splitter
        # Containis a table on the left and a "possibilities are" on the right

        sp = self.splitter = QSplitter(Qt.Horizontal)

        # ## File listing table

        t = self.tableWidget = QTableWidget()
        sp.addWidget(t)
        t.installEventFilter(self)
        # t.setSelectionBehavior(QAbstractItemView.SelectRows)
        # t.currentCellChanged.connect(self.on_tableWidget_currentCellChanged)
        t.cellDoubleClicked.connect(self.on_tableWidget_cellDoubleClicked)
        t.setEditTriggers(QTableWidget.NoEditTriggers)
        t.setFont(MONO_FONT)
        t.resizeColumnsToContents()
        t.installEventFilter(self)
        t.selectionModel().selectionChanged.connect(self.selectionChanged)


        # ## "Possibilities area"
        # The area is a widget that will be added to the main splitter.
        # The area will contain a vertical layout with two widgets: a label
        # and another splitter.

        w = QWidget()
        sp.addWidget(w)
        l = QVBoxLayout(w)

        # ### Label to indicate the detected file type
        x = self.labelFileType = QLabel()
        l.addWidget(x)

        # ### Splitter containing a "visualization options area"
        #     and a "general file info" area
        s0 = QSplitter(Qt.Vertical)
        l.addWidget(s0)

        # #### "visualization options" area
        w = self.csslff = QWidget()
        s0.addWidget(w)
        l = self.c49378 = QVBoxLayout(w)
        l.setMargin(0)
        y = self.c88888 = QLabel("<b>Visualization &options</b>")
        l.addWidget(y)
        x = self.listWidgetVis = QListWidget(self)
        x.installEventFilter(self)
        x.itemDoubleClicked.connect(self.on_listWidgetVis_itemDoubleClicked)
        y.setBuddy(x)

        l.addWidget(x)

        # #### "general file info" area
        w = self.c77777 = QWidget()
        s0.addWidget(w)
        l = self.c86888 = QVBoxLayout(w)
        l.setMargin(0)
        x = self.c83388 = QLabel("<b>Data file information</b>")
        l.addWidget(x)
        x = self.textEditInfo = QTextEdit(self)
        l.addWidget(x)
        x.setReadOnly(True)
        x.setFont(MONO_FONT)

        self.setCentralWidget(sp)
        s0.setStretchFactor(0, 3)
        s0.setStretchFactor(1, 10)

        self.set_dir(dir_)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # *
    # Qt override

    def eventFilter(self, obj, event):
        if obj == self.tableWidget:
            return check_return_space(event, self.on_load)
        elif obj == self.listWidgetVis:
            return check_return_space(event, self.__visualize)
        return False
    #
    # def eventFilter(self, source, event):
    #     if event.type() == QEvent.KeyPress:
    #         if event.key() == Qt.Key_Return:
    #             if source == self.tableWidget:
    #                 self.tableWidget.editItem(self.tableWidget.currentItem())
    #                 return True
    #     return False



    def closeEvent(self, event):
        if self.flag_close_mpl_plots_on_close:
            plt.close("all")

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # *
    # Slots for Qt library signals

    # def on_tableWidget_currentCellChanged(self, currentRow, currentColumn, previousRow,
    #                                       previousColumn):
    #     print "QUAL EH A LINHA", self.tableWidget.currentRow()
    #     return
    #     self.__update_info()


    def on_tableWidget_cellDoubleClicked(self, row, col):
        # Assuming that double-click will also select the row of the file to load
        self.on_load()

    def on_listWidgetVis_itemDoubleClicked(self, item):
        # Assuming that double-click will also select the row
        self.__visualize()

    def selectionChanged(self, *args):
        self.__update_info()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # *
    # Slots for Qt library signals

    def on_load(self, _=None):
        if not self.__flag_loading:
            pp = self.__get_current_propss()
            if len(pp) == 1 and not pp[0].isfile:
                self.set_dir(pp[0].filepath)
            else:
                self.__load()

    def on_refresh(self, _=None):
        if not self.__flag_loading:
            self.set_dir(self.dir)

    def on_cd(self, _=None):
        if not self.__flag_loading:
            dir_ = QFileDialog.getExistingDirectory(None, "Change directory", self.dir)
            if dir_:
                self.set_dir(str(dir_))

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # *
    # Internals


    def __update_window_title(self):
        self.setWindowTitle("PFANT explorer -- %s" % self.dir)

    def __get_file_list(self, isfile):


        all_ = [os.path.join(self.dir, x) for x in os.listdir(self.dir)]
        dirs = filter(os.path.isdir, all_)

    def set_dir(self, dir_):
        """Sets set of lines."""
        self.dir = os.path.relpath(dir_)
        self.__propss = []



        all_ = glob.glob(os.path.join(self.dir, "*"))
        dirs = filter(os.path.isdir, all_)
        dirs.sort()
        files = filter(os.path.isfile, all_)
        files.sort()
        print "DIRS", dirs
        print "FILES", files
        dir_ = [os.path.join(self.dir, "..")]+dirs+files
        print "ALL", all_
        print "DIRS", dirs
        print "FILES", files
        print "DIR_", dir_

        for i, f in enumerate(dir_):
            filepath = f  #os.path.join(self.dir, f)
            p = _FileProps()
            p.row_index = i
            p.isfile = os.path.isfile(filepath)
            p.filepath = filepath
            self.__propss.append(p)
        self.__update_table()
        self.__update_info()
        self.__update_window_title()

    def __update_table(self):
        n = len(self.__propss)

        t = self.tableWidget
        ResetTableWidget(t, n, 3)
        t.setHorizontalHeaderLabels(["filename", "size", "date modified"])

        for i, p in enumerate(self.__propss):
            items = []
            info = os.stat(p.filepath)
            filename = os.path.basename(p.filepath)
            # if not p.isfile:
            #     filename = "["+filename+"]"
            item = QTableWidgetItem(filename)
            items.append(item)
            t.setItem(i, 0, item)
            item = QTableWidgetItem(str(info.st_size))
            items.append(item)
            item.setTextAlignment(Qt.AlignRight | Qt.AlignVCenter)

            t.setItem(i, 1, item)
            item = QTableWidgetItem(time.ctime(info.st_mtime))
            items.append(item)
            t.setItem(i, 2, item)

            color = p.get_color()
            if color is not None:
                for item in items:
                    item.setBackground(color)

        t.resizeColumnsToContents()

    def __get_current_props(self):
        if len(self.__propss) > 0:
            return self.__propss[self.tableWidget.currentRow()]
        return None

    def __get_current_propss(self):
        ii = self.tableWidget.selectedIndexes()
        i_rows = set([idx.row() for idx in ii])
        # print "AS LINHA", i_rows
        pp = [self.__propss[i_row] for i_row in i_rows]
        return pp

    def __get_current_vis_class(self):
        return self.__vis_classes[self.listWidgetVis.currentRow()]

    def __update_info(self):
        """Updates "visualization options" and "file info" areas."""
        t = self.tableWidget
        z = self.listWidgetVis
        z.clear()
        classes = self.__vis_classes = []
        pp = self.__get_current_propss()
        # print "RRRRRRRRRRRRRR", len(pp), pp
        if len(pp) == 1:
            p = pp[0]
            # Visualization options
            if p.flag_scanned:
                if isinstance(p.f, DataFile):
                    classes.extend(get_suitable_vis_classes(p.f))
                    if VisPrint in classes:
                        classes.remove(VisPrint)
                if p.flag_text:
                    # This is an exception, since "txt" is not a Vis descendant.
                    # This will be properly handled in __visualize()
                    classes.append("txt")


                for x in classes:
                    if x == "txt":
                        text = "View plain text"
                    else:
                        text = x.__name__
                    item = QListWidgetItem(text)
                    z.addItem(item)

            # File info
            self.labelFileType.setText(p.get_summary())
            self.textEditInfo.setPlainText(p.get_info())
        elif len(pp) >= 2:
            ff = [p.f for p in pp]
            flag_spectra = all([isinstance(f, FileSpectrum) for f in ff])
            if flag_spectra:
                z.addItem(QListWidgetItem("View spectra stacked"))
                classes.append("sta")
                z.addItem(QListWidgetItem("View spectra overlapped"))
                classes.append("ovl")


    def __set_status_text(self, text):
        self.labelStatus.setText(text)

    def __load(self):
        pp = self.__get_current_propss()
        if len(pp) > 0:
            self.__flag_loading = True
            self.__set_status_text("Loading file(s), please wait...")
            t = self.__load_thread = LoadThread(self, pp)
            t.finished.connect(self.__finished_loading)
            t.start()

    def __visualize(self):
        self.__flag_visualizing = True
        self.__set_status_text("Creating visualization, please wait...")
        try:
            vis_class = self.__get_current_vis_class()
            if vis_class == "ovl":
                pp = self.__get_current_propss()
                spectra = [p.f.spectrum for p in pp]
                plot_spectra_overlapped(spectra)
            elif vis_class == "sta":
                pp = self.__get_current_propss()
                spectra = [p.f.spectrum for p in pp]
                plot_spectra(spectra)
            else:
                props = self.__get_current_props()
                if vis_class == "txt":
                    w = XText(self, open(props.filepath, "r").read(), props.filepath)
                    w.show()
                else:
                    vis_class().use(props.f)
        finally:
            self.__set_status_text("")
            self.__flag_visualizing = False
        # t = VisThread(self, self.__get_current_vis_class(), self.__get_current_props())
        # t.finished.connect(self.__finished_visualizing)
        # t.start()

    def __finished_loading(self):
        self.__set_status_text("")
        self.__flag_loading = False
        t = self.tableWidget
        row = t.currentRow()
        props = self.__get_current_props()
        for props in self.__load_thread.propss:
            i, c = props.row_index, props.get_color()
            for j in range(t.columnCount()):
                t.item(i, j).setBackground(c)
        self.__update_info()

    # def __finished_visualizing(self):
    #     self.__set_status_text("")
    #     self.__flag_visualizing = False

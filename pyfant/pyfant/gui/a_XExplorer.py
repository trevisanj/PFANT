"""Explorer window."""
__all__ = ["XExplorer"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
import numpy as np
from .guiaux import *
import os
import os.path
import time
from pyfant import load_any_file

COLOR_LOADED = "#ADFFB4"  # light green
COLOR_LOAD_ERROR = "#FFB5B5"  # light red

class XExplorer(QMainWindow):
    """Window to explore files in directory from PFANT point of view.

    Arguments:
      parent=None
      dir_="." -- directory name
    """

    def __init__(self, parent=None, dir_="."):
        QMainWindow.__init__(self, parent)
        self.dir = None



        # # Menu bar

        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")
        ac = m.addAction("Attempt &load")
        ac.setShortcut("Ctrl+L")
        ac.triggered.connect(self.on_load)
        m.addSeparator()
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)


        # # ...


        t = self.tableWidget = QTableWidget()
        t.setSelectionBehavior(QAbstractItemView.SelectRows)
        t.currentCellChanged.connect(self.on_tableWidget_currentCellChanged)
        t.cellDoubleClicked.connect(self.on_tableWidget_cellDoubleClicked)
        t.setEditTriggers(QTableWidget.NoEditTriggers)
        t.setFont(MONO_FONT)
        t.resizeColumnsToContents()

        self.setCentralWidget(t)

        self.set_dir(dir_)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # *
    # Slots for Qt library signals

    def on_tableWidget_currentCellChanged(self, currentRow, currentColumn, previousRow,
                                          previousColumn):
        pass


    def on_tableWidget_cellDoubleClicked(self, row, col):
        # Assuming that double-click will also select the row of the file to load
        self.on_load()

    # Slots for Qt library signals
    def on_load(self, _=None):
        t = self.tableWidget
        row = t.currentRow()
        filepath = os.path.join(self.dir, self.filenames[row])

        f = load_any_file(filepath)
        print "File is a "+f.__class__.__name__
        print f

        color = COLOR_LOAD_ERROR if f is None else COLOR_LOADED

        for j in range(t.columnCount()):
            t.item(row, j).setBackground(QColor(color))



    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # *

    def __update_window_title(self):
        self.setWindowTitle("PFANT explorer -- %s" % self.dir)

    def set_dir(self, dir_):
        """Sets set of lines."""
        self.dir = dir_
        self.__update_table()
        self.__update_window_title()



    def __update_table(self):
        ff = self.filenames = \
         [f for f in os.listdir(self.dir) if os.path.isfile(os.path.join(self.dir, f))]
        n = len(ff)

        t = self.tableWidget
        ResetTableWidget(t, n, 3)
        t.setHorizontalHeaderLabels(["filename", "size", "date modified"])

        for i, filename in enumerate(ff):
            info = os.stat(filename)
            item = QTableWidgetItem(filename)
            t.setItem(i, 0, item)
            item = QTableWidgetItem(str(info.st_size))
            item.setTextAlignment(Qt.AlignRight | Qt.AlignVCenter)

            t.setItem(i, 1, item)
            item = QTableWidgetItem("not yet")
            t.setItem(i, 2, item)
            item = QTableWidgetItem(time.ctime(info.st_mtime))
            t.setItem(i, 3, item)

        t.resizeColumnsToContents()

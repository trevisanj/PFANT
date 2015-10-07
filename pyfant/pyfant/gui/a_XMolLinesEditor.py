
__all__ = ["XMolLinesEditor"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
import numpy as np
from ._guiaux import *

class XMolLinesEditor(QMainWindow):

    def __init__(self, parent):
        QMainWindow.__init__(self)

        # objects in matplotlib window

        self.parent = parent
        self.flag_populating = False  # activated when populating table

        a = self.tableWidget = QTableWidget()
        a.setSelectionMode(QAbstractItemView.SingleSelection)
        a.currentCellChanged.connect(self.on_tableWidget_currentCellChanged)
        a.cellChanged.connect(self.on_tableWidget_cellChanged)
        a.setEditTriggers(QAbstractItemView.DoubleClicked | QAbstractItemView.EditKeyPressed)
        a.setFont(MONO_FONT)

        self.setCentralWidget(a)
        rect = QApplication.desktop().screenGeometry()
        W = 200  # fixed value for the width for now
        self.setGeometry(rect.width()-W, 0, W, rect.height())

    def on_tableWidget_currentCellChanged(self, currentRow, currentColumn, previousRow,
                                          previousColumn):
        self.parent.MolLinesEditor_current_row_changed(currentRow)

    def on_tableWidget_cellChanged(self, row, column):
        if not self.flag_populating:
            value = float(self.tableWidget.item(row, column).text())
            self.parent.MolLinesEditor_cell_changed(row, column, value)

    def closeEvent(self, _):
        self.parent.MolLinesEditor_closing()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # *

    def set_sol(self, sol, title):
        """Sets set of lines."""

        assert isinstance(sol, SetOfLines)

        self.flag_populating = True
        try:
            self.setWindowTitle(title)

            t = self.tableWidget
            n = len(sol)
            a = ["lambda", "sj", "jj"]
            ResetTableWidget(t, n, len(a))
            t.setHorizontalHeaderLabels(a)

            for i in xrange(len(sol)):
                item = QTableWidgetItem(str(sol.lmbdam[i]))
                t.setItem(i, 0, item)

                item = QTableWidgetItem(str(sol.sj[i]))
                t.setItem(i, 1, item)

                item = QTableWidgetItem(str(sol.jj[i]))
                t.setItem(i, 2, item)

            t.resizeColumnsToContents()
        finally:
            self.flag_populating = False


    def set_row(self, i):
        t = self.tableWidget
        c = t.currentColumn()
        self.tableWidget.setCurrentCell(i, c)
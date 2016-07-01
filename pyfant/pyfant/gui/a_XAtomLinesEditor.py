__all__ = ["XAtomLinesEditor"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
import numpy as np
from .guiaux import *

class XAtomLinesEditor(QMainWindow):

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
        a.installEventFilter(self)


        self.setCentralWidget(a)
        snap_right(self, 360)

    def on_tableWidget_currentCellChanged(self, currentRow, currentColumn, previousRow,
                                          previousColumn):
        self.parent.AtomLinesEditor_current_row_changed(currentRow)

    def on_tableWidget_cellChanged(self, row, column):
        if not self.flag_populating:
            item = self.tableWidget.item(row, column)
            try:
                value = float(item.text())
            except ValueError:
                # restores original value
                show_error("Invalid floating point value: %s" % item.text())
                item.setText(str(self.parent.atom.__getattribute__(ATOM_ATTR_NAMES[column])[row]))
            else:
                self.parent.AtomLinesEditor_cell_changed(row, column, value)

    def closeEvent(self, _):
        self.parent.AtomLinesEditor_closing()

    def eventFilter(self, source, event):
        if event.type() == QEvent.KeyPress:
            if event.key() == Qt.Key_Return:
                if source == self.tableWidget:
                    self.tableWidget.editItem(self.tableWidget.currentItem())
                    return True
        return False

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # *

    def set_atom(self, atom, title):
        """Sets set of lines."""

        assert isinstance(atom, Atom)

        self.flag_populating = True
        try:
            self.setWindowTitle(title)

            t = self.tableWidget
            n = len(atom)
            ResetTableWidget(t, n, len(ATOM_HEADERS))
            t.setHorizontalHeaderLabels(ATOM_HEADERS)

            # list with the vectors themselves
            attrs = [atom.__getattribute__(x) for x in ATOM_ATTR_NAMES]

            for i in xrange(len(atom)):
                for j, attr in enumerate(attrs):
                    item = QTableWidgetItem(str(attr[i]))
                    t.setItem(i, j, item)

            t.resizeColumnsToContents()
        finally:
            self.flag_populating = False


    def set_row(self, i):
        t = self.tableWidget
        c = t.currentColumn()
        self.tableWidget.setCurrentCell(i, c)
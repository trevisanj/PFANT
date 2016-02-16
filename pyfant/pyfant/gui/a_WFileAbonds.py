"""Widget to edit a FileAbonds object."""

__all__ = ["WFileAbonds"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from ._guiaux import *
from .guimisc import *
from pyfant import FileAbonds, adjust_atomic_symbol


ABONDS_HEADERS = ["Element", "Abundance", "Notes"]
NOTES_COLUMN_WIDTH = 200

class WFileAbonds(QWidget):
    """
    FileAbonds editor widget.

    Arguments:
      parent=None
    """

    # Emitted whenever any value changes
    edited = pyqtSignal()

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        # Whether all the values in the fields are valid or not
        self.flag_valid = True
        # Internal flag to prevent taking action when some field is updated programatically
        self.flag_process_changes = False
        self.f = None # FileAbonds object

        la = self.formLayout = QVBoxLayout()
        self.setLayout(la)

        a = self.tableWidget = QTableWidget()
        a.setSelectionMode(QAbstractItemView.SingleSelection)
        #a.currentCellChanged.connect(self.on_tableWidget_currentCellChanged)
        a.cellChanged.connect(self.on_tableWidget_cellChanged)
        a.setEditTriggers(QAbstractItemView.DoubleClicked | QAbstractItemView.EditKeyPressed)
        a.setFont(MONO_FONT)
        a.installEventFilter(self)
        la.addWidget(a)

        self.labelError = QLabel(self)
        self.labelError.setStyleSheet("color: #AA0000")
        la.addWidget(self.labelError)

        self.flag_process_changes = True


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def load(self, x):
        assert isinstance(x, FileAbonds)
        self.f = x
        self._update_from_file_abonds()
        # this is called to perform file validation upon loading
        self._update_file_abonds()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def setFocus(self, reason=None):
        """Sets focus to first field. Note: reason is ignored."""
        self.tableWidget.setFocus()

    def eventFilter(self, source, event):
        if event.type() == QEvent.KeyPress:
            if event.key() == Qt.Key_Return:
                if source == self.tableWidget:
                    self.tableWidget.editItem(self.tableWidget.currentItem())
                    return True
        return False


    # def Validate(self):
    #     # Currently calling Parameters.GetKwargs() to do the <quote>validation<quote>
    #     self._parameters.UpdateFromWidgets()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    def on_tableWidget_cellChanged(self, row, column):
        if self.flag_process_changes:
            self._update_file_abonds()
            self._update_file_abonds()
            self.edited.emit()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear

    def _set_error_text(self, x):
        """Sets text of labelError."""
        self.labelError.setText(x)

    def _update_from_file_abonds(self):
        self.flag_process_changes = False
        try:
            o, t = self.f, self.tableWidget
            n = len(o)
            ResetTableWidget(t, n, len(ABONDS_HEADERS))
            t.setHorizontalHeaderLabels(ABONDS_HEADERS)

            # list with the vectors themselves

            attrs = [o.__getattribute__(x) for x in ["ele", "abol", "notes"]]

            for i in xrange(len(o)):
                for j, attr in enumerate(attrs):
                    item = QTableWidgetItem(str(attr[i]).strip())
                    t.setItem(i, j, item)
            t.resizeColumnsToContents()
            t.setColumnWidth(2, NOTES_COLUMN_WIDTH)  # Make room for notes
        finally:
            self.flag_process_changes = True

    def _update_file_abonds(self):
        o = self.f
        emsg, flag_error = "", False
        ss = ""
        try:
            o, t = self.f, self.tableWidget
            assert isinstance(t, QTableWidget)
            n = t.rowCount()
            ele, abol, notes = [], [], []

            for i in range(n):
                # # Element
                item = t.item(i, 0)
                x = str((item.text())).upper().strip()
                if len(x) > 2:
                    raise RuntimeError("Element \"%s\" too big (maximum 2 characters" % x)
                # makes sure elements symbols are unique in the table
                for ii in range(n):
                    if ii != i:
                        if x == str(t.item(ii, 0).text()):
                            raise RuntimeError("Element \"%s\" already exists" % x)
                ele.append(adjust_atomic_symbol(x))
                # replaces text with its clean version (uppercase, no spaces)
                item.setText(x)

                # # Abundance
                item = t.item(i, 1)
                x = float((item.text()))
                abol.append(x)

                # # Notes
                item = t.item(i, 2)
                x = str((item.text())).strip()
                notes.append(x)
            o.ele = ele
            o.abol = abol
            o.notes = notes
        except Exception as E:
            flag_error = True
            emsg = str(E)
            emsg = "<b>Invalid</b>: "+emsg
            # print "Debug: ERROR: "+emsg+E.__class__.__name__
        self.flag_valid = not flag_error
        self._set_error_text(emsg)

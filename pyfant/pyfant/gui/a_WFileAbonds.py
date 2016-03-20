"""Widget to edit a FileAbonds object."""

__all__ = ["WFileAbonds"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from .guiaux import *
from pyfant import FileAbonds, adjust_atomic_symbol, FileDissoc
import copy

ABONDS_HEADERS = ["Element", "Abundance", "Notes"]
NOTES_COLUMN_WIDTH = 200

def _format_error(row_index, message):
    return "<b>Row %d</b>: %s" % (row_index+1, message)


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

        # Internals
        d = self.__default_dissoc = FileDissoc()
        d.init_default()

        la = self.formLayout = QVBoxLayout()
        la.setMargin(0)
        la.setSpacing(4)

        self.setLayout(la)


        # # Sort/Insert/append/delete toolbar

        l = self.c29378 = QHBoxLayout()
        la.addLayout(l)
        l.setMargin(0)
        l.setSpacing(4)
        b = self.button_sort_a = QPushButton("Sort &alphabetically")
        l.addWidget(b)
        b.clicked.connect(self.on_sort_a)
        b = self.button_sort_z = QPushButton("Sort by atomic &number")
        l.addWidget(b)
        b.clicked.connect(self.on_sort_z)
        l.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))


        l = self.c34985 = QHBoxLayout()
        la.addLayout(l)
        l.setMargin(0)
        l.setSpacing(4)
        b = self.button_insert = QPushButton("&Insert")
        l.addWidget(b)
        b.clicked.connect(self.on_insert)
        b = self.button_append = QPushButton("A&ppend")
        l.addWidget(b)
        b.clicked.connect(self.on_append)
        b = self.button_delete = QPushButton("&Delete")
        l.addWidget(b)
        b.clicked.connect(self.on_delete)
        l.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))


        # # Splitter containing the table and the errors area

        sp = self.splitter = QSplitter(Qt.Vertical)
        la.addWidget(sp)

        # ## The table widget

        a = self.tableWidget = QTableWidget()
        sp.addWidget(a)
        a.setSelectionMode(QAbstractItemView.SingleSelection)
        #a.currentCellChanged.connect(self.on_tableWidget_currentCellChanged)
        a.cellChanged.connect(self.on_tableWidget_cellChanged)
        a.setEditTriggers(QAbstractItemView.DoubleClicked | QAbstractItemView.EditKeyPressed)
        a.setFont(MONO_FONT)
        a.installEventFilter(self)

        # ## The errors area

        w = self.csslff = QWidget()
        sp.addWidget(w)
        l = self.c49378 = QHBoxLayout(w)
        l.setMargin(0)

        x = self.c88888 = QLabel("<b>Errors</b>")
        l.addWidget(x)

        x = self.textEditError = QTextEdit(self)
        l.addWidget(x)
        x.setReadOnly(True)
        x.setStyleSheet("QTextEdit {color: %s}" % COLOR_ERROR)
        x.setFont(MONO_FONT)

        # ## Splitter stretch factors
        # These need to be set a posteriori otherwise they do
        # not work properly.
        sp.setStretchFactor(0, 1)
        sp.setStretchFactor(1, 0)


        # finally...
        self.setEnabled(False)  # Disabled until load() is called
        self.flag_process_changes = True


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def load(self, x):
        assert isinstance(x, FileAbonds)
        self.f = x
        self._update_from_file_abonds()
        # this is called to perform file validation upon loading
        self._update_file_abonds()
        self.setEnabled(True)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def showEvent(self, event):
        self.setFocus()

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
            self.flag_process_changes = False
            try:
                t = self.tableWidget
                item = t.item(row, column)
                flag_done = False
                if column == 0:  # Element
                    try:
                        item.setText(self._validate_element(row, item.text()))
                        flag_done = True
                    except Exception as E:
                        ShowError(str(E))

                if flag_done:
                    self._update_file_abonds()


            finally:
                self.flag_process_changes = True




            self._update_file_abonds()
            self._update_file_abonds()
            self.edited.emit()

    def on_sort_a(self):
        self.f.sort_a()
        self._update_from_file_abonds()
        self._update_file_abonds()
        self.edited.emit()

    def on_sort_z(self):
        not_found = self.f.sort_z()
        self._update_from_file_abonds()
        self._update_file_abonds()
        self.edited.emit()
        if len(not_found) > 0:
            ShowMessage("Symbols not found in the periodic table:\n\n"+
                        str([x.strip() for x in not_found])+"\n\n"+
                        "These symbols will appear first and will be ordered alphabetically.")

    def on_insert(self):
        self._insertRow(self.tableWidget.currentRow())

    def on_append(self):
        self._insertRow(self.tableWidget.rowCount())

    def on_delete(self):
        r = QMessageBox.question(self, "Delete row", "Are you sure?",
         QMessageBox.Yes|QMessageBox.No, QMessageBox.Yes)
        if r == QMessageBox.Yes:
            self.flag_process_changes = False
            try:
                t = self.tableWidget
                t.removeRow(t.currentRow())
                self._update_file_abonds()
            finally:
                self.flag_process_changes = True
            self.edited.emit()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear

    def _insertRow(self, pos):
        self.flag_process_changes = False
        try:
            t = self.tableWidget
            t.insertRow(pos)
            item = QTableWidgetItem("??")
            t.setItem(pos, 0, item)
            item = QTableWidgetItem("0.0")
            t.setItem(pos, 1, item)
            item = QTableWidgetItem("")
            t.setItem(pos, 2, item)
            t.setCurrentCell(pos, 0)
            self._update_file_abonds()
        finally:
            self.flag_process_changes = True
        self.edited.emit()

    def _set_error_text(self, x):
        """Sets text of textEditError."""
        self.textEditError.setText(x)

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

    def _validate_element(self, row_index, x):
        """Validates element symbol and returns it stripped and uppercase.
        Raises in cases of validation error."""
        x = str(x).upper().strip()
        if len(x) > 2:
            raise RuntimeError(
                "Element \"%s\" too big (maximum 2 characters" % x)

        # hydrogen not allowed
        if x == "H":
            raise RuntimeError("Hydrogen not allowed (abundance of H = 12 is used as a reference throughout)")

        # makes sure elements symbols are unique in the table
        t = self.tableWidget
        n = t.rowCount()
        for ii in range(n):
            if ii != row_index:
                if x == str(t.item(ii, 0).text()):
                    raise RuntimeError("Element \"%s\" already exists" % x)
        return x


    def _update_file_abonds(self):
        errors, warnings = [], []
        o, t = self.f, self.tableWidget
        assert isinstance(t, QTableWidget)
        n = t.rowCount()
        ele, abol, notes = [], [], []

        for i in range(n):
            # # Element
            item = t.item(i, 0)
            x = str(item.text()).upper().strip()
            try:
                x = self._validate_element(i, x)
            except Exception as E:
                errors.append(_format_error(i, str(E)))
            ele.append(adjust_atomic_symbol(x))
            item.setText(x)

            # # Abundance
            item = t.item(i, 1)
            x = 0
            try:
                x = float((item.text()))
            except Exception as E:
                errors.append(_format_error(i, str(E)))
            abol.append(x)
            item.setText(str(x))

            # # Notes (no validation required)
            item = t.item(i, 2)
            x = str((item.text())).strip()
            notes.append(x)

        d = self.__default_dissoc
        print "#################################"
        print "#################################"
        print "#################################"
        print "#################################"
        print d.elems
        for elem, cclog in zip(d.elems, d.cclog):
            if elem not in ele and elem != " H":
                warnings.append("<b>Warning</b>: element \"%s\", required for "
                 "dissociative equilibrium calculation, is missing. "
                "Abundance adopted for \"%s\" will be %g" %
                 (elem.strip(), elem.strip(), cclog+12))

        o.ele = ele
        o.abol = abol
        o.notes = notes
        self.flag_valid = len(errors) == 0
        emsg = ""
        if len(errors) > 0 or len(warnings) > 0:
            emsg = '<ul><li>'+("<li>".join(warnings+errors))

        self._set_error_text(emsg)

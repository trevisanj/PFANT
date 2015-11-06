__all__ = ["ShowError", "ShowMessage", "ResetTableWidget"]

from PyQt4.QtGui import *
from PyQt4.QtCore import *


def ShowError(s):
  QMessageBox.critical(None, "Error", s)


def ShowMessage(s):
  QMessageBox.information(None, "Information", s)


def ResetTableWidget(t, rowCount, colCount):
    """Clears and resizes a table widget."""
    t.clear()
    t.sortItems(-1)
    t.setRowCount(rowCount)
    t.setColumnCount(colCount)






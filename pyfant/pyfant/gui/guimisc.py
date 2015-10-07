__all__ = ["ShowError", "ShowMessage"]

from PyQt4.QtGui import *
from PyQt4.QtCore import *


def ShowError(s):
  QMessageBox.critical(None, "Error", s)

def ShowMessage(s):
  QMessageBox.information(None, "Information", s)

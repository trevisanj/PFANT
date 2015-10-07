__all__ = ["ShowError"]

from PyQt4.QtGui import *
from PyQt4.QtCore import *


def ShowError(s):
  QMessageBox.critical(None, "Error", s)

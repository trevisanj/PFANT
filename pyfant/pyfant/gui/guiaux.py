__all__ = ["MONO_FONT", "SOL_HEADERS", "SOL_ATTR_NAMES", "ATOM_ATTR_NAMES",
           "ATOM_HEADERS", "index_nearest", "remove_line", "show_edit_form",
           "PlotInfo", "place_left_top", "place_center", "PARAMS_INVALID",
           "ShowError", "ShowMessage", "ResetTableWidget",
           "COLOR_ERROR", "INITIALIZES_SUN", "check_return_space"]

from PyQt4.QtGui import *
from PyQt4.QtCore import *
from .a_XParametersEditor import *
import numpy as np


# Messages shared in two or more different situations
INITIALIZES_SUN = "Initializes fields with default parameters (Sun)"
PARAMS_INVALID = "Can't save, invalid parameter values(s)!"

# Colors used in two or more different situations
COLOR_ERROR = "#AA0000" # sortta wine

# Standard font to be used in all GUIs
MONO_FONT = QFont("not_a_font_name")
MONO_FONT.setStyleHint(QFont.TypeWriter)

# Relating tablewidget column headers with set-of-lines attributes
# This is shared between XFileMolecules and XMolLinesEditor
SOL_HEADERS = ["lambda", "sj", "jj"]
SOL_ATTR_NAMES = ["lmbdam", "sj", "jj"]

# Relating tablewidget column headers with Atom atributes
# This is shared between XFileAtoms and XAtomLinesEditor
ATOM_HEADERS = ["lambda", "kiex", "algf", "ch", "gr", "ge", "zinf"]
ATOM_ATTR_NAMES = ["lambda_", "kiex", "algf", "ch", "gr", "ge", "zinf"]


def check_return_space(event, callable_):
    """Checks if event corresponds to Return/Space being pressed and calls callable_ if so."""
    if event.type() == QEvent.KeyPress:
        if event.key() in [Qt.Key_Return, Qt.Key_Space]:
            callable_()
            return True
    return False


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


def index_nearest(array, value):
    """
    Finds index of nearest value in array.

    http://stackoverflow.com/questions/2566412/find-nearest-value-in-numpy-array
    """
    idx = (np.abs(array-value)).argmin()
    return idx

def remove_line(line2D):
    """
    Removes line from matplotlib plot.
    # http://stackoverflow.com/questions/4981815/how-to-remove-lines-in-a-matplotlib-plot
    """
    l = line2D.pop(0)
    l.remove()
    del l


def show_edit_form(obj, attrs, title):
    """Shows parameters editor modal form.

    Arguments:
      obj -- object to extract attribute values
      attrs -- list of attribute names
    """
    specs = []
    for name in attrs:
        specs.append((name, {"value": obj.__getattribute__(name)}))
    form = XParametersEditor(specs=specs, title=title)
    r = form.exec_()
    return r, form


def place_left_top(window):
    """Places window in top left corner of screen.

    Qt, on doing this, does not count the window frame. This is being coarsely
    accounted for by setting the position coordinates to values slightly greater
    than 0.
    """
    window.setGeometry(2, 15, window.width(), window.height())

def place_center(window):
    """Places window in the center of the screen."""
    screenGeometry = QApplication.desktop().screenGeometry()
    x = (screenGeometry.width() - window.width()) / 2
    y = (screenGeometry.height() - window.height()) / 2
    window.move(x, y)


class PlotInfo(object):
    def __init__(self):
        self.flag = True  # Whether the plot is supposed to be shown or not
        self.mpl_obj = None  # matplotlib Lines2D object
        self.axis = None  # matplotlib axis
        self.y_vector = None  # reference to sol.sj or jj

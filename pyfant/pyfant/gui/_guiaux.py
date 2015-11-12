__all__ = ["MONO_FONT", "SOL_HEADERS", "SOL_ATTR_NAMES", "ATOM_ATTR_NAMES",
           "ATOM_HEADERS", "index_nearest", "remove_line", "show_edit_form",
           "PlotInfo"]

from PyQt4.QtGui import QFont
from .a_XParametersEditor import *
import numpy as np

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


class PlotInfo(object):
    def __init__(self):
        self.flag = True  # Whether the plot is supposed to be shown or not
        self.mpl_obj = None  # matplotlib Lines2D object
        self.axis = None  # matplotlib axis
        self.y_vector = None  # reference to sol.sj or jj

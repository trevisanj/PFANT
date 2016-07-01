__all__ = ["MONO_FONT", "SOL_HEADERS", "SOL_ATTR_NAMES", "ATOM_ATTR_NAMES",
           "ATOM_HEADERS", "index_nearest", "remove_line", "show_edit_form",
           "PlotInfo", "place_left_top", "place_center", "snap_left", "snap_right",
           "PARAMS_INVALID",
           "show_error", "show_message", "show_warning", "ResetTableWidget",
           "COLOR_ERROR", "COLOR_CONFIG", "COLOR_STAR", "COLOR_DESCR", "COLOR_WARNING",
           "INITIALIZES_SUN", "check_return_space",
           "enc_name", "enc_name_descr", "LLZERO_LLFIN", "DESCR_PTDISK",
           "style_checkboxes", "DESCR_MULTI", "Occurrence", "ErrorCollector",
           "VerticalLabel", "are_you_sure"]

from PyQt4.QtGui import *
from PyQt4.QtCore import *
from .a_XParametersEditor import *
import numpy as np
import os


# # Colors used in two or more different situations
# Error color
COLOR_ERROR = "#AA0000" # sortta wine
# Warning color
COLOR_WARNING = "#C98A00" # sortta yellow
# Color for labels indicating a star parameter
COLOR_STAR = "#2A8000"
# Color for labels indicating a software configuration parameter
COLOR_CONFIG = "#BD6909"
# Default color for label text
COLOR_DESCR = "#222222"

def enc_name_descr(name, descr, color=COLOR_DESCR):
    """Encodes html given name and description."""
    return enc_name(name, color)+"<br>"+descr


def enc_name(name, color=COLOR_DESCR):
    """Encodes html given name."""
    return "<span style=\"color: %s; font-weight: bold\">%s</span>" % \
           (color, name)


# Messages shared in two or more different situations
INITIALIZES_SUN = "Initializes fields with default parameters (Sun)"
PARAMS_INVALID = "Can't save, invalid parameter values(s)!"
LLZERO_LLFIN = "The calculation interval for the synthetic spectrum is given by "\
  "["+enc_name("llzero", COLOR_CONFIG)+", "+enc_name("llfin", COLOR_CONFIG)+"]."\
"""
<pre>
          aint
          &lt;--&gt;

----[----|----|----|----|-]-------&gt;
    |                     |       wavelength (angstrom)
    llzero                llfin
</pre>
"""

DESCR_PTDISK = """
This option is used to simulate a spectrum acquired
out of the center of the star disk.<br><br>
This is useful if the synthetic spectrum will be compared with an
observed spectrum acquired out of the center of the star disk.
<ul>
    <li>True: 7-point integration
    <li>False: 6- or 26-point integration, depending on option --kik
</ul>"""


DESCR_MULTI = """
Runs pfant for different abundances for each element, then run nulbad for each
pfant result for different FWHMs.

The configuration is read from a .py file.

The user must specify a list of FWHM values for nulbad convolutions, and
a dictionary containing element symbols and respective list containing n_abdif
differential abundances to be used for each element.

pfant will be run n_abdif times, each time adding to each element in ab the i-th
value in the vector for the corresponding element.

nulbad will run n_abdif*n_fwhms times, where n_fwhms is the number of different
FWHMs specified.

The result will be
- several spectra saved as  "<star name><pfant name or counter>.sp"
- several "spectra list" files saved as "cv_<FWHM>.spl". As the file indicates,
  each ".spl" file will have the names of the spectrum files for a specific FWHM.
  .spl files are subject to input for lineplot.py by E.Cantelli
"""


# Standard font to be use   d in all GUIs
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


def style_checkboxes(widget):
    """
    Iterates over widget children to change checkboxes stylesheet.

    The default rendering of checkboxes does not allow to tell a focused one
    from an unfocused one.
    """

    ww = widget.findChildren(QCheckBox)
    for w in ww:
        w.setStyleSheet("QCheckBox:focus {border: 1px solid #000000;}")


def check_return_space(event, callable_):
    """Checks if event corresponds to Return/Space being pressed and calls callable_ if so."""
    if event.type() == QEvent.KeyPress:
        if event.key() in [Qt.Key_Return, Qt.Key_Space]:
            callable_()
            return True
    return False


def show_error(s):
  QMessageBox.critical(None, "Error", s)


def show_message(s):
  QMessageBox.information(None, "Information", s)


def show_warning(s):
  QMessageBox.warning(None, "Warning", s)


def are_you_sure(flag_changed, evt, parent=None, title="File has been changed",
                 msg="Are you sure you want to exit?"):
    """
    "Are you sure you want to exit" question dialog.

    If flag_changed, shows question dialog. If answer is not yes, calls evt.ignore()

    Arguments:
      flag_changed
      evt -- QCloseEvent instance
      parent=None -- parent form, used to centralize the question dialog at
      title -- title for question dialog
      msg -- text of question dialog

    Returns True or False. True means: "yes, I want to exit"
    """
    if flag_changed:
        r = QMessageBox.question(parent, title, msg,
             QMessageBox.Yes|QMessageBox.No, QMessageBox.Yes)
        if r != QMessageBox.Yes:
            evt.ignore()


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


# # Coarse solution for hidden window title
# Qt does not account for the window frame.This is being coarsely
# accounted for by setting the position coordinates to values slightly greater
# than 0.
_DESKTOP_OFFSET_LEFT = 2
_DESKTOP_OFFSET_TOP = 15

def place_left_top(window, width=None, height=None):
    """Places window in top left corner of screen.

    Arguments:
      window -- a QWidget
      width=None -- window width, in case you want to change it (if not passed, not changed)
      height=None -- window height, in case you want to change it (if not passed, not changed)
    """

    if width is None:
        width = window.width()
    if height is None:
        height = window.height()

    window.setGeometry(_DESKTOP_OFFSET_LEFT, _DESKTOP_OFFSET_TOP, width, height)

def place_center(window):
    """Places window in the center of the screen."""
    screenGeometry = QApplication.desktop().screenGeometry()
    x = (screenGeometry.width() - window.width()) / 2
    y = (screenGeometry.height() - window.height()) / 2
    window.move(x, y)

def snap_left(window, width=None):
    """Snaps window to left of desktop.
    Arguments:
      window -- a QWidget
      width=None -- window width, in case you want to change it (if not passed, not changed)
    """
    if not width:
        width = window.width()
    rect = QApplication.desktop().screenGeometry()
    window.setGeometry(_DESKTOP_OFFSET_LEFT, _DESKTOP_OFFSET_TOP, width, rect.height())

def snap_right(window, width=None):
    """Snaps window to right of desktop.
    Arguments:
      window -- a QWidget
      width=None -- window width, in case you want to change it (if not passed, not changed)
    """
    if not width:
        width = window.width()
    rect = QApplication.desktop().screenGeometry()
    window.setGeometry(rect.width()-width, _DESKTOP_OFFSET_TOP, width, rect.height())



class PlotInfo(object):
    def __init__(self):
        self.flag = True  # Whether the plot is supposed to be shown or not
        self.mpl_obj = None  # matplotlib Lines2D object
        self.axis = None  # matplotlib axis
        self.y_vector = None  # reference to sol.sj or jj


class Occurrence(object):
    colors = {"undefined": "#000000",
              "warning": COLOR_WARNING,
              "error": COLOR_ERROR,
              "cannot open": COLOR_ERROR}

    def __init__(self, filename, type_, line, message):
        # "undefined"/"warning"/"error"/"cannot open"
        self.type = type_
        self.message = message
        self.filename = filename
        self.line = line

    def get_html(self):
        message = self.message if isinstance(self.message, str) \
         else "\n".join(self.message)
        return ("<h3>File '%s' (<span style=\"color: %s\">%s</span>)</h3>\n" %
         (self.filename, self.colors[self.type], self.type))+ \
         ("" if not self.line else "<b>line %d</b>\n" % self.line)+ \
         ("<pre>%s</pre>" % message if message else "")


class ErrorCollector(object):
    def __init__(self):
        # List of Occurrence
        self.occurrences = []

    def collect_errors(self, path_):
        self.occurrences = []
        for p, dirs, files in os.walk(path_):
            for file in files:
                if file == "python.log":
                    pass
                    # parses python log
                elif file == "fortran.log":
                    path__ = os.path.relpath(os.path.join(p, file), ".") # os.path.relpath(os.path.join(path_, file), path_)
                    try:
                        linetext_last = ""
                        line, flag_halting = 1, False
                        with open(path__, "r") as h:
                            for t in h:
                                linetext = t.strip()
                                if "HALTING" in linetext:
                                    if not flag_halting:
                                        occ = Occurrence(path__, "error", line, [])
                                        self.occurrences.append(occ)
                                        flag_halting = True
                                    occ.message.append(linetext)
                                else:
                                    flag_halting = False

                                if "Fortran runtime error" in linetext:
                                    occ = Occurrence(path__, "error", line,
                                     [linetext_last, linetext])
                                    self.occurrences.append(occ)

                                if "WARNING" in linetext:
                                    occ = Occurrence(path__, "warning", line, linetext)
                                    self.occurrences.append(occ)

                                line += 1
                                linetext_last = linetext
                    except IOError, E:
                        occ = Occurrence(path__, "cannot open", 0, str(E))
                        self.occurrences.append(occ)

    def get_html(self):
        oo = [occ.get_html() for occ in self.occurrences]
        if len(oo) == 0:
            return "No errors were found."
        return "\n".join(oo)



class VerticalLabel(QLabel):
    """Label that draws itself vertically.

    This was created to be used at lateral title:
      - It paints in bold
      - No HTML support
    """
    def paintEvent(self, evt):
        painter = QPainter(self)
        painter.setPen(Qt.black)
        painter.setBrush(Qt.Dense1Pattern)
        painter.rotate(90)
        painter.font().setWeight(QFont.Bold)

        # td = QTextDocument()
        # td.setHtml(self.text())
        # td.drawContents(painter)

        painter.drawText(0,0, self.text())

    def minimumSizeHint(self):
        s = QLabel.minimumSizeHint(self)
        return QSize(s.height(), s.width())

    def sizeHint(self):
        s = QLabel.sizeHint()
        return QSize(s.height(), s.width())

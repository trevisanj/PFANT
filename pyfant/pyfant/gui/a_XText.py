"""Window to show text."""
__all__ = ["XText"]

from PyQt4.QtGui import *
from .a_WFileMain import *
from .a_WFileAbonds import *
from pyfant import FileMain
from .guiaux import *
from . import XRunnableManager
from pyfant import *

################################################################################
class XText(QMainWindow):
    """
    Arguments:
      parent=None -- nevermind
      text -- string
    """

    def __init__(self, parent=None, text="", title=""):
        QMainWindow.__init__(self, parent)

        cw = self.centralWidget = QPlainTextEdit()
        self.setCentralWidget(cw)
        cw.setReadOnly(True)  # allows copy but not editing
        cw.setFont(MONO_FONT)
        cw.setPlainText(text)

        self.setWindowTitle(title)
        self.setGeometry(0, 0, 800, 600)
        place_center(self)


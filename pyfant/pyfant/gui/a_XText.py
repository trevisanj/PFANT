"""XText: Window to show text; XHTML: window to show HTML."""
__all__ = ["XText", "XHTML"]

from PyQt4.QtGui import *
from .guiaux import *

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

################################################################################
class XHTML(QMainWindow):
    """
    Arguments:
      parent=None -- nevermind
      html -- string
    """

    def __init__(self, parent=None, html="", title=""):
        QMainWindow.__init__(self, parent)

        cw = self.centralWidget = QTextEdit()
        self.setCentralWidget(cw)
        cw.setReadOnly(True)  # allows copy but not editing
        cw.setText(html)

        self.setWindowTitle(title)
        self.setGeometry(0, 0, 800, 600)
        place_center(self)


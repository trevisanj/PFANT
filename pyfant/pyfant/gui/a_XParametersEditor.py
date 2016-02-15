"""Parameters Editor dialog."""

__all__ = ["XParametersEditor"]

from PyQt4.QtGui import *
import ui_XParametersEditor
from . import a_WParametersEditor


################################################################################
class XParametersEditor(QDialog, ui_XParametersEditor.Ui_XParametersEditor):
  """
  Arguments:
    parent=None -- nevermind
    specs=None -- *Required*! list as [(name, {...}), ...], which will be passed
     to Parameters constructor. Parameter.FromSpec()
     for full documentation.
  """

  def __init__(self, parent=None, specs=None, title=None):
    ## State variables
    QDialog.__init__(self, parent)
    self._ReadConfig()
    self.setupUi(self)

    pe = self.pe = a_WParametersEditor.WParametersEditor(specs=specs)
    self.wsbLayout = QVBoxLayout(self.frame)
    self.wsbLayout.setContentsMargins(0, 0, 0, 0)
    self.frame.layout().addWidget(pe)
    pe.setFocus()

    if title is not None:
      self.setWindowTitle(title)


  def GetKwargs(self):
    return self.pe.get_kwargs()

  def GetSpecs(self):
    return self.pe.get_specs()

  def _ReadConfig(self):
    pass

  def accept(self):
    try:
      self.pe.validate()
    except Exception as E:
      QMessageBox.critical(None, "Error", E.message)

    QDialog.accept(self)

  def show(self):
    super(XParametersEditor, self).show()



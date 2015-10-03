# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '.\XParametersEditor.ui'
#
# Created: Fri Jul 18 20:44:20 2014
#    by: PyQt4 UI code generator 4.10.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
  _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
  def _fromUtf8(s):
    return s

try:
  _encoding = QtGui.QApplication.UnicodeUTF8
  def _translate(context, text, disambig):
    return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
  def _translate(context, text, disambig):
    return QtGui.QApplication.translate(context, text, disambig)

class Ui_XParametersEditor(object):
  def setupUi(self, XParametersEditor):
    XParametersEditor.setObjectName(_fromUtf8("XParametersEditor"))
    XParametersEditor.setWindowModality(QtCore.Qt.WindowModal)
    XParametersEditor.resize(446, 62)
    self.horizontalLayout_2 = QtGui.QHBoxLayout(XParametersEditor)
    self.horizontalLayout_2.setSpacing(2)
    self.horizontalLayout_2.setMargin(2)
    self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
    self.horizontalLayout = QtGui.QHBoxLayout()
    self.horizontalLayout.setSpacing(2)
    self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
    self.frame = QtGui.QFrame(XParametersEditor)
    self.frame.setFrameShape(QtGui.QFrame.StyledPanel)
    self.frame.setFrameShadow(QtGui.QFrame.Raised)
    self.frame.setObjectName(_fromUtf8("frame"))
    self.horizontalLayout.addWidget(self.frame)
    self.buttonBox = QtGui.QDialogButtonBox(XParametersEditor)
    self.buttonBox.setOrientation(QtCore.Qt.Vertical)
    self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
    self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
    self.horizontalLayout.addWidget(self.buttonBox)
    self.horizontalLayout_2.addLayout(self.horizontalLayout)

    self.retranslateUi(XParametersEditor)
    QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), XParametersEditor.reject)
    QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), XParametersEditor.accept)
    QtCore.QMetaObject.connectSlotsByName(XParametersEditor)

  def retranslateUi(self, XParametersEditor):
    XParametersEditor.setWindowTitle(_translate("XParametersEditor", "Parameters Editor", None))


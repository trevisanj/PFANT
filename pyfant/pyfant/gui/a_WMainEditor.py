"""Widget to edit a FileMain object."""

__all__ = ["WMainEditor"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from ._guiaux import *
from .guimisc import *
from .parameter import Parameters
from pyfant import FileMain


# Color for labels indicating a star parameter
COLOR_STAR = "#2A8000"
# Color for labels indicating a software configuration parameter
COLOR_CONFIG = "#BD6909"

def _enc_html(name, descr, color="#FFFFFF"):
    # Encodes html given name and description
    return "<span style=\"color: %s; font-weight: bold\">%s</span><br>%s" % \
           (color, name, descr)

class WMainEditor(QWidget):
    """
    FileMain editor widget.

    Arguments:
      parent=None
    """

    # Emitted whenever any value changes
    edited = pyqtSignal()

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.flag_valid = True
        self.f = None # FileMain object

        la = self.formLayout = QGridLayout()
        la.setVerticalSpacing(4)
        la.setHorizontalSpacing(5)
        self.setLayout(la)
        # field map: [(label, edit, description), ...]
        pp = self._map = []

        x = self.label_titrav = QLabel(_enc_html("t&itrav", "star name", COLOR_STAR))
        y = self.lineEdit_titrav = QLineEdit()
        # y.editingFinished.connect(self._on_editing_finished)
        y.textEdited.connect(self._on_edited)
        y.installEventFilter(self)
        x.setBuddy(y)
        pp.append((x, y, "Name of the star."))

        x = self.label_teff = QLabel(_enc_html("&teff", "effective temperature", COLOR_STAR))
        y = self.lineEdit_teff = QLineEdit()
        y.textEdited.connect(self._on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(0, 1e10, 0))
        x.setBuddy(y)
        pp.append((x, y, "Sun: 5777."))

        x = self.label_glog = QLabel(_enc_html("&glog", "gravity", COLOR_STAR))
        y = self.lineEdit_glog = QLineEdit()
        y.textEdited.connect(self._on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(0, 1e10, 5))
        x.setBuddy(y)
        pp.append((x, y, "Sun: 4.44."))

        x = self.label_asalog = QLabel(_enc_html("&asalog", "metallicity", COLOR_STAR))
        y = self.lineEdit_asalog = QLineEdit()
        y.textEdited.connect(self._on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(-10, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "Sun: 0"))

        x = self.label_nhe = QLabel(_enc_html("&nhe", "abundance of Helium", COLOR_STAR))
        y = self.lineEdit_nhe = QLineEdit()
        y.textEdited.connect(self._on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "Sun: 0.1"))

        x = self.label_ptdisk = QLabel(
         _enc_html("pt&disk", "point of disk?", COLOR_CONFIG))
        y = self.checkBox_ptdisk = QCheckBox()
        y.setTristate(False)
        y.installEventFilter(self)
        y.stateChanged.connect(self._on_edited)
        x.setBuddy(y)
        pp.append((x, y, "This option is used to simulate a spectrum acquired "
         "out of the center of the star disk."))

        x = self.label_mu = QLabel(_enc_html("&mu", "cosine of angle", COLOR_CONFIG))
        y = self.lineEdit_mu = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self._on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "This is the cosine of the angle formed by center of "
         "the star disk, the point of observation, and the Earth as vertex. "
         "This value will be used only if ptdisk is True"))


        x = self.label_flprefix = QLabel(_enc_html("flprefi&x", "prefix of filename", COLOR_CONFIG))
        y = self.lineEdit_flprefix = QLineEdit()
        # y.editingFinished.connect(self._on_editing_finished)
        y.textEdited.connect(self._on_edited)
        y.installEventFilter(self)
        x.setBuddy(y)
        pp.append((x, y, "pfant will create three output files: "
         "<flprefix>.cont (continuum), <flprefix>.norm (normalized spectrum), "
         "and <flprefix>.spec (continuum*normalized)"))



        x = self.label_pas = QLabel(_enc_html("&pas", "calculation step (angstrom)", COLOR_CONFIG))
        y = self.lineEdit_pas = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self._on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y,
         "The synthetic spectrum will have points <pas> angstrom distant from "
         "each other. Use this to specify the resolution of the synthetic spectrum."))

        # KKK is a repeated description
        KKK = "The calculation interval for the synthetic spectrum is given by "\
              "[<llzero>, <llfin>]"
        x = self.label_llzero = QLabel(_enc_html("ll&zero",
         "calculation beginning (angstrom)", COLOR_CONFIG))
        y = self.lineEdit_llzero = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self._on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, KKK))

        x = self.label_llfin = QLabel(_enc_html("ll&fin",
         "calculation calculation end (angstrom)", COLOR_CONFIG))
        y = self.lineEdit_llfin = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self._on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, KKK))

        x = self.label_aint = QLabel(_enc_html("&aint", "length of sub-interval (angstrom)", COLOR_CONFIG))
        y = self.lineEdit_aint = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self._on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "This is length of each calculation sub-interval."
         "(<llfin>-<llzero>) is divided into intervals of roughly <aint> angstrom."
         "Note: <aint> must be a multiple of <pas>."))


        x = self.label_fwhm = QLabel(_enc_html("&fwhm", "convolution full-width-half-maximum", COLOR_CONFIG))
        y = self.lineEdit_fwhm = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self._on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "This parameter specifies the full-width-half-maximum"
         "of a Gaussian curve to convolve the synthetic spectrum with. It is "
         "used by nulbad."))



        for i, (label, edit, descr) in enumerate(pp):
            # label.setStyleSheet("QLabel {text-align: right}")
            label.setAlignment(Qt.AlignRight)
            la.addWidget(label, i, 0)
            la.addWidget(edit, i, 1)
            edit.setToolTip(descr)

        x = self.labelDescr = QLabel(self)
        x.setGeometry(0, 0, 100, 0)
        x.setWordWrap(True)
        x.setStyleSheet("QLabel {color: #333333; font-style: italic}")
        la.addWidget(self.labelDescr, la.rowCount(), 0, 1, 2)

        self.labelError = QLabel(self)
        self.labelError.setStyleSheet("color: red")
        la.addWidget(self.labelError, la.rowCount(), 0, 1, 2)


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def load(self, x):
        assert isinstance(x, FileMain)
        self.f = x
        self._update_from_file_main()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def setFocus(self, reason=None):
        """Sets focus to first field. Note: reason is ignored."""
        self.lineEdit_titrav.setFocus()

    def eventFilter(self, obj_focused, event):
        if event.type() == QEvent.FocusIn:
            descr = ""
            for _, obj, descr_ in self._map:
                if obj_focused == obj:
                    print "achei o objectiop"
                    descr = descr_
                    break
            self._set_descr_text(descr)
        return False


    # def Validate(self):
    #     # Currently calling Parameters.GetKwargs() to do the <quote>validation<quote>
    #     self._parameters.UpdateFromWidgets()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    # def _on_editing_finished(self):
    #     print "Editasse eh"

    def _on_edited(self):
        print "text_edited inicio"
        self._update_file_main()
        self.edited.emit()
        print "text_edited fim"

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear

    def _update_from_file_main(self):
        o = self.f
        self.lineEdit_titrav.setText(o.titrav)
        self.lineEdit_teff.setText(str(o.teff))
        self.lineEdit_glog.setText(str(o.glog))
        self.lineEdit_asalog.setText(str(o.asalog))
        self.lineEdit_nhe.setText(str(o.nhe))
        self.checkBox_ptdisk.setChecked(o.ptdisk)
        self.lineEdit_mu.setText(str(o.mu))
        self.lineEdit_flprefix.setText(o.flprefix)
        self.lineEdit_pas.setText(str(o.pas))
        self.lineEdit_llzero.setText(str(o.llzero))
        self.lineEdit_llfin.setText(str(o.llfin))
        self.lineEdit_aint.setText(str(o.aint))
        self.lineEdit_fwhm.setText(str(o.fwhm))

    def _set_error_text(self, x):
        """Sets text of labelError."""
        self.labelError.setText(x)

    def _set_descr_text(self, x):
        """Sets text of labelDescr."""
        self.labelDescr.setText(x)

    def _update_file_main(self):
        o = self.f
        emsg, flag_error = "", False
        ss = ""
        try:
            ss = "titrav"
            o.titrav = self.lineEdit_titrav.text()
            ss = "teff"
            o.teff = float(self.lineEdit_teff.text())
            ss = "glog"
            o.glog = float(self.lineEdit_glog.text())
            ss = "asalog"
            o.asalog = float(self.lineEdit_asalog.text())
            ss = "nhe"
            o.nhe = float(self.lineEdit_nhe.text())
            ss = "ptdisk"
            o.ptdisk = self.checkBox_ptdisk.isChecked()
            ss = "mu"
            o.mu = float(self.lineEdit_mu.text())
            ss = "flprefix"
            o.flprefix = self.lineEdit_flprefix.text()
            ss = "pas"
            o.pas = float(self.lineEdit_pas.text())
            ss = "llzero"
            o.llzero = float(self.lineEdit_llzero.text())
            ss = "llfin"
            o.llfin = float(self.lineEdit_llfin.text())
            ss = "aint"
            o.aint = float(self.lineEdit_aint.text())
            ss = "fwhm"
            o.fwhm = float(self.lineEdit_fwhm.text())
        except Exception as E:
            flag_error = True
            print "Debug", str(E)
            emsg = "Field \"%s\": %s" % (ss, str(E))
            # ShowError(str(E))
        self.flag_valid = not flag_error
        self._set_error_text(emsg)

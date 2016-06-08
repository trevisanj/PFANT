"""Widget to edit a FileMain object."""

__all__ = ["WFileMain"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from .guiaux import *
from pyfant import FileMain


class WFileMain(QWidget):
    """
    FileMain editor widget.

    Arguments:
      parent=None
    """

    # Emitted whenever any value changes
    edited = pyqtSignal()

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        # Whether all the values in the fields are valid or not
        self.flag_valid = False
        # Internal flag to prevent taking action when some field is updated programatically
        self.flag_process_changes = False
        self.f = None # FileMain object


        # # Central layout
        la = self.centralLayout = QVBoxLayout()
        la.setMargin(0)
        self.setLayout(la)

        # ## Splitter with scroll area and descripton+error area
        sp = self.splitter = QSplitter(Qt.Vertical)
        la.addWidget(sp)


        # ### Widget + grid layout to be first in splitter
        sa = self.c33441 = QScrollArea()
        sp.addWidget(sa)
        sa.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        sa.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Widget that will be handled by the scrollable area
        w = self.scrollWidget = QWidget()
        sa.setWidget(self.scrollWidget)
        sa.setWidgetResizable(True)
#        la.addWidget(w)


        lw = QVBoxLayout()
        w.setLayout(lw)
        # Form layout
        lg = self.formLayout = QGridLayout()
        lw.addLayout(lg)
        lg.setMargin(0)
        lg.setVerticalSpacing(4)
        lg.setHorizontalSpacing(5)
        lw.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))



        # field map: [(label widget, edit widget, field name, short description,
        #              field name color, long description), ...]
        pp = self._map = []

        x = self.label_titrav = QLabel()
        y = self.lineEdit_titrav = QLineEdit()
        # y.editingFinished.connect(self._on_editing_finished)
        y.textEdited.connect(self.on_edited)
        y.installEventFilter(self)
        x.setBuddy(y)
        pp.append((x, y, "t&itrav", "star name", COLOR_STAR,
         "Name of the star."))

        x = self.label_teff = QLabel()
        y = self.lineEdit_teff = QLineEdit()
        y.textEdited.connect(self.on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(0, 1e10, 0))
        x.setBuddy(y)
        pp.append((x, y, "&teff", "effective temperature", COLOR_STAR,
         "Sun: 5777"))

        x = self.label_glog = QLabel()
        y = self.lineEdit_glog = QLineEdit()
        y.textEdited.connect(self.on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(0, 1e10, 5))
        x.setBuddy(y)
        pp.append((x, y, "&glog", "gravity", COLOR_STAR,
         "Sun: 4.44"))

        x = self.label_asalog = QLabel()
        y = self.lineEdit_asalog = QLineEdit()
        y.textEdited.connect(self.on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(-10, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "&asalog", "metallicity", COLOR_STAR,
         "Sun: 0"))

        x = self.label_vvt = QLabel()
        y = self.lineEdit_vvt = QLineEdit()
        y.textEdited.connect(self.on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(0, 900, 2))
        x.setBuddy(y)
        pp.append((x, y, "&vvt", "velocity of microturbulence", COLOR_STAR,
         "Sun: 0.9"))

        x = self.label_nhe = QLabel()
        y = self.lineEdit_nhe = QLineEdit()
        y.textEdited.connect(self.on_edited)
        y.installEventFilter(self)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "&nhe", "abundance of Helium", COLOR_STAR,
         "Sun: 0.1"))

        x = self.label_ptdisk = QLabel()
        y = self.checkBox_ptdisk = QCheckBox()
        y.setTristate(False)
        y.installEventFilter(self)
        y.stateChanged.connect(self.on_edited)
        x.setBuddy(y)
        pp.append((x, y, "pt&disk", "point of disk?", COLOR_CONFIG,
         DESCR_PTDISK))

        x = self.label_mu = QLabel()
        y = self.lineEdit_mu = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self.on_edited)
        y.setValidator(QDoubleValidator(-1, 1, 5))
        x.setBuddy(y)
        pp.append((x, y, "&mu", "cosine of angle", COLOR_CONFIG,
         "This is the cosine of the angle formed by center of "
         "the star disk, the point of observation, and the Earth as vertex. "
         "<br><br>This value will be used only if "+enc_name("ptdisk", COLOR_CONFIG)+" is True.<br>\n<pre>\n"
         "             point of observation   \n"
         "            /                       \n"
         "           /                        \n"
         "          /                         \n"
         "         /                          \n"
         "        /                           \n"
         "       / ) acos(mu)                 \n"
         "Earth /------------------- center of star disk\n</pre>"))

        x = self.label_flprefix = QLabel()
        y = self.lineEdit_flprefix = QLineEdit()
        # y.editingFinished.connect(self._on_editing_finished)
        y.textEdited.connect(self.on_edited)
        y.installEventFilter(self)
        x.setBuddy(y)
        pp.append((x, y, "flprefi&x", "prefix of filename", COLOR_CONFIG,
         "pfant will create three output files:<ul>"
         "<li>"+enc_name("flprefix", COLOR_CONFIG)+".cont (continuum),"
         "<li>"+enc_name("flprefix", COLOR_CONFIG)+".norm (normalized spectrum), and"
         "<li>"+enc_name("flprefix", COLOR_CONFIG)+".spec (continuum*normalized)</ul>"))

        x = self.label_pas = QLabel()
        y = self.lineEdit_pas = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self.on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "&pas", "calculation step (&Aring;)", COLOR_CONFIG,
         "The synthetic spectrum will have points "+enc_name("pas", COLOR_CONFIG)+" &Aring; distant from "
         "each other.<br><br>Use this to specify the resolution of the synthetic spectrum."))

        x = self.label_llzero = QLabel()
        y = self.lineEdit_llzero = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self.on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "ll&zero", "lower boundary of synthesis interval (&Aring;)", COLOR_CONFIG, LLZERO_LLFIN))

        x = self.label_llfin = QLabel()
        y = self.lineEdit_llfin = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self.on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "&llfin", "upper boundary of synthesis interval (&Aring;)", COLOR_CONFIG, LLZERO_LLFIN))

        x = self.label_aint = QLabel()
        y = self.lineEdit_aint = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self.on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "&aint", "length of sub-interval (&Aring;)", COLOR_CONFIG,
        "This is length of each calculation sub-interval "
        "(the calculation interval ["+enc_name("llzero", COLOR_CONFIG)+", "+enc_name("llfin", COLOR_CONFIG)+"] is split in sub-intervals of roughly "+enc_name("aint", COLOR_CONFIG)+" &Aring;)."
        "<br><br>Note: "+enc_name("aint", COLOR_CONFIG)+" must be a multiple of "+enc_name("pas", COLOR_CONFIG)+"."))

        x = self.label_fwhm = QLabel()
        y = self.lineEdit_fwhm = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self.on_edited)
        y.setValidator(QDoubleValidator(0, 10, 5))
        x.setBuddy(y)
        pp.append((x, y, "f&whm", "convolution full-width-half-maximum", COLOR_CONFIG,
         "This parameter specifies the full-width-half-maximum "
         "of a Gaussian curve to convolve the synthetic spectrum with. <br><br>It is "
         "used by <em>nulbad</em> (Fortran code that calculates such convolution)."))


        for i, (label, edit, name, short_descr, color, long_descr) in enumerate(pp):
            # label.setStyleSheet("QLabel {text-align: right}")
            assert isinstance(label, QLabel)
            label.setText(enc_name_descr(name, short_descr, color))
            label.setAlignment(Qt.AlignRight)
            lg.addWidget(label, i, 0)
            lg.addWidget(edit, i, 1)
            edit.setToolTip(long_descr)



        # ### Second widget of splitter
        # layout containing description area and a error label
        wlu = QWidget()
        lu = QVBoxLayout(wlu)
        lu.setMargin(0)
        lu.setSpacing(4)
        x = self.textEditDescr = QTextEdit(self)
        x.setReadOnly(True)
        # x.setGeometry(0, 0, 100, 0)
        # x.setWordWrap(True)
        x.setStyleSheet("QTextEdit {color: %s}" % COLOR_DESCR)
        lu.addWidget(x)
        x = self.labelError = QLabel(self)
        x.setStyleSheet("QLabel {color: %s}" % COLOR_ERROR)
        lu.addWidget(self.labelError)
        sp.addWidget(wlu)


        self.setEnabled(False)  # disabled until load() is called
        style_checkboxes(self)
        self.flag_process_changes = True


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def load(self, x):
        assert isinstance(x, FileMain)
        self.f = x
        self.__update_from_file_main()
        # this is called to perform file validation upon loading
        self.__update_file_main()
        self.setEnabled(True)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def setFocus(self, reason=None):
        """Sets focus to first field. Note: reason is ignored."""
        self.lineEdit_titrav.setFocus()

    def eventFilter(self, obj_focused, event):
        if event.type() == QEvent.FocusIn:
            text = ""
            for label, obj, name, short_descr, color, long_descr in self._map:
                if obj_focused == obj:
                    text = "%s<br><br>%s" % \
                           (enc_name(name.replace("&", ""), color), long_descr)
                    break
            self.__set_descr_text(text)
        return False

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    def on_edited(self):
        # print "THE SENDER IS ", self.sender()
        if self.flag_process_changes:
            self.__update_file_main()
            self.edited.emit()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear

    def __update_from_file_main(self):
        self.flag_process_changes = False
        try:
            o = self.f
            self.lineEdit_titrav.setText(o.titrav)
            self.lineEdit_teff.setText(str(o.teff))
            self.lineEdit_glog.setText(str(o.glog))
            self.lineEdit_asalog.setText(str(o.asalog))
            self.lineEdit_vvt.setText(str(o.vvt[0]))
            self.lineEdit_nhe.setText(str(o.nhe))
            self.checkBox_ptdisk.setChecked(o.ptdisk)
            self.lineEdit_mu.setText(str(o.mu))
            self.lineEdit_flprefix.setText(o.flprefix)
            self.lineEdit_pas.setText(str(o.pas))
            self.lineEdit_llzero.setText(str(o.llzero))
            self.lineEdit_llfin.setText(str(o.llfin))
            self.lineEdit_aint.setText(str(o.aint))
            self.lineEdit_fwhm.setText(str(o.fwhm))
        finally:
            self.flag_process_changes = True

    def __set_error_text(self, x):
        """Sets text of labelError."""
        self.labelError.setText(x)

    def __set_descr_text(self, x):
        """Sets text of labelDescr."""
        self.textEditDescr.setText(x)

    def __update_file_main(self):
        o = self.f
        emsg, flag_error = "", False
        ss = ""
        try:
            ss = "titrav"
            o.titrav = str(self.lineEdit_titrav.text())
            ss = "teff"
            o.teff = float(self.lineEdit_teff.text())
            ss = "glog"
            o.glog = float(self.lineEdit_glog.text())
            ss = "asalog"
            o.asalog = float(self.lineEdit_asalog.text())
            o.afstar = o.asalog  # makes afstar match asalog
            ss = "vvt"
            temp = float(self.lineEdit_vvt.text())
            if temp > 900:
                raise RuntimeError("Not prepared for depth-variable velocity "
                 "of microturbulence (maximum allowed: 900)")
            o.vvt = [temp]
            ss = "nhe"
            o.nhe = float(self.lineEdit_nhe.text())
            ss = "ptdisk"
            o.ptdisk = self.checkBox_ptdisk.isChecked()
            ss = "mu"
            o.mu = float(self.lineEdit_mu.text())
            ss = "flprefix"
            o.flprefix = str(self.lineEdit_flprefix.text())
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

            # Some error checks
            ss = ""
            if o.llzero >= o.llfin:
                raise RuntimeError("llzero must be lower than llfin!")
            if not (-1 <= o.mu <= 1):
                raise RuntimeError("mu must be between -1 and 1!")
        except Exception as E:
            flag_error = True
            if ss:
                emsg = "Field \"%s\": %s" % (ss, str(E))
            else:
                emsg = str(E)
            emsg = "<b>Invalid</b>: "+emsg
            # ShowError(str(E))
        self.flag_valid = not flag_error
        self.__set_error_text(emsg)

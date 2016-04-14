"""Widget to edit a Conf object."""

__all__ = ["WOptionsEditor"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from .guiaux import *
from pyfant import *
from .a_XText import *

_FROM_MAIN = ' (read from main configuration file)'
_NUM_MOL = 21
_EXE_NAMES = {"i": "innewmarcs", "h": "hydro2", "p": "pfant", "n": "nulbad"}


@froze_it
class _Option(AttrsPart):
    attrs = ["name", "argname", "descr"]

    def __init__(self):
        AttrsPart.__init__(self)
        self.checkbox = None
        self.label = None
        self.edit = None
        self.name = None
        self.ihpn = None
        self.argname =  None
        self.default =  None
        self.short_descr = None
        self.long_descr = None
        # Whether or not the option overrides a value in main.dat
        self.flag_main = False
        # Whether or not the option is considered to be an option for developers (to aid debuggin etc)
        self.flag_devel = False
        self.color = COLOR_DESCR
        # other widgets to be shown/hidden (besides .checkbox, .label, .edit)
        self.other_widgets = []
        # Type (e.g. float, str). Only required for options whose default is None
        self.type = None
        self.flag_never_used = True
        self.possible_values = None
        self.min = None
        self.max = None

    def __clear_edit(self):
        if isinstance(self.edit, QCheckBox):
            self.edit.setChecked(False)
        else:
            self.edit.setText("")

    def __update_edit(self, value):
        if isinstance(self.edit, QCheckBox):
            self.edit.setChecked(value == True)
        else:
            self.edit.setText(str(value))

    def update_edit_with_default(self):
        if self.default is not None:
            self.__update_edit(self.default)

    def update_gui(self, options):
        assert isinstance(options, Options)
        attr = options.__getattribute__(self.name)
        flag_check = attr is not None
        self.checkbox.setChecked(flag_check)
        if self.flag_never_used and flag_check:
            self.flag_never_used = False
        if attr is None:
            pass
            # todo not sure if clearing edit of not
            # self.__clear_edit()
        else:
            self.__update_edit(attr)

    def set_gui_default_if_never_used(self):
        """Sets edit/checkbox default if first time the option is set to "in use"."""
        if self.default is not None and self.flag_never_used:
            if isinstance(self.edit, QCheckBox):
                self.edit.setChecked(self.default)
            else:
                self.edit.setText(str(self.default))
        self.flag_never_used = False

    def get_label_text(self):
        return enc_name_descr("--%s" % self.name, self.short_descr, self.color)


    def get_value(self):
        type_ = type(self.default) if self.default is not None else self.type
        w = self.edit
        if type_ == bool:
            value = w.isChecked()
        else:
            s = str(w.text())

            # validation
            if len(s) == 0:
                raise RuntimeError("empty")

            value = type_(s)

            # validation
            if self.possible_values is not None:
                if not value in self.possible_values:
                    raise RuntimeError("invalid value: \"%s\" -- possible values are: %s" %
                                       (str(value), '"'+('"/"'.join([str(x) for x in self.possible_values]))+'"'))
            if self.min is not None:
                if value < self.min:
                    raise RuntimeError("value too small (minimum allowed: %g)" % self.min)
            if self.max is not None:
                if value > self.max:
                    raise RuntimeError("value too big (maximum allowed: %g)" % self.max)

        return value

class WOptionsEditor(QWidget):
    """
    Options editor widget.

    Arguments:
      parent=None
    """

    # Emitted whenever any value changes
    edited = pyqtSignal()

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)


        # # Setup & accessible attributes

        # Whether all the values in the fields are valid or not
        self.flag_valid = False
        self.f = None # Options object
        self.logger = get_python_logger()

        # # Internal stuff that must not be accessed from outside

        # options map: list of _Option
        self.omap = []
        # Internal flag to prevent taking action when some field is updated programatically
        self.flag_process_changes = False

        # # Central layout

        la = self.centralLayout = QVBoxLayout()
        la.setMargin(0)
        self.setLayout(la)

        # ## Toolbar: checkboxes with executables
        l1 = self.layout_exes = QHBoxLayout()
        la.addLayout(l1)

        w = self.label_filter = QLabel("<b>Fil&ter:</b>")
        l1.addWidget(w)
        w = self.lineEdit_filter = QLineEdit()
        self.label_filter.setBuddy(w)
        w.textEdited.connect(self.on_filter)
        l1.addWidget(w)
        w.setFixedWidth(100)

        w = self.line12312 = QFrame() # vertical line
        l1.addWidget(w)
        # w.setGeometry(QRect(240, 240, 3, 61))
        w.setFrameShape(QFrame.VLine)
        w.setFrameShadow(QFrame.Sunken)
        w.setFixedWidth(3)

        w0 = self.checkbox_i = QCheckBox("innewmarcs")
        w1 = self.checkbox_h = QCheckBox("hydro2")
        w2 = self.checkbox_p = QCheckBox("pfant")
        w3 = self.checkbox_n = QCheckBox("nulbad")
        ww = self.checkboxes_exes = [w0, w1, w2, w3]
        l1.addWidget(QLabel("<b>Show options for Fortran binaries:</b>"))
        for w in ww:
            l1.addWidget(w)
            w.setTristate(False)
            w.setChecked(True)
            w.stateChanged.connect(self.on_checkbox_exe_clicked)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # ## Toolbar: checkbox for main configuration file override
        l1 = self.layout23432 = QHBoxLayout()
        la.addLayout(l1)
        w = self.checkbox_main = QCheckBox("Show options that override main configuration file")
        w.setStyleSheet("QCheckBox {color: %s}" % COLOR_CONFIG)
        w.setChecked(True)
        w.stateChanged.connect(self.on_checkbox_main_clicked)
        w.setToolTip("Show options that, if set, will override values that appear in the main configuration file.")
        l1.addWidget(w)
        w = self.checkbox_devel = QCheckBox("Show developer options")
        w.setChecked(False)
        w.stateChanged.connect(self.on_checkbox_devel_clicked)
        w.setToolTip("Show options that were created to aid software development and debugging.")
        l1.addWidget(w)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))
        b = QPushButton("Preview command line")
        b.clicked.connect(self.on_preview)
        l1.addWidget(b)


        # ## Splitter with scroll area and descripton+error area
        sp = self.splitter = QSplitter(Qt.Vertical)
        la.addWidget(sp)

        # ### Scroll area containing the form

        sa = self.scrollArea = QScrollArea()
        sp.addWidget(sa)
        sa.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        sa.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Widget that will be handled by the scrollable area
        w = self.scrollWidget = QWidget()
        sa.setWidget(self.scrollWidget)
        sa.setWidgetResizable(True)
#        la.addWidget(w)

        # #### The form layout
        # This layout contains the form layout and a spacer item
        lw = QVBoxLayout()
        w.setLayout(lw)
        # Form layout
        lo = self.formLayout = QGridLayout()
        lw.addLayout(lo)
        lo.setVerticalSpacing(4)
        lo.setHorizontalSpacing(5)
        lw.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))

        # ##### The editing controls

        self.w_logging_level = QLineEdit()
        self.w_logging_console = QCheckBox()
        self.w_allow = QCheckBox()
        self.w_logging_file = QCheckBox()
        self.w_fn_logging = QLineEdit()
        self.w_fn_main = QLineEdit()
        self.w_fn_progress = QLineEdit()
        self.w_explain = QCheckBox()
        self.w_fn_modeles = QLineEdit()
        self.w_teff = QLineEdit()
        self.w_glog = QLineEdit()
        self.w_asalog = QLineEdit()
        self.w_fn_absoru2 = QLineEdit()
        self.w_fn_hmap = QLineEdit()
        self.w_llzero = QLineEdit()
        self.w_llfin = QLineEdit()
        self.w_fn_moddat = QLineEdit()
        self.w_modcode = QLineEdit()
        self.w_tirb = QLineEdit()
        self.w_ptdisk = QLineEdit()
        self.w_kik = QLineEdit()
        self.w_amores = QCheckBox()
        self.w_kq = QLineEdit()
        self.w_vvt = QLineEdit()
        self.w_zph = QLineEdit()
        self.w_interp = QLineEdit()
        self.w_fn_dissoc = QLineEdit()
        self.w_fn_partit = QLineEdit()
        self.w_fn_abonds = QLineEdit()
        self.w_fn_atoms = QLineEdit()
        self.w_fn_moo = QLineEdit()
        self.w_fn_modgrid = QLineEdit()
        self.w_fn_molecules = QLineEdit()
        self.w_molidxs_off = QLineEdit()
        self.w_no_molecules = QCheckBox()
        self.w_no_atoms = QCheckBox()
        self.w_no_h = QCheckBox()
        self.w_no_opa = QCheckBox()
        self.w_zinf = QLineEdit()
        self.w_pas = QLineEdit()
        self.w_aint = QLineEdit()
        self.w_flprefix = QLineEdit()
        self.w_fn_flux = QLineEdit()
        self.w_flam = QCheckBox()
        self.w_fn_cv = QCheckBox()
        self.w_pat = QLineEdit()
        self.w_convol = QCheckBox()
        self.w_fwhm = QLineEdit()

        # ##### The options map
        # (*) A few options have been commented out because they are probably
        # irrelevant nowadays, but may be shown again some time.

        o = self.__add_option(self.w_logging_level, 'ihpn', 'logging_level', 'debug',
         'logging level',
         'These are the available options:<ul>'+
         '<li>debug'+
         '<li>info'+
         '<li>warning'+
         '<li>error'+
         '<li>critical'+
         '<li>halt</ul>')
        o.flag_devel = True
        o.possible_values = ["debug", "info", "warning", "error", "critical", "halt"]
        o = self.__add_option(self.w_logging_console, 'ihpn', 'logging_console', True,
         'Log to console?',
         'Whether or not to log messages to standard output (usually the command console)')
        o.flag_devel = True
        o = self.__add_option(self.w_logging_file, 'ihpn', 'logging_file', False,
          'Log to file?',
          'Whether or not to log messages to log file '+
          '(specified by option --fn_logging)')
        o.flag_devel = True
        o = self.__add_option(self.w_explain, 'ihpn', 'explain', False,
          'Save additional debugging information?',
          'This flag informs the Fortran code to save additional information in file explain.txt '+
          '(debugging purposes; output varies, or flag may be ignored)')
        o.flag_devel = True
        o = self.__add_option(self.w_fn_logging, 'ihpn', 'fn_logging', None,
         'log filename',
         'default: <executable name>_dump.log, <i>e.g.</i>, <b>pfant_dump.log</b>')
        o.type = str
        o.flag_devel = True
        self.__add_option(self.w_fn_main, 'ihpn', 'fn_main', FileMain.default_filename,
         'input file name: main configuration',
         'Contains star parameters and additional software configuration.<br><br>'
         '<b>Attention</b>: the following command-line options, if used, will '
            'override values in the main configuration file: '
         '--llzero, --llfin, --pas, --flprefix, --fwhm.')
        # (*) self.__add_option(self.w_fn_progress, 'ihpn', 'fn_progress', 'file name', 'progress.txt',
        # (*)  'output file name - progress indicator')

        #
        # innewmarcs, hydro2, pfant
        #
        self.__add_option(self.w_fn_modeles, 'ihp', 'fn_modeles', FileModBin.default_filename,
         'atmospheric model file name',
         'This is a binary file containing information about atmospheric model. '
         'This file is created by innewmarcs.')

        #
        # innewmarcs, pfant
        #
        self.__add_option(self.w_no_opa, 'ip', 'no_opa', True,
         'Skip opacities?',
         'If set, skips the calculation of opacities based on MARCS ".opa" opacities file.')

        #
        # innewmarcs-only
        #
        # (*) self.__add_option(self.w_modcode, 'i', 'modcode', 'string up to 25 characters', 'NoName',
        # (*)  '"Model name"')
        self.__add_option(self.w_fn_modgrid, 'i', 'fn_modgrid', "grid.mod",
         'atmospheric model grid (<b>without opacities</b>)',
         'This is a binary file containing a grid of atmospheric models for interpolation.'
         '<p>Whether this file or the one specified by <em>--fn_moo</em> will be used '
         'will depend on the <em>--opa</em>option.')
        self.__add_option(self.w_fn_moo, 'i', 'fn_moo', FileMoo.default_filename,
         'atmospheric model grid (<b>with opacities</b>)',
         'This is a binary file containing a grid of atmospheric models for interpolation, '
         'opacities included.'
         '<p>Whether this file or the one specified by <em>--fn_modgrid</em> will be used '
         'will depend on the <em>--opa</em>option.')
        self.__add_option(self.w_allow, 'i', 'allow', False,
         'Allow point out of model grid?', 'If this option is disabled, target '
         ' (glog, teff, asalog) point for interpolation must lie inside the 3D '
         'model grid.')

        #
        # hydro2-only
        #
        self.__add_option(self.w_amores, 'h', 'amores', True,
         'AMOrtissement de RESonnance?', '')
        o = self.__add_option(self.w_kq, 'h', 'kq', 1,
         'theorie',
         '<ul>'
         '<li>0: THEORIE DE GRIEM'+
         '<li>1: THEORIE QUASISTATIQUE</ul>')
        o.possible_values = [0, 1]

        o = self.__add_option(self.w_zph, 'h', 'zph', 12.,
         'hydrogen-reference-abundance',
         'abundance d\'H pour laquelle sont donn&eacute;es les abondances metalliques')
        o.min = 0.
        o.max = 24.  # actually I would like to remove this option

        #
        # hydro2, pfant
        #
        o = self.__add_option(self.w_llzero, 'hp', 'llzero', 6000.,
         "lower boundary of calculation interval (&Aring;)",
         'default: &lt;main_llzero&gt; '+_FROM_MAIN)
        o.flag_main = True
        o.color = COLOR_CONFIG
        o.type = float
        o = self.__add_option(self.w_llfin, 'hp', 'llfin', 6200.,
         'upper boundary of calculation interval (&Aring;)',
         'default: &lt;main_llfin&gt; '+_FROM_MAIN)
        o.flag_main = True
        o.color = COLOR_CONFIG
        o.type = float
        self.__add_option(self.w_fn_absoru2, 'hp', 'fn_absoru2', FileAbsoru2.default_filename,
         'input file name - absoru2',
         'This file contains physical data for pfant and hydro2 "absoru" module')
        self.__add_option(self.w_fn_hmap, 'hp', 'fn_hmap', FileHmap.default_filename,
         'input file name - hydrogen lines data',
         'Contains a table with<pre>'+
         'filename, niv inf, niv sup, central lambda, kiex, c1</pre>'
         'for each hydrogen line.')
        o = self.__add_option(self.w_kik, 'hp', 'kik', 0,
         'option affecting the flux integration',
         '<ul>'+
         '<li>0: integration using 6/7 points depending on <em>ptdisk</em> parameter in main configuration file;'+
         '<li>1: 26-point integration</ul>')
        o.possible_values = [0, 1]

        #
        # pfant-only
        #
        o = self.__add_option(self.w_aint, 'p', 'aint', 10,
         'interval length per iteration (&Aring;)',
         'default: &lt;main_aint&gt; '+_FROM_MAIN)
        o.flag_devel = True
        o = self.__add_option(self.w_interp, 'p', 'interp', 1,
         'interpolation for subroutine turbul()',
         'interpolation type for subroutine turbul()<ul>'+
         '<li>1: linear;'+
         '<li>2: parabolic</ul>')
        o.possible_values = [1, 2]
        # > @todo Find names for each file and update options help
        self.__add_option(self.w_fn_dissoc, 'p', 'fn_dissoc', FileDissoc.default_filename,
         'input file name - dissociative equilibrium', '')
        self.__add_option(self.w_fn_partit, 'p', 'fn_partit', FilePartit.default_filename,
         'input file name - partition functions', '')
        self.__add_option(self.w_fn_abonds, 'p', 'fn_abonds', FileAbonds.default_filename,
         'input file name - atomic abundances', '')
        self.__add_option(self.w_fn_atoms, 'p', 'fn_atoms', FileAtoms.default_filename,
         'input file name - atomic lines', '')
        self.__add_option(self.w_fn_molecules, 'p', 'fn_molecules', FileMolecules.default_filename,
         'input file name - molecular lines', '')
        self.__add_option(self.w_molidxs_off, 'p', 'molidxs_off', '',
         'moleculed to be "turned off"',
         'These are comma-separated ids of molecules to be "turned off" (1 to '+str(_NUM_MOL)+').')
        self.__add_option(self.w_no_molecules, 'p', 'no_molecules', False,
         'Skip molecules?',
         'If set, skips the calculation of molecular lines.')
        self.__add_option(self.w_no_atoms, 'p', 'no_atoms', False,
         'Skip atomic lines?',
         'If set, skips the calculation of atomic lines (except hydrogen).')
        self.__add_option(self.w_no_h, 'p', 'no_h', False,
         'Skip hydrogen lines?',
         'If set, skips the calculation of hydrogen lines.')
        o = self.__add_option(self.w_zinf, 'p', 'zinf', 0.5,
         '(zinf per-line in dfile:atoms)',
         'This is the distance from center of line to consider in atomic line calculation.<br><br>'+
         'If this option is used, it will override all the zinf defined '+
         'in the atomic lines file with a single value.')
        o.flag_devel = True
        o = self.__add_option(self.w_pas, 'p', 'pas', 0.02,
         'calculation delta-lambda (&Aring;)',
         'default: &lt;main_pas&gt; '+_FROM_MAIN)
        o.flag_main = True
        o.color = COLOR_CONFIG

        #
        # pfant, nulbad
        #
        o = self.__add_option(self.w_flprefix, 'pn', 'flprefix', 'flux',
         'pfant output - prefix for flux output files',
         'Three files will be created based on this prefix:<ul>'+
         '<li><flprefix>.spec: un-normalized spectrum'+
         '<li><flprefix>.cont: continuum'+
         '<li><flprefix>.norm: normalized spectrum</ul><br>'
         'default: &lt;main_flprefix&gt; '+_FROM_MAIN)
        o.flag_main = True
        o.color = COLOR_CONFIG

        #
        # nulbad-only
        #
        self.__add_option(self.w_fn_flux, 'n', 'fn_flux', 'flux.norm',
         'nulbad input - flux file name',
         'default: &lt;main_flprefix>.norm '+_FROM_MAIN)
        o.flag_main = True
        o.color = COLOR_CONFIG
        self.__add_option(self.w_flam, 'n', 'flam', False,
         'Perform Fnu-to-FLambda transformation?',
         'If True, Fnu-to-FLambda transformation is done before the convolution')
        self.__add_option(self.w_fn_cv, 'n', 'fn_cv', True,
          'nulbad output -- convolved spectrum file name',
          'default: &lt;flux file name&gt;.nulbad.&lt;fwhm&gt;')
        self.__add_option(self.w_pat, 'n', 'pat', 0.02,
          'wavelength delta-lambda of nulbad output spectrum (&Aring;)',
          'default: same as delta-lambda of input spectrum')
        self.__add_option(self.w_convol, 'n', 'convol', True,
          'Apply convolution?', '')
        o = self.__add_option(self.w_fwhm, 'n', 'fwhm', 0.12,
          'full-width-half-maximum of Gaussian function',
          'default: &lt;main_fwhm&gt; '+_FROM_MAIN+')')
        o.flag_main = True
        o.color = COLOR_CONFIG

        IHPN = "ihpn"
        NIHPN = len(IHPN)
        for j, letter in enumerate(IHPN):
            label = QLabel("<b>%s</b>" % letter)
            label.setToolTip("Option is used by %s?" % _EXE_NAMES[letter])
            lo.addWidget(label, 0, 3+j, Qt.AlignCenter)
        label = QLabel("<b>in use?</b>")
        label.setToolTip("Option will appear in command line?")
        lo.addWidget(label, 0, 2)

        i = 1  # index of next row in layout
        for option in self.omap:
            try:
                for j, letter in enumerate(IHPN):
                    # unicode is for a "v"-like check mark
                    flag_checked = letter in option.ihpn
                    label = QLabel(QString(unichr(10003)) if flag_checked else "")
                    if flag_checked:
                        label.setToolTip("This option is used by %s." % _EXE_NAMES[letter   ])

                    option.other_widgets.append(label)
                    lo.addWidget(label, i, 3+j, Qt.AlignCenter)
                label = option.label = \
                    QLabel(option.get_label_text())
                label.setAlignment(Qt.AlignRight)
                checkbox = option.checkbox = QCheckBox()
                checkbox.installEventFilter(self)
                checkbox.stateChanged.connect(self.on_in_use_checkbox_clicked)
                edit = option.edit
                edit.installEventFilter(self)

                lo.addWidget(label, i, 0)
                lo.addWidget(edit, i, 1)
                lo.addWidget(checkbox, i, 2, Qt.AlignCenter)
                i += 1
                option.edit.setToolTip(option.long_descr)
                if isinstance(edit, QCheckBox):
                    edit.stateChanged.connect(self.on_edited)
                else:
                    edit.textEdited.connect(self.on_edited)

                # todo No consensus yet if it is better to show the default values
                option.update_edit_with_default()
            except:
                self.logger.exception("Processing option '%s'" % option.name)
                raise


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

        # ### Adjust splitter proportion
        sp.setStretchFactor(0, 8)
        sp.setStretchFactor(1, 2)

        self.setEnabled(False)  # disabled until load() is called
        self.__update_visible_options()
        style_checkboxes(self)
        self.flag_process_changes = True


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def load(self, x):
        assert isinstance(x, Options)
        self.f = x
        self.__update_from_data()
        # this is called to perform file validation upon loading
        self.__update_data()
        self.setEnabled(True)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def setFocus(self, reason=None):
        """Sets focus to first field. Note: reason is ignored."""

        # todo not working, dunno why but it is called but the first field does not acquire focus.

        # print "SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS"
        # print self.omap[0].edit
        self.omap[0].edit.setFocus()

    def eventFilter(self, obj_focused, event):
        if event.type() == QEvent.FocusIn:
            option = self.__find_option_by_widget(obj_focused)
            if option:
                text = "%s<br><br>%s" % \
                       (enc_name(option.name.replace("&", ""), option.color),
                        option.long_descr)
                self.__set_descr_text(text)

        return False

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    def on_edited(self):
        if not self.flag_process_changes:
            return
        #self._update_file_main()
        option = self.__find_option_by_edit(self.sender())
        self.flag_process_changes = False
        option.checkbox.setChecked(True)
        self.flag_process_changes = True
        option.flag_never_used = False
        self.__update_data()
        self.edited.emit()

    def on_in_use_checkbox_clicked(self):
        if not self.flag_process_changes:
            return
        checkbox = self.sender()
        option = self.__find_option_by_in_use_checkbox(checkbox)
        if checkbox.isChecked():
            option.set_gui_default_if_never_used()
        self.__update_data()

    def on_checkbox_exe_clicked(self):
        self.__update_visible_options()

    def on_checkbox_main_clicked(self):
        self.__update_visible_options()

    def on_checkbox_devel_clicked(self):
        self.__update_visible_options()

    def on_preview(self):
        args = self.f.get_args()
        print args
        line = "fortran-binary-xxxx "+(" ".join(args))
        w = XText(self, line, "Command line")
        w.show()

    def on_filter(self):
        if not self.flag_process_changes:
            return
        self.__update_visible_options()



    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear
    
    def __add_option(self, widget, ihpn, name, default, short_descr, long_descr):
        o = _Option()
        w = o.checkbox = QCheckBox()
        w.setTristate(False)
        w.stateChanged.connect(self.on_in_use_checkbox_clicked)


        o.edit = widget
        o.ihpn = ihpn
        o.name = name
        o.default = default
        o.short_descr = short_descr
        o.long_descr = long_descr
        self.omap.append(o)
        return o

    def __find_option_by_widget(self, widget):
        if isinstance(widget, QCheckBox):
            ret = self.__find_option_by_in_use_checkbox(widget)
            if not ret:
                ret = self.__find_option_by_edit(widget)
        else:
            ret = self.__find_option_by_edit(widget)
        return ret

    def __find_option_by_in_use_checkbox(self, checkbox):
        ret = None
        for option in self.omap:
            if option.checkbox == checkbox:
                ret = option
                break
        return ret

    def __find_option_by_edit(self, widget):
        ret = None
        for option in self.omap:
            if option.edit == widget:
                ret = option
                break
        return ret

    def __update_from_data(self):
        self.flag_process_changes = False
        try:
            for option in self.omap:
                option.update_gui(self.f)
        finally:
            self.flag_process_changes = True

    def __update_data(self):
        emsg, flag_error = "", False
        ss = ""
        try:
            for option in self.omap:
                ss = option.name
                if not option.checkbox.isChecked():
                    value = None
                else:
                    value = option.get_value()
                self.f.__setattr__(option.name, value)

            ss = ""
            if self.f.llzero >= self.f.llfin:
                "llzero must be lower than llfin"

        except Exception as E:
            flag_error = True
            if ss:
                emsg = "Field \"%s\": %s" % (ss, str(E))
            else:
                emsg = str(E)
            emsg = "<b>Invalid</b>: "+emsg
        self.flag_valid = not flag_error
        self.__set_error_text(emsg)


    def __update_visible_options(self):
        LETTERS = "ihpn"
        ll = []
        for i, (checkbox, letter) in enumerate(zip(self.checkboxes_exes, LETTERS)):
            if checkbox.isChecked():
                ll.append(letter)
        flag_main_goes = self.checkbox_main.isChecked()
        flag_devel_goes = self.checkbox_devel.isChecked()
        hidden_set_count = 0 # whether there will be set options hidden
        s_filter = str(self.lineEdit_filter.text()).upper()
        flag_filter = len(s_filter) > 0
        for option in self.omap:
            flag_visible = (flag_main_goes or not option.flag_main) and \
             (flag_devel_goes or not option.flag_devel) and \
             any([letter in option.ihpn for letter in ll])

            if flag_visible and flag_filter:
                flag_visible = s_filter in option.name.upper() or \
                 s_filter in option.short_descr.upper() or \
                 s_filter in option.long_descr.upper()

            option.checkbox.setVisible(flag_visible)
            option.label.setVisible(flag_visible)
            option.edit.setVisible(flag_visible)
            for w in option.other_widgets:
                w.setVisible(flag_visible)
            if not flag_visible and option.checkbox.isChecked():
                hidden_set_count += 1

        if hidden_set_count > 0:
            ShowWarning("Hiding %d option%s in use." %
             (hidden_set_count, "" if hidden_set_count == 1 else "s"))

    def __set_error_text(self, x):
        """Sets text of labelError."""
        self.labelError.setText(x)

    def __set_descr_text(self, x):
        """Sets text of labelDescr."""
        self.textEditDescr.setText(x)





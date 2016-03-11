"""Widget to edit a Conf object."""

__all__ = ["WOptionsEditor"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from .guiaux import *
from pyfant import *

# Color for labels indicating a star parameter
COLOR_STAR = "#2A8000"
# Color for labels indicating a software configuration parameter
COLOR_CONFIG = "#BD6909"

FROM_MAIN = ' (read from main configuration file)'
NUM_MOL = 21

def _enc_name_descr(name, descr, color="#FFFFFF"):
    # Encodes html given name and description
    return _enc_name(name, color)+"<br>"+descr

def _enc_name(name, color="#FFFFFF"):
    # Encodes html given name and description
    return "<span style=\"color: %s; font-weight: bold\">%s</span>" % \
           (color, name)

@froze_it
class _Option(AttrsPart):
    attrs = ["name", "argname", "descr"]

    def __init__(self):
        AttrsPart.__init__(self)
        self.checkbox = None
        self.label = None
        self.widget = None
        self.name = None
        self.ihpn = None
        self.argname =  None
        self.default =  None
        self.descr_short = None
        self.descr_long = None
        # Whether or not the option overrides a value in main.dat
        self.flag_main = False

    def __clear_widget(self):
        if isinstance(self.widget, QCheckBox):
            self.widget.setChecked(False)
        else:
            self.widget.setText("")

    def __update_widget(self, value):
        if isinstance(self.widget, QCheckBox):
            self.widget.setChecked(value == True)
        else:
            self.widget.setText(str(value))

    def update_gui(self, options):
        assert isinstance(options, Options)
        attr = options.__getattribute__(self.name)
        self.checkbox.setChecked(attr is not None)
        if attr is None:
            self.__clear_widget()
        else:
            self.__update_widget(attr)


class WOptionsEditor(QWidget):
    """
    FileMain editor widget.

    Arguments:
      parent=None
    """

    # Emitted whenever any value changes
    edited = pyqtSignal()

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)


        # # Setup & accessible attributes

        # Whether all the values in the fields are valid or not
        self.flag_valid = True
        self.options = None # Options object
        self.logger = get_python_logger()

        # # Internal stuff that must not be accessed from outside

        # options map: list of _Option
        self.omap = []
        # Internal flag to prevent taking action when some field is updated programatically
        self.flag_process_changes = False

        # # Central layout

        la = self.centralLayout = QVBoxLayout()
        self.setLayout(la)

        # ## Toolbar: checkboxes with executables
        l1 = self.c29378 = QHBoxLayout()
        la.addLayout(l1)
        w0 = self.checkbox_i = QCheckBox("innewmarcs")
        w1 = self.checkbox_h = QCheckBox("hydro2")
        w2 = self.checkbox_p = QCheckBox("pfant")
        w3 = self.checkbox_n = QCheckBox("nulbad")
        ww = self.checkboxes_exes = [w0, w1, w2, w3]
        for w in ww:
            l1.addWidget(w)
            w.setTristate(False)
            w.setChecked(True)
            w.stateChanged.connect(self.on_widget_exe_clicked)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))


        # ## Scroll area containing the form

        sa = self.scrollArea = QScrollArea()
        la.addWidget(sa)
        sa.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        sa.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Widget that will be handled by the scrollable area
        w = self.scrollWidget = QWidget()
        sa.setWidget(self.scrollWidget)
        sa.setWidgetResizable(True)
#        la.addWidget(w)

        # ## The form layout
        # This layout contains the form layout and a spacer item
        lw = QVBoxLayout()
        w.setLayout(lw)
        # Form layout
        lo = self.formLayout = QGridLayout()
        lw.addLayout(lo)
        lo.setVerticalSpacing(4)
        lo.setHorizontalSpacing(5)
        lw.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))

        # #### The editing controls

        self.w_logging_level = QLineEdit()
        self.w_logging_console = QCheckBox()
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
        self.w_fn_gridsmap = QLineEdit()
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
        self.w_fn_molecules = QLineEdit()
        self.w_molidxs_off = QLineEdit()
        self.w_no_molecules = QCheckBox()
        self.w_no_atoms = QCheckBox()
        self.w_no_h = QCheckBox()
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

        # #### The options map

        self.__add_option(self.w_logging_level, 'ihpn', 'logging_level', 'level', 'debug',
         'logging level:<ul>'+
         '<li>debug'+
         '<li>info'+
         '<li>warning'+
         '<li>error'+
         '<li>critical'+
         '<li>halt</ul>')
        self.__add_option(self.w_logging_console, 'ihpn', 'logging_console', True,
         'Log to console?',
         'Whether or not to log messages to standard output (usually the command console)')
        self.__add_option(self.w_logging_file, 'ihpn', 'logging_file', False,
          'Log to file?',
          'Whether or not to log messages to log file '+
          '(specified by option --fn_logging)')
        self.__add_option(self.w_fn_logging, 'ihpn', 'fn_logging', None,
         'log filename',
         'default: <executable name>_dump.log, <i>e.g.</i>, <b>pfant_dump.log</b>')
        self.__add_option(self.w_fn_main, 'ihpn', 'fn_main', None,
         'input file name: main configuration',
         'default: '+FileMain.default_filename)
        self.__add_option(self.w_fn_progress, 'ihpn', 'fn_progress', 'file name', 'progress.txt',
         'output file name - progress indicator')
        self.__add_option(self.w_explain, 'ihpn', 'explain', False,
          'Save additional debugging information?',
          'This flag informs the Fortran code to save additional information in file explain.txt '+
          '(debugging purposes; output varies, or flag may be ignored)')

        #
        # innewmarcs, hydro2, pfant
        #
        self.__add_option(self.w_fn_modeles, 'ihp', 'fn_modeles', FileMod.default_filename,
         'atmospheric model file name',
         'This is a binary file containing information about atmospheric model'
         '(created by innewmarcs).')

        #
        # innewmarcs, hydro2
        #
        o = self.__add_option(self.w_teff, 'ih', 'teff', None,
         'effective temperature',
         'Sun: 5777<br><br>default: &lt;main_teff&gt; '+FROM_MAIN)
        o.flag_main = True
        o = self.__add_option(self.w_glog, 'ih', 'glog', None,
         'gravity',
         'Sun: 4.44<br><br>default: &lt;main_glog&gt; '+FROM_MAIN)
        o.flag_main = True
        o = self.__add_option(self.w_asalog, 'ih', 'asalog', None,
         'metallicity ([M/H])',
         'Sun: 0<br><br>'+
         'default: &lt;main_asalog&gt; '+FROM_MAIN)
        o.flag_main = True
        o = self.__add_option(self.w_fn_absoru2, 'hp', 'fn_absoru2', FileAbsoru2.default_filename,
         'input file name - absoru2',
         'This file contains physical data for pfant and hydro2 "absoru" module')
        o.flag_main = True
        self.__add_option(self.w_fn_hmap, 'hp', 'fn_hmap', FileAbsoru2.default_filename,
         'input file name - hydrogen lines data',
         'Contains a table with<pre>'+
         'filename, niv inf, niv sup, central lambda, kiex, c1</pre>')
        o = self.__add_option(self.w_llzero, 'hp', 'llzero', None,
         "lower boundary of calculation interval (&Aring;)",
         'default: &lt;main_llzero&gt; '+FROM_MAIN)
        o.flag_main = True
        o = self.__add_option(self.w_llfin, 'hp', 'llfin', None,
         'upper boundary of calculation interval (&Aring;)',
         'default: &lt;main_llfin&gt; '+FROM_MAIN)
        o.flag_main = True

        #
        # innewmarcs-only
        #
        self.__add_option(self.w_fn_moddat, 'i', 'fn_moddat', 'file name', 'modeles.dat',
         'ASCII file containing information about atmospheric model (created by innewmarcs)')
        self.__add_option(self.w_modcode, 'i', 'modcode', 'string up to 25 characters', 'NoName',
         '"Model name"')
        self.__add_option(self.w_tirb, 'i', 'tirb', 'string up to 15 characters', '?',
         '"Titre"')
        self.__add_option(self.w_fn_gridsmap, 'i', 'fn_gridsmap', 'file name', 'gridsmap.dat',
         'input file name - file containing list of MARCS models file names')

        #
        # hydro2-only
        #
        self.__add_option(self.w_ptdisk, 'h', 'ptdisk', None,
         'point of disk?',
         DESCR_PTDISK+'<br><br>default: &lt;main_ptdisk&gt; '+FROM_MAIN)
        self.__add_option(self.w_kik, 'hp', 'kik', 0,
         'option affecting the flux integration',
         '<ul>'+
         '<li>0: integration using 6/7 points depending on option --ptdisk;<br>'+
         '<li>1: 26-point integration</ul>')
        self.__add_option(self.w_amores, 'h', 'amores', True,
         'AMOrtissement de RESonnance?', '')
        self.__add_option(self.w_kq, 'h', 'kq', 1,
         'theorie',
         '<ul>'
         '<li>0: THEORIE DE GRIEM;<br>'+
         '<li>1: THEORIE QUASISTATIQUE</li>')
        self.__add_option(self.w_vvt, 'h', 'vvt', None,
         'velocity of microturbulence',
         'default: &lt;main_vvt(1)&gt; '+FROM_MAIN)
        self.__add_option(self.w_zph, 'h', 'zph', 12,
         'hydrogen-reference-abundance',
         'abondance d''H pour laquelle sont donnees les abondances metalliques')

        #
        # pfant-only
        #
        self.__add_option(self.w_interp, 'p', 'interp', 1,
         'interpolation for subroutine turbul()',
         'interpolation type for subroutine turbul()<ul>'+
         '<li>1: linear;<br>'+
         '<li>2: parabolic</ul>')
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
         'comma-separated ids of molecules to be "turned off" (1 to '+str(NUM_MOL)+').')
        self.__add_option(self.w_no_molecules, 'p', 'no_molecules', False,
         'Skip molecules?',
         'If set, skips the calculation of molecular lines.')
        self.__add_option(self.w_no_atoms, 'p', 'no_atoms', False,
         'Skip atomic lines?',
         'If set, skips the calculation of atomic lines (except hydrogen).')
        self.__add_option(self.w_no_h, 'p', 'no_h', False,
         'Skip hydrogen lines?',
         'If set, skips the calculation of hydrogen lines.')
        self.__add_option(self.w_zinf, 'p', 'zinf', None,
         '(zinf per-line in dfile:atoms)',
         'This is the distance from center of line to consider in atomic line calculation.<br><br>'+
         'If this option is used, will override the zinf defined for each atomic line<br>'+
         '<li>of dfine:atoms and use the value passed')
        o = self.__add_option(self.w_pas, 'p', 'pas', None,
         'calculation delta-lambda (&Aring;)',
         'default: &lt;main_pas&gt; '+FROM_MAIN)
        o.flag_main = True
        o = self.__add_option(self.w_aint, 'p', 'aint', None,
         'interval length per iteration (&Aring;)',
         'default: &lt;main_aint&gt; '+FROM_MAIN)
        o.flag_main = True

        #
        # pfant, nulbad
        #
        self.__add_option(self.w_flprefix, 'pn', 'flprefix', None,
         'pfant output - prefix for flux output files',
         'Three files will be created based on this prefix:<ul>'+
         '<li><flprefix>.spec: un-normalized spectrum<br>'+
         '<li><flprefix>.cont: continuum<br>'+
         '<li><flprefix>.norm: normalized spectrum</ul><br>'
         'default: &lt;main_flprefix&gt; '+FROM_MAIN)

        #
        # nulbad-only
        #
        self.__add_option(self.w_fn_flux, 'n', 'fn_flux', None,
         'flux file name',
         'default: &lt;main_flprefix>.norm '+FROM_MAIN)
        self.__add_option(self.w_flam, 'n', 'flam', False,
         'Perform Fnu-to-FLambda transformation?',
         'If True, Fnu-to-FLambda transformation is done before the convolution')
        self.__add_option(self.w_fn_cv, 'n', 'fn_cv', True,
          'nulbad output -- convolved spectrum file name',
          'default: &lt;flux file name&gt;.nulbad.&lt;fwhm&gt;')
        self.__add_option(self.w_pat, 'n', 'pat', None,
          'wavelength delta-lambda of nulbad output spectrum (&Aring;)',
          'default: same as delta-lambda of input spectrum')
        self.__add_option(self.w_convol, 'n', 'convol', True,
          'Apply convolution?', '')
        self.__add_option(self.w_fwhm, 'n', 'fwhm', None,
          'full-width-half-maximum of Gaussian function',
          'default: &lt;main_fwhm&gt; '+FROM_MAIN+')')

        for i, option in enumerate(self.omap):
            try:
                label = option.label = QLabel(enc_name_descr("--%s" %option.name, option.descr_short))
                label.setAlignment(Qt.AlignRight)
                checkbox = option.checkbox = QCheckBox()
                checkbox.setTristate(False)
                widget = option.widget
                lo.addWidget(checkbox, i, 0)
                lo.addWidget(label, i, 1)
                lo.addWidget(widget, i, 2)
                option.widget.setToolTip(option.descr_long)
                if isinstance(widget, QCheckBox):
                    widget.stateChanged.connect(self.on_edited)
                else:
                    widget.textEdited.connect(self.on_edited)
            except:
                self.logger.exception("Processing option '%s'" % option.name)
                raise\


        self.__update_visible_options()
        self.flag_process_changes = True


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def load(self, x):
        assert isinstance(x, Options)
        self.options = x
        self.__update_from_options()
        # this is called to perform file validation upon loading
        self.__update_options()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def setFocus(self, reason=None):
        """Sets focus to first field. Note: reason is ignored."""
        self.omap[0].widget.setFocus()

    def eventFilter(self, obj_focused, event):
        if event.type() == QEvent.FocusIn:
            text = ""
            for label, obj, name, short_descr, color, long_descr in self._map:
                if obj_focused == obj:
                    text = "%s<br><br>%s" % \
                           (_enc_name(name.replace("&", ""), color), long_descr)
                    break
            self._set_descr_text(text)
        return False

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    def on_edited(self):
        #self._update_file_main()

        option = self.__find_by_widget(self.sender())
        option.checkbox.setChecked(True)


        self.edited.emit()

    def on_option_checkbox_clicked(self):
        checkbox = self.sender()
        option = self.__find_by_checkbox(checkbox)
        print "found option ", option

    def on_widget_exe_clicked(self):
        self.__update_visible_options()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear
    
    def __add_option(self, widget, ihpn, name, default, descr_short, descr_long):
        o = _Option()
        w = o.checkbox = QCheckBox()
        w.setTristate(False)
        w.stateChanged.connect(self.on_option_checkbox_clicked)


        o.widget = widget
        o.ihpn = ihpn
        o.name = name
        o.default = default
        o.descr_short = descr_short
        o.descr_long = descr_long
        self.omap.append(o)
        return o

    def __find_by_checkbox(self, checkbox):
        ret = None
        for option in self.omap:
            if option.checkbox == checkbox:
                ret = option
                break
        return ret

    def __find_by_widget(self, widget):
        ret = None
        for option in self.omap:
            if option.widget == widget:
                ret = option
                break
        return ret

    def __update_from_options(self):
        self.flag_process_changes = False
        try:
            for option in self.omap:
                option.update_gui(self.options)
        finally:
            self.flag_process_changes = True

    def __update_options(self):
        print "__update_options(self):............ not yet, not yet"


    def __update_visible_options(self):
        LETTERS = "ihpn"
        ll = []
        for i, (checkbox, letter) in enumerate(zip(self.checkboxes_exes, LETTERS)):
            if checkbox.isChecked():
                ll.append(letter)

        for option in self.omap:
            flag_visible = any([letter in option.ihpn for letter in ll])
            option.checkbox.setVisible(flag_visible)
            option.label.setVisible(flag_visible)
            option.widget.setVisible(flag_visible)



    def _set_error_text(self, x):
        """Sets text of labelError."""
        self.labelError.setText(x)

    def _set_descr_text(self, x):
        """Sets text of labelDescr."""
        self.textEditDescr.setText(x)





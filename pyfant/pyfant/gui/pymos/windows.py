"""Widget to edit a FileDCube object."""

__all__ = ["WSpectrumTable", "XQuery", "XFileSpectrumList", "XFileDCube"]

import collections
import copy
import matplotlib.pyplot as plt
import numbers
import numpy as np
import os
import os.path
from itertools import product, combinations, cycle

from PyQt4.QtCore import *
from PyQt4.QtGui import *

from pyfant import *
from pyfant.data.filespectrumlist import *
from pyfant.gui import *
from .basewindows import *
from .a_WChooseSpectrum import *
from .a_XScaleSpectrum import *

_COLORS_SQ = [(.1, .6, .5), (.5, .1, .7)]
_ITER_COLORS_SQ = cycle(_COLORS_SQ)


def _style_widget(w, flag_changed):
    """(Paints background yellow)/(removes stylesheet)"""
    w.setStyleSheet("QWidget {background-color: #FFFF00}" if flag_changed else "")

# Because several windows of the same class may be created, we'll give them different titles to help avoid confusion
_window_titles = collections.Counter()
def _get_window_title(prefix):
    _window_titles[prefix] += 1
    i = _window_titles[prefix]
    if i == 1:
        return prefix
    else:
        return "%s #%d" % (prefix, i)


def _str_exc(E):
    """Generates a string from an Exception"""
    return "%s: %s" % (E.__class__.__name__, str(E))

class WSpectrumTable(WBase):
    """
    FileDCube editor widget.

    Arguments:
      parent=None
    """

    # argument0 -- flag_changed_header
    edited = pyqtSignal(bool)

    def __init__(self, parent):
        WBase.__init__(self, parent)

        def keep_ref(obj):
            self._refs.append(obj)
            return obj

        # Whether all the values in the fields are valid or not
        self.flag_valid = False
        # Internal flag to prevent taking action when some field is updated programatically
        self.flag_process_changes = False
        self.collection = None # SpectrumCollection


        # # Central layout
        lwex = self.centralLayout = QVBoxLayout()
        lwex.setMargin(0)
        self.setLayout(lwex)
        ###
        lwexex = QHBoxLayout()
        lwexex.setMargin(0)
        lwexex.setSpacing(2)
        lwex.addLayout(lwexex)
        ###
        b = keep_ref(QPushButton("Scale..."))
        b.clicked.connect(self.scale_clicked)
        lwexex.addWidget(b)
        ###
        b = keep_ref(QPushButton("Export CSV..."))
        b.clicked.connect(self.on_export_csv)
        lwexex.addWidget(b)
        ###
        b = self.button_query = QPushButton("Query")
        b.clicked.connect(self.on_query)
        lwexex.addWidget(b)
        ###
        b = self.button_query = QPushButton("Merge with...")
        b.clicked.connect(self.on_merge_with)
        lwexex.addWidget(b)
        ###
        lwexex.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))
        ###
        lwexex = QHBoxLayout()
        lwexex.setMargin(0)
        lwexex.setSpacing(2)
        lwex.addLayout(lwexex)
        ###
        lwexex.addWidget(keep_ref(QLabel("With selected:")))
        ###
        b = keep_ref(QPushButton("Plot &stacked"))
        b.clicked.connect(self.plot_stacked_clicked)
        lwexex.addWidget(b)        
        ###
        b = keep_ref(QPushButton("Plot &overlapped"))
        b.clicked.connect(self.plot_overlapped_clicked)
        lwexex.addWidget(b)
        ###
        b = keep_ref(QPushButton("Calc.Mag."))
        b.clicked.connect(self.calc_mag_clicked)
        lwexex.addWidget(b)
        ###
        b = keep_ref(QPushButton("Open in new window"))
        b.clicked.connect(self.open_in_new_clicked)
        lwexex.addWidget(b)
        ###
        b = keep_ref(QPushButton("Delete"))
        b.clicked.connect(self.delete_selected)
        lwexex.addWidget(b)
        ###
        lwexex.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))
        ###
        a = self.twSpectra = QTableWidget()
        lwex.addWidget(a)
        a.setSelectionMode(QAbstractItemView.MultiSelection)
        a.setSelectionBehavior(QAbstractItemView.SelectRows)
        a.setAlternatingRowColors(True)
        a.cellChanged.connect(self.on_twSpectra_cellChanged)
        a.setEditTriggers(QAbstractItemView.DoubleClicked | QAbstractItemView.EditKeyPressed)
        a.setFont(MONO_FONT)
        a.installEventFilter(self)
        a.setContextMenuPolicy(Qt.CustomContextMenu)
        a.customContextMenuRequested.connect(self.on_twSpectra_customContextMenuRequested)

        self.setEnabled(False)  # disabled until load() is called
        style_checkboxes(self)
        self.flag_process_changes = True
        self.add_log("Welcome from %s.__init__()" % (self.__class__.__name__))


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def set_collection(self, x):
        assert isinstance(x, SpectrumCollection)
        self.collection = x
        self.__update_gui()
        self.setEnabled(True)
        self.button_query.setEnabled(isinstance(self.collection, SpectrumList))

    def get_selected_spectra(self):
        return [self.collection.spectra[i] for i in self.get_selected_row_indexes()]

    def get_selected_row_indexes(self):
        ii = list(set([index.row() for index in self.twSpectra.selectedIndexes()]))
        return ii

    def update(self):
        """Refreshes the GUI to reflect what is in self.collection"""
        self.__update_gui()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def setFocus(self, reason=None):
        """Sets focus to first field. Note: reason is ignored."""
        self.twSpectra.setFocus()

    def eventFilter(self, source, event):
        if event.type() == QEvent.FocusIn:
            # text = random_name()
            # self.__add_log(text)
            pass

        if event.type() == QEvent.KeyPress:
            if event.key() == Qt.Key_Delete:
                if source == self.twSpectra:
                    n_deleted = self.__delete_spectra()
                    if n_deleted > 0:
                        self.edited.emit(False)
        return False

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    def on_twSpectra_customContextMenuRequested(self, position):
        """Mounts, shows popupmenu for the tableWidget control, and takes action."""
        menu = QMenu()
        actions = []
        actions.append(menu.addAction("Delete selected (Del)"))
        action = menu.exec_(self.twSpectra.mapToGlobal(position))
        if action is not None:
            idx = actions.index(action)
            if idx == 0:
                self.__delete_spectra()

    def plot_stacked_clicked(self):
        sspp = self.get_selected_spectra()
        if len(sspp) > 0:
            plot_spectra(sspp)

    def plot_overlapped_clicked(self):
        sspp = self.get_selected_spectra()
        if len(sspp) > 0:
            plot_spectra_overlapped(sspp)

    def calc_mag_clicked(self):
        sspp = self.get_selected_spectra()
        flag_emit, flag_changed_header = False, False
        if len(sspp) > 0:

            try:
                specs = (("band_name", {"value": "V", "labelText": "Band name (%s)" % ("/".join(Bands.bands.keys()),)}),
                         ("flag_force_parametric", {"value": False, "labelText": "Always use parametric band form?",
                                                    "toolTip": "Use (center, FWHM) to calculate the band even if tabular data is available"}),
                        )
                form = XParametersEditor(specs=specs, title="Calculate magnitudee")
                while True:
                    r = form.exec_()
                    if not r:
                        break
                    kk = form.GetKwargs()
                    band_name = kk["band_name"].upper()

                    if not band_name in Bands.bands.keys():
                        show_error("Invalid band name")
                        continue

                    for sp in sspp:
                        dict_ = sp.calculate_magnitude(band_name, kk["flag_force_parametric"])

                    # appends field names so that newly calculated magnitude will appear in the table
                    for fn in dict_["fieldnames"]:
                        if fn not in self.collection.fieldnames:
                            self.collection.fieldnames.append(fn)
                            flag_changed_header = True

                    self.__update_gui()
                    flag_emit = True
                    break

            except Exception as E:
                self.add_log_error("Magnitude calculation: %s" % str(E), True)
                raise

        if flag_emit:
            self.edited.emit(flag_changed_header)

    def open_in_new_clicked(self):
        ii = self.get_selected_row_indexes()
        if len(ii) > 0:
            other = copy.deepcopy(self.collection)
            other.spectra = [other.spectra[i] for i in ii]
            f = FileSpectrumList()
            f.splist = other

            form = self.keep_ref(XFileSpectrumList())
            form.load(f)
            form.show()


    def delete_selected(self):
        n = self.__delete_spectra()
        if n > 0:
            self.edited.emit(False)
        
    def scale_clicked(self):
        if len(self.collection) > 0:
          sp = self.collection.spectra[self.twSpectra.currentRow()]
          
        form = XScaleSpectrum()
        form.set_spectrum(sp)
        if form.exec_():
            k = form.factor()
            if k != 1:
                sp.y *= k
                self.__update_gui()
                self.edited.emit(False)

    def on_twSpectra_cellChanged(self, row, column):
        if self.flag_process_changes:
            flag_emit = False
            text = None
            item = self.twSpectra.item(row, column)
            name = self.__get_tw_header(column)
            try:
                value = str(item.text())
                # Tries to convert to float, otherwise stores as string
                try:
                    value = float(value)
                except:
                    pass

                # Certain fields must evaluate to integer because they are pixel positions
                if name in ("PIXEL-X", "PIXEL-Y", "Z-START"):
                    value = int(value)

                self.collection.spectra[row].more_headers[name] = value

                flag_emit = True
                # replaces edited text with eventually cleaner version, e.g. decimals from integers are discarded
                item.setText(str(value))

            except Exception as E:
                # restores original value
                item.setText(str(self.collection.spectra[row].more_headers.get(name)))
                self.add_log_error(_str_exc(E), True)
                raise

            if flag_emit:
                self.edited.emit(False)

    def on_export_csv(self):
        new_filename = QFileDialog.getSaveFileName(self, "Export text file (CSV format)", "export.csv", "*.csv")
        if new_filename:
            # self.save_dir, _ = os.path.split(str(new_filename))
            try:
                lines = self.collection.to_csv()
                with open(str(new_filename), "w") as file:
                    file.writelines(lines)
            except Exception as E:
                msg = str("Error exporting text file: %s" % _str_exc(E))
                self.add_log_error(msg, True)
                raise

    def on_query(self):
        form = self.keep_ref(XQuery())
        form.set_splist(copy.deepcopy(self.collection))
        form.show()

    def on_merge_with(self):
        flag_emit = False
        try:
            # TODO another SpectrumCollection, not SpectrumList
            new_filename = QFileDialog.getOpenFileName(self, "Merge with another Spectrum List file", "", "*.splist.fits")
            if new_filename:
                new_filename = str(new_filename)
                f = FileSpectrumList()
                f.load(new_filename)
                self.collection.merge_with(f.splist)
                self.__update_gui()
                flag_emit = True
        except Exception as E:
            msg = "Error merging: %s" % _str_exc(E)
            self.add_log_error(msg, True)
            raise

        if flag_emit:
            self.edited.emit(False)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear


    def __get_tw_header(self, column):
        return str(self.twSpectra.horizontalHeaderItem(column).text())

    def __update_gui(self):
        self.flag_process_changes = False
        try:
            # Builds table widget contents
            spectra = self.collection.spectra
            t = self.twSpectra
            n = len(spectra)
            FIXED = ["Spectrum summary report"]
            more_headers = self.collection.fieldnames
            all_headers = more_headers+FIXED
            nc = len(all_headers)
            ResetTableWidget(t, n, nc)
            t.setHorizontalHeaderLabels(all_headers)
            i = 0
            for sp in spectra:
                j = 0

                # Spectrum.more_headers columns
                for h in more_headers:
                    twi = QTableWidgetItem(str(sp.more_headers.get(h)))
                    if h in "Z-START":  # fields that should be made read-only
                        twi.setFlags(twi.flags() & ~Qt.ItemIsEditable)
                    t.setItem(i, j, twi)
                    j += 1

                # Spectrum self-generated report
                twi = QTableWidgetItem(sp.one_liner_str())
                twi.setFlags(twi.flags() & ~Qt.ItemIsEditable)
                t.setItem(i, j, twi)
                j += 1

                i += 1

            t.resizeColumnsToContents()

        finally:
            self.flag_process_changes = True

    def __delete_spectra(self):
        ii = self.get_selected_row_indexes()
        if len(ii) > 0:
            self.collection.delete_spectra(ii)
            self.__update_gui()

        return len(ii)


#######################################################################################################################
#                ####################################################################################
#                               #####################################################
#                                           ##############################


class XQuery(XLogMainWindow):
    """Rudimentary query system for SpectrumCollection"""

    def __init__(self, parent=None, file_main=None, file_abonds=None):
        XLogMainWindow.__init__(self, parent)

        def keep_ref(obj):
            self._refs.append(obj)
            return obj

        self.setWindowTitle(_get_window_title("Query"))

        self.splist = None

        # # Central layout
        cw = self.centralWidget = QWidget()
        self.setCentralWidget(cw)
        lantanide = self.centralLayout = QVBoxLayout(cw)
        lantanide.setMargin(1)
        # self.setLayout(lantanide)

        lgrid = keep_ref(QGridLayout())
        lantanide.addLayout(lgrid)
        lgrid.setMargin(0)
        lgrid.setVerticalSpacing(4)
        lgrid.setHorizontalSpacing(5)

        # field map: [(label widget, edit widget, field name, short description, long description), ...]
        pp = self._map0 = []
        ###
        x = keep_ref(QLabel())
        y = self.edit_expr = QLineEdit("SNR()")
        x.setBuddy(y)
        pp.append((x, y, "&Block expresion", "Other examples: 'MergeDown(np.mean)'", ""))
        ###
        x = keep_ref(QLabel())
        y = self.edit_group_by = QLineEdit()
        x.setBuddy(y)
        pp.append((x, y, "'&Group by' fieldnames", "Comma-separated without quotes", ""))

        for i, (label, edit, name, short_descr, long_descr) in enumerate(pp):
            # label.setStyleSheet("QLabel {text-align: right}")
            assert isinstance(label, QLabel)
            label.setText(enc_name_descr(name, short_descr))
            label.setAlignment(Qt.AlignRight)
            lgrid.addWidget(label, i, 0)
            lgrid.addWidget(edit, i, 1)
            label.setToolTip(long_descr)
            edit.setToolTip(long_descr)

        lgo = QHBoxLayout()
        lantanide.addLayout(lgo)
        ###
        lgo.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))
        ###
        b = keep_ref(QPushButton("&Run"))
        b.clicked.connect(self.on_run)
        lgo.addWidget(b)

        w = self.wsptable = WSpectrumTable(self)
        lantanide.addWidget(w)

        self.setEnabled(False)  # disabled until load() is called
        style_checkboxes(self)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface


    def set_splist(self, x):
        assert isinstance(x, SpectrumList)
        self.splist = x
        self.setEnabled(True)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    def on_run(self):
        try:
            expr = str(self.edit_expr.text())
            s_group_by = str(self.edit_group_by.text())
            group_by = [str(y).upper() for y in [x.strip() for x in s_group_by.split(",")] if len(y) > 0]
            other, errors = self.splist.query_merge_down(expr, group_by)

            if errors:
                S = "\n  - "
                show_error("Running query returned the following errors:"+S+S.join(errors))
            else:
                self.wsptable.set_collection(other)
                # f = self.keep_ref(FileSpectrumList())
                # f.splist = other
                # form = self.keep_ref(XFileSpectrumList())
                # form.load(f)
                # form.show()

        except Exception as E:
            msg = "Error running query: %s" % _str_exc(E)
            self.add_log_error(msg, True)
            raise




    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear



#######################################################################################################################
#                ####################################################################################
#                               #####################################################
#                                           ##############################


class WFileSpectrumList(WBase):
    """
    FileSpectrumList editor widget.

    Arguments:
      parent=None
    """

    def __init__(self, parent):
        WBase.__init__(self, parent)

        def keep_ref(obj):
            self._refs.append(obj)
            return obj

        # Whether __update_f() went ok
        self.flag_valid = False
        # Internal flag to prevent taking action when some field is updated programatically
        self.flag_process_changes = False
        # Whether there is sth in yellow background in the Headers tab
        self.flag_header_changed = False
        self.f = None  # FileSpectrumList object
        self.obj_square = None

        # # Central layout
        lantanide = self.centralLayout = QVBoxLayout()
        lantanide.setMargin(0)
        self.setLayout(lantanide)

        # ## Horizontal splitter occupying main area: (options area) | (plot area)
        sp2 = self.splitter2 = QSplitter(Qt.Horizontal)
        lantanide.addWidget(sp2)

        # ## Widget left of horizontal splitter, containing (File Line) / (Options area)
        wfilett0 = keep_ref(QWidget())
        lwfilett0 = QVBoxLayout(wfilett0)
        lwfilett0.setMargin(0)

        # ### Line showing the File Name
        wfile = keep_ref(QWidget())
        lwfilett0.addWidget(wfile)
        l1 = keep_ref(QHBoxLayout(wfile))
        l1.setMargin(0)
        l1.addWidget(keep_ref(QLabel("<b>File:<b>")))
        w = self.label_fn = QLabel()
        l1.addWidget(w)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # ### Tabbed widget occupying left of horizontal splitter (OPTIONS TAB)
        tt0 = self.tabWidgetOptions = QTabWidget(self)
        lwfilett0.addWidget(tt0)
        tt0.setFont(MONO_FONT)
        tt0.currentChanged.connect(self.current_tab_changed_options)

        # #### Tab: Vertical Splitter between "Place Spectrum" and "Existing Spectra"
        spp = QSplitter(Qt.Vertical)
        tt0.addTab(spp, "&Spectra")

        # ##### Place Spectrum area
        # Widget that will be handled by the scrollable area
        sa0 = keep_ref(QScrollArea())
        sa0.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        sa0.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        wscrw = keep_ref(QWidget())
        sa0.setWidget(wscrw)
        sa0.setWidgetResizable(True)
        ###
        lscrw = QVBoxLayout(wscrw)
        lscrw.setMargin(3)
        ###
        alabel = keep_ref(QLabel("<b>Add spectrum</b>"))
        lscrw.addWidget(alabel)
        ###
        # Place Spectrum variables & button
        lg = keep_ref(QGridLayout())
        lscrw.addLayout(lg)
        lg.setMargin(0)
        lg.setVerticalSpacing(4)
        lg.setHorizontalSpacing(5)
        # lscrw.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))

        # field map: [(label widget, edit widget, field name, short description, long description), ...]
        pp = self._map0 = []
        ###
        x = self.label_sp = QLabel()
        y = self.choosesp = WChooseSpectrum()
        y.installEventFilter(self)
        y.edited.connect(self.on_colors_setup_edited)
        # y.setValidator(QIntValidator())
        x.setBuddy(y)
        pp.append((x, y, "&spectrum", ".dat, .fits ...", ""))

        for i, (label, edit, name, short_descr, long_descr) in enumerate(pp):
            # label.setStyleSheet("QLabel {text-align: right}")
            assert isinstance(label, QLabel)
            label.setText(enc_name_descr(name, short_descr))
            label.setAlignment(Qt.AlignRight)
            lg.addWidget(label, i, 0)
            lg.addWidget(edit, i, 1)
            label.setToolTip(long_descr)
            edit.setToolTip(long_descr)

        # button
        l = QHBoxLayout()
        lscrw.addLayout(l)
        l.setMargin(0)
        l.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))
        b = QPushButton("&Place spectrum")
        l.addWidget(b)
        b.clicked.connect(self.add_spectrum_clicked)

        # ##### Existing Spectra area
        wex = QWidget()
        lwex = QVBoxLayout(wex)
        lwex.setMargin(3)
        ###
        lwex.addWidget(keep_ref(QLabel("<b>Existing spectra</b>")))
        ###
        w = self.wsptable = WSpectrumTable(self.parent_form)
        w.edited.connect(self.on_spectra_edited)
        lwex.addWidget(w)

        # ##### Finally...
        spp.addWidget(sa0)
        spp.addWidget(wex)

        # #### Headers tab
        sa1 = keep_ref(QScrollArea())
        tt0.addTab(sa1, "&Header")
        sa1.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        sa1.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Widget that will be handled by the scrollable area
        w = keep_ref(QWidget())
        sa1.setWidget(w)
        sa1.setWidgetResizable(True)
        lscrw = QVBoxLayout(w)
        lscrw.setMargin(3)
        ###
        lscrw.addWidget(keep_ref(QLabel("<b>Header properties</b>")))

        ###
        b = keep_ref(QPushButton("Collect field names"))
        b.clicked.connect(self.on_collect_fieldnames)
        lscrw.addWidget(b)

        # Form layout
        lg = keep_ref(QGridLayout())
        lg.setMargin(0)
        lg.setVerticalSpacing(4)
        lg.setHorizontalSpacing(5)
        lscrw.addLayout(lg)
        ###
        lscrw.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))

        # field map: [(label widget, edit widget, field name, short description, long description, f_from_f, f_from_edit), ...]
        pp = self._map1 = []

        ###
        x = keep_ref(QLabel())
        y = self.edit_fieldnames = QPlainTextEdit()
        y.textChanged.connect(self.on_header_edited)
        x.setBuddy(y)
        pp.append((x, y, "&Field names", "'header' information for each spectrum", "", lambda: self.f.splist.fieldnames,
                   lambda: self.edit_fieldnames.toPlainText()))
        ###

        for i, (label, edit, name, short_descr, long_descr, f_from_f, f_from_edit) in enumerate(pp):
            # label.setStyleSheet("QLabel {text-align: right}")
            assert isinstance(label, QLabel)
            label.setText(enc_name_descr(name, short_descr))
            label.setAlignment(Qt.AlignRight)
            lg.addWidget(label, i, 0)
            lg.addWidget(edit, i, 1)
            label.setToolTip(long_descr)
            edit.setToolTip(long_descr)

        lgo = QHBoxLayout()
        lgo.setMargin(0)
        lscrw.addLayout(lgo)
        ###
        bgo = self.button_revert = QPushButton("Revert")
        lgo.addWidget(bgo)
        bgo.clicked.connect(self.header_revert)
        ###
        bgo = self.button_apply = QPushButton("Apply")
        lgo.addWidget(bgo)
        bgo.clicked.connect(self.header_apply)
        ###
        lgo.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # #### More Tools tab
        wset = keep_ref(QWidget())
        tt0.addTab(wset, "&More")
        lwset = keep_ref(QVBoxLayout(wset))
        ###
        la = keep_ref(QLabel("<b>Data manipulation</b>"))
        lwset.addWidget(la)
        ###
        b = keep_ref(QPushButton("&Crop in new window..."))
        lwset.addWidget(b)
        b.clicked.connect(self.crop_clicked)
        ###
        b = keep_ref(QPushButton("Add &noise..."))
        lwset.addWidget(b)
        b.clicked.connect(self.add_noise_clicked)
        ###
        b = keep_ref(QPushButton("&Upper envelopes"))
        lwset.addWidget(b)
        b.clicked.connect(self.rubberband_clicked)
        ###
        b = keep_ref(QPushButton("&Extract continua"))
        lwset.addWidget(b)
        b.clicked.connect(self.extract_continua_clicked)
        ###
        lwset.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))

        # # ### Tabbed widget occupying right of horizontal splitter
        # tt1 = self.tabWidgetVis = QTabWidget(self)
        # tt1.setFont(MONO_FONT)
        # tt1.currentChanged.connect(self.current_tab_changed_vis)
        #
        # # #### Tab containing 3D plot representation
        # w0 = keep_ref(QWidget())
        # tt1.addTab(w0, "&P")
        # # #### Colors tab
        # w1 = keep_ref(QWidget())
        # tt1.addTab(w1, "&Q")

        # ### Finally ...
        sp2.addWidget(wfilett0)
        # sp2.addWidget(tt1)


        self.setEnabled(False)  # disabled until load() is called
        style_checkboxes(self)
        self.flag_process_changes = True
        self.add_log("Welcome from %s.__init__()" % (self.__class__.__name__))

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def load(self, x):
        assert isinstance(x, FileSpectrumList)
        self.f = x
        self.wsptable.set_collection(x.splist)
        self.__update_gui(True)
        self.flag_valid = True  # assuming that file does not come with errors
        self.setEnabled(True)

    def update_splist_headers(self, splist):
        """Updates headers of a SpectrumList objects using contents of the Headers tab"""
        emsg, flag_error = "", False
        ss = ""
        flag_emit = False
        try:
            ss = "fieldnames"
            ff = eval_fieldnames(str(self.edit_fieldnames.toPlainText()))
            splist.fieldnames = ff
            self.__update_gui(True)
            flag_emit = True
        except Exception as E:
            flag_error = True
            if ss:
                emsg = "Field '%s': %s" % (ss, _str_exc(E))
            else:
                emsg = _str_exc(E)
            self.add_log_error(emsg)
        if flag_emit:
            self.__emit_if()
        return not flag_error

    def update_gui_label_fn(self):
        self.__update_gui_label_fn()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def setFocus(self, reason=None):
        """Sets focus to first field. Note: reason is ignored."""

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    def on_colors_setup_edited(self):
        if self.flag_process_changes:
            pass
            # self.flag_plot_colors_pending = True

    def on_header_edited(self):
        if self.flag_process_changes:
            sth = False
            sndr = self.sender()
            for _, edit, _, _, _, f_from_f, f_from_edit in self._map1:
                changed = f_from_f() != f_from_edit()
                sth = sth or changed
                if edit == sndr:
                    _style_widget(self.sender(), changed)
            self.set_flag_header_changed(sth)

    def add_spectrum_clicked(self):
        flag_emit = False
        try:
            sp = self.choosesp.sp
            if not sp:
                raise RuntimeError("Spectrum not loaded")
            sp = copy.deepcopy(sp)
            self.f.splist.add_spectrum(sp)
            self.__update_gui()
            flag_emit = True
        except Exception as E:
            self.add_log_error(_str_exc(E), True)
            raise
        if flag_emit:
            self.edited.emit()

    def header_revert(self):
        self.__update_gui_header()

    def header_apply(self):
        if self.update_splist_headers(self.f.splist):
            self.__update_gui(True)

    def current_tab_changed_vis(self):
        pass
        # if self.flag_plot_colors_pending:
        #     self.plot_colors()

    def current_tab_changed_options(self):
        pass

    def crop_clicked(self):
        try:
            splist = self.f.splist
            specs = (("wavelength_range", {"value": "[%g, %g]" % (splist.wavelength[0], splist.wavelength[-1])}),)
            form = XParametersEditor(specs=specs, title="Add Gaussian noise")
            while True:
                r = form.exec_()
                if not r:
                    break
                kk = form.GetKwargs()
                s = ""
                try:
                    s = "wavelength_range"
                    lambda0, lambda1 = eval(kk["wavelength_range"])
                except Exception as E:
                    self.add_log_error("Failed evaluating %s: %s" % (s, _str_exc(E)), True)
                    continue

                # Works with clone, then replaces original, to ensure atomic operation
                clone = copy.deepcopy(self.f)
                clone.filename = None
                try:
                    clone.splist.crop(lambda0, lambda1)
                except Exception as E:
                    self.add_log_error("Crop operation failed: %s" % _str_exc(E), True)
                    continue

                self.__new_window(clone)
                break

        except Exception as E:
            self.add_log_error("Crop failed: %s" % _str_exc(E), True)
            raise

    def rubberband_clicked(self):
        self.__use_sblock(SB_Rubberband(flag_upper=True))

    def add_noise_clicked(self):
        specs = (("std", {"labelText": "Noise standard deviation", "value": 1.}),)
        form = XParametersEditor(specs=specs, title="Select sub-range")
        if form.exec_():
            block = SB_AddNoise(**form.GetKwargs())
            self.__use_sblock(block)

    def extract_continua_clicked(self):
        self.__use_slblock(ExtractContinua())

    # def std_clicked(self):
    #     self.__use_slblock(MergeDown(np.std))
    #
    # def snr_clicked(self):
    #     self.__use_slblock(SNR())

    def on_collect_fieldnames(self):
        # TODO confirmation

        self.edit_fieldnames.setPlainText(str(self.f.splist.collect_fieldnames()))

    #        self.__update_gui(True)

    def on_spectra_edited(self, flag_changed_header):
        if flag_changed_header:
            self.__update_gui_header()
        self.__update_gui_vis()
        self.edited.emit()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear

    def __emit_if(self):
        if self.flag_process_changes:
            self.edited.emit()

    def __update_gui(self, flag_header=False):
        """Updates GUI to reflect what is in self.f"""
        self.flag_process_changes = False
        try:
            self.__update_gui_label_fn()
            self.wsptable.update()
            if flag_header:
                self.__update_gui_header()
        finally:
            self.flag_process_changes = True

    def __update_gui_label_fn(self):
        if not self.f:
            text = "(not loaded)"
        elif self.f.filename:
            text = os.path.relpath(self.f.filename, ".")
        else:
            text = "(filename not set)"
        self.label_fn.setText(text)

    def __update_gui_header(self):
        """Updates header controls only"""
        splist = self.f.splist
        self.edit_fieldnames.setPlainText(str(splist.fieldnames))
        self.set_flag_header_changed(False)

    def __update_gui_vis(self):
        pass

    def set_flag_header_changed(self, flag):
        self.button_apply.setEnabled(flag)
        self.button_revert.setEnabled(flag)
        self.flag_header_changed = flag
        if not flag:
            # If not changed, removes all eventual yellows
            for _, edit, _, _, _, _, _ in self._map1:
                _style_widget(edit, False)

    def __update_f(self):
        self.flag_valid = self.update_splist_headers(self.f.splist)

    def __new_window(self, clone):
        """Opens new FileDCube in new window"""
        form1 = self.keep_ref(self.parent_form.__class__())
        form1.load(clone)
        form1.show()

    def __use_sblock(self, block):
        """Uses block and opens result in new window"""

        # Does not touch the original self.f
        clone = copy.deepcopy(self.f)
        clone.filename = None
        slblock = UseSBlock()
        for i, sp in enumerate(clone.splist.spectra):
            clone.splist.spectra[i] = block.use(sp)
        self.__new_window(clone)

    def __use_slblock(self, block):
        """Uses block and opens result in new window"""
        # Here not cloning current spectrum list, but trusting the block
        block.flag_copy_wavelength = True
        output = block.use(self.f.splist)
        f = self.__new_from_existing()
        f.splist = output
        self.__new_window(f)

    def __new_from_existing(self):
        """Creates new FileSpectrumList from existing one"""
        f = FileSpectrumList()
        return f

#######################################################################################################################
#                ####################################################################################
#                               #####################################################
#                                           ##############################

class XFileSpectrumList(XFileMainWindow):
    def __init__(self, parent=None, fileobj=None):
        XFileMainWindow.__init__(self, parent)

        def keep_ref(obj):
            self._refs.append(obj)
            return obj

        self.setWindowTitle(_get_window_title("Spectrum List Editor"))

        # # Synchronized sequences
        _VVV = FileSpectrumList.description
        self.tab_texts[0] =  "FileSpectrumList editor (Alt+&1)"
        self.tabWidget.setTabText(0, self.tab_texts[0])
        self.save_as_texts[0] = "Save %s as..." % _VVV
        self.open_texts[0] = "Load %s" % _VVV
        self.clss[0] = FileSpectrumList
        self.clsss[0] = (FileSpectrumList, FileCCube)  # file types that can be opened
        self.wilds[0] = "*.splist.fits"

        lv = keep_ref(QVBoxLayout(self.gotting))
        ce = self.ce = WFileSpectrumList(self)
        lv.addWidget(ce)
        ce.edited.connect(self.on_tab0_file_edited)
        self.editors[0] = ce

        # # Loads default file by default
        if os.path.isfile(FileSpectrumList.default_filename):
            f = FileSpectrumList()
            f.load()
            self.ce.load(f)

        if fileobj is not None:
            self.load(fileobj)


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Interface

    def set_manager_form(self, x):
        assert isinstance(x, XRunnableManager)
        self._manager_form = x
        self._rm = x.rm

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Qt override

    def closeEvent(self, event):
        flag_exit, ff = True, []
        for ed, flag_changed in zip(self.editors, self.flags_changed):
            if ed and ed.f and flag_changed:
                ff.append(ed.f.description)

        if len(ff) > 0:
            s = "Unsaved changes\n  -"+("\n  -".join(ff))+"\n\nAre you sure you want to exit?"
            flag_exit = are_you_sure(True, event, self, "Unsaved changes", s)
        if flag_exit:
            plt.close("all")

    def keyPressEvent(self, evt):
        incr = 0
        if evt.modifiers() == Qt.ControlModifier:
            n = self.tabWidget.count()
            if evt.key() in [Qt.Key_PageUp, Qt.Key_Backtab]:
                incr = -1
            elif evt.key() in [Qt.Key_PageDown, Qt.Key_Tab]:
                incr = 1
            if incr != 0:
                new_index = self._get_tab_index() + incr
                if new_index < 0:
                    new_index = n-1
                elif new_index >= n:
                    new_index = 0
                self.tabWidget.setCurrentIndex(new_index)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    # def on_show_rm(self):
    #     if self._manager_form:
    #         self._manager_form.show()
    #         self._manager_form.raise_()
    #         self._manager_form.activateWindow()

    def on_tab0_file_edited(self):
        self._on_edited()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Protected methods to be overriden or used by descendant classes

    def _on_edited(self):
        index = self._get_tab_index()
        self.flags_changed[index] = True
        self._update_tab_texts()

    def _filter_on_load(self, f):
        """Converts from FileCCube to FileSpectrumList format, if necessary"""
        if isinstance(f, FileCCube):
            f1 = FileSpectrumList()
            f1.dcube.from_compass_cube(f.ccube)
            f = f1
        return f


class WFileSky(WBase):
    """
    FileDCube editor widget.

    Arguments:
      parent=None
    """

    def __init__(self, parent):
        WBase.__init__(self, parent)

        def keep_ref(obj):
            self._refs.append(obj)
            return obj

        # Whether all the values in the fields are valid or not
        self.flag_valid = False
        # Internal flag to prevent taking action when some field is updated programatically
        self.flag_process_changes = False
        # GUI update needed but cannot be applied immediately, e.g. visible range being edited
        # each flag for one tab
        self.flag_update_pending = [False, False]
        # callables to update each visualization tab
        self.map_update_vis = [self.plot_spectra, self.plot_colors]
        # Whether there is sth in yellow background in the Headers tab
        self.flag_header_changed = False
        self.f = None  # FileDCube object
        self.obj_square = None

        # # Central layout
        lantanide = self.centralLayout = QVBoxLayout()
        lantanide.setMargin(0)
        self.setLayout(lantanide)

        # ## Horizontal splitter occupying main area: (options area) | (plot area)
        sp2 = self.splitter2 = QSplitter(Qt.Horizontal)
        lantanide.addWidget(sp2)

        # ## Widget left of horizontal splitter, containing (File Line) / (Options area)
        wfilett0 = keep_ref(QWidget())
        lwfilett0 = QVBoxLayout(wfilett0)
        lwfilett0.setMargin(0)

        # ### Line showing the File Name
        wfile = keep_ref(QWidget())
        lwfilett0.addWidget(wfile)
        l1 = keep_ref(QHBoxLayout(wfile))
        l1.setMargin(0)
        l1.addWidget(keep_ref(QLabel("<b>File:<b>")))
        w = self.label_fn_sky = QLabel()
        l1.addWidget(w)
        l1.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # ### Tabbed widget occupying left of horizontal splitter (OPTIONS TAB)
        tt0 = self.tabWidgetOptions = QTabWidget(self)
        lwfilett0.addWidget(tt0)
        tt0.setFont(MONO_FONT)
        tt0.currentChanged.connect(self.current_tab_changed_options)

        # #### Tab: Vertical Splitter between "Place Spectrum" and "Existing Spectra"
        spp = QSplitter(Qt.Vertical)
        tt0.addTab(spp, "&Spectra")

        # ##### Place Spectrum area
        # Widget that will be handled by the scrollable area
        sa0 = keep_ref(QScrollArea())
        sa0.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        sa0.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        wscrw = keep_ref(QWidget())
        sa0.setWidget(wscrw)
        sa0.setWidgetResizable(True)
        ###
        lscrw = QVBoxLayout(wscrw)
        lscrw.setMargin(3)
        ###
        alabel = keep_ref(QLabel("<b>Place spectrum</b>"))
        lscrw.addWidget(alabel)
        ###
        # Place Spectrum variables & button
        lg = keep_ref(QGridLayout())
        lscrw.addLayout(lg)
        lg.setMargin(0)
        lg.setVerticalSpacing(4)
        lg.setHorizontalSpacing(5)
        # lscrw.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))

        # field map: [(label widget, edit widget, field name, short description, long description), ...]
        pp = self._map0 = []
        ###
        x = self.label_sp = QLabel()
        y = self.choosesp = WChooseSpectrum()
        y.installEventFilter(self)
        y.edited.connect(self.on_colors_setup_edited)
        # y.setValidator(QIntValidator())
        x.setBuddy(y)
        pp.append((x, y, "&spectrum", ".dat, .fits ...", ""))
        ###
        x = self.label_x = QLabel()
        y = self.spinbox_X = QSpinBox()
        y.valueChanged.connect(self.on_place_spectrum_edited)
        y.setMinimum(0)
        y.setMaximum(100)
        x.setBuddy(y)
        pp.append((x, y, "&x", "x-coordinate<br>(pixels; 0-based)", ""))
        ###
        x = self.label_y = QLabel()
        # TODO more elegant as spinboxes
        y = self.spinbox_Y = QSpinBox()
        y.valueChanged.connect(self.on_place_spectrum_edited)
        y.setMinimum(0)
        y.setMaximum(100)
        x.setBuddy(y)
        pp.append((x, y, "&y", "y-coordinate", ""))
        # ##### FWHM maybe later
        # x = self.label_fwhm = QLabel()
        # y = self.lineEdit_fwhm = QLineEdit()
        # y.installEventFilter(self)
        # y.textEdited.connect(self.on_place_spectrum_edited)
        # y.setValidator(QDoubleValidator(0, 10, 5))
        # x.setBuddy(y)
        # pp.append((x, y, "f&whm", "full width at<br>half-maximum (pixels)", ""))


        for i, (label, edit, name, short_descr, long_descr) in enumerate(pp):
            # label.setStyleSheet("QLabel {text-align: right}")
            assert isinstance(label, QLabel)
            label.setText(enc_name_descr(name, short_descr))
            label.setAlignment(Qt.AlignRight)
            lg.addWidget(label, i, 0)
            lg.addWidget(edit, i, 1)
            label.setToolTip(long_descr)
            edit.setToolTip(long_descr)

        # button
        l = QHBoxLayout()
        lscrw.addLayout(l)
        l.setMargin(0)
        l.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))
        b = QPushButton("&Place spectrum")
        l.addWidget(b)
        b.clicked.connect(self.go_clicked)

        # ##### Existing Spectra area
        wex = QWidget()
        lwex = QVBoxLayout(wex)
        lwex.setMargin(3)
        ###
        lwex.addWidget(keep_ref(QLabel("<b>Existing spectra</b>")))
        ###
        w = self.wsptable = WSpectrumTable(self.parent_form)
        w.edited.connect(self.on_spectra_edited)
        lwex.addWidget(w)

        # ##### Finally...
        spp.addWidget(sa0)
        spp.addWidget(wex)

        # #### Second tab (NEW FileDCube)
        sa1 = keep_ref(QScrollArea())
        tt0.addTab(sa1, "&Header")
        sa1.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        sa1.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        # Widget that will be handled by the scrollable area
        w = keep_ref(QWidget())
        sa1.setWidget(w)
        sa1.setWidgetResizable(True)
        lscrw = QVBoxLayout(w)
        lscrw.setMargin(3)
        ###
        lscrw.addWidget(keep_ref(QLabel("<b>Header properties</b>")))
        ###
        b = keep_ref(QPushButton("Collect field names"))
        b.clicked.connect(self.on_collect_fieldnames)
        lscrw.addWidget(b)

        # Form layout
        lg = keep_ref(QGridLayout())
        lg.setMargin(0)
        lg.setVerticalSpacing(4)
        lg.setHorizontalSpacing(5)
        lscrw.addLayout(lg)
        ###
        lscrw.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))

        # field map: [(label widget, edit widget, field name, short description, long description), ...]
        pp = self._map1 = []

        ###
        x = keep_ref(QLabel())
        y = self.edit_fieldnames = QPlainTextEdit()
        y.textChanged.connect(self.on_header_edited)
        x.setBuddy(y)
        pp.append((x, y, "&Field names", "'header' information for each spectrum", "", lambda: self.f.dcube.fieldnames,
                   lambda: self.edit_fieldnames.toPlainText()))
        ###
        x = self.label_width = QLabel()
        y = self.spinbox_width = QSpinBox()
        y.valueChanged.connect(self.on_header_edited)
        y.setMinimum(1)
        y.setMaximum(100)
        x.setBuddy(y)
        pp.append((x, y, "&width", "hi-resolution (HR) width (pixels)", "", lambda: self.f.dcube.width,
                   lambda: self.spinbox_width.value()))
        ###
        x = self.label_height = QLabel()
        y = self.spinbox_height = QSpinBox()
        y.valueChanged.connect(self.on_header_edited)
        y.setMinimum(0)
        y.setMaximum(100)
        x.setBuddy(y)
        pp.append(
            (x, y, "&height", "HR height (pixels)", "", lambda: self.f.dcube.height, lambda: self.spinbox_height.value()))
        ###
        x = self.label_hrfactor = QLabel()
        y = self.spinbox_hrfactor = QSpinBox()
        y.valueChanged.connect(self.on_header_edited)
        y.setMinimum(1)
        y.setMaximum(100)
        x.setBuddy(y)
        pp.append((x, y, "&hrfactor", "(HR width)/(ifu width)", "", lambda: self.f.dcube.hrfactor,
                   lambda: self.spinbox_hrfactor.value()))
        ###
        x = self.label_hr_pix_size = QLabel()
        y = self.lineEdit_hr_pix_size = QLineEdit()
        y.installEventFilter(self)
        y.textEdited.connect(self.on_header_edited)
        y.setValidator(QDoubleValidator(0, 1, 7))
        x.setBuddy(y)
        pp.append((x, y, "&hr_pix_size", "HR pixel width/height (arcsec)", "", lambda: self.f.dcube.hr_pix_size,
                   lambda: float(self.lineEdit_hr_pix_size.text())))
        ###
        x = self.label_hrfactor = QLabel()
        y = self.spinbox_R = QSpinBox()
        y.valueChanged.connect(self.on_header_edited)
        y.setMinimum(100)
        y.setMaximum(100000)
        y.setSingleStep(100)
        x.setBuddy(y)
        pp.append(
            (x, y, "&R", "resolution (delta lambda)/lambda", "", lambda: self.f.dcube.R, lambda: self.spinbox_R.value()))

        for i, (label, edit, name, short_descr, long_descr, f_from_f, f_from_edit) in enumerate(pp):
            # label.setStyleSheet("QLabel {text-align: right}")
            assert isinstance(label, QLabel)
            label.setText(enc_name_descr(name, short_descr))
            label.setAlignment(Qt.AlignRight)
            lg.addWidget(label, i, 0)
            lg.addWidget(edit, i, 1)
            label.setToolTip(long_descr)
            edit.setToolTip(long_descr)

        lgo = QHBoxLayout()
        lgo.setMargin(0)
        lscrw.addLayout(lgo)
        ###
        bgo = self.button_revert = QPushButton("Revert")
        lgo.addWidget(bgo)
        bgo.clicked.connect(self.header_revert)
        ###
        bgo = self.button_apply = QPushButton("Apply")
        lgo.addWidget(bgo)
        bgo.clicked.connect(self.header_apply)
        ###
        lgo.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))

        # #### More Tools tab
        wset = keep_ref(QWidget())
        tt0.addTab(wset, "&More")
        lwset = keep_ref(QVBoxLayout(wset))
        ###
        b = keep_ref(QPushButton("&Crop in new window..."))
        lwset.addWidget(b)
        b.clicked.connect(self.crop_clicked)
        ###
        b = keep_ref(QPushButton("E&xport %s ..." % FileCCube.description))
        lwset.addWidget(b)
        b.clicked.connect(self.export_ccube_clicked)
        ###
        lwset.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding))

        # ### Tabbed widget occupying right of horizontal splitter
        tt1 = self.tabWidgetVis = QTabWidget(self)
        tt1.setFont(MONO_FONT)
        tt1.currentChanged.connect(self.current_tab_changed_vis)

        # #### Tab containing 3D plot representation
        w0 = keep_ref(QWidget())
        tt1.addTab(w0, "&Plot 3D")
        # http://stackoverflow.com/questions/12459811
        self.figure0, self.canvas0, self.lfig0 = get_matplotlib_layout(w0)

        # lscrw.addLayout(lfig)

        # #### Colors tab
        w1 = keep_ref(QWidget())
        tt1.addTab(w1, "&Colors")
        ###
        lw1 = QVBoxLayout(w1)
        lwset = keep_ref(QHBoxLayout())
        lw1.addLayout(lwset)
        ###
        la = keep_ref(QLabel("&Visible range"))
        lwset.addWidget(la)
        ###
        ed = self.lineEdit_visibleRange = QLineEdit("[3800., 7500.]")
        lwset.addWidget(ed)
        la.setBuddy(ed)
        ed.textEdited.connect(self.on_colors_setup_edited)
        ###
        la = keep_ref(QLabel("Color map"))
        lwset.addWidget(la)
        ###
        ed = self.comboBox_cmap = QComboBox()
        ed.addItem("0-Rainbow")
        ed.addItem("1-RGB")
        ed.setCurrentIndex(0)
        lwset.addWidget(ed)
        la.setBuddy(ed)
        ed.currentIndexChanged.connect(self.on_colors_setup_edited)
        ###
        cb = self.checkBox_scale = QCheckBox("Scale colors")
        lwset.addWidget(cb)
        # cb.setTooltip("If checked, will make color luminosity proportional to flux area under the visible range")
        cb.stateChanged.connect(self.on_colors_setup_edited)
        ###
        b = keep_ref(QPushButton("Redra&w"))
        lwset.addWidget(b)
        b.clicked.connect(self.replot_colors)
        ###
        lwset.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum))
        ###
        wm = keep_ref(QWidget())
        # wm.setMargin(0)
        lw1.addWidget(wm)
        self.figure1, self.canvas1, self.lfig1 = get_matplotlib_layout(wm)
        self.canvas1.mpl_connect('button_press_event', self.on_colors_click)

        # ### Finally ...
        sp2.addWidget(wfilett0)
        sp2.addWidget(tt1)

        # # Timers
        ##########
        t = self.timer_place = QTimer(self)
        t.timeout.connect(self.timeout_place)
        t.setInterval(500)
        t.start()

        self.setEnabled(False)  # disabled until load() is called
        style_checkboxes(self)
        self.flag_process_changes = True
        self.add_log("Welcome from %s.__init__()" % (self.__class__.__name__))

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Interface

    def load(self, x):
        assert isinstance(x, FileDCube)
        self.f = x
        self.wsptable.set_collection(x.dcube)
        self.__update_gui(True)
        self.flag_valid = True  # assuming that file does not come with errors
        self.setEnabled(True)

    def update_gui_label_fn(self):
        self.__update_gui_label_fn()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def setFocus(self, reason=None):
        """Sets focus to first field. Note: reason is ignored."""
        # TODO self.lineEdit_titrav.setFocus()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    def on_colors_setup_edited(self):
        if self.flag_process_changes:
            self.flag_update_pending[1] = True

    def on_header_edited(self):
        if self.flag_process_changes:
            sth = False
            sndr = self.sender()
            for _, edit, _, _, _, f_from_f, f_from_edit in self._map1:
                changed = f_from_f() != f_from_edit()
                sth = sth or changed
                if edit == sndr:
                    style_widget(self.sender(), changed)
            self.set_flag_header_changed(sth)

    def on_spectra_edited(self):
        self.__update_gui_vis()
        self.edited.emit()

    def on_place_spectrum_edited(self):
        # could only update the obj_square but this is easier
        self.plot_colors()

    def go_clicked(self):
        print "GO CLICKED\n" * 10
        flag_emit = False
        try:
            x, y = self.get_place_spectrum_xy()
            sp = self.choosesp.sp
            if not sp:
                raise RuntimeError("Spectrum not loaded")
            sp.pixel_x, sp.pixel_y = x, y
            self.f.dcube.add_spectrum(sp)
            self.__update_gui()
            flag_emit = True
        except Exception as E:
            self.add_log_error(_str_exc(E), True)
            raise
        if flag_emit:
            self.edited.emit()

    def header_revert(self):
        self.__update_gui_header()

    def header_apply(self):
        if self.__update_f_header(self.f.dcube):
            self.__update_gui(True)

    def current_tab_changed_vis(self):
        self.__update_gui_vis_if_pending()

    def current_tab_changed_options(self):
        pass

    def timeout_place(self):
        if self.obj_square:
            next_color = _ITER_COLORS_SQ.next()
            for obj in self.obj_square:
                obj.set_color(next_color)
                self.canvas1.draw()

    def crop_clicked(self):
        try:
            sky = self.f.dcube

            specs = (("x_range", {"value": "[%d, %d]" % (0, sky.width - 1)}),
                     ("y_range", {"value": "[%d, %d]" % (0, sky.height - 1)}),
                     ("wavelength_range", {"value": "[%g, %g]" % (sky.wavelength[0], sky.wavelength[-1])})
                     )
            # fields = ["x_range", "y_range", "wavelength_range"]
            form = XParametersEditor(specs=specs, title="Select sub-cube")
            while True:
                r = form.exec_()
                if not r:
                    break
                kk = form.GetKwargs()
                s = ""
                try:
                    s = "x_range"
                    x0, x1 = eval(kk["x_range"])
                    s = "y_range"
                    y0, y1 = eval(kk["y_range"])
                    s = "wavelength_range"
                    lambda0, lambda1 = eval(kk["wavelength_range"])
                except Exception as E:
                    self.add_log_error("Failed evaluating %s: %s" % (s, _str_exc(E)), True)
                    continue

                # Works with clone, then replaces original, to ensure atomic operation
                clone = copy.deepcopy(self.f)
                clone.filename = None
                try:
                    clone.dcube.crop(x0, x1, y0, y1, lambda0, lambda1)
                except Exception as E:
                    self.add_log_error("Crop operation failed: %s" % _str_exc(E), True)
                    continue

                form1 = self.keep_ref(self.parent_form.__class__())
                form1.load(clone)
                form1.show()

                # # Replaces original
                # self.f = clone
                # self.__update_gui(True)
                break

        except Exception as E:
            self.add_log_error("Crop failed: %s" % _str_exc(E), True)
            raise

    def export_ccube_clicked(self):
        fn = QFileDialog.getSaveFileName(self, "Save file in %s format" % FileCCube.description,
                                         FileCCube.default_filename, "*.fits")
        if fn:
            try:
                fn = str(fn)
                ccube = self.f.dcube.to_compass_cube()
                fccube = FileCCube()
                fccube.ccube = ccube
                fccube.save_as(fn)
            except Exception as E:
                self.add_log_error("Failed export: %s" % _str_exc(E), True)
                raise

    def replot_colors(self):
        self.plot_colors()

    def on_colors_click(self, event):
        x, y = int(event.xdata + .5), int(event.ydata + .5)
        if 0 <= x < self.f.dcube.width and 0 <= y < self.f.dcube.height:
            self.spinbox_X.setValue(x)
            self.spinbox_Y.setValue(y)
            self.plot_colors()

    def on_collect_fieldnames(self):
        # TODO confirmation
        self.edit_fieldnames.setPlainText(str(self.f.dcube.collect_fieldnames()))

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Internal gear

    def __emit_if(self):
        if self.flag_process_changes:
            self.edited.emit()

    def get_place_spectrum_xy(self):
        x = int(self.spinbox_X.value())
        if not (0 <= x < self.f.dcube.width):
            raise RuntimeError("x must be in [0, %s)" % self.f.dcube.width)
        y = int(self.spinbox_Y.value())
        if not (0 <= y < self.f.dcube.height):
            raise RuntimeError("y must be in [0, %s)" % self.f.dcube.height)
        return x, y

    def __update_gui(self, flag_header=False):
        """Updates GUI to reflect what is in self.f"""
        self.flag_process_changes = False
        try:
            self.__update_gui_label_fn()
            self.wsptable.update()
            self.__update_gui_vis()
            if flag_header:
                self.__update_gui_header()
        finally:
            self.flag_process_changes = True

    def __update_gui_vis(self):
        idx = self.tabWidgetVis.currentIndex()
        for i, callable_ in enumerate(self.map_update_vis):
            # Updates current visualization tab and flags update pending for other tabs
            if i == idx:
                callable_()
                self.flag_update_pending[i] = False
            else:
                self.flag_update_pending[i] = True

    def __update_gui_label_fn(self):
        if not self.f:
            text = "(not loaded)"
        elif self.f.filename:
            text = os.path.relpath(self.f.filename, ".")
        else:
            text = "(filename not set)"
        self.label_fn_sky.setText(text)

    def __update_gui_header(self):
        """Updates header controls only"""
        sky = self.f.dcube
        self.spinbox_width.setValue(sky.width)
        self.spinbox_height.setValue(sky.height)
        self.spinbox_hrfactor.setValue(sky.hrfactor)
        self.lineEdit_hr_pix_size.setText(str(sky.hr_pix_size))
        self.spinbox_R.setValue(sky.R)
        self.edit_fieldnames.setPlainText(str(sky.fieldnames))
        self.set_flag_header_changed(False)

    def set_flag_header_changed(self, flag):
        self.button_apply.setEnabled(flag)
        self.button_revert.setEnabled(flag)
        self.flag_header_changed = flag
        if not flag:
            # If not changed, removes all eventual yellows
            for _, edit, _, _, _, _, _ in self._map1:
                style_widget(edit, False)

    def __update_f(self):
        o = self.f
        sky = self.f.dcube
        self.flag_valid = self.__update_f_header(sky)

    def __update_f_header(self, sky):
        """Updates headers of a DataCube objects using contents of the Headers tab"""
        emsg, flag_error = "", False
        ss = ""
        try:
            ss = "fieldnames"
            ff = eval_fieldnames(str(self.edit_fieldnames.toPlainText()))
            sky.fieldnames = ff
            ss = "width"
            sky.width = int(self.spinbox_width.value())
            ss = "height"
            sky.height = int(self.spinbox_height.value())
            ss = "hrfactor"
            sky.hrfactor = int(self.spinbox_hrfactor.value())
            ss = "hr_pix_size"
            sky.hr_pix_size = float(self.lineEdit_hr_pix_size.text())
            ss = "R"
            sky.R = float(self.spinbox_R.value())
            self.__update_gui(True)
            flag_emit = True
        except Exception as E:
            flag_error = True
            if ss:
                emsg = "Field '%s': %s" % (ss, _str_exc(E))
            else:
                emsg = _str_exc(E)
            self.add_log_error(emsg)
        if flag_emit:
            self.__emit_if()
        return not flag_error

    def __update_gui_vis_if_pending(self):
        idx = self.tabWidgetVis.currentIndex()
        if self.flag_update_pending[idx]:
            self.map_update_vis[idx]()
            self.flag_update_pending[idx] = False

    def plot_spectra(self):
        # self.clear_markers()
        if self.f is None:
            return

        try:
            fig = self.figure0
            fig.clear()
            ax = fig.gca(projection='3d')
            _plot_spectra(ax, self.f.dcube)
            fig.tight_layout()
            self.canvas0.draw()

        except Exception as E:
            self.add_log_error(_str_exc(E))
            get_python_logger().exception("Could not plot spectra")

    def plot_colors(self):
        # self.clear_markers()
        if self.f is None:
            return

        try:
            vrange = eval(str(self.lineEdit_visibleRange.text()))
            if len(vrange) != 2 or not all([isinstance(x, numbers.Number) for x in vrange]):
                raise RuntimeError('Visible range must be a sequence with two numbers')

            fig = self.figure1
            fig.clear()
            ax = fig.gca()
            sqx, sqy = None, None
            flag_scale = self.checkBox_scale.isChecked()
            method = self.comboBox_cmap.currentIndex()
            try:
                sqx, sqy = self.get_place_spectrum_xy()
            except:
                pass  # Nevermind (does not draw square)
            self.obj_square = _plot_colors(ax, self.f.dcube, vrange, sqx, sqy, flag_scale, method)

            fig.tight_layout()
            self.canvas1.draw()

            self.flag_plot_colors_pending = False
        except Exception as E:
            self.add_log_error(_str_exc(E))
            get_python_logger().exception("Could not plot colors")


def _plot_spectra(ax, sky):
    """
    Plots front and back grid, scaled fluxes

    data cube            mapped to 3D axis
    ------------------   -----------------
    X pixel coordinate   x
    Y pixel coordinate   z
    Z wavelength         y
    """
    assert isinstance(sky, DataCube)

    flag_empty = len(sky.spectra) == 0
    r0 = [-.5, sky.width + .5]
    r2 = [-.5, sky.height + .5]
    if flag_empty:
        r1 = [-.5, .5]
    else:
        max_flux = max([max(sp.flux) for sp in sky.spectra])
        _y = sky.wavelength
        dlambda = _y[1] - _y[0]
        r1 = [_y[0] - dlambda / 2, _y[-1] + dlambda / 2]
        scale = 1. / max_flux

    PAR = {"color": "y", "alpha": 0.3}

    def draw_line(*args):
        ax.plot3D(*args, **PAR)

    # draws cube
    if not flag_empty:
        for s, e in combinations(np.array(list(product(r0, r1, r2))), 2):
            if np.sum(s == e) == 2:
                # if np.sum(np.abs(s - e)) == r[1] - r[0]:
                draw_line(*zip(s, e))

    # draws grids
    for i in range(sky.width + 1):
        draw_line([i - .5] * 2, [r1[0]] * 2, r2)
        draw_line([i - .5] * 2, [r1[1]] * 2, r2)
    for i in range(sky.height + 1):
        draw_line(r0, [r1[0]] * 2, [i - .5] * 2)
        draw_line(r0, [r1[1]] * 2, [i - .5] * 2)

    for sp in sky.spectra:
        n = len(sp)
        flux1 = sp.flux * scale + sp.pixel_y - .5
        ax.plot(np.ones(n) * sp.pixel_x,
                sp.wavelength,
                flux1, color='k')

    # ax.set_aspect("equal")
    ax.set_xlabel("x (pixel)")
    ax.set_ylabel('wavelength ($\AA$)')  # ax.set_ylabel('wavelength ($\AA$)')
    ax.set_zlabel('y (pixel)')
    # ax.set_zlabel('?')
    # plt.show()


def _plot_colors(ax, sky, vrange, sqx=None, sqy=None, flag_scale=False, method=0):
    """
    Plots image on axis

    Arguments
      ax -- matplotlib axis
      sky -- DataCube instance
      vrange -- visible range
      sqx -- "place spectrum" x
      sqy -- "place spectrum" y

    Returns: matplotlib plot object representing square, or None
    """
    assert isinstance(sky, DataCube)
    im = sky.to_colors(vrange, flag_scale, method)
    ax.imshow(im, interpolation="nearest")
    ax.invert_yaxis()
    obj_square = None
    K = .5
    if sqx is not None:
        x0, x1, y0, y1 = sqx - K, sqx + K, sqy - K, sqy + K
        obj_square = ax.plot([x0, x1, x1, x0, x0], [y0, y0, y1, y1, y0],
                             c=_COLORS_SQ[0], ls='solid', lw=3, alpha=0.5, zorder=99999)
    ax.set_xlim([-K, sky.width - .5])
    ax.set_ylim([-K, sky.height - .5])
    return obj_square

#######################################################################################################################
#                ####################################################################################
#                               #####################################################
#                                           ##############################

class XFileDCube(XFileMainWindow):
    def __init__(self, parent=None, fileobj=None):
        XFileMainWindow.__init__(self, parent)

        def keep_ref(obj):
            self._refs.append(obj)
            return obj

        self.setWindowTitle(_get_window_title("Data Cube Editor"))


        # # Synchronized sequences
        _VVV = FileDCube.description
        self.tab_texts[0] =  "FileDCube editor (Alt+&1)"
        self.tabWidget.setTabText(0, self.tab_texts[0])
        self.save_as_texts[0] = "Save %s as..." % _VVV
        self.open_texts[0] = "Load %s" % _VVV
        self.clss[0] = FileDCube
        self.clsss[0] = (FileDCube, FileCCube)  # file types that can be opened
        self.wilds[0] = "*.fits"

        lv = keep_ref(QVBoxLayout(self.gotting))
        ce = self.ce = WFileSky(self)
        lv.addWidget(ce)
        ce.edited.connect(self.on_tab0_file_edited)
        self.editors[0] = ce

        # # # Loads default file by default ... SQN
        # if os.path.isfile(FileDCube.default_filename):
        #     f = FileDCube()
        #     f.load()
        #     self.ce.load(f)

        if fileobj is not None:
            self.load(fileobj)


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Interface

    def set_manager_form(self, x):
        assert isinstance(x, XRunnableManager)
        self._manager_form = x
        self._rm = x.rm

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Qt override

    def closeEvent(self, event):
        flag_exit, ff = True, []
        for ed, flag_changed in zip(self.editors, self.flags_changed):
            if ed and ed.f and flag_changed:
                ff.append(ed.f.description)

        if len(ff) > 0:
            s = "Unsaved changes\n  -"+("\n  -".join(ff))+"\n\nAre you sure you want to exit?"
            flag_exit = are_you_sure(True, event, self, "Unsaved changes", s)
        if flag_exit:
            plt.close("all")

    def keyPressEvent(self, evt):
        incr = 0
        if evt.modifiers() == Qt.ControlModifier:
            n = self.tabWidget.count()
            if evt.key() in [Qt.Key_PageUp, Qt.Key_Backtab]:
                incr = -1
            elif evt.key() in [Qt.Key_PageDown, Qt.Key_Tab]:
                incr = 1
            if incr != 0:
                new_index = self._get_tab_index() + incr
                if new_index < 0:
                    new_index = n-1
                elif new_index >= n:
                    new_index = 0
                self.tabWidget.setCurrentIndex(new_index)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Slots for Qt library signals

    # def on_show_rm(self):
    #     if self._manager_form:
    #         self._manager_form.show()
    #         self._manager_form.raise_()
    #         self._manager_form.activateWindow()


    def on_tab0_file_edited(self):
        self._on_edited()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Protected methods to be overriden or used by descendant classes

    def _on_edited(self):
        index = self._get_tab_index()
        self.flags_changed[index] = True
        self._update_tab_texts()

    def _filter_on_load(self, f):
        """Converts from FileCCube to FileDCube format, if necessary"""
        if isinstance(f, FileCCube):
            f1 = FileDCube()
            f1.dcube.from_compass_cube(f.ccube)
            f = f1
        return f

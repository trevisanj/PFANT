# todo plot "X" instead of line
# find line in editor by clicking
# rename molecule

__all__ = ["XFileAtoms"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QT # as NavigationToolbar2QT
import matplotlib.pyplot as plt
import numpy as np
from .a_XAtomLinesEditor import *
from ._guiaux import *
from .guimisc import *
import os.path
import webbrowser
import sys

NUM_PLOTS = len(ATOM_HEADERS)-1  # -1 because whe "lambda" does not have its plot

class XFileAtoms(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)

        self.f = None  # FileAtoms object
        self.flag_sort = False
        self.atom_index = None
        self.atom = None  # Atom instance
        self.flag_changed = False
        # Form with the table to edit the lines
        self.form_lines = None

        # Information about the plots
        self.marker_row = None  # points into current atom, self.atom
        self.plot_info = [PlotInfo() for i in range(NUM_PLOTS)]
        # keptself.set_flag_plot(ATOM_ATTR_NAMES.index("kiex")-1, True)
        self.set_flag_plot(ATOM_ATTR_NAMES.index("algf")-1, True)


        # ** "Atoms browser"

        # ** ** left of splitter
        self.labelAtoms = QLabel('Atoms list (Alt+&1)')
        a = self.listWidgetAtoms = QListWidget()
        a.currentRowChanged.connect(self.on_listWidgetAtoms_currentRowChanged)
        # a.setEditTriggers(QAbstractItemView.DoubleClicked)
        # a.setEditTriggers(QAbstractItemView.AllEditTriggers)
        a.setContextMenuPolicy(Qt.CustomContextMenu)

        self.labelAtoms.setBuddy(self.listWidgetAtoms)

        l = self.layoutMol = QVBoxLayout()
        l.setMargin(0)
        l.setSpacing(1)
        l.addWidget(self.labelAtoms)
        l.addWidget(self.listWidgetAtoms)

        a = self.widgetAtoms = QWidget()
        a.setLayout(self.layoutMol)


        # ** ** right of splitter

        # ** ** ** ** Plot widget

        # ** ** ** ** ** Toolbar above plot

        am = self.buttonSort = QPushButton("Sort wave (Alt+&W)")
        am.clicked.connect(self.on_buttonSort_clicked)
        am.setCheckable(True)
        am.setToolTip("Sort spectral lines in ascending order of wavelength")
        if self.flag_sort:
            am.setChecked(True)

        # adds checkable buttons sj, jj
        self.plot_buttons = bb = []
        for i in range(NUM_PLOTS):  # sj, jj etc
            s = ATOM_HEADERS[i+1]
            b = QPushButton("%s (Alt+&%d)" % (s, i+1))
            b.clicked.connect(self.on_button_plot_clicked)
            b.setCheckable(True)
            if self.flag_plot(i):
                b.setChecked(True)
            bb.append(b)

        a11 = self.buttonEditLines = QPushButton("Edit lines (Ctrl+3)")
        a11.clicked.connect(self.on_buttonEditLines_clicked)
        a2 = self.labelNumLines = QLabel("--")
        a3 = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)

        l0 = self.layoutPlotToolbar = QHBoxLayout()
        l0.addWidget(am)
        for b in bb:
            l0.addWidget(b)
        l0.addWidget(a11)
        l0.addWidget(a2)
        l0.addItem(a3)
        l0.setMargin(1)
        a = self.widgetPlotToolbar = QWidget()
        a.setLayout(l0)
        a.setFixedHeight(40)

        # ** ** ** ** ** ** Plot widget

        # http://stackoverflow.com/questions/12459811
        self.figure = plt.figure()
        self.canvas = FigureCanvas(self.figure)
        self.canvas.mpl_connect('button_press_event', self.on_plot_click)
        self.toolbar = NavigationToolbar2QT(self.canvas, self)
        layout = QVBoxLayout()
        layout.addWidget(self.toolbar)
        layout.addWidget(self.canvas)
        layout.setMargin(0)

        a = self.widgetPlot = QWidget()
        a.setLayout(layout)

        l1 = self.layoutPlot = QVBoxLayout()
        l1.addWidget(self.widgetPlotToolbar)
        l1.addWidget(self.widgetPlot)
        l1.setMargin(0)

        a = self.widgetPlot = QWidget()
        a.setLayout(l1)


        # ** splitter: (list of Atoms) | (Molecule tab widget)
        a = self.splitter = QSplitter(Qt.Horizontal)
        a.addWidget(self.widgetAtoms)
        a.addWidget(self.widgetPlot)
        a.setStretchFactor(0, 2)
        a.setStretchFactor(1, 10)


        # * # * # * # * # * # * # *
        # Now the menu bar

        # self.menubar = QMenuBar(self)
        # self.menubar.setGeometry(QRect(0, 0, 772, 18))
        #self.menubar.setObjectName(_fromUtf8("menubar"))
        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")
        self.act_save = ac = m.addAction("&Save")
        ac.setShortcut("Ctrl+S")
        ac.triggered.connect(self.on_save)
        self.act_save_as = ac = m.addAction("Save &as...")
        ac.setShortcut("Ctrl+Alt+S")
        ac.triggered.connect(self.on_save_as)
        m.addSeparator()
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

        m = self.menu_help = b.addMenu("&Help")
        ac = m.addAction("&Open help in browser")
        ac.setShortcut("F1")
        ac.triggered.connect(self.on_help)

        # * # * # * # * # * # * # *
        # Final adjustments

        self.splitter.setFont(MONO_FONT)
        self.setCentralWidget(self.splitter)
        self.setGeometry(0, 0, 800, 600)

    def on_help(self, _):
        base_dir = os.path.dirname(sys.argv[0])
        print "aaa", sys.argv[0]
        print "bbb", base_dir
        webbrowser.open_new(os.path.join(base_dir, "ated.html"))
        ShowMessage("Help file mled.html was opened in web browser.")

    def on_save(self, _):
        self.disable_save_actions()
        try:
            self.save()
        finally:
            self.enable_save_actions()

    def on_save_as(self, _):
        self.disable_save_actions()
        try:
            if self.f:
                new_filename = QFileDialog.getSaveFileName(self, "Save file", ".", ".dat")
                if new_filename:
                    self.save_as(new_filename)
        finally:
            self.enable_save_actions()

    def on_listWidgetAtoms_currentRowChanged(self, row):
        if row > -1:
            self.set_atom(row)

    def on_button_plot_clicked(self):
        # This may be triggered by any of the plot buttons
        button = self.sender()
        idx = self.plot_buttons.index(button)
        self.set_flag_plot(idx, button.isChecked())
        self.plot_lines()

    def on_buttonSort_clicked(self):
        self.flag_sort = self.buttonSort.isChecked()
        self.plot_lines()


    def on_listWidgetAtoms_doubleClicked(self):
        self.edit_mol()

    def eventFilter(self, source, event):
        if event.type() == QEvent.KeyPress:
            if event.key() == Qt.Key_Return:
                if source == self.listWidgetAtoms:
                    self.edit_mol()
                    return True
        return False

    def closeEvent(self, event):
        if self.flag_changed:
            # http://straightedgelinux.com/blog/python/html/pyqtxt.html
            r = QMessageBox.question(self,
                        "About to exit",
                        "File \"%s\" has unsaved changes. Save now?" % self.f.filename,
                        QMessageBox.Yes|QMessageBox.No|
                        QMessageBox.Cancel)
            if r == QMessageBox.Cancel:
                event.ignore()
            elif r == QMessageBox.No:
                pass
            elif r == QMessageBox.Yes:
                try:
                    self.save()
                except:
                    # In case of error saving file, will not exit the program
                    event.ignore()
                    raise()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #



    def load(self, f):
        """Loads file into GUI."""
        assert isinstance(f, FileAtoms)

        self.f = f

        for m in f.atoms:
            assert isinstance(m, Atom)
            item = QListWidgetItem(self.get_atom_string(m))
            # not going to allow editing yet item.setFlags(item.flags() | Qt.ItemIsEditable)
            self.listWidgetAtoms.addItem(item)

        if len(f.atoms) > 0:
            self.listWidgetAtoms.setCurrentRow(0)

        self.flag_changed = False
        self.update_window_title()

    def save(self):
        if self.f:
            self.f.save_as()
            self.flag_changed = False
            self.update_window_title()

    def save_as(self, filename):
        if self.f:
            self.f.save_as(filename)
            self.flag_changed = False
            self.update_window_title()

    def set_atom(self, i):
        self.atom_index = i
        m = self.atom = self.f.atoms[i]
        self.update_atom_info()
        self.plot_lines()
        self.set_editor_atom()

    def plot_lines(self):
        self.clear_markers()
        o = self.atom
        if o is not None:
            self.figure.clear()

            n = sum([info.flag for info in self.plot_info])  # number of subplots (0, 1 or 2)
            # map to reuse plotting routine, contains what differs between each plot
            map_ = [(ATOM_HEADERS[i], o.__getattribute__(ATOM_ATTR_NAMES[i])) \
                    for i in range(1, len(ATOM_HEADERS))]

            # number of rows and columns for each different number of subplots
            SL = [(1, 1), (2, 1), (2, 2), (2, 2), (3, 2), (3, 2)]

            i_subplot = 1
            for i in range(len(map_)):
                y_label = map_[i][0]
                pi = self.plot_info[i]
                pi.y_vector = _y = map_[i][1]

                if pi.flag:
                    if not self.flag_sort:
                        x = o.lambda_
                        y = _y
                    else:
                        _x = np.array(o.lambda_)
                        _y = np.array(_y)
                        ii = np.argsort(_x)
                        x = _x[ii]
                        y = _y[ii]

                    format_BLB()

                    self.figure.add_subplot(SL[n-1][0], SL[n-1][1], i_subplot)
                    pi.axis = ax = self.figure.gca()
                    ax.clear()
                    ax.plot(x, y, 'k'+('' if len(x) > 1 else 'x'))
                    ax.set_xlabel('WaveLength ($\AA$)')
                    ax.set_ylabel(y_label)

                    # x-limits
                    xmin, xmax = min(x), max(x)
                    K = .02*(xmax-xmin)
                    ax.set_xlim([xmin-K, xmax+K])

                    # y-limits
                    ymin, ymax = min(y), max(y)
                    K = .02*(ymax-ymin)
                    ax.set_ylim([ymin-K, ymax+K])

                    i_subplot += 1

            if i_subplot > 1: plt.tight_layout()

            self.canvas.draw()
            self.draw_markers()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

    # def edit_molecule_name(self):
    #     """Edits current item from listWidgetMol."""
    #
    #     a = self.listWidgetMol
    #     item = a.currentItem()
    #     item.setFlags(item.flags() | Qt.ItemIsEditable)
    #     a.editItem(item)


    # def edit_file(self):
    # #     paramSpecs = [
    # #     ("titm", {"value": 5}),
    # #     ("number", {"value": 8, "description": "Size for advisor's TopBottomDetector4"}),
    # # ("SM_distance", {"value": 150}),
    # # ("SM_gainRiskRatio", {"value": 2}),
    # # ("SM_AF0", {"value": 0.02, "description": "Initial acceleration factor (AF)"}),
    # # ("SM_AFIncrement", {"value": 0.02, "description": "AF increment"})
    # # ]
    #     print "quer editar o file eh"


    def edit_points(self):
        print "quer editar os pontos eh"

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

    def update_atom_info(self):
        """Updates the "number of lines" label."""
        a = self.atom
        n = len(a)
        self.labelNumLines.setText('Number of lines: %d' % (n,))

    def update_window_title(self):
        self.setWindowTitle(self.f.filename+("" if not self.flag_changed else " (changed)"))

    def enable_save_actions(self):
        self.act_save.setEnabled(True)
        self.act_save_as.setEnabled(True)

    def disable_save_actions(self):
        self.act_save.setEnabled(False)
        self.act_save_as.setEnabled(False)


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Routines related to the lines editor

    def on_buttonEditLines_clicked(self):
        if self.form_lines is None:
            f = self.form_lines = XAtomLinesEditor(self)
            f.show()
            self.set_editor_atom()
        self.plot_lines()

    def AtomLinesEditor_closing(self):
        """Called by the molecular lines editor to notify that it is closing."""
        print "AHH QUECH FECHAR EH"
        self.form_lines = None
        self.marker_row = None

    def AtomLinesEditor_current_row_changed(self, currentRow):
        """Called by the molecular lines editor to notify that the current row has changed."""
        self.set_marker_row(currentRow)

    def AtomLinesEditor_cell_changed(self, row, column, value):
        """Called by the molecular lines editor to notify that a value has changed."""
        attr_name = ATOM_ATTR_NAMES[column]
        v = self.sol.__getattribute__(attr_name)
        if v[row] != value:
            v[row] = value
            self.flag_changed = True
            self.plot_lines()
            self.update_window_title()

    def set_editor_atom(self):
        if self.atom is not None and self.form_lines is not None:
            self.form_lines.set_atom(self.sol, "Atom: "+self.listWidgetAtom.currentItem().text())

    # Controlling plots and markers for current row

    def set_marker_row(self, i):
        self.clear_markers()
        self.marker_row = i
        self.draw_markers()

    def clear_markers(self):
        for o in self.plot_info:
            if o.mpl_obj:
                remove_line(o.mpl_obj)
                o.mpl_obj = None

    def draw_markers(self):
        self.clear_markers()
        if self.marker_row is not None and any([o.flag for o in self.plot_info]):
            i = self.marker_row
            lambda_ = self.atom.lambda_[i]
            for o in self.plot_info:
                if o.flag:
                    # http://stackoverflow.com/questions/22172565/matplotlib-make-plus-sign-thicker
                    o.mpl_obj = o.axis.plot([lambda_], [o.y_vector[i]], 'xr', mew=2, ms=10)
            self.canvas.draw()

    def flag_plot(self, idx):
        return self.plot_info[idx].flag

    def set_flag_plot(self, idx, x):
        self.plot_info[idx].flag = x

    def on_plot_click(self, event):
        lambda_ = event.xdata
        if lambda_ is not None and self.form_lines is not None:
            idx = index_nearest(self.atom.lambda_, lambda_)
            self.form_lines.set_row(idx)
            # self.set_marker_row(idx)

            # print 'button=%d, x=%d, y=%d, xdata=%f, ydata=%f'%(
            #     event.button, event.x, event.y, event.xdata, event.ydata)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

    @staticmethod
    def get_atom_string(a):
        assert isinstance(a, Atom)
        return "%-3s (%4d)" % (str(a).strip(), len(a))


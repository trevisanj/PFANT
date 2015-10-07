# todo plot "X" instead of line
# find line in editor by clicking
# rename molecule

__all__ = ["XFileMolecules"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QT # as NavigationToolbar2QT
import matplotlib.pyplot as plt
import numpy as np
from .a_XParametersEditor import *
from .a_XMolLinesEditor import *

class PlotInfo(object):
    def __init__(self):
        self.flag = True
        self.mpl_obj = None  # matplotlib Lines2D object
        self.axis = None  # matplotlib axis
        self.y_vector = None  # reference to sol.sj or jj

class XFileMolecules(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)

        self.f = None  # FileMolecules object
        self.flag_sort = False
        self.mol_index = None
        self.sol_index = None
        self.mol = None
        self.sol = None
        self.flag_changed = False
        self.form_lines = None

        # Information about the plots
        self.marker_row = None  # points into current set-of-lines, self.sol
        self.plot_info = [PlotInfo() for i in range(2)]
        self.set_flag_sj(True)
        self.set_flag_jj(True)

        MONO_FONT = QFont("not_a_font_name")
        MONO_FONT.setStyleHint(QFont.TypeWriter)

        # ** tab "General file info"
        a = self.plainTextEditFileInfo = QPlainTextEdit()
        a.setFont(MONO_FONT)


        # ** "Molecules browser"

        # ** ** left of splitter
        self.labelMol = QLabel('Molecules list (Alt+&1)')
        a = self.listWidgetMol = QListWidget()
        a.currentRowChanged.connect(self.on_listWidgetMol_currentRowChanged)
        # a.setEditTriggers(QAbstractItemView.DoubleClicked)
        # a.setEditTriggers(QAbstractItemView.AllEditTriggers)
        a.setContextMenuPolicy(Qt.CustomContextMenu)
        a.customContextMenuRequested.connect(self.on_listWidgetMol_customContextMenuRequested)
        a.itemDoubleClicked.connect(self.on_listWidgetMol_doubleClicked)
        a.installEventFilter(self)

        self.labelMol.setBuddy(self.listWidgetMol)

        l = self.layoutMol = QVBoxLayout()
        l.setMargin(0)
        l.setSpacing(1)
        l.addWidget(self.labelMol)
        l.addWidget(self.listWidgetMol)

        a = self.widgetMol = QWidget()
        a.setLayout(self.layoutMol)


        # ** ** right of splitter

        # ** ** ** tab "Molecule info"
        a = self.plainTextEditMolInfo = QPlainTextEdit()
        a.setFont(MONO_FONT)

        # ** ** ** tab "Molecular lines"

        # ** ** ** ** left

        #P = QSizePolicy(QSizePolicy.Fixed, QSizePolicy.Expanding)
        self.labelSol = QLabel('Sets of lines (Alt+&2)')

        a = self.listWidgetSol = QListWidget()
        a.setFont(MONO_FONT)
        #a.setFixedWidth(100)
        a.currentRowChanged.connect(self.on_listWidgetSol_currentRowChanged)
        a.setContextMenuPolicy(Qt.CustomContextMenu)
        a.customContextMenuRequested.connect(self.on_listWidgetSol_customContextMenuRequested)
        a.itemDoubleClicked.connect(self.on_listWidgetSol_doubleClicked)
        a.installEventFilter(self)

        self.labelSol.setBuddy(self.listWidgetSol)

        l = self.layoutSol = QVBoxLayout()
        l.setMargin(0)
        l.setSpacing(1)
        l.addWidget(self.labelSol)
        l.addWidget(self.listWidgetSol)

        a = self.widgetSol = QWidget()
        a.setLayout(self.layoutSol)

        # ** ** ** ** right

        # ** ** ** ** ** Toolbar above plot

        am = self.buttonSort = QPushButton("Sort wavelengths (Alt+&W)")
        am.clicked.connect(self.on_buttonSort_clicked)
        am.setCheckable(True)
        if self.flag_sort:
            am.setChecked(True)
        a0 = self.buttonSJ = QPushButton("SJ (Alt+&S)")
        a0.clicked.connect(self.on_buttonSJ_clicked)
        a0.setCheckable(True)
        if self.flag_sj():
            a0.setChecked(True)
        a1 = self.buttonJJ = QPushButton("JJ (Alt+&J)")
        a1.clicked.connect(self.on_buttonJJ_clicked)
        a1.setCheckable(True)
        if self.flag_jj():
            a0.setChecked(True)
        a11 = self.buttonEditLines = QPushButton("Edit &lines (Alt+L)")
        a11.clicked.connect(self.on_buttonEditLines_clicked)
        if self.flag_jj():
            a1.setChecked(True)
        a2 = self.labelNumLines = QLabel("--")
        a3 = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)

        l0 = self.layoutSolToolbar = QHBoxLayout()
        l0.addWidget(am)
        l0.addWidget(a0)
        l0.addWidget(a1)
        l0.addWidget(a11)
        l0.addWidget(a2)
        l0.addItem(a3)
        l0.setMargin(1)
        a = self.widgetSolToolbar = QWidget()
        a.setLayout(l0)
        a.setFixedHeight(40)

        # ** ** ** ** ** Set-of-lines info + Plot TABS

        # ** ** ** ** ** ** tab "Set-of-lines info"
        a = self.plainTextEditSolInfo = QPlainTextEdit()
        a.setFont(MONO_FONT)

        # ** ** ** ** ** ** Plot tab

        # http://stackoverflow.com/questions/12459811
        self.figure = plt.figure()
        self.canvas = FigureCanvas(self.figure)
        self.canvas.mpl_connect('button_press_event', self.on_plot_click)
        self.toolbar = NavigationToolbar2QT(self.canvas, self)
        layout = QVBoxLayout()
        layout.addWidget(self.toolbar)
        layout.addWidget(self.canvas)
        layout.setMargin(0)

        a = self.widgetSolPlot = QWidget()
        a.setLayout(layout)

        l1 = self.layoutSolPlot = QVBoxLayout()
        l1.addWidget(self.widgetSolToolbar)
        l1.addWidget(self.widgetSolPlot)
        l1.setMargin(0)

        a = self.widgetSolPlot = QWidget()
        a.setLayout(l1)


        # ** ** ** ** **  Set-of-lines info + Plot TABS
        a = self.tabWidgetSol = QTabWidget(self)
        a.addTab(self.plainTextEditSolInfo, "Set-of-lines Info (Alt+&N)")
        a.addTab(self.widgetSolPlot, "Set-of-lines plots (Alt+&P)")
        a.setCurrentIndex(1)
        a.setFont(MONO_FONT)

        # ** ** ** ** ** splitter: (list of set-of-lines) | (plot)
        a = self.splitterSol = QSplitter(Qt.Horizontal)
        a.addWidget(self.widgetSol)
        a.addWidget(self.tabWidgetSol)
        a.setStretchFactor(0, 2)
        a.setStretchFactor(1, 10)

        a = self.tabWidgetMol = QTabWidget()
        a.addTab(self.plainTextEditMolInfo, "Molecule info (Alt+&M)")
        a.addTab(self.splitterSol, "Sets of lines (Alt+&L)")
        a.setCurrentIndex(1)
        a.setFont(MONO_FONT)

        # ** splitter: (list of molecules) | (molecules tab widget)
        a = self.splitterMol = QSplitter(Qt.Horizontal)
        a.addWidget(self.widgetMol)
        a.addWidget(self.tabWidgetMol)
        a.setStretchFactor(0, 2)
        a.setStretchFactor(1, 10)

        # tab "File" (main tab widget
        a = self.tabWidgetFile = QTabWidget(self)
        a.addTab(self.plainTextEditFileInfo, "General File Info (Alt+&I)")
        a.addTab(self.splitterMol, "Molecules Browser (Alt+&B)")
        a.setCurrentIndex(1)
        a.setFont(MONO_FONT)


        # * # * # * # * # * # * # *
        # Now the menu bar

        # self.menubar = QMenuBar(self)
        # self.menubar.setGeometry(QRect(0, 0, 772, 18))
        #self.menubar.setObjectName(_fromUtf8("menubar"))
        b = self.menuBar()
        m = self.menu_file = b.addMenu("&File")
        ac = m.addAction("&Save")
        ac.setShortcut("Ctrl+S")
        ac.triggered.connect(self.on_save)
        ac = m.addAction("Save &as...")
        ac.setShortcut("Ctrl+Alt+S")
        ac.triggered.connect(self.on_save_as)
        m.addSeparator()
        ac = m.addAction("&Quit")
        ac.setShortcut("Ctrl+Q")
        ac.triggered.connect(self.close)

        # self.setMenuBar(self.menubar)
        # self.setupUi()


        # * # * # * # * # * # * # *
        # Final adjustments

        self.setCentralWidget(self.tabWidgetFile)
        self.setGeometry(0, 0, 800, 600)


    def on_save(self, _):
        self.save()

    def on_save_as(self, _):
        if self.f:
            new_filename = QFileDialog.getSaveFileName(self, "Save file", ".", ".dat")
            if new_filename:
                self.save_as(new_filename)
    def on_listWidgetMol_currentRowChanged(self, row):
        self.set_molecule(row)

    def on_listWidgetSol_currentRowChanged(self, row):
        self.set_sol(row)

    def on_buttonSJ_clicked(self):
        self.set_flag_sj(self.buttonSJ.isChecked())
        self.plot_lines()

    def on_buttonJJ_clicked(self):
        self.set_flag_jj(self.buttonJJ.isChecked())
        self.plot_lines()

    def on_buttonSort_clicked(self):
        self.flag_sort = self.buttonSort.isChecked()
        self.plot_lines()

    def on_listWidgetMol_customContextMenuRequested(self, position):
        menu = QMenu()
        a_edit = menu.addAction("&Edit")
        action = menu.exec_(self.listWidgetMol.mapToGlobal(position))
        if action == a_edit:
            self.edit_mol()

    def on_listWidgetSol_customContextMenuRequested(self, position):
        menu = QMenu()
        a_edit = menu.addAction("&Edit")
        action = menu.exec_(self.listWidgetSol.mapToGlobal(position))
        if action == a_edit:
            self.edit_sol()

    def on_listWidgetMol_doubleClicked(self):
        self.edit_mol()

    def on_listWidgetSol_doubleClicked(self):
        self.edit_sol()

    def eventFilter(self, source, event):
        if event.type() == QEvent.KeyPress:
            if event.key() == Qt.Key_Return:
                if source == self.listWidgetMol:
                    self.edit_mol()
                    return True
                if source == self.listWidgetSol:
                    self.edit_sol()
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

        #print "Flw"

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #



    def load(self, f):
        """Loads file into GUI."""
        assert isinstance(f, FileMolecules)

        self.f = f

        self.plainTextEditFileInfo.setPlainText(str(f))

        for m in f.molecules:
            assert isinstance(m, Molecule)
            item = QListWidgetItem(self.get_mol_string(m))
            # not going to allow editing yet item.setFlags(item.flags() | Qt.ItemIsEditable)
            self.listWidgetMol.addItem(item)

        if len(f.molecules) > 0:
            self.listWidgetMol.setCurrentRow(0)

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

    def set_molecule(self, i):
        self.mol_index = i
        m = self.mol = self.f.molecules[i]

        self.update_mol_info()

        w = self.listWidgetSol
        w.clear()

        for i, sol in enumerate(m.sol):
            item = QListWidgetItem(self.get_sol_string(i, sol))
            w.addItem(item)

        if len(m) > 0:
            w.setCurrentRow(0)

    def set_sol(self, j):
        self.sol_index = j
        self.sol = self.f.molecules[self.mol_index].sol[j]
        self.update_sol_info()
        self.plot_lines()
        self.set_editor_sol()

    def plot_lines(self):
        self.clear_markers()
        o = self.sol
        if o is not None:
            self.figure.clear()

            n = self.flag_sj() + self.flag_jj()  # number of subplots (0, 1 or 2)
            # map to reuse plotting routine, contains what differs between each plot
            map_ = [('SJ', o.sj), ('JJ', o.jj)]
            i_subplot = 1
            for i in range(len(map_)):
                y_label = map_[i][0]
                pi = self.plot_info[i]
                pi.y_vector = _y = map_[i][1]

                if pi.flag:
                    if not self.flag_sort:
                        x = o.lmbdam
                        y = _y
                    else:
                        _x = np.array(o.lmbdam)
                        _y = np.array(_y)
                        ii = np.argsort(_x)
                        x = _x[ii]
                        y = _y[ii]

                    format_BLB()

                    print "subplot", n, 1, i+1
                    self.figure.add_subplot(n, 1, i_subplot)
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

    def edit_mol(self):
        obj = self.mol
        if obj is None: return
        item = self.listWidgetMol.currentItem()
        r, form = _show_edit_form(obj,
            ["titulo", "fe", "do", "mm", "am", "bm", "ua", "ub", "te", "cro", "s"],
            item.text())
        flag_changed = False
        if r == QDialog.Accepted:
            kwargs = form.GetKwargs()
            for name, value in kwargs.iteritems():
                orig = obj.__getattribute__(name)
                if orig != value:
                    obj.__setattr__(name, value)
                    flag_changed = True
                    print "setting %s = %s" % (name, value)
        if flag_changed:
            self.flag_changed = True
            item.setText(self.get_mol_string(obj))
            #item.setStyleSheet("selected:active{background: yellow}")
            # item.setTextColor(QColor(255, 0, 0))
            #item.setBackgroundColor(QColor(255, 0, 0))
            self.update_mol_info()
            self.update_window_title()


    def edit_sol(self):
        obj = self.sol
        if obj is None: return
        item = self.listWidgetMol.currentItem()
        r, form = _show_edit_form(self.sol, ["qqv", "ggv", "bbv", "ddv", "fact"],
                                  item.text())
        flag_changed = False
        if r == QDialog.Accepted:
            kwargs = form.GetKwargs()
            for name, value in kwargs.iteritems():
                orig = obj.__getattribute__(name)
                if orig != value:
                    obj.__setattr__(name, value)
                    flag_changed = True
                    print "setting %s = %s" % (name, value)
        if flag_changed:
            self.flag_changed = True
            item.setText(self.get_mol_string(obj))
            # item.setTextColor(QColor(255, 0, 0))
            self.update_mol_info()
            self.update_window_title()

    def edit_points(self):
        print "quer editar os pontos eh"

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

    def update_mol_info(self):
        """Makes report for the current molecule."""
        m = self.mol
        s = str(m)
        # No need to repeat this information, already shown in listWidgetSol
        # s += '\n\n' \
        # 'Sets of lines\n' \
        #      '-------------\n' \
        #      ' # Number of lines\n'
        # for i in range(len(m)):
        #     s += '%2d %15d\n' % (i+1, len(m.lmbdam[i]))
        self.plainTextEditMolInfo.setPlainText(s)

    def update_sol_info(self):
        """Makes report for the current set-of-lines."""
        o = self.sol
        s = str(o)
        self.plainTextEditSolInfo.setPlainText(s)
        n = len(o)
        self.labelNumLines.setText('Number of lines: %d' % (n,))

    def update_window_title(self):
        self.setWindowTitle(self.f.filename+("" if not self.flag_changed else " (changed)"))



    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # Routines related to the lines editor

    def on_buttonEditLines_clicked(self):
        if self.form_lines is None:
            f = self.form_lines = XMolLinesEditor(self)
            f.show()
            self.set_editor_sol()
        self.plot_lines()

    def MolLinesEditor_closing(self):
        """Called by the molecular lines editor to notify that it is closing."""
        print "AHH QUECH FECHAR EH"
        self.form_lines = None

    def MolLinesEditor_current_row_changed(self, currentRow):
        """Called by the molecular lines editor to notify that the current row has changed."""
        self.set_marker_row(currentRow)

    def MolLinesEditor_cell_changed(self, row, column, value):
        """Called by the molecular lines editor to notify that a value has changed."""
        attr_name = ["lmbdam", "sj", "jj"][column]
        v = self.sol.__getattribute__(attr_name)
        if v[row] != value:
            v[row] = value
            self.flag_changed = True
            self.plot_lines()
            self.draw_markers()
            self.update_window_title()

    def set_editor_sol(self):
        if self.sol is not None and self.form_lines is not None:
            self.form_lines.set_sol(self.sol, "Set-of-lines: "+self.listWidgetSol.currentItem().text())

    # Controlling plots and markers for current row

    def set_marker_row(self, i):
        self.clear_markers()
        self.marker_row = i
        self.draw_markers()

    def clear_markers(self):
        for o in self.plot_info:
            if o.mpl_obj:
                _remove_line(o.mpl_obj)
                o.mpl_obj = None

    def draw_markers(self):
        self.clear_markers()
        if any([o.flag for o in self.plot_info]):
            i = self.marker_row
            lambda_ = self.sol.lmbdam[i]
            for o in self.plot_info:
                if o.flag:
                    # http://stackoverflow.com/questions/22172565/matplotlib-make-plus-sign-thicker
                    o.mpl_obj = o.axis.plot([lambda_], [o.y_vector[i]], 'xr', mew=2, ms=10)
                    #print "drawing", [lambda_], [o.y_vector[i]]
            self.canvas.draw()

    def flag_sj(self):
        return self.plot_info[0].flag

    def flag_jj(self):
        return self.plot_info[1].flag

    def set_flag_sj(self, x):
        self.plot_info[0].flag = x

    def set_flag_jj(self, x):
        self.plot_info[1].flag = x

    def on_plot_click(self, event):
        lambda_ = event.xdata
        if lambda_ is not None and self.form_lines is not None:
            idx = _index_nearest(self.sol.lmbdam, lambda_)
            self.form_lines.set_row(idx)
            # self.set_marker_row(idx)

            # print 'button=%d, x=%d, y=%d, xdata=%f, ydata=%f'%(
            #     event.button, event.x, event.y, event.xdata, event.ydata)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

    @staticmethod
    def get_mol_string(m):
        return m.titulo

    @staticmethod
    def get_sol_string(index, sol):
        return "%3d %7s" % (index+1, '(%d)' % len(sol))



def _index_nearest(array, value):
    """
    Finds index of nearest value in array.

    http://stackoverflow.com/questions/2566412/find-nearest-value-in-numpy-array
    """
    idx = (np.abs(array-value)).argmin()
    return idx

def _remove_line(line2D):
    """
    Removes line from matplotlib plot.
    # http://stackoverflow.com/questions/4981815/how-to-remove-lines-in-a-matplotlib-plot
    """
    l = line2D.pop(0)
    l.remove()
    del l


def _show_edit_form(obj, attrs, title):
    """Shows parameters editor modal form.

    Arguments:
      obj -- object to extract attribute values
      attrs -- list of attribute names
    """
    specs = []
    for name in attrs:
        specs.append((name, {"value": obj.__getattribute__(name)}))
    form = XParametersEditor(specs=specs, title=title)
    r = form.exec_()
    return r, form
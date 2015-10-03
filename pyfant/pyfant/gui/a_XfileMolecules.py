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
#from .a_XParametersEditor import *

class XFileMolecules(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)

        self.f = None  # FileMolecules object
        self.flag_sj = True
        self.flag_jj = False
        self.flag_sort = False
        self.mol_index = None
        self.sol_index = None
        self.mol = None
        self.sol = None

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
        if self.flag_sj:
            a0.setChecked(True)
        a1 = self.buttonJJ = QPushButton("JJ (Alt+&J)")
        a1.clicked.connect(self.on_buttonJJ_clicked)
        a1.setCheckable(True)
        if self.flag_jj:
            a1.setChecked(True)
        a2 = self.labelNumLines = QLabel("--")
        a3 = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)

        l0 = self.layoutSolToolbar = QHBoxLayout()
        l0.addWidget(am)
        l0.addWidget(a0)
        l0.addWidget(a1)
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

        self.setCentralWidget(self.tabWidgetFile)



    def load(self, f):
        """Loads file into GUI."""
        assert isinstance(f, FileMolecules)

        self.f = f
        self.setWindowTitle(f.filename)

        self.plainTextEditFileInfo.setPlainText(str(f))

        for m in f.molecules:
            assert isinstance(m, Molecule)
            item = QListWidgetItem(self.get_mol_string(m))
            # not going to allow editing yet item.setFlags(item.flags() | Qt.ItemIsEditable)
            self.listWidgetMol.addItem(item)

        if len(f.molecules) > 0:
            self.listWidgetMol.setCurrentRow(0)


    def on_listWidgetMol_currentRowChanged(self, row):
        self.set_molecule(row)

    def on_listWidgetSol_currentRowChanged(self, row):
        self.set_sol(row)

    def on_buttonSJ_clicked(self):
        self.flag_sj = self.buttonSJ.isChecked()
        self.plot_lines()


    def on_buttonJJ_clicked(self):
        self.flag_jj = self.buttonJJ.isChecked()
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

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

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

    def plot_lines(self):
        o = self.sol
        if o is not None:
            self.figure.clear()

            n = self.flag_sj + self.flag_jj  # number of subplots (0, 1 or 2)
            map_ = []  # map to reuse plotting routine, contains what differs between each plot
            i = 1
            if self.flag_sj:
                map_.append(('SJ', o.sj, n*100+10+i))
                i += 1
            if self.flag_jj:
                map_.append(('JJ', o.jj, n*100+10+i))
                i += 1

            for y_label, _y, subp in map_:

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

                self.figure.add_subplot(subp)
                ax = self.figure.gca()
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

                plt.tight_layout()

            self.canvas.draw()

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

    # def edit_molecule_name(self):
    #     """Edits current item from listWidgetMol."""
    #
    #     a = self.listWidgetMol
    #     item = a.currentItem()
    #     item.setFlags(item.flags() | Qt.ItemIsEditable)
    #     a.editItem(item)


    def edit_file(self):
    #     paramSpecs = [
    #     ("titm", {"value": 5}),
    #     ("number", {"value": 8, "description": "Size for advisor's TopBottomDetector4"}),
    # ("SM_distance", {"value": 150}),
    # ("SM_gainRiskRatio", {"value": 2}),
    # ("SM_AF0", {"value": 0.02, "description": "Initial acceleration factor (AF)"}),
    # ("SM_AFIncrement", {"value": 0.02, "description": "AF increment"})
    # ]
        print "quer editar o file eh"

    def edit_mol(self):
        if self.mol is None:
            return
        attrs = ["titulo", "fe", "do", "mm", "am", "bm", "ua", "ub", "te", "cro", "s"]
        specs = []
        obj = self.mol
        for name in attrs:
            specs.append((name, {"value": obj.__getattribute__(name)}))

        form = XParametersEditor(specs=specs)
        r = form.exec_()
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
            self.listWidgetMol.currentItem().setText(self.get_mol_string(obj))
            self.update_mol_info()




    def edit_sol(self):
        if self.sol is None:
            return
        print "quer editar o set-of-lines eh"

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



    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

    @staticmethod
    def get_mol_string(m):
        return m.titulo

    @staticmethod
    def get_sol_string(index, sol):
        return "%3d %7s" % (index+1, '(%d)' % len(sol))
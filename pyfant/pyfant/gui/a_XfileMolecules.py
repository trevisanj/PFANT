__all__ = ["XFileMolecules"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QT # as NavigationToolbar2QT
import matplotlib.pyplot as plt
import numpy as np

class XFileMolecules(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)

        self.f = None  # FileMolecules object
        self.is_sj = True
        self.flag_sort = False
        self.mol_index = None
        self.SOL_index = None

        MONO_FONT = QFont("not_a_font_name")
        MONO_FONT.setStyleHint(QFont.TypeWriter)

        self.setWindowIcon(QIcon('c:/python26_/repy26/icons/iqor1.ico'))


        # ** tab "General file info"
        a = self.plainTextEditFileInfo = QPlainTextEdit()
        a.setFont(MONO_FONT)


        # ** "Molecules browser"

        # ** ** left of splitter
        self.labelMol = QLabel('Molecules list (Alt+&1)')
        self.listWidgetMol = QListWidget()
        self.listWidgetMol.currentRowChanged.connect(self.on_listWidgetMol_currentRowChanged)
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
        self.labelSOL = QLabel('Sets of lines (Alt+&2)')

        a = self.listWidgetSOL = QListWidget()
        a.setFont(MONO_FONT)
        a.setFixedWidth(100)
        a.currentRowChanged.connect(self.on_listWidgetSOL_currentRowChanged)

        self.labelSOL.setBuddy(self.listWidgetSOL)

        l = self.layoutSOL = QVBoxLayout()
        l.setMargin(0)
        l.setSpacing(1)
        l.addWidget(self.labelSOL)
        l.addWidget(self.listWidgetSOL)

        a = self.widgetSOL = QWidget()
        a.setLayout(self.layoutSOL)


        # ** ** ** ** right

        # ** ** ** ** ** Toolbar above plot

        am = self.buttonSort = QPushButton("Sort wavelengths (Alt+&W)")
        am.clicked.connect(self.on_buttonSort_clicked)
        am.setCheckable(True)
        a0 = self.buttonSJ = QPushButton("SJ (Alt+&S)")
        a0.clicked.connect(self.on_buttonSJ_clicked)
        a1 = self.buttonJJ = QPushButton("JJ (Alt+&J)")
        a1.clicked.connect(self.on_buttonJJ_clicked)
        a2 = self.labelNumLines = QLabel("--")
        a3 = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)

        l0 = self.layoutSOLToolbar = QHBoxLayout()
        l0.addWidget(am)
        l0.addWidget(a0)
        l0.addWidget(a1)
        l0.addWidget(a2)
        l0.addItem(a3)
        l0.setMargin(1)
        a = self.widgetSOLToolbar = QWidget()
        a.setLayout(l0)
        a.setFixedHeight(40)

        # ** ** ** ** ** Plot

        # http://stackoverflow.com/questions/12459811
        self.figure = plt.figure()
        self.canvas = FigureCanvas(self.figure)
        self.toolbar = NavigationToolbar2QT(self.canvas, self)
        layout = QVBoxLayout()
        layout.addWidget(self.toolbar)
        layout.addWidget(self.canvas)
        layout.setMargin(0)

        a = self.widgetSOLPlot = QWidget()
        a.setLayout(layout)

        l1 = self.layoutSOLPlot = QVBoxLayout()
        l1.addWidget(self.widgetSOLToolbar)
        l1.addWidget(self.widgetSOLPlot)
        l1.setMargin(0)

        a = self.widgetSOLPlot = QWidget()
        a.setLayout(l1)


        # ** ** ** ** ** splitter: (list of set-of-lines) | (plot)
        a = self.splitterSOL = QSplitter(Qt.Horizontal)
        a.addWidget(self.widgetSOL)
        a.addWidget(self.widgetSOLPlot)
        a.setStretchFactor(0, 2)
        a.setStretchFactor(1, 10)

        a = self.tabWidgetMol = QTabWidget()
        a.addTab(self.plainTextEditMolInfo, "Molecule info (Alt+&M)")
        a.addTab(self.splitterSOL, "Sets of lines (Alt+&L)")
        a.setCurrentIndex(1)

        # ** splitter: (list of molecules) | (molecules tab widget)
        a = self.splitterMol = QSplitter(Qt.Horizontal)
        a.addWidget(self.widgetMol)
        a.addWidget(self.tabWidgetMol)
        a.setStretchFactor(0, 2)
        a.setStretchFactor(1, 10)

        # tab "File" (main tab widget
        a = self.tabWidgetFile = QTabWidget(self)
        a.addTab(self.plainTextEditFileInfo, "General File Info (Alt+&G)")
        a.addTab(self.splitterMol, "Molecules Browser (Alt+&B)")
        a.setCurrentIndex(1)

        self.setCentralWidget(self.tabWidgetFile)



    def load(self, f):
        """Loads file into GUI."""
        assert isinstance(f, FileMolecules)

        self.f = f
        self.setWindowTitle(f.filename)
        self.plainTextEditFileInfo.setPlainText(str(f))

        for m in f.molecules:
            assert isinstance(m, Molecule)
            item = QListWidgetItem(m.titulo)
            self.listWidgetMol.addItem(item)

        if len(f.molecules) > 0:
            self.listWidgetMol.setCurrentRow(0)


    def on_listWidgetMol_currentRowChanged(self, row):
        print "CALLED on_listWidgetMol_currentRowChanged"
        self.set_molecule(row)

    def on_listWidgetSOL_currentRowChanged(self, row):
        print "CALLED on_listWidgetSOL_currentRowChanged"
        self.set_SOL(row)

    def on_buttonSJ_clicked(self):
        self.is_sj = True
        self.plot_lines()


    def on_buttonJJ_clicked(self):
        self.is_sj = False
        self.plot_lines()

    def on_buttonSort_clicked(self):
        self.flag_sort = self.buttonSort.isChecked()
        self.plot_lines()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #

    def set_molecule(self, i):
        self.mol_index = i

        # makes kindda report for the current molecule
        m = self.f.molecules[i]
        s = str(m)

        # No need to repeat this information, already shown in listWidgetSOL
        # s += '\n\n' \
        #      'Sets of lines\n' \
        #      '-------------\n' \
        #      ' # Number of lines\n'
        # for i in range(len(m)):
        #     s += '%2d %15d\n' % (i+1, len(m.lmbdam[i]))

        self.plainTextEditMolInfo.setPlainText(s)

        w = self.listWidgetSOL
        w.clear()

        for i in range(len(m)):
            item = QListWidgetItem("%3d %7s" % (i+1, '(%d)' % len(m.lmbdam[i])))
            w.addItem(item)

        if len(m) > 0:
            w.setCurrentRow(0)

    def set_SOL(self, j):
        self.SOL_index = j
        m = self.f.molecules[self.mol_index]
        n = len(m.lmbdam[j])
        self.labelNumLines.setText('Number of lines: %d' % (n,))
        #self.labelNumLines.setText()

        self.plot_lines()

    def plot_lines(self):
        j = self.SOL_index
        if j is not None:
            m = self.f.molecules[self.mol_index]

            if not self.flag_sort:
                x = m.lmbdam[j]
                y = m.sj[j] if self.is_sj else m.jj[j]
            else:
                _x = np.array(m.lmbdam[j])
                _y = np.array(m.sj[j] if self.is_sj else m.jj[j])
                ii = np.argsort(_x)
                x = _x[ii]
                y = _y[ii]

            y_label = 'SJ' if self.is_sj else 'JJ'

            format_BLB()
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

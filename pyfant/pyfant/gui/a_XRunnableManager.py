__all__ = ["XRunnableManager"]


from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
import numpy as np
from .guiaux import *
from ..rm import RunnableManager
from ..misc import *
from threading import Lock
import time
from .a_XExplorer import *
import matplotlib.pyplot as plt

#todo clenaup
import sys

COLORS = [QColor(255, 0, 0),
          QColor(192, 0, 0),
          QColor(96, 96, 96),
          QColor(128, 128, 128)]
STATUS_COLUMN_WIDTH = 730
WINDOW_WIDTH = 700
WINDOW_HEIGHT = 700
# # Defaults for checkable buttons (True: checked)
FLAG_SHOW_STATUS = True
FLAG_SHOW_RUNNABLES = True

_logger = get_python_logger()

class XRunnableManager(QMainWindow):
    """
    Thread manager window.

    Allows to monitor running runnables.
    """

    def __init__(self, parent=None, rm=None):
        QMainWindow.__init__(self, parent)
        assert isinstance(rm, RunnableManager)
        self.rm = rm
        # Whether to close matplotlib plots when window is closed
        self.flag_close_mpl_plots_on_close = True
        # Whether to show "close window to continue" when finished
        self.flag_close_message = True
        self.__num_finished_shown = 0  # outdated version of self.rm.num_finished
        self.__lock_table = Lock()
        self.__lfcs = 0  # labelFinished color state
        self.__explorer_form = None


        # # Window design

        # ## Toolbar

        b0 = self.pushButtonStatus = QPushButton("Show &status")
        b0.clicked.connect(self.on_status)
        b0.setCheckable(True)
        b0.setChecked(FLAG_SHOW_STATUS)
        b0.setToolTip("Turn status panel on/off")
        b1 = self.pushButtonTable = QPushButton("Show &runnables")
        b1.clicked.connect(self.on_table)
        b1.setCheckable(True)
        b1.setChecked(FLAG_SHOW_RUNNABLES)
        b1.setToolTip("Turn status panel on/off")
        b2 = self.pushButtonPause = QPushButton("&Pause")
        b2.clicked.connect(self.on_pause)
        b2.setCheckable(True)
        b2.setToolTip("Pause/Resume")
        b3 = self.pushButtonCancel = QPushButton("&Cancel")
        b3.clicked.connect(self.on_cancel)
        b4 = self.pushButtonRetryFailed = QPushButton("Retry &failed")
        b4.clicked.connect(self.on_retry_failed)
        # todo + note: assuming that there is no interest in adding new tasks later
        l = self.labelEnd = QLabel()
        l.setVisible(False)

        s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)

        bb = [b0, b1, b2, b3, b4, l]
        l0 = self.layoutToolbar = QHBoxLayout()
        for b in bb:
            l0.addWidget(b)
        l0.addItem(s)

        a = self.widgetToolbar = QWidget()
        a.setLayout(l0)
        a.setFixedHeight(40)

        # ## Status widget

        # ### Left column
        w0 = self.labelStatusFinished = QLabel()
        w1 = self.labelStatusFailed = QLabel()
        w2 = self.labelStatusElla = QLabel()
        w3 = self.labelStatusTPR = QLabel()
        w4 = self.labelStatusTotal = QLabel()
        w5 = self.labelStatusRema = QLabel()
        ww = [("Num finished", w0),
              ("Num failed", w1),
              ("Time ellapsed", w2),
              ("Time total estimate", w4),
              ("Time remaining estimate", w5),
              ("Average time per runnable", w3)]
        l = self.layoutStatus0 = QFormLayout()
        for caption, w in ww:
            w.setStyleSheet("QLabel {font-weight: bold}")
            l.addRow(caption, w)

        # ### Right column
        w0 = self.checkBox_paused = QCheckBox()
        w1 = self.checkBox_cancelled = QCheckBox()
        w2 = self.checkBox_finished = QCheckBox()
        w3 = self.checkBox_failed = QCheckBox()
        w4 = self.checkBox_exited = QCheckBox()
        l = self.layoutStatus1 = QFormLayout()
        ww = [("Paused", w0),
              ("Cancelled", w1),
              ("Finished", w2),
              ("Failed", w3),
              ("Exited", w4)]
        for caption, w in ww:
            w.setChecked(False)
            w.setEnabled(False)
            l.addRow(caption, w)

        l = QHBoxLayout()
        l.addLayout(self.layoutStatus0)
        l.addLayout(self.layoutStatus1)
        x = self.widgetStatus = QWidget()
        x.setVisible(b0.isChecked())
        x.setLayout(l)


        # ## Table Widget

        a = self.tableWidget = QTableWidget()
        a.setSelectionMode(QAbstractItemView.SingleSelection)
        a.setAlternatingRowColors(True)
        a.setEditTriggers(QTableWidget.NoEditTriggers)
        a.setSelectionBehavior(QTableWidget.SelectRows)
        a.cellDoubleClicked.connect(self.on_tableWidget_cellDoubleClicked)
        # a.setFont(MONO_FONT)
        a.setVisible(self.pushButtonTable.isChecked())
        a.installEventFilter(self)

        # ## Mounts central widget

        l2 = self.layoutCentral = QVBoxLayout()
        ww = [self.widgetToolbar, self.widgetStatus, self.tableWidget]
        for w in ww:
            l2.addWidget(w)
        # l2.addWidget(self.widgetPlot)
        l2.setMargin(0)
        a = self.centralWidget = QWidget()
        a.setLayout(l2)
        a.setFont(MONO_FONT)
        self.setCentralWidget(self.centralWidget)



        # # # Status bar
        # self.label_n = QLabel()
        # self.label_t = QLabel()
        # sb = self.statusBar()
        # sb.insertWidget(0, self.label_n, 0)
        # sb.insertWidget(1, self.label_t, 0)


        # # Final adjustments
        self.setGeometry(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
        place_center(self)
        self.setWindowTitle("Runnables Manager")

        # # Non-visual stuff

        # ## Wiring of signals to trigger status update
        t = self.timerUpdate = QTimer()
        t.setInterval(1000)  # miliseconds
        signals = [t.timeout, self.rm.runnable_changed, self.rm.finished]
        self.changed_proxy = SignalProxy(signals,
         delay=0, rateLimit=1, slot=self.__update, flag_connect=False)
        self.rm.runnable_added.connect(self.__populate, Qt.QueuedConnection)

        # ## Timer to flick the finish indicator
        t = self.timerFinished = QTimer()
        t.setInterval(200)  # miliseconds
        t.timeout.connect(self.on_timerFinished_timeout)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def showEvent(self, event):
        self.__populate()
        self.changed_proxy.connect_all()
        # self.rm.runnable_added.connect(self.on_tm_thread_added, Qt.QueuedConnection)
        self.timerUpdate.start()

    def closeEvent(self, event):
        if not self.rm.flag_finished:
            r = QMessageBox.question(self, "Close window", "Not finished yet. Are you sure?",
             QMessageBox.Yes|QMessageBox.No, QMessageBox.Yes)
            if r != QMessageBox.Yes:
                event.ignore()
                return
        self.changed_proxy.disconnect_all()
        # self.rm.runnable_added.disconnect(self.on_tm_thread_added)
        self.timerUpdate.stop()
        self.timerFinished.stop()

        if self.flag_close_mpl_plots_on_close:
            plt.close("all")

    def eventFilter(self, obj, event):
        if obj == self.tableWidget:
            return check_return_space(event, self.on_tableWidget_cellDoubleClicked)
        return False

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Slots

    # def on_tm_thread_changed(self):
    #     print "on_tm_thread_changed"
    #     self._update()

    # def on_tm_thread_added(self):
    #     self._populate()

    def on_pause(self):
        if self.pushButtonPause.isChecked():
            self.rm.pause()
        else:
            self.rm.resume()

    def on_status(self):
        self.widgetStatus.setVisible(self.pushButtonStatus.isChecked())

    def on_table(self):
        self.tableWidget.setVisible(self.pushButtonTable.isChecked())

    def on_cancel(self):
        self.rm.cancel()

    def on_retry_failed(self):
        try:
            self.rm.retry_failed()
        except Exception as e:
            _logger.exception("Could not retry failed")
            ShowError(str(e))

    # def on_timer_timeout(self):
    #     print "on_timer_timeout"
    #     self._update()

    def on_timerFinished_timeout(self):
        self.__set_labelFinished_color(COLORS[self.__lfcs])
        self.__lfcs += 1
        if self.__lfcs == len(COLORS):
            self.__lfcs = 0

    def on_tableWidget_cellDoubleClicked(self, row=0, col=0):
        self.__explore_directory()


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Gear

    def __populate(self):
        """Clears and rebuilds table widget."""
        with self.__lock_table:
            a = self.tableWidget
            self.runnables = runnables = self.rm.get_runnables_copy()
            a.clear()
            self.__num_finished_shown = 0
            a.setAlternatingRowColors(True)
            a.setRowCount(len(runnables))
            a.setColumnCount(2)
            a.setHorizontalHeaderLabels(["Session directory", "Status"])
            for i, runnable in enumerate(runnables):
                title = runnable.session_dir
                if title is None:
                    title = '...'
                item = QTableWidgetItem(title)
                a.setItem(i, 0, item)
                status = str(runnable.get_status())
                item = QTableWidgetItem("?" if status is None else str(status))
                a.setItem(i, 1, item)
            a.resizeColumnsToContents()
            a.setColumnWidth(1, STATUS_COLUMN_WIDTH)
            self.__update_status()

    def __update(self):
        """Updates second column of table widget."""
        print "UPDATE UPDATE UPDATE UPDATE UPDATE "
        t = time.time()
        self.__update_status()
        self.__update_table()
        print "&&&&&&&&&&&&&&& time to update: %g" % (time.time()-t,)

    def __update_table(self):
        with self.__lock_table:
            a = self.tableWidget
            runnables = self.runnables
            nf = self.rm.num_finished  # grabs this before last table update,
                                       # so that it never skips a row update
            mt = self.rm.max_simultaneous
            for i in xrange(max(0, self.__num_finished_shown-mt+1),
                            min(len(runnables), self.rm.num_finished+mt)):
                runnable = runnables[i]

                item = a.item(i, 0)
                title = runnable.session_dir
                if title is None:
                    title = "..."
                item.setText(title)

                item = a.item(i, 1)
                status = runnable.get_status()
                item.setText("?" if status is None else str(status))

            if nf != self.__num_finished_shown:
                a.setCurrentCell(self.rm.num_finished+self.rm.max_simultaneous-1, 0)
                self.__num_finished_shown = nf

    def __update_status(self):
        """Updates everything except the table widget."""
        flag_todo = not (self.rm.flag_finished or self.rm.flag_exited)
        self.pushButtonCancel.setEnabled(flag_todo)
        self.pushButtonPause.setEnabled(flag_todo)
        if not flag_todo and self.flag_close_message:
            s = "Please close window to continue"
            self.labelEnd.setText(s)
            self.labelEnd.setVisible(True)
            self.timerFinished.start()
        if self.pushButtonStatus.isChecked():
            self.labelStatusFinished.setText("%d/%d" %
             (self.rm.num_finished, self.rm.num_runnables))
            self.labelStatusFailed.setText(str(self.rm.num_failed))
            ella, tot, rema = self.rm.get_times()
            self.labelStatusElla.setText(seconds2str(ella))
            self.labelStatusTPR.setText(seconds2str(self.rm.time_per_runnable))
            self.labelStatusTotal.setText(seconds2str(tot))
            self.labelStatusRema.setText(seconds2str(rema))

            self.checkBox_paused.setChecked(self.rm.flag_paused)
            self.checkBox_cancelled.setChecked(self.rm.flag_cancelled)
            self.checkBox_finished.setChecked(self.rm.flag_finished)
            self.checkBox_failed.setChecked(self.rm.flag_failed)
            self.checkBox_exited.setChecked(self.rm.flag_exited)

    def __set_labelFinished_color(self, color):
        self.labelEnd.setStyleSheet("QLabel {color: "+color.name()+";}")

    def __explore_directory(self):
        runnable = self.runnables[self.tableWidget.currentRow()]
        dir_ = runnable.session_dir
        if not self.__explorer_form:
            f = self.__explorer_form = XExplorer(self, dir_)
            f.flag_close_mpl_plots_on_close = False
        else:
            self.__explorer_form.set_dir(dir_)
        self.__explorer_form.show()
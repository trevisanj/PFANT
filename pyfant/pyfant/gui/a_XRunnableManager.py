__all__ = ["XRunnableManager"]


from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
import numpy as np
from ._guiaux import *
from .guimisc import *
from ..rm import RunnableManager
from ..misc import *
from threading import Lock
import time

#todo clenaup
import sys

COLORS = [QColor(255, 0, 0),
          QColor(192, 0, 0),
          QColor(96, 96, 96),
          QColor(128, 128, 128)]
STATUS_COLUMN_WIDTH = 730
WINDOW_WIDTH = 700
WINDOW_HEIGHT = 700

class XRunnableManager(QMainWindow):
    """
    Thread manager window.

    Allows to monitor running runnables.
    """

    def __init__(self, rm):
        QMainWindow.__init__(self)
        assert isinstance(rm, RunnableManager)
        self.rm = rm
        self.num_finished = 0  # outdated version of self.rm.num_finished
        self.lock_table = Lock()
        self.lfcs = 0  # labelFinished color state


        # # Window design

        # ## Toolbar

        b0 = self.pushButtonStatus = QPushButton("Show &status")
        b0.clicked.connect(self.on_status)
        b0.setCheckable(True)
        b0.setChecked(True)
        b0.setToolTip("Turn status panel on/off")
        b1 = self.pushButtonTable = QPushButton("Show &runnables")
        b1.clicked.connect(self.on_table)
        b1.setCheckable(True)
        b1.setChecked(False)
        b1.setToolTip("Turn status panel on/off")
        b2 = self.pushButtonPause = QPushButton("&Pause")
        b2.clicked.connect(self.on_pause)
        b2.setCheckable(True)
        b2.setToolTip("Pause/Resume")
        b3 = self.pushButtonCancel = QPushButton("&Cancel")
        b3.clicked.connect(self.on_cancel)
        # todo + note: assuming that there is no interest in adding new tasks later
        l = self.labelEnd = QLabel()
        l.setVisible(False)

        s = self.spacer0 = QSpacerItem(0, 0, QSizePolicy.Expanding, QSizePolicy.Minimum)

        bb = [b0, b1, b2, b3, l]
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
        # a.setFont(MONO_FONT)
        a.setVisible(self.pushButtonTable.isChecked())

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
         delay=0, rateLimit=1, slot=self._update, flag_connect=False)

        # ## Timer to flick the finish indicator
        t = self.timerFinished = QTimer()
        t.setInterval(100)  # miliseconds
        t.timeout.connect(self.on_timerFinished_timeout)

    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Qt override

    def showEvent(self, event):
        self._populate()
        self.changed_proxy.connect_all()
        # self.rm.runnable_added.connect(self.on_tm_thread_added, Qt.QueuedConnection)
        self.timerUpdate.start()

    def closeEvent(self, event):
        self.changed_proxy.disconnect_all()
        # self.rm.runnable_added.disconnect(self.on_tm_thread_added)
        self.timerUpdate.stop()
        self.timerFinished.stop()

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

    # def on_timer_timeout(self):
    #     print "on_timer_timeout"
    #     self._update()

    def on_timerFinished_timeout(self):
        self._set_labelFinished_color(COLORS[self.lfcs])
        self.lfcs += 1
        if self.lfcs == len(COLORS):
            self.lfcs = 0


    # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * #
    # # Gear

    def _populate(self):
        """Clears and rebuilds table widget."""
        with self.lock_table:
            a = self.tableWidget
            self.runnables = runnables = self.rm.get_runnables_copy()
            a.clear()
            a.setAlternatingRowColors(True)
            a.setRowCount(len(runnables))
            a.setColumnCount(2)
            a.setHorizontalHeaderLabels(["Session directory", "Status"])
            for i, runnable in enumerate(runnables):
                title = runnable.conf.session_dir
                if title is None:
                    title = '...'
                item = QTableWidgetItem(title)
                a.setItem(i, 0, item)
                status = str(runnable.get_status())
                item = QTableWidgetItem("?" if status is None else str(status))
                a.setItem(i, 1, item)
            a.resizeColumnsToContents()
            a.setColumnWidth(1, STATUS_COLUMN_WIDTH)
            self._update_status()

    def _update(self):
        """Updates second column of table widget."""
        # print "UPDATE UPDATE UPDATE UPDATE UPDATE "
        # t = time.time()
        self._update_status()
        self._update_table()
        # print "&&&&&&&&&&&&&&& time to update: %g" % (time.time()-t,)

    def _update_table(self):
        with self.lock_table:
            a = self.tableWidget
            runnables = self.runnables
            nf = self.rm.num_finished  # grabs this before last table update,
                                       # so that it never skips a row update
            mt = self.rm.max_simultaneous
            for i in xrange(max(0, self.num_finished-mt+1),
                            min(len(runnables), self.rm.num_finished+mt)):
                runnable = runnables[i]

                item = a.item(i, 0)
                title = runnable.conf.session_dir
                if title is None:
                    title = "..."
                item.setText(title)

                item = a.item(i, 1)
                status = runnable.get_status()
                item.setText("?" if status is None else str(status))

            if nf != self.num_finished:
                a.setCurrentCell(self.rm.num_finished+self.rm.max_simultaneous-1, 0)
                self.num_finished = nf

    def _update_status(self):
        """Updates everything except the table widget."""
        flag_todo = not (self.rm.flag_finished or self.rm.flag_exited)
        self.pushButtonCancel.setEnabled(flag_todo)
        self.pushButtonPause.setEnabled(flag_todo)
        if not flag_todo:
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

    def _set_labelFinished_color(self, color):
        self.labelEnd.setStyleSheet("QLabel {color: "+color.name()+";}")
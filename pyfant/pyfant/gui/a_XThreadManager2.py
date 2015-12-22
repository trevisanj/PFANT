__all__ = ["XThreadManager2"]

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from pyfant import *
import numpy as np
from ._guiaux import *
from .guimisc import *
from ..tm2 import ThreadManager2
from ..misc import SignalProxy
from threading import Lock
import time


class XThreadManager2(QMainWindow):
    """
    Thread manager window.

    Allows to monitor running threads.
    """

    def __init__(self, tm):
        QMainWindow.__init__(self)
        assert isinstance(tm, ThreadManager2)
        self.tm = tm
        self.threads = []
        self.num_finished = 0
        self._lock = Lock()

        a = self.tableWidget = QTableWidget()
        a.setSelectionMode(QAbstractItemView.SingleSelection)
        a.setAlternatingRowColors(True)
        # a.currentCellChanged.connect(self.on_tableWidget_currentCellChanged)
        # a.cellChanged.connect(self.on_tableWidget_cellChanged)
        a.setEditTriggers(QTableWidget.NoEditTriggers)
        a.setSelectionBehavior(QTableWidget.SelectRows)
        # a.setFont(MONO_FONT)
        # a.installEventFilter(self)
        self.setCentralWidget(a)

        # # Timer to update progress
        # Apart from responding to changed signals from the thread manager,
        # will update progress indicators automatically
        t = self.timer = QTimer()
        t.setInterval(1000)  # miliseconds
        # t.timeout.connect(self.on_timer_timeout)

        # # Status bar
        self.label_n = QLabel()
        self.label_t = QLabel()
        sb = self.statusBar()
        sb.insertWidget(0, self.label_n, 0)
        sb.insertWidget(1, self.label_t, 0)

        # # proxy to "changed" signal from thread manager
        self.changed_proxy = SignalProxy([t.timeout, self.tm.thread_changed],
         delay=0, rateLimit=1, slot=self._update)

        # # Positions window screen-centered
        rect = QApplication.desktop().screenGeometry()
        W, H = 640, 480
        self.setGeometry((rect.width()-W)/2, (rect.height()-H)/2, W, H)

    def showEvent(self, event):
        self._populate()
        self.changed_proxy.connect_all()
        self.tm.thread_added.connect(self.on_tm_thread_added, Qt.QueuedConnection)
        self.timer.start()

    def closeEvent(self, event):
        self.changed_proxy.disconnect_all()
        self.tm.thread_added.disconnect(self.on_tm_thread_added)
        self.timer.stop()

    # def on_tm_thread_changed(self):
    #     print "on_tm_thread_changed"
    #     self._update()

    def on_tm_thread_added(self):
        print "on_tm_thread_added"
        self._populate()

    # def on_timer_timeout(self):
    #     print "on_timer_timeout"
    #     self._update()

    def _populate(self):
        """Clears and rebuilds table widget."""
        with self._lock:
            a = self.tableWidget
            self.threads = threads = self.tm.get_threads_copy()
            a.clear()
            a.setAlternatingRowColors(True)
            a.setRowCount(len(threads))
            a.setColumnCount(2)
            a.setHorizontalHeaderLabels(["Thread name", "Progress info              "])
            for i, thread in enumerate(threads):
                item = QTableWidgetItem(thread.name)
                a.setItem(i, 0, item)
                item = QTableWidgetItem(str(thread.runnable.get_status()))
                a.setItem(i, 1, item)
            a.resizeColumnsToContents()
            self.__update_status()

    def _update(self):
        """Updates second column of table widget."""
        with self._lock:
            t = time.time()
            a = self.tableWidget
            threads = self.threads
            nf = self.tm.num_finished  # grabs this before last table update,
                                       # so that it never skips a row update
            mt = self.tm.max_threads
            for i in xrange(max(0, self.num_finished-mt+1),
                            min(len(threads), self.tm.num_finished+mt)):
                thread = threads[i]
                item = a.item(i, 1)
                item.setText(str(thread.runnable.get_status()))
            a.setCurrentCell(self.tm.num_finished+self.tm.num_active-1, 0)
            self.__update_status()
            self.num_finished = nf
            print "&&&&&&&&&&&&&&& time to update: %g" % (time.time()-t,)


    def __update_status(self):
        nf, nt = self.tm.num_finished, len(self.threads)
        ella = time.time()-self.tassadsasdadsasdasdasadsdasdasdasdsadsadasdsadsadsadsaella, tot, rema = self.get_times()ella, tot, rema = self.get_times()ella, tot, rema = self.get_times()m.time_started # ellapsed time
        if nf > 0:
            estt = seconds2str(ella/nf*nt)
        else:
            estt = "?"
        self.label_n.setText("Threads: %d/%d" % (nf, nt))
        self.label_t.setText("Time: %s / %s" % (seconds2str(ella), estt))

"""ThreadManager class."""
__all__ = ["ThreadManager"]

from .runnables import *
from .misc import random_name
import threading
import traceback
import time
from PyQt4.QtCore import QObject, pyqtSignal
import multiprocessing
import copy
import time

class _Runner(threading.Thread):
    """
    Thread that runs object of class Runnable.
    """

    def __init__(self, runnable, manager, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        assert isinstance(runnable, Runnable)
        assert isinstance(manager, ThreadManager)
        self.runnable = runnable
        self.manager = manager
        self.name = random_name()
        self.flag_finished = False
        self.flag_started = False
    # def __init__(self, runnable, *args, **kwargs):
    #     threading.Thread.__init__(self, *args, **kwargs)
    #     assert isinstance(runnable, Runnable)
    #     # assert isinstance(pool, RunnerPool)
    #     self.runnable = runnable
    #     # self.pool = pool
    #     self.name = random_name()

    def run(self):
        self.manager._activate(self)
        self.flag_started = True
        print 'Now running: %s' % str(self)
        try:
            self.runnable.run()
        except:
            traceback.print_exc()
        self.flag_finished = True
        self.manager._finish(self)


def _tm_print(s):
    """print for the thread manager (debugging)."""
    print "^^ %s ^^" % s


class ThreadManager(QObject, threading.Thread):
    """
    Thread takes care of other threads.

    Keyword arguments:
      max_threads=multiprocessing.cpu_count()
    """

    # Emitted when a new thread is added
    thread_added = pyqtSignal()

    # Emitted whenever the state of any thread has changed
    thread_changed = pyqtSignal()

    @property
    def num_finished(self):
        with self.__lock:
            return self.__num_finished

    @property
    def num_active(self):
        with self.__lock:
            return len(self.__active)

    @property
    def time_started(self):
        return self.__time_started

    @property
    def max_threads(self):
        return self.__max_threads

    def __init__(self, *args, **kwargs):
        self.__max_threads = kwargs.pop("max_threads", multiprocessing.cpu_count())
        QObject.__init__(self)
        threading.Thread.__init__(self, *args, **kwargs)
        self.__num_finished = 0
        self.__lock = threading.Lock()
        self.__threads = []
        self.__active = []
        self.__flag_exit = False
        self.__time_started = None

    def start(self):
        self.__time_started = time.time()
        threading.Thread.start(self)

    def get_threads_copy(self):
        """Returns a copy of self.__threads.

        The result is a copy of the self.__threads list, but the elements are
        the actual thread objects."""
        with self.__lock:
            return copy.copy(self.__threads)

    def add_runnable(self, runnable):
        assert isinstance(runnable, Runnable)

        t = _Runner(runnable, self)
        with self.__lock:
            self.__threads.append(t)
        self.thread_added.emit()

    def exit(self):
        with self.__lock:
            self.__flag_exit = True

    def has_finished(self):
        with self.__lock:
            return len(self.__threads) == self.__num_finished

    def run(self):
        _tm_print("Will run %d threads" % len(self.__threads))
        while True:
            # _tm_print("NUM: %d; ACTIVE: %d; FINISHED: %d" % (len(self.__threads), len(self.__active), self.__num_finished))
            if self.__flag_exit:
                break

            # starts threads
            with self.__lock:
                num_to_start = self.__max_threads-len(self.__active)
            if num_to_start > 0:
                for t in self.__threads:
                    if not t.is_alive() and not t.flag_started:
                        _tm_print("Starting %s" % t.name)
                        t.start()
                        _tm_print("Started %s" % t.name)
                        num_to_start -= 1
                    if num_to_start == 0:
                        break

            # time.sleep(0.001)
            # time.sleep(0.5)


        _tm_print("TM exited")

    def __str__(self):
        l, temp = [], []  # string list, temporary list
        with self.__lock:
            # loop to determine name width
            w = 0
            for i, t in enumerate(self.__threads):
                w = max(w, len(t.name))
                if t.is_alive():
                    s_prog = t.runnable.get_progress()
                else:
                    s_prog = 'finished' if t.flag_finished else '-'
                temp.append((i, t.name, s_prog))

            for i, name, progress in temp:
                l.append(("%03d %-"+str(w)+"s %s") % (i, name, progress))
            l.append("***finished: %d/%d" % (self.__num_finished, len(self.__threads)))
        return "\n".join(l)


    def _activate(self, runnable):
        with self.__lock:
            self.__active.append(runnable)
        self.thread_changed.emit()

    def _finish(self, runnable):
        with self.__lock:
            self.__active.remove(runnable)
            self.__num_finished += 1
        self.thread_changed.emit()

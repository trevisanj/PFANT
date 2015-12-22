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
        self.__max_threads = kwargs.pop("max_threads", None)
        if self.__max_threads is None: self.__max_threads = multiprocessing.cpu_count()
        QObject.__init__(self)
        threading.Thread.__init__(self, *args, **kwargs)
        # counts finished
        self.__num_finished = 0
        # points to next to start
        self.__ptrs = 0
        self.__lock = threading.Lock()
        # all threads, added in sequence
        self.__runnables = []
        # subset of __runnables
        self.__active = []
        # flag to exit as soon as possible
        self.__flag_exit = False
        # time the thread has started
        self.__time_started = None

    def start(self):
        self.__time_started = time.time()
        threading.Thread.start(self)

    def get_threads_copy(self):
        """Returns a copy of self.__runnables.

        The result is a copy of the self.__runnables list, but the elements are
        the actual thread objects."""
        with self.__lock:
            return copy.copy(self.__runnables)

    def add_runnable(self, runnable):
        assert isinstance(runnable, Runnable)

        t = _Runner(runnable, self)
        with self.__lock:
            self.__runnables.append(t)
        self.thread_added.emit()

    def exit(self):
        self.__flag_exit = True

    def has_finished(self):
        with self.__lock:
            return self.__num_finished == len(self.__runnables)
 
    def __str__(self):
        l, temp = [], []  # string list, temporary list
        with self.__lock:
            # loop to determine name width
            w = 0
            for i, t in enumerate(self.__runnables):
                w = max(w, len(t.name))
                if t.is_alive():
                    s_prog = t.runnable.get_status()
                else:
                    s_prog = 'finished' if t.flag_finished else '-'
                temp.append((i, t.name, s_prog))

            for i, name, progress in temp:
                l.append(("%03d %-"+str(w)+"s %s") % (i, name, progress))
            l.append("***finished: %d/%d" % (self.__num_finished, len(self.__runnables)))
        return "\n".join(l)

    def _activate(self, runnable):
        """This is called from the thread."""
        with self.__lock:
            self.__active.append(runnable)
            self.thread_changed.emit()

    def _finish(self, runnable):
        with self.__lock:
            # idx = self.__runnables.index(runnable)
            self.__active.remove(runnable)
            self.__num_finished += 1
        self.thread_changed.emit()
        
    def run(self):
        _tm_print("Will run %d threads" % len(self.__runnables))
        while True:
            #  _tm_print("NUM: %d; ACTIVE: %d; FINISHED: %d" % (len(self.__runnables), len(self.__active), self.__num_finished))
            if self.__flag_exit:
            
            
                # TODO what about cancelling what's running
            
                break

            # starts threads
            while True:
                with self.__lock:
                    if self.__ptrs >= len(self.__runnables):
                        break                        
                    if self.__max_threads-len(self.__active) <= 0:
                        break
                        
                    t = self.__runnables[self.__ptrs]
                    _tm_print("Started #%d: %s" % (self.__ptrs, t.name))
                    self.__ptrs += 1
                    t.start()

                T = 0.1
                time.sleep(T)  # Starts maximum 1 thread every T seconds, that is

        _tm_print("TM exited")
        

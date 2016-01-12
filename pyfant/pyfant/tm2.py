"""ThreadManager2 class."""
__all__ = ["ThreadManager2"]

from .runnables import *
from .misc import random_name, seconds2str
import threading
import traceback
import time
from PyQt4.QtCore import QObject, pyqtSignal
import multiprocessing
import copy
import time
from threading import Lock
import logging

class _Runner2(threading.Thread):
    """
    Thread that runs object of class Runnable.
    """

    def __init__(self, manager, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        assert isinstance(manager, ThreadManager2)
        self.runnable = None
        self.manager = manager
        self.name = self.__class__.__name__+random_name()
        self.flag_exit = False
        self.flag_idle = True
        self.lock = Lock()
        self.logger = None

        
    def set_runnable(self, x):
        assert self.flag_idle or x is None
        with self.lock:
            self.runnable = x
            self.flag_idle = x is None
    
    def exit(self):
        """Intention to exit asap."""
        self.flag_exit = True
        self.logger.info("ok to exit")
        
    def kill_runnable(self):
        """Attempt to kill whatever is running."""
        if self.runnable:
            self.runnable.kill()

    def run(self):
        self.logger = logging.getLogger(self.name)
        # this was leaving file open after finished add_file_handler(self.logger, "python.log")
        self.logger.info("\\o/ %s is alive \\o/" % (self.name))
        misses = 0
        flag_sleep = False
        while True:
            if self.flag_exit:
                break           
            with self.lock:
                if self.runnable:
                    self.flag_idle = False
                    try:
                        self.runnable.run()
                    except:
                        self.logger.exception("%s failed" % self.runnable.__class__.__name__)
                    self.manager._finish(self)  # logging
                    self.runnable = None
                    self.flag_idle = True
                else:
                    misses += 1
                    if misses >= 34:
                        flag_sleep = True
                        misses = 0                       
            if flag_sleep:
                T = 0.1
                time.sleep(T)
                flag_sleep = False
                


def _tm_print(s):
    """print for the thread manager2 (debugging)."""
    print "^^ %s ^^" % s


class ThreadManager2(QObject, threading.Thread):
    """
    Thread takes care of other threads.

    Keyword arguments:
      max_simultaneous=multiprocessing.cpu_count()
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
    def num_runnables(self):
        return len(self.__runnables)

    @property
    def time_started(self):
        return self.__time_started

    @property
    def max_simultaneous(self):
        return self.__max_simultaneous

    def __init__(self, *args, **kwargs):
        self.__max_simultaneous = kwargs.pop("max_simultaneous", None)
        if self.__max_simultaneous is None: self.__max_simultaneous = multiprocessing.cpu_count()
        QObject.__init__(self)
        threading.Thread.__init__(self, *args, **kwargs)
        # counts finished
        self.__num_finished = 0
        # points to next to start
        self.__ptrs = 0
        self.__lock = threading.Lock()
        # all runners
        self.__runners = []
        # all runnables
        self.__runnables = []
        # flag to exit as soon as possible
        self.__flag_exit = False
        # time the thread has started
        self.__time_started = None
        # time the last runnable has finished
        self.__time_finished = None
        # averate time to run each runnable
        self.time_per_runnable = 0
        
        for i in range(self.__max_simultaneous):
            t = _Runner2(self)
            self.__runners.append(t)

    def start(self):
        self.__time_started = time.time()
        threading.Thread.start(self)

    def get_threads_copy(self):
        """Returns a copy of self.__runnables.

        The result is a copy of the self.__runnables list, but the elements are
        the actual thread objects."""
        with self.__lock:
            return copy.copy(self.__runnables)
            
    def get_times(self):
        """Returns (ellapsed, total, remaining)."""
        inf = float("inf")
        ella, tot, rema = t = time.time()-self.__time_started, inf, inf
        nf = self.__num_finished
        if nf > 0:
            n = len(self.__runnables)
            tpr = self.time_per_runnable
            tf = tpr*nf  # estimated time until last recorded finished
            tot = tpr*n
            rema = tpr*(n-nf)-(ella-tf)
        return ella, tot, rema
            
    def add_runnable(self, runnable):
        assert isinstance(runnable, Runnable)
        with self.__lock:
            self.__runnables.append(runnable)
#        self.thread_added.emit()  # todo why emit this here??

    def exit(self):
        self.__flag_exit = True
        
    def kill_runnables(self):
        for t in self.__runners:
            if t.runnable and t.runnable.flag_running:
                t.kill_runnable()

    def has_finished(self):
        with self.__lock:
            return self.__num_finished == len(self.__runnables)

    def get_summary_report(self):
        """Returns list with information such as ellapsed time, remaining time etc."""

        with self.__lock:
            l = []
            l.append("***finished: %d/%d" % (self.__num_finished, len(self.__runnables)))
            if self.__time_started:
                ella, tot, rema = self.get_times()
                l.append("***time ellapsed: %s" % seconds2str(ella))
                if self.__num_finished > 0:
                    tpr = self.time_per_runnable
                    l.append("***time per runnable: %s" % seconds2str(tpr))
                    l.append("***time total estimate: %s" % seconds2str(tot))
                    l.append("***time remaining estimate: %s" % seconds2str(rema))
        return l


    def __str__(self):
        l, temp = [], []  # string list, temporary list
        with self.__lock:
            # loop to determine name width
            w = 0
            for i, t in enumerate(self.__runnables):
                w = max(w, len(t.name))
                s_prog = str(t.get_status())
                #else:
                #    s_prog = 'finished' if t.flag_finished else '-'
                temp.append((i, t.name, s_prog))

            for i, name, progress in temp:
                l.append(("%03d %-"+str(w)+"s %s") % (i, name, progress))
        l.extend(self.get_summary_report())

        return "\n".join(l)

#    def _activate(self, runnable):
#        """This is called from the thread."""
#        with self.__lock:
#            self.__active.append(runnable)
#            self.thread_changed.emit()

    def _finish(self, runnable):
        self.__num_finished += 1
        t = time.time()
        self.time_per_runnable = (t-self.__time_started)/self.__num_finished
        if self.__num_finished == len(self.__runnables):
            self.time_finished = t
        self.thread_changed.emit()
        
    def run(self):
        flag_sleep = False
        it = 0  # thread index
        _tm_print("Will run %d runnables" % len(self.__runnables))

        while True:
            #  _tm_print("NUM: %d; ACTIVE: %d; FINISHED: %d" % (len(self.__runnables), len(self.__active), self.__num_finished))
            if self.__flag_exit:
                for t in self.__runners:
                    if t.runnable and t.runnable.flag_running:
                        t.kill_runnable()
                    t.exit()
                break
                
            with self.__lock:
                if self.__ptrs >= len(self.__runnables):
                    flag_sleep = True
                else:                        
                    t = self.__runnables[self.__ptrs]
                    j = 0
                    # loop to find idle thread
                    while True:
                        if j >= self.__max_simultaneous:
                            # gave a full turn without finding idle thread
                            flag_sleep = True
                            break
                        r = self.__runners[it]
                        
                        it += 1
                        if it >= self.__max_simultaneous:
                            it = 0
                            
                        if r.flag_idle:
                            r.set_runnable(t)
                            _tm_print("Assigned #%d :%s to %s" % (self.__ptrs, t.name, r.name))
                            self.__ptrs += 1
                            
                            if not r.is_alive():
                                r.start()
                            
                            break
                            
                        j += 1
            
            if flag_sleep:
                T = 0.1
                time.sleep(T)  # Chilling out for a while
                flag_sleep = False
    
        _tm_print("TM exited")
        

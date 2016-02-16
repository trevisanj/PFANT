"""ThreadManager2 class."""
__all__ = ["RunnableManager"]

from .runnables import *
from .misc import random_name, seconds2str, get_python_logger
import threading
import traceback
import time
from PyQt4.QtCore import QObject, pyqtSignal
import multiprocessing
import copy
import time
from threading import Lock
import logging
import sys
from .misc import froze_it


class _Runner(threading.Thread):
    """
    Thread that runs object of class Runnable.
    """

    def __init__(self, manager, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        assert isinstance(manager, RunnableManager)
        self.runnable = None
        self.manager = manager
        self.name = self.__class__.__name__+random_name()
        self.flag_exit = False
        self.flag_idle = True
        self.lock = Lock()
        self.__logger = None

        
    def set_runnable(self, x):
        assert self.flag_idle or x is None
        with self.lock:
            self.runnable = x
            self.flag_idle = x is None
    
    def exit(self):
        """Intention to exit asap."""
        self.flag_exit = True
        self.__logger.debug("ok to exit")
        
    def kill_runnable(self):
        """Attempt to kill whatever is running."""
        if self.runnable:
            self.runnable.kill()

    def run(self):
        self.__logger = get_python_logger()
        # this was leaving file open after finished add_file_handler(self.__logger, "python.log")
        self.__logger.debug("\\o/ %s is alive \\o/" % (self.name))
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
                    except Exception as E:
                        # # todo cleanup
                        #
                        # if isinstance(E, IOError):
                        #   printOpenFiles()
                        #
                        # self.__logger.exception("%s failed" % self.runnable.__class__.__name__)
                        # print "EXITING SO THAT YOU CAN SEE THE ERROR"
                        # self.manager.exit()
                        # raise

                        self.__logger.exception("%s failed" % self.runnable.__class__.__name__)
                    self.manager._finish(self)
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


@froze_it
class RunnableManager(QObject, threading.Thread):
    """
    Thread takes care of other threads.

    Keyword arguments:
      max_simultaneous=multiprocessing.cpu_count()
    """

    # Emitted when a new thread is added
    runnable_added = pyqtSignal()

    # Emitted whenever the state of any thread has changed
    runnable_changed = pyqtSignal()

    # Emitted when there are no runnables left to run
    finished = pyqtSignal()

    @property
    def num_finished(self):
        with self.__lock:
            return self.__num_finished

    @property
    def time_finished(self):
        with self.__lock:
            return self.__time_finished

    @property
    def num_runnables(self):
        return len(self.__runnables)

    @property
    def num_failed(self):
        return self.__num_failed

    @property
    def time_started(self):
        return self.__time_started

    @property
    def max_simultaneous(self):
        return self.__max_simultaneous

    @property
    def flag_cancelled(self):
        return self.__flag_cancelled

    @property
    def flag_exited(self):
        return self.__time_started is not None and not self.is_alive()

    @property
    def flag_finished(self):
        with self.__lock:
            return self.__num_finished == len(self.__runnables)

    @property
    def flag_paused(self):
        return self.__flag_paused

    @property
    def flag_failed(self):
        return self.__num_failed > 0

    def __init__(self, *args, **kwargs):
        self.__max_simultaneous = kwargs.pop("max_simultaneous", None)
        if self.__max_simultaneous is None: self.__max_simultaneous = multiprocessing.cpu_count()
        QObject.__init__(self)
        threading.Thread.__init__(self, *args, **kwargs)
        self.__logger = get_python_logger()
        # counts finished
        self.__num_finished = 0
        # counts failed
        self.__num_failed = 0
        # points to next to start
        self.__ptrs = 0
        self.__lock = threading.Lock()
        # all runners
        self.__runners = []
        # all runnables
        self.__runnables = []
        # flag to exit as soon as possible
        self.__flag_exit = False
        # set to True if explicitly cancelled through calling cancel()
        self.__flag_cancelled = False
        # indicates whether the runnable manages is paused.
        # When paused, it will not delegate new tasks to the runners.
        self.__flag_paused = False

        # # Statistics
        # time the thread has started
        self.__time_started = None
        # time the last runnable has finished
        self.__time_finished = None
        # average time to run each runnable
        self.time_per_runnable = 0
        
        for i in range(self.__max_simultaneous):
            t = _Runner(self)
            self.__runners.append(t)

    def start(self):
        self.__time_started = time.time()
        threading.Thread.start(self)

    def cancel(self):
        self.__flag_cancelled = True
        self.__flag_exit = True

    def pause(self):
        """Pauses the delegation of runnables to runners.

        *Attention*: messes with time estimate."""
        self.__flag_paused = True

    def resume(self):
        self.__flag_paused = False

    def get_runnables_copy(self):
        """Returns a copy of self.__runnables.

        The result is a copy of the self.__runnables list, but the elements are
        the actual thread objects."""
        with self.__lock:
            return copy.copy(self.__runnables)
            
    def get_times(self):
        """Returns (ellapsed, total, remaining)."""
        inf = float("inf")
        ella, tot, rema = 0, 0, 0
        nr = self.num_runnables
        if self.__time_started is None:
            if nr > 0:
                tot, rema = inf, inf
        else:
            ella = time.time()-self.__time_started
            if nr > 0:
                nf = self.__num_finished
                if nf == 0:
                    tot, rema = inf, inf
                else:
                    tpr = self.time_per_runnable
                    tf = tpr*nf  # estimated time until last recorded finished
                    tot = tpr*nr
                    if not self.__flag_exit:
                        rema = tpr*(nr-nf)-(ella-tf)
                        if rema < 0:
                            rema = 0
                    else:
                        rema = inf
        return ella, tot, rema
            
    def add_runnables(self, runnables):
        with self.__lock:
            self.__runnables.extend(runnables)
        self.runnable_added.emit()

    def exit(self):
        self.__flag_exit = True
        
    def kill_runnables(self):
        for t in self.__runners:
            if t.runnable and t.runnable.flag_running:
                t.kill_runnable()

    def get_summary_report(self):
        """Returns list with information such as ellapsed time, remaining time etc."""

        with self.__lock:
            l = []
            l.append("***finished: %d/%d" % (self.__num_finished, len(self.__runnables)))
            if self.__num_failed > 0:
                l.append("***failed: %d" % (self.__num_failed))
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
                s_title = t.conf.session_dir
                if s_title is None:
                    s_title = '...'
                w = max(w, len(s_title))
                status = t.get_status()
                s_status = "?" if status is None else str(status)
                #else:
                #    s_prog = 'finished' if t.flag_finished else '-'
                temp.append((i, s_title, s_status))

            for i, name, progress in temp:
                l.append(("%03d %-"+str(w)+"s %s") % (i, name, progress))
        l.extend(self.get_summary_report())

        return "\n".join(l)

    def _finish(self, runner):
        """Called by a _Runner to inform that a runnable has finished.

        This is for monitoring purpose only and has no effect in the workings
        of the thread manager.
        """
        self.__num_finished += 1
        if runner.runnable.flag_error:
            self.__num_failed += 1

        t = time.time()
        self.time_per_runnable = (t-self.__time_started)/self.__num_finished
        if self.__num_finished == len(self.__runnables):
            self.__time_finished = t
        self.runnable_changed.emit()
        if self.flag_finished:
            self.finished.emit()
        
    def run(self):
        flag_sleep = False
        it = 0  # thread index
        self.__logger.debug("Will run %d runnables" % len(self.__runnables))

        while True:
            #  self.__logger.debug("NUM: %d; ACTIVE: %d; FINISHED: %d" % (len(self.__runnables), len(self.__active), self.__num_finished))
            if self.__flag_exit:
                for runner in self.__runners:
                    if runner.runnable and runner.runnable.flag_running:
                        runner.kill_runnable()
                    if runner.is_alive():
                        runner.exit()
                break

            if self.__flag_paused:
                flag_sleep = True
            else:
                with self.__lock:
                    if self.__ptrs >= len(self.__runnables):
                        flag_sleep = True
                    else:
                        runnable = self.__runnables[self.__ptrs]
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
                                r.set_runnable(runnable)
                                self.__logger.debug("Assigned #%d :%s to %s" %
                                 (self.__ptrs, runnable.name, r.name))
                                self.__ptrs += 1

                                if not r.is_alive():
                                    r.start()
                                break
                            j += 1
            
            if flag_sleep:
                T = 0.1
                time.sleep(T)  # Chilling out for a while
                flag_sleep = False
    
        self.__logger.debug("TM exited")

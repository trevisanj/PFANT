"""ThreadManager2 class."""
__all__ = ["RunnableManager"]

from .runnables import *
from .misc import random_name, seconds2str, get_python_logger, MyLock
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
import collections


def _tm_print(s):
    """print for the thread manager2 (debugging)."""
    print "^^ %s ^^" % s


class RunnableManagerError(Exception):
    pass


@froze_it
class RunnableManager(QObject, threading.Thread):
    """
    Thread takes care of other threads.

    Keyword arguments:
      max_simultaneous=multiprocessing.cpu_count()
      flag_auto_clean=False -- if set, will load result and remove the session
       directory as soon as a runnable has finished
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
        with self.__lock:
            return len(self.__runnables)

    @property
    def num_failed(self):
        with self.__lock:
            return self.__num_failed

    @property
    def time_started(self):
        with self.__lock:
            return self.__time_started

    @property
    def max_simultaneous(self):
        with self.__lock:
            return self.__max_simultaneous

    @property
    def flag_cancelled(self):
        with self.__lock:
            return self.__flag_cancelled

    @property
    def flag_exited(self):
        with self.__lock:
            return self.__time_started is not None and not self.is_alive()

    @property
    def flag_finished(self):
        with self.__lock:
            return self.__num_finished == len(self.__runnables)

    @property
    def flag_start_called(self):
        with self.__lock:
            return self.__time_started is not None

    @property
    def flag_paused(self):
        with self.__lock:
            return self.__flag_paused

    @property
    def flag_failed(self):
        with self.__lock:
            return self.__num_failed > 0

    @property
    def flag_auto_clean(self):
        with self.__lock:
            return self.__flag_auto_clean

    @property
    def flag_success(self):
        """Success is defined as nothing left to run and no fails."""
        with self.__lock:
            return self.__num_failed == 0 and len(self.__idxs_to_run) == 0

    @property
    def time_per_runnable(self):
        with self.__lock:
            return self.__time_per_runnable

    def __init__(self, *args, **kwargs):
        self.__max_simultaneous = kwargs.pop("max_simultaneous", None)
        self.__flag_auto_clean = kwargs.pop("flag_auto_clean", False)
        if self.__max_simultaneous is None: self.__max_simultaneous = multiprocessing.cpu_count()
        QObject.__init__(self)
        threading.Thread.__init__(self, *args, **kwargs)
        self.__logger = get_python_logger()
        # counts finished
        self.__num_finished = 0
        # counts failed
        self.__num_failed = 0
        # all runnables
        self.__runnables = []
        # FIFO stack containing indexes of __runnables to run
        self.__idxs_to_run = collections.deque()
        # flag to exit as soon as possible
        self.__flag_exit = False
        # set to True if explicitly cancelled through calling cancel()
        self.__flag_cancelled = False
        # indicates whether the runnable manages is paused.
        # When paused, it will not delegate new tasks to the runners.
        self.__flag_paused = False

        # Runner threads
        self.__runners = []

        # # Locks
        self.__lock = Lock()
        # self.__lock = MyLock("RM Lock", True)

        # # Statistics
        # time the thread has started
        self.__time_started = None
        # time the last runnable has finished
        self.__time_finished = None
        # average time to run each runnable
        self.__time_per_runnable = 0
        
        for i in range(self.__max_simultaneous):
            t = _Runner(self)
            self.__runners.append(t)

    def start(self):
        self.__time_started = time.time()
        threading.Thread.start(self)

    def cancel(self):
        with self.__lock:
            self.__flag_cancelled = True
            self.__flag_exit = True

    def pause(self):
        """Pauses the delegation of runnables to runners.

        *Attention*: messes with time estimate."""
        with self.__lock:
            self.__flag_paused = True

    def resume(self):
        with self.__lock:
            self.__flag_paused = False

    def get_runnables_copy(self):
        """Returns a copy of self.__runnables.

        The result is a copy of the self.__runnables list, but the elements are
        the actual thread objects."""
        with self.__lock:
            return copy.copy(self.__runnables)

    def get_times(self):
        """Returns (ellapsed, total, remaining)."""
        with self.__lock:
            return self.__unlocked_get_times()

    def add_runnables(self, runnables):
        with self.__lock:
            self.__unlocked_add_runnables(runnables)
        self.runnable_added.emit()

    def exit(self):
        self.__flag_exit = True
        
    def kill_runnables(self):
        with self.__lock:
            for runner in self.__runners:
                if runner.runnable and runner.runnable.flag_running:
                    runner.__unlocked_kill_runnable()

    def kill_runnable(self, runnable):
        with self.__lock:
            self.__unlocked_kill_runnable(runnable)

    def get_summary_report(self):
        """Returns list with information such as ellapsed time, remaining time etc."""

        with self.__lock:
            l = []
            l.append("***finished: %d/%d" % (self.__num_finished, len(self.__runnables)))
            if self.__num_failed > 0:
                l.append("***failed: %d" % (self.__num_failed))
            if self.__time_started:
                ella, tot, rema = self.__unlocked_get_times()
                l.append("***time ellapsed: %s" % seconds2str(ella))
                if self.__num_finished > 0:
                    tpr = self.__time_per_runnable
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
                s_title = t.conf.sid.dir
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

    def wait_until_finished(self):
        if not self.is_alive():
            raise RuntimeError("Runnable manager not running")
        while True:
            if self.flag_finished:
                # todo cleanup
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info("FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD")
                self.__logger.info(str(self))
                self.exit()
                break
            self.__logger.info("\n".join(self.get_summary_report()))
            time.sleep(1)


    def retry_failed(self):
        """Retries all failed runnables."""
        if not self.flag_finished:
            raise RuntimeError("Can only retry when finished!")
        with self.__lock:
            temp = self.__num_failed
            self.__num_failed = 0
            self.__num_finished -= temp
            to_add = [x for x in self.__runnables if x.flag_error]
            for runnable in to_add:
                print "gonna reset %s" % runnable
                runnable.reset()
                self.__runnables.remove(runnable)
            self.__unlocked_add_runnables(to_add)
        self.runnable_added.emit()

    def _finish(self, runner):
        """Called by a _Runner to inform that a runnable has finished.

        This is for monitoring purpose only and has no effect in the workings
        of the thread manager.
        """
        with self.__lock:
            if runner.runnable.flag_error:
                self.__num_failed += 1

            self.__num_finished += 1
            t = time.time()
            self.__time_per_runnable = (t-self.__time_started)/self.__num_finished
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
                    if len(self.__idxs_to_run) == 0:
                        # No new tasks for the moment
                        flag_sleep = True
                    else:
                        idx_to_run = self.__idxs_to_run[0]
                        runnable = self.__runnables[idx_to_run]
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
                                 (idx_to_run, runnable.name, r.name))
                                self.__idxs_to_run.popleft()

                                if not r.is_alive():
                                    r.start()
                                break
                            j += 1


            if flag_sleep:
                T = 0.1
                time.sleep(T)  # Chilling out for a while
                flag_sleep = False
    
        self.__logger.debug("TM exited")

    def __unlocked_add_runnables(self, runnables):
        n = len(self.__runnables)
        self.__runnables.extend(runnables)
        self.__idxs_to_run.extend(range(n, n + len(runnables)))


    def __unlocked_kill_runnable(self, runnable):
        flag_found = False
        with self.__lock:
            for runner in self.__runners:
                if runner.runnable == runnable and runner.runnable.flag_running:
                    flag_found = True
                    runner.kill_runnable()
                    break
        if not flag_found:
            raise RunnableManagerError("Runnable '%s' not running!" % runnable)

    def __unlocked_get_times(self):
        inf = float("inf")
        ella, tot, rema = 0, 0, 0
        nr = len(self.__runnables)
        if self.__time_started is None:
            if nr > 0:
                tot, rema = inf, inf
        else:
            ella = time.time() - self.__time_started
            if nr > 0:
                nf = self.__num_finished
                if nf == 0:
                    tot, rema = inf, inf
                else:
                    tpr = self.__time_per_runnable
                    tf = tpr * nf  # estimated time until last recorded finished
                    tot = tpr * nr
                    if not self.__flag_exit:
                        rema = tpr * (nr - nf) - (ella - tf)
                        if rema < 0:
                            rema = 0
                    else:
                        rema = inf
        return ella, tot, rema


@froze_it
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
                        if self.manager.flag_auto_clean and self.runnable.flag_success:
                            self.runnable.load_result()
                            self.runnable.conf.clean()

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


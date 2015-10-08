
__all__ = ["Runner", "ThreadManager"]

from .runnables import *
from ..misc import random_name
import threading
import traceback
import time


class Runner(threading.Thread):
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
        self.manager.makeActive(self)
        self.flag_started = True
        print 'Now running: %s' % str(self)
        try:
            self.runnable.run()
        except:
            traceback.print_exc()
        self.flag_finished = True
        self.manager.makeInactive(self)


def tm_print(s):
    """print for the thread manager (debugging)."""
    print "^^ %s ^^" % s


class ThreadManager(threading.Thread):
    """
    Thread takes care of other threads
    """

    def __init__(self, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        self.max_threads = 4
        self.lock = threading.Lock()
        self.threads = []

        self.active = []  # list of threads

        self.num_finished = 0

        self._flag_exit = False

    def add_runnable(self, runnable):
        assert isinstance(runnable, Runnable)

        t = Runner(runnable, self)
        with self.lock:
            self.threads.append(t)

    def makeActive(self, runnable):
        with self.lock:
            self.active.append(runnable)

    def makeInactive(self, runnable):
        with self.lock:
            self.active.remove(runnable)
            self.num_finished += 1

    def exit(self):
        with self.lock:
            self._flag_exit = True

    def has_finished(self):
        with self.lock:
            return len(self.threads) == self.num_finished

    def run(self):
        tm_print("Will run %d threads" % len(self.threads))
        while True:
            tm_print("NUM: %d; ACTIVE: %d; FINISHED: %d" % (len(self.threads), len(self.active), self.num_finished))
            if self._flag_exit:
                break

            # starts threads
            with self.lock:
                num_to_start = self.max_threads-len(self.active)
            if num_to_start > 0:
                for t in self.threads:
                    if not t.flag_started:
                        tm_print("Starting %s" % t.name)
                        t.start()
                        tm_print("Started %s" % t.name)
                        num_to_start -= 1
                    if num_to_start == 0:
                        break

            time.sleep(0.5)


        tm_print("TM exited")

    def __str__(self):
        l, temp = [], []  # string list, temporary list
        with self.lock:
            # loop to determine name width
            w = 0
            for i, t in enumerate(self.threads):
                w = max(w, len(t.name))
                if t.is_alive():
                    s_prog = t.runnable.get_progress()
                else:
                    s_prog = 'finished' if t.flag_finished else '-'
                temp.append((i, t.name, s_prog))

            for i, name, progress in temp:
                l.append(("%03d %-"+str(w)+"s %s") % (i, name, progress))
            l.append("***finished: %d/%d" % (self.num_finished, len(self.threads)))
        return "\n".join(l)


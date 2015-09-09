
__all__ = ["Runner", "RunnerPool"]

from .runnables import *
from ..misc import random_name
import threading
import traceback

class Runner(threading.Thread):
    """
    Thread that runs object of class Runnable.
    """

    def __init__(self, runnable, pool, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        assert isinstance(runnable, Runnable)
        assert isinstance(pool, RunnerPool)
        self.runnable = runnable
        self.pool = pool
        self.name = random_name()

    def run(self):
        self.pool.makeActive(self)
        print 'Now running: %s' % str(self)
        try:
            self.runnable.run()
        except:
            traceback.print_exc()
        self.pool.makeInactive(self)


class RunnerPool(object):
    """
    Pool of active Runner's

    References:
    http://stackoverflow.com/questions/4046986

    """
    def __init__(self):
        super(RunnerPool, self).__init__()
        self.active = []  # list of threads
        self.lock = threading.Lock()

    def __len__(self):
        with self.lock:
            return len(self.active)

    def makeActive(self, runnable):
        with self.lock:
            self.active.append(runnable)

    def makeInactive(self, runnable):
        with self.lock:
            self.active.remove(runnable)

    def __str__(self):
        l, temp = [], []  # string list, temporary list
        with self.lock:
            # loop to determine name width
            w = 0
            for i, t in enumerate(self.active):
                temp.append((i, t.name, t.runnable.get_progress()))
                w = max(w, len(t.name))

            for i, name, progress in temp:
                l.append(("%03d %-"+str(w)+"s %s") % (i, name, progress))
            l.append("*** total: %d" % len(self.active))
        return "\n".join(l)

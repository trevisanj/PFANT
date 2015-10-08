"""This module does this, that."""
__all__ = ["run_parallel"]


from pyfant import ThreadManager
import time


def run_parallel(rr, max_simultaneous=4, flag_console=True):
    """
    Arguments:
      rr -- list of Combo
      max_simultaneous -- maximum number of simultaneous processes.
    """
    # Adds to pool
    tm = ThreadManager()
    tm.max_threads = max_simultaneous
    tm.start()

    for p in rr:
        tm.add_runnable(p)

    # Primitive thread monitor
    while True:
        if flag_console:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
            s = raw_input("press enter or 'e' to exit")
            if s.lower() == "e":
                break
            print "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
            print tm
        else:
            if tm.has_finished():
                break
            print "waiting..."
            time.sleep(.5)

    tm.exit()

    print "FINISHED"


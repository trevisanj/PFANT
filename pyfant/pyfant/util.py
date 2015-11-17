"""
Miscellanea routines that depend on other pyfant modules.

Rule: no pyfant module can import util!!!

"""
__all__ = ["run_parallel", "show_menu"]
from pyfant.misc import *
from pyfant import ThreadManager, get_suitable_vis_classes
import time


# ##################################################################################################
# Terminal-based interface

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


def show_menu(obj):
    classes = get_suitable_vis_classes(obj)

    oo = [x.__name__ for x in classes]

    opt = menu("Please select", oo, cancel_label="Back", flag_allow_empty=True)
    if 1 <= opt <= len(oo):
        vis = classes[opt-1]()
        vis.use(obj)


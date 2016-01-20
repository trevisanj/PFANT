"""
Miscellanea routines that depend on other pyfant modules.

Rule: no pyfant module can import util!!!

"""
__all__ = ["run_parallel", "show_menu"]
from pyfant.misc import *
from pyfant import ThreadManager, get_suitable_vis_classes
import time
import traceback


# ##################################################################################################
# Terminal-based interface

def run_parallel(rr, max_simultaneous=None, flag_console=True):
    """
    Arguments:
      rr -- list of Runnable instances
      max_simultaneous -- maximum number of simultaneous processes.

    Returns: the ThreadManager object
    """
    # Adds to pool
    tm = ThreadManager(max_simultaneous=max_simultaneous)
    tm.start()

    for p in rr:
        tm.add_runnable(p)

    # Primitive thread monitor
    if flag_console:
        while True:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if tm.is_alive() else " DEAD")
            print tm
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if tm.is_alive() else " DEAD")
            s = raw_input("[Enter] -- [e]xit & keep in loop -- [q]uit -- [k]ill running >>> ")
            if s.lower() == "q":
                if tm.is_alive():
                    try:
                        tm.exit()
                    except:
                        traceback.print_exc()
                break
            if s.lower() == "e":
                try:
                    tm.exit()
                except:
                    traceback.print_exc()
            if s.lower() == "k":
                tm.kill_runnables()
    else:
        while True:
            if tm.has_finished():
                print "FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD"
                tm.exit()
                break
            print "\n".join(tm.get_summary_report())
            time.sleep(1)


    print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if tm.is_alive() else " DEAD")
    print "test-tm2 [SUPPOSED TO HAVE] EXITED"
    print tm

    return tm









# def run_parallel(rr, max_simultaneous=None, flag_console=True):
#     """
#     Arguments:
#       rr -- list of Combo
#       max_simultaneous -- maximum number of simultaneous processes.
#     """
#     # Adds to pool
#     tm = ThreadManager(max_threads=max_simultaneous)
#     tm.start()
#
#     for p in rr:
#         tm.add_runnable(p)
#
#     # Primitive thread monitor
#     while True:
#         if flag_console:
#             print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
#             s = raw_input("press enter or 'e' to exit")
#             if s.lower() == "e":
#                 break
#             print "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
#             print tm
#         else:
#             if tm.has_finished():
#                 break
#             print "waiting..."
#             time.sleep(.5)
#
#     tm.exit()
#
#     print "FINISHED"


def show_menu(obj):
    classes = get_suitable_vis_classes(obj)

    oo = [x.__name__ for x in classes]

    opt = menu("Please select", oo, cancel_label="Back", flag_allow_empty=True)
    if 1 <= opt <= len(oo):
        vis = classes[opt-1]()
        vis.use(obj)

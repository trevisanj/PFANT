"""
Miscellanea routines that depend on other pyfant modules.

Rule: no pyfant module can import util!!!

"""
__all__ = ["run_parallel"]
from pyfant.misc import *
from pyfant import RunnableManager, get_suitable_vis_classes
import time
import traceback


# ##################################################################################################
# Terminal-based interface

def run_parallel(rr, max_simultaneous=None, flag_console=True):
    """
    Arguments:
      rr -- list of Runnable instances
      max_simultaneous -- maximum number of simultaneous processes.

    Returns: the RunnableManager object
    """
    # Adds to pool
    rm = RunnableManager(max_simultaneous=max_simultaneous)
    rm.start()

    rm.add_runnables(rr)

    # Primitive thread monitor
    if flag_console:
        while True:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
            print rm
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
            s = raw_input("[Enter] -- [e]xit & keep in loop -- [q]uit -- [k]ill running >>> ")
            if s.lower() == "q":
                if rm.is_alive():
                    try:
                        rm.exit()
                    except:
                        traceback.print_exc()
                break
            if s.lower() == "e":
                try:
                    rm.exit()
                except:
                    traceback.print_exc()
            if s.lower() == "k":
                rm.kill_runnables()
    else:
        while True:
            if rm.flag_finished:
                print "FFFFFFFIIIIINNNNNNIIIISSSSHHHEEEEDDDDDD"
                rm.exit()
                break
            print "\n".join(rm.get_summary_report())
            time.sleep(1)


    print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
    print "test-tm2 [SUPPOSED TO HAVE] EXITED"
    print rm

    return rm









# def run_parallel(rr, max_simultaneous=None, flag_console=True):
#     """
#     Arguments:
#       rr -- list of Combo
#       max_simultaneous -- maximum number of simultaneous processes.
#     """
#     # Adds to pool
#     rm = RunnableManager(max_threads=max_simultaneous)
#     rm.start()
#
#     for p in rr:
#         rm.add_runnable(p)
#
#     # Primitive thread monitor
#     while True:
#         if flag_console:
#             print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
#             s = raw_input("press enter or 'e' to exit")
#             if s.lower() == "e":
#                 break
#             print "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
#             print rm
#         else:
#             if rm.flag_finished():
#                 break
#             print "waiting..."
#             time.sleep(.5)
#
#     rm.exit()
#
#     print "FINISHED"



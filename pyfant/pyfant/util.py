"""
Miscellanea routines that depend on other pyfant modules.

Rule: no pyfant module can import util!!!

"""
__all__ = ["run_parallel"]
# from pyfant.misc import *
from pyfant import *
import time
import traceback
import numpy as np
import copy
import os

# ##################################################################################################
# Terminal-based interface

def run_parallel(rr, max_simultaneous=None, flag_console=True, runnable_manager=None):
    """
    Arguments:
      rr -- list of Runnable instances
      max_simultaneous -- (optional, default is RunnableManager default)
       maximum number of simultaneous processes.
      runnable_manager=None -- (optional) if passed, will use passed;
       if not, will create new.

    Returns: the RunnableManager object
    """
    # Adds to pool
    if runnable_manager:
        assert isinstance(runnable_manager, RunnableManager)
        rm = runnable_manager
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
        rm.wait_until_finished()


    print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
    print "test-tm2 [SUPPOSED TO HAVE] EXITED"
    print rm

    return rm



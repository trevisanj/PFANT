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
    logger = get_python_logger()
    if runnable_manager:
        assert isinstance(runnable_manager, RunnableManager)
        rm = runnable_manager
    else:
        rm = RunnableManager(max_simultaneous=max_simultaneous)
    flag_had_to_start = False
    if not rm.flag_start_called:
        rm.start()
        flag_had_to_start = True

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
                        logger.exception("Error trying to exit")
                break
            if s.lower() == "e":
                try:
                    rm.exit()
                except:
                    logger.exception("Error trying to exit")
            if s.lower() == "k":
                rm.kill_runnables()
    else:
        rm.wait_until_finished()
        if flag_had_to_start:
            rm.exit()


    print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
    print "test-tm2 [SUPPOSED TO HAVE] EXITED"
    print rm

    return rm



"""
Miscellanea routines that depend on other pyfant modules.

Rule: no pyfant module can import util!!!

"""
__all__ = ["run_parallel",
           "load_with_classes", "load_any_file", "load_spectrum"]
# from pyfant.misc import *
from pyfant import *
import time
import traceback
import numpy as np
import copy
import os


###############################################################################
# # Routines to load file of unknown format

# List of classe representing all file formats either read or written by
# Fortran.
#
# note: leave FileAbonds to the end because it is too general
_classes = [FileAbsoru2, FileHmap, FileMain, FileDissoc, FileModBin,
            FileSpectrumNulbad,FileSpectrumPfant, FileToH, FileAbonds,
            FileSpectrumXY, FileAtoms, FileMolecules, FileSpectrumFits,
            FileOpa, FileModTxt, FileMoo]

_classes_sp = [FileModBin, FileSpectrumNulbad, FileSpectrumPfant, FileSpectrumXY,
               FileSpectrumFits]

def load_any_file(filename):
    """
    Attempts to load filename by trial-and-error using _classes as list of classes.
    """
    return load_with_classes(filename, _classes)

def load_spectrum(filename):
    """
    Attempts to load spectrum as one of the supported types.
    """
    return load_with_classes(filename, _classes_sp)

def load_with_classes(filename, classes):
    """Attempts to load file by trial-and-error using a given list of classes.

    Arguments:
      filename -- full path to file
      classes -- list of DataFile descendant classes

    Returns: DataFile object if loaded successfully, or None if not.

    Note: it will stop at the first successful load.

    Attention: this is not good if there is a bug in any of the file readers,
    because *all exceptions will be silenced!*
    """

    ok = False
    for class_ in classes:
        obj = class_()
        try:
            obj.load(filename)
            ok = True
        # cannot let IOError through because pyfits raises IOError!!
        # except IOError:
        #     raise
        except OSError:
            raise
        except Exception as e:  # (ValueError, NotImplementedError):
            # Note: for debugging, switch the below to True
            if False:
                get_python_logger().exception("Error trying with class \"%s\"" % \
                                              class_.__name__)
            pass
        if ok:
            break
    if ok:
        return obj
    return None


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



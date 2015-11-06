"""
Miscellanea routines that depend on other pyfant modules.
"""
__all__ = ["run_parallel", "cut_spectrum", "load_any_file", "show_menu", "console"]
from pyfant import *
import time
import numpy as np
import os


# ##################################################################################################
# Pipeline routines -- manipulation of spectra

def cut_spectrum(sp, lambda0, lambda1):
    """
    Cuts spectrum to interval [lambda0, lambda1]
    """
    assert isinstance(sp, Spectrum), "sp argument mus be a Spectrum!"
    idx0 = np.argmin(np.abs(sp.x-lambda0))
    idx1 = np.argmin(np.abs(sp.x-lambda1))
    ret = Spectrum()
    ret.x = sp.x[idx0:idx1]
    ret.y = sp.y[idx0:idx1]

    return ret


# ##################################################################################################
# I/O routines

# List of classe representing all file formats either read or written by
# Fortran.
#
# note: leave FileAbonds to the end because it is too general
_classes = [FileAbsoru2, FileHmap, FileMain, FileMod, FileSpectrumNulbad,
           FileSpectrumPfant, FileToH, FileAbonds]
def load_any_file(filename):
    """
    Attempts to load filename by trial-and-error.

    TODO Not 100% reliable: FileAbonds is known to read absoru2.dat without errors
    (however for a meaningless result).
    """
    ok = False
    for class_ in _classes:
        obj = class_()
        try:
            obj.load(filename)
            ok = True
        except (IOError, OSError):
            raise
        except Exception as e:  # (ValueError, NotImplementedError):
            #traceback.print_exc()
            #print "^^^^^^", e.__class__.__name__, "^^^^^^"
            pass

        if ok:
            break

    if ok:
        return obj
    return None


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


def console():
    """Allows navigation & file loading & visualization."""

    objs = []
    idx = 0
    flag_set_idx = False

    while True:
        if len(objs) == 0:
            print "\n*** No objects loaded ***"
        else:
            print "\n*** Loaded objects ***"
            print "**********************"
            mac, maf = 0, 0
            for obj in objs:
               mac, maf = max(mac, len(obj.__class__.__name__)), max(maf, len(obj.filename))
            fmt = "*** %-2s %2d * %-"+str(maf)+"s * %-"+str(mac)+"s"
            for i, obj in enumerate(objs):
                print fmt % ("->" if idx == i else "", i+1, obj.filename, obj.__class__.__name__)
        opt = menu("MAIN MENU", ["List directory", "Change directory", "Load file",
                                 "Select file", "Visualization options"], cancel_label="Exit", flag_allow_empty=False)
        if opt == 0:
            break
        elif opt == 1:
            _, dd, ff = next(os.walk('.'))
            dd.sort()
            dd.insert(0, '..')
            dd.insert(0, '.')
            ff.sort()
            for d in dd:
                print '[%s]' % d
            for f in ff:
                print f
        elif opt == 2:
            x = raw_input("cd ")
            if len(x) > 0:
                try:
                    os.chdir(x)
                except OSError, e:
                    print e
        elif opt == 3:
            x = raw_input("filename: ")
            if len(x) > 0:
                try:
                    temp = load_any_file(x)
                    if temp is not None:
                        objs.append(temp)
                    else:
                        print_error("File was not recognized")
                    if not flag_set_idx:
                        # If not explicitly set selected object, it will be always the tail object
                        idx = len(objs)-1

                except (OSError, IOError), e:
                    print_error(e)
        elif opt == 4:
            if len(objs) == 0:
                print_error("No file loaded")
            elif len(objs) == 1:
                print_error("Only one file loaded")
            else:
                x = raw_input("Enter i (1 <= i <= %d)" % len(objs))
                try:
                    i = int(x)
                    if 1 <= i <= len(objs):
                        idx = i-1
                    else:
                        print_error("Out of range")
                except:
                    print_error("Invalid integer")
        elif opt == 5:
            if len(objs) == 0:
                print_error("No file loaded")
            else:
                show_menu(objs[idx])

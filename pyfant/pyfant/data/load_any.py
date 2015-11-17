"""
Contains routine load_any_file()
"""

from . import *

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

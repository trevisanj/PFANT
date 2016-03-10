"""
Contains routine load_any_file()
"""
__all__ = ["load_with_classes", "load_any_file"]
from . import *

# List of classe representing all file formats either read or written by
# Fortran.
#
# note: leave FileAbonds to the end because it is too general
_classes = [FileAbsoru2, FileHmap, FileMain, FileDissoc, FileMod, FileSpectrumNulbad,
           FileSpectrumPfant, FileToH, FileAbonds, FileSpectrumXY, FileAtoms, FileMolecules]

def load_any_file(filename):
    """
    Attempts to load filename by trial-and-error using _classes as list of classes.
    """
    return load_with_classes(filename, _classes)

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
            # Note: this is not good for debugging

            #traceback.print_exc()
            #print "^^^^^^", e.__class__.__name__, "^^^^^^"
            pass
        if ok:
            break
    if ok:
        return obj
    return None

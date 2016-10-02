"""Miscellanea routines used by pyfant.

The rule here is *not* to import any pyfant module, except for errors. This is to
avoid cyclic imports.

Miscellanea routines that import pyfant modules should be put in util.py instead.
"""
__all__ = ["str_vector", "float_vector", "int_vector", "readline_strip",
 "multirow_str_vector", "ordinal_suffix", "path_to_default", "new_filename",
 "write_lf", "slugify", "adjust_atomic_symbol", "str2bool", "bool2str",
 "list2str", "chunk_string", "add_file_handler", "LogTwo", "SmartFormatter",
 "X", "HR", "log_noisy", "fmt_ascii_h1", "fmt_error", "print_error", "menu",
 "random_name", "format_BLB", "seconds2str", "SignalProxy", "get_python_logger",
 "AttrsPart", "froze_it", "format_progress", "symbols", "SYMBOLS",
 "is_text_file", "get_QApplication", "copy_default_file", "MyLock",
 "get_data_dir", "get_data_subdirs", "get_star_data_subdirs", "SESSION_PREFIX_SINGULAR",
 "SESSION_PREFIX_PLURAL", "MULTISESSION_PREFIX",
 "symlink", "print_skipped", "format_legend", "get_scripts", "get_fortrans",
 "get_pfant_dir", "rainbow_colors", "BSearch", "BSearchCeil", "BSearchFloor", "BSearchRound",
 "FindNotNaNBackwards", "load_with_classes", "MAGNITUDE_BASE", "Bands", "eval_fieldnames",
 "set_facecolor_white", "set_figure_size", "rubberband", "poly_baseline"
]

# # todo cleanup
# # This is just for debugging
# # http://stackoverflow.com/questions/2023608/check-what-files-are-open-in-python
# import __builtin__
# openfiles = set()
# oldfile = __builtin__.file
# class newfile(oldfile):
#     def __init__(self, *args):
#         self.x = args[0]
#         print "### OPENING %s ###" % str(self.x)
#         oldfile.__init__(self, *args)
#         openfiles.add(self)
#
#     def close(self):
#         print "### CLOSING %s ###" % str(self.x)
#         oldfile.close(self)
#         openfiles.remove(self)
# oldopen = __builtin__.open
# def newopen(*args):
#     return newfile(*args)
# __builtin__.file = newfile
# __builtin__.open = newopen
#
# def printOpenFiles():
#     print "### %d OPEN FILES: [%s]" % (len(openfiles), ", ".join(f.x for f in openfiles))
# __all__.append("printOpenFiles")


import os.path
import random
import logging
import inspect
import sys
from matplotlib import rc, pyplot as plt
from matplotlib.ticker import AutoMinorLocator
import re
from argparse import *
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import time
from functools import wraps
import numpy as np
import shutil
from threading import Lock, current_thread
import glob
import traceback
import imp
import textwrap
import platform
from bisect import bisect_left
import collections
from scipy.interpolate import interp1d

# Logger for internal use
_logger = logging.getLogger(__name__)
_logger.addHandler(logging.NullHandler())


###############################################################################
# # Constants to be shared among pyfant

SESSION_PREFIX_SINGULAR = 'session-'
SESSION_PREFIX_PLURAL = 'session-'
MULTISESSION_PREFIX = 'multi-session-'

# List of all atomic symbols
# obtained using elements.py from http://www.lfd.uci.edu/~gohlke/, then
# > import elements
# > [x.symbol for x in ELEMENTS]
symbols = [
'H', 'He', 'Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Ne', 'Na', 'Mg', 'Al', 'Si',
 'P', 'S', 'Cl', 'Ar', 'K', 'Ca', 'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co',
 'Ni', 'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y', 'Zr',
 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', 'Sb', 'Te', 'I',
 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy',
 'Ho', 'Er', 'Tm', 'Yb', 'Lu', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', 'Au',
 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', 'Pa', 'U',
 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Db',
 'Sg', 'Bh', 'Hs', 'Mt'
]
# List of all atomic symbols in UPPERCASE
SYMBOLS = [x.upper() for x in symbols]


###############################################################################
# # Classes

def froze_it(cls):
    """
    Decorator to prevent from creating attributes in the object ouside __init__().

    This decorator must be applied to the final class (doesn't work if a
    decorated class is inherited).

    Yoann's answer at http://stackoverflow.com/questions/3603502
    """
    cls._frozen = False

    def frozensetattr(self, key, value):
        if self._frozen and not hasattr(self, key):
            raise AttributeError("Attribute '{}' of class '{}' does not exist!"
                  .format(key, cls.__name__))
        else:
            object.__setattr__(self, key, value)

    def init_decorator(func):
        @wraps(func)
        def wrapper(self, *args, **kwargs):
            func(self, *args, **kwargs)
            self._frozen = True
        return wrapper

    cls.__setattr__ = frozensetattr
    cls.__init__ = init_decorator(cls.__init__)

    return cls

class AttrsPart(object):
    """
    Implements a new __str__() to print selected attributes.

    Note: when descending from this class, set the attrs class variable.
    """

    # for __str__()
    attrs = None
    # for __repr__()
    # Optional; if not set, will be overwritten with self.attrs at __init__()
    less_attrs = None

    def __init__(self):
        if self.less_attrs is None:
            self.less_attrs = self.attrs

    def __repr__(self):
        assert self.attrs is not None, "Forgot to set attrs class variable"

        s_format = "{:>%d} = {}" % max([len(x) for x in self.attrs])
        l = []
        for x in self.attrs:
            y = self.__getattribute__(x)
            if isinstance(y, list):
                # list gets special treatment
                l.append(("%s = [\n" % x)+
                 (" \n".join([z.one_liner_str() if isinstance(z, AttrsPart)
                             else repr(z) for z in y]))+"\n]")
            else:
                l.append(s_format.format(x, self.__getattribute__(x)))

        s = "\n".join(l)
        return s

    def one_liner_str(self):
        assert self.less_attrs is not None, "Forgot to set attrs class variable"
        s_format = "{}={}"
        s = "; ".join([s_format.format(x, self.__getattribute__(x)) for x in self.less_attrs])
        return s


# #################################################################################################
# # I/O routines

def str_vector(f):
    """Reads next line of file and makes it a vector of strings

    Note that each str.split() already strips each resulting string of any whitespaces."""
    return f.readline().split()


def float_vector(f):
    """Reads next line of file and makes it a vector of floats."""
    return [float(s) for s in str_vector(f)]


def int_vector(f):
    """Reads next line of file and makes it a vector of floats."""
    return [int(s) for s in str_vector(f)]


def readline_strip(f):
    """Reads next line of file and strips the newline."""
    return f.readline().strip('\n')


def multirow_str_vector(f, n, r=0):
    """
    Assembles a vector that spans several rows in a text file.

    Arguments:
      f -- file-like object
      n -- number of values expected
      r (optional) -- Index of last row read in file (to tell which file row in
                      case of error)

    Returns:
      (list-of-strings, number-of-rows-read-from-file)
    """

    so_far = 0
    n_rows = 0
    v = []
    while True:
        temp = str_vector(f)
        n_rows += 1
        n_now = len(temp)

        if n_now+so_far > n:
            _logger.warning(('Reading multi-row vector: '
                'row %d should have %d values (has %d)') %
                (r+n_rows, n-so_far, n_now))

            v.extend(temp[:n-so_far])
            so_far = n

        elif n_now+so_far <= n:
            so_far += n_now
            v.extend(temp)

        if so_far == n:
            break

    return v, n_rows


def path_to_default(fn):
  """Returns full path to default data file."""
  p = os.path.join(os.path.dirname(os.path.realpath(__file__)), "data", "default", fn)
  return p


def new_filename(prefix, extension=""):
  """Returns a file name that does not exist yet, e.g. prefix.0001.extension"""
  i = 0
  while True:
    ret = '%s.%04d.%s' % (prefix, i, extension) \
      if extension else '%s.%04d' % (prefix, i)
    if not os.path.exists(ret):
      break
    i += 1
    return ret


def write_lf(h, s):
  """Adds lf to end of string and writes it to file."""
  h.write(s+"\n")


def slugify(value, flagLower=True):
  """
  Converts to lowercase, removes non-alpha characters,
  and converts spaces to hyphens.

  Useful for making file names.

  Source: http://stackoverflow.com/questions/5574042/string-slugification-in-python
  """
  value = re.sub('[^\w\s.]', '', value).strip()
  if flagLower:
    value = value.lower()
  value = re.sub('[-\s]+', '-', value)
  return value


def copy_default_file(filename):
    """Copies file from pyfant/data/default directory to local directory."""
    fullpath = path_to_default(filename)
    shutil.copy(fullpath, ".")


def get_pfant_dir(*args):
    """
    Returns absolute path to the "PFANT" directory.

    Arguments passed will be incorporated into path
    """
    return os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', *args))


def get_data_dir():
    """returns absolute path to PFANT/data."""
    return get_pfant_dir("data")


def get_data_subdirs():
    """returns a list containing all subdirectories of PFANT/data (their names only, not full path)."""
    dd = glob.glob(os.path.join(get_data_dir(), "*"))
    ret = []
    for d in dd:
        if os.path.isdir(d):
            ret.append(os.path.basename(d))
    return ret


def get_star_data_subdirs():
    """Returns only subdirectories of PFANT/data that contain file main.dat."""
    dd = glob.glob(os.path.join(get_data_dir(), "*"))
    ret = []
    for d in dd:
        if os.path.isdir(d) and os.path.isfile(os.path.join(d, 'main.dat')):
            ret.append(os.path.basename(d))
    return ret


def symlink(source, link_name):
    """
    Creates symbolic link for either operating system.

    http://stackoverflow.com/questions/6260149/os-symlink-support-in-windows
    """
    os_symlink = getattr(os, "symlink", None)
    if callable(os_symlink):
        os_symlink(source, link_name)
    else:
        import ctypes
        csl = ctypes.windll.kernel32.CreateSymbolicLinkW
        csl.argtypes = (ctypes.c_wchar_p, ctypes.c_wchar_p, ctypes.c_uint32)
        csl.restype = ctypes.c_ubyte
        flags = 1 if os.path.isdir(source) else 0
        if csl(link_name, source, flags) == 0:
            raise ctypes.WinError()


def print_skipped(reason):
    """Standardized printing for when a file was skipped."""
    print "   ... SKIPPED (%s)." % reason


def crunch_dir(name, n=50):
    """Puts "..." in the middle of a directory name if lengh > n."""
    if len(name) > n + 3:
        name = "..." + name[-n:]
    return name


def get_scripts(flag_header=True, flag_markdown=False):
    """
    Generates listing of all Python scripts available as command-line programs.

    Arguments:
      flag_header=True -- whether or not to include a header at the beginning of
                          the text.
      flag_markdown=False -- if set, will generate MarkDown, otherwise plain text.

    Returns: (list of strings, maximum filename size)
      list of strings -- can be joined with a "\n"
      maximum filename size
    """

    ret = []
    base_dir = get_pfant_dir("pyfant", "scripts")
    # gets all scripts in script directory
    ff = glob.glob(os.path.join(base_dir, "*.py"))
    # discards scripts whose file name starts with a "_"
    ff = [f for f in ff if not os.path.basename(f).startswith("_")]
    ff.sort()
    py_len = max([len(os.path.split(f)[1]) for f in ff])
    if flag_markdown:
        mask = "%%-%ds | %%s" % py_len
        ret.append(mask % ("Script name", "Purpose"))
        ret.append( "-" * (py_len + 1) + "|" + "-" * 10)
        for f in ff:
            _, filename = os.path.split(f)
            try:
                script_ = imp.load_source('script_', f)  # module object
                descr = script_.__doc__.strip()
                descr = descr.split("\n")[0]  # first line of docstring
            except Exception as e:
                descr = "*%s*: %s" % (e.__class__.__name__, str(e))
            ret.append(mask % (filename, descr))
    else:
        if flag_header:
            s = "Scripts in " + crunch_dir(base_dir)
            ret.append(fmt_ascii_h1(s))
        for f in ff:
            _, filename = os.path.split(f)

            piece = filename + " " + ("." * (py_len - len(filename)))
            flag_error = False
            try:
                script_ = imp.load_source('script_', f)  # module object
            except Exception as e:
                flag_error = True
                ret.append("%s*%s*: %s" % (piece, e.__class__.__name__, str(e)))

            if not flag_error:
                descr = script_.__doc__.strip()
                descr = descr.split("\n")[0]  # first line of docstring
                ss = textwrap.wrap(descr, 79 - py_len - 1)

                ret.append(piece+" "+(ss[0] if ss and len(ss) > 0 else "no doc"))
                for i in range(1, len(ss)):
                    ret.append((" " * (py_len + 2))+ss[i])
    return ret, py_len


def get_fortrans(max_len=None):
    """
    Generates listing of files in the Fortran bin directory

    Arguments
      max_len -- (optional) if passed, will use it, otherwise will be the
                 maximum number of characters among all Fortran executable names.

    Returns: list of filenames
    """

    ret = []
    bindir = os.path.join(get_pfant_dir(), "fortran", "bin")
    ihpn = ["innewmarcs", "hydro2", "pfant", "nulbad"]
    if max_len is None:
        max_len = max(len(x) for x in ihpn)

    if platform.system() == "Windows":
        def has_it(x):
            return os.path.isfile(os.path.join(bindir, x+".exe"))
    else:
        def has_it(x):
            return os.path.isfile(os.path.join(bindir, x))
    for name in ihpn:
        status = "not found"
        if has_it(name):
            status = "found"
        piece = name + " " + ("." * (max_len-len(name)))
        ret.append(("%-"+str(max_len)+"s %s") % (piece, status))
    return ret


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


# #################################################################################################
# # Conversion routines

def adjust_atomic_symbol(x):
    """Makes sure atomic symbol is right-aligned and upper case (PFANT convention)."""
    assert isinstance(x, str)
    return "%2s" % (x.strip().upper())


def str2bool(s):
  """Understands "T"/"F" only (case-sensitive). To be used for file parsing."""
  if s == "T":
    return True
  elif s == "F":
    return False
  raise ValueError("I don't understand '%s' as a logical value" % s)


def bool2str(x):
  """Converts bool variable to either "T" or "F"."""
  assert isinstance(x, bool)
  return "T" if x else "F"


def list2str(l):
  """Converts list to string without the brackets."""
  return " ".join([str(x) for x in l])


def chunk_string(string, length):
  """
  Splits a string into fixed-length chunks.
  
  This function returns a generator, using a generator comprehension. The
  generator returns the string sliced, from 0 + a multiple of the length
  of the chunks, to the length of the chunks + a multiple of the length
  of the chunks.

  Reference: http://stackoverflow.com/questions/18854620
  """
  return (string[0+i:length+i] for i in range(0, len(string), length))


def ordinal_suffix(i):
    """Returns 'st', 'nd', or 'rd'."""
    return 'st' if i == 1 else 'nd' if i == 2 else 'rd' if i == 3 else 'th'


def seconds2str(seconds):
    """Returns string such as 1h 05m 55s."""
    
    if seconds < 0:
        return "%.3gs" % seconds
    elif np.isnan(seconds):
        return "NaN"
    elif np.isinf(seconds):
        return "Inf"
    
    m, s = divmod(seconds, 60)
    h, m = divmod(m, 60)
    if h >= 1:
        return "%dh %02dm %.3gs" % (h, m, s)
    elif m >= 1:
        return "%02dm %.3gs" % (m, s)
    else:
        return "%.3gs" % s

# #################################################################################################
# # Logging routines

# Set this to make the python logger to log to the console. Note: will have no
# effect if changed after the first call to get_python_logger()
flag_log_console = True
# Set this to make the python logger to log to a file named "python.log".
# Note: will have no effect if changed after the first call to get_python_logger()
flag_log_file = True
# Logging level for the python logger
logging_level = logging.INFO

_python_logger = None
_fmtr = logging.Formatter('[%(levelname)-8s] %(message)s')
def get_python_logger():
    """Returns logger to receive Python messages (as opposed to Fortran)."""
    global _python_logger
    if _python_logger is None:
        fn = "python.log"
        l = logging.Logger("python", level=logging_level)
        if flag_log_file:
            add_file_handler(l, fn)
        if flag_log_console:
            ch = logging.StreamHandler()
            ch.setFormatter(_fmtr)
            l.addHandler(ch)
        _python_logger = l

    return _python_logger


def add_file_handler(logger, logFilename=None):
    """Adds file handler to logger.

    File is opened in "a" mode (append)
    """
    assert isinstance(logger, logging.Logger)
    ch = logging.FileHandler(logFilename, "a")
    # ch.setFormatter(logging._defaultFormatter) # todo may change to have same formatter as last handler of logger
    ch.setFormatter(_fmtr)
    logger.addHandler(ch)


@froze_it
class LogTwo(object):
  """Logs messages to both stdout and file."""
  def __init__(self, filename):
    self.terminal = sys.stdout
    self.log = open(filename, "w")

  def write(self, message):
    self.terminal.write(message)
    self.log.write(message)

  def close(self):
      self.log.close()


class SmartFormatter(RawDescriptionHelpFormatter):
    """
    Help formatter that will show default option values and also respect
    newlines in description. Neither are done in default help formatter.
    """

    def _get_help_string(self, action):
        help = action.help
        if '%(default)' not in action.help:
            if action.default is not SUPPRESS:
                defaulting_nargs = [OPTIONAL, ZERO_OR_MORE]
                if action.option_strings or action.nargs in defaulting_nargs:
                    help += ' (default: %(default)s)'
        return help


        # # this is the RawTextHelpFormatter._split_lines
        # if text.startswith('R|'):
        #     return text[2:].splitlines()
        # return argparse.ArgumentDefaultsHelpFormatter._split_lines(self, text, width)


# character for boxing strings
X = "*"
HR = X*40
def log_noisy(logger, msg):
    """Prints string message with box around.

    This was designed to outstand in a long log dump.
    """

    xx = X*(len(msg)+4)
    logger.info("")
    logger.info(xx)
    logger.info("%s %s %s" % (X, msg, X))
    logger.info(xx)


def format_progress(i, n):
    """Returns string containing a progress bar, a percentage, etc."""
    if n == 0:
        fraction = 0
    else:
        fraction = float(i)/n
    LEN_BAR = 25
    num_plus = int(round(fraction*LEN_BAR))
    s_plus = '+'*num_plus
    s_point = '.'*(LEN_BAR-num_plus)
    return '[%s%s] %d/%d - %.1f%%' % (s_plus, s_point, i, n, fraction*100)


# #################################################################################################
# # Text interface routines - routines that are useful for building a text interface

def fmt_ascii_h1(s):
    """Returns string enclosed in an ASCII frame, with \n line separators. Does not end in \n."""
    n = len(s)
    return "+"+("-"*(n+2))+"+\n"+ \
           "| "+s+" |\n"+ \
           "+"+("-"*(n+2))+"+"

def fmt_error(s):
    """Standardized embellishment. Adds formatting to error message."""
    return "!! %s !!" % s

def print_error(s):
    """Prints string as error message."""
    print fmt_error(s)

def menu(title, options, cancel_label="Cancel", flag_allow_empty=False, flag_cancel=True, ch='.'):
  """Text menu.

  Arguments:
    title
    options -- sequence of strings
    cancel_label='Cancel' -- label to show at last "zero" option
    flag_allow_empty=0 -- Whether to allow empty option

  Returns:
    option -- an integer: None; 0-Back/Cancel/etc; 1, 2, ...

  Adapted from irootlab menu.m"""

  no_options, flag_ok, lt = len(options), 0, len(title)
  option = None  # result
  min_allowed = 0 if flag_cancel else 1  # minimum option value allowed (if option not empty)

  while True:
    print ""
    print "  "+ch*(lt+8)
    print "  "+ch*3+" "+title+" "+ch*3
    print "  "+ch*(lt+8)
    for i, s in enumerate(options):
      print "  %d - %s" % (i+1, s)
    if flag_cancel: print "  0 - << (*%s*)" % cancel_label
    s_option = raw_input('? ')

    n_try = 0
    while True:
      if n_try >= 10:
        print 'You are messing up!'
        break

      if len(s_option) == 0 and flag_allow_empty:
        flag_ok = True
        break

      try:
        option = int(s_option)
        if min_allowed <= option <= no_options:
          flag_ok = True
          break
      except ValueError:
        print "Invalid integer value!"

      print "Invalid option, range is [%d, %d]!" % (0, no_options)

      n_try += 1
      s_option = raw_input("? ")

    if flag_ok:
      break
  return option


# #################################################################################################
# Miscellanea of miscellanea

_forenames = ["Solomon", "John", "Loretta", "Stephen", "Harry", "Nancy", "Tracy", "Maggie", "Lafanda", "Napoleon", "Joe",
        "Ana", "Olivia", "Lucia", "Julien", "June", "Ada", "Blaise", "Platypus", "R2D2", "Obi-Wan",
        "Yoda", "Lancelot", "Shaun", "C3PO", "Luke", "George", "Martin", "Elvira", "Galileo", "Elizabeth",
        "Genie", "Mark", "Karl", "Henry-David", "Ludmilla", "Darth", "Bayden", "Plamen", "Margareth", "Javier",
        "Pouria", "Klen", "Lydiane", "Charlotte", "Edna", "Ricardo", "Francis", "Jemma", "Valon", "Imran", "Sian",
        "Hayat", "Taghreed", "Orla", "Michael", "Lourdes", "Weiyi", "Thomas", "Willian", "Miguel", "Rui",
        "Abdullah", "Angus", "Malcolm", "Donald", "Mickey", "Polona", "Rashmi", "Xiaowei", "Sasha", "Luciano",
        "Avinash", "Anthony", "Karen", "Matthew", "Tatiana", "Mariana", "Antonio", "Hamilton", "Pauderney",
        "BB-8"]
_surnames = ["Northupp", "Kanobi", "de Morgan", "de Vries", "van Halen", "McFly", "Wallace", "McLeod", "Skywalker", "Smith",
       "Silva", "da Silva", "Sexy", "Coupat", "Coupable", "Byron", "Lovelace", "Pascal", "Kareninski", "Dynamite",
       "Souza", "Ha", "Balboa", "Durden", "V.", "Li", "Manco", "Kelly", "Torquato", "Sampaio", "Bittencourt", "Parisi",
       "Oliveira", "Crap", "Copppercup", "Motherfucker", "Firehead", "Martin", "Papanicolau", "Galilei", "Stuart",
       "Bitch", "King", "Cleese", "Thoreau", "Twain", "Marx", "Yankovicz", "Vader", "Prado", "Teixeira", "Oliveira",
       "Nogueira", "Pereira", "Sant'anna", "Kerns", "Patel", "Ahmadzai", "Riding", "Llabjani", "Maus",
       "Liger", "Byrne", "Wood", "Angelov", "Andreu", "Sadeghi", "Gajjar", "Kara", "Wolstenholme", "Alghaith",
       "Young", "Scott", "Luz", "Copic", "Pucihar", "Zhou", "Dutta", "Baruah", "Singh", "Sauro", "do Nascimento",
       "Lee", "Trevisan", "Travisani", "Pereira", "Nandwani", "Moura", "Senna"]
_prefixes = ["Dr.", "Prof.", "Sir", "Ven."]
_suffixes = ["The 3rd", "Jr.", "Sobrinho", "Neto", "VIII", "XVI", "I", "II", "III", "IV"]
_PROB_PREF = 0.1
_PROB_SUFF = 0.1
def random_name():
  a = []

  # Prefix
  if random.random() < _PROB_PREF:
    a.append(_prefixes[random.randint(0, len(_prefixes)-1)])

  # Forename
  a.append(_forenames[random.randint(0, len(_forenames)-1)])

  # Surnames
  n = 2  # Number of surnames
  for i in range(n):
    a.append(_surnames[random.randint(0, len(_surnames)-1)])

  # Suffix
  if random.random() < _PROB_SUFF:
    a.append(_suffixes[random.randint(0, len(_suffixes)-1)])

  return " ".join(a)


# #################################################################################################
# # Matplotlib-related routines

def set_facecolor_white():
    rc("figure", facecolor="white")

def format_BLB():
    """Sets some formatting options in Matplotlib."""
    rc("figure", facecolor="white")
    rc('font', family = 'serif', size=14) #, serif = 'cmr10')
    rc('xtick', labelsize=14)
    rc('ytick', labelsize=14)
    rc('axes', linewidth=1)
    rc('xtick.major', size=4, width=1)
    rc('xtick.minor', size=2, width=1)
    rc('ytick.major', size=4, width=1)
    rc('ytick.minor', size=2, width=1)
    # plt.minorticks_on()
    #minorLocator = AutoMinorLocator()
    # plt.tick_params(which="both", width=2)
    # plt.tick_params(which="major", width=2)
    #rc('text', usetex=True)

def format_legend(leg):
    """Sets some formatting options in a matplotlib legend object."""
    # rect = leg.get_frame()
    # rect.set_linewidth(2.)

def set_figure_size(fig, width, height):
    """Sets MatPlotLib figure width and height in pixels

    Reference: https://github.com/matplotlib/matplotlib/issues/2305/
    """
    dpi = float(fig.get_dpi())
    fig.set_size_inches(float(width) / dpi, float(height) / dpi)


# #################################################################################################
# # PyQt-related routines

_qapp = None
def get_QApplication(args=[]):
    """Returns the QApplication instance, creating it is does not yet exist."""
    global _qapp
    if _qapp is None:
        _qapp = QApplication(args)
    return _qapp


class _ThreadsafeTimer(QObject):
    """
    Thread-safe replacement for QTimer.

    Original author: Luke Campagnola -- pyqtgraph package
    """

    timeout = pyqtSignal()
    sigTimerStopRequested = pyqtSignal()
    sigTimerStartRequested = pyqtSignal(object)

    def __init__(self):
        QObject.__init__(self)
        self.timer = QTimer()
        self.timer.timeout.connect(self.timerFinished)
        self.timer.moveToThread(QCoreApplication.instance().thread())
        self.moveToThread(QCoreApplication.instance().thread())
        self.sigTimerStopRequested.connect(self.stop, Qt.QueuedConnection)
        self.sigTimerStartRequested.connect(self.start, Qt.QueuedConnection)


    def start(self, timeout):
        isGuiThread = QThread.currentThread() == QCoreApplication.instance().thread()
        if isGuiThread:
            #print "start timer", self, "from gui thread"
            self.timer.start(timeout)
        else:
            #print "start timer", self, "from remote thread"
            self.sigTimerStartRequested.emit(timeout)

    def stop(self):
        isGuiThread = QThread.currentThread() == QCoreApplication.instance().thread()
        if isGuiThread:
            #print "stop timer", self, "from gui thread"
            self.timer.stop()
        else:
            #print "stop timer", self, "from remote thread"
            self.sigTimerStopRequested.emit()

    def timerFinished(self):
        self.timeout.emit()


class SignalProxy(QObject):
    """Object which collects rapid-fire signals and condenses them
    into a single signal or a rate-limited stream of signals.
    Used, for example, to prevent a SpinBox from generating multiple
    signals when the mouse wheel is rolled over it.

    Emits sigDelayed after input signals have stopped for a certain period of time.

    Note: *queued* connection is made to slot.

    Original author: Luke Campagnola -- pyqtgraph package

    Arguments:
      signals -- a list of bound signals or pyqtSignal instance
      delay=0.3 -- Time (in seconds) to wait for signals to stop before emitting
      slot -- Optional function to connect sigDelayed to.
      rateLimit=0 -- (signals/second) if greater than 0, this allows signals to
       stream out at a steady rate while they are being received.
      flag_connect=True -- whether or not to start with the connections already
       made. If False, the signals and slots can be connected by calling
       connect_all()
    """

    __sigDelayed = pyqtSignal(object)

    def __init__(self, signals, delay=0.3, rateLimit=0, slot=None,
                 flag_connect=True):
        QObject.__init__(self)
        # for signal in signals:
        #     self.__connect_signal(signal)
        self.__signals = signals
        self.__delay = delay
        self.__rateLimit = rateLimit
        self.__args = None
        self.__timer = _ThreadsafeTimer()
        self.__timer.timeout.connect(self.__flush)
        self.__disconnecting = False
        self.__slot = slot
        self.__lastFlushTime = None
        self.__lock = Lock()
        # State: connected/disconnected
        self.__connected = False
        if flag_connect:
            self.connect_all()
        # if slot is not None:
        #     self.__sigDelayed.connect(slot, Qt.QueuedConnection)

    def add_signal(self, signal):
        """Adds "input" signal to connected signals.
        Internally connects the signal to a control slot."""
        self.__signals.append(signal)
        if self.__connected:
            # Connects signal if the current state is "connected"
            self.__connect_signal(signal)

    def connect_all(self):
        """[Re-]connects all signals and slots.

        If already in "connected" state, ignores the call.
        """
        if self.__connected:
            return  # assert not self.__connected, "connect_all() already in \"connected\" state"
        with self.__lock:
            for signal in self.__signals:
                self.__connect_signal(signal)
            if self.__slot is not None:
                self.__sigDelayed.connect(self.__slot, Qt.QueuedConnection)
            self.__connected = True

    def disconnect_all(self):
        """Disconnects all signals and slots.

        If already in "disconnected" state, ignores the call.
        """
        if not self.__connected:
            return  # assert self.__connected, "disconnect_all() already in \"disconnected\" state"
        self.__disconnecting = True
        try:
            for signal in self.__signals:
                signal.disconnect(self.__signalReceived)
            if self.__slot is not None:
                self.__sigDelayed.disconnect(self.__slot)
            self.__connected = False
        finally:
            self.__disconnecting = False

    def __signalReceived(self, *args):
        """Received signal. Cancel previous timer and store args to be forwarded later."""
        if self.__disconnecting:
            return
        with self.__lock:
            self.__args = args
            if self.__rateLimit == 0:
                self.__timer.stop()
                self.__timer.start((self.__delay * 1000) + 1)
            else:
                now = time.time()
                if self.__lastFlushTime is None:
                    leakTime = 0
                else:
                    lastFlush = self.__lastFlushTime
                    leakTime = max(0, (lastFlush + (1.0 / self.__rateLimit)) - now)

                self.__timer.stop()
                # Note: original was min() below.
                timeout = (max(leakTime, self.__delay) * 1000) + 1
                self.__timer.start(timeout)

    def __flush(self):
        """If there is a signal queued up, send it now."""
        if self.__args is None or self.__disconnecting:
            return False
        #self.emit(self.signal, *self.args)
        self.__sigDelayed.emit(self.__args)
        self.__args = None
        self.__timer.stop()
        self.__lastFlushTime = time.time()
        return True

    def __connect_signal(self, signal):
        signal.connect(self.__signalReceived, Qt.QueuedConnection)


###############################################################################
#http://eli.thegreenplace.net/2011/10/19/perls-guess-if-file-is-text-or-binary-implemented-in-python
import sys
PY3 = sys.version_info[0] == 3

# A function that takes an integer in the 8-bit range and returns
# a single-character byte object in py3 / a single-character string
# in py2.
#
int2byte = (lambda x: bytes((x,))) if PY3 else chr

_text_characters = (
        b''.join(int2byte(i) for i in range(32, 127)) +
        b'\n\r\t\f\b')

def is_text_file(filepath, blocksize=2**13):
    """ Uses heuristics to guess whether the given file is text or binary,
        by reading a single block of bytes from the file.
        If more than 30% of the chars in the block are non-text, or there
        are NUL ('\x00') bytes in the block, assume this is a binary file.
    """
    with open(filepath, "rb") as fileobj:
        block = fileobj.read(blocksize)
        if b'\x00' in block:
            # Files with null bytes are binary
            return False
        elif not block:
            # An empty file is considered a valid text file
            return True

        # Use translate's 'deletechars' argument to efficiently remove all
        # occurrences of _text_characters from the block
        nontext = block.translate(None, _text_characters)
        return float(len(nontext)) / len(block) <= 0.30


###############################################################################
# # Debugging facilities

def GetCurrentFunctionName(k=0):
  """Returns the function name of the caller.

  Arguments:
    k=0 -- 1: caller of the caller,
       2: caller of the caller of the caller etc

  Obs: profiling shows that this function grabs a lot of execution time for itself."""
  return inspect.stack()[1+k][3]


class MyLock(object):
    """Lock with verbosing, helps to find deadlocks"""

    def __init__(self, name=None, flag_verbose=False):
        if name is None:
            name = random_name()
        self.name = name
        self.flag_verbose = flag_verbose
        self.__lock = Lock()

    def acquire(self, *args):
        if self.__lock.locked():
            self.__log("Tried to lock, but is already locked!!")
            get_python_logger().info("\n".join(traceback.format_stack()))
        else:
            pass
            # self.__log("Will acquire lock")
        self.__lock.acquire(*args)
        self.__log("Acquired lock, good")

    def release(self):
        self.__lock.release()
        self.__log("Released lock")

    # For the "with" statement
    def __exit__(self, *args):
        self.release()
    __enter__ = acquire

    def __log(self, s):
        if self.flag_verbose:
            get_python_logger().info("--- MyLock %s --- %s (caller: %s; thread: %s)" %
            (self.name, s, GetCurrentFunctionName(2), current_thread().name))


###############################################################################
# # RAINBOW

class Color(AttrsPart):
    attrs = ["name", "rgb", "clambda", "l0", "lf"]
    def __init__(self, name, rgb, clambda):
        AttrsPart.__init__(self)
        self.name = name
        self.rgb = rgb
        self.clambda = clambda
        self.l0 = -1.  # initialized later
        self.lf = -1.
    def __repr__(self):
        return '"%s"' % self.one_liner_str()
        # return ", ".join(["%s: %s" % (name, self.__getattribute__(name)) for name in ["name", "rgb", "clambda", "l0", "lf"]])
rainbow_colors = [Color("Violet", [139, 0, 255], 4000),
                  Color("Indigo", [75, 0, 130], 4450),
                  Color("Blue", [0, 0, 255], 4750),
                  Color("Green(X11)", [0, 255, 0], 5100),
                  Color("Yellow", [255, 255, 0], 5700),
                  Color("Orange", [255, 127, 0], 5900),
                  Color("Red", [255, 0, 0], 6500),
                  ]

# Calculates l0, lf
# rainbow_colors[0].l0 = 0.
# rainbow_colors[-1].lf = float("inf")
for c in rainbow_colors:
    c.clambda = float(c.clambda)
ncolors = len(rainbow_colors)
for i in range(1, ncolors):
    cprev, cnow = rainbow_colors[i-1], rainbow_colors[i]
    avg = (cprev.clambda + cnow.clambda) / 2
    cprev.lf = avg
    cnow.l0 = avg

    if i == 1:
        cprev.l0 = 2*cprev.clambda-cprev.lf
    if i == ncolors-1:
        cnow.lf = 2*cnow.clambda-cnow.l0
# converts RGB from [0, 255] to [0, 1.] interval
for c in rainbow_colors:
    c.rgb = np.array([float(x)/255 for x in c.rgb])



################################################################################
# # SEARCHES

def BSearch(a, x, lo=0, hi=None):
  """Returns index of x in a, or -1 if x not in a.

  Arguments:
    a -- ordered numeric sequence
    x -- element to search within a
    lo -- lowest index to consider in search*
    hi -- highest index to consider in search*

  *bisect.bisect_left capability that we don't need to loose."""
  if len(a) == 0: return -1
  hi = hi if hi is not None else len(a)
  pos = bisect_left(a, x, lo, hi)
  return pos if pos != hi and a[pos] == x else -1 # doesn't walk off the end


def BSearchRound(a, x, lo=0, hi=None):
  """Returns index of a that is closest to x.

  Arguments:
    a -- ordered numeric sequence
    x -- element to search within a
    lo -- lowest index to consider in search*
    hi -- highest index to consider in search*

  *bisect.bisect_left capability that we don't need to loose."""
  if len(a) == 0: return -1
  hi = hi if hi is not None else len(a)
  pos = bisect_left(a, x, lo, hi)

  if pos >= hi:
    return hi-1
  elif a[pos] == x or pos == lo:
    return pos
  else:
    return pos-1 if x-a[pos-1] <= a[pos]-x else pos


def BSearchCeil(a, x, lo=0, hi=None):
  """Returns lowest i such as a[i] >= x, or -1 if x > all elements in a

  So, if x is in between two elements in a, this function will return the
  index of the higher element, hence "Ceil".

  Arguments:
    a -- ordered numeric sequence
    x -- element to search within a
    lo -- lowest index to consider in search
    hi -- highest index to consider in search"""
  if len(a) == 0: return -1
  hi = hi if hi is not None else len(a)
  pos = bisect_left(a, x, lo, hi)
  return pos if pos < hi else -1


def BSearchFloor(a, x, lo=0, hi=None):
  """Returns highest i such as a[i] <= x, or -1 if x < all elements in a

  So, if x is in between two elements in a, this function will return the
  index of the lower element, hence "Floor".

  Arguments:
    a -- ordered numeric sequence
    x -- element to search within a
    lo -- lowest index to consider in search
    hi -- highest index to consider in search"""
  if len(a) == 0: return -1
  hi = hi if hi is not None else len(a)
  pos = bisect_left(a, x, lo, hi)
  return pos-1 if pos >= hi\
       else (pos if x == a[pos] else (pos-1 if pos > lo else -1))

def FindNotNaNBackwards(x, i):
  """Returns last position (starting at i backwards) which is not NaN, or -1."""
  while i >= 0:
    if not np.isnan(x[i]):
      return i
    i -= 1
  return -1



########################################################################################################################
# PHOTOMETRY

MAGNITUDE_BASE = 100. ** (1. / 5)  # approx. 2.512


def ufunc_gauss(x0, fwhm):
    """Returns a Gaussian function given the x at maximum value and the fwhm. Works as a numpy ufunc

    **Note** the maximum value is 1. at x=x0

    Reference: https://en.wikipedia.org/wiki/Gaussian_function

    Test code:

      >> from pymos import *
      >> import matplotlib.pyplot as plt
      >> import numpy as np
      >> f = ufunc_gauss(0, 5.)
      >> x = np.linspace(-10, 10, 200)
      >> y = f(x)
      >> plt.plot(x, y)
      >> plt.show()
    """

    # K = 1/(2*c**2)
    # c = fwhm/(2*sqrt(2*ln(2)))
    K = 4 * np.log(2) / fwhm ** 2
    def f(x):
        return np.exp(-(x - x0) ** 2 * K)

    return f


class Band(object):
    """
    Represents wavelength filter band, containing a few tools

    This class is kept clean whereas Bands has examples and deeper documentation on parameters

    Arguments:
        tabular -- ((wl, y), ...), 0 <= y <= 1
        parametric -- ((wl, fwhm), ...)
        ref_mean_flux -- reference mean flux passing through filter at magnitude 0 in Jy units
    """
    def __init__(self, name, tabular=None, parametric=None, ref_mean_flux=None):
        self.name = name
        self.tabular = tabular
        self.parametric = parametric
        self.ref_mean_flux = ref_mean_flux

    def ufunc_band(self, flag_force_parametric):
        """Uses tabular data if available and not flag_force_parametric"""
        flag_parametric = flag_force_parametric
        if not flag_force_parametric and self.tabular:
            x, y = zip(*self.tabular)
            f = interp1d(x, y, kind='linear', bounds_error=False, fill_value=0)
        else:
            flag_parametric = True

        if flag_parametric:
            x0, fwhm = self.parametric
            f = ufunc_gauss(x0, fwhm)
        return f

    def range(self, flag_force_parametric=False, no_stds=3):
        """Returns [wl0, wl1], using edges of tabular data or given number of standard deviations"""
        flag_parametric = flag_force_parametric

        if not flag_force_parametric and self.tabular:
            points = self.tabular
            p0, pf = points[0], points[-1]
            # The following is assumed for the code to work
            assert p0[1] == 0 and pf[1] == 0, "Bands.TABULAR tables must start and end with a zero"
            ret = [p0[0], pf[0]]
        else:
            flag_parametric = True

        if flag_parametric:
            x0, fwhm = self.parametric
            std = fwhm * (1. / np.sqrt(8 * np.log(2)))
            ret = [x0 - no_stds * std, x0 + no_stds * std]
        return ret


class Bands(object):
    # Michael Bessel 1990
    # Taken from http://spiff.rit.edu/classes/phys440/lectures/filters/filters.html
    #
    # The following code should get a plot of these tabulated bands:
    #
    #   >> from pymos import *
    #   >> import matplotlib.pyplot as plt
    #   >> import numpy as np
    #   >> band_names = "UBVRI"
    #   >> colors = np.array([[139., 0, 255], [0, 0, 255], [0, 196, 0], [255, 0, 0], [128, 0, 0]], dtype=float)/255
    #   >> plt.figure()
    #   >> for color, band_name in zip(colors, band_names):
    #   >>     x, y = zip(*UBVRI[band_name])
    #   >>     plt.plot(x, y, label=band_name, c=color)
    #   >> plt.legend()
    #   >> plt.show()
    TABULAR = collections.OrderedDict((
    ("U", ((3000, 0.00),  (3050, 0.016), (3100, 0.068), (3150, 0.167), (3200, 0.287), (3250, 0.423), (3300, 0.560),
           (3350, 0.673), (3400, 0.772), (3450, 0.841), (3500, 0.905), (3550, 0.943), (3600, 0.981), (3650, 0.993),
           (3700, 1.000), (3750, 0.989), (3800, 0.916), (3850, 0.804), (3900, 0.625), (3950, 0.423), (4000, 0.238),
           (4050, 0.114), (4100, 0.051), (4150, 0.019), (4200, 0.000))),
    ("B", ((3600, 0.0), (3700, 0.030), (3800, 0.134), (3900, 0.567), (4000, 0.920), (4100, 0.978), (4200, 1.000),
           (4300, 0.978), (4400, 0.935), (4500, 0.853), (4600, 0.740), (4700, 0.640), (4800, 0.536), (4900, 0.424),
           (5000, 0.325), (5100, 0.235), (5200, 0.150), (5300, 0.095), (5400, 0.043), (5500, 0.009), (5600, 0.0))),
    ("V", ((4700, 0.000), (4800, 0.030), (4900, 0.163), (5000, 0.458), (5100, 0.780), (5200, 0.967), (5300, 1.000),
          (5400, 0.973), (5500, 0.898), (5600, 0.792), (5700, 0.684), (5800, 0.574), (5900, 0.461), (6000, 0.359),
          (6100, 0.270), (6200, 0.197), (6300, 0.135), (6400, 0.081), (6500, 0.045), (6600, 0.025), (6700, 0.017),
          (6800, 0.013), (6900, 0.009), (7000, 0.000))),
    ("R", ((5500, 0.0), (5600, 0.23), (5700, 0.74), (5800, 0.91), (5900, 0.98), (6000, 1.000), (6100, 0.98),
           (6200, 0.96), (6300, 0.93), (6400, 0.90), (6500, 0.86), (6600, 0.81), (6700, 0.78), (6800, 0.72),
           (6900, 0.67), (7000, 0.61), (7100, 0.56), (7200, 0.51), (7300, 0.46), (7400, 0.40), (7500, 0.35),
           (8000, 0.14), (8500, 0.03), (9000, 0.00))),
    ("I", ((7000, 0.000), (7100, 0.024), (7200, 0.232), (7300, 0.555), (7400, 0.785), (7500, 0.910), (7600, 0.965),
           (7700, 0.985), (7800, 0.990), (7900, 0.995), (8000, 1.000), (8100, 1.000), (8200, 0.990), (8300, 0.980),
           (8400, 0.950), (8500, 0.910), (8600, 0.860), (8700, 0.750), (8800, 0.560), (8900, 0.330), (9000, 0.150),
           (9100, 0.030), (9200, 0.000)))
    ))


    # Values taken from https://en.wikipedia.org/wiki/Photometric_system
    PARAMETRIC = collections.OrderedDict((
    ("U", (3650., 660.)),
    ("B", (4450., 940.)),
    ("V", (5510., 880.)),
    ("R", (6580., 1380.)),
    ("I", (8060., 1490.)),
    ("Y", (10200., 1200.)),
    ("J", (12200., 2130.)),
    ("H", (16300., 3070.)),
    ("K", (21900., 3900.)),
    ("L", (34500., 4720.)),
    ("M", (47500., 4600.)),
    ("N", (105000., 25000.)),
    ("Q", (210000., 58000.))
    ))

    # values taken from https://en.wikipedia.org/wiki/Apparent_magnitude, but I- and J-band values agree with
    # Evans, C.J., et al., A&A 527(2011): A50.
    REF_JY = collections.OrderedDict((
    ("U", 1810),
    ("B", 4260),
    ("V", 3640),
    ("R", 3080),
    ("I", 2550),
    ("Y", None),
    ("J", 1600),
    ("H", 1080),
    ("K", 670),
    ("L", None),
    ("M", None),
    ("N", None),
    ("Q", None),

    ))

    bands = collections.OrderedDict()
    for k in PARAMETRIC:
        bands[k] = Band(k, TABULAR.get(k), PARAMETRIC.get(k), REF_JY.get(k))

    @classmethod
    def names(cls):
        """Returns string containing all band names"""
        return "".join([x[0] for x in cls.PARAMETRIC])

    @classmethod
    def ufunc_band(cls, band_name, flag_force_parametric=False):
        """Returns a function(wavelength) for the transmission filter given the band name. Works as a numpy ufunc

        Arguments:
            band_name -- e.g. "U", "J"
            flag_force_parametric -- if set, will use parametric data even for the tabulated bands UBVRI


        Test code:
          >> from pymos import *
          >> import matplotlib.pyplot as plt
          >> import numpy as np
          >> l0, lf = 3000, 250000
          >> x =  np.logspace(np.log10(l0), np.log10(lf), 1000, base=10.)
          >> for band_name, (x0, fwhm) in Bands.PARAMETRIC.iteritems():
          >>     plt.subplot(211)
          >>     plt.semilogx(x, ufunc_band(band_name)(x), label=band_name)
          >>     plt.subplot(212)
          >>     plt.semilogx(x, ufunc_band(band_name, True)(x), label=band_name)
          >> plt.subplot(211)
          >> plt.title("Tabulated UBVRI")
          >> plt.xlim([l0, lf])
          >> plt.subplot(212)
          >> plt.title("Parametric UBVRI")
          >> plt.xlabel("Wavelength (angstrom)")
          >> plt.xlim([l0, lf])
          >> l = plt.legend(loc='lower right')
          >> plt.tight_layout()
          >> plt.show()
        """
        return cls.bands[band_name].ufunc_band(flag_force_parametric)

    @classmethod
    def range(cls, band_name, flag_force_parametric=False, no_stds=3):
        """Returns wavelength range beyond which the transmission function value is zero or negligible

        **Note**

        Arguments:
            band_name -- e.g. "U", "J"
            flag_force_parametric -- if set, will use parametric data even for the tabulated bands UBVRI
            no_stds -- number of standard deviations away from center to consider the range limit (parametric cases only).
                       At 3 standard deviations from the center the value drops to approximately 1.1% of the maximum
        """

        return cls.bands[band_name].range(flag_force_parametric, no_stds)


########################################################################################################################
# Originally from pymos

def eval_fieldnames(string_, varname="fieldnames"):
    """Evaluates string_, must evaluate to list of strings. Also converts field names to uppercase"""
    ff = eval(string_)
    if not isinstance(ff, list):
        raise RuntimeError("%s must be a list" % varname)
    if not all([isinstance(x, str) for x in ff]):
        raise RuntimeError("%s must be a list of strings" % varname)
    ff = [x.upper() for x in ff]
    return ff


########################################################################################################################
# MATHS

def bc_rubber(vx):
    """Convex Polygonal Line baseline correction

    Arguments:
      vx -- vector
    """

    return vx-rubberband(vx)


def rubberband(vx):
    """
    Convex polygonal line (aka rubberbadn) whose vertices touch troughs of x
    without crossing x, where x = X[i, :], i <= 0 < no (see below).


    This was inspired on OPUS SB_Rubberband baseline correction (RBBC) [1]. However,
    this one is parameterless, whereas OPUS RBBC asks for a number of points.

    References:
        [1] Bruker Optik GmbH, OPUS 5 Reference Manual. Ettlingen: Bruker, 2004.
    """
    pieces = [[vx[0]]]
    _rubber_pieces(vx, pieces)
    rubberband = np.concatenate(pieces)

    return rubberband


def _rubber_pieces(x, pieces):
    """Recursive function that add straight lines to list. Together, these lines form the rubberband."""
    nf = len(x)
    l = np.linspace(x[0], x[-1], nf)
    xflat = x - l
    idx = np.argmin(xflat)
    val = xflat[idx]
    if val < 0:
        _rubber_pieces(x[0:idx + 1], pieces)
        _rubber_pieces(x[idx:], pieces)
    else:
        pieces.append(l[1:])


def poly_baseline(flux, order, epsilon=None, maxit=None):
    """"
    Polynomial baseline

    Arguments:
      vx -- np 1D array ("flux")
      order -- polynomial order
      epsilon -- tolerance to stop iterations. If zero or no value given, will default to sqrt(1/30*num_points)
      maxit -- if informed, will restrict the maximum number of iterations to this value

    Returns: the baseline vector

    Reference: Beier BD, Berger AJ. Method for automated background subtraction from Raman spectra containing known
    contaminants. The Analyst. 2009; 134(6):1198-202. Available at: http://www.ncbi.nlm.nih.gov/pubmed/19475148.

    **Converted from MATLAB (IRootLab)**
    """

    nf = len(flux)
    if epsilon is None:
        # epsilon will be compared to a vector norm. If we assume the error at all elements to have the same importance, it
        # the norm of the error will be something like sqrt(nf*error_i)
        # For the tolerance to be 1 when nf = 1500 (empirically found to work) the formula below is set.
        epsilon = np.sqrt(1/90*nf);

    x = np.arange(1, nf+1) # x-values to be powered as columns of a design matrix
    M = np.zeros((nf, order+1))
    for i in range(0, order+1):
        M[:, i] = x**i
    MM = np.dot(M.T, M)
    flag_first = True
    it = 0
    while True:
        # (29/03/2011) Least-Squares solution is faster than polyfit()

        # Original formula in MATLAB: yp = (M*(MM\(M'*vx')))'
        yp = np.dot(M, (np.linalg.solve(MM, np.dot(M.T, flux)))).T

        flux = np.min(np.vstack((flux, yp)), 0)

        if not flag_first:
            if np.linalg.norm(flux-y_previous, 2) < epsilon:
                break
        else:
            flag_first = False

        y_previous = flux

        it += 1
        if it >= maxit:
            break

    return yp



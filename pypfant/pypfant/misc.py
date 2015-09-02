
__all__ = ["str_vector", "float_vector", "path_to_default", "new_filename", "str2bool",
       "write_lf", "bool2str", "list2str", "menu", "chunk_string", "readline_strip",
       "LogTwo", "print_noisy", "X"]

import os.path
import glob
import random
from threading import Lock
import logging
import sys

logger = logging.getLogger(__name__)

# Reads next line of file and makes it a vector of strings
# Note that each str.split() already strips each resulting string of any whitespaces.
str_vector = lambda f: f.readline().split()

# Reads next line of file and makes it a vector of floats
float_vector = lambda f: [float(s) for s in str_vector(f)]

# reads next line of file and strips the newline
readline_strip = lambda f: f.readline().strip('\n')

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


def write_lf(h, s):
  """Adds lf to end of string and writes it to file."""
  h.write(s+"\n")


def menu(title, options, cancel_label="Cancel", flag_allow_empty=False):
  """Text menu.

  Arguments:
    title
    options -- sequence of strings
    cancel_label='Cancel' -- label to show at last "zero" option
    flag_allow_empty=0 -- Whether to allow empty option

  Returns:
    option -- an integer: None; 0-Back/Cancel/etc; 1, 2, ...

  Adapted from irootlab menu.m
  """

  no_options, flag_ok, ch, lt = len(options), 0, 'p', len(title)
  option = None  # result

  while True:
    print ""
    print "  "+ch*(lt+8)
    print "  "+ch*3+" "+title+" "+ch*3
    print "  "+ch*(lt+8)
    for i, s in enumerate(options):
      print "  %d - %s" % (i+1, s)
    print "  0 - << (*%s*)" % cancel_label
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
        if 0 <= option <= no_options:
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



#
# def add_file_handler(logger, logFilename=None):
#   """Adds file handler to logger."""
#
#   assert isinstance(logger, logging.Logger)
#
#   ch = logging.FileHandler(filename=logFilename)
#   ch.setFormatter(logging._defaultFormatter) # todo may change to have same formatter as last handler of logger
#   logger.addHandler(ch)
#




class LogTwo(object):
  """Logs messages to both stdout and file."""
  def __init__(self, filename):
    self.terminal = sys.stdout
    self.log = open(filename, "a")

  def write(self, message):
    self.terminal.write(message)
    self.log.write(message)

# character for boxing strings
X = "*"
HR = X*40

def print_noisy(msg):
  """Prints string message with box around.

  This was designed to outstand in a long log dump.
  """

  xx = X*(len(msg)+4)
  print ""
  print xx
  print X, msg, X
  print xx

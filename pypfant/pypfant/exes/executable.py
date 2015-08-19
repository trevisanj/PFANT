__all__ = ["Executable"]

import os
import subprocess
import logging
from pypfant.misc import find_session_id
from pypfant.data.datafile import *
from .runnable import *

logger = logging.getLogger("exe")


class Executable(Runnable):
  """
  Generic class to represent an executable.
  """

  def __init__(self):
    Runnable.__init__(self)
    # Full path to executable (including executable name)
    self.exe_path = "./none"

    # Command-line options present in all executables
    self.opt_logging_level = None
    self.opt_logging_screen = None
    self.opt_logging_dump = None
    self.opt_logging_fn_dump = None
    self.opt_fn_main = None

    # FileMain instance
    # If set, main configuration file (of random name) will be created before
    # running the executable.
    self.fo_main = None

    self.stdout = None

    self.popen = None


  ##############################################################################
  # PUBLIC INTERFACE

  def run(self):
    """Blocking routine. Overwrites self.popen."""

    assert not self.is_running(), "Already running"

    self.create_data_files()
    args = self.get_args()
    cmd_line = [self.exe_path]+args

    logger.debug("%s command-line:" % (self.__class__.__name__,))
    logger.debug(" ".join(cmd_line))

    self.popen = subprocess.Popen(cmd_line, stdout=self.stdout)
    self.popen.wait()  # Blocks execution until finished

    self.popen.poll()

    logger.debug("%s finished with returncode=%s" %
                 (self.__class__.__name__, self.popen.returncode))

  def is_running(self):
    if self.popen is None:
      return False
    self.popen.poll()
    return self.popen.returncode is None

  def kill(self):
    assert self.is_running(), "Not running"
    self.popen.kill()


  ##############################################################################
  # ROUTINES TO BE OVERRIDEN

  def get_args(self):
    """
    Returns a list of command-line arguments from "opt_" and "optf_" attributes.

    Sweeps all object attributes.
      - If attribute starts with "opt_", it is an option requiring an argument
      - If attribute starts with "optf_", it is a flag option without argument

    Example:
      Suppose that a class descending from Executable has the following
      attributes and values:
        - self.opt_fn_main = "main1234.dat"
        - self.optf_hmap = True

      In such case, this method will return
      "--fn_main", "main1234.dat", "--hmap"]

      If self.optf_hmap were False, "--hmap" would be absent from the result.

    """

    l = []
    for attr_name in dir(self):
      if attr_name.startswith("opt_"):
        value = self.__getattribute__(attr_name)
        if value is not None:
          s_value = ("T" if value else "F") if isinstance(value, bool) else str(value)
          l.extend(["--"+attr_name[4:], s_value])
      elif attr_name.startswith("optf_"):
        value = self.__getattribute__(attr_name)
        if value:
          l.append("--"+attr_name[5:])
    return l

  ##############################################################################
  # ROUTINES TO BE LEFT AS-ARE

  def full_path_w(self, fn):
    """Joins self.inputdir with specified filename."""
    if self.opt_wdir is not None:
      return os.path.join(self.opt_wdir, fn)
    else:
      return fn

  def create_data_files(self):
    """
    Creates files for all object attributes starting with prefix "fo_"

    Example:
      Consider the attribute fo_main:
        If self.fo_main is None:
          - File specified by self.fn_main must exist.
        else:
          - File of random name (e.g., main_23498.dat) will be generated
          - self.fn_main will be overwritten

    Now this example extends to fo_* attributes.
    """

    for attr_name in dir(self):
      if attr_name[0:3] == "fo_":
        obj = self.__getattribute__(attr_name)

        if obj is not None:
          assert isinstance(obj, DataFile)

          new_fn = self.nman.get_relative_path(obj.default_filename)
          fullpath = self.full_path_w(new_fn)
          # Saves file
          obj.save(fullpath)
          # Overwrites config option
          self.__setattr__("opt_fn_"+attr_name[3:], new_fn)


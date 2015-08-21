"""
Executable and subclasses
"""

__all__ = ["Executable", "Innewmarcs", "Hydro2", "Pfant", "Nulbad"]

import subprocess
import logging
from .execonf import ExeConf
import os

logger = logging.getLogger("exe")

class Executable(object):
  """
  Generic class to represent an executable.
  """

  def __init__(self):
    # Full path to executable (including executable name)
    self.exe_path = "./none"

    # ExeConf instance
    self.execonf = ExeConf()
    # todo Not implemented properly
    self.stdout = None
    # Created by _run()
    self.popen = None



STOPPED TRYING TO DEFINE WHERE TO LOG WHAT IS GOING ON
session123456/commands.log
session123456/stdout_pfant.log
.....

  def run(self):
    """Runs executable.

    Blocking routine. Only returns when executable finishes running.
    """
    assert not self.is_running(), "Already running"
    self.execonf.make_session_id()
    self.execonf.create_data_files()
    self._run()

  def run_from_combo(self):
    """Alternative to run executable (called from Combo class)."""
    assert not self.is_running(), "Already running"
    self._run()

  def _run(self):
    args = self.execonf.get_args()
    cmd_line = [self.exe_path]+args

    logger.debug("%s command-line:" % (self.__class__.__name__,))
    logger.debug(" ".join(cmd_line))

    try:
      self.popen = subprocess.Popen(cmd_line, stdout=self.stdout)
    except OSError:
      logger.error("Failed to execute $ "+(" ".join(cmd_line)))
      raise
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


class Innewmarcs(Executable):
  """Class representing the innewmarcs executable."""

  def __init__(self):
    Executable.__init__(self)

    self.exe_path = "./innewmarcs"

class Hydro2(Executable):
  """Class representing the hydro2 executable."""

  def __init__(self):
    Executable.__init__(self)

    self.exe_path = "./hydro2"

class Pfant(Executable):
  def __init__(self):
    Executable.__init__(self)

    self.exe_path = "./pfant"  # Path to PFANT executable (including executable name)

    # ** Variables assigned by poll_progress()
    self.ikey = None  # Current iteration (Fortran: ikey)
    self.ikeytot = None  # Current iteration (Fortran: ikeytot)

  def poll_progress(self):
    """
    Tries to open progress indicator file. If succeeds, stores
    current iteration in attribute ikey, and
    number of iterations in attribute ikeytot.

    """
    p = self.execonf.full_path_w(self.execonf.opt_fn_progress)
    if os.path.isfile(p):
      with open(p) as h:
        self.ikey, self.ikeytot = map(int, h.readline().split("/"))
    else:
      self.ikey, self.ikeytot = None, None

class Nulbad(Executable):
  """Class representing the nulbad executable."""

  def __init__(self):
    Executable.__init__(self)

    self.exe_path = "./nulbad"

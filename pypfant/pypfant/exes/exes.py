"""
Executable and subclasses
"""

__all__ = ["Executable", "Innewmarcs", "Hydro2", "Pfant", "Nulbad"]

import subprocess
import logging
from .execonf import ExeConf
import os
import sys
from pypfant import print_noisy, X


class Executable(object):
  """
  Generic class to represent an executable.
  """

  def __init__(self):
    # self.logger = None

    # Full path to executable (including executable name)
    self.exe_path = "./none"

    # File object to log executable stdout
    self.logfile = None

    # ExeConf instance
    self.execonf = ExeConf()
    # Created by _run()
    self.popen = None

  def run(self):
    """Runs executable.

    Blocking routine. Only returns when executable finishes running.
    """
    assert not self.is_running(), "Already running"
    # self.logger = logging.getLogger(self.__class__.__name__.lower())
    self.execonf.make_session_id()
    self.execonf.create_data_files()
    self._run()


  def run_from_combo(self):
    """Alternative to run executable (called from Combo class).

    This routine bypasses all the configuration that is done prior to running.
    (Combo.configure() will do the necessary configuration).
    """
    assert not self.is_running(), "Already running"
    self._run()

  def _run(self):
    args = self.execonf.get_args()
    cmd_line = [self.exe_path]+args

    s = "%s command-line:" % (self.__class__.__name__.lower(),)
    print_noisy(s)
    print " ".join(cmd_line)
    print X*(len(s)+4)


    try:
      self.popen = subprocess.Popen(cmd_line, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
      for line in self.popen.stdout:
        sys.stdout.write(line)
    except OSError:
      print_noisy("Failed to execute command-line")
      raise
    self.popen.wait()  # Blocks execution until finished
    self.popen.poll()
    print_noisy("%s %s (returncode=%s)" %
                (self.__class__.__name__.lower(),
                 'finished successfully' if self.popen.returncode == 0 else '*failed*',
                 self.popen.returncode))

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

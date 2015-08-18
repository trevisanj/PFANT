__all__ = ["Combo"]

from pypfant import *
import os
import subprocess
import logging
from threading import Lock

logger = logging.getLogger("combo")

class Combo(object):
  """
  Runs sequence of executables: innermarcs, hydro2, pfant, nulbad.
  """

  @property
  def is_running(self):
    with self.__L_running:
      return self.__is_running
  @is_running.setter
  def is_running(self, x):
    with self.__L_running:
      self.__is_running = x
  @property
  def running_exe(self):
    with self.__L_running:
      return self.__running_exe
  @running_exe.setter
  def running_exe(self, x):
    with self.__L_running:
      self.__running_exe = x



  def __init__(self, exe):

    assert isinstance(exe, Executable)

    # Object of class Executable to hold the command-line options that are
    # common to all executables. Attributes starting with "opt" will be
    # assigned to Innewmarcs, Hydro2, Pfant, Nulbad  executables
    self.exe = exe

    # Directory containing the 4 executables
    self.exe_dir = "."

    self.innewmarcs = Innewmarcs()
    self.hydro2 = Hydro2()
    self.pfant = Pfant()
    self.nulbad = Nulbad()

    ## Variables taken by poll_progress() from the Pfant executable when it is
    ## running
    self.ikey = None
    self.ikeytot = None


    ## Internal variables
    self.__L_running = Lock()  # To make the following variables thread-safe
    self.__is_running = False
    self.__running_exe = None  # Executable object currently running



  ##############################################################################
  # PUBLIC INTERFACE

  def run(self):
    """Blocking routine. Overwrites self.popen."""

    assert not self.is_running, "Already running"

    def run_e():
      self.assign_opt(e)
      e.run()
      e.poll()
      if e.popen.returncode != 0:
        raise RuntimeError("%s failed" % e.__class__.__name__)


    self.is_running = True
    try:
      self.running_exe = e = f = self.innewmarcs
      run_e()

      self.running_exe = e = f = self.hydro2
      run_e()

      self.running_exe = e = f = self.pfant
      run_e()

      self.running_exe = e = self.nulbad
      run_e()

    finally:
      self.is_running = False

  def assign_opt(self, target):
    """Copies all attributes starting with "opt" between self.exe and target."""
    assert isinstance(target, Executable)
    for attr_name in dir(self.exe):
      if attr_name.startswith("opt"):
        attr = self.exe.__getattribute__(attr_name)
        target.__setattr__(attr_name, attr)


  def poll_progress(self):
    """
    Wraps Pfant.poll_progress().
    """

    with self.__L_running:
      if self.__is_running and self.__running_exe == self.pfant:
        self.pfant.poll_progress()
        self.ikey, self.ikeytot = self.pfant.ikey, self.pfant.ikeytot
      else:
        self.ikey, self.ikeytot = None, None


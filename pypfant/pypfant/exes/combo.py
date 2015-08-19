__all__ = ["Combo"]

from pypfant import *
import os
import subprocess
import logging
from threading import Lock
from .runnable import *

logger = logging.getLogger("combo")

class Combo(Runnable):
  """
  Runs sequence of executables: innermarcs, hydro2, pfant, nulbad.

  Important: some opt* attributes will be propagated when the combo is run.
   - self.opt_wdir (working directory) should be set to change the working
     directory
   - options common to all executables should be set at self.common

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
    Runnable.__init__(self)

    assert isinstance(exe, Executable)


    # Directory containing the 4 executables
    self.exe_dir = "."

    # Object of class Executable to hold the command-line options that are
    # common to all executables. Attributes starting with "opt" will be
    # assigned to Innewmarcs, Hydro2, Pfant, Nulbad  executables
    #
    # In practice, all options that are common to all executables must be set
    # here.
    self.common = Executable()
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

    self.is_running = True
    try:
      # Propagates working directory
      self.common.opt_wdir = self.opt_wdir

      ## Runs innewmarcs

      self.running_exe = e = f = self.innewmarcs
      assert isinstance(e, Innewmarcs)
      if e.opt_fn_modeles is None:
        e.opt_fn_modeles = asdsda
      self.__run_e(e)

      self.running_exe = e = f = self.hydro2
      self.__run_e(e)

      self.running_exe = e = f = self.pfant
      self.__run_e(e)

      self.running_exe = e = self.nulbad
      self.__run_e(e)

    finally:
      self.is_running = False



  def __run_e(self, e):
    """Sets up executable object and runs it."""

    assert isinstance(e, Executable)

    # Propagates session id and name manager
    e.session_id = self.session_id
    e.nman = self.nman

    # Propagates all common options
    for attr_name in dir(self.common):
      if attr_name.startswith("opt"):
        attr = self.common.__getattribute__(attr_name)
        e.__setattr__(attr_name, attr)

    e.run()
    if e.popen.returncode != 0:
      raise RuntimeError("%s failed" % e.__class__.__name__)



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


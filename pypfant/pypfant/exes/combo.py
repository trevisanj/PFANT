__all__ = ["Combo"]

from pypfant import * #ExeConf, Pfant, Innewmarcs, Hydro2, Nulbad, add_file_handler
from pypfant.exes.execonf import *
from pypfant.exes.exes import *
import os
import logging
from threading import Lock

class Combo(object):
  """
  Runs sequence of executables: innermarcs, hydro2, pfant, nulbad.

  There are several restrictions imposed
  - files are created inside a session directory such as session123456
  - pfant and hydro2 need to run in "--hmap" mode
  - all four executables must be in the same directory

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

  def __init__(self):
    # Directory containing the 4 executables
    self.exe_dir = "."

    # ExeConf instance
    self.execonf = ExeConf()

    # ** Executable instances
    self.innewmarcs = Innewmarcs()
    self.hydro2 = Hydro2()
    self.pfant = Pfant()
    self.nulbad = Nulbad()

    # ** Variables taken by poll_progress() from the Pfant executable when it is
    # ** running
    self.ikey = None
    self.ikeytot = None

    # ** Internal variables
    self.__L_running = Lock()  # To make the following variables thread-safe
    self.__is_running = False
    self.__running_exe = None  # Executable object currently running

    self.logger = None

  def configure(self):
    """
    Sets several properties of execonf and executables to achieve the expected
    synchronization.
    """

    c = self.execonf

    c.make_session_id()
    c.prepare_filenames_for_combo()

    self.logger = logging.getLogger("combo") # % id(self))
    add_file_handler(self.logger, c.full_path_w(c.add_session_dir("commands.log")))


    # All files that will be created need to have the session directory added to their names
    for e in self.get_exes():
      # Propagates configuration
      e.execonf = c
      e.logger = self.logger
      # Sets exe output to log file
      log_path = c.full_path_w(c.add_session_dir("stdout_%s.log" % e.__class__.__name__.lower()))
      e.stdout = open(log_path, 'w')
      # Fixes exe path
      exe_filename = os.path.split(e.exe_path)[-1]
      e.exe_path = os.path.join(self.exe_dir, exe_filename)


    c.create_data_files()

  def get_exes(self):
    """Returns the four exe objects in a list."""
    return [self.innewmarcs, self.hydro2, self.pfant, self.nulbad]

  def run(self):
    """Blocking routine. Overwrites self.popen."""

    assert not self.is_running, "Already running"

    self.is_running = True
    try:
      self.configure()

      for e in self.get_exes():
        e.run_from_combo()
        if e.popen.returncode != 0:
          raise RuntimeError("%s failed" % e.__class__.__name__)
    finally:
      self.is_running = False

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


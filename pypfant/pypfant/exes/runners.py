# better to run with subprocess POpen, will be able to terminate
# tired

"""
Wraps over the PFANT executable
"""

__all__ = ["ExeRunner"]

from pypfant import *
import threading

class ExeRunner(threading.Thread):
  """
  Thread that runs object of class Executable.
  """

  def __init__(self, exe, *args, **kwargs):
    threading.Thread.__init__(self, *args, **kwargs)
    assert isinstance(exe, Executable)
    self.exe = exe

  def run(self):
    self.exe.run()


class ComboRunner(threading.Thread):
  def __init__(self, exe, *args, **kwargs):
    threading.Thread.__init__(self, *args, **kwargs)
    assert isinstance(exe, Executable)
    self.exe = exe

  def run(self):
    self.exe.run()

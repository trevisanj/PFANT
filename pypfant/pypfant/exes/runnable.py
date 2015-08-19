__all__ = ["Runnable"]

from pypfant.misc import find_session_id
from .nameman import *


class Runnable(object):
  """
  Common ancestor for Executable and Combo.

  The most important feature of this class is its session id.

  Session id is used to
  create unique directory or file names when running in a parallel scheme.

  """

  def __init__(self):
    # Session id
    self.session_id = None
    # Working directory
    self.opt_wdir = None
    self.nman = NameMan(self)


  def make_session_id(self):
    """
    Makes sure that session id is assigned.
    """
    assert self.session_id is None, "Session id already made"
    x = self.session_id = find_session_id(self.opt_wdir)

import os
import shutil

__all__ = ["NameMan"]


class NameMan(object):
  """
  Filename manager. Finds names for files based on session id. Also does the cleaning.

  Arguments:
   runnable -- Runnable instance that will be used to get session id and working
   directory from.
  """

  def __init__(self, runnable):
    # Runnable instance
    self.runnable = runnable

    # Directory to be created (read-only)
    self.dir_name = None
    # First call to get_file_path()? (read-only)
    self.flag_first = True
    # Records working directory used for later cleaning
    self.wdir = None

  def get_file_path(self, filename):
    """
    Returns relative path to file with directory added.

    Path returned is relative to wdir.

    If first time called, creates a directory.

    Note: (for clarity) wdir is not part of the result.

    Arguments:
     filename
     wdir -- working directory
    """

    if self.flag_first:
      assert self.runnable.session_id is not None, "Session id not assigned"
      d = self.dir_name = "session"+self.runnable.session_id
      os.mkdir(os.path.join(self.get_wdir()), d)
      self.flag_first = False

    return os.path.join(self.dir_name, filename)

  def clean(self):
    """Deletes directory with all files inside."""
    assert self.dir_name is not None, "Nothing to clean"
    shutil.rmtree(os.path.join(self.get_wdir(), self.dir_name))

  def get_wdir(self):
    """Returns working directory, this routine exists for "None-prevention"."""
    wdir = self.runnable.opt_wdir if self.runnable.opt_wdir is not None else ""
    return wdir
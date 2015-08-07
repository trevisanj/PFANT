"""
Ancestor class for all classes that represent an input file.
"""

__all__ = ["InputFile"]

from ..misc import *
from ..parts import *


class InputFile(AttrsPart):
  default_filename = None  ## Descendants shoulds set this

  def save(self, filename):
    raise NotImplementedError()

  def load(self, filename):
    raise NotImplementedError()

  def init_default(self):
      """
      Default initialization of attributes listed in self.attrs.

      InputFile behaviour is to try to load self.default_filename from default
      directory.
      """
      fullpath = path_to_default(self.default_filename)
      self.load(fullpath)

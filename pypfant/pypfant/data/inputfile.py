"""
Ancestor class for all classes that represent an input file.
"""

__all__ = ["InputFile"]

from ..misc import *


class InputFile(object):
  default_filename = None  ## Descendants shoulds set this

  # for __str__()
  attrs = []

  def __str__(self):
    s_format = "{:>%d} = {}" % max([len(x) for x in self.attrs])
    s = object.__str__(self)+"\n"+\
      "\n".join([s_format.format(x, self.__getattribute__(x)) for x in self.attrs])
    return s


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

"""
This module contains "parts" classes -- classes be used in multiple inheritance
"""

__all__ = ["AttrsPart"]


class AttrsPart(object):
  """
  Implements a new __str__() to print selected attributes.

  Note: when descending from this class, set the attrs class variable.
  """

  # for __str__()
  attrs = None

  def __str__(self):
    assert self.attrs is not None, "Forgot to set attrs class variable"

    s_format = "{:>%d} = {}" % max([len(x) for x in self.attrs])
    s = '' # object.__str__(self)+"\n"
    s = "\n".join([s_format.format(x, self.__getattribute__(x)) for x in self.attrs])
    return s

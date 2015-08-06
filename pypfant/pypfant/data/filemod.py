__all__ = ["FileMod"]

from .inputfile import *
from ..errors import *
from ..misc import *
import struct
import math
import numpy as np


# @todo actually I probably will have to read all the records

class FileMod(InputFile):
  """
  Represents atmospheric model file, e.g. "modeles.mod" (infile:modeles)

  Note: while a infile:modeles may have several models stored in it,
   this class only stores one of these models, specified by "inum"  argument
   of load()

  Imitates the logic of reader_modeles.f90::read_modele().

  Attributes match reader_modeles.f90:modeles_* (minus the "modeles_" prefix)
  """
  default_filename = "modeles.mod"

  attrs = ["ntot", "teff", "glog", "asalog", "asalalf", "nhe", "tit", "tiabs",
           "nh", "teta", "pe", "pg", "t5l"]


  def __init__(self):
    self.ntot = None
    self.teff = None
    self.glog = None
    self.asalog = None
    self.asalalf = None
    self.nhe = None
    self.tit = None
    self.tiabs = None
    self.nh = None
    self.teta = None
    self.pe = None
    self.pg = None
    self.t5l = None

  def load(self, filename, inum=1):
    """
    Arguments:
     filename
     inum -- record number within binary file.
             *note*: unpythonic variable is 1-based to match PFANT inum
             variable. So, *first record is 1*!

    """
    REC_SIZE = 1200




    with open(filename, "rb") as h:
      ostr = struct.Struct('<i 5f 20s 20s')
      h.seek(REC_SIZE*(inum-1))
      x = h.read(REC_SIZE)
      [self.ntot,
       self.teff,
       self.glog,
       self.asalog,
       self.asalalf,
       self.nhe,
       self.tit,
       self.tiabs] = ostr.unpack(x[:64])

      v = np.frombuffer(x, dtype='<f4', count=self.ntot*5, offset=64)
      w = np.reshape(v, (self.ntot, 5))

      self.nh, self.teta, self.pe, self.pg, self.t5l = [w[:, i] for i in range(5)]

  def save(self, filename):
    """Saves to file."""
    raise NotImplementedError("Maybe tomorrow")

  def init_default(self):
    raise RuntimeError("Not applicable")


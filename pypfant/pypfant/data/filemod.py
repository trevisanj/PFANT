__all__ = ["FileMod", "ModRecord"]

from .datafile import *
from ..errors import *
from ..misc import *
from ..parts import *
import struct
import math
import numpy as np
import os

# @todo actually I probably will have to read all the records

class FileMod(DataFile):
  """
  Represents atmospheric model file, e.g. "modeles.mod" (infile:modeles)

  File may have several 1200-byte records.

  Files created by the innermarcs executable (usually named "modeles.mod")

  Imitates the logic of reader_modeles.f90::read_modele().

  Attributes match reader_modeles.f90:modeles_* (minus the "modeles_" prefix)
  """
  default_filename = "modeles.mod"

  attrs = ["records"]

  def __init__(self):
    self.records = None

  def __len__(self):
    if self.records is None:
      raise RuntimeError("File not loaded yet, len() is undefined")
    return len(self.records)

  def _do_load(self, filename):
    REC_SIZE = 1200

    b = os.path.getsize(filename)
    num_rec = b/REC_SIZE-1

    self.records = []

    with open(filename, "rb") as h:
      ostr = struct.Struct('<i 5f 20s 20s')

      for inum in range(1, num_rec+1):
        pos = REC_SIZE*(inum-1)  # position of beginning of record requested

        rec = ModRecord()

        h.seek(pos)
        x = h.read(REC_SIZE)
        [rec.ntot,
         rec.teff,
         rec.glog,
         rec.asalog,
         rec.asalalf,
         rec.nhe,
         rec.tit,
         rec.tiabs] = ostr.unpack(x[:64])

        v = np.frombuffer(x, dtype='<f4', count=rec.ntot*5, offset=64)
        w = np.reshape(v, (rec.ntot, 5))

        rec.nh, rec.teta, rec.pe, rec.pg, rec.t5l = [w[:, i] for i in range(5)]

        self.records.append(rec)

  def save(self, filename):
    """Saves to file."""
    raise NotImplementedError("Maybe tomorrow")

  def init_default(self):
    raise RuntimeError("Not applicable")



















class ModRecord(AttrsPart):
  """
  Represents a single record from an atmospheric model file

  Note: while a infile:modeles may have several 1200-byte records stored in it,
   this class only stores one of these records, specified by "inum"  argument
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



class InvalidRecordError(Exception):
  pass




















class InvalidRecordError(Exception):
  pass



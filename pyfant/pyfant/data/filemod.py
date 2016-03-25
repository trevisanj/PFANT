__all__ = ["FileMod", "ModRecord", "FileMarcs"]
from .datafile import *
from ..misc import *
import struct
import numpy as np
import os


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
    less_attrs = ["ntot", "teff", "glog"]

    def __init__(self):
        AttrsPart.__init__(self)
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

    def __repr__(self):
        return "/"+self.one_liner_str()+"/"

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
    DataFile.__init__(self)
    self.records = None

  def __len__(self):
    if self.records is None:
      raise RuntimeError("File not loaded yet, len() is undefined")
    return len(self.records)

  def _do_load(self, filename):
    REC_SIZE = 1200

    if istextfile(filename):
        raise RuntimeError("File must be binary")

    b = os.path.getsize(filename)

    if b < 2400:
        raise RuntimeError("File too small")

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

        # This routine will read almost any binary, so we perform some range checks
        if not (1 < rec.ntot <= 1000):
            raise RuntimeError("ntot invalid")
        if not (100 < rec.teff < 100000):
            raise RuntimeError("teff invalid")

        v = np.frombuffer(x, dtype='<f4', count=rec.ntot*5, offset=64)
        w = np.reshape(v, (rec.ntot, 5))

        rec.nh, rec.teta, rec.pe, rec.pg, rec.t5l = [w[:, i] for i in range(5)]

        self.records.append(rec)

  def _do_save_as(self, filename):
    """Saves to file."""
    raise NotImplementedError("Maybe tomorrow")

  def init_default(self):
    raise RuntimeError("Not applicable")




class FileMarcs(DataFile):
    """
    Represents *ASCII* file (althought ".mod" extension) from MARCS homepage.

    http://marcs.astro.uu.se/

    This file contains a single atmospheric model for a given
    (teff, glog, asalog)

    """

    attrs = ["record"]

    def __init__(self):
        DataFile.__init__(self)
        self.record = None

    def __len__(self):
        return 1

    def _do_load(self, filename):
        if not istextfile(filename):
            raise RuntimeError("File must be a text file")
        r = ModRecord()
        with open(filename, "r") as h:
            _skip = lambda: h.readline()
            r.tit = struct.unpack("20s", h.readline()[:20])
            r.tiabs = ""
            r.teff = float(struct.unpack("7s", h.readline()[:7])[0])
            _skip()  # flux row
            r.glog = np.log10(float(struct.unpack("12s", h.readline()[:12])[0]))
            _skip()  # microturbulence parameter
            _skip()  # mass
            r.asalog, r.asalalf = map(float, struct.unpack("6s 6s", h.readline()[:12]))
            _skip()  # "1 cm radius for plane-parallel models"
            _skip()  # "Luminosity"
            _skip()  # "convection parameters"
            _skip()  # "X, Y and Z"
            _skip()  # "Logarithmic chemical number abundances, H always 12.00"
            # reads He abundance to calculate "nhe"
            r.nhe = 10**(float(struct.unpack("7x 7s", h.readline()[:14])[0])-12)
            for i in range(9):  # skips 9 rows (file always has 92 abundances)
                _skip()
            n = r.ntot = int(struct.unpack("4s", h.readline()[:4])[0])
            _skip()  # "Model structure"
            _skip()  # header "k lgTauR  lgTau5    Depth     T        Pe          Pg         Prad       Pturb"

            r.nh, r.teta, r.pe, r.pg, r.t5l = np.zeros(n), np.zeros(n), \
             np.zeros(n), np.zeros(n), np.zeros(n)

            for i in range(n):
                # TODO NH
                qwe = h.readline()
                r.t5l[i], t, r.pe[i], r.pg[i] = map(float,
                 struct.unpack("9x 8s 11x 8s 12s 12s", qwe[:60]))
                r.teta[i] = 5040./t

__all__ = ["FileModBin", "ModRecord", "FileModTxt", "FileOpa", "FileMoo",
           "MooRecord"]
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

    attrs = ["ntot", "teff", "glog", "asalog", "asalalf", "nhe", "tit", "tiabs",
           "nh", "teta", "pe", "pg", "log_tau_ross"]
    less_attrs = ["teff", "glog", "asalog"]

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
        self.log_tau_ross = None

    def __repr__(self):
        return "/"+self.one_liner_str()+"/"


MOD_REC_SIZE = 1200


class FileModBin(DataFile):
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

    if is_text_file(filename):
        raise RuntimeError("File must be binary")

    b = os.path.getsize(filename)

    if b < MOD_REC_SIZE:
        raise RuntimeError("File too small")

    num_rec = b/MOD_REC_SIZE-1
    self.records = []
    with open(filename, "rb") as h:

      for inum in range(1, num_rec+1):
        pos = MOD_REC_SIZE*(inum-1)  # position of beginning of record requested
        h.seek(pos)
        x = h.read(MOD_REC_SIZE)
        rec = ModRecord()
        _decode_mod_record(x, rec, inum)
        self.records.append(rec)

  def _do_save_as(self, filename):
      """Saves to file."""

      with open(filename, "wab") as h:
          for i, rec in enumerate(self.records):
              _encode_mod_record(rec, h)
          h.write(struct.pack("<i", 9999))
          h.write("\x00"*1196)

  def init_default(self):
    raise RuntimeError("Not applicable")


class FileModTxt(DataFile):
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
        if not is_text_file(filename):
            raise RuntimeError("File must be a text file")
        r = ModRecord()
        with open(filename, "r") as h:
            _skip = lambda: h.readline()
            r.tit = h.readline().strip()
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

            # Reads hydrogen mass fraction to use later to calculate the nh vector
            # X, Y and Z are the mass fractions of H, He, and metals respectively (X+Y+Z=1)
            h_frac = float(h.readline()[:8])

            _skip()  # "Logarithmic chemical number abundances, H always 12.00"
            # reads He abundance to calculate "nhe"
            r.nhe = 10**(float(struct.unpack("7x 7s", h.readline()[:14])[0])-12)
            for i in range(9):  # skips 9 rows (file always has 92 abundances)
                _skip()
            n = r.ntot = int(struct.unpack("4s", h.readline()[:4])[0])
            _skip()  # "Model structure"
            _skip()  # header "k lgTauR  lgTau5    Depth     T        Pe          Pg         Prad       Pturb"

            r.teta, r.pe, r.pg, r.log_tau_ross = np.zeros(n), np.zeros(n), \
             np.zeros(n), np.zeros(n)

            # reads log(tau(Rosseland)), T, Pe, Pg
            for i in range(n):
                qwe = h.readline()
                r.log_tau_ross[i], t, r.pe[i], r.pg[i] = map(float,
                 struct.unpack("3x 6s 19x 8s 12s 12s", qwe[:60]))
                r.teta[i] = 5040./t

            # reads rhox to use in nh calculation
            _skip()  # hreader "k lgTauR    KappaRoss   Density   Mu      Vconv   Fconv/F      RHOX"
            rhox = np.zeros(n)
            for i in range(n):
                rhox[i] = float(h.readline()[60:72])

            # calculates nh using the formula in "transosmarcsok3.f"
            r.nh = rhox*(6.022142e23*h_frac)


        self.record = r


class FileOpa(DataFile):
    """MARCS ".opa" (opacity model) file format.

    Reference: http://marcs.astro.uu.se
    """

    attrs = ["ndp", "swave", "nwav"]

    def __init__(self):
        DataFile.__init__(self)

        # # Global properties of the opacity model file
        # the 4-byte standard model code 'MRXF'
        self.mcode = None
        # number of depth points (=56)
        self.ndp = None
        # standard wavelength for the continuous optical depth (tau)
        # scale and for the total standard opacity (ops)
        self.swave = None
        # number of wavelengths for which continuous absorption and
        # scattering opacities are given. These are chosen so that
        # linear interpolation should suffice for any wavelength
        # (=1071)
        self.nwav = None
        # wav(j): wavelengths for which opacities are given
        self.wav = None

        # # Model structure
        # rad(k): radius, normalized on the outermost point, k=1. For use with
        # spherical radiative transfer. For plane-parallel models rad == 1.0.
        self.rad = None
        # tau(k): continuumm optical depth at the standard wavelength swave
        self.tau = None
        # t(k): temperature (K)
        self.t = None
        # pe(k): electron pressure (dyn/cm2)
        self.pe = None
        # pg(k)  = total gas pressure (dyn/cm2)
        self.pg = None
        # rho(k) = densigy (g/cm3)
        self.rho = None
        # xi(k)  = microturbulence parameter (km/s)
        self.xi = None
        # ops(k) = continuumm opacity at the standard wavelength (cm2/g)
        self.ops = None

        # # Wavelength-dependent opacities
        # abs(j,k) = specific continuous absorption opacity (cm2/g)
        self.abs = None
        # sca(j,k) = specific continuous scattering opacity (cm2/g)
        self.sca = None


    def cut(self, llzero, llfin):
        """Keeps only region within lambda interval [llzero, llfin]."""
        mask = np.logical_and(llzero <= self.wav, self.wav <= llfin)
        self.sca = self.sca[mask, :]
        self.abs = self.abs[mask, :]
        self.wav = self.wav[mask]
        self.nwav = len(self.wav)

    def _do_load(self, filename):
        """Loads from file.

        Based on http://marcs.astro.uu.se/documents/auxiliary/readopa.f
        """

        with open(filename, "r") as h:
            self.mcode, self.ndp, self.swave = struct.unpack("1x 4s 5s 10s", readline_strip(h))
            if self.mcode != "MRXF":
                # Does not satisfy magic string
                raise RuntimeError("Model code '%s' is not 'MRXF'" % self.mcode)
            self.ndp = int(self.ndp)
            self.swave = float(self.swave)

            self.nwav = int(h.readline())

            v, n_rows = multirow_str_vector(h, self.nwav)
            self.wav = np.array(map(float, v))

            self.rad, self.tau, self.t, self.pe, self.pg, self.rho, self.xi, \
            self.ops = np.zeros(self.ndp), np.zeros(self.ndp), np.zeros(self.ndp), \
                       np.zeros(self.ndp), np.zeros(self.ndp), np.zeros(self.ndp), \
                       np.zeros(self.ndp), np.zeros(self.ndp)
            self.abs = np.zeros((self.nwav, self.ndp))
            self.sca = np.zeros((self.nwav, self.ndp))
            for k in range(self.ndp):
                self.rad[k], self.tau[k], self.t[k], self.pe[k], self.pg[k], \
                self.rho[k], self.xi[k], self.ops[k] = float_vector(h)
                v, n_rows = multirow_str_vector(h, 2*self.nwav)
                abs_sca = np.array(map(float, v))
                # This multiplication is performed as in original readopa.f
                self.abs[:, k] = abs_sca[0::2]*self.ops[k]
                self.sca[:, k] = abs_sca[1::2]*self.ops[k]



class MooRecord(AttrsPart):
    """
    Represents a single record of a ".mog" file

    The attributes are almost a concatenation of ModRecord with FileOpa
    attributes, except for redundant attributes, i.e.:
      - ntot = ndp (ntot kept)
      - pe, pg
    """

    attrs = ["ntot", "teff", "glog", "asalog", "asalalf", "nhe", "tit", "tiabs",
           "nh", "teta", "pe", "pg", "log_tau_ross", "swave", "nwav"]
    less_attrs = ["teff", "glog", "asalog"]

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
        self.log_tau_ross = None

        # These attributes exist in FileOpa as well, please check it for
        # descriptions.
        # Remaining attributes of FileOpa are not relevant
        self.swave = None
        self.nwav = None
        self.wav = None
        self.ops = None
        self.abs = None
        self.sca = None

    def __repr__(self):
        return "/"+self.one_liner_str()+"/"


    def from_marcs_files(self, file_mod, file_opa):
        """Copies attributes from a FileModTxt and a FileOpa."""

        aa = ["ntot", "teff", "glog", "asalog", "asalalf", "nhe", "tit",
              "tiabs", "nh", "teta", "pe", "pg", "log_tau_ross"]
        rec = file_mod.record
        for a in aa:
            self.__setattr__(a, rec.__getattribute__(a))
        aa = ["swave", "nwav", "wav", "ops", "abs", "sca"]
        for a in aa:
            self.__setattr__(a, file_opa.__getattribute__(a))



# Record size is record size in modeles.mod plus opacity part
# 13432 = 4+4+     (swave, nwav)
#         56*4     (ops)
#         1071*(1+2*56)*4 (wav, abs, sca)
OPA_REC_SIZE = 484324
MOG_REC_SIZE = MOD_REC_SIZE+OPA_REC_SIZE
# Number of layers and wavelengths must have these fixed values because
# the ".mog" file I/O operations both in Python and Fortran are assuming thi.
# THe main reason for the assumption is that Fortran reads much faster when
# the READ() statement reads into whole array instead of implied DO.
#
# These fixed values seem to be part of the file specification anyway.
NTOT = 56    # must be 56
NWAV = 1071  # must be 1071

class FileMoo(DataFile):
    """
    Represents a ".moo" (model, including opacities) file.

    This file contains all the fields in modeles.mod, plus the opacity information
    """
    default_filename = "grid.moo"

    attrs = ["records"]

    def __init__(self):
        DataFile.__init__(self)
        self.records = None

    def __len__(self):
        if self.records is None:
            raise RuntimeError("File not loaded yet, len() is undefined")
        return len(self.records)

    def _do_load(self, filename):

        if is_text_file(filename):
            raise RuntimeError("File must be binary")

        b = os.path.getsize(filename)

        if b % MOG_REC_SIZE != 0:
            raise RuntimeError("Incorrect file size! Must be a multiple of %d" %
                               MOG_REC_SIZE)

        num_rec = b/MOG_REC_SIZE
        self.records = []
        with open(filename, "rb") as h:
            for inum in range(1, num_rec+1):
                pos = MOG_REC_SIZE*(inum-1)  # position of beginning of record requested
                h.seek(pos)
                x = h.read(MOG_REC_SIZE)
                rec = MooRecord()
                _decode_mod_record(x, rec, inum)

                if rec.ntot != NTOT:
                    raise RuntimeError("Number of layers be %d, not %d" %
                                       (NTOT, rec.ntot))

                h.seek(pos+MOD_REC_SIZE)
                x = h.read(MOG_REC_SIZE)

                ostr = struct.Struct('<f i')
                [rec.swave,
                 rec.nwav] = ostr.unpack(x[:8])

                if rec.nwav != NWAV:
                    raise RuntimeError("Number of wavelengths must be %d, not %d" %
                                       (NWAV, rec.nwav))

                rec.ops = np.frombuffer(x, dtype='<f4', count=rec.ntot,
                                        offset=8)
                rec.wav = np.frombuffer(x, dtype='<f4', count=rec.nwav,
                                        offset=8+rec.ntot*4)
                rec.abs = np.frombuffer(x, dtype='<f4', count=rec.nwav*rec.ntot,
                 offset=8+(rec.ntot+rec.nwav)*4).reshape((rec.nwav, rec.ntot)).T
                rec.sca = np.frombuffer(x, dtype='<f4', count=rec.nwav*rec.ntot,
                 offset=8+(rec.ntot+2*rec.nwav)*4).reshape((rec.nwav, rec.ntot)).T
                self.records.append(rec)

    def _do_save_as(self, filename):
        """Saves to file."""

        with open(filename, "wab") as h:
            for i, rec in enumerate(self.records):
                assert rec.ntot == NTOT, \
                    "Number of layers be %d, not %d" % (NTOT, rec.ntot)
                assert rec.nwav == NWAV, \
                    "Number of wavelengths must be %d, not %d" % (NWAV, rec.nwav)

                _encode_mod_record(rec, h)

                ostr = struct.Struct('<f i')
                h.write(ostr.pack(rec.swave, rec.nwav))
                h.write(struct.pack("<"+"f"*rec.ntot, *rec.ops))
                s_temp ="<"+"f"*(rec.nwav*rec.ntot)


                h.write(struct.pack("<"+"f"*rec.nwav, *rec.wav))
                h.write(struct.pack(s_temp, *rec.abs.T.flatten()))
                h.write(struct.pack(s_temp, *rec.sca.T.flatten()))


    def init_default(self):
        raise RuntimeError("Not applicable")


_ostr = struct.Struct('<i 5f 20s 20s')  # header

def _decode_mod_record(x, rec, inum):
    """Decodes "x", binary record as in modeles.mod into "rec" attributes.
    Argument "inum" (record number) is for error reporting."""
    assert isinstance(rec, (ModRecord, MooRecord))
    [rec.ntot,
     rec.teff,
     rec.glog,
     rec.asalog,
     rec.asalalf,
     rec.nhe,
     rec.tit,
     rec.tiabs] = _ostr.unpack(x[:64])

    # This routine will read almost any binary, so we perform some range checks
    if not (1 < rec.ntot <= 1000):
        raise RuntimeError("record #%d: ntot invalid: %d" % (inum, rec.ntot))
    if not (100 < rec.teff < 100000):
        raise RuntimeError("record #%d: teff invalid %g" % (inum, rec.teff))

    v = np.frombuffer(x, dtype='<f4', count=rec.ntot*5, offset=64)
    w = np.reshape(v, (rec.ntot, 5))

    rec.nh, rec.teta, rec.pe, rec.pg, rec.log_tau_ross = [w[:, i] for i in range(5)]

def _encode_mod_record(rec, h):
    """Encodes record into open file."""
    assert isinstance(rec, (ModRecord, MooRecord))
    h.write(_ostr.pack(rec.ntot, rec.teff, rec.glog, rec.asalog,
                       rec.asalalf, rec.nhe, rec.tit, rec.tiabs))
    ny = 5 * rec.ntot
    y = np.reshape(np.vstack([rec.nh, rec.teta, rec.pe, rec.pg, rec.log_tau_ross]).T,
                   ny)
    h.write(struct.pack("<" + "f" * ny, *y))
    h.write("\x00" * (MOD_REC_SIZE-ny*4-64))  # fills record with \x0 to have 1200 bytes

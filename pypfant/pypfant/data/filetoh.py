__all__ = ["FileToH"]

from .inputfile import *
from ..errors import *
from ..misc import *
import struct
import math
import numpy as np
import fortranformat as ff

class FileToH(InputFile):
  """
  Represents single-band hydrogen lines file.

  Imitates the logic of reader_filetoh.f90::read_filetoh(). Difference: pfant
  reads second row as single character-variable "ttt", but hydro2 saves 5 values
  in second row. I am reading these values.

  Attributes match reader_filetoh.f90:filetoh_r_* (minus the "filetoh_r_" prefix)
  """
  default_filename = "thalpha"

  attrs = ["titre", "ntot", "zut1", "zut2", "zut3", "th", "lambdh", "jmax"]

  def __init__(self):
    self.titre = None

    # second row
    self.ntot = None
    self.zut1 = None
    self.zut2 = None
    self.zut3 = None

    # will be NumPy matrix [jmax]x[modeles_ntot]
    self.th = None
    self.lambdh = None
    self.jmax = None


  def load(self, filename):
    with open(filename, "r") as h:
      #-- row 1
      self.titre = h.readline().strip('\n')

      #-- row 2
      fr = ff.FortranRecordReader('(I6,2F6.1,2X,2F8.4)')
      vars = fr.read(h.readline())
      [self.ntot, self.zut1, _, self.zut2, self.zut3] = vars

      #-- row 3
      self.jmax = int(h.readline())


      #-- lambda vector
      #-- Reads sequence of jmax float numbers of 14 characters each disposed in
      #-- rows of 5 columns or less
      # function to calculate number of 5-column rows needed to store a sequence
      num_rows = lambda x: int(math.ceil(float(x)/5))
      C = 14  # size of stored float value in characters
      nr = num_rows(self.jmax)
      self.lambdh = []
      for i in range(nr):
        self.lambdh.extend([float(x) for x in chunk_string(readline_strip(h), C)])

      if not (len(self.lambdh) == self.jmax):
        raise FileConsistencyError("len(lambdh)=%d should be %d" % (len(self.lambdh), self.jmax))

      #-- (lambda) x (atmospheric layer) matrix
      v = []  # Will accumulate values for future reshape
      C = 12  # size of stored float value in characters
      for s in h.readlines():
        s = s.strip('\n')
        v.extend([float(x) for x in chunk_string(s, C)])
      if len(v)/self.jmax != self.ntot:
        raise FileConsistencyError("Should have found %d values for th matrix (found %d)" %
                                   (self.jmax*self.ntot, len(v)))
      self.th = np.reshape(v, (self.jmax, self.ntot)).T

  def save(self, filename):
    """Saves to file."""
    raise NotImplementedError("Maybe tomorrow")

  def init_default(self):
    raise RuntimeError("Not applicable")


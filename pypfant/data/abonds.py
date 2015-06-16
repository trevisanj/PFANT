__all__ = ["abonds"]

import struct
from .input_structure import *

from misc import *


class abonds(input_structure):
    default_filename = "abonds.dat"

    def __init__(self):
        self.ele = []   ## list of atomic symbols
        self.abol = []  ## corresponding abundances

    def __len__(self):
        """Returns length of "ele" attribute."""
        return len(self.ele)

    def load(self, filename):
        """Clears internal lists and loads from file."""
        self.abol, self.ele = [], []

        ostr = struct.Struct("1x 2s 6s")
        with open(filename, "r") as h:
            for s in h:
                if len(s) > 0:
                    if s[0] == "1":  # sign to stop reading file
                        break
                [ele, abol] = ostr.unpack_from(s)
                self.ele.append(ele)
                self.abol.append(float(abol))

    def save(self, filename):
        """Saves current lists to file."""
        with open(filename, "w") as h:
            h.writelines([' %-2s%6.2f\n' % (self.ele[i], self.abol[i])
                          for i in xrange(len(self))])
            h.writelines(['1\n', '1\n'])

    def init_default(self):
        """Initializes with default abundances (which are hard-coded)."""
        fullpath = path_to_default("abonds.dat")
        self.load(fullpath)

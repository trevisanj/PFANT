__all__ = ["FileAbonds"]

import struct
from .datafile import *
from ..misc import adjust_atomic_symbol
import re


class FileAbonds(DataFile):
    default_filename = "abonds.dat"
    attrs = ["ele", "abol"]

    def __init__(self):
        self.ele = []   ## list of atomic symbols
        self.abol = []  ## corresponding abundances

    def __len__(self):
        """Returns length of "ele" attribute."""
        return len(self.ele)

    def _do_load(self, filename):
        """Clears internal lists and loads from file."""
        self.abol, self.ele = [], []

        ostr = struct.Struct("1x 2s 6s")
        with open(filename, "r") as h:
            for s in h:
                if len(s) > 0:
                    if s[0] == "1":  # sign to stop reading file
                        break
                [ele, abol] = ostr.unpack_from(s)

                if not re.search(r'[a-z]', ele, re.IGNORECASE):
                    raise RuntimeError("Invalid element symbol: '%s'" % ele.strip())

                self.ele.append(adjust_atomic_symbol(ele))
                self.abol.append(float(abol))

    def _do_save_as(self, filename):
        with open(filename, "w") as h:
            h.writelines([' %-2s%6.2f\n' % (self.ele[i], self.abol[i])
                          for i in xrange(len(self))])
            h.writelines(['1\n', '1\n'])


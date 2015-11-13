__all__ = ["FileAtoms", "Atom", "AtomicLine"]

from .datafile import *
from ..misc import *
from ..parts import *
from ..errors import *
import struct
import logging
import sys
import numpy as np

_logger = logging.getLogger(__name__)
_logger.addHandler(logging.NullHandler())

class Atom(PyfantObject):
    """
    Represents element with its atomic lines.

    Atom is identified by key symbol+ionization
    """
    attrs = ["elem", "ioni"]

    # Properties that iterate through the AtomicLine objects to mount vectors
    @property
    def lambda_(self):
        return np.array([x.lambda_ for x in self.lines])
    @property
    def kiex(self):
        return np.array([x.kiex for x in self.lines])
    @property
    def algf(self):
        return np.array([x.algf for x in self.lines])
    @property
    def ch(self):
        return np.array([x.ch for x in self.lines])
    @property
    def gr(self):
        return np.array([x.gr for x in self.lines])
    @property
    def ge(self):
        return np.array([x.ge for x in self.lines])
    @property
    def zinf(self):
        return np.array([x.zinf for x in self.lines])
    @property
    def abondr(self):
        return np.array([x.abondr for x in self.lines])


    def __init__(self):
        AttrsPart.__init__(self)
        self.lines = []  # list of AtomicLine
        self.elem = None
        self.ioni = None

    def __len__(self):
        return len(self.lines)

    def __str__(self):
        return "%s%s" % (self.elem, self.ioni)

    def __repr__(self):
        return "'%s%s'" % (self.elem, self.ioni)


class AtomicLine(AttrsPart):
    attrs = ["lambda_", "kiex", "algf", "ch", "gr", "ge", "zinf", "abondr"]

    def __init__(self):
        AttrsPart.__init__(self)

        # all scalars
        self.lambda_ = None
        self.kiex = None
        self.algf = None
        self.ch = None
        self.gr = None
        self.ge = None
        self.zinf = None
        self.abondr = None

    def filter(self, lzero, lfin):
        """Reduces the number of lines to only the ones whose lambda is inside [lzero, lfin]"""
        raise NotImplementedError()


class FileAtoms(DataFile):
    """
    Represents file with atomic lines.
    """

    default_filename = "atomgrade.dat"
    attrs = ["atoms"]

    def __len__(self):
        """Length of FileAtoms object is defined as number of elements."""
        return len(self.atoms)

    def __init__(self):
        DataFile.__init__(self)

        # list of Atom objects
        self.atoms = []


    def _do_load(self, filename):
        """Clears internal lists and loads from file."""

        with open(filename, "r") as h:

            r = 0 # counts rows of file
            edict = {}  # links atomic symbols with Atom objects created (key is atomic symbol)
            try:
                while True:
                    a = AtomicLine()

                    # (EE)(I) --whitespace-- (float) --ignored...--
                    temp = str_vector(h)
                    elem, s_ioni = temp[0][:-1], temp[0][-1]
                    a.lambda_ = float(temp[1])
                    elem = adjust_atomic_symbol(elem)
                    key = elem+s_ioni  # will group elements by this key
                    if edict.has_key(key):
                        e = edict[key]
                    else:
                        e = edict[key] = Atom()
                        e.elem = elem
                        e.ioni = int(s_ioni)
                        self.atoms.append(e)
                    e.lines.append(a)
                    r += 1

                    [a.kiex, a.algf, a.ch, a.gr, a.ge, a.zinf, a.abondr, finrai] = \
                        float_vector(h)
                    r += 1
                    # last element is end-of-file flag "finrai"
                    if finrai == 1:
                        break

            except Exception as a:
                raise type(a)(("Error around %d%s row of file '%s'" %
                    (r+1, ordinal_suffix(r+1), filename))+": "+str(a)), None, sys.exc_info()[2]

    def _do_save_as(self, filename):
        with open(filename, "w") as h:
            n = len(self.atoms)
            for i, e in enumerate(self.atoms.values()):
                p = len(e)
                for j, a in enumerate(e.lines):
                    finrai = 1 if i == n-1 and j == p-1 else 0
                    write_lf(h, "%2s%1d %10.3f" % (e.elem, e.ioni, a.lambda_))
                    write_lf(h, "%8.3f %8.3f %8.3f %8.3f %8.3f %6.1f %3.1f %1d" % \
                        (a.kiex, a.algf, a.ch, a.gr, a.ge, a.zinf, a.abondr, finrai))

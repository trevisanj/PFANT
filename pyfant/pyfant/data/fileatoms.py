__all__ = ["FileAtoms", "Element", "AtomicLine"]

from .datafile import *
from ..misc import *
from ..parts import *
from ..errors import *
import struct
import logging
import sys

logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())

# class SetOfLines(object):
#     def __init__(self):
#         self.lmbdam = None
#         self.sj = None
#         self.jj = None
#
#     """Represents one "set of lines" for one molecule."""
#     def __len__(self):
#         return len(self.lmbdam)

class Element(AttrsPart):
    attrs = ["elem"]

    def __init__(self):
        AttrsPart.__init__(self)
        self.lines = []
        self.elem = None

    def __len__(self):
        return len(self.lines)

class AtomicLine(AttrsPart):
    attrs = ["ioni", "lambda_", "kiex", "algf", "ch", "gr", "ge", "zinf", "abondr"]

    def __init__(self):
        AttrsPart.__init__(self)

        # all scalars
        self.ioni = None
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
    Represents file with molecular lines.

    Rather than as read_Atoms() in readers.f90, this class stores
    information for each molecule inside a Molecule object.
    """

    default_filename = "atomgrade.dat"


    def __len__(self):
        """Length of FileAtoms object is defined as number of elements."""
        return len(self.elements)

    def __init__(self):
        DataFile.__init__(self)

        # list of Element objects
        self.elements = []


    def _do_load(self, filename):
        """Clears internal lists and loads from file."""

        with open(filename, "r") as h:

            r = 0 # counts rows of file
            edict = {}  # links atomic symbols with Element objects created (key is atomic symbol)
            try:
                while True:
                    a = AtomicLine()

                    # (EE)(I) --whitespace-- (float) --ignored...--
                    temp = str_vector(h)
                    elem, a.ioni = temp[0][:-1], int(temp[0][-1])
                    a.lambda_ = float(temp[1])
                    elem = adjust_atomic_symbol(elem)
                    if edict.has_key(elem):
                        e = edict[elem]
                    else:
                        e = edict[elem] = Element()
                        e.elem = elem
                        self.elements.append(e)
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
            n = len(self.elements)
            for i, e in enumerate(self.elements.values()):
                p = len(e)
                for j, a in enumerate(e.lines):
                    finrai = 1 if i == n-1 and j == p-1 else 0
                    write_lf(h, "%2s%1d %10.3f" % (e.elem, a.ioni, a.lambda_))
                    write_lf(h, "%8.3f %8.3f %8.3f %8.3f %8.3f %6.1f %3.1f %1d" % \
                        (a.kiex, a.algf, a.ch, a.gr, a.ge, a.zinf, a.abondr, finrai))

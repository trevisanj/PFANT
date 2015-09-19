__all__ = ["FileMolecules", "Molecule"]

from .datafile import *
from ..misc import *
from ..parts import *
from ..errors import *
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


class Molecule(AttrsPart):
    attrs = ["titulo", "fe", "do", "mm", "am", "bm", "ua", "ub",
             "te", "cro", "s", "nv", "lines_total", "qqv", "ggv", "bbv", "ddv", "fact"]

    @property
    def nv(self):
        return len(self)

    @property
    def lines_total(self):
        ret = sum(map(len, self.lmbdam))
        return ret

    def __init__(self):
        AttrsPart.__init__(self)
        
        self.titulo = None
        self.fe = None
        self.do = None
        self.mm = None
        self.am = None
        self.bm = None
        self.ua = None
        self.ub = None
        self.te = None
        self.cro = None
        self.s = None

        self.qqv = None
        self.ggv = None
        self.bbv = None
        self.ddv = None
        self.fact = None

        # ** Sets of lines: list of list
        # ** first dimension is the set-of-lines
        # ** second dimension is the line information
        self.lmbdam = []
        self.sj = []
        self.jj = []

    def __len__(self):
        """Returns number of set-of-lines."""
        return len(self.lmbdam)



class FileMolecules(DataFile):
    """
    Represents file with molecular lines.

    Rather than as read_molecules() in readers.f90, this class stores
    information for each molecule inside a Molecule object.
    """

    default_filename = "moleculagrade.dat"

    attrs = ["titm", "number", "lines_total"]


    @property
    def lines_total(self):
        """Total number of spectral line, counting all molecules."""
        return sum(map(lambda x: x.lines_total, self.molecules))

    def __init__(self):
        DataFile.__init__(self)

        # Array of Molecule objects
        self.molecules = []

        # Integer number in first row of file.Specifies how many molecules to use
        self.number = None
        self.titm = None

    def __len__(self):
        return len(self.molecules)


    def _do_load(self, filename):
        """Clears internal lists and loads from file."""

        with open(filename, "r") as h:
            r = 0 # counts rows of file
            try:
                self.number = int(h.readline())
                r += 1
                self.titm = readline_strip(h)
                r += 1

                nv = int_vector(h)  # number of transitions=sets-of-lines for each molecule
                r += 1
                # Uses length of nv vector to know how many molecules to read (ignored self.number)
                nm = len(nv)

                for im in range(nm):
                    nvi = nv[im]

                    m = Molecule()
                    self.molecules.append(m)

                    m.titulo = readline_strip(h)
                    logger.debug('Reading %d%s molecule \'%s\'' % (im+1, ordinal_suffix(im+1), m.titulo))
                    r += 1
                    m.fe, m.do, m.mm, m.am, m.bm, m.ua, m.ub, m.te, m.cro = float_vector(h)
                    r += 1

                    h.readline()  # Skips line which is blank in file
                                  # In readers.f90 the variables are ise, a0, a1, a2 a3, a4, als
                                  # but the pfant does not use them.
                    r += 1

                    m.s = float(h.readline())
                    r += 1

                    # These vectors must have nvi elements
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    m.qqv = map(float, s_v)
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    m.ggv = map(float, s_v)
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    m.bbv = map(float, s_v)
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    m.ddv = map(float, s_v)
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    m.fact = map(float, s_v)
                    for name in ["qqv", "ggv", "bbv", "ddv", "fact"]:
                        v = m.__getattribute__(name)
                        if len(v) != nvi:
                            raise FileConsistencyError(
                                'Attribute %s of molecule #%d must be a vector with %d elements (has %d)' %
                                (name, im+1, nvi, len(v)))

                    # Now reads lines
                    i_sol = 0  # index of current set-of-lines, must go from 0 to nvi-1
                    v_lmbdam, v_sj, v_jj = [], [], []
                    while True:
                        # Someone added "*" signs as a 6th column of some lines
                        # which was causing my reading to crash.
                        # Therefore I read the line and discard beyond the 5th column before
                        # converting to float
                        temp = str_vector(h)
                        temp = temp[:5]

                        lmbdam, sj, jj, iz, numlin = map(float, temp)
                        r += 1

                        v_lmbdam.append(lmbdam)
                        v_sj.append(sj)
                        v_jj.append(jj)

                        if numlin > 0:
                            m.lmbdam.append(v_lmbdam)
                            m.sj.append(v_sj)
                            m.jj.append(v_jj)
                            v_lmbdam, v_sj, v_jj = [], [], []

                            if numlin == 9:
                                break

                            i_sol += 1

                    if im+1 == nm:
                        break

                    im += 1
            except Exception as e:
                raise type(e)(("Error around %d%s row of file '%s'" % (r+1, ordinal_suffix(r+1), filename))+": "+str(e)), None, sys.exc_info()[2]

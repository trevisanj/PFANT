__all__ = ["FileMolecules", "Molecule", "SetOfLines"]
from .datafile import *
from ..misc import *
from ..errors import *
import logging
import sys
import numpy as np
_logger = logging.getLogger(__name__)
_logger.addHandler(logging.NullHandler())


class SetOfLines(AttrsPart):
    attrs = ["qqv", "ggv", "bbv", "ddv", "fact", "num_lines"]


    def __init__(self):
        AttrsPart.__init__(self)

        self.qqv = None
        self.ggv = None
        self.bbv = None
        self.ddv = None
        self.fact = None

        self.lmbdam = []
        self.sj = []
        self.jj = []

    @property
    def num_lines(self):
        return len(self)

    def __len__(self):
        return len(self.lmbdam)

    def cut(self, lzero, lfin):
        """Reduces the number of lines to only the ones whose lmbdam is inside [lzero, lfin]"""
        l, s, j = [], [], []
        for _l, _s, _j in zip(self.lmbdam, self.sj, self.jj):
            if lzero <= _l <= lfin:
                l.append(_l)
                s.append(_s)
                j.append(_j)
        self.lmbdam, self.sj, self.jj = l, s, j


class Molecule(AttrsPart):
    attrs = ["titulo", "fe", "do", "mm", "am", "bm", "ua", "ub",
             "te", "cro", "s", "nv", "num_lines"]

    @property
    def nv(self):
        return len(self.sol)

    @property
    def num_lines(self):
        ret = sum(map(len, self.sol))
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

        self.sol = []  # list of SetOfLines objects


    def __len__(self):
        """Returns number of set-of-lines."""
        return len(self.sol)

    def cut(self, lzero, lfin):
        """Reduces the number of lines to only the ones whose lmbdam is inside [lzero, lfin]"""

        for set in self.sol:
            set.cut(lzero, lfin)


class FileMolecules(DataFile):
    """
    Represents file with molecular lines.

    Rather than as read_molecules() in readers.f90, this class stores
    information for each molecule inside a Molecule object.
    """

    default_filename = "molecules.dat"
    attrs = ["titm", "num_lines"]

    @property
    def num_lines(self):
        """Total number of spectral line, counting all molecules."""
        return sum(map(lambda x: x.num_lines, self.molecules))

    @property
    def lmbdam(self):
        return np.hstack([np.hstack([x.lmbdam for x in m.sol]) for m in self.molecules])

    @property
    def sj(self):
        return np.hstack([np.hstack([x.sj for x in m.sol]) for m in self.molecules])

    @property
    def jj(self):
        return np.hstack([np.hstack([x.jj for x in m.sol]) for m in self.molecules])

    def __init__(self):
        DataFile.__init__(self)

        # Array of Molecule objects
        self.molecules = []

        # Integer number in first row of file.Specifies how many molecules to use
        self.titm = None

    def __len__(self):
        return len(self.molecules)

    def _do_load(self, filename):
        """Clears internal lists and loads from file."""

        with open(filename, "r") as h:
            r = 0 # counts rows of file
            try:
                number = int(h.readline())  # not used (see below)
                r += 1
                self.titm = readline_strip(h)
                r += 1

                nv = int_vector(h)  # number of transitions=sets-of-lines for each molecule
                r += 1
                # Uses length of nv vector to know how many molecules to read (ignores "number")
                nm = len(nv)

                for im in range(nm):
                    nvi = nv[im]

                    m = Molecule()
                    self.molecules.append(m)

                    m.titulo = readline_strip(h)
                    _logger.debug('Reading %d%s molecule \'%s\'' % (im+1, ordinal_suffix(im+1), m.titulo))
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
                    qqv = map(float, s_v)
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    ggv = map(float, s_v)
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    bbv = map(float, s_v)
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    ddv = map(float, s_v)
                    s_v, r_inc = multirow_str_vector(h, nvi, r)
                    r += r_inc
                    fact = map(float, s_v)
                    for name in ["qqv", "ggv", "bbv", "ddv", "fact"]:
                        v = eval(name)
                        if len(v) != nvi:
                            raise FileConsistencyError(
                                'Attribute %s of molecule #%d must be a vector with %d elements (has %d)' %
                                (name, im+1, nvi, len(v)))

                    # creates sets of lines and appends to molecule
                    for q, g, b, d, f in zip(qqv, ggv, bbv, ddv, fact):
                        o = SetOfLines()
                        o.qqv = q
                        o.ggv = g
                        o.bbv = b
                        o.ddv = d
                        o.fact = f
                        m.sol.append(o)

                    # Now reads lines
                    sol_iter = iter(m.sol)  # iterator to change the current set-of-lines with the "numlin" flag
                    o = sol_iter.next()  # current set-of-lines
                    while True:
                        # Someone added "*" signs as a 6th column of some lines
                        # which was causing my reading to crash.
                        # Therefore I read the line and discard beyond the 5th column before
                        # converting to float
                        temp = str_vector(h)
                        temp = temp[:5]

                        lmbdam, sj, jj, iz, numlin = map(float, temp)
                        r += 1

                        o.lmbdam.append(lmbdam)
                        o.sj.append(sj)
                        o.jj.append(jj)

                        if numlin > 0:
                            if numlin == 9:
                                break
                            o = sol_iter.next()

                    if im+1 == nm:
                        break

                    im += 1
            except Exception as e:
                raise type(e)(("Error around %d%s row of file '%s'" % (r+1, ordinal_suffix(r+1), filename))+": "+str(e)), None, sys.exc_info()[2]

    def cut(self, lzero, lfin):
        """Reduces the number of lines to only the ones whose lmbdam is inside [lzero, lfin]"""

        for m in self.molecules:
            m.cut(lzero, lfin)

    def _do_save_as(self, filename):
        with open(filename, "w") as h:
            write_lf(h, str(len(self.molecules)))
            write_lf(h, self.titm)
            write_lf(h, " ".join([str(x.nv) for x in self.molecules]))
            for m in self.molecules:
                assert isinstance(m, Molecule)
                write_lf(h, m.titulo)
                write_lf(h, (" ".join(["%.10g"]*9)) % (m.fe, m.do, m.mm, m.am,
                    m.bm, m.ua, m.ub, m.te, m.cro))
                write_lf(h, "")
                write_lf(h, str(m.s))
                write_lf(h, " ".join([str(x.qqv) for x in m.sol]))
                write_lf(h, " ".join([str(x.ggv) for x in m.sol]))
                write_lf(h, " ".join([str(x.bbv) for x in m.sol]))
                write_lf(h, " ".join([str(x.ddv) for x in m.sol]))
                write_lf(h, " ".join([str(x.fact) for x in m.sol]))

                num_sol = len(m.sol)
                for i, s in enumerate(m.sol):
                    assert isinstance(s, SetOfLines)
                    num_lines = len(s)  # number of lines for current set-of-lines
                    for j in xrange(num_lines):
                        numlin = 0 if j < num_lines-1 else 9 if i == num_sol-1 else 1
                        write_lf(h, "%.10g %.10g %.10g 0 %d" % (s.lmbdam[j], s.sj[j], s.jj[j], numlin))

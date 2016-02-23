__all__ = ["FileAbonds"]

import struct
from .datafile import *
from ..misc import adjust_atomic_symbol
import re
from . import FileDissoc
from ..misc import SYMBOLS


class FileAbonds(DataFile):
    default_filename = "abonds.dat"
    attrs = ["ele", "abol", "notes"]

    def __init__(self):
        DataFile.__init__(self)
        self.ele = []   # list of atomic symbols
        self.abol = []  # corresponding abundances
        self.notes = []  # ignored by pfant

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
                [ele, abol, notes] = s[1:3], s[3:9], s[10:]

                if not re.search(r'[a-z]', ele, re.IGNORECASE):
                    raise RuntimeError("Invalid element symbol: '%s'" % ele.strip())

                self.ele.append(adjust_atomic_symbol(ele))
                self.abol.append(float(abol))
                self.notes.append(notes.strip())

    def make_dissoc(self):
        """Creates a new FileDissoc object.

        To do so, it loads the default dissoc.dat file from pyfant/data/default
        directory and searches for the new abundances within self using the
        element symbol as key.

        The abundance values are "corrected" by subtracting 12.

        If element is not found in self, list.index() raises.
        """

        f = FileDissoc()
        f.init_default()
        for i, elem in enumerate(f.elems):
            j = self.ele.index(elem)
            f.cclog[i] = self.abol[j]-12
        return f

    def sort_a(self):
        """Sorts alphabetically using self.ele.

        Symbols not found in the periodic table will appear first and  orderered
        alphabetically.
        """

        indexes = sorted(range(len(self)), key=lambda k: self.ele[k].strip())
        self.ele = [self.ele[i] for i in indexes]
        self.abol = [self.abol[i] for i in indexes]
        self.notes = [self.notes[i] for i in indexes]

    def sort_z(self):
        """
        Sorts by atomic number.

        Symbols not found in the periodic table will appear first.

        Returns: list with those symbols not found in the periodic table.
        """

        # first determines the atomic numbers of the elements
        sort_keys = []
        not_found = []
        for symbol in self.ele:
            s = symbol.strip()
            try:
                sort_keys.append("%3d" % SYMBOLS.index(s))
            except ValueError:
                print "NOT FOUND", s
                sort_keys.append("    "+s)
                not_found.append(symbol)

        indexes = sorted(range(len(sort_keys)), key=lambda k: sort_keys[k])
        self.ele = [self.ele[i] for i in indexes]
        self.abol = [self.abol[i] for i in indexes]
        self.notes = [self.notes[i] for i in indexes]

        return not_found


    # def make_dissoc(self):
    #     """Creates a new FileDissoc object.
    #
    #     Returns (file_dissoc, log), where
    #      - file_dissoc is a FileDissoc instance,
    #      - log is a list with this structure:
    #        [(element, dissoc abundance, message string), ...]
    #        if all atomic symbols are found in self, log will be an empty list.
    #        Otherwise it will contain the registry of the elements not found
    #        in self.
    #
    #     To do so, it loads the default dissoc.dat file from pyfant/data/default
    #     directory and searches for the new abundances within self using the
    #     element symbol as key.
    #
    #     The abundance values are "corrected" by subtracting 12.
    #     """
    #
    #     f, log = FileDissoc(), []
    #     f.init_default()
    #     for i, elem in enumerate(f.elems):
    #         try:
    #             j = self.ele.index(elem)
    #             f.cclog[i] = self.abol[j]-12
    #         except ValueError:
    #             log.append((elem, f.cclog[i],
    #              "Element \"%s\", kept original %g" % (elem, f.cclog[i])))
    #     return f, log


    def _do_save_as(self, filename):
        with open(filename, "w") as h:
            h.writelines([' %-2s%6.2f %s\n' % (self.ele[i], self.abol[i], self.notes[i])
                          for i in xrange(len(self))])
            h.writelines(['1\n', '1\n'])


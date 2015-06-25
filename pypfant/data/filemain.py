__all__ = ["FileMain"]

from .inputfile import *
from ..errors import *
from ..misc import *


class FileMain(InputFile):
    """
    Represents PFANT main configuration file.

    Imitates the logic of read_most_files.f90::read_main().

    Attributes match read_most_files.f90::main_* (minus the "main_" prefix)
    """
    default_filename = "main.dat"

    def __init__(self):
        self.titrav = None
        self.ecrit = None
        self.pas = None
        self.echx = None
        self.echy = None
        self.fwhm = None
        self.ivtot = None
        self.vvt = None
        self.tolv = None
        self.teff = None
        self.glog = None
        self.asalog = None
        self.nhe = None
        self.inum = None
        self.ptdisk = None
        self.mu = None
        self.afstar = None
        self.xxcor = None
        self.fileflux = None
        self.llzero = None
        self.llfin = None
        self.aint = None
        self.filetohy = None

    def __str__(self):
        map_ = ["titrav", "ecrit", "pas", "echx", "echy", "fwhm", "ivtot", "vvt", "tolv",
               "teff", "glog", "asalog", "nhe", "inum", "ptdisk", "mu", "afstar",
               "xxcor", "fileflux", "llzero", "llfin", "aint", "filetohy"]
        s_format = "{:>%d} = {}" % max([len(x) for x in map_])
        s = InputFile.__str__(self)+"\n"+\
            "\n".join([s_format.format(x, self.__getattribute__(x)) for x in map_])
        return s


    def load(self, filename):
        """Loads from file."""

        with open(filename, "r") as h:
            self.titrav = h.readline().strip('\n')
            self.ecrit, self.pas, self.echx, self.echy, self.fwhm = str_vector(h)

            vvt = float(h.readline())
            self.ivtot = 1
            if vvt <= 900:
                self.vvt = [vvt]
            else:
                self.ivtot = int(h.readline())
                self.tolv = float_vector(h)
                self.vvt = float_vector(h)

                if not (self.ivtot == len(self.tolv) == len(vvt)):
                    raise FileConsistencyError("ivtot must match len(tolv) must match len(vvt)")

            self.teff, self.glog, self.asalog, self.nhe, self.inum = str_vector(h)
            self.ptdisk, self.mu = str_vector(h)
            self.afstar = float(h.readline())
            self.xxcor = float_vector(h)
            self.fileflux = h.readline().strip()
            self.llzero, self.llfin, self.aint = float_vector(h)

            self.filetohy = []
            for s in h.readlines():
                s = s.strip()
                if len(s) > 0:
                    self.filetohy.append(s)

        # remaining conversions
        self.ecrit, self.ptdisk = map(str2bool, (self.ecrit, self.ptdisk))
        self.pas, self.echx, self.echy, self.fwhm, self.teff, self.glog, \
        self.asalog, self.nhe, self.mu = \
            map(float, (self.pas, self.echx, self.echy, self.fwhm, self.teff,
                        self.glog, self.asalog, self.nhe, self.mu))
        self.inum = int(self.inum)

    def save(self, filename):
        """Saves to file."""
        with open(filename, "w") as h:
            write_lf(h, "%-20s" % self.titrav)
            write_lf(h, "%s %s %s %s %s" % (bool2str(self.ecrit),
                                            self.pas, self.echx, self.echy, self.fwhm))
            write_lf(h, "%s" % self.vvt[0])
            if self.vvt[0] > 900:
                write_lf(h, "%s" % self.ivtot)
                write_lf(list2str(self.tolv))
                write_lf(list2str(self.vvt))

            write_lf(h, list2str([self.teff, self.glog, self.asalog, self.nhe, self.inum]))
            write_lf(h, list2str([bool2str(self.ptdisk), self.mu]))
            write_lf(h, str(self.afstar))
            write_lf(h, list2str(self.xxcor))
            write_lf(h, self.fileflux)
            write_lf(h, list2str([self.llzero, self.llfin, self.aint]))
            for filetoh in self.filetohy:
                write_lf(h, filetoh)

    def init_default(self):
        """Initializes with default abundances (which are hard-coded)."""
        fullpath = path_to_default("main.dat")
        self.load(fullpath)
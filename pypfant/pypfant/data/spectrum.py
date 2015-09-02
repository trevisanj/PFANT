__all__ = ["Spectrum", "FileSpectrum", "FileSpectrumPfant", "FileSpectrumNulbad"]

import fortranformat as ff
import struct
import re
import logging
import numpy as np
from pypfant.errors import *
from .datafile import DataFile


class Spectrum(object):
    def __init__(self):
        self.ikeytot = None
        self.tit = None
        self.tetaef = None
        self.glog = None
        self.asalog = None
        self.modeles_nhe = None
        self.amg = None
        self.l0 = None
        self.lf = None
        # self.lzero = None
        # self.lfin = None
        # self.itot = None
        self.pas = None
        self.echx = None
        self.echy = None
        self.fwhm = None

        self.x = None  # x-values
        self.y = None  # y-values

        self.filename = None

    def __len__(self):
        """Corresponds to nulbad "ktot"."""
        return len(self.x) if self.x is not None else 0

    def __str__(self):

        s = "".join(["ikeytot = ", str(self.ikeytot), "\n",
                     "tit = ", str(self.tit), "\n",
                     "tetaef = ", str(self.tetaef), "\n",
                     "glog = ", str(self.glog), "\n",
                     "asalog = ", str(self.asalog), "\n",
                     "modeles_nhe = ", str(self.modeles_nhe), "\n",
                     "amg = ", str(self.amg), "\n",
                     "l0 = ", str(self.l0), "\n",
                     "lf = ", str(self.lf), "\n",
                     "pas = ", str(self.pas), "\n",
                     "echx = ", str(self.echx), "\n",
                     "echy = ", str(self.echy), "\n",
                     "fwhm = ", str(self.fwhm), "\n",
                     "============\n"
                     "Size of Spectrum: ", str(len(self)), "\n"])
        return s


class FileSpectrum(DataFile):
    attrs = ['spectrum']

    def __init__(self):
        DataFile.__init__(self)
        self.spectrum = Spectrum()


class FileSpectrumPfant(FileSpectrum):
    """
    Represents a pfant output spectrum file.

    This file alternates a "header" line where most of the information is
    repeated, and a "values" line, with the values of the flux
    corresponding to the lambda interval lzero-lfin
    """

    def _do_load(self, filename):
        with open(filename, 'r') as h:
            f_header = ff.FortranRecordReader(
                "(i5, a20, 5f15.5, 4f10.1, i10, 4f15.5)")
            i = 0
            y = []
            sp = self.spectrum = Spectrum()

            while True:
                s = h.readline()
                if i == 0:
                    vars = f_header.read(s)  # This is actually quite slow, gotta avoid it
                    [sp.ikeytot,
                     sp.tit,
                     sp.tetaef,
                     sp.glog,
                     sp.asalog,
                     sp.modeles_nhe,
                     sp.amg,
                     sp.l0,
                     sp.lf,
                     _,  # self.lzero,
                     _,  #self.lfin,
                     _,  #self.itot,
                     sp.pas,
                     sp.echx,
                     sp.echy,
                     sp.fwhm] = vars

                itot = vars[11]

                # if False:
                # # Will have to compile a different formatter depending on itot
                #     if i == 0 or itot != last_itot:
                #       f_flux = ff.FortranRecordReader("(%df15.5)" % itot)
                #
                #     v = f_flux.read(h.readline())
                # else:
                # Will see if can read faster than Fortran formatter
                s = h.readline()

                # Note: last character of s is "\n"
                v = [float(s[0 + j:15 + j]) for j in range(0, len(s) - 1, 15)]

                # print len(v)
                # print "==========================="
                # print v
                # print i, self.ikeytot

                if i < sp.ikeytot - 1:
                    # for some reason the last point is not used...
                    y = y + v[:-1]
                else:
                    # ...except for in the last calculation interval
                    # (then the last point is used).
                    # This is an imitation of NULBADGRADE fileflux reading
                    y = y + v
                    break

                i += 1
                last_itot = itot

        # Lambdas
        sp.x = np.array([sp.l0 + k * sp.pas for k in range(0, len(y))])
        sp.y = np.array(y)
        sp.filename = filename

        logging.debug("Just read PFANT Spectrum '%s'" % filename)


class FileSpectrumNulbad(FileSpectrum):
    """
    Represents a file created by nulbad.

    This file alternates a "header" line where most of the information is
    repeated, and a "values" line, with the values of the flux
    corresponding to the lambda interval lzero-lfin
    """

    def _do_load(self, filename):
        x = []
        y = []
        with open(filename, 'r') as h:
            sp = self.spectrum = Spectrum()
            # -- row 01 --
            # Original format: ('#',A,'Tef=',F6.3,X,'log g=',F4.1,X,'[M/H]=',F5.2,X,F5.2)
            s_header0 = struct.Struct("1x 20s 4x 6s 7x 4s 7x 5s 1x 5s")
            s = h.readline()
            [sp.tit, sp.tetaef, sp.glog, sp.asalog, sp.amg] = \
                s_header0.unpack_from(s)
            [sp.tetaef, sp.glog, sp.asalog, sp.amg] = \
                map(float, [sp.tetaef, sp.glog, sp.asalog, sp.amg])

            # -- row 02 --
            # Original format: ('#',I6,2X,'0. 0. 1. 1. Lzero =',F10.2,2x,'Lfin =', &
            # F10.2,2X,'PAS =',F5.2,2x,'FWHM =',F5.2)
            s_header1 = struct.Struct("1x 6s 21x 10s 8x 10s 7x 5s 8x 5s")
            s = h.readline()

            [n, sp.l0, sp.lf, sp.pas, sp.fwhm] = \
                s_header1.unpack_from(s)
            [sp.l0, sp.lf, sp.pas, sp.fwhm] = \
                map(float, [sp.l0, sp.lf, sp.pas, sp.fwhm])
            n = int(n)


            # -- rows 03 ... --
            #
            # Examples:
            #    4790.0000000000000       0.99463000000000001
            #    4790.0400000000000        2.0321771294932130E-004
            pattern = re.compile(r"^\s+([0-9.E+-]+)\s+([0-9.E+-]+)")
            for i in xrange(n):
                s = h.readline()
                match = pattern.match(s)
                if match is None:
                    raise ParseError('Row %d of file %s is invalid' % (i + 3, filename))
                a, b = map(float, match.groups())

                x.append(a)
                y.append(b)

        sp.x = np.array(x)
        sp.y = np.array(y)
        sp.filename = filename

        logging.debug("Just read NULBAD Spectrum '%s'" % filename)

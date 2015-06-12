__all__ = ["spectrum"]

import fortranformat as ff
import struct
import re

class ParseError(Exception):
    pass

class spectrum(object):
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

    def __len__(self):
        """Corresponds to nulbad "ktot"."""
        return len(self.x) if self.x is not None else 0

    def read_pfant(self, filename):
        """

        This file alternates a "header" line where most of the information is
        repeated, and a "values" line, with the values of the flux
        corresponding to the lambda interval lzero-lfin

        :param filename:
        :return:
        """

        with open(filename, 'r') as h:
            f_header = ff.FortranRecordReader(
                "(i5, a20, 5f15.5, 4f10.1, i10, 4f15.5)")
            i = 0
            self.y = []

            while True:
                s = h.readline()
                if i == 0:
                    vars = f_header.read(s)  # This is actually quite slow, gotta avoid it
                    [self.ikeytot,
                     self.tit,
                     self.tetaef,
                     self.glog,
                     self.asalog,
                     self.modeles_nhe,
                     self.amg,
                     self.l0,
                     self.lf,
                     _,  #self.lzero,
                     _,  #self.lfin,
                     _,  #self.itot,
                     self.pas,
                     self.echx,
                     self.echy,
                     self.fwhm] = vars

                itot = vars[11]

                # if False:
                #     # Will have to compile a different formatter depending on itot
                #     if i == 0 or itot != last_itot:
                #       f_flux = ff.FortranRecordReader("(%df15.5)" % itot)
                #
                #     v = f_flux.read(h.readline())
                # else:
                # Will see if can read faster than Fortran formatter
                s = h.readline()

                # Note: last character of s is "\n"
                v = [float(s[0+j:15+j]) for j in range(0, len(s)-1, 15)]

                # print len(v)
                # print "==========================="
                # print v
                # print i, self.ikeytot

                if i < self.ikeytot-1:
                    # for some reason the last point is not used...
                    self.y = self.y+v[:-1]
                else:
                    # ...except for in the last calculation interval
                    # (then the last point is used).
                    # This is an imitation of NULBADGRADE fileflux reading
                    self.y = self.y+v
                    break

                i += 1
                last_itot = itot

        # Lambdas
        self.x = [self.l0+k*self.pas for k in range(0, len(self.y))]

    def read_nulbad(self, filename):
        """

        This file alternates a "header" line where most of the information is
        repeated, and a "values" line, with the values of the flux
        corresponding to the lambda interval lzero-lfin

        :param filename:
        :return:
        """

        self.x = []
        self.y = []

        with open(filename, 'r') as h:
            # -- row 01 --
            # Original format: ('#',A,'Tef=',F6.3,X,'log g=',F4.1,X,'[M/H]=',F5.2,X,F5.2)
            s_header0 = struct.Struct("1x 20s 4x 6s 7x 4s 7x 5s 1x 5s")
            s = h.readline()
            [self.tit, self.tetaef, self.glog, self.asalog, self.amg] = \
                s_header0.unpack_from(s)
            [self.tetaef, self.glog, self.asalog, self.amg] = \
                map(float, [self.tetaef, self.glog, self.asalog, self.amg])

            # -- row 02 --
            # Original format: ('#',I6,2X,'0. 0. 1. 1. Lzero =',F10.2,2x,'Lfin =', &
            #                   F10.2,2X,'PAS =',F5.2,2x,'FWHM =',F5.2)
            s_header1 = struct.Struct("1x 6s 21x 10s 8x 10s 7x 5s 8x 5s")
            s = h.readline()

            [n, self.l0, self.lf, self.pas, self.fwhm] = \
                s_header1.unpack_from(s)
            [self.l0, self.lf, self.pas, self.fwhm] = \
                map(float, [self.l0, self.lf, self.pas, self.fwhm])
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
                    raise ParseError('Row %d of file %s is invalid' % (i+3, filename))
                a, b = map(float, match.groups())

                self.x.append(a)
                self.y.append(b)



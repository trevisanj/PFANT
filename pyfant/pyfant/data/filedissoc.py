__all__ = ["FileDissoc"]
from .datafile import *
import fortranformat as ff
from ..misc import *


class FileDissoc(DataFile):
    default_filename = "dissoc.dat"

    attrs = ['nmetal', 'nimax', 'elems', 'nelemx', 'ip', 'ig0', 'ig1', 'cclog',
             'nmol', 'mol', 'c', 'mmax', 'nelem', 'natom', 'eps', 'switer']

    def __init__(self):
        DataFile.__init__(self)
        # atom-related
        self.nmetal = None
        self.nimax = None
        self.elems = None
        self.nelemx = None
        self.ip = None
        self.ig0 = None
        self.ig1 = None
        self.cclog = None

        # molecule-related
        self.nmol = None
        self.mol = None
        self.c = None
        self.mmax = None
        self.nelem = None
        self.natom = None
        self.eps = None
        self.switer = None

    def __len__(self):
        """Returns length of "ele" attribute."""
        return len(self.ele)

    def _do_load(self, filename):
        """Clears internal lists and loads from file."""
        self.abol, self.ele = [], []

        with open(filename, "r") as h:
            fr = ff.FortranRecordReader('(2i5, 2f10.5)')
            self.nmetal, self.nimax, self.eps, self.switer = fr.read(h.readline())

            # atoms part
            self.elems, self.nelemx, self.ip, self.ig0, self.ig1, self.cclog = [], [], [], [], [], []
            fr = ff.FortranRecordReader('(a2, 2x, i6, f10.3, 2i5, f10.5)')
            for i in range(self.nmetal):
                symbol_, nelemx, ip, ig0, ig1, cclog = fr.read(h.readline())

                self.elems.append(adjust_atomic_symbol(symbol_))
                self.nelemx.append(nelemx)
                self.ip.append(ip)
                self.ig0.append(ig0)
                self.ig1.append(ig1)
                self.cclog.append(cclog)

            # molecules part (remaining lines)
            self.mol, self.c, self.mmax, self.nelem, self.natom = [], [], [], [], []
            fr = ff.FortranRecordReader('(a3, 5x, e11.5, 4e12.5, i1, 4(i2,i1))')
            for line in h:
                if not line.strip():
                    # empty line is considered end-of-file
                    break
                vars = fr.read(line)

                self.mol.append(vars[0])
                self.c.append(vars[1:6])
                mmax = vars[6]
                self.mmax.append(mmax)
                self.nelem.append(vars[7::2][:mmax])
                self.natom.append(vars[8::2][:mmax])
            self.nmol = len(self.mol)



    def _do_save_as(self, filename):
        with open(filename, "w") as h:
            # h.writelines([' %-2s%6.2f\n' % (self.ele[i], self.abol[i])
            #               for i in xrange(len(self))])
            # h.writelines(['1\n', '1\n'])

            write_lf(h, "%5d%5d%10.5f%10.5f" % (self.nmetal, self.nimax, self.eps, self.switer))

            # atoms part
            fr = ff.FortranRecordReader('(a2, 2x, i6, f10.3, 2i5, f10.5)')
            for elems, nelemx, ip, ig0, ig1, cclog in \
                    zip(self.elems, self.nelemx, self.ip, self.ig0, self.ig1, self.cclog):
                elems = elems.upper().strip()  # to follow convention: right-aligned upper case
                write_lf(h, '%2s  %6d%10.3f%5d%5d%10.5f' % (elems, nelemx, ip, ig0, ig1, cclog))


            for mol, c, mmax, nelem, natom in \
                    zip(self.mol, self.c, self.mmax, self.nelem, self.natom):
                mol = mol.upper().strip()  # to follow convention: right-aligned upper case
                l = ["%3s     %11.5e%12.5e%12.5e%12.5e%12.5e%1d" % tuple([mol]+c+[mmax])]
                for nelemm, natomm in zip(nelem, natom):
                    l.append("%2d%1d" % (nelemm, natomm))
                s = "".join(l)
                write_lf(h, s)

            write_lf(h, "")
            write_lf(h, "")  # two blank lines to signal end-of-file



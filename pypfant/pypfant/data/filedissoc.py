__all__ = ["FileDissoc"]

import struct
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
            self.nmol, self.mol, self.c, self.mmax, self.nelem, self.natom = [], [], [], [], [], []
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


# ['AN ', 12.8051, -8.27934, 0.0641622, -0.00736267, 0.000346663, 2, 9, 1, 7, 1, None, None, None, None]
# ['CN ', 12.8051, -8.27934, 0.0641622, -0.00736267, 0.000346663, 2, 6, 1, 7, 1, None, None, None, None]
# ['CAH', 11.3401, -3.01442, 0.423487, -0.0614674, 0.00316392, 2, 20, 1, 1, 1, None, None, None, None]
# ['MGO', 11.7018, -5.03261, 0.296408, -0.0428111, 0.00220232, 2, 12, 1, 8, 1, None, None, None, None]
# ['TIO', 13.3981, -8.59562, 0.408726, -0.0579369, 0.00292873, 2, 22, 1, 8, 1, None, None, None, None]
# ['MGH', 11.2853, -2.71637, 0.196585, -0.0273103, 0.00138164, 2, 12, 1, 1, 1, None, None, None, None]
# ['AC ', 12.8038, -6.5178, 0.0977186, -0.0127393, 0.000626035, 2, 9, 1, 6, 1, None, None, None, None]
# ['AA ', 12.8038, -6.5178, 0.0977186, -0.0127393, 0.000626035, 1, 9, 2, None, None, None, None, None, None]

                print vars
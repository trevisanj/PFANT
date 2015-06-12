import unittest
from pypfant import *

class test_read_spectrum_01(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass


    def test_read_pfant(self):
        print "*****************************************************************"

        o = spectrum()
        o.read_pfant('fl.norm')

        print "ikeytot = ", o.ikeytot
        print "tit = ", o.tit
        print "tetaef = ", o.tetaef
        print "glog = ", o.glog
        print "asalog = ", o.asalog
        print "modeles_nhe = ", o.modeles_nhe
        print "amg = ", o.amg
        print "l0 = ", o.l0
        print "lf = ", o.lf
        print "pas = ", o.pas
        print "echx = ", o.echx
        print "echy = ", o.echy
        print "fwhm = ", o.fwhm
        print "============"
        print "Size of spectrum: ", len(o)

    def test_read_nulbad(self):
        print "*****************************************************************"
        o = spectrum()
        o.read_nulbad('fl.norm.nulbad')

        print "ikeytot = ", o.ikeytot
        print "tit = ", o.tit
        print "tetaef = ", o.tetaef
        print "glog = ", o.glog
        print "asalog = ", o.asalog
        print "modeles_nhe = ", o.modeles_nhe
        print "amg = ", o.amg
        print "l0 = ", o.l0
        print "lf = ", o.lf
        print "pas = ", o.pas
        print "echx = ", o.echx
        print "echy = ", o.echy
        print "fwhm = ", o.fwhm
        print "============"
        print "Size of spectrum: ", len(o)

if __name__ == '__main__':
    unittest.main()

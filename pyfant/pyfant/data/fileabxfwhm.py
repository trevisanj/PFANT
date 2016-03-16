"""
FileMAbFwhm class (differential ABundances and FWHMs)
"""
__all__ = ["FileAbXFwhm"]


from .datafile import *
from pyfant import adjust_atomic_symbol
import imp
import numpy as np


_COMMENT0 = """# specification of differential abundances for each chemical
# - differential abundance: number to add to original value in abonds.dat
# - all list of abundances for each element must have the same length"""
_COMMENT1 = """# Convolutions specification for fwhm parameter:
# [first value, last value, step]"""


class FileAbXFwhm(DataFile):
    __doc__ = """
Represents file containing lists of differential abundances and FWHMs.

This file is actually Python source. Here is a sample:

%s
ab = {"Ca": [-.3, 0, .3, .5],
      "Si": [-.3, 0, .3, .5]}
%s
conv = [0.08, 0.6,  0.04]
""" % (_COMMENT0, _COMMENT1)

    default_filename = "abxfwhm.py"
    attrs = ["ab", "conv"]

    @property
    def ab(self):
        """Abundances dictionary.

        Setting this property will cause source to be rebuilt and comments to
        be lost."""
        return self.__ab

    @ab.setter
    def ab(self, x):
        self.__ab = x
        self.__adjust_atomic_symbols()
        self.__rebuild_source()

    @property
    def conv(self):
        """Convolutions FWHM list.

        Setting this property will cause source to be rebuilt and comments to
        be lost."""
        return self.__conv

    @conv.setter
    def conv(self, x):
        self.__conv = x
        self.__rebuild_source()

    @property
    def source(self):
        """Source code.

        Better to set this property than to set ab/conv. Setting this property
        will preserve the source code, whereas setting ab/conv separately will
        cause the source code to be rebuilt and become as boring as possible.
        """
        return self.__source

    @source.setter
    def source(self, x):
        self.__parse(x)

    def __init__(self):
        DataFile.__init__(self)

        self.__flag_parsed = False
        self.__flag_rebuild = False
        self.__source = ""
        self.__ab = None
        self.__conv = None

    def get_fwhms(self):
        """Returns FWHM's as a numpy vector."""
        return np.arange(self.conv[0], self.conv[1]+.00000001, self.conv[2])

    def __parse(self, x):
        """Populates __ab, __conf, and __source."""
        cfg = imp.new_module('cfg')
        exec x in cfg.__dict__
        self.__ab = cfg.ab
        self.__adjust_atomic_symbols()
        self.__conv = cfg.conv
        self.__source = x

    def __rebuild_source(self):
        self.__source = "%s\n%s\n\n%s\n%s\n" % (_COMMENT0,
         "ab = "+repr(self.__ab).replace("], ", "],\n      "),
         _COMMENT1, "conv = "+repr(self.__conv))

    def __adjust_atomic_symbols(self):
        """replaces self.__ab keys with the adjusted atomic symbols."""
        for key in self.__ab:
            self.__ab[adjust_atomic_symbol(key)] = self.__ab.pop(key)

    def _do_load(self, filename):
        with open(filename, "r") as h:
            self.__parse(h.read())

    def _do_save_as(self, filename):
        with open(filename, "w") as h:
            h.write(self.__source)


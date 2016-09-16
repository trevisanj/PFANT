""""
Support for WebSim-Compass FITS spectral cubes

Based on IDL source file chris_J4000.pro
"""

__all__ = ["WebsimCube", "FileWebsimCube"]

from pyfant import AttrsPart, get_python_logger
from .datafile import DataFile
from .spectrum import Spectrum
import numpy as np
#from pyfant.misc import *
from astropy.io import fits
import os

class WebsimCube(AttrsPart):
    """X-Y-wavelength cube compliant with WebSim-Compass specs

    Data is stored primarily in self.hdu, with a few other relevant attributes

    Notes on FITS HDU:
      - data is stored in reverse dimensions, i.e., (z, y, x)
      - CDELT1, CRVAL1 refer to x
      - CDELT2, CRVAL2 refer to y
      - CDELT3, CRVAL3 refer to z
    """

    attrs = ["R", "hrfactor", "ifu_pix_size"]

    @property
    def flag_created(self):
        """Whether the data cube has already been created"""
        return self.hdu is not None

    @property
    def flag_wavelengthed(self):
        """Whether wavelength information is already present """
        return self.flag_created and self.hdu.header["CDELT3"] != -1

    def __init__(self, hdu=None):
        AttrsPart.__init__(self)
        self.hdu = None  # PyFITS HDU object
        self.wavelength = None  # the wavelength axis (angstrom) (shared among all spectra in the cube)
        self.filename = None

        if hdu is not None:
            self.from_hdu(hdu)

    def from_hdu(self, hdu):
        """
        Sets self.hdu and with HDU object passed. Warning: may change HDU object.

        Default HDU headers:
         CDELT1 defaults to 1.
         CDELT2 defaults to CDELT1
         CRDELT3 defaults to 1.
         CRVAL3 defaults to 0.
        """
        assert isinstance(hdu, fits.PrimaryHDU)

        # ensures all required headers are present
        keys = ["NAXIS1", "NAXIS2", "NAXIS3"]
        for key in keys:
            assert key in hdu.header, 'Key "%s" not found in headers' % key

        if not "CDELT3" in hdu.header:
            get_python_logger().warning("HDU lacks header 'CDELT3', assumed 1.0")
            hdu.header["CDELT3"] = 1.
        if not "CRVAL3" in hdu.header:
            get_python_logger().warning("HDU lacks header 'CRVAL3', assumed 0.0")
            hdu.header["CRVAL3"] = 0.
        if not "CDELT1" in hdu.header:
            get_python_logger().warning("HDU lacks header 'CDELT1', assumed 1.0")
            hdu.header["CDELT1"] = 1.0
        if not "CDELT2" in hdu.header:
            get_python_logger().warning("HDU lacks header 'CDELT2', assumed value in 'CDELT1'")
            hdu.header["CDELT2"] = hdu.header["CDELT1"]
        if not "HRFACTOR" in hdu.header:
            hdu.header["HRFACTOR"] = 1.

        l0 = hdu.header["CRVAL3"]+hdu.header["CDELT3"]
        delta_lambda = hdu.header["CDELT3"]
        nlambda = hdu.header["NAXIS3"]
        self.hdu = hdu
        self.set_wavelength(np.array([l0 + k * delta_lambda for k in range(0, nlambda)]))

    def __len__(self):
        raise NotImplementedError()

    def __repr__(self):
        return "Please implement WebsimCube.__repr__()"

    def create1(self, R, dims, hr_pix_size, hrfactor):
        """Creates FITS HDU, including the cube full with zeros 

          dims -- (nlambda, height, width)
        """
        cube = np.zeros(dims)
        hdu = fits.PrimaryHDU()
        hdu.header["CDELT1"] = hr_pix_size
        hdu.header["CDELT2"] = hr_pix_size
        hdu.header["CDELT3"] = -1.0  # this will become known then paint() is called for the first time
        hdu.header["CRVAL3"] = -1.0  # "
        hdu.header["HRFACTOR"] = hrfactor
        hdu.header["R"] = R
        hdu.data = cube
        self.hdu = hdu

    def get_spectrum(self, x, y):
        """
        Returns spectrum at coordinate x, y (copied vectors)

        **Note** coordinate (x=0, y=0) corresponds to lower left pixel of cube cross-section
        """
        assert self.flag_wavelengthed

        sp = Spectrum()
        sp.x = np.copy(self.wavelength)
        sp.y = np.copy(self.hdu.data[:, y, x])
        sp.more_headers["PIXEL-X"] = x  # why not
        sp.more_headers["PIXEL-Y"] = y
        return sp

    def set_wavelength(self, w):
        delta_lambda = w[1] - w[0]
        self.hdu.header["CDELT3"] = delta_lambda
        # dunno why the initial lambda must be one delta lambda lower than the initial lambda in the spectrum, but
        # this is how it was in the original
        self.hdu.header["CRVAL3"] = w[0] - delta_lambda
        self.wavelength = w


class FileWebsimCube(DataFile):
    """Represents a Compass data cube file, which is also a FITS file"""
    attrs = ['wcube']
    description = "WebSim Compass Data Cube (FITS file)"
    default_filename = "default.wcube"

    def __init__(self):
        DataFile.__init__(self)
        self.wcube = WebsimCube()

    def _do_load(self, filename):
        fits_obj = fits.open(filename)
        self.wcube = WebsimCube(fits_obj[0])
        self.filename = filename

    def _do_save_as(self, filename):
        assert self.wcube.flag_wavelengthed, "Cannot save before at least one pixel has been \"painted\""""
        if os.path.isfile(filename):
            os.unlink(filename)  # PyFITS does not overwrite file
        self.wcube.hdu.writeto(filename)

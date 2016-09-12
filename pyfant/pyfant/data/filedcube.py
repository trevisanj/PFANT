""""
Spectral cube allocated in a way that will take less space.

Saved in FITS format, as it is small and can be opened by other programs

Based on IDL source file chris_J4000.pro
"""

__all__ = ["DataCube", "FileDCube"]

from .filespectrumlist import *
from . import DataFile, Spectrum
from .fileccube import *
from ..misc import *
import numpy as np
import os
from scipy.interpolate import interp1d
from astropy.io import fits

# headers to care about when importing from a CompassCube HDU
_HEADERS_COMPASS_CUBE = ["CDELT1", "HRFACTOR", "R"]


class DataCube(SpectrumCollection):
    attrs = SpectrumCollection.attrs+["R", "hrfactor", "hr_pix_size"]

    @property
    def hr_pix_size(self):
        return self.more_headers.get("CDELT1")
        
    @hr_pix_size.setter
    def hr_pix_size(self, value):
        self.more_headers["CDELT1"] = value

    @property
    def hrfactor(self):
        return self.more_headers.get("HRFACTOR")
        
    @hrfactor.setter
    def hrfactor(self, value):
        self.more_headers["HRFACTOR"] = value

    @property
    def R(self):
        return self.more_headers.get("R")
        
    @R.setter
    def R(self, value):
        self.more_headers["R"] = value

    @property
    def width(self):
        return self.more_headers.get("WIDTH")
        
    @width.setter
    def width(self, value):
        self.more_headers["WIDTH"] = value

    @property
    def height(self):
        return self.more_headers.get("HEIGHT")
        
    @height.setter
    def height(self, value):
        self.more_headers["HEIGHT"] = value

    # read-only
    @property
    def delta_lambda(self):
        return self.wavelength[1]-self.wavelength[0]

    def __init__(self, hdu=None):
        SpectrumCollection.__init__(self)
        self.__flag_update = True
        self.__flag_update_pending = False
        self.wavelength = np.array([-1., -1.])  # Like this delta_lambda property does not give error
        # Header data initialized to default
        self.R = 5000  # TODO this does not match the spectrum R necessarily, should I do sth about it?
        self.hrfactor = 10
        self.hr_pix_size = 0.0375/self.hrfactor
        self.width = 50
        self.height = 30

        if hdu is not None:
            self.from_hdu(hdu)

    def crop(self, x0=None, x1=None, y0=None, y1=None, lambda0=None, lambda1=None):
        """
        Reduces region, discards spectra, cuts remaining spectra

        intervals:
            X - [x0, x1]
            Y - [y0, y1]
            wavelength - [lambda0, lambda1]

        **Note** x1, y1, lambda 1 **included** in interval (not pythonic).
        """
        if len(self.spectra) == 0:
            raise RuntimeError("Need at least one spectrum added in order to crop")

        if x0 is None:
            x0 = 0
        if x1 is None:
            x1 = self.width-1
        if y0 is None:
            y0 = 0
        if y1 is None:
            y1 = self.height-1
        if lambda0 is None:
            lambda0 = self.wavelength[0]
        if lambda1 is None:
            lambda1 = self.wavelength[-1]

        if x0 > x1:
            raise RuntimeError('x0 is greater than x1')
        if y0 > y1:
            raise RuntimeError('y0 is greater than y1')
        if lambda0 > lambda1:
            raise RuntimeError('lambda0 is greater than lambda1')

        if not any([x0 != 0, x1 != self.width-1, y0 != 0, y1 != self.height-1, lambda0 != self.wavelength[0], lambda1 != self.wavelength[-1]]):
            return

        if any([x0 != 0, x1 != self.width-1, y0 != 0, y1 != self.height-1]):
            self.width = x1-x0+1
            self.height = y1-y0+1
            for sp in self.spectra:
                sp.x -= x0
                sp.y -= y0

            # Deletes spectra out of XY range
            for sp in reversed(self.spectra):
                if not 0 <= sp.pixel_x < self.width or not 0 <= sp.pixel_y < self.height:
                    self.spectra.remove(sp)

        if any([lambda0 != self.wavelength[0], lambda1 != self.wavelength[-1]]):
            # cuts remaining spectra
            for sp in reversed(self.spectra):
                sp.cut(lambda0, lambda1)
                if len(sp) == 0:
                    # If spectrum is now out of range, it is deleted
                    self.spectra.remove(sp)
        self.__update()


    def from_hdulist(self, hdul):
        """
        Checks for required field names existence

        Toad successfully, file must have the following field names:
        'PIXEL-X', 'PIXEL-Y', 'Z-START'
        """

        self.__flag_update = False
        try:
            # if not hdul[0].header.get("TAINHA"):
            #     raise RuntimeError("Wrong HDUList")

            SpectrumCollection.from_hdulist(self, hdul)

            required = ('PIXEL-X', 'PIXEL-Y', 'Z-START')
            for fn in required:
                if not fn in self.fieldnames:
                    raise RuntimeError("Required field name '%s' not found" % fn)

#            for i, hdu in enumerate(hdul):
#                if i == 0:
#                    for na0, na1 in _MMM:
#                        try:
#                            self.__setattr__(na0, hdu.header[na1])
#                        except:
#                            get_python_logger().exception("Failed setting '%s' = '%s'" % (na0, na1))
#
#                else:
#                    sp = Spectrum()
#                    sp.from_hdu(hdu)
#                    if i == 1:
#                        # TODO this must be settable, not just taken from first spectrum
#                        self.reference = _LambdaReference(sp.x)
#                    self.add_spectrum(sp)
        finally:
            self.enable_update()


    # def to_hdulist(self):
    #     """Inherited to add FileDCube-specific header marker"""
    #     hdul = SpectrumCollection.to_hdulist(self)
    #     hdul[0].header["TAINHA"] = 26.9752
    #     return hdul
    #
    def from_compass_cube(self, ccube):
        assert isinstance(ccube, CompassCube)
        hdu = ccube.hdu
        assert isinstance(hdu, fits.PrimaryHDU)
        data = hdu.data
        nlambda, nY, nX = data.shape

        # Reads some attributes from the headers
        # Uses self.attrs, but this is just a coincidence, may need detachment
        for name in _HEADERS_COMPASS_CUBE:
            self.more_headers[name] = hdu.header.get(name, self.more_headers[name])

        self.__flag_update = False
        try:
            for i in range(nX):
                for j in range(nY):
                    Yi = j + 1
                    flux0 = data[:, j, i]
                    if np.any(flux0 > 0):
                        sp = ccube.get_spectrum(i, j)
                        # discards edges that are zeros
                        where_positive = np.where(sp.flux > 0)[0]
                        sp.cut_idxs(where_positive[0], where_positive[-1]+1)
                        sp.pixel_x, sp.pixel_y = i, j
                        self.add_spectrum(sp)

            self.height = nY
            self.width = nX
        finally:
            self.enable_update()

    def to_compass_cube(self):
        """Creates CompassCube object"""
        assert len(self.spectra) > 0, "No spectra added"

        ccube = CompassCube()
        wl_new = ccube.wavelength = self.wavelength.copy()
        dims = len(wl_new), self.height, self.width
        ccube.create1(self.R, dims, self.hr_pix_size, self.hrfactor)
        for sp in self.spectra:
            ii0 = BSearchCeil(wl_new, sp.x[0])
            ccube.hdu.data[ii0:ii0+len(sp), sp.pixel_y, sp.pixel_x] = sp.y
        ccube.set_wavelength(self.wavelength)
        return ccube

    def to_colors(self, visible_range=None, flag_scale=False, method=0):
        """Returns a [nY, nX, 3] red-green-blue (0.-1.) matrix

        Arguments:
          visible_range=None -- if passed, the true human visible range will be
                                affine-transformed to visible_range in order
                                to use the red-to-blue scale to paint the pixels
          flag_scale -- whether to scale the luminosities proportionally
                        the weight for each spectra will be the area under the flux
          method -- see Spectrum.get_rgb()
        """
        im = np.zeros((self.height, self.width, 3))
        weights = np.zeros((self.height, self.width, 3))
        max_area = 0.
        for i in range(self.width):
            for j in range(self.height):
                sp = self.get_pixel(i, j, False)
                if len(sp) > 0:
                    im[j, i, :] = sp.get_rgb(visible_range, method)
                    sp_area = np.sum(sp.y)
                    max_area = max(max_area, sp_area)
                    if flag_scale:
                        weights[j, i, :] = sp_area
        if flag_scale:
            weights *= 1./max_area
            im *= weights
        return im

#    def to_hdulist(self):
#        if len(self.spectra) == 0:
#            raise RuntimeError("At the moment, saving file with no spectra is not supported")
#            # ... because it uses the 
#
#        dl = self.reference.delta_lambda
#
#        hdul = fits.HDUList()
#
#        hdu = fits.PrimaryHDU()
#        hdu.header["CDELT1"] = self.hr_pix_size
#        hdu.header["CDELT2"] = self.hr_pix_size
#        hdu.header["CDELT3"] = dl
#        hdu.header["CRVAL3"] = self.wavelength[0]-dl
#        hdu.header["HRFACTOR"] = self.hrfactor
#        hdu.header["R"] = self.R
#        hdu.header["TAINHA"] = 26.9752
#
#        hdul.append(hdu)
#
#        for item in self.spectra:
#            hdul.append(item.sp.to_hdu())
#            # hdu = fits.PrimaryHDU()
#            # hdu.header["PIXEL-X"] = item.x
#            # hdu.header["PIXEL-Y"] = item.y
#            # hdu.header["CDELT1"] = dl
#            # hdu.header["CRVAL1"] = item.sp.x[0]  # **note** not subtracting dl as required in WebSimCompass format
#            # hdu.data = item.sp.y
#            # hdul.append(hdu)
#
#        return hdul


    def add_spectrum(self, sp):
        """
        "Paints" pixel with given spectrum

        Arguments:
            sp -- Spectrum instance

        **Note** coordinate (x=0, y=0) corresponds to lower left pixel of cube cross-section
        """
        assert isinstance(sp, Spectrum)
        # assert self.flag_created, "Cube has not been created yet"

        if len(sp.x) < 2:
            raise RuntimeError("Spectrum must have at least two points")

        sp.start_z = -1  # will cause update to check on spectrum
        SpectrumCollection.add_spectrum(self, sp)
        self.__update()

#    def delete_spectra(self, indexes):
#        indexes = list(set(indexes))
#        if isinstance(indexes, numbers.Integral):
#            indexes = [indexes]
#        for index in reversed(indexes):
#            del self.spectra[index]
#        self.__update()

    def enable_update(self):
        self.__flag_update = True
        if self.__flag_update_pending:
            self.__update()
            self.__flag_update_pending = False

    def get_pixel(self, x, y, flag_copy=True):
        """Creates spectrum on-the-fly containing all spectra merged for pixel (x, y)

        **Note** if there is no spectra at point (x, y), returns an empty spectrum, for speed

        Arguments:
            x, y -- 0-based pixel coordinates
            flag_copy -- disable vector copies to speed up but don't the spectrum
        """

        ret = Spectrum()
        if len(self.spectra) > 0:
            any_ = False
            for sp in self.spectra:
                if sp.pixel_x == x and sp.pixel_y == y:
                    if not any_:
                        ret.x = self.wavelength if not flag_copy else np.copy(self.wavelength)
                        ret.y = np.zeros(len(ret.x))
                        any_ = True
                    ii0 = BSearchCeil(ret.x, sp.x[0])
                    ret.y[ii0:ii0 + len(sp)] = sp.y
        return ret

    def __update(self):
        """Updates internal state

        - resamples necessary vectors
        - spectra z-positions
        - wavelength vector
        """

        if not self.__flag_update:
            self.__flag_update_pending = True
            return
            
        self.wavelength = np.array([-1., -1.])

        if len(self.spectra) == 0:
            return

        wlmax = max([sp.x[-1] for sp in self.spectra])
        wlmin = min([sp.x[0] for sp in self.spectra])
        sp = self.spectra[0]
        dl = sp.x[1]-sp.x[0]
        wlmin = (wlmin//dl)*dl
        wlmax = np.ceil(wlmax/dl)*dl
        wl = self.wavelength = np.arange(wlmin, wlmax+dl, dl)

        for sp in self.spectra:
            i0 = BSearchCeil(wl, sp.x[0])
            assert i0 != -1, "BSearchCeil(wl, sp.x[0]) FAILED"
            i1 = BSearchFloor(wl, sp.x[-1])
            assert i1 != -1, "BSearchFloor(wl, sp.x[-1]) FAILED"

            if sp.delta_lambda != dl or wl[i0] != sp.x[0] or wl[i1] != sp.x[-1]:
                # # Linear interpolation: either adjust for different delta lambda or misaligned wavelengths
                wl_new = wl[i0:i1+1]
                f = interp1d(sp.x, sp.y)
                sp.x = wl_new
                sp.y = f(wl_new)
            sp.z_start = i0


class FileDCube(DataFile):
    """Represents a Compass data cube file, which is also a FITS file"""
    attrs = ['dcube']
    description = "Data Cube (FITS file)"
    default_filename = "default.dcube"

    def __init__(self):
        DataFile.__init__(self)
        self.dcube = DataCube()

    def _do_load(self, filename):
        fits_obj = fits.open(filename)
        self.dcube = DataCube()
        self.dcube.from_hdulist(fits_obj)
        self.filename = filename

    def _do_save_as(self, filename):
        if os.path.isfile(filename):
            os.unlink(filename)  # PyFITS does not overwrite file
        hdul = self.dcube.to_hdulist()
        hdul.writeto(filename)

    def init_default(self):
        # Already created OK
        pass

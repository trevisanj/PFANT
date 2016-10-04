__all__ = ["Spectrum", "FileSpectrum", "FileSpectrumPfant",
           "FileSpectrumNulbad", "FileSpectrumXY", "FileSpectrumFits",
            "cut_spectrum", "cut_spectrum_idxs", "normalize_spectrum"]

import fortranformat as ff
import struct
import logging
import numpy as np
from .datafile import DataFile
from ..misc import *
from astropy.io import fits
from pyfant import write_lf
import copy
import os
# from scipy.interpolate import interp1d
from scipy import interp

class Spectrum(object):
    """
    Spectrum with several *in-place* operations, conversion utilities etc.

    Routines that have the term *in place*: it means that the spectrum
    itself is changed and nothing is returned
    """

    @property
    def title(self):
        """Calculated, read-only property"""
        ret = None
        if ret is None:
            ret = self.filename
        if ret is None:
            ret = self.more_headers.get("ORIGIN")
        return ret

    @property
    def wavelength(self):
        return self.x

    @wavelength.setter
    def wavelength(self, x):
        self.x = x

    @property
    def flux(self):
        return self.y

    @flux.setter
    def flux(self, y):
        self.y = y

    @property
    def delta_lambda(self):
        """Should agree with self.pas"""
        return self.x[1]-self.x[0]

    @property
    def pixel_x(self):
        return self.more_headers.get("PIXEL-X")
        
    @pixel_x.setter
    def pixel_x(self, value):
        self.more_headers["PIXEL-X"] = value

    @property
    def pixel_y(self):
        return self.more_headers.get("PIXEL-Y")
        
    @pixel_y.setter
    def pixel_y(self, value):
        self.more_headers["PIXEL-Y"] = value

    @property
    def z_start(self):
        return self.more_headers.get("Z-START")

    @z_start.setter
    def z_start(self, value):
        self.more_headers["Z-START"] = value

    @property
    def ylabel(self):
        return self.more_headers.get("YLABEL")

    @ylabel.setter
    def ylabel(self, value):
        self.more_headers["YLABEL"] = value

    def __init__(self):
        self._flag_created_by_block = False  # assertion
        self.x = None  # x-values
        self.y = None  # y-values
        self.filename = None
        self.more_headers = {}  # for Spectrum, just cargo

        # Attributes used only by FileSpectrumPfant
        self.ikeytot = None
        self.tit = None
        self.tetaef = None
        self.glog = None
        self.asalog = None
        self.modeles_nhe = None
        self.amg = None
        self.l0 = None
        self.lf = None
        self.pas = None
        self.echx = None
        self.echy = None
        self.fwhm = None

    def __len__(self):
        """Corresponds to nulbad "ktot"."""
        return len(self.x) if self.x is not None else 0

    def one_liner_str(self):
        if self.x is not None and len(self.x) > 0:
            # s = ", ".join(["%g <= lambda <= %g" % (self.x[0], self.x[-1]),
            #                "delta_lambda = %g" % self.delta_lambda,
            #                "no. points: %d" % len(self.x)])

            s = " | ".join([u"%g \u2264 \u03BB \u2264 %g" % (self.x[0], self.x[-1]),
                           u"\u0394\u03BB = %g" % self.delta_lambda,
                           u"%g \u2264 flux \u2264 %g" % (np.min(self.y), np.max(self.y)),
                           "length: %d" % len(self.x)])

        else:
            s = "(empty)"
        return s

    def __str__(self):
        s = ", ".join(["ikeytot = ", str(self.ikeytot), "\n",
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

    def get_rgb(self, visible_range=None, method=0):
        """Takes weighted average of rainbow colors RGB's

        Arguments:
            visible_range=None -- if passed, affine-transforms the rainbow colors
            method --
              0: rainbow colors
              1: RGB
        """

        if len(visible_range) < 2:
            raise RuntimeError("Invalid visible range: %s" % (visible_range,))
        if visible_range[1] <= visible_range[0]:
            raise RuntimeError("Second element of visible range (%s) must be greater than first element" % (visible_range,))

        if method == 1:
            # new color system splitting visible range in three
            dl = float(visible_range[1]-visible_range[0])/3
            ranges = np.array([[visible_range[0]+i*dl, visible_range[0]+(i+1)*dl] for i in range(3)])
            colors = np.array([(0., 0., 1.), (0., 1., 0.), (1., 0., 0.)])  # blue, green, red

            tot_area, tot_sum = 0., np.zeros(3)
            for color, range_ in zip(colors, ranges):
                b = np.logical_and(self.x >= range_[0], self.x <= range_[1])
                area = np.sum(self.y[b])
                tot_area += area
                tot_sum += color*area
            if tot_area == 0.:
                tot_area = 1.
            ret = tot_sum/tot_area
            return ret

        elif method == 0:
            tot_area, tot_sum = 0., np.zeros(3)
            ftrans = lambda x: x
            if visible_range:
                ll0 = rainbow_colors[0].l0
                llf = rainbow_colors[-1].lf
                ftrans = lambda lold: visible_range[0] + (visible_range[1] - visible_range[0]) / (llf - ll0) * (lold - ll0)

            for color in rainbow_colors:
                b = np.logical_and(self.x >= ftrans(color.l0), self.x <= ftrans(color.lf))
                area = np.sum(self.y[b])
                tot_area += area
                tot_sum += color.rgb*area
            if tot_area == 0.:
                tot_area = 1.
            ret = tot_sum/tot_area
            return ret
        else:
            raise RuntimeError("Unknown method: %s" % method)



    # header parameters *not* to be put automatically in self.more_headers
    #                  saved separately  dealt with automatically by the FITS module
    #                  ----------------  ------------------------------------------------
    _IGNORE_HEADERS = ("CRVAL", "CDELT", "NAXIS", "PCOUNT", "BITPIX", "GCOUNT", "XTENSION")
    def from_hdu(self, hdu):
        # x/wavelength and y/flux
        n = hdu.data.shape[0]
        lambda0 = hdu.header["CRVAL1"]
        try:
            delta_lambda = hdu.header["CDELT1"]
        except Exception as E:  # todo figure out the type of exception (KeyError?)
            delta_lambda = hdu.header['CD1_1']
            print "Alternative delta lambda in FITS header: CD1_1"
            print "Please narrow the Exception specification in the code"
            print "Exception is: " + str(E) + " " + E.__class__.__name__
            print delta_lambda
        self.x = np.linspace(lambda0, lambda0 + delta_lambda * (n - 1), n)
        self.y = hdu.data

        # Additional header fields
        for name in hdu.header:
            if not name.startswith(self._IGNORE_HEADERS):
                # **Note** that it eliminates "#" comments from FITS header
                value = hdu.header[name]
                if isinstance(value, str):
                    value = value.split("#")[0].strip()
                self.more_headers[name] = value

    def to_hdu(self):
        """Converts to fits.PrimaryHDU object. Assumes XUNIT is "A" (angstrom)"""
        hdu = fits.PrimaryHDU()
        # hdu.header[
        #     "telescop"] = "PFANT synthetic spectrum"  # "suggested" by https://python4astronomers.github.io/astropy/fits.html
        hdu.header["CRVAL1"] = self.x[0]
        hdu.header["CDELT1"] = self.x[1] - self.x[0]
        hdu.header["XUNIT"] = "A"
        for key, value in self.more_headers.iteritems():
            try:
                # Strips line feed which were present in some of the Websim-Compass fits files
                if isinstance(value, str):
                    value = value.replace("\n", "")
                hdu.header[key] = value
            except:
                get_python_logger().exception("Error adding header['%s'] = '%s'" % (key, value))
                raise
        hdu.data = self.y
        return hdu
        
    def cut(self, l0, l1):
        """Cuts *in place* to wavelength interval [l0, l1]. Cut is done within the array objects thus keeping the same objects"""
        ii_delete = np.where(np.logical_or(self.x < l0, self.x > l1))
        np.delete(self.x, ii_delete)
        np.delete(self.y, ii_delete)
        
    def cut_idxs(self, i0, i1):
        """Cuts *in place* to slice i0:i1 (pythonic, interval = [i0, i1["""
        self.x = self.x[i0:i1]
        self.y = self.y[i0:i1]

    def calculate_magnitude(self, band_name, flag_force_parametric=False):
        """
        Calculates magnitude and stores values in more_headers "MAG_CALC" and "MAG_BAND"

        Returns several variables in a dict, which may be also useful for plotting or other purposes
        """

        band = Bands.bands[band_name]
        band_l0, band_lf = band.range(flag_force_parametric)  # band practical limits
        band_f = band.ufunc_band(flag_force_parametric)  # callable transmission function
        spc = copy.deepcopy(self)
        spc.cut(band_l0, band_lf)
        band_f_spc_x = band_f(spc.x)
        band_area = np.sum(band_f_spc_x) * spc.delta_lambda
        out_y = spc.y * band_f_spc_x

        # Calculates apparent magnitude and filtered flux area
        cmag, weighted_mean_flux, out_area = None, 0, 0
        if band.ref_mean_flux:
            if len(spc) > 0:
                out_area = np.sum(out_y) * spc.delta_lambda
                weighted_mean_flux = out_area / band_area
                cmag = -2.5 * np.log10(weighted_mean_flux / band.ref_mean_flux)

                # cosmetic manipulation
                cmag = np.round(cmag, 3)
                if cmag == 0.0:
                    cmag = 0.0  # get rid of "-0.0"

            else:
                cmag = np.inf

        self.more_headers["MAG_CALC"] = cmag
        self.more_headers["MAG_BAND"] = band_name
        return {"band_area": band_area,  # area under band filter
                "band_f": band_f,  # transmission function
                "band_f_spc_x": band_f_spc_x,  # transmission function evaluated over spc.x
                "band_l0": band_l0,  # l0 as in [l0, lf], band range considered
                "band_lf": band_lf,  # lf as in [l0, lf], band range considered
                "spc": spc,  # spectrum cut to l0, lf, used in calculations
                "cmag": cmag,  # calculated magnitude
                "weighted_mean_flux": weighted_mean_flux,  # weighted-average flux where the weights are the transmission function
                "out_y": out_y,  # spc with flux multiplied by transmission function
                "out_area": out_area,  # area under out_y
                "fieldnames": ["MAG_CALC", "MAG_BAND"]  # names of fields added/replaces to self.more_headers
                }


    def resample(self, new_wavelength, kind='linear'):
        """Resamples *in-place* to new wavelength vector using interpolation

        Arguments:
            new_wavelength -- new wavelength vector
            kind -- interpolation kind to be passed to scipy.interpolate.interp1d
        """
        # f = interp1d(self.x, self.y, kind='linear', bounds_error=True, fill_value=0)
        self.y = interp(new_wavelength, self.x, self.y)
        self.wavelength = new_wavelength


class FileSpectrum(DataFile):
    attrs = ['spectrum']

    def __init__(self):
        DataFile.__init__(self)
        self.spectrum = Spectrum()

    def load(self, filename=None):
        # Method was overriden to set spectrum filename automatcially so that
        # descendants don't have to bother about this.
        DataFile.load(self, filename)
        self.spectrum.filename = filename


class FileSpectrumPfant(FileSpectrum):
    """
    Represents a pfant output spectrum file.

    This file alternates a "header" line where most of the information is
    repeated, and a "values" line, with the values of the flux
    corresponding to the lambda interval lzero-lfin
    """

    default_filename = "flux.norm"

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
                    vars_ = f_header.read(s)  # This is actually quite slow, gotta avoid it
                    [sp.ikeytot,
                     sp.tit,
                     sp.tetaef,
                     sp.glog,
                     sp.asalog,
                     sp.modeles_nhe,
                     sp.amg,
                     sp.l0,
                     sp.lf,
                     _,  # lzero,
                     _,  # lfin,
                     _,  # itot
                     sp.pas,
                     sp.echx,
                     sp.echy,
                     sp.fwhm] = vars_


                #itot = vars[11]

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
                    # Last point is discarded because pfant writes reduntantly:
                    # last point of iteration ikey is the same as first point of
                    # iteration ikey+1
                    # y = y + v  # update: taking all points calculated yes [:-1]
                    y = y + v[:-1]
                else:
                    # ...except for in the last calculation interval
                    # (then the last point is used).
                    y = y + v
                    break

                i += 1

                #last_itot = itot

        # Lambdas
        sp.x = np.array([sp.l0 + k * sp.pas for k in range(0, len(y))])
        sp.y = np.array(y)

#        logging.debug("Just read PFANT Spectrum '%s'" % filename)


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

            [_, sp.l0, sp.lf, sp.pas, sp.fwhm] = \
                s_header1.unpack_from(s)
            [sp.l0, sp.lf, sp.pas, sp.fwhm] = \
                map(float, [sp.l0, sp.lf, sp.pas, sp.fwhm])
            #n = int(n)


            # -- rows 03 ... --
            #
            # Examples:
            #    4790.0000000000000       0.99463000000000001
            #    4790.0400000000000        2.0321771294932130E-004

            # pattern = re.compile(r"^\s+([0-9.E+-]+)\s+([0-9.E+-]+)")
            while True:
                s = h.readline().strip()
                if not s:
                  break

                a, b = [float(z) for z in s.split()]
                # if match is None:
                #     raise ParseError('Row %d of file %s is invalid' % (i + 3, filename))
                # a, b = map(float, match.groups())

                x.append(a)
                y.append(b)

        sp.x = np.array(x)
        sp.y = np.array(y)

#        logging.debug("Just read NULBAD Spectrum '%s'" % filename)


class FileSpectrumXY(FileSpectrum):
    """
    Represents a file with two columns: first is lambda (x), second is flux (y)

    File may have comment lines; these will be ignored altogether, because the file
    is read with numpy.loadtxt()
    """

    def _do_load(self, filename):
        A = np.loadtxt(filename)
        if len(A.shape) < 2:
            raise RuntimeError("File %s does not contain 2D array" % filename)
        if A.shape[1] < 2:
            raise RuntimeError("File %s must contain at least two columns" % filename)
        if A.shape[1] > 2:
            logging.warning("File %s has more than two columns, taking only first and second" % filename)
        sp = self.spectrum = Spectrum()
        sp.x = A[:, 0]
        sp.y = A[:, 1]
#        logging.debug("Just read XY Spectrum '%s'" % filename)


    def _do_save_as(self, filename):
        with open(filename, "w") as h:
            print "xxxxxxxxxxxxxxxxxx", self.spectrum.x
            for x, y in zip(self.spectrum.x, self.spectrum.y):
                write_lf(h, "%.10g %.10g" % (x, y))


class FileSpectrumFits(FileSpectrum):
    """
    Represents a FITS file with a spectrum inside
    """

    def _do_load(self, filename):
        fits_obj = fits.open(filename)
        # fits_obj.info()
        hdu = fits_obj[0]
        sp = self.spectrum = Spectrum()
        sp.from_hdu(hdu)

    def _do_save_as(self, filename):
        """Saves spectrum back to FITS file."""

        if len(self.spectrum.x) < 2:
            raise RuntimeError("Spectrum must have at least two points")
        if os.path.isfile(filename):
            os.unlink(filename)  # PyFITS does not overwrite file

        hdu = self.spectrum.to_hdu()
        hdu.writeto(filename)




########################################################################################################################
# Pipeline routines -- spectrum manipulation
#
# **IMPORTANT** all functions here must make copies of original data.
# dest.wavelength = source.wavelength.copy()  <--- correct
# dest.wavelength = source.wavelength  <--- *WRONG*

def cut_spectrum(sp, lambda0, lambda1):
    """Cuts spectrum to interval [lambda0, lambda1]. Returns new"""
    assert isinstance(sp, Spectrum), "sp argument must be a Spectrum!"
    assert lambda0 <= lambda1
    idx0 = np.argmin(np.abs(sp.x-lambda0))
    idx1 = np.argmin(np.abs(sp.x-lambda1))
    return cut_spectrum_idxs(sp, idx0, idx1)


def cut_spectrum_idxs(sp, idx0, idx1):
    """Cuts spectrum using index interval. Returns new"""
    ret = copy.deepcopy(sp)
    ret.x = ret.x[idx0:idx1]
    ret.y = sp.y[idx0:idx1].copy()
    return ret

def normalize_spectrum(sp, method="01"):
    """
    Normalizes spectrum according to specified method. Returns new

    Arguments:
      method="01" -- Normalization method:
        "01": normalizes between 0 and 1
    """

    ret = copy.deepcopy(sp)
    if method == "01":
        miny, maxy = np.min(ret.y), np.max(ret.y)
        if miny == maxy:
            raise RuntimeError("Cannot normalize, y-span is ZERO")
        ret.y = (ret.y-miny)/(maxy-miny)
    else:
        raise RuntimeError("Invalid method: '%s'" % method)
    return ret
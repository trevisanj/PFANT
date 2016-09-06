import numpy as np

from pyfant import *
from pyfant.data.filespectrumlist import SpectrumList
from pymos.misc import *

__all__ = ["SBlock", "SB_ElementWise", "SB_Rubberband", "SB_AddNoise", "SLBlock", "UseSBlock", "ExtractContinua", "SNR", "MergeDownBlock", "MergeDown"]

# All values in CGS
_C = 299792458*100  # light speed in cm/s

########################################################################################################################

class BaseBlock(object):
    def __init__(self):
        self.flag_copy_wavelength = False
        self._input = None

    def copy_or_not(self, x):
        """Sensitive to self.flag_copy_wavelength"""
        if self.flag_copy_wavelength:
            return np.copy(x)
        else:
            return x


class SBlock(BaseBlock):
    """SBlock -- class with a "use" method accepting only Spectrum as input"""

    def use(self, input):
        assert isinstance(input, Spectrum)
        self._input = input
        try:
            # If the output wavelength vector is the same, flag_copy_wavelength determines whether this vector
            # will be copied or just assigned to the output
            #
            # **Attention** blocks that handle the wavelength vectors must comply
            output = self._do_use(input)
            assert output is not None  # it is common to forger to return in _do_use()
            if isinstance(output, Spectrum):
                assert output._flag_created_by_block
            # Automatically assigns output wavelength vector if applicable
            if isinstance(output, Spectrum) and output.wavelength is None and len(output.y) == len(input.y):
                output.wavelength = np.copy(input.wavelength) if self.flag_copy_wavelength else input.wavelength
            return output
        finally:
            self._input = None

    def _new_output(self):
        """Call from _do_use() to create new spectrum based on input spectrum.

        Never create spectrum directly because we want to keep certain attributes, such as more_headers"""
        output = Spectrum()
        output._flag_created_by_block = True  # assertion
        output.more_headers = self._input.more_headers
        return output

    def _do_use(self, input):
        """Default implementation is identity"""
        return input


class SB_Rubberband(SBlock):
    """Returns a rubber band"""

    def __init__(self, flag_upper=True):
        SBlock.__init__(self)
        # Upper or lower rubberband
        self.flag_upper = flag_upper

    def _do_use(self, input):
        output = self._new_output()
        y = input.y
        if self.flag_upper:
            y = -y
        output.y = rubberband(y)
        if self.flag_upper:
            output.y = -output.y
        return output


class SB_AddNoise(SBlock):
    def __init__(self, std=1.):
        SBlock.__init__(self)
        # Standard deviation of noise
        self.std = std

    def _do_use(self, input):
        n = len(input)
        output = self._new_output()
        output.y = np.copy(input.y)+np.random.normal(0, self.std, n)
        return output


class SB_FNuToFLambda(SBlock):
    """
    Flux-nu to flux-lambda conversion. Assumes the wavelength axis is in angstrom
    """
    def _do_use(self, input):
        raise NotImplementedError()
        output = self._new_output()
        output.y = input.y

class SB_ElementWise(SBlock):
    """Applies function to input.flux. function must return vector of same dimension as input"""

    def __init__(self, func):
        SBlock.__init__(self)
        self.func = func

    def _do_use(self, input):
        output = self._new_output()
        output.wavelength = self.copy_or_not(input.wavelength)
        output.flux = self.func(input.flux)
        if len(output.flux) != len(output.wavelength):
            raise RuntimeError(
                "func returned vector of length %d, but should be %d" % (len(output.flux), len(output.wavelength)))
        return output


########################################################################################################################

class SLBlock(BaseBlock):
    """SBlock -- class with a "use" method accepting a SpectrumList as input"""

    def use(self, input):
        assert isinstance(input, SpectrumList)
        self._input = input
        try:
            output = self._do_use(input)
            assert output is not None
            if isinstance(output, SpectrumList):
                assert output._flag_created_by_block
            return output
        finally:
            self._input = None

    def _new_output(self):
        """Call from _do_use() to create new SpectrumList based on input"""
        output = SpectrumList()
        output._flag_created_by_block = True  # assertion
        return output

    def _do_use(self, input):
        """Default implementation is identity"""
        return input


class UseSBlock(SLBlock):
    """Calls sblock.use() for each individual spectrum"""
    def __init__(self, sblock=None):
        SLBlock.__init__(self)
        self.sblock = sblock

    def _do_use(self, input):
        output = self._new_output()
        for i, sp in enumerate(input.spectra):
            output.add_spectrum(self.sblock.use(sp))
        return output


class ExtractContinua(SLBlock):
    """Calculates upper envelopes and subtracts mean(noise std)"""

    # TODO this is not a great system. Just de-noising could substantially improve the extracted continua

    def _do_use(self, input):
        output = UseSBlock(SB_Rubberband(flag_upper=True)).use(input)
        spectrum_std = MergeDown(func=np.std).use(input)
        mean_std = np.mean(spectrum_std.spectra[0].y)
        for spectrum in output.spectra:
            spectrum.y -= mean_std*3
        return output


class MergeDownBlock(SLBlock):
    """Base class for all SpectrumList-to-Spectrumlist blocks whose output has only one row in it

    This class has no gear and exists for grouping purposes only"""

class MergeDown(MergeDownBlock):
    """Output contains single spectrum whose y-vector is calculated using a numpy function

    The numpy function must be able to operate on the first axis, e.g., np.mean(), np.std()
    """

    def __init__(self, func=np.std):
        MergeDownBlock.__init__(self)
        self.func = func

    def _do_use(self, input):
        output = self._new_output()
        sp = Spectrum()
        sp.wavelength = self.copy_or_not(input.wavelength)
        sp.flux = self.func(input.matrix(), 0)
        if len(sp.flux) != len(sp.wavelength):
            raise RuntimeError("func returned vector of length %d, but should be %d" % (len(sp.flux), len(sp.wavelength)))
        output.add_spectrum(sp)
        return output

class SNR(MergeDownBlock):
    """Calculates the SNR(lambda) = Power_signal(lambda)/Power_noise(lambda)

    Arguments:
        continua -- SpectrumList containing the continua that will be used as the "signal" level.
                    If not passed, will be calculated from the input spectra using a SB_Rubberband(True) block

    References:
        [1] https://en.wikipedia.org/wiki/Signal_averaging
    """

    # TODO I think that it is more correct to talk about "continuum" not continua

    def __init__(self, continua=None):
        MergeDownBlock.__init__(self)
        self.continua = continua

    def _do_use(self, input):
        if self.continua is None:
            continua = UseSBlock(SB_Rubberband(True)).use(input)
        else:
            continua = self.continua
        cont_2 = UseSBlock(SB_ElementWise(np.square)).use(continua)  # continua squared
        mean_cont_2 = MergeDown(np.mean).use(cont_2)
        var_spectra = MergeDown(np.var).use(input)
        output = self._new_output()
        sp = Spectrum()
        sp.wavelength = self.copy_or_not(input.wavelength)
        sp.flux = mean_cont_2.spectra[0].flux/var_spectra.spectra[0].flux
        output.add_spectrum(sp)
        return output

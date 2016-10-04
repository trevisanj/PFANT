__all__ = ["SBlock", "SB_ElementWise", "SB_Rubberband", "SB_AddNoise", "SLBlock", "SLB_UseSBlock",
           "SLB_ExtractContinua",
           "SLB_SNR", "SLB_MergeDownBlock", "SLB_MergeDown", "SB_Extend", "SB_SNR"]

import numpy as np
from pyfant import *
from pyfant.data.filesplist import SpectrumList
import copy

# All values in CGS
_C = 299792458*100  # light speed in cm/s

########################################################################################################################

class BaseBlock(object):
    def __init__(self):
        self._input = None



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
        output.more_headers = copy.deepcopy(self._input.more_headers_)
        return output

    def _copy_input(self, input):
        """
        Returns copy of input + extra annotations

        Don't copy spectra explicitly; use this method instead
        """

        output = copy.deepcopy(input)
        output._flag_created_by_block = True

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
        output.wavelength = np.copy(input.wavelength)
        output.flux = self.func(input.flux)
        if len(output.flux) != len(output.wavelength):
            raise RuntimeError(
                "func returned vector of length %d, but should be %d" % (len(output.flux), len(output.wavelength)))
        return output


class SB_Extend(SBlock):
    """
    Extends to left and/or right side

    Arguments:
      fraction -- amount relative to number of points. Note that this
                  applies individually to left and right (see below)
      flag_left -- whether to extend by fraction to left
      flag_right -- whether to extend by fraction to right

    The y-value to use is found by using a "coarse" 2nd-order polynomial baseline.
    The baseline is "coarse" because it does not allow for many iterations until the
    baseline is found

    **Case**
    >> self.extend(.1, True, True)
    # if original has 100 points, resulting will have 120 points

    >> self.extend(.1, True, False)
    # if original has 100 points, resulting will have 110 points
    """

    def __init__(self, fraction=.1, flag_left=True, flag_right=False):
        SBlock.__init__(self)
        self.fraction = fraction
        self.flag_left = flag_left
        self.flag_right = flag_right

    def _do_use(self, input):
        output = self._copy_input(input)

        if not (self.flag_left or self.flag_right):
            return output
        num_add = int(self.fraction*len(output.wavelength))
        if num_add <= 0:
            return output

        x_left, x_right, y_left, y_right = np.array([]), np.array([]), np.array([]), np.array([])

        rubber = -poly_baseline(-output.y, 2, maxit=15)

        if self.flag_left:
            x_left = np.arange(num_add)*output.delta_lambda+(output.x[0]-output.delta_lambda*num_add)
            y_left = np.ones(num_add)*rubber[0]

        if self.flag_right:
            x_right = np.arange(num_add) * output.delta_lambda + (output.x[-1] + output.delta_lambda)
            y_right = np.ones(num_add) * rubber[-1]

        output.x = np.concatenate((x_left, output.x, x_right))
        output.y = np.concatenate((y_left, output.y, y_right))

        return output


class SB_SNR(SBlock):
    """
    Calculates Signal-to-noise ratio (SNR) using a part of the "signal" (i.e. the spectrum)

    The signal-to-noise ratio (SNR) is often defined as (signal power) / (noise power), herein
    calculated as

        y_RMS**2 / variance(y)     (https://en.wikipedia.org/wiki/Signal-to-noise_ratio)

    It is assumed that the "signal" is *stationary* within [llzero, llfin]
    meaning that the mean and variance of the "signal" is the same for all points within
    this region (more precisely "weak-sense stationary"
    (https://en.wikipedia.org/wiki/Stationary_process#Weak_or_wide-sense_stationarity))
    """

    def __init__(self, llzero, llfin):
        SBlock.__init__(self)
        self.llzero = llzero
        self.llfin = llfin

    def _do_use(self, input):
        x = input.x
        y = input.y
        signal = y[np.logical_and(x >= self.llzero, x <= self.llfin)]

        output = np.mean(signal**2)/np.var(signal)
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


class SLB_UseSBlock(SLBlock):
    """Calls sblock.use() for each individual spectrum"""
    def __init__(self, sblock=None):
        SLBlock.__init__(self)
        self.sblock = sblock

    def _do_use(self, input):
        output = self._new_output()
        for i, sp in enumerate(input.spectra):
            output.add_spectrum(self.sblock.use(sp))
        return output


class SLB_ExtractContinua(SLBlock):
    """Calculates upper envelopes and subtracts mean(noise std)"""

    # TODO this is not a great system. Just de-noising could substantially improve the extracted continua

    def _do_use(self, input):
        output = SLB_UseSBlock(SB_Rubberband(flag_upper=True)).use(input)
        spectrum_std = SLB_MergeDown(func=np.std).use(input)
        mean_std = np.mean(spectrum_std.spectra[0].y)
        for spectrum in output.spectra:
            spectrum.y -= mean_std*3
        return output


class SLB_MergeDownBlock(SLBlock):
    """Base class for all SpectrumList-to-Spectrumlist blocks whose output has only one row in it

    This class has no gear and exists for grouping purposes only"""

class SLB_MergeDown(SLB_MergeDownBlock):
    """Output contains single spectrum whose y-vector is calculated using a numpy function

    The numpy function must be able to operate on the first axis, e.g., np.mean(), np.std()
    """

    def __init__(self, func=np.std):
        SLB_MergeDownBlock.__init__(self)
        self.func = func

    def _do_use(self, input):
        output = self._new_output()
        sp = Spectrum()
        sp.wavelength = np.copy(input.wavelength)
        sp.flux = self.func(input.matrix(), 0)
        if len(sp.flux) != len(sp.wavelength):
            raise RuntimeError("func returned vector of length %d, but should be %d" % (len(sp.flux), len(sp.wavelength)))
        output.add_spectrum(sp)
        return output

class SLB_SNR(SLB_MergeDownBlock):
    """Calculates the SNR(lambda) = Power_signal(lambda)/Power_noise(lambda)

    Arguments:
        continua -- SpectrumList containing the continua that will be used as the "signal" level.
                    If not passed, will be calculated from the input spectra using a SB_Rubberband(True) block

    References:
        [1] https://en.wikipedia.org/wiki/Signal_averaging
    """

    # TODO I think that it is more correct to talk about "continuum" not continua

    def __init__(self, continua=None):
        SLB_MergeDownBlock.__init__(self)
        self.continua = continua

    def _do_use(self, input):
        if self.continua is None:
            continua = SLB_UseSBlock(SB_Rubberband(True)).use(input)
        else:
            continua = self.continua
        cont_2 = SLB_UseSBlock(SB_ElementWise(np.square)).use(continua)  # continua squared
        mean_cont_2 = SLB_MergeDown(np.mean).use(cont_2)
        var_spectra = SLB_MergeDown(np.var).use(input)
        output = self._new_output()
        sp = Spectrum()
        sp.wavelength = np.copy(input.wavelength)
        sp.flux = mean_cont_2.spectra[0].flux/var_spectra.spectra[0].flux
        output.add_spectrum(sp)
        return output

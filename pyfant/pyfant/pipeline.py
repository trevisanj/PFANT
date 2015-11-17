"""Pipeline routines -- manipulation of spectra."""
__all__ = ["cut_spectrum"]

from pyfant import Spectrum
import numpy as np

def cut_spectrum(sp, lambda0, lambda1):
    """
    Cuts spectrum to interval [lambda0, lambda1]
    """
    assert isinstance(sp, Spectrum), "sp argument must be a Spectrum!"
    idx0 = np.argmin(np.abs(sp.x-lambda0))
    idx1 = np.argmin(np.abs(sp.x-lambda1))
    ret = Spectrum()
    ret.x = sp.x[idx0:idx1]
    ret.y = sp.y[idx0:idx1]

    return ret


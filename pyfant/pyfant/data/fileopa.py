"""
MARCS ".opa" file format -- opacity model."""


__all__ = ["FileOpa"]
from .datafile import *
from ..misc import *
import struct
import numpy as np

NINE_HUNDRED = 900

class FileOpa(DataFile):
    """MARCS ".opa" (opacity model) file format.

    Reference: http://marcs.astro.uu.se
    """

    attrs = ["ndp", "swave", "nwav"]

    def __init__(self):
        DataFile.__init__(self)

        # # Global properties of the opacity model file
        # the 4-byte standard model code 'MRXF'
        self.mcode = None
        # number of depth points (=56)
        self.ndp = None
        # standard wavelength for the continuous optical depth (tau)
        # scale and for the total standard opacity (ops)
        self.swave = None
        # number of wavelengths for which continuous absorption and
        # scattering opacities are given. These are chosen so that
        # linear interpolation should suffice for any wavelength
        # (=1071)
        self.nwav = None
        # wav(j): wavelengths for which opacities are given
        self.wav = None

        # # Model structure
        # rad(k): radius, normalized on the outermost point, k=1. For use with
        # spherical radiative transfer. For plane-parallel models rad == 1.0.
        self.rad = None
        # tau(k): continuumm optical depth at the standard wavelength swave
        self.tau = None
        # t(k): temperature (K)
        self.t = None
        # pe(k): electron pressure (dyn/cm2)
        self.pe = None
        # pg(k)  = total gas pressure (dyn/cm2)
        self.pg = None
        # rho(k) = densigy (g/cm3)
        self.rho = None
        # xi(k)  = microturbulence parameter (km/s)
        self.xi = None
        # ops(k) = continuumm opacity at the standard wavelength (cm2/g)
        self.ops = None

        # # Wavelength-dependent opacities
        # abs(j,k) = specific continuous absorption opacity (cm2/g)
        self.abs = None
        # sca(j,k) = specific continuous scattering opacity (cm2/g)
        self.sca = None


    def cut(self, llzero, llfin):
        """Keeps only region within lambda interval [llzero, llfin]."""
        mask = np.logical_and(llzero <= self.wav, self.wav <= llfin)
        self.sca = self.sca[mask, :]
        self.abs = self.abs[mask, :]
        self.wav = self.wav[mask]
        self.nwav = len(self.wav)

    def _do_load(self, filename):
        """Loads from file.

        Based on http://marcs.astro.uu.se/documents/auxiliary/readopa.f
        """

        with open(filename, "r") as h:
            self.mcode, self.ndp, self.swave = struct.unpack("1x 4s 5s 10s", readline_strip(h))
            if self.mcode != "MRXF":
                # Does not satisfy magic string
                raise RuntimeError("Model code '%s' is not 'MRXF'" % self.mcode)
            self.ndp = int(self.ndp)
            self.swave = float(self.swave)

            self.nwav = int(h.readline())

            v, n_rows = multirow_str_vector(h, self.nwav)
            self.wav = np.array(map(float, v))

            self.rad, self.tau, self.t, self.pe, self.pg, self.rho, self.xi, \
            self.ops = np.zeros(self.ndp), np.zeros(self.ndp), np.zeros(self.ndp), \
                       np.zeros(self.ndp), np.zeros(self.ndp), np.zeros(self.ndp), \
                       np.zeros(self.ndp), np.zeros(self.ndp)
            self.abs = np.zeros((self.nwav, self.ndp))
            self.sca = np.zeros((self.nwav, self.ndp))
            for k in range(self.ndp):
                self.rad[k], self.tau[k], self.t[k], self.pe[k], self.pg[k], \
                self.rho[k], self.xi[k], self.ops[k] = float_vector(h)
                v, n_rows = multirow_str_vector(h, 2*self.nwav)
                abs_sca = np.array(map(float, v))
                # This multiplication is performed as in original readopa.f
                self.abs[:, k] = abs_sca[0::2]*self.ops[k]
                self.sca[:, k] = abs_sca[1::2]*self.ops[k]


                # CLIP = 200
                # self.nwav = CLIP
                # self.wav = self.wav[:CLIP]
                # self.abs = self.abs[:CLIP, :]
                # self.sca = self.sca[:CLIP, :]
                # print "FILEOPAFILEOPAFILEOPAFILEOPAFILEOPA"
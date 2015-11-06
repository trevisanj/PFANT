"""
Routines used by plot-spectra.py
"""
from pyfant import *
import matplotlib.pyplot as plt
# import numpy as np
# import matplotlib as mpl
# from mpl_toolkits.mplot3d import Axes3D  # yes, required (see below)
__all__ = ["plot_spectra", "plot_spectra_overlapped"]


def plot_spectra(ss, title=None):
    """
    Plots one or more stacked in subplots sharing same x-axis.

    Arguments:
      ss -- list of Spectrum objects
      title=None -- window title
    """

    n = len(ss)

    format_BLB()
    if n == 1:
        f = plt.figure()
    else:
        f, axarr = plt.subplots(n, sharex=True)

    T = 0.02  # amount of extra space on both left and right of graphics

    mi = 1e38
    ma = -1e38
    for i, s in enumerate(ss):
        assert isinstance(s, Spectrum)
        if n == 1:
            ax = plt.gca()
        else:
            ax = axarr[i]

        ax.plot(s.x, s.y)
        ymi, yma = ax.get_ylim()
        ax.set_ylim([ymi, ymi + (yma - ymi) * (1 + T)])  # prevents top of line from being hidden by plot box
        ax.set_ylabel(s.filename)

        mi, ma = min(min(s.x), mi), max(max(s.x), ma)

        if i == n-1:
            ax.set_xlabel('Wavelength')
            span = ma - mi
            ax.set_xlim([mi - span * T, ma + span * T])

    plt.tight_layout()
    if title is not None:
        f.canvas.set_window_title(title)


def plot_spectra_overlapped(ss, title=None):
    """
    Plots one or more stacked in subplots sharing same x-axis.

    Arguments:
      ss -- list of Spectrum objects
      title=None -- window title
    """

    n = len(ss)

    format_BLB()
    f = plt.figure()

    T = 0.02  # amount of extra space on both left and right of graphics

    mi = 1e38
    ma = -1e38
    for i, s in enumerate(ss):
        assert isinstance(s, Spectrum)
        ax = plt.gca()

        ax.plot(s.x, s.y, label=s.filename)
        ymi, yma = ax.get_ylim()
        # ax.set_ylim([ymi, ymi + (yma - ymi) * (1 + T)])  # prevents top of line from being hidden by plot box
        # ax.set_ylabel(s.filename)

        mi, ma = min(min(s.x), mi), max(max(s.x), ma)

    plt.xlabel('Wavelength')
    span = ma - mi
    ax.set_xlim([mi - span * T, ma + span * T])


    plt.legend(loc=0)
    plt.tight_layout()
    if title is not None:
        f.canvas.set_window_title(title)










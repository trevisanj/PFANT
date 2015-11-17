"""
Routines used by plot-spectra.py
"""
from pyfant import format_BLB, cut_spectrum, Spectrum
import matplotlib.pyplot as plt
import math
import matplotlib.backends.backend_pdf
import numpy as np
# import matplotlib as mpl
# from mpl_toolkits.mplot3d import Axes3D  # yes, required (see below)
__all__ = ["plot_spectra", "plot_spectra_overlapped", "plot_spectra_pieces_pdf"]


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
    Plots one or more spectra in the same plot.

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


def plot_spectra_pieces_pdf(ss, aint=50, pdf_filename='pieces.pdf'):
    """
    Plots spectra in parts into a PDF file.

    Splits into several pieces of width

    Arguments:
      ss -- list of Spectrum objects
      title=None -- window title
    """

    # Finds minimum and maximum wavelength
    xmi, xma, ymi, yma = 1e38, -1e38, 1e38, -1e38
    for s in ss:
        assert isinstance(s, Spectrum)
        xmi, xma = min(min(s.x), xmi), max(max(s.x), xma)
        ymi, yma = min(min(s.y), ymi), max(max(s.y), yma)
    yspan = yma-ymi  # same y-limits will be used for all pages


    num_pages = int(math.ceil((xma-xmi)/aint)) # rightmost point may be left out...or not
    # num_spectra = len(ss)

    format_BLB()
    pdf = matplotlib.backends.backend_pdf.PdfPages(pdf_filename)

    T = 0.02  # percentual amount of extra space on left, right, top, bottom of graphics

    for h in range(num_pages):
        fig = plt.figure()
        lambda0 = xmi+h*aint
        lambda1 = lambda0+aint

        print "Printing page %d/%d ([%g, %g])" % (h+1, num_pages, lambda0, lambda1)

        for i, s in enumerate(ss):
            s_cut = cut_spectrum(s, lambda0, lambda1)
            ax = plt.gca()
            ax.plot(s_cut.x, s_cut.y, label=s.filename)

        plt.xlabel('Wavelength (interval: [%g, %g])' % (lambda0, lambda1))
        xspan = lambda1-lambda0
        ax.set_xlim([lambda0 - xspan * T, lambda1 + xspan * T])
        ax.set_ylim([ymi - yspan * T, yma + yspan * T])


        plt.legend(loc=0)
        plt.tight_layout()


        pdf.savefig(fig)
        plt.close()

    # for fig in xrange(1, figure().number): ## will open an empty extra figure :(
    #     pdf.savefig( fig )
    pdf.close()












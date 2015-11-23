"""
Routines used by plot-spectra.py
"""
from pyfant import format_BLB, cut_spectrum, Spectrum
import matplotlib.pyplot as plt
import math
import matplotlib.backends.backend_pdf
import logging
import numpy as np
# import matplotlib as mpl
# from mpl_toolkits.mplot3d import Axes3D  # yes, required (see below)
__all__ = ["plot_spectra", "plot_spectra_overlapped", "plot_spectra_pieces_pdf",
 "plot_spectra_pages_pdf"]


_T = 0.02  # percentual amount of extra space on left, right, top, bottom of graphics
_FAV_COLOR = 'k'  # "favourite color" for single-spectrum plots

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
        fig = plt.figure()
    else:
        fig, axarr = plt.subplots(n, sharex=True)

    xmin = 1e38
    xmax = -1e38
    for i, s in enumerate(ss):
        assert isinstance(s, Spectrum)
        if n == 1:
            ax = plt.gca()
        else:
            ax = axarr[i]

        ax.plot(s.x, s.y)
        ymin, ymax = ax.get_ylim()
        ax.set_ylim([ymin, ymin + (ymax - ymin) * (1 + _T)])  # prevents top of line from being hidden by plot box
        ax.set_ylabel(s.filename)

        xmin, xmax = min(min(s.x), xmin), max(max(s.x), xmax)

        if i == n-1:
            ax.set_xlabel('Wavelength')
            span = xmax - xmin
            ax.set_xlim([xmin - span * _T, xmax + span * _T])

    plt.tight_layout()
    if title is not None:
        fig.canvas.set_window_title(title)
    return fig


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

    xmin = 1e38
    xmax = -1e38
    for i, s in enumerate(ss):
        assert isinstance(s, Spectrum)
        ax = plt.gca()

        ax.plot(s.x, s.y, label=s.filename)
        # ymin, ymax = ax.get_ylim()
        # ax.set_ylim([ymin, ymin + (ymax - ymin) * (1 + T)])  # prevents top of line from being hidden by plot box
        # ax.set_ylabel(s.filename)

        xmin, xmax = min(min(s.x), xmin), max(max(s.x), xmax)

    plt.xlabel('Wavelength')
    span = xmax - xmin
    ax.set_xlim([xmin - span * _T, xmax + span * _T])


    plt.legend(loc=0)
    plt.tight_layout()
    if title is not None:
        f.canvas.set_window_title(title)


def plot_spectra_pieces_pdf(ss, aint=10, pdf_filename='pieces.pdf'):
    """
    Plots spectra, overlapped, in small wavelength intervals into a PDF file,
    one interval per page of the PDF file.

    Arguments:
      ss -- list of Spectrum objects
      aint -- wavelength interval for each plot
      pdf_filename -- name of output file
    """

    xmin, xmax, ymin, ymax, _, yspan = _calc_max_min(ss)

    num_pages = int(math.ceil((xmax-xmin)/aint)) # rightmost point may be left out...or not
    # num_spectra = len(ss)

    format_BLB()
    pdf = matplotlib.backends.backend_pdf.PdfPages(pdf_filename)

    for h in range(num_pages):
        fig = plt.figure()
        lambda0 = xmin+h*aint
        lambda1 = lambda0+aint
        logging.info("Printing page %d/%d ([%g, %g])" % (h+1, num_pages, lambda0, lambda1))
        for i, s in enumerate(ss):
            s_cut = cut_spectrum(s, lambda0, lambda1)
            ax = plt.gca()
            ax.plot(s_cut.x, s_cut.y, label=s.filename)
        plt.xlabel('Wavelength (interval: [%g, %g])' % (lambda0, lambda1))
        xspan = lambda1-lambda0
        ax.set_xlim([lambda0 - xspan * _T, lambda1 + xspan * _T])
        ax.set_ylim([ymin - yspan * _T, ymax + yspan * _T])
        plt.legend(loc=0)
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close()

    # for fig in xrange(1, figure().number): ## will open an empty extra figure :(
    #     pdf.savefig( fig )
    pdf.close()
    logging.info("File %s successfully created." % pdf_filename)


def plot_spectra_pages_pdf(ss, pdf_filename='pages.pdf'):
    """
    Plots spectra into a PDF file, one spectrum per page.

    Splits into several pieces of width

    Arguments:
      ss -- list of Spectrum objects
      pdf_filename -- name of output file
    """
    xmin, xmax, ymin, ymax, xspan, yspan = _calc_max_min(ss)
    num_pages = len(ss)
    format_BLB()
    pdf = matplotlib.backends.backend_pdf.PdfPages(pdf_filename)
    for i, s in enumerate(ss):
        title = s.filename
        fig = plt.figure()
        plt.plot(s.x, s.y, c=_FAV_COLOR)
        plt.xlabel('Wavelength')
        plt.title(title)
        plt.xlim([xmin-xspan*_T, xmax+xspan*_T])
        plt.ylim([ymin-yspan*_T, ymax+yspan*_T])
        plt.tight_layout()
        plt.subplots_adjust(top=0.94) # workaround for cropped title
        logging.info("Printing page %d/%d ('%s')" % (i+1, num_pages, title))
        pdf.savefig(fig)
        plt.close()
    pdf.close()
    logging.info("File %s successfully created." % pdf_filename)


def _calc_max_min(ss):
    """"Calculates (x, y) (max, min) for a list of Spectrum objects.

    Returns (xmin, xmax, ymin, ymax, xspan, yspan)
    """
    xmin, xmax, ymin, ymax = 1e38, -1e38, 1e38, -1e38
    for s in ss:
        assert isinstance(s, Spectrum)
        xmin, xmax = min(min(s.x), xmin), max(max(s.x), xmax)
        ymin, ymax = min(min(s.y), ymin), max(max(s.y), ymax)
    xspan = xmax-xmin
    yspan = ymax - ymin
    return xmin, xmax, ymin, ymax, xspan, yspan

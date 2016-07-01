"""
Routines used by plot-spectra.py
"""
from pyfant import *
import matplotlib.pyplot as plt
import math
import matplotlib.backends.backend_pdf
import logging
import numpy as np
__all__ = ["plot_spectra", "plot_spectra_overlapped", "plot_spectra_pieces_pdf",
 "plot_spectra_pages_pdf"]


_T = 0.02  # percentual amount of extra space on left, right, top, bottom of graphics
_FAV_COLOR = 'k'  # "favourite color" for single-spectrum plots

def plot_spectra(ss, title=None, ymin=None, num_rows=None):
    """
    Plots one or more stacked in subplots sharing same x-axis.

    Arguments:
      ss -- list of Spectrum objects
      title=None -- window title
      ymin -- (optional) force mininum y-value. If not passed, ymin is
              calculated every subplot
      num_rows=None -- (optional) number of rows for subplot grid. If not passed,
        num_rows will be the number of plots, and the number of columns will be 1.
        If passed, number of columns is calculated automatically.

    """

    n = len(ss)
    assert n > 0, "ss is empty"
    if not num_rows:
        num_rows = n
        num_cols = 1
    else:
        num_cols = int(np.ceil(float(n)/num_rows))

    format_BLB()
    # if n == 1:
    #     fig = plt.figure()
    # else:
    fig, axarr = plt.subplots(num_rows, num_cols, sharex=True, squeeze=False)

    xmin = 1e38
    xmax = -1e38
    i, j = -1, num_cols
    for s in ss:
        j += 1
        if j >= num_cols:
            i += 1
            if i >= num_rows:
                break
            j = 0
        assert isinstance(s, Spectrum)
        ax = axarr[i, j]

        ax.plot(s.x, s.y)
        ymin_, ymax = ax.get_ylim()
        ymin_now = ymin_ if ymin is None else ymin
        ax.set_ylim([ymin_now, ymin_now + (ymax - ymin_now) * (1 + _T)])  # prevents top of line from being hidden by plot box
        ax.set_ylabel(s.filename)

        xmin, xmax = min(min(s.x), xmin), max(max(s.x), xmax)

    span = xmax - xmin
    ax.set_xlim([xmin - span * _T, xmax + span * _T])

    for j in range(num_cols):
        ax = axarr[num_rows-1, j]
        ax.set_xlabel('Wavelength ($\AA$)')

    plt.tight_layout()
    if title is not None:
        fig.canvas.set_window_title(title)
    plt.show()
    # return fig


def plot_spectra_overlapped(ss, title=None, ymin=None):
    """
    Plots one or more spectra in the same plot.

    Arguments:
      ss -- list of Spectrum objects
      title=None -- window title
      ymin -- (optional) force mininum y-value
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

    plt.xlabel('Wavelength ($\AA$)')
    xmin, xmax, ymin_, ymax, xspan, yspan = _calc_max_min(ss)
    if ymin is None:
        ymin = ymin_
    plt.xlim([xmin-xspan*_T, xmax+xspan*_T])
    plt.ylim([ymin-yspan*_T, ymax+yspan*_T])


    leg = plt.legend(loc=0)
    format_legend(leg)

    plt.tight_layout()
    if title is not None:
        f.canvas.set_window_title(title)
    plt.show()


def plot_spectra_pieces_pdf(ss, aint=10, pdf_filename='pieces.pdf', ymin=None):
    """
    Plots spectra, overlapped, in small wavelength intervals into a PDF file,
    one interval per page of the PDF file.

    Arguments:
      ss -- list of Spectrum objects
      aint -- wavelength interval for each plot
      pdf_filename -- name of output file
      ymin -- (optional) force mininum y-value
    """

    xmin, xmax, ymin_, ymax, _, yspan = _calc_max_min(ss)

    if ymin is None:
        ymin = ymin_

    num_pages = int(math.ceil((xmax-xmin)/aint)) # rightmost point may be left out...or not
    # num_spectra = len(ss)

    format_BLB()
    pdf = matplotlib.backends.backend_pdf.PdfPages(pdf_filename)
    logger = get_python_logger()

    for h in range(num_pages):
        fig = plt.figure()
        lambda0 = xmin+h*aint
        lambda1 = lambda0+aint
        logger.info("Printing page %d/%d ([%g, %g])" % (h+1, num_pages, lambda0, lambda1))
        for i, s in enumerate(ss):
            s_cut = cut_spectrum(s, lambda0, lambda1)
            ax = plt.gca()
            ax.plot(s_cut.x, s_cut.y, label=s.filename)
        plt.xlabel('Wavelength (interval: [%g, %g])' % (lambda0, lambda1))
        xspan = lambda1-lambda0
        ax.set_xlim([lambda0 - xspan * _T, lambda1 + xspan * _T])
        ax.set_ylim([ymin - yspan * _T, ymax + yspan * _T])
        leg = plt.legend(loc=0)
        format_legend(leg)
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close()

    # for fig in xrange(1, figure().number): ## will open an empty extra figure :(
    #     pdf.savefig( fig )
    pdf.close()
    logger.info("File %s successfully created." % pdf_filename)


def plot_spectra_pages_pdf(ss, pdf_filename='pages.pdf', ymin=None):
    """
    Plots spectra into a PDF file, one spectrum per page.

    Splits into several pieces of width

    Arguments:
      ss -- list of Spectrum objects
      pdf_filename -- name of output file
      ymin -- (optional) force mininum y-value
    """
    logger = get_python_logger()
    xmin, xmax, ymin_, ymax, xspan, yspan = _calc_max_min(ss)
    if ymin is None:
        ymin = ymin_
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
        logger.info("Printing page %d/%d ('%s')" % (i+1, num_pages, title))
        pdf.savefig(fig)
        plt.close()
    pdf.close()
    logger.info("File %s successfully created." % pdf_filename)


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

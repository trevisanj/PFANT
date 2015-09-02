"""
Assorted functions for plotting file objects

References:
http://matplotlib.org/mpl_toolkits/mplot3d/tutorial.html#line-plots

"""
from pypfant import *
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # yes, required (see below)

__all__ = ["plot_spectra"]

#
# def plot_filetoh(fig, r, title):
#     """
#     Plots hydrogen lines: each atmospheric layer is plotted as a y-axis-dislocated
#     Spectrum in a 3D plot.
#     """
#     assert isinstance(fig, mpl.figure.Figure)
#     assert isinstance(r, FileToH)
#     mpl.rcParams['legend.fontsize'] = 10
#
#     fig.canvas.set_window_title(title)  # requires the Axes3D module
#
#     ax = fig.gca(projection='3d')
#
#     x = np.concatenate((2 * r.lambdh[0] - r.lambdh[-2::-1], r.lambdh))
#     _y = np.ones(len(x))
#     for i in range(r.th.shape[1]):
#         z = np.concatenate((r.th[-2::-1, i], r.th[:, i]))
#
#         ax.plot(x, _y * (i + 1), z, label='a', color='k')
#
#     ax.set_xlabel('Wavelength (A)')
#     ax.set_ylabel('Atmospheric layer')
#     ax.set_zlabel('Intensity (?)')
#

# def plot_mod_record(r, title):
#     """
#     Plots vectors (nh, teta, pe, pg, t5l) of ".mod" file in 5 subplots sharing
#     the same x-axis.
#     """
#     # plt.subplots() will create the figure anyway, so no point in passing figure as argument
#
#     assert isinstance(r, ModRecord)
#
#     f, axarr = plt.subplots(5, sharex=True)
#     f.canvas.set_window_title(title)
#     x = np.linspace(1, r.ntot)
#     axarr[0].plot(x, r.nh)
#     axarr[0].set_ylabel('nh')
#     axarr[1].plot(x, r.teta)
#     axarr[1].set_ylabel('teta')
#     axarr[2].plot(x, r.pe)
#     axarr[2].set_ylabel('pe')
#     axarr[3].plot(x, r.pg)
#     axarr[3].set_ylabel('pg')
#     axarr[4].plot(x, r.t5l)
#     axarr[4].set_ylabel('t5l')
#     axarr[4].set_xlabel("Atmospheric layer #")
#     plt.tight_layout()


# def plot_mod_records(m, title):
#     """
#     Plots vectors
#     (teff, glog, asalog, asalalf, nhe) in 2D (record #)x(value) plots, and
#     (nh, teta, pe, pg, t5l) (layer #)x(record #)x(value) 3D plots
#     """
#
#     assert isinstance(m, FileMod)
#
#     nr = len(m)
#
#     # 5 subplots sharing same x-axis
#     # Plotting teff, glog, ...
#     f, axarr = plt.subplots(5, sharex=True)
#     x = np.linspace(1, nr, nr)
#     rr = m.records
#     aa = ['teff', 'glog', 'asalog', 'asalalf', 'nhe']
#     n = len(aa)
#     for i, a in enumerate(aa):
#         v = []
#         for r in rr:
#             v.append(r.__getattribute__(a))
#
#         axarr[i].plot(x, v)
#
#         axarr[i].set_ylabel(a)
#     axarr[4].set_xlabel("Record #")
#     f.canvas.set_window_title("%s -- %s" % (title, 'one-value-per-model'))
#     plt.tight_layout()
#     #################
#     # 3D plots
#     vars = ['nh', 'teta', 'pe', 'pg', 't5l']
#     for var in vars:
#
#         fig = plt.figure()
#         ax = fig.gca(projection='3d')
#         fig.canvas.set_window_title('%s -- %s' % (title, var))
#
#         for i, r in enumerate(rr):
#             x = np.linspace(1, r.ntot, r.ntot)
#             y = np.ones(len(x)) * (i + 1)
#             z = r.__getattribute__(var)
#
#             # print "#%d - %d; (%d, %d, %d)" % (i, r.ntot, len(x), len(y), len(z))
#
#             ax.plot(x, y, z, label='a', color='k')
#
#         # ax.set_xlabel('Wavelength (A)')
#         # ax.set_ylabel('Atmospheric layer')
#         ax.set_xlabel('Atmospheric layer')
#         ax.set_ylabel('Record number')
#         ax.set_zlabel(var)


def plot_spectra(ss, title=None):
    """
    Plots one or more stacked in subplots sharing same x-axis.

    Arguments:
      ss -- list of Spectrum objects
      title=None -- window title
    """

    n = len(ss)

    def plot1(x, y):
        pass

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

        #print "mimimimamama", mi, ma, s.x

        if i == n-1:
            ax.set_xlabel('Wavelength')
            span = ma - mi
            ax.set_xlim([mi - span * T, ma + span * T])



    plt.tight_layout()
    if title is not None:
        f.canvas.set_window_title(title)










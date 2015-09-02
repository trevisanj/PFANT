__all__ = ["VisPrint", "VisModRecord", "VisModRecords", "VisSpectrum", "VisFileToH", "get_classes"]

from pypfant.data import *
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D  # yes, required (see below)


def get_classes(obj):
    """Retuns a list of Vis classes that can handle obj."""

    ret = []
    for name in __all__:
        if name.startswith("Vis"):
            class_ = eval(name)
            if isinstance(obj, class_.input_classes):
                ret.append(class_)
    return ret

class Vis(object):

    # Set the classes accepted by the use() method
    input_classes = ()

    def __init__(self):
        self.title = None

    def use(self, obj):
        """Note: if title is None, will be replaced with obj.filename
        """

        if not isinstance(obj, self.input_classes):
            raise RuntimeError('%s cannot handle a %s' %
                               (self.__class__.__name__, obj.__class__.__name__))

        if self.title is None:
            self.title = 'file: '+obj.filename
        self._do_use(obj)

    def _do_use(self, obj):
        raise NotImplementedError()


class VisPrint(Vis):
    """Prints object to screen."""

    input_classes = (object,)

    def _do_use(self, obj):
        print obj

class VisModRecord(Vis):
    """
    Plots vectors (nh, teta, pe, pg, t5l) of ".mod" file in 5 subplots sharing
    the same x-axis.
    """

    input_classes = (FileMod,)

    def __init__(self):
        Vis.__init__(self)
        # 1-based (first is 1 (as in Fortran), not 0) record index in .mod file
        self.inum = 1

    def _do_use(self, obj):
        # plt.subplots() will create the figure anyway, so no point in passing figure as argument

        assert isinstance(obj, FileMod)

        r = obj.records[self.inum-1]


        f, axarr = plt.subplots(5, sharex=True)
        f.canvas.set_window_title(self.title)
        x = np.linspace(1, r.ntot)
        axarr[0].plot(x, r.nh)
        axarr[0].set_ylabel('nh')
        axarr[1].plot(x, r.teta)
        axarr[1].set_ylabel('teta')
        axarr[2].plot(x, r.pe)
        axarr[2].set_ylabel('pe')
        axarr[3].plot(x, r.pg)
        axarr[3].set_ylabel('pg')
        axarr[4].plot(x, r.t5l)
        axarr[4].set_ylabel('t5l')
        axarr[4].set_xlabel("Atmospheric layer #")
        plt.tight_layout()
        plt.show()



class VisModRecords(Vis):
    """
    Plots vectors
    (teff, glog, asalog, asalalf, nhe) in 2D (record #)x(value) plots, and
    (nh, teta, pe, pg, t5l) (layer #)x(record #)x(value) 3D plots
    """

    input_classes = (FileMod,)

    def _do_use(self, m):
        nr = len(m)

        # 5 subplots sharing same x-axis
        # Plotting teff, glog, ...
        aa = ['teff', 'glog', 'asalog', 'asalalf', 'nhe']
        f, axarr = plt.subplots(len(aa), sharex=True)
        x = np.linspace(1, nr, nr)
        rr = m.records
        for i, a in enumerate(aa):
            v = []
            for r in rr:
                v.append(r.__getattribute__(a))

            axarr[i].plot(x, v)

            axarr[i].set_ylabel(a)
        axarr[4].set_xlabel("Record #")
        f.canvas.set_window_title("%s -- %s" % (self.title, 'one-value-per-model'))
        plt.tight_layout()
        #################
        # 3D plots
        vars = ['nh', 'teta', 'pe', 'pg', 't5l']
        for var in vars:

            fig = plt.figure()
            ax = fig.gca(projection='3d')
            fig.canvas.set_window_title('%s -- %s' % (self.title, var))

            for i, r in enumerate(rr):
                x = np.linspace(1, r.ntot, r.ntot)
                y = np.ones(len(x)) * (i + 1)
                z = r.__getattribute__(var)

                # print "#%d - %d; (%d, %d, %d)" % (i, r.ntot, len(x), len(y), len(z))

                ax.plot(x, y, z, label='a', color='k')

            # ax.set_xlabel('Wavelength (A)')
            # ax.set_ylabel('Atmospheric layer')
            ax.set_xlabel('Atmospheric layer')
            ax.set_ylabel('Record number')
            ax.set_zlabel(var)

        plt.show()


class VisSpectrum(Vis):
    """Plots single spectrum."""

    input_classes = (FileSpectrum,)

    def _do_use(self, m):
        s = m.spectrum
        T = 0.02  # amount of extra space on both left and right of graphics
        f = plt.figure()
        ax = plt.gca()
        ax.plot(s.x, s.y)
        ymi, yma = ax.get_ylim()
        ax.set_ylim([ymi, ymi + (yma - ymi) * (1 + T)])  # prevents top of line from being hidden by plot box
        ax.set_ylabel(s.filename)
        mi, ma = min(s.x), max(s.x)
        ax.set_xlabel('Wavelength')
        span = ma - mi
        ax.set_xlim([mi - span * T, ma + span * T])
        plt.tight_layout()
        if self.title is not None:
            f.canvas.set_window_title(self.title)
        plt.show()


class VisFileToH(Vis):
    """
    Plots hydrogen lines: each atmospheric layer is plotted as a y-axis-dislocated
    Spectrum in a 3D plot.
    """

    input_classes = (FileToH,)

    def _do_use(self, r):
        assert isinstance(r, FileToH)

        fig = plt.figure()
        mpl.rcParams['legend.fontsize'] = 10
        fig.canvas.set_window_title(self.title)  # requires the Axes3D module
        ax = fig.gca(projection='3d')
        x = np.concatenate((2 * r.lambdh[0] - r.lambdh[-2::-1], r.lambdh))
        _y = np.ones(len(x))
        for i in range(r.th.shape[1]):
            z = np.concatenate((r.th[-2::-1, i], r.th[:, i]))
            ax.plot(x, _y * (i + 1), z, label='a', color='k')
        ax.set_xlabel('Wavelength (A)')
        ax.set_ylabel('Atmospheric layer')
        ax.set_zlabel('Intensity (?)')
        plt.tight_layout()
        plt.show()

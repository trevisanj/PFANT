
from pyfant import *
from mpl_toolkits.mplot3d import Axes3D  # yes, required
from pyfant import *
import matplotlib.pyplot as plt
__all__ = ["plot_mod_grid"]


def plot_mod_grid(ff, title=None):
    """Plots Teff x glog x metallicity in 3D"""

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    teff, glog, asalog = [], [], []
    for f in ff:
        assert isinstance(f, FileMod)
        for r in f.records:
            teff.append(r.teff)
            glog.append(r.glog)
            asalog.append(r.asalog)
    ax.scatter(teff, glog, asalog, c='r', s=60, marker='o')
    ax.set_xlabel('teff')
    ax.set_ylabel('glog')
    ax.set_zlabel('asalog')
    fig.canvas.set_window_title('teff-glog-asalog scatterplot')
    plt.show()

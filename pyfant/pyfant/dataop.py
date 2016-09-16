"""
Functional operations involving DataFile objects
"""

from pyfant.data import *


#########################################################################################################################################
# # PLOTTERS

def plot_colors(ax, wcube):
    """
    """
    assert isinstance(wcube, WebsimCube)
    data = wcube.hdu.data
    nlambda, nY, nX = data.shape


    im = np.zeros((nY, nX, 3))
    for x in range(nX):
        for y in range(nY):
            sp = wcube.get_spectrum(x, y)
            im[y, x, :] = sp.get_rgb()

    ax.imshow(im, interpolation="nearest")



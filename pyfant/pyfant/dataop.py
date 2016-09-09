"""
Functional operations involving DataFile objects
"""

from pyfant.data import *


#########################################################################################################################################
# # PLOTTERS

def plot_colors(ax, ccube):
    """
    """
    assert isinstance(ccube, CompassCube)
    data = ccube.hdu.data
    nlambda, nY, nX = data.shape


    im = np.zeros((nY, nX, 3))
    for x in range(nX):
        for y in range(nY):
            sp = ccube.get_spectrum(x, y)
            im[y, x, :] = sp.get_rgb()

    ax.imshow(im, interpolation="nearest")



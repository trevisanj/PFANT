__all__ = ["draw_cube_3d", "draw_cube_colors"]

from pyfant.data import *

import matplotlib.pyplot as plt
from pylab import MaxNLocator
import copy
import numpy as np
from itertools import product, combinations, cycle

_ZERO_OFFSET = 0.
def draw_cube_3d(ax, dcube, height_threshold=15):
    """
    Plots front and back grid, scaled fluxes, into existing axis

    Arguments:
        dcube -- DataCube instance
        height_threshold -- maximum cube height to plot actual spectra.
         If the cube height is greated than this, line segments will be drawn
         instead of plotting the spectra (for speed)

    data cube            mapped to 3D axis
    ------------------   -----------------
    X pixel coordinate   x
    Y pixel coordinate   z
    Z wavelength         y
    """
    assert isinstance(dcube, DataCube)

    flag_segments = dcube.height > height_threshold
    flag_empty = len(dcube.spectra) == 0
    r0 = [_ZERO_OFFSET, dcube.width +_ZERO_OFFSET]
    r2 = [_ZERO_OFFSET, dcube.height +_ZERO_OFFSET]
    if flag_empty:
        r1 = [_ZERO_OFFSET, 1+_ZERO_OFFSET]
    else:
        max_flux = max([max(sp.flux) for sp in dcube.spectra])
        _y = dcube.wavelength
        dlambda = _y[1] - _y[0]
        r1 = [_y[0] - dlambda / 2, _y[-1] + dlambda / 2]
        scale = 1. / max_flux

    PAR = {"color": "y", "alpha": 0.3}

    def draw_line(*args, **kwargs):
        tempdict = copy.copy(PAR)
        tempdict.update(kwargs)
        ax.plot3D(*args, **tempdict)

    # draws cube edges using a thicker line
    if not flag_empty:
        for s, e in combinations(np.array(list(product(r0, r1, r2))), 2):
            if np.sum(s == e) == 2:
                # if np.sum(np.abs(s - e)) == r[1] - r[0]:
                draw_line(*zip(s, e), lw=2)

    # draws grids
    for i in range(dcube.width):
        draw_line([i + _ZERO_OFFSET] * 2, [r1[0]] * 2, r2)
        draw_line([i + _ZERO_OFFSET] * 2, [r1[1]] * 2, r2)
    for i in range(dcube.height):
        draw_line(r0, [r1[0]] * 2, [i + _ZERO_OFFSET] * 2)
        draw_line(r0, [r1[1]] * 2, [i + _ZERO_OFFSET] * 2)

    for sp in dcube.spectra:
        if flag_segments:
            flux1 = sp.flux[(0, -1),] * scale + sp.pixel_y + _ZERO_OFFSET
            ax.plot(np.array([1, 1]) * sp.pixel_x + _ZERO_OFFSET + .5,
                    sp.wavelength[(0, -1),],
                    flux1, color='k')
        else:
            n = len(sp)
            flux1 = sp.flux * scale + sp.pixel_y +_ZERO_OFFSET
            ax.plot(np.ones(n) * sp.pixel_x+_ZERO_OFFSET+.5,
                    sp.wavelength,
                    flux1, color='k')

    # ax.set_aspect("equal")
    ax.set_xlabel("x (pixel)")
    ax.set_ylabel('wavelength ($\AA$)')  # ax.set_ylabel('wavelength ($\AA$)')
    ax.set_zlabel('y (pixel)')

    ax.set_ylim([dcube.wavelength[0], dcube.wavelength[-1]])
    ax.set_zlim([_ZERO_OFFSET, _ZERO_OFFSET+dcube.height])
    ax.set_xlim([_ZERO_OFFSET, _ZERO_OFFSET+dcube.width])
    ax.zaxis.set_major_locator(MaxNLocator(integer=True))


def draw_cube_colors(ax, datacube, vrange, sqx=None, sqy=None, flag_scale=False, method=0):
    """
    Plots image on existing axis

    Arguments
      ax -- matplotlib axis
      sky -- DataCube instance
      vrange -- visible range
      sqx -- "place spectrum" x
      sqy -- "place spectrum" y

    Returns: matplotlib plot object representing square, or None
    """
    assert isinstance(datacube, DataCube)
    im = datacube.to_colors(vrange, flag_scale, method)
    ax.imshow(im, interpolation="nearest")
    ax.invert_yaxis()
    obj_square = None
    K = .5
    if sqx is not None:
        x0, x1, y0, y1 = sqx - K, sqx + K, sqy - K, sqy + K
        obj_square = ax.plot([x0, x1, x1, x0, x0], [y0, y0, y1, y1, y0],
                             c='w', ls='solid', lw=3, alpha=0.5, zorder=99999)
    ax.set_xlim([-K, datacube.width - .5])
    ax.set_ylim([-K, datacube.height - .5])
    return obj_square

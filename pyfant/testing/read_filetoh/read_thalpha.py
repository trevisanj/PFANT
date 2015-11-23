

"""
Loads thalpha file and plots each atmospheric layer as a different Spectrum in a 3D plot

References:
http://matplotlib.org/mpl_toolkits/mplot3d/tutorial.html#line-plots
"""

from pyfant import *

r = FileToH()
r.load('thalpha')

print r


import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.pyplot as plt

mpl.rcParams['legend.fontsize'] = 10

fig = plt.figure()
ax = fig.gca(projection='3d')


x = np.concatenate((2*r.lambdh[0]-r.lambdh[-2::-1], r.lambdh))
_y = np.ones(len(x))
for i in range(r.th.shape[1]):
  z = np.concatenate((r.th[-2::-1, i], r.th[:, i]))

  ax.plot(x, _y*(i+1), z, label='a', color='k')

ax.set_xlabel('Wavelength (A)')
ax.set_ylabel('Atmospheric layer')
ax.set_zlabel('th (?)')



plt.show()



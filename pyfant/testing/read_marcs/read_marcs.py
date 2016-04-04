"""
Reads models from MARCS table and plots vectors (nh, teta, pe, pg, log_tau_ross)

This is something I discovered about these files: for different 1200-byte
records,
- teff goes in steps
- glog is saw-line
- asalog, asalalf and nhe are constant

So, each record has (nh, teta, pe, pg, log_tau_ross) vectors for a given (teff, glog)

"""

from pyfant import *
import numpy as np
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt


filename = 'newnewm150.mod'
m = FileMod()
m.load(filename)

nr = len(m)
print "Read %d records" % nr


# 5 subplots sharing same x-axis
# Plotting teff, glog, ...

f, axarr = plt.subplots(5, sharex=True)
x = np.linspace(1, nr, nr)
rr = m.records
aa = ['teff', 'glog', 'asalog', 'asalalf', 'nhe']
n = len(aa)
for i, a in enumerate(aa):
  v = []
  for r in rr:
    v.append(r.__getattribute__(a))

  axarr[i].plot(x, v)

  axarr[i].set_ylabel(a)

axarr[4].set_xlabel("Record #")
axarr[0].set_title(filename)
plt.tight_layout()


vars = ['nh', 'teta', 'pe', 'pg', 'log_tau_ross']


for var in vars:

  fig = plt.figure()
  ax = fig.gca(projection='3d')
  ax.set_title(filename)

  for i, r in enumerate(rr):
    x = np.linspace(1, r.ntot, r.ntot)
    y = np.ones(len(x))*(i+1)
    z = r.__getattribute__(var)

    # print "#%d - %d; (%d, %d, %d)" % (i, r.ntot, len(x), len(y), len(z))

    ax.plot(x, y, z, label='a', color='k')

  # ax.set_xlabel('Wavelength (A)')
  # ax.set_ylabel('Atmospheric layer')
  ax.set_xlabel('Atmospheric layer')
  ax.set_ylabel('Record number')
  ax.set_zlabel(var)

plt.show()



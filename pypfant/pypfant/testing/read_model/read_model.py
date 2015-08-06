"""
Loads modeles.mod file and plots vectors nh, teta, pe, pg, t5l
"""

from pypfant import *
import numpy as np
import matplotlib.pyplot as plt

r = FileMod()
r.load('modeles.mod')

print r


# 5 subplots sharing same x-axis
f, axarr = plt.subplots(5, sharex=True)
x = np.linspace(1,r.ntot)
axarr[0].plot(x, r.nh)
axarr[0].set_title('nh')
axarr[1].plot(x, r.teta)
axarr[1].set_title('teta')
axarr[2].plot(x, r.pe)
axarr[2].set_title('pe')
axarr[3].plot(x, r.pg)
axarr[3].set_title('pg')
axarr[4].plot(x, r.t5l)
axarr[4].set_title('t5l')

axarr[4].set_xlabel("Atmospheric layer #")
plt.tight_layout()
plt.show()



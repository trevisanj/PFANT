#!/usr/bin/python

"""
Looks for file "flux.norm" inside directories session_*
and saves one figure per page in pdf file output.pdf

Reference:
http://stackoverflow.com/questions/17788685
"""


from matplotlib import pyplot as plt
from pyfant import *
import glob
import os
import matplotlib.backends.backend_pdf

dd = glob.glob("session_*")
dd.sort()

v = VisSpectrum()
pdf = matplotlib.backends.backend_pdf.PdfPages("output.pdf")

format_BLB()

for d in dd:
    name = d[8:]

    print "Saving ", name, "..."

    f = FileSpectrumPfant()
    f.load(os.path.join(d, "flux.norm"))

    v.title = "%s" % name
    v.use(f)

    fig = plt.gcf()
    plt.ylim([0, 1.02])
    ax = plt.gca()
    p = ax.get_position()
    p.x0 = 0.11
    ax.set_position(p)  # Try to apply same position for all figures to improve flicking experience


    pdf.savefig(fig)

# for fig in xrange(1, figure().number): ## will open an empty extra figure :(
#     pdf.savefig( fig )
pdf.close()


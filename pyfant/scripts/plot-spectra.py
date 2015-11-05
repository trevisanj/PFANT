#!/usr/bin/python
"""
Plots one or more spectra, either stacked or overlapped.
"""

import argparse
from pyfant import *
import matplotlib.pyplot as plt

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    parser.add_argument('fn', type=str, nargs='+',
                        help='name of spertrum file (either generated by pfant or nulbad)')
    parser.add_argument('--ovl', help='Overlapped graphics', action="store_true")

    args = parser.parse_args()

    ss = []
    for x in args.fn:

        print "Trying to read file '%s'..." % x
        # tries to load as pfant ouput; if fails, tries as nulbad output
        try:
            f = FileSpectrumPfant()
            f.load(x)
            print "... file is pfant output"
        except:
            try:
                f = FileSpectrumNulbad()
                f.load(x)
                print "... file is nulbad output"
            except:
                f = FileSpectrumXY()
                f.load(x)
                print "... read file as generic X-Y file"


        ss.append(f.spectrum)
    if args.ovl:
        f = plot_spectra_overlapped
    else:
        f = plot_spectra
    f(ss, "")
    plt.show()
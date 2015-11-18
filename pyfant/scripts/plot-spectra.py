#!/usr/bin/python
"""
Plots one or more spectra, either stacked or overlapped, or cut to pieces into PDF.
"""
import argparse
from pyfant import *
import matplotlib.pyplot as plt
import traceback
import logging

logging.basicConfig(level=logging.INFO)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    parser.add_argument('fn', type=str, nargs='+',
     help='name of spertrum file (either generated by pfant or nulbad)')
    parser.add_argument('--ovl', help='Overlapped graphics', action="store_true")
    parser.add_argument('--pieces', help='If set, will generate a PDF file instead,'
     'and each page of the PDF will contain one "piece" of the spectra of length'
     'given by the --aint option. --ovl is ignored', action="store_true")
    parser.add_argument('--aint', type=int, nargs='?', default=50,
     help='length of each piece-plot in wavelength units (used only if --pieces)')
    parser.add_argument('--fn_output', nargs='?', default='pieces.pdf', type=str,
     help='PDF output file name (used only if --pieces)')

    args = parser.parse_args()


    classes = [FileSpectrumPfant, FileSpectrumNulbad, FileSpectrumXY, FileSpectrumFits]

    ss = []
    flag_ok = False
    for x in args.fn:

        print "Trying to read file '%s'..." % x
        # tries to load as pfant ouput; if fails, tries as nulbad output

        for class_ in classes:
            try:
                f = class_()
                f.load(x)
                print "... successfully read using reader %s." % class_.__name__
                flag_ok = True
                break
            except OSError:
                raise
            except IOError:
                raise
            except:
                # Note: this is not good if the code has bugs, gotta make sure that the
                # FileSpectrum* classes are are working properly.

                # traceback.print_exc()
                pass
        if not flag_ok:
            print_error("... not recognized, sorry!")
        else:
            ss.append(f.spectrum)
    if len(ss) == 0:
        print_error("Nothing to plot!")
    else:
        if args.pieces:
            plot_spectra_pieces_pdf(ss, aint=args.aint, pdf_filename=args.fn_output)
        else:
            if args.ovl:
                f = plot_spectra_overlapped
            else:
                f = plot_spectra
            f(ss, "")
            plt.show()

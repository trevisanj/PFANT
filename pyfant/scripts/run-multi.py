#!/usr/bin/python

"""
Runs pfant for different abundances for each element, then run nulbad for each pfant result for different FWHMs.

The configuration is read from a .py file.

The user must specify a list of FWHM values for nulbad convolutions, and
a dictionary containing element symbols and respective list of differential
abundances (n_abdif) to be used for each element. See sample config file below
for details (should be clear from the example)

pfant will be run n_abdif times, each time adding to each element in ab the i-th
value in the vector for the corresponding element.

nulbad will run n_abdif*n_fwhms times, where n_fwhms is the number of different
FWHMs specified.

The result will be
- several spectra saved as  "<star name><pfant run counter>.sp"
- several "spectra list" files saved as "cv_<FWHM>.spl". As the file indicates,
  each ".spl" file will have the names of the spectrum files for a specific FWHM.
  .spl files are subject to input for lineplot.py by E.Cantelli

Note: pfant and nulbad are run in parallel. Currently the number of simultaneous
pfants and nulbads to is set to 6

Note2: before running, it will remove all "session_*" directories

===Begin sample config file=====================================================

# specification of differential abundances for each chemical
# - differential abundance: number to add to original value in abonds.dat
# - all list of abundances for each element must have the same length
ab = {"Ca": [-.3, 0, .3, .5],
      "Si": [-.3, 0, .3, .5]}

# Convolutions specification for fwhm parameter
conv = [0.08, # first vaule
        0.6, # last value
        0.04  # step
       ]
===End sample config file=======================================================
.
.
.
"""
from pyfant import *
import argparse
import os.path
import numpy as np
import imp
import logging

misc.logging_level = logging.INFO
make_filename = lambda pref, ab_diff, conv_: "%s_%s_%s.dat" % (pref, ab_diff, conv_)
MAX_FORTRANS = 6  # number of pfants/nulbads to run simultaneously


if __name__ == "__main__":
    # Parses command-line options
    # ***************************

    parser = argparse.ArgumentParser(
    description="Nulbadboy powered by pyfant.\n"+__doc__,
    formatter_class=SmartFormatter
    )
    parser.add_argument('--fn_cfg', default="config.py", help='Config filename')
    args = parser.parse_args()
    print "Using config file '%s'" % args.fn_cfg

    module_name = os.path.splitext(args.fn_cfg)[0]
    # exec("import %s as cfg" % module_name)
    print "module_NAME", module_name
    cfg = imp.load_source("cfg", args.fn_cfg)  # exec("import %s as cfg" % module_name)


    os.system('rm -rf session*')

    # # Treats config file

    symbols = [adjust_atomic_symbol(symbol) for symbol in cfg.ab.keys()]
    abmargss = cfg.ab.values()

    n_abdif = len(abmargss[0])
    for abdifs in abmargss:
        if len(abdifs) != n_abdif:
            raise RuntimeError('All abundances vectors must have length of %d' % n_abdif)

    file_main = FileMain()
    file_main.load("main.dat")

    fwhms = np.arange(cfg.conv[0], cfg.conv[1]+.00000001, cfg.conv[2])
    n_fwhms = len(fwhms)

    for fwhm in fwhms:
        if fwhm > 9.99:
            raise RuntimeError("fhwm maximum is 9.99")

    # # Runs pfant, pfant creates several .norm files

    print "+++ pfant phase..."

    pfant_list = []
    for j in range(n_abdif):
        file_abonds = FileAbonds()
        file_abonds.load("abonds.dat")

        found = False
        for i, symbol in enumerate(symbols):

            found = False
            for k in range(len(file_abonds.ele)):
                if file_abonds.ele[k] == symbol:
                    abdif = abmargss[i][j]
                    file_abonds.abol[k] += abdif
                    print j, " - ", symbol, "with abundance", file_abonds.abol[k]
                    found = True

            if not found:
                raise RuntimeError("Atom '%s' not found" % symbol)

        pfant = Pfant()
        pfant.conf.file_abonds = file_abonds
        # todo replace j with a descriptive name, configurable in FileAbXFwhm
        pfant.conf.opt.flprefix = "%s_%02d" % (file_main.titrav, j)
        pfant.conf.sid.id = pfant.conf.opt.flprefix

        print pfant.conf.opt.flprefix

        f = pfant.conf.file_abonds
        pfant_list.append(pfant)

    run_parallel(pfant_list, MAX_FORTRANS, flag_console=True)

    # # Runs nulbad, saves .sp and .spl files

    print "+++ nulbad phase..."

    # function to convert given FWHM to string to use as part of a file name
    fmt_fwhm = lambda x: "%03d" % round(x*100)

    nulbad_list = []
    sp_filenames_by_fwhm = {}  # dictionary containing a list of .sp filenames for each FWHM
    for pfant in pfant_list:
        prefix = pfant.conf.opt.flprefix

        for fwhm in fwhms:
            nulbad = Nulbad()
            nulbad.conf.opt.fn_flux = prefix+".norm"
            nulbad.conf.opt.fwhm = fwhm
            nulbad.conf.opt.fn_cv = "%s_%s.sp" % (prefix, fmt_fwhm(fwhm))
            nulbad_list.append(nulbad)

            if not fwhm in sp_filenames_by_fwhm:
                sp_filenames_by_fwhm[fwhm] = []
            sp_filenames_by_fwhm[fwhm].append(nulbad.conf.opt.fn_cv)

    # # Saves files for lineplot.py
    for fwhm, sp_filenames in sp_filenames_by_fwhm.iteritems():
        with open("cv_%s.spl" % fmt_fwhm(fwhm), "w") as h:
            for sp_filename in sp_filenames:
                h.write(sp_filename+"\n")

    run_parallel(nulbad_list, MAX_FORTRANS, flag_console=False)

    # # Deletes session-* directories

    print "+++ cleaning up..."

    # for pfant in pfant_list:
    #   pfant.conf.clean()
    # for nulbad in nulbad_list:
    #   nulbad.conf.clean()




#!/usr/bin/python

"""
Sample config file:

ab = {"Ca": [-.3, 0, .3, .5],
      "Si": [-.3, 0, .3, .5]}


# Convolutions specification for fwhm parameter
conv = [0.08, # first vaule
        0.6, # last value
        0.04  # step
       ]

"""
from pyfant import *
import argparse
import os.path
import numpy as np

make_filename = lambda pref, ab_diff, conv_: "%s_%s_%s.dat" % (pref, ab_diff, conv_)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
    description="Descricao do programa",
    formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    parser.add_argument('--cfg', default="config.py", help='Config filename')


    args = parser.parse_args()

    print args.cfg

    module_name = os.path.splitext(args.cfg)[0]
    exec("import %s as cfg" % module_name)

    print cfg.ab

    os.system('rm -rf session_*')


    # Treats config file

    symbols = [adjust_atomic_symbol(symbol) for symbol in cfg.ab.keys()]
    abdifss = cfg.ab.values()

    n = len(abdifss[0])
    for abdifs in abdifss:
        if len(abdifs) != n:
            raise RuntimeError('All abondances vectors must have %d elements' % n)

    file_main = FileMain()
    file_main.load("main.dat")

    fwhms = np.arange(cfg.conv[0], cfg.conv[1]+.00000001, cfg.conv[2])

    for fwhm in fwhms:
        if fwhm > 9.99:
            raise RuntimeError("fhwm maximum is 9.99")

    # Runs pfant

    rr = []
    for j in range(n):
        file_abonds = FileAbonds()
        file_abonds.load("abonds.dat")

        found = False
        for i, symbol in enumerate(symbols):

            found = False
            for k in range(len(file_abonds.ele)):
                if file_abonds.ele[k] == symbol:
                    abdif = abdifss[i][j]
                    file_abonds.abol[k] += abdif
                    print j, " - ", symbol, "with abondance", file_abonds.abol[k]
                    found = True

            if not found:
                raise RuntimeError("Element '%s' not found" % symbol)

        pfant = Pfant()
        pfant.execonf.fo_abonds = file_abonds
        pfant.execonf.opt.flprefix = "%s%02d" % (file_main.titrav, j)
        pfant.execonf.session_id = pfant.execonf.opt.flprefix

        print pfant.execonf.opt.flprefix

        f = pfant.execonf.fo_abonds
        rr.append(pfant)

    run_parallel(rr, 6, flag_console=False)

    fmt_fwhm = lambda x: "%03d" % round(x*100)

    ss = []
    filenames = {}
    for pfant in rr:
        prefix = pfant.execonf.opt.flprefix

        for fwhm in fwhms:
            nulbad = Nulbad()
            nulbad.execonf.opt.fn_flux = prefix+".norm"
            nulbad.execonf.opt.fwhm = fwhm
            s = str(fwhm)
            nulbad.execonf.opt.fn_cv = "%s_%s.sp" % (prefix, fmt_fwhm(fwhm))

            ss.append(nulbad)

            # print nulbad.execonf.opt.fn_cv

            if not fwhm in filenames:
                filenames[fwhm] = []
            filenames[fwhm].append(nulbad.execonf.opt.fn_cv)


    for fwhm, spectra in filenames.iteritems():
        with open("cv_%s.spl" % fmt_fwhm(fwhm), "w") as h:
            for s in spectra:
                h.write(s+"\n")



    run_parallel(ss, 12, flag_console=False)




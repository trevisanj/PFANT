#!/usr/bin/python
"""
Tunes the zinf parameter for each atomic line in atomic lines file.

The "zinf" parameter is a distance in angstrom from the centre of an atomic
line. It specifies the calculation range for the line:
[centre-zinf, centre+zinf].

This script runs pfant for each atomic line to determine the width of each
atomic line and thus zinf.

Note: pfant is run using most of its default settings and will require the
following files to exist in the current directory:
  - main.dat
  - dissoc.dat
  - abonds.dat
  - modeles.mod
  - partit.dat
  - absoru2.dat

Note: the precision in the zinf found depends on the calculation step ("pas")
specified in main.dat. A higher "pas" means lower precision and a tendency to
get higher zinf's. This is really not critical. pas=0.02 or pas=0.04 should do.

"""

import argparse
from pyfant import *
import logging
import copy
import numpy as np
from pyfant.gui import XRunnableManager
from PyQt4.QtGui import *
import time
import os.path
import glob

misc.logging_level = logging.INFO

# Fraction of peak to be considered its "settlement"
EPSILON = 1e-4

DEFOUT = "<made-up filename>"


def _get_zinf(lambda_centre, norm):
    """Returns zinf given *normalized* spectrum, or 0 if line if flat.

    Arguments:
      lambda_centre -- centre of line
      norm -- Spectrum instance

    Spectrum is expected to contain one line only.
    """

    y = norm.y  # normalized spectrum: values between 0 and 1
    if np.all(np.diff(y) == 0):
        # flat line
        return 0
    ymax = 1
    ymin = min(y)
    i_zinf = np.argmax(norm.y < ymax-(ymax-ymin)*EPSILON)
    zinf = lambda_centre-norm.x[i_zinf]
    return zinf


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    parser.add_argument('--min', type=float, nargs='?', default=.1,
     help='minimum zinf. If zinf found for a particular line is smaller than '
          'this value, this value will be used instead')
    parser.add_argument('--max', type=float, nargs='?', default=50.,
     help='maximum zinf. If zinf found for a particular line is greater than '
          'this value, this value will be used instead')
    parser.add_argument('--inflate', type=float, nargs='?', default=1.1,
     help='Multiplicative constant to apply a "safety margin". '
          'Each zinf found will be multiplied by this value. For example '
          'a value of INFLATE=1.1 means that all the zinf\'s saved will be 10 '
          'percent larger than those calculated')
    parser.add_argument('--ge_current', action="store_true",
     help='"Greater or Equal to current": If this option is set, the current '
          'zinf in the atomic lines file is used as a lower boundary.')
    parser.add_argument('fn_input', type=str, help='input file name', nargs=1)
    parser.add_argument('fn_output', type=str, help='output file name',
     nargs="?", default=DEFOUT)
    parser.add_argument('--no_clean', action="store_true",
    help='If set, will not remove the session directories.')

    args = parser.parse_args()
    logger = get_python_logger()

    fn_out = args.fn_output
    if fn_out == DEFOUT:
        temp = args.fn_input[0].replace("atoms-untuned-", "")
        fn_out = "atoms-"+temp
        logger.info("Made up output filename: '%s'" % fn_out)

    logger.info("Using minimum zinf = %g" % args.min)
    logger.info("Using MAXIMUM zinf = %g" % args.max)
    logger.info("Using inflate = %g" % args.inflate)
    logger.info("Using ge_current = %s" % args.ge_current)

    file_atoms = FileAtoms()
    file_atoms.load(args.fn_input[0])

    misc.flag_log_console = True
    misc.flag_log_file = True
    logger.info("Number of lines in file '%s': %d" % \
     (args.fn_input[0], file_atoms.num_lines))

    # # Tries to get input files together
    setup_inputs(h=False, atoms=False, molecules=False, opa=False)


    # ## Runs innewmarcs if modeles.mod not present
    
    if not os.path.isfile('modeles.mod'):
        logger.info("'modeles.mod' does not exist, running innewmarcs...")
        inn = Innewmarcs()
        inn.conf.opt.opa = False
        inn.run()



    # ## As suggested by M. Trevisan, adds 0.5 to all abundances, except ...
    K_ADD = 0.5
    EXCEPTIONS = ["FE", "HE"]
    logger.info("Preparing abundances (Adding %g to all abundances, " % K_ADD)
    logger.info("except those of %s)..." % str(EXCEPTIONS))
    fa = FileAbonds()
    fa.load("abonds.dat")
    cnt = 0
    for i in range(len(fa)):
       if fa.ele[i] not in EXCEPTIONS:
           fa.abol[i] += K_ADD
           cnt += 1
    logger.info("%d abundance(s) changed" % cnt)


    # This is old code, with a minimum value for the abundances but I don't think it is very reallistic
    # MIN_ABOND = 3
    # logger.info("Preparing abundances file with all abundances >= %g ..." % MIN_ABOND)
    # fa = FileAbonds()
    # fa.load("abonds.dat")
    # cnt = 0
    # for i in range(len(fa)):
    #     if fa.abol[i] < MIN_ABOND:
    #         fa.abol[i] = MIN_ABOND
    #         cnt += 1
    # logger.info("%d abundance(s) changed" % cnt)


    # ## Creates the Runnable instances
    PAS = 0.04
    logger.info("Calculation step will be set to %g" % PAS)
    logger.info("Preparing pfant's...")
    pp, aa, i = [], [], 0
    ii = 0
    n = file_atoms.num_lines
    for atom in file_atoms.atoms:
        for line in atom.lines:
            a = copy.copy(atom)  # shallow copy
            a.lines = [line]
            f = FileAtoms()
            f.atoms = [a]
            combo = Combo([FOR_PFANT])
            # Fortran messages will not be displayed in terminal
            combo.conf.flag_log_console = False
            combo.conf.sid.flag_split_dirs = True
            # Forces pfant to run one single iteration (ikeytot will be =1)
            combo.conf.opt.aint = args.max+10
            combo.conf.file_atoms = f
            combo.conf.file_abonds = fa
            combo.conf.opt.logging_level = "warning"
            combo.conf.opt.pas = PAS
            combo.conf.flag_output_to_dir = True
            combo.conf.opt.zinf = args.max
            combo.conf.opt.no_molecules = True
            combo.conf.opt.opa = False
            combo.conf.opt.no_h = True
            # Note that half of the line (needs to)/(will be) calculated
            combo.conf.opt.llzero = line.lambda_-args.max
            combo.conf.opt.llfin = line.lambda_
            pp.append(combo)
            aa.append(a)

            i += 1
            ii += 1
            if ii == 1000:
                print format_progress(i, n)
                ii = 0

    # # Runs pfant
    logger.info("Running pfant's...")






    # FLAG_CLEAN = True
    # rm = RunnableManager(flag_auto_clean=FLAG_CLEAN)






    rm = RunnableManager()
    rm.add_runnables(pp)
    app = get_QApplication([])
    form = XRunnableManager(None, rm)
    form.show()
    # it is good to start the manager as late as possible, otherwise
    # the program will hang if, for example, the form fails to be created.
    rm.start()
    app.exec_()
    rm.exit()

    # # Saves log
    LOG_FILENAME = "tune-zinf-status.log"
    with open(LOG_FILENAME, "w") as h:
      h.write(str(rm))
    logger.info("Final status saved to file '%s'" % LOG_FILENAME)

    # # Finds zinf's and creates output file
    flag_success, flag_clean = False, False
    if not rm.flag_finished:
        logger.info("Not finished")
    elif rm.num_failed > 0:
        logger.info("Number of failed tasks: %d" % rm.num_failed)
        logger.info("Please check log files inside directories of sessions that failed")
        logger.info("(session directories will not be removed).")
    else:
        logger.info("Finding zinf's, please wait...")
        # # Calculates zinf and save new atomic lines file
        n = len(pp)
        X = np.zeros((n, 3))  # [algf, kiex, zinf], ...]
        ii = 0
        print format_progress(0, n)
        cnt_min = 0
        cnt_max = 0
        for i, (a, combo) in enumerate(zip(aa, pp)):
            combo.pfant.load_result()


            # if not FLAG_CLEAN:
            #     combo.pfant.load_result()
            # else:
            #     pass  # result will be already loaded



            norm = combo.pfant.norm  # normalized spectrum
            line = a.lines[0]  # a is single-line FileAtoms object
            zinf = _get_zinf(line.lambda_, norm)
            zinf *= args.inflate
            if zinf == 0:
                # Note that some lines don't appear at all, so there is no way
                # to determine zinf
                logger.warning("zinf is ZERO: (%s) (%s) (%s)" %
                 (combo.conf.sid.dir, a.one_liner_str(), line.one_liner_str()))
            if zinf < args.min:
                zinf = args.min
                cnt_min += 1
            if zinf > args.max:
                # print " %gOLHOLHO (%s) (%s) (%s)" % \
                #  (zinf, combo.conf.session_dir, a.one_liner_str(), line.one_liner_str())
                zinf = args.max
                cnt_max += 1
            if args.ge_current and line.zinf > zinf:
                zinf = line.zinf
            line.zinf = zinf

            i += 1
            ii += 1
            if ii == 100:
                print format_progress(i, n)
                ii = 0
        print format_progress(n, n)
        logger.info("zinf's clipped to minimum: %d" % cnt_min)
        logger.info("zinf's clipped to maximum: %d" % cnt_max)

        logger.info("Creating output file...")
        file_atoms.save_as(fn_out)
        flag_clean = not args.no_clean
        flag_success = True


    # # Removes session-* directories
    if flag_clean:
        # this part removes sessionxxxx/sessionyyyy
        logger.info("Cleaning...")

        for i, combo in enumerate(pp):
            try:
                combo.sid.clean()
            except Exception as E:
                logger.info("Error cleaning session for %s: %s" % (combo.name, str(E)))
        # this part removes sessionxxxxx
        # but also removes previous sessions
        # not gonna do it dd = glob.glob("session*")

    if flag_success:
        logger.info("Successfully created file '%s' !!" % fn_out)

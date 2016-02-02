#!/usr/bin/python
"""
Tunes the zinf parameter for each atomic line in atomic lines file.

The "zinf" parameter is a distance in angstrom from the centre of an atomic
line. It specifies the calculation range for the line:
[centre-zinf, centre+zinf].

This script runs pfant* for each atomic line to determine the width of each
atomic line and thus zinf.

Note: *pfant is run using most of its default settings and will require the
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


# Fraction of peak to be considered its "settlement"
EPSILON = 1e-4


def _get_zinf(lambda_centre, norm):
    """Finds zinf given *normalized* spectrum

    Arguments:
      lambda_centre -- centre of line
      norm -- Spectrum instance

    Spectrum is expected to contain one line only.
    """

    y = norm.y  # normalized spectrum: values between 0 and 1
    maxy = 1
    miny = min(y)
    i_zinf = np.argmax(norm.y < maxy-miny*EPSILON)
    zinf = lambda_centre-norm.x[i_zinf]

    return zinf


logging.basicConfig(level=logging.DEBUG)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
     description=__doc__,
     formatter_class=SmartFormatter
     )
    parser.add_argument('--min', type=float, nargs='?', default=.01,
     help='minimum zinf. If zinf found for a particular line is smaller than '
          'this value, this value will be used instead')
    parser.add_argument('--max', type=float, nargs='?', default=50.,
     help='maximum zinf. If zinf found for a particular line is greater than '
          'this value, this value will be used instead')
    parser.add_argument('--ge_current', action="store_true",
     help='"Greater or Equal to current": If this option is set, the current '
          'zinf in the atomic lines file is used as a lower boundary.')
    parser.add_argument('fn_input', type=str, help='input file name', nargs=1)
    parser.add_argument('fn_output', type=str, help='output file name', nargs=1)
    parser.add_argument('--no_clean', action="store_true",
     help='If set, will not remove the session directories.')

    args = parser.parse_args()

    file_atoms = FileAtoms()
    file_atoms.load(args.fn_input[0])

    print "Number of lines in file '%s': %d" % \
          (args.fn_input[0], file_atoms.num_lines)

    # # Preparation

    print "Preparing pfant's..."

    pp, ll, i = [], [], 0
    for atom in file_atoms.atoms:
        for line in atom.lines:
            a = copy.copy(atom)  # shallow copy
            a.lines = [line]
            f = FileAtoms()
            f.atoms = [a]
            combo = Combo([e_pfant])  # using Combo because it saves results in session dir. For parallel run, has to be combo, really
            combo.flag_log_console = False  # Fortran messages will not be displayed in terminal
            combo.conf.file_atoms = f
            combo.conf.opt.logging_level = "warning"
            combo.conf.opt.zinf = args.max
            combo.conf.opt.no_molecules = True
            combo.conf.opt.no_h = True
            # Note that half of the line (needs to)/(will be) calculated
            combo.conf.opt.llzero = line.lambda_-args.max
            combo.conf.opt.llfin = line.lambda_
            pp.append(combo)
            ll.append(line)

            i += 1
            #     if i == 5:
            #         break
            # if i == 5:
            #     break

    # # Runs pfant
    print "Running pfant's..."
    rm = RunnableManager()
    for p in pp:
        rm.add_runnable(p)
    app = QApplication([])
    form = XRunnableManager(rm)
    form.show()
    # it is good to start the manager as late as possible, otherwise
    # the program will hang if, for example, the form fails to be created.
    rm.start()
    app.exec_()
    rm.exit()
    # print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
    # print "[SUPPOSED TO HAVE] EXITED"
    # print rm
    # # Saves log
    LOG_FILENAME = "tune-zinf-status.log"
    with open(LOG_FILENAME, "w") as h:
      h.write(str(rm))
    print "Final status saved to file '%s'" % LOG_FILENAME

    # todo not closing form automatically. This is OK, but perhaps it would
    # be good to show a message to continue


    # # Finds zinf's and creates output file
    flag_clean, flag_success = False, False
    if not rm.flag_finished:
        print "Not finished"
    elif rm.num_failed > 0:
        print "Number of failed tasks: " % rm.num_failed
        print "Please check log files inside directories of sessions that failed"
        print "(session directories will not be removed)."
    else:
        print "Calculating zinf's, please wait..."
        # # Calculates zinf and save new atomic lines file
        n = len(pp)
        X = np.zeros((n, 3))  # [algf, kiex, zinf], ...]
        for i, (line, combo) in enumerate(zip(ll, pp)):
            combo.pfant.load_result()
            norm = combo.pfant.norm  # normalized spectrum
            zinf = _get_zinf(line.lambda_, norm)
            if zinf < args.min:
                zinf = args.min
            if zinf > args.max:
                zinf = args.max
            if args.ge_current and line.zinf > zinf:
                zinf = line.zinf
            line.zinf = zinf
        print "Creating output file..."
        file_atoms.save_as(args.fn_output[0])
        flag_clean = not args.no_clean
        flag_success = True


    # # Removes session-* directories
    if flag_clean:
        print "Cleaning..."
        for i, combo in enumerate(pp):
            try:
                combo.conf.clean()
            except Exception as E:
                print "Error cleaning session for %s: %s" % (combo.name, str(E))

    if flag_success:
        print "Successfully created file '%s' !!" % args.fn_output[0]



# Why not closing ...
#!/usr/bin/python
"""
Tunes the zinf parameter for each atomic line in atomic lines file.

The "zinf" parameter is a distance in angstrom from the centre of an atomic
line. It specifies the calculation range for the line:
[centre-zinf, centre+zinf].

This script runs pfant* for each atomic line to determine the width of each
atomic line and thus zinf.

*pfant is run using most of its default settings and will require the following
files to exist in the current directory:
  - main.dat
  - dissoc.dat
  - abonds.dat
  - modeles.mod
  - partit.dat
  - absoru2.dat
"""

import argparse
from pyfant import *
import logging
import copy
import numpy as np

# Fraction of peak to be considered its "settlement"
EPSILON = 1e-4


def get_zinf(lambda_centre, norm):
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
    parser.add_argument('--min', type=float, nargs='?', default=.3,
     help='minimum zinf. If zinf found for a particular line is smaller than '
          'this value, this value will be used instead')
    parser.add_argument('--max', type=float, nargs='?', default=30.,
     help='maximum zinf. If zinf found for a particular line is greater than '
          'this value, this value will be used instead')
    parser.add_argument('--ge_current', action="store_true",
     help='"Greater or Equal to current": If this option is set, the current '
          'zinf in the atomic lines file is used as a lower boundary.')
    parser.add_argument('fn_input', type=str, help='input file name', nargs=1)
    parser.add_argument('fn_output', type=str, help='output file name', nargs=1)

    args = parser.parse_args()

    file_atoms = FileAtoms()
    file_atoms.load(args.fn_input[0])

    print "Number of lines in file '%s': %d" % \
          (args.fn_input[0], file_atoms.num_lines)

    # # Calculates spectra

    pp, ll, i = [], [], 0
    for atom in file_atoms.atoms:
        for line in atom.lines:
            a = copy.copy(atom)  # shallow copy
            a.lines = [line]
            f = FileAtoms()
            f.atoms = [a]
            combo = Combo([e_pfant])  # using Combo because it saves results in session dir. For parallel run, has to be combo, really
            combo.conf.file_atoms = f
            combo.conf.opt.logging_level = "warning"
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

    run_parallel(pp, None, False)

    # # Calculates zinf and saves files

    n = len(pp)
    X = np.zeros((n, 3))  # [algf, kiex, zinf], ...]
    for i, (line, combo) in enumerate(zip(ll, pp)):
        combo.pfant.load_result()
        norm = combo.pfant.norm  # normalized spectrum
        zinf = get_zinf(line.lambda_, norm)


        # todo check min and max

        line.zinf = zinf

    file_atoms.save_as(args.fn_output[0])
    print "Successfully created file '%s'" % args.fn_output[0]

    # # Removes session-* directories

    for i, combo in enumerate(pp):
        pass # combo.conf.clean()

#!/usr/bin/python

"""
Runs pfant for each atom isolated from the others, and no molecular lines.
Then, creates a PDF containing one spectrum per page.
*Careful*: all session-* directories will be removed before starting.
"""

from pyfant import *
import copy
import os

os.system('rm -rf session-*')

# # creates modeles.mod
r = Innewmarcs()
r.run()

# # loads original abundances file
file_abonds = FileAbonds()
file_abonds.load("abonds.dat")

runnables_ = []  # iist of runnables

# # prepares a pfant with all atoms for comparison
_i = 0
r = Pfant()
r.name = "All atoms"
r.conf.sid.id = "%03d-original" % _i  # affects session directory name
r.conf.flag_output_to_dir = True  # synthetic spectra will be saved in session directory
r.conf.opt.no_molecules = True  # Skips calculation of molecular lines
r.conf.opt.no_h = True  # Skips calculation of hydrogen lines
runnables_.append(r)

# # prepares pfant's to run, one element at a time
_i += 1
for ele in file_abonds.ele:
    # creates copy of original abundances list
    file_abonds2 = copy.deepcopy(file_abonds)

    # "zeroes" all abundances, except for one element
    for i, e in enumerate(file_abonds2.ele):
        if e != ele:
            file_abonds2.abol[i] = -99.99

    r = Pfant()
    r.name = "%s only" % ele
    r.conf.sid.id = "%03d-%s" % (_i, ele.strip())
    r.conf.flag_output_to_dir = True
    r.conf.file_abonds = file_abonds2  # will cause custom abonds.dat to be
                                       # created inside session directory
    r.conf.opt.no_molecules = True
    r.conf.opt.no_h = True

    runnables_.append(r)
    _i += 1

# # runs pfant's in parallel
manager = run_parallel(runnables_, flag_console=False)

# # creates pdf
if manager.flag_success:
    # ## loads spectra
    spectra = []
    for r in runnables_:
        r.load_result()  # will populate r.norm, r.spec, r.cont
                         # (normalized, spectrum, continuum respectively)
        spectra.append(r.norm)  # keeps the normalized spectrum

    # ## creates PDF
    plot_spectra_pages_pdf(spectra, "separate-atoms.pdf")
else:
    print "Not all pfant's succeeded"
print "FINISHED"
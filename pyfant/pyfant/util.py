"""
Miscellanea routines that depend on other pyfant modules.

Rule: no pyfant module can import util!!!

"""
__all__ = ["run_parallel", "run_multi"]
# from pyfant.misc import *
from pyfant import *
import time
import traceback
import numpy as np
import copy
import os

# ##################################################################################################
# Terminal-based interface

def run_parallel(rr, max_simultaneous=None, flag_console=True):
    """
    Arguments:
      rr -- list of Runnable instances
      max_simultaneous -- maximum number of simultaneous processes.

    Returns: the RunnableManager object
    """
    # Adds to pool
    rm = RunnableManager(max_simultaneous=max_simultaneous)
    rm.start()

    rm.add_runnables(rr)

    # Primitive thread monitor
    if flag_console:
        while True:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
            print rm
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
            s = raw_input("[Enter] -- [e]xit & keep in loop -- [q]uit -- [k]ill running >>> ")
            if s.lower() == "q":
                if rm.is_alive():
                    try:
                        rm.exit()
                    except:
                        traceback.print_exc()
                break
            if s.lower() == "e":
                try:
                    rm.exit()
                except:
                    traceback.print_exc()
            if s.lower() == "k":
                rm.kill_runnables()
    else:
        rm.wait_until_finished()


    print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
    print "test-tm2 [SUPPOSED TO HAVE] EXITED"
    print rm

    return rm



def run_abxfwhm(runnable_manager, file_main, file_abonds, options,
                file_abxfwhm, custom_id=None):
    """
    """
    assert isinstance(runnable_manager, RunnableManager)
    assert isinstance(file_main, FileMain)
    assert isinstance(file_abonds, FileAbonds)
    assert isinstance(options, Options)
    assert isinstance(file_abxfwhm, FileAbXFwhm)

    logger = get_python_logger()


    # # Preparation

    ms_dir = "multi-session-"+str(custom_id)
    os.mkdir(ms_dir)


    symbols = file_abxfwhm.ab.keys()
    abdiffss = file_abxfwhm.ab.values()

    n_abdif = len(abdiffss[0])
    for abdifs in abdiffss:
        if len(abdifs) != n_abdif:
            raise RuntimeError('All abundances vectors must have length of %d' % n_abdif)

    fwhms = file_abxfwhm.get_fwhms()
    n_fwhms = len(fwhms)

    for fwhm in fwhms:
        if fwhm > 9.99:
            raise RuntimeError("fhwm maximum is 9.99")

    # # Runs pfant, pfant creates several .norm files

    logger.info("+++ pfant phase...")

    pfant_list = []
    for j in range(n_abdif):
        file_abonds_ = copy.deepcopy(file_abonds)

        for i, symbol in enumerate(symbols):
            found = False
            for k in range(len(file_abonds_.ele)):
                if file_abonds_.ele[k] == symbol:
                    abdif = abdiffss[i][j]
                    file_abonds_.abol[k] += abdif
                    print j, " - ", symbol, "with abundance", file_abonds_.abol[k]
                    found = True

            if not found:
                raise RuntimeError("Atom '%s' not found" % symbol)

        pfant = Pfant()
        pfant.conf.opt = copy.copy(options)
        pfant.conf.file_main = file_main
        pfant.conf.file_abonds = file_abonds_
        pfant.conf.file_dissoc = file_abonds_
        # todo replace j with a descriptive name, configurable in FileAbXFwhm
        pfant.conf.opt.flprefix = "%s_%02d" % (file_main.titrav, j)
        pfant.conf.session_id = pfant.conf.opt.flprefix

        logger.debug(pfant.conf.opt.flprefix)

        f = pfant.conf.file_abonds
        pfant_list.append(pfant)

    run_parallel(pfant_list, MAX_FORTRANS, flag_console=False)
    # runnable_manager.add_runnables(pfant_list)
    # runnable_manager.wait_until_finished()


    # # Runs nulbad, saves .sp and .spl files

    logger.info("+++ nulbad phase...")

    # function to convert given FWHM to string to use as part of a file name
    fmt_fwhm = lambda x: "%03d" % round(x*100)

    nulbad_list = []
    sp_filenames_by_fwhm = {}  # dictionary containing a list of .sp filenames for each FWHM
    for pfant in pfant_list:
        prefix = pfant.conf.opt.flprefix

        for fwhm in fwhms:
            nulbad = Nulbad()
            nulbad.conf.opt = copy.copy(options)
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

    runnable_manager.add_runnables(nulbad_list)
    runnable_manager.wait_until_finished()

    # # Deletes session-* directories

    print "+++ cleaning up..."

    for pfant in pfant_list:
      pfant.conf.clean()
    for nulbad in nulbad_list:
      nulbad.conf.clean()










# def run_parallel(rr, max_simultaneous=None, flag_console=True):
#     """
#     Arguments:
#       rr -- list of Combo
#       max_simultaneous -- maximum number of simultaneous processes.
#     """
#     # Adds to pool
#     rm = RunnableManager(max_threads=max_simultaneous)
#     rm.start()
#
#     for p in rr:
#         rm.add_runnable(p)
#
#     # Primitive thread monitor
#     while True:
#         if flag_console:
#             print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
#             s = raw_input("press enter or 'e' to exit")
#             if s.lower() == "e":
#                 break
#             print "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
#             print rm
#         else:
#             if rm.flag_finished():
#                 break
#             print "waiting..."
#             time.sleep(.5)
#
#     rm.exit()
#
#     print "FINISHED"



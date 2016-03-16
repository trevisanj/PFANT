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











class Executable(Runnable):
    """
    PFANT executables common ancestor class.
    """

    # Set at descendant class with a pyfant.conf.FOR_* value
    sequence_index = -1

    @property
    def returncode(self):
        return self.__returncode

    @property
    def exe_path(self):
        return self._exe_path

    @exe_path.setter
    def exe_path(self, x):
        self._exe_path = x

    # @property
    # def logger(self):
    #     return self.__logger
    # @logger.setter
    # def logger(self, x):
    #     self.__logger = x

    # @property
    # def stdout(self):
    #     return self.__stdout
    #
    # @stdout.setter
    # def stdout(self, x):
    #     self.__stdout = x

    def __init__(self):
        Runnable.__init__(self)
        # # Protected variables
        # Full path to executable (including executable name)
        self._exe_path = "none"
        # ExecutableStatus instance
        self._status = ExecutableStatus(self)

        # # Private variables
        # # Will receive Python output
        # self.__logger = None
        # fill be filled with self.popen.returncode in due time
        self.__returncode = None
        # Created by _run()
        self.__popen = None
        # # file-like object, or None: will receive Fortran output
        # self.__stdout = None
        # It is either a blocking _run() or asynchronous open..kill..close
        self.__lock = Lock()

    def run(self):
        """Runs executable.

        Blocking routine. Only returns when executable finishes running.
        """
        assert not self._flag_running, "Already running"
        assert not self._flag_finished, "Already finished"
        # self.__logger = get_python_logger()
        self.conf.configure([self.sequence_index])
        try:
            self.conf.logger.debug("Running %s '%s'" % (self.__class__.__name__.lower(), self.name))
            self.__run()
        finally:
            self.conf.close_popen_text_dest()

    def run_from_combo(self):
        """Alternative to run executable (called from Combo class).

        This routine bypasses all the configuration that is done prior to running.
        (Combo.configure() will do the necessary configuration).
        """
        assert not self._flag_running, "Already running"
        assert not self._flag_finished, "Already finished"
        self.__run()

    def kill(self):
        self._flag_killed = True
        if self._flag_running:
            self.__popen.kill()

    def get_status(self):
        return self._status

    def __run(self):
        """Called both from run() and run_from_combo()."""
        with self.__lock:
            args = self.conf.get_args()
            cmd_words = [self._exe_path] + args

            # s = "%s command-line:" % (self.__class__.__name__.lower(),)
            #log_noisy(self.__logger, s)

            s = " ".join(cmd_words)
            self.conf.logger.debug(s)
            # logs command-line to file and closes it.
            with open(self.conf.join_with_session_dir("commands.log"), "a") as h:
                h.write(s+"\n\n")
            #self.__logger.info(X*(len(s)+4))

            emsg = ""
            try:
                self.__popen = subprocess.Popen(cmd_words, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

                # if self.__stdout:
                #     self.__popen = subprocess.Popen(cmd_words, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                # else:
                #     self.__popen = subprocess.Popen(cmd_words)
                self._flag_running = True
                try:
                    if self.conf.popen_text_dest is not None:
                        for line in self.__popen.stdout:
                            self.conf.popen_text_dest.write(line)
                finally:
                    self.__popen.stdout.close()


#todo cleanup
#                 self._flag_running = True
#
#                 if self.__stdout:  # TODO disabled PIPE stdout for popen
#                     try:
#                         for line in self.__popen.stdout:
#                             self.__stdout.write(line)
#                     finally:
#                         # todo cleanup
#                         # printOpenFiles()
#
#                         self.__popen.stdout.close()
#                         self.__stdout.close()
#
# #todo cleanup
#                         # print "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
#                         # printOpenFiles()
#                         # sys.exit()

                # blocks execution until finished
                self.__popen.wait()
                self.__popen.poll()

                if self.__popen.returncode != 0:
                    raise FailedError("%s failed (returncode=%s)" % (self.__class__.__name__.lower(), self.__popen.returncode))

            except Exception as e:
                flag_dismiss = False
                if self.__popen:
                    self.__popen.poll()
                    if isinstance(e, FailedError) and self._flag_killed:
                        # dismisses error if explicitly killed
                        flag_dismiss = True
                    elif isinstance(e, IOError) and self.__popen.returncode == 0:
                        # Sometimes a IOError is raised even if Fortran executes
                        # successfully, so the error is dismissed
                        self.conf.logger.warning("Harmless error in: %s %s" %
                         (self.conf.session_dir, self.get_status()))
                        self.conf.logger.warning(str(e))
                        flag_dismiss = True

                if not flag_dismiss:
                    self._error_message = e.__class__.__name__+": "+str(e)
                    self._flag_error = True
                    raise
            finally:
                self._flag_finished = True
                self._flag_running = False
                if self.__popen is not None:
                    self.__returncode = self.__popen.returncode
                self.conf.logger.debug(str(self._status))
















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



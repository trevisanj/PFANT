"""
Miscellanea routines that depend on other pyfant modules.

Rule: no pyfant module can import util!!!

"""
__all__ = ["MultiRunnable"]
from pyfant import *
import copy
import os
from .misc import MULTISESSION_PREFIX
import logging


_multi_id_maker = IdMaker()
_multi_id_maker.session_prefix_singular = MULTISESSION_PREFIX


@froze_it
class MultiRunnableStatus(object):
    def __init__(self, runnable):
        assert isinstance(runnable, MultiRunnable)
        self.runnable = runnable
        self.stage = ""

    def __str__(self):
        l = []
        if self.runnable.flag_finished:
            l.append("finished")
        if self.runnable.flag_running:
            l.append("running - "+self.stage)
        if self.runnable.flag_killed:
            l.append("*killed*")
        if self.runnable.flag_error:
            l.append("*error*")
        if self.runnable.error_message:
            l.append("*" + str(self.runnable.error_message) + "*")
        if len(l) > 0:
            return " ".join(l)
        return "?"


class MultiRunnable(Runnable):
    """
    Differential abundances X FWHM's runnable.
    """

    def __init__(self, file_main, file_abonds, options, file_abxfwhm,
                 custom_id=None):
        Runnable.__init__(self)
        assert isinstance(file_main, FileMain)
        assert isinstance(file_abonds, FileAbonds)
        assert isinstance(options, Options)
        assert isinstance(file_abxfwhm, FileAbXFwhm)
        self.__file_main = file_main
        self.__file_abonds = file_abonds
        self.__options = options
        self.__file_abxfwhm = file_abxfwhm
        self.__custom_id = custom_id

        # # Protected variables
        # ExecutableStatus instance
        self.__status = MultiRunnableStatus(self)

        # # Private variables
        self.__logger = get_python_logger()
        self.__sid = SID(_multi_id_maker)
        self.__runnable_manager = None

    def kill(self):
        self._flag_killed = True
        if self._flag_running and self.__rm:
            self.__rm.exit()

    def get_status(self):
        return self.__status

    def run(self):
        """Runs executable.

        Blocking routine. Only returns when executable finishes running.
        """
        assert not self._flag_running, "Already running"
        assert not self._flag_finished, "Already finished"


        self._flag_running = True
        try:
            self.__run()
        except Exception as e:
            self._error_message = e.__class__.__name__+": "+str(e)
            self._flag_error = True
            raise
        finally:
            self._flag_finished = True
            self._flag_running = False
            self.__logger.debug(str(self.__status))

    def __run(self):
        # Called from run() to lower one indentation lever.
        # If something is not right here, just raise.

        ####
        # # Preparation
        ####
        self.__status.stage = "preparing"
        if self.__custom_id:
            self.__sid.id = self.__custom_id
        else:
            self.__sid.make_id()


        ih = Combo([FOR_INNEWMARCS, FOR_HYDRO2])
        ih.conf.flag_output_to_dir = True
        ih.conf.logger = self.__logger
        ih.conf.opt = copy.copy(self.__options)
        ih.conf.sid = self.sid

        # running innewmarcs and hydro2
        ih.run()


        # print ih.conf.opt.fn_modeles
        # print "HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
        # print "HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
        # print "HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
        # print "HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
        # raise RuntimeError("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")


        custom_id_maker = IdMaker()
        custom_id_maker.session_prefix_singular = \
         os.path.join(self.__sid.dir, "session-")

        symbols = self.__file_abxfwhm.ab.keys()
        abdiffss = self.__file_abxfwhm.ab.values()
        n_abdif = len(abdiffss[0])

        # for abdifs in abdiffss:
        #     if len(abdifs) != n_abdif:
        #         raise RuntimeError(
        #             'All abundances vectors must have length of %d' % n_abdif)


        fwhms = self.__file_abxfwhm.get_fwhms()
        n_fwhms = len(fwhms)

        for fwhm in fwhms:
            if fwhm > 9.99:
                raise RuntimeError("fhwm maximum is 9.99")

        ####
        # # Runs pfant, pfant creates several .norm files
        self.__status.stage = "pfant stage"
        self.__logger.info("+++ pfant stage...")
        pfant_list = []
        for j in range(n_abdif):
            file_abonds_ = copy.deepcopy(self.__file_abonds)

            for i, symbol in enumerate(symbols):
                found = False
                for k in range(len(file_abonds_.ele)):
                    if file_abonds_.ele[k] == symbol:
                        abdif = abdiffss[i][j]
                        file_abonds_.abol[k] += abdif
                        self.__logger.debug(j, " - ", symbol, "with abundance", file_abonds_.abol[k])
                        found = True

                if not found:
                    raise RuntimeError("Atom '%s' not found" % symbol)

            pfant = Pfant()
            pfant.conf.sid.id_maker = custom_id_maker
            pfant.conf.logger = self.__logger
            pfant.conf.opt = copy.copy(self.__options)
            pfant.conf.file_main = self.__file_main
            pfant.conf.file_abonds = file_abonds_
            pfant.conf.file_dissoc = file_abonds_.get_file_dissoc()
            # configuration overridden to match that of the Combo object
            # (in order to find the files created by innewmarcs and hydro2).
            pfant.conf.opt.fn_modeles = ih.conf.opt.fn_modeles
            pfant.conf.file_hmap = ih.conf.file_hmap

            pfant_name = self.__file_abxfwhm.pfant_names[j] \
                if self.__file_abxfwhm.pfant_names \
                else "%02d" % j
            flprefix = "%s_%s" % (self.__file_main.titrav, pfant_name)
            pfant.conf.opt.flprefix = os.path.join(self.__sid.dir, flprefix)
            pfant.conf.sid.id = flprefix

            self.__logger.debug(pfant.conf.opt.flprefix)

            pfant_list.append(pfant)
        rm = self.__rm = RunnableManager()
        run_parallel(pfant_list, flag_console=False, runnable_manager=rm)
        if self._flag_killed:
            return

        if not rm.flag_success:
            raise RuntimeError("Not all pfant's succeeded running.")

        ####
        # # Runs nulbad, saves .sp and .spl files
        # function to convert given FWHM to string to use as part of a file name
        fmt_fwhm = lambda x: "%03d" % round(x * 100)

        # ## Prepares nulbad's
        nulbad_list = []
        sp_filenames_by_fwhm = {}  # dictionary containing a list of .sp filenames for each FWHM
        for pfant in pfant_list:
            # prefix is sth like: multi-session-1/Sun_00
            prefix = pfant.conf.opt.flprefix

            for fwhm in fwhms:
                nulbad = Nulbad()
                nulbad.conf.sid.id_maker = custom_id_maker
                nulbad.conf.logger = self.__logger
                nulbad.conf.opt = copy.copy(self.__options)
                nulbad.conf.opt.fn_flux = pfant.conf.opt.flprefix+".norm"
                nulbad.conf.opt.fwhm = fwhm
                nulbad.conf.opt.fn_cv = "%s_%s.sp" % (prefix, fmt_fwhm(fwhm))
                nulbad_list.append(nulbad)

                if not fwhm in sp_filenames_by_fwhm:
                    sp_filenames_by_fwhm[fwhm] = []
                sp_filenames_by_fwhm[fwhm].append(nulbad.conf.opt.fn_cv)

        # ## Saves files for lineplot.py (lists of spectra)
        for fwhm, sp_filenames in sp_filenames_by_fwhm.iteritems():
            spl_filename = os.path.join(self.__sid.dir, "cv_%s.spl" % fmt_fwhm(fwhm))
            with open(spl_filename, "w") as h:
                for sp_filename in sp_filenames:
                    h.write(os.path.basename(sp_filename)+"\n")

        # ## Runs nulbads
        self.__status.stage = "nulbad stage"
        self.__logger.info("+++ nulbad stage...")
        rm = self.__rm = RunnableManager()
        run_parallel(nulbad_list, flag_console=False, runnable_manager=rm)
        if self._flag_killed:
            return
        if not rm.flag_success:
            raise RuntimeError("Not all nulbad's succeeded running.")

        ####
        # # Deletes session-* directories if successful
        self.__logger.info("+++ Cleaning up...")

        for pfant in pfant_list:
            pfant.sid.clean()
        for nulbad in nulbad_list:
            nulbad.sid.clean()

    def _get_sid(self):
        return self.__sid

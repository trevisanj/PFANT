__all__ = ["Runnable", "RunnableStatus", "Executable", "Innewmarcs", "Hydro2",
           "Pfant", "Nulbad", "Combo"]

import subprocess
import logging
from .execonf import *
import os
import sys
from .misc import *
from .errors import *
from .parts import *
from pyfant import FileSpectrumPfant, FileSpectrumNulbad, FileMod
from threading import Lock

class RunnableStatus(PyfantObject):
    """Data class, stores progress information."""

    def __init__(self, runnable, ikey=None, ikeytot=None):
        assert isinstance(runnable, Runnable)
        self.ikey = ikey
        self.ikeytot = ikeytot
        self.exe_name = runnable.__class__.__name__.lower()
        self.is_running = runnable.is_running
        self.is_finished = runnable.is_finished
        self.error_message = runnable.error_message

    def __str__(self):
        l = []

        if self.exe_name is not None:
            l.append(self.exe_name)
        if self.is_finished:
            l.append("finished")
            if self.error_message:
                l.append("*"+self.error_message+"*")
        elif self.is_finished:
            l.append("running")
        if self.ikey is not None:
            l.append ("%5.1f %% (%d/%d)" % (100.*self.ikey/self.ikeytot, self.ikey, self.ikeytot))
        if len(l) > 0:
            return " ".join(l)
        return "?"


class Runnable(PyfantObject):
    """
    Object with a run() method.

    The run() method is blocking, i.e., it only returns when running is done.

    This is a base class for Executable and Combo.
    """

    def __init__(self):
        self.name = random_name()

    def run(self):
        raise NotImplementedError()

    def get_status(self):
        """
        Returns progress information.

        Draft of a protocol:
          string:  just human-readable information
          (ikey, ikeytot)
          None -- nothing available
        """
        return RunnableStatus(self)


class Executable(Runnable):
    """
    Generic class to represent an executable.
    """

    @property
    def is_finished(self):
        with self.__L_running:
            return self.__is_finished
    @property
    def is_running(self):
        with self.__L_running:
            return self.__is_running
    @property
    def error_message(self):
        with self.__L_running:
            return self.__error_message

    def __init__(self):
        Runnable.__init__(self)
        # Full path to executable (including executable name)
        self.exe_path = "none"
        # File object to log executable stdout
        self.logfile = None
        # ExeConf instance
        self.conf = ExeConf()
        # Created by _run()
        self.popen = None
        # file-like object, or None: will receive Fortran output
        self.stdout = None
        # Will receive Python output
        self.logger = None
        # Lock is necessary because sometimes more than one locked variable
        # needs to be accessed consistently
        self.__L_running = Lock()
        # Is running?
        self.__is_running = False
        # Is finished?
        self.__is_finished = False
        # Will contain error message if finished with error
        self.__error_message = ""

    def get_status(self):
        return RunnableStatus(self)

    def run(self):
        """Runs executable.

        Blocking routine. Only returns when executable finishes running.
        """
        assert not self.__is_running, "Already running"
        self.logger = logging.getLogger("%s%d" % (self.__class__.__name__.lower(), id(self)))
        # this was leaving file open after finished add_file_handler(self.logger, "python.log")
        self.logger.info("Running %s '%s'" % (self.__class__.__name__.lower(), self.name))
        self.conf.make_session_id()
        self.conf.create_data_files()
        self._run()

    def run_from_combo(self):
        """Alternative to run executable (called from Combo class).

        This routine bypasses all the configuration that is done prior to running.
        (Combo.configure() will do the necessary configuration).
        """
        assert not self.__is_running, "Already running"
        self._run()

    # def is_running(self):
    #     if self.popen is None:
    #         return False
    #     self.popen.poll()
    #     return self.popen.returncode is None

    def kill(self):
        assert self.__is_running, "Not running"
        if self.popen:
            self.popen.kill()
        else:
            self.logger.critical("Called kill() but there is no popen for %s instance" % self.__class__.__name__)

    def _run(self):
        assert not self.__is_finished, "Can only run once!"
        args = self.conf.get_args()
        cmd_line = [self.exe_path]+args

        s = "%s command-line:" % (self.__class__.__name__.lower(),)
        log_noisy(self.logger, s)
        self.logger.info(" ".join(cmd_line))
        self.logger.info(X*(len(s)+4))

        self.__is_running = True
        emsg = ""
        try:
            if self.stdout:
                try:
                    self.popen = subprocess.Popen(cmd_line, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                    for line in self.popen.stdout:
                        self.stdout.write(line)
                finally:
                    self.popen.stdout.close()
                    self.stdout.close()
            else:
                self.popen = subprocess.Popen(cmd_line)
            # blocks execution until finished
            self.popen.wait()
            self.popen.poll()
        except Exception as e:
            self.logger.critical(fmt_error("Failed to execute command-line"))
            emsg = e.__class__.__name__+": "+str(e)
            raise
        finally:
            with self.__L_running:
                # records status
                self.__error_message = emsg
                self.__is_finished = True
                self.__is_running = False
        log_noisy(self.logger, "%s %s (returncode=%s)" %
                    (self.__class__.__name__.lower(),
                     'finished successfully' if self.popen.returncode == 0 else '*failed*',
                     self.popen.returncode))

    def load_result(self):
        """Override this method to open the result file(s) particular to the
        executable."""


class Innewmarcs(Executable):
    """Class representing the innewmarcs executable."""

    def __init__(self):
        Executable.__init__(self)
        self.exe_path = "innewmarcs"

        # FileMod object
        self.modeles = None

    def load_result(self):
        file_mod = FileMod()
        filepath = self.conf.get_fn_modeles()
        file_mod.load(filepath)
        self.modeles = file_mod


class Hydro2(Executable):
    """Class representing the hydro2 executable."""

    def __init__(self):
        Executable.__init__(self)
        self.exe_path = "hydro2"

    def load_result(self):
        raise NotImplementedError("Opening hydro2 result will need hydro2 to save a side file containing a list of the files that it has created!!!")


class Pfant(Executable):
    def __init__(self):
        Executable.__init__(self)
        self.exe_path = "pfant"  # Path to PFANT executable (including executable name)

        # ** Variables assigned by poll_progress()
        self.ikey = None  # Current iteration (Fortran: ikey)
        self.ikeytot = None  # Current iteration (Fortran: ikeytot)

        # # Spectra loaded after pfant runs if self.flag_open_result is True

        # .spec file, e.g., flux.spec
        self.spec = None
        # .cont file, e.g., flux.cont
        self.cont = None
        # .norm file, e.g., flux.norm
        self.norm = None

    def get_status(self):
        """
        Tries to open progress indicator file. If succeeds, stores
        current iteration in attribute ikey, and
        number of iterations in attribute ikeytot.

        """
        p = self.conf.opt.fn_progress
        ret = RunnableStatus(self)
        if not self.is_finished and os.path.isfile(p):
            with open(p) as h:
                try:
                    t = map(int, h.readline().split("/"))
                    ret.ikey = t[0]
                    ret.ikeytot = t[1]
                except ValueError:
                    # Ignores conversion errors
                    pass
        return ret

    def load_result(self):
        file_sp = FileSpectrumPfant()
        for type_ in ("norm", "cont", "spec"):
            filepath = self.conf.get_pfant_output_filepath(type_)
            file_sp.load(filepath)
            self.__setattr__(type_, file_sp.spectrum)


class Nulbad(Executable):
    """Class representing the nulbad executable."""

    def __init__(self):
        Executable.__init__(self)
        self.exe_path = "nulbad"

        # nulbad output
        self.convolved = None

    def load_result(self):
        file_sp = FileSpectrumNulbad()
        filepath = self.conf.get_nulbad_output_filepath()
        file_sp.load(filepath)
        self.convolved = file_sp.spectrum


class Combo(Runnable):
    """
    Runs sequence of executables: innermarcs, hydro2, pfant, nulbad.

    Arguments:
      sequence (optional) -- sequence of executables to run. Defaults to
        [e_innewmarcs, e_hydro2, e_pfant, e_nulbad]. If you want to run only
        pfant and nulbad, for example, you can pass [e_pfant, e_nulbad]

    There are several restrictions imposed
    - files are created inside a session directory such as session123456
    - all four executables must be in the same directory

    """

    @property
    def is_finished(self):
        with self.__L_running:
            return self.__is_finished
    @property
    def is_running(self):
        with self.__L_running:
            return self.__is_running
    @property
    def running_exe(self):
        """Returns the current or last running exe."""
        with self.__L_running:
            return self.__running_exe

    def __init__(self, sequence=None):
        Runnable.__init__(self)
        # Directory containing the 4 executables
        self.exe_dir = ""

        # Executables to run
        # order is irrelevant (will be sorted anyway).
        self.sequence = [e_innewmarcs, e_hydro2, e_pfant, e_nulbad] \
            if sequence is None else sequence

        # ExeConf instance
        self.conf = ExeConf()

        # ** Executable instances
        self.innewmarcs = Innewmarcs()
        self.hydro2 = Hydro2()
        self.pfant = Pfant()
        self.nulbad = Nulbad()

        # ** Internal variables
        self.__L_running = Lock()  # To make the following variables thread-safe
        self.__is_running = False
        self.__running_exe = None  # Executable object currently running
        self.__is_finished = False

        self.logger = None

    def configure(self):
        """
        Sets several properties of conf and executables to achieve the expected
        synchronization.

        Note: this routine messes with sys.stdout to log to screen and file
        simultaneously.
        """

        c = self.conf

        c.make_session_id()
        c.prepare_filenames_for_combo(self.sequence)

        self.logger = logging.getLogger("combo%d" % id(self))
        # this was leaving file open after finished add_file_handler(self.logger, c.join_with_session_dir("python.log"))
        self.logger.info("Running %s '%s'" % (self.__class__.__name__.lower(), self.name))

        stdout_ = LogTwo(c.join_with_session_dir("fortran.log"))


        # All files that will be created need to have the session directory added to their names
        for e in self.get_exes():
            # Propagates configuration
            e.conf = c
            e.stdout = stdout_
            e.logger = self.logger
            # e.logger = self.logger

            # Fixes exe path
            exe_filename = os.path.split(e.exe_path)[-1]
            e.exe_path = os.path.join(self.exe_dir, exe_filename)

        c.create_data_files()

    def get_exes(self):
        """Returns exe objects in a list according with self.sequence."""

        map = [(e_innewmarcs, self.innewmarcs), (e_hydro2, self.hydro2), (e_pfant, self.pfant),
               (e_nulbad, self.nulbad)]
        res = []
        ii, ee = zip(*map)
        self.sequence.sort()
        for i_exe in self.sequence:
            if i_exe in ii:
                res.append(ee[ii.index(i_exe)])
        return res

    def run(self):
        """Blocking routine. Overwrites self.popen."""

        assert not self.is_running, "Already running"

        self.__is_running = True
        try:
            self.configure()

            for e in self.get_exes():
                self.__running_exe = e
                e.run_from_combo()
                if e.popen.returncode != 0:
                    raise FailedError("%s failed" % e.__class__.__name__.lower())
        finally:
            with self.__L_running:
                # leave it as last self.__running_exe = None
                self.__is_running = False
                self.__is_finished = True

    def get_status(self):
        """Returns status of running executable or none."""
        with self.__L_running:
            if self.__running_exe:
                return self.__running_exe.get_status()
            else:
                return "?"

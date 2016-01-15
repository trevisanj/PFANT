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
    
    def __init__(self, runnable):
        assert isinstance(runnable, Runnable)
        self.exe_name = runnable.__class__.__name__.lower()
        self.runnable = runnable
        self.ikey = None
        self.ikeytot = None
        self.flag_kill = False
        self.returncode = None

    def __str__(self):
        l = []

        if self.exe_name is not None:
            l.append(self.exe_name)
        if self.runnable.flag_finished:
            l.append("finished")
        elif self.runnable.flag_running:
            l.append("running")
        elif self.runnable.flag_killed:
            l.append("killed")
        if self.runnable.flag_error:
            l.append("error")
        if self.runnable.error_message:
            l.append("*"+str(self.runnable.error_message)+"*")
        if self.ikey is not None:
            l.append("%5.1f %% (%d/%d)" % (100.*self.ikey/self.ikeytot, self.ikey, self.ikeytot))
        if self.flag_kill:
            l.append("*kill*")
        if self.returncode is not None:
            l.append("returncode=%d" % self.returncode)
        if len(l) > 0:
            return " ".join(l)
        return "?"


class Runnable(PyfantObject):
    """
    Object with a run() method.

    The run() method is blocking, i.e., it only returns when running is done.

    This is a base class for Executable and Combo.
    """

    @property
    def flag_finished(self):
        return self._flag_finished
    @property
    def flag_running(self):
        return self._flag_running
    @property
    def flag_killed(self):
        return self._flag_killed
    @property
    def flag_error(self):
        return self._flag_error
    @property
    def error_message(self):
        return self._error_message

    def __init__(self):
        self.name = random_name()
        self.status = RunnableStatus(self)
        # Is running?
        self._flag_running = False
        # Is finished?
        self._flag_finished = False
        # Was killed?
        self._flag_killed = False
        # Had error?
        self._flag_error = False
        # Will contain error message if finished with error
        self._error_message = ""
        
    def get_status(self):
        return self.status

    def run(self):
        raise NotImplementedError()


class Executable(Runnable):
    """
    PFANT executables common ancestor class.
    """

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
        # It is either a blocking _run() or asynchronous open..kill..close
        self.__lock = Lock()
        # Registers kill() call. Debugging purpose initially, because the popen's don't seem to exit on demand
        self.flag_kill = False

    def run(self):
        """Runs executable.

        Blocking routine. Only returns when executable finishes running.
        """
        assert not self._flag_running, "Already running"
        self.logger = get_python_logger()
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
        assert not self._flag_running, "Already running"
        assert not self._flag_finished, "Already finished"
        self._run()

    # def flag_running(self):
    #     if self.popen is None:
    #         return False
    #     self.popen.poll()
    #     return self.popen.returncode is None

    def kill(self):
        assert self._flag_running, "Not running"
        self.popen.kill()
        self.status.flag_kill = True

    def _run(self):
        assert not self._flag_running, "Already running"
        assert not self._flag_finished, "Already finished"
        with self.__lock:
            args = self.conf.get_args()
            cmd_words = [self.exe_path]+args

            # s = "%s command-line:" % (self.__class__.__name__.lower(),)
            #log_noisy(self.logger, s)
            self.logger.info(" ".join(cmd_words))
            #self.logger.info(X*(len(s)+4))

            emsg = ""
            try:
                if self.stdout:  # TODO disabled PIPE stdout for popen
                    self.popen = subprocess.Popen(cmd_words, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                else:
                    self.popen = subprocess.Popen(cmd_words)

                self._flag_running = True

                if self.stdout:  # TODO disabled PIPE stdout for popen
                    try:
                        for line in self.popen.stdout:
                            self.stdout.write(line)
                    finally:
                        self.popen.stdout.close()
                        self.stdout.close()


                # blocks execution until finished
                self.popen.wait()
                self.popen.poll()
            except Exception as e:
                self.popen.poll()
                # self.logger.critical(fmt_error("Failed to execute command-line (returncode=%s)" % self.popen.returncode))
                emsg = e.__class__.__name__+": "+str(e)
                raise
            finally:
                self._error_message = emsg
                if emsg:
                    self._flag_error = True
                self._flag_finished = True
                self._flag_running = False
                self.status.returncode = self.popen.returncode
                self.logger.info("%s %s (returncode=%s)%s" %
                        (self.__class__.__name__.lower(),
                         'finished successfully' if self.popen.returncode == 0 else '*failed*',
                         self.popen.returncode,
                         " *KILL*" if self.status.flag_kill else ""))

#    def _open_popen(self):
#        with self.__lock:
#            args = self.conf.get_args()
#            cmd_words = [self.exe_path]+args
#
#            s = "%s command-line:" % (self.__class__.__name__.lower(),)
#            log_noisy(self.logger, s)
#            self.logger.info(" ".join(cmd_words))
#            self.logger.info(X*(len(s)+4))
#
#
#            try:
#                if self.stdout:
#                    self.popen = subprocess.Popen(cmd_words, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
#                else:
#                    self.popen = subprocess.Popen(cmd_words)
#                self._flag_running = True
#            except Exception as e:
#                self.logger.critical(fmt_error("Failed to execute command-line"))
#                self._error_message = e.__class__.__name__+": "+str(e)
#                self._flag_finished = True  # finished prematurely
#                raise
#       
#    def _close_popen(self):
#        """To be called on a popen that has finished."""
#        with self.__lock:
#            if self.stdout:
#                for line in self.popen.stdout:
#                    self.stdout.write(line)
#                self.popen.stdout.close()
#                self.stdout.close()
#            self.popen.poll()
#            self._flag_finished = True
#            self._flag_running = False
#        
#    def _kill_popen(self):
#        with self.__lock:
#            self.popen.kill()
#            self._flag_finished = True
#            self._flag_running = False
#            self._flag_killed = True
#            if self.stdout:
#                self.popen.stdout.close()
#                self.stdout.close()

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
        ret = self.status
        if (not self.ikey or self.ikey < self.ikeytot) and os.path.isfile(p):
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
    def running_exe(self):
        """Returns the current or last running exe."""
        return self.__running_exe

    def __init__(self, sequence=None):
        Runnable.__init__(self)
        # # Configuration
        # Directory containing the 4 executables
        self.exe_dir = ""
        # Whether to display Fortran messages in the terminal.
        # NoteL a file named "<session dir>/fortran.log" is always created.
        self.flag_log_console = True

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
        self.__running_exe = None  # Executable object currently running
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

        self.logger = get_python_logger()
        # this was leaving file open after finished add_file_handler(self.logger, c.join_with_session_dir("python.log"))
        self.logger.info("Running %s '%s'" % (self.__class__.__name__.lower(), self.name))

        log_path = c.join_with_session_dir("fortran.log")
        if self.flag_log_console:
            stdout_ = LogTwo(log_path)
        else:
            stdout_ = open(log_path, "w")

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

        assert not self.flag_running, "Already running"

        self._flag_running = True
        try:
            self.configure()

            for e in self.get_exes():
                self.__running_exe = e
                e.run_from_combo()
                if e.popen.returncode != 0:
                    raise FailedError("%s failed (returncode=%s)" % (e.__class__.__name__.lower(), e.popen.returncode))
        finally:
            # leave it as last self.__running_exe = None
            self._flag_running = False
            self._flag_finished = True

    def kill(self):
        if self._flag_running:
            if self.__running_exe:
                self.__running_exe.kill()

    def get_status(self):
        """Returns status of running executable or "?"."""
        if self.__running_exe:
            return self.__running_exe.get_status()
        else:
            return "?"
                
                

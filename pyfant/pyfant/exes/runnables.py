__all__ = ["Runnable", "Progress", "Executable", "Innewmarcs", "Hydro2", "Pfant", "Nulbad", "Combo"]

import subprocess
import logging
from .execonf import *
import os
import sys
from ..misc import *
from threading import Lock
from ..errors import *
from ..parts import *

class Progress(PyfantObject):
    """Data class, stores progress information."""

    def __init__(self, ikey=None, ikeytot=None, exe_name=None):
        self.ikey = ikey
        self.ikeytot = ikeytot
        self.exe_name = exe_name

    def __str__(self):
        l = []
        if self.exe_name is not None:
            l.append('running %s' % self.exe_name)
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

    def get_progress(self):
        """
        Returns progress information.

        Draft of a protocol:
          string:  just human-readable information
          (ikey, ikeytot)
          None -- nothing available
        """
        return Progress()


class Executable(Runnable):
    """
    Generic class to represent an executable.
    """

    def __init__(self):
        Runnable.__init__(self)

        # Full path to executable (including executable name)
        self.exe_path = "none"

        # File object to log executable stdout
        self.logfile = None

        # ExeConf instance
        self.execonf = ExeConf()
        # Created by _run()
        self.popen = None

        # file-like object, or None: will receive Fortran output
        self.stdout = None

        # Will receive Python output
        self.logger = None

    def get_progress(self):
        return Progress(exe_name=self.__class__.__name__.lower())

    def run(self):
        """Runs executable.

        Blocking routine. Only returns when executable finishes running.
        """
        assert not self.is_running(), "Already running"
        self.logger = logging.getLogger("%s%d" % (self.__class__.__name__.lower(), id(self)))
        add_file_handler(self.logger, self.execonf.full_path_w("python.log"))
        self.logger.info("Running %s '%s'" % (self.__class__.__name__.lower(), self.name))
        self.execonf.make_session_id()
        self.execonf.create_data_files()
        self._run()

    def run_from_combo(self):
        """Alternative to run executable (called from Combo class).

        This routine bypasses all the configuration that is done prior to running.
        (Combo.configure() will do the necessary configuration).
        """
        assert not self.is_running(), "Already running"
        self._run()

    def _run(self):
        args = self.execonf.get_args()
        cmd_line = [self.exe_path]+args

        s = "%s command-line:" % (self.__class__.__name__.lower(),)
        log_noisy(self.logger, s)
        self.logger.info(" ".join(cmd_line))
        self.logger.info(X*(len(s)+4))


        try:
            if self.stdout:
                self.popen = subprocess.Popen(cmd_line, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                for line in self.popen.stdout:
                    self.stdout.write(line)
            else:
                self.popen = subprocess.Popen(cmd_line)

        except OSError:
            self.logger.critical(fmt_error("Failed to execute command-line"))
            raise
        self.popen.wait()  # Blocks execution until finished
        self.popen.poll()
        log_noisy(self.logger, "%s %s (returncode=%s)" %
                    (self.__class__.__name__.lower(),
                     'finished successfully' if self.popen.returncode == 0 else '*failed*',
                     self.popen.returncode))

    def is_running(self):
        if self.popen is None:
            return False
        self.popen.poll()
        return self.popen.returncode is None

    def kill(self):
        assert self.is_running(), "Not running"
        self.popen.kill()


class Innewmarcs(Executable):
    """Class representing the innewmarcs executable."""

    def __init__(self):
        Executable.__init__(self)
        self.exe_path = "innewmarcs"


class Hydro2(Executable):
    """Class representing the hydro2 executable."""

    def __init__(self):
        Executable.__init__(self)
        self.exe_path = "hydro2"


class Pfant(Executable):
    def __init__(self):
        Executable.__init__(self)
        self.exe_path = "pfant"  # Path to PFANT executable (including executable name)

        # ** Variables assigned by poll_progress()
        self.ikey = None  # Current iteration (Fortran: ikey)
        self.ikeytot = None  # Current iteration (Fortran: ikeytot)

    def get_progress(self):
        """
        Tries to open progress indicator file. If succeeds, stores
        current iteration in attribute ikey, and
        number of iterations in attribute ikeytot.

        """
        p = self.execonf.full_path_w(self.execonf.opt.fn_progress)
        ret = Progress(exe_name="pfant")
        if os.path.isfile(p):
            with open(p) as h:
                try:
                    t = map(int, h.readline().split("/"))
                    ret = Progress(exe_name="pfant", ikey=t[0], ikeytot=t[1])
                except ValueError:
                    # Ignores conversion errors
                    pass
        return ret


class Nulbad(Executable):
    """Class representing the nulbad executable."""

    def __init__(self):
        Executable.__init__(self)
        self.exe_path = "nulbad"


class Combo(Runnable):
    """
    Runs sequence of executables: innermarcs, hydro2, pfant, nulbad.

    There are several restrictions imposed
    - files are created inside a session directory such as session123456
    - pfant and hydro2 need to run in "--hmap" mode
    - all four executables must be in the same directory

    """

    @property
    def is_running(self):
        with self.__L_running:
            return self.__is_running
    @is_running.setter
    def is_running(self, x):
        with self.__L_running:
            self.__is_running = x
    @property
    def running_exe(self):
        with self.__L_running:
            return self.__running_exe
    @running_exe.setter
    def running_exe(self, x):
        with self.__L_running:
            self.__running_exe = x

    def __init__(self):
        Runnable.__init__(self)
        # Directory containing the 4 executables
        self.exe_dir = ""

        # Executables to run
        # order is irrelevant (will be sorted anyway).
        self.sequence = [e_innewmarcs, e_hydro2, e_pfant, e_nulbad]

        # ExeConf instance
        self.execonf = ExeConf()

        # ** Executable instances
        self.innewmarcs = Innewmarcs()
        self.hydro2 = Hydro2()
        self.pfant = Pfant()
        self.nulbad = Nulbad()

        # ** Internal variables
        self.__L_running = Lock()  # To make the following variables thread-safe
        self.__is_running = False
        self.__running_exe = None  # Executable object currently running

        self.logger = None

    def configure(self):
        """
        Sets several properties of execonf and executables to achieve the expected
        synchronization.

        Note: this routine messes with sys.stdout to log to screen and file
        simultaneously.
        """

        c = self.execonf

        c.make_session_id()
        c.prepare_filenames_for_combo(self.sequence)

        self.logger = logging.getLogger("combo%d" % id(self))
        add_file_handler(self.logger, c.full_path_w(c.join_with_session_dir("python.log")))
        self.logger.info("Running %s '%s'" % (self.__class__.__name__.lower(), self.name))

        stdout_ = LogTwo(c.full_path_w(c.join_with_session_dir("fortran.log")))


        # All files that will be created need to have the session directory added to their names
        for e in self.get_exes():
            # Propagates configuration
            e.execonf = c
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

        self.is_running = True
        try:
            self.configure()

            for e in self.get_exes():
                self.running_exe = e
                e.run_from_combo()
                if e.popen.returncode != 0:
                    raise FailedError("%s failed" % e.__class__.__name__.lower())
        finally:
            self.running_exe = None
            self.is_running = False

    def get_progress(self):
        """
        Wraps Pfant.get_progress().
        """

        with self.__L_running:
            if self.__is_running and self.__running_exe:
                return self.__running_exe.get_progress()
            else:
                return Progress()

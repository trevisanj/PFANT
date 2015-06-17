# better to run with subprocess POpen, will be able to terminate
# tired

"""
Wraps over the PFANT executable
"""

__all__ = ["Pfant"]

from .data import *
from .misc import *
import subprocess
import os
import os.path
import threading
import logging

logger = logging.getLogger("pfant")

class PfantRunner(threading.Thread):
    """Thread that runs PFANT."""

    def __init__(self, pfant, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        assert isinstance(pfant, Pfant)
        self._pfant = pfant

    def run(self):
        self._pfant.run()


class PfantProgress(threading.Thread):
    pass





class Pfant(object):
    def __init__(self):
        self.exec_path = "./pfant"  # Path to PFANT executable (including executable name)

        # -----
        # Note: If any of these attributes are set, their corresponding fn_* variable
        #       will be overwritten
        # FileAbonds instance
        self.abonds = None
        # FileMain instance
        self.main = None

        self.fn_dissoc        = None
        self.fn_main          = None
        self.fn_partit        = None
        self.fn_absoru2       = None
        self.fn_modeles       = None
        self.fn_abonds        = None
        self.fn_atomgrade     = None
        self.fn_moleculagrade = None
        self.fn_lines         = None
        self.fn_log           = None

        self.inputdir = None
        self.outputdir = None

        self.stdout = None

        self.popen = None
        self.ikey = None  # Current iteration (Fortran: ikey)
        self.ikeytot = None  # Current iteration (Fortran: ikeytot)

        self._fn_progress = "progress.txt"

    def is_running(self):
        if self.popen is None:
            return False
        self.popen.poll()
        return self.popen.returncode is None

    def run(self):
        """Blocking routine. Overwrites self.popen."""

        assert not self.is_running(), "Already running"

        map0 = [(self.main, 'fn_main'),
                (self.abonds, 'fn_abonds'),
                ]

        for obj, attr_name in map0:
            if obj is not None:
                assert isinstance(obj, InputFile)
                # Makes filename, e.g., abonds32732.dat
                # -----
                # Uses default name as a base for name of file that doesn't exist
                name, ext = obj.default_filename.split('.')
                new_fn = new_filename(name, ext)
                fullpath = self._get_fullpath_i(new_fn)
                # Saves file
                obj.save(fullpath)
                # Overwrites config option
                self.__setattr__(attr_name, new_fn)

        l = self._get_command_line()

        logger.debug("PFANT Command-line:")
        logger.debug(" ".join(l))

        self.popen = subprocess.Popen(l, stdout=self.stdout)
        self.popen.wait()  # Blocks execution until finished
        logger.debug("PFANT returned")

    def kill(self):
        assert self.is_running(), "Not running"
        self.popen.kill()

    def poll(self):
        """Calls popen poll() and tries to read progress file."""
        if self.popen is not None:
            self.popen.poll()

        p = self._get_fullpath_o(self._fn_progress)
        if os.path.isfile(p):
            with open(p) as h:
                self.ikey, self.ikeytot = map(int, h.readline().split("/"))
        else:
            self.ikey, self.ikeytot = None, None

    def _get_fullpath_i(self, fn):
        """Joins self.inputdir with specified filename."""
        if self.inputdir is not None:
            return os.path.join(self.inputdir, fn)
        else:
            return fn

    def _get_fullpath_o(self, fn):
        """Joins self.inputdir with specified filename."""
        if self.inputdir is not None:
            return os.path.join(self.inputdir, fn)
        else:
            return fn

    def _get_command_line(self):
        """Returns list [program, arg0, arg1, ...]."""
        if self.exec_path is None:
            raise RuntimeError("Must set path to executable (exec_path)")

        cmd_line = [self.exec_path, "--fn_progress", self._fn_progress]

        # Input/output file names
        # Actually, options with argument
        # -----
        # command-line option has same name as self.* attribute
        map0 = ["fn_dissoc", "fn_main", "fn_partit", "fn_absoru2", "fn_modeles",
                "fn_abonds", "fn_atomgrade", "fn_moleculagrade", "fn_lines", "fn_log",
                "inputdir", "outputdir"] # @todo actually all options can be treated thus, I think

        for option_name in map0:
            arg = self.__getattribute__(option_name)
            if arg is not None:
                if isinstance(arg, str):
                    # Checks if string has space in order to add quotes
                    if " " in arg:
                        arg = '"'+arg+'"'
                cmd_line.append("--%s" % option_name)
                cmd_line.append(str(arg))

        return cmd_line
better to run with subprocess POpen, will be able to terminate
tired

"""
Wraps over the PFANT executable
"""

__all__ = ["Pfant"]

from .data import *
from .misc import *
import os.path
import os
import threading


class PfantRunner(threading.Thread):
    """Thread that runs PFANT."""

    def __init__(self, pfant, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        assert isinstance(pfant, Pfant)
        self._pfant = pfant

    def run(self):
        self._pfant.run()


class PfantProgress(threading.Thread):






class Pfant(object):

    @property
    def is_running(self):
        with self._L_running:
            return self._is_running


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

        self._is_running = False
        self._L_running = threading.Lock()

    def run(self):

        if self._is_running:
            raise RuntimeError('PFANT already running')

        self._is_running = True

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

        s = self._get_command_line()

        os.system(s)

        self._is_running = False


    def _get_fullpath_i(self, fn):
        """Joins self.inputdir with specified filename."""
        if self.inputdir is not None:
            return os.path.join(self.inputdir, fn)
        else:
            return fn

    def _get_command_line(self):
        if self.exec_path is None:
            raise RuntimeError("Must set path to executable (exec_path)")

        options = []

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
                options.append("--%s %s" % (option_name, arg))


        cmd_line = self.exec_path
        if len(options) > 0:
            cmd_line += " "+(" ".join(options))

        return cmd_line
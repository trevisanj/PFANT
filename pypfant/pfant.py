"""
Wraps over the PFANT executable
"""

from .data import *
from .misc import *
import os.path
import os

class pfant(object):
    def __init__(self):
        self.exec_path = None  # Path to PFANT executable (including executable name)

        self.abonds = None
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

    def run(self):
        map0 = [(self.abonds, 'fn_abonds')]

        for obj, attr_name in map0:
            if obj is not None:
                assert isinstance(obj, input_structure)
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


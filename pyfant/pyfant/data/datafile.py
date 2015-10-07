"""
Ancestor class for all classes that represent an input file.
"""

__all__ = ["DataFile"]

from ..misc import *
from ..parts import *


class DataFile(AttrsPart):
    default_filename = None  ## Descendants shoulds set this

    def __init__(self):
        AttrsPart.__init__(self)
        # File name is set by load()
        self.filename = None

    def save_as(self, filename=None):
        """
        Dumps object contents into file on disk.

        Arguments:
          filename (optional) -- defaults to self.filename. If passed, self.filename
            will be updated to filename.
        """
        if filename is None:
            filename = self.filename
        self.filename = filename
        self._do_save_as(filename)

    def _do_save_as(self, filename):
        raise NotImplementedError()

    def load(self, filename=None):
        """Loads file and registers filename as attribute."""
        if filename is None:
            filename = self.default_filename
        assert filename is not None, \
            "%s class has no default filename" % self.__class__.__name__

        self._do_load(filename)
        self.filename = filename


    def _do_load(self, filename):
        raise NotImplementedError()

    def init_default(self):
        """
        Default initialization of attributes listed in self.attrs.

        DataFile behaviour is to try to load self.default_filename from default
        directory.
        """
        fullpath = path_to_default(self.default_filename)
        self.load(fullpath)

"""
Class to store all command-line options & DataFile instances used to run one or more
executables.
"""

__all__ = ["Options", "ExeConf", "innewmarcs", "hydro2", "pfant", "nulbad"]

from pyfant.data import DataFile, FileHmap, FileMod
import shutil
import os
from ..parts import *
import re
from threading import Lock

# Indexes in workflow sequence
innewmarcs = 0
hydro2 = 1
pfant = 2
nulbad = 3


@froze_it
class Options(object):
    """Stores the command-line options.

    For each xxxx attribute not starting with "_" there exists
    a variable in config.f90 named config_xxxx, and
    a command-line option "--xxxx"

    """

    def __init__(self):
        # innewmarcs, hydro2, pfant, nulbad
        self.wdir = None
        self.logging_level = None
        self.logging_screen = None
        self.logging_dump = None
        self.logging_fn_dump = None
        self.fn_main = None
        self.explain = None

        # innewmarcs, hydro2, pfant
        self.fn_modeles = None

        # innewmarcs, hydro2
        self.teff = None
        self.glog = None
        self.asalog = None
        self.inum = None

        # hydro2, pfant
        self.fn_absoru2 = None
        self.hmap = True # Overriding default, which is .false.
        self.fn_hmap = None
        self.llzero = None
        self.llfin = None

        # innewmarcs
        self.open_status = None
        self.fn_moddat = None
        self.fn_gridslist = None
        self.modcode = None
        self.tirb = None

        # hydro2
        self.zph = None
        self.ptdisk = None
        self.kik = None
        self.amores = None
        self.kq = None
        self.nomplot = None
        self.vvt = None
        self.na = None
        self.nb = None
        self.clam = None
        self.kiex = None
        self.c1 = None

        # pfant
        self.fn_dissoc    = None
        self.fn_partit    = None
        self.fn_abonds    = None
        self.fn_atoms   = None
        self.fn_molecules = None
        self.fn_lines     = None
        self.fn_log       = None
        self.fn_progress = "progress.txt"
        self.flprefix = None
        self.molidxs_off = None

        # nulbad
        self.norm = None
        self.flam = None
        self.convol = None
        self.fwhm = None
        self.pat = None
        self.fn_flux = None
        self.fn_cv = None

    def get_names(self):
        """Returns a list with the names of all the options. Names come sorted"""
        return filter(lambda x: not (x.startswith('_') or x == "get_names"), dir(self))


@froze_it
class ExeConf(object):
    """
    Class holds all command-line options.

    Most options are initialized do None. This means that they won't be added to the
    command line.
    """

    def __init__(self):
        # **
        # ** Session control

        # Session id is used to make up filenames as needed.
        # This variable can be set directly
        self.session_id = None
        self._flag_first = True # first time calling _get_relative_path()
        self._session_dir = None

        # **
        # ** DataFile instances
        # ** If set, their corresponding fn_ attribute will be overwritten

        # FileMain instance
        # If set, main configuration file (of random name) will be created before
        # running the executable.
        self.fo_main = None
        # FileAbonds instance
        self.fo_abonds = None
        # FileAbonds instance
        self.fo_dissoc = None
        # FileHmap instance
        self.fo_hmap = None

        # Command-line options
        self.opt = Options()


    def get_wdir(self):
        """Gets working directory. Always returns a string."""
        wdir = self.opt.wdir if self.opt.wdir is not None else ""
        return wdir

    def make_session_id(self):
        """Finds an id for a new session and creates corresponding
        directory session_<id>.

        Directory must be created, otherwise two concurrent threads may grab
        the same id.
        """

        # assert self.session_id is None, "Session id already made"

        if self.session_id is not None:
            return


        PREFIX = "session-"
        self.session_id = _make_session_id(self.get_wdir(), PREFIX)
        self._session_dir = PREFIX+self.session_id

    def full_path_w(self, fn):
        """Joins self.opt.wdir with specified filename.

        There is a function of same name doing the same in config.f90
        """
        if self.opt.wdir is not None:
            return os.path.join(self.opt.wdir, fn)
        else:
            return fn

    def join_with_session_dir(self, fn):
        """
        Joins self._session_dir with specified filename.

        Atention: *wdir* is not part of the result.
        """
        if self._flag_first:
            assert self.session_id is not None, "Session id not assigned"
            self._flag_first = False
        return os.path.join(self._session_dir, fn)

    def clean(self):
        """Deletes directory with all files inside."""
        assert self._session_dir is not None, "Nothing to clean"
        shutil.rmtree(os.path.join(self.get_wdir(), self._session_dir))

    def get_args(self):
        """
        Returns a list of command-line arguments (only options that have been set)
        """

        l = []
        names = self.opt.get_names()
        for attr_name in names:
            value = self.opt.__getattribute__(attr_name)
            if value is not None:
                s_value = ("T" if value else "F") if isinstance(value, bool) else str(value)
                if re.search(r"[,|& ]", s_value):
                    s_value = '"'+s_value+'"'  # adds quotes if string contains one of the characters above
                l.extend(["--"+attr_name, s_value])
        return l

    def create_data_files(self):
        """
        Creates files for all self-attributes starting with prefix "fo_"

        Example:
          Consider the attribute fo_main:
            If self.fo_main is None:
              - File specified by self.fn_main must exist.
            else:
              - File of random name (e.g., main_23498.dat) will be generated
              - self.fn_main will be overwritten

        Now this example extends to fo_* attributes.
        """
        for attr_name in dir(self):
            if attr_name[0:3] == "fo_":
                obj = self.__getattribute__(attr_name)

                if obj is not None:
                    assert isinstance(obj, DataFile)

                    new_fn = os.path.join(self.get_wdir(),
                        self.join_with_session_dir(obj.default_filename))
                    # Saves file
                    obj.save_as(new_fn)
                    # Overwrites config option
                    self.opt.__setattr__("fn_"+attr_name[3:], new_fn)

    def prepare_filenames_for_combo(self, sequence):
        """
        Adds session dir to names of files that will be created by any of the
        executables. To be called *before* create_data_files

        Arguments:
          sequence: list containing one or more i_* values such as innewmarcs etc

        Restrictions:
        - flux prefix will be always "flux"
        """
        if innewmarcs in sequence:
            # ** innewmarcs -> hydro2
            # **            -> pfant
            # ** (infile:modeles) is innewmarcs output and (pfant, hydro2) input
            self.opt.fn_modeles = self.join_with_session_dir(self.opt.fn_modeles if self.opt.fn_modeles is not None else FileMod.default_filename)

        if hydro2 in sequence:
            # ** hydro2 -> pfant
            # ** Task here is to add session_dir to all hydrogen lines filenames, e.g.,
            # ** if it was "thalpha" it will be something like "session123456/thalpha"
            if not self.fo_hmap:
                # if self doesn't have a Hmap object, will load from file
                o = self.fo_hmap = FileHmap()
                fn = self.opt.fn_hmap if self.opt.fn_hmap is not None else \
                 FileHmap.default_filename
                o.load(self.full_path_w(fn))
            else:
                o = self.fo_hmap
            for row in o.rows:
                # directory/filename is put between single quotes.
                #
                # Fortran reads this correctly. If not put between quotes, Fortran thinks that the "/"
                # denotes the end of the string.
                row.fn = "'"+self.join_with_session_dir(row.fn)+"'"
                # Done! new hmap file will be created by create_data_files

        # ** pfant -> nulbad
        # ** Restriction: flux prefix will be always "flux"
        if pfant in sequence:
            self.opt.flprefix = self.join_with_session_dir("flux")  # this is for pfant
            self.opt.fn_progress = self.join_with_session_dir("progress.txt")  # this is for pfant

        if nulbad in sequence:
            # ** these two are options for nulbad
            self.opt.norm = True
            # Below it is assumed that pfant inserts ".norm" suffix (not a bad assumption))
            self.opt.fn_flux = self.join_with_session_dir("flux.norm")


# Part of code that finds new session id and creates corresponding directory
# This has been isolated because needs to be locked

_lock_session_id = Lock()
def _make_session_id(wdir, prefix):
    """Finds new session id (a string containing a four-digit integer)
    corresponding with a directory that does not yet exist
    named <prefix><session id>, and creates such directory.

    Returns the new session id

    This routine is thread-safe.
    """
    with _lock_session_id:
        i = 0
        while True:
            ret = "%d" % i
            new_dir = os.path.join(wdir, prefix+ret)
            if not os.path.isdir(new_dir):
                break
            i += 1
        os.mkdir(new_dir)
        return ret


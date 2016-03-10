"""
Class to store all command-line options & DataFile instances used to run one or more
executables.
"""

__all__ = ["Options", "Conf", "FOR_INNEWMARCS", "FOR_HYDRO2", "FOR_PFANT",
           "FOR_NULBAD", "session_prefix_singular"]

from pyfant.data import DataFile, FileHmap, FileMod, FileMain
import shutil
import os
from .misc import *
import re
from threading import Lock
import logging
import subprocess

# Indexes to use in Conf.sequence property
FOR_INNEWMARCS = 0
FOR_HYDRO2 = 1
FOR_PFANT = 2
FOR_NULBAD = 3

# Configurable
session_prefix_singular = "session-"
session_prefix_plural = "sessions-"


@froze_it
class Options(object):
    """Stores the command-line options.

    For each xxxx attribute not starting with "_" there exists
    a variable in config.f90 named config_xxxx, and
    a command-line option "--xxxx"

    """

    def __init__(self):
        # innewmarcs, hydro2, pfant, nulbad
        self.logging_level = None
        self.logging_screen = None
        self.logging_dump = None
        self.logging_fn_dump = None
        self.fn_main = None
        self.explain = None
        self.play = None

        # innewmarcs, hydro2, pfant
        self.fn_modeles = None

        # innewmarcs, hydro2
        self.teff = None
        self.glog = None
        self.asalog = None
        self.inum = None

        # hydro2, pfant
        self.fn_absoru2 = None
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
        self.vvt = None

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
        self.no_molecules = None
        self.no_atoms = None
        self.no_h = None
        self.zinf = None
        self.pas = None
        self.aint = None

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
class Conf(object):
    """
    Class holds the configuration of an executable.

    Most options are initialized do None. This means that they won't be added to the
    command line.
    """

    @property
    def session_id(self):
        """
        Information used to create a session directory and to identify this directory.

        When this property is set, a directory "session-<session_id>" is created
        immediately."""
        return self.__session_id

    @session_id.setter
    def session_id(self, x):
        assert self.__session_id is None, "Session id already set to \"%s\"" % self.__session_id
        self.__session_id = str(x)
        new_dir = session_prefix_singular+x
        # Will raise if directory exists: directory names must be unique.
        os.mkdir(new_dir)

    @property
    def session_dir(self):
        """Name made up with a prefix + (the session id)."""
        return self.__session_dir

    @property
    def popen_text_dest(self):
        """(Read-only) File-like object to act as destination to popen text messages."""
        return self.__popen_text_dest

    @property
    def flag_log_console(self):
        """Display Fortran messages in console (default=True)?"""
        return self.__flag_log_console

    @flag_log_console.setter
    def flag_log_console(self, x):
        self.__flag_log_console = x

    @property
    def flag_log_file(self):
        """Log Fortran messages to "<session dir>/fortran.log" (default=True)?"""
        return self.__flag_log_file

    @flag_log_file.setter
    def flag_log_file(self, x):
        self.__flag_log_file = x

    @property
    def flag_output_to_dir(self):
        """Tweak output filenames to be created inside the session directory
        (default=False)?"""
        return self.__flag_output_to_dir
    @flag_output_to_dir.setter
    def flag_output_to_dir(self, x):
        self.__flag_output_to_dir = x

    @property
    def flag_split_dirs(self):
        """Split sessions into several directories with 1000 sessions maximum each.

        This option was created to speed up directory access."""
        return self.__flag_split_dirs
    @flag_split_dirs.setter
    def flag_split_dirs(self, x):
        self.__flag_split_dirs = x

    @property
    def logger(self):
        if not self.__logger:
            self.__logger = get_python_logger()
        return self.__logger

    def __init__(self):
        # # Setup properties
        # ## Session control
        self.__session_id = None
        self.__session_dir = None
        # ## Setup flags
        self.__flag_log_console = True
        self.__flag_log_file = True
        self.__flag_output_to_dir = False
        self.__flag_split_dirs = False

        # # DataFile instances
        # See create_data_files() to see what happens when one or more
        # of these objects is set.
        # running the executable.
        # ## FileMain instance
        self.file_main = None
        # ## FileAbonds instance
        self.file_abonds = None
        # ## FileAbonds instance
        self.file_dissoc = None
        # ## FileHmap instance
        self.file_hmap = None
        # ## FileAtoms instance
        self.file_atoms = None

        # # Command-line options
        self.opt = Options()

        # # Read-only properties
        self.__popen_text_dest = None
        self.__logger = None

    def configure(self, sequence):
        """Series of configuration actions to take before Runnable can run."""
        self.__make_session_id()
        if self.__flag_output_to_dir:
            self.__rename_outputs(sequence)

        self.__logger = get_python_logger()

        if self.__flag_log_file:
            log_path = self.join_with_session_dir("fortran.log")
            if self.__flag_log_console:
                stdout_ = LogTwo(log_path)
            else:
                stdout_ = open(log_path, "w")
        else:
            if self.__flag_log_console:
                stdout_ = subprocess.STD_OUTPUT_HANDLE
            else:
                stdout_ = None
        self.__popen_text_dest = stdout_

        if FOR_PFANT in sequence:
            self.opt.fn_progress = self.join_with_session_dir("progress.txt")  # this is for pfant

        self.__create_data_files()

    def close_popen_text_dest(self):
        """Closes self.popen_text_dest opened in configure().

        To be called after the Runnable runs."""
        if self.__popen_text_dest is not None:
            self.__popen_text_dest.close()

    def get_file_main(self):
        """Returns either self.file_main, or if None, tries to open file and
        return a new FileMain object.
        """
        if self.file_main is not None:
            return self.file_main
        file_ = FileMain()
        if self.opt.fn_main is None:
            file_.load()  # will try to load default file
        else:
            file_.load(self.opt.fn_main)
        return file_

    def get_flprefix(self, _flag_skip_opt=False):
        """Returns the prefix for a pfant output file.

        The returned prefix is used to compose a pfant output file name, e.g.
        <prefix>.norm

        Prefix is looked for in the following locations (in this order):
          1) command-line option, i.e., self.opt.flprefix; if None,
          2) self.file_main.flprefix; if self.file_main is None,
          3) tries to open the main configuration file
        """
        if not _flag_skip_opt and self.opt.flprefix is not None:
            return self.opt.flprefix
        if self.file_main is not None:
            return self.file_main.flprefix
        file_ = self.get_file_main()
        return file_.flprefix

    def get_pfant_output_filepath(self, type_="norm"):
        """Returns path to a pfant output filename.

        Arguments:
          type -- "spec", "norm", or "cont"

        Looks for this information in several places; see get_flprefix()
        for more information.
        """
        valid_types = ("spec", "norm", "cont")
        assert type_ in valid_types, "type must be in %s" % (valid_types,)
        return self.get_flprefix()+"."+type_

    def get_nulbad_output_filepath(self):
        """Returns path to nulbad output filename.

        Reproduces nulbad logic in determining its output filename, i.e.,
          1) uses --fn_cv if present; if not,
          2) gets flprefix from main configuration file and adds
             ".norm" or ".spec"
        """
        if self.opt.fn_cv is not None:
            filename = self.opt.fn_cv
        else:
            flprefix = self.get_flprefix()
            # True or None evaluates to "norm"
            ext = "spec" if self.opt.norm == False else "norm"
            filename = flprefix+"."+ext
        return filename

    def get_fn_modeles(self):
        """Returns name of atmospheric model file."""
        return FileMod.default_filename if self.opt.fn_modeles is None \
         else self.opt.fn_modeles

    def join_with_session_dir(self, fn):
        """Joins self.session_dir with specified filename to make a path."""
        return os.path.join(self.session_dir, fn)

    def clean(self, flag_remove_dir=True):
        """Deletes directory with all files inside.

        Arguments:
          flag_remove_dif=True -- if not set, will only delete the contents,
            keeping the directory
        """

        if flag_remove_dir:
            logging.debug("About to remove directory '%s'" % self.session_dir)
            shutil.rmtree(self.session_dir)
        else:
            # reference: http://stackoverflow.com/questions/185936/delete-folder-contents-in-python
            for the_file in os.listdir(self.session_dir):
                file_path = os.path.join(self.session_dir, the_file)
                try:
                    if os.path.isfile(file_path):
                        os.unlink(file_path)
                except:
                    self.logger.exception("Error removing '%s'" % the_file)

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

    def __create_data_files(self):
        """
        Creates files for all self-attributes starting with prefix "file_"

        Example:
          Consider the attribute file_main:
            If self.file_main is None:
              - File specified by self.fn_main must exist.
            else:
              - File will be created inside session directory
              - self.opt.fn_main will be overwritten

        Now this example extends to file_* attributes.
        """
        for attr_name in dir(self):
            if attr_name.startswith("file_"):
                obj = self.__getattribute__(attr_name)

                if obj is not None:
                    assert isinstance(obj, DataFile)

                    new_fn = self.join_with_session_dir(obj.default_filename)
                    # Saves file
                    obj.save_as(new_fn)
                    # Overwrites config option
                    self.opt.__setattr__("fn_"+attr_name[5:], new_fn)

    def __rename_outputs(self, sequence):
        """
        Adds session dir to names of files that will be created by any of the
        executables. To be called *before* create_data_files

        Arguments:
          sequence: list containing one or more i_* values such as innewmarcs etc

        Note: in order to link pfant->nulbad correctly,
              nulbad "--fn_flux" option will not be used.
        """
        if FOR_INNEWMARCS in sequence:
            # ** innewmarcs -> hydro2
            # **            -> pfant
            # ** (infile:modeles) is innewmarcs output and (pfant, hydro2) input
            self.opt.fn_modeles = self.join_with_session_dir(self.opt.fn_modeles if self.opt.fn_modeles is not None else FileMod.default_filename)

        if FOR_HYDRO2 in sequence:
            # ** hydro2 -> pfant
            # ** Task here is to add session_dir to all hydrogen lines filenames, e.g.,
            # ** if it was "thalpha" it will be something like "session123456/thalpha"
            if not self.file_hmap:
                # if self doesn't have a Hmap object, will load from file
                o = self.file_hmap = FileHmap()
                fn = self.opt.fn_hmap if self.opt.fn_hmap is not None else \
                 FileHmap.default_filename
                o.load(fn)
            else:
                o = self.file_hmap
            for row in o.rows:
                # directory/filename is put between single quotes.
                #
                # Fortran reads this correctly. If not put between quotes, Fortran thinks that the "/"
                # denotes the end of the string.
                row.fn = "'"+self.join_with_session_dir(row.fn)+"'"
                # Done! new hmap file will be created by create_data_files

        # ** pfant -> nulbad
        if FOR_PFANT in sequence or FOR_NULBAD in sequence:
            flprefix = self.get_flprefix(True)
            self.opt.flprefix = self.join_with_session_dir(flprefix)

        if FOR_NULBAD in sequence:
            self.opt.fn_flux = None  # will cause nulbad to use flprefix
            if self.opt.fn_cv:
                # Note that this is recursive, but Combo is meant to be
                # run only once.
                self.opt.fn_cv = self.join_with_session_dir(self.opt.fn_cv)

    def __make_session_id(self):
        """Finds an id for a new session and creates corresponding
        directory.

        Directory must be created, otherwise two concurrent threads may grab
        the same id.
        """

        # assert self.session_id is None, "Session id already made"

        if self.__session_id is not None:
            return
        self.__session_id, self.__session_dir = _IdMaker.make_session_id(self.__flag_split_dirs)


# # Section id managhement
# Part of code that finds new session id and creates corresponding directory



class _IdMaker(object):
    """
    Class to hold the thread-safe id maker + directory maker functionality.

    This class is not supposed to be instantiated.
    """

    # Lock is necessary to make unique session ids
    lock_session_id = Lock()
    i_id = 0
    dirs_per_dir = 1000  # only if flag_split_dirs

    @classmethod
    def make_session_id(cls, flag_split_dirs):
        """Makes session id and creates corresponding directory.

        This routine is thread-safe.
        """
        with cls.lock_session_id:
            while True:
                new_dir = cls.get_session_dirname(flag_split_dirs)
                cls.i_id += 1
                if not os.path.isdir(new_dir):
                    new_id = "%d" % cls.i_id
                    break
            os.makedirs(new_dir)
            return new_id, new_dir


    @classmethod
    def get_session_dirname(cls, flag_split_dirs):
        """Returns string which is the name of a filesystem directory."""
        if not flag_split_dirs:
            return session_prefix_singular+str(cls.i_id)
        d0 = session_prefix_plural+str(int(cls.i_id//cls.dirs_per_dir)*cls.dirs_per_dir)
        d1 = session_prefix_singular+str(cls.i_id)
        return os.path.join(d0, d1)

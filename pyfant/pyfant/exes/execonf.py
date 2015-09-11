"""
Class to store all command-line options & DataFile instances used to run one or more
executables.
"""

__all__ = ["ExeConf", "i_innewmarcs", "i_hydro2", "i_pfant", "i_nulbad"]

from pyfant.data import DataFile, FileHmap, FileMod
import shutil
import os
import random
import glob

# Indexes in workflow sequence
i_innewmarcs = 0
i_hydro2 = 1
i_pfant = 2
i_nulbad = 3

class ExeConf(object):
  """
  Class holds all command-line options.

  Most options are initialized do None. This means that they won't be added to the
  command line.
  """

  # Used in run_4exes.py
  # Futurely for GUIs
  # todo parse config.f90 to get the list automatically
  option_map = [
  "wdir",
  "logging_level",
  "logging_screen",
  "logging_dump",
  "logging_fn_dump",
  "fn_main",
  "fn_modeles",
  "teff",
  "glog",
  "asalog",
  "inum",
  "fn_absoru2",
  "hmap",
  "fn_hmap",
  "llzero",
  "llfin",
  "open_status",
  "fn_moddat",
  "fn_gridslist",
  "modcode",
  "tirb",
  "zph",
  "ptdisk",
  "kik",
  "amores",
  "kq",
  "nomplot",
  "vvt",
  "na",
  "nb",
  "clam",
  "kiex",
  "c1",
  "fn_dissoc",
  "fn_partit",
  "fn_abonds",
  "fn_atoms",
  "fn_molecules",
  "fn_lines",
  "fn_log",
  "fn_progress",
  "flprefix",
  "norm",
  "flam",
  "convol",
  "fwhm",
  "pat",
  "fn_flux",
  "fn_cv"
  ]


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

    # **
    # ** Command-line options
    # **
    # ** For each xxxx attribute there exists
    # ** a variable in config.f90 named config_xxxx, and
    # ** a command-line option "--xxxx"

    # innewmarcs, hydro2, pfant, nulbad
    self.opt_wdir = None
    self.opt_logging_level = None
    self.opt_logging_screen = None
    self.opt_logging_dump = None
    self.opt_logging_fn_dump = None
    self.opt_fn_main = None

    # innewmarcs, hydro2, pfant
    self.opt_fn_modeles = None

    # innewmarcs, hydro2
    self.opt_teff = None
    self.opt_glog = None
    self.opt_asalog = None
    self.opt_inum = None

    # hydro2, pfant
    self.opt_fn_absoru2 = None
    self.opt_hmap = True # Overriding default, which is .false.
    self.opt_fn_hmap = None
    self.opt_llzero = None
    self.opt_llfin = None

    # innewmarcs
    self.opt_open_status = None
    self.opt_fn_moddat = None
    self.opt_fn_gridslist = None
    self.opt_modcode = None
    self.opt_tirb = None

    # hydro2
    self.opt_zph = None
    self.opt_ptdisk = None
    self.opt_kik = None
    self.opt_amores = None
    self.opt_kq = None
    self.opt_nomplot = None
    self.opt_vvt = None
    self.opt_na = None
    self.opt_nb = None
    self.opt_clam = None
    self.opt_kiex = None
    self.opt_c1 = None

    # pfant
    self.opt_fn_dissoc    = None
    self.opt_fn_partit    = None
    self.opt_fn_abonds    = None
    self.opt_fn_atoms   = None
    self.opt_fn_molecules = None
    self.opt_fn_lines     = None
    self.opt_fn_log       = None
    self.opt_fn_progress = "progress.txt"
    self.opt_flprefix = None
    self.opt_molids_off = None

    # nulbad
    self.opt_norm = None
    self.opt_flam = None
    self.opt_convol = None
    self.opt_fwhm = None
    self.opt_pat = None
    self.opt_fn_flux = None
    self.opt_fn_cv = None

  def get_wdir(self):
    """Gets working directory. Always returns a string."""
    wdir = self.opt_wdir if self.opt_wdir is not None else ""
    return wdir

  def make_session_id(self):
    """Makes new session id."""

    # assert self.session_id is None, "Session id already made"

    if self.session_id is not None:
        return

    # Finds a 6-digit integer that is not part of any file in directory.
    w = self.get_wdir()
    mask = os.path.join(w, "*")
    i = 0
    while True:
      #s = "%04d" % random.randint(0, 999999)
      s = "%04d" % i
      ff = glob.glob(mask)
      flag_exit = True
      for f in ff:
        if s in f:
          flag_exit = False
          break
      if flag_exit:
        break
      i += 1

    self.session_id = s

  def full_path_w(self, fn):
    """Joins self.opt_wdir with specified filename.

    There is a function of same name doing the same in config.f90
    """
    if self.opt_wdir is not None:
      return os.path.join(self.opt_wdir, fn)
    else:
      return fn

  def add_session_dir(self, fn):
    """
    Joins self._session_dir with specified filename.

    If first time called, creates the session directory.

    Atention: *wdir* is not part of the result.
    """

    if self._flag_first:
      assert self.session_id is not None, "Session id not assigned"
      d = self._session_dir = "session_"+self.session_id
      os.mkdir(os.path.join(self.get_wdir(), d))
      self._flag_first = False
    return os.path.join(self._session_dir, fn)

  def clean(self):
    """Deletes directory with all files inside."""
    assert self._session_dir is not None, "Nothing to clean"
    shutil.rmtree(os.path.join(self.get_wdir(), self._session_dir))

  def get_args(self):
    """
    Returns a list of command-line arguments by instrospection.

    Looks for self-attributes starting with "opt_" and creates strings for
    those which are not None.
    """

    l = []
    for attr_name in dir(self):
      if attr_name.startswith("opt_"):
        value = self.__getattribute__(attr_name)
        if value is not None:
          s_value = ("T" if value else "F") if isinstance(value, bool) else str(value)
          l.extend(["--"+attr_name[4:], s_value])
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

          new_fn = os.path.join(self.get_wdir(), self.add_session_dir(obj.default_filename))
          # Saves file
          obj.save(new_fn)
          # Overwrites config option
          self.__setattr__("opt_fn_"+attr_name[3:], new_fn)

  def prepare_filenames_for_combo(self, sequence):
    """
    Adds session dir to names of files that will be created by any of the
    executables. To be called *before* create_data_files

    Arguments:
      sequence: list containing one or more i_* values such as i_innewmarcs etc

    Restrictions:
    - flux prefix will be always "flux"

    """


    if i_innewmarcs in sequence:
        # ** innewmarcs -> hydro2
        # **            -> pfant
        # ** (infile:modeles) is innewmarcs output and (pfant, hydro2) input
        self.opt_fn_modeles = self.add_session_dir(self.opt_fn_modeles if self.opt_fn_modeles is not None else FileMod.default_filename)

    if i_hydro2 in sequence:
        # ** hydro2 -> pfant
        # ** Task here is to add session_dir to all hydrogen lines filenames, e.g.,
        # ** if it was "thalpha" it will be something like "session123456/thalpha"
        if not self.fo_hmap:
          # if self doesn't have a Hmap object, will load from file
          o = self.fo_hmap = FileHmap()
          fn = self.opt_fn_hmap if self.opt_fn_hmap is not None else FileHmap.default_filename
          o.load(self.full_path_w(fn))
        else:
          o = self.fo_hmap
        for row in o.rows:
          # directory/filename is put between single quotes.
          #
          # Fortran reads this correctly. If not put between quotes, Fortran thinks that the "/"
          # denotes the end of the string.
          row.fn = "'"+self.add_session_dir(row.fn)+"'"
        # Done! new hmap file will be created by create_data_files

    # ** pfant -> nulbad
    # ** Restriction: flux prefix will be always "flux"
    if i_pfant in sequence:
        self.opt_flprefix = self.add_session_dir("flux")  # this is for pfant
        self.opt_fn_progress = self.add_session_dir("progress.txt")  # this is for pfant


    if i_nulbad in sequence:
        # ** these two are options for nulbad
        self.opt_norm = True
        # Below it is assumed that pfant inserts ".norm" suffix (not a bad assumption))
        self.opt_fn_flux = self.add_session_dir("flux.norm")


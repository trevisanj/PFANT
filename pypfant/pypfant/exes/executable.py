__all__ = ["Executable"]

from pypfant import *

class Executable(object):
  """
  Generic class to represent an executable.
  """

  def __init__(self):
    # Full path to executable (including executable name)
    self.exe_path = "./none"


    # Command-line options present in all executables
    self.opt_inputdir = None
    self.opt_outputdir = None

    self.stdout = None

    self.popen = None


  def is_running(self):
    if self.popen is None:
        return False
    self.popen.poll()
    return self.popen.returncode is None


  def _create_data(self):
    """
    Creates files for all object attributes starting with prefix "fo_"

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

          # Makes filename, e.g., abonds32732.dat
          # -----
          # Uses default name as a base for name of file that doesn't exist
          name, ext = obj.default_filename.split('.')
          new_fn = new_filename("auto_"+name, ext)
          fullpath = self._get_fullpath_i(new_fn)
          # Saves file
          obj.save(fullpath)
          # Overwrites config option
          self.__setattr__("opt_fn_"+attr_name[3:], new_fn)

    def get_opt_args(self):
      """
      Returns a list of command-line arguments.

      Sweeps all object attributes.
        - If attribute starts with "opt_", it is an option requiring an argument
        - If attribute starts with "optf_", it is a flag option without argument

      Example:
        Suppose that a class descending from Executable has the following
        attributes and values:
          - self.opt_fn_main = "main1234.dat"
          - self.optf_thmap = True

        In such case, this method will return
        "--fn_main", "main1234.dat", "--thmap"]

        If self.optf_thmap were False, "--thmap" would be absent from the result.
      """

      l = []
      for attr_name in dir(self):
        if attr_name[0:4] == "opt_":
          value = self.__getattribute__(attr_name)
          if value is not None:
            s_value = ("T" if value else "F") if isinstance(value, bool) else str(value)
            l.extend([attr_name[4:], s_value])
        elif attr_name[0:5] == "optf_":
          value = self.__getattribute__(attr_name)
          if value:
            l.extend([attr_name[5:]])
      return l


    def run(self):
      """Blocking routine. Overwrites self.popen."""

      assert not self.is_running(), "Already running"

      self._run

        self._create_data
        map0 = [(self.o_main, 'fn_main'),
                (self.o_abonds, 'fn_abonds'),
                ]

        for obj, attr_name in map0:
            if obj is not None:
                assert isinstance(obj, DataFile)
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
        if self.exe_path is None:
            raise RuntimeError("Must set path to executable (exe_path)")

        cmd_line = [self.exe_path, "--fn_progress", self._fn_progress]

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
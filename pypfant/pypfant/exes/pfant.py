# better to run with subprocess POpen, will be able to terminate
# tired

"""
Wraps over the PFANT executable
"""

__all__ = ["Pfant"]

from .executable import Executable

import os
import os.path
import logging

logger = logging.getLogger("pfant")

class Pfant(Executable):
  def __init__(self):
    Executable.__init__(self)
    
    self.exe_path = "./pfant"  # Path to PFANT executable (including executable name)

    # -----
    # DataFile objects: o_*
    #
    # How this works:
    #  If self.o_main is None:
    #  File specified by self.fn_main must exist.
    #  else:
    #  - File of random name (e.g., main_23498.dat) will be generated
    #  - self.fn_main will be overwritten
    #
    # The above was an example, now replicate the above for all self.o_*.

    # FileAbonds instance
    self.fo_abonds = None

    self.opt_fn_dissoc    = None
    self.opt_fn_partit    = None
    self.opt_fn_absoru2     = None
    self.opt_fn_modeles     = None
    self.opt_fn_abonds    = None
    self.opt_fn_atomgrade   = None
    self.opt_fn_moleculagrade = None
    self.opt_fn_lines     = None
    self.opt_fn_log       = None
    self.opt_fn_progress = "progress.txt"
    self.optf_hmap = True  # Overriding pfant default, which is .false.

    ## Variables assigned by poll_progress()
    self.ikey = None  # Current iteration (Fortran: ikey)
    self.ikeytot = None  # Current iteration (Fortran: ikeytot)


  def poll_progress(self):
    """
    Tries to open progress indicator file. If succeeds, stores
    current iteration in attribute ikey, and
    number of iterations in attribute ikeytot.

    """
    p = self.get_fullpath_o(self.opt_fn_progress)
    if os.path.isfile(p):
      with open(p) as h:
        self.ikey, self.ikeytot = map(int, h.readline().split("/"))
    else:
      self.ikey, self.ikeytot = None, None

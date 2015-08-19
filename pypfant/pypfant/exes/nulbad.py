__all__ = ["Nulbad"]

from .executable import Executable

import os
import os.path
import logging

logger = logging.getLogger("nulbad")

class Nulbad(Executable):
  """Class representing the nulbad executable."""
  
  def __init__(self):
    Executable.__init__(self)

    self.exe_path = "./nulbad"

    self.opt_norm = None
    self.opt_flam = None
    self.opt_convol = None
    self.opt_fwhm = None
    self.opt_pat = None
    self.opt_fn_flux = None
    self.opt_fn_cv = None
__all__ = ["Innewmarcs"]

from .executable import Executable

import os
import os.path
import logging

logger = logging.getLogger("innewmarcs")

class Innewmarcs(Executable):
  """Class representing the innewmarcs executable."""

  def __init__(self):
    Executable.__init__(self)

    self.exe_path = "./innewmarcs"

    self.opt_fn_modeles = None
    self.opt_fn_moddat = None
    self.opt_fn_gridslist = None
    self.opt_modcode = None
    self.opt_tirb = None
    self.opt_teff = None
    self.opt_glog = None
    self.opt_asalog = None
    self.opt_inum = None

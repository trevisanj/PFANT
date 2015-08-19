__all__ = ["Hydro2"]

from .executable import Executable
import logging

logger = logging.getLogger("hydro2")

class Hydro2(Executable):
  """Class representing the hydro2 executable."""

  def __init__(self):
    Executable.__init__(self)

    self.exe_path = "./hydro2"

    self.optf_hmap = True # Overriding hydro2 default, which is .false.
    self.opt_zph = None
    self.opt_teff = None
    self.opt_glog = None
    self.opt_asalog = None
    self.opt_inum = None
    self.opt_ptdisk = None
    self.opt_kik = None
    self.opt_amores = None
    self.opt_kq = None
    self.opt_nomplot = None
    self.opt_vvt = None
    self.opt_fn_absoru2 = None
    self.opt_fn_modeles = None
    self.opt_fn_hmap = None
    self.opt_na = None
    self.opt_nb = None
    self.opt_clam = None
    self.opt_kiex = None
    self.opt_c1 = None

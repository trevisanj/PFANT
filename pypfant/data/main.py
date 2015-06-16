__all__ = ["main"]

import struct
from .input_structure import *

from misc import *


class main(input_structure):
    default_filename = "main.dat"

    def __init__(self):
        self.titrav = None

    main_pas, \
    main_echx, \
    main_echy, \
    main_fwhm

    main_vvt

    ! rows 03.(1-3): (three conditional rows that MUST exist if and only if main_VVT(1) > 900) ?doc?
    if(main_vvt(1) .gt. 900)  then   ! vt variable avec la profondeur
      read(unit_, *) main_ivtot
      ! ivtot, affects subroutine turbul() issue ?what? ?doc?
      if (main_ivtot .gt. MAX_MODELES_NTOT) then
        write (lll, *) 'main_ivtot .gt. MAX_MODELES_NTOT (', &
         main_ivtot, ' .gt. ', MAX_MODELES_NTOT, ')'
         call pfant_halt(lll)
      end if

      read(unit_,*) (main_tolv(i), i=1, main_ivtot)
      read(unit_,*) (main_vvt(i) ,i=1, main_ivtot)
    end if

    ! row 04
    read(unit_, *) main_teff, main_glog, main_asalog, main_nhe,main_inum

    ! row 05
    read(unit_, *) main_ptdisk, main_mu

    ! row 06
    read(unit_, *) main_afstar  ! metallicity of the star (in log scale)

    ! row 07: XXCOR(i)
    ! @todo ISSUE: Should be a column in dissoc.dat !!!!!
    ! (MT) I agree
    read(unit_, *)(main_xxcor(i), i=1, dissoc_nmetal)

    ! row 08 -- part of a filename
    ! This line will define the names of three output files:
    !   FILEFLUX.cont
    !   FILEFLUX.norm
    !   FILEFLUX.spec
    read(unit_, '(a)') main_fileflux

    ! row 09
    read(unit_, *) main_llzero, main_llfin, main_aint

    write(*,*) main_llzero, main_llfin, main_aint

    ! row 10 - ....
    ! Considers the remaining rows as the filetoh file names
    ! Doesn't know yet the number of files
    ih = 1
    110 continue
    read(unit_, '(a)', end=111) filetoh_temp

    if (len_trim(filetoh_temp) .eq. 0) goto 110  ! skips blank rows

    !__spill check__
    if (ih .gt. MAX_MAIN_FILETOH_NUMFILES) then
      call pfant_halt('Too many filetoh files specified (maximum is '//&
       'MAX_MAIN_FILETOH_NUMFILES='//int2str(MAX_MAIN_FILETOH_NUMFILES)//')')
    end if

    main_filetohy(ih) = filetoh_temp

    write(lll,*) 'filetohy(', ih, ') = "', trim(main_filetohy(ih)), '"'
    call log_debug(lll)

    ih = ih+1
    goto 110

    111 continue
    main_filetoh_numfiles = ih-1

    write(lll,*) 'Number of filetoh files: ', main_filetoh_numfiles

























    def __len__(self):
        """Returns length of "ele" attribute."""
        return len(self.ele)

    def load(self, filename):
        """Clears internal lists and loads from file."""




        self.abol, self.ele = [], []

        ostr = struct.Struct("1x 2s 6s")
        with open(filename, "r") as h:
            for s in h:
                if len(s) > 0:
                    if s[0] == "1":  # sign to stop reading file
                        break
                [ele, abol] = ostr.unpack_from(s)
                self.ele.append(ele)
                self.abol.append(float(abol))

    def save(self, filename):
        """Saves current lists to file."""
        with open(filename, "w") as h:
            h.writelines([' %-2s%6.2f\n' % (self.ele[i], self.abol[i])
                          for i in xrange(len(self))])
            h.writelines(['1\n', '1\n'])

    def init_default(self):
        """Initializes with default abundances (which are hard-coded)."""
        fullpath = path_to_default("main.dat")
        self.load(fullpath)

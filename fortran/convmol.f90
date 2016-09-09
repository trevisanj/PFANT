! This file is part of PFANT.
!
! PFANT is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! PFANT is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with PFANT.  If not, see <http://www.gnu.org/licenses/>.





!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! PFANT-to-VALD3 molecular lines file conversion
!
! Creates file specified by option --fn_out

program convmol
  use pfantlib
  implicit none

  !=====
  ! Startup
  !=====
  execonf_name = 'convmol'
  call molecules_idxs_init()
  call config_init()

  call read_molecules(config_fn_molecules)

  write (*, *) 'BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE'

end program convmol

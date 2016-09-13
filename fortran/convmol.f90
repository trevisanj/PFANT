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
  real*8 x_llzero, x_llfin
  logical :: main_exists  ! whether or not main configuration file exists


  !=====
  ! Startup
  !=====
  execonf_name = 'convmol'
  call molecules_idxs_init()
  call config_init()

  inquire(file=config_fn_main, exist=main_exists)
  if (config_fn_out .eq. '?') then
    config_fn_out = trim(config_fn_molecules)//'.vald'
    call parse_aux_log_assignment('config_fn_out', config_fn_out)
  end if

  !
  ! Assigns x_*
  !
  ! values in config_* variables have preference, but if they are uninitialized, will
  ! pick values from *main file*
  x_llzero = config_llzero
  x_llfin = config_llfin
  if (config_llzero .eq. -1) then
    if(.not. main_exists) &
      call log_and_halt('--llzero option not set and '''//&
       trim(config_fn_main)//''' does not exist')
    call assure_read_main(config_fn_main)
    x_llzero = main_llzero
    call parse_aux_log_assignment('x_llzero', real82str(x_llzero, 4))
  end if
  if (config_llfin .eq. -1) then
    if(.not. main_exists) &
      call log_and_halt('--llfin option not set and '''//&
       trim(config_fn_main)//''' does not exist')
    call assure_read_main(config_fn_main)
    x_llfin = main_llfin
    call parse_aux_log_assignment('x_llfin', real82str(x_llfin, 4))
  end if

  call log_info('Reading file '''//trim(config_fn_molecules)//'''...')
  call read_molecules(config_fn_molecules)
  call log_info('Filtering to specified wavelength region...')
  call filter_molecules(x_llzero, x_llfin)

  write (*, *) 'BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE'

end program convmol

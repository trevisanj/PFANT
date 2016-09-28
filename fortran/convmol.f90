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
  integer :: myunit, l


  !=====
  ! Startup
  !=====
  execonf_name = 'convmol'
  call molecules_idxs_init()
  call config_init()

  inquire(file=config_fn_main, exist=main_exists)
  if (config_fn_out .eq. '?') then
    config_fn_out = trim(config_fn_molecules)//'.plez'
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


  call log_info('Writing to file '''//trim(config_fn_out)//'''...')



! # Plez specs
!
!   lambda_air(A)   gf         Elow(cm-1)  vl  Jl    Nl  syml  Eup(cm-1) vu   Ju    Nu  symu  gamrad    mol trans branch
!     3164.8716 0.769183E-11   3459.6228   0   5.0   5.0  0  35047.3396  15   6.0   6.0  0 1.821557E+07 'TiO a f  R    '
! 1234567890123 123456789012 12345678901 123 12345 12345 12 12345678901 123 12345 12345 12 123456789012 1234567890123456
!
! # VALD3 sample
!
! Elm Ion      WL_air(A)   log gf* E_low(eV) J lo  E_up(eV) J up  lower  upper   mean   Rad.  Stark  Waals
! 'Fe 1',       16400.045,  -4.131,  6.5922,  6.0,  7.3480,  5.0, 1.320, 1.090, 1.910, 8.140,-3.840,-7.330,
! '  LS                                                                       3d7.(4F).4d f5G'
! '  JK                                                             3d7.(4F<7/2>).4f 2[11/2]*'


  open(newunit=myunit, file=config_fn_out, status='replace', err=10)

  write(myunit, '(a)') &
   '  lambda_air(A)   gf         Elow(cm-1)  vl  Jl    Nl  syml  Eup(cm-1) vu   Ju    Nu  symu  gamrad    mol trans branch'

  do l = 1, km_f_mblend
    write(myunit, '(f13.4, 1x, e12.6, 1x, f11.4, 1x, i3, 1x, f5.1, 1x, f5.1, 1x, i2, '//&  ! lambda_air(A), ...
                  '1x, f11.4, 1x, i3, 1x, f5.1, 1x, f5.1, 1x, i2, 1x, e12.6, 1x, a16)') &  ! Eup(cm-1), ...
     km_f_lmbdam(l), &  ! lambda_air(A)
     km_c_gfm(l), &  ! gf
     3., &  ! Elow(cm-1)
     4, &  ! vl
     5., &  ! Jl
     6., &  ! Nl
     7, &  ! syml
     8., &  ! Eup(cm-1)
     9, &  ! vu
     10., &  ! Ju
     11., &  ! Nu
     12, &  ! symu
     13., &  ! gamrad
     '''---'''  ! mol trans branch
  end do
  close(myunit)
  goto 11

  10 continue
  call log_info('Could not create output file')

  11 continue

  write (*, *) 'BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE * BYE'
end program convmol

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
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Declaration and initialization of x_* variables
!
! This module deals with the variables whose values may come either from *main file* or
! command-line arguments (the latter has precedence).
!
! Prefixes:
!   - x_ -- these variable values may come either from *main file* or command-line options.

module pfant_x
  use pfantlib
  implicit none

  character*128 :: x_flprefix
  real*8 :: x_llzero, x_llfin, x_pas, x_aint
contains

  ! Initializes x_* variables
  !
  ! Note: to be called after read_main()

  subroutine pfant_init_x()
    if (config_flprefix .eq. '?') then
      x_flprefix = main_flprefix
      call parse_aux_log_assignment('x_flprefix', x_flprefix)
    else
      x_flprefix = config_flprefix
    end if
    if (config_llzero .eq. -1) then
      x_llzero = main_llzero
      call parse_aux_log_assignment('x_llzero', real82str(x_llzero, 2))
    else
      x_llzero = config_llzero
    end if
    if (config_llfin .eq. -1) then
      x_llfin = main_llfin
      call parse_aux_log_assignment('x_llfin', real82str(x_llfin, 2))
    else
      x_llfin = config_llfin
    end if
    if (config_pas .eq. -1) then
      x_pas = main_pas
      call parse_aux_log_assignment('x_pas', real82str(x_pas, 2))
    else
      x_pas = config_pas
    end if
    if (config_aint .eq. -1) then
      x_aint = main_aint
      call parse_aux_log_assignment('x_aint', real82str(x_aint, 0))
    else
      x_aint = config_aint
    end if
  end
end





!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! PFANT main executable: spectral synthesis
!
! Creates three files such as (flux.spec, flux.cont, flux.norm).
! These files can be inputted into nulbad
!

program pfant
  use pfantlib
!@  use synthesis
  use pfant_x
!@  implicit none
!@  integer i
!@  logical dissoc_exists
!@  real*8 temp

  print *, 'Hiiiiiiiiiiiiiiiiiiiiiiieeeeeeeeeeeeeeeeei'

  !=====
  ! Startup
  !=====
  execonf_name = 'pfant'
  call config_init()

  !=====
  ! File reading
  !=====
!@  call read_main(config_fn_main)

  !---
  ! (intermission)
  ! After reading *main file*, initializes variables whose values may come
  ! either from *main file* or command-line option
  !---
!@  call pfant_init_x()

  ! x_aint has to be divisible by x_pas
!@  temp = x_aint/x_pas
!@  if (abs(temp-nint(temp)) .gt. 1.e-10) then
!@    call log_and_halt('pas='//real82str(x_pas, 5)//' (delta-lambda) must be a '//&
!@     'sub-multiple of aint='//real82str(x_aint, 1)//' (they are set in '//&
!@     'main configuration file, or command-line options "--aint" and "--pas").')
!@  end if


  ! continues file reading
!@  call read_abonds(config_fn_abonds)
!@  inquire(file=config_fn_dissoc, exist=dissoc_exists)
!@  if (dissoc_exists) then
!@    call read_dissoc(config_fn_dissoc)
!@  else
!@    call log_warning('File "'//trim(config_fn_dissoc)//'" not found: will take '//&
!@     'internally stored template and replace abundances with those in "'//trim(config_fn_abonds)//'" -12')
!@    call auto_dissoc()
!@  end if

!@  call read_partit(config_fn_partit)  ! LECTURE DES FCTS DE PARTITION
!@  call read_absoru2(config_fn_absoru2)  ! LECTURE DES DONNEES ABSORPTION CONTINUE
!@  call read_modele(config_fn_modeles)  ! LECTURE DU MODELE
!@  if (config_opa) call read_opa(config_fn_opa)
!@  if (.not. config_no_atoms) then
!@    call read_atoms(config_fn_atoms)
!@    if (config_zinf .ne. -1) then
!@      do i = 1, atoms_nblend
!@        atoms_zinf(i) = config_zinf
!@      end do
!@    end if
!@  end if
!@  if (.not. config_no_h) then
!@    call read_hmap(config_fn_hmap)
!@    call read_filetoh(x_llzero, x_llfin)
!@  end if
!@  if (.not. config_no_molecules) call read_molecules(config_fn_molecules)

!@  if (abs(modele%asalog-main_afstar) > 0.01) then
!@    call log_and_halt('asalog from model ('//real82str(modele%asalog, 2)//&
!@     ') does not match afstar in main configuration file ('//real82str(main_afstar, 2)//')')
!@  end if


  !=====
  ! Spectral synthesis
  !=====
  ! Does the calculus
!@  call synthesis_()
end program pfant


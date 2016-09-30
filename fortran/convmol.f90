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
  integer :: myunit, l, molidx_last, molidx, num_species
  logical :: dissoc_exists
  character*6 :: mol_name, mol_names(MAX_NUM_MOL)


  ! # Startup
  execonf_name = 'convmol'
  call config_init()

  inquire(file=config_fn_main, exist=main_exists)
  if (config_fn_out .eq. '?') then
    config_fn_out = trim(config_fn_molecules)//'.plez'
    call parse_aux_log_assignment('config_fn_out', config_fn_out)
  end if

  ! ## Assigns x_*
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


  ! ## File reading
  ! ### dissoc.dat required because kapmol requires sat4
  inquire(file=config_fn_dissoc, exist=dissoc_exists)
  if (dissoc_exists) then
    call read_dissoc(config_fn_dissoc)
  else
    call log_warning('File "'//trim(config_fn_dissoc)//'" not found: will take '//&
     'internally stored template and replace abundances with those in "'//trim(config_fn_abonds)//'" -12')
    call assure_read_main(config_fn_main)
    call read_abonds(config_fn_abonds)
    call auto_dissoc()
  end if
  call read_modele(config_fn_modeles)
  call read_molecules(config_fn_molecules)

  ! # Calculation
  call sat4()
  call filter_molecules(x_llzero, x_llfin)
  call log_info('Number of lines within ['//real82str(x_llzero, 3)//', '//&
   real82str(x_llfin, 3)//']: '//int2str(km_f_mblend))
  call log_info('Calculating...')
  call kapmol_()

  call log_info('Writing to file '''//trim(config_fn_out)//'''...')


! # Plez specs
!
!   lambda_air(A)   gf         Elow(cm-1)  vl  Jl    Nl  syml  Eup(cm-1) vu   Ju    Nu  symu  gamrad    mol trans branch
!     3164.8716 0.769183E-11   3459.6228   0   5.0   5.0  0  35047.3396  15   6.0   6.0  0 1.821557E+07 'TiO a f  R    '
! 1234567890123 123456789012 12345678901 123 12345 12345 12 12345678901 123 12345 12345 12 123456789012 1234567890123456
!
! # VALD3 sample
!
!                                                                    Lande factors      Damping parameters
! Elm Ion      WL_air(A)   log gf* E_low(eV) J lo  E_up(eV) J up  lower  upper   mean   Rad.  Stark  Waals
! 'CO 1',       16400.058,  -5.951,  1.3820, 38.0,  2.1378, 39.0,99.000,99.000,99.000, 2.000, 0.000, 0.000,
! 1234567890,123456789012,12345678,12345678,12345,12345678,12345,123456,123456,123456,123456,123456,123456,
! '  Hb                                                4s2.5s2.1p4              X,2,0,,none,4'
! '  Hb                                                    4s2.5s2.1p4          X,2,0,,none,7'
! 'KCO                                  2 KCO       2 KCO       2 KCO       2 KCO       2 KCO       2 KCO       2 KCO       2 KCO       2 KCO     (12)C(16)O    '


  open(newunit=myunit, file=config_fn_out, status='replace', err=10)

  write(myunit, '(a)') &
   '                                                                   Lande factors      Damping parameters'
  write(myunit, '(a)') &
   'Elm Ion      WL_air(A)   log gf* E_low(eV) J lo  E_up(eV) J up  lower  upper   mean   Rad.  Stark  Waals'

  molidx_last = 0
  num_species = 0

  do l = 1, km_f_mblend
    molidx = km_f_molidx(l)
    if (molidx_last .ne. molidx) then
      molidx_last = molidx
      mol_name = make_mol_name(km_symbols(1, molidx), km_symbols(2, molidx))

      ! note that this condition is repeated below; it exists here for logging purpose,
      ! and below, for skipping molecules without the transitional information in the "titulo"
      if (.not. km_has_transitions(km_f_molidx(l))) then
        call log_warning('Cannot convert molecule '''//trim(km_comments(molidx))//&
         ''', for it has no (v'', v'''') information')
      else
        num_species = num_species+1
        mol_names(num_species) = mol_name

        call log_warning('Converting molecule '''//trim(km_comments(molidx))//'''...')
      end if
    end if

    if (.not. km_has_transitions(km_f_molidx(l))) cycle

    write(myunit, '(f13.4, 1x, e12.6, 1x, f11.4, 1x, i3, 1x, f5.1, 1x, f5.1, 1x, i2, '//&  ! lambda_air(A), ...
                  '1x, f11.4, 1x, i3, 1x, f5.1, 1x, f5.1, 1x, i2, 1x, e12.6, 1x, a16)') &  ! Eup(cm-1), ...
     km_f_lmbdam(l), &  ! lambda_air(A)
     km_c_gfm(l), &  ! gf
     0., &  ! Elow(cm-1); (BLB: not needed); TODO check TurboSpectrum for all "not needed"
     km_transitions(2, km_f_transidx(l), km_f_molidx(l)), &  ! vl
     km_f_jj(l), &  ! Jl
     km_f_jj(l), &  ! Nl; TODO check if all the same for a Plez file
     0, &  ! syml
     0., &  ! Eup(cm-1); (BLB: not needed)
     km_transitions(1, km_f_transidx(l), km_f_molidx(l)), &  ! vu
     km_f_jj(l)+1, &  ! Ju; set as J+1 as a "first approximation"; TODO is it going to stay like this?
     km_f_jj(l)+1, &  ! Nu; TODO check if they follow this pattern in Plez file
     0, &  ! symu
     0., &  ! gamrad
     ''''//trim(mol_name)//''''  ! mol trans branch
  end do




!Plez  molidx_last = 0
!Plez  num_species = 0
!Plez
!Plez  do l = 1, km_f_mblend
!Plez    molidx = km_f_molidx(l)
!Plez    if (molidx_last .ne. molidx) then
!Plez      molidx_last = molidx
!Plez      mol_name = make_mol_name(km_symbols(1, molidx), km_symbols(2, molidx))
!Plez
!Plez      ! note that this condition is repeated below; it exists here for logging purpose,
!Plez      ! and below, for skipping molecules without the transitional information in the "titulo"
!Plez      if (.not. km_has_transitions(km_f_molidx(l))) then
!Plez        call log_warning('Cannot convert molecule '''//trim(km_comments(molidx))//&
!Plez         ''', for it has no (v'', v'''') information')
!Plez      else
!Plez        num_species = num_species+1
!Plez        mol_names(num_species) = mol_name
!Plez
!Plez        call log_warning('Converting molecule '''//trim(km_comments(molidx))//'''...')
!Plez      end if
!Plez    end if
!Plez
!Plez    if (.not. km_has_transitions(km_f_molidx(l))) cycle
!Plez
!Plez    write(myunit, '(f13.4, 1x, e12.6, 1x, f11.4, 1x, i3, 1x, f5.1, 1x, f5.1, 1x, i2, '//&  ! lambda_air(A), ...
!Plez                  '1x, f11.4, 1x, i3, 1x, f5.1, 1x, f5.1, 1x, i2, 1x, e12.6, 1x, a16)') &  ! Eup(cm-1), ...
!Plez     km_f_lmbdam(l), &  ! lambda_air(A)
!Plez     km_c_gfm(l), &  ! gf
!Plez     0., &  ! Elow(cm-1); (BLB: not needed); TODO check TurboSpectrum for all "not needed"
!Plez     km_transitions(2, km_f_transidx(l), km_f_molidx(l)), &  ! vl
!Plez     km_f_jj(l), &  ! Jl
!Plez     km_f_jj(l), &  ! Nl; TODO check if all the same for a Plez file
!Plez     0, &  ! syml
!Plez     0., &  ! Eup(cm-1); (BLB: not needed)
!Plez     km_transitions(1, km_f_transidx(l), km_f_molidx(l)), &  ! vu
!Plez     km_f_jj(l)+1, &  ! Ju; set as J+1 as a "first approximation"; TODO is it going to stay like this?
!Plez     km_f_jj(l)+1, &  ! Nu; TODO check if they follow this pattern in Plez file
!Plez     0, &  ! symu
!Plez     0., &  ! gamrad
!Plez     ''''//trim(mol_name)//''''  ! mol trans branch
!Plez  end do










  close(myunit)


  call log_info('# FINAL REPORT')
  call log_info('')
  call log_info('## Summary')
  call log_info('Number of lines within ['//real82str(x_llzero, 3)//', '//&
   real82str(x_llfin, 3)//']: '//int2str(km_f_mblend))
  call log_info('')
  call log_info('## List of ''species''')
  do l = 1, num_species
    call log_info(trim(mol_names(l)))
  end do
  call log_info('')
  call log_info('  -x-x-x-')


  goto 11

  10 continue
  call log_info('Could not create output file')

  11 continue

contains
  ! Makes molecule name from two symbols
  ! Assu

  function make_mol_name(symbol1, symbol2) result(res)
    use pfantlib
    implicit none
    character(len=*), intent(in) :: symbol1, symbol2
    character(len=:), allocatable :: res
    character(len=:), allocatable :: sym1, sym2


print *,'######################'
print *, symbol1, symbol2
    sym1 = trim(adjustl(adjust_atomic_symbol(symbol1)))
    sym2 = trim(adjustl(adjust_atomic_symbol(symbol2)))
print *, sym1, sym2

    ! converts to lowercase the second letter of the atomic symbols
    if (len(sym1) .eq. 2) then
      sym1(2:2) = char(iachar(sym1(2:2))+32)
    end if
    if (len(sym2) .eq. 2) then
      sym2(2:2) = char(iachar(sym2(2:2))+32)
    end if
    if (sym1 .eq. sym2) then
      res = trim(sym1)//'2'
    else
      res = trim(sym1)//trim(sym2)
    end if


    print *, res

  end


end program convmol



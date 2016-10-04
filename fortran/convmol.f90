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






module convmol_aux
  use pfantlib
  implicit none

  integer, parameter :: NUM_SYMBOLS = 8
  character*2, parameter :: SYMBOLS_PFANT(NUM_SYMBOLS) = &
   (/'MG',     ' H',     ' C',     ' N',     ' A',     'FE',     'TI',     ' O'/)
  character*6, parameter :: SYMBOLS_VALD3(NUM_SYMBOLS) = &
   (/'(24)Mg', '(1)H  ', '(12)C ', '(14)N ', '(13)C ', '(56)Fe', '(48)Ti', '(16)O '/)

  real*8 :: x_llzero, x_llfin
  character*69 :: x_fn_out = '?'
contains

  ! Makes molecule formula without the isotopes
  !
  ! Examples: ('C', 'C') becomes 'C2'
  ! Examples: ('MG', 'H') becomes 'MgH' (converts second letter of atomic symbol to lowercase)

  function make_mol_formula_short(symbol1, symbol2) result(res)
    use pfantlib
    implicit none

    ! - List of symbols "featuring" in molecules.dat, dissoc.dat and abonds.dat and
    ! - Corresponding VALD3 notation, using the most "probable" (most abundant) isotope
    ! - "A" is an exception, as it means (13)C
    !
    ! **Note** TODO When/if this is expanded, it would be nice to
    !          - implement a proper naming convention in molecules.dat, abonds.dat and dissoc.dat
    !            e.g. '(235)U', i.e., '(isotope)symbol' with 7 characters. This implies also thorough
    !            code checking because the the existing right-aligned symbolic naming convention (e.g. ' O')
    !            will have to change
    !          - associate symbols without explicit isotope with their most common isotope
    !          - for backwards compatibility, keep translating A --> (13)C
    !

    character(len=*), intent(in) :: symbol1, symbol2
    character(len=:), allocatable :: res
    character(len=:), allocatable :: sym1, sym2


    !print *,'######################'
    !print *, symbol1, symbol2
    sym1 = trim(adjustl(adjust_atomic_symbol(symbol1)))
    sym2 = trim(adjustl(adjust_atomic_symbol(symbol2)))
    !print *, sym1, sym2

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
  end


  ! Makes molecule name from two symbols
  !
  ! Example: ('TI', 'O') becomes '(48)Ti(16)O'

  function make_mol_formula_with_isotopes(symbol1, symbol2) result(res)
    use pfantlib
    implicit none

    ! - List of symbols "featuring" in molecules.dat, dissoc.dat and abonds.dat and
    ! - Corresponding VALD3 notation, using the most "probable" (most abundant) isotope
    ! - "A" is an exception, as it means (13)C
    !
    ! **Note** TODO When/if this is expanded, it would be nice to
    !          - implement a proper naming convention in molecules.dat, abonds.dat and dissoc.dat
    !            e.g. '(235)U', i.e., '(isotope)symbol' with 7 characters. This implies also thorough
    !            code checking because the the existing right-aligned symbolic naming convention (e.g. ' O')
    !            will have to change
    !          - associate symbols without explicit isotope with their most common isotope
    !          - for backwards compatibility, keep translating A --> (13)C
    !

    character(len=*), intent(in) :: symbol1, symbol2
    character(len=:), allocatable :: res

    ! sym1 = adjust_atomic_symbol(symbol1)
    ! sym2 = adjust_atomic_symbol(symbol2)

    res = symbol_pfant2vald(symbol1)//symbol_pfant2vald(symbol2)

  contains

    function symbol_pfant2vald(symbol) result(res)
      character(len=*), intent(in) :: symbol
      character(len=:), allocatable :: res
      integer i
      logical flag_found

      flag_found = .false.
      do i = 1, NUM_SYMBOLS
        if (symbol .eq. SYMBOLS_PFANT(i)) then
          flag_found = .true.
          res = trim(SYMBOLS_VALD3(i))
          exit
        end if
      end do

      if (.not. flag_found) then
        call log_and_halt('Symbol '''//symbol//&
         ''' not found in internal PFANT-to-VALD3 conversion catalogue')
      end if
    end
  end


  ! Assigns values to variables x_*
  !
  ! These values may come frmo varied source, either main.dat, or command-line option,
  ! or made up according to some rule

  subroutine convmol_init_x()
    logical :: main_exists  ! whether or not main configuration file exists
    inquire(file=config_fn_main, exist=main_exists)
    x_fn_out = config_fn_out
    if (config_fn_out .eq. '?') then
      x_fn_out = trim(config_fn_molecules)//'.vald3'
      call parse_aux_log_assignment('x_fn_out', x_fn_out)
    end if

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
  end
end


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! PFANT-to-VALD3 molecular lines file conversion
!
! Creates file specified by option --fn_out

program convmol
  use pfantlib
  use convmol_aux
  implicit none
  integer :: myunit, l, molidx_last, molidx, num_molecules
  logical :: dissoc_exists  !, flag_new_molecule
  character*6 :: mol_formula
  ! have to mount a string to write, for the desired effect, otherwise Fortran will
  ! right-align the formula, and Plez's conversion utility will not understand
  character*11 :: mol_formula_write
  character*14 :: mol_formula_vald3
  real*8 log_gf
  ! ! Formulas of molecules converted
  ! character*6 :: mol_formulas(MAX_NUM_MOL)  ! this store unique names (different "molecules" in molecules.dat may have same formula)
  ! character*30 :: mol_formulas_report(MAX_NUM_MOL)

  ! # Startup
  execonf_name = 'convmol'
  call config_init()
  call convmol_init_x()

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

  call log_info('Writing to file '''//trim(x_fn_out)//'''...')


! # Plez specs
!
!   lambda_air(A)   gf         Elow(cm-1)  vl  Jl    Nl  syml  Eup(cm-1) vu   Ju    Nu  symu  gamrad    mol trans branch
!     3164.8716 0.769183E-11   3459.6228   0   5.0   5.0  0  35047.3396  15   6.0   6.0  0 1.821557E+07 'TiO a f  R    '
! 1234567890123 123456789012 12345678901 123 12345 12345 12 12345678901 123 12345 12345 12 123456789012 1234567890123456
!
! # VALD3 sample (Extraction format: **LONG**; Van der Waals syntax: **EXTENDED**)
!                                                                    Lande factors      Damping parameters
! Elm Ion      WL_air(A)   log gf* E_low(eV) J lo  E_up(eV) J up  lower  upper   mean   Rad.  Stark  Waals
! 'MgH 1',      5500.0418,  -1.522,  0.9240, 14.5,  3.1776, 13.5,99.000,99.000,99.000, 7.060, 0.000,   0.000
! 1234567890,123456789012,12345678,12345678,12345,12345678,12345,123456,123456,123456,123456,123456,12345678,
! '  Hb                                                 4s2.5s1                  X,2,0,e,14,5'
! 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
! '  Hb                                                 4s1.5s2/4s2.7s1/4s2.6s1  B,2,0,e,13,4'
! 'KMGH                                 6 KMGH      6 KMGH      6 KMGH      6 KMGH      6 KMGH      6 KMGH      6 KMGH      6 KMGH      6 KMGH    (24)MgH       '
! 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234              1
!                                                                                                                                                 12345678901234
!                                                                                                                                                 |            |
!========= (VALD3 file spans 159 columns) ==========>>>                                                                                         145          158


  open(newunit=myunit, file=x_fn_out, status='replace', err=100)

  write(myunit, '(a)') &
   '                                                                   Lande factors      Damping parameters'
  write(myunit, '(a)') &
   'Elm Ion      WL_air(A)   log gf* E_low(eV) J lo  E_up(eV) J up  lower  upper   mean   Rad.  Stark  Waals'

  molidx_last = 0
  num_molecules = 0

  do l = 1, km_f_mblend
    molidx = km_f_molidx(l)
    if (molidx_last .ne. molidx) then
      molidx_last = molidx
      mol_formula = make_mol_formula_short(km_symbols(1, molidx), km_symbols(2, molidx))
      mol_formula_vald3 = make_mol_formula_with_isotopes(km_symbols(1, molidx), km_symbols(2, molidx))

      ! TRYING-ALL ! note that this condition is repeated below; it exists here for logging purpose,
      ! TRYING-ALL ! and below, for skipping molecules without the transitional information in the "titulo"
      ! TRYING-ALL if (.not. km_has_transitions(km_f_molidx(l))) then
      ! TRYING-ALL   call log_warning('Cannot convert molecule '''//trim(km_comments(molidx))//&
      ! TRYING-ALL    ''', for it has no (v'', v'''') information')
      ! TRYING-ALL else
        num_molecules = num_molecules+1
        !  mol_formulas(num_molecules) = mol_formula

        call log_info('Converting molecule '''//trim(km_comments(molidx))//'''...')
        call log_info('... '''//mol_formula//''' aka '''//mol_formula_vald3//'''...')
      ! TRYING-ALL end if
    end if

    ! TRYING-ALL if (.not. km_has_transitions(km_f_molidx(l))) cycle


    ! Protection against -Infinity
    log_gf = log10(km_c_gfm(l))
    if (log_gf .lt. -99.99) log_gf = -99.99

    mol_formula_write = ''''//trim(mol_formula)//' 1'','

    ! note that the comma is inside the a11, for the desired effect
    11 format(a11,f12.3,',',f8.3,',',f8.4,',',f5.1,',',f8.4,',',f5.1,',',f6.3,',',&
              f6.3,',',f6.3,',',f6.3,',',f6.3,',',f8.3,',')
    12 format('''', 90x, '''')
    13 format('''', 90x, '''')
    14 format('''', 143x, a14, '''')

    write(myunit, 11) &
     mol_formula_write, &
     km_f_lmbdam(l), &  ! WL_air(A)
     -2., &  ! log gf
     1., &  ! 1., &  ! Elow(eV)
     km_f_jj(l)-100, &  ! J lo
     99.,  &  !  3., &  !E_up(eV)
     km_f_jj(l)+100, &  ! J up
     99., &  ! lower
     99., &  ! upper
     99., &  ! mean
     10., &  ! Rad.
     99., &  ! Stark
     99.  ! Walls
    write(myunit, 12)
    write(myunit, 13)
    write(myunit, 14) mol_formula_vald3
  end do



  close(myunit)


  ! why so much?? call log_info('# FINAL REPORT')
  ! why so much?? call log_info('')
  ! why so much?? call log_info('## Summary')
  ! why so much?? call log_info('Number of lines within ['//real82str(x_llzero, 3)//', '//&
  ! why so much??  real82str(x_llfin, 3)//']: '//int2str(km_f_mblend))
  ! why so much?? call log_info('')
  ! why so much?? call log_info('## List of ''molecules converted''')
  ! why so much?? do l = 1, num_molecules
  ! why so much??   call log_info(trim(mol_formulas(l)))
  ! why so much?? end do
  ! why so much?? call log_info('')
  call log_info('  -x-x-x-')
  call log_info('File '//trim(x_fn_out)//' successfully created.')

  goto 110

  100 continue
  call log_info('Could not create output file')

  110 continue
end program convmol















!Plez  molidx_last = 0
!Plez  num_molecules = 0
!Plez
!Plez  do l = 1, km_f_mblend
!Plez    molidx = km_f_molidx(l)
!Plez    if (molidx_last .ne. molidx) then
!Plez      molidx_last = molidx
!Plez      mol_formula = make_mol_formula(km_symbols(1, molidx), km_symbols(2, molidx))
!Plez
!Plez      ! note that this condition is repeated below; it exists here for logging purpose,
!Plez      ! and below, for skipping molecules without the transitional information in the "titulo"
!Plez      if (.not. km_has_transitions(km_f_molidx(l))) then
!Plez        call log_warning('Cannot convert molecule '''//trim(km_comments(molidx))//&
!Plez         ''', for it has no (v'', v'''') information')
!Plez      else
!Plez        num_molecules = num_molecules+1
!Plez        mol_formulas(num_molecules) = mol_formula
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
!Plez     ''''//trim(mol_formula)//''''  ! mol trans branch
!Plez  end do

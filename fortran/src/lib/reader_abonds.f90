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

!> Reading routines and variable declarations for infile:abonds

module reader_abonds
  use logging
  use max_
  implicit none

  !=====
  ! Variables filled by read_abonds() (file abonds.dat)
  !=====
  integer abonds_nabond !< ?doc?
  character*2 abonds_ele(MAX_ABONDS_NABOND)     !< ?doc?
  real*8      abonds_abol(MAX_ABONDS_NABOND), & !< ?doc?
              abonds_abo(MAX_ABONDS_NABOND)     !< ?doc? This is calculated

  !> Flag indicating whether read_abonds() has already been called
  logical :: flag_read_abonds = .false.

contains

  !=======================================================================================
  !> Reads file abonds.dat to fill variables abonds_*
  !>
  !> In addition, searches elements in dissoc atoms table to take XXCOR values.
  !>
  !> The input file has 3 columns:
  !> @li Empty space or "1"
  !> @li 2-character atomic element symbol (?)
  !> @li absolute abundance (a) (exponent; actual abundance is 10^a), unit "dex"
  !>
  !> @attention The end of the file is signalled by two rows containing only "1" each at
  !> column 1 and nothing else at the others, i.e.,
  !> @verbatim
  !> .......(last data row).......
  !> 1
  !> 1
  !> @endverbatim
  !>
  !> @todo Test one row!!!
  !> @todo use READ()'s "END=" option

  subroutine read_abonds(path_to_file)
    implicit none
    integer UNIT_, finab, k, j
    parameter(UNIT_=199)
    character(len=*) :: path_to_file
    logical flag_found
    real*8 fstar

    if (.not. flag_read_main) then
      call pfant_halt('read_main() must be called before read_abonds()')
    end if

    open(unit=UNIT_,file=path_to_file, status='old')

    fstar = 10**main_afstar

    j = 1
    finab = 0
    do while (finab .lt. 1)
      read(UNIT_, '(i1,a2,f6.3)') finab, abonds_ele(j), abonds_abol(j)

      if (finab .lt. 1) then
        ! Extra tasks (i.e., apart from reading file) [1], [2]:
        !
        ! [1] Searches dissoc.dat' metals table by atomic symbol to get value of XXCOR variable
        !> @todo ISSUE: lot of effort to keep dubious feature
        !> @todo ISSUE: This is the thing that Beatriz mentioned that is not used anymore

        flag_found = .false.
        do k = 1, dissoc_nmetal
          if(abonds_ele(j) .eq. dissoc_elems(k)) then
            flag_found = .true.
            exit
          end if
        end do

        if (flag_found) then
          abonds_abol(j) = abonds_abol(j)+main_xxcor(k)
        else
          ! if not found, doesn't bother
        end if


        ! [2] Calculates abonds_ABO based on abonds_ABOL

        !> @todo ISSUE ask MT Why "-12" ?? (MT) By definition, the "abundance of element X" is given by (X/H) = log10(NX/NH) - 12

        !> @note What is in abonds.dat is log10(NX/NH) - [Fe/H]

        !> @note What is in dissoc.dat is log10(NX/NH) - 12 - [Fe/H]

        !> @todo issue abundances in abonds.dat and dissoc.dat correspond to (X/H) - [Fe/H]. The purpose is to [X/Fe] = 0.0

        !> @todo issue dissoc.dat has redundant information: all abundances are these but with 12 subtracted and they should match!
        abonds_abo(j) = 10.**(abonds_abol(j)-12.)
        abonds_abo(j) = abonds_abo(j)*fstar
      end if

      j = j+1
    end do
    abonds_nabond = j-2

    close(unit=UNIT_)
    flag_read_abonds = .true.
  end
end

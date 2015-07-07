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

!> Reading routines and variable declarations for infile:dissoc

module reader_dissoc
  use logging
  use max_
  implicit none

  flag_read_dissoc = .false.    !< Whether read_dissoc() has already been called


  ! dissoc.dat, metals part
  integer dissoc_nmetal !< number of elements considered in chemical equilibrium
  integer dissoc_nimax  !< maximum number of iterations in Newton-Rapson method
  character*2 dissoc_elems(MAX_DISSOC_NMETAL)    !< Elements table field 1/6: element symbol
  integer     dissoc_nelemx(MAX_DISSOC_NMETAL)   !< Elements table field 2/6: atomic number
  real*8      dissoc_ip(MAX_DISSOC_NMETAL)      !< Elements table field 3/6: ?
  integer     dissoc_ig0(MAX_DISSOC_NMETAL),  & !< Elements table field 4/6: ?
              dissoc_ig1(MAX_DISSOC_NMETAL)     !< Elements table field 5/6: ?
  real*8      dissoc_cclog(MAX_DISSOC_NMETAL)   !< Elements table field 6/6: ?

  ! dissoc.dat, molecules part
  character*3 dissoc_mol(MAX_DISSOC_NMOL)     !< Molecules table field 01: molecule name
  real*8 dissoc_c(MAX_DISSOC_NMOL, 5)         !< Molecules table fields 02-06
  integer dissoc_mmax(MAX_DISSOC_NMOL), &     !< Molecules table field 07
          dissoc_nelem(5, MAX_DISSOC_NMOL), & !< Molecules table fields 08, 10, ...
          dissoc_natom(5, MAX_DISSOC_NMOL)    !< Molecules table fields 09, 11, ...
  integer dissoc_nmol
  real*8 dissoc_eps,   & !< if abs((x(i+1)-x(i))/x(i)) .le. eps: converged
         dissoc_switer   !< flag affecting x(i+1)
                         !< @li if switer .gt. 0 then x(i+1)=0.5*(x(i+1)+x(i))
                         !< @li if switer .le. 0 then x(i+1)=x(i+1)

contains

  !=======================================================================================
  !> Reads dissoc.dat to fill variables dissoc_*
  !>
  !> @attention This file must end with a <b>blank row</b> so that the routine can detect
  !> the end of the file.

  !> @todo Various tests:
  !> @todo - mismatched NMETAL and metal rows
  !> @todo - check if NMETAL and NMOL match what they are supposed to (assertions in test)
  !> @todo ISSUE: This 1X does not appear in my sample dissoc.dat file
  !> @todo ISSUE: Atually the file that Beatriz sent me does not work under this format!!!!
  !> @todo ISSUE: THere is no 1X
  !<      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
  !> @todo PROPOSE: use READ()'s "END=" option

  subroutine read_dissoc(path_to_file)
    integer, parameter :: UNIT_=199
    integer i, j, k, m, mmaxj
    character(len=*) :: path_to_file
    character*2 symbol
    logical flag_found

    ! Auxiliary temp variables for reading file
    integer*4 natomm, nelemm
    dimension natomm(5), nelemm(5)

    open(unit=UNIT_,file=path_to_file, status='old')

    ! row 01
    read(UNIT_,'(2i5, 2f10.5, i10)') dissoc_nmetal, dissoc_nimax, dissoc_eps, dissoc_switer

    ! rows 02 to NMETAL+1: 6-column rows
    !
    !
    !
    do i = 1, dissoc_nmetal
      read (UNIT_, '(a2, 2x, i6, f10.3, 2i5, f10.5)') &
       symbol, dissoc_nelemx(i), dissoc_ip(i), &
       dissoc_ig0(i), dissoc_ig1(i), dissoc_cclog(i)

        ! makes sure that elements first and second are h and he, respectively,
        ! because sat4() and die() count on this
        select case (i)
          case (1)
            if (to_lower(trim(symbol)) .ne. 'h') then
              write(lll,*) 'First element must be hydrogen ("H"), not "', symbol, '"!'
              call pfant_halt(lll)
            end if
          case (2)
            if (to_lower(symbol) .ne. 'he') then
              write(lll,*) 'First element must be helium ("He"), not "', symbol, '"!'
              call pfant_halt(lll)
            end if
        end select

        dissoc_elems(i) = symbol

        !#spill_check
        if (dissoc_nelemx(i) .gt. MAX_DISSOC_Z) then
          call pfant_halt('read_dissoc(): metal # '//int2str(i)//': nelemxi = '//&
           int2str(dissoc_nelemx(i))//' over maximum allowed (MAX_DISSOC_Z='//int2str(MAX_DISSOC_Z)//')')
        end if

      end do



      ! rows NMETAL+2 till end-of-file
      !   col  1     -- "name" of molecule
      !   cols 2-6   -- c(J, 1-5)
      !   col  7     -- mmax(j) (number of subsequent columns)/2
      !   cols 8-... -- maximum of 8 columns here.
      !                 pairs (nelem(m), natom(m)), m = 1 to mmax(j)
      j = 0

      1010 continue
      j = j+1

      read(UNIT_, '(a3, 5x, e11.5, 4e12.5, i1, 4(i2,i1))') &
                   dissoc_mol(j), &
                   (dissoc_c(j, k), k=1,5), &
                   dissoc_mmax(j), &
                   (nelemm(m), natomm(m), m=1,4)

      mmaxj = dissoc_mmax(j)
      if(mmaxj .eq. 0) then
        ! note: interesting that Fortran accepts a blank line and reads everything blank or zero

        go to 1014  ! means end-of-file
      end if

      !__consistency check__:
      if (mmaxj .gt. 4) then
        write(lll,*) 'read_dissoc() molecule "', dissoc_mol(j), &
         '", mmaxj = ', mmaxj, ' cannot be greater than 4!'
        call pfant_halt(lll)
      end if

      !__consistency check__
      do m = 1, mmaxj
        flag_found = .false.
        do i = 1, dissoc_nmetal
          if (nelemm(m) .eq. dissoc_nelemx(i)) then
            flag_found = .true.
            exit
          end if
        end do

        if (.not. flag_found) then
          write(lll,*) 'read_dissoc() molecule "', dissoc_mol(j), &
           '" atomic number ', nelemm(m), 'not in atoms list above'
          call pfant_halt(lll)
        end if
      end do

      do m = 1, mmaxj
          dissoc_nelem(m,j) = nelemm(m)
          dissoc_natom(m,j) = natomm(m)
      end do

      go to 1010

    1014 dissoc_nmol = j-1

    write(lll,*) 'Last molecule considered in dissoc file is ', dissoc_mol(dissoc_nmol)
    call log_debug(lll)

    close(unit=UNIT_)
    flag_read_dissoc = .true.
  end
end

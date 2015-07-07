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

!> Reading routines and variable declarations for infile:partit

module reader_partit
  use logging
  use misc
  use max_
  implicit none

  character*2, dimension(MAX_PARTIT_NPAR) :: &
   partit_el !< ?doc?
  integer partit_npar !< ?doc?

  real*8, dimension (MAX_PARTIT_NPAR) :: &
   partit_ki1,  & !< ?doc?
   partit_pa,   & !< ?doc?
   partit_m,    & !< ?doc?
   partit_tini, & !< ?doc?
   partit_ki2
  integer, dimension (MAX_PARTIT_NPAR) :: &
   partit_jkmax !< ?doc?

  real*8, dimension (MAX_PARTIT_NPAR, 3, MAX_PARTIT_KMAX) :: &
   partit_tabu !< ?doc?

contains
  !=======================================================================================
  !> Reads file infile:partit to fill variables partit_*
  !>
  !> LECTURE DES FCTS DE PARTITION
  !>
  !> Rows in this file alternate between:
  !> @verbatim
  !> 1) 8-column row
  !>    col 8 -- signals end-of-file. If 1, it ignores the row and
  !>             stops reading
  !>
  !> 2) Series of rows to fill in partit_TABU(J, :, :)
  !> @endverbatim

  subroutine read_partit(path_to_file)
    implicit none
    integer UNIT_
    parameter(UNIT_=199)
    character(len=*) :: path_to_file

    integer finpar, j, kmax, l, k

    open(unit=UNIT_,file=path_to_file, status='old')


    j = 1
    finpar = 0
    do while (finpar .lt. 1)
      read (UNIT_, '(a2, 2f5.2, i3, 3f10.2, 34x, i1)') &
       partit_el(j), &
       partit_tini(j), &
       partit_pa(j), &
       partit_jkmax(j), &
       partit_m(j), &
       partit_ki1(j), &
       partit_ki2(j), finpar

      if (finpar .ne. 1) then

        !#spill_check: checks if exceeds maximum number of elements allowed
        if (j .gt. MAX_PARTIT_NPAR) then
          call pfant_halt('read_partit(): par exceeded maximum of MAX_PARTIT_NPAR='//&
           int2str(MAX_PARTIT_NPAR))
        end if


        kmax = partit_jkmax(j)

        !#spill_check: checks if exceeds maximum number of elements allowed
        if (kmax .gt. MAX_PARTIT_KMAX) then
          call pfant_halt('read_partit(): par number '//int2str(j)//'; kmax='//&
           int2str(kmax)//' exceeded maximum of MAX_PARTIT_KMAX='//int2str(MAX_PARTIT_KMAX))
        end if


        read(UNIT_, '(13f6.4)') ((partit_tabu(j, l, k), l=1, 3), k=1, kmax)

        j = j+1
      end if
    end do

    partit_npar = j-1

    return
  end
end
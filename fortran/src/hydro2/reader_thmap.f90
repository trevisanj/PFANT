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

!> Reading routines and variable declarations for infile:thmap

module reader_thmap
  use logging
  use max_
  use reader_dissoc
  implicit none

  !> Structure to store one row of infile:thmap
  type thmap_row  
    !> file name
    character*16 fn
    !> NIV INF
    integer na
    !> NIV SUP
    integer nb
    !> Central lambda
    real*8 clam
    !> KIEX ?doc?
    real*8 kiex
    !> C1 ?doc?
    real*8 c1
  end type

  !> Array to store all rows in infile:thmap
  type(thmap_row) :: thmap_rows(MAX_FILETOH_NUMFILES)

  !> Number of rows in infile:thmap
  integer :: thmap_n

contains

  !=======================================================================================
  !> Reads infile:thmap to fill variables thmap_*
  !>

  subroutine read_thmap(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer, parameter :: UNIT_ = 199

    ! Temporary auxiliary variables for reading file
    character*16 t_fn
    integer t_na
    integer t_nb
    real*8 t_clam
    real*8 t_kiex
    real*8 t_c1

    open(unit=UNIT_,file=path_to_file, status='old')

    ! row 01: (skipped) general information
    read(UNIT_,*)
    ! row 02: (skipped) table header
    read(UNIT_,*)

    thmap_n = 0
    do while (.true.)
      ! rows 03..end
      read(UNIT_, *, end=10) t_fn, t_na, t_nb, t_clam, t_kiex, t_c1

      thmap_n = thmap_n+1

      thmap_rows(thmap_n)%fn = t_fn
      thmap_rows(thmap_n)%na = t_na
      thmap_rows(thmap_n)%nb = t_nb
      thmap_rows(thmap_n)%clam = t_clam
      thmap_rows(thmap_n)%kiex = t_kiex
      thmap_rows(thmap_n)%c1 = t_c1
    end do

    10 continue  ! reached EOF

    close(unit=UNIT_)
  end
end
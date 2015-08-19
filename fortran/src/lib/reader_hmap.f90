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

!> Reading routines and variable declarations for infile:hmap

module reader_hmap
  use logging
  use max_
  use misc
  use reader_main
  ! use reader_dissoc
  implicit none

  !> Structure to store one row of infile:hmap
  type hmap_row
    !> file name
    character*64 fn
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

  !> Array to store all rows in infile:hmap
  type(hmap_row) :: hmap_rows(MAX_FILETOH_NUMFILES)

  !> Number of rows in infile:hmap
  integer :: hmap_n

contains

  !=======================================================================================
  !> Reads infile:hmap to fill variables hmap_*
  !>

  subroutine read_hmap(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer, parameter :: UNIT_ = 199
    logical :: skip_row(MAX_FILE_ROWS)
    integer :: num_rows, i

    ! Temporary auxiliary variables for reading file
    character*64 t_fn
    integer t_na
    integer t_nb
    real*8 t_clam
    real*8 t_kiex
    real*8 t_c1

    call map_file_comments(path_to_file, skip_row, num_rows)

    write(*,*) 'num_rows=', num_rows

    open(unit=UNIT_,file=path_to_file, status='old')

    hmap_n = 0
    do i = 1, num_rows
      if (skip_row(i)) then
        read(UNIT_,*)   ! skips comment row
      else
        read(UNIT_, *) t_fn, t_na, t_nb, t_clam, t_kiex, t_c1

        hmap_n = hmap_n+1

        hmap_rows(hmap_n)%fn = t_fn
        hmap_rows(hmap_n)%na = t_na
        hmap_rows(hmap_n)%nb = t_nb
        hmap_rows(hmap_n)%clam = t_clam
        hmap_rows(hmap_n)%kiex = t_kiex
        hmap_rows(hmap_n)%c1 = t_c1
      end if
      print *, 'read row ', i, ' successfully'
    end do
    close(unit=UNIT_)


    !#logging
    call log_info('reader_hmap():')
    call log_info('filename         na nb c.lambda     kiex       c1')
    do i = 1, hmap_n
      11 format(a16,1x,i2,1x,i2,1x,f8.2,1x,f8.2,1x,f8.2)
      write(lll, 11) hmap_rows(i)
      call log_info(lll)
    end do
  end


  !========================================================================================
  !> Fills hmap_rows according with main_filetohy. For compatibility with old way of
  !> informing the "filetoh" filenames, which was inside infile:main
  !>
  !> This routine fills only the "fn" (filename) field of the hmap_row structure because
  !> the filenames are all that's available in infile:main.
  !>
  !> This routine is used only by pfant when not in "--hmap" mode.
  !>
  !> This routine is part of a mechanism to allow pfant to work in two ways.
  !> @li either use infile:hmap for the list of hydrogen line files (likely to become the
  !>     standard way in the future)
  !> @li take this list from infile:main (legacy)

  subroutine hmap_copy_from_main()
    integer :: i

    hmap_n = 0
    do i = 1, main_filetoh_numfiles
      hmap_n = hmap_n+1
      hmap_rows(hmap_n)%fn = main_filetohy(i)
      hmap_rows(hmap_n)%na = 0
      hmap_rows(hmap_n)%nb = 0
      hmap_rows(hmap_n)%clam = 0
      hmap_rows(hmap_n)%kiex = 0
      hmap_rows(hmap_n)%c1 = 0
    end do
  end
end

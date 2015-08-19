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

!> Reading routines and variable declarations for infile:filetoh

module reader_filetoh
  use max_
  use reader_modeles
  use config_pfant
  use reader_hmap
  implicit none

  !=====
  ! Read directly from file
  !=====
  ! These are read by read_filetoh() and processed by filetoh_auh()
  !> ?doc?
  character*80 filetoh_r_titre(MAX_FILETOH_NUMFILES)
  !> ?doc?
  character*11 filetoh_r_ttt(MAX_FILETOH_NUMFILES)
  !> Will be pointer target
  !> ?doc?
  real*8, target, dimension(MAX_FILETOH_NUMFILES, MAX_FILETOH_JMAX, MAX_MODELES_NTOT) :: &
   filetoh_r_th
  !> Will be pointer target
  !> ?doc?
  real*8, target, dimension(MAX_FILETOH_NUMFILES, MAX_FILETOH_JMAX) :: filetoh_r_lambdh
  !> ?doc?
  integer filetoh_r_jmax(MAX_FILETOH_NUMFILES)

  !> This variable was a constant hard-coded as
  !> @code
  !> /3750.150, 3770.630, 3797.900, 3835.390, 3889.050, 3970.076, 4101.748, 4340.468, 4861.332, 6562.817/
  !> @endcode
  !> Now it is opening the files and taking the initial lambda for each file instead
  !> @todo line is symmetric; check if left-side is assumed
  real*8, dimension(MAX_FILETOH_NUMFILES) :: filetoh_llhy
  !> Number of filetoh files that were actually found in disk
  integer :: filetoh_numfiles = 0
  !> Names of filetoh files that were actually found in disk
  character*64 filetoh_filenames(MAX_FILETOH_NUMFILES)


  !!=====
  !! Calculated for external use
  !!=====
  !!> ?doc?
  !real*8 ct_tauhi(MAX_DTOT, MAX_MODELES_NTOT)
  !integer :: &
  ! ct_dhmi, & !< ?doc?
  ! ct_dhpi    !< ?doc?!!
!
!  ! 888b. 888b. 888 Yb    dP  db   88888 8888
!  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
!  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
!  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols!!
!
!
 ! !  integer :: jjmax

  save
contains

  !=======================================================================================
  !> Tries to open and read all files listed in variable hmap_rows
  !>
  !> [llzero, llfin] is the calculation lambda interval. Here this interval is used for
  !> error checking: if it doesn't find a file that should exist, it will give an error
  !>
  !> @note For compatibility, it will allow to pass if the filetoh list came from inside
  !> infile:main. This is assumed to have happened if the "clam" field of a given hmap
  !> row is zero.
  !>
  !> LECTURE DE LA PROFONDEUR OPTIQUE DANS LA RAIE D H

  subroutine read_filetoh(llzero, llfin)
    real*8, intent(in) :: llzero, llfin
    integer unit_
    parameter(unit_=199)
    integer i, j, n, i_file
    character(len=:), allocatable :: file_now
    real*8 :: clam
    logical :: must_exist


    i = 0
    do i_file = 1, hmap_n
      file_now = full_path_w(hmap_rows(i_file)%fn)
      clam = hmap_rows(i_file)%clam

      must_exist = .false.
      if (clam .ne. 0) then
        if (h_line_is_inside(clam, llzero, llfin)) must_exist = .true.
      end if

      open(err=111, unit=unit_,file=file_now,status='old')

      i = i+1

      read(unit_,'(a80)') filetoh_r_titre(i)
      read(unit_,'(a11)') filetoh_r_ttt(i)
      read(unit_,*) filetoh_r_jmax(i)  ! Note: format was i3
      read(unit_,'(5f14.3)') (filetoh_r_lambdh(i,j), j=1,filetoh_r_jmax(i))
      read(unit_,'(5e12.4)') ((filetoh_r_th(i,j,n),&
       j=1,filetoh_r_jmax(i)), n=1,modeles_ntot)

      ! Takes first lambda of file as a reference
      filetoh_llhy(i) = filetoh_r_lambdh(i, 1)

      ! Registers filename in list of files that were found
      filetoh_filenames(i) = hmap_rows(i_file)%fn

      close(unit_)
      goto 112

      111 continue
      if (must_exist) then
        130 format('[',F7.1,'-',F5.1,',',F7.1,'+',F5.1,'] overlaps with [',&
         F7.1,'-',F5.1,',',F7.1,'+',F5.1,'], but cannot open file "',A,'"')
        write(lll,130) clam, H_LINE_WIDTH, clam, H_LINE_WIDTH, llzero, LAMBDA_STRETCH, &
         llfin, LAMBDA_STRETCH, file_now
        call pfant_halt(lll)
      end if
      call log_warning('Error opening file "' // file_now // '"')

      112 continue
    end do

    ! Note: when taking the "filetohy" from main configuration file, will not bother
    ! about hydrogen lines files not found, so bewhare (--hmap is the preferred mode anyway)
  end
end

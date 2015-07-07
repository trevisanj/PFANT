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


  !=====
  ! Calculated for external use
  !=====
  !> ?doc?
  real*8 ct_tauhi(MAX_DTOT, MAX_MODELES_NTOT)
  integer :: &
   ct_dhmi, & !< ?doc?
   ct_dhpi    !< ?doc?

  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols


  !  integer :: jjmax

  save
contains

  !=======================================================================================
  !> Tries to open and read all files listed in variable read_most_files::main_filetohy
  !>
  !> If a1 filetoh file listed in infile:main file
  !> LECTURE DE LA PROFONDEUR OPTIQUE DANS LA RAIE D H

  subroutine read_filetoh()
    integer unit_
    parameter(unit_=199)
    integer i, j, n, i_file
    character(len=:), allocatable :: file_now


    i = 0
    do i_file = 1, main_filetoh_numfiles
      file_now = full_path_i(main_filetohy(i_file))

      open(err=111, unit=unit_,file=file_now,status='old')

      i = i+1

      read(unit_,'(a80)') filetoh_r_titre(i)
      read(unit_,'(a11)') filetoh_r_ttt(i)
      read(unit_,*) filetoh_r_jmax(i)  ! Note: format was i3
      read(unit_,'(5f14.3)') (filetoh_r_lambdh(i,j), j=1,filetoh_r_jmax(i))
      read(unit_,'(5e12.4)') ((filetoh_r_th(i,j,n),&
       j=1,filetoh_r_jmax(i)), n=1,modeles_ntot)

      !> @todo ask blb not taking lambda from files instead
      ! Takes first lambda of file as a reference
      filetoh_llhy(i) = filetoh_r_lambdh(i, 1)

      ! Registers filename in list of files that were found
      filetoh_filenames(i) = main_filetohy(i_file)

      close(unit_)
      goto 112

      111 continue
      call log_warning('Error opening file "' // file_now // '"')


      112 continue

    !> @todo Check jmax spill here
    end do

    if (i .eq. 0 .and. main_filetoh_numfiles .gt. 0 .and. .not. config_allow_no_filetoh) then
      call pfant_halt('Expecting '//int2str(main_filetoh_numfiles)//' filetoh files, but ZERO files found')
    end if
  end


  !=======================================================================================
  !> Calculates tauhi, dhmi and dhpi for file specified
  !>
  !> @note this is originally subroutine "LECTAUH" without the file reading part
  !>
  !> @todo test the pointers

  subroutine filetoh_calc_tauh(i_file, dtot, ttd, ilzero)
    integer, intent(in) :: i_file !< index of a filetoh file
    !> ?doc? Number of calculation steps, I think. ISSUE: better explanation
    !> Calculated as: @code dtot = (lfin-lzero)/main_pas + 1.0005 @endcode
    integer, intent(in) :: dtot
    !> integer version of variable lzero in main module
    integer, intent(in) :: ilzero
    !> ?doc? Calculated as: ttd(d) = alzero+main_pas*(d-1)
    real*8, intent(in) :: ttd(MAX_DTOT)
    integer d, j, jj, jma1, n, &
     jjmax, &
     now_jmax ! jmax of file i_file

    real*8, dimension(MAX_FILETOH_JJMAX) :: llambdh, allh, tauhn
    real*8 :: tth(MAX_FILETOH_JJMAX, MAX_MODELES_NTOT)
    real*8 :: ftth(MAX_DTOT)

    real*8 del
    ! pointers, point to information within filetoh_r_* matrices at the beginning of\
    ! a specific file.
    ! This simplifies the notation within the loop below and is probably faster than
    ! accessing the variables filetoh_r_* directly
    real*8, pointer, dimension(:,:) :: now_th
    real*8, pointer, dimension(:)   :: now_lambdh

    now_jmax   = filetoh_r_jmax(i_file)
    now_th     => filetoh_r_th(i_file, :, :)
    now_lambdh => filetoh_r_lambdh(i_file, :)

    jjmax = 2*now_jmax-1
    jma1 = now_jmax-1
    do jj = 1, now_jmax
      del = now_lambdh(now_jmax+1-jj)-now_lambdh(1)
      llambdh(jj) = now_lambdh(now_jmax+1-jj)-2*del
    end do
    do jj = now_jmax+1, jjmax
      llambdh(jj) = now_lambdh(jj-jma1)
    end do
    do n = 1, modeles_ntot
      do jj = 1, now_jmax
        tth(jj, n) = now_th(now_jmax+1-jj, n)
      end do
      do jj = now_jmax+1, jjmax
        tth(jj, n) = now_th(jj-jma1, n)
      end do
    end do

    !~WRITE(6,'(A80)') filetoh_r_TITRE
    !~WRITE(6,'(A11)') filetoh_r_TTT
    !~WRITE(6,'('' now_jmax='',I3)') now_jmax
    !~WRITE(6,'(2X,5F14.3)') (LLAMBDH(JJ), JJ=1,JJMAX)
    !~WRITE(6,'(2X,5F14.3)') (LLAMBDH(JJ), JJ=1,JJMAX)
    !~
    !~DO N = 1,modeles_NTOT,5
    !~  WRITE(6,'('' N='',I3)') N
    !~  WRITE(6,'(2X,5E12.4)') (TTH(JJ,N), JJ=1,JJMAX)
    !~END DO


    do j = 1,jjmax
      allh(j) = llambdh(j)-ilzero
    end do

    !~ WRITE(6, '('' ALLH(1)='',F8.3,2X,''ALLH(JJMAX)='',F8.3,2X)')
    !~+      ALLH(1),ALLH(JJMAX)
    !~ WRITE(6, '('' JJMAX='',I3,2X,''NTOT='',I3,2X,''DTOT='',I5)')
    !~       JJMAX, modeles_NTOT, DTOT

    do n = 1,modeles_ntot
      do j = 1,jjmax
        tauhn(j) = tth(j,n)
      end do

      call ftlin3h()

      do d = 1,dtot
        ct_tauhi(d, n) = ftth(d)
      end do
    end do


    !~ !--debugging--!
    !~ WRITE(6,'('' TAUHI(1,1)='',E14.7,2X,''TAUHI(1,NTOT)='',E14.7)')
    !~+ ct_tauhi(1,1), ct_tauhi(1,modeles_NTOT)
    !~ WRITE(6,'('' TAUHI(DTOT,1)='',E14.7,2X,'
    !~+ //'''TAUHI(DTOT,NTOT)='',E14.7)')
    !~+ ct_tauhi(DTOT,1), ct_tauhi(DTOT,modeles_NTOT)

  contains
    !-------------------------------------------------------------------------------
    !> @todo ISSUE ?what?
    !>
    !> @todo This routine is *very similar to misc_math::ftlin3()*, I think the latter
    !> has been duplicated to build ftlin3h(). Not sure what to do. At least write more
    !> about the differences.
    !>
    !> Uses variables from parent filetoh_auh():
    !> @li dtot
    !> @li ttd
    !> @li jjmax
    !>
    subroutine ftlin3h()
      real*8 dy, ft, t, t0, t1, t2, u0
      integer j, k, kk, jj, kk1, kq

      j=2
      kk=1
      24 continue
      do 4 k = kk,dtot
        kq=k
        t=ttd(k)

        jj=j-1
        do 1  j=jj,jjmax
          if(t-allh(j) ) 3,2,1
          1 continue
          go to 10
          2 ft=tauhn(j)
        if(j .eq. 1) j = j+1
        go to 4

        3 if (j .eq. 1) go to 10
        u0 = tauhn(j)-tauhn(j-1)
        t0 = allh(j)-allh(j-1)
        t1 = t-allh(j-1)

        t2= t1/t0
        dy= u0*t2
        ft= tauhn(j-1) + dy
        ftth(k) = ft
      4 continue

      14 continue

      do k=1,dtot
        if(ftth(k).ne.0.0) go to 20
      end do

      20 ct_dhmi = k

      !> @todo issue ask blb why this? Take the opportunity to ask for a line on
      !>
      !> @todo ask blb or ask pc marie noel reference on this
      !>
      !> tauhi(:,:), dhmi and dhpi
      if (ct_dhmi .eq. dtot) ct_dhmi = 1


      kk1 = ct_dhmi+1
      do k = kk1,dtot
        if (ftth(k) .eq. 0.0) go to 30
      end do

      30 ct_dhpi = k

      ! (Paula Coelho 21/11/04) instrucao da Marie Noel
      !> @todo issue ask blb or ask pc why this?
      if (ftth(dtot) .ne. 0.0) ct_dhpi = dtot

      return

      10 ftth(k) = 0.
      j = j+1

      kk = kq
      kk = kk+1
      if (kq .gt. dtot) go to 14
      go to 24
    end
  end
end

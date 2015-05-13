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

!> @ingroup gr_io
!> Module "FILETOH"
!>
!> Routines that deal with the FILETOH files: reading and related calculations
!>
!> Variables
!>   f_filetoh_* -- read directly from file
!>   c_filetoh_* -- calculated
!> @todo The distinction above may be temporary, it is easier to merge prefixes than to split them
!>
!> @todo this part of the program needs implementation of the filtering thing

!>

module filetoh
  use read_files
  implicit none

  integer, parameter :: &
   FILETOH_NP=7000, &    !< ?doc? Maximum number of ?
   MAX_FILETOH_JMAX=50,& !< ?doc?
   FILETOH_N=10          !< Number of "filetoh" files
  !> Tied with other constant by relation: @code MAX_FILETOH_JJMAX = MAX_F_FILETOH_JMAX*2-1 @endcode
  integer, parameter :: MAX_FILETOH_JJMAX=MAX_FILETOH_JMAX*2-1

  !=====
  ! Read directly from file
  !=====
  ! These are read by read_filetoh() and processed by filetoh_auh()
  !> ?doc?
  character*80 f_filetoh_titre(FILETOH_N)
  !> ?doc?
  character*11 f_filetoh_ttt(FILETOH_N)
  !> ?doc?
  real*8, dimension(MAX_FILETOH_JMAX, MAX_MODELES_NTOT) :: f_filetoh_th
  !> ?doc?
  real*8, dimension(MAX_FILETOH_JMAX) :: f_filetoh_lambdh
  !> ?doc?
  integer f_filetoh_jmax(FILETOH_N)

  !=====
  ! Calculated for external use
  !=====
  real*8 c_filetoh_tauhi(FILETOH_NP, MAX_MODELES_NTOT)
  integer c_filetoh_dhmi, c_filetoh_dhpi

  !=====
  ! Private variables, shared between routines
  !=====
  private

  real*8, dimension(MAX_FILETOH_JJMAX) :: mi_llambdh, mi_allh, mi_tauhn
  real*8 :: mi_tth(MAX_FILETOH_JJMAX, MAX_MODELES_NTOT)
  real*8 :: mi_ftth(FILETOH_NP)

  !  integer :: jjmax


  SAVE

CONTAINS


  !================================================================================================================================
  !> LECTURE DE LA PROFONDEUR OPTIQUE DANS LA RAIE D H

  subroutine read_filetoh(filename)
    integer unit_
    parameter(unit_=199)
    character*256 filename
    integer j, n

    open(unit=unit_,file=filename,status='old')
    read(unit_,'(a80)') f_filetoh_titre(i)
    read(unit_,'(i4)') f_filetoh_ttt(i)
    read(unit_,'(i4)') f_filetoh_jmax(i)
    read(unit_,'(5f14.3)') (f_filetoh_lambdh(j), j=1,f_filetoh_jmax)
    read(unit_,'(5e12.4)') ((f_filetoh_th(j,n), j=1,f_filetoh_jmax), n=1,modeles_ntot)

    !> @todo Check jmax spill here

    close(unit_)
  end


  !================================================================================================================================
  !> @todo ?what? Find better name
  !>
  !> @note this is originally subroutine "LECTAUH" without the file reading part

  subroutine filetoh_auh(dtot, ttd, ilzero)
    !> ?doc? Number of calculation steps, I think. ISSUE: better explanation
    !> Calculated as: @code dtot = (lfin-lzero)/main_pas + 1.0005 @endcode
    integer, intent(in) :: dtot
    !> integer version of variable lzero in main module
    integer, intent(in) :: ilzero
    !> ?doc? Calculated as: ttd(d) = alzero+main_pas*(d-1)
    real*8, intent(in) :: ttd(FILETOH_NP)

    integer d, j, jj, jma1, n
    real*8 del

    integer jjmax !< @todo may become filetoh_jjmax

    jjmax = 2*f_filetoh_jmax-1
    jma1 = f_filetoh_jmax-1
    do jj = 1, f_filetoh_jmax
      del = f_filetoh_lambdh(f_filetoh_jmax+1-jj)-f_filetoh_lambdh(1)
      mi_llambdh(jj) = f_filetoh_lambdh(f_filetoh_jmax+1-jj)-2*del
    end do
    do jj = f_filetoh_jmax+1, jjmax
      mi_llambdh(jj) = f_filetoh_lambdh(jj-jma1)
    end do
    do n = 1, modeles_ntot
      do jj = 1, f_filetoh_jmax
        mi_tth(jj, n) = f_filetoh_th(f_filetoh_jmax+1-jj, n)
      end do
      do jj = f_filetoh_jmax+1, jjmax
        mi_tth(jj, n) = f_filetoh_th(jj-jma1, n)
      end do
    end do

    !~WRITE(6,'(A80)') f_filetoh_TITRE
    !~WRITE(6,'(A11)') f_filetoh_TTT
    !~WRITE(6,'('' f_filetoh_JMAX='',I3)') f_filetoh_JMAX
    !~WRITE(6,'(2X,5F14.3)') (mi_LLAMBDH(JJ), JJ=1,JJMAX)
    !~WRITE(6,'(2X,5F14.3)') (mi_LLAMBDH(JJ), JJ=1,JJMAX)
    !~
    !~DO N = 1,modeles_NTOT,5
    !~  WRITE(6,'('' N='',I3)') N
    !~  WRITE(6,'(2X,5E12.4)') (mi_TTH(JJ,N), JJ=1,JJMAX)
    !~END DO


    do j = 1,jjmax
      mi_allh(j) = mi_llambdh(j)-ilzero
    end do


    !~ WRITE(6, '('' mi_ALLH(1)='',F8.3,2X,''mi_ALLH(JJMAX)='',F8.3,2X)')
    !~+      mi_ALLH(1),mi_ALLH(JJMAX)
    !~ WRITE(6, '('' JJMAX='',I3,2X,''NTOT='',I3,2X,''DTOT='',I5)')
    !~       JJMAX, modeles_NTOT, DTOT


    do n = 1,modeles_ntot
      do j = 1,jjmax
        mi_tauhn(j) = mi_tth(j,n)
      end do

      call ftlin3h()

      do d = 1,dtot
        c_filetoh_tauhi(d, n) = mi_ftth(d)
      end do
    end do


    !~ !--debugging--!
    !~ WRITE(6,'('' TAUHI(1,1)='',E14.7,2X,''TAUHI(1,NTOT)='',E14.7)')
    !~+ c_filetoh_TAUHI(1,1), c_filetoh_TAUHI(1,modeles_NTOT)
    !~ WRITE(6,'('' TAUHI(DTOT,1)='',E14.7,2X,'
    !~+ //'''TAUHI(DTOT,NTOT)='',E14.7)')
    !~+ c_filetoh_TAUHI(DTOT,1), c_filetoh_TAUHI(DTOT,modeles_NTOT)

    return

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
          if(t-mi_allh(j) ) 3,2,1
          1 continue
          go to 10
          2 ft=mi_tauhn(j)
        if(j .eq. 1) j = j+1
        go to 4

        3 if (j .eq. 1) go to 10
        u0 = mi_tauhn(j)-mi_tauhn(j-1)
        t0 = mi_allh(j)-mi_allh(j-1)
        t1 = t-mi_allh(j-1)

        t2= t1/t0
        dy= u0*t2
        ft= mi_tauhn(j-1) + dy
        mi_ftth(k) = ft
      4 continue

      14 continue

      do k=1,dtot
        if(mi_ftth(k).ne.0.0) go to 20
      end do

      20 c_filetoh_dhmi = k

      if (c_filetoh_dhmi .eq. dtot) c_filetoh_dhmi = 1
      kk1 = c_filetoh_dhmi+1
      do k = kk1,dtot
        if (mi_ftth(k) .eq. 0.0) go to 30
      end do

      30 c_filetoh_dhpi = k

      ! (Paula Coelho 21/11/04) instrucao da Marie Noel
      if (mi_ftth(dtot) .ne. 0.0) c_filetoh_dhpi = dtot

      return

      10 mi_ftth(k) = 0.
      j = j+1

      kk = kq
      kk = kk+1
      if (kq .gt. dtot) go to 14
      go to 24
    end
  end


end module filetoh

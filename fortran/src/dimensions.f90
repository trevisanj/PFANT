! This file is part of PFANT.
!
! PFANT is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Foobar is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with PFANT.  If not, see <http://www.gnu.org/licenses/>.

! Declarations of constants (PARAMETER) used throughout.
!
!   - *all* dimensions of arrays
!   - *all* file I/O UNITs used anywhere (must be declared to avoid unit conflict)

module dimensions
  implicit none


  !=====
  ! "delta lambda" parameters
  !=====

  ! amount to stretch calculation interval (both to the left and to the right)
  !
  real*8, parameter :: LAMBDA_STRETCH = 0.
  ! assumed width of a hydrogen line
  real*8, parameter :: H_LINE_WIDTH = 35.


  !=====
  ! Dimensions related to dfile:filetoh
  !=====

  ! Maximum number of "filetoh" files
  integer, parameter :: MAX_FILETOH_NUM_FILES=13
  ! ?doc?
  integer, parameter :: MAX_FILETOH_JMAX=50
  ! Tied with other constant by relation: @code MAX_FILETOH_JJMAX = MAX_FILETOH_JMAX*2-1 @endcode
  integer, parameter :: MAX_FILETOH_JJMAX = MAX_FILETOH_JMAX*2-1

  !=====
  ! Dimensions related to dfile:dissoc
  !=====

  ! Maximum number of metal rows in dissoc.dat
  ! (number of elements actually used is specified by variable
  ! dissoc_nmetal <= MAX_DISSOC_NMETAL)
  integer, parameter :: MAX_DISSOC_NMETAL=50

  ! Maximum number of molecule rows in dissoc.dat
  ! *Note: bit overdimensioned at the moment, considering that only 21 molecules have been catalogued.
  integer, parameter :: MAX_DISSOC_NMOL=600

  ! Maximum atomic number that can be found in dissoc.dat
  integer, parameter :: MAX_DISSOC_Z = 100

  !=====
  ! Dimensions related to dfile:modeles
  !=====

  ! Maximum possible value of modeles_ntot
  integer, parameter :: MAX_MODELES_NTOT=50

  !=====
  ! Dimensions related to dfile:partit
  !=====

  ! Maximum number of "items" in dfile:partit:
  !   - Maximum value for partit_npar
  !   - Second dimension of partit_tabu
  integer, parameter :: MAX_PARTIT_NPAR=85
  ! Third dimension of partit_TABU
  integer, parameter :: MAX_PARTIT_KMAX=63

  !=====
  ! Dimensions related to dfile:abonds
  !=====

  ! Maximum number of abundances in abonds.dat
  integer, parameter :: MAX_ABONDS_NABOND=100

  !=====
  ! Dimensions related to dfile:absoru
  !=====

  ! Maximum value for absoru2_NM
  integer, parameter :: MAX_ABSORU2_NM=30

  ! Maximum value for absoru2_NR(J)
  integer, parameter :: MAX_ABSORU2_NRR=9  ! Maximum value for each element of absoru2_numset
                                           ! (maximum number of ionization degrees)
  integer, parameter :: MAX_ABSORU2_NUMSET_I=41

  !=====
  ! Dimensions related to dfile:atoms
  !=====

  integer, parameter :: &
   MAX_ATOMS_NBLEND=100000, & ! Half of the maximum the number of rows in dfile:atoms
   MAX_ATOMS_F_NBLEND=20000      ! Maximum number of spectral lines possible within the interval LZERO, LFIN

  !=====
  ! Dimensions related to dfile:molecules
  !=====

  ! Maximum Number of molecules in dfile:molecules
  ! TODO change name to MAX_KM_STH
  integer, parameter :: NUM_MOL=21

  ! Maximum number of transitions ("Set-Of-Lines") for each molecule (Old "NTR")
  ! TODO change name to MAX_KM_NV_PER_MOL
  integer, parameter :: MAX_NV_PER_MOL=200

  integer, parameter :: &
   MAX_KM_LINES_TOTAL=1400000, & ! Maximum number of spectral lines in dfile:molecules
                                   ! pertaining all molecules
   MAX_KM_F_MBLEND=200000 ! Maximum number of spectral lines that can be filtered in at a
                        ! filtering operation performed by filter_molecules()

  !=====
  ! Misc
  !=====
  ! Maximum number of command-line options
  integer, parameter :: MAX_NUM_OPTIONS = 100

  ! dtot maximum: maximum number of points in each calculation interval.
  integer, parameter :: MAX_DTOT=100000
end


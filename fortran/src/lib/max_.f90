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

!> This module contains the declarations for most of the "MAX_" parameters.
!>
!> This module is supposed to contain the declaration for all the "MAX_"
!> parameters that can be considered of "general interest".

module max_

  !=====
  ! "delta lambda" parameters
  !=====

  !> amount to stretch calculation interval (both to the left and to the right)
  real*8, parameter :: LAMBDA_STRETCH = 20.
  !> assumed width of a hydrogen line
  real*8, parameter :: H_LINE_WIDTH = 35.


  !=====
  ! Dimensions related to infile:filetoh
  !=====

  !> Maximum number of "filetoh" files
  integer, parameter :: MAX_FILETOH_NUMFILES=13
  !> ?doc?
  integer, parameter :: MAX_FILETOH_JMAX=50
  !> Tied with other constant by relation: @code MAX_FILETOH_JJMAX = MAX_filetoh_r_JMAX*2-1 @endcode
  integer, parameter :: MAX_FILETOH_JJMAX = MAX_FILETOH_JMAX*2-1

  !=====
  ! Dimensions related to infile:dissoc
  !=====

  !> Maximum number of metal rows in dissoc.dat
  !> (number of elements actually used is specified by variable
  !> dissoc_nmetal <= MAX_DISSOC_NMETAL)
  integer, parameter :: MAX_DISSOC_NMETAL=50

  !> Maximum number of molecule rows in dissoc.dat
  !> @todo ISSUE: bit overdimensioned?? (considering the file has only about 30 molecules)
  integer, parameter :: MAX_DISSOC_NMOL=600

  !> Maximum atomic number that can be found in dissoc.dat
  integer, parameter :: MAX_DISSOC_Z = 100

  !=====
  ! Dimensions related to infile:modeles
  !=====

  !> Maximum possible value of modeles_ntot
  integer, parameter :: MAX_MODELES_NTOT=50

  !=====
  ! Dimensions related to infile:partit
  !=====

  !> Maximum number of "items" in infile:partit:
  !> @li Maximum value for partit_npar
  !> @li Second dimension of partit_tabu
  integer, parameter :: MAX_PARTIT_NPAR=85
  !> Third dimension of partit_TABU
  integer, parameter :: MAX_PARTIT_KMAX=63

  !=====
  ! Dimensions related to infile:abonds
  !=====

  !> Maximum number of abundances in abonds.dat
  integer, parameter :: MAX_ABONDS_NABOND=100

  !=====
  ! Dimensions related to infile:absoru
  !=====

  !> Maximum value for absoru2_NM
  integer, parameter :: MAX_ABSORU2_NM=30

  !> Maximum value for absoru2_NR(J)
  integer, parameter :: MAX_ABSORU2_NRR=9  !> Maximum value for each element of absoru2_NUMSET
  integer, parameter :: MAX_ABSORU2_NUMSET_I=41

  !=====
  ! Dimensions related to infile:atomgrade
  !=====

  integer, parameter :: &
   MAX_ATOMGRADE_R_NBLEND=14000, & !< Half of the maximum the number of rows in infile:atomgrade
   MAX_ATOMGRADE_NBLEND=8000      !< Maximum number of spectral lines possible within the interval LZERO, LFIN

  !=====
  ! Dimensions related to infile:molecules
  !=====

  !> Number of molecules configured in the program.
  !> Conceptually, this should be defined in molecules.f, but there would be cyclic USEs
  integer, parameter :: NUM_MOL=21

  !> Maximum number of transitions ("Set-Of-Lines") for each molecule (Old "NTR")
  integer, parameter :: MAX_SOL_PER_MOL=200

  integer, parameter :: &
   MAX_KM_R_LINES_TOTAL=1400000, & !< Maximum number of spectral lines in infile:molecules
                                   !< pertaining all molecules
   MAX_KM_F_MBLEND=200000 !< Maximum number of spectral lines that can be filtered in at a
                        !< filtering operation performed by filter_molecules()

  !=====
  ! Misc
  !=====
  !> Maximum number of command-line options
  integer, parameter :: MAX_NUM_OPTIONS = 50

  !> dtot maximum: maximum number of points in each calculation interval.
  integer, parameter :: MAX_DTOT=7000
end


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

!> Reading routines and variable declarations for infile:absoru2

module reader_absoru2
  use logging
  use max_
  implicit none

  integer absoru2_nm,    & !< NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISATION
          absoru2_nmeta, & !< NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
          absoru2_numset(2) !< NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
                            !< POUR H,HE ET HE+

  character*2 :: absoru2_nomet(MAX_ABSORU2_NM) !< NOM DE L'ELEMENT; not used
  real*8 absoru2_abmet, & !< ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
         absoru2_abhel    !< ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
  !> @todo ISSUE: i am not sure about these character*4, made this from the format below
  character*4 absoru2_titre(17) !< ?doc?

  !> Two possibilities
  !> @li IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
  !> @li IUNITE=' NOYAU H' SI ON VEUT CALCULER KAPPA PAR NOYAU D'HYDROGENE
  character*4 absoru2_iunite(2)

  integer, dimension(MAX_ABSORU2_NM) :: &
   absoru2_nr !< NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
  real*8, dimension(MAX_ABSORU2_NM) :: &
   absoru2_zp, & !< ZP=NBR. D'ABONDANCE DE L'ELEMENT
   absoru2_zm    !< ZM=POIDS MOLECULAIRE DE L'ELEMENT
  real*8, dimension(MAX_ABSORU2_NM, MAX_ABSORU2_NRR) :: &
    absoru2_xi,&!< XI(J,I) = POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATION
    absoru2_pf  !< PF(J,I) = FONCTION DE PARTITION DE L'ELEMENT J AU STADE D'IONISATION
  real*8 absoru2_wi(MAX_ABSORU2_NUMSET_I, 2) !< ?doc?

contains
  !=======================================================================================
  !> Reads file infile:absoru2 to fill variables absoru2_*
  !>
  !> @note Variables absoru2_ZP, absoru2_XI, and absoru2_PF
  !>       undergo transformation after their values are read from file.
  !>
  !> @note Historically, this routine is in essence the old routine called "LECTUR"
  !>
  !> CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
  !> PUIS LES DONNEES CORRESPONDANTS AUX ABSORBANTS METALLIQUES SI NECE

  subroutine read_absoru2(path_to_file)
    implicit none
    integer UNIT_
    parameter(UNIT_=199)
    character(len=*) :: path_to_file
    character*3 neant
    integer nion, i, ith, j, nrr, nset

    open(unit=UNIT_,file=path_to_file, status='old')

    ! ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
    ! ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
    read (UNIT_,'(2e15.7)') absoru2_abmet, absoru2_abhel


    ! NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISATION
    ! NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
    ! IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
    ! IUNITE=' NOYAU H'  ''    ''    ''       ''      NOYAU D'HYDROGENE
    read (UNIT_,'(2i2, 19a4)') absoru2_nm, absoru2_nmeta, &
          (absoru2_iunite(i),i=1,2), (absoru2_titre(i),i=1,17)

    !#spill_check: checks if exceeds maximum number of elements allowed
    if (absoru2_nm .gt. MAX_ABSORU2_NM) then
      call pfant_halt('read_absoru2(): nm='//int2str(absoru2_nm)//&
         ' exceeded maximum of MAX_ABSORU2_NM='//int2str(MAX_ABSORU2_NM))
    end if


    ! LECTURE DE LA TABLE D'IONISATION CHOISIE
    do j = 1, absoru2_nm
      read (UNIT_, '(3x,i3,2e16.5)') absoru2_nr(j), absoru2_zp(j), absoru2_zm(j)
      absoru2_zp(j) = 10**absoru2_zp(j)

      ! NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
      ! ZP=NBR. D'ABONDANCE DE L'ELEMENT
      ! ZM=POIDS MOLECULAIRE DE L'ELEMENT
      nrr = absoru2_nr(j)

      ! checks if exceeds maximum number of elements allowed
      if (nrr .gt. MAX_ABSORU2_NRR) then
        write(lll,*) 'read_absoru2(): j = ', j, 'nr=', nrr, ' exceeded maximum of', MAX_ABSORU2_NRR
        call pfant_halt(lll)
      end if

      do i = 1, nrr
        ! neant="nothing"
        ! NION is also not used
        read (UNIT_, '(a3,a2,i1,2e16.5)') neant, absoru2_nomet(j), &
         nion, absoru2_xi(j,i), absoru2_pf(j,i)

        ! ON LIT NR CARTES CONTENANT CHACUNE LE POTENTIEL D'IONISATION ET LA
        ! FONCTION DE PARTITION(LOG10(2UI+1)/UI)DE CHAQUE DEGRE D'IONISATION
        ! CES VALEURS SONT LUES DANS L'ORDRE CROISSANT DU DEGRE D'IONISATION
        ! NOMET  =NOM DE L'ELEMENT
        ! NION   =SON ETAT D'IONISATION
        ! XI(J,I)=POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATIO
        ! PF(J,I)=FONCTION DE PARTITION         ''   ''     ''      ''   ''

        absoru2_xi(j, i) = absoru2_xi(j, i)*2.302585
        absoru2_pf(j, i) = absoru2_pf(j, i)*2.302585
      end do
    END DO


    read (UNIT_, '(2i2)') (absoru2_numset(ith), ith=1,2)

  !> @todo ISSUE: I am not sure if this last part is being read correcly. Perhaps I didn't de-spag right. Anyway, the test verbose is not good.


    !#spill_check: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(1) .gt. MAX_ABSORU2_NUMSET_I) then
      call pfant_halt('read_absoru2(): numset(1) = '//int2str(absoru2_numset(1))//&
       ' exceeded maximum of MAX_ABSORU2_NUMSET_I='//int2str(MAX_ABSORU2_NUMSET_I))
    end if
    !#spill_check: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(2) .gt. MAX_ABSORU2_NUMSET_I) then
      call pfant_halt('read_absoru2(): numset(2) = '//int2str(absoru2_numset(2))//&
       ' exceeded maximum of MAX_ABSORU2_NUMSET_I='//int2str(MAX_ABSORU2_NUMSET_I))
    end if


    ! NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
    ! POUR H,HE ET HE+
    ! PREMIERE LISTE POUR TH.LE.0.8 ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
    do ith = 1,2
      nset = absoru2_numset(ith)
      read (UNIT_,'(8f10.1)') (absoru2_wi(i,ith),i=1,nset)
    end do
  end
end

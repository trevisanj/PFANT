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

!> Subroutines FLIN1() and FLINH()
!>
!> Calcul du flux ou de l'intensite par la methode d'integration
!> a 6 pts (ou 13pts) de R.Cayrel (these).
!> nouvelle methode de calcul de to . flin_TO(1) est calcule et
!> est different de 0 (On pose flin_TO(0)=0)   -Avril 1988-
!>
!> Originally by Roger Cayrel
!>
!> Outputs ONLY in module variables: flin_TO, flin_F
!> NO LONGER modifying arguments F and CAVA (they are no longer arguments; CAVA (error code) extinct)

!> @ingroup gr_math, gr_data

MODULE FLIN
  IMPLICIT NONE

  !=====
  > Output variables
  !=====
  REAL*8, DIMENSION(0:50) flin_TO
  REAL*8 flin_F

  PRIVATE, REAL*8 :: TD2, TD,TP, CD, CP, C1, C2, C3
  DIMENSION TD2(26),TD(6),TP(7),CD(6),CP(7),C1(13),C2(12),C3(12)


  DATA TD /0.038,0.154,0.335,0.793,1.467,3.890 /
  DATA CD /0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
  DATA TP /0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
  DATA CP /0.176273,0.153405,0.167016,0.135428,0.210244,0.107848, 0.049787/
  DATA TD2 /0.,.05,.1,.15,.3,.45,.60,.80,1.,1.2,1.4,1.6,1.8,2., &
   2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
  DATA C1 /.032517,.047456,.046138,.036113,.019493,.011037,.006425, &
   .003820,.002303,.001404,.000864,.001045,.002769/
  DATA C2 /.111077,.154237,.143783,.108330,.059794,.034293, &
   .020169,.012060,.007308,.004473,.002761,.002757/
  DATA C3 /.023823,.030806,.027061,.019274,.010946,.006390, &
   .003796,.002292,.001398,.000860,.000533,.000396/



  LOGICAL, PRIVATE, PARAMETER :: MODE_FLINH = .TRUE., &
                                 MODE_FLIN1 = .FALSE.

  !> This variable is needed just to fill in the allocation requisites for FLIN_() in FLIN1 mode
  REAL*8, PRIVATE, DIMENSION(MAX_modeles_NTOT) :: DUMMY_TAUHD

CONTAINS
  !---------------------------------------------------------------------------------------------------
  SUBROUTINE FLIN1(KAP, B, NH, NTOT, PTDISK, MU, KIK)
    INTEGER KIK
    LOGICAL PTDISK
    REAL NH, KAP, MU
    REAL*8, DIMENSION(0:MAX_modeles_NTOT) :: B
    REAL*8, DIMENSION(MAX_modeles_NTOT) :: NH, KAP
    CALL FLIN_((KAP, B, NH, NTOT, PTDISK, MU, KIK, DUMMY_TAUHD, MODE_FLIN1)
  END
      

  !---------------------------------------------------------------------------------------------------
  !>
  !> Two differences from FLIN1()
  !> 1) adds TAUHD vector to flin_TO
  !> 2) ignores PTDISK ISSUE!!!
  !>
  SUBROUTINE FLINH(KAP, B, NH, NTOT, PTDISK, MU, KIK, TAUHD)
    INTEGER KIK
    LOGICAL PTDISK
    REAL NH, KAP, MU
    REAL*8, DIMENSION(0:MAX_modeles_NTOT) :: B
    REAL*8, DIMENSION(MAX_modeles_NTOT) :: NH, KAP, TAUHD
    CALL FLIN_((KAP, B, NH, NTOT, PTDISK, MU, KIK, TAUHD, MODE_FLINH)
  END


  !> Generic routine, called by FLIN1() and FLINH()
  !>
  !> @todo ISSUE BIG using 7 points in FLIN1 MODE!!!!!!!!!!!!!!!
  
  SUBROUTINE FLIN_(KAP, B, NH, NTOT, PTDISK, MU, KIK, TAUHD, MODE_)
    USE READ_FILES
    IMPLICIT NONE
    !> ?
    REAL*8, INTENT(IN) :: KAP(MAX_modeles_NTOT)
    !> ?
    REAL*8, INTENT(IN) :: B(0:MAX_modeles_NTOT)
    !> Source is probably @ref read_files::modeles_nh
    REAL*8, INTENT(IN) :: NH(MAX_modeles_NTOT)
    !> Source is probably @ref read_files::modeles_NTOT
    INTEGER, INTENT(IN) :: NTOT
    !> Source is probably @ref read_files::main_PTDISK
    !> @li if .TRUE. : 7 points
    !> @li .FALSE.   : 6 points
    LOGICAL, INTENT(IN) :: PTDISK
    !> Source is probably @REF read_files::main_MU
    REAL*8, INTENT(IN) :: MU
    !> (old "IOP") accepts 0 or 1.
    !> @li if 0, uses the 6/7 point formulation
    !> @li if 1, uses the 26-point formulation
    INTEGER, INTENT(IN) :: KIK
    !> Used only in FLINH mode
    REAL*8, INTENT(IN) :: TAUHD(MAX_modeles_NTOT)
    !> Internal, either @ref mode_flin1 or @ref mode_flinh
    LOGICAL, INTENT(IN) :: MODE_

    REAL*8, DIMENSION(13) :: FP, CC, BB
    REAL*8, DIMENSION(26) :: BBB

    ! CALCUL DE flin_TO
    EPSI=0.05
    flin_TO(0)=0.
    flin_TO(1)=NH(1)*(KAP(1)-(KAP(2)-KAP(1))/(NH(2)-NH(1))*NH(1)/2.)
    CALL INTEGRA(NH,KAP,flin_TO, NTOT,flin_TO(1))
    IF (MODE_ .EQ. MODE_FLINH) THEN  ! FLINH() mode only!!!
      DO N=1,NTOT
      flin_TO(N) = flin_TO(N)+TAUHD(N)
      END DO
    END IF

    ! CALCUL DU FLUX
    IF (KIK .EQ. 0) THEN
      ! FORMULE A 6 OU 7 PTS
      IF(PTDISK) THEN
        IPOINT=7
        TOLIM=4.0
      ELSE
        IF (MODE_ .EQ. MODE_FLIN1) THEN
          IPOINT = 7  !> @todo ISSUE BIG !!!!! I kept this behaviour until I get feedback from BLB
        ELSE
          IPOINT = 6
        END IF
        TOLIM=3.89
      END IF

      ! On verifie que le modele n'est pas trop court
      IF (flin_TO(NTOT) .LT. TOLIM) THEN
        !> @todo MAKE IT FALL HERE!!!!! (TEST THIS)

        WRITE(LLL,1504)
        CALL LOG_HALT(LLL)
        WRITE(LLL,1503) NTOT, flin_TO(NTOT)
        CALL LOG_HALT(LLL)
        CALL LOG_HALT('')
        WRITE(LLL,131) TTD_D
        CALL PFANT_HALT(LLL)
      END IF

      2 CONTINUE
      DO L=1,IPOINT
        IF(PTDISK) THEN
          TT(L) = TP(L)*MU
          CC(L)=CP(L)
        ELSE
          TT(L) = TD(L)
          CC(L) = CD(L)
        END IF
      END DO

      flin_F=0.
      DO  L=1,IPOINT
        BB(L)=FAITK30(TT(L), flin_TO, B, NTOT)
        FP(L)=CC(L)*BB(L)
        flin_F=flin_F+FP(L)
      END DO
      RETURN

    ELSEIF (KIK .EQ. 1) THEN
      ! FORMULE A 26 PTS (NE MARCHE QUE POUR LE FLUX!)
      ! (13PTS +PTS MILIEU)
      
      !> @todo test this error condition, better: put this verification in somewhere at startup, but has to be after READ_MAIN()
      IF(PTDISK) THEN
        1500 FORMAT('LE SP FLIN_ NE PEUT CALCULER L INTENSITE EN 1 PT ', &
                    'DU DISQUE AVEC LA FORMULE A 26PTS (UTILISER 7PTS KIK=0)')
        WRITE(6,1500)
        STOP
      END IF
      TOLIM=5.487  ! Le modele doit aller au moins a une prof TOLIM

      IF(flin_TO(NTOT) .LT. TOLIM) THEN
        !> @todo MAKE IT FALL HERE!!!!! (TEST THIS)

        WRITE(LLL,1504)
        CALL LOG_HALT(LLL)
        WRITE(LLL,1503) NTOT, flin_TO(NTOT)
        CALL LOG_HALT(LLL)
        CALL LOG_HALT('')
        WRITE(LLL,131) TTD_D
        CALL PFANT_HALT(LLL)
      END IF

      DO L=1,26
        T=TD2(L)
        BBB(L) = FAITK30(TD2(L),flin_TO,B,NTOT)
      END DO

      DO M=1,12
        L=2*M - 1
        BB(M) = BBB(L+1)
        FP(M) = C1(M)*BBB(L) + C2(M)*BBB(L+1) + C3(M)*BBB(L+2)
        CC(M) = C2(M)
      END DO
      
      FP(13) = C1(13)*BBB(26)
      BB(13) = BBB(26)
      CC(13) = C1(13)
      ! CES BB ET CC NE SERVENT QUE POUR LES SORTIES (PAS AU CALCUL)
      
      flin_F=0.
      DO L=1,13
        flin_F = flin_F+FP(L)
      END DO
      RETURN
    ELSE
      CALL PFANT_HALT('Bad KIK (must be 0 or 1)')
    END IF  !(fin du IF KIK)


    1503  FORMAT(I10,5X,3HTO=,F10.4)
    1504  FORMAT(18H MODELE TROP COURT)
    131   FORMAT(' ENNUI AU CALCUL DU FLUX (CF LIGNE PRECEDENTE)', ' A LAMBD=',F10.3)
  END
END MODULE FLIN

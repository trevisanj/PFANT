
C-------------------------------------------------------------------------------
C 
c     calcul du flux ou de l'intensite par la methode d'integration
c     a 6 pts (ou 13pts) de R.Cayrel (these).
c     nouvelle methode de calcul de to . flin_TO(1) est calcule et
c     est different de 0 (On pose flin_TO(0)=0)   -Avril 1988-
C
C Originally by Roger Cayrel
C
C Outputs ONLY in module variables: flin_TO, flin_F
C NO LONGER modifying arguments F and CAVA (they are no longer arguments; CAVA (error code) extint)
      MODULE FLIN
      IMPLICIT NONE
      

      !=====
      ! Output variables
      !=====
      REAL*8, DIMENSION(0:50) flin_TO
      REAL*8 flin_F
     

      
      PRIVATE, REAL*8 :: TD2, TD,TP, CD, CP, C1, C2, C3
      DIMENSION TD2(26), TD(6),TP(7),CD(6),CP(7),C1(13),C2(12),C3(12)

      
      DATA TD /0.038,0.154,0.335,0.793,1.467,3.890 /
      DATA CD /0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
      DATA TP /0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
      DATA CP /0.176273,0.153405,0.167016,0.135428,0.210244,0.107848,
     10.049787/
      DATA TD2 /0.,.05,.1,.15,.3,.45,.60,.80,1.,1.2,1.4,1.6,1.8,2.,
     1 2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
      DATA C1 /.032517,.047456,.046138,.036113,.019493,.011037,.006425,
     1 .003820,.002303,.001404,.000864,.001045,.002769/
      DATA C2 /.111077,.154237,.143783,.108330,.059794,.034293,
     1 .020169,.012060,.007308,.004473,.002761,.002757/
      DATA C3 /.023823,.030806,.027061,.019274,.010946,.006390,
     1 .003796,.002292,.001398,.000860,.000533,.000396/

      
      
      LOGICAL, PRIVATE, PARAMETER :: MODE_FLINH = .TRUE.,
     +                               MODE_FLIN1 = .FALSE.
      
      ! This variable is needed just to fill in the allocation requisites for FLIN_() in FLIN1 mode
      REAL*8, PRIVATE, DIMENSION(MAX_modeles_NTOT) :: DUMMY_TAUHD

C     ========
      CONTAINS
C     ========
      

C---------------------------------------------------------------------------------------------------
      SUBROUTINE FLIN1(KAP, B, NH, NTOT, PTDISK, MU, KIK)
      INTEGER KIK
      LOGICAL PTDISK
      REAL NH, KAP, MU
      REAL*8, DIMENSION(0:MAX_modeles_NTOT) :: B
      REAL*8, DIMENSION(MAX_modeles_NTOT) :: NH, KAP
      CALL FLIN_((KAP, B, NH, NTOT, PTDISK, MU, KIK,
     + DUMMY_TAUHD, MODE_FLIN1)
      END
      

C---------------------------------------------------------------------------------------------------
C
C Two differences from FLIN1()
C 1) adds TAUHD vector to flin_TO
C 2) ignores PTDISK ISSUE!!!
C
      SUBROUTINE FLINH(KAP, B, NH, NTOT, PTDISK, MU, KIK, TAUHD)
      INTEGER KIK
      LOGICAL PTDISK
      REAL NH, KAP, MU
      REAL*8, DIMENSION(0:MAX_modeles_NTOT) :: B
      REAL*8, DIMENSION(MAX_modeles_NTOT) :: NH, KAP, TAUHD
      CALL FLIN_((KAP, B, NH, NTOT, PTDISK, MU, KIK,
     + TAUHD, MODE_FLINH)
      END


C Generic routine, called by FLIN1() and FLINH()
C KAP -- 
C B -- 
C NH -- probably modeles_NH
C NTOT -- probably modeles_NTOT
C PTDISK -- (logical) probably main_PTDISK
C           .TRUE. : 7 points
C           .FALSE.: 6 points ISSUE BIG using 7 points in FLIN1 MODE!!!!!!!!!!!!!!!
C MU -- probably main_MU
C KIK -- (old "IOP") (integer), accepts 0 or 1
C        if 0, uses the 6/7 point formula
C TAUHD -- used only in FLINH mode
C TTD_D -- just for logging purpose, it will be shown if the system halts
C MODE_ -- (logical; internal) .TRUE. : behaves as FLINH
C                              .FALSE.: behaves as FLIN1
      SUBROUTINE FLIN_(KAP, B, NH, NTOT, PTDISK, MU, KIK, TAUHD, MODE_)
      USE READ_FILES
      IMPLICIT NONE

      INTEGER KIK
      LOGICAL PTDISK, MODE_
      REAL NH, KAP, MU
      REAL*8, DIMENSION(0:MAX_modeles_NTOT) :: B
      REAL*8, DIMENSION(MAX_modeles_NTOT) :: NH, KAP, TAUHD
      REAL, DIMENSION(13) :: FP, CC, BB
      REAL, DIMENSION(26) :: BBB

C
C           CALCUL DE flin_TO
C
      EPSI=0.05
      flin_TO(0)=0.
        flin_TO(1)=NH(1)*(KAP(1)-(KAP(2)-KAP(1))/(NH(2)-NH(1))*NH(1)/2.)
        CALL INTEGRA(NH,KAP,flin_TO, NTOT,flin_TO(1))
        IF (MODE_ .EQ. MODE_FLINH) THEN  ! FLINH() mode only!!!
          DO N=1,NTOT
            flin_TO(N) = flin_TO(N)+TAUHD(N)
          END DO
        END IF
C
C           CALCUL DU FLUX
      IF (KIK .EQ. 0) THEN
        ! FORMULE A 6 OU 7 PTS
        IF(PTDISK) THEN
          IPOINT=7
          TOLIM=4.0
        ELSE
          IF (MODE_ .EQ. MODE_FLIN1) THEN
            IPOINT = 7  ! ISSUE BIG !!!!! I kept this behaviour until I get feedback from BLB
          ELSE
            IPOINT = 6
          END IF
          TOLIM=3.89
        END IF

c     on verifie que le modele n'est pas trop court
        IF (flin_TO(NTOT) .LT. TOLIM) THEN
          ! TODO MAKE IT FALL HERE!!!!! (TEST THIS)

          WRITE(LLL,1504)
          CALL LOG_HALT(LLL)
          WRITE(LLL,1503) NTOT, flin_TO(NTOT)
          CALL LOG_HALT(LLL)
          WRITE(LLL,1501)
          CALL LOG_HALT(LLL)
          WRITE(LLL,131) TTD_D
          CALL PFANT_HALT(LLL)
        END IF

2       DO  L=1,IPOINT
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
C     FORMULE A 26 PTS (NE MARCHE QUE POUR LE FLUX!)
C           (13PTS +PTS MILIEU)
        ! TODO test this error condition
        IF(PTDISK)   then
          WRITE(6,1500)
          STOP
        END IF
        TOLIM=5.487  ! Le modele doit aller au moins a une prof TOLIM

        IF(flin_TO(NTOT) .LT. TOLIM) THEN
          ! TODO MAKE IT FALL HERE!!!!! (TEST THIS)

          WRITE(LLL,1504)
          CALL LOG_HALT(LLL)
          WRITE(LLL,1503) NTOT, flin_TO(NTOT)
          CALL LOG_HALT(LLL)
          WRITE(LLL,1501)
          CALL LOG_HALT(LLL)
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
C     CES BB ET CC NE SERVENT QUE POUR LES SORTIES (PAS AU CALCUL)
        flin_F=0.
        DO L=1,13
          flin_F = flin_F+FP(L)
        END DO
        RETURN
      ELSE
        CALL PFANT_HALT('Bad KIK (must be 0 or 1)')
      END IF  !(fin du IF KIK)

1500  FORMAT('   LE SP FLIN1 NE PEUT CALCULER L INTENSITE EN 1 PT ',
     1 'DU DISQUE AVEC LA FORMULE A 26PTS (UTILISER 7PTS KIK=0)' )
1501  FORMAT(1H //)
1502  FORMAT(5(F7.3,F5.2,2X))
1503  FORMAT(I10,5X,3HTO=,F10.4)
1504  FORMAT(18H MODELE TROP COURT)
131   FORMAT(' ENNUI AU CALCUL DU FLUX (CF LIGNE PRECEDENTE)',
     1   ' A LAMBD=',F10.3)

      END


      END MODULE FLIN

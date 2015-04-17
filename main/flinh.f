
C Note: variable TAUHD, not present in FLIN1()
      SUBROUTINE FLINH (KAP,B,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,TAUHD,F,KIK,CAVA)
      USE READ_FILES
      IMPLICIT NONE

      LOGICAL main_PTDISK
      REAL   modeles_NH,KAP,main_MU
      INTEGER CAVA
      DIMENSION B(0:50),  TO_TOTO(0:50)
      DIMENSION modeles_NH(50),KAP(50),BBB(26)
      DIMENSION TAUHD(50)

      COMMON /TOTO/ TO_TOTO
C
C           CALCUL DE TO_TOTO
C
      CAVA=0
      EPSI=0.05
      TO_TOTO(0)=0.
        TO_TOTO(1)=modeles_NH(1)*(KAP(1)-(KAP(2)-KAP(1))/(modeles_NH(2)-modeles_NH(1))*modeles_NH(1)/2.)
        CALL INTEGRA(modeles_NH,KAP,TO_TOTO,modeles_NTOT,TO_TOTO(1))
            DO N=1,modeles_NTOT
            TO_TOTO(N)=TO_TOTO(N)+TAUHD(N)
            END DO
C
C           CALCUL DU FLUX
      IF (KIK .EQ. 0) THEN
C               FORMULE A 6 OU 7 PTS
        IF(main_PTDISK) THEN
          IPOINT=7
          TOLIM=4.0
        ELSE
          IPOINT=6
          TOLIM=3.89
        END IF

c     on verifie que le modele n'est pas trop court
        IF (TO_TOTO(modeles_NTOT) .LT. TOLIM) THEN
          WRITE(6,1504)
          WRITE(6,1503) modeles_NTOT, TO_TOTO(modeles_NTOT)
          WRITE(6,1501)
          CAVA=2
          RETURN
        END IF

2       DO  L=1,IPOINT
          IF(main_PTDISK) THEN
            TT(L) = TP(L)*main_MU
            CC(L)=CP(L)
          ELSE
            TT(L) = TD(L)
            CC(L) = CD(L)
          END IF
        END DO

        F=0.
        DO  L=1,IPOINT
          BB(L)=FAITK30(TT(L), TO_TOTO, B, modeles_NTOT)
          FP(L)=CC(L)*BB(L)
          F=F+FP(L)
        END DO
        RETURN

      ELSEIF (KIK .EQ. 1) THEN
C     FORMULE A 26 PTS (NE MARCHE QUE POUR LE FLUX!)
C           (13PTS +PTS MILIEU)
        IF(main_PTDISK)   then
          WRITE(6,1500)
          STOP
        END IF
        TOLIM=5.487  ! Le modele doit aller au moins a une prof TOLIM
        IF(TO_TOTO(modeles_NTOT).LT. TOLIM) THEN
          WRITE(6,1504)
          WRITE(6,1503) modeles_NTOT,TO_TOTO(modeles_NTOT)
          WRITE(6,1501)
          CAVA=2
          RETURN
        END IF

        DO L=1,26
          T=TD2(L)
          BBB(L) = FAITK30(TD2(L),TO_TOTO,B,modeles_NTOT)
        END DO

        DO M=1,12
          L=2*M - 1
          BB(M)=BBB(L+1)
          FP(M)=C1(M)*BBB(L) + C2(M)*BBB(L+1) + C3(M)*BBB(L+2)
          CC(M)=C2(M)
        END DO
        FP(13)=C1(13)*BBB(26)
        BB(13)=BBB(26)
        CC(13)=C1(13)
C     CES BB ET CC NE SERVENT QUE POUR LES SORTIES (PAS AU CALCUL)
        F=0.
        DO L=1,13
          F=F+FP(L)
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
      END


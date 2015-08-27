      SUBROUTINE ABONDRAI(ELE,ABO,nabond,ELEM,ABOND,nblend)
      DIMENSION ELE(100),ABO(100),ELEM(50),ABOND(50)
      DO  K=1,NBLEND
            DO  J=1,NABOND
            IF(ELE(J).EQ.ELEM(K))  GO TO 14
            END DO   !FIN BCLE SUR J
      WRITE(6,106)     ELEM(K)
      STOP
14    ABOND(K)=ABO(J)
      END DO   !FIN BCLE SUR K
      RETURN
106   FORMAT('     MANQUE L ABONDANCE DU  ',A2                  )
      end


      SUBROUTINE BKF(NH,TETA,PE,PG,NTOT,
     1              LAMBD,B,ALPH,PHN,PH2,KC,PTDISK,MU,KIK,FC)
      REAL LAMBD,NH,MU,NU,KC,KB
      LOGICAL PTDISK
      INTEGER CAVA
      DIMENSION B(0:50), TO(0:50)
      DIMENSION NH(50),TETA(50),PE(50),PG(50),KC(50),TOTKAP(2),
     2 ALPH(50),PHN(50),PH2(50)
C
      COMMON /LECT1/AMET,BHE/LECT2/ZP(30),ZM(30),BIDA(85)/ABSO2/NMETA
     1  /SAPE/BIDB(5),ZNU(30)/SAPU/BIDC(3),ZNH(12)/ABSO1/NM
C     COMMON ENTRE LE PROGRAMME PRINCIPAL ET LE SP FLIN1
      COMMON/TOTO/TO    /FAIL/ERR(50)
      C=2.997929E+10
      H=6.6252E-27
      KB=1.38046E-16
      NU= C* 1.E+8 /LAMBD
      AHNU= H*NU
      C3=(2*AHNU) * (NU/C)**2
            DO   N=1,NTOT
            T=5040./TETA(N)
            ALPH(N)=EXP(-AHNU/(KB*T))
            B(N)= C3 * (ALPH(N)/(1.-ALPH(N)))
       CALL ABSORU(LAMBD,TETA(N),ALOG10(PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
            PHN(N)=  ZNH (NMETA+4) *KB * T
            PH2(N) =ZNH (NMETA+2) *KB * T
            KC(N)=TOTKAP(1)
            END DO
      TET0= FTETA0(PG,TETA)   ! on extrapole TETA pour NH=0
      T=5040./TET0
      ALPH0=EXP(-AHNU/(KB*T))
      B(0)= C3 * (ALPH0/(1.-ALPH0))
      CALL FLIN1 (KC,B,NH,NTOT,PTDISK,MU,FC,KIK,CAVA)
            IF(CAVA.GT.0)      THEN
            WRITE(6,132) CAVA
            WRITE(6,135) (I,TO(I),ERR(I),I=1,NTOT)
            IF (CAVA .GT. 1) STOP
            END IF
      RETURN
132   FORMAT(' ENNUI AU CALCUL DU FLUX CONTINU       CAVA=',I3)
135   FORMAT(5(I4,F7.3,F5.2))
      END


      SUBROUTINE EDIGA
      INTEGER ELMAX
      REAL LAMBDA,KKIEX,NHE,LSOL(5),IKIEX(4)
        CHARACTER*2 BLC
      DIMENSION EEL(40), TITRE(5)
      COMMON/COM9/ EELEM(40),ELMAX,LAMBDA(5),LMAX,KKIEX(4),KIMAX,LECS
      COMMON /COM6/TEFF,TETAEF,GLOG,ASASOL,NHE,TITRE
      COMMON/CO/ GALOG(2,5,40,4),GALOGS(2,5,40,4), DIFGA(2,5,40,4)
      DATA BLC/'  '/
      CHARACTER*10 GETFILE,GSOFILE
      COMMON/COM7/GETFILE,GSOFILE
      ASALOG=ALOG10(ASASOL)
c     initialisation
      DO 6 J=1,40   ! maximum possible d'elements: 40
      DO 6 I=1,2    ! 2 degres d'ionisation
      DO 6 K=1,4    ! 4 potentiels d'excitation
      DO 6 L=1,5    ! 5 longueurs d'onde
6     DIFGA(I,L,J,K)=99.999
      IF(JDEF.EQ.211939)   GO TO 20
      OPEN (UNIT=9,FILE=GETFILE,STATUS='NEW')
20    WRITE(9,102) TETAEF,GLOG,ASALOG,NHE,(TITRE(I),I=1,5)
      WRITE(9,112) LMAX,ELMAX,KIMAX
      WRITE(9,113)(LAMBDA(L),L=1,LMAX)
      WRITE(9,107)(KKIEX(K),K=1,KIMAX)
      IF(LECS.EQ.3)   WRITE(6,105)
      IF(LECS.LT.3)   WRITE(6,104)
      IF(LECS.GE.2)   GO TO23
      OPEN (UNIT=19,FILE=GSOFILE,STATUS='OLD')
      READ(19,102) TSOL
      READ(19,112)ILMAX,IEMAX,IKIMAX
      READ(19,113) (LSOL(L),L=1,ILMAX)
      READ(19,107) (IKIEX(K),K=1,IKIMAX)
      EPS1=0.1
      EPS2=0.01
      DIFL=0
      DIFK=0
            DO 11 L=1,LMAX
11          DIFL=DIFL+ ABS(LAMBDA(L)-LSOL(L))
            DO 12 K=1,KIMAX
12          DIFK=DIFK+ ABS(KKIEX(K)-IKIEX(K))
      IF ( (DIFL.LT.EPS1) .AND. (DIFK.LT.EPS2) )   GO TO 15
      WRITE(6,114)
      WRITE(6,113) (LSOL(L),L=1,ILMAX)
      WRITE(6,107) (IKIEX(K),K=1,IKIMAX)
15    DO 10 IE=1,IEMAX
      READ(19,103) EEL(IE)
C     WRITE(6,109) EEL(IE)
            DO 9 I=1,2
            DO 9 K=1,4
      READ(19,108) (GALOGS(I,L,IE,K),L=1,5)
9           CONTINUE
10    CONTINUE
23    IF(LECS.LE.2)   GO TO 3
      DO 21 I=1,2
      DO 21 L=1,LMAX
      DO 21 J=1,ELMAX
      DO 21 K=1,KIMAX
21    DIFGA(I,L,J,K)=GALOG(I,L,J,K)
      GO TO 25
3     DO 5 J=1,ELMAX
      DO 2 IE=1,IEMAX
      IF(EEL(IE).NE.EELEM(J))  GO TO 2
      DO 4I=1,2
      DO 4 K=1,KIMAX
      DO 4 L=1,LMAX
4     DIFGA(I,L,J,K)=GALOG(I,L,J,K)-GALOGS(I,L,IE,K)
      GO TO 5
2     CONTINUE
      WRITE(6,100)  EELEM(J)
5     CONTINUE
25    DO 7 J=1,ELMAX
      WRITE(6,109)EELEM(J)
      WRITE(6,101)    ((DIFGA(1,L,J,K),L=1,5),K=1,4)
      WRITE(6,106)
      WRITE(6,101)    ((DIFGA(2,L,J,K),L=1,5),K=1,4)
      WRITE(9,103)  EELEM(J)
      DO 1 I=1,2
            DO K=1,KIMAX
            WRITE(9,108) (DIFGA(I,L,J,K),L=1,LMAX)
            END DO
1     CONTINUE
7     CONTINUE
      RETURN
100   FORMAT (10X,'MANQUE LOG GAMMA SOLEIL POUR',2X,A2)
101   FORMAT( (5(1X,F6.3),4X,5(1X,F6.3)))
102   FORMAT(4F8.3,5X,5A4)
103   FORMAT(5X,A2)
104   FORMAT(//2X, 'VALEUR DES   LOG GAMMA* - LOG GAMMA  * DE REF'/)
105   FORMAT(//2X,  'VALEUR DES  LOG GAMMA *'/)
106   FORMAT(1H )
107   FORMAT(4F10.2)
108   FORMAT(10F8.3)
109   FORMAT(/5X,A2)
110   FORMAT(I10)
111   FORMAT(2F10.2)
112   FORMAT(3I6)
113   FORMAT(5F10.2)
114   FORMAT('   LES GAMMA SOLAIRES N ONT PAS ETE CALCULES AVEC ',
     1 /'   LES MEMES PARAMETRES LAMBDA ET KIEX',
     2 /'   VALEURS SOLAIRES:')
      END



      SUBROUTINE EQUIV(NBLEND,LAMBDA,DELTA,A,POP,GFAL,ABOND,NTOT,
     1  NH,B,KC,FC,KIK,PTDISK,MU,W)
      REAL NH,KC,KA,KAP,MU,LAMBDA
      INTEGER D,DDTOT,DTOT,DIVTOT,CAVA
      LOGICAL  PTDISK
      DIMENSION B(0:50)
      DIMENSION NH(50),KC(50),KAP(50),LAMBDA(50),DELTA(50,50),
     1   ABOND(50),GFAL(50),POP(50,50),A(50,50),ECART(31),
     2   DELTL(29),F(35),W(50)
      DATA DELTL/5.,0.,2.,0.,1.,0.,0.5,0.,0.5,0.,0.40,0.,0.2,0.,
     1   0.2,0.,0.05,0.,.05,0.,.02,0.,.02,0.,.02,0.,.02,0.,.02/
      DTOT=29
      DIVTOT=31
      S=0.
            DO D=1,DTOT
            S=S+DELTL(D)
            END DO
      ECART(1)=S
            DO D=3,DIVTOT,2
            ECART(D)=ECART(D-2) - DELTL(D-2)
            ECART(D-1) = (ECART(D)+ECART(D-2)) / 2
            END DO
C     WRITE(6,*) ' ECARTS'
C     WRITE(6,*) (ECART(D),D=1,DIVTOT)
C
                DO K=1,NBLEND
C
      DO D=1,DIVTOT
            DO N=1,NTOT
            AA=A(K,N)
            DEL=DELTA(K,N)
            V=ABS(ECART(D)*1.E-08/DEL)
            CALL HJENOR(AA,V,DEL,PHI)
            KA=PHI*POP(K,N)*GFAL(K)*ABOND(K)
            KAP(N)=KC(N) + KA
            END DO
      CALL FLIN1 (KAP,B,NH,NTOT,PTDISK,MU,FFF,KIK,CAVA)
      F(D)=1-FFF/FC
      END DO   !                 FIN BCLE SUR D
C
C     WRITE(6,*) (F(D),D=1,DTOT)
      WW=0.
      DO D=1,DTOT,2
      WW  = WW   + DELTL(D) * (F(D)+4*F(D+1)+F(D+2) )/6
      END DO
      WEX=F(1)*ECART(1)
      W(K) = 2000 * (WW+WEX)
C     WRITE(6,*) LAMBDA(K),W(K)
               END DO                   !FIN BCLE SUR K
      RETURN
      END


      SUBROUTINE FLIN1 (KAP,B,NH,NTOT,PTDISK,MU,F,IOP,CAVA)
c     calcul du flux ou de l'intensite par la methode d'integration
c     a 6 pts (ou 13pts) de R.Cayrel (these).
c     nouvelle methode de calcul de to . TO(1)est calcule et
c     est different de 0 (On pose TO(0)=0)   -Avril 1988-
      LOGICAL PTDISK
      REAL   NH,KAP,MU
      INTEGER CAVA
      DIMENSION B(0:50),  TO(0:50)
      DIMENSION NH(50),KAP(50),T5L(50),BBB(26),TD2(26),
     1  TD(6),TP(7),CD(6),CP(7),C1(13),C2(12),C3(12)
      COMMON/TOTO/TO
      COMMON/FCO/FP(13),CC(13),TT(13),BB(13)
      COMMON/CCC/AMF(50),AMF2(50),FX1(50),FX2(50)
      COMMON/FAIL/ERR(50)
      DATA TD /0.038,0.154,0.335,0.793,1.467,3.890 /
      DATA CD/0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
      DATA TP/0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
      DATACP/0.176273,0.153405,0.167016,0.135428,0.210244,0.107848,
     10.049787/
      DATA TD2/0.,.05,.1,.15,.3,.45,.60,.80,1.,1.2,1.4,1.6,1.8,2.,
     1 2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
      DATA C1/.032517,.047456,.046138,.036113,.019493,.011037,.006425,
     1 .003820,.002303,.001404,.000864,.001045,.002769/
      DATA C2/.111077,.154237,.143783,.108330,.059794,.034293,
     1 .020169,.012060,.007308,.004473,.002761,.002757/
      DATA C3/.023823,.030806,.027061,.019274,.010946,.006390,
     1 .003796,.002292,.001398,.000860,.000533,.000396/
C
C           CALCUL DE TO
C
      CAVA=0
      EPSI=0.05
      TO(0)=0.
        TO(1)=NH(1)*(KAP(1)-(KAP(2)-KAP(1))/(NH(2)-NH(1))*NH(1)/2.)
        CALL INTEGRA(NH,KAP,TO,NTOT,TO(1))
C
C           CALCUL DU FLUX
      IF(IOP.EQ.0)   THEN
C               FORMULE A 6 OU 7 PTS
            IF(PTDISK) THEN
            IPOINT=7
            TOLIM=4.0
                     ELSE
            IPOINT=6
            TOLIM=3.89
            END IF
c     on verifie que le modele n'est pas trop court
      IF (TO(NTOT).LT.TOLIM  )    THEN
      WRITE(6,1504)
      WRITE(6,1503) NTOT,TO(NTOT)
      WRITE(6,1501)
      CAVA=2
      RETURN
      END IF
c
2     DO  L=1,IPOINT
            IF(PTDISK) THEN
            TT(L) = TP(L)*MU
            CC(L)=CP(L)
                     ELSE
            TT(L) = TD(L)
            CC(L) = CD(L)
            END IF
      END DO
c
      F=0.
            DO  L=1,IPOINT
            BB(L)=FAITK30(TT(L),TO,B,NTOT)
            FP(L)=CC(L)*BB(L)
            F=F+FP(L)
            END DO
      RETURN
C
                        ELSE
C     FORMULE A 26 PTS (NE MARCHE QUE POUR LE FLUX!)
C           (13PTS +PTS MILIEU)
            IF(PTDISK)   then
            WRITE(6,1500)
            STOP
            END IF
      TOLIM=5.487  ! Le modele doit aller au moins a une prof TOLIM
            IF(TO(NTOT).LT. TOLIM) THEN
              WRITE(6,1504)
              WRITE(6,1503) NTOT,TO(NTOT)
              WRITE(6,1501)
              CAVA=2
              RETURN
              END IF
c
      DO L=1,26
      T=TD2(L)
      BBB(L)=FAITK30(TD2(L),TO,B,NTOT)
      END DO
C
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
      END IF  !(fin du IF IOP)
C
8     WRITE(6,1500)
      STOP
1500  FORMAT('   LE SP FLIN1 NE PEUT CALCULER L INTENSITE EN 1 PT ',
     1 'DU DISQUE AVEC LA FORMULE A 26PTS (UTILISER 7PTS IOP=0)' )
1501  FORMAT(1H //)
1502  FORMAT(5(F7.3,F5.2,2X))
1503  FORMAT(I10,5X,3HTO=,F10.4)
1504  FORMAT(18H MODELE TROP COURT)
      END


      SUBROUTINE FLIN2 (KAP,B,NH,NTOT,PTDISK,MU,F,IOP,CAVA)
c     ***cette version differe de flin1 par le calcul de to
c     ***on pose TO(1)=0 .
c     ***Le calcul des TO aux autres niveaux est calcule en
c     ***valeur et en log pour tester l'ecartement des niveaux.
c     ***La methode d'integration du flux : 6pts ou 13pts these
c     ***R.Cayrel.  (ancienne version de FLIN1)
c     *************       ATTENTION     ***********************
c     NH, B, TO doivent etre dimensionnes a 50 dans le programme
c     appelant (et non 0:50).....
c****************************************************************
      LOGICAL PTDISK
      REAL   NH,KAP,MU
      INTEGER CAVA
      DIMENSION KAP(50),B(50),NH(50),T5L(50),BBB(26),TD2(26),
     1  TD(6),TP(7),CD(6),CP(7),C1(13),C2(12),C3(12)
      COMMON/TOTO/TO(50)  /FCO/FP(13),CC(13),TT(13),BB(13)
      COMMON /CCC/AMF(50),AMF2(50),FX1(50),FX2(50)
      COMMON/FAIL/ERR(50)
      DATA TD /0.038,0.154,0.335,0.793,1.467,3.890 /
      DATA CD/0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
      DATA TP/0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
      DATACP/0.176273,0.153405,0.167016,0.135428,0.210244,0.107848,
     10.049787/
      DATA TD2/0.,.05,.1,.15,.3,.45,.60,.80,1.,1.2,1.4,1.6,1.8,2.,
     1 2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
      DATA C1/.032517,.047456,.046138,.036113,.019493,.011037,.006425,
     1 .003820,.002303,.001404,.000864,.001045,.002769/
      DATA C2/.111077,.154237,.143783,.108330,.059794,.034293,
     1 .020169,.012060,.007308,.004473,.002761,.002757/
      DATA C3/.023823,.030806,.027061,.019274,.010946,.006390,
     1 .003796,.002292,.001398,.000860,.000533,.000396/
C
C           CALCUL DE TO
C
      CAVA=0
      EPSI=0.05
      TO(1)=0.
      NN=NTOT-2
      CALL INAIT(NH,KAP,TO,ERR,NTOT)
      DO   I=1,NTOT
      IF(ERR(I).GT.EPSI) CAVA=1
      END DO
C
C           CALCUL DU FLUX
C           FORMULE A 6 OU 7 PTS
      IF(IOP.EQ.1)   GO TO 1
      IF(PTDISK)   IPOINT=7
      IF(.NOT.PTDISK)   IPOINT=6
      IF( PTDISK.AND.(TO(NTOT).LT.4.0)   )  GO TO 7
      IF(  ( .NOT.PTDISK) . AND.  (TO(NTOT).LT.3.89)  )  GO TO 7
2     DO 10 L=1,IPOINT
      IF(.NOT.PTDISK)   GO TO 11
      TT(L) = TP(L)*MU
      CC(L)=CP(L)
      GO TO 10
11    TT(L) = TD(L)
      CC(L) = CD(L)
10    CONTINUE
      F=0.
      DO  L=1,IPOINT
      T=TT(L)
      BB(L)=FT(T,NTOT,TO,B)
      FP(L)=CC(L)*BB(L)
      F=F+FP(L)
      END DO
      RETURN
C
C     FORMULE A 26 PTS (NE MARCHE QUE POUR LE FLUX!)
C           (13PTS +PTS MILIEU)
1     IF(PTDISK)   GO TO 8
      IF(TO(NTOT).LT.5.487)   GO TO 7
      DO L=1,26
      T=TD2(L)
      BBB(L)=FT(T,NTOT,TO,B)
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
C
7     WRITE(6,1504)
      WRITE(6,1503) NTOT,TO(NTOT)
      WRITE(6,1501)
      CAVA=2
      RETURN
8     WRITE(6,1500)
      STOP
1500  FORMAT('   LE SP FLIN1 NE PEUT CALCULER L INTENSITE EN 1 PT ',
     1 'DU DISQUE AVEC LA FORMULE A 26PTS (UTILISER 7PTS IOP=0)' )
1501  FORMAT(1H //)
1502  FORMAT(5(F7.3,F5.2,2X))
1503  FORMAT(I10,5X,3HTO=,F10.4)
1504  FORMAT(18H MODELE TROP COURT)
      END



      SUBROUTINE FLIN2B (KAP,B,NH,NTOT,PTDISK,MU,F,IOP,CAVA)
c     ***analogue a FLIN2 mais
C     ***PAS DE CALCUL D'erreur dans cette version
c       *************       ATTENTION     ***********************
c       NH, B, TO doivent etre dimensionnes a 50 dans le programme
c       appelant (et non 0:50).....
c****************************************************************
      LOGICAL PTDISK
      REAL   NH,KAP,MU
      INTEGER CAVA
      DIMENSION KAP(50),B(50),NH(50),T5L(50),BBB(26),TD2(26),
     1  TD(6),TP(7),CD(6),CP(7),C1(13),C2(12),C3(12)
      COMMON/TOTO/TO(50)  /FCO/FP(13),CC(13),TT(13),BB(13)
      COMMON /CCC/AMF(50),AMF2(50),FX1(50),FX2(50)
      COMMON/FAIL/ERR(50)
      DATA TD /0.038,0.154,0.335,0.793,1.467,3.890 /
      DATA CD/0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
      DATA TP/0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
      DATACP/0.176273,0.153405,0.167016,0.135428,0.210244,0.107848,
     10.049787/
      DATA TD2/0.,.05,.1,.15,.3,.45,.60,.80,1.,1.2,1.4,1.6,1.8,2.,
     1 2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
      DATA C1/.032517,.047456,.046138,.036113,.019493,.011037,.006425,
     1 .003820,.002303,.001404,.000864,.001045,.002769/
      DATA C2/.111077,.154237,.143783,.108330,.059794,.034293,
     1 .020169,.012060,.007308,.004473,.002761,.002757/
      DATA C3/.023823,.030806,.027061,.019274,.010946,.006390,
     1 .003796,.002292,.001398,.000860,.000533,.000396/
C
C           CALCUL DE TO
C
      CAVA=0
      EPSI=0.05
      TO(1)=0.
      NN=NTOT-2
      CALL INAITB(NH,KAP,TO,NTOT)
C           CALCUL DU FLUX
C           FORMULE A 6 OU 7 PTS
      IF(IOP.EQ.1)   GO TO 1
      IF(PTDISK)   IPOINT=7
      IF(.NOT.PTDISK)   IPOINT=6
      IF( PTDISK.AND.(TO(NTOT).LT.4.0)   )  GO TO 7
      IF(  ( .NOT.PTDISK) . AND.  (TO(NTOT).LT.3.89)  )  GO TO 7
2     DO 10 L=1,IPOINT
      IF(.NOT.PTDISK)   GO TO 11
      TT(L) = TP(L)*MU
      CC(L)=CP(L)
      GO TO 10
11    TT(L) = TD(L)
      CC(L) = CD(L)
10    CONTINUE
      F=0.
      DO  L=1,IPOINT
      T=TT(L)
      BB(L)=FT(T,NTOT,TO,B)
      FP(L)=CC(L)*BB(L)
      F=F+FP(L)
      END DO
      RETURN
C
C     FORMULE A 26 PTS (NE MARCHE QUE POUR LE FLUX!)
C           (13PTS +PTS MILIEU)
1     IF(PTDISK)   GO TO 8
      IF(TO(NTOT).LT.5.487)   GO TO 7
      DO L=1,26
      T=TD2(L)
      BBB(L)=FT(T,NTOT,TO,B)
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
C
7     WRITE(6,1504)
      WRITE(6,1503) NTOT,TO(NTOT)
      WRITE(6,1501)
      CAVA=2
      RETURN
8     WRITE(6,1500)
      STOP
1500  FORMAT('   LE SP FLIN1 NE PEUT CALCULER L INTENSITE EN 1 PT ',
     1 'DU DISQUE AVEC LA FORMULE A 26PTS (UTILISER 7PTS IOP=0)' )
1501  FORMAT(1H //)
1502  FORMAT(5(F7.3,F5.2,2X))
1503  FORMAT(I10,5X,3HTO=,F10.4)
1504  FORMAT(18H MODELE TROP COURT)
      END


      SUBROUTINE FT2(N,X,Y,ITOT,TT,FTT)
C     INTERPOLATION PARABOLIQUE
C     DANS LA TABLE X Y (N POINTS) ON INTERPOLE LES FTT CORRESPONDANT
C     AUX TT  (ITOT POINTS) POUR TOUTE LA LISTE DES TT
      DIMENSION  X(N),Y(N),TT(ITOT),FTT(ITOT)
      INV = -1
      IF(X(N).LT.X(1) ) INV=1
      DO 4 K=1,ITOT
      T=TT(K)
      IF (INV)5,6,6
5     DO 1 J=1,N
      I=J
      IF(T-X(J))3,2,1
1     CONTINUE
      GO TO 10
6     DO 7 J=1,N
      I=J
      IF(T-X(J) )7,2,3
7     CONTINUE
      GO TO 10
2     FT=Y(J)
      GO TO 4
3     IF(I .EQ. 1) I=2
      IF( I .GE. N)  I=N-1
      T0=T-X(I-1)
      T1=T-X(I)
      T2=T-X(I+1)
      U0=X(I+1)-X(I-1)
      U1=X(I+1)-X(I)
      U2=X(I)-X(I-1)
      A=T0/U0
      B=T1/U1
      C=T2/U2
      D=T0/U1
      E=T1/U0
      FT=Y(I+1)*A*B - Y(I)*D*C + Y(I-1)*E*C
4     FTT(K) =FT
      RETURN
10    WRITE(6,100) T
      STOP
100   FORMAT(5X,'ON SORT DE LA TABLE D INTERPOLATION       T=',E15.7)
      END
C
      SUBROUTINE FTLIN3(N,X,Y,ITOT,TT,FTT)
      DIMENSION X(N),Y(N),TT(ITOT),FTT(ITOT)
C     WRITE(6,*) N
C     WRITE (6,105) (X(J),J=1,N)
C     WRITE (6,105) (Y(J),J=1,N)
C
      J=2
      DO  4 K=1,ITOT
      T=TT(K)
C     WRITE(6,103)T
      JJ=J-1
            DO 1  J=JJ,N
            IF(T-X(J) ) 3,2,1
1           CONTINUE
            GO TO 10
2     FT=Y(J)
      IF(J.EQ.1) J=J+1
      GO TO 4
C 3   WRITE(6,*) '   J=',J
3     IF(J.EQ.1)   GO TO 10
      U0= Y(J)-Y(J-1)
      T0= X(J)-X(J-1)
      T1=   T -X(J-1)
C     WRITE(6,104) J, X(J-1), Y(J-1), X(J),Y(J), U0,T0,T1
      T2= T1/T0
      DY= U0*T2
      FT= Y(J-1) + DY
4     FTT(K)=FT
      RETURN
10    WRITE(6,100) T
      WRITE(6,101)
      WRITE(6,102) (X(I),I=1,N)
      STOP
100   FORMAT(' ON SORT DE LA TABLE D INTERPOLATION    T=',E15.7)
101   FORMAT(/'   LISTE DES X')
102   FORMAT(8E15.7)
103   FORMAT(5X,F10.3)
104   FORMAT(I5,7F9.3)
C105  FORMAT(7F10.3)
      END


      SUBROUTINE HJENOR(Y,X,DEL,PHI)
C     ***      REFERENCE   Q.S.R.T.   VOL16,611 (1976)
C     ***ROUTINE COMPUTES THE VOIGHT FUNCTION  Y/PI*INTEGRAL FROM
C     ***- TO + INFINITY OF  EXP(-T*T)/(Y*Y+(X-T)*(X-T)) DT
C     *** LA FONCTION EST ENSUITE NORMALISEE
      REAL X,Y
      real VOIGT
      real    VV,UU,CO
      REAL    B(22),RI(15),XN(15)/10.,9.,2*8.,7.,6.,5.,4.,7*3./,
     &  YN(15)/3*.6,.5,2*.4,4*.3,1.,.9,.8,2*.7/,D0(35),D1(35),D2(35)
     &  ,D3(35),D4(35),HN(35),H/.201/,XX(3)/.5246476,1.65068,.7071068/
     &  ,HH(3)/.2562121,.2588268E-1,.2820948/,NBY2(19)/9.5,9.,8.5,8.,
     &  7.5,7.,6.5,6.,5.5,5.,4.5,4.,3.5,3.,2.5,2.,1.5,1.,.5/,C(21)/
     &  .7093602E-7,-.2518434E-6,.8566874E-6,-.2787638E-5,.866074E-5,
     &  -.2565551E-4,.7228775E-4,-.1933631E-3,.4899520E-3,-.1173267E-2,
     &  .2648762E-2,-.5623190E-2, .1119601E-1,-.2084976E-1,.3621573E-1,
     &  -.5851412E-1,.8770816E-1, -.121664,.15584,-.184,.2/
      LOGICAL TRU/.FALSE./
      TRU=.FALSE.
      B(1)=0.
      B(2)=0.7093602E-7
      IF (TRU) GO TO 104
C  REGION I. COMPUTE DAWSON'S FUNCTION AT MESH POINTS
      TRU=.TRUE.
      DO 101 I=1,15
101   RI(I)=-I/2.
      DO 103 I=1,25
      HN(I)=H*(I-.5)
      C0=4.*HN(I)*HN(I)/25.-2.
      DO 102 J=2,21
102   B(J+1)=C0*B(J)-B(J-1)+C(J)
      D0(I)=HN(I)*(B(22)-B(21))/5.
      D1(I)=1.-2.*HN(I)*D0(I)
      D2(I)=(HN(I)*D1(I)+D0(I))/RI(2)
      D3(I)=(HN(I)*D2(I)+D1(I))/RI(3)
      D4(I)=(HN(I)*D3(I)+D2(I))/RI(4)
C     write(6,*)i,d0(i),d1(i),d2(i),d3(i),d4(i)
103   CONTINUE
104   IF (X-5.) 105,112,112
105   IF (Y-1.) 110,110,106
106   IF (X.GT.1.85*(3.6-Y)) GO TO 112
C   REGION II CONTINUED FRACTION .COMPUTE NUMBER OF TERMS NEEDED
C     write(6,*)'region II'
      IF (Y.LT.1.45) GO TO 107
      I=Y+Y
      GO TO 108
107   I=11.*Y
108   J=X+X+1.85
      MAX=XN(J)*YN(I)+.46
      MIN=MIN0(16,21-2*MAX)
C  EVALUATED CONTINUED FRACTION
      UU=Y
      VV=X
      DO 109 J=MIN,19
      U=NBY2(J)/(UU*UU+VV*VV)
      UU=Y+U*UU
109   VV=X-U*VV
      VOIGT=UU/(UU*UU+VV*VV)/1.772454
      GO TO 10
110   Y2=Y*Y
      IF (X+Y.GE.5.) GO TO 113
C REGION I. COMMPUTE DAWSON'S FUNCTION AT X FROM TAYLOR SERIES
      N=INT(X/H)
      DX=X-HN(N+1)
      U=(((D4(N+1)*DX+D3(N+1))*DX+D2(N+1))*DX+D1(N+1))*DX+D0(N+1)
      V=1.-2.*X*U
C  TAYLOR SERIES EXPANSION ABOUT Y=0.0
      VV=EXP(Y2-X*X)*COS(2.*X*Y)/1.128379-Y*V
C     write(6,*) n,u,dx,d0(n+1),d1(n+1),d2(n+1),d3(n+1),d4(n+1)
      UU=-Y
      MAX=5.+(12.5-X)*.8*Y
      DO 111 I=2,MAX,2
      U=(X*V+U)/RI(I)
      V=(X*U+V)/RI(I+1)
      UU=-UU*Y2
111   VV=VV+V*UU
      VOIGT=1.128379*VV
C     write(6,*)'region I ',voigt,vv,x,y,del
      GO TO 10
112   Y2=Y*Y
      IF (Y.LT.11.-.6875*X) GO TO 113
C  REGION IIIB  2 POINT GAUSS-HERMITE QUADRATURE
      U=X-XX(3)
      V=X+XX(3)
      VOIGT=Y*(HH(3)/(Y2+U*U)+HH(3)/(Y2+V*V))
C     write(6,*)'region IIIb ', voigt
      GO TO 10
C  REGION IIIA 4-POINT GAUSS-HERMITE QUADRATURE.
113   U=X-XX(1)
      V=X+XX(1)
      UU=X-XX(2)
      VV=X+XX(2)
      VOIGT=Y*(HH(1)/(Y2+U*U)+HH(1)/(Y2+V*V)+HH(2)/(Y2+UU*UU)+HH(2)/
     1(Y2+VV*VV))
C     write(6,*)'region IIIa',voigt
10    PHI = VOIGT /  (1.772454 * DEL)
C     write(6,*)phi
      RETURN
      END



      SUBROUTINE INAIT(X,Y,P,ERR,N)
C CE PROGRAMME INTEGRE NUMERIQUEMENT UNE FONCTION RELLE, DONNEE PAR
C UN TABLEAU DE VALEURS X(I),Y(I). LA METHODE CONSISTE A CALCULER
C L' INTEGRALE DEFINIE SUR CHAQUE INTERVALLE (X(I),X(I+1)) D'ABORD
C  PAR SIMPSON,PUIS PAR GAUSS A DEUX POINTS. LES VALEURS DE Y AU
C  POINT MILIEU ET AU DEUX POINTS DE GAUSS SONT CALCULES PAR INTER-
C  POLATION CUBIQUE A QUATRE POINTS EN UTILISANT LES VALEURS DE LA
C  TABLE.ON FORME ENSUITE UNE MOYENNE PONDEREE DES DEUX RESULTATS
C  QUI ANNULE L'ERREUR DU QUATRIEME (ET AUUSI DU CINQUIEME PAR RAI-
C   SON DE PARITE) ORDRE.L' ERREUR SUR L'INTEGRATION EST DONC
C  GENERALEMENT NEGLIGEABLE PAR RAPPORT A L'ERREUR SUR L'INTERPOLA-
C  TION DU TROSIEME ORDRE.CETTE DERNIERE EST EVALUEE EN COMPARANT
C   LA VALEUR INTERPOLEE SUR LA FONCTION ELLE MEME ET SUR LA VALEUR
C   INTERPOLEE EN PASSANT PAR SON LOGARITHME.IL FAUT QUE LA FONCTION
C   SOIT POSITIVE PAR NATURE POUR QUE LA TRANSFORMATION LOGARITHMI-
C   QUE SOIT POSSIBLE.
C
      DIMENSION X(N),Y(N),ALY(200),P(N),ERR(N)
      COMMON /GENE/ALY,XMILIEU,XGAUSS1,XGAUSS2,DEL,YM
     1  ,YG1,YG2,CONST,I,J
      P(1)=0.
      CONST=1./SQRT(3.)
       DO I=1,N
       ALY(I)=ALOG(Y(I))
       END DO
      I=1
      J=1
      CALL STEP(X,Y,P,ERR,N)
        DO I=2,N-2
        J=I-1
      CALL STEP(X,Y,P,ERR,N)
        END DO
      I=N-1
      J=N-3
      CALL STEP(X,Y,P,ERR,N)
      END


      SUBROUTINE INAITB(X,Y,P,N)
C CE PROGRAMME INTEGRE NUMERIQUEMENT UNE FONCTION RELLE, DONNEE PAR
C UN TABLEAU DE VALEURS X(I),Y(I). LA METHODE CONSISTE A CALCULER
C L' INTEGRALE DEFINIE SUR CHAQUE INTERVALLE (X(I),X(I+1)) D'ABORD
C  PAR SIMPSON,PUIS PAR GAUSS A DEUX POINTS. LES VALEURS DE Y AU
C  POINT MILIEU ET AU DEUX POINTS DE GAUSS SONT CALCULES PAR INTER-
C  POLATION CUBIQUE A QUATRE POINTS EN UTILISANT LES VALEURS DE LA
C  TABLE.ON FORME ENSUITE UNE MOYENNE PONDEREE DES DEUX RESULTATS
C  QUI ANNULE L'ERREUR DU QUATRIEME (ET AUUSI DU CINQUIEME PAR RAI-
C   SON DE PARITE) ORDRE.L' ERREUR SUR L'INTEGRATION EST DONC
C  GENERALEMENT NEGLIGEABLE PAR RAPPORT A L'ERREUR SUR L'INTERPOLA-
C  TION DU TROSIEME ORDRE.
C
C *******PAS DE CALCUL D'ERREUR DANS CETTE VERSION*****************
C
      DIMENSION X(N),Y(N),ALY(200),P(N)
      COMMON /GENE/ALY,XMILIEU,XGAUSS1,XGAUSS2,DEL,YM
     1  ,YG1,YG2,CONST,I,J
      P(1)=0.
      CONST=1./SQRT(3.)
      I=1
      J=1
      CALL STEPB(X,Y,P,N)
        DO I=2,N-2
        J=I-1
        CALL STEPB(X,Y,P,N)
        END DO
      I=N-1
      J=N-3
      CALL STEPB(X,Y,P,N)
      END


      SUBROUTINE INTEGRA(X,Y,P,N,PDEB)
C     X=TABLEAU DE VALEURS DE LA VARIABLE INDEPENDANTE, PAR VALEURS
C     CROISSANTES
C     Y=TABLEAU DES VALEURS ASSOCIEES DE LA FONCTION A INTEGRER
C     PDEB=VALEUR DE LA PRIMITIVE POUR X(1),PREMIERE VALEUR
C     DU TABLEAU
C     P=TABLEAU DES VALEURS DE LA PRIMITIVE AUX POINTS X(I)
C     METHODE : LA VALEUR DE L'INTEGRALE SUR L'INTERVALLE X(I),X(I+1)
C     EST CALCULEE PAR LA FORMULE DE SIMPSON,LA VALEUR DE Y AU POINT
C     MILIEU ETANT CALCULEE PAR INTERPOLATION CUBIQUE ,PAR LA ROUTINE
C     AITK3
      DIMENSION X(N),Y(N),P(0:N)
      P(1)=PDEB
C     CAS SPECIAL DU PREMIER INTERVALLE
      XMILIEU=(X(1)+X(2))/2.
      CALL NAITK3(X(1),X(2),X(3),X(4),
     1           Y(1),Y(2),Y(3),Y(4),XMILIEU,FX)
      P(2)=P(1)+((X(2)-X(1))/6.)*(Y(1)+Y(2)+4.*FX)
C     ********************************
C     CAS GENERAL
            DO I=2,N-2
            XMILIEU=(X(I)+X(I+1))/2.
            J=I-1
            call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1                   Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,FX)
            P(I+1)=P(I)+((Y(I)+Y(I+1)+4.*FX)/6.)*(X(I+1)-X(I))
            END DO
C     *********************************
C     CAS SPECIAL DERNIER INTERVALLE
      XMILIEU=(X(N-1)+X(N))/2.
      J=N-3
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,FX)
      P(N)=P(N-1)+((X(N)-X(N-1))/6.)*(Y(N-1)+Y(N)+4.*FX)
C     ***********************************
      RETURN
      END


      SUBROUTINE LARGQ(ITOT,DL2,FLU,W)
      DIMENSION DL2(ITOT),FLU(ITOT)
      W=0
      IT=ITOT-2
      DO I=1,IT,2
      W= W + (FLU(I) + 4*FLU(I+1) + FLU(I+2) )* DL2(I) /6
      END DO
      RETURN
      END


      SUBROUTINE NAITK3(XdI,XdIp1,XdIp2,XdIp3,
     1                 YdI,YdIp1,YdIp2,YdIp3,XX,FX)
c
c     Nouvelle subroutine NAITK3 remplace AITK3 et AITK30
      U=XdI
      V=XdIp1
      FU=YdI
      FV=YdIp1
            F01=(FU*(V-XX)-FV*(U-XX))/(V-U)
      U=XdIp2
      FU=YdIp2
            F12=(FU*(V-XX)-FV*(U-XX))/(V-U)
      U=XdIp3
      FU=YdIp3
            F13=(FU*(V-XX)-FV*(U-XX))/(V-U)
      U=XdI
      FU=F01
      V=XdIp2
      FV=F12
            F012=(FU*(V-XX)-FV*(U-XX))/(V-U)
      U=XdIp2
      FU=F12
      V=XdIp3
      FV=F13
            F123=(FU*(V-XX)-FV*(U-XX))/(V-U)
      U=XdI
      V=XdIp3
      FU=F012
      FV=F123
            F0123=(FU*(V-XX)-FV*(U-XX))/(V-U)
      FX=F0123
      RETURN
      END


      SUBROUTINE POPADEL (NPAR,EL,KI1,KI2,M,NBLEND,ELEM,
     1 LAMBDA,KIEX,CH,CORCH,CVdW,GR,GE,IONI,NTOT,TETA,PE,ALPH,
     2 PHN,PH2,VT,P,POP,A,DELTA)
C     ***calcule la population au niveau inferieur de la transition
C     ***la largeur doppler DELTA et le coefficient d'elargissement
C     ***le "A" utilise dans le calcul de H(A,V)
        real KI1,KI2,KIEX,M,KB,KIES,KII
        parameter (NN=8000)
        real*8 LAMBDA(NN)
        CHARACTER*2 ISI,ISS
      DIMENSION PE(50),TETA(50),VT(50),ALPH(50),PHN(50),PH2(50),
     1 EL(85),M(85),KI1(85),KI2(85),U(3),P(3,85,50),
     2 ELEM(NN),IONI(NN),KIEX(NN),
     3 GF(NN),CH(NN),CORCH(NN),CVdW(NN),GR(NN),GE(NN),
     4 POP(NN,50),A(NN,50),DELTA(NN,50)
C
        data KB/1.38046E-16/, DEUXR/1.6634E+8/, C4/2.1179E+8/,
     1  C6/3.76727E+11/, PI/3.141593/, ISI/' '/, ISS/' '/,
     2  C/2.997929E+10/
      C5= 2.*PI* (3.*PI**2/2.44)**0.4
C     C6=4 * Pi * C
c
        DO  K=1,NBLEND
            CVdW(K)=0
                DO  J=1,NPAR
                IF(EL(J).EQ.ELEM(K)) GO TO 15
                END DO
        WRITE(6,104) ELEM(K)
        STOP
15      IOO=IONI(K)
C
                IF(CH(K).LT.1.E-37)  THEN
                KIES=(12398.54/LAMBDA(K)) + KIEX(K)
                IF(IOO.EQ.1)   KII=KI1(J)
                IF(IOO.EQ.2)   KII=KI2(J)
                        IF(CORCH(K).LT.1.E-37)   THEN
                        CORCH(K)=0.67 * KIEX(K) +1
                        END IF   ! FIN DE IF CORCH(K)=0
                WRITE(6,125)  LAMBDA(K), CORCH(K)
                CVdW(K)= CALCH(KII,IOO,KIEX(K),ISI,KIES,ISS)
            CH(K)= CVdW(K) * CORCH(K)
                END IF  ! FIN DE IF CH=0.

C
            IF(CH(K) .LT. 1.E-20) then
            IOPI=1
            else
            IOPI=2
            end IF
        DO  N=1,NTOT
        T=5040/TETA(N)
        TAP = 1.-ALPH(N)
        TOP = 10.**(-KIEX(K)*TETA(N))
        POP(K,N) = P(IOO,J,N)*TOP*TAP
        DELTA(K,N) =(1.E-8*LAMBDA(K))/C*SQRT(VT(N)**2+DEUXR*T/M(J))
        VREL    = SQRT(C4*T*(1.+1./M(J)))
            IF (IOPI.EQ.1)  then
            GH   =C5*CH(K)**0.4*VREL   **0.6
                  if (N.EQ.10)  write (6,100) GH
            else
            GH = CH(K) + Corch(K)*T
                  if (N.EQ.10) write(6, 101) GH
            END IF
        GAMMA = GR(K)+(GE(K)*PE (N)+GH*(PHN(N)+1.0146*PH2(N)))/(KB*T)
        A(K,N) =GAMMA*(1.E-8*LAMBDA(K))**2 / (C6*DELTA(K,N))
        END DO    !FIN BCLE SUR N
        END DO    !FIN BCLE SUR K
C
 100  FORMAT(' GamH AU 1Oeme Niv du modele:', E15.3)
 101  FORMAT(' GamH au 10eme Niv du modele:', E15.3,'  Spielfieldel')
 104  FORMAT('     MANQUE LES FCTS DE PARTITION DU ',A2)
 125  FORMAT(3X ,' POUR',F9.3,'   ON CALCULE CH ',
     1 'VAN DER WAALS ET ON MULTIPLIE PAR ',F5.1)
      return
      end



      SUBROUTINE POPUL(TETA,PE,NTOT,TINI,PA,JKMAX,KI1,KI2,NPAR,TABU,P)
C     ***calcule la pop du niv fond de l'ion pour tous les NPAR atomes de
C     ***la table des fonctions de partition ,a tous les niv du modele
C     ***
      DIMENSION TINI(85),PA(85),JKMAX(85),TABU(85,3,63),
     1 KI1(85),KI2(85),U(3),ALISTU(63),P(3,85,50),TETA(50),
     2 PE(50),UE(50),TT(51)
c           40 elements, 50 niveaux de modele, 3 niv d'ionisation par elem.
c           partit donnee pour 33 temperatures au plus ds la table.
      REAL KB,KI1,KI2
      KB=1.38046E-16
      C1=4.8298E+15   ! =2 * (2*Pi*KB*ME)**1.5 / H**3
C
      DO  N=1,NTOT
      T=5040./TETA(N)
      UE(N)=C1*KB*T /PE(N)*T**1.5
            DO  J=1,NPAR
            KMAX=JKMAX(J)
            TT(1) = TINI(J)
                  DO  L=1,3
                        DO  K=1,KMAX
                        TT(K+1) = TT(K) + PA(J)
                        ALISTU(K) = TABU(J,L,K)
                        END DO
c
                        if (teta(N).LT.TT(KMAX-1) ) then ! (inter parabolique)
                        UUU=FT(TETA(N),KMAX,TT,ALISTU)
                        else
c                  interpolation lineaire entre 2 derniers pts
                        AA=(ALISTU(KMAX)-ALISTU(KMAX-1)) / PA(J)
                        BB=ALISTU(KMAX-1) - AA * TT(KMAX-1)
                        UUU= AA*Teta(N) + BB
                        end if
c
                        U(L) = EXP(2.302585*UUU )
                  END DO   ! FIN BCLE SUR L
C
            X=U(1) / (U(2)*UE(N)) * 10.**(KI1(J)*TETA(N))
            TKI2= KI2(J) * TETA(N)
            IF(TKI2.GE.77.)   THEN
            Y=0.
            P(3,J,N)=0.
                          ELSE
            Y=U(3)*UE(N)/U(2)  *  10.**(-KI2(J)*TETA(N))
            P(3,J,N) =(1./U(3))*(Y/(1.+X+Y))
            END IF

C
            P(2,J,N) = (1./U(2))*(1./(1.+X+Y))
            P(1,J,N) =  (1./U(1))*(X/(1.+X+Y))
            END DO   ! fin bcle sur J
      END DO   ! fin bcle sur N
      RETURN
      END


      SUBROUTINE POPUL2(TETA,PE,NTOT,TINI,PA,II,
     1            JKMAX,IONI,KI1,KI2,TABU,P)
C       ***calcule la pop du niv fond de l'ion pour l'atome de rang II de
C       ***la table des fonctions de partition ,a tous les niv du modele
C       ***
c               40 elements, 50 niveaux de modele, 3 niv d'ionisation par elem.
c               partit donnee pour 33 temperatures au plus ds la table.

      DIMENSION U(3),TINI(85),PA(85),JKMAX(85),TABU(85,3,63),
     1      TETA(50),PE(50),P(50),TTT(64),ALISTU(64)
      REAL KI1,KI2,KB
      KB=1.38046E-16
      C1=4.8298E+15   ! =2*(2*PI*KB*ME)**1.5 / H**3
      KMAX=JKMAX(II)
      TTT(1)=TINI(II)
C     type *,'  IONI=',IONI,' II=',II, KMAX
C
      DO N=1,NTOT
        T=5040/TETA(N)
            DO L=1,3
                  DO K=1,KMAX
                  TTT(K+1)=TTT(K) + PA(II)
                  ALISTU(K)=TABU(II,L,K)
c                 write(6,101) L,TTT(K), ALISTU(K)
                  END DO ! fin bcle sur K
                U(L)= EXP(2.302585 *  FT(TETA(N),KMAX,TTT,ALISTU))
            END DO    !fin bcle sur  L
      UE=C1*KB*T / PE(N)*T**1.5
c     write(6,100) N,TETA(N),U(1),U(2),U(3),UE
      X=U(1)/(U(2)*UE) * 10.**( KI1*TETA(N))
      Y=U(3)*UE/U(2)   * 10.**(-KI2*TETA(N))
            DO  L=1,3
                  IF  ((IONI.EQ.L ).AND.(U(L) .LE.0.))  THEN
                  WRITE(6,*)' POPULATION NULLE'
                  STOP
                  END IF
            END DO
      IF(IONI.EQ.1) THEN
      P(N) =((1./U(1))*(X /(1.+X+Y)))
                  ELSE
            IF(IONI.EQ.2)     THEN
      P(N) =((1./U(2))*(1./(1.+X+Y)))
                        ELSE
      P(N) =((1./U(3))*(Y /(1.+X+Y)))
            END IF
      END IF
      END DO
      RETURN
 100  format(I5,2x,F8.4,4E15.4)
 101  format(I5,2E15.4)
      END


      SUBROUTINE QUID(ELEM,IONI,ITOT,EL,IO,JTOT)
      DIMENSION  ELEM(1000),IONI(1000),EL(50),IO(50)
      INTEGER  P,PTOT
      EL(1)=ELEM(1)
      IO(1)=IONI(1)
      PTOT=1
      DO 2 I=2,ITOT
      DO 1 P=1,PTOT
      IF( (ELEM(I).EQ.EL(P))  .AND.  (IONI(I).EQ.IO(P))  )  GOTO 2
1     CONTINUE
      PTOT=PTOT+1
      EL(PTOT) =ELEM(I)
      IO(PTOT)=IONI(I)
2     CONTINUE
      JTOT=PTOT
      RETURN
      END



      SUBROUTINE READER (NH,TETA,PE,PG,T5L,NTOT)
C     CE S.P. LIT SUR DISQUE ACCES DIRECT NH,TETA,PE,PG,T5L,NTOT
C     il lit toujours NH de 1 a 50 (et non 0:50)
      DIMENSION TETA(50),NH(50),PE(50),PG(50),T5L(50),BID(11)
      REAL NH,NHE
        CHARACTER*4 BLC,TIT
      COMMON /COM6/TEFF,TETAEF,GLOG,ASASOL,NHE,TIT(5)
      DATA   BLC/'    '/
      DO 7 I=1,5
7     TIT(I)=BLC
      IF(IDEF.EQ.211939)   GO TO 10
      OPEN(UNIT=18,ACCESS='DIRECT',STATUS='OLD',
     1   file='modeles.dat',
     1 RECL=256)
      ID=1
      IDEF=211939
10    READ(5,*) TETAEF,GLOG,ASALOG,NHE,INUM
      IF(INUM.GT.0)   ID=INUM
      WRITE(6,102)TETAEF,GLOG,ASALOG,NHE,INUM
C   SI L ON DESIRE IMPOSER UN MODELE  ON MET EN INUM LE NUM DU MODELE
C   SUR LE FICHIER ACCES DIRECT
9     READ(18, REC=ID) NTOT,DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT,TITABS
      WRITE(6,105)DETAEF,DGLOG,DSALOG,DNHE,DAMET,TIT
      IF(NTOT.EQ.9999)   GO TO 6
      DDTA  = ABS(TETAEF-DETAEF)
      DDG = ABS(GLOG-DGLOG)
      DDAB = ABS(ASALOG-DSALOG)
      DDHE= ABS(NHE-DNHE)
      DDIF = DDTA+DDG+DDAB+DDHE
5     IF(DDIF.GT.0.0005)   GO TO 9
      ASASOL=10.**ASALOG
      ID=ID-1
      READ(18, REC=ID)BID,(NH(I),TETA(I),PE(I),PG(I),T5L(I),I=1,NTOT)
      write(6,*)'            NH           TETA              PE',
     1 '             PG     To(5000)'
      DO 12 I=1,5
      WRITE(6,103) NH(I),TETA(I),PE(I),PG(I),T5L(I)
12    CONTINUE
      WRITE(6,107)
      ID=1
      RETURN
6     WRITE(6,101)
      STOP
101   FORMAT('   LE MODELE DESIRE N EST PAS SUR LE FICHIER'/)
102   FORMAT('1  MODELE A LIRE',4F8.2,4X,'LIGNE',I3,
     1 //' MODELES SUR LE DISQUE')
103   FORMAT(E16.4,F15.4,2E16.4,F12.4)
104   FORMAT('   ECRITURE DE TIT(1) ',A4,5X,Z4)
105   FORMAT(4F10.2,E15.4,5A4)
106   FORMAT(2X,4F5.2,I5)
107   FORMAT('     ETC.....')
      END



      SUBROUTINE SELEKF(GRAPH,PTDISK,MU,KIK,DTOT,PAS,ALZERO,
     1 NBLEND,GFAL,ABOND,ECART,ZINF,
     2 FC,NTOT,NH,B,KC,POP,DELTA,A,TT,FL)
      REAL KAPPA,KA,KAP,KC,NH,MU
      INTEGER D, DTOT,CAVA
      LOGICAL GRAPH,PTDISK,ECRIT
      DIMENSION NH(50), B(0:50)
      DIMENSION ECART(50),ECAR(50),ZINF(50),
     1 GFAL(50),ABOND(50),KA(50),
     2 KAPPA(50),KAP(50),KC(50),
     3 POP(50,50),DELTA(50,50),A(50,50)
      DIMENSION FL(5001),TT(5001)
C
      IF(GRAPH) PRINT *,' PATIENCE... JE CALCULE...'
            DO K=1,NBLEND
            ECAR(K)=ECART(K)
            END DO
c
      DO D=1,DTOT
            do K=1,NBLEND
            ECAR(K)=ECAR(K)-PAS
            end do
      TT(D)=ALZERO + PAS * (D-1)
            DO N=1,NTOT
            KAPPA(N) =0.
                  DO  K=1,NBLEND
                    IF( ABS(ECAR(K)) .GT. ZINF(K) )  then
                    KA(K)=0.
                                                else
                    V=ABS(ECAR(K)*1.E-8/DELTA(K,N))
                    CALL HJENOR(A(K,N),V,DELTA(K,N),PHI)
                    KA(K) = PHI * POP(K,N) * GFAL(K) * ABOND(K)
                    END IF
                  KAPPA(N) = KAPPA(N) + KA(K)
                  END DO   !  fin bcle sur K
            KAP(N) = KAPPA(N) + KC(N)
            END DO    ! fin bcle sur N
c
      CALL FLIN1 (KAP,B,NH,NTOT,PTDISK,MU,FL(D),KIK,CAVA)
                  IF(CAVA.GT.1)   THEN
                  WRITE(6,131) TT(D),CAVA
                  STOP
                  END IF
      FL(D) = FL(D) / FC
C
      END DO  ! fin bcle sur D
131   FORMAT(' ENNUI AU CALCUL DU FLUX (CF LIGNE PRECEDENTE)',
     1   ' A LAMBD=',F10.3,'     CAVA=',I3)
      return
      END


      SUBROUTINE TRANGX(X,Y,NTOT,MTOT)
      DIMENSION X(NTOT), Y(MTOT, NTOT),TY(25)
C     INTEGER*4 X,Y,TX,TY
      DO 1  I=2,NTOT
      DO 2 K=2,I
      J=I-K+2
      IF(X(J).GE.X(J-1))  GO TO 1
      TX=X(J)
      DO 10 M=1,MTOT
10    TY(M) =Y(M,J)
      X(J)=X(J-1)
      DO 11 M=1,MTOT
11    Y(M,J) = Y(M,J-1)
      X(J-1) =TX
      DO 12 M=1,MTOT
12    Y(M,J-1) =TY(M)
2     CONTINUE
1     CONTINUE
      RETURN
      END


      SUBROUTINE STEP(X,Y,P,ERR,N)
      DIMENSION X(N),Y(N),ALY(200),P(N),ERR(N)
c     COMMON avec INAIT
      COMMON /GENE/ALY,XMILIEU,XGAUSS1,XGAUSS2,DEL,YM
     1  ,YG1,YG2,CONST,I,J
      XMILIEU=(X(I)+X(I+1))/2.
      DEL=X(I+1)-X(I)
      XGAUSS1=XMILIEU-DEL*CONST/2.
      XGAUSS2=XMILIEU+DEL*CONST/2.
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           ALY(J),ALY(J+1),ALY(J+2),ALY(J+3),XMILIEU,OUT)
      YML =EXP(OUT)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           ALY(J),ALY(J+1),ALY(J+2),ALY(J+3),XGAUSS1,OUT)
      YG1L=EXP(OUT)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           ALY(J),ALY(J+1),ALY(J+2),ALY(J+3),XGAUSS2,OUT)
      YG2L=EXP(OUT)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,YM)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XGAUSS1,YG1)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XGAUSS2,YG2)
      AMYS=(Y(I)+4.*YM+Y(I+1))/6.
      AMYG=(YG1+YG2)/2.
      AMY=0.6*AMYG+0.4*AMYS
      AMYLS=(Y(I)+4.*YML+Y(I+1))/6.
      AMYLG=(YG1L+YG2L)/2.
      AMYL=0.6*AMYLG+0.4*AMYLS
      ERR(I+1)=(AMYL-AMY)/AMY
      AMYRUSE=0.0*AMY+1.0*AMYL
      P(I+1)=P(I)+DEL*AMYRUSE
      RETURN
      END

      SUBROUTINE STEPB(X,Y,P,N)
      DIMENSION X(N),Y(N),ALY(200),P(N)
c       COMMON avec INAIT
      COMMON /GENE/ALY,XMILIEU,XGAUSS1,XGAUSS2,DEL,YM
     1  ,YG1,YG2,CONST,I,J
      XMILIEU=(X(I)+X(I+1))/2.
      DEL=X(I+1)-X(I)
      XGAUSS1=XMILIEU-DEL*CONST/2.
      XGAUSS2=XMILIEU+DEL*CONST/2.
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,YM)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XGAUSS1,YG1)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XGAUSS2,YG2)
      AMYS=(Y(I)+4.*YM+Y(I+1))/6.
      AMYG=(YG1+YG2)/2.
      AMY=0.6*AMYG+0.4*AMYS
      P(I+1)=P(I)+DEL*AMY
      RETURN
      END

      SUBROUTINE TURBUL(INTERP,IVTOT,TOLV,VVT,NTOT,TOL,VT)
      DIMENSION VVT(20),TOLV(20),VT(50),TOL(50)
      PRINT *,'   ENTREE DS TURBUL'
      IF(IVTOT.EQ.1)   THEN
            WRITE(6,*) ' VT CONSTANT'
            DO N=1,NTOT
            VT(N)=VVT(1) * 1E5
            END DO
                   ELSE
            WRITE(6,*) ' VT VARIABLE AVEC LA PROFONDEUR'
            WRITE(6,*) '     LOG TO'
            WRITE(6,101) (TOLV(I),I=1,IVTOT)
            WRITE(6,*) '     VT'
            WRITE(6,101) (VVT(I),I=1,IVTOT)
            IF(INTERP.EQ.1) CALL FTLIN3(IVTOT,TOLV,VVT,NTOT,TOL,VT)
            IF(INTERP.GT.1) CALL FT2   (IVTOT,TOLV,VVT,NTOT,TOL,VT)
            NT2=NTOT-2
            DO N=1,NT2,3
            WRITE(6,102) N,TOL(N),VT(N),(N+1),TOL(N+1),VT(N+1),
     1       (N+2),TOL(N+2),VT(N+2)
            END DO
                  DO N=1,NTOT
                  VT(N)=VT(N) * 1E5
                  END DO
      END IF
C
      RETURN
100   FORMAT(I5)
101   FORMAT(10F8.3)
102   FORMAT(3(I5,2F8.3,5X))
      END
C
      SUBROUTINE VOLUTE (S,ITOT,FI,J,PA,PS)
      DIMENSION PS(ITOT),S(ITOT),FI(*)
      DO 1 I=1,ITOT
1     PS(I)=0.
      JP1 =J+1
      IMJ=ITOT-J
      J2P =  2*J +1
      DO 2 I=JP1,IMJ
      DO 2 K=1,J2P
2     PS(I) =PS(I)  +  S(I-J+K-1) *FI(K)*PA
            DO I=1,JP1
            PS(I)=S(I)
            END DO
            DO I=IMJ,ITOT
            PS(I)=S(I)
            END DO
      RETURN
102   FORMAT (10F10.3)
100   FORMAT(I20)
      END




      FUNCTION CALCH(KII,IZ,KIEX1,IS1,KIEX2,IS2)
C     CALCUL DE CH VAN DER WAALS  APPROXIMATIONS D UNSOLD (1955)
C     SI IS1 ET IS2 SONT EGAUX A S P D F FORMULE 82-54  SINON 82-55
      REAL KII,KIEX1,KIEX2,NET1C,NET2C
        CHARACTER*1 IS,IBL,IS1,IS2
      DIMENSION IS(4),IL(4)
      DATA IBL/' '/
      DATA IS/'S','P','D','F'/
      DATA IL/1,-5,-17,-35/
      IF(IS1.NE.IBL)   GO TO 1
      C61=1.61E-33*(13.5*IZ / (KII-KIEX1)  )**2
      C62=1.61E-33*(13.5*IZ / (KII-KIEX2)  )**2
      GO TO 10
1     DO 2 I=1,4
      IF(IS1.EQ.IS(I))   GO TO 3
2     CONTINUE
3     IL1=IL(I)
      DO 4 I=1,4
      IF(IS2.EQ.IS(I))   GO TO 5
4     CONTINUE
5     IL2=IL(I)
      IZC=IZ**2
      NET1C=13.5 * IZC / (KII-KIEX1)
      NET2C=13.5 * IZC / (KII-KIEX2)
      C61=3.22E-34 * NET1C *(5*NET1C+IL1)/ IZC
      C62=3.22E-34 * NET2C *(5*NET2C+IL2)/ IZC
10    CALCH=C62-C61
      RETURN
100   FORMAT('  NET1C=',F5.2,'  NET2C=',F5.2,5X,2E14.4)
      END

      FUNCTION FAITK30(XX,X,Y,N)
C       interpolation
      DIMENSION X(0:N),Y(0:N)
      IF(XX.LT.X(2))THEN
      I=0
      GOTO 200
      ELSE IF(XX.GT.X(N-2))THEN
      I=N-3
      GOTO 200
      ELSE
            DO J=2,N-2
                  IF(XX.LE.X(J)) GO TO 100
            END DO
      ENDIF
100   CONTINUE
      I=J-2
c
200   CALL NAITK3(X(I),X(I+1),X(I+2),X(I+3),
     1           Y(I),Y(I+1),Y(I+2),Y(I+3),XX,RESULTA)
      FAITK30=RESULTA
      RETURN
      END

      FUNCTION FTETA0(PG,TETA)
C     EXTRAPOLE TETA(PG) POUR PG=0
c     ***TETA1 extrapolation parabolique sur les 3 derniers pts
c     ***TETA2      "             "      sur les pts 2 3 et 4
c     ***TETA3      "        lineaire    sur les 2 derniers pts
c     (ici seul TETA3 est utilise)
      REAL*4 PG(50), TETA(50), PP1(5),TT1(5),PP2(5),TT2(5)
      LOGICAL ECRIT
      ECRIT=.FALSE.
      PP1(1)=PG(1)
      TT1(1)=TETA(1)
      IF(ECRIT) write(6,*) PG(1), TETA(1)
      DO I=2,5
      PP1(I)=PG(I)
      PP2(I-1)=PG(I)
      TT1(I)=TETA(I)
      TT2(I-1)=TETA(I)
      IF(ECRIT) write(6,*) PG(I), TETA(I)
      END DO
c     TETA1=FT(0.,5,PP1,TT1)
c     TETA2=FT(0.,4,PP2,TT2)
      TETA3=TT1(1) - PP1(1) * (TT1(1)-TT1(2)) / (PP1(1)-PP1(2))
      IF(ECRIT) WRITE(6,*)TETA3
      FTETA0= TETA3
      RETURN
      END


      FUNCTION FT(T,N,X,F)
C     INTERPOLATION D UNE LISTE A PAS NON CONSTANT
      DIMENSION X(N),F(N)
      DO 1 J=1,N
      I=J
      IF(T-X(J))3,2,1
2     FT=F(J)
      RETURN
1     CONTINUE
3     IF(I.EQ.1)   I=2
      IF(I.GE.N)   I=N-1
      T0=T-X(I-1)
      T1=T-X(I)
      T2=T-X(I+1)
      U0=X(I+1)-X(I-1)
      U1=X(I+1)-X(I)
      U2=X(I)-X(I-1)
      A=T0/U0
      B=T1/U1
      C=T2/U2
      D=T0/U1
      E=T1/U0
      FT=F(I+1)*A*B - F(I)*D*C + F(I-1)*E*C
      RETURN
      END


      FUNCTION GAM(EELEM,IONI,LBDA,KIEX)
      REAL LBDA,KIEX,LAMDA
      INTEGER ELMAX
        CHARACTER*2 ELOUR,ELEM,EELEM,ELG
      DIMENSION GALOG1(5,20,4),GALOG2(5,20,4),ELG(20),AKI(4),F(4),
     1   ELOUR(6),LAMDA(5),AL(5)
      COMMON /COM1/ GALOG1,GALOG2,ELG,AKI,ELMAX,LAMDA,LMAX,KIMAX
      DATA ELOUR/'LA','CE','DY','ND','EU','SM'/
      ELEM=EELEM
      DO 8 N=2,6
      IF(ELEM.EQ.ELOUR(N)) ELEM = ELOUR(1)
8     CONTINUE
      LM1=LMAX-1
C     WRITE(6,101)LMAX,LM1
      DO 17 L=1,LM1
17    AL(L)=(LAMDA(L)+LAMDA(L+1))/2
C     WRITE(6,102) (AL(L),L=1,LM1)
      DO 18 LL=1,LM1
      L=LL
C     WRITE(6,105)LL,L,LBDA,AL(LL)
      IF(LBDA.GE.AL(LL))GO TO 18
      GO TO 10
18    CONTINUE
      L=L+1
C10   WRITE(6,103) L
C     WRITE(6,106) (ELG(I),I=1,ELMAX)
10    DO 11 IL=1,ELMAX
      I=IL
      IF(ELEM.EQ.ELG(I))   GO TO 12
11    CONTINUE
      WRITE(6,100)   ELEM
      GAM=200.
      RETURN
12    JL=I
C     WRITE(6,107)JL
      IF(IONI.EQ.2)   GO TO 13
      DO 14 K=1,KIMAX
14    F(K) = GALOG1(L,JL,K)
      GO TO 16
13    DO 15 K=1,KIMAX
15    F(K) = GALOG2(L,JL,K)
16    GAM=FT(KIEX,KIMAX,AKI,F)
      RETURN
100   FORMAT(///19H MANQUE GAMMA POUR ,A2)
101   FORMAT('   LMAX=',I5,5X,'LM1=',I5)
102   FORMAT(8F10.2)
103   FORMAT('   L=',I3)
105   FORMAT('  LL=',I3,'   L=',I3,4X,'LBDA',F8.2,4X,'AL',F8.2)
106   FORMAT(20(1X,A2))
107   FORMAT('    JL=',I3)
      END


      FUNCTION IINF(FR,ITOT,IA,IZ)
      DIMENSION FR(ITOT)
      IA2=IA+1
      IINF=IA
      FMIN=FR(IA)
      DO 1 I=IA2,IZ
      IF(FR(I).GT.FMIN)   GO TO 1
      FMIN=FR(I)
      IINF=I
1     CONTINUE
      RETURN
      END


      FUNCTION ISUP(FR,ITOT,IA,IZ)
C     UNE FONCTION FR EST CONNUE EN ITOT POINTS. ON CHERCHE ENTRE
C     LES POINTS IA ET IZ QUEL EST L INDICE I OU CETTE FONCTION
C     EST MAXIMUM.
      DIMENSION FR(ITOT)
      IA2=IA+1
      ISUP=IA
      FMAX=FR(IA)
      DO 1 I=IA2,IZ
      IF(FR(I).LT.FMAX)   GO TO 1
      FMAX=FR(I)
      ISUP=I
1     CONTINUE
      RETURN
      END


      FUNCTION MAXI(IFA,NTOT,IA,IZ)
      DIMENSION IFA(NTOT)
      MAXI=IFA(IA)
      IA2=IA+1
      DO I=IA2,IZ
      IF(IFA(I).GT.MAXI) THEN
      MAXI=IFA(I)
      END IF
      END DO
      RETURN
      END


      FUNCTION XXSOL(WWSLOG,IGR)
      DIMENSION   WSLOG(21),X1(21),X2(21),X3(21)
C     X1   CDC SUN8   MG1   VT=0.6   CH=0.5E-31
C     X2   CDC SUN8   TI1   VT=0.6   CH=0.5E-31
C     X3   CDC SUN8   BA2   VT=0.6   CH=0.5E-31
      DATA WSLOG/-6.0,-5.9,-5.8,-5.7,-5.6,-5.5,-5.4,-5.3,-5.2,-5.1,
     1    -5.0,-4.9,-4.8,-4.7,-4.6,-4.5,-4.4,-4.3,-4.2,-4.1,-4.0/,
     2   X1/-6.00,-5.90,-5.79,-5.68,-5.58,-5.47,-5.35,-5.21,-5.08,
     3   -4.92,-4.76,-4.54,-4.25,-3.90,-3.42,-3.04,-2.72,-2.42,
     4   -2.20,-1.98,-1.79/,
     5   X2/-6.00,-5.90,-5.79,-5.67,-5.56,-5.43,-5.30,-5.16,-5.01,
     6   -4.83,-4.60,-4.30,-3.90,-3.50,-3.14,-2.86,-2.60,-2.36,
     7   -2.14,-1.90,-1.70/,
     8   X3/-6.00,-5.88,-5.75,-5.66,-5.53,-5.40,-5.25,-5.08,-4.88,
     9   -4.60,-4.24,-3.90,-3.60,-3.30,-3.04,-2.82,-2.58,-2.35,
     1   -2.14,-1.92,-1.80/
      IF(WWSLOG.GT.(-6.)  )  GO TO 1
      XXSOL=WWSLOG
      RETURN
1     IF(IGR.NE.2)   GO TO 3
      XXSOL=FT(WWSLOG,21,WSLOG,X2)
      RETURN
3     IF(IGR.NE.1)   GO TO 4
      XXSOL=FT(WWSLOG,21,WSLOG,X1)
      RETURN
4     XXSOL=FT(WWSLOG,21,WSLOG,X3)
      RETURN
      END

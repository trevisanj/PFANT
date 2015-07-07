C                       ABOND/ Janv 1994
C
C
      character*20 ABONFILE,WFILE
        character*1 apos, IBL
      REAL*8   LZERO,LFIN,LAMBD,LAMBDA,AZ, XX1, DIFF, mass, AAPRED
      REAL*4  B(0:50), TO(0:50), TOl2(0:50), TOc(0:50), WPRED, NH
      LOGICAL PROFIL,SYM,IGAM,PTDISK,ANAL,CHIMP,TOUSEL,BLACKW
      REAL NU,M,KI1,KI2,KIEX,KB,ME,KAPPA,KC,KA,KAP,MU,NHE,K5,
     1   MM,KKI1,KKI2,KIES,KII, KAPPA2, PHIV
      INTEGER   FINPAR,FIN,DDTOT,DTOT,D,AB,ABTOT,DIVTOT,CAVA, ABind
      DIMENSION EL(40),TINI(40),PA(40),KKI1(40),KKI2(40),MM(40),
     1 JKMAX(40),TABU(40,3,33),ALISTU(33),U(3),TTT(34),ABSO(40),
     2 NH(50),TETA(50),PE(50),UE(50),PHN(50),KC(50),KAP(50),
     3 DELTL(29),P(50),DELTA(50),A(50),TIT2(18),TOTKAP(2),
     4 KAPPA(108,50),ZLPE(53),XX(108),AAL(108),WTL(108),
     5 ECART(50),W(108),F(50,108),VT(50),ALPH(50),TOL(50),FMT(17),
     6 PH2(50),TOLV(20),VVT(50),T5L(50),AVV(20),PG(50),K5(50),
     7 BEL(25),IOB(25), KAPPA2(50), PHIV(50)
        dimension ZZP0(30) ! pour Lectur et abonio
C     Common ENTRE LE PROGRAMME PRINC ET LE S P  READER
      COMMON /COM6/TEFF,GLOG,ASALOG,ASALALF,NHE,TIABS(5),TITRE(5)
C     Common ENTRE LE PROGRAMME PRINCIPAL ET LE SP FLIN1
      COMMON/TOTO/TO     /FAIL/ERR(50)
C     Common ENTRE RAI8 ET LE SP D ABSORPTION CONTINUE
      COMMON /LECT1/AMET,BHE  /LECT2/ZP(30),ZM(30),BIDA(85)
        COMMON /ABSO2/NMETA /SAPE/BIDB(5),ZNU(30)
        COMMON /SAPU/BIDC(3),ZNH(12) /ABSO1/NM /TIT/TITABS(5)
      DATA apos/''''/
      DATA DELTL/5.,0.,2.,0.,1.,0.,0.5,0.,0.5,0.,0.40,0.,0.2,0.,
     1   0.2,0.,0.05,0.,.05,0.,.02,0.,.02,0.,.02,0.,.02,0.,.02/
        DATA  C/2.997929E+10/,H/6.6252E-27/,KB/1.38046E-16/,R/8.3170E+7/,
     1      PI/3.141593/,C1/4.8298E+15/,C2/8.8525E-13/,C4/2.1179E+8/,
     2      C6/3.76727E+11/,DEUXR/1.6634E+8/
C     C1=2.*(2*PI*KB*ME)**1.5/ H**3
C     C2=PI*E**2 / (ME*C**2)
C     C4=8.*R/PI
      C5= 2.*PI* (3.*PI**2/2.44)**0.4
C     C6=4.*PI*C
      C7=12398.54
      IBL = ' '
c
C     ------------------initialisations-------------------------------
      open(unit=7,file='abon2.dat',status='old')
        open(unit=8,file='abon2.prt',status='unknown')
      KIK=0 ! FORMULE A 6 OU 7 PTS POUR CALCUL DU FLUX OU INT
      EPSI=1.E-37
      IPIP=0   ! IPIP VAUDRA 1 APRES LA 1ERE ERREUR DANS FLIN1
C
      ANAL=.FALSE.
      PROFIL=.FALSE.
      SYM=.TRUE.
      IGAM=.FALSE.
      NBLEND=1
      DDTOT=15
      DTOT=29
      DIVTOT=31
      S=0.
            DO  D=1,DTOT,2
            S=S+DELTL(D)
            END DO
C     OPEN (UNIT=35,FILE='raies.dat',STATUS='OLD')
C
C     ****************************************************************
C
C                       I
C     LECTURE DES FONCTIONS DE PARTITION
C     ET DES DONNEES POUR LE CALCUL DU COEF D'ABSORPTION CONTINUE
C
C     *****************************************************************
C
      OPEN (UNIT=25,FILE='partit.dat',STATUS='OLD')
C
      DO J=1,40
      READ(25,108) EL(J),TINI(J),PA(J),JKMAX(J),MM(J),
     1   KKI1(J),KKI2(J),ABSO(J),FINPAR
      IF(FINPAR.EQ.1)   GO TO  203
      KMAX=JKMAX(J)
      READ(25,102) ((TABU(J,L,K),L=1,3),K=1,KMAX)
c     IF (J.EQ.18) THEN
c     print *, '*********'
c     print 10101, EL(J)
c10101      FORMAT(A4)
c      DO K=1,KMAX
c        PRINT *, 0.4+0.05*(K-1), TABU(J,1,K)
c      END DO
c     print *, '*********'
c      DO K=1,KMAX
c        PRINT *, 0.4+0.05*(K-1), TABU(J,2,K)
c      END DO
c     STOP
c     ENDIF
      END DO
203   NPAR=J-1
      CALL LECTUR(ZZP0,ZZPH)
      WRITE(6,160) NM
      WRITE(6,166) (ZP(I),ZM(I),I=1,NM)
        WRITE(8,160) NM
        WRITE(8,166) (ZP(I),ZM(I),I=1,NM)
      NABS =NMETA+7
C     NMETA = NBRE D ABSORBANTS METAL. CONSIDERES DANS ABSORN
C     NABS = NBRE TOTAL D ABSORBANTS
      IX=0
C     ****************************************************************I
C
C                       II
C     LECTURE  DU MODELE  D OU  VT(N),NH(N),TETA(N),PE(N)
C
C     *****************************************************************
      READ(7,*)    VVT(1)
      write(6,*) 'vt = ',VVT(1)
      IVTOT=1
            IF (VVT(1).GT.900) THEN   ! VT VARIABLE AVEC PROFONDEUR
            READ(7,*) IVTOT
            READ(7,*) (TOLV(I),I=1,IVTOT)
            READ(7,*) (VVT(I),I=1,IVTOT)
            END IF


      CALL READERN(NH,TETA,PE,PG,T5l,NTOT)

c     print *, NH
        TETAEF=5040/TEFF
      BHE= NHE
        ASASOL=10**ASALOG
        COEFALF=10**ASALALF
        CALL ABONIO(ZZP0,ZZPH,ASASOL,COEFALF)
C
      INTERP=1   ! INTERP LINEAIRE DE VT (SI PARABOLIQUE =2)
      CALL TURBUL(INTERP,IVTOT,TOLV,VVT,NTOT,T5L,VT)
C     ****************************************************************
C
C                       III
C     CALCUL DE QUANTITES DEPENDANT DE LAMBD ET DU MODELE
C     B(N),KC(N),FC
C           -LECTURE DES DONNEES RAIES-
C
C     ****************************************************************
      READ(7,* ) PTDISK,MU,CORCH,TOUSEL,WLIM1,WLIM2
      IF (CORCH.EQ.0)BLACKW=.TRUE.
      read(7,*) WFILE, ABONFILE
      OPEN(UNIT=18,FILE=ABONFILE,STATUS='unknown')
      OPEN (UNIT=35,FILE=WFILE,STATUS='OLD')
         IF(.NOT.TOUSEL) then
           READ (7,*) NEL
           READ(7,110) (BEL(N),IOB(N),n=1,NEL)
           END IF
      WRITE(8,*)' '
        WRITE(8,*)' '
C
C     ------------------
C     ecriture des caracteristiques du modele
      IF(IVTOT.EQ.1)    then
        WRITE(6,164)TEFF,GLOG,ASALOG,ASALALF,NHE,VVT(1),(TITRE(I),I=1,5)
        WRITE(8,164)TEFF,GLOG,ASALOG,ASALALF,NHE,VVT(1),(TITRE(I),I=1,5)
                      else
        WRITE(6,165)  TEFF,GLOG,ASALOG,ASALALF,NHE,(TITRE(I),I=1,5)
        WRITE(6,*) '  Loi de variation de VT avec log TO'
        WRITE(6,105)  ( TOLV(I),VVT(I),I=1,IVTOT)
        WRITE(8,165)  TEFF,GLOG,ASALOG,ASALALF,NHE,(TITRE(I),I=1,5)
      WRITE(8,*) '  Loi de variation de VT avec log TO'
      WRITE(8,105)  ( TOLV(I),VVT(I),I=1,IVTOT)
      END IF
c
      WRITE(6,156)DIVTOT
      WRITE(6,152)(DELTL(D),D=1,DTOT,2)
      WRITE(6,*)' '
        WRITE(8,156)DIVTOT
        WRITE(8,152)(DELTL(D),D=1,DTOT,2)
        WRITE(8,*)' '
c
        write(6,121) wlim1,wlim2
        WRITE(6,*)' '
        write(8,121) wlim1,wlim2
        WRITE(8,*)' '
c
         IF(.NOT.TOUSEL) then
           Write(6,*) '    calcul des abondances des elements suivants'
           WRITE(6,118) (BEL(N),IOB(N),n=1,NEL)
           Write(8,*) '    calcul des abondances des elements suivants'
           WRITE(8,118) (BEL(N),IOB(N),n=1,NEL)
           END IF
        WRITE(6,*)' '
      WRITE(8,*)' '
c     ------------------lecture du fichier raies-----------------
      READ(35,107) (TIT2(I),I=1,17)
        WRITE(6,163) (TIT2(I),I=1,17)
      WRITE(8,163) (TIT2(I),I=1,17)
      WRITE(6,*)' '
        WRITE(8,*)' '
              write(6,*)' Resultats dans le fichier ',ABONFILE
              WRITE(18,107) (TIT2(I),I=1,17)
              WRITE(18,120) VVT(1),TEFF,GLOG,ASALOG,NHE,
     *       wlim1,wlim2
c            WRITE(18,*) '  '
      wlim1=wlim1-0.1   ! pour que l'on prenne aussi les raies W=Wlim1
      READ(35,109)   FMT
      READ(35,109)
C           raies.dat    ------------------------

1001  READ(35,1010,END=300) LAMBDA,ELEM,IONI,KIEX,MUL,GFL,ISI,
     1      ISS,CH, GAMMAR,GAMMAE,WOB,WERR
 1010 FORMAT(F7.2,1X,A2,I1,F5.2,I4,F7.2,2a1,e9.3, 2f1.0,F7.1,F7.2)

C     write(6,*) LAMBDA,ELEM,IONI,WOB,WERR
C     --------------------------------------------------------------
C
C   KIEX POTENTIEL D EXC DU NIV INFERIEUR DE LA TRANSITION
C   KIES POTENTIEL D EXC DU NIV SUPERIEUR DE LA TRANSITION
            IF(.NOT.TOUSEL)  then
                DO n=1,NEL
                AEL=BEL(N)
                IOO=IOB(N)
            IF( (ELEM.EQ.AEL) .AND. (IONI.EQ.IOO) )   GO TO 1002
                END DO
                GO TO 1001
            END IF
c
 1002   if ( (WOB.LE.WLIM1 ).OR.(WOB.GE.WLIM2)) then ! raies hors limites
      WRITE(8,116) ELEM,IONI,LAMBDA,WOB
      AAA=99.99
      AETSO=99.99
c           WRITE(18,119)LAMBDA,ELEM,IONI,KIEX,MUL,GFL,CH,WOB,
c     1     AAA,AETSO
      GO TO 1001
      END IF   ! fin du if Wobserve trop petit ou trop grand
c
      IF(GFL.LT.-98.) then                      !gf n'existe pas
      WRITE(8,117) ELEM,IONI,LAMBDA,GFL
      AAA=99.99
      AETSO=99.99
c           WRITE(18,119)LAMBDA,ELEM,IONI,KIEX,MUL,GFL,CH,WOB,
c     1     AAA,AETSO
      GO TO 1001
      END IF   ! fin du if gf inconnu
c
      LAMBD=LAMBDA
      BLAMB=LAMBD
      WOL=-3+ ALOG10(WOB/BLAMB)
C  ADDED VHI: compute WOB1, WOB2, WOL1 and WOL2 for WOB +/- WERR
      WOB1 = WOB-WERR
      WOB2 = WOB+WERR
      WOL1=0.
      IF ( WOB1 .GT. 0.) WOL1=-3+ ALOG10(WOB1/BLAMB)
      WOL2=-3+ ALOG10(WOB2/BLAMB)
C  END OF THE ADDED VHI
      KIES=(C7/LAMBDA) + KIEX
      LZERO=LAMBD-S
      IF(GAMMAR.LT.EPSI)   GAMMAR=2.21E15 / BLAMB**2
      GAE=GAMMAE
      AL   =1.E-8*LAMBDA
      GF=10.**GFL
      GGF=GF * C2 * (AL**2)
      LFIN=LZERO+S

c     print *, BLAMB
c     do i = 1, 10000
c     BLAMB = 1000 + i * 5


      CALL BKF(NH,TETA,PE,PG,NTOT,BLAMB,
     1       B,ALPH,PHN,PH2,KC,PTDISK,MU,KIK,FC)


c     write(18,*) BLAMB, FC*1e8*c/BLAMB**2*pi
c     end do
c     STOP
c     print *, FC
c     STOP
C
C   ***************************************************************
C
C                       IV
C     CALCUL DES QUANTITES DEPENDANT DE LAMBD DU MODELE
C     ET DE L ELEMENT : P(J,N),A(J,N),DELTA(J,N)
C     (ATTENTION    P(J,N) = POPULATION * FACTEUR D EMISS INDUITE)
C
C   ****************************************************************
            DO  I=1,NPAR
            II=I
            Q=EL(II)
            QQ=ELEM
            IF(Q.EQ.QQ)  GO TO 412
            END DO
      WRITE(6,170) ELEM
      STOP

412   KI1   =KKI1(II)
      KI2   =KKI2(II)
      M=MM(II)
      ABSTAR =ASASOL * 10.**(ABSO(II)-12.)
      IF(CH.GT.EPSI)   CHIMP=.TRUE.  !    CHIMP= CH IMPOSE
            IF(CH.LT.EPSI) then
            CHIMP=.FALSE.
            KII=KI1
            IF(IONI.GT.1)KII=KI2
            CH=CALCH(KII,IONI,KIEX,ISI,KIES,ISS)
            IF (BLACKW) CORCH=0.67*KIEX+1
            CH=CH*CORCH
            END IF

      CALL POPUL2(TETA,PE,NTOT,TINI,PA,II,
     1      JKMAX,IONI,KI1,KI2,TABU,P)


      DO  N=1,NTOT
      T=5040./TETA(N)
      TOP=10.**(-KIEX   *TETA(N))
      TAP= 1.-ALPH(N)
c     print *, P(N)    ! 6.02e23/1.26 (Avogadro/peso molecular médio)
      P(N)=P(N) * TOP *TAP
      DELTA(N)  = AL   /C*SQRT(VT(N)**2+DEUXR*T/M   )
      VREL      = SQRT ( C4 *T * (1.+1./M   ))
      GAMMAH    = C5 * CH**0.4 * VREL**0.6
      IF(N.EQ.10)   GAH=GAMMAH
      GAMMA=GAMMAR+(GAMMAE*PE(N)+GAMMAH*(PHN(N)+1.0146*PH2(N)))/(KB*T)

c     GAMMA=GAMMAR+(GAMMAE*PE(N)+GAMMAH*(PHN(N)+PE(N)*0.41336+
c     1    0.85*PH2(N)))/(KB*T)
c     WRITE(18,124) N, GAMMAR,PE(N), GAMMAH, PHN(N),PH2(N),T,CH,
c     1      (GAMMAE*PE(N)+GAMMAH*(PHN(N)+PE(N)*0.41336+
c     1    0.85*PH2(N)))/(KB*T)

C     ON A SUPPOSE  GAMMA(H2) / GAMMA(H) = 1.0146   CE QUI EST VRAI
C     POUR  CA1  4227    CE RAPPORT EST INSENSIBLE A L ATOME ET A
C     LA TRANSITION CONSIDEREE
      A(N)  = GAMMA * AL**2 /(C6*DELTA(N)  )
      END DO                              !fin du do n=1,ntot
c
c     STOP
      IF(CHIMP)   WRITE(8,114)
      IF(.NOT.CHIMP)   WRITE(8,115)   CORCH
      IF(.NOT.PTDISK)   WRITE(8,104) MUL
      IF(.NOT.PTDISK)   WRITE(8,103)   ELEM,IONI,LAMBDA,ISI,ISS,GFL,
     1   KI1,KI2,KIEX,KIES,M,GAMMAR,GAE,CH,GAH,FC
      IF(PTDISK)   WRITE(8,101) MUL
      IF(PTDISK)        WRITE(8,103)   ELEM,IONI,LAMBDA,ISI,ISS,GFL,
     1   KI1,KI2,KIEX,KIES,M,GAMMAR,GAE,CH,GAH,FC,MU
C
C   *****************************************************************
C
C                       V
C     CALCUL A CHAQUE NIVEAU DU COEF D AB SELECTIF POUR DIVERSES
C     ABONDANCES
C
C   *****************************************************************
      ECART(1) = S
            DO D=3,DIVTOT,2
            ECART(D)=ECART(D-2) - DELTL(D-2)
            ECART(D-1) = (ECART(D) + ECART(D-2) )  / 2
            END DO
C
c     XX(1) = ABSTAR/8
c500  ABFIN= XX(1) * 64
c     PR=2
c           DO  AB=2,108
c           XX(AB)=XX(AB-1)*PR
c           IF(XX(AB).GT.ABFIN)   GO TO 30
c           END DO
c30   ABTOT=AB-1
C

      XX1=7.45
500   XX(1) = 10**(XX1-12)

      DO  AB=2,108
        XX1 = XX1+0.001
        XX(AB) = 10**(XX1-12)
      END DO
      ABTOT=AB-1
      XX1 = 7.500
      XX(ABTOT) = 10**(XX1-12)

            DO  D=1,DIVTOT
                  DO  N=1,NTOT
                  V= ABS (ECART(D)*1.E-08/DELTA(N)   )
                  AA=A(N)
                  DEL=DELTA(N)
                  CALL HJENOR(AA,V,DEL,PHI)
C                 PHI=H(A,V)/( DELTA(J,N) *  PI**0.5)
                  KA    = PHI * P(N)  *GGF
                  IF (D.EQ.DIVTOT) THEN
                   CALL HJENOR(AA,0,DEL,PHI)
                   PHIV(N) = PHI*DEL*pi**0.5
                  END IF
                        DO AB=1,ABTOT
                        KAPPA(AB,N) = KA *XX(AB)
                        END DO
                  END DO
C
                  DO  AB =1,ABTOT
                        DO  N=1,NTOT
                        KAP(N)  =KAPPA (AB,N)+KC(N)
                        END DO
                  CALL FLIN1 (KAP,B,NH,NTOT,PTDISK,MU,FFF,KIK,CAVA)
C                 IF(CAVA.NE.0)   STOP
                  F(D,AB)=1-FFF/FC
                  END DO
            END DO      ! fin du do D=1,DIVTOT


c     do D=1,DTOT
c     print 10101, F(D,ABTOT), DELTL(D), ECART(D)
c     end do
c10101   format(e14.4,f12.4, f12.4)
c     STOP
C *********************************************************************
C
C                       VI
C     CALCUL DE  W  POUR DIVERSES ABONDANCES
C
C *********************************************************************
      IF(ANAL)   WRITE(6,162)
C
      DO  AB=1,ABTOT
      W(AB)=0
            DO  D=1,DTOT,2
            W(AB)=W(AB)+DELTL(D)*(F(D,AB)+4*F(D+1,AB)+F(D+2,AB))/6
            END DO
      WEX1=F(1,AB)*(LAMBD-LZERO)
c     WEX1 = 0
      W(AB)=2000.*(W(AB)+WEX1)
      WTL(AB)    = -3 + ALOG10(W(AB)/BLAMB)
      AAL(AB)=12+ALOG10(XX(AB))

      IF(ANAL)
     1WRITE(6,159) AAL(AB),W(AB),WTL(AB),F(1,AB),WEX1,F(DIVTOT,AB)
      END DO                        !fin du DO AB=1,ABTOT



C AJJOUTER ICI UNE BOUCLE SUR WOBS +/- WERR
      IF ( (WOB.GE.W(1)) .AND. (WOB.LE.W(ABTOT-1))  ) then
C NOMINAL WOB
      DIFF = 10000
      DO AB=1,ABTOT-1
         IF(DIFF > ABS(WOB-W(AB))) THEN
           AAA = AAL(AB)
           AETSO= AAA-ABSO(II)
           DIFF = (WOB-W(AB))
           ABind = AB
c          print *, diff, WOB, WPRED, AAA
         ENDIF
      END DO
      WPRED = W(ABTOT)
      AAPRED = AAL(ABTOT)


      DO N=1, NTOT
         KAPPA2(N) = KAPPA(ABind, N)
      END DO

      TOl2(0)=0.
      TOl2(1)=NH(1)*(KAPPA(ABind,1)-(KAPPA(ABind,2)-KAPPA(ABind,1)))
      TOl2(1)=TOl2(1)/(NH(2)-NH(1))*NH(1)/2.
C     write(6,*)NH(1),NH(2),Kap(1),KAP(2),ntot
c     read(5,*)
        CALL INTEGRA(NH,KAPPA2,TOl2,NTOT,TOl2(1))

      TOc(0)=0.
        TOc(1)=NH(1)*(KC(1)-(KC(2)-KC(1))/(NH(2)-NH(1))*NH(1)/2.)
C     write(6,*)NH(1),NH(2),Kap(1),KAP(2),ntot
c     read(5,*)
        CALL INTEGRA(NH,KC,TOc,NTOT,TOc(1))

c     DO  D=1,DTOT
c     print *, F(D, ABind), FC, FFF
c     end do
c     STOP
C     WRITE(8,111) WOB,WERR,WOL,AAA,AETSO
C        write(6,119)LAMBDA,ELEM,IONI,KIEX,MUL,GFL,CH,WOB,WERR,AAA,AETSO
C     WRITE(18,122) LAMBDA,apos,ELEM,IONI,apos,KIEX,
C     1   MUL,GFL,CH,WOB,WERR,AAA,AETSO
C WOB - WERR
c     IF ( WOL1.NE.0. ) then
c        IF ( (WOB1.GE.W(1)) .AND. (WOB1.LE.W(ABTOT)) ) then
c        AAA1 = FT(WOL1,ABTOT,WTL,AAL)
c        else
c        AAA1 = -99.
c        endif
c     endif
c     WRITE(8,111) WOB,WERR,WOL1,AAA1,AETSO
C WOB +WERR
c     IF ( (WOB2.GE.W(1)) .AND. (WOB2.LE.W(ABTOT))  ) then
c        AAA2 = FT(WOL2,ABTOT,WTL,AAL)
c        else
c        AAA2 = 99.
c     endif
c     WRITE(8,111) WOB,WERR,WOL2,AAA2,AETSO
119    FORMAT(F10.2,1X,A2,I1,F5.2,I5,F6.2,2X,E10.3,2X,2F7.1,4F7.2,
     1 F6.1,F9.2)
      write(6,119) LAMBDA,ELEM,IONI,KIEX,MUL,GFL,CH,WOB,
     1   WERR,AAA,AETSO,AAA,AAA,WPRED, WPRED
c      write(6,*) W
C     WRITE(18,122) LAMBDA,apos,ELEM,IONI,apos,KIEX,
C     1   MUL,GFL,CH,WOB,WERR,AAA,AETSO,AAA1,AAA2
C       VHI June 2006: output also the predicted W, to check microturb
C
      WRITE(18,122) LAMBDA,apos,ELEM,IONI,apos,KIEX,
     1   MUL,GFL,CH,WOB,WERR,AAA,AETSO,AAA,AAA,WPRED, AAPRED


c     DO N = 1, NTOT
c       mass = 6.02e23/1.354536
c       WRITE(18, 123) KC(N)*mass,KAPPA(ABTOT, N)*mass,5040./TETA(N),
c     1  (1-exp(-H*C/(AL*KB*(5040./TETA(N))))),
c     1  DELTA(N), PHIV(N), A(N), P(N)*mass/(1.-ALPH(N))*XX(ABTOT)
c     END DO
      GO TO 1001
                                          else
            IF(WOB.LT.W(1))  then
            XX1= 12+ALOG10(XX(1))-0.1
                           else     !   (WOB>W(ABTOT)
            XX1= 12+ALOG10(XX(1))+0.1
            END IF
      IF(ANAL) WRITE(6,*) (' ')
      GO TO 500
      END IF                  !     fin du if    W(1)< WOB <W(ABTOT)
c
c
300   write(6,*)' Resultats dans le fichier ',ABONFILE
        STOP                    !on a deroule ttes les raies ; on s'arrete.
C *********************************************************************
C
C                       VII
C     ZONE DE DEFINITION DES FORMATS
C
C *********************************************************************
101   FORMAT(1X,I4,8H  LAMBDA,9X,4HL GF,3X,3HKI1,4X,3HKI2,3X,4HKIEX,3X,
     1   4HKIES,4X,1HM,5X,6HGAMMAR,5X,6HGAMMAE,5X,2HCH,6X,6HGAMMAH,6X,
     2   2HIC,8X,2HMU)
102   FORMAT(13F6.4)
103   FORMAT(1X,A2,I1,2X,F8.2,2X,2A1,2X,F6.2,4F7.3,F6.1,1X,3E10.3,
     1 E10.2,E12.5,F4.1)
104   FORMAT(1X,I4,8H  LAMBDA,9X,4HL GF,3X,3HKI1,4X,3HKI2,3X,4HKIEX,3X,
     1   4HKIES,4X,1HM,5X,6HGAMMAR,5X,6HGAMMAE,5X,2HCH,6X,6HGAMMAH,6X,
     2   2HFC)
105   FORMAT(3X,F7.2,F10.2 )
107   FORMAT(17A4)
108   FORMAT(A2,2F5.2,I3,4F10.2,24X,I1)
109   FORMAT(6X,17A4)
110    FORMAT(20(1x,A2,I1))
111   FORMAT(/'   W OBS=',F5.1,10X,'LOG W OBS=', F6.2,10X,
     1   'LOG AB  CALCULE=',F7.2,5X,'LOG A/ASOL =',F5.2///)
112   FORMAT('   VT AUX DIFFERENTS NIVEAUX DU MODELE')
113   FORMAT(5(I4,F6.2,10X) )
114   FORMAT(80X,'CH IMPOSE')
115   FORMAT(80X,'CH VAN DER WAALS *',F4.1)
116   FORMAT(' W hors limites',7X,A2,I1,4X,F8.2,5X,'W OBS=',f7.1)
117   FORMAT(1X,A2,I1,2X,F8.2,5X,'L GF=',F8.2,
     1  ' VALEUR L GF INFERIEURE A -99 CONSIDEREE IMPOSSIBLE')
118   FORMAT(4X,25(1X,A2,I1))
120   FORMAT(F5.1,F7.0,F5.2,2F7.3, F6.1,' > W >',F6.1)
121     FORMAT(' On n''utilise que les raies avec W >',F6.1,
     1     'mA  et W <',F6.1,'mA')
 122    FORMAT(' ', F10.2,1X,A1,A2,I1,A1,F5.2,I5,F6.2,2X,E10.3,2X,
     1     2F7.1,4F9.4,F6.1, F9.4)
123     FORMAT(2e12.4, f8.1, f10.5, e12.4, 2f10.5, e14.5)
124   FORMAT(i4, e12.3, f10.4, e12.3, 2f14.3, f8.1, e12.3, e12.3)
129   FORMAT(19F4.1,I4)
130   FORMAT(' ENNUI DANS FLIN1 CONTINU    CAVA=',I3)
131   FORMAT(' ENNUI DS FLIN1 SELECTIF CAVA=',I3,'  AB',I3,'  D',I3,
     1 '  NIV',I3,'  ERR',F5.2)
132   FORMAT(4(I4,F7.3,E12.5,F5.2) )
151   FORMAT (1H , 16HPOPULATION NULLE)
152   FORMAT(20F6.2)
153   FORMAT (/)
154   FORMAT (1H1)
156   FORMAT(5X,'NBRE DE POINTS CALCULES DANS LA RAIE',I5)
159   FORMAT(F11.3,F13.2,F18.3,2E20.2,E14.4)
160   FORMAT(I10,2F15.2,4E18.2)
162   FORMAT(5X,'LOG(AB )',8X,1HW,9X,14H LOG(W/LAMBDA),3X,
     1   19HPREM R CALC DS RAIE,5X,7HW EXTR1,10X,2HRC)
163   FORMAT(5X,20A4//)
164   FORMAT(' TEFF=',F7.0,3X,'LOG G=',F5.2,3X,'[A/H] =',F6.2,2X,
     1 '[alf/A]=',F6.2,'  NHE=',F6.3,/2X,'VT(1) =',F6.2,'KM/S',5X,5A4/)
165   FORMAT(' TEFF=',F7.0,3X,'LOG G=',F5.2,3X,'[A/H] =',F6.2,2X,
     1 '[alf/A]=',F6.2,'  NHE=',F6.3,/2X,5A4/)
166   FORMAT(11E11.4)
170   FORMAT(8X,30HMANQUE FCTION DE PARTITION DE ,A2)

      END

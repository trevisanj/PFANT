      SUBROUTINE READERH (NH,TETA,PE,PG,T5L,NTOT)
C     CE S.P. LIT SUR DISQUE ACCES DIRECT NH,TETA,PE,PG,T5L,NTOT
      DIMENSION NH(50),TETA(50),PE(50),PG(50),T5L(50),BID(11)
      REAL NH,NHE
        CHARACTER*4 BLC,TIT
      COMMON /COM6/TEFF,TETAEF,GLOG,ASASOL,NHE,TIT(5)
      COMMON/ALOG/ASALOG
      DATA   BLC/'    '/
      DO 7 I=1,5
7     TIT(I)=BLC
c     IF(IDEF.EQ.211939)   GO TO 10
      OPEN(UNIT=18,ACCESS='DIRECT',STATUS='OLD',
     1    FILE='modeles.dat',
     1 RECL=256)
      ID=1
c     IDEF=211939
10    READ(4,*) TETAEF,GLOG,ASALOG,NHE,INUM
      IF(INUM.GT.0)   ID=INUM
      WRITE(6,102)TETAEF,GLOG,ASALOG,NHE,INUM
C   SI L ON DESIRE IMPOSER UN MODELE  ON MET EN INUM LE NUM DU MODELE
C   SUR LE FICHIER ACCES DIRECT
9     READ(18, REC=ID) NTOT,DETAEF,DGLOG,DSALOG,DNHE,DAMET,TIT
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
      WRITE(6,110) NTOT
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
103   FORMAT(E20.4,4F15.4)
104   FORMAT('   ECRITURE DE TIT(1) ',A4,5X,Z4)
105   FORMAT(4F10.2,E15.4,5A4)
106   FORMAT(2X,4F5.2,I5)
107   FORMAT('     ETC.....')
110   FORMAT(1X,'NTOT=',I3)
      END

      SUBROUTINE BK(NH,TETA,PE,PG,NTOT,LAMBD,B,B1,B2,ALPH,PHN,PH2,
     1 CALFC,FC,KC,KC1,KC2,KCD,TTD,DTOT,PTDISK,MU,KIK,LZERO,LFIN)
      PARAMETER(NP=7000)
      INTEGER D,DTOT,CAVA
      LOGICAL PTDISK,CALFC,ECRIT
      REAL LAMBD,NH,MU,NU,KB,KC,KC1,KC2,LLZERO,LLFIN,NU1,NU2,KCD,
     1 LAMBDC,KCJ,KCN
      REAL*8 LZERO,LFIN
      DIMENSION B(0:50),TO(0:50),B1(0:50),B2(0:50)
      DIMENSION NH(50),TETA(50),PE(50),PG(50),KC(50),TOTKAP(2),
     1 ALPH(50),PHN(50),PH2(50),KC1(50),KC2(50)
      DIMENSION TTD(DTOT),KCD(DTOT,50),KCJ(2,50),KCN(2),LAMBDC(2)
      DIMENSION FTTC(NP)

      COMMON /LECT1/AMET,BHE/LECT2/ZP(30),ZM(30),BIDA(85)/ABSO2/NMETA
     1  /SAPE/BIDB(5),ZNU(30)/SAPU/BIDC(3),ZNH(12)/ABSO1/NM
C     COMMON ENTRE LE PROGRAMME PRINCIPAL ET LE SP FLIN1
      COMMON/TOTO/TO    /FAIL/ERR(50)
      COMMON/PRT/ECRIT
      LLZERO=LZERO
      LLFIN=LFIN
      C=2.997929E+10
      H=6.6252E-27
      KB=1.38046E-16
      NU1= C* 1.E+8 /LZERO
      AHNU1= H*NU1
      C31=(2*AHNU1) * (NU1/C)**2
            DO   N=1,NTOT
            T=5040./TETA(N)
            ALPH(N)=EXP(-AHNU1/(KB*T))
            B1(N)= C31 * (ALPH(N)/(1.-ALPH(N)))
      CALL ABSORU(LLZERO,TETA(N),ALOG10(PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
            KC1(N)=TOTKAP(1)
            END DO
      NU2= C* 1.E+8 /LFIN
      AHNU2= H*NU2
      C32=(2*AHNU2) * (NU2/C)**2
            DO   N=1,NTOT
            T=5040./TETA(N)
            ALPH(N)=EXP(-AHNU2/(KB*T))
            B2(N)= C32 * (ALPH(N)/(1.-ALPH(N)))
      CALL ABSORU(LLFIN,TETA(N),ALOG10(PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
            KC2(N)=TOTKAP(1)
            END DO
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
c     if(n.eq.1) write(6,*) alph(n),phn(n),ph2(n),znh(nmeta+4),
c     1 znh(nmeta+2),t
c     if(n.eq.ntot) write(6,*) alph(n),phn(n),ph2(n),znh(nmeta+4),
c     1 znh(nmeta+2),t
            KC(N)=TOTKAP(1)
            END DO
      TET0=FTETA0(PG,TETA)     !on extrapole TETA pour NH=0
      T=5040./TET0
      ALPH01=EXP(-AHNU1/(KB*T))
      B1(0)=C31 * (ALPH01/(1.-ALPH01))
      CALL FLIN1(KC1,B1,NH,NTOT,PTDISK,MU,FC1,KIK,CAVA)
      IF(CAVA. GT.0) THEN
      WRITE(6,132) CAVA
      WRITE(6,135) (I,TO(I),ERR(I),I=1,NTOT)
      IF(CAVA.GT.1) STOP
      END IF
      ALPH02=EXP(-AHNU2/(KB*T))
      B2(0)=C32 * (ALPH02/(1.-ALPH02))
      CALL FLIN1(KC2,B2,NH,NTOT,PTDISK,MU,FC2,KIK,CAVA)
      IF(CAVA. GT.0) THEN
      WRITE(6,132) CAVA
      WRITE(6,135) (I,TO(I),ERR(I),I=1,NTOT)
      IF(CAVA.GT.1) STOP
      END IF
      ALPH0=EXP(-AHNU/(KB*T))
      B(0)=C3 * (ALPH0/(1.-ALPH0))
      CALL FLIN1(KC,B,NH,NTOT,PTDISK,MU,FC,KIK,CAVA)
      IF(CAVA. GT.0) THEN
      WRITE(6,132) CAVA
      WRITE(6,135) (I,TO(I),ERR(I),I=1,NTOT)
      IF(CAVA.GT.1) STOP
      END IF
      WRITE(6,151) KC1(1),KC1(NTOT),B1(0),B1(1),B1(NTOT),FC1
      WRITE(6,152) KC2(1),KC2(NTOT),B2(0),B2(1),B2(NTOT),FC2
      WRITE(6,150) KC(1),KC(NTOT),B(0),B(1),B(NTOT),FC
      ILZERO=LZERO/100.
      ILZERO=1E2*ILZERO
      LAMBDC(1)=LZERO-ILZERO
      LAMBDC(2)=LFIN-ILZERO
      DO N=1,NTOT
        KCJ(1,N)=KC1(N)
        KCJ(2,N)=KC2(N)
        END DO
      DO N=1,NTOT
            DO J=1,2
            KCN(J)=KCJ(J,N)
            END DO
      CALL FTLIN3(2,LAMBDC,KCN,DTOT,TTD,FTTC)
            DO D=1,DTOT
            KCD(D,N)=FTTC(D)
            END DO
      END DO
C     IF(.NOT.ECRIT) GO TO 10
      WRITE(6,153) KCD(1,1),KCD(1,NTOT)
      WRITE(6,154) KCD(DTOT,1),KCD(DTOT,NTOT)
10    CONTINUE
      RETURN
132   FORMAT(' ENNUI AU CALCUL DU FLUX CONTINU     CAVA='I3)
135   FORMAT(5(I4,F7.3,F5.2))
151   FORMAT(' KC1(1)=',E14.7,2X,'KC1(NTOT)=',E14.7,/' B1(0)=',E14.7,
     1 2X,'B1(1)=',E14.7,2X,'B1(NTOT)=',E14.7,/' FC1=',E14.7)
152   FORMAT(' KC2(1)=',E14.7,2X,'KC2(NTOT)=',E14.7,/' B2(0)=',E14.7,
     1 2X,'B2(1)=',E14.7,2X,'B2(NTOT)=',E14.7,/' FC2=',E14.7)
150   FORMAT(' KC(1)=',E14.7,2X,'KC(NTOT)=',E14.7,/' B(0)=',E14.7,
     1 2X,'B(1)=',E14.7,2X,'B(NTOT)=',E14.7,/' FC=',E14.7)
153   FORMAT(' KCD(1,1)=',E14.7,2X,'KCD(1,NTOT)=',E14.7)
154   FORMAT(' KCD(DTOT,1)=',E14.7,2X,'KCD(DTOT,NTOT)=',E14.7)
      END
C
      SUBROUTINE LECTAUH(NH,NTOT,PAS,JJMAX,LLAMBDH,TTH,
     1 DTOT,TTD,LZERO,LFIN,TAUH,DHM,DHP,FILETOH)
      PARAMETER(NP=7000)
      LOGICAL ECRIT
      INTEGER D,DTOT,DHM,DHP
      REAL NH
      REAL*8 LAMBDH,LLAMBDH,LZERO,LFIN
      CHARACTER*20 FILETOH,TITRE*80,TTT*11
      DIMENSION NH(50),TTH(100,50),TH(50,50)
      DIMENSION LAMBDH(50),LLAMBDH(100),ALLH(100)
      DIMENSION TTD(DTOT),FTTH(NP),TAUHN(100),TAUH(DTOT,50)
      COMMON/PRT/ECRIT
C
C     LECTURE DE LA PROFONDEUR OPTIQUE DANS LA RAIE D H
      OPEN(UNIT=16,FILE=FILETOH,STATUS='OLD')
      READ(16,1500) TITRE
      READ(16,1501) TTT
      READ(16,1551)JMAX
      READ(16,1550)(LAMBDH(J),J=1,JMAX)
      READ(16,1555) ((TH(J,N),J=1,JMAX),N=1,NTOT)
C     DO N=1,NTOT,5
C     WRITE(6,1555) (TH(J,N),J=1,JMAX)
C     END DO
C     WRITE(6,1555) ((TH(J,N),J=1,JMAX),N=1,NTOT)
C
      JJMAX=2*JMAX-1
      JMA1=JMAX-1
      DO JJ=1,JMAX
      DEL=LAMBDH(JMAX+1-JJ)-LAMBDH(1)
      LLAMBDH(JJ)=LAMBDH(JMAX+1-JJ)-2*DEL
      END DO
      DO JJ=JMAX+1,JJMAX
      LLAMBDH(JJ)=LAMBDH(JJ-JMA1)
      END DO
      DO N=1,NTOT
            DO JJ=1,JMAX
            TTH(JJ,N)=TH(JMAX+1-JJ,N)
            END DO
            DO JJ=JMAX+1,JJMAX
            TTH(JJ,N)=TH(JJ-JMA1,N)
            END DO
      END DO
      WRITE(6,1500) TITRE
      WRITE(6,1501) TTT
      WRITE(6,1552)JMAX
      WRITE(6,1553)(LLAMBDH(JJ),JJ=1,JJMAX)
      IF(.NOT.ECRIT) GO TO 15
      WRITE(6,1553)(LLAMBDH(JJ),JJ=1,JJMAX)
      DO N=1,NTOT,5
      WRITE(6,1556)N
      WRITE(6,1554)(TTH(JJ,N),JJ=1,JJMAX)
      END DO
15    CONTINUE
      ILZERO=LZERO/100.
      ILZERO=1E2*ILZERO
      ALZERO=LZERO-ILZERO
      ZLZERO=LFIN-ILZERO
      DO J=1,JJMAX
      ALLH(J)=LLAMBDH(J)-ILZERO
      END DO
      WRITE(6,155) ALLH(1),ALLH(JJMAX),ALZERO,ZLZERO
      WRITE(6,156) JJMAX,NTOT,DTOT
      DO N=1,NTOT
            DO J=1,JJMAX
            TAUHN(J)=TTH(J,N)
            END DO
      CALL FTLIN3H(JJMAX,ALLH,TAUHN,DTOT,TTD,FTTH,DHM,DHP)
            DO D=1,DTOT
            TAUH(D,N)=FTTH(D)
            END DO
      END DO
      REWIND 16
      WRITE(6,150)TAUH(1,1),TAUH(1,NTOT)
      WRITE(6,160)TAUH(dtot,1),TAUH(DTOT,NTOT)
150   FORMAT(' TAUH(1,1)=',E14.7,2X,'TAUH(1,NTOT)=',E14.7)
160   FORMAT(' TAUH(dtot,1)=',E14.7,2X,'TAUH(DTOT,NTOT)=',E14.7)
155   FORMAT(' ALLH(1)=',F8.3,2X,'ALLH(JJMAX)=',F8.3,2X,
     1 'ALZERO=',F7.3,2X,'ZLZERO=',F7.3)
156   FORMAT(' JJMAX=',I3,2X,'NTOT=',I3,2X,'DTOT=',I5)
1500  FORMAT(A80)
1501  FORMAT(A11)
1550  FORMAT(5F14.3)
1551  FORMAT(I4)
1555  FORMAT(5E12.4)
1552  FORMAT(' JMAX=',I3)
1556  FORMAT(' N=',I3)
1553  FORMAT(2X,5F14.3)
1554  FORMAT(2X,5E12.4)
1560  FORMAT(A40)
      RETURN
      END
C
      SUBROUTINE SELEKFH(PTDISK,MU,KIK,DTOT,PAS,NBLEND,
     1 GFAL,ZINF,ABOND,ECART,elem,LAMBDA,TAUH,DHM,DHP,VT,
     2 NTOT,NH,TETA,B,B1,B2,KCD,POP,DELTA,A,TTD,FL)
      PARAMETER(NR=8000,NMOL=50000,NP=7000)
      LOGICAL PTDISK,ECRIT
      INTEGER D, DTOT, CAVA,DHM,DHP
      REAL MU,KAPPA,KA,KAP,NH,KCD,KCI,KAM,KAPPAM,KAPPT,MM
      REAL*8 LAMBDA,PAS,ECART,ECAR,ECARTM,ECARM,LMBDAM
      DIMENSION NH(50),TETA(50),VT(50)
      DIMENSION B(0:50),TO(0:50),B1(0:50),B2(0:50),BI(0:50)
      DIMENSION ECART(NR),ECAR(NR), ZINF(NR),ECARTL(NR),
     1 GFAL(NR),ABOND(NR),KA(NR),KAP(50),elem(NR),
     2 KAPPA(50),LAMBDA(NR),KCD(DTOT,50),KCI(50),
     3 POP(NR,50),DELTA(NR,50),A(NR,50)
      DIMENSION TTD(DTOT),FL(DTOT),TAUHD(50),TAUH(DTOT,50)
      DIMENSION DELTAM(NMOL,50),ECARTM(NMOL),ECARM(NMOL),
     1 ECARTLM(NMOL),KAM(NMOL),KAPPAM(50),KAPPT(50)
      DIMENSION LMBDAM(NMOL),GFM(NMOL),PNVJ(NMOL,50),
     1 ALARGM(NMOL),dm(8)
      COMMON/TOTO/TO /FAIL/ERR(50)
        COMMON/KAPM1/MM,MBLEND
        COMMON/KAPM2/LMBDAM,GFM,PNVJ,ALARGM
      COMMON/KAPM4/ECARTM
      COMMON/PRT/ECRIT
      common/cno/dm
       DATA DEUXR/1.6634E+8/,RPI/1.77245385/,C/2.997929E+10/
C
      if(nblend.ne.0) then
            DO K=1,NBLEND
            ECAR(K)=ECART(K)
            END DO
      end if
      if(mblend.ne.0) then
            DO K=1,MBLEND
            ECARM(K)=ECARTM(K)
            END DO
      end if
      DO D=1,DTOT
      if(nblend.ne.0) then
            DO K=1,NBLEND
            ECAR(K)=ECAR(K)-PAS
            ECARTL(K)=ECAR(K)
            END DO
      end if
      if(mblend.ne.0) then
            DO K=1,MBLEND
            ECARM(K)=ECARM(K)-PAS
            ECARTLM(K)=ECARM(K)
            END DO
      end if
            DO N=1,NTOT
            KAPPA(N) =0.
            KAPPAM(N) =0.
            T=5040./TETA(N)
c     atomes
      if(nblend.eq.0) go to 260
       DO  K=1,NBLEND
              IF( ABS(ECARTL(K)) .GT. ZINF(K) )  then
              KA(K)=0.
                else

                V=ABS(ECAR(K)*1.E-8/DELTA(K,N))
              CALL HJENOR(A(K,N),V,DELTA(K,N),PHI)
            KA(K) = PHI * POP(K,N) * GFAL(K) * ABOND(K)

C           WRITE(22, *) ABOND(K)
C NOXIG:
              if(K.eq.1)KA(K) = PHI * POP(K,N) * GFAL(K)
c      dm(1): C,dm(2):C ,dm(3): N,dm(4):N ,dm(5): O,dm(6):O ,
c      dm(7):Mg,dm(8):Ti
c       if((elem(k).eq.dm(1)).or.(elem(k).eq.dm(2))) then
c        KA(K)=PHI*POP(K,N)*GFAL(K)
c     end if
c       if((elem(k).eq.dm(3)).or.(elem(k).eq.dm(4))) then
c        KA(K)=PHI*POP(K,N)*GFAL(K)
c     end if
c       if((elem(k).eq.dm(5)).or.(elem(k).eq.dm(6))) then
c        KA(K)=PHI*POP(K,N)*GFAL(K)
c     end if
c       if(elem(k).eq.dm(7)) then
c        KA(K)=PHI*POP(K,N)*GFAL(K)
c     end if
c       if(elem(k).eq.dm(8)) then
c        KA(K)=PHI*POP(K,N)*GFAL(K)
c     end if

c     if((d.eq.1).and.(n.eq.1)) then
c     write(6,*) d,k,lambda(k),ecar(k),v,phi
c     write(6,*) n,pop(k,n),a(k,n),delta(k,n)
c     end if
c     if((d.eq.1).and.(n.eq.ntot)) then
c     write(6,*) d,k,lambda(k),ecar(k),v,phi
c     write(6,*) n,pop(k,n),a(k,n),delta(k,n)
c     end if
            end if
            KAPPA(N) = KAPPA(N) + KA(K)
         END DO   !  fin bcle sur K
260   continue
C     molecule
      IF(MBLEND.EQ.0) GO TO 250
       DO  L=1,MBLEND
            IF( ABS(ECARTLM(L)) .GT. ALARGM(L) )  then
            KAM(L)=0.
            else
         DELTAM(L,N)=(1.E-8*LMBDAM(L))/C*SQRT(VT(N)**2+DEUXR*T/MM)
         VM=ABS(ECARM(L)*1.E-08/DELTAM(L,N))
         PHI=(EXP(-VM**2))/(RPI*DELTAM(L,N))
         KAM(L)=PHI*GFM(L)*PNVJ(L,N)
            end if
         KAPPAM(N)=KAPPAM(N)+KAM(L)
       END DO   !  fin bcle sur L

250   KAPPT(N)=KAPPA(N)+KAPPAM(N)
      KCI(N)=KCD(D,N)
      KAP(N)=KAPPT(N)+KCI(N)
      BI(N)=((B2(N)-B1(N))*(FLOAT(D-1)))/(FLOAT(DTOT-1)) + B1(N)
              END DO    ! fin bcle sur N
      BI(0)=((B2(0)-B1(0))*(FLOAT(D-1)))/(FLOAT(DTOT-1)) + B1(0)
      IF(D.EQ.1) WRITE(6,151) D,BI(0),BI(1),BI(NTOT)
      IF(D.EQ.1) WRITE(6,150) D, KCI(1),KCI(NTOT),KAPPA(1),KAPPA(NTOT)
      IF(D.EQ.1) WRITE(6,152) KAPPAM(1),KAPPAM(NTOT)
      IF(D.EQ.DTOT) WRITE(6,151) D,BI(0),BI(1),BI(NTOT)
      IF(D.EQ.DTOT)WRITE(6,150) D,KCI(1),KCI(NTOT),KAPPA(1),KAPPA(NTOT)
      IF(D.EQ.DTOT)WRITE(6,152)KAPPAM(1),KAPPAM(NTOT)
      IF((D.LT.DHM).OR.(D.GE.DHP)) THEN
      CALL FLIN1 (KAP,BI,NH,NTOT,PTDISK,MU,FL(D),KIK,CAVA)
                  IF(CAVA.GT.1)   THEN
                  WRITE(6,131) TTD(D),CAVA
                  STOP
                  END IF
      ELSE
            DO N=1,NTOT
            TAUHD(N)=TAUH(D,N)
            END DO
      CALL FLINH (KAP,BI,NH,NTOT,PTDISK,MU,TAUHD,FL(D),KIK,CAVA)
                  IF(CAVA.GT.1)   THEN
                  WRITE(6,131) TTD(D),CAVA
                  STOP
                  END IF
      END IF
      END DO  ! fin bcle sur D
131   FORMAT(' ENNUI AU CALCUL DU FLUX (CF LIGNE PRECEDENTE)',
     1   ' A LAMBD=',F10.3,'     CAVA=',I3)
150   FORMAT(' D=',I5,2X,'KCI(1)=',E14.7,2X,'KCI(NTOT)=',E14.7,
     1 /,10X,'KAPPA(1)=',E14.7,2X,'KAPPA(NTOT)=',E14.7)
152   FORMAT(10X,'KAPPAM(1)=',E14.7,2X,'KAPPAM(NTOT)=',E14.7)
151   FORMAT(' D=',I5,2X,'BI(0)=',E14.7,2X,'BI(1)=',E14.7,2X,
     1 'BI(NTOT)=',E14.7)
      RETURN
      END
C
      SUBROUTINE FLINH (KAP,B,NH,NTOT,PTDISK,MU,TAUHD,F,IOP,CAVA)
c     calcul du flux ou de l'intensite par la methode d'integration
c     a 6 pts (ou 13pts) de R.Cayrel (these).
c     nouvelle methode de calcul de to . TO(1)est calcule et
c     est different de 0 (On pose TO(0)=0)   -Avril 1988-
      LOGICAL PTDISK
      REAL   NH,KAP,MU
      INTEGER CAVA
      DIMENSION B(0:50),  TO(0:50)
      DIMENSION NH(50),KAP(50),BBB(26),TD2(26),
     1  TD(6),TP(7),CD(6),CP(7),C1(13),C2(12),C3(12)
      DIMENSION TAUHD(50)
      COMMON/TOTO/TO
      COMMON/FCO/FP(13),CC(13),TT(13),BB(13)
      COMMON/CCC/AMF(50),AMF2(50),FX1(50),FX2(50)
      COMMON/FAIL/ERR(50)
      DATA TD /0.038,0.154,0.335,0.793,1.467,3.890 /
      DATA CD/0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
      DATA TP/0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
      DATA CP/0.176273,0.153405,0.167016,0.135428,0.210244,0.107848,
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
            DO N=1,NTOT
            TO(N)=TO(N)+TAUHD(N)
            END DO
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
c8    WRITE(6,1500)
c     STOP
1500  FORMAT('   LE SP FLIN1 NE PEUT CALCULER L INTENSITE EN 1 PT ',
     1 'DU DISQUE AVEC LA FORMULE A 26PTS (UTILISER 7PTS IOP=0)' )
1501  FORMAT(1H //)
1502  FORMAT(5(F7.3,F5.2,2X))
1503  FORMAT(I10,5X,3HTO=,F10.4)
1504  FORMAT(18H MODELE TROP COURT)
      END
C
      SUBROUTINE CAFCONVH(GAUSS,PAS,AA,IFT,TT,FI)
C     ON CALCULE FI(TT) la fonction de convolution EN IFT PTS
C
C     LA FONCTION DE CONVOLUTION PEUT AVOIR 500 PTS
C      SI L ON CHANGE CE NBRE DE PTS CHANGER AUSSI IPPTOT=250
C      DANS LES DATA QUELQUES LIGNES PLUS BAS.
C      (LA MOITIE DU NBRE TOT DE PTS POUR LE CALCUL DES AT)
C
      LOGICAL GAUSS
      DIMENSION XFI(1501),YFI(1501),TT(1501),FI(1501),AT(-750:+750)
      IPPTOT=750
      C7=1.772453
C
            IF (GAUSS) THEN       ! PROFIL GAUSSIEN DE 1/2 LARG AA
      FWHM=1.6651092*AA
      WRITE(6,119) FWHM,AA
      TOTLARG=3.0 * AA
      AT(0)=0
            DO I=1,IPPTOT   ! IPPTOT TAILLE MAX DE LA FCT DE CONV
            AT(I)=PAS * I
            AT(-I)=-AT(I)
            IF(AT(I).GT.TOTLARG)   GO TO 40
            END DO
      BB=3*AA
      WRITE(6,133)BB
      STOP
40    IFD=I-1
      IFT= 1 + 2*IFD
            IF(IFT.GT.1501)  THEN
            WRITE(6,137)
            STOP
            END IF
            DO I=1,IFT
            TT(I)=AT(I-IFD-1)
            END DO
      Z = C7*AA
            DO  I=1,IFT
            FI(I) = EXP ( -(TT(I)/AA)**2)
            END DO
            WRITE(6,118) (TT(I),FI(I) ,I=1,IFT )
            WRITE(6,115)
C
                        ELSE ! ( NON GAUSSIEN)------------------
            READ(4,*) IFTOT
            IF(IFTOT.GT.1500)   THEN
            WRITE(6,138)
            STOP
            END IF
            DO N=1,IFTOT
            READ(4,*) XFI(N),YFI(N)
            END DO
            WRITE(6,118)   (XFI(N),YFI(N)  ,N=1,IFTOT)
            WRITE(6,115)
C
      I=1
      DO WHILE (TT(I).LE.XFI(IFTOT))
      TT(I)=XFI(1) + PAS*(I-1)
      I=I+1
            IF(I.GT.1500) THEN
            WRITE(6,138)
            WRITE(6,139)
            STOP
            END IF
      END DO
      IFT=I-1
      CALL FT2(IFTOT,XFI,YFI,IFT,TT,FI)
            WRITE(6,118) (TT(I),FI(I) ,I=1,IFT )
            WRITE(6,115)
C
      II = IFT/2
      III= 2*II
            IF(IFT.LE.III)   THEN
            IFT=IFT+1
            FI(IFT) = 0.
            END IF  ! FIN DU IF(IFT<III)
      Z=0.
      IF1 = IFT-1
            DO  I=2,IF1
            Z = Z +2.*FI(I)
            END DO
      Z= Z+FI(1) + FI(IFT)
      Z = Z*0.5*PAS
C
                  END IF ! FIN DU IF GAUSS
C   ------------------------------------------------------------------
      WRITE(6,109)   Z
            DO  I=1,IFT
            FI(I) = FI(I) / Z
            END DO
            WRITE(6,115)
            WRITE(6,118) (TT(I),FI(I) ,I=1,IFT )
            WRITE(6,115)
C
      RETURN
109   FORMAT(7E15.4)
115   FORMAT(1H )
118   FORMAT(5(2X,F7.3,F7.3) )
119   FORMAT(10X,'PROFIL INSTRUMENTAL GAUSSIEN  FWHM =',F7.3, 3X,
     1 '1/2 LARGEUR AA =',F7.3,'ANGSTROM')
133   FORMAT(' LE PROFIL GAUSSIEN PAR LEQUEL ON CONVOLE NE PEUT',
     1' AVOIR PLUS DE 121 PTS  (60 DE CHAQUE COTE DU CENTRE)',
     2/' CETTE GAUSSIENNE EST CALCULEE JUSQU A UNE DISTANCE DE',
     3' 3 DEMI-LARGEUR DU CENTRE',
     4/' SOIT:',F7.3,' ANGSTROM',
     4/'   ELARGISSEZ LE PAS DU CALCUL')
137   FORMAT(5X,'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER A ',
     1 /' PLUS DE 500 PTS -CHANGEZ LE PAS-. (LE NBRE DE PTS TOTAL '
     2 /' SUR LA FCTION S OBTIENT EN MULTIPLIANT PAR 6 LE NBRE '
     3 /' DE PTS SUR LA DEMI LARGEUR')
138   FORMAT( 5X, 'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER',
     1 ' A PLUS DE 500 PTS. C''EST TROP !')
139   FORMAT(5X,'-CHANGEZ LE PAS DU CALCUL !-')
      END
C
      SUBROUTINE VOLUTEH (S,ITOT,FI,J,PA,PS)
      DIMENSION PS(ITOT),S(ITOT),FI(1501)
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
C
      SUBROUTINE ABONDRAIH(ELE,ABO,NABOND,ELEM,ABOND,NBLEND)
      PARAMETER(NR=8000)
        CHARACTER*2 ELE,ELEM
      DIMENSION ELE(100),ABO(100),ELEM(NR),ABOND(NR)
      DO  K=1,NBLEND
            DO  J=1,NABOND
            IF(ELE(J).EQ.ELEM(K))  GO TO 14
            END DO   !FIN BCLE SUR J
      WRITE(6,106)     ELEM(K)
      STOP
14    ABOND(K)=ABO(J)
      END DO   !FIN BCLE SUR K
      RETURN
c
106   FORMAT('     MANQUE L ABONDANCE DU  ',A2                  )
      END
C
      SUBROUTINE FTLIN3H(N,X,Y,ITOT,TT,FTT,K1,K2)
      DIMENSION X(N),Y(N),TT(ITOT),FTT(ITOT)
C     WRITE(6,*) N
C     WRITE (6,105) (X(J),J=1,N)
C     WRITE (6,105) (Y(J),J=1,N)
C
      J=2
      KK=1
24    DO  4 K=KK,ITOT
      KQ=K
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
14    continue
      DO K=1,ITOT
      IF(FTT(K).NE.0.0) GO TO 20
      END DO
20    K1=K
        if(k1.eq.itot) k1=1
      KK1=K1+1
      DO K=KK1,ITOT
      IF(FTT(K).EQ.0.0) GO TO 30
      END DO
30    K2=K
C
      RETURN
10    FTT(K)=0.
      J=J+1
C     WRITE(6,*)K,FTT(K)
      KK=KQ
      KK=KK+1
      IF(KQ.GT.ITOT) GO TO 14
      GO TO 24
C     WRITE(6,101)
C     WRITE(6,102) (X(I),I=1,N)
100   FORMAT(' ON SORT DE LA TABLE D INTERPOLATION    T=',E15.7)
101   FORMAT(/'   LISTE DES X')
102   FORMAT(8E15.7)
103   FORMAT(5X,F10.3)
104   FORMAT(I5,7F9.3)
C105  FORMAT(7F10.3)
      END

C
      SUBROUTINE POPADELH (NPAR,EL,KI1,KI2,M,NBLEND,ELEM,
     1 LAMBDA,KIEX,CH,CORCH,CVdW,GR,GE,IONI,NTOT,TETA,PE,ALPH,
     2 PHN,PH2,VT,P,POP,A,DELTA)
C     ***calcule la population au niveau inferieur de la transition
C     ***la largeur doppler DELTA et le coefficient d'elargissement
C     ***le "A" utilise dans le calcul de H(A,V)
      PARAMETER(NR=8000)
        real KI1,KI2,KIEX,M,KB,KIES,KII,NUL
        real*8 LAMBDA(NBLEND)
      DIMENSION PE(50),TETA(50),VT(50),ALPH(50),PHN(50),PH2(50),
     1 EL(85),M(85),KI1(85),KI2(85),P(3,85,50),ALPHL(50),
     2 ELEM(NR),IONI(NR),KIEX(NR),
     3 CH(NR),CORCH(NR),CVdW(NR),GR(NR),
     4 GE(NR),POP(NR,50),A(NR,50),DELTA(NR,50)
      DIMENSION PPH(50),PPC2(50),PN(50),PC13(50),PMG(50),
     1 PO(50),PTI(50),PNG(50),PIG(50),pfe(50),dm(8)
      COMMON/KAPM3/PPH,PPC2,PN,PC13,PMG,PO,PTI,PNG,PIG,pfe
      COMMON/CNO/DM
        CHARACTER*2 TTI,CC,OO,NN,MGG,ELEM,EL
        CHARACTER*1 ISI,ISS

        data KB/1.38046E-16/, DEUXR/1.6634E+8/, C4/2.1179E+8/,
     1  C6/3.76727E+11/, PI/3.141593/, ISI/' '/, ISS/' '/,
     2  C/2.997929E+10/
        DATA TTI/'TI'/,CC/' C'/,OO/' O'/,NN/' N'/,MGG/'MG'/
      H=6.6252E-27
      C5= 2.*PI* (3.*PI**2/2.44)**0.4
C     C6=4 * Pi * C
c
        DO  K=1,NBLEND
            corch(k)=0.
            CVdW(K)=0
                DO  J=1,NPAR
                IF(EL(J).EQ.ELEM(K)) GO TO 15
                END DO
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
C               WRITE(6,125)  LAMBDA(K), CORCH(K)
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
        T=5040./TETA(N)

      NUL= C* 1.E+8 /LAMBDA(K)
      AHNUL= H*NUL
      ALPHL(N)=EXP(-AHNUL/(KB*T))

        TAP = 1.-ALPHL(N)
        TOP = 10.**(-KIEX(K)*TETA(N))
        POP(K,N) = P(IOO,J,N)*TOP*TAP
C NOXIG:
        IF(K.eq.1) POP(K,N)=TOP*TAP*P(IOO,J,N)*PO(N)/PPH(N)
c     C
c       if((elem(k).eq.dm(1)).or.(elem(k).eq.dm(2))) then
c       POP(K,N)=TOP*TAP*P(IOO,J,N)*PPC2(N)/PPH(N)
c        write(48,488)lambda(k),elem(k),dm(3),ioo,pop(k,n)
c        write(48,488)lambda(k),elem(k),dm(4),ioo,pop(k,n)
c        end if
c     N
c       if((elem(k).eq.dm(3)).or.(elem(k).eq.dm(4))) then
c     POP(K,N)=TOP*TAP*P(IOO,J,N)*PN(N)/PPH(N)
c     end if
c     O
c       if((elem(k).eq.dm(5)).or.(elem(k).eq.dm(6))) then
c        POP(K,N)=TOP*TAP*P(IOO,J,N)*PO(N)/PPH(N)
c        write(48,488)lambda(k),elem(k),dm(5),ioo,pop(k,n)
c        write(48,488)lambda(k),elem(k),dm(6),ioo,pop(k,n)
c        end if
c     Mg
c       if(elem(k).eq.dm(7)) then
c     POP(K,N)=TOP*TAP*P(IOO,J,N)*PMG(N)/PPH(N)
c     end if
c     Ti
c       if(elem(k).eq.dm(8)) then
c        POP(K,N)=TOP*TAP*P(IOO,J,N)*PTI(N)/PPH(N)
c       write(48,488)lambda(k),elem(k),dm(1),ioo,pop(k,n)
c        end if

        DELTA(K,N) =(1.E-8*LAMBDA(K))/C*SQRT(VT(N)**2+DEUXR*T/M(J))
        VREL    = SQRT(C4*T*(1.+1./M(J)))
            IF (IOPI.EQ.1)  then
            GH   =C5*CH(K)**0.4*VREL   **0.6
C                 if (N.EQ.10)  write (6,100) GH
            else
            GH = CH(K) + Corch(K)*T
C                 if (N.EQ.10) write(6, 101) GH
            END IF
        GAMMA = GR(K)+(GE(K)*PE (N)+GH*(PHN(N)+1.0146*PH2(N)))/(KB*T)
        A(K,N) =GAMMA*(1.E-8*LAMBDA(K))**2 / (C6*DELTA(K,N))
c     if((k.le.3).and.(n.eq.1)) then
c     write(6,*) lambda(k),gr(k),ge(k),gh,gamma
c     write(6,*) alphl(n),p(ioo,j,n),top,tap
c     write(6,*) vrel,m(j),ch(k),corch(k)
c     write(6,*) n,pe(n),phn(n),ph2(n),t
c     end if
c     if((k.le.3).and.(n.eq.ntot)) then
c     write(6,*) lambda(k),gr(k),ge(k),gh,gamma
c     write(6,*) alphl(n),p(ioo,j,n),top,tap
c     write(6,*) vrel,m(j),ch(k),corch(k)
c     write(6,*) n,pe(n),phn(n),ph2(n),t
c     end if
        END DO    !FIN BCLE SUR N
        END DO    !FIN BCLE SUR K
C
 100  FORMAT(' GamH AU 1Oeme Niv du modele:', E15.3)
 101  FORMAT(' GamH au 10eme Niv du modele:', E15.3,'  Spielfieldel')
 104  FORMAT('     MANQUE LES FCTS DE PARTITION DU ',A2)
 125  FORMAT(3X ,' POUR',F9.3,'   ON CALCULE CH ',
     1 'VAN DER WAALS ET ON MULTIPLIE PAR ',F7.1)
 488    format(2x,f10.3,2x,a2,2x,a2,2x,i3,1x,e13.3)
      return
      end

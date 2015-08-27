C       Fantomol avec sous-programmes (MNP) - READER06
C LE MODELES.yMOD
C       calcul possible de 100 A en 100 A
C       Flux sortant est en nu: Fnu x lambda
C       Flux absolu sortant a ete multiplie par 10**5
C       raies d'hydrogene prevues:alpha,beta,gamma,delta
      PARAMETER(NR=8000,NMOL=50000,NP=7000,NT=10000)
      INTEGER FINPAR,FINRAI,FINAB,D,DTOT
      INTEGER DHM,DHP
      CHARACTER FILEFLUX*20,FILETOH*20,FILETOHY*20
      CHARACTER tti*2,mgg*2,oo1*2,cc1*2,nn1*2
        CHARACTER*2 ELEM, ELEMS, ELE,EL
      character oo2*2,cc2*2,nn2*2
      REAL NH,M,KI1,KI2,KIEX,KB,KC,MU,LAMBD,
     1   NHE,KC1,KC2,KCD,MM
      LOGICAL PTDISK,CONVOL,GAUSS,ECRIT,IDENTH,calfc
      REAL*8 LZERO,LFIN,LAMBDA,LMBDAM,LLZERO,LLFIN,LLHY(4)
        REAL*8 LLAMBDH,PAS,ECART,ECARTM,L0,LF
      DIMENSION NH(50),PE(50),TETA(50),PG(50),T5L(50),
     1 KC(50),ALPH(50),PHN(50),PH2(50),
     2 KC1(50),KC2(50),KCD(NP,50),
     3 LAMBDA(NR),DELTA(NR,50),ABOND(NR),ELEM(NR),IONI(NR),
     4 KIEX(NR),ALGF(NR),GF(NR),CH(NR),GR(NR),GE(NR),
     5 POP(NR,50),A(NR,50),GFAL(NR),ECART(NR),ZINF(NR),
     6 CORCH(NR),CVdW(NR),ABONDR(NR),
     7 FI(1501),TFI(1501),
     8 ECARTM(NMOL)
        DIMENSION ELEMS(18),XXCOR(50),DM(8)
      DIMENSION TITRAV(20)
      DIMENSION VT(50),TOLV(20),VVT(20)
C     fonctions de partition
      DIMENSION EL(85),TINI(85),PA(85),JKMAX(85),TABU(85,3,63),
     1 M(85),KI1(85),KI2(85),P(3,85,50)
      dimension ELE(100),ABOL(100),ABO(100)
      DIMENSION B(0:50),TO(0:50),B1(0:50),B2(0:50)
      DIMENSION FL(NP), TTD(NP)
      DIMENSION LLAMBDH(100),TAUH(NP,50),TTH(100,50),IHH(500),
     1 FILETOHY(4)
      DIMENSION PPH(50),PPC2(50),PN(50),PC13(50),PMG(50),
     1 PO(50),PTI(50),PNG(50),PIG(50),pfe(50)
            DIMENSION LMBDAM(NMOL),GFM(NMOL),PNVJ(NMOL,50),
     1 ALARGM(NMOL)
C
C     COMMON AVEC LE SP READER ET LE SP DE TRACE
C     COMMON /COM6/TEFF,GLOG,ASALALF,NHE,TIABS(5),TIT(5)
      COMMON/COM6/TEFF,TETAEF,GLOG,ASASOL,NHE,TIT(5)
      common/alog/asalog
C     COMMON AVEC LE SP D ABSORPTION CONTINUE
      COMMON /LECT1/AMET,BHE/LECT2/ZP(30),ZM(30),BIDA(85)/ABSO2/NMETA
     1  /SAPE/BIDB(5),ZNU(30)/SAPU/BIDC(3),ZNH(12)/ABSO1/NM
C     COMMON ENTRE LE PROGRAMME PRINCIPAL ET LE SP FLIN1
      COMMON/TOTO/TO    /FAIL/ERR(50)
C       COMMON AVEC LA SUBROUTINE D EQUILIBRE DISSOCIATIF SAT4
        COMMON/COM8/NH,TETA,PE,PG,T5L
        COMMON/COR/ELEMS,XXCOR,NNMETAL
      common/fansat/fstar
C       COMMON'S AVEC LE SP KAPMOL
        COMMON/KAPM1/MM,MBLEND
        COMMON/KAPM2/LMBDAM,GFM,PNVJ,ALARGM
      COMMON/KAPM4/ECARTM
        COMMON/OPTIM/LZERO,LFIN
      COMMON/TOTAL/MMC,MBLENQ
C       COMMON'S AVEC LE SP KAPMOL et popadelh
      COMMON/KAPM3/PPH,PPC2,PN,PC13,PMG,PO,PTI,PNG,PIG,pfe
      COMMON/PRT/ECRIT
      common avec selekfh et popadelh
      common/cno/dm
C
      KIK=0 ! FORMULE A 6 OU 7 PTS POUR CALCUL FLUX OU INT
C  *****************************************************************
        data tti/'ti'/,mgg/'mg'/,cc1/' c'/,cc2/'c '/,
     1 oo1/' o'/,oo2/'o '/,nn1/'n '/,nn2/' n'/
      DATA LLHY/4101.748,4340.468,4861.332,6562.817/
      DATA C/2.997929E+10/, H/6.6252E-27/,KB/1.38046E-16/,R/8.3170E+7/,
     1 PI/3.141593/,C1/4.8298E+15/,C2/8.8525E-13/,C4/2.1179E+8/,
     2 C6/3.76727E+11/,DEUXR/1.6634E+8/,C7/1.772453/
C       C1=2.*(2*P&*KB*ME)**1.5/ H**3
C       C2=PI*E**2 / (ME*C**2)
C       C4=8.*R/PI
        C5= 2.*PI* (3.*PI**2/2.44)**0.4
C       C6=4.*PI*C
C       C7 = SQRT( PI )
      OPEN(UNIT=4,FILE='main.dat',STATUS='OLD')
      OPEN(UNIT=25,FILE='partit.dat',STATUS='OLD')
      OPEN(UNIT=23,FILE='dissoc.dat',STATUS='OLD')
      OPEN(UNIT=30,FILE='abonds.dat',STATUS='OLD')
      OPEN(UNIT=14,FILE='atom4070g.dat',STATUS='OLD')
        RPI=1.77245385

C                       I
C                 INITIALISATIONS DIVERSES
C         LECTURE ET CALCUL  DE LA FONCTION DE CONVOLUTION
C  *****************************************************************
      PRINT *,' ENTRER UN TITRE'
      READ(4,127) TITRAV
      WRITE(6,128)   TITRAV
C     CALFC:  Calcul flux continu FC a lambda LAMBD,milieu
C     de l intervalle et F/FC
        calfc=.False.
c  calfc ne sert pas, on le laisse pour ne pas trop changer
         ICLE=0
      CONVOL=.FALSE.
      READ(4,*)   ECRIT,PAS,ECHX,ECHY,FWHM
C
        NOXIG=1
C        AGGF=-10.25
C        AGGF=-9.716
C
C     AA EST LA 1/2 LARGEUR DU PROFIL GAUSSIEN POUR Y=1/E
      DPAS=PAS
      AA=FWHM/1.6651092
C
            IF (CONVOL)  THEN
            WRITE(6,106)  DPAS
            CALL CAFCONVH(GAUSS,DPAS,AA,IFT,TFI,FI)
      WRITE(6,*) IFT
            END IF  ! FIN DU IF CONVOL
C  ****************************************************************
C                       II
C           1-   LECTURE DES FCTS DE PARTITION
C           2-   DES DONNEES ABSORPTION CONTINUE
C           3-   DU MODELE
C  ****************************************************************
C ------------1)
      J=1
      DO WHILE (FINPAR.LT.1)
      READ(25,108)EL(J),TINI(J),PA(J),JKMAX(J),M(J),
     1           KI1(J),KI2(J),FINPAR
            IF(FINPAR.NE.1) THEN
            KMAX=JKMAX(J)
            READ(25,111) ((TABU(J,L,K),L=1,3),K=1,KMAX)
            J=J+1
            END IF
      END DO
      NPAR =J-1
      write(6,300) NPAR
C ------------2)
      CALL LECTUR(1)
      A0=AMET
C ------------3)
      READ(4,*) VVT(1)
      IVTOT=1
            IF(VVT(1).GT.900)  THEN   ! VT VARIABLE AVEC LA PROFONDEUR
            READ(4,*) IVTOT
            READ(4,*) (TOLV(I),I=1,IVTOT)
            READ(4,*) (VVT(I),I=1,IVTOT)
            END IF
      CALL READER06(NH,TETA,PE,PG,T5L,NTOT)
      BHE=NHE
        TETAEF=5040/TEFF
        ASASOL=10**ASALOG
        COEFALF=10**ASALALF
      AMET=A0*ASASOL
c     fstar=asasol
C
      INTERP=1  ! interp. lineaire de vt (si parabolique 2)
      CALL TURBUL(INTERP,IVTOT,TOLV,VVT,NTOT,T5L,VT)
            IF(IVTOT.EQ.1)   THEN
            WRITE(6,131) VVT(1)
            ELSE
            WRITE(6,132)
            DO I=1,IVTOT
            WRITE(6,133) TOLV(I),VVT(I)
            ENDDO
c           WRITE(6,133) ((TOLV(I),VVT(I)),I=1,IVTOT)
            END IF
C  *****************************************************************
C                       III
C     CALCUL DE QUANT  NE DEPENDANT QUE DU METAL ET DU MODELE
C           POPULATION DU NIV FOND DES IONS
C  *****************************************************************
      CALL POPUL(TETA,PE,NTOT,TINI,PA,JKMAX,KI1,KI2,NPAR,TABU,P)
C
C  *****************************************************************
C                       IV
C           CALCUL DES QUANTITES NE DEPENDANT QUE DU
C           MODELE ET DE LAMBDA : B(N)   KC(N)   FC
C  *****************************************************************
        READ(4,*) PTDISK ,MU
        READ(4,*) afstar  ! metallicity of the star (in log scale)
      fstar=10**afstar

      CALL SAT4(PPH,PPC2,PN,PC13,PMG,PO,PTI,PNG,PIG,PFE,NTOT)
C
            J=1
            DO WHILE (FINAB.LT.1)
            READ(30,105) FINAB,ELE(J),ABOL(J)
            J=J+1
            END DO
      NABOND=J-2
C
        DO K=1,NNMETAL
        DO J=1,NABOND
        IF(ELE(J).EQ.ELEMS(K)) ABOL(J)=ABOL(J)+XXCOR(K)
        END DO
      end do
c
      DO J=1,NABOND
            ABO(J)=10.**(ABOL(J)-12.)
      ABO(J)=ABO(J)*fstar
      END DO

C
      READ(4,1500) FILEFLUX
        OPEN(UNIT=17,FILE=FILEFLUX,STATUS='unknown')
      IK=1
C     AINT =intervalle de calcul
c     CAINT=intervalle de recouvremment des intervalles
c     HINT =demi-intervalle de calcul des raies d'hydrogene
      HINT=35.
      CINT=20.
      IDENTH=.FALSE.
      READ(4,*) LLZERO,LLFIN,AINT
      write(6,711) LLZERO,LLFIN,AINT
C     WRITE(17) (TIT(I),I=1,5),TETAEF,GLOG,ASALOG,NHE
C     write(17) LLZERO,LLFIN,PAS,ECHX,ECHY,FWHM

      xlzero=llzero-20.
      xlfin=xlzero+aint+20.
      if(xlfin.ge.(llfin+20.)) then
      ikeytot=1
      go to 330
      end if
      do i=2,100
      xlfin=xlfin+aint
      if(xlfin.ge.(llfin+20.)) go to 333
      end do
333   ikeytot=i
330   continue


      LZERO=LLZERO-20.
      LFIN=LZERO+AINT+20.
        IKEY=1
      IRH=0
      DO IH=1,4
      ALLHY=LLHY(IH)-LZERO
      WRITE(6,700) ALLHY
C     IF((ALLHY.GT.0.).AND.(ALLHY.LE.35.)) IRH=1
C     IF((ALLHY.LT.0.).AND.(ALLHY.GE.(-35.))) IRH=1
C     IF(ALLHY.LE.(AINT+55.)) IRH=1
C     AINT+55.=AINT+20.+35.   AINT-15.=AINT+20.-35.   70=2*35.
      IF(((ALLHY.GT.0).AND.(ALLHY.LE.(AINT+55.))).OR.
     1 ((ALLHY.LT.0.).AND.(ALLHY.GE.(-35.)))) THEN
      IRH=1
      IHH(IKEY)=IH
      IHT=IHH(IKEY)
      WRITE(6,701)IHH(IKEY)
      END IF
      END DO
      WRITE(6,702) IRH
        GO TO 667
C
666     LZERO=LZERO+AINT
        LFIN=LFIN+AINT
      IF(LFIN.GT.(LLFIN+20.)) LFIN=LLFIN+20.
      DO IH=1,4
      ALLHY=LLHY(IH)-LZERO
      WRITE(6,700) ALLHY
      IF(((ALLHY.GT.0).AND.(ALLHY.LE.(AINT+55.))).OR.
     1 ((ALLHY.LT.0.).AND.(ALLHY.GE.(-35.)))) THEN
      IRH=1
      IHH(IKEY)=IH
      IHT=IHH(IKEY)
      WRITE(6,701)IHH(IKEY)
      IF(IHH(IKEY).EQ.IHH(IKEY-1)) IDENTH=.TRUE.
      END IF
      END DO
667     CONTINUE
      WRITE(6,702) IRH
C
      DTOT=(LFIN-LZERO) / PAS  + 1.0005
      WRITE(6,117) LZERO,LFIN,DTOT
            IF(DTOT .GT. 40000) THEN
            WRITE(6,130)   PAS,DTOT
            STOP
            END IF
      LAMBD = (LZERO+LFIN)  / 2
      ILZERO = LZERO/100.
      ILZERO = 1E2* ILZERO
      ALZERO = LZERO -ILZERO
      ZLZERO = LFIN - ILZERO
C
      DO D=1,DTOT
      TTD(D)=ALZERO+PAS*(D-1)
      END DO
C
      CALL BK(NH,TETA,PE,PG,NTOT,LAMBD,B,B1,B2,ALPH,PHN,PH2,
     1 CALFC,FC,KC,KC1,KC2,KCD,TTD,DTOT,PTDISK,MU,KIK,LZERO,LFIN)

      WRITE(6,501) LLZERO,LLFIN,LZERO,LFIN,LAMBD
      IF(PTDISK)  WRITE(6,102) LAMBD,MU,FC
      IF(.NOT.PTDISK)  WRITE(6,101) LAMBD,FC
C  ******************************************************************
C       LECTURE TAU RAIE HYDROGENE ET INTERPOLATION DE TAUH
C
      IF(IRH.EQ.0) THEN
      DHM=0
      DHP=0
      GO TO 2225
      END IF
      IF(IDENTH) GO TO 11
C     TYPE *,' NOM DU FICHIER TAU RAIE D HYDROGENE'
      DO IH=1,4
      READ(4,1560) FILETOHY(IH)
      WRITE(6,1570) FILETOHY(IH)
      END DO
11    CONTINUE
      FILETOH=FILETOHY(IHT)
      WRITE(6,710) FILETOH,IHT
      CALL LECTAUH(NH,NTOT,DPAS,JJMAX,LLAMBDH,TTH,
     1 DTOT,TTD,LZERO,LFIN,TAUH,DHM,DHP,FILETOH)
2225  CONTINUE
      write(6,703) DHM,DHP
C
C  ******************************************************************
C                       V
C     QUANTITES DEPENDANT DE LA RAIE ET DU MODELE
C  *******************************************************************
      K=1
9     READ(14,103)ELEM(K),IONI(K),LAMBDA(K)
      READ(14,*) KIEX(K),ALGF(K),CH(K),GR(K),GE(K),ZINF(K),
     1 ABONDR(K),FINRAI
c        WRITE(6,*) KIEX(K),ALGF(K),CH(K),GR(K),GE(K),ZINF(K),
c     1 ABONDR(K),FINRAI
c        write(6,103)ELEM(K),IONI(K),LAMBDA(K)
      write(34,103)ELEM(K),IONI(K),LAMBDA(K)
      GF(K)=10.**ALGF(K)
        IF(K.EQ.1) GF(K)=10**AGGF
      IF(GR(K).LT.1E-37)   GR(K)=2.21E15 / LAMBDA(K)**2
      IF(FINRAI.EQ.1) GO TO 10
      IF(((LAMBDA(K).GT.LFIN).OR.(LAMBDA(K).LT.LZERO))) GO TO 205
      K=K+1
205   CONTINUE
      GO TO 9
10    NBLEND=K-1
      write(6,455) NBLEND
c        write(6,*) ELEM
c        STOP
      if(nblend.eq.0) go to 88
C
      read(cc1,1510)dm(1)
      read(cc2,1510)dm(2)
      read(nn1,1510)dm(3)
      read(nn2,1510)dm(4)
      read(oo1,1510)dm(5)
      read(oo2,1510)dm(6)
      read(mgg,1510)dm(7)
      read(tti,1510)dm(8)
c     write(6,1511) dm(1),dm(2),dm(3),dm(4),dm(5),ddm(6),dm(7),dm(8)

      CALL POPADELH (NPAR,EL,KI1,KI2,M,NBLEND,ELEM,
     1 LAMBDA,KIEX,CH,CORCH,CVdW,GR,GE,IONI,NTOT,TETA,PE,ALPH,
     2 PHN,PH2,VT,P,POP,A,DELTA)
C
      CALL ABONDRAIH(ELE,ABO,NABOND,ELEM,ABOND,NBLEND)
c
C  *************************************************************

C                       VI
C     CALCUL DU COEFFICIENT D ABSORPTION SELECTIF
C     ET CALCUL DU SPECTRE
C  ***************************************************************
C
45          DO K=1,NBLEND
            GFAL(K) = GF(K) * C2 * (LAMBDA(K)*1.E-8)**2
            ECART(K)=LAMBDA(K)-LZERO+PAS
C           ECART(K) = LAMBDA(K)-LZERO+PAS ! le 1er sera ECART-PAS
            END DO ! fin bcle sur K
88    continue
        CALL KAPMOL(NH,TETA,NTOT)
      WRITE (6,704) MBLEND
        IF(MBLEND.EQ.0) GO TO 65
        DO 60 L=1,MBLEND
 60     ECARTM(L)=LMBDAM(L)-LZERO + PAS
 65     continue

46    CALL SELEKFH(PTDISK,MU,KIK,DTOT,PAS,NBLEND,GFAL,ZINF,
     1 ABOND,ECART,elem,LAMBDA,TAUH,DHM,DHP,VT,NTOT,NH,TETA,B,
     2 B1,B2,KCD,POP,DELTA,A,TTD,FL)
c        DO D=1,DTOT
c     WRITE(6,160) TTD(D),FL(D)
c        END DO
c     STOP

      OPEN(UNIT=32, FILE='lines.pfant', STATUS='UNKNOWN')
      IF(ECRIT) THEN
      WRITE(32,122)
         if(nblend.ne.0) then
            DO K=1,NBLEND
            WRITE(32,121) ELEM(K),IONI(K),LAMBDA(K),KIEX(K),ALGF(K),
     1      alog10(ABOND(K))-afstar+12,CH(K),GR(K),GE(K),ZINF(K),
     2      CORCH(K)
c     WRITE(32,123)
            END DO
        end if
      END IF
        AMG=XXCOR(8)
        LI=10./DPAS
      I1=LI + 1
        I2=DTOT - LI
      IF(LFIN.GE.(LLFIN+20.)) THEN
      I2=(LLFIN+10.-LZERO)/DPAS + 1.0005
      END IF
      ITOT=I2-I1+1
        WRITE(6,705)I1,I2,IKEY
      WRITE(6,706) ILZERO,TTD(I1),TTD(I2)
c        aaa=((LLFIN+20.)-(LLZERO-20.))/AINT
c       IKEYTOT=((LLFIN+20.)-(LLZERO-20.))/AINT
c       if(aaa.gt.ikeytot) ikeytot=ikeytot+1
        DO D=I1,I2
        FL(D)=FL(D)*(10.**5)
         END DO
        L0=LLZERO-10.
        LF=LLFIN+10.
        WRITE(6,1130)IKEYtot,(TIT(I),I=1,5),TETAEF,GLOG,ASALOG,NHE,AMG,
     1 L0,LF,LZERO,LFIN,ITOT,DPAS,ECHX,ECHY,FWHM
      write(6,1132) (FL(D),D=I1,I2)
      write(17,1130)IKEYtot,(TIT(I),I=1,5),TETAEF,GLOG,ASALOG,NHE,AMG,
     1 L0,LF,LZERO,LFIN,ITOT,DPAS,ECHX,ECHY,FWHM
      write(17,1132) (FL(D),D=I1,I2)
1130  FORMAT(I5, 5A4, 5F15.5, 4F10.1, I10, 4F15.5)
1132  FORMAT(40000F15.5)
C
        WRITE(6,707)IKEY,LZERO,LFIN,I1,I2
        IKEY=IKEY+1
        IF(IKEY.GT.IKEYTOT)GO TO 669
      WRITE(6,708) IKEY,IRH
      IRH=0
      IDENTH=.FALSE.
      REWIND 14
        GO TO 666
669     CONTINUE

        write(6,800)
        write(6,801)


C
C  ****************************************************************
C                       XI
C           ZONE DE DEFINITION DES FORMATS
C  *******************************************************************
100   FORMAT (3F10.3)
101   FORMAT(2X,'FLUX CONTINU A ',F10.3,' ANGSTROM',E20.7/)
501   FORMAT(2X,2X,'LLZERO=',F10.3,2X,'LLFIN=',F10.3,/
     1 2X,'LZERO=',F10.3,2X,'LFIN=',F10.3,2X,'LAMBD 1/2=',F10.3)
102   FORMAT(2X,'INTENSITE CONTINUE A ',F10.3,' ANGSTROM  ET MU=',
     1  F4.1,3X,E20.7/)
103   FORMAT(A2,I1,1X,F10.3)
104   FORMAT('    INITIALE =',F6.3,' Angstrom')
105   FORMAT(I1,A2,F6.3)
106   FORMAT(' Pas du calcul=',F6.3)
107   FORMAT(' Decalage mesure a l''ecran =',f6.2,' A')
108   FORMAT(A2,2F5.2,I3,3F10.2,34X,I1)
110   FORMAT(1H1)
111   FORMAT(13F6.4)
160   FORMAT(4(4X,F8.2,E14.7))
114   FORMAT(4 (4X,F8.2,F8.3))
155   FORMAT(12F6.3)
115   FORMAT(1H )
116   FORMAT(10X,'CONVOLUTION PAR UN PROFIL INSTRUMENTAL')
117   FORMAT(5X,'LZERO=',F10.3,10X,'LFIN=',F10.3,5X,'DTOT=',I7)
121   FORMAT(1X,A2,I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1,
     1 F7.1)
122   FORMAT(6X,'LAMBDA',4X,'KIEX',5X,'L GF',3X,'L ABOND',6X,'CH',10X,
     1  'GR',10X,'GE',5X,'ZINF',4X,'CORCH')
123   FORMAT(10X,'    FLUX EN CHAQUE POINT' )
124   FORMAT(10X,'  PROFIL APRES CONVOLUTION')
127   FORMAT(20A4)
128   FORMAT(1H1,20X,20A4)
130   FORMAT('     PAS=',F8.3,5X,'NBRE TOT DE PTS A CALCULER=',I10,
     1 /'   ON N A PAS LE DROIT DE CALCULER PLUS DE  PTS')
131   FORMAT(/' V MICRO CONSTANTE  =',F6.1,'KM/S'/)
132   FORMAT(/'V MICRO VARIABLE AVEC PROFONDEUR')
133   FORMAT('   TO=',F7.3,10X,'V=',F7.3,'Km/s')
135   FORMAT(5(I4,F7.3,F5.2))
136   FORMAT(5X,A2,I1,F10.3,F10.2)
142   FORMAT(5X,A2,F8.2)
300   format(1x,'NPAR=', I4)
455   FORMAT(' NBLEND=',I6)
456   FORMAT('     MANQUE L ABONDANCE DU  ',A2                  )
700   FORMAT(1X,'Lambda H -LZERO',F10.4)
701   FORMAT(1X,'IHH(IKEY)=',I5)
702   FORMAT(1X,'IRH=',I5)
703   FORMAT(1X,'DHM=',I5,2X,'DHP=',I5)

704   FORMAT(1X,'MBLEND=',I10)
705   FORMAT(1X,'I1=',I10,2X,'I2=',I10,2X,'IKEY=',I5)
706   FORMAT(1X,'ILZERO=',I8,2X,'TTD(I1)=',F10.5,2X,'TTD(I2)=',F10.5)
707   FORMAT(1X,'IKEY=',I10,2X,'LZERO=',F10.3,2X,'LFIN=',F10.3,
     1 2X,'I1=',I7,2X,'I2=',I7)
708   FORMAT(1X,'IKEY=',I10,2X,'IRH=',I6)
710   FORMAT(1X,A,2X,'IH=',I5)
711   FORMAT(/,2X,'LLZERO=',F10.5,2X,'LLFIN=',F10.5,2X,'AINT=',F8.3,/)
800     format('   Flux sortant est en nu: Fnu x lambda')
801     format('   Flux absolu sortant a ete multiplie par 10**5')
1500  FORMAT(A20)
1501  FORMAT(2X,'LAMBD milieu intevalle=',F8.2,2X,'FC=',E15.7)
1502  FORMAT(2X,'LDNOR =',F8.2,2X,'FMOYEN=',E15.7)
1503  FORMAT(2X,'RLAMBD=',F8.2,2x,'RLAMBF=',F8.2,2x,'FLNOR=',E15.7)
1560    FORMAT(A)
1570    FORMAT(1X,A)
1510  format(a2)
1511  format(8(1x,a2))

C3333   FORMAT(F15.8)
5550  FORMAT(2F10.3,I5,F8.3,E20.7)
5551  FORMAT(1X,5A4,F8.3,4X,F7.2,4X,F7.2,4X,F5.2)
5555  FORMAT(2F10.3,2I5,F8.3,E20.7)
      STOP
      END
      SUBROUTINE READER06 (NH,TETA,PE,PG,T5L,NTOT)
C     CE S.P. LIT SUR DISQUE ACCES DIRECT NH,TETA,PE,PG,T5L,NTOT
      DIMENSION NH(50),TETA(50),PE(50),PG(50),T5L(50),BID(16)
      REAL NH,NHE, DETEF, DGLOG, DSALOG, ASALALF
      CHARACTER*4 BLC, TIT
C     COMMON /COM6/TEFF,GLOG,ASALALF,NHE,TIABS(5),TIT(5)
      COMMON /COM6/TEFF,TETAEF,GLOG,ASASOL,NHE,TIT(5)
      COMMON/ALOG/ASALOG
      DATA   BLC/'    '/
      DO 7 I=1,5
7     TIT(I)=BLC
      IF(IDEF.EQ.211939)   GO TO 10
      OPEN(UNIT=18,ACCESS='DIRECT',STATUS='OLD',
     1    FILE='modeles.mod', RECL = 1200)
      ID=1
      IDEF=211939
10    READ(4,*) TEFF,GLOG,ASALOG,NHE,INUM
        print *, TEFF,GLOG,ASALOG,NHE,INUM
        TETAEF=5040/TEFF
      IF(INUM.GT.0)   ID=INUM
      WRITE(6,102)TETAEF,GLOG,ASALOG,NHE,INUM
C   SI L ON DESIRE IMPOSER UN MODELE  ON MET EN INUM LE NUM DU MODELE
C   SUR LE FICHIER ACCES DIRECT
9     READ(18, REC=ID) NTOT,DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT,TITABS
      WRITE(6,105)DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT
        write(6,108) TIABS
      IF(NTOT.EQ.9999)   GO TO 6
      DDT  = ABS(TEFF-DETEF)
C     DDTA  = ABS(TETAEF-DETAEF)
      DDG = ABS(GLOG-DGLOG)
      DDAB = ABS(ASALOG-DSALOG)
      DDHE= ABS(NHE-DNHE)
C     DDIF = DDTA+DDG+DDAB+DDHE
5     IF(DDT.GT.1.0)   GO TO 9
      IF(DDG.GT.0.01)   GO TO 9
      IF(DDAB.GT.0.01)   GO TO 9
      ASASOL=10.**ASALOG
c     ID=ID-1
      READ(18, REC=ID)BID,(NH(I),TETA(I),PE(I),PG(I),T5L(I),I=1,NTOT)
      WRITE(6,110) NTOT
      DO 12 I=1,50
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
 108  FORMAT('MODELE CALCULE AVEC LA TABLE',5A4)
110   FORMAT(1X,'NTOT=',I3)
      END

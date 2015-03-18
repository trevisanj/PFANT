c ======================================================================
c
c     >> pfantgrade.f << NOV 2003
c
c     este codigo eh uma uniao do codigo pfant03.f e do
c     pfant01h.f feita pela Paula Coelho e Jorge Melendez. O
c     codigo pfant03.f calculava apenas 4 linhas de hidrogenio enquanto
c     que pfant01.h (MNoel) calculava 10 linhas.
c     Esta relatado a seguir as modificacoes feitas ao pfant03.f
c     para chegar ao presente codigo.
c
c     Passos realizados (nov/2003):
c     1) copiei todos os fontes que estavam em /home1/barbuy/pfant03
c
c     2) troquei nome do arquivo de atomos para 'atomgrade.dat' e o de
c     moleculas para 'moleculagrade.dat'
c
c     3) examinei os codigos pabsor.f pcalr98bht.f, pncalr98.f e
c     psatox95t.f e retirei as rotinas que eram obsoletas e nao eram
c     mais utilizadas (rotinas RETIRADAS => pcalr98bht.f: cafconvh,
c     voluteh / pncalr98.f: bkf, ediga, equiv, flin2, flin2b, inait,
c     inaitb, largq, popul2, quid, selekf, trangx, step, stepb,
c     volute, naitk3, gam, xxsol.
c
c     4) juntei os arquivos limpos conforme item anterior pabsor.f,
c     pcalr98bht.f, pfant03.f, pncalr98.f psatox95t.f em um
c     UNICO FONTE PFANTGRADE.F. Dessa forma, apenas o pkapgeralgrade.f
c     continua sendo um arquivo externo, devido a sua atualizacao ser
c     SEMPRE paralela com alteracoes no arquivo de moleculas.
c
c     Portanto, a nova forma de compilar o programa eh
c     f90 -o pfantgrade pfantgrade.f pkapgeralgrade.f
c
c     5) Aumentei o tamanho maximo do espectro total possivel de
c     ser calculado de 10000A para 20000A.
c
c     6) AQUI COMECAM AS ALTERACOES DEVIDO AO CALCULO DAS LINHAS DE H
c
c     a. linhas de codigo foram comentadas (identificadas com 'cp Nov03':
c     (comentario da Paula em Nov03).
c
c     b. dimensao e data de LLHY e dimensao de FILETOHY foram
c     atualizadas
c
c     c. incluidos TAUHI(NP,50),TAUHY(10,NP,50) e excluido IHH(500)
c
c     d. todo o codigo que se referia ao calculo das linha de H foram
c     ocultados, e o codigo a isto referente que estava em pfant01.h
c     foi acrescentado (correspondo ao codigo na secao
c     LECTURE TAU RAIE HYDROGENE ET INTERPOLATION DE TAUH ).
c
c     e. segundo as instrucoes enviadas pela Marie Noel em 2001:
c     - na rotina FTLIN3H foi incluida a linha
c     'if (ftt(itot).ne.0.0) k2=itot'
c     - DTOT foi substituido por NP nas dimensoes das matrizes
c           BK : TTD(NP), KCD(NP,5)
c        LECTAUH : TTD(NP)
c        SELEKFH : TTD(NP), KCD(NP, 50), FL(NP), TAUH(NP,50)
c
c
c     Tambem reduzi o numero de comentario que vao para a tela
c     'write(6...)' := cpc
c
c ========================================================================
c
c     Alteracao para calcular simultaneamente o continuo FCONT(NP) e o
c     espectro normalizado FN(NP) {Paula, dez 2003}
c
c     - acrescentei as variaveis FN, FCONT, FILEFLUX2, FILEFLUX3
c     - abro mais dois arquivos binarios unit=19 (continuo) e 20 (normalizado)
c     - rotina SELEKFH:
c           - recebe tbem FCONT e FN
c           - FCONT eh calculado passando p/ a rotina FLIN1 apenas o
c           coeficiente de absorcao do continuo
c     - FN = FL / FCONT
c     - escrevo nos devidos arquivos
c
c     Portanto, para diferenciar os arquivos binarios criados,
c     alem do arquivo normal criado como 'spe.' + nome no main.dat
c     o pfant cria mais dois arquivos que comecam com 'cont.' e 'norm.'

C       Fantomol avec sous-programmes (MNP) -
C       calcul possible de 100 A en 100 A
C       Flux sortant est en nu: Fnu x lambda
C       Flux absolu sortant a ete multiplie par 10**5

      PARAMETER(MAX_atomgrade_NBLEND=8000,PARAMETER_NMOL=50000,NP=7000,NT=10000)
      INTEGER FINPAR,FINRAI,FINAB,D,DTOT
cp Nov03    INTEGER DHM,DHP
      INTEGER DHM,DHP,DHMY,DHPY,DHMI,DHPI, NTOT
      CHARACTER FILETOH*20
      character FILEFLUX2*72,FILEFLUX3*72
      CHARACTER tti*2,mgg*2,oo1*2,cc1*2,nn1*2
      character oo2*2,cc2*2,nn2*2
      REAL NH,M,KI1,KI2,atomgrade_KIEX,KB,KC,main_MU,LAMBD,
     1   NHE,KC1,KC2,KCD,MM, ABOND, ABO
      LOGICAL CONVOL,GAUSS,IDENTH
      REAL*8 LZERO,LFIN,atomgrade_LAMBDA,LMBDAM, LLHY(10)
      REAL*8 LLAMBDH,main_PAS,ECART,ECARTM,L0,LF,lllhy
      DIMENSION NH(50),PE(50),TETA(50),PG(50),T5L(50),
     1 KC(50),ALPH(50),PHN(50),PH2(50),
     2 KC1(50),KC2(50),KCD(NP,50),
     3 atomgrade_LAMBDA(MAX_atomgrade_NBLEND),DELTA(MAX_atomgrade_NBLEND,50),ABOND(MAX_atomgrade_NBLEND),atomgrade_ELEM(MAX_atomgrade_NBLEND),atomgrade_IONI(MAX_atomgrade_NBLEND),
     4 atomgrade_KIEX(MAX_atomgrade_NBLEND),atomgrade_ALGF(MAX_atomgrade_NBLEND),atomgrade_GF(MAX_atomgrade_NBLEND),atomgrade_CH(MAX_atomgrade_NBLEND),atomgrade_GR(MAX_atomgrade_NBLEND),atomgrade_GE(MAX_atomgrade_NBLEND),
     5 POP(MAX_atomgrade_NBLEND,50),A(MAX_atomgrade_NBLEND,50),GFAL(MAX_atomgrade_NBLEND),ECART(MAX_atomgrade_NBLEND),atomgrade_ZINF(MAX_atomgrade_NBLEND),
     6 CORCH(MAX_atomgrade_NBLEND),CVdW(MAX_atomgrade_NBLEND),atomgrade_ABONDR(MAX_atomgrade_NBLEND),
     7 FI(1501),TFI(1501),
     8 ECARTM(PARAMETER_NMOL)
      CHARACTER*2 dissoc_ELEMS, atomgrade_ELEM, EL
        DIMENSION dissoc_ELEMS(18),DM(8)
      DIMENSION VT(50),TOLV(20)
C     fonctions de partition
      DIMENSION EL(85),TINI(85),PA(85),JKMAX(85),TABU(85,3,63),
     1 M(85),KI1(85),KI2(85),P(3,85,50)
      DIMENSION ABO(100)  ! ISSUE: Must match dimension of abonds_ELE. Change to maxNABOND later
      DIMENSION B(0:50),TO_TOTO(0:50),B1(0:50),B2(0:50)
      DIMENSION FL(NP), TTD(NP), FCONT(NP), FN(NP)
cp Nov03    DIMENSION LLAMBDH(100),TAUH(NP,50),TTH(100,50),IHH(500),
cp Nov03    1 FILETOHY(4)
      DIMENSION LLAMBDH(100),TAUH(NP,50),TTH(100,50),
     1 FILETOHY(10),TAUHI(NP,50),TAUHY(10,NP,50)
      DIMENSION DHMY(10),DHPY(10)
      DIMENSION PPH(50),PPC2(50),PN(50),PC13(50),PMG(50),
     1 PO(50),PTI(50),PNG(50),PIG(50),pfe(50)
            DIMENSION LMBDAM(PARAMETER_NMOL),GFM(PARAMETER_NMOL),PNVJ(PARAMETER_NMOL,50),
     1 ALARGM(PARAMETER_NMOL)
C




      KIK=0 ! FORMULE A 6 OU 7 PTS POUR CALCUL FLUX OU INT
C  *****************************************************************
        data tti/'ti'/,mgg/'mg'/,cc1/' c'/,cc2/'c '/,
     1 oo1/' o'/,oo2/'o '/,nn1/'n '/,nn2/' n'/
cp Nov03    DATA LLHY/4101.748,4340.468,4861.332,6562.817/
      DATA LLHY/3750.150,3770.630,3797.900,3835.390,3889.050,3970.076,
     1  4101.748,4340.468,4861.332,6562.817/
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
      OPEN(UNIT=14,FILE='atomgrade.dat',STATUS='OLD')
        RPI=1.77245385

C                       I
C                 INITIALISATIONS DIVERSES
C         LECTURE ET CALCUL  DE LA FONCTION DE CONVOLUTION
C  *****************************************************************
      PRINT *,' ENTRER UN TITRE'
      READ(4,127) main_TITRAV
      WRITE(6,128)   main_TITRAV
         ICLE=0
      CONVOL=.FALSE.
      READ(4,*)   main_ECRIT,main_PAS,main_ECHX,main_ECHY,main_FWHM
C
        NOXIG=1
C        AGGF=-9.72
C
C     AA EST LA 1/2 LARGEUR DU PROFIL GAUSSIEN POUR Y=1/E
      DPAS=main_PAS
      AA=main_FWHM/1.6651092
C

C  ****************************************************************
C                       II
C           1-   LECTURE DES FCTS DE PARTITION
C           2-   DES DONNEES ABSORPTION CONTINUE
C           3-   DU MODELE
C  ****************************************************************
C ------------1) LECTURE DES FCTS DE PARTITION
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

C ------------2) DES DONNEES ABSORPTION CONTINUE
      CALL LECTUR(1)
      A0=AMET

C ------------3) DU MODELE
      READ(4,*) main_VVT(1)
      IVTOT=1
            IF(main_VVT(1).GT.900)  THEN   ! VT VARIABLE AVEC LA PROFONDEUR
            READ(4,*) IVTOT
            READ(4,*) (TOLV(I),I=1,IVTOT)
            READ(4,*) (main_VVT(I),I=1,IVTOT)
            END IF
      CALL READER06(NH,TETA,PE,PG,T5L,NTOT)

      BHE=NHE
      AMET=A0*ASASOL
c     fstar=asasol
C
      INTERP=1  ! interp. lineaire de vt (si parabolique 2)
      CALL TURBUL(INTERP,IVTOT,TOLV,main_VVT,NTOT,T5L,VT)
            IF(IVTOT.EQ.1)   THEN
            WRITE(6,131) main_VVT(1)
            ELSE
            WRITE(6,132)
cpc         WRITE(6,133) ((TOLV(I),main_VVT(I)),I=1,IVTOT)
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
        READ(4,*) main_PTDISK ,main_MU
        READ(4,*) main_AFSTAR  ! metallicity of the star (in log scale)
      fstar=10**main_AFSTAR

      CALL SAT4(PPH,PPC2,PN,PC13,PMG,PO,PTI,PNG,PIG,PFE,NTOT)
C
            J=1
            DO WHILE (FINAB.LT.1)
            READ(30,105) FINAB,abonds_ELE(J),abonds_ABOL(J)
            J=J+1
            END DO
      abonds_NABOND=J-2
C
        DO K = 1, dissoc_NMETAL
        DO J=1,abonds_NABOND
C ISSUE: This is the thing that Beatriz mentioned that is not used anymore
        IF(abonds_ELE(J).EQ.dissoc_ELEMS(K)) abonds_ABOL(J)=abonds_ABOL(J)+main_XXCOR(K)
        END DO
      end do
c

      DO J=1,abonds_NABOND
            ABO(J)=10.**(abonds_ABOL(J)-12.)
      ABO(J)=ABO(J)*fstar
      END DO



      READ(4,1500) main_FILEFLUX
      FILEFLUX2 = 'cont.'//main_FILEFLUX
      FILEFLUX3 = 'norm.'//main_FILEFLUX

C ISSUE: this cannot happen: change value of this global here, check what is supposed to happen instead.
      main_FILEFLUX = 'spec.'//main_FILEFLUX


        OPEN(UNIT=17,FILE=main_FILEFLUX,STATUS='unknown')
      OPEN(UNIT=19,FILE=FILEFLUX2,STATUS='unknown')
        OPEN(UNIT=20,FILE=FILEFLUX3,STATUS='unknown')
      IK=1
      IK2=1
      IK3=1
C     AINT =intervalle de calcul
c     CAINT=intervalle de recouvremment des intervalles
c     HINT =demi-intervalle de calcul des raies d'hydrogene
      HINT=35.
      CINT=20.
      IDENTH=.FALSE.
      READ(4,*) LLZERO,LLFIN,AINT
      write(6,711) LLZERO,LLFIN,AINT

        DO IH=1,8
        READ(4,1560) FILETOHY(IH)
      print *,IH,filetohy(IH)
cpc        WRITE(6,1570) FILETOHY(IH)
        END DO

      xlzero=llzero-20.
      xlfin=xlzero+aint+20.
      if(xlfin.ge.(llfin+20.)) then
      ikeytot=1
      go to 330
      end if
cp Nov03    do i=2,100
      do i=2,250
      xlfin=xlfin+aint
      if(xlfin.ge.(llfin+20.)) go to 333
      end do
333   ikeytot=i
330   continue

      LZERO=LLZERO-20.
      LFIN=LZERO+AINT+20.
        IKEY=1
        GO TO 667
C
666     LZERO=LZERO+AINT
        LFIN=LFIN+AINT
      IF(LFIN.GT.(LLFIN+20.)) LFIN=LLFIN+20.
667     CONTINUE
C
      DTOT=(LFIN-LZERO) / main_PAS  + 1.0005
      WRITE(6,117) LZERO,LFIN,DTOT
            IF(DTOT .GT. 40000) THEN
            STOP
            END IF
      LAMBD = (LZERO+LFIN)  / 2
      ILZERO = LZERO/100.
      ILZERO = 1E2* ILZERO
      ALZERO = LZERO -ILZERO
      ZLZERO = LFIN - ILZERO
C
      DO D=1,DTOT
      TTD(D)=ALZERO+main_PAS*(D-1)
      END DO
C
      CALL BK(NH,TETA,PE,PG,NTOT,LAMBD,B,B1,B2,ALPH,PHN,PH2,
     1 FC,KC,KC1,KC2,KCD,TTD,DTOT,main_PTDISK,main_MU,KIK,LZERO,LFIN)

      WRITE(6,501) LLZERO,LLFIN,LZERO,LFIN,LAMBD

C  ******************************************************************
C       LECTURE TAU RAIE HYDROGENE ET INTERPOLATION DE TAUH
C

c   Type *,' nom des fichiers TAU raies Hydrogene'


      IM=0
      DO IH=1,10
      ALLHY=LLHY(IH)-LZERO
      lllhy=llhy(IH)
      IF(((ALLHY.GT.0).AND.(ALLHY.LE.(AINT+55.))).OR.
     1 ((ALLHY.LT.0.).AND.(ALLHY.GE.(-35.)))) THEN
      IM=IM+1
      IRH=1
      IHT=IH
      FILETOH=FILETOHY(IHT)
      WRITE(6,712) IM,LLHY(IH),FILETOH,IHT
      CALL LECTAUH(NH,NTOT,DPAS,JJMAX,LLAMBDH,TTH,
     1 DTOT,TTD,LZERO,LFIN,TAUHI,DHMI,DHPI,FILETOH)
      DHMY(IM)=DHMI
      DHPY(IM)=DHPI
        DO N=1,NTOT
           DO D=1,DTOT
           TAUHY(IM,D,N)=TAUHI(D,N)
           END DO
        END DO
      END IF
      END DO
      IMY=IM
      IF(IMY.NE.0) then
      write(6,*) (DHMY(IM),IM=1,IMY)
      write(6,*) (DHPY(IM),IM=1,IMY)
      DHP=MAXI(DHPY,IMY,1,IMY)
      DHM=MINI(DHMY,IMY,1,IMY)
      DO N=1,NTOT
         DO D=1,DTOT
         TAUH(D,N)=0.0
         END DO
      END DO
      DO N=1,NTOT
         DO D=1,DTOT
              DO IM=1,IMY
              TAUH(D,N)=TAUH(D,N)+TAUHY(IM,D,N)
              END DO
         END DO
      END DO
      ELSE
      IRH=0
      DHM=0
      DHP=0
      END IF


C
C  ******************************************************************
C                       V
C     QUANTITES DEPENDANT DE LA RAIE ET DU MODELE
C  *******************************************************************
      CALL FILTER_ATOMGRADE(LZERO, LFIN)


1036  format(A4, I10)
cpc   write(6,455) atomgrade_NBLEND
      if(atomgrade_NBLEND.eq.0) go to 88
C
      read(cc1,1510)dm(1)
      read(cc2,1510)dm(2)
      read(nn1,1510)dm(3)
      read(nn2,1510)dm(4)
      read(oo1,1510)dm(5)
      read(oo2,1510)dm(6)
      read(mgg,1510)dm(7)
      read(tti,1510)dm(8)


      CALL POPADELH (NPAR,EL,KI1,KI2,M,atomgrade_NBLEND,atomgrade_ELEM,
     1 atomgrade_LAMBDA,atomgrade_KIEX,atomgrade_CH,CORCH,CVdW,atomgrade_GR,atomgrade_GE,atomgrade_IONI,NTOT,TETA,PE,ALPH,
     2 PHN,PH2,VT,P,POP,A,DELTA)
C


      CALL ABONDRAIH(abonds_ELE,ABO,abonds_NABOND,atomgrade_ELEM,ABOND,atomgrade_NBLEND)

C  *************************************************************

C                       VI
C     CALCUL DU COEFFICIENT D ABSORPTION SELECTIF
C     ET CALCUL DU SPECTRE
C  ***************************************************************
C
45          DO K=1,atomgrade_NBLEND
            GFAL(K) = atomgrade_GF(K) * C2 * (atomgrade_LAMBDA(K)*1.E-8)**2
            ECART(K)= atomgrade_LAMBDA(K)-LZERO+main_PAS
            END DO ! fin bcle sur K
88    continue
        CALL KAPMOL(NH,TETA,NTOT)
      WRITE (6,704) MBLEND
        IF(MBLEND.EQ.0) GO TO 65
        DO 60 L=1,MBLEND
 60     ECARTM(L)=LMBDAM(L)-LZERO + main_PAS
 65     continue

46    CALL SELEKFH(main_PTDISK,main_MU,KIK,DTOT,main_PAS,atomgrade_NBLEND,GFAL,atomgrade_ZINF,
     1 ABOND,ECART,atomgrade_ELEM,atomgrade_LAMBDA,TAUH,DHM,DHP,VT,NTOT,NH,TETA,B,
     2 B1,B2,KCD,POP,DELTA,A,TTD,FL,FCONT)


      OPEN(UNIT=32, FILE='lines.pfant', STATUS='UNKNOWN')
      WRITE(32,122)
         if(atomgrade_NBLEND.ne.0) then
            DO K=1,atomgrade_NBLEND
            WRITE(32,125) atomgrade_ELEM(K),atomgrade_IONI(K),atomgrade_LAMBDA(K),atomgrade_KIEX(K),atomgrade_ALGF(K),
     1      alog10(ABOND(K))-main_AFSTAR+12,atomgrade_CH(K),atomgrade_GR(K),atomgrade_GE(K),atomgrade_ZINF(K),CORCH(K)
            WRITE(91,121) atomgrade_ELEM(K),atomgrade_IONI(K),atomgrade_LAMBDA(K),atomgrade_KIEX(K),atomgrade_ALGF(K),
     1      alog10(ABOND(K))-main_AFSTAR+12,atomgrade_CH(K),atomgrade_GR(K),atomgrade_GE(K),atomgrade_ZINF(K),CORCH(K)
            END DO
        end if


        AMG=main_XXCOR(8) ! I think this is just for debugging purposes
        LI=10./DPAS
      I1=LI + 1
        I2=DTOT - LI
      IF(LFIN.GE.(LLFIN+20.)) THEN
      I2=(LLFIN+10.-LZERO)/DPAS + 1.0005
      END IF
      ITOT=I2-I1+1

        DO D=I1,I2
              FL(D)= FL(D)*(10.**5)
              FCONT(D)=FCONT(D)*(10.**5)
              FN(D)= FL(D) / FCONT(D)
        END DO
        L0=LLZERO-10.
        LF=LLFIN+10.

      open(unit=31, file = 'log.log', status = 'unknown')
      print *, DTOT, ITOT, I1, I2
      write(31,1130)IKEYtot,(TIT(I),I=1,5),TETAEF,main_GLOG,main_ASALOG,NHE,AMG,
     1 L0,LF,LZERO,LFIN,ITOT,DPAS,main_ECHX,main_ECHY,main_FWHM
      do D = I1,I2
        write(31, *) L0 + (D-1) * DPAS, FL(D)
      end do

      write(17,1130)IKEYtot,(TIT(I),I=1,5),TETAEF,main_GLOG,main_ASALOG,NHE,AMG,
     1 L0,LF,LZERO,LFIN,ITOT,DPAS,main_ECHX,main_ECHY,main_FWHM
      write(17,1132) (FL(D),D=I1,I2)

      write(19,1130)IKEYtot,(TIT(I),I=1,5),TETAEF,main_GLOG,main_ASALOG,NHE,AMG,
     1 L0,LF,LZERO,LFIN,ITOT,DPAS,main_ECHX,main_ECHY,main_FWHM
      write(19,1132) (FCONT(D),D=I1,I2)

      write(20,1130)IKEYtot,(TIT(I),I=1,5),TETAEF,main_GLOG,main_ASALOG,NHE,AMG,
     1 L0,LF,LZERO,LFIN,ITOT,DPAS,main_ECHX,main_ECHY,main_FWHM
      write(20,1132) (FN(D),D=I1,I2)

1130  FORMAT(I5, 5A4, 5F15.5, 4F10.1, I10, 4F15.5)
1132  FORMAT(40000F15.5)

C
      WRITE(6,707) IKEY, LZERO, LFIN, I1, I2

      IKEY = IKEY+1

      IF (IKEY .GT. IKEYTOT) GO TO 669
      WRITE(6, 708) IKEY, IRH
      IDENTH = .FALSE.
      REWIND 14
      GO TO 666

669   CONTINUE
      close(17)

      write(6,800)
      write(6,801)



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
125   FORMAT(A2,1X, I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1,
     1 F7.1)
121   FORMAT(1X,A2,I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1,
     1 F7.1)
122   FORMAT(6X,'# LAMBDA',4X,'KIEX',5X,'L GF',3X,'L ABOND',6X,'CH',10X,
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
709   FORMAT(1x,'IMY=',I3)
710   FORMAT(1X,A,2X,'IH=',I5)
711   FORMAT(/,2X,'LLZERO=',F10.5,2X,'LLFIN=',F10.5,2X,'AINT=',F8.3,/)
712   FORMAT(1X,'IM=',I3,2X,'Lambda H=',F8.3,2X,A,2X,'IH=',I5)
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
C--- END MAIN ------------------------------------------------------------------
C--- END MAIN ------------------------------------------------------------------
C--- END MAIN ------------------------------------------------------------------
C--- END MAIN ------------------------------------------------------------------
C--- END MAIN ------------------------------------------------------------------















C-------------------------------------------------------------------------------
      SUBROUTINE LECTUR(CALMET)
      INTEGER*4 PMAX,PNAX,P,CAL,CALMET
      CHARACTER*2 VELEM
      DIMENSION ZEF4(5,11),GAU(5,11),ZN(5,11),atomgrade_ELEM(5),NMIN(5), VELEM(5)

      COMMON /LECT1/ ABMET, ABHEL
      COMMON /LECT2/ ZP(30),ZM(30), WI(41,2), NUMSET(2), CAL
      COMMON /SAHT/  ZK(11), ZKM(30,9), NR_SAHT(30)
      COMMON /SAHTP/ XI(30,9), PF(30,9)
      COMMON /ABSO2/ NMETA
      COMMON /UNI/   IUNITE(2)
      COMMON /ABSO1/ NM
      COMMON /NOM/   NOMET(30)
      COMMON /TIT/   TITRE(17)

      DATA VELEM/' C','MG','SI','AL','FE'/
C
C     CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
C     PUIS LES DONNEES CORRESPONDANTS AUX ABSORBANTS METALLIQUES SI NECE
C     CALMET=1 SI ON NE TIENT PAS COMPTE DES METAUX
C     CALMET=2 SI ON    TIENT     COMPTE DES METAUX
C
      OPEN(UNIT=15,STATUS='OLD',FILE='absoru2.dat')
      READ (15,40) ABMET,ABHEL
40    FORMAT (2E15.7)
C
C     ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
C     ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
C
      READ (15,39) NM,NMETA,(IUNITE(I),I=1,2),(TITRE(I),I=1,17)
39    FORMAT (2I2,19A4)
C
C     NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISA
C     NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
C     IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
C     IUNITE=' NOYAU H'  ''    ''    ''       ''      NOYAU D'HYDROGENE
C
      WRITE (6,80) (TITRE(I),I=1,17),ABMET,ABHEL
80    FORMAT ('0'17A4/'0   ABONDANCE DES METAUX=',F11.7,'   ABONDANCE D'
     1'HELIUM='F11.7/)
C
C     LECTURE DE LA TABLE D'IONISATION CHOISIE
C     ----------------------------------------
      DO 4092 J=1,NM
      READ (15,45) NR_SAHT(J),ZP(J),ZM(J)
45    FORMAT (3X,I3,2E16.5)
      ZP(J) = 10**(ZP(J))

C
C     NR_SAHT=DEGRE MAXIMUM D'IONISATION CONSIDERE
C     ZP=NBR. D'ABONDANCE DE L'ELEMENT
C     ZM=POIDS MOLECULAIRE DE L'ELEMENT
C
      NRR=NR_SAHT(J)
      DO 4092 I=1,NRR
      READ (15,46) NEANT,NOMET(J),NION,XI(J,I),PF(J,I)
46    FORMAT (A3,A2,I1,2E16.5)
C
C     ON LIT NR_SAHT CARTES CONTENANT CHACUNE LE POTENTIEL D'IONISATION ET LA
C     FONCTION DE PARTITION(LOG10(2UI+1)/UI)DE CHAQUE DEGRE D'IONISATION
C     CES VALEURS SONT LUES DANS L'ORDRE CROISSANT DU DEGRE D'IONISATION
C     NOMET  =NOM DE L'ELEMENT
C     NION   =SON ETAT D'IONISATION
C     XI(J,I)=POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATIO
C     PF(J,I)=FONCTION DE PARTITION         ''   ''     ''      ''   ''
C
      XI(J,I)=XI(J,I)*2.302585
4092  PF(J,I)=PF(J,I)*2.302585
999   READ (15,11) (NUMSET(ITH),ITH=1,2)
11    FORMAT (2I2)
C
C     NUMSET=NBR.DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
C     POUR H,HE ET HE+
C     PREMIERE LISTE POUR TH.LE.0.8  ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
C
      DO 4005 ITH=1,2
      NSET=NUMSET(ITH)
4005  READ (15,54) (WI(I,ITH),I=1,NSET)
54    FORMAT(8F10.1)
1000  CAL=1
      GO TO 1002
1002  CONTINUE
      RETURN
      END







C-------------------------------------------------------------------------------
      SUBROUTINE ABSORU (WL,TH,ZLPE,CALLAM,CALTH,CALPE,CALMET,CALU,CALSO
     1R,KKK,TOTKAP)
      INTEGER*4 CAL,CALU,CALMET,CALLAM,CALTH,CALPE,PMAX,CALSOR
      DIMENSION ZZKK(11,2),TOTKAP(2),DIF(2,3),SCATH(2),ZZK(11,2),SCAT(2)

      COMMON /LECT1/  ABMET, ABHEL
      COMMON /LECT2/  ZP(30), ZM(30), WI(41,2),NUMSET(2), CAL
      COMMON /GBF/    ZLH(19)
      COMMON /GBFH/   G2D(2,19), JSHYD(2), JH
      COMMON /TETA/   AHE, AH, AHEP, UH1, ZEMH, UHEP1, UHE1, ZEUHE1
      COMMON /TEHE/   ZLHEM(5), ZLHE(10)
      COMMON /TEMPE/  STWTM(5)
      COMMON /THE/    ZEUH(20), ZEUHEP(20), ZEXPM(5), ZEXP(10), UL
      COMMON /SAHT/   ZK(11), ZKM(30,9), NR_SAHT(30)
      COMMON /SAHTP/  XI(30,9), PF(30,9)
      COMMON /ABSO2/  NMETA
      COMMON /ABSO3/  JFZ
      COMMON /HYHE/   GRDM(46), V1(46), U2(46), WINV(46), YY(4),
     1                ZLETAG(18), G3D(12,18), AA(4), ZEFF4(10),
     2                RHOG(126), ZLHEP(19)
      COMMON /ZION/   AC, AC1(3), AC2(30,9)
      COMMON /ABME/   STIMU
      COMMON /SAPE/   AVM, ZNU1, ZNU2, ZNU3, ZMUZE, ZNU(30)
      COMMON /SAPDIV/ ZMU, PG
      COMMON /SAPU/   PE, RHO, TOC, ZNH(12)
      COMMON /UNI/    IUNITE(2)
      COMMON /ABSO1/  NM
      COMMON /NOM/    NOMET(30)
      COMMON /TIT/    TITRE(17)
      COMMON /SOMAB/  SUM1

      DATA DIF/5.799E-13,8.14E-13,1.422E-6,1.28E-6,2.784,1.61/
      ILT=CALLAM
      IF (CALPE.EQ.2) GO TO 9003
      SUM1=0.0
      SUM2=0.0
      DO 4100 I=1,NM
      SUM1=SUM1+ZP(I)
4100  SUM2=SUM2+ZP(I)*ZM(I)
      AVM=SUM2/SUM1

C
C     AVM=MASSE ATOMIQUE MOYENNE DES ELEMENTS PLUS LOURDS QUE L'HELIUM
C     SUM1=SOMME DES ABONDANCES DES METAUX
      DO 4110 I=1,NM
4110  ZNU(I)=ZP(I)/SUM1
C
C     ZNU1,ZNU2,ZNU3=SUCCESSIVEMENT FRACTION D'(H,HE,METAL) PAR NOMBRE T
C
      ZNU1=1.0/(1.0+ABMET+ABHEL)
      ZNU2=ZNU1*ABHEL
      ZNU3=ZNU1*ABMET
      ZMUZE=1.008*ZNU1+4.003*ZNU2+AVM*ZNU3
9005  IF ((CALTH.EQ.2).AND.(CALLAM.EQ.2)) GO TO 5016
      IF (CALTH.EQ.2) GO TO 9001
      IF (TH.LE.0.8) ITH=1
      IF (TH.GT.0.8) ITH=2
      NSET=NUMSET(ITH)-1
9001  DO 6500 I=1,NSET
      IF (ABS(WL-WI(I+1,ITH)).LE.0.50) GO TO 8000
      IF (WL.LT.WI(I+1,ITH)) GO TO 7000
6500  CONTINUE
7000  JFZ=1
      GO TO 9002
8000  JFZ=2
C
C     DIFFUSION DE RAYLEIGH PAR H ET H2 (DALGARNO) HARVARD JUIN 1964
C     SCATH(1)=DIFFUSION DE H
C     SCATH(2)=DIFFUSION DE H2
C
9002  DO 9023 I=1,2
      IF (I.EQ.2) GO TO 9020
      IF (WL.GT.1026.0) GO TO 9021
      SCATH(1)=4.0E-24
      GO TO 9023
9020  IF (WL.GT.1200.0) GO TO 9021
      WLH=1200.0
      GO TO 9022
9021  WLH=WL
9022  WL4=WLH**4
      SCATH(I)=(DIF(I,1)+DIF(I,2)/SQRT(WL4)+DIF(I,3)/WL4)/WL4
9023  CONTINUE
      GO TO 5018
5016  IF ((JFZ.NE.2).OR.(ILT.EQ.1)) GO TO 5017
      ILT=CALLAM-1
5018  CALL GAUNTH (WL)
5017  CALL  TEMPA(WL,TH,CALTH,CALLAM)
      IF (CALTH.EQ.2) GO TO 9007
      CALL SAHATH (TH)
9007  IF ((CALTH.EQ.2).AND.(CALLAM.EQ.2)) GO TO 9006
      CALL ATHYHE (WL,TH,CALTH,CALLAM,ZZK)
9006  IF (CALMET.EQ.1) GO TO 9003
9003  CALL IONIPE (TH,ZLPE,CALTH,CALMET)
      MM=NMETA+1
      MMM=NMETA+6
      SCATEL=9.559063E-13*PE*TH
C
C     9.559063E-13=4.81815E-9/5040.39 ET 4.81815E-9=6.625E-25/1.38024E-1
C     =ELECTRON SCATTERING/(K*T)  UNSOLD P. 180 1955
C
      GO TO (9008,9009),CALSOR
9009  WRITE (6,86) (IUNITE(I),I=1,2)
86    FORMAT ('0LAMBDA KKK   C1'7X'MG'7X'SI1'7X'AL1'8X'H-'7X'H2-'7X'H2+'
     19X'H'7X'HE+'8X'HE'5X'K TOTAL/',2A4)
9008  KKK=JFZ
      MIN=MM
      DO 9015 I=1,JFZ
      DO 9015 M=1,NMETA
9015  ZZKK(M,I)=0.0
1750  TOTKAP(2)=0.
      IF (CALU.EQ.1) UNIT=RHO
      IF (CALU.EQ.2) UNIT=TOC
C     RAPPEL  TOC=NBRE DE NOYAUX DE H PAR CM3
      SCATEL=SCATEL/UNIT
      DO 4220 I=1,KKK
      TOTKAP(I)=0.0
      SCAT(1)=SCATH(1)*ZNH(NMETA+4)/UNIT
      SCAT(2)=SCATH(2)*ZNH(NMETA+2)/UNIT
      DO 4221 M=MIN,MMM
C     LES ZNH POUR LES METAUX SONT EN CM-3*1.0E-18
      IF ((M.NE.(NMETA+1)).AND.(M.NE.(NMETA+3))) GO TO 4222
      IF (M.EQ.(NMETA+1)) ZZKK(M,I)=ZZK(M,I)*(ZNH(NMETA+4)*PE*1.E-26)/UNIT
      IF (M.EQ.(NMETA+3)) ZZKK(M,I)=ZZK(M,I)*((ZNH(NMETA+4)*1.E-19)*(ZNH(M
     1MAX+7)*1.0E-20))/UNIT
      GO TO 4221
4222  ZZKK(M,I)=ZZK(M,I)*ZNH(M)/UNIT
      IF (M.EQ.(NMETA+2)) ZZKK(M,I)=ZZKK(M,I)*PE
4221  TOTKAP(I)=TOTKAP(I)+ZZKK(M,I)
      TOTKAP(I)=(TOTKAP(I)+SCATEL+SCAT(1)+SCAT(2))
      GO TO (4220,9010),CALSOR
9010  WRITE (6,87) WL,KKK,(ZZKK(M,I),M=1,MMM),TOTKAP(I)
87    FORMAT (F9.1,I2,1P12E10.2)
4220  CONTINUE
      GO TO (9011,9012),CALSOR
9012  WRITE (6,89) SCATEL,SCAT(1),SCAT(2),RHO,TOC,ZLPE,TH
89    FORMAT ('0SIG(E)='1PE11.4,' SIG(H)='E11.4,' SIG(H2)='E11.4,' DENSI
     1TE='E11.4,' NBR.NOYAU D H/CM3='E11.4,' LOG10PE='0PF5.2,' TETA='F5.
     22)
9011  CAL=2
      RETURN
      END





C-------------------------------------------------------------------------------
      SUBROUTINE IONIPE(TH,ZLPE,CALTH,CALMET)
C
C     A.M COLLE  18/01/1971
C
      INTEGER*4 CALTH,CALMET
      REAL KTH
      DIMENSION PHI(30),PA(10)
      COMMON /SAPE/ AVM, ZNU1, ZNU2, ZNU3, ZMUZE, ZNU(30)
      COMMON /ZION/ AC, AC1(3), AC2(30,9)
      COMMON /SAHT/ ZK(11), ZKM(30,9), NR_SAHT(30)
      COMMON /SAPU/ PE, RHO, TOC, ZNH(12)
      COMMON /SAPDIV/ ZMU, PG
      COMMON /ABSO1/ NM
      COMMON /ABSO2/ NMETA
C
C     SSP CALCULANT LES QUANTITES SUIVANTES
C     PARTH =NBRE TOTAL DE NOYAUX PAR CM3
C     PG    =PRESSION TOTALE EN DYNES/CM2
C     ZMU   =POIDS MOLECULAIRE MOYEN
C     RHO   =DENSITE (G-CM-3)
C     TOC   =NOMBRE DE NOYAUX D'HYDROGENE PAR CM3
C     AC    =DEGRE D'IONISATION MOYEN
C     AC1(1)=  ''        ''     DE H
C     AC1(2)=  ''        ''     DE HE+
C     AC1(3)=  ''        ''     DE HE
C     AC2   =  ''        ''     DES METAUX
C     PHI(J)=  ''        ''     DE L ELEMENT J POUR MULTIPLE IONISATION
C     ZNH(M)=POPULATION POUR CHAQUE ABSORBANT M (H,HE OU METAUX)
C     VOIR ARTICLE DE 'VARDYA' APJ VOL.133,P.107,1961
C
      KTH=6.956948E-13/TH
C     6.956948E-13=1.38024E-16*5040.39
      PE=EXP(ZLPE*2.302585)
      SIGM3=0.0
      DO 2380 J=1,NM
      NRR=NR_SAHT(J)
      SIGM1=0.0
      SIGM2=0.0
      PA(1)=1.0
      DO 2370 I=1,NRR
      IF ((PA(I).LE.0.0).OR.(ZKM(J,I).LE.0.0)) GO TO 2375
      PA(I+1)=PA(I)*(ZKM(J,I)/PE)
      SIGM1=I*PA(I+1)+SIGM1
2370  SIGM2=SIGM2+PA(I+1)
2375  DEN=1.0+SIGM2
      PHI(J)=SIGM1/DEN
      DO 1 I=1,NRR
1     AC2(J,I)=PA(I)/DEN
2380  SIGM3=SIGM3+ZNU(J)*PHI(J)
      IF (CALTH.EQ.2) GO TO 2390
      IF (TH.GE.0.25) GO TO 2382
      TEMPOR=0.0
      ANY=0.0
      COND=0.0
      GO TO 2390
2382  ANY=1.0/ZK(NMETA+3)
      TEMPOR=1.0/ZK(NMETA+2)
      COND=1.0/ZK(NMETA+1)
2390  W2=ZK(NMETA+4)/PE
      W1=W2*ANY
      W3=PE*COND
      W4=ZNU2*ZK(NMETA+6)*(PE+2*ZK(NMETA+5))/((PE+ZK(NMETA+6))*PE+ZK(NMETA+6
     1)*ZK(NMETA+5))+(ZNU3*SIGM3)
      FUN1=ZNU1*W1+2*(TEMPOR+W1)*W4
      FUN2=ZNU1*(W2-W3)+(1.0+W2+W3)*W4
      PH=2*ZNU1*PE/(FUN2+SQRT(FUN2**2+4*FUN1*ZNU1*PE))
      ZNH(NMETA+4)=PH/KTH
      ZNH(NMETA+2)=PH*TEMPOR*ZNH(NMETA+4)
      ZNH(NMETA+1)=ZNH(NMETA+4)*W3
      ZNH(NMETA+7)=ZNH(NMETA+4)*W2
      ZNH(NMETA+3)=ZNH(NMETA+7)*PH*ANY
      TP1=ZNH(NMETA+1)+ZNH(NMETA+4)+ZNH(NMETA+7)
      TP2=ZNH(NMETA+2)+ZNH(NMETA+3)
      TOC=2*TP2+TP1
      PARTH=TOC/ZNU1
      PPAR=(TP1+TP2+PARTH*(ZNU2+ZNU3))*KTH
      PG=PPAR+PE
      AC=PE/PPAR
      W5=ZK(NMETA+6)/PE
      W6=ZK(NMETA+5)*W5/PE
      S=1.0+W5+W6
      AC1(1)=W2/(1.0+W2)
      AC1(2)=W6/S
      AC1(3)=W5/S
      ZNH(NMETA+6)=ZNU2*PARTH/(1.0+W5+W6)
      ZNH(NMETA+5)=ZNH(NMETA+6)*W5
      RHO=1.6602E-24*PARTH*ZMUZE
C     1.6602E-24=MASSE DE L'UNITE DE POIDS
      ZMU=RHO*41904.28E+7/(TH*PG)
C     41904.275E+7=8.313697E+7*5040.39,OU 8.313697E+7=CONSTANTE DES GAZ
11    RETURN
      END

C-------------------------------------------------------------------------------
      SUBROUTINE  TEMPA(WL,TH,CALTH,CALLAM)
C
C     A.M COLLE   8/5/69
C
      INTEGER*4 CALLAM,CALTH
      COMMON /TETA/ AHE, AH, AHEP, UH1, ZEMH, UHEP1, UHE1, ZEUHE1
      COMMON /TEHE/ ZLHEM(5), ZLHE(10)
      COMMON /TEMPE/ STWTM(5)
      COMMON /THE/ ZEUH(20), ZEUHEP(20), ZEXPM(5), ZEXP(10), UL
C
C     HCBKTM=(H*C/K*T)*1.0E8
C     0.0010967876=CONSTANTE DE RYDBERG POUR H  *1.0E-8  ALLEN 1963
C     0.0043890867=CONSTANTE DE RYDBERG POUR HE+*1.0E-8  MOORE 1950 (HE4
C     AHE =POUR HE 4*C/T**3
C     AH  =POUR H   C*Z**4/T**3  AVEC Z=1
C     AHEP=POUR HE+ C*Z**4/T**3  AVEC Z=2
C     C=64*PI**4*ME*E**10/(3*RAC(3)*C*H**3*K**3)
C     ME=9.10E-28,E**10=4.8E-10,K=1.38024E-16,H=6.6237E-27,C=2.99791E+10
C
      IF (CALTH.EQ.2) GO TO 1001
      HCBKTM=0.2854306E-3*TH*1.0E8
      AHE=0.9717088E-12*TH**3
      AH=0.2429272E-12*TH**3
      AHEP=16.*AH
      UH1=1.096788E-3*HCBKTM
      ZEMH=EXP(-UH1)
      IF (TH.GT.1.4) GO TO 1001
      DO 1304 J=1,20
      UH=UH1/J**2
1304  ZEUH(J)=EXP(UH-UH1)/J**3
      ZEUH(20)=ZEUH(20)*8000.
      UHEP1=4.389087E-3*HCBKTM
      IF (TH.GT.0.3) GO TO 5290
      DO 1313 J=1,20
      UHEP=UHEP1/J**2
1313  ZEUHEP(J)=EXP(UHEP-UHEP1)/J**3
      ZEUHEP(20)=ZEUHEP(20)*8000.
5290  UHE1=HCBKTM/504.3
      ZEUHE1=EXP(-UHE1)
      IF (TH.GT.0.8) GO TO 1001
      COMHE=-HCBKTM*(1.0/ZLHEM(1))
      DO 5310 K=1,5
5310  ZEXPM(K)=EXP(COMHE+HCBKTM*(1.0/ZLHEM(K)))*STWTM(K)
      DO 5340 L=3,10
5340  ZEXP(L)= EXP(COMHE+HCBKTM*(1.0/ZLHE(L)))/L**3
1001  IF ((CALLAM.EQ.2).AND.(CALTH.EQ.2)) GO TO 5010
      UL=HCBKTM/WL
5010  RETURN
      END

C-------------------------------------------------------------------------------
      SUBROUTINE SAHATH(TH)
C
C     A.M COLLE   13/5/69
C
      DIMENSION POTION(6),C1(3),C2(3),C3(3),C4(3)

      COMMON /ABSO2/ NMETA
      COMMON /SAHT/ ZK(11), ZKM(30,9), NR_SAHT(30)
      COMMON /SAHTP/ XI(30,9), PF(30,9)
      COMMON /ABSO1/ NM

      DATA C1 /0.0,-7.526612E-3,5.708280E-2/,
     1     C2 /0.0,1.293852E-1,-1.823574E-1/,
     2     C3 /0.0,-11.34061,-6.434060/,
     3     C4 /0.0,28.85946,25.80507/,
     4 POTION /2-1.720031, 0.0, 0.0, 31.30364, 125.2675, -56.59754/

      DO 1 N=2,3
1     ZK(NMETA+N)=EXP(((C1(N)*TH+C2(N))*TH+C3(N))*TH+C4(N))
C
C     LOI DE SAHA=LOG((NR_SAHT+1)/NR_SAHT)*PE= -POT.ION.*TH+5/2*LOG(T)-0.4772+FONC
C     LES FONCTIONS DE PARTITION (L0G(2UR+1)/UR) SONT INCLUSES DANS LES
C     CONSTANTES AJOUTEES A TEMPOR POUR H ET HE LES AUTRES SONT LUES
C     31.303644,1.7200311,56.597541,125.26753,SONT RESPECTIVEMENT LES
C     POTENTIELS D'IONISATION DE (H,H-,HE,HE+)*2.3025851
C
      TEMPOR=2.5*ALOG(5040.39/TH)
      TEMPO=TEMPOR-1.098794
      DO 2 N=4,5
      ZK(NMETA+N)=POTION(N)*TH-TEMPO
      IF (N.EQ.4) GO TO 12
      IF  (ZK(NMETA+5).LT.100.0) GO TO 12
      ZK(NMETA+5)=0.0
      GO TO 2
12    ZK(NMETA+N)=EXP(-ZK(NMETA+N))
2     CONTINUE
      DO 2270 J=1,NM
      NRR=NR_SAHT(J)
      DO 2270 I=1,NRR
      ZKM(J,I)=TH*XI(J,I)-PF(J,I)-TEMPO
      IF (ZKM(J,I).LT.100.0) GO TO 2269
      ZKM(J,I)=0.0
      GO TO 2270
2269  ZKM(J,I)=EXP(-ZKM(J,I))
2270  CONTINUE
      TEMPO=TEMPOR+0.2875929
      DO 3 N=1,6,5
3     ZK(NMETA+N)=EXP( POTION(N)*TH+TEMPO)
      RETURN
      END

      BLOCK DATA
C     BLOCK DATA POUR LE SOUS PROGRAMME  ABSORU POUR LES RAIES D'HYDROGE
C
C     A.M COLLE  12/08/70
      DIMENSION G3D1(12,9),G3D2(12,9)

C ISSUE: Block /D1/ is never used!!!!!
      COMMON /D1/ TET(7), ALP(9), TAB(9,7), MMAX_D1, NMAX
      COMMON /GBF/ ZLH(19)
      COMMON /HYHE/ GRDM(46), U1(46), U2(46), WINV(46), YY(4),
     1              ZLETAG(18), G3D(12,18), AA(4), ZEFF4(10),
     2              RHOG(12), ZLHEP(19)
      COMMON /TEHE/ ZLHEM(5), ZLHE(10)
      COMMON /TEMPE/ STWTM(5)

      EQUIVALENCE (G3D(1,1),G3D1(1,1)),(G3D(1,10),G3D2(1,1))

      DATA ALP/5.,4.,3.,2.,1.,0.,-1.,-2.,-3./,MMAX_D1,NMAX/9,7/,TET/0.1,0.2
     1,0.3,0.4,0.5,0.6,0.7/
      DATA TAB/1.43,1.83,2.28,2.77,3.27,3.77,4.27,4.77,5.27,
     1         0.47,0.62,0.91,1.31,1.78,2.26,2.75,3.25,3.76,
     2         0.31,0.32,0.35,0.42,0.61,0.93,1.35,1.82,2.32,
     3         0.30,0.30,0.30,0.31,0.32,0.35,0.44,0.65,0.99,
     4         0.30,0.30,0.30,0.30,0.30,0.30,0.31,0.32,0.36,
     5         18*0.30/
      DATA WINV/361.9,429.9,514.4,615.3,733.7,869.7,1028.7,1226.2,1460.9
     1,1737.3,2044.4,2407.4,2851.6,3378.1,3986.8,4677.7,5477.3,6442.5,75
     274.3,8872.9,10338.2,12064.6,14049.7,16352.9,18996.2,22056.2,25576.
     38,29634.9,34307.2,39703.3,45943.9,53171.7,61529.1,71213.6,82433.7,
     495441.4,110445.3,127774.4,147406.7,169671.2,194568.0,221877.7,2516
     500.4,282968.2,312800.5,329032.7/
      DATA GRDM/1729.881 ,1591.634 ,1450.598 ,1317.928 ,1198.805 ,1091.6
     134 ,994.4223,896.8127,805.5777,722.7092,649.4024,583.2669,517.9283
     2,457.7689,405.1793,358.9641,316.3347,275.7371,238.9641,206.6136,18
     30.4781,153.5857,130.4781,109.6813,91.9920,76.1753,62.7092,50.9960,
     440.9562,32.5498,25.4263,19.6693,14.9920,11.2470,8.2869,5.9960,4.23
     511,2.8900,1.9020,1.1733,0.6781,0.3544,0.1605,0.0602,0.0171,0.0000/
      DATA U1/0.00263,0.00286,0.00322,0.00372,0.00435,0.00511,0.00600,0.
     100701,0.00821,0.00958,0.01190,0.01305,0.01522,0.01772,0.02061,0.02
     2394,0.02775,0.03207,0.03699,0.04257,0.04884,0.05584,0.06367,0.0722
     39,0.08180,0.09216,0.10340,0.11542,0.12815,0.14147,0.15511,0.16871,
     40.18167,0.19309,0.20167,0.20525,0.20052,0.18186,0.13996,0.05794,-0
     5.09644,-0.39105,-0.99032,-2.39840,-7.14260,-85.0/
      DATA U2/0.00114,0.00124,0.00145,0.00178,0.00221,0.00277,0.00342,0.
     100416,0.00508,0.00615,0.00745,0.00899,0.01083,0.01302,0.01561,0.01
     2869,0.02237,0.02676,0.03195,0.03810,0.04540,0.05412,0.06445,0.0767
     36,0.09140,0.10889,0.12977,0.15473,0.18466,0.22057,0.26382,0.31606,
     40.37932,0.45618,0.54997,0.66493,0.80665,0.98279,1.20442,1.48940,1.
     587040,2.41450,3.28470,4.97840,9.99460,85.0/
      DATA ZLHEM/504.3,2601.0,3122.0,3422.0,3680.0/,STWTM/1.0,3.0,1.0,9.
     10,3.0/,ZLHE/0.0,0.0,7932.0,14380.0,22535.0,32513.0,44313.0,57936.0
     2,73383.0,90603.0/,ZEFF4/0.0,0.0,1.069373 ,1.028328 ,1.022291 ,1.01
     38358,1.015639,1.013614,1.012019,1.011869/,ZLH/911.8,3647.0,8205.9,
     414588.2,22794.1,32823.5,44676.4,58352.9,73852.8,91176.3,110323.4,1
     531293.9,154088.0,178705.6,205146.7,233411.4,263499.6,295411.3,3291
     646.5/,ZLHEP/227.8,911.8,2050.6,3645.6,5696.2,8202.5,11164.5,14582.
     73,18455.7,22784.8,27569.6,32810.1,38506.3,44658.2,51265.8,58329.0,
     865848.0,73822.6,82253.0/
      DATAG3D1/2.885,2.419,2.047,1.679,1.468,1.323,1.212,1.124,1.051,0.9
     189,0.810,0.693,2.906,2.420,2.049,1.684,1.474,1.330,1.220,1.133,1.0
     261,1.000,0.824,0.708,2.912,2.430,2.072,1.723,1.527,1.395,1.296,1.2
     318,1.155,1.102,0.951,0.856,2.892,2.423,2.082,1.760,1.583,1.469,1.3
     485,1.320,1.268,1.226,1.111,1.045,2.815,2.365,2.046,1.755,1.604,1.5
     507,1.438,1.387,1.346,1.314,1.230,1.185,2.715,2.280,1.978,1.709,1.5
     673,1.488,1.428,1.383,1.348,1.320,1.249,1.208,2.615,2.194,1.906,1.6
     754,1.530,1.452,1.398,1.357,1.326,1.303,1.237,1.202,2.231,1.868,1.6
     829,1.440,1.352,1.298,1.261,1.235,1.215,1.198,1.158,1.136,1.955,1.6
     935,1.445,1.303,1.238,1.201,1.175,1.157,1.144,1.133,1.106,1.091/
      DATA G3D2/1.807,1.518,1.357,1.239,1.187,1.157,1.137,1.123,1.112,1.
     1104,1.082,1.065,1.707,1.446,1.303,1.201,1.157,1.131,1.115,1.103,1.
     2094,1.087,1.069,1.054,1.634,1.394,1.266,1.175,1.136,1.114,1.100,1.
     3089,1.081,1.075,1.056,1.046,1.579,1.357,1.239,1.157,1.121,1.102,1.
     4088,1.079,1.072,1.067,1.049,1.042,1.497,1.302,1.201,1.131,1.101,1.
     5085,1.073,1.066,1.060,1.055,1.042,1.034,1.442,1.265,1.175,1.113,1.
     6088,1.073,1.064,1.057,1.052,1.046,1.035,1.030,1.400,1.237,1.156,1.
     7101,1.078,1.065,1.057,1.051,1.045,1.042,1.032,1.026,1.367,1.217,1.
     8142,1.091,1.071,1.059,1.051,1.045,1.041,1.037,1.029,1.024,1.342,1.
     9200,1.130,1.084,1.065,1.053,1.047,1.042,1.037,1.034,1.026,1.022/
      DATA RHOG/1.010,1.025,1.050,1.100,1.150,1.200,1.250,1.300,1.350,1.
     1400,1.600,1.800/,ZLETAG/-3.0000,-2.0000,-1.0000,-0.6021,-0.3010,-0
     2.1249,0.0000,0.3979,0.6990,0.8751,1.0000,1.0969,1.1761,1.3010,1.39
     379,1.4771,1.5441,1.6021/,YY/0.3225,1.7458,4.5366,9.3951/,AA/0.6032
     4,0.3574,0.0389,0.0005/
C
C     DONNEES POUR H2+ =TABLE DE BATES(1952) COMPLETEE PAR HARVARD(1964)
C     ------------------
C     WINV=NU/C,GRDM=(-NU/DNU/DR)*R*D(R),U1=-U(1S SIGMA/R),U2=U(+2P SIGM
C     DONNEES POUR L'HELIUM NEUTRE
C     ----------------------------
C     ZLHEM(K)ET STWTM(K)=LAMBDAS DES DISCONTINUITES  ET VALEUR DU POIDS
C     STATISTIQUE CORRESPONDANT
C     ZLHE(L) ET ZEFF4(L)=SUITE DES DISCONTINUITEES ET SECTIONS EFFICACE
C     CORRESPONDANTES
C     DONNEES POUR L'HYDROGENE GRANT=MON. NOT. VOL. 118 P. 241 1958
C     ------------------------
C     G3D(I,J) =FACTEUR DE GAUNT EN FONCTION DE RHO ET DE LOG(-ETA)
C     RHO(I)  =VALEUR DE RHO CORRESPONDANTES
C     ZLETAG(J)=VALEUR DE LOG(-ETA)
C     YY=ZEROS DU POLYNOME DE LAGUERRE=CHANDRASEKHAR RADIATIVE TRANSFER
C     AA=NBR. DE CHRISTOFFEL CORRESPONDANT
C     DONNEES POUR QUASI MOLECULE H+H (DOYLE,APJ,153,187.1968)
C
      END

C-------------------------------------------------------------------------------
      SUBROUTINE ATHYHE (WL,TH,CALTH,CALLAM,ZZK)
C
C     A.M COLLE  07/12/1970
C
      INTEGER*4 CALLAM,CALTH
      DIMENSION TGAUNT(5),TRHOG(5),OPNU(46),ZZK(11,2),COTE(2),SNIV(2),EX
     1PO(2),CUK(2),CONS(2),AN(2),C1(3),C2(3),C3(3),C4(3),C5(3),C6(3),EXP
     2ON(2)

      COMMON /ABSO2/ NMETA
      COMMON /ABSO3/ JFZ
      COMMON /HYHE/  GRDM(46), U1(46), U2(46), WINV(46), YY(4),
     1               ZLETAG(18), G3D(12,18), AA(4), ZEFF4(10),
     2               RHOG(12), ZLHEP(19)

      COMMON /ABME/  STIMU
      COMMON /GBFH/  G2D(2,19), JSHYD(2), JH
      COMMON /TETA/  AHE, AH, AHEP, UH1, ZEMH, UHEP1, UHE1,ZEUHE1
      COMMON /TEHE/  ZLHEM(5), ZLHE(10)
      COMMON /THE/   ZEUH(20), ZEUHEP(20), ZEXPM(5), ZEXP(10), UL

      DATA EXPO/-68.88230,-71.45087/,CONS/3.3,3.6/,COTE/3.136954E-23,8.1
     195952E-24/,SNIV/0.55,0.485/,CUK/0.3025,0.235225/,AN/0.3099204E-21,
     20.22849203E-21/,C1/-2.850692E-2,-7.056869E-3,3.591294E-3/,C2/0.208
     30816,0.1809394,-0.1959804/,C3/2.549101,-1.828635,4.233733/,C4/-14.
     497997,8.900841,-20.84862/,C5/0.0,-17.78231,0.0/,C6/0.0,-7.89472E-2
     5,0.0/
C
C     CE SSP CALCULE LE COEFFICIENT D'ABSORPTION PAR ATOME NEUTRE POUR
C     L'HYDROGENE ET L'HELIUM, ON SORT 2 VALEURS DE ZZK SI WL= A UNE
C     DISCONTINUITE DE L'UN DE CES ABSORBANTS
C
      JHE=0
      JHEP=0
      JHEM=0
      IF (CALLAM.EQ.1) INDTH=0
      JHYT=1
      GCONST=31.3213*TH
C     31.3213= 157871.62/5040.39 , 157871.62=M*Z**2*E**4/(2*K*(H/2*PI)**
C     M ET E SONT LA MASSE ET LA CHARGE DE L'ELECTRON,H ET K LES CONSTAN
C     DE PLANCK ET DE BOLTZMANN
C
      IF (UL.LT.100.0) GO TO 1333
C     UL=H*NU/(K*T)
      STIMU=1.0
      GO TO 1334
1333  STIMU=1.0-EXP(-UL)
1334  STIMU3=STIMU/UL**3
      IF (CALLAM.EQ.2) GO TO 1335
      ZNL=ALOG(WL)
      ZLAMIN=1.0E8/WL
      DO 3 N=1,2
3     EXPON(N)=EXP(EXPO(N)+CONS(N)*ZNL)
C
C     I   H-
C     H- GINGERICH= HARVARD JUIN 1964 (RESULTATS*1.0E-26)
1335  IF (TH.GE.0.3) GO TO 6060
      ZZK(NMETA+1,1)=0.0
      GO TO 6210
6060  IF ((CALLAM.EQ.2).AND.(INDTH.EQ.1)) GO TO 6100
      INDTH=1
      IF (WL.LE.16419.0) GO TO 6070
      ALTHMB=0.0
      GO TO 6190
6070  WLM=WL/1.0E3
      IF (WL.GT.14200.0) GO TO 6090
      ZKAS=(((5.95244E-4*WLM-0.0204842)*WLM+0.164790)*WLM+0.178708)*WLM+
     10.680133E-2
      GO TO 6100
6090  WLM=16.149-WLM
      ZKAS=((0.273236E-2*WLM-0.411288E-1)*WLM+0.220190)*WLM**2+0.269818
6100  FACT=1.0-EXP((-TH)*28.54310E+3/WL)
C
C     TRANSITION BOUND-FREE= GELTMAN APJ. VOL. 136 P. 935 1962
C
      ALTHMB=ZKAS*4.158E-1*TH**2.5*EXP(1.726*TH)*FACT
C
C     TRANSITION FREE-FREE=T L JOHN (OCT.1963)
C
6190  ALTHML=(WL/1.0E6)*(((-5.939*TH+11.934)*TH-3.2062)+(WL/1.0E3)*((-0.
     134592*TH+7.0355)*TH-0.40192))+((0.027039*TH-0.011493)*TH+0.0053666
     2)
      ZZK(NMETA+1,1)=ALTHMB+ALTHML
C
C     II   H2-
C     H2- SOMMERVILLE= APJ. VOL. 139 P. 195 1963
6210  IF (TH.LT.0.5) GO TO 2050
      IF (WL.GE.3040.0) GO TO 2070
2050  ZZK(NMETA+2,1)=0.0
      GO TO 2080
2070  DKSQ=911.27/WL
      ZZK(NMETA+2,1)=(((0.09319*TH+2.857-0.9316/TH)/DKSQ-(2.6*TH+6.831-4.
     1993/TH))/DKSQ+(35.29*TH-9.804-10.62/TH)-(74.52*TH-62.48+0.4679/TH)
     2*DKSQ)*1.0E-29
C
C     III   H2+
C     H2+ BATES=HARVARD JUIN 1964  (RESULTATS *1.0E+39)
2080  IF ((TH.LT.0.25).OR.((ZLAMIN.LT.WINV(1)).OR.(ZLAMIN.GT.WINV(46))))
     1 GO TO 1012
      BKT=3.19286E-2/TH
C     BKT=K*T EN RYDBERGS POUR H2+
C
      DO 1006 J=1,46
1006  OPNU(J)=2.51E-3*GRDM(J)*(EXP(U1(J)/BKT)-(EXP(-U2(J)/BKT)))
      DO 1013 J=1,46
      JJ=J
      IF (ABS(ZLAMIN-WINV(J)).LE.0.5) GO TO 1014
      IF (ZLAMIN.LT.WINV(J)) GO TO 1015
1013  CONTINUE
1014  ZZK(NMETA+3,1)=OPNU(JJ)
      GO TO 1016
C     INTERPOLATION LINEAIRE
1015  ZZK(NMETA+3,1)=(OPNU(JJ-1)*WINV(JJ)-OPNU(JJ)*WINV(JJ-1)+(OPNU(JJ)-O
     1PNU(JJ-1))*ZLAMIN)/(WINV(JJ)-WINV(JJ-1))
      GO TO 1016
1012  ZZK(NMETA+3,1)=0.0
C
C     CAS OU WL EST UNE DISCONTINUITE
1016  IF (JFZ.NE.2) GO TO 1017
      DO 1019 N=1,3
1019  ZZK(NMETA+N,2)=ZZK(NMETA+N,1)
C
C     IV   H
C     H UNSOLD (1955) PAGE 168
C     FACTEUR DE GAUNT FREE-FREE POUR H=GRANT M.N.,VOL.118
C     SYMBOLES CF VARDYA APJ.SUP. VOL. 8,P.277,1964
1017  IF (TH.GT.1.4) GO TO 1809
      DO 1855 K=1,4
      TRHOG(K)=SQRT(1.0+UL/YY(K))
      IF (TRHOG(K).GE.1.01) GO TO 1820
      IF (TRHOG(K).NE.1.0) TGAUNT(K)=2.0/(TRHOG(K)-1.0)
      IF (TRHOG(K).EQ.1.0) GO TO 1856
      TGAUNT(K)=0.5513289*ALOG(TGAUNT(K))
C     0.5513289=SQRT(3)/PI
      GO TO 1855
1856  TGAUNT(K)=0.0
      GO TO 1855
1820  IF (TRHOG(K).LE.1.8) GO TO 1830
      TEMPOR=(TRHOG(K)-1.0)*SQRT(GCONST/(YY(K)+UL))
      ANY=TEMPOR**(-0.6666667)
      TGAUNT(K)=(-0.01312*ANY+0.21775)*ANY+1.0
      GO TO 1855
1830  TEMPOR=0.2171473*ALOG(GCONST/(YY(K)+UL))
C
C     0.2171473=0.434294482/2
C
      IF ((TEMPOR.LT.ZLETAG(1)).OR.(TEMPOR.GT.ZLETAG(18))) GO TO 1847
C     INTERPOLATION A PARTIR DE LA TABLE 1 DE GRANT (1958)
      DO 1835 IR=1,12
      JR=IR
      IF (ABS(TRHOG(K)-RHOG(IR)).LE.1.0E-4) GO TO 1836
      IF  (TRHOG(K).LT.RHOG(IR)) GO TO 1837
1835  CONTINUE
1836  CARO=1.0
C
C     INTERPOLATION SUR LOG(-ETA) SEULEMENT
C
      GO TO 1838
1837  RHOG1=RHOG(JR-1)
      CARO=TRHOG(K)-RHOG1
1838  RHOG2=RHOG(JR)
      IF (CARO.EQ.1.0) DIFRO=1.0
      IF (CARO.NE.1.0) DIFRO=RHOG2-RHOG1
      DO 1845 IE=1,18
      JE=IE
      IF (ABS(TEMPOR-ZLETAG(IE)).LE.1.0E-4) GO TO 1846
      IF  (TEMPOR.LT.ZLETAG(IE)) GO TO 1848
1845  CONTINUE
1846  IF (CARO.EQ.1.0) GO TO 1850
      CAETA=1.0
C
C     INTERPOLATION SUR RHO SEULEMENT
C
      GO TO 1849
1848  ZLETA1=ZLETAG(JE-1)
      CAETA=TEMPOR-ZLETA1
1849  ZLETA2=ZLETAG(JE)
      IF(CAETA.EQ.1.0)  DIFETA=1.0
      IF(CAETA.NE.1.0)  DIFETA=ZLETA2-ZLETA1
      GO TO 1851
1850  TGAUNT(K)=G3D(JR,JE)
      GO TO 1855
1851  TGAUNT(K)=((G3D(JR-1,JE-1)*(RHOG2-TRHOG(K))+G3D(JR,JE-1)*CARO)*(ZL
     1ETA2-TEMPOR)+(G3D(JR,JE)*CARO+G3D(JR-1,JE)*(RHOG2-TRHOG(K)))*CAETA
     2)/DIFRO/DIFETA
      GO TO 1855
1847  WRITE (6,100)
100   FORMAT ('0 ON SORT DE LA TABLE DE GFF')
1855  CONTINUE
      G3=0.0
      DO 1860 K=1,4
1860  G3=G3+TGAUNT(K)*AA(K)
C     G3=FACTEUR DE GAUNT FREE FREE
      GO TO 4199
1809  ZZK(NMETA+4,1)=0.0
      JHYT=0
4199  DO 4200 I=1,JFZ
      IF (((I.EQ.1).AND.(JHYT.NE.0)).OR.(JH.EQ.1)) GO TO 4201
C
C     WL N'EST PAS UNE DISCONTINUITE DE H
C
      IF (I.EQ.2) ZZK(NMETA+4,2)=ZZK(NMETA+4,1)
      GO TO 1451
4201  SIGH=0.0
      JS=JSHYD(I)
      DO 1410 J=JS,19
1410  SIGH=SIGH+G2D(I,J)*ZEUH(J)
C     RAPPEL  G2D= FACTEUR DE GAUNT BOUND FREE
      BH=SIGH+(ZEUH(20)-(1.0-G3)*ZEMH)/(2*UH1)
      ZZK(NMETA+4,I)=AH*BH*STIMU3
C
C     V   HE+
C     HE+  VARDYA APJ.SUP. VOL. 8,P.277,1964
1451  IF (TH.GT.0.3) GO TO 1552
      SIGHEP=0.0
      DO 1462 J=1,19
      JJ=J
      IF (ABS(WL-ZLHEP(J)).LE.0.50) GO TO 1465
      IF (WL.LT.ZLHEP(J)) GO TO 1463
1462  CONTINUE
1463  JJS=JJ
      GO TO 1470
1465  JHEP=1
      IF (I.EQ.1) GO TO 1463
1468  JJS=JJ+1
1470  IF ((I.EQ.1).OR.(JHEP.EQ.1)) GO TO 1471
C
C     WL N'EST PAS UNE DISCONTINUITE DE HE+
C
      ZZK(NMETA+5,2)=ZZK(NMETA+5,1)
      GO TO 1554
1471  DO 1520 JJ=JJS,19
1520  SIGHEP=SIGHEP+ZEUHEP(JJ)
      BHEP=SIGHEP+ZEUHEP(20)/(2*UHEP1)
      ZZK(NMETA+5,I)=AHEP*BHEP*STIMU3
      GO TO 1554
1552  ZZK(NMETA+5,I)=0.0
C
C     VI   HE
C     HE  VARDYA=APJ. SUP. 80 VOL. 8 P. 277 JANVIER 1964
1554  IF (TH.LE.0.8) GO TO 5400
      ZZK(NMETA+6,I)=0.0
      GO TO 4200
5400  IF ((I.EQ.2).AND.(JH.EQ.1)) GO TO 5872
      SIGHEM=0.0
      SIGHE=0.0
      IF ((WL-ZLHEM(5)).GT.0.50) GO TO 5740
      DO 5460 K=1,5
      KK=K
      IF (ABS(WL-ZLHEM(K)).LE.0.50) GO TO 5490
      IF (WL.LT.ZLHEM(K)) GO TO 5470
5460  CONTINUE
5470  KKS=KK
      GO TO 5540
5490  JHEM=1
      IF (I.EQ.1) GO TO 5470
5520  KKS=KK+1
      IF (KKS.GT.5) GO TO 5740
5540  IF ((JHEM.EQ.1).OR.(I.EQ.1)) GO TO 5541
C
C     WL N'EST PAS = A UNE VALEUR DE ZLHEM
C     RAPPEL  ZLHEM=504,2601,3122,3422,3680 A.
C
      GO TO 5741
5541  DO 5730 K=KKS,5
      GO TO (5560,5560,5560,5620,5680),K
5560  IF (K.EQ.2) ZNL1=ZNL
      IF (K.NE.2) ZNL1=1.0
      ANU=EXP(((((C1(K)*ZNL+C2(K))*ZNL+C3(K))*ZNL+C4(K))*ZNL1+C5(K))*ZNL
     11+C6(K))*1.0E-18
      GO TO 5730
5620  N=1
C
C     GOLDBERG APJ. VOL. 90 P. 414 1939 ET UNDERHILL PUB. COP. OBS. N0.
C
5621  IF (ABS(WL-ZLHEM(3+N)).GT.0.50) GO TO 5640
C     NIVEAUX 4 A 7 DE HE1
      ANU=AN(N)/WL+EXPON(N)
      GO TO 5730
5640  ZK=1.097224E-3*ZLHEM(3+N)*WL/(ZLHEM(3+N)-WL)
      RK=SQRT(ZK)
      UK=1.0+CUK(N)*ZK
      ANU=(COTE(N)/(WL*(1.0-EXP(-6.283185*RK)))*(ZK/UK   )**6*((1.0+ZK)/
     1UK)*((4.0+ZK)/UK)*EXP(-4.0*RK*ATAN(1.0/(SNIV(N)*RK))))+EXPON(N)
      GO TO 5730
5680  N=2
      GO TO 5621
5730  SIGHEM=SIGHEM+ANU*ZEXPM(K)
      BHEM=SIGHEM*STIMU
      GO TO 5741
5740  BHEM=0.0
C     NIVEAUX 8 ET SQ (N.GE.3)
5741  DO 5780 L=3,9
      LL=L
      IF (ABS(WL-ZLHE(L)).LE.0.50) GO TO 5810
      IF  (WL.LT.ZLHE(L)) GO TO 5790
5780  CONTINUE
5790  LLS=LL
      GO TO 5860
5810  JHE=1
      IF (I.EQ.1) GO TO 5790
5840  LLS=LL+1
5860  IF ((I.EQ.1).OR.(JHE.EQ.1)) GO TO 5861
C
C     WL N'EST PAS = A UNE VALEUR DE ZLHE
C
      GO TO 5871
5861  DO 5870 L=LLS,9
5870  SIGHE=SIGHE+ZEXP(L)*ZEFF4(L)
      BHE=SIGHE+(1807.240*ZEXP(10)-0.8072399*ZEUHE1)/(2*UHE1)
5871  ZZK(NMETA+6,I)=AHE*BHE*STIMU3+BHEM
      GO TO 4200
C
C     WL N'EST PAS UNE DISCONTINUITE DE HE
C
5872  ZZK(NMETA+6,2)=ZZK(NMETA+6,1)
4200  CONTINUE
      RETURN
      END


C-------------------------------------------------------------------------------
      SUBROUTINE GAUNTH (WL)
C
C     A.M COLLE   19/8/69
C
      COMMON /GBF/   ZLH(19)
      COMMON /GBFH/  G2D(2,19), JSHYD(2), JH
      COMMON /ABSO3/ JFZ
C
C     DETERMINATION DU FACTEUR DE GAUNT BOUND FREE POUR L HYDROGENE
C
      JH=0
      DO 1410 I=1,JFZ
      DO 1332 J=1,19
      JJ=J
      IF (ABS(WL-ZLH(J)).LE.0.5) GO TO 1335
      IF (WL.LT.ZLH(J)) GO TO 1333
1332  CONTINUE
1333  IF (I.NE.2) GO TO 1334
C


C     CE N'EST PAS UNE DISCONTINUITE DE L'HYDROGENE
C
      DO 1336 J=1,19
1336  G2D(2,J)=G2D(1,J)
      GO TO 1420
1334  JS=JJ
      GO TO 1340
1335  JH=1
C
C     C'EST UNE DISCONTINUITE DE L'HYDROGENE
C
      IF (I.EQ.1) GO TO 1334
      JS=JJ+1
1340  JSHYD(I)=JS
      DO 1410 J=JS,19
      ZJ=J
      IF (J.GT.7) GO TO 1400
      COND=ZLH(J)-WL
      IF (ABS(COND).LE.0.50) GO TO 1122
      IF (COND.LT.0.0) GO TO 1410
      ZQ=WL*J**2/COND
      RK=SQRT(ZQ)
      GO TO (1111,1113,1115,1117,1119,2000,2010),J
C
C     MENZEL ET PEKERIS=MON. NOT. VOL. 96 P. 77 1935
C
1111  DELTA=8.*RK/SQRT(ZQ+1.0)
      GO TO 1120
1113  DELTA=(16.*RK*(3.*ZQ+4.)*(5.*ZQ+4.))/(ZQ+4.)**2.5
      GO TO 1120
1115  DELTA=(24.*RK*((13.*ZQ+78.)*ZQ+81.)*((29.*ZQ+126.)*ZQ+81.))/(ZQ+9.
     1)**4.5
      GO TO 1120
1117  DELTA=32.*RK*(((197.*ZQ+3152.)*ZQ+13056.)*ZQ+12288.)*(((539.*ZQ+68
     100.)*ZQ+20736.)*ZQ+12288.)/(9.*(ZQ+16.)**6.5)
      GO TO 1120
1119  DELTA=40.*RK*((((1083.*ZQ+36100.)*ZQ+372250.)*ZQ+1312500.)*ZQ+1171
     1875.)*((((3467.*ZQ+95700.)*ZQ+786250.)*ZQ+2062500.)*ZQ+1171875.)/(
     29.*(ZQ+25.)**8.5)
      GO TO 1120
C
C     HAGIHARA AND SOMA=J.OF ASTR. AND GEOPHYS. JAPANESE VOL. 20 P. 59 1
C
2000  ZP=(ZQ+36.)**5.25
      DELTA=48.*RK*((((((38081.*ZQ+1953540.)*ZQ+3348086.E1)*ZQ+2262816.E
     12)*ZQ+5458752.E2)*ZQ+3023309.E2)/ZP)*((((((10471.*ZQ+628260.)*ZQ+1
     2290902.E1)*ZQ+1087085.E2)*ZQ+34992.0E4)*ZQ+3023309.E2)/25./ZP)
      GO TO 1120
2010  ZP=(ZQ+49.)**6.25
      DELTA=56.*RK*(((((((56740.9*ZQ+5560608.)*ZQ+1993433.E2)*ZQ+3248060
     1.E3)*ZQ+2428999.E4)*ZQ+7372604.E4)*ZQ+6228579.E4)/ZP)*(((((((22974
     22.5*ZQ+1968907.E1)*ZQ+6067219.E2)*ZQ+8290160.E3)*ZQ+5002406.E4)*ZQ
     3+1144025.E5)*ZQ+6228579.E4)/20.25/ZP)
1120  G2D(I,J)=5.441398*RK*J*EXP(-4.*RK*ATAN(ZJ/RK))*DELTA/(SQRT(ZQ+ZJ**
     12)*(1.-EXP(-6.283185*RK)))
      GO TO 1410
1122  GO TO (1123,1125,1127,1129,1131,2020,2030),J
1123  G2D(I,J)=0.7973
      GO TO 1410
1125  G2D(I,J)=0.8762
      GO TO 1410
1127  G2D(I,J)=0.9075
      GO TO 1410
1129  G2D(I,J)=0.9247
      GO TO 1410
1131  G2D(I,J)=0.9358
      GO TO 1410
2020  G2D(I,J)=0.9436
      GO TO 1410
2030  G2D(I,J)=0.9494
      GO TO 1410
1400  G2D(I,J)=1.0
1410  CONTINUE
1420  RETURN
      END

C-------------------------------------------------------------------------------
      SUBROUTINE READER06 (NH,TETA,PE,PG,T5L,NTOT)
C     CE S.P. LIT SUR DISQUE ACCES DIRECT NH,TETA,PE,PG,T5L,NTOT
      DIMENSION NH(50),TETA(50),PE(50),PG(50),T5L(50),BID(16)
      REAL NH,NHE, DETEF, DGLOG, DSALOG, ASALALF
      CHARACTER*4 BLC, TIT

      COMMON /COM6/ TETAEF, main_GLOG, ASASOL, NHE, TIT(5)
      COMMON /ALOG/ main_ASALOG

      DATA   BLC/'    '/
      DO 7 I=1,5
7     TIT(I)=BLC
      IF(IDEF.EQ.211939)   GO TO 10
      OPEN(UNIT=18,ACCESS='DIRECT',STATUS='OLD',
     1    FILE='modeles.mod', RECL = 1200)
      ID=1
      IDEF=211939
10    READ(4,*) main_TEFF,main_GLOG,main_ASALOG,NHE,INUM
        print *, main_TEFF,main_GLOG,main_ASALOG,NHE,INUM
        TETAEF=5040/main_TEFF
      IF(INUM.GT.0)   ID=INUM
      WRITE(6,102)TETAEF,main_GLOG,main_ASALOG,NHE,INUM

C   SI L ON DESIRE IMPOSER UN MODELE  ON MET EN INUM LE NUM DU MODELE
C   SUR LE FICHIER ACCES DIRECT

C ISSUE: Variable NHE read again from a different file!!!!!!!!!

9     READ(18, REC=ID) NTOT,DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT,TITABS
      WRITE(6,105)DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT
        write(6,108) TIABS
      IF(NTOT.EQ.9999)   GO TO 6
      DDT  = ABS(main_TEFF-DETEF)
C     DDTA  = ABS(TETAEF-DETAEF)
      DDG = ABS(main_GLOG-DGLOG)
      DDAB = ABS(main_ASALOG-DSALOG)
      DDHE= ABS(NHE-DNHE)
C     DDIF = DDTA+DDG+DDAB+DDHE
5     IF(DDT.GT.1.0)   GO TO 9
      IF(DDG.GT.0.01)   GO TO 9
      IF(DDAB.GT.0.01)   GO TO 9
      ASASOL=10.**main_ASALOG
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

C-------------------------------------------------------------------------------
      SUBROUTINE BK(NH,TETA,PE,PG,NTOT,LAMBD,B,B1,B2,ALPH,PHN,PH2,
     1              FC,KC,KC1,KC2,KCD,TTD,DTOT,main_PTDISK,main_MU,KIK,LZERO,LFIN)
      PARAMETER(NP=7000)
      INTEGER D,DTOT,CAVA
      LOGICAL main_PTDISK,main_ECRIT
      REAL LAMBD,NH,main_MU,NU,KB,KC,KC1,KC2,LLZERO,LLFIN,NU1,NU2,KCD,
     1 LAMBDC,KCJ,KCN
      REAL*8 LZERO,LFIN
      DIMENSION B(0:50),TO_TOTO(0:50),B1(0:50),B2(0:50)
      DIMENSION NH(50),TETA(50),PE(50),PG(50),KC(50),TOTKAP(2),
     1 ALPH(50),PHN(50),PH2(50),KC1(50),KC2(50)

c p 21/11/04 M-N  DIMENSION TTD(DTOT),KCD(DTOT,50),KCJ(2,50),KCN(2),LAMBDC(2)
      DIMENSION TTD(NP),KCD(NP,50),KCJ(2,50),KCN(2),LAMBDC(2)
      DIMENSION FTTC(NP)
      DIMENSION FC(NP)

      COMMON /LECT1/ AMET, BHE
      COMMON /LECT2/ ZP(30), ZM(30), WI(41,2), NUMSET(2), CAL
      COMMON /ABSO2/ NMETA
      COMMON /SAPE/  AVM, ZNU1, ZNU2, ZNU3, ZMUZE, ZNU(30)
      COMMON /SAPU/  PE, RHO, TOC, ZNH(12)
      COMMON /ABSO1/ NM

C     COMMON ENTRE LE PROGRAMME PRINCIPAL ET LE SP FLIN1
      COMMON /TOTO/  TO_TOTO
      COMMON /FAIL/  ERR(50)
      COMMON /PRT/   main_ECRIT
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
      CALL FLIN1(KC1,B1,NH,NTOT,main_PTDISK,main_MU,FC1,KIK,CAVA)
      IF(CAVA. GT.0) THEN
      WRITE(6,132) CAVA
      WRITE(6,135) (I, TO_TOTO(I),ERR(I),I=1,NTOT)
      IF(CAVA.GT.1) STOP
      END IF
      ALPH02=EXP(-AHNU2/(KB*T))
      B2(0)=C32 * (ALPH02/(1.-ALPH02))
      CALL FLIN1(KC2,B2,NH,NTOT,main_PTDISK,main_MU,FC2,KIK,CAVA)
      IF(CAVA. GT.0) THEN
      WRITE(6,132) CAVA
      WRITE(6,135) (I,TO_TOTO(I),ERR(I),I=1,NTOT)
      IF(CAVA.GT.1) STOP
      END IF
      ALPH0=EXP(-AHNU/(KB*T))
      B(0)=C3 * (ALPH0/(1.-ALPH0))
      CALL FLIN1(KC,B,NH,NTOT,main_PTDISK,main_MU,FC,KIK,CAVA)
      IF(CAVA. GT.0) THEN
      WRITE(6,132) CAVA
      WRITE(6,135) (I,TO_TOTO(I),ERR(I),I=1,NTOT)
      IF(CAVA.GT.1) STOP
      END IF
cpc   WRITE(6,151) KC1(1),KC1(NTOT),B1(0),B1(1),B1(NTOT),FC1
cpc   WRITE(6,152) KC2(1),KC2(NTOT),B2(0),B2(1),B2(NTOT),FC2
cpc   WRITE(6,150) KC(1),KC(NTOT),B(0),B(1),B(NTOT),FC
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
C     IF(.NOT.main_ECRIT) GO TO 10
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


C-------------------------------------------------------------------------------
      SUBROUTINE LECTAUH(NH,NTOT,main_PAS,JJMAX,LLAMBDH,TTH,
     1 DTOT,TTD,LZERO,LFIN,TAUH,DHM,DHP,FILETOH)
      PARAMETER(NP=7000)
      LOGICAL main_ECRIT
      INTEGER D,DTOT,DHM,DHP
      REAL NH
      REAL*8 LAMBDH,LLAMBDH,LZERO,LFIN
      CHARACTER*20 FILETOH,TITRE*80,TTT*11
      DIMENSION NH(50),TTH(100,50),TH(50,50)
      DIMENSION LAMBDH(50),LLAMBDH(100),ALLH(100)
c p 20/11/04 M-N  DIMENSION TTD(DTOT),FTTH(NP),TAUHN(100),TAUH(DTOT,50)
      DIMENSION TTD(NP),FTTH(NP),TAUHN(100),TAUH(NP,50)

      COMMON /PRT/ main_ECRIT
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
      IF(.NOT.main_ECRIT) GO TO 15
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

C-------------------------------------------------------------------------------
      SUBROUTINE SELEKFH(main_PTDISK,main_MU,KIK,DTOT,main_PAS,atomgrade_NBLEND,
     1 GFAL,atomgrade_ZINF,ABOND,ECART,atomgrade_ELEM,atomgrade_LAMBDA,TAUH,DHM,DHP,VT,
     2 NTOT,NH,TETA,B,B1,B2,KCD,POP,DELTA,A,TTD,FL,FCONT)
      PARAMETER(MAX_atomgrade_NBLEND=8000,PARAMETER_NMOL=50000,NP=7000)
      LOGICAL main_PTDISK,main_ECRIT
      INTEGER D, DTOT, CAVA,DHM,DHP
      REAL lambi
      REAL main_MU,KAPPA,KA,KAP,NH,KCD,KCI,KAM,KAPPAM,KAPPT,MM
      REAL*8 atomgrade_LAMBDA,main_PAS,ECART,ECAR,ECARTM,ECARM,LMBDAM
      DIMENSION NH(50),TETA(50),VT(50)
      DIMENSION B(0:50),TO_TOTO(0:50),B1(0:50),B2(0:50),BI(0:50)
      DIMENSION ECART(MAX_atomgrade_NBLEND),ECAR(MAX_atomgrade_NBLEND), atomgrade_ZINF(MAX_atomgrade_NBLEND),ECARTL(MAX_atomgrade_NBLEND),
     1 GFAL(MAX_atomgrade_NBLEND),ABOND(MAX_atomgrade_NBLEND),KA(MAX_atomgrade_NBLEND),KAP(50),atomgrade_ELEM(MAX_atomgrade_NBLEND),
     2 KAPPA(50),atomgrade_LAMBDA(MAX_atomgrade_NBLEND),KCD(NP,50),KCI(50),
     3 POP(MAX_atomgrade_NBLEND,50),DELTA(MAX_atomgrade_NBLEND,50),A(MAX_atomgrade_NBLEND,50)
cp 20/11/04 M-N   DIMENSION TTD(DTOT),FL(DTOT),TAUHD(50),TAUH(DTOT,50)
      DIMENSION TTD(NP),FL(NP),TAUHD(50),TAUH(NP,50)
      DIMENSION FCONT(NP)
      DIMENSION DELTAM(PARAMETER_NMOL,50),ECARTM(PARAMETER_NMOL),ECARM(PARAMETER_NMOL),
     1 ECARTLM(PARAMETER_NMOL),KAM(PARAMETER_NMOL),KAPPAM(50),KAPPT(50)
      DIMENSION LMBDAM(PARAMETER_NMOL),GFM(PARAMETER_NMOL),PNVJ(PARAMETER_NMOL,50),
     1 ALARGM(PARAMETER_NMOL),dm(8)
      COMMON /TOTO/  TO_TOTO
      COMMON /FAIL/  ERR(50)
      COMMON /KAPM1/ MM,MBLEND
      COMMON /KAPM2/ LMBDAM,GFM,PNVJ,ALARGM
      COMMON /KAPM4/ ECARTM
      COMMON /PRT/   main_ECRIT
      COMMON /CNO/   DM

      DATA DEUXR/1.6634E+8/,RPI/1.77245385/,C/2.997929E+10/
C

      if(atomgrade_NBLEND.ne.0) then
            DO K=1,atomgrade_NBLEND
            ECAR(K)=ECART(K)
            END DO
      end if
      if(mblend.ne.0) then
            DO K=1,MBLEND
            ECARM(K)=ECARTM(K)
            END DO
      end if
      DO D=1,DTOT
      lambi = (6270+(D-1)*0.02)
      if(atomgrade_NBLEND.ne.0) then
            DO K=1,atomgrade_NBLEND
            ECAR(K)=ECAR(K)-main_PAS
            ECARTL(K)=ECAR(K)
            END DO
      end if
      if(mblend.ne.0) then
            DO K=1,MBLEND
            ECARM(K)=ECARM(K)-main_PAS
            ECARTLM(K)=ECARM(K)
            END DO
      end if
            DO N=1,NTOT
            KAPPA(N) =0.
            KAPPAM(N) =0.
            T=5040./TETA(N)
c     atomes
      if(atomgrade_NBLEND.eq.0) go to 260

       DO  K=1,atomgrade_NBLEND
         IF( ABS(ECARTL(K)) .GT. atomgrade_ZINF(K) )  THEN
           KA(K)=0.
           ELSE
             V=ABS(ECAR(K)*1.E-8/DELTA(K,N))
           CALL HJENOR(A(K,N),V,DELTA(K,N),PHI)
           KA(K) = PHI * POP(K,N) * GFAL(K) * ABOND(K)
             IF(K.eq.1)KA(K) = PHI * POP(K,N) * GFAL(K)

         END IF
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

c     WRITE(6,151) D,BI(0),BI(1),BI(NTOT)
c     WRITE(6,150) D, KCI(1),KCI(NTOT),KAPPA(1),KAPPA(NTOT)
c     WRITE(6,152) KAPPAM(1),KAPPAM(NTOT)

      IF(D.EQ.DTOT) WRITE(6,151) D,BI(0),BI(1),BI(NTOT)
      IF(D.EQ.DTOT)WRITE(6,150) D,KCI(1),KCI(NTOT),KAPPA(1),KAPPA(NTOT)
      IF(D.EQ.DTOT)WRITE(6,152)KAPPAM(1),KAPPAM(NTOT)

      IF((D.LT.DHM).OR.(D.GE.DHP)) THEN
      CALL FLIN1 (KAP,BI,NH,NTOT,main_PTDISK,main_MU,FL(D),KIK,CAVA)
                  IF(CAVA.GT.1)   THEN
                  WRITE(6,131) TTD(D),CAVA
                  STOP
                  END IF
c     FN(D) = FL(D) / FCONT(D)
      ELSE
            DO N=1,NTOT
            TAUHD(N)=TAUH(D,N)
            END DO
      CALL FLINH (KAP,BI,NH,NTOT,main_PTDISK,main_MU,TAUHD,FL(D),KIK,CAVA)
                  IF(CAVA.GT.1)   THEN
                  WRITE(6,131) TTD(D),CAVA
                  STOP
                  END IF
      END IF
c Dez 03-P. Coelho - calculate the continuum and normalized spectra
      CALL FLIN1 (KCI,BI,NH,NTOT,main_PTDISK,main_MU,FCONT(D),KIK,CAVA)
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

C-------------------------------------------------------------------------------
      SUBROUTINE ABONDRAIH(abonds_ELE,ABO,abonds_NABOND,atomgrade_ELEM,ABOND,atomgrade_NBLEND)
      CHARACTER*2 abonds_ELE, atomgrade_ELEM
      REAL ABO, ABOND
      INTEGER abonds_NABOND, atomgrade_NBLEND, MAX_atomgrade_NBLEND
      PARAMETER(MAX_atomgrade_NBLEND=8000)
      DIMENSION abonds_ELE(100),ABO(100),atomgrade_ELEM(8000),ABOND(8000)

      DO  K=1,atomgrade_NBLEND
            DO  J=1,abonds_NABOND
C           print 1035, abonds_ELE(J), atomgrade_ELEM(k), ALOG10(abo(j))-0.37+12
            IF(abonds_ELE(J).EQ.atomgrade_ELEM(K))  GO TO 14
            END DO   !FIN BCLE SUR J
      WRITE(6,106)     atomgrade_ELEM(K)
      STOP
14    ABOND(K)=ABO(J)
      END DO   !FIN BCLE SUR K
      RETURN
c
106   FORMAT('     MANQUE L ABONDANCE DU  ',A2                  )
      END

C-------------------------------------------------------------------------------
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
c p 21/11/04 instrucao da Marie Noel
      if (FTT(ITOT).NE.0.0) K2=ITOT
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

C-------------------------------------------------------------------------------
      SUBROUTINE POPADELH (NPAR,EL,KI1,KI2,M,atomgrade_NBLEND,atomgrade_ELEM,
     1 atomgrade_LAMBDA,atomgrade_KIEX,atomgrade_CH,CORCH,CVdW,atomgrade_GR,atomgrade_GE,atomgrade_IONI,NTOT,TETA,PE,ALPH,
     2 PHN,PH2,VT,P,POP,A,DELTA)
C     ***calcule la population au niveau inferieur de la transition
C     ***la largeur doppler DELTA et le coefficient d'elargissement
C     ***le "A" utilise dans le calcul de H(A,V)
      PARAMETER(MAX_atomgrade_NBLEND=8000)
      CHARACTER*1 ISI(1), ISS(1)
      CHARACTER*2 atomgrade_ELEM, EL
      INTEGER atomgrade_NBLEND, NPAR, J, K
        real KI1,KI2,atomgrade_KIEX,M,KB,KIES,KII,NUL
      DIMENSION PE(50),TETA(50),VT(50),ALPH(50),PHN(50),PH2(50),
     1 EL(85),M(85),KI1(85),KI2(85),P(3,85,50),ALPHL(50),
     2 atomgrade_ELEM(MAX_atomgrade_NBLEND),atomgrade_IONI(MAX_atomgrade_NBLEND),atomgrade_KIEX(MAX_atomgrade_NBLEND),
     3 atomgrade_CH(MAX_atomgrade_NBLEND),CORCH(MAX_atomgrade_NBLEND),CVdW(MAX_atomgrade_NBLEND),atomgrade_GR(MAX_atomgrade_NBLEND),
     4 atomgrade_GE(MAX_atomgrade_NBLEND),POP(MAX_atomgrade_NBLEND,50),A(MAX_atomgrade_NBLEND,50),DELTA(MAX_atomgrade_NBLEND,50)
      DIMENSION PPH(50),PPC2(50),PN(50),PC13(50),PMG(50),
     1 PO(50),PTI(50),PNG(50),PIG(50),pfe(50),dm(8)
      CHARACTER*2 TTI, CC, OO, NN, MGG
      COMMON /KAPM3/ PPH, PPC2, PN, PC13, PMG, PO, PTI, PNG, PIG, pfe
      COMMON /CNO/ DM

        data KB/1.38046E-16/, DEUXR/1.6634E+8/, C4/2.1179E+8/,
     1  C6/3.76727E+11/, PI/3.141593/, C/2.997929E+10/
      DATA ISI/' '/, ISS/' '/
        DATA TTI/'TI'/,CC/' C'/,OO/' O'/,NN/' N'/,MGG/'MG'/
      H=6.6252E-27
      C5= 2.*PI* (3.*PI**2/2.44)**0.4
C     C6=4 * Pi * C
c
        DO  K=1,atomgrade_NBLEND
            corch(k)=0.
            CVdW(K)=0
                DO  J=1,NPAR
                IF(EL(J).EQ.atomgrade_ELEM(K)) GO TO 15
                END DO
        WRITE(6,104) atomgrade_ELEM(K)
        STOP
15      IOO=atomgrade_IONI(K)
C
            write(77,*)atomgrade_ELEM(k),atomgrade_LAMBDA(k)
                IF(atomgrade_CH(K).LT.1.E-37)  THEN
                KIES=(12398.54/atomgrade_LAMBDA(K)) + atomgrade_KIEX(K)
                IF(IOO.EQ.1)   KII=KI1(J)
                IF(IOO.EQ.2)   KII=KI2(J)
                        IF(CORCH(K).LT.1.E-37)   THEN
                        CORCH(K)=0.67 * atomgrade_KIEX(K) +1
                        END IF   ! FIN DE IF CORCH(K)=0
C               WRITE(6,125)  atomgrade_LAMBDA(K), CORCH(K)
                CVdW(K)= CALCH(KII,IOO,atomgrade_KIEX(K),ISI,KIES,ISS)
            atomgrade_CH(K)= CVdW(K) * CORCH(K)
                END IF  ! FIN DE IF atomgrade_CH=0.

C
            IF(atomgrade_CH(K) .LT. 1.E-20) then
            IOPI=1
            else
            IOPI=2
            end IF
        DO  N=1,NTOT
        T=5040./TETA(N)

      NUL= C* 1.E+8 /atomgrade_LAMBDA(K)
      AHNUL= H*NUL
      ALPHL(N)=EXP(-AHNUL/(KB*T))

        TAP = 1.-ALPHL(N)
        TOP = 10.**(-atomgrade_KIEX(K)*TETA(N))
        POP(K,N) = P(IOO,J,N)*TOP*TAP
C NOXIG:
        IF(K.eq.1) POP(K,N)=TOP*TAP*P(IOO,J,N)*PO(N)/PPH(N)
c     C
c       if((atomgrade_ELEM(k).eq.dm(1)).or.(atomgrade_ELEM(k).eq.dm(2))) then
c       POP(K,N)=TOP*TAP*P(IOO,J,N)*PPC2(N)/PPH(N)
c        write(48,488)atomgrade_LAMBDA(k),atomgrade_ELEM(k),dm(3),ioo,pop(k,n)
c        write(48,488)atomgrade_LAMBDA(k),atomgrade_ELEM(k),dm(4),ioo,pop(k,n)
c        end if
c     N
c       if((atomgrade_ELEM(k).eq.dm(3)).or.(atomgrade_ELEM(k).eq.dm(4))) then
c     POP(K,N)=TOP*TAP*P(IOO,J,N)*PN(N)/PPH(N)
c     end if
c     O
c       if((atomgrade_ELEM(k).eq.dm(5)).or.(atomgrade_ELEM(k).eq.dm(6))) then
c        POP(K,N)=TOP*TAP*P(IOO,J,N)*PO(N)/PPH(N)
c        write(48,488)atomgrade_LAMBDA(k),atomgrade_ELEM(k),dm(5),ioo,pop(k,n)
c        write(48,488)atomgrade_LAMBDA(k),atomgrade_ELEM(k),dm(6),ioo,pop(k,n)
c        end if
c     Mg
c       if(atomgrade_ELEM(k).eq.dm(7)) then
c     POP(K,N)=TOP*TAP*P(IOO,J,N)*PMG(N)/PPH(N)
c     end if
c     Ti
c       if(atomgrade_ELEM(k).eq.dm(8)) then
c        POP(K,N)=TOP*TAP*P(IOO,J,N)*PTI(N)/PPH(N)
c       write(48,488)atomgrade_LAMBDA(k),atomgrade_ELEM(k),dm(1),ioo,pop(k,n)
c        end if

        DELTA(K,N) =(1.E-8*atomgrade_LAMBDA(K))/C*SQRT(VT(N)**2+DEUXR*T/M(J))
        VREL    = SQRT(C4*T*(1.+1./M(J)))
            IF (IOPI.EQ.1)  then
            GH   =C5*atomgrade_CH(K)**0.4*VREL   **0.6
C                 if (N.EQ.10)  write (6,100) GH
            else
            GH = atomgrade_CH(K) + Corch(K)*T
C                 if (N.EQ.10) write(6, 101) GH
            END IF
        GAMMA = atomgrade_GR(K)+(atomgrade_GE(K)*PE (N)+GH*(PHN(N)+1.0146*PH2(N)))/(KB*T)
        A(K,N) =GAMMA*(1.E-8*atomgrade_LAMBDA(K))**2 / (C6*DELTA(K,N))
c     if((k.le.3).and.(n.eq.1)) then
c     write(6,*) atomgrade_LAMBDA(k),atomgrade_GR(k),atomgrade_GE(k),gh,gamma
c     write(6,*) alphl(n),p(ioo,j,n),top,tap
c     write(6,*) vrel,m(j),atomgrade_CH(k),corch(k)
c     write(6,*) n,pe(n),phn(n),ph2(n),t
c     end if
c     if((k.le.3).and.(n.eq.ntot)) then
c     write(6,*) atomgrade_LAMBDA(k),atomgrade_GR(k),atomgrade_GE(k),gh,gamma
c     write(6,*) alphl(n),p(ioo,j,n),top,tap
c     write(6,*) vrel,m(j),atomgrade_CH(k),corch(k)
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

C-------------------------------------------------------------------------------
      SUBROUTINE FLINH (KAP,B,NH,NTOT,main_PTDISK,main_MU,TAUHD,F,IOP,CAVA)
c     calcul du flux ou de l'intensite par la methode d'integration
c     a 6 pts (ou 13pts) de R.Cayrel (these).
c     nouvelle methode de calcul de to . TO(1)est calcule et
c     est different de 0 (On pose TO(0)=0)   -Avril 1988-
      LOGICAL main_PTDISK
      REAL   NH,KAP,main_MU
      INTEGER CAVA
      DIMENSION B(0:50),  TO_TOTO(0:50)
      DIMENSION NH(50),KAP(50),BBB(26),TD2(26),
     1  TD(6),TP(7),CD(6),CP(7),C1(13),C2(12),C3(12)
      DIMENSION TAUHD(50)

      COMMON /TOTO/ TO_TOTO
      COMMON /FCO/  FP_FCO(13),CC(13),TT(13),BB(13)
      COMMON /CCC/  AMF(50), AMF2(50), FX1(50), FX2(50)
      COMMON /FAIL/ ERR(50)

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
C           (now dubbed "TO_TOTO")
C
      CAVA=0
      EPSI=0.05
      TO_TOTO(0)=0.
        TO_TOTO(1)=NH(1)*(KAP(1)-(KAP(2)-KAP(1))/(NH(2)-NH(1))*NH(1)/2.)
        CALL INTEGRA(NH,KAP,TO_TOTO,NTOT,TO_TOTO(1))
            DO N=1,NTOT
            TO_TOTO(N)=TO_TOTO(N)+TAUHD(N)
            END DO
C
C           CALCUL DU FLUX
      IF(IOP.EQ.0)   THEN
C               FORMULE A 6 OU 7 PTS
            IF(main_PTDISK) THEN
            IPOINT=7
            TOLIM=4.0
                     ELSE
            IPOINT=6
            TOLIM=3.89
            END IF
c     on verifie que le modele n'est pas trop court
      IF (TO_TOTO(NTOT) .LT. TOLIM)    THEN
      WRITE(6,1504)
      WRITE(6,1503) NTOT, TO_TOTO(NTOT)
      WRITE(6,1501)
      CAVA=2
      RETURN
      END IF
c
2     DO  L=1,IPOINT
            IF(main_PTDISK) THEN
            TT(L) = TP(L)*main_MU
            CC(L)=CP(L)
                     ELSE
            TT(L) = TD(L)
            CC(L) = CD(L)
            END IF
      END DO
c
      F=0.
            DO  L=1,IPOINT
            BB(L)=FAITK30(TT(L), TO_TOTO, B, NTOT)
            FP_FCO(L)=CC(L)*BB(L)
            F=F+FP_FCO(L)
            END DO
      RETURN
C
                        ELSE
C     FORMULE A 26 PTS (NE MARCHE QUE POUR LE FLUX!)
C           (13PTS +PTS MILIEU)
            IF(main_PTDISK)   then
            WRITE(6,1500)
            STOP
            END IF
      TOLIM=5.487  ! Le modele doit aller au moins a une prof TOLIM
            IF(TO_TOTO(NTOT).LT. TOLIM) THEN
              WRITE(6,1504)
              WRITE(6,1503) NTOT,TO_TOTO(NTOT)
              WRITE(6,1501)
              CAVA=2
              RETURN
              END IF
c
      DO L=1,26
      T=TD2(L)
      BBB(L) = FAITK30(TD2(L),TO_TOTO,B,NTOT)
      END DO
C
      DO M=1,12
      L=2*M - 1
      BB(M)=BBB(L+1)
      FP_FCO(M)=C1(M)*BBB(L) + C2(M)*BBB(L+1) + C3(M)*BBB(L+2)
      CC(M)=C2(M)
      END DO
      FP_FCO(13)=C1(13)*BBB(26)
      BB(13)=BBB(26)
      CC(13)=C1(13)
C     CES BB ET CC NE SERVENT QUE POUR LES SORTIES (PAS AU CALCUL)
      F=0.
      DO L=1,13
      F=F+FP_FCO(L)
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

C-------------------------------------------------------------------------------
      SUBROUTINE FLIN1 (KAP,B,NH,NTOT,main_PTDISK,main_MU,F,IOP,CAVA)
c     calcul du flux ou de l'intensite par la methode d'integration
c     a 6 pts (ou 13pts) de R.Cayrel (these).
c     nouvelle methode de calcul de to . TO_TOTO(1) est calcule et
c     est different de 0 (On pose TO_TOTO(0)=0)   -Avril 1988-
C (JT2015) dubbed TO_TOTO because the 2-letter variable that I won't
C          mention is a reserved word
      LOGICAL main_PTDISK
      REAL   NH,KAP,main_MU
      INTEGER CAVA
      DIMENSION B(0:50),  TO_TOTO(0:50)
      DIMENSION NH(50),KAP(50),T5L(50),BBB(26),TD2(26),
     1  TD(6),TP(7),CD(6),CP(7),C1(13),C2(12),C3(12),
     1  TT2(6000),BB2(6000),FP2(6000),CC2(6000)

      COMMON /TOTO/ TO_TOTO
      COMMON /FCO/  FP_FCO(13), CC(13), TT(13), BB(13)
      COMMON /CCC/  AMF(50), AMF2(50), FX1(50), FX2(50)
      COMMON /FAIL/ ERR(50)

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
C           CALCUL DE TO_TOTO
C
      CAVA=0
      EPSI=0.05
      TO_TOTO(0)=0.
        TO_TOTO(1)=NH(1)*(KAP(1)-(KAP(2)-KAP(1))/(NH(2)-NH(1))*NH(1)/2.)
c     write(6,*)NH(1),NH(2),Kap(1),KAP(2),ntot
c     read(5,*)

        CALL INTEGRA(NH,KAP,TO_TOTO,NTOT,TO_TOTO(1))
c     do i = 1,50
c     print *, TO_TOTO(i), B(I)
c     end do
c     STOP
C
C           CALCUL DU FLUX
      IF(IOP.EQ.0)   THEN
C               FORMULE A 6 OU 7 PTS
            IF(main_PTDISK) THEN
            IPOINT=7
            TOLIM=4.0
                     ELSE
            IPOINT=7
            TOLIM=3.89
            END IF

c     on verifie que le modele n'est pas trop court
      IF (TO_TOTO(NTOT) .LT. TOLIM) THEN
      WRITE(6,1504)
      WRITE(6,1503) NTOT,TO(NTOT)
      WRITE(6,1501)
      CAVA=2
      RETURN
      END IF

2     DO  L=1,IPOINT
            IF(main_PTDISK) THEN
            TT(L) = TP(L)*main_MU
            CC(L)=CP(L)
                     ELSE
            TT(L) = TD(L)
            CC(L) = CD(L)
            END IF
      END DO
c
      F=0.
            DO  L=1,IPOINT
            BB(L)=FAITK30(TT(L), TO_TOTO, B, NTOT)
            FP_FCO(L)=CC(L)*BB(L)
            F=F+FP_FCO(L)
            END DO
      RETURN

      END IF

C
      IF(IOP.EQ.1) THEN
C     FORMULE A 26 PTS (NE MARCHE QUE POUR LE FLUX!)
C           (13PTS +PTS MILIEU)
            IF(main_PTDISK)   then
            WRITE(6,1500)
            STOP
            END IF
      TOLIM=5.487  ! Le modele doit aller au moins a une prof TOLIM
            IF(TO_TOTO(NTOT) .LT. TOLIM) THEN
              WRITE(6,1504)
              WRITE(6,1503) NTOT,TO_TOTO(NTOT)
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
      FP_FCO(M)=C1(M)*BBB(L) + C2(M)*BBB(L+1) + C3(M)*BBB(L+2)
      CC(M)=C2(M)
      END DO
      FP_FCO(13)=C1(13)*BBB(26)
      BB(13)=BBB(26)
      CC(13)=C1(13)
C     CES BB ET CC NE SERVENT QUE POUR LES SORTIES (PAS AU CALCUL)
      F=0.
      DO L=1,13
      F=F+FP_FCO(L)
      END DO
      RETURN

      END IF  !(fin du IF IOP)

      IF(IOP.EQ.2) THEN
            IPOINT=600
            TOLIM=6.00

c     on verifie que le modele n'est pas trop court
      IF (TO(NTOT).LT.TOLIM  )    THEN
      WRITE(6,1504)
      WRITE(6,1503) NTOT,TO(NTOT)
      WRITE(6,1501)
      CAVA=2
      RETURN
      END IF

      DO  L=1,IPOINT
            TT2(L) = 0.01 + (L-1) * 0.01
            CC2(L) = EXP(-1*(TT2(L))) * 0.01
c     print *, TT(L), CC(L)
      END DO
c      STOP
c
      F=0.
            DO  L=1,IPOINT
            BB2(L)=FAITK30(TT2(L),TO,B,NTOT)
            FP2(L)=CC2(L)*BB2(L)
            F=F+FP2(L)
            END DO

      RETURN
      END IF  !(fin du IF IOP)

C
1500  FORMAT('   LE SP FLIN1 NE PEUT CALCULER L INTENSITE EN 1 PT ',
     1 'DU DISQUE AVEC LA FORMULE A 26PTS (UTILISER 7PTS IOP=0)' )
1501  FORMAT(1H //)
1502  FORMAT(5(F7.3,F5.2,2X))
1503  FORMAT(I10,5X,3HTO=,F10.4)
1504  FORMAT(18H MODELE TROP COURT)
      END

C-------------------------------------------------------------------------------
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

C-------------------------------------------------------------------------------
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

C-------------------------------------------------------------------------------
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


C-------------------------------------------------------------------------------
C X -- TABLEAU DE VALEURS DE LA VARIABLE INDEPENDANTE, PAR VALEURS
C      CROISSANTES
C Y -- TABLEAU DES VALEURS ASSOCIEES DE LA FONCTION A INTEGRER
C P -- TABLEAU DES VALEURS DE LA PRIMITIVE AUX POINTS X(I)
C N --
C PDEB -- VALEUR DE LA PRIMITIVE POUR X(1),PREMIERE VALEUR
C         DU TABLEAU
C
C METHODE: LA VALEUR DE L'INTEGRALE SUR L'INTERVALLE X(I), X(I+1)
C   EST CALCULEE PAR LA FORMULE DE SIMPSON, LA VALEUR DE Y AU POINT
C   MILIEU ETANT CALCULEE PAR INTERPOLATION CUBIQUE, PAR LA ROUTINE
C   AITK3
      SUBROUTINE INTEGRA(X,Y,P,N,PDEB)
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



C-------------------------------------------------------------------------------
      SUBROUTINE NAITK3(XdI,XdIp1,XdIp2,XdIp3,
     1                  YdI,YdIp1,YdIp2,YdIp3,XX,FX)
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



C-------------------------------------------------------------------------------
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


C-------------------------------------------------------------------------------
      SUBROUTINE TURBUL(INTERP,IVTOT,TOLV,main_VVT,NTOT,TOL,VT)
      DIMENSION main_VVT(20),TOLV(20),VT(50),TOL(50)
      PRINT *,'   ENTREE DS TURBUL'
      IF(IVTOT.EQ.1)   THEN
        WRITE(6,*) ' VT CONSTANT'
        DO N = 1, NTOT
          VT(N) = main_VVT(1)*1E5
        END DO
      ELSE
        WRITE(6,*) ' VT VARIABLE AVEC LA PROFONDEUR'
        WRITE(6,*) '     LOG TO'
        WRITE(6,101) (TOLV(I),I=1,IVTOT)
        WRITE(6,*) '     VT'
        WRITE(6,101) (main_VVT(I),I=1,IVTOT)
        IF(INTERP.EQ.1) CALL FTLIN3(IVTOT,TOLV,main_VVT,NTOT,TOL,VT)
          IF(INTERP.GT.1) CALL FT2(IVTOT,TOLV,main_VVT,NTOT,TOL,VT)
            NT2=NTOT-2
            DO N=1,NT2,3
              WRITE(6,102) N,TOL(N),VT(N),(N+1),TOL(N+1),VT(N+1),
     1                     (N+2),TOL(N+2),VT(N+2)
            END DO

            DO N = 1, NTOT
              VT(N) = VT(N)*1E5
            END DO
      END IF
C
      RETURN
100   FORMAT(I5)
101   FORMAT(10F8.3)
102   FORMAT(3(I5,2F8.3,5X))
      END
C

      FUNCTION CALCH(KII,IZ,KIEX1,IS1,KIEX2,IS2)
C     CALCUL DE CH VAN DER WAALS  APPROXIMATIONS D UNSOLD (1955)
C     SI IS1 ET IS2 SONT EGAUX A S P D F FORMULE 82-54  SINON 82-55
      REAL KII,KIEX1,KIEX2,NET1C,NET2C
      DIMENSION IS(4),IL(4)
      CHARACTER*1 IS, IBL
      CHARACTER*1 IS1, IS2
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

C ISSUE: it this ECRIT a flag to turn verbose on/off? If so, it is not being respected throughout!!!

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






C-------------------------------------------------------------------------------
C Contents of pkapgeralgrade.f


      SUBROUTINE KAPMOL(NH,TETA,NTOT)
C     KAPMOL GERAL, COM MOLECULAS NA SEGUINTE ORDEM:
C
C     MgH,C2,CN blue,red,nir,CH AX,BX,CX,13,CO nir,NH blue,OH blue,nir,FeH,Tio Gama,Gama linha,alfa,beta,delta,epsilon,phi
C     Todos os formatos foram transferidos para o final do arquivo.
C     Os labels das moleculas foram modificados para contarem sequencialmente a partir de 100.
c
c
C      CALCUL DE PNVJ ET GFM -
      PARAMETER(NM=50000,NTR=200)
      REAL*8 LMBDAM,LZERO,LFIN,LZERO1,LFIN1
      REAL NH,KB,JJ,MM,MMC
      DIMENSION NH(50),TETA(50),PE(50),PG(50),T5L(50),PPA(50),
     -PPH(50),PB(50),SJ(NM),PPC2(50),PMG(50),JJ(NM),TITM(20),
     -CSC(NM),GGV(NTR),BBV(NTR),DDV(NTR),PC13(50),PO(50),PTI(50),
     6 QQV(NTR),M(NTR),TITULO(20),FACT(NTR),LN(NTR),ITRANS(NM),
     7 PN(50),MBLENQ(NTR),NV(NTR),FMAT(20)
      DIMENSION PNG(50),PIG(50),pfe(50)
      DIMENSION LMBDAM(NM),GFM(NM),PNVJ(NM,50),ALARGM(NM)

      COMMON /KAPM1/ MM,MBLEND
      COMMON /TOTAL/ MMC,MBLENQ
      COMMON /OPTIM/ LZERO,LFIN
      COMMON /KAPM2/ LMBDAM,GFM,PNVJ,ALARGM
      COMMON /KAPM3/ PPH,PPC2,PN,PC13,PMG,PO,PTI,PNG,PIG,pfe

      DATA H/6.6252E-27/,C/2.997929E+10/,KB/1.38046E-16/,
     -CK/2.85474E-04/,C2/8.8525E-13/
       OPEN(UNIT=12,FILE='moleculagrade.dat',STATUS='OLD')
      NMOL_KAPMOL=1
      K=1
      I=1
      L=1
      READ(12,*) NUMBER
      WRITE(6,800) NUMBER
C      NUMBER = NUMBER OF MOLECULES
      READ(12,61) TITM
      WRITE(6,61) TITM
      LFIN1=LFIN+1.
      LZERO1=LZERO-1.
C     NV=N0 OF TRANSITIONS (V,V) FOR EACH MOLECULE
      READ(12,*) (NV(J),J=1,NUMBER)
cpc   WRITE(6,1001) (NV(J),J=1,NUMBER)
      J=1
      I1=1
 101    DO 3074 N=1,NTOT
      PPA(N)=PMG(N)
 3074   PB(N)=PPH(N)
      READ(12,61) TITULO
      WRITE(6,61) TITULO
      GO TO 999
  996 READ(12,61) TITULO
      WRITE(6,61) TITULO
 102  IF(NMOL_KAPMOL.NE.2) GO TO 103
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PPC2(N)
        END DO
      GO TO 999
 103  IF(NMOL_KAPMOL.NE.3) GO TO 104
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PN(N)
        END DO
      GO TO 999
 104  IF(NMOL_KAPMOL.NE.4) GO TO 105
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PN(N)
        END DO
      GO TO 999
 105  IF(NMOL_KAPMOL.NE.5) GO TO 106
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PN(N)
        END DO
        GO TO 999
 106    IF(NMOL_KAPMOL.NE.6) GO TO 107
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 107    IF(NMOL_KAPMOL.NE.7) GO TO 108
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 108    IF(NMOL_KAPMOL.NE.8) GO TO 109
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 109    IF(NMOL_KAPMOL.NE.9) GO TO 110
        DO N=1,NTOT
        PPA(N)=PC13(N)
        PB(N)=PPH(N)
        END DO
        GO TO 999
 110    IF(NMOL_KAPMOL.NE.10) GO TO 111
        DO N=1,NTOT
        PPA(N)=PPC2(N)
        PB(N)=PO(N)
        END DO
        GO TO 999
 111    IF(NMOL_KAPMOL.NE.11) go to 112
      DO  N=1,NTOT
      PPA(N)=PN(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 112    IF(NMOL_KAPMOL.NE.12) go to 113
      DO  N=1,NTOT
      PPA(N)=PO(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 113    IF(NMOL_KAPMOL.NE.13) go to 114
      DO  N=1,NTOT
      PPA(N)=PO(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 114    IF(NMOL_KAPMOL.NE.14) go to 115
      DO  N=1,NTOT
      PPA(N)=PFE(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 115    IF(NMOL_KAPMOL.NE.15) go to 116
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 116    IF(NMOL_KAPMOL.NE.16) go to 117
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 117    IF(NMOL_KAPMOL.NE.17) go to 118
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 118    IF(NMOL_KAPMOL.NE.18) go to 119
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 119    IF(NMOL_KAPMOL.NE.19) go to 120
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 120    IF(NMOL_KAPMOL.NE.20) go to 121
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 121    IF(NMOL_KAPMOL.NE.21) go to 23
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
  999       READ(12,*) FE,D0,MM,AM,BM,UA,UB,TE,CRO
cpc      WRITE(6,63) FE,TE,CRO,D0
      READ(12,52) ISE,A0,A1,A2,A3,A4,ALS
      NNV=NV(J)
      READ(12,*) S
      READ(12,*) (QQV(I),I=1,NNV)
cpc   WRITE(6,*) (QQV(I),I=1,NNV)
      READ(12,*) (GGV(I),I=1,NNV)
cpc   WRITE(6,*) (GGV(I),I=1,NNV)
      READ(12,*) (BBV(I),I=1,NNV)
cpc   WRITE(6,*) (BBV(I),I=1,NNV)
      READ(12,*) (DDV(I),I=1,NNV)
        DO I=1,NNV
        DDV(I)=1.E-6*DDV(I)
        END DO
cpc   WRITE(6,*) (DDV(I),I=1,NNV)
      READ(12,*) (FACT(I),I=1,NNV)
      IF(NMOL_KAPMOL.EQ.1) L=1
      IF(NMOL_KAPMOL.GE.2) L=MBLENQ(K-1)+1
C    EX.: GGV(I),I=1,2,3,4...   ITRANS=0,1,2,3....
      I=1
   15  READ(12,*) LMBDAM(L),SJ(L),JJ(L),  IZ,  NUMLIN
            IF((LMBDAM(L).GT.LFIN).OR.(LMBDAM(L).LT.LZERO))
     1      GO TO 200
      GO TO 201
  200 CONTINUE
      IF(NUMLIN.EQ.9) GO TO 16
      IF(NUMLIN.NE.0) GO TO 14
      GO TO 15
  201 CONTINUE
      IF(NUMLIN.NE.0) GO TO 14
      L=L+1
      GO TO 15
   14 IF(NUMLIN.EQ.9) GO TO 16
      LN(I)=L
      M(I)=L
      I=I+1
      L=L+1
      GO TO 15
   16 MBLEND=L
      MBLENQ(K)=L
      LN(I)=L
      M(I)=L
      I=I+1
       L=L+1
      KTEST=I
      GO TO 68
   69 CONTINUE
  778 CONTINUE
   68 CONTINUE
cpc   WRITE(6,1021) MBLEND
      RM=AM*BM/MM
      DO 4 N=1,NTOT
      PSI=D0*TETA(N)+2.5*ALOG10(TETA(N))-1.5*ALOG10(RM)-ALOG10
     1(UA*UB)-13.670
      PSI=10.**PSI
      DO 4 I=1,NNV
      MXX=LN(I)
      IF((I.EQ.1).AND.(K.EQ.1)) N1=1
       IF((I.EQ.1).AND.(K.GE.2)) N1=MBLENQ(K-1)+1
       IF(I.NE.1) N1=LN(I-1)+1
  280 QV=QQV(I)
      GV=GGV(I)
      BV=BBV(I)
      DV=DDV(I)
      DO 4 L=N1,MXX
      CSC(L)=EXP(-H*C/KB*TETA(N)/5040.*(TE+GV+BV*(JJ(L)+1)*JJ(L)))*(2.-
     1 CRO)*(2.*JJ(L)+1.)*
     2 EXP(H*C/KB*TETA(N)/5040.*(DV*(JJ(L)*(JJ(L)+1))**2+2.*BV))
    4 PNVJ(L,N)=CSC(L)*PSI*PPA(N)*PB(N)/PPH(N)
      GO TO 20
    3 DO 5 N=1,NTOT
      DO 5 L=1,MBLEND
      CSC(L)=EXP(-H*C/KB*TETA(N)/5040.*(TE+GV+BV*(JJ(L)+1)*JJ(L)))*(2.-
     1      CRO)
      UUA=UA+EXP(A0+A1*ALOG(TETA(N))+A2*(ALOG(TETA(N)))**2+A3*
     1      (ALOG(TETA(N)))**3+A4*(ALOG(TETA(N)))**4)
      PSI=D0*TETA(N)+2.5*ALOG10(TETA(N))-1.5*ALOG10(RM)-ALOG10
     1(UUA*UB)-13.670
      PSI=10.**PSI
    5 PNVJ(L,N)=CSC(L)*PSI*PPA(N)*PB(N)/PPH(N)
      GO TO 20
    6 IF(ISE.NE.0) GO TO 7
      DO 8 N=1,NTOT
      DO 8 L=1,MBLEND
   35 CSC(L)=EXP(-H*C/KB*TETA(N)/5040.*(TE+GV+BV*(JJ(L)+1)*JJ(L)))*(2.-
     X      CRO)*
     1    (2.*S+1)*EXP(-ALS *CK*TETA(N))/(1.+2.*COSH(ALS *CK*TETA(N)))*
     2(2.*JJ(L)+1.)*
     3       EXP(-H*C/KB*TETA(N)/5040.*(-DV*(JJ(L)*(JJ(L)+1.))**2))
      PSI=D0*TETA(N)+2.5*ALOG10(TETA(N))-1.5*ALOG10(RM)-ALOG10
     1(UA*UB)-13.670
      PSI=10.**PSI
    8 PNVJ(L,N)=CSC(L)*PSI*PPA(N)*PB(N)/PPH(N)
      GO TO 20
    7 DO 9 N=1,NTOT
      DO 9 L=1,MBLEND
      CSC(L)=EXP(-H*C/KB*TETA(N)/5040.*(TE+GV+BV*(JJ(L)+1)*JJ(L)))*(2.-
     X      CRO)
     1   *(2.*S+1)*EXP(-ALS *CK*TETA(N))/(1.+2.*COSH(ALS *CK*TETA(N)))
      UUA=UA+EXP(A0+A1*ALOG(TETA(N))+A2*(ALOG(TETA(N)))**2+
     X      A3*(ALOG(TETA
     1(N)))**3+A4*(ALOG(TETA(N)))**4)
      PSI=D0*TETA(N)+2.5*ALOG10(TETA(N))-1.5*ALOG10(RM)-ALOG10
     1(UUA*UB)-13.670
      PSI=10.**PSI
    9 PNVJ(L,N)=CSC(L)*PSI*PPA(N)*PB(N)/PPH(N)
      GO TO 20
    2 IF(ISE.NE.0) GO TO 10
      DO 11 N=1,NTOT
      DO 11 L=1,MBLEND
      CSC(L)=EXP(-H*C/KB*TETA(N)/5040.*(TE+GV+BV*(JJ(L)+1)*JJ(L)))*(2.-
     X      CRO)
     1      *(2.*S+1)*EXP(-2*ALS*CK*TETA(N))/(1.+2.*COSH(2*ALS*CK*TETA(N)))
      PSI=D0*TETA(N)+2.5*ALOG10(TETA(N))-1.5*ALOG10(RM)-ALOG10
     1(UA*UB)-13.670
      PSI=10.**PSI
   11 PNVJ(L,N)=CSC(L)*PSI*PPA(N)*PB(N)/PPH(N)
      GO TO 20
   10 DO 12 N=1,NTOT
      DO 12 L=1,MBLEND
      CSC(L)=EXP(-H*C/KB*TETA(N)/5040.*(TE+GV+BV*(JJ(L)+1)*JJ(L)))*(2.-
     X      CRO)
     1      *(2.*S+1)*EXP(-2*ALS*CK*TETA(N))/(1.+2.*COSH(2*ALS*CK*TETA(N)))
      UUA=UA+EXP(A0+A1*ALOG(TETA(N))+A2*(ALOG(TETA(N)))**2+
     X      A3*(ALOG(TETA
     1(N)))**3+A4*(ALOG(TETA(N)))**4)
      PSI=D0*TETA(N)+2.5*ALOG10(TETA(N))-1.5*ALOG10(RM)-ALOG10
     1(UUA*UB)-13.670
      PSI=10.**PSI
   12 PNVJ(L,N)=CSC(L)*PSI*PPA(N)*PB(N)/PPH(N)
   20 CONTINUE
      IF(NMOL_KAPMOL.GT.1) L=MBLENQ(K-1)+1
      IF(NMOL_KAPMOL.EQ.1) L=1
      I=1
   40 LL=ITRANS(L)
      QV=QQV(I)
      FACTO=FACT(I)
   21 GFM(L)=C2*((1.E-8*LMBDAM(L))**2)*FE*QV*SJ(L)*FACTO
      IF(L.EQ.MBLEND) GO TO 24
      IF(L.EQ.M(I)) GO TO 29
      L=L+1
      GO TO 21
   29 I=I+1
      L=L+1
      GO TO 40
cpc   24    PRINT 57
   24 L=L+1
      IF(NMOL_KAPMOL.LT.NUMBER) GO TO 994
      GO TO 23
  994 NMOL_KAPMOL=NMOL_KAPMOL+1
      K=K+1
      J=J+1
      GO TO 996
   23 CONTINUE
      DO 886 L=1,MBLEND
  886 ALARGM(L)=0.1
      REWIND 12
        RETURN
   50 FORMAT(7E11.4)
   51 FORMAT(F9.3,F7.3,F5.1,F5.2,I2,10X,I2,5X,I1)
   52 FORMAT(2X,I3,5F10.6,10X,F6.3)
   57 FORMAT(3X,'L',8X,'LAMBDA',10X,'SJ',10X,'GFM')
   58 FORMAT(I4,2X,2F12.3,2X,E12.3)
   59 FORMAT(10E12.3)
   60 FORMAT(3F10.3,3E10.3)
   61 FORMAT(20A4)
   62 FORMAT(E12.3,4F5.2,12X,2E10.3,E12.5,F3.0)
   63 FORMAT(20X,'FEL=',E10.3,2X,'TE=',E12.5,2X,'CRO=',F3.0,4X,'D0=',
     1F8.3)
c   64 FORMAT(20X,'FRANCK-CONDON=',F7.4,2X,'G(VSEC)=',E12.5,2X,
c     1     'B(VSEC)=',E12.5)
  300 FORMAT(7E11.4)
  500 FORMAT('   BIZARRE, BIZARRE')
  590 FORMAT(I3,2E10.3,2F6.3,F10.3)
  777 FORMAT(20A4)
  800 FORMAT('    NUMBER=',I3)
  883 FORMAT('   L=',I6,'  I=',I6,' M(I)=',I6,'LN=',I6)
 1001 FORMAT('     NV=',10I5)
 1020 FORMAT('  M(I)=',9(2X,I3))
 1021 FORMAT('   MBLEND=',I4)
      END
































































































C Subroutines SAT4 and DIE


      MODULE SAT4_DIE

C     ========
      CONTAINS
C     ========

C-------------------------------------------------------------------------------
C "SUBROUTINE D'EQUILIBRE DISSOCIATIF"
      SUBROUTINE SAT4(PPH,PPC2,PN,PC13,PMG,PO,PTI,PNG,pig,pfe,IT)
C
      REAL  IP,KP,KPLOG,IPI,NH,NNH
      REAL  CCOMP, UIIDUI, P, FP,dissoc_NELEMX
      REAL  dissoc_EPS,dissoc_SWITER, dissoc_C
      INTEGER dissoc_NIMAX, dissoc_NMETAL, dissoc_NMOL, dissoc_NATOM, dissoc_NELEM,dissoc_MMAX
        DIMENSION PPH(50),PPC2(50),
     1      PO(50),PTI(50),PMG(50),PC13(50),PN(50),
     2      PNG(50),pig(50),pfe(50)
        dimension dissoc_ELEMS(18)
      COMMON /COM8/   NNH(50), TETA(50), PPE(50), PGG(50), T5L(50)
      COMMON /COMFH1/ dissoc_C(600,5), dissoc_NELEM(5,600), dissoc_NATOM(5,600), dissoc_MMAX(600),
     1                PPMOL(600), APMLOG(600),dissoc_MOL(600), IP(100),
     2                CCOMP(100), UIIDUI(100), COMFH1_P(100), FP(100),
     3                KP(100),
     4                dissoc_NELEMX(50), dissoc_NIMAX, dissoc_EPS, dissoc_SWITER, dissoc_NMETAL, dissoc_NMOL
      COMMON /VAL/    PPG(600,50)


C ISSUE Careful with all this ELE<something> variables, this is messy
      CHARACTER*2 ELEMNT, ELEMXI, YA
      CHARACTER*3 dissoc_MOL
      DIMENSION   TO(50)
      DIMENSION YA(525), YB(525), YC(525), YD(525),ELEMNT(100),
     2      CCLOG(100),G0(100),G1(100),NATOMM(5),NELEMM(5),
     3      XP(50,20)
      DATA ELEMNT(99),ELEMNT(100)/'E-','H*'/
C
C
C*****IMPUT A

      AVO=0.602217E 24
      SPA=0.196E-01
      GRA=0.275423E 05
      AHE=0.100E 00
        IND = 1
      ECONST = 4.342945E-1


C (JT2015) This fragment is already adapted
      CALL READ_DISSOC('dissoc.dat')
C (JT2015) This fragment is already adapted
      ! Infers other variables from the metal rows of dissoc.dat
      DO I = 1, dissoc_NMETAL

          CCLOGI = dissoc__CCLOG(I)+main_AFSTAR
C ISSUE This is the thing that Beatriz mentioned that it is not used anymore, I think
          CCLOGI = CCLOGI+main_XXCOR(I)
          IF(I .EQ .1) CCLOGI = 0.0
          IF(I .EQ .2) CCLOGI = -1.0

          NELEMXI = dissoc_NELEMX(I)
          IG0I = dissoc__IG0(I)
          IG1I = dissoc__IG1(I)

          ELEMNT(NELEMXI) = dissoc_ELEMS(I)
          IP(NELEMI) = dissoc__IP(I)
          UIIDUI(NELEMXI) = IG1I * 0.661 / IG0I
          G0(NELEMXI) = IG0I
          G1(NELEMXI) = IG1I
          CCLOG(NELEMXI) = CCLOGI
          CCOMP(NELEMXI) = EXP(CCLOGI/ECONST)
          WRITE(*, '(1H ,5X,A4,8X,I5,3X, F10.3,5X, 2I5,3X,F10.5)')
     1          dissoc_ELEMS(I), NELEMXI, dissoc__IP(I),
                IG0I, IG1I, CCLOGI-main_AFSTAR
      END DO
C
C*****IMPUT D
      J = 0
 1010 J = J + 1
      READ(23,5011)dissoc_MOL(J),(dissoc_C(J,K),K=1,5)
     1   ,dissoc_MMAX(J),(NELEMM(M),NATOMM(M),M=1,4)
      print 5011, dissoc_MOL(J),(dissoc_C(J,K),K=1,5)
     1   ,dissoc_MMAX(J),(NELEMM(M),NATOMM(M),M=1,4)
      MMAXJ = dissoc_MMAX(J)
      IF(MMAXJ.EQ.0) GO TO 1014
      DO 1012 M=1,MMAXJ
      dissoc_NELEM(M,J) = NELEMM(M)
      dissoc_NATOM(M,J) = NATOMM(M)
 1012 CONTINUE
 1110 GO TO 1010
C     STARTING VALUE OF THE SOLUTION
 1014 dissoc_NMOL = J - 1
      DO 1400 I=1,dissoc_NMETAL
      NELEMI=dissoc_NELEMX(I)
      P(NELEMI)=1.0E-20
 1400 CONTINUE
      P(99)=1.0E-10
C
C*****IMPUT E
      DO 1020  ITO=1,IT
 1023 THETA=TETA(ITO)
      TEM=5040.0/THETA
 1024 PG=PGG(ITO)
      PGLOG=ALOG10(PG)
      NH=NNH(ITO)
      TO(ITO)=10.**T5L(ITO)
C

      CALL DIE(TEM,PG)
      PE=P(99)
      PELOG=ALOG10(PE)
C
C     PRINT OUT OF THE RESULTS
C        PRINT 6300
C     PRINT 6091,   PGLOG, PELOG,PE, THETA, TEM, TO(ITO)
C     PRINT 6301
      DO 1303 I=1,dissoc_NMETAL
      NELEMI=dissoc_NELEMX(I)
      FPLOG=ALOG10(FP(NELEMI))
      XP(ITO,I) = P(NELEMI)+1.0E-30
      PLOG = ALOG10( XP(ITO,I) )
      PDFPL = PLOG - FPLOG
C     WRITE(6,6302) ELEMNT(NELEMI),NELEMI,CCLOG(NELEMI),FP(NELEMI),
C    1FPLOG,PLOG,PDFPL
      IF(MOD(I,5)) 1303,1304,1303
 1304 CONTINUE
C1304    PRINT 6305
 1303 CONTINUE
C        PRINT 6307
C     PRINT 6031, (DD(K),K=1,20)
 1231 CONTINUE
C1231 WRITE(6,6091) PGLOG,PELOG,PE,THETA,TEM, TO(ITO)
C     PUNCH 710,NH,THETA,PE
C 710 FORMAT(2X,E11.5,2X,F6.4,2X,E11.5,46X)
C        PRINT 6992
      IRL = 120
      DO 1184 I=1,dissoc_NMETAL
      NELEMI=dissoc_NELEMX(I)
      YA(I)  =  ELEMNT(NELEMI)
      PLOG=ALOG10(P(NELEMI)+1.0E-30)
      KPLOG=ALOG10(KP(NELEMI)+1.0E-30)
      YD(I)  =  KPLOG
      PIONL = PLOG + KPLOG - PELOG
      YB(I) = PIONL
      XLOG = PIONL - PGLOG
      YC(I)  =  XLOG
C
      IF (  I.NE.dissoc_NMETAL )  GO TO 1450
       IQ = I / 120
      IR  =  dissoc_NMETAL  -  IQ * 120
      IRL  =  IR / 3
      GO TO 1460
 1450 IF ( MOD(I,120) )  1184,1460,1184
C
 1460 NBL = 0
      DO 1470  K1=1,120,3
      NBL = NBL + 1
      K2 = K1 + 1
      K3 = K1 + 2
      IF ( NBL.EQ.IRL + 1)  GO TO 1480
 1475 CONTINUE
C1475 PRINT 6495, ( YA(K1), YB(K1), YC(K1), YD(K1),
C    2              YA(K2), YB(K2), YC(K2), YD(K2),
C    3              YA(K3), YB(K3), YC(K3), YD(K3) )
      IF ( MOD(NBL,5) ) 1470,1500,1470
 1500 CONTINUE
C1500    PRINT 6089
 1470 CONTINUE
      GO TO 1184
 1480 IRR  =  IR  -  IRL * 3
      IF ( IRR.EQ.0 )  GO TO 1184
      GO TO (1482,1484), IRR
 1482 CONTINUE
C1482 PRINT 6495, ( YA(K1), YB(K1), YC(K1), YD(K1) )
      GO TO 1184
 1484 CONTINUE
C1484 PRINT 6495, ( YA(K1), YB(K1), YC(K1), YD(K1),
C    2              YA(K2), YB(K2), YC(K2), YD(K2) )
 1184 CONTINUE
C
      IRL = 120
      KD =-119
      DO 1084 J=1,dissoc_NMOL
         YA(J)  =  dissoc_MOL(J)
      JCOUNT = JCOUNT + 1
      PMOLL=ALOG10(PPMOL(J)+1.0E-30)
         YB(J) = PMOLL
      XLOG = PMOLL - PGLOG
         YC(J)  =  XLOG
      PPG(J,ITO)  =  XLOG
          YD(J)  =  APMLOG(J)
C
      IF ( J.NE.dissoc_NMOL ) GO TO 2450
       IQ = J / 120
      IR  =  dissoc_NMOL    -  IQ * 120
      IRL  =  IR / 3
      GO TO 2460
 2450 IF ( MOD(J,120) )  2184,2460,2184
 2460 NBL = 0
C        PRINT 6092
      KD = KD + 120
      KF = KD + 119
      DO 2470  K1=KD,KF,3
      NBL = NBL + 1
      K2 = K1 + 1
      K3 = K1 + 2
      IF ( NBL.EQ.IRL + 1)  GO TO 2480
 2475 CONTINUE
C2475 PRINT 6496, ( YA(K1), YB(K1), YC(K1), YD(K1),
C    2              YA(K2), YB(K2), YC(K2), YD(K2),
C    3              YA(K3), YB(K3), YC(K3), YD(K3) )
      IF ( MOD(NBL,5) ) 2470,2500,2470
 2500 CONTINUE
C2500  PRINT 6089
 2470 CONTINUE
      GO TO 2184
 2480 IRR  =  IR  -  IRL * 3
      IF ( IRR.EQ.0 )  GO TO 2184
      GO TO (2482,2484), IRR
 2482 CONTINUE
C2482 PRINT 6496, ( YA(K1), YB(K1), YC(K1), YD(K1) )
      GO TO 2184
 2484 CONTINUE
C2484 PRINT 6496, ( YA(K1), YB(K1), YC(K1), YD(K1),
C    2              YA(K2), YB(K2), YC(K2), YD(K2) )
 2184 CONTINUE
 1084 CONTINUE
C     PRINT 6600
C
 1020  CONTINUE
cpc      WRITE(6,100) (TO(ITO),ITO=1,IT)
  100 FORMAT(' TO=',5E15.5)
C
C1111  PUNCH 700,   atomgrade_ELEM(I),( XP(ITX,I),ITX=1,IT )
      DO 1111 I=1,4
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
 1111 PRINT 50, (XP(ITX,I),ITX=1,IT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
   50 FORMAT(7E11.4)
cpc      write(40,*) IT
      DO 51 ITX=1,IT
      PPH(ITX)=XP(ITX,1)
       PPC2(ITX)=XP(ITX,3)
cpc        write(40,300) PPC2(ITX)
       PN(ITX)=XP(ITX,4)
       PO(ITX)=XP(ITX,5)
       PC13(ITX)=XP(ITX,6)
          PTI(ITX)=XP(ITX,15)
C        write(90,300) PTI(ITX)
        PMG(ITX)=XP(ITX,8)
        PNG(ITX)=XP(ITX,9)
        PIG(ITX)=XP(ITX,10)
        pfe(ITX)=XP(ITX,16)
   51  CONTINUE
 1100  CONTINUE
  300  Format('  PPC2 SAT4 ', E11.3)
C   FORMATS
C
  700  FORMAT ( A3,7E11.4,/(7E11.4) )
  701 FORMAT(1X,A3,7E11.4,/(7E11.4))
 500  FORMAT (20I5)
  501  FORMAT (I4)

 5011 FORMAT(A3,5X,E11.5,4E12.5,1X,I1,4(I2,I1) )
 5021 FORMAT (F10.3,E12.5,E12.6)
 5030 FORMAT(20A4)
C
 6031 FORMAT(1H1,20A4/)
c 6091 FORMAT (/,10X,'LOG PG =',F8.4,20X,'LOG PE =',F8.4,20X,'PE =',E13.
c     2     6/,10X,'TETA=',F8.4,20X,'TEMP=',F8.0,20X,'T0=',E14.6/)
 6102 FORMAT(1H0, 61H ELEMENT    ATOMIC NUMBER     I.P.        G(0)   G(
     11)   LOG N/)

 6300 FORMAT(1H0, 51H EQUILIBRIUM PARTIAL PRESSURES OF THE GASEOUS ATOM
     1///)
c 6301 FORMAT(1H0,'ELEMENT',3X,'LOG N(E)/N(H)',4X,'P(E)',6X,'LOG P(E)',2
c     1     X,'LOG P(A)',2X,'LOG P(A)/P(E)'/)
 6302 FORMAT(1H ,1X,A4,1X,I2,4X,F10.3,1X,E11.4,2F10.3,4X,F10.3)
 6305 FORMAT(1H )
 6307 FORMAT('   P(E) 8888 FICTITIUS PRESSURE OF THE
     1      NUCLEUS OF THE ELEMENT',/'P(A) **** PARTIAL PRESSURE OF THE
     2      MONOATOMIC GAS OF THE ELEMENT')
 6089 FORMAT (1H )
 6092 FORMAT (1H1,3(5X,'MOLECULE  LOG P   LOG P/PG  LOG KP  ')//)
 6495 FORMAT ( 3(8X,A4,'+',3F9.3) )
 6496 FORMAT ( 3(9X,A4,3F9.3) )
 6600 FORMAT (1H1)
 6992 FORMAT (///,3(9X,'ION   LOG P   LOG P/PG  LOG KP ')//)
      RETURN
      END


C*****DIE9

C-------------------------------------------------------------------------------
      SUBROUTINE DIE(TEM,PG)
      REAL  IP,KP,KPLOG,IPI,NH,NNH
      REAL  CCOMP, UIIDUI, P, FP,dissoc_NELEMX
      REAL  dissoc_EPS,dissoc_SWITER, dissoc_C
      INTEGER dissoc_NIMAX, dissoc_NMETAL, dissoc_NMOL, dissoc_NATOM, dissoc_NELEM,dissoc_MMAX

      COMMON /COM8/   NNH(50), TETA(50), PPE(50), PGG(50), T5L(50)
      COMMON /COMFH1/ dissoc_C(600,5), dissoc_NELEM(5,600), dissoc_NATOM(5,600), dissoc_MMAX(600),
                      PPMOL(600), APMLOG(600), dissoc_MOL(600), IP(100),
                      CCOMP(100), UIIDUI(100), P(100), FP(100), KP(100),
                      dissoc_NELEMX(50), dissoc_NIMAX, dissoc_EPS, dissoc_SWITER, dissoc_NMETAL, dissoc_NMOL

      CHARACTER*3 dissoc_MOL
      DIMENSION FX(100),DFX(100),Z(100),PREV(100),WA(50)
      ECONST = 4.342945E-1
      EPSDIE=5.0E-3
      T=5040.0/TEM
      PGLOG=ALOG10(PG)
C
C     HEH=HELIUM TO HYDROGEN RATIO BY NUMBER
      HEH=CCOMP(2)/CCOMP(1)
C
C     EVALUATION OF LOG KP(MOL)
      DO 1025 J =1, dissoc_NMOL
      APLOGJ = dissoc_C(J,5)
      DO 1026 K=1,4
      KM5=5-K
      APLOGJ = APLOGJ*T     + dissoc_C(J,KM5)
 1026 CONTINUE
      APMLOG(J) = APLOGJ
 1025 CONTINUE
      DHH = (((0.1196952E-02*T-0.2125713E-01)*T+0.1545253E+00)*
     1  (-0.5161452E+01))*T+0.1277356E+02
      DHH=EXP(DHH/ECONST)
C
C     EVALUATION OF THE IONIZATION CONSTANTS
      TEM25 = TEM**2*SQRT(TEM)
      DO 1060 I = 1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      KP(NELEMI) =UIIDUI(NELEMI)*TEM25*EXP(-IP(NELEMI)*T/ECONST)
 1060 CONTINUE
      HKP=KP(1)
      IF(T-0.6) 1084,1072,1072
C
C     PRELIMINARY VALUE OF PH AT HIGH TEMPERATURES
 1084 PPH=SQRT(HKP *(PG/(1.0+HEH)+HKP ))-HKP
      PH=PPH**2/HKP
      GO TO 1102
C
C     PRELIMINARY VALUE OF PH AT LOW TEMPERATURES
 1072 IF(PG/DHH-  0.1 ) 1073,1073,1074
 1073 PH=PG/(1.0+HEH)
      GO TO 1102
 1074 PH=0.5*(SQRT(DHH*(DHH+4.0*PG/(1.0+HEH)))-DHH)
C
C     EVALUATION OF THE FICTITIOUS PRESSURES OF HYDROGEN
C     PG=PH+PHH+2.0*PPH+HEH*(PH+2.0*PHH+PPH)
 1102 U=(1.0+2.0*HEH)/DHH
      Q=1.0+HEH
      R=(2.0+HEH)*SQRT(HKP )
      S=-1.0*PG
      X=SQRT(PH)
      ITERAT=0
 1103 F=((U*X**2+Q)*X+R)*X+S
      DF=2.0*(2.0*U*X**2+Q)*X+R
      XR=X-F/DF
      IF(ABS((X-XR)/XR)-EPSDIE) 1105,1105,1106
 1106 ITERAT=ITERAT+1
      IF(ITERAT-50) 1104,1104,1107
 1107   PRINT 6108, TEM,PG,X,XR,PH
 6108 FORMAT(1H1,'NOT CONVERGE IN DIE  TEM=', F9.2, 5X, 'PG=', E12.5, 5X
     1'X1=', E12.5, 5X,'X2=', E12.5, 5X, 'PH=', E12.5)
      GO TO 1105
 1104 X=XR
      GO TO 1103
 1105 PH=XR**2
      PHH=PH**2/DHH
      PPH=SQRT(HKP *PH)
      FPH  =PH+2.0*PHH+PPH
C      PRINT 6109,  TEM,T,PG,FPH,PH
c 6109      FORMAT(1X,'TEM=',F10.2,10X,'THETA=',F8.4
c     1     ,10X,'PG=',E13.5,10X,'FPH',E12.3,10X,'PH=',E12.3)
C     P(100)=PH+
      P(100)=PPH
C
C     EVALUATION OF THE FICTITIOUSPRESSURE OF EACH ELEMENT
      DO 1070 I=1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      FP(NELEMI) = CCOMP(NELEMI)*FPH
 1070 CONTINUE
C
C     CHECK OF INITIALIZATION
      PE=P(99)
      IF(PH-P(1)) 1402,1402,1401
 1401 DO 1403 I=1,dissoc_NMETAL
      NELEMI=dissoc_NELEMX(I)
      P(NELEMI)=FP(NELEMI)*EXP(-5.0*T/ECONST)
 1403 CONTINUE
      P(1)=PH
C
C     RUSSELL EQUATIONS
 1402 CONTINUE
 6003 FORMAT(1H0)
      NITER = 0
 1040 DO 1030 I =1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      FX(NELEMI) = -FP(NELEMI) + P(NELEMI)*(1.0 + KP(NELEMI)/PE)
      DFX(NELEMI) = 1.0 + KP(NELEMI)/PE
 1030 CONTINUE
      SPNION=0.0
      DO 1041 J=1,dissoc_NMOL
      MMAXJ = dissoc_MMAX(J)
      PMOLJL=-APMLOG(J)
      DO 1042 M =1,MMAXJ
      NELEMJ = dissoc_NELEM(M,J)
      NATOMJ = dissoc_NATOM(M,J)
      PMOLJL = PMOLJL + FLOAT(NATOMJ)*ALOG10(P(NELEMJ))
 1042 CONTINUE
      IF(PMOLJL - (PGLOG+1.0) ) 1046,1046,1047
 1047 DO 1048 M =1,MMAXJ
      NELEMJ = dissoc_NELEM(M,J)
      NATOMJ = dissoc_NATOM(M,J)
      P(NELEMJ)=1.0E-2*P(NELEMJ)
      PMOLJL = PMOLJL + FLOAT(NATOMJ)*(-2.0)
 1048 CONTINUE
 1046 PMOLJ = EXP(PMOLJL/ECONST)
      DO 1044 M =1,MMAXJ
      NELEMJ = dissoc_NELEM(M,J)
      NATOMJ = dissoc_NATOM(M,J)
      ATOMJ = FLOAT(NATOMJ)
      IF(NELEMJ.EQ.99) SPNION=SPNION + PMOLJ
      DO 1043 I=1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      IF(NELEMJ.EQ.NELEMI) GO TO 1045
      GO TO 1043
 1045 FX(NELEMI) = FX(NELEMI) + ATOMJ*PMOLJ
      DFX(NELEMI) = DFX(NELEMI) + ATOMJ**2*PMOLJ/P(NELEMI)
 1043 CONTINUE
 1044 CONTINUE
      PPMOL(J)= PMOLJ
 1041 CONTINUE
C
C     SOLUTION OF THE RUSSELL EQUATIONS BY NEWTON-RAPHSON METHOD
      DO 2001 I=1,dissoc_NMETAL
      NELEMI=dissoc_NELEMX(I)
      WA(I)=ALOG10(P(NELEMI)+1.0E-30)
 2001 CONTINUE
      IMAXP1=dissoc_NMETAL+1
      WA(IMAXP1)=ALOG10(PE+1.0E-30)
      DELTA = 0.0
      DO 1050 I=1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      PREV(NELEMI) = P(NELEMI) - FX(NELEMI)/DFX(NELEMI)
      PREV(NELEMI) = ABS(PREV(NELEMI))
      IF(PREV(NELEMI).LT.1.0E-30)PREV(NELEMI)=1.0E-30
      Z(NELEMI) = PREV(NELEMI)/P(NELEMI)
      DELTA = DELTA + ABS(Z(NELEMI) - 1.0)
      IF(dissoc_SWITER) 2500,2500,2501
 2501 P(NELEMI) = (PREV(NELEMI) + P(NELEMI) )*0.5
      GO TO 1050
 2500 P(NELEMI)=PREV(NELEMI)
 1050 CONTINUE
C     IONIZATION EQUILIBRIUM
      PEREV = 0.0
      DO 1061 I=1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      PEREV = PEREV + KP(NELEMI)*P(NELEMI)
 1061 CONTINUE
      PEREV=SQRT( PEREV/(1.0+SPNION/PE) )
      DELTA = DELTA + ABS((PE-PEREV)/PE)
      PE =(PEREV + PE )*0.5
      P(99)=PE
      IF(DELTA - dissoc_EPS) 1051,1051,1052
 1052 NITER = NITER + 1
      IF(NITER-dissoc_NIMAX)1040,1040,1054
 1054    PRINT 6055,  dissoc_NIMAX
 6055 FORMAT(1H0,39H *DOES NOT CONVERGE AFTER ITERATIONS OF,I4/////)
C     ATOMIC NUMBER 99 = ELECTRON
 1051 RETURN
      END

C-------------------------------------------------------------------------------
      FUNCTION MINI(IFA,NTOT,IA,IZ)
      DIMENSION IFA(NTOT)
      MINI=IFA(IA)
      IA2=IA+1
      DO I=IA2,IZ
        IF(IFA(I).LT.MINI) THEN
          MINI=IFA(I)
        END IF
      END DO
      RETURN
      END


      END MODULE SAT4_DIE

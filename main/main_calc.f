
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
c     b. dimensao e data de LLHY e dimensao de main_FILETOHY foram
c     atualizadas
c
c     c. incluidos c_filetoh_TAUHI(NP,50),TAUHY(10,NP,50) e excluido IHH(500)
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
c        SELEKFH : TTD(NP), KCD(NP, 50), FL(NP), TAUH(NP,50)`
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

      PARAMETER(PARAMETER_NMOL=50000,NP=7000,NT=10000)



      ! TODO organize this once I have a module
      LOGICAL config_VERBOSE
      config_VERBOSE = .FALSE.


C main, KAPMOL



      !====
      ! Making a new declaration section
      !====
      REAL*8 LZERO, LFIN
      CHARACTER*256 FILEFLUX, FILEFLUX2,FILEFLUX3




      INTEGER FINPAR,FINRAI,FINAB,D,DTOT
      INTEGER DHM,DHP,DHMY,DHPY
      CHARACTER FILETOH*20
      
      
      CHARACTER tti*2,mgg*2,oo1*2,cc1*2,nn1*2
      character oo2*2,cc2*2,nn2*2
      REAL KB,KC,LAMBD,
     1   KC1,KC2,KCD,km_MM, ABOND, ABO
      LOGICAL GAUSS,IDENTH
      REAL*8 LZERO,LFIN,LLHY(10)
      REAL*8 ECART,ECARTM,L0,LF,lllhy
      DIMENSION
     1 KC(50),ALPH(50),PHN(50),PH2(50),
     2 KC1(50),KC2(50),KCD(NP,50),
     3 DELTA(MAX_atomgrade_NBLEND,50),ABOND(MAX_atomgrade_NBLEND),
     5 POP(MAX_atomgrade_NBLEND,50),A(MAX_atomgrade_NBLEND,50),GFAL(MAX_atomgrade_NBLEND),ECART(MAX_atomgrade_NBLEND),
     6 CORCH(MAX_atomgrade_NBLEND),CVdW(MAX_atomgrade_NBLEND),
     7 FI(1501),TFI(1501),
     8 ECARTM(PARAMETER_NMOL)
      DIMENSION VT(50),TOLV(20)
C     fonctions de partition

C ISSUE: I think this 50 should be MAX_partit_KMAX... GOtta check all these variables, what they sync with!
      DIMENSION P(3,MAX_partit_NPAR, 50)
      DIMENSION ABO(100)  ! ISSUE: Must match dimension of abonds_ELE. Change to maxNABOND later
      DIMENSION B(0:50),B1(0:50),B2(0:50)
      DIMENSION FL(NP), TTD(NP), FCONT(NP), FN(NP)
      DIMENSION TAUH(NP,50),TAUHY(10,NP,50)
      DIMENSION DHMY(10),DHPY(10)
C


      ! TODO command-line option to select this integration; config
      KIK=0 ! FORMULE A 6 OU 7 PTS POUR CALCUL FLUX OU INT


C  *****************************************************************
      DATA LLHY /3750.150, 3770.630, 3797.900, 3835.390, 3889.050,
     +           3970.076, 4101.748, 4340.468, 4861.332, 6562.817/
      DATA C/2.997929E+10/, H/6.6252E-27/,KB/1.38046E-16/,R/8.3170E+7/,
     1     PI/3.141593/,C1/4.8298E+15/,C2/8.8525E-13/,C4/2.1179E+8/,
     2     C6/3.76727E+11/,DEUXR/1.6634E+8/,C7/1.772453/
        C5= 2.*PI* (3.*PI**2/2.44)**0.4


      !~!~!~!~!~OPEN(UNIT=4,FILE='main.dat',STATUS='OLD')
      !~!~!~!~!~OPEN(UNIT=25,FILE='partit.dat',STATUS='OLD')
      !~!~!~!~!~OPEN(UNIT=23,FILE='dissoc.dat',STATUS='OLD')
      !~!~!~!~!~OPEN(UNIT=30,FILE='abonds.dat',STATUS='OLD')
      !~!~!~!~!~OPEN(UNIT=14,FILE='atomgrade.dat',STATUS='OLD')



      ! ISSUE what is this?
      RPI = 1.77245385



      ! dissoc.dat needs to be read first because READ_MAIN() depends on dissoc_NMETAL
      CALL READ_DISSOC(filename_DISSOC)


      CALL READ_MAIN(filename_MAIN)

      !---
      ! Variable values derived directly from information in main.dat
      !---

      ! ISSUE: Is this right, I think it has something to do with the sun, no??
      ASASOL = 10.**main_ASALOG  ! This was in READER06, but depends directly from something read from main.dat
      TETAEF = 5040/main_TEFF    ! This was in READER06, but depends directly from something read from main.dat
      FSTAR  = 10**main_AFSTAR   ! ISSUE This was further below

      ! ISSUE: breaking rule!!! this cannot happen: change value of this global here, check what is supposed to happen instead.
      FILEFLUX1 = 'spec.'//main_FILEFLUX
      FILEFLUX2 = 'cont.'//main_FILEFLUX
      FILEFLUX3 = 'norm.'//main_FILEFLUX




C                       I
C                 INITIALISATIONS DIVERSES
C         LECTURE ET CALCUL  DE LA FONCTION DE CONVOLUTION
C  *****************************************************************



C  ****************************************************************
C                       II
C           1-   LECTURE DES FCTS DE PARTITION
C           2-   LECTURE DES DONNEES ABSORPTION CONTINUE
C           3-   LECTURE DU MODELE
C  ****************************************************************

      ! 1) LECTURE DES FCTS DE PARTITION
      CALL READ_PARTIT(filename_PARTIT)

      ! 2) LECTURE DES DONNEES ABSORPTION CONTINUE
      CALL READ_ABSORU2(filename_ABSORU2)


      ! ISSUE What is this doing here?
      A0=AMET

      ! 3) LECTURE DU MODELE
      CALL READ_MODELE(filename_MODELES)

      BHE = modeles_NHE
      AMET=A0*ASASOL


      ! Reads abonds.dat
      CALL READ_ABONDS(config_fn_ABONDS)



      ! ISSUE: this is used, shouldn't it be some command-line option???
      INTERP = 1  ! interp. lineaire de vt (si parabolique 2)

      CALL TURBUL(INTERP,IVTOT,TOLV,main_VVT,modeles_NTOT,modeles_T5L,VT)
      IF(IVTOT .EQ. 1) THEN
        WRITE(6,131) main_VVT(1)
      ELSE
        WRITE(6,132)
      END IF

C  *****************************************************************
C                       III
C     CALCUL DE QUANT  NE DEPENDANT QUE DU METAL ET DU MODELE
C           POPULATION DU NIV FOND DES IONS
C  *****************************************************************
      CALL POPUL(modeles_TETA,modeles_PE,modeles_NTOT,partit_TINI,partit_PA,partit_JKMAX,partit_KI1,partit_KI2,partit_NPAR,partit_TABU,P)
C
C  *****************************************************************
C                       IV
C           CALCUL DES QUANTITES NE DEPENDANT QUE DU
C           MODELE ET DE LAMBDA : B(N)   KC(N)   FC
C  *****************************************************************

      CALL SAT4()

      DO K = 1, dissoc_NMETAL
        DO J=1,abonds_NABOND
          ! ISSUE: This is the thing that Beatriz mentioned that is not used anymore
          IF(abonds_ELE(J).EQ.dissoc_ELEMS(K)) THEN
            ! ISSUE: breaking rule: changing variable filled in READ_*() not allowed!!!
            abonds_ABOL(J) = abonds_ABOL(J)+main_XXCOR(K)
          END IF
        END DO
      END DO

      DO J=1,abonds_NABOND
        ABO(J) = 10.**(abonds_ABOL(J)-12.)
        ABO(J) = ABO(J)*FSTAR
      END DO



      OPEN(UNIT=17,FILE=FILEFLUX1,STATUS='unknown')
      OPEN(UNIT=19,FILE=FILEFLUX2,STATUS='unknown')
      OPEN(UNIT=20,FILE=FILEFLUX3,STATUS='unknown')


      IK=1
      IK2=1
      IK3=1
C     AINT =intervalle de calcul
C     CAINT=intervalle de recouvremment des intervalles
C     HINT =demi-intervalle de calcul des raies d'hydrogene
      HINT=35.
      CINT=20.
      IDENTH=.FALSE.


      ! ISSUE Explain what it does
      XLZERO = main_LLZERO-20.
      XLFIN = XLZERO+main_AINT+20.
      IF(XLFIN .GE. (main_LLFIN+20.)) THEN
        IKEYTOT = 1
      ELSE
        ! ISSUE it seems that I could write this using a modulus operator
        DO I = 2,250
          XLFIN = XLFIN+main_AINT
          IF(XLFIN .GE. (main_LLFIN+20.)) EXIT
        END DO
        IKEYTOT = I
      END IF

      LZERO = main_LLZERO-20.
      LFIN = LZERO+main_AINT+20.
      IKEY = 1





      ! =========
      ! Main loop
      ! =========
      DO WHILE .T. !Main loop!
C
        DTOT = (LFIN-LZERO)/main_PAS + 1.0005
        WRITE(6, 117) LZERO, LFIN, DTOT
        IF(DTOT .GT. 40000) THEN
          ! ISSUE: replace with EXIT statement
          ! TODO Make a more elegant exit
          STOP  !Main loop exit door!
        END IF

        LAMBD = (LZERO+LFIN)/2
        ILZERO = (LZERO/100.)*1E2
        ALZERO = LZERO -ILZERO
C
        DO D = 1,DTOT
          TTD(D) = ALZERO+main_PAS*(D-1)
        END DO

        CALL BK(modeles_NH,modeles_TETA,modeles_PE,modeles_PG,modeles_NTOT,LAMBD,B,B1,B2,ALPH,PHN,PH2,
     1        FC,KC,KC1,KC2,KCD,TTD,DTOT,main_PTDISK,main_MU,KIK,LZERO,LFIN)

        IF config_VERBOSE WRITE(6,501) main_LLZERO,main_LLFIN,LZERO,LFIN,LAMBD

        ! ******************************************************************
        ! LECTURE TAU RAIE HYDROGENE ET INTERPOLATION DE TAUH
        !
        ! Type *,' nom des fichiers TAU raies Hydrogene'

        IM = 0
        DO IH = 1,10
          ALLHY = LLHY(IH)-LZERO
          LLLHY = LLHY(IH)
          IF (((ALLHY .GT. 0) .AND. (ALLHY .LE. (main_AINT+55.))) .OR.
     1        ((ALLHY .LT. 0.) .AND. (ALLHY .GE. (-35.)))) THEN
            IM = IM+1
            IRH = 1
            IHT = IH
            FILETOH = main_FILETOHY(IHT)

            !--verbose--!
            IF config_VERBOSE WRITE(6,712) IM, LLHY(IH), FILETOH, IHT
            !-----------!

            ! ISSUE Extract this from main loop. Not too hard: c_filetoh_* just need one extra dimension
            CALL READ_FILETOH(FILETOH)
            CALL FILETOH_AUH(DTOT,TTD, ILZERO)

            DHMY(IM) = c_filetoh_DHMI
            DHPY(IM) = c_filetoh_DHPI
            DO N = 1,modeles_NTOT
               DO D = 1,DTOT
               TAUHY(IM, D, N) = c_filetoh_TAUHI(D,N)
               END DO
            END DO
          END IF
        END DO

        IMY = IM
        IF(IMY .NE. 0) THEN
          IF config_VERBOSE THEN
            WRITE(6,*) (DHMY(IM), IM=1,IMY)
            WRITE(6,*) (DHPY(IM), IM=1,IMY)
          END IF

          DHP = MAXI(DHPY, IMY, 1, IMY)
          DHM = MINI(DHMY, IMY, 1, IMY)
          DO N = 1,modeles_NTOT
            DO D = 1,DTOT
              TAUH(D,N) = 0.0
            END DO
          END DO

          DO N = 1,modeles_NTOT
            DO D = 1,DTOT
              DO IM = 1,IMY
                TAUH(D,N) = TAUH(D,N)+TAUHY(IM,D,N)
              END DO
            END DO
          END DO
        ELSE
          IRH=0
          DHM=0
          DHP=0
        END IF



        ! ******************************************************************
        ! -- V --
        ! QUANTITES DEPENDANT DE LA RAIE ET DU MODELE
        ! ******************************************************************
        CALL FILTER_ATOMGRADE(LZERO, LFIN)

        IF(atomgrade_NBLEND .GT. 0) THEN
          CALL POPADELH (partit_NPAR,partit_EL,partit_KI1,partit_KI2,partit_M,atomgrade_NBLEND,atomgrade_ELEM,
     1     atomgrade_LAMBDA,atomgrade_KIEX,atomgrade_CH,CORCH,CVdW,atomgrade_GR,atomgrade_GE,atomgrade_IONI,modeles_NTOT,modeles_TETA,modeles_PE,ALPH,
     2     PHN,PH2,VT,P,POP,A,DELTA)

          CALL ABONDRAIH(abonds_ELE,ABO,abonds_NABOND,atomgrade_ELEM,ABOND,atomgrade_NBLEND)

          ! *************************************************************
          !                       VI
          !     CALCUL DU COEFFICIENT D ABSORPTION SELECTIF
          !     ET CALCUL DU SPECTRE
          !  ***************************************************************
          DO K = 1,atomgrade_NBLEND
            GFAL(K) = atomgrade_GF(K)*C2*(atomgrade_LAMBDA(K)*1.E-8)**2
            ECART(K)= atomgrade_LAMBDA(K)-LZERO+main_PAS
          END DO
        END IF

        CALL KAPMOL(modeles_NTOT)

        IF config_VERBOSE WRITE (6,704) km_MBLEND

        DO L = 1, km_MBLEND
          ECARTM(L) = km_LMBDAM(L)-LZERO + main_PAS
        END DO

        CALL SELEKFH(main_PTDISK,main_MU,KIK,DTOT,main_PAS,atomgrade_NBLEND,GFAL,atomgrade_ZINF,
     1   ABOND,ECART,atomgrade_ELEM,atomgrade_LAMBDA,TAUH,DHM,DHP,VT,modeles_NTOT,modeles_NH,modeles_TETA,B,
     2   B1,B2,KCD,POP,DELTA,A,TTD,FL,FCONT, ECARTM, atomgrade_NBLEND)


        CALL WRITE_LINES_PFANT(filename_LINES_PFANT)


        AMG = main_XXCOR(8) ! I think this is just for debugging purposes
        LI = 10./main_PAS
        I1 = LI+1
        I2 = DTOT - LI
        IF (LFIN .GE. (main_LLFIN+20.)) THEN
          I2 = (main_LLFIN+10.-LZERO)/main_PAS + 1.0005
        END IF
        ITOT=I2-I1+1

        DO D=I1,I2
          FL(D) = FL(D)*(10.**5)
          FCONT(D) = FCONT(D)*(10.**5)
          FN(D) = FL(D) / FCONT(D)
        END DO
        L0 = main_LLZERO-10.
        LF = main_LLFIN+10.

        CALL WRITE_LOG_LOG(filename_LOG_LOG)

        WRITE(17,1130)IKEYtot,(modeles_TIT(I),I=1,5),TETAEF,main_GLOG,main_ASALOG,modeles_NHE,AMG,
     1   L0,LF,LZERO,LFIN,ITOT,main_PAS,main_ECHX,main_ECHY,main_FWHM
        WRITE(17,1132) (FL(D),D=I1,I2)

        WRITE(19,1130)IKEYtot,(modeles_TIT(I),I=1,5),TETAEF,main_GLOG,main_ASALOG,modeles_NHE,AMG,
     1   L0,LF,LZERO,LFIN,ITOT,main_PAS,main_ECHX,main_ECHY,main_FWHM
        WRITE(19,1132) (FCONT(D),D=I1,I2)

        WRITE(20,1130)IKEYtot,(modeles_TIT(I),I=1,5),TETAEF,main_GLOG,main_ASALOG,modeles_NHE,AMG,
     1   L0,LF,LZERO,LFIN,ITOT,main_PAS,main_ECHX,main_ECHY,main_FWHM
        WRITE(20,1132) (FN(D),D=I1,I2)

C
        IF config_VERBOSE WRITE(6,707) IKEY, LZERO, LFIN, I1, I2

        IKEY = IKEY+1
        IF (IKEY .GT. IKEYTOT) EXIT !Main loop exit door!

        WRITE(6, 708) IKEY, IRH
        IDENTH = .FALSE.

        LZERO = LZERO+main_AINT
        LFIN = LFIN+main_AINT
        IF(LFIN .GT. (main_LLFIN+20.)) LFIN = main_LLFIN+20.

      END DO  !Main loop!

669   CONTINUE
      CLOSE(17)

      ! ISSUE Do I need to print this?
      IF config_VERBOSE THEN
        WRITE(6,*) '   Flux sortant est en nu: Fnu x lambda'
        WRITE(6,*) '   Flux absolu sortant a ete multiplie par 10**5'
      END IF



C  ****************************************************************
C                       XI
C           ZONE DE DEFINITION DES FORMATS
C  *******************************************************************



1130  FORMAT(I5, 5A4, 5F15.5, 4F10.1, I10, 4F15.5)
1132  FORMAT(40000F15.5)



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
110   FORMAT(1H1)
160   FORMAT(4(4X,F8.2,E14.7))
114   FORMAT(4 (4X,F8.2,F8.3))
155   FORMAT(12F6.3)
115   FORMAT(1H )
116   FORMAT(10X,'CONVOLUTION PAR UN PROFIL INSTRUMENTAL')
117   FORMAT(5X,'LZERO=',F10.3,10X,'LFIN=',F10.3,5X,'DTOT=',I7)
121   FORMAT(1X,A2,I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1,
     1 F7.1)
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
1500  FORMAT(A20)
1501  FORMAT(2X,'LAMBD milieu intevalle=',F8.2,2X,'FC=',E15.7)
1502  FORMAT(2X,'LDNOR =',F8.2,2X,'FMOYEN=',E15.7)
1503  FORMAT(2X,'RLAMBD=',F8.2,2x,'RLAMBF=',F8.2,2x,'FLNOR=',E15.7)
1560    FORMAT(A)
1570    FORMAT(1X,A)
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
C ISSUE WHAT

      SUBROUTINE TURBUL(INTERP,IVTOT,TOLV,main_VVT,modeles_NTOT,TOL,VT)
      DIMENSION TOLV(20),VT(50),TOL(50)
      PRINT *,'   ENTREE DS TURBUL'
      IF(IVTOT.EQ.1)   THEN
        WRITE(6,*) ' VT CONSTANT'
        DO N = 1, modeles_NTOT
          VT(N) = main_VVT(1)*1E5
        END DO
      ELSE
        WRITE(6,*) ' VT VARIABLE AVEC LA PROFONDEUR'
        WRITE(6,*) '     LOG TO'
        WRITE(6,101) (TOLV(I),I=1,IVTOT)
        WRITE(6,*) '     VT'
        WRITE(6,101) (main_VVT(I),I=1,IVTOT)
        IF(INTERP .EQ. 1) CALL FTLIN3(IVTOT,TOLV,main_VVT,modeles_NTOT,TOL,VT)
        
        ! ISSUE: is this still useful?? (SWITCHED OFF IN CODE)

        IF(INTERP .GT. 1) CALL FT2(IVTOT,TOLV,main_VVT,modeles_NTOT,TOL,VT)
        NT2=modeles_NTOT-2
        DO N=1,NT2,3
          WRITE(6,102) N,TOL(N),VT(N),(N+1),TOL(N+1),VT(N+1),
     1                 (N+2),TOL(N+2),VT(N+2)
        END DO

        DO N = 1, modeles_NTOT
          VT(N) = VT(N)*1E5
        END DO
      END IF

      RETURN
100   FORMAT(I5)
101   FORMAT(10F8.3)
102   FORMAT(3(I5,2F8.3,5X))
      END
C





















































C-------------------------------------------------------------------------------
C ISSUE: This seems to be some kind of search, gotta check if better to do it upon reading the file!!
      SUBROUTINE ABONDRAIH(abonds_ELE,ABO,abonds_NABOND,atomgrade_ELEM,ABOND,atomgrade_NBLEND)
      REAL ABO, ABOND
      DIMENSION ABO(100),ABOND(8000)

      DO  K=1,atomgrade_NBLEND
        DO  J=1,abonds_NABOND
C           print 1035, abonds_ELE(J), atomgrade_ELEM(k), ALOG10(abo(j))-0.37+12
          IF(abonds_ELE(J) .EQ. atomgrade_ELEM(K))  GO TO 14
        END DO   !FIN BCLE SUR J
            
        ! TODO check this while reading file, not here!!!!
        WRITE(6,106) atomgrade_ELEM(K)
        STOP
14      ABOND(K) = ABO(J)
      END DO   !FIN BCLE SUR K
      RETURN
c
106   FORMAT('     MANQUE L ABONDANCE DU  ', A2)
      END






C-------------------------------------------------------------------------------
C     ***calcule la population au niveau inferieur de la transition
C     ***la largeur doppler DELTA et le coefficient d'elargissement
C     ***le "A" utilise dans le calcul de H(A,V)
C
C Note: (JT) seems to use variables atomgrade_* and modeles_*
      SUBROUTINE POPADELH (NPAR,partit_EL,partit_KI1,partit_KI2,M,atomgrade_NBLEND,atomgrade_ELEM,
     1 atomgrade_LAMBDA,atomgrade_KIEX,atomgrade_CH,CORCH,CVdW,atomgrade_GR,atomgrade_GE,atomgrade_IONI,modeles_NTOT,modeles_TETA,modeles_PE,ALPH,
     2 PHN,PH2,VT,P,POP,A,DELTA)

      PARAMETER(MAX_atomgrade_NBLEND=8000)
      CHARACTER*1 ISI(1), ISS(1)
      CHARACTER*2 atomgrade_ELEM, partit_EL
      INTEGER atomgrade_NBLEND, NPAR, J, K
      real KB,KIES,KII,NUL
      DIMENSION VT(50),ALPH(50),PHN(50),PH2(50),
     1 P(3,85,50),ALPHL(50),
     3 CORCH(MAX_atomgrade_NBLEND),CVdW(MAX_atomgrade_NBLEND),
     4 POP(MAX_atomgrade_NBLEND,50),A(MAX_atomgrade_NBLEND,50),DELTA(MAX_atomgrade_NBLEND,50)
      CHARACTER*2 TTI, CC, OO, NN, MGG

      DATA KB/1.38046E-16/, DEUXR/1.6634E+8/, C4/2.1179E+8/,
     1 C6/3.76727E+11/, PI/3.141593/, C/2.997929E+10/
      DATA ISI/' '/, ISS/' '/
      DATA TTI/'TI'/,CC/' C'/,OO/' O'/,NN/' N'/,MGG/'MG'/
      H  = 6.6252E-27
      C5 = 2.*PI* (3.*PI**2/2.44)**0.4
c
      DO  K=1,atomgrade_NBLEND
        corch(k)=0.
        CVdW(K)=0
        DO  J=1,NPAR
          IF(partit_EL(J).EQ.atomgrade_ELEM(K)) GO TO 15
        END DO
        WRITE(6,104) atomgrade_ELEM(K)
        STOP
        
15      IOO=atomgrade_IONI(K)
C
        write(77,*)atomgrade_ELEM(k),atomgrade_LAMBDA(k)
        IF(atomgrade_CH(K).LT.1.E-37)  THEN
          KIES=(12398.54/atomgrade_LAMBDA(K)) + atomgrade_KIEX(K)
          IF(IOO.EQ.1)   KII=partit_KI1(J)
          IF(IOO.EQ.2)   KII=partit_KI2(J)
          IF(CORCH(K).LT.1.E-37)   THEN
            CORCH(K)=0.67 * atomgrade_KIEX(K) +1
          END IF   ! FIN DE IF CORCH(K)=0
C               WRITE(6,125)  atomgrade_LAMBDA(K), CORCH(K)
          CVdW(K)= CALCH(KII,IOO,atomgrade_KIEX(K),ISI,KIES,ISS)
          atomgrade_CH(K)= CVdW(K) * CORCH(K)
        END IF  ! FIN DE IF atomgrade_CH=0.

C
        IF(atomgrade_CH(K) .LT. 1.E-20) THEN 
          IOPI=1
        ELSE
          IOPI=2
        END IF
        
        DO  N=1,modeles_NTOT
          T=5040./modeles_TETA(N)
          NUL= C* 1.E+8 /atomgrade_LAMBDA(K)
          AHNUL= H*NUL
          ALPHL(N)=EXP(-AHNUL/(KB*T))

          TAP = 1.-ALPHL(N)
          TOP = 10.**(-atomgrade_KIEX(K)*modeles_TETA(N))
          POP(K,N) = P(IOO,J,N)*TOP*TAP
C NOXIG: ISSUE what does it mean?
          IF(K .EQ. 1) POP(K,N) = TOP*TAP*P(IOO,J,N)*sat4_PO(N)/sat4_PPH(N)
          DELTA(K,N) =(1.E-8*atomgrade_LAMBDA(K))/C*SQRT(VT(N)**2+DEUXR*T/partit_M(J))
          VREL    = SQRT(C4*T*(1.+1./partit_M(J)))
          IF (IOPI.EQ.1) THEN
            GH = C5*atomgrade_CH(K)**0.4*VREL**0.6
C                 if (N.EQ.10)  write (6,100) GH
          ELSE
            GH = atomgrade_CH(K) + Corch(K)*T
C                 if (N.EQ.10) write(6, 101) GH
          END IF
          GAMMA = atomgrade_GR(K)+(atomgrade_GE(K)*modeles_PE(N)+GH*(PHN(N)+1.0146*PH2(N)))/(KB*T)
          A(K,N) =GAMMA*(1.E-8*atomgrade_LAMBDA(K))**2 / (C6*DELTA(K,N))
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
C     ***calcule la pop du niv fond de l'ion pour tous les NPAR atomes de
C     ***la table des fonctions de partition ,a tous les niv du modele
C     ***
      SUBROUTINE POPUL(modeles_TETA,modeles_PE,modeles_NTOT,partit_TINI,partit_PA,partit_JKMAX,partit_KI1,partit_KI2,NPAR,partit_TABU,P)
      DIMENSION U(3),ALISTU(63),P(3,85,50), UE(50),TT(51)
c           40 elements, 50 niveaux de modele, 3 niv d'ionisation par elem.
c           partit donnee pour 33 temperatures au plus ds la table.
      REAL KB
      KB=1.38046E-16
      C1=4.8298E+15   ! =2 * (2*Pi*KB*ME)**1.5 / H**3
C
      DO  N=1,modeles_NTOT
      T=5040./modeles_TETA(N)
      UE(N)=C1*KB*T /modeles_PE(N)*T**1.5
            DO  J=1,NPAR
            KMAX=partit_JKMAX(J)
            TT(1) = partit_TINI(J)
                  DO  L=1,3
                        DO  K=1,KMAX
                        TT(K+1) = TT(K) + partit_PA(J)
                        ALISTU(K) = partit_TABU(J,L,K)
                        END DO
c
                        if (modeles_TETA(N).LT.TT(KMAX-1) ) then ! (inter parabolique)
                        UUU=FT(modeles_TETA(N),KMAX,TT,ALISTU)
                        else
c                  interpolation lineaire entre 2 derniers pts
                        AA=(ALISTU(KMAX)-ALISTU(KMAX-1)) / partit_PA(J)
                        BB=ALISTU(KMAX-1) - AA * TT(KMAX-1)
                        UUU= AA*modeles_TETA(N) + BB
                        end if
c
                        U(L) = EXP(2.302585*UUU )
                  END DO   ! FIN BCLE SUR L
C
            X=U(1) / (U(2)*UE(N)) * 10.**(partit_KI1(J)*modeles_TETA(N))
            TKI2= partit_KI2(J) * modeles_TETA(N)
            IF(TKI2.GE.77.)   THEN
            Y=0.
            P(3,J,N)=0.
                          ELSE
            Y=U(3)*UE(N)/U(2)  *  10.**(-partit_KI2(J)*modeles_TETA(N))
            P(3,J,N) =(1./U(3))*(Y/(1.+X+Y))
            END IF

C
            P(2,J,N) = (1./U(2))*(1./(1.+X+Y))
            P(1,J,N) =  (1./U(1))*(X/(1.+X+Y))
            END DO   ! fin bcle sur J
      END DO   ! fin bcle sur N
      RETURN
      END








! TODO Fix Initializations
! TOdO explain parameters
! TODO verbose
! TODO discover what is input and what is output

C-------------------------------------------------------------------------------
C Sets the Voigt profile using Hjertings' constants.
C
C Note: convolution for molecules uses Gaussian profile.
C

! ISSUE with variable MM
      SUBROUTINE SELEKFH(KIK,    ! 0/1, passed to FLINH
     +                   DTOT,   ! ?
     +                   GFAL, 
     +                   ABOND, 
     +                   ECART, 
     +                   TAUH,
     +                   DHM,
     +                   DHP,
     +                   VT,
     +                   B, 
     +                   B1,
     +                   B2,
     +                   KCD,
     +                   POP,
     +                   DELTA,
     +                   A,
     +                   TTD,
     +                   FL,
     +                   FCONT,
     +                   ECARTM
     +                  )
      USE READ_FILES
      IMPLICIT NONE
      PARAMETER(PARAMETER_NMOL=50000,NP=7000)
      INTEGER D, DTOT, DHM,DHP
      REAL lambi
      REAL KAPPA,KA,KAP,KCD,KCI,KAM,KAPPAM,KAPPT
      REAL*8 ECART,ECAR,ECARTM,ECARM
      DIMENSION VT(50)
      DIMENSION B(0:50),B1(0:50),B2(0:50),BI(0:50)
      DIMENSION ECART(MAX_atomgrade_NBLEND),
     +          ECAR(MAX_atomgrade_NBLEND),
     +          ECARTL(MAX_atomgrade_NBLEND),
     +          GFAL(MAX_atomgrade_NBLEND),
     +          ABOND(MAX_atomgrade_NBLEND),
     +          KA(MAX_atomgrade_NBLEND),
     +          KAP(50),
     +          KAPPA(50),
     +          KCD(NP,50),
     +          KCI(50),
     +          POP(MAX_atomgrade_NBLEND,50),
     +          DELTA(MAX_atomgrade_NBLEND,50),
     +          A(MAX_atomgrade_NBLEND,50)

      DIMENSION TTD(NP),FL(NP),TAUHD(50),TAUH(NP,50)
      DIMENSION FCONT(NP)
      DIMENSION DELTAM(PARAMETER_NMOL,50),
     +          ECARTM(PARAMETER_NMOL),
     +          ECARM(PARAMETER_NMOL),
     +          ECARTLM(PARAMETER_NMOL),
     +          KAM(PARAMETER_NMOL),
     +          KAPPAM(50),
     +          KAPPT(50)

      DATA DEUXR/1.6634E+8/,RPI/1.77245385/,C/2.997929E+10/
C

      IF (atomgrade_NBLEND .NE. 0) then
        DO K = 1,atomgrade_NBLEND
          ECAR(K) = ECART(K)
        END DO
      END IF
      
      IF (km_MBLEND .ne. 0) then
        DO K=1,km_MBLEND
          ECARM(K)=ECARTM(K)
        END DO
      end if
      
      DO D=1,DTOT
        lambi = (6270+(D-1)*0.02)
        if (atomgrade_NBLEND .ne. 0) then
          DO K=1,atomgrade_NBLEND
            ECAR(K)=ECAR(K)-main_PAS
            ECARTL(K)=ECAR(K)
          END DO
        end if
        
        if(km_MBLEND.ne.0) then
          DO K=1,km_MBLEND
            ECARM(K) = ECARM(K)-main_PAS
            ECARTLM(K) = ECARM(K)
          END DO
        end if
      
        DO N = 1,modeles_NTOT
          KAPPA(N) =0.
          KAPPAM(N) =0.
          T = 5040./modeles_TETA(N)
          
          ! atomes
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

260       CONTINUE

          ! molecule
          IF(km_MBLEND.EQ.0) GO TO 250
          DO L=1,km_MBLEND
            IF( ABS(ECARTLM(L)) .GT. km_ALARGM(L) )  then
              KAM(L)=0.
            else
          
              ! ISSUE uses MM, which is read within KAPMOL and potentially has a different value for each molecule!!!!! this is very weird
              ! Note that km_MM no longer exists but it is the ancient "MM" read within ancient "KAPMOL()"
              DELTAM(L,N)=(1.E-8*km_LMBDAM(L))/C*SQRT(VT(N)**2+DEUXR*T/km_MM)
              VM=ABS(ECARM(L)*1.E-08/DELTAM(L,N))
              PHI=(EXP(-VM**2))/(RPI*DELTAM(L,N))
              KAM(L)=PHI*km_GFM(L)*km_PNVJ(L,N)
            end if
            KAPPAM(N)=KAPPAM(N)+KAM(L)
          END DO   !  fin bcle sur L
        
250       KAPPT(N)=KAPPA(N)+KAPPAM(N)
          KCI(N)=KCD(D,N)
          KAP(N)=KAPPT(N)+KCI(N)
          BI(N)=((B2(N)-B1(N))*(FLOAT(D-1)))/(FLOAT(DTOT-1)) + B1(N)
        END DO    ! fin bcle sur N
        
        BI(0)=((B2(0)-B1(0))*(FLOAT(D-1)))/(FLOAT(DTOT-1)) + B1(0)
        
        IF(D.EQ.1) WRITE(6,151) D,BI(0),BI(1),BI(modeles_NTOT)
        IF(D.EQ.1) WRITE(6,150) D, KCI(1),KCI(modeles_NTOT),KAPPA(1),KAPPA(modeles_NTOT)
        IF(D.EQ.1) WRITE(6,152) KAPPAM(1),KAPPAM(modeles_NTOT)
        
c       WRITE(6,151) D,BI(0),BI(1),BI(modeles_NTOT)
c       WRITE(6,150) D, KCI(1),KCI(modeles_NTOT),KAPPA(1),KAPPA(modeles_NTOT)
c       WRITE(6,152) KAPPAM(1),KAPPAM(modeles_NTOT)
        
        !--verbose--!
        IF (VERBOSE .AND. D .EQ. DTOT) THEN 
          WRITE(6,151) D,BI(0),BI(1),BI(modeles_NTOT)
          WRITE(6,150) D,KCI(1),KCI(modeles_NTOT),KAPPA(1),KAPPA(modeles_NTOT)
          WRITE(6,152)KAPPAM(1),KAPPAM(modeles_NTOT)
        END IF
        
        IF((D.LT.DHM).OR.(D.GE.DHP)) THEN
          CALL FLIN1(KAP,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,KIK)
          FL(D) = flin_F
          IF (flin_CAVA.GT.1) THEN
            WRITE(6,131) TTD(D),CAVA
            STOP
          END IF
          
c         FN(D) = FL(D) / FCONT(D)
        ELSE
          DO N = 1,modeles_NTOT
              TAUHD(N) = TAUH(D,N)
          END DO
          CALL FLINH(KAP,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,
     +     KIK,TAUHD)
          FL(D) = flin_F
          IF(CAVA .GT. 1) THEN
            WRITE(6,131) TTD(D),CAVA
            STOP
          END IF
        END IF
            
        ! Dez 03-P. Coelho - calculate the continuum and normalized spectra
        CALL FLIN1(KCI,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,KIK)
        FCONT(D) = flin_F
        ! TODO Not checking CAVA, really gotta make it STOP from within FLIN_
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






































































C Module BK
C Calling map
C BK calls ABSORU calls GAUNTH
C                 calls TEMPA
C                 calls SAHATH
C                 calls ATHYHE
C                 calls IONIPE
C

      MODULE BK
      
      CONTAINS
      
      
      
      
      


C-------------------------------------------------------------------------------
C Calculates the flux in the continuum.
C
      SUBROUTINE BK(LAMBD,B,B1,B2,ALPH,PHN,PH2,
     1              FC,KC,KC1,KC2,KCD,TTD,DTOT,KIK,LZERO,LFIN)
      PARAMETER(NP=7000)
      INTEGER D,DTOT,CAVA
      REAL LAMBD,NU,KB,KC,KC1,KC2,LLZERO,LLFIN,NU1,NU2,KCD,
     1 LAMBDC,KCJ,KCN
      REAL*8 LZERO,LFIN
      DIMENSION B(0:50),B1(0:50),B2(0:50)
      DIMENSION KC(50),TOTKAP(2),
     1 ALPH(50),PHN(50),PH2(50),KC1(50),KC2(50)

c p 21/11/04 M-N  DIMENSION TTD(DTOT),KCD(DTOT,50),KCJ(2,50),KCN(2),LAMBDC(2)
      DIMENSION TTD(NP),KCD(NP,50),KCJ(2,50),KCN(2),LAMBDC(2)
      DIMENSION FTTC(NP)
      DIMENSION FC(NP)

      COMMON /SAPE/  AVM, ZNU1, ZNU2, ZNU3, ZMUZE, ZNU(30)
      COMMON /SAPU/  PE_SAPU, RHO, TOC, ZNH(12)

      
      ! ISSUE I don't like this name LLZERO, it is the same name as the variable read from main.dat
      LLZERO=LZERO
      LLFIN=LFIN
      C  = 2.997929E+10
      H  = 6.6252E-27
      KB = 1.38046E-16
      NU1 = C* 1.E+8 /LZERO
      AHNU1 = H*NU1
      C31 = (2*AHNU1) * (NU1/C)**2
      
      DO N = 1,modeles_NTOT
        T = 5040./modeles_TETA(N)
        ALPH(N) = EXP(-AHNU1/(KB*T))
        B1(N) = C31 * (ALPH(N)/(1.-ALPH(N)))
        CALL ABSORU(LLZERO,modeles_TETA(N),ALOG10(modeles_PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
        KC1(N) = TOTKAP(1)
      END DO
      
      NU2= C* 1.E+8 /LFIN
      AHNU2= H*NU2
      C32=(2*AHNU2) * (NU2/C)**2
      DO N = 1,modeles_NTOT
        ! TODO: calculate this "T" somewhere else, this is calculated all the time! a lot of waste
        T = 5040./modeles_TETA(N)
        ALPH(N) = EXP(-AHNU2/(KB*T))
        B2(N) = C32 * (ALPH(N)/(1.-ALPH(N)))
        CALL ABSORU(LLFIN,modeles_TETA(N),ALOG10(modeles_PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
        KC2(N)=TOTKAP(1)
      END DO
      
      NU= C* 1.E+8 /LAMBD
      AHNU= H*NU
      C3=(2*AHNU) * (NU/C)**2
      DO N=1,modeles_NTOT
        T=5040./modeles_TETA(N)
        ALPH(N) = EXP(-AHNU/(KB*T))
        B(N) = C3 * (ALPH(N)/(1.-ALPH(N)))
        CALL ABSORU(LAMBD,modeles_TETA(N),ALOG10(modeles_PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
        PHN(N) = ZNH(absoru2_NMETA+4) *KB * T
        PH2(N) = ZNH(absoru2_NMETA+2) *KB * T

!ISSUE Can I delete this?        
c     if(n.eq.1) write(6,*) alph(n),phn(n),ph2(n),znh(absoru2_NMETA+4),
c     1 znh(absoru2_NMETA+2),t
c     if(n.eq.modeles_NTOT) write(6,*) alph(n),phn(n),ph2(n),znh(absoru2_NMETA+4),
c     1 znh(absoru2_NMETA+2),t

        KC(N) = TOTKAP(1)
      END DO
      
      TET0 = FTETA0(modeles_PG, modeles_TETA)     !on extrapole modeles_TETA pour modeles_NH=0
      T = 5040./TET0
      
      
      ALPH01 = EXP(-AHNU1/(KB*T))
      B1(0) = C31 * (ALPH01/(1.-ALPH01))
      CALL FLIN1(KC1,B1,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,KIK)
      FC1 = flin_F
      IF(flin_CAVA. GT. 0) THEN
        !--verbose--!
        IF (VERBOSE) THEN

          WRITE(6,132) CAVA
          ! ISSUE ERR was in a common, but not being assigned. I have to see what it was about
          WRITE(6,135) (I, flin_TO(I),ERR(I),I=1,modeles_NTOT)
        END IF
        
        IF(CAVA.GT.1) STOP
      END IF
      
      ALPH02 = EXP(-AHNU2/(KB*T))
      B2(0) = C32 * (ALPH02/(1.-ALPH02))
      CALL FLIN1(KC2,B2,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,KIK)
      FC2 = flin_F
      IF (flin_CAVA .GT. 0) THEN
        !--verbose--!
        IF (VERBOSE) THEN
          WRITE(6,132) CAVA
          WRITE(6,135) (I,flin_TO(I),ERR(I),I=1,modeles_NTOT)
        END IF
        
        IF(CAVA .GT. 1) STOP
      END IF
      
      ALPH0 = EXP(-AHNU/(KB*T))
      B(0) = C3 * (ALPH0/(1.-ALPH0))
      CALL FLIN1(KC,B,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,KIK)
      FC = flin_F
      IF(flin_CAVA. GT.0) THEN
        !--verbose--!
        IF (VERBOSE) THEN
          WRITE(6,132) CAVA
          WRITE(6,135) (I,flin_TO(I),ERR(I),I=1,modeles_NTOT)
        END IF
        IF(CAVA.GT.1) STOP
      END IF
      
cpc   WRITE(6,151) KC1(1),KC1(modeles_NTOT),B1(0),B1(1),B1(modeles_NTOT),FC1
cpc   WRITE(6,152) KC2(1),KC2(modeles_NTOT),B2(0),B2(1),B2(modeles_NTOT),FC2
cpc   WRITE(6,150) KC(1),KC(modeles_NTOT),B(0),B(1),B(modeles_NTOT),FC

      ILZERO = LZERO/100.
      ILZERO = 1E2*ILZERO
      LAMBDC(1) = LZERO-ILZERO
      LAMBDC(2) = LFIN-ILZERO
      DO N=1,modeles_NTOT
        KCJ(1,N)=KC1(N)
        KCJ(2,N)=KC2(N)
      END DO
      DO N=1,modeles_NTOT
        DO J=1,2
          KCN(J)=KCJ(J,N)
        END DO
        CALL FTLIN3(2,LAMBDC,KCN,DTOT,TTD,FTTC)
        DO D=1,DTOT
          KCD(D,N)=FTTC(D)
        END DO
      END DO
      
      IF (VERBOSE) THEN
        WRITE(6,153) KCD(1,1),KCD(1,modeles_NTOT)
        WRITE(6,154) KCD(DTOT,1),KCD(DTOT,modeles_NTOT)
      END IF
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


      
      
      
      
      
      
      
      
      
      








      
      
      END MODULE BK
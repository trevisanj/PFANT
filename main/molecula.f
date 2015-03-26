      MODULE MOLECULA

      ! TODO Write a common module to put all this ERROR_stuff

      ! ERROR_EXCEEDED: Error code if any array index blow maximum number of elements
      !                 allowed while reading from file.
      ! ERROR_NOT_FOUND: When value from somewhere selects something that does not exist
      !                  (example in READ_MODELES()).
      ! ERROR_BAD_VALUE: generic error for bad value found inside file
      INTEGER ERROR_EXCEEDED, ERROR_NOT_FOUND, ERROR_BAD_VALUE
      PARAMETER(ERROR_EXCEEDED=111, ERROR_NOT_FOUND=112,
     +           ERROR_BAD_VALUE=113)

      INTEGER, PARAMETER :: MAX_km__NUMBER=21

      ! Old "MAX_LINES_PER_MOL"; ISSUE: what does "MAX_LINES_PER_MOL" stand for?
      INTEGER, PARAMETER :: MAX_LINES_PER_MOL=50000
      ! Old "NTR"; Maximum number of transitions for each molecule
      INTEGER, PARAMETER :: MAX_TRANSITIONS_PER_MOL=200

      INTEGER, PARAMETER ::
     +  MAX_LINES_TOTAL = MAX_LINES_PER_MOL*MAX_km__NUMBER


      INTEGER km__NUMBER


      CHARACTER*256 km__TITM, km__TITULO

      DIMENSION km__TITULO(MAX_km__NUMBER)


      REAL, DIMENSION(MAX_km__NUMBER) :: km__FE, km__DO,
     +  km__MM, km__AM, km__BM, km__UA, km__UB, km__TE, km__CRO,
     +  km__A0, km__A1, km__A2, km__A3, km__A4, km__ALS, km__S

      INTEGER, DIMENSION(MAX_km__NUMBER)  :: km__ISE, km__NV, 
     +  km__N_LAMBDA  ! This stores the number of spectral lines for each molecule

      REAL, DIMENSION(MAX_TRANSITIONS_PER_MOL, MAX_km__NUMBER) ::
     +  km__QQV, km__GGV, km__BBV, km__DDV, km__FACT

      REAL*8,  DIMENSION(MAX_LINES_PER_MOL, MAX_km__NUMBER) :: km__LMBDAM
      REAL,    DIMENSION(MAX_LINES_PER_MOL, MAX_km__NUMBER) ::
     +  km__SJ, km__JJ
      INTEGER, DIMENSION(MAX_LINES_PER_MOL, MAX_km__NUMBER) :: km__NUMLIN


      SAVE

C     ========
      CONTAINS
C     ========

C================================================================================================================================
C READ_MOLECULAGRADE(): reads file moleculagrade.dat to fill variables km_*
C
C Original UNIT: 12
C
C Obs: this is part of original subroutine KAPMOL()

      SUBROUTINE READ_MOLECULAGRADE(filename)
      IMPLICIT NONE
      INTEGER UNIT_, I,
     +  I_MOL,   ! Old "NMOL", number of current molecule
     +  I_LAMBDA,  ! Counts lines within each molecule (reset at each new molecule)
     +  NNV, IZ, J,
     +  NUMLIN   ! Temporary variable
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename

      OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

      WRITE (*,*) 'MAX_LINES_TOTAL = ', MAX_LINES_TOTAL



      ! BLB: NUMBER -- number of molecules do be considered
      READ(UNIT_,*) km__NUMBER

      write (*,*) 'NUMBER--------------', km__NUMBER

      READ(UNIT_,'(A)') km__TITM
      !~READ(UNIT_,'(20A4)') km__TITM
      write (*,*) 'TITM--------------', km__TITM

      ! BLB:
      ! BLB: km__NV -- number of transitions (v', v'') for each molecule
      ! BLB: Example: if (0,0)(1,1)(2,2) are considered for CH
      ! BLB:             (1,1)(2,2) are considered for CN
      ! BLB:             NV(J) = 3 2
      READ(UNIT_,*) (km__NV(J), J=1,km__NUMBER)

      ! TODO check km__number spill

      DO I_MOL = 1, km__NUMBER
        ! TODO check spill in each element in km__NV

        ! BLB:
        ! BLB: title -- specifying the molecule to follow
        ! BLB:          format: 20A4
        READ(UNIT_,'(A)') km__TITULO(I_MOL)


      WRITE(*,*) 'TITULO NOW ', km__TITULO(I_MOL)

        ! BLB: FE, DO, MM, AM, BM, UA, UB, Te, CRO
        ! BLB: Format: free
        ! BLB: FE -- molecular oscillator strength fel
        ! BLB: DO -- dissociation constant (eV)
        ! BLB:    | MM -- [mass of A + mass of B] for molecula AB
        ! BLB:  +-| AM -- mass of A
        ! BLB:  | | BM -- mass of B
        ! BLB:  |
        ! BLB:  +---> where (12)C = 12, H = 1.008
        ! BLB:
        ! BLB: UA -- molar(?) value of partition function for element A     ISSUE
        ! BLB: UB -- molar(?) value of partition function for element B     ISSUE
        ! BLB: TE -- eletronic term.
        ! BLB: CRO - delta Kronecker (2-delta_{Sigma, 0})
        ! BLB:       delta_{Sigma, 0} = 0 for Sigma transitions
        ! BLB:                          1 for non-Sigma transitions

        READ(UNIT_,*) km__FE(I_MOL), km__DO(I_MOL), km__MM(I_MOL),
     +             km__AM(I_MOL), km__BM(I_MOL), km__UA(I_MOL),
     +             km__UB(I_MOL), km__TE(I_MOL), km__CRO(I_MOL)


        ! ISSUE Documentation
        ! ISSUE My sample file is blank here
        READ(UNIT_,'(2X,I3, 5F10.6, 10X, F6.3)') km__ISE(I_MOL),
     +    km__A0(I_MOL), km__A1(I_MOL), km__A2(I_MOL),
     +    km__A3(I_MOL), km__A4(I_MOL), km__ALS(I_MOL)

        ! Issue what is S??
        READ(UNIT_,*) km__S(I_MOL)

        NNV = km__NV(I_MOL)

        WRITE(*,*) 'NV NOW=', NNV

        ! TODO type in documentation
        READ(UNIT_,*) (km__QQV(I, I_MOL), I=1,NNV)
        READ(UNIT_,*) (km__GGV(I, I_MOL), I=1,NNV)
        READ(UNIT_,*) (km__BBV(I, I_MOL), I=1,NNV)
        READ(UNIT_,*) (km__DDV(I, I_MOL), I=1,NNV)
        READ(UNIT_,*) (km__FACT(I, I_MOL),I=1,NNV)

        DO I = 1,NNV
          km__DDV(I, I_MOL)=1.E-6*km__DDV(I, I_MOL)
        END DO


        !~L = (I_MOL-1)*MAX_LINES_PER_MOL+1  ! Initial index for all vectors inside this loop
        !~WRITE(*,*) 'LLLLLLLLLLLLL', L
        I_LAMBDA = 1  ! Counts spectral lines for current molecule
        I_SET_COUNT = 0  ! File consistency check: has to match NNV when the following loop ends
        DO WHILE (.TRUE.)

          ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
          IF (I_LAMBDA .GT. MAX_LINES_PER_MOL) THEN
            WRITE(*,*) 'READ_MOLECULAGRADE(): molecule number ', I_MOL,
     +          ': number of lines exceeded ', MAX_LINES_PER_MOL
            STOP ERROR_EXCEEDED
          END IF

          ! ISSUE what does each row stand for??

          ! BLB: LMBDAM(L), SJ(L), JJ(L), IZ, ITRANS(L), NUMLIN
          ! BLB: Format: free
          ! BLB:
          ! BLB: LMBDAM -- wavelength in angstron
          ! BLB: Sj -- Hön-London factor calculated such that sum(S_j/(2*j+1)) = 1 ISSUE: Hön-London?
          ! BLB: JJ -- rotational quantum number
          ! BLB: IZ -- branch as table:  ISSUE not used
          ! BLB:       P  -  1
          ! BLB:       Q  -  2
          ! BLB:       R  -  3
          ! BLB:       P1 -  4
          ! BLB:       Q1 -  5
          ! BLB:       R1 -  6
          ! BLB:       P2 -  7
          ! BLB:       Q2 -  8
          ! BLB:       R2 -  9
          ! BLB:       P3 - 10
          ! BLB:       Q3 - 11
          ! BLB:       R3 - 12
          ! BLB: ITRANS -- key to indicate which is the (v',v'') -- only used in isotropic calculations ISSUE: missing from sample file moleculagrade.dat
          ! ISSUE (question) Where does moleculagrade.dat come from?
          ! BLB: NUMLIN -- key as table:
          ! BLB:           = 1 for the last line of the first (v',v'') set of lines
          ! BLB:           = 2 for the last line of the second (v', v'') set of lines  ISSUE never used
          ! BLB:           .
          ! BLB:           . (etc)  ISSUE never used
          ! BLB:           .
          ! BLB:
          ! BLB:           = 9 for the last line of the last (v', v'') set of lines of a certain molecula
          READ(UNIT_,*) km__LMBDAM(I_LAMBDA, I_MOL), 
     +         km__SJ(I_LAMBDA, I_MOL),
     +         km__JJ(I_LAMBDA, I_MOL), IZ, NUMLIN
     
          km__NUMLIN(I_LAMBDA, I_MOL) = NUMLIN

          IF (NUMLIN .NE. 0) I_SET_COUNT = I_SET_COUNT+1
          IF (NUMLIN .EQ. 9) EXIT

          !~L = L+1
          I_LAMBDA = I_LAMBDA+1
        END DO
        
        ! ISSUE: I don't get this; it will keep looping forever???
        IF(I_SET_COUNT .NE. NNV) THEN
          WRITE(*,*) 'Incorrect number of spectral line sets ', 
     +          I_SET_COUNT, '(should be ', NNV, ')'
          STOP ERROR_BAD_VALUE
        END IF

        
        WRITE (*,*) 'IIIIIIIIIIIIIIIIIIIIIIIIIIII', I
      END DO

      CLOSE(UNIT_)

      WRITE (*,*) 'NUMBER (again) --------------', km__NUMBER

      END





C================================================================================================================================
C================================================================================================================================
      SUBROUTINE SHOW_PAULA(NH,TETA,NTOT)
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
      COMMON/KAPM1/MM,MBLEND
      COMMON/TOTAL/MMC,MBLENQ
      COMMON/OPTIM/LZERO,LFIN
       COMMON/KAPM2/LMBDAM,GFM,PNVJ,ALARGM
       COMMON/KAPM3/PPH,PPC2,PN,PC13,PMG,PO,PTI,PNG,PIG,pfe
      DATA H/6.6252E-27/,C/2.997929E+10/,KB/1.38046E-16/,
     -CK/2.85474E-04/,C2/8.8525E-13/
       OPEN(UNIT=12,FILE='moleculagrade.dat',STATUS='OLD')
      NMOL=1
      K=1  ! ISSUE, why J, K, why not only NMOL?
      I=1
      L=1  ! ISSUE is this the "spectral line" counter?
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
 102  IF(NMOL.NE.2) GO TO 103
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PPC2(N)
        END DO
      GO TO 999
 103  IF(NMOL.NE.3) GO TO 104
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PN(N)
        END DO
      GO TO 999
 104  IF(NMOL.NE.4) GO TO 105
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PN(N)
        END DO
      GO TO 999
 105  IF(NMOL.NE.5) GO TO 106
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PN(N)
        END DO
        GO TO 999
 106    IF(NMOL.NE.6) GO TO 107
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 107    IF(NMOL.NE.7) GO TO 108
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 108    IF(NMOL.NE.8) GO TO 109
      DO  N=1,NTOT
      PPA(N)=PPC2(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 109    IF(NMOL.NE.9) GO TO 110
        DO N=1,NTOT
        PPA(N)=PC13(N)
        PB(N)=PPH(N)
        END DO
        GO TO 999
 110    IF(NMOL.NE.10) GO TO 111
        DO N=1,NTOT
        PPA(N)=PPC2(N)
        PB(N)=PO(N)
        END DO
        GO TO 999
 111    IF(NMOL.NE.11) go to 112
      DO  N=1,NTOT
      PPA(N)=PN(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 112    IF(NMOL.NE.12) go to 113
      DO  N=1,NTOT
      PPA(N)=PO(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 113    IF(NMOL.NE.13) go to 114
      DO  N=1,NTOT
      PPA(N)=PO(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 114    IF(NMOL.NE.14) go to 115
      DO  N=1,NTOT
      PPA(N)=PFE(N)
      PB(N)=PPH(N)
        END DO
        GO TO 999
 115    IF(NMOL.NE.15) go to 116
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 116    IF(NMOL.NE.16) go to 117
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 117    IF(NMOL.NE.17) go to 118
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 118    IF(NMOL.NE.18) go to 119
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 119    IF(NMOL.NE.19) go to 120
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 120    IF(NMOL.NE.20) go to 121
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO
        GO TO 999
 121    IF(NMOL.NE.21) go to 23
      DO  N=1,NTOT
      PPA(N)=PTI(N)
      PB(N)=PO(N)
        END DO










  999 READ(12,*) FE,D0,MM,AM,BM,UA,UB,TE,CRO
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

      IF(NMOL.EQ.1) L=1
      IF(NMOL.GE.2) L=MBLENQ(K-1)+1

C    EX.: GGV(I),I=1,2,3,4...   ITRANS=0,1,2,3....

      I=1  ! ISSUE is this the "line set" counter? Or "transition" counter??
   15 READ(12,*) LMBDAM(L),SJ(L),JJ(L), IZ, NUMLIN
   
   
   

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

   14 IF(NUMLIN.EQ.9) GO TO 16  ! ISSUE: going to 16 will increment I, L even if the line was rejected because it is out of the lambda range
      ! ISSUE what happens if "set" I has no line within LZERO-LFIN?
      LN(I)=L  ! ISSUE Why LN and M if they are the same???
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

      IF(NMOL.GT.1) L=MBLENQ(K-1)+1
      IF(NMOL.EQ.1) L=1
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
      IF(NMOL.LT.NUMBER) GO TO 994
      GO TO 23


  994 NMOL=NMOL+1
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









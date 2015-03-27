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

      ! Old "NM"; ISSUE: what does "NM" stand for?
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







      !=====
      ! Variables filled by FILTER_MOLECULAGRADE()
     
      INTEGER km_MBLEND  ! Total number of spectral lines *filtered in*

      ! Valid elements of these are from 1 to km_MBLEND
      REAL*8, DIMENSION(MAX_LINES_TOTAL) :: km_LMBDAM
      REAL, DIMENSION(MAX_LINES_TOTAL) :: km_SJ, km_JJ, km_GFM, km_ALARGM
     



      !------
      ! These two arrays contain indexes pointing at km_LMBDAM, km_SJ, and km_JJ
      !------
      ! This one points to the last index of the lines of each molecule within 
      ! km_LMBDAM, km_SJ and km_JJ (after the filtering)
      ! Update: **augmented!** -- first element is 0 (ZERO) -- facilitates the algorithm
      DIMENSION km_MBLENQ(MAX_km__NUMBER+1) 
      ! This is similar but is a "local" one, it contains index of the last 
      ! line of each set of lines within km_LMBDAM, km_SJ and km_JJ
      ! **for the current molecule** I_MOL
      ! Update: **augmented!** -- first row is 0 (ZERO) -- facilitates the algorithm     
      ! TODO Explain better
      DIMENSION km_LN(MAX_TRANSITIONS_PER_MOL+1, MAX_km__NUMBER) 


      REAL, DIMENSION(MAX_LINES_TOTAL, MAX_modeles_NTOT) :: km_GFM




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
        ! ISSUE !P! My sample file is blank here
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
          ! BLB: ITRANS -- key to indicate which is the (v',v'') -- only used in isotropic calculations ISSUE: !P! missing from sample file moleculagrade.dat
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
C FILTER_MOLECULAGRADE(): sweeps km__* to populate a few km_*
C                         depending on the interval LZERO-LFIN
C

      SUBROUTINE FILTER_MOLECULAGRADE(LZERO, LFIN)
      IMPLICIT NONE
      
      REAL*8 LZERO,LFIN


      ! Initializes the zero elements of the augmented matrices
      km_MBLENQ(1) = 0
      DO I = 1, MAX_km__NUMBER
        km_LN(1, I) = 0
      END DO

      I_LINE = 0  ! Current spectral line. Keeps growing (not reset when the molecule changes). Related to old "L"
      DO I_MOL = 1, km__NUMBER
        IF (VERBOSE) WRITE(*, *) 'MOLECULE NUMBER', I_MOL, ': ',  !--verbose--!
     +                           km__TITULO(I_MOL)
        
        
        ! Counters starting with "J _" restart at each molecule
        J_SET = 0   ! Current "transition"/"set of spectral lines" ISSUE: is the concept correct?
        FLAG_IN = .FALSE.  ! Whether has filtered in at least one line 
        DO J_LAMBDA = 1, km__N_LAMBDA(I_MOL)  ! Points to first dimension within km__LMBDAM, km__SJ, km__JJ
          !~READ(12,*) km_LMBDAM(L),SJ(L),JJ(L), km_IZ, km_NUMLIN
          LAMBDA = km__LMBDAM(J_LAMBDA, I_MOL)
          NUMLIN = km__NUMLIN(J_LAMBDA, I_MOL)
          
          IF ((LAMBDA .GE. LZERO) .AND. (LAMBDA .LE. LFIN)) THEN
            ! Filters in a new spectral line!
            I_LINE = I_LINE+1
            
            km_LMBDAM(I_LINE) = LAMBDA
            km_SJ(I_LINE) = km__SJ(J_LAMBDA, I_MOL)
            km_JJ(I_LINE) = km__JJ(J_LAMBDA, I_MOL)
            
            FLAG_IN = .TRUE.
          END IF
            
          IF (NUMLIN .NE. 0) THEN
            ! Reached last line of current set of lines
        
            ! ISSUE Should we think about preparing it for not having a single line within LZERO-LFIN for set J_SET, J_SET=1,NNV?????
            IF (.NOT. FLAG_IN) THEN
              ! TODO, IDEA Actually I think that it might work without having lines within a given lambda range, because the routines that use the calculations just don't care which molecule it is
        
        
              !--error checking--!
              ! TODO test this error
              WRITE (*, *) 'USE_MOLECULAGRADE(): Molecule ', I_MOL, 
     +            ' titled  "', km__TITULO(I_MOL), '"'
              WRITE (*, *) 'Set of lines ', (J_SET+1), 'has no lambda '
     +            //'within ', LZERO, ' <= lambda <= ', LFIN
              WRITE (*, *) 'The algorithm is not prepared for this, '
                  //'sorry!'
              STOP ERROR_BAD_VALUE
            END IF
              
            J_SET = J_SET+1           
            km_LN(J_SET+1, I_MOL) = I_LINE
          END IF
        END DO
        
        km_MBLENQ(I_MOL+1) = I_LINE
        
        ! Note: (J_SET .EQ. km__NV(I_MOL)) should hold: this has been checked when the file was read already
      END DO !--end of I_MOL loop--!     

      km_MBLEND = I_LINE
      
      RETURN
      






C================================================================================================================================
C USE_MOLECULAGRADE(): uses km_* filled by FILTER_MOLECULAGRADE() to perform its calculations
C

      SUBROUTINE USE_MOLECULAGRADE()
      IMPLICIT NONE

      ! TODO see if I can use pointers
      REAL, DIMENSION(MAX_modeles_NTOT) :: PPA, PB
               
      REAL*8 T5040
      REAL CSC, H, C, KB, CK, C2
      REAL FE, DO_, MM, AM, BM, UA, UB, TE, CRO 

      DATA H  /6.6252E-27/,
     +     C  /2.997929E+10/,
     +     KB /1.38046E-16/,
     +     CK /2.85474E-04/,
     +     C2 /8.8525E-13/


      DO I_MOL = 1, km__NUMBER
        IF (VERBOSE) WRITE(*, *) 'MOLECULE NUMBER', I_MOL, ': ',  !--verbose--!
     +                           km__TITULO(I_MOL)
        
        
        ! TODO must be prepared to switch molecules on/off!!!
        ! TODO use pointers instead of copying elements!!!
        SELECT CASE (I_MOL)  ! ISSUE Check molecule names and cases
          CASE (1)  ! MgH
            DO N = 1,modeles_NTOT
              PPA(N)=sat4_PMG(N)
              PB(N)=sat4_PPH(N)
            END DO
          CASE (2)  ! C2
            DO N=1,modeles_NTOT
              PPA(N)=sat4_PPC2(N)
              PB(N)=sat4_PPC2(N)
            END DO  
          CASE (3, 4, 5)  ! CN blue,red, nir
            DO N=1,modeles_NTOT
              PPA(N)=sat4_PPC2(N)
              PB(N)=sat4_PN(N)
            END DO
          CASE (6, 7, 8)  ! CH AX, BX, CX
            DO  N=1,modeles_NTOT
              PPA(N)=sat4_PPC2(N)
              PB(N)=sat4_PPH(N)
            END DO
          CASE (9)  ! 13
            DO N=1,modeles_NTOT
              PPA(N)=sat4_PC13(N)
              PB(N)=sat4_PPH(N)
            END DO
          CASE (10)  ! CO nir
            DO N=1,modeles_NTOT
              PPA(N)=sat4_PPC2(N)
              PB(N)=sat4_PO(N)
            END DO
          CASE (11)  ! NH blue
            DO N=1,modeles_NTOT
              PPA(N)=sat4_PN(N)
              PB(N)=sat4_PPH(N)
            END DO
          CASE (12, 13)  ! OH blue,nir
            DO N=1,modeles_NTOT
              PPA(N)=sat4_PO(N)
              PB(N)=sat4_PPH(N)
            END DO
          CASE (14)  ! FeH
            DO N=1,modeles_NTOT
              PPA(N)=sat4_PFE(N)
              PB(N)=sat4_PPH(N)
            END DO
          CASE (15, 16, 17, 18, 19, 20, 21)  ! Tio Gama,Gama linha,alfa,beta,delta,epsilon,phi
            DO N=1,modeles_NTOT
              PPA(N)=sat4_PTI(N)
              PB(N)=sat4_PO(N)
            END DO
        END SELECT
        
        
        NNV = km__NV(I_MOL)
        
        FE  = km__FE(I_MOL)
        DO_ = km__DO(I_MOL)
        MM  = km__MM(I_MOL)
        AM  = km__AM(I_MOL)
        BM  = km__BM(I_MOL)  ! ISSUE Not used!!
        UA  = km__UA(I_MOL)
        UB  = km__UB(I_MOL)
        TE  = km__TE(I_MOL)
        CRO = km___CRO(I_MOL)
        
        
        !======
        ! This part of the code calculates km_PNVL(
        RM = AM*BM/MM
        DO N = 1,modeles_NTOT
          T5040 = modeles_TETA(N)/5040
          PSI = DO_*modeles_TETA(N)+2.5*ALOG10(modeles_TETA(N))-
     @          1.5*ALOG10(RM)-ALOG10(UA*UB)-13.670
          PSI = 10.**PSI
          DO J_SET = 1,NNV
            ! TODO check if it is possible to have a pointer to km_LN(*, I_MOL), which may well be called "LN"
        
            QV = km__QQV(J_SET, I_MOL)
            GV = km__GGV(J_SET, I_MOL)
            BV = km__BBV(J_SET, I_MOL)
            DV = km__DDV(J_SET, I_MOL)
           
            L_INI = km_LN(J_SET, I_MOL)+1
            L_FIN = km_LN(J_SET+1, I_MOL)
            
            ! L is index within km_LMBDAM, km_SJ and km_JJ
            DO L= L_INI, L_FIN
              ! PC: default value for CSC does not exist physically
              CSC = EXP(-H*C/KB*modeles_TETA(N)/5040.*
     @                  (TE+GV+BV*(km_JJ(L)+1)*km_JJ(L))
     @                 )*
     @              (2.-CRO)*(2.*km_JJ(L)+1.)*
     @              EXP(H*C/KB*modeles_TETA(N)/5040.*
     @                  (DV*(km_JJ(L)*(km_JJ(L)+1))**2+2.*BV)
     @                 )
        
              km_PNVJ(L,N) = CSC*PSI*PPA(N)*PB(N)/sat4_PPH(N)
            END DO
            
            
            ! Takes advantage of current J_SET loop so it is not necessary to create 
            ! another double loop as in the original KAPMOL to calculate km_GFM
            IF (N .EQ. 1) THEN
              ! Because GFM does not depend on N, runs this part just once, when N is 1.
              FACTO = km__FACT(J_SET, I_MOL)
              km_GFM(L) = C2*((1.E-8*km_LMBDAM(L))**2)*km_FE*QV*
     @                    km_SJ(L)*FACTO
            END IF
          END DO
        END DO
      END DO !--end of I_MOL loop--!     

      DO L = 1,km_MBLEND
        km_ALARGM(L) = 0.1
      END DO
      
      RETURN
      





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

      INTEGER, DIMENSION(MAX_km__NUMBER)  :: km__ISE, km__NV

      REAL, DIMENSION(MAX_TRANSITIONS_PER_MOL, MAX_km__NUMBER) ::
     +  km__QQV, km__GGV, km__BBV, km__DDV, km__FACT

      REAL*8,  DIMENSION(MAX_LINES_TOTAL) :: km__LMBDAM
      REAL,    DIMENSION(MAX_LINES_TOTAL) :: km__SJ, km__JJ
      INTEGER, DIMENSION(MAX_LINES_TOTAL) :: km__NUMLIN


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
      INTEGER UNIT_, I, L, I_MOL, NNV, IZ, J
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


        L = (I_MOL-1)*MAX_LINES_PER_MOL+1  ! Initial index for all vectors inside this loop
        WRITE(*,*) 'LLLLLLLLLLLLL', L
        I = 1
        DO WHILE (.TRUE.)

          ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
          IF (I .GT. MAX_LINES_PER_MOL) THEN
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


          READ(UNIT_,*) km__LMBDAM(L), km__SJ(L), km__JJ(L), IZ,
     +                   km__NUMLIN(L)

          IF (km__NUMLIN(L) .EQ. 9) EXIT

          L = L+1
          I = I+1
        END DO
        WRITE (*,*) 'IIIIIIIIIIIIIIIIIIIIIIIIIIII', I
      END DO

      CLOSE(UNIT_)

      WRITE (*,*) 'NUMBER (again) --------------', km__NUMBER

      END
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

      END MODULE MOLECULA

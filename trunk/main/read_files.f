      MODULE READ_FILES


C Variables filled by READ_DISSOC() (file dissoc.dat)
C Number of elements actually used is specified by variable dissoc_NMETAL <= MAX_dissoc_NMETAL

C ISSUE: I created new variables to decouple from logic while reading this file.
C        The original mixes reading and logic, and I don't want to track down the
C        variables that are read right now. It seems, BTW, that many of them aren't used.
C ISSUE: this may be temporary, or to test not using COMMON, or remain just like this


C RESULT: common blocks are really not needed!!!

C Variables filled by READ_DISSOC() (file dissoc.dat)
C ISSUE: I find very confusing NELEMX (metal atoms) versus NELEM (molecules)
      ! dissoc.dat, metals part
      PARAMETER(MAX_dissoc_NMETAL=50)  ! Limit number of metal rows in dissoc.dat
      INTEGER*4 dissoc_NMETAL, dissoc_NIMAX, dissoc_NELEMX

      CHARACTER*2 dissoc_ELEMS
      INTEGER dissoc_IG0, dissoc_IG1
      REAL*8 dissoc_IP, dissoc_CCLOG
      DIMENSION dissoc_ELEMS(MAX_dissoc_NMETAL),   ! Only this ...
     1          dissoc_NELEMX(MAX_dissoc_NMETAL),  ! ... and this are used directly.

     2          dissoc_IP(MAX_dissoc_NMETAL),      ! These other 4 variables are used
     3          dissoc_IG0(MAX_dissoc_NMETAL),     ! for filling local variables
     4          dissoc_IG1(MAX_dissoc_NMETAL),     ! within SAT4()
     5          dissoc_CCLOG(MAX_dissoc_NMETAL)    !

      ! dissoc.dat, molecules part
      PARAMETER(MAX_dissoc_NMOL=600)  ! Limit number of molecule rows
      CHARACTER dissoc_MOL*3
      INTEGER*4 dissoc_NMOL, dissoc_MMAX, dissoc_NELEM, dissoc_NATOM
      DIMENSION dissoc_MOL(MAX_dissoc_NMOL),
     1          dissoc_C(MAX_dissoc_NMOL, 5),
     2          dissoc_MMAX(MAX_dissoc_NMOL),
     3          dissoc_NELEM(5, MAX_dissoc_NMOL),
     4          dissoc_NATOM(5, MAX_dissoc_NMOL)



C Variables filled by READ_MAIN() (file main.dat)
      CHARACTER main_TITRAV*10, main_FILEFLUX*64
      LOGICAL   main_PTDISK
      REAL*8    main_PAS, main_ECHX, main_ECHY, main_FWHM, main_VVT,
     1          main_TEFF, main_MU, main_AFSTAR, main_LLZERO,
     2          main_LLFIN, main_AINT
      INTEGER   main_IVTOT
      CHARACTER main_FILETOHY*64

      DIMENSION main_FILETOHY(10)
      DIMENSION main_TITRAV(20)
      DIMENSION main_VVT(20)
      DIMENSION main_XXCOR(MAX_dissoc_NMETAL)


C Variables filled by READ_ABONDS() (file abonds.dat)
      PARAMETER(MAX_abonds_NABOND=100)  ! Limit number of abundances in abonds.dat
      INTEGER abonds_NABOND
      CHARACTER*2 abonds_ELE
      DIMENSION abonds_ELE(MAX_abonds_NABOND),
     1          abonds_ABOL(MAX_abonds_NABOND)








      SAVE

C     ========
      CONTAINS
C     ========

C-----------------------------------------------------------------------
C READ_MAIN(): reads file main.dat to fill variables main_*
C
C IMPORTANT: Depends on variable dissoc_NMETAL (this variable is filled
C            by READ_DISSOC())
C-----------------------------------------------------------------------
      SUBROUTINE READ_MAIN(fileName)
      INTEGER UNIT_
      PARAMETER(UNIT_=4)
      CHARACTER*256 fileName

      OPEN(UNIT=UNIT_,FILE=fileName, STATUS='OLD')

C row 01: object name, e.g. "sun"
C =======
*      PRINT *,' ENTRER UN TITRE'
      READ(UNIT_, '(20A)') main_TITRAV
*      WRITE(6,'(1H1, 20X, 20A4)') TITRAV

C row 02: ???
C =======
C Example:  T      0.02 5.0   1.    .12
      READ(UNIT_, *) main_ECRIT, main_PAS, main_ECHX, main_ECHY,
     1               main_FWHM

C row 03:
C =======
C Example: 0.9

      READ(UNIT_, *) main_VVT(1)
      main_IVTOT=1
C ISSUE: it seems that here there are three conditional rows;
C ISSUE: these are present/read only if the value in the third line
C ISSUE: is greater than 900
C ISSUE: UPDATE: I checked pfantgrade.f and only VVT(1) is used
C ISSUE:         throughout the program, so I think I can safely
C ISSUE;         exclude these
C      IF(VVT(1) .GT. 900)  THEN   ! VT VARIABLE AVEC LA PROFONDEUR
C            READ(4,*) IVTOT
C            READ(4,*) (TOLV(I), I=1, IVTOT)
C            READ(4,*) (VVT(I) ,I=1, IVTOT)
C      END IF


C line 04
C =======
C Example:      5777  4.44  0       0.1  1
      READ(UNIT_, *) main_TEFF, main_GLOG, main_ASALOG, main_NHE,
     1               main_INUM
*      print *, TEFF, GLOG, ASALOG, NHE, INUM


C line 05
C =======
C Example:      F       1.
      READ(UNIT_, *) main_PTDISK, main_MU

C line 06
C =======
C Example: 0
      READ(UNIT_, *) main_AFSTAR  ! metallicity of the star (in log scale)



C line 07 -- XXCOR(I)
C =======
C ISSUE: Should be a column in dissoc.dat !!!!!
C ISSUE: Is this actually used?
      READ(UNIT_, *)(main_XXCOR(I), I=1, dissoc_NMETAL)



C line 08 -- part of filename
C =======
C This line will define the names of other three files to be read:
C   cont.<FILEFLUX>
C   norm.<FILEFLUX>
C   spec.<FILEFLUX>
      READ(UNIT_, '(A)') main_FILEFLUX



C line 09 --
C =======
C Example:      4800    4820   50
      READ(UNIT_, *) main_LLZERO, main_LLFIN, main_AINT
*711   FORMAT(/,2X,'LLZERO=',F10.5,2X,'LLFIN=',F10.5,2X,'AINT=',F8.3,/)
*      write(6,711) LLZERO,LLFIN,AINT


C rows 10-19 -- file names, in sync with variable LLHY
C ===========
C Example: thkappa
C          thiota
C          ththeta
C          theta
C          thzeta
C          thepsilon
C          thdelta
C          thgamma
C          thbeta
C          thalpha

      DO IH = 1, 10
          READ(UNIT_, '(A)') main_FILETOHY(IH)
      END DO
*1560  FORMAT(A20)


      CLOSE(UNIT=UNIT_)

      RETURN
      END





C-----------------------------------------------------------------------
C READ_ABONDS(): reads file abonds.dat to fill variables abonds_*
C
C The input file has 3 columns:
C   1) Empty space or "1"
C   2) 2-character atomic element symbol (?)
C   3) absolute abundance (a) (exponent; actual abundance is 10^a), unit "dex"
C
C The end of the file is signalled by two rows containing only "1" each at
C column 1 and nothing else at the others
C-----------------------------------------------------------------------

C PROPOSE: use READ()'s "END=" option

      SUBROUTINE READ_ABONDS(fileName)
      INTEGER UNIT_
      INTEGER FINAB
      PARAMETER(UNIT_=199)
      CHARACTER*256 fileName

      OPEN(UNIT=UNIT_,FILE=fileName, STATUS='OLD')

      J = 1
      FINAB = 0
      DO WHILE (FINAB .LT. 1)
        READ(UNIT_, '(I1,A2,F6.3)') FINAB, abonds_ELE(J), abonds_ABOL(J)
        J = J+1
      END DO
      abonds_NABOND = J-2

      CLOSE(UNIT=UNIT_)

      RETURN
      END






C-----------------------------------------------------------------------
C READ_DISSOC(): reads file dissoc.dat to fill variables dissoc_*
C
C This file must end with a blank row so that the routine can detect
C the end of the file
C-----------------------------------------------------------------------

C PROPOSE: use READ()'s "END=" option

      SUBROUTINE READ_DISSOC(fileName)
      INTEGER UNIT_
      INTEGER I
      PARAMETER(UNIT_=199)
      CHARACTER*256 fileName

      INTEGER*4 NATOMM, NELEMM
      DIMENSION NATOMM(5), NELEMM(5)  ! Auxiliary temp variables for reading file

      OPEN(UNIT=UNIT_,FILE=fileName, STATUS='OLD')

C row 01
C =======
C NMETAL - NUMBER OF ELEMENTS CONSIDERED IN CHEMICAL EQUILIBRIUM
C NIMAX  - MAXIMUM NUMBER OF ITERATION IN NEWTON-RAPSON METHOD
C EPS    - IF ABS((X(I+1)-X(I))/X(I)).LE. EPS; CONVERGED
C SWITER - IF SWITER .GT. 0;   X(I+1)=0.5*(X(I+1)+X(I))
C          IF SWITER .LE. 0;   X(I+1)=X(I+1)
C
C Example:    18  100    0.005   -1.0
      READ(UNIT_,'(2I5, 2F10.5, I10)')
     1     dissoc_NMETAL, dissoc_NIMAX, dissoc_EPS, dissoc_SWITER

C rows 2 to NMETAL+1
C ==================
C 6 columns:
C   col 1 -- symbol of chemical element
C   col 2 -- atomic number "N"
C   col 3 -- (?)
C   col 4 -- (?)
C   col 5 -- (?)
C   col 6 -- (?)
      DO I = 1, dissoc_NMETAL
        READ (UNIT_, '(A2, 2X, I6, F10.3, 2I5, F10.5)')
     1        dissoc_ELEMS(I), dissoc_NELEMX(I), dissoc_IP(I),
     2        dissoc_IG0(I), dissoc_IG1(I), dissoc_CCLOG(I)
      END DO


      WRITE(*,*) 'sadkjhfdskjhdfskjlfdshkjfdshkdjfsh'

C rows NMETAL+2 till end-of-file
C ==============================
C   col  1     -- "name" of molecule
C   cols 2-6   -- C(J, 1-5)
C   col  7     -- MMAX(J) (number of subsequent columns)/2
C   cols 8-... -- Maximum of 8 columns here.
C                 Pairs (NELEM(M), NATOM(M)), M = 1 to MMAX(J)
      J = 0
 1010 J = J + 1
C ISSUE: This 1X does not appear in my sample dissoc.dat file
C ISSUE: Atually the file that Beatriz sent me does not work under this format!!!!
C ISSUE: THere is no 1X
*      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, I1, 4(I2,I1))')
     1             dissoc_MOL(J),
     2             (dissoc_C(J, K), K=1,5),
     3             dissoc_MMAX(J),
     4             (NELEMM(M), NATOMM(M), M=1,4)


*
*        WRITE(*, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
*     1             dissoc_MOL(J),
*     2             (dissoc_C(J, K), K=1,5),
*     3             dissoc_MMAX(J),
*     4             (NELEMM(M), NATOMM(M), M=1,4)



      MMAXJ = dissoc_MMAX(J)
      IF(MMAXJ .EQ. 0) GO TO 1014  ! means end-of-file
      DO M = 1, MMAXJ
          dissoc_NELEM(M,J) = NELEMM(M)
          dissoc_NATOM(M,J) = NATOMM(M)

      END DO


*        WRITE(*, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
*     1             dissoc_MOL(J),
*     2             (dissoc_C(J, K), K=1,5),
*     3             dissoc_MMAX(J),
*     4             (dissoc_NELEM(M, J), dissoc_NATOM(M, J), M=1,4)



      GO TO 1010

 1014 dissoc_NMOL = J-1

      CLOSE(UNIT=UNIT_)

      RETURN
      END

      END MODULE READ_FILES

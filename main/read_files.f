      MODULE READ_FILES

      ! ERROR_EXCEEDED: Error code if any array index blow maximum number of elements
      !                 allowed while reading from file.
      ! ERROR_NOT_FOUND: When value from somewhere selects something that does not exist
      !                  (example in READ_MODELES()).
      ! ERROR_BAD_VALUE: generic error for bad value found inside file
      INTEGER ERROR_EXCEEDED, ERROR_NOT_FOUND, ERROR_BAD_VALUE
      PARAMETER(ERROR_EXCEEDED=111, ERROR_NOT_FOUND=112,
     +           ERROR_BAD_VALUE=113)


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
      INTEGER dissoc__IG0, dissoc__IG1
      REAL*8 dissoc__IP, dissoc__CCLOG
      DIMENSION dissoc_ELEMS(MAX_dissoc_NMETAL),   ! Only this ...
     1          dissoc_NELEMX(MAX_dissoc_NMETAL),  ! ... and this are used directly.
      ! JT2015 I introduced these variables, double underscore to emphasize that they
      !        are not just old variables that had a prefix added.
     2          dissoc__IP(MAX_dissoc_NMETAL),      ! These other 4 variables are used
     3          dissoc__IG0(MAX_dissoc_NMETAL),     ! for filling local variables
     4          dissoc__IG1(MAX_dissoc_NMETAL),     ! within SAT4()
     5          dissoc__CCLOG(MAX_dissoc_NMETAL)    !

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
      LOGICAL   main_ECRIT, main_PTDISK
      REAL*8    main_PAS, main_ECHX, main_ECHY, main_FWHM, main_VVT,
     1          main_MU, main_AFSTAR, main_LLZERO,
     2          main_LLFIN, main_AINT,
     3          main_TEFF, main_GLOG, main_ASALOG, main_NHE
      INTEGER   main_IVTOT, main_INUM
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




C Variables filled by READ_ATOMGRADE() (file atomgrade.dat)

      ! Half of the maximum the number of rows in atomgrade.dat
      PARAMETER(MAX_atomgrade__NBLEND=13000)

      ! Maximum number of spectral lines possible within the interval LZERO, LFIN
      PARAMETER(MAX_atomgrade_NBLEND=8000)

      INTEGER*4 atomgrade_NBLEND, atomgrade__NBLEND
      CHARACTER*2 atomgrade_ELEM, atomgrade__ELEM
      INTEGER*4 atomgrade_IONI, atomgrade__IONI

      ! Only these variables had their sizes specified originally


C ISSUE: Can I just declare everything as DOUBLE PRECISION (by default!!)??
      REAL atomgrade_KIEX, atomgrade__KIEX
      REAL*8 atomgrade_GR, atomgrade_LAMBDA, atomgrade__LAMBDA


      ! Filtered variables
C ISSUE: nobody cares about ABONDR
      DIMENSION atomgrade_ELEM(MAX_atomgrade_NBLEND),
     1          atomgrade_IONI(MAX_atomgrade_NBLEND),
     2          atomgrade_LAMBDA(MAX_atomgrade_NBLEND),
     3          atomgrade_KIEX(MAX_atomgrade_NBLEND),
     4          atomgrade_ALGF(MAX_atomgrade_NBLEND),
     5          atomgrade_GF(MAX_atomgrade_NBLEND),
     6          atomgrade_CH(MAX_atomgrade_NBLEND),
     7          atomgrade_GR(MAX_atomgrade_NBLEND),
     8          atomgrade_GE(MAX_atomgrade_NBLEND),
     9          atomgrade_ZINF(MAX_atomgrade_NBLEND),
     +          atomgrade_ABONDR(MAX_atomgrade_NBLEND)

      ! File originals
      DIMENSION atomgrade__ELEM(MAX_atomgrade__NBLEND),
     1          atomgrade__IONI(MAX_atomgrade__NBLEND),
     2          atomgrade__LAMBDA(MAX_atomgrade__NBLEND),
     3          atomgrade__KIEX(MAX_atomgrade__NBLEND),
     4          atomgrade__ALGF(MAX_atomgrade__NBLEND),
     5          atomgrade__CH(MAX_atomgrade__NBLEND),
     6          atomgrade__GR(MAX_atomgrade__NBLEND),
     7          atomgrade__GE(MAX_atomgrade__NBLEND),
     8          atomgrade__ZINF(MAX_atomgrade__NBLEND),
     9          atomgrade__ABONDR(MAX_atomgrade__NBLEND)



C Variables filled by READ_PARTIT() (file partit.dat)
C ---------------------------------------------------
      ! Limit number of "items" in partit.dat:
      ! - Maximum value for partit_NPAR
      ! - Second dimension of partit_TABU
      PARAMETER(MAX_partit_NPAR=85)
      ! Third dimension of partit_TABU
      PARAMETER(MAX_partit_KMAX=63)

      CHARACTER*2  partit_EL
      INTEGER partit_NPAR, partit_JKMAX

      DIMENSION partit_EL(MAX_partit_NPAR),
     1           partit_TINI(MAX_partit_NPAR),
     2           partit_PA(MAX_partit_NPAR),
     3           partit_JKMAX(MAX_partit_NPAR),
     4           partit_TABU(MAX_partit_NPAR, 3, MAX_partit_KMAX),
     5           partit_M(MAX_partit_NPAR),
     6           partit_KI1(MAX_partit_NPAR),
     7           partit_KI2(MAX_partit_NPAR)






C Variables filled by READ_ABSORU2() (file absoru2.dat)
C ---------------------------------------------------
      ! Maximum value for absoru_NM
      PARAMETER(MAX_absoru2_NM=30)
      ! Maximum value for absoru2_NR(J)
      PARAMETER(MAX_absoru2_NRR=9)
      ! Maximum value for each element of absoru2_NUMSET
      PARAMETER(MAX_absoru2_NUMSET_I=41)

      INTEGER absoru2_NM, absoru2_NMETA, absoru2_NUMSET, absoru2_NR
      REAL*8 absoru2_ABMET, absoru2_ABHEL
      CHARACTER*4 absoru2_TITRE, absoru2_IUNITE  ! ISSUE: I am not sure about this, made this from the FORMAT below

      DIMENSION  absoru2_IUNITE(2), absoru2_TITRE(17),
     1           absoru2_NR(MAX_absoru2_NM),
     2           absoru2_ZP(MAX_absoru2_NM),
     3           absoru2_ZM(MAX_absoru2_NM),
     4           absoru2_XI(MAX_absoru2_NM, MAX_absoru2_NRR),
     5           absoru2_PF(MAX_absoru2_NM, MAX_absoru2_NRR),
     6           absoru2_NOMET(MAX_absoru2_NM)

      DIMENSION absoru2_WI(MAX_absoru2_NUMSET_I, 2), absoru2_NUMSET(2)






C Variables filled by READ_MODELE() (file modeles.mod)
C ----------------------------------------------------
      ! Maximum possible value of modeles_NTOT
      PARAMETER(MAX_modeles_NTOT=50)

      ! Attention: one has to specify sizes of all the variables here, because
      ! this may change with compiler
      INTEGER*4 modeles_NTOT
      CHARACTER*4 modeles_TIT
      CHARACTER*20 modeles_TIABS ! I just want to see this string at testing
      REAL*4 modeles_DETEF, modeles_DGLOG, modeles_DSALOG,
     +       modeles_ASALALF, modeles_NHE,
     +       modeles_NH, modeles_TETA, modeles_PE, modeles_PG,
     +       modeles_T5L

      DIMENSION modeles_NH(MAX_modeles_NTOT),
     +           modeles_TETA(MAX_modeles_NTOT),
     +           modeles_PE(MAX_modeles_NTOT),
     +           modeles_PG(MAX_modeles_NTOT),
     +           modeles_T5L(MAX_modeles_NTOT),
     +           BID(16),   ! ISSUE: I counted 12... let's see, I have to see INEWMARCS... actually, it seems that it should be 12!!
     +           modeles_TIT(5)



      SAVE


C     ========
      CONTAINS
C     ========



C================================================================================================================================
C-----------------------------------------------------------------------
C READ_MAIN(): reads file main.dat to fill variables main_*
C
C Original UNIT: 4
C
C IMPORTANT: Depends on variable dissoc_NMETAL (this variable is filled
C            by READ_DISSOC())
C-----------------------------------------------------------------------
      SUBROUTINE READ_MAIN(filename)
      INTEGER UNIT_
      PARAMETER(UNIT_=4)
      CHARACTER*256 filename

      OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

C row 01: object name, e.g. "sun"
C =======
*      PRINT *,' ENTRER UN TITRE'
      READ(UNIT_, '(20A)') main_TITRAV

C row 02: ???
C =======
C Example:  T      0.02 5.0   1.    .12
      READ(UNIT_, *) main_ECRIT, main_PAS, main_ECHX, main_ECHY,
     1               main_FWHM

C row 03:
C =======
C Example: 0.9

      READ(UNIT_, *) main_VVT(1)
      main_IVTOT = 1
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
C     AINT =intervalle de calcul

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





C================================================================================================================================
C-----------------------------------------------------------------------
C READ_ABONDS(): reads file abonds.dat to fill variables abonds_*
C
C Original UNIT: 30
C
C The input file has 3 columns:
C   1) Empty space or "1"
C   2) 2-character atomic element symbol (?)
C   3) absolute abundance (a) (exponent; actual abundance is 10^a), unit "dex"
C
C The end of the file is signalled by two rows containing only "1" each at
C column 1 and nothing else at the others
C-----------------------------------------------------------------------


C TODO Test one row!!!

C PROPOSE: use READ()'s "END=" option

      SUBROUTINE READ_ABONDS(filename)
      INTEGER UNIT_
      INTEGER FINAB
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename

      OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

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






C================================================================================================================================
C-----------------------------------------------------------------------
C READ_DISSOC(): reads file dissoc.dat to fill variables dissoc_*
C
C Original UNIT: 23
C
C This file must end with a blank row so that the routine can detect
C the end of the file
C-----------------------------------------------------------------------


C TODO Various tests:
C TODO - mismatched NMETAL and metal rows
C TODO - check if NMETAL and NMOL match what they are supposed to (assertions in test)

C PROPOSE: use READ()'s "END=" option

      SUBROUTINE READ_DISSOC(filename)
      INTEGER UNIT_
      INTEGER I
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename

      ! Auxiliary temp variables for reading file
      INTEGER*4 NATOMM, NELEMM
      DIMENSION NATOMM(5), NELEMM(5)

      OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

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
     1        dissoc_ELEMS(I), dissoc_NELEMX(I), dissoc__IP(I),
     2        dissoc__IG0(I), dissoc__IG1(I), dissoc__CCLOG(I)
      END DO

C rows NMETAL+2 till end-of-file
C ==============================
C   col  1     -- "name" of molecule
C   cols 2-6   -- C(J, 1-5)
C   col  7     -- MMAX(J) (number of subsequent columns)/2
C   cols 8-... -- Maximum of 8 columns here.
C                 Pairs (NELEM(M), NATOM(M)), M = 1 to MMAX(J)
      J = 0
 1010 J = J+1
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






C================================================================================================================================
C--------------------------------------------------------------------------
C READ_ATOMGRADE(): reads file atomgrade.dat to fill variables atomgrade__* (double underscore)
C
C Original UNIT: 14
C
C This file has 2 types of alternating rows:
C   odd row
C     col 1 -- 2-letter atomgrade_ELEM(K) FILLDOC
C     col 2 -- atomgrade_IONI(K) FILLDOC
C     col 3 -- atomgrade_LAMBDA(K) FILLDOC
C   even row
C     col 1 --
C     col 2 --
C     col 3 --
C     col 4 --
C     col 5 --
C     col 6 --
C     col 7 --
C     col 8 -- signals end-of-file. If "1", reading stops
C-------------------------------------------------------------------------


C TODO: give error if blows MAX!!!!!!!!!!!!!!!!!
C TODO: including other reading routines!!!!!!!!!!!!!!

C ISSUE: Cannot cope with empty file (tested), however there are if's inside the program: IF NBLEND .EQ. 0 .........

C TODO Assertions in test to see if numbers match what they are supposed to



C PROPOSE: use READ()'s "END=" option

      SUBROUTINE READ_ATOMGRADE(filename)
      INTEGER UNIT_
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename

      INTEGER FINRAI, K

      OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

C orig ******************************************************************
C orig                      V
C orig     QUANTITES DEPENDANT DE LA RAIE ET DU MODELE
C orig *******************************************************************

      K = 1

9     READ(UNIT_, '(A2, I1, 1X, F10.3)') atomgrade__ELEM(K),
     1                                   atomgrade__IONI(K),
     2                                   atomgrade__LAMBDA(K)

      READ(UNIT_, *) atomgrade__KIEX(K),
     1               atomgrade__ALGF(K),
     2               atomgrade__CH(K),
     3               atomgrade__GR(K),
     4               atomgrade__GE(K),
     5               atomgrade__ZINF(K),
     6               atomgrade__ABONDR(K), FINRAI

      IF (atomgrade_GR(K) .LT. 1E-37)
     1    atomgrade_GR(K) = 2.21E15 / atomgrade_LAMBDA(K)**2

      IF (FINRAI .EQ. 1) GO TO 10
      K = K+1

          ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
          IF (K .GT. MAX_atomgrade__NBLEND) THEN
            WRITE(*,*) 'READ_ATOMGRADE(): exceeded maximum of',
     1                 MAX_atomgrade__NBLEND, ' spectral lines'
            STOP ERROR_EXCEEDED
          END IF

      GO TO 9

10    atomgrade__NBLEND = K  ! ISSUE: check this K, K-1, if ignoring last row was on purpose (no longer ignoring)

      CLOSE(UNIT=UNIT_)

      RETURN

      END

C ISSUE: I gotta check with BLB, it seems that the last 2 rows were being ignored, look:

*      K=1
*9     READ(14,103)atomgrade_ELEM(K),atomgrade_IONI(K),atomgrade_LAMBDA(K)
*      READ(14,*) KIEX(K),ALGF(K),CH(K),GR(K),GE(K),ZINF(K),
*     1 ABONDR(K),FINRAI
*      write(34,103)atomgrade_ELEM(K),atomgrade_IONI(K),atomgrade_LAMBDA(K)
*      GF(K)=10.**ALGF(K)
*C        IF(K.EQ.1) GF(K)=10**AGGF
*      IF(GR(K).LT.1E-37)   GR(K)=2.21E15 / atomgrade_LAMBDA(K)**2
*      IF(FINRAI.EQ.1) GO TO 10
*      IF(((atomgrade_LAMBDA(K).GT.LFIN).OR.(atomgrade_LAMBDA(K).LT.LZERO))) GO TO 205
*      K=K+1
*205   CONTINUE
*      GO TO 9
*10    NBLEND=K-1




C-------------------------------------------------------------------------
C FILTER_ATOMGRADE(): selects only spectral lines within range LZERO, LFIN
C
C Populates variables atomgrade_* (single underscore)
C-------------------------------------------------------------------------
      SUBROUTINE FILTER_ATOMGRADE(LZERO, LFIN)
      REAL*8 LZERO, LFIN

      K = 0
      DO J = 1, atomgrade__NBLEND
        IF((atomgrade__LAMBDA(J).LE.LFIN) .AND.
     1     (atomgrade__LAMBDA(J) .GE. LZERO)) THEN
          K = K+1


          ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
          IF (K .GT. MAX_atomgrade_NBLEND) THEN
            WRITE(*,*) 'FILTER_ATOMGRADE(): exceeded maximum of',
     1                 MAX_atomgrade_NBLEND, ' spectral lines'
            STOP ERROR_EXCEEDED
          END IF
          !Filters in!
          atomgrade_ELEM(K)   = atomgrade__ELEM(J)
          atomgrade_IONI(K)   = atomgrade__IONI(J)
          atomgrade_LAMBDA(K) = atomgrade__LAMBDA(J)
          atomgrade_KIEX(K)   = atomgrade__KIEX(J)
          atomgrade_ALGF(K)   = atomgrade__ALGF(J)
          atomgrade_GF(K)     = 10.**atomgrade__ALGF(J)
          atomgrade_CH(K)     = atomgrade__CH(J)
          atomgrade_GR(K)     = atomgrade__GR(J)
          atomgrade_GE(K)     = atomgrade__GE(J)
          atomgrade_ZINF(K)   = atomgrade__ZINF(J)
          atomgrade_ABONDR(K) = atomgrade__ABONDR(J)
        END IF
      END DO

      atomgrade_NBLEND = K

      END









C================================================================================================================================
C-------------------------------------------------------------------------
C READ_PARTIT(): reads file partit.dat to fill variables partit_*
C
C orig "LECTURE DES FCTS DE PARTITION"
C
C Rows in this file alternate between:
C 1) 8-column row
C    col 8 -- signals end-of-file. If 1, it ignores the row and
C             stops reading
C
C 2) Series of rows to fill in partit_TABU(J, :, :)
C
C-------------------------------------------------------------------------

      SUBROUTINE READ_PARTIT(filename)
      INTEGER UNIT_
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename

      INTEGER FINPAR, J

      OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')


      J = 1
      FINPAR = 0
      DO WHILE (FINPAR .LT. 1)
        READ (UNIT_, '(A2, 2F5.2, I3, 3F10.2, 34X, I1)')
     1         partit_EL(J),
     2         partit_TINI(J),
     3         partit_PA(J),
     4         partit_JKMAX(J),
     5         partit_M(J),
     6         partit_KI1(J),
     7         partit_KI2(J), FINPAR

        IF (FINPAR .NE. 1) THEN

          ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
          IF (J .GT. MAX_partit_NPAR) THEN
            WRITE(*,*) 'READ_PARTIT(): PAR exceeded maximum of ',
     1                  MAX_partit_NPAR
            STOP ERROR_EXCEEDED
          END IF


          KMAX = partit_JKMAX(J)

          ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
          IF (KMAX .GT. MAX_partit_KMAX) THEN
            WRITE(*,*) 'READ_PARTIT(): PAR number', J, 'KMAX=', KMAX,
     1       ' exceeded maximum of', MAX_partit_KMAX
            STOP ERROR_EXCEEDED
          END IF


          READ(UNIT_, '(13F6.4)')
     1         ((partit_TABU(J, L, K), L=1, 3), K=1, KMAX)

          J = J+1
        END IF
      END DO

      partit_NPAR = J-1

      RETURN
      END
























C================================================================================================================================
C-------------------------------------------------------------------------
C READ_ABSORU2(): reads file absoru2.dat to fill variables absoru2_*
C
C Original UNIT: 15
C
C Attention: variables absoru2_ZP, absoru2_XI, and absoru2_PF
C            undergo transformation!
C
C Obs: this routine is in essence the old routine called "LECTUR"
C
C I think SSP means "SubProgram" but it is a typo with an additional "S"
C
C orig CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
C orig PUIS LES DONNEES CORRESPONDANTS AUX ABSORBANTS METALLIQUES SI NECE
C orig CALMET=1 SI ON NE TIENT PAS COMPTE DES METAUX
C orig CALMET=2 SI ON    TIENT     COMPTE DES METAUX
C
C ISSUE: see observation below
C Obs: as opposed to original description, CALMET was being ignored in
C      original routine LECTUR()
C-------------------------------------------------------------------------

      SUBROUTINE READ_ABSORU2(filename)
      INTEGER UNIT_
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename

      CHARACTER*3 NEANT
      INTEGER NION


      OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

      ! orig ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
      ! orig ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
      READ (UNIT_,'(2E15.7)') absoru2_ABMET, absoru2_ABHEL


      ! orig NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISA
      ! orig NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
      ! orig IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
      ! orig IUNITE=' NOYAU H'  ''    ''    ''       ''      NOYAU D'HYDROGENE
      READ (UNIT_,'(2I2, 19A4)') absoru2_NM, absoru2_NMETA,
     1      (absoru2_IUNITE(I),I=1,2), (absoru2_TITRE(I),I=1,17)


      ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
      IF (absoru2_NM .GT. MAX_absoru2_NM) THEN
        WRITE(*,*) 'READ_ABSORU2(): NM=', absoru2_NM,
     1        ' exceeded maximum of', MAX_absoru2_NM
        STOP ERROR_EXCEEDED
      END IF


      ! orig LECTURE DE LA TABLE D'IONISATION CHOISIE
      ! orig ----------------------------------------
      DO J = 1, absoru2_NM
        READ (UNIT_, '(3X,I3,2E16.5)') absoru2_NR(J), absoru2_ZP(J),
     1                                 absoru2_ZM(J)
        absoru2_ZP(J) = 10**absoru2_ZP(J)

        ! orig    NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
        ! orig    ZP=NBR. D'ABONDANCE DE L'ELEMENT
        ! orig    ZM=POIDS MOLECULAIRE DE L'ELEMENT
        NRR = absoru2_NR(J)

        ! *BOUNDARY CHECK*: Checks if exceeds maximum number of elements allowed
        IF (NRR .GT. MAX_absoru2_NRR) THEN
          WRITE(*,*) 'READ_ABSORU2(): J = ', J, 'NR=', NRR,
     1       ' exceeded maximum of', MAX_absoru2_NRR
          STOP ERROR_EXCEEDED
        END IF


        DO I = 1, NRR
          ! neant="nothing"
          ! NION is also not used
          ! ISSUE: NOMET is not used in the program
          READ (UNIT_, '(A3,A2,I1,2E16.5)') NEANT, absoru2_NOMET(J),
     1           NION, absoru2_XI(J,I), absoru2_PF(J,I)

          ! orig ON LIT NR CARTES CONTENANT CHACUNE LE POTENTIEL D'IONISATION ET LA
          ! orig FONCTION DE PARTITION(LOG10(2UI+1)/UI)DE CHAQUE DEGRE D'IONISATION
          ! orig CES VALEURS SONT LUES DANS L'ORDRE CROISSANT DU DEGRE D'IONISATION
          ! orig NOMET  =NOM DE L'ELEMENT
          ! orig NION   =SON ETAT D'IONISATION
          ! orig XI(J,I)=POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATIO
          ! orig PF(J,I)=FONCTION DE PARTITION         ''   ''     ''      ''   ''

          absoru2_XI(J, I) = absoru2_XI(J, I)*2.302585
          absoru2_PF(J, I) = absoru2_PF(J, I)*2.302585
        END DO
      END DO


      READ (UNIT_, '(2I2)') (absoru2_NUMSET(ITH), ITH=1,2)

C ISSUE: I am not sure if this last part is being read correcly. Perhaps I didn't de-spag right. Anyway, the test verbose is not good.


      ! *BOUNDARY CHECK*: Checks if exceeds maximum number of elements allowed
      IF (absoru2_NUMSET(1) .GT. MAX_absoru2_NUMSET_I) THEN
        WRITE(*,*) 'READ_ABSORU2(): NUMSET(1) = ', absoru2_NUMSET(1),
     1         ' exceeded maximum of', MAX_absoru2_NUMSET_I
        STOP ERROR_EXCEEDED
      END IF
      ! *BOUNDARY CHECK*: Checks if exceeds maximum number of elements allowed
      IF (absoru2_NUMSET(2) .GT. MAX_absoru2_NUMSET_I) THEN
        WRITE(*,*) 'READ_ABSORU2(): NUMSET(2) = ', absoru2_NUMSET(2),
     1         ' exceeded maximum of', MAX_absoru2_NUMSET_I
        STOP ERROR_EXCEEDED
      END IF


      ! orig NUMSET=NBR.DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
      ! orig POUR H,HE ET HE+
      ! orig PREMIERE LISTE POUR TH.LE.0.8  ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
      DO ITH = 1,2
        NSET = absoru2_NUMSET(ITH)
        READ (UNIT_,'(8F10.1)') (absoru2_WI(I,ITH),I=1,NSET)
      END DO

      RETURN
      END















C================================================================================================================================
C-------------------------------------------------------------------------
C READ_MODELE(): reads single record from file modeles.mod into
C                 variables modeles_*
C
C Original UNIT: 18
C
C orig SI L ON DESIRE IMPOSER UN MODELE  ON MET EN INUM LE NUM DU MODELE
C orig SUR LE FICHIER ACCES DIRECT
C
C-------------------------------------------------------------------------

C Depends on main_INUM
C ISSUE: depends on other main_* but I think not for long

      SUBROUTINE READ_MODELE(filename)
      INTEGER UNIT_
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename


      OPEN(UNIT=UNIT_, ACCESS='DIRECT',STATUS='OLD',
     1     FILE=filename, RECL=1200)

      ID_ = 1

      IF (main_INUM .GT. 0) ID_ = main_INUM  ! Selects record number

C ISSUE: Variable NHE read again from a different file!!!!!!!!! I decided to opt for modeles_NHE because it overwrites main_NHE


9     READ(UNIT_, REC=ID_) modeles_NTOT, modeles_DETEF, modeles_DGLOG,
     +      modeles_DSALOG, modeles_ASALALF, modeles_NHE, modeles_TIT,
     +      modeles_TIABS
      IF (modeles_NTOT .EQ. 9999) THEN
        ! ISSUE perhaps I should check the condition that leads to this error
        ! TODO STOP with error level
        WRITE(6, *) 'LE MODELE DESIRE NE EST PAS SUR LE FICHIER'
        STOP ERROR_NOT_FOUND
      END IF


      ! *BOUNDARY CHECK*: Checks if exceeds maximum number of elements allowed
      IF (modeles_NTOT .GT. MAX_modeles_NTOT) THEN
        WRITE(*,*) 'READ_MODEABSORU2(): NUMSET(1) = ',absoru2_NUMSET(1),
     1         ' exceeded maximum of', MAX_absoru2_NUMSET_I
        STOP ERROR_EXCEEDED
      END IF



      WRITE(6, *) 'modeles_DETEF', modeles_DETEF
      WRITE(*, *) 'modeles_DGLOG', modeles_DGLOG
      WRITE(*, *) 'modeles_DSALOG', modeles_DSALOG


      DDT  = ABS(main_TEFF-modeles_DETEF)
      DDG = ABS(main_GLOG-modeles_DGLOG)
      DDAB = ABS(main_ASALOG-modeles_DSALOG)

      ! ISSUE: Variable DNHE does not exist!!! Anyway, it is not used
      ! ISSUE: this seems to be some kind of error check, but will loop forever, better to place it outside, also because of dependence on main_* variables
      DDHE= ABS(modeles_NHE-DNHE)

      ! ISSUE: I don't get this; it will keep looping forever???
      IF(DDT .GT. 1.0) THEN
        WRITE(*,*) 'ABS(main_TEFF-modeles_DETEF) = ', DDT, ' > 1.0'
        STOP ERROR_BAD_VALUE
      END IF
      IF(DDG .GT. 0.01) THEN
        WRITE(*,*) 'ABS(main_GLOG-modeles_DGLOG) = ', DDG, ' > 0.01'
        STOP ERROR_BAD_VALUE
      END IF
      IF(DDAB .GT. 0.01) THEN
        WRITE(*,*) 'ABS(main_ASALOG-modeles_DSALOG) = ', DDAB, ' > 0.01'
        STOP ERROR_BAD_VALUE
      END IF



      READ(UNIT_, REC=ID_) BID,
     +     (modeles_NH(I),
     +      modeles_TETA(I),
     +      modeles_PE(I),
     +      modeles_PG(I),
     +      modeles_T5L(I), I=1,modeles_NTOT)


      CLOSE(UNIT_)
      END


      END MODULE READ_FILES

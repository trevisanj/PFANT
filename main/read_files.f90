! This file is part of PFANT.
!
! PFANT is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Foobar is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with PFANT.  If not, see <http://www.gnu.org/licenses/>.

MODULE READ_FILES
  IMPLICIT NONE




!> Variables filled by READ_DISSOC() (file dissoc.dat)
!> ISSUE: I find very confusing NELEMX (metal atoms) versus NELEM (molecules)

!> Number of elements actually used is specified by variable dissoc_NMETAL <= MAX_dissoc_NMETAL
      INTEGER, PARAMETER :: MAX_dissoc_NMETAL=50  ! Limit number of metal rows in dissoc.dat


      INTEGER, PARAMETER :: MAX_Z = 100  ! Maximum atomic number that can be found in dissoc.dat

      !> Maximum possible value of modeles_NTOT
      INTEGER, PARAMETER :: MAX_modeles_NTOT=50



      ! dissoc.dat, metals part
      INTEGER dissoc_NMETAL, dissoc_NIMAX, dissoc_NELEMX
      CHARACTER*2 dissoc_ELEMS
      INTEGER dissoc__IG0, dissoc__IG1
      REAL dissoc__IP, dissoc__CCLOG
      DIMENSION dissoc_ELEMS(MAX_dissoc_NMETAL),  & ! Only this ...
                dissoc_NELEMX(MAX_dissoc_NMETAL), & ! ... and this are used directly.
      ! JT2015 I introduced these variables, double underscore to emphasize that they
      !        are not just old variables that had a prefix added.
                dissoc__IP(MAX_dissoc_NMETAL),    & ! These other 4 variables are used
                dissoc__IG0(MAX_dissoc_NMETAL),   & ! for filling local variables
                dissoc__IG1(MAX_dissoc_NMETAL),   & ! within SAT4()
                dissoc__CCLOG(MAX_dissoc_NMETAL)    !

      ! dissoc.dat, molecules part
      INTEGER, PARAMETER:: MAX_dissoc_NMOL=600  ! Limit number of molecule rows ISSUE: bit overdimensioned?? (considering the file has only about 30 molecules)
      CHARACTER dissoc_MOL*3
      INTEGER dissoc_NMOL, dissoc_MMAX, dissoc_NELEM, dissoc_NATOM
      REAL dissoc_C, dissoc_EPS, dissoc_SWITER
      DIMENSION dissoc_MOL(MAX_dissoc_NMOL), &
                dissoc_C(MAX_dissoc_NMOL, 5), &
                dissoc_MMAX(MAX_dissoc_NMOL), &
                dissoc_NELEM(5, MAX_dissoc_NMOL), &
                dissoc_NATOM(5, MAX_dissoc_NMOL)





!> Variables filled by READ_MAIN() (file main.dat)
      CHARACTER main_TITRAV*10, main_FILEFLUX*64
      LOGICAL   main_ECRIT, main_PTDISK
      REAL*8    main_PAS, main_ECHX, main_ECHY, main_FWHM, &
                main_MU, main_AFSTAR, main_LLZERO, &
                main_LLFIN, main_AINT, &
                main_TEFF, main_GLOG, main_ASALOG, main_NHE
      INTEGER   main_IVTOT, & ! affects TURBUL() ISSUE what
                              ! = 1 -- "VT" constant
                              ! > 1 -- "VT" variable
                main_INUM
      CHARACTER main_FILETOHY*64

      DIMENSION main_FILETOHY(10)
      DIMENSION main_TITRAV(20)
      REAL*8, DIMENSION(MAX_modeles_NTOT) :: main_VVT, main_TOLV
      REAL*8, DIMENSION(MAX_dissoc_NMETAL) :: main_XXCOR   ! This vector goes along with dissoc_ELEMS and dissoc_NELEMX


!> Variables filled by READ_ABONDS() (file abonds.dat)
      INTEGER, PARAMETER :: MAX_abonds_NABOND=100  ! Limit number of abundances in abonds.dat
      INTEGER abonds_NABOND
      CHARACTER*2 abonds_ELE
      REAL*8 abonds_ABOL, abonds_ABO
      DIMENSION abonds_ELE(MAX_abonds_NABOND), &
                abonds_ABOL(MAX_abonds_NABOND), &
                abonds_ABO(MAX_abonds_NABOND)   ! This is calculated





!> Variables filled by READ_ATOMGRADE() (file atomgrade.dat)


      INTEGER, PARAMETER :: &
       MAX_atomgrade__NBLEND=13000, & ! Half of the maximum the number of rows in atomgrade.dat
       MAX_atomgrade_NBLEND=8000      ! Maximum number of spectral lines possible within the interval LZERO, LFIN

      INTEGER*4 atomgrade_NBLEND, atomgrade__NBLEND
      CHARACTER*2 atomgrade_ELEM, atomgrade__ELEM
      INTEGER*4 atomgrade_IONI, atomgrade__IONI

      ! Only these variables had their sizes specified originally


!> ISSUE: Can I just declare everything as DOUBLE PRECISION (by default!!)??
!> (MT) Yes and modeles.mod could become an ASCII file!!!
      REAL atomgrade_KIEX, atomgrade__KIEX
      REAL*8 atomgrade_GR, atomgrade_LAMBDA, atomgrade__LAMBDA


!> ISSUE: nobody cares about ABONDR_DUMMY. Why? I might have the answer, this is taken from elsewere, file abonds.dat, maybe


      ! Filtered variables
      REAL, DIMENSION(MAX_atomgrade__NBLEND) :: &
       atomgrade_GE, atomgrade_ZINF, atomgrade_ABONDR_DUMMY, atomgrade_ALGF, &
       atomgrade_GF, atomgrade_CH
      REAL*8 atomgrade_ABONDS_ABO
      DIMENSION atomgrade_ELEM(MAX_atomgrade_NBLEND), &
                atomgrade_IONI(MAX_atomgrade_NBLEND), &
                atomgrade_LAMBDA(MAX_atomgrade_NBLEND), &
                atomgrade_KIEX(MAX_atomgrade_NBLEND), &
                atomgrade_GR(MAX_atomgrade_NBLEND), &
                atomgrade_ABONDS_ABO(MAX_atomgrade_NBLEND)


      ! File originals
      REAL*8, DIMENSION(MAX_atomgrade__NBLEND) :: atomgrade__ABONDR_DUMMY, &
       atomgrade__ALGF, atomgrade__CH, atomgrade__GE, atomgrade__ZINF, &
       atomgrade__GR
      REAL*8 atomgrade__ABONDS_ABO
      DIMENSION atomgrade__ELEM(MAX_atomgrade__NBLEND), &
                atomgrade__IONI(MAX_atomgrade__NBLEND), &
                atomgrade__LAMBDA(MAX_atomgrade__NBLEND), &
                atomgrade__KIEX(MAX_atomgrade__NBLEND), &
                atomgrade__ABONDS_ABO(MAX_atomgrade__NBLEND)  ! will be filled by searching atomgrade__ELEM within abonds_ELE



!> Variables filled by READ_PARTIT() (file partit.dat)
!> ---------------------------------------------------
      ! Limit number of "items" in partit.dat:
      ! - Maximum value for partit_NPAR
      ! - Second dimension of partit_TABU
      INTEGER, PARAMETER :: MAX_partit_NPAR=85
      ! Third dimension of partit_TABU
      INTEGER, PARAMETER :: MAX_partit_KMAX=63

      CHARACTER*2  partit_EL
      INTEGER partit_NPAR

      REAL, DIMENSION (MAX_partit_NPAR) :: partit_KI1, partit_PA, &
       partit_M, partit_TINI, partit_KI2
      INTEGER, DIMENSION (MAX_partit_NPAR) :: partit_JKMAX
      REAL partit_TABU
      DIMENSION partit_EL(MAX_partit_NPAR), &
                partit_TABU(MAX_partit_NPAR, 3, MAX_partit_KMAX)







!> Variables filled by READ_ABSORU2() (file absoru2.dat)
!> ---------------------------------------------------
      ! Maximum value for absoru_NM
      INTEGER, PARAMETER :: MAX_absoru2_NM=30
      ! Maximum value for absoru2_NR(J)
      INTEGER, PARAMETER :: MAX_absoru2_NRR=9
      ! Maximum value for each element of absoru2_NUMSET
      INTEGER, PARAMETER :: MAX_absoru2_NUMSET_I=41

      INTEGER absoru2_NM, absoru2_NMETA, absoru2_NUMSET, absoru2_NR, absoru2_NOMET
      REAL*8 absoru2_ABMET, absoru2_ABHEL
      REAL absoru2_PF, absoru2_WI, absoru2_XI, absoru2_ZM, absoru2_ZP
      CHARACTER*4 absoru2_TITRE, absoru2_IUNITE  ! ISSUE: I am not sure about this, made this from the FORMAT below

      DIMENSION  absoru2_IUNITE(2), absoru2_TITRE(17), &
                 absoru2_NR(MAX_absoru2_NM), &
                 absoru2_ZP(MAX_absoru2_NM), &
                 absoru2_ZM(MAX_absoru2_NM), &
                 absoru2_XI(MAX_absoru2_NM, MAX_absoru2_NRR), &
                 absoru2_PF(MAX_absoru2_NM, MAX_absoru2_NRR), &
                 absoru2_NOMET(MAX_absoru2_NM)

      DIMENSION absoru2_WI(MAX_absoru2_NUMSET_I, 2), absoru2_NUMSET(2)





!> Variables filled by READ_MODELE() (file modeles.mod)
!> ----------------------------------------------------

      ! Attention: one has to specify sizes of all the variables here, because
      ! this may change with compiler
      INTEGER*4 modeles_NTOT
      CHARACTER*4 modeles_TIT
      CHARACTER*20 modeles_TIABS ! I just want to see this string at testing

      ! TODO create REAL*4 temp locals, then transfer to REAL*8 ones
      REAL*4 modeles_DETEF, modeles_DGLOG, modeles_DSALOG, &
             modeles_ASALALF, modeles_NHE, &
             modeles_NH, modeles_TETA, modeles_PE, modeles_PG, &
             modeles_T5L, BID

      DIMENSION modeles_NH(MAX_modeles_NTOT), &
                modeles_TETA(MAX_modeles_NTOT), &
                modeles_PE(MAX_modeles_NTOT), &
                modeles_PG(MAX_modeles_NTOT), &
                modeles_T5L(MAX_modeles_NTOT), &
                BID(16), &  ! ISSUE: I counted 12... let's see, I have to see INEWMARCS... actually, it seems that it should be 12!! &
                modeles_TIT(5)





      ! Flags indicating whether corresponding routine has already been called.
      ! There is a strict order in which files must be read, because there are
      ! consistency checks and inner joins being percormed while reading files.
      ! At the moment, this order is reinforces: DISSOC -> MAIN -> ABONDS -> ATOMGRADE
      LOGICAL, PRIVATE :: FLAG_READ_ABONDS = .FALSE., &
                          FLAG_READ_MAIN   = .FALSE., &
                          FLAG_READ_DISSOC = .FALSE.



      SAVE

CONTAINS



!================================================================================================================================
!> READ_MAIN(): reads file main.dat to fill variables main_*
!>
!> Original UNIT: 4
!>
!> IMPORTANT: Depends on variable dissoc_NMETAL (this variable is filled
!>            by READ_DISSOC())

  SUBROUTINE READ_MAIN(filename)
    IMPLICIT NONE
    INTEGER UNIT_
    PARAMETER(UNIT_=4)
    CHARACTER(LEN=*) :: filename
    INTEGER IH, I
    CHARACTER*80 LLL

    IF (.NOT. FLAG_READ_DISSOC) THEN
      CALL PFANT_HALT('READ_DISSOC() must be called before READ_MAIN()')
    END IF

    OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

    !> row 01: object name, e.g. "sun"
    !> =======
    READ(UNIT_, '(20A)') main_TITRAV

    !> row 02: ???
    !> =======
    !> Example:  T      0.02 5.0   1.    .12
    READ(UNIT_, *) main_ECRIT, main_PAS, main_ECHX, main_ECHY, main_FWHM

    !> row 03
    !> =======
    !> Example: 0.9
    READ(UNIT_, *) main_VVT(1)
    main_IVTOT = 1

    !> rows 03.(0-2) (three conditional rows that MUST exist if and only if main_VVT(1) > 900)
    !> =============
    !> ISSUE: VVT(1) is used throughout the program, so why reading VVT(I)??
    !> ISSUE Explain this part VERY WELL because it is an "anomaly", i.e., main.dat will have MORE LINES
    !> ISSUE Documentation
    !> (MT) Yes it makes sense, it specifies microturbulence velocities for each layer of the atmosphere
    IF(main_VVT(1) .GT. 900)  THEN   ! VT VARIABLE AVEC LA PROFONDEUR
      READ(UNIT_, *) main_IVTOT
      ! IVTOT, affects subroutine TURBUL() ISSUE what
      IF (main_IVTOT .GT. MAX_modeles_NTOT) THEN
        WRITE (LLL, *) 'main_IVTOT .GT. MAX_modeles_NTOT (', &
         main_IVTOT, ' .GT. ', MAX_modeles_NTOT, ')'
         CALL PFANT_HALT(LLL)
      END IF

      READ(UNIT_,*) (main_TOLV(I), I=1, main_IVTOT)
      READ(UNIT_,*) (main_VVT(I) ,I=1, main_IVTOT)
    END IF


    !> line 04
    !> =======
    !> Example:      5777  4.44  0       0.1  1
    READ(UNIT_, *) main_TEFF, main_GLOG, main_ASALOG, main_NHE,main_INUM

    !> line 05
    !> =======
    !> Example:      F       1.
    READ(UNIT_, *) main_PTDISK, main_MU

    !> line 06
    !> =======
    !> Example: 0
    READ(UNIT_, *) main_AFSTAR  ! metallicity of the star (in log scale)

    !> line 07 -- XXCOR(I)
    !> =======
    !> ISSUE: Should be a column in dissoc.dat !!!!!
    !> (MT) I agree

    ! Feature "XXCOR" --
    READ(UNIT_, *)(main_XXCOR(I), I=1, dissoc_NMETAL)

    !> line 08 -- part of filename
    !> =======
    !> This line will define the names of other three files to be read:
    !>   cont.<FILEFLUX>
    !>   norm.<FILEFLUX>
    !>   spec.<FILEFLUX>
    READ(UNIT_, '(A)') main_FILEFLUX

    !> line 09 --
    !> =======
    !>     AINT =intervalle de calcul

    !> Example:      4800    4820   50
    READ(UNIT_, *) main_LLZERO, main_LLFIN, main_AINT


  !> rows 10-19 -- file names, in sync with variable LLHY
  !> ===========
  !> Example: thkappa
  !>          thiota
  !>          ththeta
  !>          theta
  !>          thzeta
  !>          thepsilon
  !>          thdelta
  !>          thgamma
  !>          thbeta
  !>          thalpha

    DO IH = 1, 10
      READ(UNIT_, '(A)') main_FILETOHY(IH)
    END DO



    CLOSE(UNIT=UNIT_)
    FLAG_READ_MAIN = .TRUE.
  END




!================================================================================================================================
!> READ_DISSOC(): reads file dissoc.dat to fill variables dissoc_*
!>
!> Original UNIT: 23
!>
!> This file must end with a blank row so that the routine can detect
!> the end of the file


!> TODO Various tests:
!> TODO - mismatched NMETAL and metal rows
!> TODO - check if NMETAL and NMOL match what they are supposed to (assertions in test)

!> PROPOSE: use READ()'s "END=" option

  SUBROUTINE READ_DISSOC(filename)
    INTEGER UNIT_
    INTEGER I, J, K, M, MMAXJ
    PARAMETER(UNIT_=199)
    CHARACTER(LEN=*) :: filename
    CHARACTER*192 LLL
    CHARACTER*2 SYMBOl
    LOGICAL FLAG_FOUND

    ! Auxiliary temp variables for reading file
    INTEGER*4 NATOMM, NELEMM
    DIMENSION NATOMM(5), NELEMM(5)

    OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

    !> row 01
    !> =======
    !> NMETAL - NUMBER OF ELEMENTS CONSIDERED IN CHEMICAL EQUILIBRIUM
    !> NIMAX  - MAXIMUM NUMBER OF ITERATION IN NEWTON-RAPSON METHOD
    !> EPS    - IF ABS((X(I+1)-X(I))/X(I)).LE. EPS; CONVERGED
    !> SWITER - IF SWITER .GT. 0;   X(I+1)=0.5*(X(I+1)+X(I))
    !>          IF SWITER .LE. 0;   X(I+1)=X(I+1)
    !>
    !> Example:    18  100    0.005   -1.0
    READ(UNIT_,'(2I5, 2F10.5, I10)') dissoc_NMETAL, dissoc_NIMAX, dissoc_EPS, dissoc_SWITER

    !> rows 2 to NMETAL+1
    !> ==================
    !> 6 columns:
    !>   col 1 -- symbol of chemical element
    !>   col 2 -- atomic number "N"
    !>   col 3 -- (?)
    !>   col 4 -- (?)
    !>   col 5 -- (?)
    !>   col 6 -- (?)
    DO I = 1, dissoc_NMETAL
      READ (UNIT_, '(A2, 2X, I6, F10.3, 2I5, F10.5)') &
       SYMBOL, dissoc_NELEMX(I), dissoc__IP(I), &
       dissoc__IG0(I), dissoc__IG1(I), dissoc__CCLOG(I)

        ! Makes sure that elements first and second are H and HE, respectively,
        ! because SAT4() and DIE() count on this
        SELECT CASE (I)
          CASE (1)
            IF (SYMBOL .NE. 'H ') &
             CALL PFANT_HALT('First element must be hydrogen ("H ")!')
          CASE (2)
            IF (SYMBOL .NE. 'HE' .AND. SYMBOL .NE. 'He') &
             CALL PFANT_HALT('Second element must be helium ("He")!')
        END SELECT

        dissoc_ELEMS(I) = SYMBOL

        !__spill check__
        IF (dissoc_NELEMX(I) .GT. MAX_Z) THEN
          WRITE(LLL,*) 'READ_DISSOC(): metal # ', I, ': NELEMXI = ', &
           dissoc_NELEMX(I), ' over maximum allowed (', &
           MAX_Z, ')'
          CALL PFANT_HALT(LLL)
        END IF

      END DO

      !> rows NMETAL+2 till end-of-file
      !> ==============================
      !>   col  1     -- "name" of molecule
      !>   cols 2-6   -- !>(J, 1-5)
      !>   col  7     -- MMAX(J) (number of subsequent columns)/2
      !>   cols 8-... -- Maximum of 8 columns here.
      !>                 Pairs (NELEM(M), NATOM(M)), M = 1 to MMAX(J) ISSUE NELEM(M) is atomic number, what about NATOM(M)???
      J = 0

      1010 CONTINUE
      J = J+1

      !> ISSUE: This 1X does not appear in my sample dissoc.dat file
      !> ISSUE: Atually the file that Beatriz sent me does not work under this format!!!!
      !> ISSUE: THere is no 1X
      !      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')


      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, I1, 4(I2,I1))') &
                   dissoc_MOL(J), &
                   (dissoc_C(J, K), K=1,5), &
                   dissoc_MMAX(J), &
                   (NELEMM(M), NATOMM(M), M=1,4)


      MMAXJ = dissoc_MMAX(J)
      IF(MMAXJ .EQ. 0) GO TO 1014  ! means end-of-file

      IF (MMAXJ .GT. 4) THEN
        WRITE(LLL,*) 'READ_DISSOC() molecule "', dissoc_MOL(J), &
         '", MMAXJ = ', MMAXJ, ' cannot be greater than 4!'
        CALL PFANT_HALT(LLL)
      END IF


      DO M = 1, 4
        FLAG_FOUND = .FALSE.
        DO I = 1, dissoc_NMETAL
          IF (NELEMM(M) .EQ. dissoc_NELEMX(I)) THEN
            FLAG_FOUND = .TRUE.
            EXIT
          END IF
        END DO

        IF (.NOT. FLAG_FOUND) THEN
          WRITE(LLL,*) 'READ_DISSOC() molecule "', dissoc_MOL(J), &
           '" atomic number ', NELEMM(M), 'not in atoms list above'
          CALL PFANT_HALT(LLL)
        END IF
      END DO

      DO M = 1, MMAXJ
          dissoc_NELEM(M,J) = NELEMM(M)
          dissoc_NATOM(M,J) = NATOMM(M)
      END DO

      GO TO 1010

    1014 dissoc_NMOL = J-1

    CLOSE(UNIT=UNIT_)
    FLAG_READ_DISSOC = .TRUE.
  END




  !================================================================================================================================
  !> READ_ABONDS(): reads file abonds.dat to fill variables abonds_*
  !>
  !> In addition, searches elements in dissoc atoms table to take XXCOR values.
  !>
  !> The input file has 3 columns:
  !>   1) Empty space or "1"
  !>   2) 2-character atomic element symbol (?)
  !>   3) absolute abundance (a) (exponent; actual abundance is 10^a), unit "dex"
  !>
  !> The end of the file is signalled by two rows containing only "1" each at
  !> column 1 and nothing else at the others
  !>
  !> TODO Test one row!!!
  !> PROPOSE: use READ()'s "END=" option

  SUBROUTINE READ_ABONDS(filename)
    IMPLICIT NONE
    INTEGER UNIT_, FINAB, K, J
    PARAMETER(UNIT_=199)
    CHARACTER(LEN=*) :: filename
    CHARACTER*80 LLL
    LOGICAL FLAG_FOUND
    REAL*8 FSTAR

    IF (.NOT. FLAG_READ_MAIN) THEN
      CALL PFANT_HALT('READ_MAIN() must be called before READ_ABONDS()')
    END IF

    OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

    FSTAR = 10**main_AFSTAR

    J = 1
    FINAB = 0
    DO WHILE (FINAB .LT. 1)
      READ(UNIT_, '(I1,A2,F6.3)') FINAB, abonds_ELE(J), abonds_ABOL(J)

      IF (FINAB .LT. 1) THEN
        ! Extra tasks (i.e., apart from reading file) [1], [2]:
        !
        ! [1] Searches dissoc.dat' metals table by atomic symbol to get value of XXCOR variable
        ! ISSUE: lot of effort to keep dubious feature
        ! ISSUE: This is the thing that Beatriz mentioned that is not used anymore

        FLAG_FOUND = .FALSE.
        DO K = 1, dissoc_NMETAL
          IF(abonds_ELE(J) .EQ. dissoc_ELEMS(K)) THEN
            FLAG_FOUND = .TRUE.
            EXIT
          END IF
        END DO

        IF (.NOT. FLAG_FOUND) THEN
          WRITE(LLL,*) 'READ_ABONDS() element "', abonds_ELE(J), '" is not in dissoc atoms list!'
          CALL PFANT_HALT(LLL)
        END IF

        abonds_ABOL(J) = abonds_ABOL(J)+main_XXCOR(K)

        ! [2] Calculates abonds_ABO based on abonds_ABOL

        ! ISSUE Why "-12" ??
        abonds_ABO(J) = 10.**(abonds_ABOL(J)-12.)
        abonds_ABO(J) = abonds_ABO(J)*FSTAR
      END IF

      J = J+1
    END DO
    abonds_NABOND = J-2

    CLOSE(UNIT=UNIT_)
    FLAG_READ_ABONDS = .TRUE.
  END








  !================================================================================================================================
  !> READ_ATOMGRADE(): reads file atomgrade.dat to fill variables atomgrade__* (double underscore)
  !>
  !> Original UNIT: 14
  !>
  !> Depends on abonds_*, so must be called after READ_ABONDS()
  !>
  !> This file has 2 types of alternating rows:
  !>   odd row
  !>     col 1 -- 2-letter atomgrade_ELEM(K) FILLDOC
  !>     col 2 -- atomgrade_IONI(K) FILLDOC
  !>     col 3 -- atomgrade_LAMBDA(K) FILLDOC
  !>   even row
  !>     col 1 --
  !>     col 2 --
  !>     col 3 --
  !>     col 4 --
  !>     col 5 --
  !>     col 6 --
  !>     col 7 --
  !>     col 8 -- signals end-of-file. If "1", reading stops
  !>
  !> TODO: give error if blows MAX!!!!!!!!!!!!!!!!!
  !> ISSUE: Cannot cope with empty file (tested), however there are if's inside the program: IF NBLEND .EQ. 0 .........
  !> (MT) use READ()'s "END=" option IS A GOOD IDEA, and we will do the following: stop reading EITHER when the "1" is found or when the file ends!
  !> TODO Assertions in test to see if numbers match what they are supposed to
  !> PROPOSE: use READ()'s "END=" option

  SUBROUTINE READ_ATOMGRADE(filename)
    IMPLICIT NONE
    INTEGER UNIT_
    PARAMETER(UNIT_=199)
    CHARACTER(LEN=*) :: filename
    CHARACTER*192 LLL
    INTEGER FINRAI, K, J
    LOGICAL FLAG_FOUND

    IF (.NOT. FLAG_READ_ABONDS) THEN
      CALL PFANT_HALT('READ_ABONDS() must be called before READ_ATOMGRADE()')
    END IF

    OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

    K = 1

    9 CONTINUE
    READ(UNIT_, '(A2, I1, 1X, F10.3)') atomgrade__ELEM(K), &
                                       atomgrade__IONI(K), &
                                       atomgrade__LAMBDA(K)

    READ(UNIT_, *) atomgrade__KIEX(K), &
     atomgrade__ALGF(K), &
     atomgrade__CH(K), &
     atomgrade__GR(K), &
     atomgrade__GE(K), &
     atomgrade__ZINF(K), &
     atomgrade__ABONDR_DUMMY(K), FINRAI


    ! ISSUE Why this? Document!!!
    IF (atomgrade_GR(K) .LT. 1E-37) atomgrade_GR(K) = 2.21E15 / atomgrade_LAMBDA(K)**2

    IF (FINRAI .EQ. 1) GO TO 10
    K = K+1

    ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
    IF (K .GT. MAX_atomgrade__NBLEND) THEN
      WRITE(LLL,*) 'READ_ATOMGRADE(): exceeded maximum of', MAX_atomgrade__NBLEND, ' spectral lines'
      CALL PFANT_HALT(LLL)
    END IF

    ! Searches atomgrade's element within abonds' elements and copies corresponding
    ! abonds_ABO value into atomgrade_ABONDS_ABO. Halts program if element not found.
    ! This is a "inner join" (SQL terminology)!
    ! Historical note: this corresponds to old routine "ABONDRAIH". The search was being carried out
    !   every time (LZERO, LFIN) changed and only for the filtered atomgrade rows. I decided to
    !   perform this search for all atomgrade rows and then filter atomgrade_ABONDS_ABO together
    !   with other variables in FILTER_ATOMGRADE(), which is probably cheaper.

    ! ISSUE Why name was "ABONDRAIH"?? Did it mean "abundances of hydrogen lines"? I ask this because if it has really a physical meaning, I shouldn't bury this inside read_files.f

    FLAG_FOUND = .FALSE.
    DO  J = 1, abonds_NABOND
      IF (abonds_ELE(J) .EQ. atomgrade__ELEM(K)) THEN
        FLAG_FOUND = .TRUE.
        EXIT
      END IF
    END DO
    IF (.NOT. FLAG_FOUND) THEN
      WRITE(LLL,*)  'MANQUE L ABONDANCE DU ', atomgrade__ELEM(K)
      CALL PFANT_HALT(LLL)
    END IF
    atomgrade__ABONDS_ABO(K) = abonds_ABO(J)

    GO TO 9

    10 CONTINUE
    atomgrade__NBLEND = K

! MENTION: last atomic line wasn't being used!! (this has been fixed/changed). Original code:
!~	K=1
!~9	READ(14,103)ELEM(K),IONI(K),LAMBDA(K)
!~	READ(14,*) KIEX(K),ALGF(K),CH(K),GR(K),GE(K),ZINF(K),
!~	1 ABONDR(K),FINRAI
!~	write(34,103)ELEM(K),IONI(K),LAMBDA(K)
!~	GF(K)=10.**ALGF(K)
!~C        IF(K.EQ.1) GF(K)=10**AGGF
!~	IF(GR(K).LT.1E-37)   GR(K)=2.21E15 / LAMBDA(K)**2
!~	IF(FINRAI.EQ.1) GO TO 10
!~	IF(((LAMBDA(K).GT.LFIN).OR.(LAMBDA(K).LT.LZERO))) GO TO 205
!~	K=K+1
!~205	CONTINUE
!~	GO TO 9
!~10	NBLEND=K-1

    CLOSE(UNIT=UNIT_)

    RETURN

  END



!>-------------------------------------------------------------------------
!> FILTER_ATOMGRADE(): selects only spectral lines within range LZERO, LFIN
!>
!> Populates variables atomgrade_* (single underscore)
!>
!> Also looks into abonds_ELE(:) to fill atomgrade_ABOND_ABONDS
!>-------------------------------------------------------------------------
  SUBROUTINE FILTER_ATOMGRADE(LZERO, LFIN)
    REAL*8 LZERO, LFIN
    INTEGER J, K
    CHARACTER*192 LLL

    K = 0
    DO J = 1, atomgrade__NBLEND
      IF((atomgrade__LAMBDA(J).LE.LFIN) .AND. (atomgrade__LAMBDA(J) .GE. LZERO)) THEN
        K = K+1


        ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
        IF (K .GT. MAX_atomgrade_NBLEND) THEN
          WRITE(LLL,*) 'FILTER_ATOMGRADE(): exceeded maximum of', &
           MAX_atomgrade_NBLEND, ' spectral lines'
          CALL PFANT_HALT(LLL)
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

        atomgrade_ABONDS_ABO(K) = atomgrade__ABONDS_ABO(J)

        atomgrade_ABONDR_DUMMY(K) = atomgrade__ABONDR_DUMMY(J)


      END IF
    END DO

    atomgrade_NBLEND = K

  END









!================================================================================================================================
!> READ_PARTIT(): reads file partit.dat to fill variables partit_*
!>
!> orig "LECTURE DES FCTS DE PARTITION"
!>
!> Rows in this file alternate between:
!> 1) 8-column row
!>    col 8 -- signals end-of-file. If 1, it ignores the row and
!>             stops reading
!>
!> 2) Series of rows to fill in partit_TABU(J, :, :)
!>

  SUBROUTINE READ_PARTIT(filename)
    IMPLICIT NONE
    INTEGER UNIT_
    PARAMETER(UNIT_=199)
    CHARACTER(LEN=*) :: filename
    CHARACTER*192 LLL

    INTEGER FINPAR, J, KMAX, L, K

    OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')


    J = 1
    FINPAR = 0
    DO WHILE (FINPAR .LT. 1)
      READ (UNIT_, '(A2, 2F5.2, I3, 3F10.2, 34X, I1)') &
       partit_EL(J), &
       partit_TINI(J), &
       partit_PA(J), &
       partit_JKMAX(J), &
       partit_M(J), &
       partit_KI1(J), &
       partit_KI2(J), FINPAR

      IF (FINPAR .NE. 1) THEN

        ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
        IF (J .GT. MAX_partit_NPAR) THEN
          WRITE(LLL,*) 'READ_PARTIT(): PAR exceeded maximum of ', MAX_partit_NPAR
          CALL PFANT_HALT(LLL)
        END IF


        KMAX = partit_JKMAX(J)

        ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
        IF (KMAX .GT. MAX_partit_KMAX) THEN
          WRITE(LLL,*) 'READ_PARTIT(): PAR number', J, 'KMAX=', KMAX, &
           ' exceeded maximum of', MAX_partit_KMAX
          CALL PFANT_HALT(LLL)
        END IF


        READ(UNIT_, '(13F6.4)') ((partit_TABU(J, L, K), L=1, 3), K=1, KMAX)

        J = J+1
      END IF
    END DO

    partit_NPAR = J-1

    RETURN
  END
























!================================================================================================================================
!> READ_ABSORU2(): reads file absoru2.dat to fill variables absoru2_*
!>
!> Original UNIT: 15
!>
!> Attention: variables absoru2_ZP, absoru2_XI, and absoru2_PF
!>            undergo transformation!
!>
!> Obs: this routine is in essence the old routine called "LECTUR"
!>
!> I think SSP means "SubProgram" but it is a typo with an additional "S"
!>
!> orig CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
!> orig PUIS LES DONNEES CORRESPONDANTS AUX ABSORBANTS METALLIQUES SI NECE
!> orig CALMET=1 SI ON NE TIENT PAS COMPTE DES METAUX
!> orig CALMET=2 SI ON    TIENT     COMPTE DES METAUX
!>
!> ISSUE: as opposed to original description, CALMET was being ignored in original routine LECTUR()
!> (MT) ???
!> (JT) I guess it is a remain of sth and nowadays the metals are always taken into account
!>

  SUBROUTINE READ_ABSORU2(filename)
    IMPLICIT NONE
    INTEGER UNIT_
    PARAMETER(UNIT_=199)
    CHARACTER(LEN=*) :: filename
    CHARACTER*80 LLL
    CHARACTER*3 NEANT
    INTEGER NION, I, ITH, J, NRR, NSET

    OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

    ! orig ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
    ! orig ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
    READ (UNIT_,'(2E15.7)') absoru2_ABMET, absoru2_ABHEL


    ! orig NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISA
    ! orig NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
    ! orig IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
    ! orig IUNITE=' NOYAU H'  ''    ''    ''       ''      NOYAU D'HYDROGENE
    READ (UNIT_,'(2I2, 19A4)') absoru2_NM, absoru2_NMETA, &
          (absoru2_IUNITE(I),I=1,2), (absoru2_TITRE(I),I=1,17)


    ! *BOUNDARY CHECK*: checks if exceeds maximum number of elements allowed
    IF (absoru2_NM .GT. MAX_absoru2_NM) THEN
      WRITE(LLL,*) 'READ_ABSORU2(): NM=', absoru2_NM, ' exceeded maximum of', MAX_absoru2_NM
      CALL PFANT_HALT(LLL)
    END IF


    ! orig LECTURE DE LA TABLE D'IONISATION CHOISIE
    ! orig ----------------------------------------
    DO J = 1, absoru2_NM
      READ (UNIT_, '(3X,I3,2E16.5)') absoru2_NR(J), absoru2_ZP(J), absoru2_ZM(J)
      absoru2_ZP(J) = 10**absoru2_ZP(J)

      ! orig    NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
      ! orig    ZP=NBR. D'ABONDANCE DE L'ELEMENT
      ! orig    ZM=POIDS MOLECULAIRE DE L'ELEMENT
      NRR = absoru2_NR(J)

      ! *BOUNDARY CHECK*: Checks if exceeds maximum number of elements allowed
      IF (NRR .GT. MAX_absoru2_NRR) THEN
        WRITE(LLL,*) 'READ_ABSORU2(): J = ', J, 'NR=', NRR, ' exceeded maximum of', MAX_absoru2_NRR
        CALL PFANT_HALT(LLL)
      END IF


      DO I = 1, NRR
        ! neant="nothing"
        ! NION is also not used
        ! ISSUE: NOMET is not used in the program
        READ (UNIT_, '(A3,A2,I1,2E16.5)') NEANT, absoru2_NOMET(J), &
         NION, absoru2_XI(J,I), absoru2_PF(J,I)

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

!> ISSUE: I am not sure if this last part is being read correcly. Perhaps I didn't de-spag right. Anyway, the test verbose is not good.


    ! *BOUNDARY CHECK*: Checks if exceeds maximum number of elements allowed
    IF (absoru2_NUMSET(1) .GT. MAX_absoru2_NUMSET_I) THEN
      WRITE(LLL,*) 'READ_ABSORU2(): NUMSET(1) = ', absoru2_NUMSET(1), &
       ' exceeded maximum of', MAX_absoru2_NUMSET_I
      CALL PFANT_HALT(LLL)
    END IF
    ! *BOUNDARY CHECK*: Checks if exceeds maximum number of elements allowed
    IF (absoru2_NUMSET(2) .GT. MAX_absoru2_NUMSET_I) THEN
      WRITE(LLL,*) 'READ_ABSORU2(): NUMSET(2) = ', absoru2_NUMSET(2), &
       ' exceeded maximum of', MAX_absoru2_NUMSET_I
      CALL PFANT_HALT(LLL)
    END IF


    ! orig NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
    ! orig POUR H,HE ET HE+
    ! orig PREMIERE LISTE POUR TH.LE.0.8  ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
    DO ITH = 1,2
      NSET = absoru2_NUMSET(ITH)
      READ (UNIT_,'(8F10.1)') (absoru2_WI(I,ITH),I=1,NSET)
    END DO

  END


!================================================================================================================================
!> READ_MODELE(): reads single record from file modeles.mod into
!>                 variables modeles_*
!>
!> Original UNIT: 18
!>
!> orig SI L ON DESIRE IMPOSER UN MODELE  ON MET EN INUM LE NUM DU MODELE
!> orig SUR LE FICHIER ACCES DIRECT
!>
!> Depends on main_INUM
!> ISSUE: depends on other main_* but I think not for long

  SUBROUTINE READ_MODELE(filename)
    IMPLICIT NONE
    INTEGER UNIT_
    PARAMETER(UNIT_=199)
    CHARACTER(LEN=*) :: filename
    REAL*8 DDT, DDG, DDAB
    INTEGER I, &
            ID_   ! TODO This could well be an input parameter, because it wouldn't have to rely on main.dat and would become MUCH more flexible
    CHARACTER*192 LLL


    OPEN(UNIT=UNIT_, ACCESS='DIRECT',STATUS='OLD', FILE=filename, RECL=1200)

    ID_ = 1

    ! TODO better to give error if main_INUM is not set
    ! TODO Check if FORTRAN initializes variables to zero automatically: can I rely on this??
    ! TODO Maybe implement variable main_FLAG to FLAG that main.dat has been read already
    IF (main_INUM .GT. 0) ID_ = main_INUM  ! Selects record number

!> ISSUE: Variable NHE read again from a different file!!!!!!!!! I decided to opt for modeles_NHE because it overwrites main_NHE
!> (MT) ASSERT main_NHE == modeles_NHE


    READ(UNIT_, REC=ID_) modeles_NTOT, modeles_DETEF, modeles_DGLOG, &
     modeles_DSALOG, modeles_ASALALF, modeles_NHE, modeles_TIT, &
     modeles_TIABS
    IF (modeles_NTOT .EQ. 9999) THEN
      ! ISSUE perhaps I should check the condition that leads to this error
      ! TODO STOP with error level
      CALL PFANT_HALT('LE MODELE DESIRE NE EST PAS SUR LE FICHIER')
    END IF


    ! *BOUNDARY CHECK*: Checks if exceeds maximum number of elements allowed
    IF (modeles_NTOT .GT. MAX_modeles_NTOT) THEN
      WRITE(LLL,*) 'READ_MODEABSORU2(): NUMSET(1) = ',absoru2_NUMSET(1), &
       ' exceeded maximum of', MAX_absoru2_NUMSET_I
      CALL PFANT_HALT(LLL)
    END IF



    WRITE(6, *) 'modeles_DETEF', modeles_DETEF
    WRITE(*, *) 'modeles_DGLOG', modeles_DGLOG
    WRITE(*, *) 'modeles_DSALOG', modeles_DSALOG


    DDT  = ABS(main_TEFF-modeles_DETEF)
    DDG = ABS(main_GLOG-modeles_DGLOG)
    DDAB = ABS(main_ASALOG-modeles_DSALOG)

!      ! ISSUE: Variable DNHE does not exist!!! Anyway, it is not used
!      ! ISSUE: this seems to be some kind of error check, but will loop forever, better to place it outside, also because of dependence on main_* variables
!      DDHE= ABS(modeles_NHE-DNHE)

    ! ISSUE: I don't get this; it will keep looping forever??? Look at the original code. What should happen if one of these three conditions hold?? Actually, got this. These are really consistency checks
    ! (MT) It is annoying for the user to know exactly the index of the model that they are using. The code could have a "search feature"


!~9	READ(18, REC=ID) NTOT,DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT,TITABS
!~	WRITE(6,105)DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT
!~        write(6,108) TIABS
!~	IF(NTOT.EQ.9999)   GO TO 6
!~	DDT  = ABS(TEFF-DETEF)
!~C	DDTA  = ABS(TETAEF-DETAEF)
!~	DDG = ABS(GLOG-DGLOG)
!~	DDAB = ABS(ASALOG-DSALOG)
!~	DDHE= ABS(NHE-DNHE)
!~C	DDIF = DDTA+DDG+DDAB+DDHE
!~5	IF(DDT.GT.1.0)   GO TO 9
!~	IF(DDG.GT.0.01)   GO TO 9
!~	IF(DDAB.GT.0.01)   GO TO 9


    ! TODO ABS(main_NHE - modeles_NHE) <= 0.01     <--- this is the epsilon
    ! TODO Get the epsilon from MT
    IF(DDT .GT. 1.0) THEN
      WRITE(LLL,*) 'ABS(main_TEFF-modeles_DETEF) = ', DDT, ' > 1.0'
      CALL PFANT_HALT(LLL)
    END IF
    IF(DDG .GT. 0.01) THEN
      WRITE(LLL,*) 'ABS(main_GLOG-modeles_DGLOG) = ', DDG, ' > 0.01'
      CALL PFANT_HALT(LLL)
    END IF
    IF(DDAB .GT. 0.01) THEN
      WRITE(LLL,*) 'ABS(main_ASALOG-modeles_DSALOG) = ', DDAB, ' > 0.01'
      CALL PFANT_HALT(LLL)
    END IF



    READ(UNIT_, REC=ID_) BID, &
         (modeles_NH(I), &
          modeles_TETA(I), &
          modeles_PE(I), &
          modeles_PG(I), &
          modeles_T5L(I), I=1,modeles_NTOT)


    CLOSE(UNIT_)
  END



END MODULE READ_FILES

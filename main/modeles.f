

      MODULE MODELES


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
C READ_MODELE(): reads single record from file modeles.mod into
C                 variables modeles_*
C
C Original UNIT: 18
C
C orig SI L ON DESIRE IMPOSER UN MODELE  ON MET EN INUM LE NUM DU MODELE
C orig SUR LE FICHIER ACCES DIRECT
C
C Depends on main_INUM
C ISSUE: depends on other main_* but I think not for long

      SUBROUTINE READ_MODELE(filename)
      USE ERRORS
      IMPLICIT NONE
      INTEGER UNIT_
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename
      REAL*8 DDT, DDG, DDAB
      INTEGER I,
     +         ID_   ! TODO This could well be an input parameter, because it wouldn't have to rely on main.dat and would become MUCH more flexible


      OPEN(UNIT=UNIT_, ACCESS='DIRECT',STATUS='OLD',
     1     FILE=filename, RECL=1200)

      ID_ = 1


      ! TODO better to give error if main_INUM is not set
      ! TODO Check if FORTRAN initializes variables to zero automatically: can I rely on this??
      ! TODO Maybe implement variable main_FLAG to FLAG that main.dat has been read already
      IF (main_INUM .GT. 0) ID_ = main_INUM  ! Selects record number

C ISSUE: Variable NHE read again from a different file!!!!!!!!! I decided to opt for modeles_NHE because it overwrites main_NHE
C (MT) ASSERT main_NHE == modeles_NHE


      READ(UNIT_, REC=ID_) modeles_NTOT, modeles_DETEF, modeles_DGLOG,
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

*      ! ISSUE: Variable DNHE does not exist!!! Anyway, it is not used
*      ! ISSUE: this seems to be some kind of error check, but will loop forever, better to place it outside, also because of dependence on main_* variables
*      DDHE= ABS(modeles_NHE-DNHE)

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












      END MODULE MODELES
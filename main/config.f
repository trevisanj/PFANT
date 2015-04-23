C CONFIG module:
C - Configuration globals with their default values
C - Routines to parse command-line arguments
C - All globals have prefix "config_"

      MODULE CONFIG
      USE LOGGING

      INTEGER, PARAMETER :: NUM_MOL=21  ! Number of molecules configured in the program.
                                        ! Conceptually, this should be defined in molecula.f, but there would be cyclic USEs

      INTEGER config_logging_LEVEL /logging_INFO/
      LOGICAL config_DEBUG /.FALSE./    ! Allows for debugging messages



      !=====
      ! Variables related to molecules
      !=====

      ! These are configurable
      INTEGER config_NUM_MOL_OFF /0/ ! Number of molecules switched off (excluded from calculations)
      INTEGER, DIMENSION(NUM_MOL) :: config_MOLIDS_OFF  ! IDs=indexes of the molecules to be switched off

      ! These are filled by MAKE_MOLIDS()
      INTEGER, DIMENSION(NUM_MOL) :: config_MOLIDS_ON  ! List of molecule IDs. Valid indexes range from 1 to km__NUMBER
      INTEGER config_NUM_MOL_ON  ! This is actually = NUM_MOL-config_NUM_MOL_OFF


      !=====
      ! Misc
      !=====
      
      
      ! Interpolation type of turbul_VT
      ! 1 -- linear
      ! 2 -- parabolic
      INTEGER config_INTERP = 1  ! TODO not sure if I can initialize thus
      




      !=====
      ! Private variables
      !=====
      LOGICAL, PRIVATE :: FLAG_SETUP /.FALSE./

      CONTAINS


C================================================================================================================================
C CONFIG_SETUP():
C   - sets up configuration defaults,
C   - parses command-line arguments, and
C   - does other necessary operations
C
C Must be called at system startup
C
      SUBROUTINE CONFIG_SETUP()
      USE LOGGING
      IMPLICIT NONE

      ! TODO parse command-line here


      ! Validation of config_INTERP
      SELECT CASE (config_INTERP)
        CASE (1, 2)
        CASE DEFAULT
          WRITE (*,*) 'Invalid INTERP: ', INTERP, ' (valid: 1/2)',
     +     config_INTERP
      END SELECT
      
      logging_LEVEL = config_logging_LEVEL  ! sets logging level at LOGGING module based on config variable
          

      CALL MAKE_MOLIDS()
      FLAG_SETUP = .TRUE.

      END



C================================================================================================================================
C GET_MOLID(): returns molecule ID given index
C
C Molecule ID is a number from 1 to NUM_MOL, which is uniquely related to a chemical molecule within PFANT.
C
      FUNCTION GET_MOLID(I_MOL)
      USE ERRORS
      IMPLICIT NONE
      INTEGER I_MOL, GET_MOLID
      CHARACTER*128 S  !--logging--!

      !--assertion--!
      IF (.NOT. FLAG_SETUP)
     + CALL PFANT_HALT('GET_MOLID(): forgot to call CONFIG_SETUP()')

      !--spill check--!
      IF (I_MOL .GT. config_NUM_MOL_ON) THEN
        WRITE (S, *) 'GET_MOLID(): Invalid molecule index I_MOL (',
     +   I_MOL, ') must be maximum ', config_NUM_MOL_ON
        CALL PFANT_HALT(S)
      END IF

      GET_MOLID = config_MOLIDS_ON(I_MOL)
      RETURN
      END

C================================================================================================================================
C MOLECULE_IS_ON(): returns .TRUE. or .FALSE. whether molecule represented by MOLID is "on" or "off"

      FUNCTION MOLECULE_IS_ON(MOLID)
      USE ERRORS
      IMPLICIT NONE
      INTEGER MOLID, J
      LOGICAL MOLECULE_IS_ON

      !--assertion--!
      IF (.NOT. FLAG_SETUP)
     + CALL PFANT_HALT('MOLECULE_IS_ON(): '
     + //'forgot to call CONFIG_SETUP()')

      MOlECULE_IS_ON = .TRUE.
      DO J = 1, config_NUM_MOL_OFF
        IF (MOLID .EQ. config_MOLIDS_OFF(J)) THEN
          MOLECULE_IS_ON = .FALSE.
          EXIT
        END IF
      END DO
      END


C================================================================================================================================
C MAKE_MOLIDS(): fills config_MOLIDS_ON and config_NUM_MOL_ON
C
      SUBROUTINE MAKE_MOLIDS()
      IMPLICIT NONE
      INTEGER I_MOL, J, MOLID
      LOGICAL IS_OFF

      I_MOL = 0
      DO MOLID = 1, NUM_MOL
        IS_OFF = .FALSE.  ! Whether molecule I_MOL is off
        DO J = 1, config_NUM_MOL_OFF
          IF (MOLID .EQ. config_MOLIDS_OFF(J)) THEN
            IS_OFF = .TRUE.
            EXIT
          END IF
        END DO
        IF (.NOT. IS_OFF) THEN
          I_MOL = I_MOL+1
          config_MOLIDS_ON(I_MOL) = MOLID
        END IF
      END DO
      config_NUM_MOL_ON = I_MOL
      END


      END MODULE CONFIG







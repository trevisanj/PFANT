C LOGGING Module
C
C Usage:
C - Set logging_LEVEL (optional, defaults to DEBUG)
C - Call CRITICAL() / ERROR() / WARNING() / INFO() / DEBUG()
C
C Logging message will only be shown if logging_LEVEL is <= corresponding level of subroutine called.
C E.g., corresponding level of subroutine DEBUG() is logging_DEBUG


      MODULE LOGGING

      ! Logging levels copied from Python
      INTEGER, PARAMETER ::
     + logging_HALT     = 60,  ! I invented this, logging message when system halts
     + logging_CRITICAL = 50,
     + logging_ERROR    = 40,
     + logging_WARNING  = 30,
     + logging_INFO     = 20,
     + logging_DEBUG    = 10


      !=====
      ! Configurable variable
      !=====

      INTEGER logging_LEVEL /logging_DEBUG/



      PRIVATE :: DO_LOGGING

C     =========
      CONTAINS
C     =========


C-------------------------------------------------------------------------------
C Logs message as CRITICAL
      SUBROUTINE LOG_CRITICAL(S)
      CHARACTER(LEN=*) :: S
      IF (logging_LEVEL .LE. logging_CRITICAL) THEN
        CALL DO_LOGGING(S, logging_CRITICAL)
      END IF
      END

C-------------------------------------------------------------------------------
C Logs message as HALT. Logs unconditionally (independent of logging level).
      SUBROUTINE LOG_HALT(S)
      CHARACTER(LEN=*) :: S
      CALL DO_LOGGING(S, logging_HALT)
      END

C-------------------------------------------------------------------------------
C Logs message as ERROR
      SUBROUTINE LOG_ERROR(S)
      CHARACTER(LEN=*) :: S
      IF (logging_LEVEL .LE. logging_ERROR) THEN
        CALL DO_LOGGING(S, logging_ERROR)
      END IF
      END

C-------------------------------------------------------------------------------
C Logs message as WARNING
      SUBROUTINE LOG_WARNING(S)
      CHARACTER(LEN=*) :: S
      IF (logging_LEVEL .LE. logging_WARNING) THEN
        CALL DO_LOGGING(S, logging_WARNING)
      END IF
      END

C-------------------------------------------------------------------------------
C Logs message as INFO
      SUBROUTINE LOG_INFO(S)
      CHARACTER(LEN=*) :: S
      IF (logging_LEVEL .LE. logging_INFO) THEN
        CALL DO_LOGGING(S, logging_INFO)
      END IF
      END

C-------------------------------------------------------------------------------
C Logs message as DEBUG
      SUBROUTINE LOG_DEBUG(S)
      CHARACTER(LEN=*) :: S
      IF (logging_LEVEL .LE. logging_DEBUG) THEN
        CALL DO_LOGGING(S, logging_DEBUG)
      END IF
      END


C-------------------------------------------------------------------------------
C Logs message at HALT level and halts program execution
      SUBROUTINE PFANT_HALT(S)
      CHARACTER(LEN=*) :: S
      CALL DO_LOGGING(S, logging_HALT)
      STOP
      END


C Internal routine, MUST NOT be called from outside
      SUBROUTINE DO_LOGGING(S, LEVEL)
      CHARACTER(LEN=*) :: S
      CHARACTER(LEN=8) :: T
      INTEGER LEVEL

      SELECT CASE (LEVEL)
        CASE (logging_HALT)
          T = 'HALTING'
        CASE (logging_CRITICAL)
          T = 'CRITICAL'
        CASE (logging_ERROR)
          T = 'ERROR'
        CASE (logging_WARNING)
          T = 'WARNING'
        CASE (logging_INFO)
          T = 'INFO'
        CASE (logging_DEBUG)
          T = 'DEBUG'
      END SELECT

      WRITE(*,*) '(', T, ') :: ', TRIM(S)

      END

      END MODULE LOGGING





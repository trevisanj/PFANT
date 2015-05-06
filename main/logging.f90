! This file is part of PFANT.
! 
! PFANT is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! PFANT is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with PFANT.  If not, see <http://www.gnu.org/licenses/>.

!> LOGGING Module
!>
!> Usage:
!> - Set logging_LEVEL (optional, defaults to DEBUG)
!> - Call CRITICAL() / ERROR() / WARNING() / INFO() / DEBUG()
!>
!> Logging message will only be shown if logging_LEVEL is <= corresponding level of subroutine called.
!> E.g., corresponding level of subroutine DEBUG() is logging_DEBUG
MODULE LOGGING

  !> Logging levels copied from Python
  INTEGER, PARAMETER ::   &
   logging_HALT     = 60, &  ! I invented this, logging message when system halts
   logging_CRITICAL = 50, &
   logging_ERROR    = 40, &
   logging_WARNING  = 30, &
   logging_INFO     = 20, &
   logging_DEBUG    = 10  &


  !=====
  !> Configurable variable
  !=====
  INTEGER logging_LEVEL = logging_DEBUG



  PRIVATE :: DO_LOGGING

CONTAINS

  !-------------------------------------------------------------------------------
  !> Logs message at HALT level and halts program execution with error code -1.
  !>
  !> Error code -1 allows a program that calls PFANT to know that PFANT stopped
  !> due to an error situation (normal program execution ends with error code 0).

  SUBROUTINE PFANT_HALT(S)
    CHARACTER(LEN=*) :: S
    CALL DO_LOGGING(S, logging_HALT)
    STOP -1
  END
  
  !-------------------------------------------------------------------------------
  !> Logs message as HALT. Logs unconditionally (independent of logging level).
  !>
  !> This allows the calling routine to log HALT-level messages before calling 
  !> PFANT_HALT(). As a rule, always call PFTANT_HALT() after 1 or more calls to
  !> LOG_HALT().
  
  SUBROUTINE LOG_HALT(S)
    CHARACTER(LEN=*) :: S
    CALL DO_LOGGING(S, logging_HALT)
  END

  !-------------------------------------------------------------------------------
  !> Logs message as CRITICAL
  
  SUBROUTINE LOG_CRITICAL(S)
    CHARACTER(LEN=*) :: S
    IF (logging_LEVEL .LE. logging_CRITICAL) THEN
	  CALL DO_LOGGING(S, logging_CRITICAL)
    END IF
  END

  !-------------------------------------------------------------------------------
  !> Logs message as ERROR
  
  SUBROUTINE LOG_ERROR(S)
    CHARACTER(LEN=*) :: S
    IF (logging_LEVEL .LE. logging_ERROR) THEN
	  CALL DO_LOGGING(S, logging_ERROR)
    END IF
  END

  !-------------------------------------------------------------------------------
  !> Logs message as WARNING
  
  SUBROUTINE LOG_WARNING(S)
    CHARACTER(LEN=*) :: S
    IF (logging_LEVEL .LE. logging_WARNING) THEN
	  CALL DO_LOGGING(S, logging_WARNING)
    END IF
  END

  !-------------------------------------------------------------------------------
  !> Logs message as INFO
  
  SUBROUTINE LOG_INFO(S)
    CHARACTER(LEN=*) :: S
    IF (logging_LEVEL .LE. logging_INFO) THEN
	  CALL DO_LOGGING(S, logging_INFO)
    END IF
  END

  !-------------------------------------------------------------------------------
  !> Logs message as DEBUG
  
  SUBROUTINE LOG_DEBUG(S)
    CHARACTER(LEN=*) :: S
    IF (logging_LEVEL .LE. logging_DEBUG) THEN
	  CALL DO_LOGGING(S, logging_DEBUG)
    END IF
  END

  !===============================================================================
  
  !-------------------------------------------------------------------------------
  !> Internal routine, MUST NOT be called from outside
  
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

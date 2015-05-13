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

!> @ingroup gr_io
!> Routines to write text to screen and/or log file.
!>
!> Usage:
!> - Set logging_LEVEL (optional, defaults to DEBUG)
!> - Call CRITICAL() / ERROR() / WARNING() / INFO() / DEBUG()
!>
!> Logging message will only be shown if logging_LEVEL is <= corresponding level of subroutine called.
!> E.g., corresponding level of subroutine DEBUG() is LOGGING_DEBUG
module logging

  !> Logging levels copied from Python
  integer, parameter ::   &
   LOGGING_HALT     = 60, &  !< Maximum logging level; logging just before system halts
   LOGGING_CRITICAL = 50, &
   LOGGING_ERROR    = 40, &
   LOGGING_WARNING  = 30, &
   LOGGING_INFO     = 20, &
   LOGGING_DEBUG    = 10


  !=====
  !> Configurable variable
  !=====
  integer :: logging_level = LOGGING_DEBUG



  private :: do_logging

contains

  !-------------------------------------------------------------------------------
  !> Logs message at HALT level and halts program execution with error code -1.
  !>
  !> Error code -1 allows a program that calls PFANT to know that PFANT stopped
  !> due to an error situation (normal program execution ends with error code 0).

  subroutine pfant_halt(s, is_bug)
    character(len=*), intent(in) :: s
    !> (default=.false.) Whether halting program because of a bug.
    logical, optional, intent(in) :: is_bug
    IF (.NOT. PRESENT(IS_BUG)) is_bug = .False.

    call do_logging(s, LOGGING_HALT)
    !> @todo actually as a second thought, I might always print some message as the following, drop this is_bug option, and always ask kindly for error (STOP) situations to be reported
    IF (is_bug) THEN
      call do_logging('*************************************', LOGGING_HALT)
      call do_logging('* This is a bug! ********************', LOGGING_HALT)
      call do_logging('* Please help to fix it by mailing **', LOGGING_HALT)
      call do_logging('**the message above to the authors. *', LOGGING_HALT)
      call do_logging('*************************************', LOGGING_HALT)
    end if
    stop -1
  end

  !-------------------------------------------------------------------------------
  !> Logs message as HALT. Logs unconditionally (independent of logging level).
  !>
  !> This allows the calling routine to log halt-level messages before calling
  !> pfant_halt(). As a rule, always call pftant_halt() after 1 or more calls to
  !> log_halt().

  subroutine log_halt(s)
    character(len=*), intent(in) :: s
    call do_logging(s, LOGGING_HALT)
  end

  !-------------------------------------------------------------------------------
  !> Logs message as CRITICAL

  SUBROUTINE LOG_CRITICAL(S)
    CHARACTER(LEN=*), intent(in) :: S
    IF (logging_LEVEL .LE. LOGGING_CRITICAL) THEN
      CALL DO_LOGGING(S, LOGGING_CRITICAL)
    END IF
  END

  !-------------------------------------------------------------------------------
  !> Logs message as ERROR

  SUBROUTINE LOG_ERROR(S)
    CHARACTER(LEN=*), intent(in) :: S
    IF (logging_LEVEL .LE. LOGGING_ERROR) THEN
    CALL DO_LOGGING(S, LOGGING_ERROR)
    END IF
  END

  !-------------------------------------------------------------------------------
  !> Logs message as WARNING

  SUBROUTINE LOG_WARNING(S)
    CHARACTER(LEN=*), intent(in) :: S
    IF (logging_LEVEL .LE. LOGGING_WARNING) THEN
    CALL DO_LOGGING(S, LOGGING_WARNING)
    END IF
  END

  !-------------------------------------------------------------------------------
  !> Logs message as INFO

  SUBROUTINE LOG_INFO(S)
    CHARACTER(LEN=*), intent(in) :: S
    IF (logging_LEVEL .LE. LOGGING_INFO) THEN
    CALL DO_LOGGING(S, LOGGING_INFO)
    END IF
  END

  !-------------------------------------------------------------------------------
  !> Logs message as DEBUG

  SUBROUTINE LOG_DEBUG(S)
    CHARACTER(LEN=*), intent(in) :: S
    IF (logging_LEVEL .LE. LOGGING_DEBUG) THEN
    CALL DO_LOGGING(S, LOGGING_DEBUG)
    END IF
  END

  !===============================================================================

  !-------------------------------------------------------------------------------
  !> Internal routine, MUST NOT be called from outside

  SUBROUTINE DO_LOGGING(S, LEVEL)
  CHARACTER(LEN=*), intent(in) :: S
  CHARACTER(LEN=8) :: T
  INTEGER LEVEL

  SELECT CASE (LEVEL)
    CASE (LOGGING_HALT)
      T = 'HALTING'
    CASE (LOGGING_CRITICAL)
      T = 'CRITICAL'
    CASE (LOGGING_ERROR)
      T = 'ERROR'
    CASE (LOGGING_WARNING)
      T = 'WARNING'
    CASE (LOGGING_INFO)
      T = 'INFO'
  CASE (LOGGING_DEBUG)
      T = 'DEBUG'
    END SELECT

    WRITE(*,*) '(', T, ') :: ', TRIM(S)
  END
END MODULE LOGGING

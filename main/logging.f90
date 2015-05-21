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
  implicit none

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

  save

contains

  !-------------------------------------------------------------------------------
  !> Logs message at HALT level and halts program execution with error code -1.
  !>
  !> Error code -1 allows a program that calls PFANT to know that PFANT stopped
  !> due to an error situation (normal program execution ends with error code 0).

  subroutine pfant_halt(s, is_bug)
    !> Log message
    character(len=*), intent(in) :: s
    !> (default=.false.) Whether halting program because of a bug.
    logical, optional, intent(in) :: is_bug
    logical is_bug_ ! Argument is_bug, or default value if not passed.
    if (.not. present(is_bug)) then
      is_bug_ = .False.
    else
      is_bug_ = is_bug
    end if

    call do_logging(s, LOGGING_HALT)
    !> @todo actually as a second thought, I might always print some message as the following, drop this is_bug option, and always ask kindly for error (STOP) situations to be reported
    !> @todo actually, as a third thought, I may use this not necessarily meaning bug, but ask kindly for the used to tell us what happened to help us improve the software.
    !> @todo the time to solve this is when I tackle all the error situations systematically
    if (is_bug_) then
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

  subroutine log_critical(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_CRITICAL) then
      call do_logging(s, LOGGING_CRITICAL)
    end if
  end

  !-------------------------------------------------------------------------------
  !> Logs message as ERROR

  subroutine log_error(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_ERROR) then
    call do_logging(s, LOGGING_ERROR)
    end if
  end

  !-------------------------------------------------------------------------------
  !> Logs message as WARNING

  subroutine log_warning(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_WARNING) then
    call do_logging(s, LOGGING_WARNING)
    end if
  end

  !-------------------------------------------------------------------------------
  !> Logs message as INFO

  subroutine log_info(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_INFO) then
    call do_logging(s, LOGGING_INFO)
    end if
  end

  !-------------------------------------------------------------------------------
  !> Logs message as DEBUG

  subroutine log_debug(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_DEBUG) then
    call do_logging(s, LOGGING_DEBUG)
    end if
  end

  !===============================================================================

  !-------------------------------------------------------------------------------
  !> Internal routine, MUST NOT be called from outside

  subroutine do_logging(s, level)
  character(len=*), intent(in) :: s
  character(len=8) :: t
  integer level

  select case (level)
    case (logging_halt)
      t = 'HALTING'
    case (LOGGING_CRITICAL)
      t = 'CRITICAL'
    case (LOGGING_ERROR)
      t = 'ERROR'
    case (LOGGING_WARNING)
      t = 'WARNING'
    case (LOGGING_INFO)
      t = 'INFO'
  case (LOGGING_DEBUG)
      t = 'DEBUG'
    end select

    write(*,*) '(', t, ') :: ', trim(s)
  end
end module logging

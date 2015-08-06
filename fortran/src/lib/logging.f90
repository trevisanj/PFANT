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

  !> variable declared for convenience, to be used to log formatted output as in example:
  !> @code
  !> write(lll,*) ...
  !> call log_info(lll)
  !> @endcode
  character*256 lll

  ! Logging levels copied from Python (except LOGGING_HALT)
  integer, parameter ::   &
   LOGGING_HALT     = 60, &  !< Maximum logging level; logging just before system halts
   LOGGING_CRITICAL = 50, &
   LOGGING_ERROR    = 40, &
   LOGGING_WARNING  = 30, &
   LOGGING_INFO     = 20, &
   LOGGING_DEBUG    = 10


  ! Signs to be used as you wish
  character(:), parameter :: &
   ENTERING = '>>>>>>>>>> ENTERING ', &
   LEAVING  = '<<<<<<<<<< LEAVING '



  !=====
  ! Configurable variables
  !=====
  !> Possible values: logging::LOGGING_HALT, logging::LOGGING_CRITICAL, logging::LOGGING_ERROR,
  !> logging::LOGGING_WARNING, logging::LOGGING_INFO (default), logging::LOGGING_DEBUG
  integer :: logging_level = LOGGING_DEBUG
  !> Full path to file to record progress indication
  character*256 :: logging_path_progress = 'progress.txt'
  !> If set to .true., will display messages do standard output (usually the screen)
  logical :: logging_stdout = .true.
  !> If set to .true., will echo logged messages into dump file specified by logging_path_dump,
  !> besides logging to standard output.
  logical :: logging_dump = .false.
  !> Full path to file to record logging messages indication
  character*256 :: logging_path_dump = 'logging_dump.log'



  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  private :: do_logging

  save
contains

  !---------------------------------------------------------------------------------------
  !> Logs message at HALT level and halts program execution with error code -1.
  !>
  !> Error code -1 allows a program that calls PFANT to know that PFANT stopped
  !> due to an error situation (normal program execution ends with error code 0).

  subroutine pfant_halt(s, is_bug, is_assertion)
    !> Log message
    character(len=*), intent(in) :: s
    !> (default=.false.) Whether halting program because of a bug.
    logical, optional, intent(in) :: is_bug
    !> (default=.false.) Whether halting program because of assertion error.
    !> If .true., will print "assertion error" before the message
    logical, optional, intent(in) :: is_assertion
    logical is_bug_, is_assertion_
    if (.not. present(is_bug)) then
      is_bug_ = .False.
    else
      is_bug_ = is_bug
    end if
    if (.not. present(is_assertion)) then
      is_assertion_ = .False.
    else
      is_assertion_ = is_assertion
    end if

    if (.not. is_assertion_) then
      call do_logging(s, LOGGING_HALT)
    else
      call do_logging('(*assertion error*) '//s, LOGGING_HALT)
    end if

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

  !---------------------------------------------------------------------------------------
  !> Logs message as HALT. Logs unconditionally (independent of logging level).
  !>
  !> This allows the calling routine to log halt-level messages before calling
  !> pfant_halt(). As a rule, always call pftant_halt() after 1 or more calls to
  !> log_halt().

  subroutine log_halt(s)
    character(len=*), intent(in) :: s
    call do_logging(s, LOGGING_HALT)
  end

  !---------------------------------------------------------------------------------------
  !> Logs message as CRITICAL

  subroutine log_critical(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_CRITICAL) then
      call do_logging(s, LOGGING_CRITICAL)
    end if
  end

  !---------------------------------------------------------------------------------------
  !> Logs message as ERROR

  subroutine log_error(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_ERROR) then
    call do_logging(s, LOGGING_ERROR)
    end if
  end

  !---------------------------------------------------------------------------------------
  !> Logs message as WARNING

  subroutine log_warning(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_WARNING) then
    call do_logging(s, LOGGING_WARNING)
    end if
  end

  !---------------------------------------------------------------------------------------
  !> Logs message as INFO

  subroutine log_info(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_INFO) then
    call do_logging(s, LOGGING_INFO)
    end if
  end

  !---------------------------------------------------------------------------------------
  !> Logs message as DEBUG

  subroutine log_debug(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_DEBUG) then
    call do_logging(s, LOGGING_DEBUG)
    end if
  end

  !---------------------------------------------------------------------------------------
  !> Logs progress
  !>
  !> uses log_info() to write to screen + writes information into
  !> logging::logging_path_progress
  !>
  !> If cannot create file, does not bother (warns)

  subroutine log_progress(i, n)
    integer, intent(in) :: i, & !< current iteration
                           n    !< number of iterations
    real*8 perc
    integer, parameter :: UNIT_ = 199
    perc = 100.*i/n

    !#assertion
    if (n .gt. 9999) then
      call pfant_halt('Cannot log progress for number of iterations > 9999', &
       is_assertion=.true.)
    end if

    write (lll,'(''$-$-$ progress: '', f5.1, ''% ('', i4, ''/'', i4, '') $-$-$'')') &
     perc, i, n
    call log_info(lll)
    open(unit=UNIT_, file=logging_path_progress, status='replace', err=10)

    write(UNIT_, '(i4, ''/'', i4)') i, n
    close(UNIT_)
    goto 11

    10 continue
    call log_warning('Could not create progress indicator file')

    11 continue
  end


  !---------------------------------------------------------------------------------------
  !> Asserts i1 <= i2; if not, halts showing message
  !>
  !> This routine is called to make sure that we won't try to access an element beyond the
  !> maximum allocated for an array, since Fortran doesn't care about that.

  subroutine assert_le(i1, i2, title, name1, name2)
    integer, intent(in) :: i1, i2
    character(len=*), intent(in) :: &
     title, & !< title of caller routine, e.g., "integra()"
     name1, & !< name of argument corresponding to the value in i1
     name2    !< name of argument corresponding to the value in i2

    if (i1 .gt. i2) then
      write(lll,10) title, name1, name2, i1, i2
      10 format(A,': ',A,' > ',A,' (',I5,' > ',I5,')')
      call pfant_halt(lll)
    end if
  end



  !=======================================================================================

  !---------------------------------------------------------------------------------------
  !> Internal routine, MUST NOT be called from outside

  subroutine do_logging(s, level)
    character(len=*), intent(in) :: s
    character(len=8) :: t
    integer level
    integer, parameter :: UNIT_DUMP = 179
    logical, save :: flag_first_call = .true.

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

    if (logging_stdout) then
      write(*,*) '(', t, ') :: ', trim(s)
    end if

    if (logging_dump) then
      if (flag_first_call) then
        open(unit=UNIT_DUMP, file=logging_path_dump, status='unknown')
      end if

      write(UNIT_DUMP, *) '(', t, ') :: ', trim(s)
    end if

    flag_first_call = .false.
  end

end module logging

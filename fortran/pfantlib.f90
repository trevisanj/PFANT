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

!-----------------------------------------------------------------------------------------
! **pfantlib.f90**
! This file contains rountines shared between innewmarcs, hydro2, pfant, nulbad etc



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Declarations of constants (PARAMETER) used throughout.
!   - *all* dimensions of arrays

module dimensions
  implicit none


  !=====
  ! "delta lambda" parameters
  !=====

  ! amount to stretch calculation interval (both to the left and to the right)
  !
  real*8, parameter :: LAMBDA_STRETCH = 0.
  ! assumed width of a hydrogen line (from center to end of tail)
  real*8, parameter :: H_LINE_WIDTH = 35.


  !=====
  ! Dimensions related to dfile:filetoh
  !=====

  ! Maximum number of "filetoh" files
  integer, parameter :: MAX_FILETOH_NUM_FILES=13
  ! maximum number of points of each hydrogen line calculated (half of the line, from peak to either bottom)
  integer, parameter :: MAX_FILETOH_JMAX=100
  ! Tied with other constant by relation: MAX_FILETOH_JJMAX = MAX_FILETOH_JMAX*2-1
  integer, parameter :: MAX_FILETOH_JJMAX = MAX_FILETOH_JMAX*2-1

  !=====
  ! Dimensions related to *dissoc file*
  !=====

  ! Maximum number of metal rows in dissoc.dat
  ! (number of elements actually used is specified by variable
  ! dissoc_nmetal <= MAX_DISSOC_NMETAL)
  integer, parameter :: MAX_DISSOC_NMETAL=50

  ! Maximum number of molecule rows in dissoc.dat
  ! *Note: bit overdimensioned at the moment, considering that only 21 molecules have been catalogued.
  integer, parameter :: MAX_DISSOC_NMOL=600

  ! Maximum atomic number that can be found in dissoc.dat
  integer, parameter :: MAX_DISSOC_Z = 100

  !=====
  ! Dimensions related to *mod file*
  !=====

  ! Maximum possible value of modele%ntot. This used to be 50, but MARCS Sun models have
  ! 56 layers. The maximum possible is really 56, because the model record is 1200-byte long,
  ! fitting maximum 56 layers of data. If MARCS website changes to accomodate more layers,
  ! everything will neew to be redimensioned (but it is easy)
  integer, parameter :: MAX_MODELES_NTOT=56

  !=====
  ! Dimensions related to *partit file*
  !=====

  ! Maximum number of "items" in *partit file*:
  !   - Maximum value for partit_npar
  !   - Second dimension of partit_tabu
  integer, parameter :: MAX_PARTIT_NPAR=85
  ! Third dimension of partit_TABU
  integer, parameter :: MAX_PARTIT_KMAX=63

  !=====
  ! Dimensions related to *abonds file*
  !=====

  ! Maximum number of abundances in abonds.dat
  integer, parameter :: MAX_ABONDS_NABOND=100

  !=====
  ! Dimensions related to *absoru2 file*
  !=====

  ! Maximum value for absoru2_NM
  integer, parameter :: MAX_ABSORU2_NM=30

  ! Maximum value for absoru2_NR(J)
  integer, parameter :: MAX_ABSORU2_NRR=9  ! Maximum value for each element of absoru2_numset
                                           ! (maximum number of ionization degrees)
  integer, parameter :: MAX_ABSORU2_NUMSET_I=41

  !=====
  ! Dimensions related to *atoms file*
  !=====

  integer, parameter :: &
   MAX_ATOMS_NBLEND=100000, & ! Half of the maximum the number of rows in *atoms file*
   MAX_ATOMS_F_NBLEND=20000      ! Maximum number of spectral lines possible within the interval LZERO, LFIN

  !=====
  ! Dimensions related to *molecules file*
  !=====

  ! Maximum Number of molecules in *molecules file*
  integer, parameter :: MAX_NUM_MOL=30

  ! Maximum number of transitions ("Set-Of-Lines") for each molecule (Old "NTR")
  integer, parameter :: MAX_KM_NV_PER_MOL=200

  integer, parameter :: &
   MAX_KM_LINES_TOTAL=1400000, & ! Maximum number of spectral lines in *molecules file*
                                   ! pertaining all molecules
   MAX_KM_F_MBLEND=1400000 ! Maximum number of spectral lines that can be filtered in at a
                        ! filtering operation performed by filter_molecules()

  !=====
  ! Misc
  !=====
  ! Maximum number of command-line options
  integer, parameter :: MAX_NUM_OPTIONS = 100

  ! dtot maximum: maximum number of points in each calculation interval.
  integer, parameter :: MAX_DTOT=100000
end




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! module "logging":
!   - Logging facility: routines to write text to screen and/or log file.
!
! Usage:
! - Set logging_LEVEL (optional, defaults to DEBUG)
! - Call CRITICAL() / ERROR() / WARNING() / INFO() / DEBUG()
!
! Logging message will only be shown if logging_LEVEL is <= corresponding level of subroutine called.
! E.g., corresponding level of subroutine DEBUG() is LOGGING_DEBUG

module logging
  implicit none

  ! variable declared for convenience, to be used to log formatted output as the
  ! following example:
  !     write(lll,*) ...
  !     call log_info(lll)
  character*512 lll

  ! Logging levels copied from Python (except LOGGING_HALT)
  integer, parameter ::   &
   LOGGING_HALT     = 60, &  ! Maximum logging level; logging just before system halts
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
  ! Possible values: LOGGING_HALT, LOGGING_CRITICAL, LOGGING_ERROR,
  ! LOGGING_WARNING, LOGGING_INFO (default), LOGGING_DEBUG
  integer :: logging_level = LOGGING_INFO
  ! Full path to file to record progress indication
  character*256 :: logging_fn_progress = 'progress.txt'
  ! If set to .true., will display messages do standard output (usually the screen)
  logical :: logging_console = .true.
  ! If set to .true., will echo logged messages into dump file specified by logging_fn_dump,
  ! besides logging to standard output.
  logical :: logging_dump = .false.
  ! Full path to file to record logging messages indication
  character*256 :: logging_fn_dump = 'fortran_messages.log'


  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  private :: do_logging

  integer, private :: unit_dump


  save
contains
  !---------------------------------------------------------------------------------------
  ! Logs message at HALT level and halts program execution with error code -1.
  !
  ! Error code -1 allows a program that calls PFANT to know that PFANT stopped
  ! due to an error situation (normal program execution ends with error code 0).

  subroutine log_and_halt(s, is_bug, is_assertion)
    ! Log message
    character(len=*), intent(in) :: s
    ! (default=.false.) Whether halting program because of a bug.
    logical, optional, intent(in) :: is_bug
    ! (default=.false.) Whether halting program because of assertion error.
    ! If .true., will print "assertion error" before the message
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
  ! Logs message as HALT. Logs unconditionally (independent of logging level).
  !
  ! This allows the calling routine to log halt-level messages before calling
  ! log_and_halt(). As a rule, always call pftant_halt() after 1 or more calls to
  ! log_halt().

  subroutine log_halt(s)
    character(len=*), intent(in) :: s
    call do_logging(s, LOGGING_HALT)
  end


  !---------------------------------------------------------------------------------------
  ! Generic logging with level passed as argument.
  !
  ! This routine also provides an additional flag_dress argument, which allows to switch
  ! off logging message "dressing" ("dressing" here means the addition of extra
  ! information such as the logging level, date/time etc)

  subroutine log_any(s, level, flag_dress)
    character(len=*), intent(in) :: s
    integer, intent(in) :: level
    ! If .true., adds information to string, e.g., logging level.
    ! If .false., logs exactly what is in "s", without dressing with more info.
    ! Default: .true.
    logical, intent(in), optional :: flag_dress
    logical :: flag_dress_

    flag_dress_ = .true.
    if (present(flag_dress)) flag_dress_ = flag_dress

    if (logging_level .le. level) then
      call do_logging(s, level, flag_dress)
    end if
  end

  !---------------------------------------------------------------------------------------
  ! Logs message as CRITICAL

  subroutine log_critical(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_CRITICAL) then
      call do_logging(s, LOGGING_CRITICAL)
    end if
  end

  !---------------------------------------------------------------------------------------
  ! Logs message as ERROR

  subroutine log_error(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_ERROR) then
    call do_logging(s, LOGGING_ERROR)
    end if
  end

  !---------------------------------------------------------------------------------------
  ! Logs message as WARNING

  subroutine log_warning(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_WARNING) then
    call do_logging(s, LOGGING_WARNING)
    end if
  end

  !---------------------------------------------------------------------------------------
  ! Logs message as INFO

  subroutine log_info(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_INFO) then
    call do_logging(s, LOGGING_INFO)
    end if
  end

  !---------------------------------------------------------------------------------------
  ! Logs message as DEBUG

  subroutine log_debug(s)
    character(len=*), intent(in) :: s
    if (logging_level .le. LOGGING_DEBUG) then
    call do_logging(s, LOGGING_DEBUG)
    end if
  end

  !---------------------------------------------------------------------------------------
  ! Logs progress
  !
  ! uses log_info() to write to screen + writes information into
  ! logging_fn_progress
  !
  ! If cannot create file, does not bother (warns)

  subroutine log_progress(i, n)
    integer, intent(in) :: i, & ! current iteration
                           n    ! number of iterations
    real*8 perc
    integer myunit
    perc = 100.*i/n

    !#assertion
    if (n .gt. 9999) then
      call log_and_halt('Cannot log progress for number of iterations > 9999', &
       is_assertion=.true.)
    end if

    write (lll,'(''$-$-$ progress: '', f5.1, ''% ('', i4, ''/'', i4, '') $-$-$'')') &
     perc, i, n
    call log_info(lll)
    open(newunit=myunit, file=logging_fn_progress, status='replace', err=10)

    write(myunit, '(i4, ''/'', i4)') i, n
    close(myunit)
    goto 11

    10 continue
    call log_warning('Could not create progress indicator file')

    11 continue
  end


  !---------------------------------------------------------------------------------------
  ! Asserts i1 <= i2; if not, halts showing message
  !
  ! This routine is called to make sure that we won't try to access an element beyond the
  ! maximum allocated for an array, since Fortran doesn't care about that.

  subroutine assert_le(i1, i2, title, name1, name2)
    integer, intent(in) :: i1, i2
    character(len=*), intent(in) :: &
     title, & ! title of caller routine, e.g., "integra()"
     name1, & ! name of argument corresponding to the value in i1
     name2    ! name of argument corresponding to the value in i2

    if (i1 .gt. i2) then
      write(lll,10) title, name1, name2, i1, i2
      10 format(A,': ',A,' > ',A,' (',I5,' > ',I5,')')
      call log_and_halt(lll)
    end if
  end


  !=======================================================================================

  !---------------------------------------------------------------------------------------
  ! Internal routine, MUST NOT be called from outside

  subroutine do_logging(s, level, flag_dress)
    character(len=*), intent(in) :: s
    ! If .true., adds information to string, e.g., logging level.
    ! If .false., logs exactly what is in "s", without dressing with more info.
    ! Default: .true.
    logical, intent(in), optional :: flag_dress
    character(len=8) :: t
    integer level
    logical, save :: flag_first_call = .true.
    logical :: flag_dress_

    flag_dress_ = .true.
    if (present(flag_dress)) flag_dress_ = flag_dress

    if (flag_dress_) then
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
    end if

    if (logging_console) then
      call do_writing(6)
    end if

    if (logging_dump) then
      if (flag_first_call) then
        open(newunit=unit_dump, file=logging_fn_dump, status='unknown')
      end if

      call do_writing(unit_dump)
    end if

    flag_first_call = .false.

  contains
    ! Writes (dressed or undressed) to unit of choice

    subroutine do_writing(unit_)
      integer :: unit_
      if (flag_dress_) then
        write(unit_, '(4a)') '(', t, ') :: ', trim(s)
      else
        write(unit_, '(a)') trim(s)
      end if
    end
  end

end module logging



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Miscellanea of routines.
!
module misc
  use logging
  use dimensions
  implicit none

  ! Maximum number of rows in a file with comments, e.g. hmap.dat.
  ! Used in misc.f90
  integer, parameter :: MAX_FILE_ROWS=1000
contains

  ! Checks if hydrogen line interval overlaps with calculation interval
  !
  ! Overlap is checked for these two intervals:
  !
  ! [clam-H_LINE_WIDTH, clam+H_LINE_WIDTH]  and
  !
  ! [llzero-LAMBDA_STRETCH, llfin+LAMBDA_STRETCH]
  logical function h_line_is_inside(clam, llzero, llfin) result(res)
    real*8, intent(in) :: &
     clam,   & ! central lambda of a hydrogen line
     llzero, & ! lower boundary of calculation interval, probably taken from *main file*
     llfin     ! lower boundary of calculation interval, probably taken from *main file*

    res = .false.
    if (clam+H_LINE_WIDTH .ge. llzero-LAMBDA_STRETCH .and. &
        clam-H_LINE_WIDTH .le. llfin+LAMBDA_STRETCH) then
      res = .true.
    end if
  end

  ! Adjusts a 2-character string containing atomic symbol to right-aligned uppercase
  !
  ! This is used to prevent against left-alignment/lovercase  in specifying the atomic
  ! symbols in several input data files.
  !
  ! Example: "O ", " o", "o ", all will result in " O"

  function adjust_atomic_symbol(elem) result(res)
    character(len=*), intent(in) :: elem
    character(len=2) :: res
    if (len(elem) .ne. 2) &
      call log_and_halt('adjust_atomic_symbol(): got '//int2str(len(elem))//&
       '-char string (wants 2-char string)', is_assertion=.true.)
    res = to_upper(adjustr(elem))
  end

  ! Converts a string to lower case.
  !
  ! *Note* Works on A-Z letters only (does not handle letter modifiers such as acute,
  !       tilde etc)
  !
  ! Source: http://stackoverflow.com/questions/10759375/how-can-i-write-a-to-upper-or-to-lower-function-in-f90

  pure function to_lower(x) result (string)
      character(*), intent(in) :: x
      character(len(x))      :: string
      integer :: ic, i
      character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

      string = x
      do i = 1, len_trim(x)
          ic = index(cap, x(i:i))
          if (ic > 0) string(i:i) = low(ic:ic)
      end do
  end function to_lower

  ! Converts a string to upper case.
  !
  ! *Note* Works on a-z letters only (does not handle letter modifiers such as acute,
  !       tilde etc)
  !
  ! Source: http://stackoverflow.com/questions/10759375/how-can-i-write-a-to-upper-or-to-lower-function-in-f90

  pure function to_upper(x) result (string)
      character(*), intent(in) :: x
      character(len(x))      :: string
      integer :: ic, i
      character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

      string = x
      do i = 1, len_trim(x)
          ic = index(low, x(i:i))
          if (ic > 0) string(i:i) = cap(ic:ic)
      end do
  end

  ! Converts an integer to string
  !
  ! *Note* @c x is limited to 80 digits

  pure function int2str(x) result (string)
    integer, intent(in) :: x
    character(:), allocatable :: string
    character(80) :: ch

    write(ch,*) x
    string = trim(adjustl(ch))
  end function


  ! Converts a real*8 number to string

  pure function real82str(x, dec) result (string)
    real*8, intent(in) :: x
    ! Number of decimal places (optional). If not passed, will use Fortran "*" formatting.
    integer, intent(in), optional :: dec
    character(:), allocatable :: string
    character(80) :: ch

    if (present(dec)) then
      write(ch,'(f18.'//int2str(dec)//')') x
    else
      write(ch,*) x
    end if

    string = trim(adjustl(ch))
  end function

  ! Converts a real*4 number to string

  pure function real42str(x, dec) result (string)
    real*4, intent(in) :: x
    ! Number of decimal places (optional). If not passed, will use Fortran "*" formatting.
    integer, intent(in), optional :: dec
    character(:), allocatable :: string
    character(80) :: ch

    if (present(dec)) then
      write(ch,'(f18.'//int2str(dec)//')') x
    else
      write(ch,*) x
    end if

    string = trim(adjustl(ch))
  end function


  ! Converts a logical value to string
  !
  !   - .true. is converted to "T"
  !   - .false. is converted to "F"

  pure function logical2str(x) result (string)
    logical, intent(in) :: x
    character(:), allocatable :: string

    if (x) then
      string = 'T'
    else
      string = 'F'
    end if
  end function

  !=======================================================================================
  ! Converts a logical value to 0/1

  pure function logical2int(x) result (y)
    logical, intent(in) :: x
    integer :: y

    if (x) then
      y = 1
    else
      y = 2
    end if
  end function

  !=======================================================================================
  ! Converts an integer to logical. Accepts only 0 or 1
  !
  ! If argument is not (0 or 1), halts the program

  function int2logical(x) result (y)
    integer, intent(in) :: x
    logical :: y

    if (x .eq. 0) then
      y = .false.
    elseif (x .eq. 1) then
      y = .true.
    else
      y = .false.  ! no effect, just to shut up compiler "maybe uninitialized" message
      call log_and_halt('int2logical() accepts only 0/1, not '//int2str(x))
    end if
  end function

  !=======================================================================================
  ! Trims spaces at the right and adds a final slash, if the latter is not present.
  !
  ! Examples:
  !
  ! input             --> result
  ! --------------------------------
  ! "qwe/asd        " --> "qwe/asd/"
  ! "qwe/asd/       " --> "qwe/asd/"
  ! "qwe/asd/"        --> "qwe/asd/"
  !

  function trim_and_add_slash(x) result(y)
    character(1) :: BACKSLASH = '\'
    character(*), intent(in) :: x
    character(:), allocatable :: y
    integer i, n
    y = trim(x)
    n = len(y)
    do i = 1, n  ! Replaces backslash by forward slash
      if (y(i:i) .eq. BACKSLASH) y(i:i) = '/'
    end do
    if (y(n:n) .ne. '/') y = y // '/'
  end


  !=======================================================================================
  ! Replaces one character by another in string
  !
  ! The input argument is changed itself
  !

  subroutine replace_char(x, search, replace)
    character(*), intent(inout) :: x
    character(1), intent(in) :: search, replace
    integer i
    do i = 1, len(x)
      if (x(i:i) .eq. search) x(i:i) = replace
    end do
  end

  !=======================================================================================
  ! Creates a logical vector indicating which rows should be skipped in a text file.
  !
  ! This subroutine opens a text file and sweeps it until the end, then closes it.
  ! If row i starts with a "#" or is blank, skip_row(i) will be .true., otherwise
  ! .false.
  !
  ! n is the total number of rows in file.

  subroutine map_file_comments(path_to_file, skip_row, n)
    integer, parameter :: UNIT_=195
    character(len=*), intent(in) :: path_to_file
    ! element i  (i=1,n) will be .true. if row i is a comment line (starts with "#"),
    ! or is blank
    logical, intent(out) :: skip_row(MAX_FILE_ROWS)
    ! Number of rows in text file
    integer, intent(out) :: n

    integer :: n_temp
    character(len=128) :: s_temp0
    character(len=:), allocatable :: s_temp
    logical :: skip

    open(unit=UNIT_,file=path_to_file, status='old')

    n_temp = 0
    do while (.true.)
      read(UNIT_,'(a)',end=10) s_temp0

      n_temp = n_temp+1

      if (n_temp .gt. MAX_FILE_ROWS) &
        call log_and_halt('Increase MAX_FILE_ROWS (='//int2str(MAX_FILE_ROWS)//')')

      s_temp = trim(adjustl(s_temp0))  ! tolerant with rows starting with spaces

      if (len(s_temp) .eq. 0) then
        skip = .true.
      else if (s_temp(1:1) .eq. '#') then
        skip = .true.
      else
        skip = .false.
      end if
      skip_row(n_temp) = skip
    end do

    10 continue
    n = n_temp
    close(UNIT_)
  end


  ! Parses string into array of integers
  !
  ! Examples of valid strings:
  !
  ! '[1, 2, 3, 4]'
  ! '1, 2, 3, 4'
  ! '1  2 3     4'
  ! 'kdjd1sdkljfsd2shjdfkl3sdfklsdfjkl4sdlk'
  !
  ! The examples above all result in <code>(1, 2, 3, 4)</code>.
  !
  ! Very tolerant: everything that is not 0-9 is considered a separator
  !
  ! **Note** This was used to parse list of molecules to "turn off"; however,
  !          as now there is an editor for the molecular lines file (mled.py),
  !          one can delete the molecules directly from the file, so this routine is
  !          no longer used, however it was kept here as it may be useful for something
  !          in the future.

  subroutine parse_int_array(str, molidxs, n)
    use logging
    ! String to be parsed, e.g. '[3, 4, 5]'
    character(*), intent(in) :: str
    ! Integer numbers found in string
    integer, intent(out) :: molidxs(:)
    ! Number of integer numbers found
    integer, intent(out) :: n
    integer, parameter :: UNINIT=-1, SEP=0, NUM=1
    integer i_len, i, state, break_point, ascii
    logical flag_digit

    i_len = len(str)

    state = UNINIT
    flag_digit = .false.
    n = 0
    break_point = -1
    do i = 1, i_len+1
      ! only two possibilities: character is either a digit or not
      if (i .eq. i_len+1) then
        flag_digit = .false.
      else
        ascii = ichar(str(i:i))
        flag_digit = ascii .ge. 48 .and. ascii .le. 57  ! 48:0; 57: 9
      end if

      if (.not. flag_digit) then
        if (state .eq. NUM) then
          ! end-of-number
          n = n+1
          call assert_le(n, size(molidxs), 'parse_int_array()', 'n', 'size(molidxs)')
          read(str(break_point:i-1), *) molidxs(n)
        else
          ! does nothing until finds a digit
        end if
        state = SEP
      else
        if (state .ne. NUM) then
          ! beginning-of-number
          state = NUM
          break_point = i
        end if
      end if
    end do
  end

  ! Splits string into two alphanumeric characters representing atomic symbols
  !
  ! Delimiter is space
  !
  ! **Example** of str: 'MG H' --> ('MG', ' H')

  subroutine parse_symbols(str, symbols)
    use logging
    ! String to be parsed
    character(*), intent(in) :: str
    ! symbols
    character(len=2), intent(out) :: symbols(2)
    integer, parameter :: UNINIT=-1, SEP=0, SYM=1
    integer i_len, i, state, break_point, ascii, n
    logical flag_alpha

    i_len = len(str)

    state = UNINIT
    flag_alpha = .false.
    n = 0
    break_point = -1
    do i = 1, i_len+1
      ! only two possibilities: character is either a digit or not
      if (i .eq. i_len+1) then
        flag_alpha = .false.
      else
        ascii = ichar(str(i:i))
        flag_alpha = ascii .ge. 65 .and. ascii .le.  90 .or. &  ! A; Z
                     ascii .ge. 97 .and. ascii .le. 122         ! a; z
      end if

      if (.not. flag_alpha) then
        if (state .eq. SYM) then
          ! end-of-number
          n = n+1

          if (n .gt. 2) then
            call log_and_halt('More than two atomic symbols in '''//trim(str)//''', '//&
             'however diatomic molecule expected')
          end if

          if (i-break_point .gt. 2) then
            call log_and_halt('Symbol '''//str(break_point:i-1)//&
             ''' is too big, maximum allowed size is 2')
          end if

          read(str(break_point:i-1), *) symbols(n)
          symbols(n) = adjust_atomic_symbol(symbols(n))
        else
          ! does nothing until finds a digit
        end if
        state = SEP
      else
        if (state .ne. SYM) then
          ! beginning-of-number
          state = SYM
          break_point = i
        end if
      end if
    end do

    if (n .ne. 2) then
      call log_and_halt('Less than two atomic symbols in '''//trim(str)//''', '//&
       'however diatomic molecule expected')
    end if

  end


  ! Parses string into pairs of integers
  !
  ! This is similar to parse_int_array(). Anything that is not 0-9 is considered
  ! a separator

  subroutine parse_transitions(str, transitions, nv, titulo)
    use logging
    ! String to be parsed, e.g.
    character(*), intent(in) :: str
    ! Integer numbers found in string
    integer, intent(out) :: transitions(2, MAX_KM_NV_PER_MOL)
    ! Number of transitions found
    integer, intent(out) :: nv
    ! Complete molecule 'titulo' for error logging only
    character(*), intent(in) :: titulo
    integer, parameter :: UNINIT=-1, SEP=0, NUM=1
    integer i_len, i, state, break_point, ascii, sup_low, n
    logical flag_digit

    i_len = len(str)

    state = UNINIT
    flag_digit = .false.
    n = 0  ! number counter (must be even  at the end)
    nv = 0
    sup_low = 2  ! either 1 or 2, first index in transitions (initialized with 2)
    break_point = -1
    do i = 1, i_len+1
      ! only two possibilities: character is either a digit or not
      if (i .eq. i_len+1) then
        flag_digit = .false.
      else
        ascii = ichar(str(i:i))
        flag_digit = ascii .ge. 48 .and. ascii .le. 57  ! 48:0| 57: 9
      end if

      if (.not. flag_digit) then
        if (state .eq. NUM) then
          ! end-of-number

          n = n+1
          if (sup_low .eq. 1) then
            sup_low = 2
          else
            sup_low = 1
            nv = nv+1
          end if

          call assert_le(nv, MAX_KM_NV_PER_MOL, 'parse_transitions()', 'nv', 'maximum number of transitions per molecule')
          read(str(break_point:i-1), *) transitions(sup_low, nv)

        else
          ! does nothing until finds a digit
        end if
        state = SEP
      else
        if (state .ne. NUM) then
          ! beginning-of-number
          state = NUM
          break_point = i
        end if
      end if
    end do

    if (mod(n, 2) .ne. 0) then
      call log_and_halt('parse_transitions() in molecule '''//trim(titulo)//&
       ''': number of numbers to parse must be even, but was '//int2str(n))
    end if
  end
end module misc




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Module containing quicksort for array of real numbers
!
!
! ! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.
!

module qsort
  implicit none
contains

  !=======================================================================================
  ! Credits (verbatim from @ref http://jblevins.org/mirror/amiller/qsort.f90):
  !
  ! Quick sort routine from:
  ! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
  ! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
  ! Modified by Alan Miller to include an associated integer array which gives
  ! the positions of the elements in the original order.
  !
  !
  ! Further changed by JT to use argument "n" instead of size(list).

  RECURSIVE SUBROUTINE quick_sort(list, order, n)
    REAL*8, DIMENSION (:), INTENT(IN OUT)  :: list
    INTEGER, DIMENSION (:), INTENT(OUT)  :: order
    ! Operation is restricted to elements 1 to n
    INTEGER, INTENT(IN) :: n

    ! Local variable
    INTEGER :: i

    DO i = 1, n
      order(i) = i
    END DO

    CALL quick_sort_1(1, n)

  CONTAINS

    !-------------------------------------------------------------------------------------
    ! Quick sort auxiliary internal.
    !
    ! Does the actual hard work (the main routine is mostly initialization).

    RECURSIVE SUBROUTINE quick_sort_1(left_end, right_end)

      INTEGER, INTENT(IN) :: left_end, right_end

      !     Local variables
      INTEGER             :: i, j, itemp
      REAL*8                :: reference, temp
      INTEGER, PARAMETER  :: max_simple_sort_size = 6

      IF (right_end < left_end + max_simple_sort_size) THEN
        ! Use interchange sort for small lists
        CALL interchange_sort(left_end, right_end)

      ELSE
        ! Use partition ("quick") sort
        reference = list((left_end + right_end)/2)
        i = left_end - 1; j = right_end + 1

        DO
          ! Scan list from left end until element >= reference is found
          DO
            i = i + 1
            IF (list(i) >= reference) EXIT
          END DO
          ! Scan list from right end until element <= reference is found
          DO
            j = j - 1
            IF (list(j) <= reference) EXIT
          END DO


          IF (i < j) THEN
            ! Swap two out-of-order elements
            temp = list(i); list(i) = list(j); list(j) = temp
            itemp = order(i); order(i) = order(j); order(j) = itemp
          ELSE IF (i == j) THEN
            i = i + 1
            EXIT
          ELSE
            EXIT
          END IF
        END DO

        IF (left_end < j) CALL quick_sort_1(left_end, j)
        IF (i < right_end) CALL quick_sort_1(i, right_end)
      END IF

    END SUBROUTINE quick_sort_1


    !-------------------------------------------------------------------------------------
    ! Quick sort auxiliary internal, does the swapping

    SUBROUTINE interchange_sort(left_end, right_end)

      INTEGER, INTENT(IN) :: left_end, right_end

      !     Local variables
      INTEGER             :: i, j, itemp
      REAL*8                :: temp

      DO i = left_end, right_end - 1
        DO j = i+1, right_end
          IF (list(i) > list(j)) THEN
            temp = list(i)
            list(i) = list(j)
            list(j) = temp

            itemp = order(i)
            order(i) = order(j)
            order(j) = itemp
          END IF
        END DO
      END DO

    END SUBROUTINE interchange_sort

  END SUBROUTINE quick_sort
end











!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
module welcome
  use logging
  use misc
contains
  ! Returns string representing current PFANT version

  function pfant_version() result(v)
    character(:), allocatable :: v
    v = ' v16.9.20-beta'
  end

  ! Displays welcome message
  !
  ! Banner created by patorjk.com Text to ASCII Art Generator (TAAG)
  !   - http://patorjk.com/software/taag/#p=display&f=Ivrit&t=PFANT
  !
  ! Other fonts considered:
  !   - http://patorjk.com/software/taag/#p=display&f=Small&t=PFANT
  !   - http://patorjk.com/software/taag/#p=display&f=Calvin%20S&t=Options
  !   - http://patorjk.com/software/taag/#p=display&f=Thick&t=PRIVATE
  !   - http://patorjk.com/software/taag/#p=display&f=Banner&t=AMERU
  !
  !
  ! *Note* This is considered logging at INFO level; therefore nothing will be outputted
  !       if the logging level is greater than this.
  !
  ! TODO (just an idea): I could make a bigger sky and select a random rectangle of it. At least 2x visible width and height to get uniform probability of given pixel being in the rectangle

  subroutine print_welcome(unit_)
    integer, intent(in) :: unit_

    if (logging_level .gt. LOGGING_INFO) return


  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  ! Shadow
  !  write(unit_,*) '   _ \   ____|  \      \  | __ __| '
  !  write(unit_,*) '  |   |  |     _ \      \ |    |   '
  !  write(unit_,*) '  ___/   __|  ___ \   |\  |    |   '
  !  write(unit_,*) ' _|     _|  _/    _\ _| \_|   _|   '
  !  write(unit_,*) '                                   '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  ! Italic
  !  write(unit_,*) '     ____  _________    _   ________'
  !  write(unit_,*) '    / __ \/ ____/   |  / | / /_  __/'
  !  write(unit_,*) '   / /_/ / /_  / /| | /  |/ / / /   '
  !  write(unit_,*) '  / ____/ __/ / ___ |/ /|  / / /    '
  !  write(unit_,*) ' /_/   /_/   /_/  |_/_/ |_/ /_/     '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  ! Glenyn
  !  write(unit_,*) ' '
  !  write(unit_,*) ' ____ ____ ___  __   ____ '
  !  write(unit_,*) ' | . \|  _\|  \ | \|\|_ _\'
  !  write(unit_,*) ' | __/| _\ | . \|  \|  || '
  !  write(unit_,*) ' |/   |/   |/\_/|/\_/  |/'
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !  write(unit_,*) '  ____  _____ _    _   _ _____ '
  !  write(unit_,*) ' |  _ \|  ___/ \  | \ | |_   _|'
  !  write(unit_,*) ' | |_) | |_ / _ \ |  \| | | |  '
  !  write(unit_,*) ' |  __/|  _/ ___ \| |\  | | |  '
  !  write(unit_,*) ' |_|   |_|/_/   \_\_| \_| |_|  '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '



    !write(unit_,*) '________________________________________________________________________'
    !write(unit_,*) '          `                  `                   `            ``        '
    !write(unit_,*) '                                                   `    `     ``        '
    !write(unit_,*) '             `                   `             `                        '
    !write(unit_,*) '                                           ``  ``                       '
    !write(unit_,*) '   `          @.     ``             `        ``                         '
    !write(unit_,*) '                                                `                       '
    !write(unit_,*) '                                               `                        '
    !write(unit_,*) '                        @                                               '
    !write(unit_,*) '             `      `              `                 `                  '
    !write(unit_,*) '`                     `           `                                     '
    !write(unit_,*) '                  `                                                     '
    !write(unit_,*) '               @`@`@ `          `                                       '
    !write(unit_,*) '`              `    `                            `             `        '
    !write(unit_,*) '               `                                                        '
    !write(unit_,*) '              `                   ____  _____ _    _   _ _____          '
    !write(unit_,*) '               `        `        |  _ \|  ___/ \  | \ | |_   _|         '
    !write(unit_,*) '        @        `  `            | |_) | |_ / _ \ |  \| | | |         ` '
    !write(unit_,*) '                    @            |  __/|  _/ ___ \| |\  | | |           '
    !write(unit_,*) '__/\/\/\_________________________|_|___|_|/_/___\_\_|_\_|_|_|_____/\/\__'


    write(unit_,*) ''
    write(unit_,*) ''
    write(unit_,*) ''
    write(unit_,*) '`   `        `              `                                           '
    write(unit_,*) '       Welcome to PFANT      `                   `            ``        '
    write(unit_,*) '                                                   `    `     ``        '
    write(unit_,*) '             `                   `             `                        '
    write(unit_,*) '                                           ``  ``                       '
    write(unit_,*) '   `          @.     ``             `        ``                         '
    write(unit_,*) '                                                `                       '
    write(unit_,*) '                                               `                        '
    write(unit_,*) '                        @                                               '
    write(unit_,*) '             `      `              `                 `                  '
    write(unit_,*) '`                     `           ` Bugs/crashes: please report issue at'
    write(unit_,*) '                  `                                                     '
    write(unit_,*) '               @`@`@ `          `      http://github.com/trevisanj/pfant'
    write(unit_,*) '`              `    `                            `             `        '
    write(unit_,*) '               `                                                        '
    write(unit_,*) '              `                     ____  _____ _    _   _ _____        '
    write(unit_,*) '               `        `          |  _ \|  ___/ \  | \ | |_   _|       '
    write(unit_,*) '        @        `  `              | |_) | |_ / _ \ |  \| | | |       ` '
    write(unit_,*) '   _                @              |  __/|  _/ ___ \| |\  | | |         '
    write(unit_,*) '__|o|______________________________|_|___|_|/_/___\_\_|_\_|_|_|_________'
    write(unit_,*) ''
    write(unit_,*) ''
    write(unit_,*) ''
  end

  subroutine play()
    print *, 'not yet'
  end
end


!'`   `        `              `                                           '
!'          `                  `                   `            ``        '
!'                                                   `    `     ``        '
!'             `                   `             `                        '
!'                                           ``  ``                       '
!'   `          @.     ``             `        ``                         '
!'                                                `                       '
!'                                               `                        '
!'                        @                                               '
!'             `      `              `                 `                  '
!'`                     `           `                                     '
!'                  `                                                     '
!'               @`@`@ `          `                                       '
!'`              `    `                            `             `        '
!'               `                                                        '
!'              `                                                         '
!'               `        `                                               '
!'        @        `  `                                                 ` '
!'                    @                                                   '

! Allows printing with coordinates on a "canvas"
!
! ASCII image is mounted, then printed on-screen at once

module ascii_canvas
  use logging
  use misc
  implicit none

  integer, parameter, private :: MAX_DIM=100

  type canvas
    character(len=MAX_DIM) :: rows(MAX_DIM)
    integer :: nr ! number of rows
    integer :: nc ! number of columns
  contains
    procedure :: init => canvas_init
    procedure :: paint => canvas_paint
    procedure :: print => canvas_print
  end type

contains

  ! Initializes with given number of rows and columns

  subroutine canvas_init(this, nr, nc)
    class(canvas) :: this
    integer, intent(in) :: nr, nc

    character(len=MAX_DIM) :: s_temp
    integer :: i

    call assert_le(nr, MAX_DIM, 'canvas_init()', 'nr', 'MAX_DIM')
    call assert_le(nc, MAX_DIM, 'canvas_init()', 'nc', 'MAX_DIM')

    s_temp = repeat(' ', MAX_DIM)

    this%nr = nr
    this%nc = nc
    do i = 1,nr
      this%rows(i) = s_temp
    end do
  end

  ! "paints" string in canvas

  subroutine canvas_paint(this, lin, col, str)
    class(canvas) :: this
    integer :: lin, col
    character(len=*), intent(in) :: str

    !integer :: len_eff, i


    call assert_le(lin, this%nr, 'canvas_paint()', 'lin', 'this%nr')

    call replace_part(this%rows(lin))

    !len_eff = min(len(str), this%nc-col+1)

    !write(*,*) 'len_eff=',len_eff


    !if (len_eff .gt. 0) then
    !  this%rows(lin)(col:col+len_eff-1) = str(1:len_eff)
      !do i = 1, len_eff
      !  this%rows(i+col-1, lin) = str(i:i)
      !end do

      !this%rows(col:col+len_eff-1, lin) = str(1:len_eff)
      !this%rows(lin,col:col+len_eff-1) = str(1:len_eff)
    !end if

    !write(*,*) '#',this%rows(col:col+len_eff-1, lin),'#'
    !write(*,*) '!',str(1:len_eff),'!'

    !write(*,*) '$',this%rows,'$'

  contains
    ! Does the "painting"

    subroutine replace_part(row)
      character(len=*) :: row ! row of the canvas
      integer :: len_eff

      len_eff = min(len(str), this%nc-col+1)


      if (len_eff .gt. 0) then
        row(col:col+len_eff-1) = str(1:len_eff)
      !do i = 1, len_eff
      !  this%rows(i+col-1, lin) = str(i:i)
      !end do

      !this%rows(col:col+len_eff-1, lin) = str(1:len_eff)
      !this%rows(lin,col:col+len_eff-1) = str(1:len_eff)
    end if

    !write(*,*) '#',this%rows(col:col+len_eff-1, lin),'#'
    !write(*,*) '!',str(1:len_eff),'!'

    !write(*,*) '$',this%rows,'$'
  end
  end

  ! prints to given unit

  subroutine canvas_print(this, flag_frame, level)
    class(canvas) :: this
    ! Whether to print a "frame" around (optional). Default: .false.
    logical, intent(in), optional :: flag_frame
    ! Logging level (optional). Default: LOGGING_INFO
    integer, intent(in), optional :: level

    logical :: flag_frame_
    character(len=:), allocatable :: dashes ! for the frame
    character(len=MAX_DIM) :: s_temp
    integer :: level_, i

    flag_frame_ = .false.
    if (present(flag_frame)) flag_frame_ = flag_frame

    level_ = LOGGING_INFO
    if (present(level)) level_ = level


    if (flag_frame_) then
      dashes = repeat('-', this%nc)
      call log_any('+'//dashes//'+', level_, .false.)
      do i = 1, this%nr
        s_temp = this%rows(i)
        call log_any('|'//s_temp(1:this%nc)//'|', level_, .false.)
      end do
      call log_any('+'//dashes//'+', level_, .false.)
    else
      do i = 1, this%nr
        s_temp = this%rows(i)
        call log_any(s_temp(1:this%nc), level_, .false.)
      end do
    end if
  end
end




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! MISCellaneous MATHs: re-usable Math library
!
! This module contains routines existing in the 2015- code base .
!
! Arrays passed to routines now have assumed-shape declarations, e.g.,
! @code
! real*8 :: fr(:)
! @endcode
!
! instead of
!
! @code
! real*8 :: fr(itot)
! @endcode
!
! In the example latter, Fortran won't give a runtime error if
! <code>itot > size(fr)</code>. Therefore, declaring <code>fr(ntot)</code>
! provides *no* error protection.
!
! On the other hand, declaring <code>fr(:)</code> gives access to the array size
! using function <code>size()</code>, allows for placing assertions inside the
! routines.
!
! **Note** However, assertions may slow down the code. Once the code is tested enough,
! assertions should be taken out from routines that are called many times. *However*,
! assertions are good documentation, so don't delete them; rather, comment them out.!


module misc_math
  use logging
  implicit none

  ! Mathematical constants used throughout. Better to use these than define them every
  ! time they are needed inside a routine
  real*8, parameter :: PI = acos(-1.), & ! 3.14169265...
                       RPI = sqrt(PI), & ! square root of pi, approx 1.772453851
                       H = 6.6252E-27, & ! Plank constant in erg*s
                                         ! ISSUE actual value may be 6.6260E-27
                       C = 2.997929E+10,&! speed of light in cm/s
                       KB = 1.38046E-16  ! Boltzmann's constant in erg/K



contains
  ! Computes the Voigt function.
  !
  !
  ! COMPUTES THE VOIGHT FUNCTION  Y/PI*INTEGRAL FROM
  ! - TO + INFINITY OF  EXP(-T*T)/(Y*Y+(X-T)*(X-T)) DT
  ! LA FONCTION EST ENSUITE NORMALISEE
  !
  !
  ! The code seems to originate from Appendix A of [1]
  !
  ! **References**
  ! [1] Drayson, S. Roland. "Rapid computation of the Voigt profile." Journal of
  ! Quantitative Spectroscopy and Radiative Transfer 16.7 (1976): 611-614.
  !
  ! [2] A. Belafhal "The shape of spectral lines: widths and equivalent widths of the
  !     Voigt profile". Optics Communications 177 (2000). 111–118

  function hjenor(y,x,del) result (phi)
    real*8, intent(in) :: &
     y,   & ! "Relative importance of the Lorentzian and Gaussian contributions" (?) [2]
     x,   & ! **The actual variable** here
     del    ! Normalization term
    real*8 :: phi ! Result value
    real*8 voigt ! un-normalized Voigt function value
    real*8 vv,uu
    real*8 b(22),ri(15),xn(15)/10.,9.,2*8.,7.,6.,5.,4.,7*3./,        &
     yn(15)/3*.6,.5,2*.4,4*.3,1.,.9,.8,2*.7/,d0(35),d1(35),d2(35)    &
     ,d3(35),d4(35),hn(35),h/.201/,xx(3)/.5246476,1.65068,.7071068/  &
     ,hh(3)/.2562121,.2588268e-1,.2820948/,nby2(19)/9.5,9.,8.5,8.,   &
     7.5,7.,6.5,6.,5.5,5.,4.5,4.,3.5,3.,2.5,2.,1.5,1.,.5/,c(21)/     &
     .7093602e-7,-.2518434e-6,.8566874e-6,-.2787638e-5,.866074e-5,   &
     -.2565551e-4,.7228775e-4,-.1933631e-3,.4899520e-3,-.1173267e-2, &
     .2648762e-2,-.5623190e-2, .1119601e-1,-.2084976e-1,.3621573e-1, &
     -.5851412e-1,.8770816e-1, -.121664,.15584,-.184,.2/
    logical :: flag_first = .true.
    logical tru/.false./
    real*8 c0, dx, v, u, y2
    integer i, j, min, max, n
    save :: flag_first, b, ri, xn, yn, d0, d1, d2, d3, d4, hn, h, xx, hh, nby2, c

    if (flag_first) then
      ! initialization executed at first call

      flag_first = .false.

      tru=.false.
      b(1)=0.
      b(2)=0.7093602e-7

      if (tru) go to 104

      ! ! REGION I. COMPUTE DAWSON'S FUNCTION AT MESH POINTS
      ! tru = .true.

      do i=1,15
        ri(i)=-i/2.
      end do

      do i=1,25
        hn(i)=h*(i-.5)
        c0=4.*hn(i)*hn(i)/25.-2.

        do j = 2,21
          b(j+1)=c0*b(j)-b(j-1)+c(j)
        end do

        d0(i)=hn(i)*(b(22)-b(21))/5.
        d1(i)=1.-2.*hn(i)*d0(i)
        d2(i)=(hn(i)*d1(i)+d0(i))/ri(2)
        d3(i)=(hn(i)*d2(i)+d1(i))/ri(3)
        d4(i)=(hn(i)*d3(i)+d2(i))/ri(4)

        ! write(6,*)i,d0(i),d1(i),d2(i),d3(i),d4(i)
      end do
      flag_first = .false.
    else
    end if

    104 if (x-5.) 105,112,112
    105 if (y-1.) 110,110,106
    106 if (x.gt.1.85*(3.6-y)) go to 112

    ! REGION II CONTINUED FRACTION .COMPUTE NUMBER OF TERMS NEEDED
    ! write(6,*)'region II'
    if (y .lt. 1.45) go to 107
    i = int(y+y)
    go to 108

    107 continue
    i=int(11.*y)

    108 continue
    j = int(x+x+1.85)
    max = int(xn(j)*yn(i)+.46)
    min = min0(16,21-2*max)


    ! EVALUATED CONTINUED FRACTION
    uu=y
    vv=x
    do 109 j=min,19
      u=nby2(j)/(uu*uu+vv*vv)
      uu=y+u*uu
      vv=x-u*vv
    109 continue

    voigt=uu/(uu*uu+vv*vv)/RPI
    go to 10

    110 continue
    y2=y*y
    if (x+y.ge.5.) go to 113

    ! REGION I. COMMPUTE DAWSON'S FUNCTION AT X FROM TAYLOR SERIES
    n=int(x/h)
    dx=x-hn(n+1)
    u=(((d4(n+1)*dx+d3(n+1))*dx+d2(n+1))*dx+d1(n+1))*dx+d0(n+1)
    v=1.-2.*x*u

    ! TAYLOR SERIES EXPANSION ABOUT Y=0.0
    vv = exp(y2-x*x)*cos(2.*x*y)/1.128379-y*v
    ! write(6,*) n,u,dx,d0(n+1),d1(n+1),d2(n+1),d3(n+1),d4(n+1)
    uu = -y
    max = int(5.+(12.5-x)*.8*y)
    do 111 i = 2,max,2
      u = (x*v+u)/ri(i)
      v = (x*u+v)/ri(i+1)
      uu = -uu*y2
      vv = vv+v*uu
    111 continue
    voigt = 1.128379*vv
    ! write(6,*)'region i ',voigt,vv,x,y,del
    go to 10

    112 continue
    y2 = y*y
    if (y .lt. 11.-.6875*x) go to 113

    !  REGION IIIB  2 POINT GAUSS-HERMITE QUADRATURE
    u = x-xx(3)
    v = x+xx(3)
    voigt = y*(hh(3)/(y2+u*u)+hh(3)/(y2+v*v))
    ! write(6,*)'region IIIb ', voigt
    go to 10

    !  REGION IIIA 4-POINT GAUSS-HERMITE QUADRATURE.
    113 continue
    u = x-xx(1)
    v = x+xx(1)
    uu = x-xx(2)
    vv = x+xx(2)
    voigt = y*(hh(1)/(y2+u*u)+hh(1)/(y2+v*v)+hh(2)/(y2+uu*uu)+hh(2)/(y2+vv*vv))
    ! write(6,*)'region IIIa',voigt

    10 continue
    phi = voigt /  (RPI * del)
  end


  !---------------------------------------------------------------------------------------
  ! Returns index of minimum value of fr within interval [ia, iz]
  !
  ! Returns
  ! i* (ia <= i* <= iz) such that
  ! fr(i*) = minimum( fr(i), ia <= i <= iz)
  integer function iinf(fr,itot,ia,iz)
    real*8, intent(in):: fr(:) ! search vector
    integer, intent(in) :: &
     itot, & ! size of vector fr
     ia,   & ! interval lower index
     iz      ! interval upper index
    integer i, ia2
    real*8 fmin

    call assert_le(itot, size(fr), 'iinf()', 'itot', 'size(fr)')

    ia2=ia+1
    iinf=ia
    fmin=fr(ia)
    do 1 i=ia2,iz
      if(fr(i).gt.fmin) go to 1
      fmin=fr(i)
      iinf=i
    1 continue
  end


  !---------------------------------------------------------------------------------------
  ! Returns index of maximum value of fr within interval [ia, iz]
  !
  ! UNE FONCTION FR EST CONNUE EN ITOT POINTS. ON CHERCHE ENTRE
  ! LES POINTS IA ET IZ QUEL EST L INDICE I OU CETTE FONCTION
  ! EST MAXIMUM.

  integer function isup(fr, itot, ia, iz)
    real*8, intent(in):: fr(:) ! search vector
    integer, intent(in) :: &
     itot, & ! size of vector fr
     ia,   & ! interval lower index
     iz      ! interval upper index
    integer i, ia2
    real*8 fmax

    call assert_le(itot, size(fr), 'isup()', 'itot', 'size(fr)')

    ia2=ia+1
    isup=ia
    fmax=fr(ia)
    do 1 i=ia2,iz
      if(fr(i) .lt. fmax) go to 1
      fmax=fr(i)
      isup=i
    1 continue
  end

  !---------------------------------------------------------------------------------------
  ! Returns minimum value of ifa within interval [ia, iz]
  !
  ! This function is designed for integer vectors.

  integer function mini(ifa, ntot, ia, iz)
    integer, intent(in) :: &
     ifa(:), & ! search vector
     ntot,      & ! size of vector ifa
     ia,        & ! interval lower index
     iz           ! interval upper index
    integer i, ia2

    call assert_le(ntot, size(ifa), 'mini()', 'ntot', 'size(ifa)')

    mini=ifa(ia)
    ia2=ia+1
    do i=ia2,iz
      if(ifa(i) .lt. mini) then
        mini=ifa(i)
      end if
    end do
  end

  !---------------------------------------------------------------------------------------
  ! Returns maximum value of ifa within interval [ia, iz]

  integer function maxi(ifa, ntot, ia, iz)
    integer, intent(in) :: &
     ifa(:), & ! search vector
     ntot,      & ! size of vector ifa
     ia,        & ! interval lower index
     iz           ! interval upper index
    integer i, ia2

    call assert_le(ntot, size(ifa), 'maxi()', 'ntot', 'size(ifa)')

    maxi=ifa(ia)
    ia2=ia+1
    do i=ia2,iz
      if(ifa(i).gt.maxi) then
        maxi=ifa(i)
      end if
    end do
  end

  !---------------------------------------------------------------------------------------
  ! Numerical integration
  !
  ! *METHODE*: LA VALEUR DE L'INTEGRALE SUR L'INTERVALLE X(I), X(I+1)
  !   EST CALCULEE PAR LA FORMULE DE SIMPSON, LA VALEUR DE Y AU POINT
  !   MILIEU ETANT CALCULEE PAR INTERPOLATION CUBIQUE, PAR LA ROUTINE
  !   naitk3()
  subroutine integra(x, y, p, n, pdeb)
    real*8, intent(in) :: &
     x(:), & ! TABLEAU DE VALEURS DE LA VARIABLE INDEPENDANTE, PAR VALEURS CROISSANTES
     y(:), & ! TABLEAU DES VALEURS ASSOCIEES DE LA FONCTION A INTEGRER
     pdeb    ! VALEUR DE LA PRIMITIVE POUR X(1),PREMIERE VALEUR DU TABLEAU
    integer, intent(in) :: n ! maximum indexes in x, y, and p
    real*8, intent(out) :: p(0:) ! TABLEAU DES VALEURS DE LA PRIMITIVE AUX POINTS X(I)
    real*8 fx, xmilieu
    integer i, j

    call assert_le(n, size(p)-1, 'integra()', 'n', 'size(p)-1')
    call assert_le(n, size(x), 'integra()', 'n', 'size(x)')
    call assert_le(n, size(y), 'integra()', 'n', 'size(y)')

    p(1) = pdeb

    ! CAS SPECIAL DU PREMIER INTERVALLE
    xmilieu=(x(1)+x(2))/2.
    call naitk3(x(1),x(2),x(3),x(4), y(1),y(2),y(3),y(4),xmilieu,fx)
    p(2) = p(1)+((x(2)-x(1))/6.)*(y(1)+y(2)+4.*fx)

    ! CAS GENERAL
    do i=2,n-2
      xmilieu = (x(i)+x(i+1))/2.
      j = i-1
      call naitk3(x(j),x(j+1),x(j+2),x(j+3),y(j),y(j+1),y(j+2),y(j+3),xmilieu,fx)
      p(i+1) = p(i)+((y(i)+y(i+1)+4.*fx)/6.)*(x(i+1)-x(i))
    end do

    ! CAS SPECIAL DERNIER INTERVALLE
    xmilieu = (x(n-1)+x(n))/2.
    j = n-3
    call naitk3(x(j),x(j+1),x(j+2),x(j+3),y(j),y(j+1),y(j+2),y(j+3),xmilieu,fx)
    p(n) = p(n-1)+((x(n)-x(n-1))/6.)*(y(n-1)+y(n)+4.*fx)
  end

  !---------------------------------------------------------------------------------------
  ! CALCUL DE CH VAN DER WAALS,  APPROXIMATIONS D'UNSOLD (1955)
  !
  ! SI IS1 ET IS2 SONT EGAUX A S P D F FORMULE 82-54, SINON 82-55
  !

  real*8 function calch(kii, iz, kiex1, is1, kiex2, is2)
    real*8, intent(in) :: &
     kii,   & ! ?doc?
     kiex1, & ! ?doc?
     kiex2    ! ?doc?
    integer, intent(in) :: iz ! ?doc?
    character*1, intent(in) :: &
     is1, & ! ?doc?
     is2    ! ?doc?
    real*8 net1c, net2c, c61, c62
    integer, parameter :: il(4) = (/1,-5,-17,-35/)
    character*1, parameter :: &
     is(4) = (/'s','p','d','f'/), &
     ibl = ' '
    integer il1, il2, izc, i

    if (is1 .ne. ibl) go to 1
    c61 = 1.61e-33*(13.5*iz / (kii-kiex1))**2
    c62 = 1.61e-33*(13.5*iz / (kii-kiex2))**2
    go to 10

    1 continue
    do i=1,4
      if(is1 .eq. is(i)) exit
    end do

    il1=il(i)
    do i=1,4
      if(is2 .eq. is(i)) exit
    end do

    il2 = il(i)
    izc = iz**2
    net1c = 13.5 * izc / (kii-kiex1)
    net2c = 13.5 * izc / (kii-kiex2)
    c61 = 3.22e-34 * net1c *(5*net1c+il1)/ izc
    c62 = 3.22e-34 * net2c *(5*net2c+il2)/ izc

    10 calch = c62-c61
    return
  end



  !===============================================================================
  ! INTERPOLATION ROUTINES
  !===============================================================================


  !---------------------------------------------------------------------------------------
  ! EXTRAPOLE TETA(PG) POUR PG=0
  !
  ! ***TETA1 extrapolation parabolique sur les 3 derniers pts
  ! ***TETA2      "             "      sur les pts 2 3 et 4
  ! ***TETA3      "        lineaire    sur les 2 derniers pts
  ! (ici seul TETA3 est utilise)
  !

  real*8 function fteta0(pg, teta, n)
    ! Size of vectors pf and teta; example source is modele%ntot
    integer, intent(in) :: n
    real*8, intent(in) :: &
      pg(:), & ! Example source is modele%pg
      teta(:)  ! Example source is modele%teta
    real*8, dimension(5) :: pp1,tt1,pp2,tt2
    integer i
    real*8 teta3

    call assert_le(n, size(pg), 'fteta0()', 'n', 'size(pg)')
    call assert_le(n, size(teta), 'fteta0()', 'n', 'size(teta)')

    !~logical ecrit  i think this has been tested already, no need to verbose flag, would slow down the maths
    !~ecrit=.false.
    pp1(1)=pg(1)
    tt1(1)=teta(1)
    !~if(ecrit) write(6,*) pg(1), teta(1)
    do i=2,5
      pp1(i)=pg(i)
      pp2(i-1)=pg(i)
      tt1(i)=teta(i)
      tt2(i-1)=teta(i)
      !~if(ecrit) write(6,*) pg(i), teta(i)
    end do
    ! teta1=ft(0.,5,pp1,tt1)
    ! teta2=ft(0.,4,pp2,tt2)
    teta3=tt1(1) - pp1(1) * (tt1(1)-tt1(2)) / (pp1(1)-pp1(2))
    !~if(ecrit) write(6,*)teta3
    fteta0= teta3
    return
  end


  !---------------------------------------------------------------------------------------
  ! INTERPOLATION D UNE LISTE A PAS NON CONSTANT
  !
  ! Returns: f*(t), approximation of f(t)

  real*8 function ft(t,n,x,f)
    integer, intent(in) :: n ! Size of vectors x and f
    real*8, intent(in) :: &
     t,    & ! point of interest in the x-axis
     x(:), & ! x-values
     f(:)    ! f(x)-values
    real*8 t0, t1, t2, u0, u1, u2, a, b, c, d, e
    integer i, j

    call assert_le(n, size(x), 'ft()', 'n', 'size(x)')
    call assert_le(n, size(f), 'ft()', 'n', 'size(f)')

    do 1 j = 1,n
      i = j
      if(t-x(j)) 3, 2, 1
      2 continue
      ft = f(j)
      return
    1 continue

    3 continue
    if (i .eq. 1) i = 2
    if (i .ge. n) i = n-1

    t0 = t-x(i-1)
    t1 = t-x(i)
    t2 = t-x(i+1)
    u0 = x(i+1)-x(i-1)
    u1 = x(i+1)-x(i)
    u2 = x(i)-x(i-1)
    a  = t0/u0
    b  = t1/u1
    c  = t2/u2
    d  = t0/u1
    e  = t1/u0
    ft = f(i+1)*a*b - f(i)*d*c + f(i-1)*e*c
  end


  !---------------------------------------------------------------------------------------
  ! Linear interpolation
  !
  ! *Note* This routine is very similar to ftlin3h().

  subroutine ftlin3(n, x, y, itot, tt, ftt)
    integer, intent(in) :: n      ! Size of vectors x and y
    real*8, intent(in) :: x(:), & ! x-axis values of "known data"
                          y(:)    ! y-axis values of "known data"
    integer, intent(in) :: itot   ! Size of vectors tt and ftt
    real*8, intent(in) :: tt(:)   ! x-axis values of "wanted values"
    real*8, intent(out) :: ftt(:) ! y-axis values that will be calculated

    real*8 ft, dy, t0, t1, t2, u0, t
    integer j, jj, k

    call assert_le(n, size(x), 'ftlin3()', 'n', 'size(x)')
    call assert_le(n, size(y), 'ftlin3()', 'n', 'size(y)')
    call assert_le(itot, size(tt), 'ftlin3()', 'itot', 'size(tt)')
    call assert_le(itot, size(ftt), 'ftlin3()', 'itot', 'size(ftt)')

    j=2
    do k=1,itot
      t=tt(k)
      ! 103 format(5x,f10.3)
      ! write(6,103)t
      jj = j-1
      do 1 j = jj, n
        if (t-x(j)) 3, 2, 1
      1 continue

      !if (abs(t-x(j)) .le. 1e-38) goto 2  ! tolerance because numbers may not match because of tolerance
      !go to 10

      2 continue
      ft = y(j)
      if (j .eq. 1) j = j+1
      go to 4

      3 continue
      ! write(6,*) '   j=',j
      if(j .eq. 1) go to 10

      u0= y(j)-y(j-1)
      t0= x(j)-x(j-1)
      t1=   t -x(j-1)
      ! 104 format(i5,7f9.3)
      ! write(6,104) j, x(j-1), y(j-1), x(j),y(j), u0,t0,t1
      t2= t1/t0
      dy= u0*t2
      ft= y(j-1) + dy

      4 continue
      ftt(k)=ft
    end do

    return

    10 continue
    100 format('ftlin3(): On sort de la table d interpolation avec t=',e15.7, '. x(1)=', e15.7, '; x(n)=', e15.7)
    write(lll,100) t, x(1), x(n)
    call log_and_halt(lll)
  end



  !---------------------------------------------------------------------------------------
  ! interpolation

  real*8 function faitk30(xx, x, y, n)
    integer, intent(in) :: n ! Size of vectors x and y
    real*8, intent(in) :: &
     xx,     & ! ?doc?
     x(0:), & ! ?doc?; 0-based
     y(0:)    ! ?doc?; 0-based
    integer i, j
    real*8 resulta

    call assert_le(n, size(x)-1, 'faitk30()', 'n', 'size(x)-1')
    call assert_le(n, size(y)-1, 'faitk30()', 'n', 'size(y)-1')

    if (xx .lt. x(2)) then
      i=0
      goto 200
    else if (xx.gt.x(n-2)) then
      i = n-3
      goto 200
    else
      do j = 2, n-2
        if(xx .le. x(j)) go to 100
      end do
    endif

    100 continue
    i = j-2

    200 call naitk3(x(i),x(i+1),x(i+2),x(i+3),y(i),y(i+1),y(i+2),y(i+3),xx,resulta)
    faitk30 = resulta
  end


  !---------------------------------------------------------------------------------------
  ! INTERPOLATION PARABOLIQUE
  ! DANS LA TABLE X Y (N POINTS) ON INTERPOLE LES FTT CORRESPONDANT
  ! AUX TT  (ITOT POINTS) POUR TOUTE LA LISTE DES TT
  !
  ! *Note* ft2_hydro2() is very similar!

  subroutine ft2(n,x,y,itot,tt,ftt)
    integer, intent(in) :: &
     n, & ! Size of vectors x and y
     itot ! Size of vectors tt and ftt
    real*8, intent(in) :: &
     x(:),     & ! input x-axis values
     y(:),     & ! input y-axis values
     tt(:)       ! x-axis values of output (to be given)
    real*8, intent(out) :: &
     ftt(:)   ! y-axis values of output (to be calculated)
    real*8 ft, a, b, c, d, e, t, t0, t1, t2, u0, u1, u2
    integer i, inv, j, k

    call assert_le(n, size(x), 'ft2()', 'n', 'size(x)')
    call assert_le(n, size(y), 'ft22()', 'n', 'size(y)')
    call assert_le(itot, size(tt), 'ft2()', 'itot', 'size(tt)')
    call assert_le(itot, size(ftt), 'ft2()', 'itot', 'size(ftt)')

    inv = -1
    if (x(n).lt.x(1)) inv = 1
    do k = 1,itot
      t = tt(k)
      if (inv) 5, 6, 6
      5 continue
      do 1 j = 1,n
        i = j
        if (t-x(j)) 3, 2, 1
      1 continue
      go to 10

      6 continue
      do 7 j = 1, n
        i = j
        if(t-x(j)) 7, 2, 3
      7 continue
      go to 10

      2 continue
      ft = y(j)
      go to 4

      3 continue
      if (i .eq. 1) i = 2
      if (i .ge. n) i = n-1
      t0 = t-x(i-1)
      t1 = t-x(i)
      t2 = t-x(i+1)
      u0 = x(i+1)-x(i-1)
      u1 = x(i+1)-x(i)
      u2 = x(i)-x(i-1)
      a  = t0/u0
      b  = t1/u1
      c  = t2/u2
      d  = t0/u1
      e  = t1/u0
      ft = y(i+1)*a*b - y(i)*d*c + y(i-1)*e*c

      4 continue
      ftt(k) = ft
    end do
    return

    ! Some value within tt is out of x boundaries
    10 continue
    100 format(5x,'ft2(): On sort de la table d interpolation avec t=',e15.7)
    write(lll,100) t
    call log_and_halt(lll)
  end


  !---------------------------------------------------------------------------------------
  ! Nouvelle subroutine naitk3, remplace aitk3 et aitk30.
  ! ISSUE ?doc?

  subroutine naitk3(xdi, xdip1, xdip2, xdip3, ydi, ydip1, ydip2, ydip3, xx, fx)
    real*8, intent(in) :: &
     xdi,   & ! ?doc?
     xdip1, & ! ?doc?
     xdip2, & ! ?doc?
     xdip3, & ! ?doc?
     ydi,   & ! ?doc?
     ydip1, & ! ?doc?
     ydip2, & ! ?doc?
     ydip3, & ! ?doc?
     xx       ! ?doc?
    real*8, intent(out) :: &
     fx       ! ?doc?
    real*8 f01, f12, f13, f012, f123, f0123, fu, fv, u, v

    u   = xdi
    v   = xdip1
    fu  = ydi
    fv  = ydip1
    f01 =(fu*(v-xx)-fv*(u-xx))/(v-u)

    u   = xdip2
    fu  = ydip2
    f12 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    u   = xdip3
    fu  = ydip3
    f13 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    u    = xdi
    fu   = f01
    v    = xdip2
    fv   = f12
    f012 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    u    = xdip2
    fu   = f12
    v    = xdip3
    fv   = f13
    f123 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    u     = xdi
    v     = xdip3
    fu    = f012
    fv    = f123
    f0123 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    fx = f0123
  end
end module misc_math


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Functions flin1() and flinh()
!
! Calcul du flux ou de l'intensite par la methode d'integration
! a 6 pts (ou 13pts) de R.Cayrel (these).
! nouvelle methode de calcul de to . to(1) est calcule et
! est different de 0 (On pose to(0)=0)   -Avril 1988-
!
! *Note* in flin_(), MT+JT ipoint was being set to 7 regardless of ptdisk. This has been
!        changed and now works as described: kik .true.: ptdisk .true. -> ipoint = 7; else 6
!
! *Author* Roger Cayrel

module flin
  use misc_math
  use misc
  use dimensions
  implicit none
  private

  public flinh, flin1

  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  real*8 :: TD2, TD, TP, CD, CP, C1, C2, C3
  dimension TD2(26),TD(6),TP(7),CD(6),CP(7), C1(13),C2(12),C3(12)

  data TD /0.038,0.154,0.335,0.793,1.467,3.890 /
  data CD /0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
  data TP /0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
  data CP /0.176273,0.153405,0.167016,0.135428,0.210244,0.107848, 0.049787/
  data TD2 /0.,.05,.1,.15,.3,.45,.60,.80,1.,1.2,1.4,1.6,1.8,2., &
   2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
  data C1 /.032517,.047456,.046138,.036113,.019493,.011037,.006425, &
   .003820,.002303,.001404,.000864,.001045,.002769/
  data C2 /.111077,.154237,.143783,.108330,.059794,.034293, &
   .020169,.012060,.007308,.004473,.002761,.002757/
  data C3 /.023823,.030806,.027061,.019274,.010946,.006390, &
   .003796,.002292,.001398,.000860,.000533,.000396/

  logical, parameter :: MODE_FLINH = .true., &
                        MODE_FLIN1 = .false.

  private flin_
contains
  !---------------------------------------------------------------------------------------------------

  ! See routine flin_() for description of parameters

  real*8 function flin1(kap, b, nh, ntot, ptdisk, mu, kik)
    real*8, intent(in), dimension(MAX_MODELES_NTOT) :: kap, nh
    real*8, dimension(0:MAX_MODELES_NTOT), intent(in) :: b
    logical, intent(in) :: ptdisk
    real*8, intent(in) :: mu
    integer, intent(in) :: ntot, kik
    ! This variable is needed just to fill in the allocation requisites for FLIN_() in FLIN1 mode
    real*8, dimension(MAX_MODELES_NTOT) :: dummy_tauhd
    flin1 = flin_(kap, b, nh, ntot, ptdisk, mu, kik, dummy_tauhd, MODE_FLIN1)
  end


  !---------------------------------------------------------------------------------------------------

  ! See routine flin_() for description of parameters
  !
  ! Differences from FLIN1(): adds tauhd vector to to_

  real*8 function flinh(kap, b, nh, ntot, ptdisk, mu, kik, tauhd)
    real*8, intent(in), dimension(MAX_MODELES_NTOT) :: kap, nh, tauhd
    real*8, dimension(0:MAX_MODELES_NTOT), intent(in) :: b
    logical, intent(in) :: ptdisk
    real*8, intent(in) :: mu
    integer, intent(in) :: ntot, kik

    flinh = flin_(kap, b, nh, ntot, ptdisk, mu, kik, tauhd, MODE_FLINH)
  end


  ! Generic routine, called by flin1() and flinh()

  real*8 function flin_(kap, b, nh, ntot, ptdisk, mu, kik, tauhd, mode_)
    implicit none
    ! ?doc?
    real*8, intent(in) :: kap(MAX_MODELES_NTOT)
    ! ?doc?
    real*8, intent(in) :: b(0:MAX_MODELES_NTOT)
    ! Source is probably modele%nh
    real*8, intent(in) :: nh(MAX_MODELES_NTOT)
    ! Source is probably modele%ntot
    integer, intent(in) :: ntot
    ! Source is probably main_ptdisk
    !   - if .TRUE. : 7 points
    !   - .FALSE.   : 6 points
    logical, intent(in) :: ptdisk
    ! cos(angle). Source is probably file_main::main_mu
    real*8, intent(in) :: mu
    ! (old "IOP") accepts 0 or 1.
    !   - if 0, uses the 6/7 point formulation
    !   - if 1, uses the 26-point formulation
    integer, intent(in) :: kik
    ! Used only in FLINH mode
    real*8, intent(in) :: tauhd(MAX_MODELES_NTOT)
    ! Internal, either @ref MODE_FLIN1 or @ref MODE_FLINH
    logical, intent(in) :: mode_
    ! Optical depth
    real*8 :: to_(0:MAX_MODELES_NTOT)

    real*8, dimension(13) :: fp, cc, bb, tt
    real*8, dimension(26) :: bbb

    real*8 tolim
    integer ipoint, l, m, n

    ! Calcul de to_
    to_(0) = 0.
    to_(1) = nh(1)*(kap(1)-(kap(2)-kap(1))/(nh(2)-nh(1))*nh(1)/2.)
    call integra(nh, kap, to_, ntot, to_(1))
    if (mode_ .eqv. MODE_FLINH) then  ! flinh() mode only!!
      do n = 1,ntot
        to_(n) = to_(n)+tauhd(n)
      end do
    end if

    ! Calcul du flux
    if (kik .eq. 0) then
      ! Formule a 6 ou 7 pts
      if(ptdisk) then
        ipoint = 7
        tolim = 4.0
      else
        ipoint = 6
        tolim = 3.89
      end if

      call check_modele_trop_court(1)

      continue
      do l = 1, ipoint
        if (ptdisk) then
          tt(l) = TP(l)*mu
          cc(l) = CP(l)
        else
          tt(l) = TD(l)
          cc(l) = CD(l)
        end if
      end do

      flin_ = 0.
      do  l = 1, ipoint
        bb(l) = faitk30(tt(l), to_, b, ntot)
        fp(l) = cc(l)*bb(l)
        flin_ = flin_+fp(l)
      end do
      return

    elseif (kik .eq. 1) then
      ! Formule a 26 pts (ne marche que pour le flux!)
      ! (13pts +pts milieu)

      if(ptdisk) then
        call log_and_halt('Le sp flin_ ne peut calculer l''intensite en 1 pt '// &
         'du disque avec la formule a 26pts (utiliser 6pts/7pts: kik=0)', is_assertion=.true.)
      end if
      tolim = 5.487  ! Le modele doit aller au moins a une prof tolim

      call check_modele_trop_court(2)

      do l = 1,26
        bbb(l) = faitk30(TD2(l), to_, b, ntot)
      end do

      do m = 1, 12
        l = 2*m - 1
        bb(m) = bbb(l+1)
        fp(m) = C1(m)*bbb(l) + C2(m)*bbb(l+1) + C3(m)*bbb(l+2)
        cc(m) = C2(m)
      end do

      fp(13) = C1(13)*bbb(26)
      bb(13) = bbb(26)
      cc(13) = C1(13)
      ! Ces bb et cc ne servent que pour les sorties (pas au calcul)

      flin_ = 0.
      do l = 1,13
        flin_ = flin_+fp(l)
      end do
      return
    else
      call log_and_halt('Bad kik (must be 0 or 1)')
    end if  !(fin du if kik)
  contains
    ! Error verification, called twice

    subroutine check_modele_trop_court(i_call)
      ! Indicates where it was called from, used in error message: facilitates debugging
      integer, intent(in) :: i_call
      if(to_(ntot) .lt. tolim) then
        call log_and_halt('Modele too short (call #'//int2str(i_call)//'): ntot=' //&
         int2str(ntot) //'; to_(' //&
         int2str(ntot) // ') = ' // real82str(to_(ntot), 7) // ' (must be >= '//&
          real82str(tolim, 3) // ')')
      end if
    end
  end
end module flin


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

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Command-line parser.
!
! Originally from the Fortran wiki (http://fortranwiki.org/fortran/show/Command-line+arguments)

module options2
  use logging
  implicit none

  integer, parameter :: MAX_LEN_DESCR = 500 ! Maximum length of option description

  type option
    ! Initials of executable(s) where option is applicable.
    ! [i]nnewmarcs, [h]ydro2, [p]fant, [n]ulbad, [c]onvmol
    character(len=5) :: ihpnc
    ! Long name.
    character(len=100) :: name
    ! Corresponding short name.
    character :: chr
    ! Does the option require an argument?
    logical :: has_arg
    ! Argument name, if required.
    character(len=20) :: argname
    ! Default value. For the purpose of displaying the help text only (not used to set
    ! the default values of config_* variables; in fact, string versions of the default values
    ! of these variables are used to fill this field)
    character(len=100) :: default_
    ! Description.
    ! *Note* The newline marker "<br>" is recognized.
    character(len=MAX_LEN_DESCR) :: descr
    ! Whether the option appears or not in --help text
    ! Some options may be secret
    logical :: appears
  end type


  ! Configurable unit to output command-line parsing errors
  integer :: error_unit = 6

  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  integer, private, parameter :: MAX_TEMP_PTR=300  ! In practice: maximum size of options_ string

contains

  ! Parse command line options. Options and their arguments must come before
  ! all non-option arguments. Short options have the form "-X", long options
  ! have the form "--XXXX..." where "X" is any character.

  subroutine getopt(options, optindex, arg, arglen, stat, &
      offset, remain)
    use iso_fortran_env, only: error_unit

    ! Array, items are of option type. Each option may have either a long name
    ! (accessible in  the form '--XXXX...'), a single-char short name ("-X"),
    ! or both. Be careful not to repeat yourself, uniqueness of name is not
    ! checked.
    type(option), intent(in) :: options(:)

    ! If stat is 0, contains the id (index) of the option that was parsed.
    integer, intent(out) :: optindex

    ! If the parsed option requires an argument, arg contains
    ! the first len(arg) (but at most MAX_LEN_DESCR) characters of that argument.
    ! Otherwise its value is undefined. If the arguments length exceeds MAX_LEN_DESCR
    ! characters and err is .true., a warning is issued.
    character(len=*), intent(out) :: arg

    ! If the parsed option requires an argument, arglen contains
    ! the actual length of that argument. Otherwise its value is undefined.
    ! This can be used to make sure the argument was not truncated by the
    ! limited length of arg.
    integer, intent(out) :: arglen

    ! Status indicator. Can have the following values:
    !   -  0: An option was successfully parsed.
    !   -  1: Parsing stopped because a '--' was encountered
    !         (not an error, but nothing was parsed).
    !   -  2: Parsing stopped because a non-option was encountered
    !         (not an error, but nothing was parsed).
    !   -  3: Parsing stopped because there are no arguments left
    !         (not an error, but nothing was parsed).
    !   .
    ! Its value is never undefined.
    integer, intent(out) :: stat

    ! If stat is 1, offset contains the number of the argument before the
    ! first non-option argument, i.e. offset+n is the nth non-option argument.
    ! If stat is not 1, offset contains the number of the argument that would
    ! be parsed in the next call to getopt. This number can be greater than
    ! the actual number of arguments.
    integer, intent(out), optional :: offset

    ! If stat is 1, remain contains the number of remaining non-option
    ! arguments, i.e. the non-option arguments are in the range
    ! (offset+1:offset+remain). If stat is not 1, remain is undefined.
    integer, intent(out), optional :: remain

    integer, save :: pos = 1, cnt = 0
    character(len=100), save :: arg0  ! used for option name
    character(len=512), save :: arg1  ! used for option argument

    integer :: length, st, id
    character :: ch
    logical :: flagLong
    character*128 lll


    if (cnt == 0) cnt = command_argument_count()

    if (pos > cnt) then
      ! no more arguments left
      st = 3
      goto 10
    end if

    call get_command_argument(pos, arg0, length)
    flagLong = .false.


    !-- This part tries to find the "option id"

    if (arg0(1:1) == '-') then
      ! Argument is an option

      ch = arg0(2:2)


      if (ch /= '-' .and. len_trim(arg0) > 2) then
        ! single dash, but name too long ('-xxxx...')
        write(lll, *) 'Invalid name for short option: "', trim(arg0(2:)), '"'
        call log_and_halt(lll)
      end if

      if (ch == '-' .and. arg0(3:3) == ' ') then
        ! forced stop ('--')
        st = 1
        goto 10
      end if


      if (ch == '-') then
        ! long option ('--xxx...')
        flagLong = .true.
        id = lookup_long(arg0(3:))
      else
        ! short option
        id = lookup_short(ch)
      end if

      if (id == 0) then
        write(lll, *) 'Invalid option: "', trim(arg0), '"'
        call log_and_halt(lll)
      end if

      if (.not. options(id)%has_arg) then
        st = 0
        goto 10
      end if

      if (pos == cnt) then
        write(lll, *) 'Option "', trim(arg0), '" requires an argument, but none found'
        call log_and_halt(lll)
      end if

      pos = pos + 1
      call get_command_argument(pos, arg1, length)

      ! make sure argument is not an option
      if (arg1(1:1) == '-') then
        write(lll, *) 'Option "', trim(arg0), '" requires an argument, but option found'
        call log_and_halt(lll)
      end if

      st = 0
      goto 10
    end if

    !-- Not an option, parsing stops

    st = 2
    ! we are already at the first non-option argument
    ! go one step back to the last option or option argument
    pos = pos - 1


    !-- Error handling and setting of return values
    10 continue

    offset = pos
    stat   = st
    if (stat == 0) then
      optindex = id
      remain   = cnt-pos
      if (options(id)%has_arg) then
        arg = arg1
        arglen = length
      end if

      ! setup pos for next call to getopt
      pos = pos + 1
    end if


  contains

    integer function lookup_long(name)
      character(len=*), intent(in) :: name
      integer :: i

      do i = 1, size(options)
        if (name == options(i)%name) then
          lookup_long = i
          return
        else
        end if
      end do
      ! if we get to this point, the option was not found

      lookup_long = 0
    end function

    integer function lookup_short(chr)
      character, intent(in) :: chr
      integer :: i

      do i = 1, size(options)
        if (chr == options(i)%chr) then
          lookup_short = i
          return
        end if
      end do
      ! if we get to this point, the option was not found

      lookup_short = 0
    end function

  end subroutine



  !=======================================================================================

  ! Print an option in the style of a man page. I.e.
  ! <pre>
  ! -o <arg>
  ! --option <arg>
  !    [=default_value]
  !    description.................................................................
  !    ............................................................................
  ! </pre>
  subroutine print_opt(opt, unit)
    ! the option
    type(option), intent(in) :: opt
    ! logical unit number
    integer, intent(in) :: unit

    integer :: l0, l, c1, c2, idx, new_start
    character(:), allocatable :: descr_till_end, s_to_wrap, temp

    if (opt%has_arg) then
      if (opt%chr .ne. ' ') then  ! option may or may not have short version
        write (unit, '(1x,"-",a,1x,1h<,a,4h> or)') opt%chr, trim(opt%argname)
      end if
      ! long version is mandatory
      write (unit, '(1x,"--",a,2h <,a,1h>)') trim(opt%name), trim(opt%argname)
    else
      if (opt%chr .ne. ' ') then
        write (unit, '(1x,"-",a, 3h or)') opt%chr
      end if
      write (unit, '(1x,"--",a)') trim(opt%name)
    end if

    if (len_trim(opt%default_) .gt. 0) then
        write(unit, ' (4x,2h[=,a,1h])') trim(opt%default_)
    end if


    ! Splits description using "<br>" as separator, and wraps each piece to print
    ! at 80 columns maximum
    descr_till_end = trim(opt%descr)
    do
      l0 = len(descr_till_end)
      if (l0 .eq. 0) exit

      idx = index(descr_till_end, '<br>')

      if (idx .eq. 0) then
        s_to_wrap = descr_till_end
      else
        s_to_wrap = descr_till_end(:idx-1)
      end if


      ! Wraps string s_to_wrap printing each line
      ! with 4 characters of indentation at the beginning

      l = len(s_to_wrap)
      c1 = 1  ! position of first character of the line to print
      do
        if (c1 > l) exit

        ! prints at maximum 4+76 = 80 characters
        c2 = min(c1 + 76, l) ! position of the last character of the line to print
        ! if not at the end of the whole s_to_wrap
        if (c2 /= l) then
          ! find the end of a word
          do
            if (descr_till_end(c2:c2) == ' ') exit
            c2 = c2-1
          end do
        end if
        write (unit, '(4x,a)') descr_till_end(c1:c2)
        c1 = c2+2
      end do

      ! Discards part of descr_till_end
      if (idx .eq. 0) then
        new_start = l+1
      else
        new_start = l+5
      end if
      if (new_start .gt. l0) exit

      temp = descr_till_end(new_start:)
      descr_till_end = temp
    end do

!    do
!
!      if (c1 > l) exit
!
!      idx = index()
!
!
!      ! print at maximum 4+76 = 80 characters
!      c2 = min(c1 + 76, MAX_LEN_DESCR)
!      ! if not at the end of the whole string
!      if (c2 /= MAX_LEN_DESCR) then
!        ! find the end of a word
!        do
!          if (opt%descr(c2:c2) == ' ') exit
!          c2 = c2-1
!        end do
!      end if
!      write (unit, '(4x,a)') opt%descr(c1:c2-1)
!      c1 = c2+1
!    end do


  end subroutine

  !=======================================================================================
  ! Returns option name with single quotes.

  function get_option_name(opt) result(res)
    character(len=:), allocatable :: res
    ! Option, will be used only in case of error
    type(option), intent(in) :: opt

    if (len(opt%chr) > 0 .and. opt%chr .ne. ' ') then
      if (len(opt%name) > 0) then
        res = '''-' // opt%chr // '''/''--' // trim(opt%name) // ''''
      else
        res = '''-' // opt%chr // ''''
      end if
    else
      res = '''--' // trim(opt%name) // ''''
    end if
  end
end module







!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

! Configuration module for all executables
!
! Prefixes defined here:
!   - HANDLER_
!   - config_ -- throughout configuration variables
!   - execonf_ -- executable-specific variables

module config
  use logging
  use options2
  use misc
  use welcome
  implicit none

  ! Possible return values of option handler
  integer, parameter ::    &
   HANDLER_OK = 0,         & ! option was handled successfully
   HANDLER_ERROR = 1,      & ! error handling option, e.g. invalid argument
   HANDLER_DONT_CARE = 2     ! handler not responsible for handling that option


  !=====
  ! Executable-specific options that must be set programatically
  !=====
  ! Name of executable. Case doesn't matter. It will be converted to all uppercase or
  ! lowercave, depending on the use.
  character*16 :: execonf_name = '?'


  !=====
  ! Variables configurable by command line
  !=====
  ! *Note* for option description, check subroutine init_options()
  ! *Note* to add a new command-line option, follow the steps in "../README.md"
  !        (those also cover how to update the Python code)

  !---
  ! all executables
  !---
  character*64 :: config_fn_main     = 'main.dat'      ! option: --fn_main
  character*64 :: config_fn_progress = 'progress.txt'  ! option: --fn_progress
  character*64 :: config_fn_logging = '?'         ! option: --logging_fn_dump
  logical :: config_logging_console = .true., &  ! option --logging_console
             config_logging_dump   = .false., & ! option --logging_dump
             config_explain        = .false., & ! option --explain
             config_no_molecules   = .false., & ! option --no_molecules
             config_no_atoms       = .false., & ! option --no_atoms
             config_no_h           = .false.    ! option --no_ah

  !---
  ! innewmarcs, hydro2, pfant
  !---
  character*64 :: config_fn_modeles = 'modeles.mod' ! option: --fn_modeles

  !---
  ! innewmarcs, pfant
  !---
  logical :: config_opa = .false.               ! option --opa
  character*64 :: config_fn_opa = 'modeles.opa' ! option: --fn_opa


  !
  ! hydro2, pfant
  !
  character*64 :: &
   config_fn_hmap    = 'hmap.dat', &  ! option: --fn_hmap
   config_fn_absoru2 = 'absoru2.dat'  ! option: --fn_absoru2
  real*8 :: config_llzero = -1 ! option: --llzero
  real*8 :: config_llfin  = -1 ! option: --llfin
  real*8 :: config_pas    = -1 ! option: --pas
  real*8 :: config_aint   = -1 ! option: --aint
                               !
                               ! It was found that subroutine selekfh() time varies is
                               ! approximately proportional to aint**2. Therefore, this
                               ! parameter is set to a low value, which makes it run much
                               ! faster than the usual/historical 50. Also, this value is
                               ! no longer read from *main file*.
                               ! Below is a table containing the results of a test
                               ! perfomed in the 5000-5100 angstrom region:
                               ! <pre>
                               ! aint    synthesis time
                               ! ----------------------
                               ! 100              16.84
                               !  50               9.52
                               !  25               5.87
                               !  13               4.11
                               !  10               3.74
                               !   5               3.15
                               !   1               3.48
                               ! </pre>

  !---
  ! innewmarcs-only
  !---
  character*64 :: &
   config_fn_modgrid = 'grid.mod', &        ! option: --fn_modgrid
   config_fn_moo = 'grid.moo'               ! option: --fn_moo
  logical :: config_allow = .false.         ! option: --allow

  !---
  ! hydro2-only
  !---
  ! Option: --zph
  ! *Note* (historical note) This value was being read from an altered-format
  ! *absoru2 file* which was incompatible with the pfant executable. Therefore,
  ! it has been assigned a default value and this command-line option was added
  real*8 :: config_zph = 12
  ! option: --kik; affects subroutine flin_()
  integer :: config_kik = 0
  ! option: --amores
  ! *Note* Default value taken from M.Trevisan's pfant12.R script
  logical :: config_amores = .true.
  ! option: --kq
  ! *Note* Default taken from M.Trevisan's pfant12.R script
  integer :: config_kq = 1

  !---
  ! pfant-only
  !---
  character*64 :: &
   config_fn_partit        = 'partit.dat',        & ! option: --fn_partit
   config_fn_abonds        = 'abonds.dat',        & ! option: --fn_abonds
   config_fn_atoms         = 'atoms.dat',         & ! option: --fn_atoms
   config_fn_molecules_out = '?',                 & ! option: --fn_molecules_out
   config_fn_lines         = 'lines.pfant',       & ! option: --fn_lines
   config_fn_log           = 'log.log',           & ! option: --fn_log
   config_flprefix         = '?'                    ! option: --flprefix
  integer :: config_interp = 1                      ! option: --interp
  logical :: config_abs = .true., &                 ! option: --abs
             config_sca = .true., &                 ! option: --sca
             config_absoru = .true.                 ! option: --config_absoru

  !---
  ! pfant and convmol
  !---
  character*64 :: &
   config_fn_dissoc        = 'dissoc.dat',        & ! option: --fn_dissoc
   config_fn_molecules     = 'molecules.dat'        ! option: --fn_molecules

  !---
  ! nulbad-only
  !---
  logical :: &
   config_flam = .false., &                      ! option: --flam
   config_convol = .true.                        ! option: --convol
  ! These variables are "uninitialized". If left so, nulbad_calc::nulbad_init() will
  ! take values within *main file*
  real*8 :: &
   config_fwhm = -1, &               ! option: --fwhm
   config_pat = -1                   ! option: --pat
  character*64 :: &
    config_fn_flux = '?', &          ! option: --fn_flux
    config_fn_cv = '?'               ! option: --fn_cv

  real*8 :: config_zinf = -1         ! option: --zinf


  !---
  ! convmol
  !---
  character*69 :: &
   config_fn_out = '?'  ! option: --fn_out

  !===== end of command-line variables declarations



  ! Unit to write to "explain" file (file containing debugging information)
  integer, parameter :: UNIT_EXPLAIN = 145


  !=========^ PUBLIC  ^==========
  !=========v PRIVATE v==========

  !=====
  ! Command-line options definition
  !=====
  ! List of command-line options
  type(option), private :: options(MAX_NUM_OPTIONS)
  ! Maximum valid index of the options variable
  integer, private :: num_options = 0

  !=====
  ! Other stuff
  !=====

  ! indentation string to be used in help text at will
  character(3), private, parameter :: IND = '.. '
  ! Message to be used in help text at will.
  character(:), private, parameter :: FROM_MAIN = ' (read from main configuration file)'

  private :: validate_options, &
   init_options, parse_args, show_help, exe_wants, handle_option

contains

  !=======================================================================================
  ! Initialization of this module
  !
  ! *Note* Must be called *after* module-specific initialization

  subroutine config_init()
    if (execonf_name .eq. '?') &
     call log_and_halt('Executable name not set', is_assertion=.true.)

    write(*,*) to_upper(execonf_name)//pfant_version()
    write(*,*) ''

    call init_options()
    call validate_options()
    call parse_args()

    ! logging module...
    logging_fn_progress = config_fn_progress
    if (config_fn_logging .eq. '?') then
      config_fn_logging = to_lower(execonf_name)//'_dump.log'
    end if
    logging_fn_dump = config_fn_logging

    call print_welcome(6)
  end


  !=======================================================================================
  ! Creates option and adds to options variable
  !
  ! *Note* character arguments are declared with len=*, truncation may occur when
  ! option structure is created.

  subroutine add_option(ihpnc, name, chr, has_arg, argname, default_, descr, appears)
    character(len=*), intent(in) :: ihpnc  ! initials of executables for which option is valid
    character(len=*), intent(in) :: name, argname, default_, descr
    character(len=1), intent(in) :: chr
    logical, intent(in) :: has_arg
    logical, intent(in), optional :: appears
    logical :: appears_

    num_options = num_options+1

    if (num_options .gt. MAX_NUM_OPTIONS) then
      call log_and_halt('Reached maximum number of command-line options, increase '//&
       'value of MAX_NUM_OPTIONS', is_assertion=.true.)
    end if

    appears_ = .true.
    if (present(appears)) then
      appears_ = appears
    end if

    options(num_options) = option(ihpnc, name, chr, has_arg, argname, default_, descr, &
      appears_)
  end

  !=======================================================================================
  ! Initialization of command-line options
  !
  ! - initializes all options for all executables
  ! - assertions to try to catch options initialization errors (e.g. repeated option(s))
  !
  ! *Note* The option description allows for some formatting (check existing code):
  !        - <br> -- breaks line
  !        - IND parameter -- inserts indentation for the 2nd, 3rd etc. lines of a paragraph

  subroutine init_options()
    ! Note that the order of calling add_option() doesn't matter for the program,
    ! but will affect the option printing order when you invoke with "--help"

    !
    ! All executables
    !
    call add_option('ihpnc', 'help', 'h', .false., '', '', &
      'Displays this help text.')
    call add_option('ihpnc', 'logging_level', 'l', .true., 'level', 'debug', &
     'logging level<br>'//&
     IND//'debug<br>'//&
     IND//'info<br>'//&
     IND//'warning<br>'//&
     IND//'error<br>'//&
     IND//'critical<br>'//&
     IND//'halt')
    call add_option('ihpnc', 'logging_console',   ' ', .true., 'T/F', logical2str(config_logging_console), &
     'Print log messages to standard output (usually monitor screen)?')
    call add_option('ihpnc', 'logging_dump',     ' ', .true., 'T/F', logical2str(config_logging_dump), &
      'Print log messages to dump log file?')
    call add_option('ihpnc', 'logging_fn_dump',   ' ', .true., 'file name', '<executable name>_dump.log', &
     'output file name - dump log file')
    call add_option('ihpnc', 'fn_main',          ' ', .true., 'file name', config_fn_main, &
     'input file name - main configuration')
    call add_option('ihpnc', 'explain',     ' ', .true., 'T/F', logical2str(config_explain), &
      'Save additional information in file explain.txt (debugging purposes; output varies, or flag may be ignored)', .false.)
    call add_option('ihpnc', 'play',     ' ', .false., '', '', &
      'Fed up of calculating spectra', .false.)

    !
    ! innewmarcs, hydro2, pfant, convmol
    !
    call add_option('ihpc', 'fn_modeles',' ', .true., 'file name', config_fn_modeles, &
     'Binary file containing information about atmospheric model (created by innewmarcs)')

    !
    ! hydro2, pfant
    !
    call add_option('hp', 'fn_absoru2',       ' ', .true., 'file name', config_fn_absoru2, &
     'input file name - absoru2')
    call add_option('hp', 'fn_hmap',       ' ', .true., 'file name', config_fn_hmap, &
     'input file name - contains table with<br>'//&
     IND//'(filename, niv inf, niv sup, central lambda, kiex, c1)')
    call add_option('hp', 'interp', ' ', .true., 'type', int2str(config_interp), &
     'interpolation type for subroutine turbul()<br>'//&
     IND//'1: linear;<br>'//&
     IND//'2: parabolic)')
    call add_option('hp', 'kik', ' ', .TRUE., '0/1', int2str(config_kik), &
     'option for flux integration<br>'//&
     IND//'0: integration using 6/7 points depending on option --ptdisk;<br>'//&
     IND//'1: 26-point integration')

    !
    ! hydro2, pfant, convmol
    !
    call add_option('hpc', 'llzero',' ', .true., 'real value', '<main_llzero> '//FROM_MAIN, &
     'Lower boundary of calculation interval (angstrom)')
    call add_option('hpc', 'llfin',' ', .true., 'real value', '<main_llfin> '//FROM_MAIN, &
     'Upper boundary of calculation interval (angstrom)')

    !
    ! innewmarcs-only
    !
    call add_option('i', 'fn_modgrid', ' ', .true., 'file name', config_fn_modgrid, &
     'input file name - atmospheric model grid')
    call add_option('i', 'fn_moo', ' ', .true., 'file name', config_fn_moo, &
     'input file name - complete atmospheric model grid with opacities included')
    call add_option('i', 'allow', ' ', .true., 'T/F', logical2str(config_allow), &
     'allow (teff, glog, asalog) point out of model grid?')

    !
    ! innewmarcs, pfant
    !
    call add_option('ip', 'fn_opa', ' ', .true., 'file name', config_fn_opa, &
     'MARCS ".opa" opacities file (most probably created by innewmarcs by interpolation,<br>'//&
     ' or downloaded from MARCS website)')

    !
    ! hydro2-only
    !
    call add_option('h', 'amores',' ', .true., 'T/F', logical2str(config_amores), &
     'AMOrtissement de RESonnance?')
    call add_option('h', 'kq', ' ', .true., '0/1', int2str(config_kq), &
     '"Theorie"<br>'//&
     IND//'0: THEORIE DE GRIEM;<br>'//&
     IND//'1: THEORIE QUASISTATIQUE')
    call add_option('h', 'zph', ' ', .true., 'real value', real82str(config_zph, 2), &
     'abondance d''H pour laquelle sont donnees les abondances metalliques')

    !
    ! pfant-only
    !
    call add_option('p', 'fn_partit',        ' ', .true., 'file name', config_fn_partit, &
     'input file name - partition functions')
    call add_option('p', 'fn_abonds',        ' ', .true., 'file name', config_fn_abonds, &
     'input file name - atomic abundances')
    call add_option('p', 'fn_atoms',     ' ', .true., 'file name', config_fn_atoms, &
     'input file name - atomic lines')
    call add_option('p', 'no_molecules',' ', .true., 'T/F', logical2str(config_no_molecules), &
     'If set, skips the calculation of molecular lines')
    call add_option('p', 'no_atoms',' ', .true., 'T/F', logical2str(config_no_atoms), &
     'If set, skips the calculation of atomic lines')
    call add_option('p', 'no_h',' ', .true., 'T/F', logical2str(config_no_h), &
     'If set, skips the calculation of hydrogen lines')
    call add_option('p', 'zinf', ' ', .true., 'real value', '(zinf per-line in *atoms file*)', &
     'distance from center of line to consider in atomic line calculation.<br>'//&
     IND//'If this option is used, will bypass the zinf defined for each atomic line<br>'//&
     IND//'of dfine:atoms and use the value passed', .false.)  ! option will not appear in --help printout
    call add_option('p', 'pas', ' ', .true., 'real value', '<main_pas> '//FROM_MAIN, &
     'Calculation delta-lambda (angstrom)')
    call add_option('p', 'aint', ' ', .true., 'integer value', real82str(config_aint, 0), &
     'Interval length per iteration (angstrom)')
    call add_option('p', 'fn_progress',      ' ', .true., 'file name', config_fn_progress, &
     'output file name - progress indicator', .false.)

    ! better to group the opacity-related options
    call add_option('ip', 'opa',' ', .true., 'T/F', logical2str(config_opa), &
     'Whether or not to include MARCS opacity coefficients (absorption and scattering)<br>'//&
     IND//'in the continuum')
    call add_option('p', 'abs',' ', .true., 'T/F', logical2str(config_abs), &
     'Whether or not to include MARCS *absorption* coefficients in the continuum.<br>'//&
     'This option only has effect if --opa is True.')
    call add_option('p', 'sca',' ', .true., 'T/F', logical2str(config_sca), &
     'Whether or not to include MARCS *scattering* coefficients in the continuum.<br>'//&
     'This option only has effect if --opa is True.')
    call add_option('p', 'absoru',' ', .true., 'T/F', logical2str(config_absoru), &
     'Whether or not to include coefficients calculated by subroutine absoru()<br>'//&
     IND//'in the continuum.')

    !
    ! pfant, convmol
    !
    call add_option('pc', 'fn_dissoc',        ' ', .true., 'file name', config_fn_dissoc, &
     'input file name - dissociative equilibrium')
    call add_option('pc', 'fn_molecules', ' ', .true., 'file name', config_fn_molecules, &
     'input file name - molecular lines')

    !
    ! pfant, nulbad
    !
    call add_option('pn', 'flprefix',        ' ', .true., 'filename prefix', &
                    '<main_flprefix> '//FROM_MAIN, &
     'pfant output - prefix for flux output files.<br>'//&
     'Three files will be created based on this prefix:<br>'//&
     IND//'<flprefix>.spec: un-normalized spectrum<br>'//&
     IND//'<flprefix>.cont: continuum<br>'//&
     IND//'<flprefix>.norm: normalized spectrum')

    !
    ! nulbad-only
    !
    call add_option('n', 'fn_flux', ' ', .true., 'file name', &
     '<main_flprefix>.norm '//FROM_MAIN, &
     'flux file name')
    call add_option('n', 'flam',     ' ', .true., 'T/F', logical2str(config_flam), &
      'Fnu to FLambda transformation?')
    call add_option('n', 'fn_cv',     ' ', .true., 'file name', '<flux file name>.nulbad.<fwhm>', &
      'output file name, which will have the convolved spectrum')
    call add_option('n', 'pat',      ' ', .true., 'real value', '<main_pas> '//FROM_MAIN, &
      'wavelength delta-lambda of the output spectrum (angstrom)')
    call add_option('n', 'convol',   ' ', .true., 'T/F', logical2str(config_convol), &
      'Apply convolution?')
    call add_option('n', 'fwhm',     ' ', .true., 'real value', '<main_fwhm> '//FROM_MAIN, &
      'full-width-half-maximum of Gaussian function')

    !
    ! convmol
    !
    call add_option('c', 'fn_out', ' ', .true., 'file name', &
      '<molecular lines file name>.plez', &
      'output file name - molecular lines in VALD3 extended format.')

  end



  !=======================================================================================
  ! Handles single option
  !
  ! *Note* Must be called by executable-specific option handler when the latter does not
  !       recognize the option.
  !
  ! *Note* If finds "-h" or "--help", will display help text and halt.

  function handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res, itemp

    res = HANDLER_OK

    select case(opt%name)
      case ('help')
          call show_help(6)
          stop('Bye')
      case ('play')
          call play()
          stop('Bye')
      case ('logging_level')
        ! Note that logging level change takes effect immediately
        select case (to_lower(o_arg))
          case ('debug')
            logging_level = LOGGING_DEBUG
          case ('info')
            logging_level = LOGGING_INFO
          case ('warning')
            logging_level = LOGGING_WARNING
          case ('critical')
            logging_level = LOGGING_CRITICAL
          case ('error')
            logging_level = LOGGING_ERROR
          case ('halt')
            logging_level = LOGGING_HALT
          case default
            res = HANDLER_ERROR
        end select
        if (res .eq. HANDLER_OK) then
          call parse_aux_log_assignment('logging level', to_lower(o_arg))
        end if
      case ('logging_console')
        config_logging_console = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_logging_console', logical2str(config_logging_console))
      case ('logging_dump')
        config_logging_dump = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_logging_dump', logical2str(config_logging_dump))
      case ('explain')
        config_explain = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_explain', logical2str(config_explain))
      case ('fn_main')
        call parse_aux_assign_fn(o_arg, config_fn_main, 'config_fn_main')
      case ('logging_fn_dump')
        call parse_aux_assign_fn(o_arg, config_fn_logging, 'config_fn_logging')
      case ('fn_progress')
        call parse_aux_assign_fn(o_arg, config_fn_progress, 'config_fn_progress')
      case ('fn_modeles')
        call parse_aux_assign_fn(o_arg, config_fn_modeles, 'fn_modeles')
      case ('kik')
        config_kik = parse_aux_str2int(opt, o_arg)
        if (config_kik .eq. 0 .or. config_kik .eq. 1) then !#validation
          call parse_aux_log_assignment('config_kik', int2str(config_kik))
        else
          res = HANDLER_ERROR
        end if
      case ('kq')
        config_kq = parse_aux_str2int(opt, o_arg)
        if (config_kq .eq. 0 .or. config_kq .eq. 1) then !#validation
          call parse_aux_log_assignment('config_kq', int2str(config_kq))
        else
          res = HANDLER_ERROR
        end if
      case ('amores')
        config_amores = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_amores', logical2str(config_amores))
      case ('zph')
        config_zph = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_zph', real82str(config_zph, 2))
      case ('fn_absoru2')
        call parse_aux_assign_fn(o_arg, config_fn_absoru2, 'config_fn_absoru2')
      case ('fn_hmap')
        call parse_aux_assign_fn(o_arg, config_fn_hmap, 'config_fn_hmap')
      case ('llzero')
        ! Note (JT): decimal llzero/llfin are not allowed due to [not completely understood]
        ! numerical complications.
        config_llzero = floor(parse_aux_str2real8(opt, o_arg))
        call parse_aux_log_assignment('config_llzero', real82str(config_llzero, 1))
      case ('llfin')
        config_llfin = ceiling(parse_aux_str2real8(opt, o_arg))
        call parse_aux_log_assignment('config_llfin', real82str(config_llfin, 1))
      case ('pas')
        config_pas = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_pas', real82str(config_pas, 4))
      case ('aint')
        config_aint = floor(parse_aux_str2real8(opt, o_arg))
        call parse_aux_log_assignment('config_aint', real82str(config_aint, 1))
      case ('interp')
        iTemp = parse_aux_str2int(opt, o_arg)
        select case (iTemp)
          case (1, 2)
            config_interp = iTemp
            call parse_aux_log_assignment('config_interp', int2str(config_interp))
          case default
            res = HANDLER_ERROR
        end select
      case ('fn_dissoc')
        call parse_aux_assign_fn(o_arg, config_fn_dissoc, 'config_fn_dissoc')
      case ('fn_partit')
        call parse_aux_assign_fn(o_arg, config_fn_partit, 'config_fn_partit')
      case ('fn_abonds')
        call parse_aux_assign_fn(o_arg, config_fn_abonds, 'config_fn_abonds')
      case ('fn_atoms')
        call parse_aux_assign_fn(o_arg, config_fn_atoms, 'config_fn_atoms')
      case ('fn_molecules')
        call parse_aux_assign_fn(o_arg, config_fn_molecules, 'config_fn_molecules')
      case ('fn_out')
        call parse_aux_assign_fn(o_arg, config_fn_out, 'config_fn_out')
      case ('fn_opa')
        call parse_aux_assign_fn(o_arg, config_fn_opa, 'config_fn_opa')
!            case ('fn_lines')
!              call parse_aux_assign_fn(o_arg, config_fn_lines)
!            case ('fn_log')
!              call parse_aux_assign_fn(o_arg, config_fn_log)
      case ('flprefix')
        call parse_aux_assign_fn(o_arg, config_flprefix, 'config_flprefix')
      case ('no_molecules')
        config_no_molecules = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_no_molecules', logical2str(config_no_molecules))
      case ('opa')
        config_opa = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_opa', logical2str(config_opa))
      case ('abs')
        config_abs = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_abs', logical2str(config_abs))
      case ('sca')
        config_sca = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_sca', logical2str(config_sca))
      case ('absoru')
        config_absoru = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_absoru', logical2str(config_absoru))
      case ('no_atoms')
        config_no_atoms = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_no_atoms', logical2str(config_no_atoms))
      case ('no_h')
        config_no_h = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_no_h', logical2str(config_no_h))
      case ('zinf')
        config_zinf = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_zinf', real82str(config_zinf, 1))
      case ('fwhm')
        config_fwhm = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_fwhm', real82str(config_fwhm, 3))
      case ('convol')
        config_convol = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_convol', logical2str(config_convol))
      case ('pat')
        config_pat = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_pat', real82str(config_pat, 3))
      case ('fn_cv')
        call parse_aux_assign_fn(o_arg, config_fn_cv, 'config_fn_cv')
      case ('flam')
        config_flam = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_flam', logical2str(config_flam))
      case ('fn_flux')
        call parse_aux_assign_fn(o_arg, config_fn_flux, 'config_fn_flux')
      case ('fn_modgrid')
        call parse_aux_assign_fn(o_arg, config_fn_modgrid, 'config_fn_modgrid')
      case ('fn_moo')
        call parse_aux_assign_fn(o_arg, config_fn_moo, 'config_fn_moo')
      case ('allow')
        config_allow = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_allow', logical2str(config_allow))

      case default
        res = HANDLER_DONT_CARE
    end select
  end

  ! 88888 .d88b. .d88b. 8    .d88b.
  !   8   8P  Y8 8P  Y8 8    YPwww.
  !   8   8b  d8 8b  d8 8        d8
  !   8   `Y88P' `Y88P' 8888 `Y88P'  Tools

  !=======================================================================================
  ! Prints variable and assigned value

  subroutine parse_aux_log_assignment(varname, value)
    character(*), intent(in) :: varname, value
    call log_info('set '//varname//' = '''//trim(adjustl(value))//'''')
  end

  !=======================================================================================
  ! Assigns option argument to filename variable
  !
  ! TODO Filename validation could be added here.

  subroutine parse_aux_assign_fn(arg, dest, varname)
    character(len=*), intent(in)  :: arg ! command-line option argument
    character(len=*), intent(out) :: dest! One of config_fn_* variables
    ! name of variable being assigned, for logging purpose
    character(len=*), intent(in) :: varname
    dest = arg
    call parse_aux_log_assignment(varname, arg)
  end

  !=======================================================================================
  ! Converts string to integer, halting the program if conversion fails.
  !
  ! This function takes an option as argument in order to form a comprehensible
  ! error message if the conversion to integer fails.

  integer function parse_aux_str2int(opt, s)
    ! Option, will be used only in case of error
    type(option), intent(in) :: opt
    ! String to be converted to integer
    character(len=*), intent(in) :: s

    read(s, *, err=20) parse_aux_str2int
    go to 30

    20 continue
    call log_and_halt('Error parsing option '//get_option_name(opt)//&
     ': invalid integer argument: '''//trim(s)//'''')

    30 continue
  end

  !=======================================================================================
  ! Converts string to real*8, halting the program if conversion fails.
  !
  ! This function takes an option as argument in order to form a comprehensible
  ! error message if the conversion to real fails.

  real*8 function parse_aux_str2real8(opt, s)
    ! Option, will be used only in case of error
    type(option), intent(in) :: opt
    ! String to be converted to integer
    character(len=*), intent(in) :: s

    read(s, *, err=20) parse_aux_str2real8
    go to 30

    20 continue
    call log_and_halt('Error parsing option '//get_option_name(opt)//&
     ': invalid real argument: '''//trim(s)//'''')

    30 continue
  end

  !=======================================================================================
  ! Converts string to real*4, halting the program if conversion fails.
  !
  ! This function takes an option as argument in order to form a comprehensible
  ! error message if the conversion to real fails.

  real*4 function parse_aux_str2real4(opt, s)
    ! Option, will be used only in case of error
    type(option), intent(in) :: opt
    ! String to be converted to integer
    character(len=*), intent(in) :: s

    read(s, *, err=20) parse_aux_str2real4
    go to 30

    20 continue
    call log_and_halt('Error parsing option '//get_option_name(opt)//&
     ': invalid real argument: '''//trim(s)//'''')

    30 continue
  end

  !=======================================================================================
  ! Converts string to logical, halting the program if conversion fails.
  !
  ! This function takes an option as argument in order to form a comprehensible
  ! error message if the conversion to logical  fails.
  !
  ! Please check the source code for recognized representations of a logical value.
  !
  ! *Note* Conversion is case insensitive.

  logical function parse_aux_str2logical(opt, s)
    ! Option, will be used only in case of error
    type(option), intent(in) :: opt
    ! String to be converted to logical
    character(len=*), intent(in) :: s

    select case (to_lower(s))
      case ('.true.', 'true', 't', 'on', '1')
        parse_aux_str2logical = .true.
        return
    end select

    select case (to_lower(s))
      case ('.false.', 'false', 'f', 'off', '0')
        parse_aux_str2logical = .false.
        return
    end select

    call log_and_halt('Error parsing option '//get_option_name(opt)//&
     ': invalid logical argument: '''//trim(s)//'''')
  end


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888


  !=======================================================================================
  ! Parses and validates all command-line arguments.
  !
  ! *Note* If finds "-h" or "--help", will display help text and halt.

  subroutine parse_args()
    integer o_len, o_stat, o_remain, o_offset, o_index
    character*500 o_arg  ! option argument maximum size is 500
    integer res  ! handler result
    type(option) opt

    do while (.TRUE.)
      call getopt(options, o_index, o_arg, o_len, o_stat, o_offset, o_remain)
      select case(o_stat)
        case (1,2,3)  ! parsing stopped (no error)
           exit
        case (0)  ! option successfully parsed
          opt = options(o_index)

          if (exe_wants(opt)) then
            res = handle_option(opt, o_arg)
            select case(res)
              case(HANDLER_DONT_CARE)
                call log_and_halt('Forgot to handle option '//get_option_name(opt), is_assertion=.true.)
              case (HANDLER_ERROR)
                call log_and_halt('Invalid argument for option '//get_option_name(opt)//': "'//o_arg//'"')
            end select
          else
            ! Executable will ignore options that are not meant for it
            call log_debug(execonf_name//' ignoring option '//get_option_name(opt))
          end if
      end select
    end do
  end


  !=======================================================================================
  ! Returns whether or not the option is applicable to the executable running.

  logical function exe_wants(opt) result(res)
    type(option), intent(in) :: opt
    res = .true.
    if (index(opt%ihpnc, execonf_name(1:1)) .eq. 0) then
      res = .false.
    end if
  end

  !=======================================================================================
  ! Writes help to particular unit

  subroutine show_help(unit)
    ! logical unit number, e.g., 6=screen
    integer, intent(in) :: unit

    integer i

    write(*,*) 'Command-line options'
    write(*,*) '----------------------'
    write(*,*) ' Legend:'
    write(*,*) '  [=xxx]     default value'
    write(*,*) '  <arg name> argument that must be specified'

    do i = 1, num_options
      if (exe_wants(options(i)) .and. options(i)%appears) then
        write(unit,*) ''
        call print_opt(options(i), unit)
      end if
    end do
  end

  !=======================================================================================
  ! Performs a series of checks to avoid programming errors while defining the
  ! command-line options
  !
  ! This subroutine must be called at the end of the init_options() of a config module

  subroutine validate_options()
    integer i, j
    character*1 :: chr
    character*100 :: name

    !#assertion: make sure that there are no repeated options. Checks all against all
    do i = 1, num_options
      name = options(i)%name
      ! write(*,*) i, name
      chr = options(i)%chr
      do j = i+1, num_options
        if (name .eq. options(j)%name) then
          call log_and_halt('Repeated long option: "'//trim(name)//'"', is_assertion=.true.)
        end if
        if (chr .ne. ' ' .and. chr .eq. options(j)%chr) then
          call log_and_halt('Repeated short option: "'//chr//'"', is_assertion=.true.)
        end if
      end do
    end do
  end
end













!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reading routines and variable declarations for *main file*

module file_main
  use logging
  use dimensions
  !use file_dissoc
  use config
  implicit none

  ! Flag indicating whether read_main() has already been called.
  logical :: flag_read_main   = .false.

  character*64 main_flprefix  ! prefix for flux files: <main_flprefix>.(spec, cont, norm)
  logical   main_ptdisk    ! Whether or not the [simulated] measurement was taken in a given point of the
                           ! star disk. This should be True for the Sun and False for any other stars.
  real*8 :: &
   main_pas,    & ! calculation step within each calculation sub-interval; this is a "delta lambda"
   main_echx,   & ! not used in calculation, only written to flux file
   main_echy,   & ! not used in calculation, only written to flux file
   main_mu,     & ! cosine of angle between center of star disk and the [simulated] point of measurement.
                  ! (1.0 - center of disk). Only used if main_ptdisk is True (i.e., the Sun).
   main_afstar, & ! log10 of metallicity. Must match main_asalog
   main_llzero, & ! lower boundary of calculation interval
   main_llfin,  & ! upper boundary of calculation interval
   main_aint,   & ! length of each calculation sub-interval, *no longer used*
                           ! (now config_aint (which has a default value) is used instead)
   main_teff,   & ! effective temperature of the star
   main_glog,   & ! log10 of gravity
   main_asalog, & ! log10 of metallicity. Must match main_afstar
   main_nhe       ! only used to check if matches with modele%nhe
  ! "Full-width-half-maximum" of Gaussian function for
  ! convolution of calculated spectrum; used only by nulbad executable
  real*8 :: main_fwhm

  integer   main_ivtot ! Number of velocities of microturbulence informed (1 in great majority of cases)
                       ! = 1 -- "vt" constant
                       ! > 1 -- "vt" variable

  character*15 main_titrav                ! Title, e.g. "Sun"
  real*8 :: main_vvt(MAX_MODELES_NTOT), & ! microturbulence velocities
            main_tolv(MAX_MODELES_NTOT)   ! log(tau(Rosseland)) for each velocity in main_vvt
contains

  !=======================================================================================
  ! Makes sure that read_main() has been called
  !
  ! This routine is used by hydro2, innewmarcs, nulbad. None of these care about dissoc
  ! or filetoh variables.

  subroutine assure_read_main(path_to_file)
    character(len=*), intent(in) :: path_to_file
    if (.not. flag_read_main) then
      call read_main(path_to_file)
    end if
  end

  !=======================================================================================
  ! Reads *main file* to fill variables main_*

  subroutine read_main(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer myunit
    integer i, inum_obsolete
    logical ecrit_obsolete
    real*8 temp

    ! this logging message is important in case of errors to know which file it was
    call log_info('read_main(): reading file '''//trim(path_to_file)//'''...')

    open(newunit=myunit,file=path_to_file, status='old')

    ! row 01: object name, e.g. "sun"
    read(myunit, '(a15)') main_titrav

    ! row 02
    ! ecrit is obsolete
    read(myunit, *) ecrit_obsolete, main_pas, main_echx, main_echy, main_fwhm

    ! row 03
    read(myunit, *) main_vvt(1)
    main_ivtot = 1

    ! rows 03.(1-3): (three conditional rows that MUST exist if and only if main_VVT(1) > 900)
    ! main_vvt(1) may work as a flag. If this value is > 900, then the file needs to have three
    ! extra rows at this point:
    ! - 1) number of atmospheric layers
    ! - 2) vector of optical depths
    ! - 3) vector of velocities of microturbulence
    !
    ! *Note* the number of layers does not need to match that of modeles.mod because an interpolation
    !        will take place in turbul_() to "synchronize" these two vectors
    if(main_vvt(1) .gt. 900)  then   ! vt variable avec la profondeur
      read(myunit, *) main_ivtot
      if (main_ivtot .gt. MAX_MODELES_NTOT) then
        write (lll, *) 'main_ivtot .gt. MAX_MODELES_NTOT (', &
         main_ivtot, ' .gt. ', MAX_MODELES_NTOT, ')'
         call log_and_halt(lll)
      end if

      read(myunit,*) (main_tolv(i), i=1, main_ivtot)
      read(myunit,*) (main_vvt(i) ,i=1, main_ivtot)
    end if

    ! row 04
    read(myunit, *) main_teff, main_glog, main_asalog, main_nhe, inum_obsolete

    ! row 05
    read(myunit, *) main_ptdisk, main_mu

    ! row 06
    read(myunit, *) main_afstar

    ! ISSUE decide whether to use asalog of afstar throughout the source code
    !       For the time, in order to avoid mistakes I am forcing them to match
    if (abs(main_asalog-main_afstar) .gt. 0.001) then
      call log_and_halt('asalog ('//real82str(main_asalog, 2)//&
       ') does not match afstar ('//real82str(main_afstar, 2)//') in file '''//trim(path_to_file)//'''')
    end if


    ! row 07: XXCOR(i)
    ! (JT) Should be a column in dissoc.dat !!! (MT) I agree (JT) The "xxcor" feature is now obsolete
    read(myunit, *) ! skips the line

    ! row 08 -- part of a file name
    ! This line will define the names of three output files:
    !   <fn_flux>.cont
    !   <fn_flux>.norm
    !   <fn_flux>.spec
    read(myunit, '(a)') main_flprefix

    ! row 09
    read(myunit, *) main_llzero, main_llfin, main_aint
    ! Some interpolation routines don't deal well with lambda having decimal places, therefore
    ! gonna round them
    main_llzero = floor(main_llzero)
    main_llfin = ceiling(main_llfin)
    main_aint = floor(main_aint)

    ! 101 format('read_main(): llzero=',f8.2,'; llfin=',f8.2,'; aint=',f6.2)
    ! write(lll,101) main_llzero, main_llfin, main_aint
    ! call log_info(lll)

    if (main_llzero .ge. main_llfin) then
      call log_and_halt('llzero must be lower than llfin!')
    end if

    close(unit=myunit)
    flag_read_main = .true.
  end
end












!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reading routines and variable declarations for *abonds file*

module file_abonds
  use logging
  ! use file_dissoc
  use file_main
  use dimensions
  implicit none

  !=====
  ! Variables filled by read_abonds() (file abonds.dat)
  !=====
  integer abonds_nabond ! ?doc?
  character*2 abonds_ele(MAX_ABONDS_NABOND)     ! symbol of element, e.g., HE, LI etc
  real*8      abonds_abol(MAX_ABONDS_NABOND), & ! logarithmic abundance (H=12)
              abonds_abo(MAX_ABONDS_NABOND)     ! number abundance calculated as 10**(abol-12)

  ! Flag indicating whether read_abonds() has already been called
  logical :: flag_read_abonds = .false.

contains

  !=======================================================================================
  ! Reads file abonds.dat to fill variables abonds_*
  !
  ! The input file has 3 columns:
  !   - Empty space or "1"
  !   - 2-character atomic element symbol (?)
  !   - absolute abundance (a) (exponent; actual abundance is 10^a), unit "dex"
  !
  ! The end of the file is signalled by two rows containing only "1" each at
  ! column 1 and nothing else at the others, i.e.,
  !
  ! .......(last data row).......
  ! 1
  ! 1

  subroutine read_abonds(path_to_file)
    implicit none
    integer myunit, finab, j
    character(len=*) :: path_to_file
    real*8 fstar

    if (.not. flag_read_main) then
      call log_and_halt('read_main() must be called before read_abonds()')
    end if

    ! this logging message is important in case of errors to know which file it was
    call log_info('read_abonds(): reading file '''//trim(path_to_file)//'''...')

    open(newunit=myunit,file=path_to_file, status='old')

    fstar = 10**main_afstar

    j = 1
    finab = 0
    do while (finab .lt. 1)
      read(myunit, '(i1,a2,f6.3)') finab, abonds_ele(j), abonds_abol(j)

      if (finab .lt. 1) then
        abonds_ele(j) = adjust_atomic_symbol(abonds_ele(j))

        ! [2] Calculates abonds_ABO based on abonds_ABOL
        ! (MT) By definition, the "abundance of element X" is given by (X/H) = log10(NX/NH) - 12
        !      In abonds.dat the values are is log10(NX/NH) - [Fe/H]
        !      In dissoc.dat. the valus are log10(NX/NH) - 12 - [Fe/H]
        abonds_abo(j) = 10.**(abonds_abol(j)-12.)
        abonds_abo(j) = abonds_abo(j)*fstar
      end if

      j = j+1
    end do
    abonds_nabond = j-2

    close(unit=myunit)
    flag_read_abonds = .true.
  end
end











!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reading routines and variable declarations for *dissoc file*

module file_dissoc
  use logging
  use dimensions
  use misc
  use config
  use file_abonds
  implicit none

  logical :: flag_read_dissoc = .false.    ! Whether read_dissoc() has already been called


  ! dissoc.dat, metals part
  integer dissoc_nmetal ! number of elements considered in chemical equilibrium
  integer dissoc_nimax  ! maximum number of iterations in Newton-Rapson method
  character*2 dissoc_elems(MAX_DISSOC_NMETAL)    ! Elements table field 1/6: atomic symbol
  integer     dissoc_nelemx(MAX_DISSOC_NMETAL)   ! Elements table field 2/6: atomic number
  real*8      dissoc_ip(MAX_DISSOC_NMETAL)      ! Elements table field 3/6: ?
  integer     dissoc_ig0(MAX_DISSOC_NMETAL),  & ! Elements table field 4/6: ?
              dissoc_ig1(MAX_DISSOC_NMETAL)     ! Elements table field 5/6: ?
  real*8      dissoc_cclog(MAX_DISSOC_NMETAL)   ! Elements table field 6/6: ?

  ! dissoc.dat, molecules part
  character*3 dissoc_mol(MAX_DISSOC_NMOL)     ! Molecules table field 01: molecule name
  real*8 dissoc_c(MAX_DISSOC_NMOL, 5)         ! Molecules table fields 02-06
  integer dissoc_mmax(MAX_DISSOC_NMOL), &     ! Molecules table field 07
          dissoc_nelem(5, MAX_DISSOC_NMOL), & ! Molecules table fields 08, 10, ... (atomic number)
          dissoc_natom(5, MAX_DISSOC_NMOL)    ! Molecules table fields 09, 11, ... (number of atoms)
  integer dissoc_nmol
  real*8 dissoc_eps,   & ! if abs((x(i+1)-x(i))/x(i)) .le. eps: converged
         dissoc_switer   ! flag affecting x(i+1)
                         !   - if switer .gt. 0 then x(i+1)=0.5*(x(i+1)+x(i))
                         !   - if switer .le. 0 then x(i+1)=x(i+1)

  private internal_read_dissoc
contains

  !=======================================================================================
  ! Reads dissoc.dat to fill variables dissoc_*
  !
  ! TODO PROPOSE: use READ()'s "END=" option

  subroutine read_dissoc(path_to_file)
    integer myunit
    character(len=*) :: path_to_file
    call log_info('read_dissoc(): reading file '''//trim(path_to_file)//'''...')
    open(newunit=myunit,file=path_to_file, status='old')
    call internal_read_dissoc(myunit)
    close(unit=myunit)
    flag_read_dissoc = .true.
  end

  !=======================================================================================
  ! Searches for atomic symbol inside dissoc_elems
  !
  ! Returns index, or 0 if not found

  integer function find_atomic_symbol_dissoc(symbol, flag_ignore)
    ! Atomic symbol, case-insensitive
    character(*), intent(in) :: symbol
    ! (optional, defaults to .false.) If set, will return ZERO if symbol is not found.
    ! The default behaviour is to halt the program.
    logical, intent(in), optional :: flag_ignore
    character(:), allocatable :: symbol_ ! formatted symbol
    integer :: i, j
    logical :: flag_ignore_

    flag_ignore_ = .false.
    if (present(flag_ignore)) flag_ignore_ = flag_ignore
    symbol_ = adjust_atomic_symbol(symbol)

    do i = 1, dissoc_nmetal
      if (dissoc_elems(i) .eq. symbol_) then
        find_atomic_symbol_dissoc = i
        return
      end if
    end do

    if (.not. flag_ignore_) then
      call log_and_halt('Atomic symbol "'//symbol_//'" not found in dissoc metals table')
    end if
    find_atomic_symbol_dissoc = 0
  end

  !=======================================================================================
  ! In the absence of a *dissoc file*, fills in dissoc_* using another strategy.
  !
  ! The strategy is to use an internal buffer and replace the abundances using the values
  ! read from the *abonds file* .
  !

  subroutine auto_dissoc()
    integer myunit, i, j, num_el
    character(2) :: el
    logical :: found
    real*8 abund
    integer, parameter :: NUM_LINES = 59, NUM_COLS = 74
    character(NUM_COLS) :: buffer(NUM_LINES) = (/&
'   18  100   0.00500  -1.00000 Created by pfant:auto_dissoc()             ', &
' H       1    13.598    2    1                                            ', &
'HE       2    24.587    1    2                                            ', &
' C       6    11.260    9    6                                            ', &
' N       7    14.534    4    9                                            ', &
' O       8    13.618    9    4                                            ', &
' A       9    12.268    9    6                                            ', &
'NA      11     5.139    2    1                                            ', &
'MG      12     7.646    1    2                                            ', &
'AL      13     5.986    6    1                                            ', &
'SI      14     8.151    9    6                                            ', &
' P      15    10.486    4    9                                            ', &
' S      16    10.360    9    4                                            ', &
'CA      20     6.113    1    2                                            ', &
'SC      21     6.540   10   15                                            ', &
'TI      22     6.820   21   28                                            ', &
'FE      26     7.870   25   30                                            ', &
'NI      28     7.635   21   10                                            ', &
'CU      29     7.726    2    1                                            ', &
' AN     1.28051e+01-8.27934e+00 6.41622e-02-7.36267e-03 3.46663e-042 91 71', &
' CN     1.28051e+01-8.27934e+00 6.41622e-02-7.36267e-03 3.46663e-042 61 71', &
'CAH     1.13401e+01-3.01442e+00 4.23487e-01-6.14674e-02 3.16392e-032201 11', &
'MGO     1.17018e+01-5.03261e+00 2.96408e-01-4.28111e-02 2.20232e-032121 81', &
'TIO     1.33981e+01-8.59562e+00 4.08726e-01-5.79369e-02 2.92873e-032221 81', &
'MGH     1.12853e+01-2.71637e+00 1.96585e-01-2.73103e-02 1.38164e-032121 11', &
' AC     1.28038e+01-6.51780e+00 9.77186e-02-1.27393e-02 6.26035e-042 91 61', &
' AA     1.28038e+01-6.51780e+00 9.77186e-02-1.27393e-02 6.26035e-041 92   ', &
' CC     1.28038e+01-6.51780e+00 9.77186e-02-1.27393e-02 6.26035e-041 62   ', &
'HOH     2.54204e+01-1.05223e+01 1.69394e-01-1.83684e-02 8.17296e-042 12 81', &
'ALH     1.21913e+01-3.76361e+00 2.55568e-01-3.72612e-02 1.94061e-032131 11', &
'ALO     1.27393e+01-5.25336e+00 1.82177e-01-2.57927e-02 1.31850e-032131 81', &
' AO     1.38200e+01-1.17953e+01 1.72167e-01-2.28885e-02 1.13491e-032 91 81', &
' CO     1.38200e+01-1.17953e+01 1.72167e-01-2.28885e-02 1.13491e-032 61 81', &
' AH     1.21355e+01-4.07599e+00 1.27676e-01-1.54727e-02 7.26615e-042 91 11', &
' CH     1.21355e+01-4.07599e+00 1.27676e-01-1.54727e-02 7.26615e-042 61 11', &
' AP     1.24988e+01-6.47155e+00 1.23609e-01-1.74113e-02 8.97797e-042 91151', &
' CP     1.24988e+01-6.47155e+00 1.23609e-01-1.74113e-02 8.97797e-042 61151', &
' AS     1.34357e+01-8.55736e+00 1.87544e-01-2.55069e-02 1.27346e-032 91161', &
' CS     1.34357e+01-8.55736e+00 1.87544e-01-2.55069e-02 1.27346e-032 61161', &
' NH     1.20327e+01-3.84349e+00 1.36286e-01-1.66427e-02 7.86913e-042 71 11', &
' NO     1.28310e+01-7.19644e+00 1.73495e-01-2.30652e-02 1.13799e-032 71 81', &
' NN     1.35903e+01-1.05855e+01 2.20671e-01-2.99975e-02 1.49927e-031 72   ', &
' OH     1.23710e+01-5.05783e+00 1.38217e-01-1.65474e-02 7.72245e-042 81 11', &
' OO     1.32282e+01-5.51807e+00 6.99354e-02-8.15109e-03 3.79699e-041 82   ', &
'NAH     1.14155e+01-2.75079e+00 1.96460e-01-2.73828e-02 1.38445e-032111 11', &
'SIF     1.24272e+01-5.83726e+00 1.66854e-01-2.27883e-02 1.13711e-032141 91', &
'SIH     1.18522e+01-3.74185e+00 1.59988e-01-2.06292e-02 9.98967e-042141 11', &
'SIO     1.34132e+01-8.87098e+00 1.50424e-01-1.95811e-02 9.48283e-042141 81', &
'SIN     1.23989e+01-5.48756e+00 9.53008e-02-1.33693e-02 6.93956e-042141 71', &
' PH     1.20802e+01-4.64388e+00 3.41137e-01-4.87959e-02 2.50567e-032151 11', &
'CAO     1.22598e+01-6.05249e+00 5.82844e-01-8.58050e-02 4.44251e-032201 81', &
'SCO     1.37467e+01-8.64196e+00 4.80722e-01-6.96697e-02 3.57468e-032211 81', &
'FEO     1.29845e+01-5.33182e+00 3.17452e-01-4.45649e-02 2.25240e-032261 81', &
'NIH     1.25203e+01-3.43307e+00 1.96303e-01-2.53774e-02 1.24117e-032281 11', &
'CUH     1.21518e+01-3.91900e+00 3.09765e-01-4.33886e-02 2.19329e-032291 11', &
'CUO     1.21756e+01-4.28270e+00 2.04849e-01-2.82166e-02 1.40747e-032291 81', &
' HH     1.27388e+01-5.11717e+00 1.25720e-01-1.41494e-02 6.30214e-041 12   ', &
'                                                                          ', &
'                                                                          ' &
/)

    if (.not. flag_read_abonds) &
     call log_and_halt('auto_dissoc(): must call read_abonds() first', is_assertion=.true.)


    ! Inserts abundances from abonds.dat in buffer
    read(buffer(1), *) num_el  ! number of elements is in "upper left corner" of buffer
    do i = 1, num_el
      el = buffer(i+1)(1:2)

      if (el .eq. ' H') then
        abund = 0.  ! hydrogen in not in abonds.dat, and must be 0 in dissoc.dat by convention
      else
        found = .false.
        do j = 1, abonds_nabond
          if (abonds_ele(j) .eq. el) then
            found = .True.
            exit
          end if
        end do
        if (.not. found) then
          call log_and_halt('auto_dissoc(): Element "'//trim(el)//'" not found in abundances file "'//&
           trim(config_fn_abonds)//'"')
        end if
        abund = abonds_abol(j)-12
      end if

      write(buffer(i+1)(31:40), '(f10.5)') abund
    end do


    !do i = 1, NUM_LINES
    !  print '(a74)', buffer(i)
    !end do
    !stop -1


    ! Creates and fills temporary file
    open(newunit=myunit, status='scratch')
    do i = 1, NUM_LINES
      write(myunit, '(a'//int2str(NUM_COLS)//')') buffer(i)
    end do
    rewind(myunit)

    ! Finally calls the reading subroutine
    call internal_read_dissoc(myunit)
    close(myunit)
  end

  !=======================================================================================
  ! Fill variables dissoc_* given a Fortran unit number
  !

  subroutine internal_read_dissoc(myunit)
    integer, intent(in) :: myunit
    integer i, j, k, m, mmaxj
    character*2 symbol, symbol_
    logical flag_found
    integer*4 natomm, nelemm
    dimension natomm(5), nelemm(5)

    ! row 01
    ! read(myunit,'(2i5, 2f10.5, i10)') dissoc_nmetal, dissoc_nimax, dissoc_eps, dissoc_switer

    ! 20160920 don't need formats, free format is better, I think
    read(myunit,*) dissoc_nmetal, dissoc_nimax, dissoc_eps, dissoc_switer

    ! rows 02 to NMETAL+1: 6-column rows
    !
    !
    !
    do i = 1, dissoc_nmetal
      ! read (myunit, '(a2, 2x, i6, f10.3, 2i5, f10.5)')

      read (myunit, *) &
       symbol_, dissoc_nelemx(i), dissoc_ip(i), &
       dissoc_ig0(i), dissoc_ig1(i), dissoc_cclog(i)

      symbol = adjust_atomic_symbol(symbol_)

      ! makes sure that elements first and second are h and he, respectively,
      ! because sat4() and die() count on this
      select case (i)
        case (1)
          if (symbol .ne. ' H') then
            write(lll,*) 'First element must be hydrogen (" H"), not "', symbol_, '"!'
            call log_and_halt(lll)
          end if
        case (2)
          if (symbol .ne. 'HE') then
            write(lll,*) 'First element must be helium ("HE"), not "', symbol_, '"!'
            call log_and_halt(lll)
          end if
      end select

      dissoc_elems(i) = symbol

      ! spill check
      if (dissoc_nelemx(i) .gt. MAX_DISSOC_Z) then
        call log_and_halt('read_dissoc(): metal # '//int2str(i)//': nelemxi = '//&
         int2str(dissoc_nelemx(i))//' over maximum allowed (MAX_DISSOC_Z='//int2str(MAX_DISSOC_Z)//')')
      end if
    end do



    ! rows NMETAL+2 till end-of-file
    !   col  1     -- "name" of molecule
    !   cols 2-6   -- c(J, 1-5)
    !   col  7     -- mmax(j) (number of subsequent columns)/2
    !   cols 8-... -- maximum of 8 columns here.
    !                 pairs (nelem(m), natom(m)), m = 1 to mmax(j)
    j = 0

    1010 continue
    j = j+1


    ! TFree format here is not possible because there is no space between the last fields
    read(myunit, '(a3, 5x, e11.5, 4e12.5, i1, 4(i2,i1))') &
                 dissoc_mol(j), &
                 (dissoc_c(j, k), k=1,5), &
                 dissoc_mmax(j), &
                 (nelemm(m), natomm(m), m=1,4)

    mmaxj = dissoc_mmax(j)
    if(mmaxj .eq. 0) then
      ! note: interesting that Fortran accepts a blank line and reads everything blank or zero

      go to 1014  ! means end-of-file
    end if

    ! consistency check:
    if (mmaxj .gt. 4) then
      write(lll,*) 'read_dissoc() molecule "', dissoc_mol(j), &
       '", mmaxj = ', mmaxj, ' cannot be greater than 4!'
      call log_and_halt(lll)
    end if

    ! consistency check
    do m = 1, mmaxj
      flag_found = .false.
      do i = 1, dissoc_nmetal
        if (nelemm(m) .eq. dissoc_nelemx(i)) then
          flag_found = .true.
          exit
        end if
      end do

      if (.not. flag_found) then
        write(lll,*) 'read_dissoc() molecule "', dissoc_mol(j), &
         '" atomic number ', nelemm(m), 'not in atoms list above'
        call log_and_halt(lll)
      end if
    end do

    do m = 1, mmaxj
        dissoc_nelem(m,j) = nelemm(m)
        dissoc_natom(m,j) = natomm(m)
    end do

    go to 1010

    1014 continue
    dissoc_nmol = j-1

    write(lll,*) 'Last molecule considered in dissoc file is ', dissoc_mol(dissoc_nmol)
    call log_debug(lll)
  end
end






!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Atmospheric models
!
! This module contains routines to deal with three types of files:
!
!   ".mod": binary file containing one or more atmospheric models.
!           Such a file can be created either
!           by innewmarcs (single-model file, e.g., "modeles.mod"), or
!           by create-grid.py (multi-model file aka model grid, e.g., "grid.mod").

!   ".moo": binary file containing one or more atmospheric models,
!           *including opacity information*, e.g., "grid.moo"
!           Such a file is created by create-grid.py.
!
!   ".opa": this is MARCS text format. Here we can either read or write such files.
!           innewmarcs uses the routine write_opa() to create an interpolated opacity file
!           compliant with the MARCS ".opa" standard.
!

module file_models
  use logging
  use dimensions
  use file_main
  use misc
  implicit none


  integer, parameter :: MOD_RECL = 1200 ! record length
  ! Restrictions on vector and record sizes. They could be lifted if the MARCS models get
  ! updated, but at the moment it is the simplest form:
  ! - allows to easily know the number of records based on the file size
  ! - allows for faster file reading
  integer, parameter :: MOO_RECL = 1200+484692, & ! record length
                        MOO_NTOT = 56, &  ! number of layers *must be 56*
                        MOO_NWAV = 1071   ! number of wavelengths *must be 1071


  ! Structure to store atmospheric model read from binary file.
  ! Binary file follows "NewMarcs" structure containing real numbers stored as real*4.
  ! Hence the real*4 declarations.
  type moo_record
    ! # "Standard model" information
    ! Size of variables nh, teta, pe, pg, log_tau_ross
    integer*4 :: ntot

    real*8 :: teff,    & ! Teff (Kelvin)
              glog,    & ! log10(g) (g in [cm/s^2])
              asalog,  & ! [Fe/H]
              asalalf, & ! [alpha/Fe]
              nhe        ! abundance of Helium: 10**([He/H]-12)
                         ! e.g., Sun: 10**(10.93-12) ~= 0.0851

    real*8, dimension(MAX_MODELES_NTOT) :: &
     nh,   & ! density of the hydrogen column
     teta, & ! 5040./temperature (temperature in K)
     pe,   & ! electron pressure [dyn/cm2]
     pg,   & ! gas pressure [dyn/cm2]
     log_tau_ross     ! log(tau(Rosseland))

    character*20 :: tit, tiabs


    ! # Opacity information
    ! standard wavelength for the continuous optical depth (tau)
    ! scale and for the total standard opacity (ops)
    real*8 swave
    ! number of wavelengths for which continuous absorption and
    ! scattering opacities are given. These are chosen so that
    ! linear interpolation should suffice for any wavelength
    integer nwav
    ! wav(j): wavelengths for which opacities are given
    real*8 wav(MOO_NWAV)
    ! **Model structure**
    ! rad(k): radius, normalized on the outermost point, k=1. For use with
    !          spherical radiative transfer.
    !          For plane-parallel models rad == 1.0.
    ! tau(k): continuumm optical depth at the standard wavelength swave
    ! t(k)  : temperature (K)
    ! rho(k): densigy (g/cm3)
    ! xi(k) : microturbulence parameter (km/s)
    ! ops(k): continuumm opacity at the standard wavelength (cm2/g)
    real*8, dimension(MAX_MODELES_NTOT) :: rad, tau, t, rho, xi, ops
    ! **Wavelength dependent opacities**
    ! abs(j,k): specific continuous absorption opacity (cm2/g)
    ! sca(j,k): specific continuous scattering opacity (cm2/g)
    real*8, dimension(MOO_NWAV, MAX_MODELES_NTOT) :: abs, sca

    real*8 :: abund(92)  ! Abundances of atomic elements from 1 to 92

  end type

  ! Public variable that will contain the model loaded by read_modele()
  type(moo_record) modele

   ! Stores unit number of open *mod file*
   integer, private :: unit_mod

   ! Whether the file is open
   logical, private :: flag_open = .false.

  ! "Magic characters" to identify a file as a MARCS opacity file.
  ! This is part of the file specs in MARCS documentation.
  character(4), parameter :: OPA_MAGIC_CHARS = "MRXF"
contains
  !=======================================================================================
  ! Returns number of records in file

  integer function get_mod_num_records(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer :: size

    inquire(FILE=path_to_file, SIZE=size)

    ! Note the "-1": last 1200 bytes are used as a end-of-file flag with only an integer
    ! value of 9999 recorded
    get_mod_num_records = size/MOD_RECL-1
  end

  !=======================================================================================
  ! Opens existing models binary file

  subroutine open_mod_file(path_to_file, status_)
    character(*), intent(in) :: path_to_file
    character(*), intent(in), optional :: status_ ! default: "old"
    character(:), allocatable :: status__

    if (present(status_)) then
      status__ = status_
    else
      status__ = 'old'
    end if

    if (flag_open) then
      call log_and_halt('There is already a models file open', is_assertion=.true.)
    end if
    open(newunit=unit_mod, access='direct',status=status__, file=path_to_file, recl=MOD_RECL)
    flag_open = .true.
  end

  !=======================================================================================
  ! Closes open file

  subroutine close_mod_file()
    if (.not. flag_open) then
      call log_and_halt('No models is open', is_assertion=.true.)
    end if
    close(unit_mod)
    flag_open = .false.
  end


  !=======================================================================================
  ! Reads single record from .mod file
  !
  ! This routine is constructed in a way that it can be used by read_modele() and
  ! also within the inewmarcs module.
  !
  ! *Note* Output goes within "rec". This could be made as a function and return the
  !       record, but then it wouldn't be so clear how the compiler would work out the
  !       memory, i.e., would the whole structure be copied into the variable declared at
  !       the caller?? Better to avoid doubts: we know that args are passed by reference.

  subroutine read_mod_record(rec_id, rec)
    integer, intent(in) :: rec_id       ! record identifier (>= 1)
    type(moo_record), intent(out) :: rec
    ! Gotta use single precision  variables to read file because the real numbers are all
    ! stored with 4 bytes only (not 8)
    real*4 teff, glog, asalog, asalalf, nhe
    real*4, dimension(max_modeles_ntot) :: nh, teta, pe, pg, log_tau_ross

    integer i

    ! Record is read twice; second time bid "grabs" everything that was read before
    real*4 bid(16)

    read(unit=unit_mod, rec=rec_id) &
     rec%ntot,    &
     teff,    &
     glog,    &
     asalog,  &
     asalalf, &
     nhe,     &
     rec%tit,     &
     rec%tiabs

    ! The last record of the binary file is a "flag" record. If it contains a 9999,
    ! It is the last record of the file.
    if (rec%ntot .eq. 9999) then
      call log_and_halt('Le modele desire ne est pas sur le fichier')
    end if

    ! spill check: Checks if exceeds maximum number of elements allowed
    if (rec%ntot .gt. MAX_MODELES_NTOT) then
      call log_and_halt('read_mod_record(): ntot = '//int2str(rec%ntot)//&
       ' exceeded maximum of MAX_MODELES_NTOT='//int2str(MAX_MODELES_NTOT))
    end if

    ! replaces \x00 character by space
    call replace_char(rec%tit, char(0), ' ')
    call replace_char(rec%tiabs, char(0), ' ')

    rec%teff = teff
    rec%teff = teff
    rec%glog = glog
    rec%asalog = asalog
    rec%asalalf = asalalf
    rec%nhe = nhe

    read(unit_mod, rec=rec_id) bid, &
         (nh(i),   &
          teta(i), &
          pe(i),   &
          pg(i),   &
          log_tau_ross(i), i=1,rec%ntot)

    rec%nh = nh
    rec%teta = teta
    rec%pe = pe
    rec%pg = pg
    rec%log_tau_ross = log_tau_ross

    write(lll, *) 'read_mod_record(): ntot=', rec%ntot
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): teff=', rec%teff
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): glog=', rec%glog
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): asalog=', rec%asalog
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): asalalf=', rec%asalalf
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): nhe=', rec%nhe
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): tit=', rec%tit
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): tiabs=', rec%tiabs
    call log_debug(lll)
  end


  !=======================================================================================
  ! Reads single record from file *mod file* into variables modeles_*
  !

  subroutine read_modele(path_to_file)
    character(len=*) :: path_to_file
    real*8 ddt, ddg, ddab
    integer i, &
            id_
    type(moo_record) :: r

    ! this logging message is important in case of errors to know which file it was
    call log_info('read_modele(): reading file '''//trim(path_to_file)//'''...')
    call open_mod_file(path_to_file)
    call read_mod_record(1, r)
    call close_mod_file()

    ! consistency check: these were already present in the 2015- code
    ddt  = abs(main_teff-r%teff)
    ddg = abs(main_glog-r%glog)
    ddab = abs(main_asalog-r%asalog)

    ! ISSUE I think that nhe doesn't need to match. Anyway, this was already commented out in the original pfantgrade.f
    ! r%nhe comes straight from the newmarcs grid file and is not calculated.
    ! if (abs(main_nhe-r%nhe) .gt. 0.001) then
    !  write(lll, *) 'modele nhe (', r%nhe, ') does not match main nhe (', main_nhe, ')'
    !  call log_and_halt(lll)
    ! end if

    if(ddt .gt. 1.0) then
      write(lll,*) 'read_modele(): abs(main_teff-(model teff)) = ', ddt, ' > 1.0'
      call log_and_halt(lll)
    end if
    if(ddg .gt. 0.01) then
      write(lll,*) 'read_modele(): abs(main_glog-(model glog)) = ', ddg, ' > 0.01'
      call log_and_halt(lll)
    end if
    if(ddab .gt. 0.01) then
      write(lll,*) 'read_modele(): abs(main_asalog-(model asalog)) = ', ddab, ' > 0.01'
      call log_and_halt(lll)
    end if

    ! ready to copy (& convert) variables to their counterparts
    modele%ntot    = r%ntot     ! integer(4)-to-integer(?)
    modele%teff    = r%teff    ! real(4) to real(8)
    modele%glog    = r%glog    ! "
    modele%asalog  = r%asalog   ! "
    modele%asalalf = r%asalalf  ! "
    modele%nhe     = r%nhe      ! "
    do i = 1, modele%ntot
      modele%nh(i)   = r%nh(i)   ! real(4) to real(8)
      modele%teta(i) = r%teta(i) ! "
      modele%pe(i)   = r%pe(i)   ! "
      modele%pg(i)   = r%pg(i)   ! "
      modele%log_tau_ross(i)  = r%log_tau_ross(i)  ! "
    end do
    modele%tit = r%tit
    modele%tiabs = r%tiabs
  end



  !---------------------------------------------------------------------------------------
  ! Creates single-model file such as "modeles.mod"

  subroutine write_modele(path_to_file, r)
    character(len=*), intent(in) :: path_to_file
    type(moo_record), intent(in) :: r
    integer k
    call open_mod_file(path_to_file, 'replace')
    call write_mod_record(unit_mod, 1, r)
    write(unit_mod, rec=2) 9999
    call close_mod_file()
  end

  !---------------------------------------------------------------------------------------
  ! Writes record to an open ".mod" binary file

  subroutine write_mod_record(unit_, inum, r)
    integer, intent(in) :: unit_ ! unit of an open file
    integer, intent(in) :: inum ! record number (first=1)
    type(moo_record), intent(in) :: r ! record to be written
    real*4 :: a(MAX_MODELES_NTOT*5)
    integer n, k

    k = 1
    do n = 1,r%ntot
      a(k)   = r%nh(n)
      a(k+1) = r%teta(n)
      a(k+2) = r%pe(n)
      a(k+3) = r%pg(n)
      a(k+4) = r%log_tau_ross(n)
      k = k+5
    end do

    write(unit_, rec=inum) &
     r%ntot,         &
     sngl(r%teff),   &
     sngl(r%glog),   &
     sngl(r%asalog), &
     sngl(r%asalalf),&
     sngl(r%nhe),    &
     r%tit,          &
     r%tiabs,        &
     (a(k),k=1,r%ntot*5)
    return
  end


  !---------------------------------------------------------------------------------------
  ! Reads entire model grid into array of moo_record

  subroutine read_mod_grid(path_to_file, recs)
    character(len=*), intent(in) :: path_to_file
    type(moo_record), allocatable, intent(out) :: recs(:)
    integer num_rec, iid

    ! this logging message is important in case of errors to know which file it was
    call log_info('read_mod_grid(): reading file '''//trim(path_to_file)//'''...')

    num_rec = get_mod_num_records(path_to_file)
    allocate(recs(num_rec))
    call open_mod_file(path_to_file)
    do iid = 1, num_rec
      call read_mod_record(iid, recs(iid))
    end do
    call close_mod_file()
  end



  !=======================================================================================
  ! Returns number of records in file

  integer function get_moo_num_records(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer :: size
    inquire(FILE=path_to_file, SIZE=size)
    get_moo_num_records = size/MOO_RECL
  end

  !=======================================================================================
  ! Reads a model grid file and returns an array of moo_record

  subroutine read_moo(path_to_file, recs)
    character(len=*), intent(in) :: path_to_file
    type(moo_record), allocatable :: recs(:)
    integer myunit, num_rec,  iid, i, j, nwav
    real*4 teff, glog, asalog, asalalf, nhe
    real*4, dimension(MAX_MODELES_NTOT) :: nh, teta, pe, pg, log_tau_ross
    real*4 swave
    real*4 wav(MOO_NWAV)
    real*4, dimension(MAX_MODELES_NTOT) :: ops
    real*4, dimension(92) :: abund
    real*4, dimension(MOO_NWAV, MAX_MODELES_NTOT) :: abs_, sca
    ! Variable used to skip a few characters
    character bid(MOD_RECL)


    num_rec = get_moo_num_records(path_to_file)
    allocate(recs(num_rec))

    ! this logging message is important in case of errors to know which file it was
    call log_info('read_moo(): reading file '''//trim(path_to_file)//'''...')

    open(newunit=myunit, form='unformatted', access='stream',status='old', file=path_to_file)
    do iid = 1, num_rec
      read(myunit) &
       recs(iid)%ntot,    &
       teff,    &
       glog,    &
       asalog,  &
       asalalf, &
       nhe,     &
       recs(iid)%tit,     &
       recs(iid)%tiabs

      if (recs(iid)%ntot .ne. MOO_NTOT) &
        call log_and_halt('read_moo(): (record #'//int2str(iid)//') Number of depth layers must be '//&
         int2str(MOO_NTOT)//', not '//int2str(recs(iid)%ntot))

      ! replaces \x00 character by space
      call replace_char(recs(iid)%tit, char(0), ' ')
      call replace_char(recs(iid)%tiabs, char(0), ' ')

      recs(iid)%teff = teff
      recs(iid)%teff = teff
      recs(iid)%glog = glog
      recs(iid)%asalog = asalog
      recs(iid)%asalalf = asalalf
      recs(iid)%nhe = nhe

      read(myunit) &
           (nh(i),   &
            teta(i), &
            pe(i),   &
            pg(i),   &
            log_tau_ross(i), i=1,recs(iid)%ntot)

      recs(iid)%nh = nh
      recs(iid)%teta = teta
      recs(iid)%pe = pe
      recs(iid)%pg = pg
      recs(iid)%log_tau_ross = log_tau_ross

      ! skips unused characters at the end of the MOD_RECL-sized record
      read(myunit) bid(1:(MOD_RECL-5*4*recs(iid)%ntot-64))

      ! now the opacity part
      read(myunit) swave, recs(iid)%nwav

      if (recs(iid)%nwav .ne. MOO_NWAV) &
        call log_and_halt('read_moo(): Number of opacity wavelengths layers must be '//&
         int2str(MOO_NWAV)//', not '//int2str(recs(iid)%nwav))

      recs(iid)%swave = swave

      ! This statement is much faster than the one commented below
      read(myunit) &
       ops, wav, abs_, sca
      !-- Reading as follows does not req
      !-- read(myunit) &
      !--  (ops(j), j=1,recs(iid)%ntot), &
      !--  (wav(i), i=1,recs(iid)%nwav), &
      !--  ((abs_(i,j), i=1,recs(iid)%nwav), j=1,recs(iid)%ntot), &
      !--  ((sca(i,j), i=1,recs(iid)%nwav), j=1,recs(iid)%ntot)

      recs(iid)%ops = ops
      recs(iid)%wav = wav
      recs(iid)%abs = abs_
      recs(iid)%sca = sca

      read(myunit) abund
      recs(iid)%abund = abund
    end do

    close(myunit)
  end

  !---------------------------------------------------------------------------------------
  ! reads a MARCS opacity data file and stores the data inTO ẗhe *modele* module variable

  subroutine read_opa(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer myunit, i, k
    character mcode*4

    ! this logging message is important in case of errors to know which file it was
    call log_info('read_opa(): reading file '''//trim(path_to_file)//'''...')

    open(newunit=myunit,file=path_to_file, status='old')

    read(myunit, '(1x,a4,i5,f10.0)') mcode, modele%ntot, modele%swave
    ! Validates "magic characters"
    if (mcode .ne. OPA_MAGIC_CHARS) &
      call log_and_halt('Invalid opacities file: "'//trim(path_to_file)//'"')
    call assert_le(modele%ntot, MAX_MODELES_NTOT, 'read_opa()', 'ndp', 'MDP')

    read(myunit, *) modele%nwav
    call assert_le(modele%nwav, MOO_NWAV, 'read_opa()', 'nwav', 'MWAV')

    read(myunit, *) (modele%wav(i), i=1, modele%nwav)

    do k = 1, modele%ntot
      read(myunit,*) modele%rad(k), modele%tau(k), modele%t(k),  modele%pe(k), &
                     modele%pg(k),  modele%rho(k), modele%xi(k), modele%ops(k)
      read(myunit,*) (modele%abs(i,k), modele%sca(i, k), i=1,modele%nwav)

      do i = 1, modele%nwav
        ! absorption and scattering multiplied by standard value as in original readopa.f
        modele%abs(i,k) = modele%abs(i,k)*modele%ops(k)
        modele%sca(i,k) = modele%sca(i,k)*modele%ops(k)
      enddo
    enddo

    ! Reads abundances
    read(myunit,*) modele%abund
  end

  !---------------------------------------------------------------------------------------
  ! Creates a MARCS opacity file

  subroutine write_opa(path_to_file, r)
    character(len=*), intent(in) :: path_to_file
    type(moo_record), intent(in) :: r
    integer myunit, i, k
    character mcode*4
    real*8, dimension(MOO_NWAV) :: abs, sca

    open(newunit=myunit,file=path_to_file, status='replace')

    write(myunit, '(1x,a4,i5,f10.2)') OPA_MAGIC_CHARS, r%ntot, r%swave
    write(myunit, '(i6)') r%nwav
    write(myunit, '(1x,10f11.2)') (r%wav(i), i=1, r%nwav)
    do k = 1, r%ntot
      abs = r%abs(:, k)/r%ops(k)
      sca = r%sca(:, k)/r%ops(k)

      write(myunit,'(1x,1p8e11.4,0p)') r%rad(k), r%tau(k), r%t(k),  r%pe(k), &
       r%pg(k),  r%rho(k), r%xi(k), r%ops(k)
      write(myunit,'(1x,1p6e11.4,0p)') (abs(i), sca(i), i=1,r%nwav)

    enddo

    write (myunit, '(10f8.3)') r%abund
  end

end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reading routines and variable declarations for *hmap file*

module file_hmap
  use logging
  use dimensions
  use misc
  use file_main
  implicit none

  ! Structure to store one row of *hmap file*
  type hmap_row
    ! file name
    character*64 fn
    ! lower level
    integer na
    ! upper level
    integer nb
    ! Central lambda
    real*8 clam
    ! excitation potential for the lower level of transition
    real*8 kiex
    ! C1 ?doc?
    real*8 c1
  end type

  ! Array to store all rows in *hmap file*
  type(hmap_row) :: hmap_rows(MAX_FILETOH_NUM_FILES)

  ! Number of rows in *hmap file*
  integer :: hmap_n

contains

  !=======================================================================================
  ! Reads *hmap file* to fill variables hmap_*
  !

  subroutine read_hmap(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer myunit
    logical :: skip_row(MAX_FILE_ROWS)
    integer :: num_rows, i

    ! Temporary auxiliary variables for reading file
    character*64 t_fn
    integer t_na
    integer t_nb
    real*8 t_clam
    real*8 t_kiex
    real*8 t_c1

    ! this logging message is important in case of errors to know which file it was
    call log_info('read_hmap(): reading file '''//trim(path_to_file)//'''...')

    call map_file_comments(path_to_file, skip_row, num_rows)

    open(newunit=myunit,file=path_to_file, status='old')

    hmap_n = 0
    do i = 1, num_rows
      if (skip_row(i)) then
        read(myunit,*)   ! skips comment row
      else
        read(myunit, *) t_fn, t_na, t_nb, t_clam, t_kiex, t_c1

        hmap_n = hmap_n+1

        hmap_rows(hmap_n)%fn = t_fn
        hmap_rows(hmap_n)%na = t_na
        hmap_rows(hmap_n)%nb = t_nb
        hmap_rows(hmap_n)%clam = t_clam
        hmap_rows(hmap_n)%kiex = t_kiex
        hmap_rows(hmap_n)%c1 = t_c1
      end if
    end do
    close(unit=myunit)


    !#logging
    call log_debug('read_hmap():')
    call log_debug('filename         na nb c.lambda     kiex       c1')
    do i = 1, hmap_n
      11 format(a,1x,i2,1x,i2,1x,f8.2,1x,f8.2,1x,f8.2)
      write(lll, 11) hmap_rows(i)
      call log_debug(lll)
    end do
  end
end








!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reading routines and variable declarations for dfile:filetoh

module file_toh
  use dimensions
  use file_models
  use file_hmap
  implicit none

  !=====
  ! Read directly from file
  !=====
  ! These are read by read_filetoh()
  ! ?doc?
  character*80 filetoh_titre(MAX_FILETOH_NUM_FILES)
  ! ?doc?
  character*11 filetoh_ttt(MAX_FILETOH_NUM_FILES)
  ! Will be pointer target
  ! ?doc?
  real*8, target, dimension(MAX_FILETOH_NUM_FILES, MAX_FILETOH_JMAX, MAX_MODELES_NTOT) :: &
   filetoh_th
  ! Will be pointer target
  ! ?doc?
  real*8, target, dimension(MAX_FILETOH_NUM_FILES, MAX_FILETOH_JMAX) :: filetoh_lambdh
  ! ?doc?
  integer filetoh_jmax(MAX_FILETOH_NUM_FILES)

  ! List of lambdas for the hydrogen lines (read from *hmap file*)
  real*8, dimension(MAX_FILETOH_NUM_FILES) :: filetoh_llhy
  ! Number of filetoh files that were actually found in disk
  integer :: filetoh_num_files = 0
  ! Names of filetoh files that were actually found in disk
  character*64 filetoh_filenames(MAX_FILETOH_NUM_FILES)

contains

  !=======================================================================================
  ! Tries to open and read all files listed in variable hmap_rows
  !
  ! [llzero, llfin] is the calculation lambda interval. Here this interval is used for
  ! error checking: if it doesn't find a file that should exist, it will give an error
  !
  ! LECTURE DE LA PROFONDEUR OPTIQUE DANS LA RAIE D H

  subroutine read_filetoh(llzero, llfin)
    real*8, intent(in) :: llzero, llfin
    integer myunit
    integer i, j, n, i_file, ntot
    character(len=:), allocatable :: fn_now
    real*8 :: clam
    logical :: must_exist, flag_inside

    i = 0

    do i_file = 1, hmap_n
      fn_now = trim(hmap_rows(i_file)%fn)
      clam = hmap_rows(i_file)%clam

      must_exist = .false.
      flag_inside = .true.
      if (clam .ne. 0) then
        if (h_line_is_inside(clam, llzero, llfin)) then
          must_exist = .true.
        else
          flag_inside = .false.  ! hydrogen line if outside calculation interval, skips it
        end if
      else
        ! list of nydrogen line files came from *main file* and we don't know their central lambda unless we open the file
      end if

      if (flag_inside) then
      ! this logging message is important in case of errors to know which file it was
      call log_info('read_filetoh(): reading file '''//trim(fn_now)//'''...')
      open(err=111, unit=myunit,file=fn_now,status='old')

        i = i+1

        read(myunit,'(a80)') filetoh_titre(i)
        read(myunit,'(i6)') ntot
        if (ntot .ne. modele%ntot) then
          call log_and_halt('read_filetoh(): file "'//trim(fn_now)//&
           '": number of atmospheric layers must be '//int2str(modele%ntot)//&
           ', not '//int2str(ntot))
        end if
        read(myunit,*) filetoh_jmax(i)
        read(myunit,'(5f14.3)') (filetoh_lambdh(i,j), j=1,filetoh_jmax(i))
        read(myunit,'(5e12.4)') ((filetoh_th(i,j,n),j=1,filetoh_jmax(i)), n=1,modele%ntot)
        write(14, '(53e12.4)') ((filetoh_th(i,j,n),j=1,filetoh_jmax(i)), n=1,modele%ntot)
        close(myunit)

        ! Takes first lambda of file as a reference
        clam = filetoh_lambdh(i, 1)

        if (.not. h_line_is_inside(clam, llzero, llfin)) then
          i = i-1  ! "rewinds" 1
          go to 112
        end if

        filetoh_llhy(i) = clam

        ! Registers filename in list of files that were found
        filetoh_filenames(i) = hmap_rows(i_file)%fn

        goto 112

        111 continue
        if (must_exist) then
          130 format('[',F7.1,'-',F5.1,',',F7.1,'+',F5.1,'] overlaps with [',&
           F7.1,'-',F5.1,',',F7.1,'+',F5.1,'], but cannot open file "',A,'"')
          write(lll,130) clam, H_LINE_WIDTH, clam, H_LINE_WIDTH, llzero, LAMBDA_STRETCH, &
           llfin, LAMBDA_STRETCH, fn_now
          call log_and_halt(lll)
        end if
        call log_warning('Error opening file "' // fn_now // '"')
      end if

      112 continue
    end do

    filetoh_num_files = i


    ! Note: when taking the "filetohy" from main configuration file, will not bother
    ! about hydrogen lines files not found, so bewhare (--hmap is the preferred mode anyway)
  end
end
















!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reading routines and variable declarations for *absoru2 file*

module file_absoru2
  use logging
  use dimensions
  use misc
  implicit none

  integer absoru2_nm,    & ! NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISATION
          absoru2_nmeta, & ! NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
          absoru2_numset(2) ! NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
                            ! POUR H,HE ET HE+

  character*2 :: absoru2_nomet(MAX_ABSORU2_NM) ! NOM DE L'ELEMENT; not used
  real*8 absoru2_abmet, & ! ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
                          ! (read from file, then multiplied by modele%asalog)
         absoru2_abhel    ! ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
                          ! (read from file but overwritten by modele%nhe in synthesis_())
  character*68 absoru2_titre ! ?doc?

  ! Two possibilities
  !   - IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
  !   - IUNITE=' NOYAU H' SI ON VEUT CALCULER KAPPA PAR NOYAU D'HYDROGENE
  character*8 absoru2_iunite

  integer, dimension(MAX_ABSORU2_NM) :: &
   absoru2_nr ! NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
  real*8, dimension(MAX_ABSORU2_NM) :: &
   absoru2_zp, & ! ZP=NBR. D'ABONDANCE DE L'ELEMENT
   absoru2_zm    ! ZM=POIDS MOLECULAIRE DE L'ELEMENT
  real*8, dimension(MAX_ABSORU2_NM, MAX_ABSORU2_NRR) :: &
    absoru2_xi,&! XI(J,I) = POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATION
    absoru2_pf  ! PF(J,I) = FONCTION DE PARTITION DE L'ELEMENT J AU STADE D'IONISATION
  real*8 absoru2_wi(MAX_ABSORU2_NUMSET_I, 2) ! ?doc?

contains
  !=======================================================================================
  ! Reads file *absoru2 file* to fill variables absoru2_*
  !
  ! *Note* Variables absoru2_ZP, absoru2_XI, and absoru2_PF
  !       undergo transformation after their values are read from file.
  !
  ! *Note* Historically, this routine is in essence the old routine called "LECTUR"
  !
  ! CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
  ! PUIS LES DONNEES CORRESPONDANTS AUX ABSORBANTS METALLIQUES SI NECE

  subroutine read_absoru2(path_to_file)
    implicit none
    integer myunit
    character(len=*) :: path_to_file
    character*3 neant
    integer nion, i, ith, j, nrr, nset

    ! this logging message is important in case of errors to know which file it was
    call log_info('read_absoru2(): reading file '''//trim(path_to_file)//'''...')

    open(newunit=myunit,file=path_to_file, status='old')

    ! ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
    ! ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
    read (myunit,'(2e15.7)') absoru2_abmet, absoru2_abhel

    ! NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISATION
    ! NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
    ! IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
    ! IUNITE=' NOYAU H'  ''    ''    ''       ''      NOYAU D'HYDROGENE
    read (myunit,'(2i2,a8,a)') absoru2_nm, absoru2_nmeta, absoru2_iunite, absoru2_titre

    ! spill check: checks if exceeds maximum number of elements allowed
    if (absoru2_nm .gt. MAX_ABSORU2_NM) then
      call log_and_halt('read_absoru2(): nm='//int2str(absoru2_nm)//&
         ' exceeded maximum of MAX_ABSORU2_NM='//int2str(MAX_ABSORU2_NM))
    end if


    ! LECTURE DE LA TABLE D'IONISATION CHOISIE
    do j = 1, absoru2_nm
      read (myunit, '(3x,i3,2e16.5)') absoru2_nr(j), absoru2_zp(j), absoru2_zm(j)
      absoru2_zp(j) = 10**absoru2_zp(j)

      ! NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
      ! ZP=NBR. D'ABONDANCE DE L'ELEMENT
      ! ZM=POIDS MOLECULAIRE DE L'ELEMENT
      nrr = absoru2_nr(j)

      ! checks if exceeds maximum number of elements allowed
      if (nrr .gt. MAX_ABSORU2_NRR) then
        write(lll,*) 'read_absoru2(): j = ', j, 'nr=', nrr, ' exceeded maximum of', MAX_ABSORU2_NRR
        call log_and_halt(lll)
      end if

      do i = 1, nrr
        ! neant="nothing"
        ! NION is also not used
        read (myunit, '(a3,a2,i1,2e16.5)') neant, absoru2_nomet(j), &
         nion, absoru2_xi(j,i), absoru2_pf(j,i)

        ! makes sure the atomic symbol looks OK (even thou this variable is not used so far)
        absoru2_nomet(j) = adjust_atomic_symbol(absoru2_nomet(j))

        ! ON LIT NR CARTES CONTENANT CHACUNE LE POTENTIEL D'IONISATION ET LA
        ! FONCTION DE PARTITION(LOG10(2UI+1)/UI)DE CHAQUE DEGRE D'IONISATION
        ! CES VALEURS SONT LUES DANS L'ORDRE CROISSANT DU DEGRE D'IONISATION
        ! NOMET  =NOM DE L'ELEMENT
        ! NION   =SON ETAT D'IONISATION
        ! XI(J,I)=POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATIO
        ! PF(J,I)=FONCTION DE PARTITION         ''   ''     ''      ''   ''

        absoru2_xi(j, i) = absoru2_xi(j, i)*2.302585
        absoru2_pf(j, i) = absoru2_pf(j, i)*2.302585
      end do
    END DO


    ! ASK BLB (actually TELL BLB) Was not reading the "listed des discontinuites H e HE pour TH>0.8 !!
    ! this is wrong, see below read (myunit, '(2i2)') (absoru2_numset(ith), ith=1,2)

    ! spill check: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(1) .gt. MAX_ABSORU2_NUMSET_I) then
      call log_and_halt('read_absoru2(): numset(1) = '//int2str(absoru2_numset(1))//&
       ' exceeded maximum of MAX_ABSORU2_NUMSET_I='//int2str(MAX_ABSORU2_NUMSET_I))
    end if
    ! spill check: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(2) .gt. MAX_ABSORU2_NUMSET_I) then
      call log_and_halt('read_absoru2(): numset(2) = '//int2str(absoru2_numset(2))//&
       ' exceeded maximum of MAX_ABSORU2_NUMSET_I='//int2str(MAX_ABSORU2_NUMSET_I))
    end if


    ! NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
    ! POUR H,HE ET HE+
    ! PREMIERE LISTE POUR TH.LE.0.8 ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
    do ith = 1,2
      read(myunit, *) absoru2_numset(ith)
      nset = absoru2_numset(ith)
      read (myunit,'(8f10.1)') (absoru2_wi(i,ith),i=1,nset)
    end do
  end
end







!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reading routines and variable declarations for *partit file*

module file_partit
  use logging
  use misc
  use dimensions
  implicit none

  character*2, dimension(MAX_PARTIT_NPAR) :: &
   partit_el ! Element symbol. Must be right-aligned and uppercase.
  integer partit_npar ! ?doc?

  real*8, dimension (MAX_PARTIT_NPAR) :: &
   partit_ki1,  & ! ?doc?
   partit_pa,   & ! ?doc?
   partit_m,    & ! ?doc?
   partit_tini, & ! ?doc?
   partit_ki2
  integer, dimension (MAX_PARTIT_NPAR) :: &
   partit_jkmax ! ?doc?

  real*8, dimension (MAX_PARTIT_NPAR, 3, MAX_PARTIT_KMAX) :: &
   partit_tabu ! ?doc?

contains
  !=======================================================================================
  ! Reads file *partit file* to fill variables partit_*
  !
  ! LECTURE DES FCTS DE PARTITION
  !
  ! Rows in this file alternate between:
  !
  ! 1) 8-column row
  !    col 8 -- signals end-of-file. If 1, it ignores the row and
  !             stops reading
  !
  ! 2) Series of rows to fill in partit_TABU(J, :, :)
  !

  subroutine read_partit(path_to_file)
    implicit none
    integer myunit
    character(len=*) :: path_to_file

    integer finpar, j, kmax, l, k

    open(newunit=myunit,file=path_to_file, status='old')


    j = 1
    finpar = 0
    do while (finpar .lt. 1)
      read (myunit, '(a2, 2f5.2, i3, 3f10.2, 34x, i1)') &
       partit_el(j), &
       partit_tini(j), &
       partit_pa(j), &
       partit_jkmax(j), &
       partit_m(j), &
       partit_ki1(j), &
       partit_ki2(j), finpar

      if (finpar .ne. 1) then

        ! spill check: checks if exceeds maximum number of elements allowed
        if (j .gt. MAX_PARTIT_NPAR) then
          call log_and_halt('read_partit(): par exceeded maximum of MAX_PARTIT_NPAR='//&
           int2str(MAX_PARTIT_NPAR))
        end if

        partit_el(j) = adjust_atomic_symbol(partit_el(j))

        kmax = partit_jkmax(j)

        ! spill check: checks if exceeds maximum number of elements allowed
        if (kmax .gt. MAX_PARTIT_KMAX) then
          call log_and_halt('read_partit(): par number '//int2str(j)//'; kmax='//&
           int2str(kmax)//' exceeded maximum of MAX_PARTIT_KMAX='//int2str(MAX_PARTIT_KMAX))
        end if


        read(myunit, '(13f6.4)') ((partit_tabu(j, l, k), l=1, 3), k=1, kmax)

        j = j+1
      end if
    end do

    partit_npar = j-1

    return
  end
end












!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reader and variable declarations for *atoms file*

module file_atoms
  use dimensions
  use logging
  use file_abonds
  implicit none

  !=====
  ! Variables filled by read_atoms() (file *atoms file*)
  !=====
  ! *atoms file*, file originals
  integer atoms_nblend ! ?doc?
  ! atomic symbol. Must be right-aligned and uppercase
  character*2 atoms_elem(MAX_ATOMS_NBLEND)
  integer, dimension(MAX_ATOMS_NBLEND) :: &
   atoms_ioni ! ionization level, e.g. 2 means 1 electron missing
  real*8, dimension(MAX_ATOMS_NBLEND) :: &
   atoms_lambda,       & ! wavelength
   atoms_kiex,         & ! excitation potential for the lower level
   atoms_algf,         & ! log gf -- oscillator strength -- laboratory-adjusted
   atoms_ch,           & ! the "C6" parameter, Van der Walls broadening parameter
   atoms_gr,           & ! ?doc?
   atoms_ge,           & ! ?doc?
   atoms_zinf,         & ! ?doc?
   atoms_abondr_dummy, & ! ?doc?
   atoms_abonds_abo  ! will be filled by "inner join" by searching
                          ! atoms_elem through abonds_ele

contains
  !=======================================================================================
  ! Reads file *atoms file* to fill variables atoms_* (double underscore)
  !
  ! Depends on abonds_*, so must be called after READ_ABONDS()
  !
  ! This file has 2 types of alternating rows:
  !
  !   odd row
  !     col 1 -- 2-letter atoms_elem atomic symbol
  !     col 2 -- atoms_ioni
  !     col 3 -- atoms_lambda
  !   even row
  !     col 1 --
  !     col 2 --
  !     col 3 --
  !     col 4 --
  !     col 5 --
  !     col 6 --
  !     col 7 --
  !     col 8 -- signals end-of-file. If "1", reading stops
  !
  !
  !  *Note* The line that has the end-of-file flag set is also taken into account.

  subroutine read_atoms(filename)
    implicit none
    integer myunit
    character(len=*) :: filename
    integer finrai, k, j
    logical flag_found

    if (.not. flag_read_abonds) then
      call log_and_halt('read_abonds() must be called before read_atoms()')
    end if

    open(newunit=myunit,file=filename, status='old')

    k = 1
    do while (.true.)
      ! spill check: checks if exceeds maximum number of elements allowed
      if (k .gt. MAX_ATOMS_NBLEND) then
        call log_and_halt('read_atoms(): exceeded maximum of MAX_ATOMS_NBLEND='//&
         int2str(MAX_ATOMS_NBLEND)//' spectral lines')
      end if
      read(myunit, '(a2, i1, 1x, f10.3)') atoms_elem(k), &
                                         atoms_ioni(k), &
                                         atoms_lambda(k)
      if (atoms_ioni(k) .ne. 1 .and. atoms_ioni(k) .ne. 2) then
        ! Only ionization levels 1 and 2 are accepted because subroutine popadelh() only considers these levels

        call log_and_halt('read_atoms(): error in line '//int2str(k*2-1)//' of file '//trim(filename)//&
         ': invalid ionization level: '//int2str(atoms_ioni(k)))
      end if
      atoms_elem(k) = adjust_atomic_symbol(atoms_elem(k))

      read(myunit, *) &
       atoms_kiex(k), &
       atoms_algf(k), &
       atoms_ch(k), &
       atoms_gr(k), &
       atoms_ge(k), &
       atoms_zinf(k), &
       atoms_abondr_dummy(k), finrai

      ! (MT) If the "radiative broadening" is zero,
      ! it is calculated as a function of lambda; otherwise, it is assumed that it has been inputted manually.
      ! ?doc? What does 2.21e15 stand for? + reference
      if (atoms_gr(k) .lt. 1e-37) atoms_gr(k) = 2.21e15 / atoms_lambda(k)**2

      ! Besides reading the file, this routine searches atoms's element within abonds'
      ! elements and copies corresponding
      ! abonds_abo value into atoms_abonds_abo. Halts program if element not found.
      !
      ! In database terminology, this is sort of a "inner join".

      flag_found = .false.
      do  j = 1, abonds_nabond
        if (abonds_ele(j) .eq. atoms_elem(k)) then
          flag_found = .true.
          exit
        end if
      end do
      if (.not. flag_found) then
        write(lll,*)  'read_atoms(): element "', atoms_elem(k), &
         ' (spectral line number ', k, ') cannot be found in abundance file'
        call log_and_halt(lll)
      end if
      atoms_abonds_abo(k) = abonds_abo(j)

      if (finrai .eq. 1) exit !__end-of-file__

      k = k+1
    end do

    atoms_nblend = k

    write(lll,*) 'read_atoms(): last line taken: element: "', atoms_elem(atoms_nblend), &
      '"; lambda: ', atoms_lambda(atoms_nblend)
    call log_debug(lll)


! ISSUE
! MENTION: last atomic line wasn't being used! (this has been fixed/changed). Original code commented in subroutine
!~  K=1
!~9 READ(14,103)ELEM(K),IONI(K),LAMBDA(K)
!~  READ(14,*) KIEX(K),ALGF(K),CH(K),GR(K),GE(K),ZINF(K),
!~  1 ABONDR(K),FINRAI
!~  write(34,103)ELEM(K),IONI(K),LAMBDA(K)
!~  GF(K)=10.**ALGF(K)
!~C        IF(K.EQ.1) GF(K)=10**AGGF
!~  IF(GR(K).LT.1E-37)   GR(K)=2.21E15 / LAMBDA(K)**2
!~  IF(FINRAI.EQ.1) GO TO 10
!~  IF(((LAMBDA(K).GT.LFIN).OR.(LAMBDA(K).LT.LZERO))) GO TO 205
!~  K=K+1
!~205 CONTINUE
!~  GO TO 9
!~10  NBLEND=K-1

    close(unit=myunit)
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Reading routines and variable declarations for *molecules file*
!

module file_molecules
  use logging
  use dimensions
  use misc
  use file_dissoc
  implicit none

  ! Number of different "chemical molecules" that the system supports. Differs from number of
  ! molecules listes in *molecules file* because in the latter the same molecule is
  ! repeated, such as "CN AZUL, CN AX, CN BX", but in this case the formula is always CN
  !
  ! **Note** this is provided for backwards compatibility and used only if the separate symbols
  !          are not found in the second section of km_titulo
  integer, parameter :: NUM_FORMULAE = 10
  ! Molecule formulae
  character*6, parameter :: FORMULAE(NUM_FORMULAE) = &
   (/'MgH   ', &
     'C2    ', &
     'CN    ', &
     'CH    ', &
     '13CH  ', &
     '12C16O', &
     'NH    ', &
     'OH    ', &
     'FeH   ', &
     'TiO   '/)
  ! Corresponding isotope names. **Note** that 13C is referred to as 'A' in abonds.dat
  ! and dissoc.dat
  character*2, parameter :: FORMULA_SYMBOLS(2, NUM_FORMULAE) = reshape((/&
    'MG', ' H', ' C', ' C', ' C', ' N', ' C', ' H', ' A', ' H', ' C', ' O', &
    ' N', ' H', ' O', ' H', 'FE', ' H', 'TI', ' O'/), (/2, NUM_FORMULAE/))



  ! Specifies how many molecules to read
  integer km_number

  integer km_lines_total  ! Total number of spectral line, counting all molecules

  character*160 km_titm
  integer, parameter :: SIZE_TITULO=4096
  character(SIZE_TITULO) :: km_titulo(MAX_NUM_MOL), &
   km_comments(MAX_NUM_MOL)  ! Stores only first section in km_titulo

  ! 20160920
  ! (v_sup, v_inf) list for each molecule
  integer, dimension(2, MAX_KM_NV_PER_MOL, MAX_NUM_MOL) :: km_transitions
  logical :: km_has_transitions(MAX_NUM_MOL)
  ! 20160920
  ! atomic symbols for each molecule (all are diatomic)
  character*2 :: km_symbols(2, MAX_NUM_MOL)
  integer :: km_isotopes(2, MAX_NUM_MOL)

  real*8, dimension(MAX_NUM_MOL) :: km_fe, km_do, &
   km_mm, km_am, km_bm, km_ua, km_ub, km_te, km_cro, &
   km_a0, km_a1, km_a2, km_a3, km_a4, km_als, km_s

  integer, dimension(MAX_NUM_MOL)  :: km_ise, km_nv, &
   km_lines_per_mol  ! This stores the number of spectral lines for each molecule

  real*8, dimension(MAX_KM_NV_PER_MOL, MAX_NUM_MOL) :: km_qqv, km_ggv, km_bbv, km_ddv, km_fact

  ! "Index of Last Lambda Of Set-Of-Lines"
  ! Points to km_lmbdam, km_sj, km_jj.
  ! This is mounted at reading to help with the filtering and avoid
  ! allocating 2 dimensions for (lm__lmbdam, km_sj, km_jj)
  real*8, dimension(MAX_KM_NV_PER_MOL, MAX_NUM_MOL) :: km_ln

  real*8,  dimension(MAX_KM_LINES_TOTAL) :: &
   km_lmbdam, &
   km_sj,     &
   km_jj

  private split_titulo
contains
  !=======================================================================================
  ! Reads file *molecules file* to fill variables km_*
  !
  ! Reads molecular lines
  !
  ! Note: The whole file is read into memory, independent of which molecules are "switched on".
  !       There is not much of a point in skipping molecules here, because the space is already
  !       pre-allocated, and I have to read the whole file anyway, so it is much easier
  !       programming-wise (and not time-costly either) to filter
  !       molecules in filter_molecules() when they are already in memory.

  subroutine read_molecules(filename)
    character(len=*) :: filename
    integer myunit, i, &
     molidx,   &  ! Old "NMOL", index/ID of molecule, ranges from 1 to MAX_NUM_MOL
     i_line,  &  ! Counts lines within each molecule (reset at each new molecule)
     nnv, iz, &
     numlin , &  ! Temporary variable
     j_set,   &
     j_line, &
     nv_in_titulo, &
     temp
    character(len=SIZE_TITULO) :: sections(3)
    ! integer :: sizes(3)


    ! # Initialization
    !
    km_has_transitions = .false.
    km_titulo = ' '

    ! # Reading
    !

    open(newunit=myunit,file=filename, status='old')

    ! row 01:
    ! BLB: NUMBER -- number of molecules do be considered
    ! Note: This is no longer used for anything, now the molecules to be switched on/off are configured

    read(myunit,*) km_number

    ! spill check
    if (km_number .gt. MAX_NUM_MOL) then
      call log_and_halt("Number of molecules ("//int2str(km_number)// &
       ") exceeds maximum allowed ("//int2str(MAX_NUM_MOL)//")")
    end if


    ! row 02: string containing list of names of all molecules
    read(myunit,'(a)') km_titm
    !~READ(myunit,'(20A4)') km_TITM

    !write(lll, *) 'titm--------------', km_titm
    !call log_debug(lll)

    ! BLB:
    ! BLB: km_NV -- number of transitions (v', v'') for each molecule
    ! BLB: Example: if (0,0)(1,1)(2,2) are considered for CH
    ! BLB:             (1,1)(2,2) are considered for CN
    ! BLB:             NV(J) = 3 2
    read(myunit,*) (km_nv(molidx), molidx=1,km_number)

    ! spill check
    do molidx = 1, km_number
      if (km_nv(molidx) .gt. MAX_KM_NV_PER_MOL) then
          call log_and_halt('read_molecules(): molecule id '//int2str(molidx)//&
           ' has nv = '//int2str(km_nv(molidx))//' (maximum is MAX_KM_NV_PER_MOL='//&
           int2str(MAX_KM_NV_PER_MOL)//')')
        end if
    end do

    i_line = 0
    do molidx = 1, km_number
      ! 20160920
      ! 'titulo' was historically only a string, but now was made *big* and has a
      ! structure like this:
      !
      !   'comments # atomic symbols # transitions', where
      !
      !   - the '#' character: separator between the following three sections:
      !   - comments: anything
      !   - atomic symbols: separated by space; example: 'MG H'
      !   - transitions: pairs of integer numbers separated by any character
      !
      ! **Note 1** [for backward compatibility, ] if atomic symbols not specified,
      !            will try to find molecule formula in the comments. In this case,
      !            the formula must match one in the constant FORMULAE
      !
      ! **Note 2** 'titulo' was the only place left in this file specification to add
      !            this extra information
      !
      read(myunit,'(a)') km_titulo(molidx)

      call split_titulo(km_titulo(molidx), sections)  ! , sizes)

      km_comments(molidx) = sections(1)

      if (len(trim(sections(2))) .eq. 0) then
        ! If atomic symbols not present in section 2, will try to find the formula in the comments
        call log_info('Looking for formula in comments '''//trim(sections(1))//'''...')

        call assign_symbols_by_formula(sections(1), km_symbols(:, molidx))

        !  call log_and_halt('Atomic symbols not available for molecule #'//int2str(molidx))
      else
        call parse_symbols(sections(2), km_symbols(:, molidx))
        ! To test if elements are valid
        if (flag_read_dissoc) then
          temp = find_atomic_symbol_dissoc(km_symbols(1, molidx))
          temp = find_atomic_symbol_dissoc(km_symbols(2, molidx))
        end if
      end if


      if (len(trim(sections(3))) .gt. 0) then
        ! print *, 'SECTIONS(3) has len', len(trim(sections(3)))
        ! print *, '$$$', trim(sections(3)), '$$$'

        ! if it has the transitional information it must be correct
        call parse_transitions(sections(3), km_transitions(:,:,molidx), nv_in_titulo, &
         km_titulo(molidx))

        if (nv_in_titulo .ne. km_nv(molidx)) then
          call log_and_halt('read_molecules(): Wrong (vsup, vinf) information in '''//trim(sections(3))//''''//&
           int2str(molidx)//': number of transitions specified should be '//&
           int2str(km_nv(molidx))//' but was '//int2str(nv_in_titulo))
        end if

        km_has_transitions(molidx) = .true.
      end if


      !write(lll,*) 'molecule index ', molidx
      !call log_debug(lll)
      !write(lll,*) 'titulo:  ', km_titulo(molidx)
      !call log_debug(lll)

      ! BLB: FE, DO, MM, AM, BM, UA, UB, Te, CRO
      ! BLB: Format: free
      ! BLB: FE -- molecular oscillator strength fel
      ! BLB: DO -- dissociation constant (eV)
      ! BLB:    | MM -- [mass of A + mass of B] for molecule AB
      ! BLB:  +-| AM -- mass of A
      ! BLB:  | | BM -- mass of B
      ! BLB:  |
      ! BLB:  +---> where (12)C = 12, H = 1.008
      ! BLB:
      ! BLB: UA -- value of partition function for element A
      ! BLB: UB -- value of partition function for element B
      ! BLB: TE -- eletronic term.
      ! BLB: CRO - delta Kronecker (2-delta_{Sigma, 0})
      ! BLB:       delta_{Sigma, 0} = 0 for Sigma transitions
      ! BLB:                          1 for non-Sigma transitions

      read(myunit,*) km_fe(molidx), km_do(molidx), km_mm(molidx), &
       km_am(molidx), km_bm(molidx), km_ua(molidx), &
       km_ub(molidx), km_te(molidx), km_cro(molidx)


      ! *Note * there is only a blank row at this position
      ! and these variables are not in use in the program.
      read(myunit,'(2x,i3, 5f10.6, 10x, f6.3)') km_ise(molidx), &
       km_a0(molidx), km_a1(molidx), km_a2(molidx), &
       km_a3(molidx), km_a4(molidx), km_als(molidx)

      ! ?doc?
      read(myunit,*) km_s(molidx)

      nnv = km_nv(molidx)

      !#logging
      !write(lll,*) 'nv=', nnv
      !call log_debug(lll)

      ! BLB: Values, one for each set-of-lines (transition) (v',v")
      ! BLB: qqv  -- Franck-Condon factors
      ! BLB: ggv  -- rotational terms
      ! BLB:          G_A(v) = ??? (ininteligible formula)
      ! BLB: bbv  -- rotational constants B_v for different v"
      ! BLB:          B_v = B_e - alpha_e(v+1/2)
      ! BLB: ddv  -- rotational constants D_v for different v"
      ! BLB:         D_v = D_e + beta_e(v+1/2)
      ! BLB: fact -- factor of adjustment for fitting bottom of lines
      ! BLB:         In general: 1. 1. 1. 1. ...
      ! BLB:         Sometimes used for isotopic determinations
      read(myunit,*) (km_qqv(i, molidx), i=1,nnv)
      read(myunit,*) (km_ggv(i, molidx), i=1,nnv)
      read(myunit,*) (km_bbv(i, molidx), i=1,nnv)
      read(myunit,*) (km_ddv(i, molidx), i=1,nnv)
      read(myunit,*) (km_fact(i, molidx),i=1,nnv)

      do i = 1,nnv
        km_ddv(i, molidx)=1.e-6*km_ddv(i, molidx)
      end do


      !~L = (MOLID-1)*MAX_LINES_PER_MOL+1  ! Initial index for all vectors inside this loop
      j_set = 0
      j_line = 0 ! Counts how many lines per molecule and stores.
      do while (.true.)
        i_line = i_line+1

        ! spill check: checks if exceeds maximum number of elements allowed
        if (i_line .gt. MAX_KM_LINES_TOTAL) then
          call log_and_halt('read_molecules(): exceeded maximum number of total '//&
            'spectral lines  MAX_KM_LINES_TOTAL= '//int2str(MAX_KM_LINES_TOTAL)//&
            ' (at molecule id '//int2str(molidx)//')')
        end if

        ! BLB: LMBDAM(L), SJ(L), JJ(L), IZ, ITRANS(L), NUMLIN
        ! BLB:
        ! BLB: LMBDAM -- wavelength in angstron
        ! BLB: Sj -- Honl-London factor calculated such that sum(S_j/(2*j+1)) = 1
        ! BLB: JJ -- rotational quantum number
        ! BLB: IZ -- branch as table:  *not used*
        ! BLB:       P  -  1
        ! BLB:       Q  -  2
        ! BLB:       R  -  3
        ! BLB:       P1 -  4
        ! BLB:       Q1 -  5
        ! BLB:       R1 -  6
        ! BLB:       P2 -  7
        ! BLB:       Q2 -  8
        ! BLB:       R2 -  9
        ! BLB:       P3 - 10
        ! BLB:       Q3 - 11
        ! BLB:       R3 - 12
        ! BLB: ITRANS -- key to indicate which is the (v',v'') -- only used in isotropic calculations
        !                (JT) I think this column has been discarded because it does not exist in molecules.dat
        ! BLB: NUMLIN -- key as table:
        ! BLB:           = 1 for the last line of a given (v',v'') set of lines of a given molecule
        ! BLB:           = 9 for the last line of the last (v', v'') set of lines of a given molecule
        read(myunit,*) km_lmbdam(i_line), km_sj(i_line), km_jj(i_line), iz, numlin


        !~km_NUMLIN(J_LAMBDA, MOLID) = NUMLIN

        if (numlin .ne. 0) then
          j_set = j_set+1
          km_ln(j_set, molidx) = i_line
        end if

        j_line = j_line+1

        if (numlin .eq. 9) exit
      end do

      ! consistency check: J_SET must match NNV
      if(j_set .ne. nnv) then
        call log_and_halt('read_molecules():  incorrect number of set-of-lines: '//&
         int2str(j_set)//' (should be '//int2str(nnv)//') (in molecule number '//&
         int2str(molidx)//')')
      end if

      km_lines_per_mol(molidx) = j_line

      !#logging
      write(lll,*) 'This molecule has ', j_line, ' lines'
      call log_debug(lll)
    end do

    km_lines_total = i_line

    close(myunit)
  end


  ! Splits titulo in three parts; no space trimming

  subroutine split_titulo(titulo, sections)  !, sizes)
    character(len=SIZE_TITULO), intent(in) :: titulo
    ! each section will be contained
    character(len=SIZE_TITULO), intent(out) :: sections(3)
    ! ! Number of characters per section
    ! integer, intent(out) :: sizes(3)
    integer i_pos, start, n
    character, parameter :: DELI='#'

    sections = ' '  ! very important
    start = 1
    ! sizes = 0
    n = 0
    ! print *, 'SPLIT_TITULO():'
    ! print *, trim(titulo)
    ! print *, '-------------'

    do i_pos = 1, SIZE_TITULO+1
      if (titulo(i_pos:i_pos) .eq. '#' .or. i_pos .eq. SIZE_TITULO+1) then
        n = n+1
        if (n .gt. 3) then
          call log_and_halt('split_titulo(): number of sections in separated by ''#'' in '''//&
           trim(titulo)//''' > 3')
        end if

        sections(n) = titulo(start:(i_pos-1))

        ! print *, 'section', n, trim(sections(n))

        start = i_pos+1

      end if
    end do
  end


  ! Assigns separate atomic symbols given the formula of the molecule
  !
  ! **Example**, if str containg "MGH", symbols will be (/'MG', ' H'/)
  !
  ! **Note** str is case-insensitive
  !

  subroutine assign_symbols_by_formula(str, symbols)
    ! String; comments section of km_titulo
    character(len=*), intent(in) :: str
    ! 2-element character array of two characters each
    character(len=2), intent(out) :: symbols(2)

    character(len=:), allocatable :: formula, str_upper
    integer :: idx_found(NUM_FORMULAE), num_found, i, n, idx
    character(len=7*NUM_FORMULAE) :: s_matches, s_matches2  ! logging buffer
    integer :: i_ch

    str_upper = to_upper(trim(adjustl(str)))

    num_found = 0
    do i = 1, NUM_FORMULAE
      formula = to_upper(trim(adjustl(FORMULAE(i))))

      idx = index(str_upper, formula)
      if (idx .gt. 0) then
        if (idx .eq. 1) go to 1  ! found in beginning of str
        i_ch = ichar(str_upper(idx-1:idx-1))
        ! compares with space or tab
        if (i_ch .eq. 32 .or. i_ch .eq. 9) goto 1  ! found in beginning of word
        goto 2  ! found in middle of word, not condidered

        1 continue
        num_found = num_found+1
        idx_found(num_found) = i

        2 continue
      end if
    end do

    if (num_found .eq. 0) then
      call log_and_halt('No valid molecule formula was found in "'//str_upper//'"')
    elseif (num_found .gt. 1) then
      ! joins molecule names separated by comma
      ! Fortran formatting does most of the job, ...
      write(s_matches, '('//int2str(num_found)//'(A,2H, ))') &
       (to_upper(trim(adjustl(FORMULAE(idx_found(i))))), i=1, num_found)
      ! ... but there is one extra comma that needs to be removed
      n = len(trim(s_matches))
      s_matches2 = s_matches(1:n-1)

      call log_and_halt('Ambiguity in "'//str_upper//'": matches: '//s_matches2)
    end if

    symbols = FORMULA_SYMBOLS(:, idx_found(1))
  end
end




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

!
! Subroutine absoru_() and related sub-subroutines
!
!   PARTH : NBRE TOTAL DE NOYAUX PAR CM**3
!   PG    : PRESSION TOTALE EN DYNES/CM**2
!   ZMU   : POIDS MOLECULAIRE MOYEN
!   RHO   : DENSITE (G-CM-3)
!   TOC   : NOMBRE DE NOYAUX D'HYDROGENE PAR CM**3
!   AC    : DEGRE D'IONISATION MOYEN
!   AC1(1): ''        ''     DE H
!   AC1(2): ''        ''     DE HE+
!   AC1(3): ''        ''     DE HE
!   AC2   : ''        ''     DES METAUX
!   PHI(J): ''        ''     DE L ELEMENT J POUR MULTIPLE IONISATION
!   ZNH(M): POPULATION POUR CHAQUE ABSORBANT M (H,HE OU METAUX)
!
! DONNEES POUR H2+ =TABLE DE BATES(1952) COMPLETEE PAR HARVARD(1964)
! ------------------
! AU_WINV=NU/C,AU_GRDM=(-NU/DNU/DR)*R*D(R),U1=-U(1S SIGMA/R),AU_U2=U(+2P SIGM
! DONNEES POUR L'HELIUM NEUTRE
! ----------------------------
! AU_ZLHEM(K)ET AU_STWTM(K)=LAMBDAS DES DISCONTINUITES  ET VALEUR DU POIDS
! STATISTIQUE CORRESPONDANT
! AU_ZLHE(L) ET AU_ZEFF4(L)=SUITE DES DISCONTINUITEES ET SECTIONS EFFICACE
! CORRESPONDANTES
! DONNEES POUR L'HYDROGENE GRANT=MON. NOT. VOL. 118 P. 241 1958
! ------------------------
! au_G3D(I,J) =FACTEUR DE GAUNT EN FONCTION DE au_RHO ET DE LOG(-ETA)
! au_RHO(I)  =VALEUR DE RHO CORRESPONDANTES
! AU_ZLETAG(J)=VALEUR DE LOG(-ETA)
! AU_YY=ZEROS DU POLYNOME DE LAGUERRE=CHANDRASEKHAR RADIATIVE TRANSFER
! AU_AA=NBR. DE CHRISTOFFEL CORRESPONDANT
! DONNEES POUR QUASI MOLECULE H+H (DOYLE,APJ,153,187.1968)
!
!
! **References**
! DONNEES POUR L'HYDROGENE GRANT=MON. NOT. VOL. 118 P. 241 1958
! DONNEES POUR QUASI MOLECULE H+H (DOYLE,APJ,153,187.1968)
!
!
! References:
!   Vardya, M.S., Astrophysics Journal, Vol.133, pp.107-129, 1961
!
! Prefixes:
!   - absoru_ -- public variables used in other modules
!   - au_, at_ -- private variables shared among routines
module absoru
  use file_absoru2
  implicit none

  private  ! This statement makes all symbols private by default

  ! Public subroutine
  public absoru_

  real*8, parameter :: AU_WINV(46) = (/                                   &
   361.9,429.9,514.4,615.3,733.7,869.7,1028.7,1226.2,1460.9,              &
   1737.3,2044.4,2407.4,2851.6,3378.1,3986.8,4677.7,5477.3,6442.5,7574.3, &
   8872.9,10338.2,12064.6,14049.7,16352.9,18996.2,22056.2,25576.8,        &
   29634.9,34307.2,39703.3,45943.9,53171.7,61529.1,71213.6,82433.7,       &
   95441.4,110445.3,127774.4,147406.7,169671.2,194568.0,221877.7,         &
   251600.4,282968.2,312800.5,329032.7/)

  real*8, parameter :: AU_GRDM(46) = (/&
   1729.881 ,1591.634 ,1450.598 ,1317.928 ,1198.805 ,1091.634,            &
   994.4223,896.8127,805.5777,722.7092,649.4024,583.2669,517.9283,        &
   457.7689,405.1793,358.9641,316.3347,275.7371,238.9641,206.6136,        &
   180.4781,153.5857,130.4781,109.6813,91.9920,76.1753,62.7092,50.9960,   &
   40.9562,32.5498,25.4263,19.6693,14.9920,11.2470,8.2869,5.9960,4.2311,  &
   2.8900,1.9020,1.1733,0.6781,0.3544,0.1605,0.0602,0.0171,0.0000/)

  real*8, parameter :: AU_U1(46) = (/&
   0.00263,0.00286,0.00322,0.00372,0.00435,0.00511,0.00600,0.00701,       &
   0.00821,0.00958,0.01190,0.01305,0.01522,0.01772,0.02061,0.02394,       &
   0.02775,0.03207,0.03699,0.04257,0.04884,0.05584,0.06367,0.07229,       &
   0.08180,0.09216,0.10340,0.11542,0.12815,0.14147,0.15511,0.16871,       &
   0.18167,0.19309,0.20167,0.20525,0.20052,0.18186,0.13996,0.05794,       &
   -0.09644,-0.39105,-0.99032,-2.39840,-7.14260,-85.0/) ! ?doc?

  real*8, parameter :: AU_U2(46) = (/&
   0.00114,0.00124,0.00145,0.00178,0.00221,0.00277,0.00342,0.00416,       &
   0.00508,0.00615,0.00745,0.00899,0.01083,0.01302,0.01561,0.01869,       &
   0.02237,0.02676,0.03195,0.03810,0.04540,0.05412,0.06445,0.07676,       &
   0.09140,0.10889,0.12977,0.15473,0.18466,0.22057,0.26382,0.31606,       &
   0.37932,0.45618,0.54997,0.66493,0.80665,0.98279,1.20442,1.48940,       &
   1.87040,2.41450,3.28470,4.97840,9.99460,85.0/) ! ?doc?

  real*8, parameter :: AU_ZLHEM(5) = (/504.3,2601.0,3122.0,3422.0,3680.0/) ! ?doc?

  real*8, parameter :: AU_STWTM(5) = (/1.0,3.0,1.0,9.0,3.0/) ! ?doc?

  real*8, parameter :: AU_ZLHE(10) = (/&
   0.0,0.0,7932.0,14380.0,22535.0,32513.0,44313.0,57936.0,73383.0,90603.0/) ! ?doc?

  real*8, parameter :: AU_ZEFF4(10) = (/&
   0.0,0.0,1.069373 ,1.028328 ,1.022291 ,1.018358,1.015639,1.013614,1.012019,&
   1.011869/) ! ?doc?

  real*8, parameter :: AU_ZLH(19) = (/&
   911.8,3647.0,8205.9,14588.2,22794.1,32823.5,44676.4,58352.9,73852.8,91176.3, &
   110323.4,131293.9,154088.0,178705.6,205146.7,233411.4,263499.6,295411.3,329146.5/) ! ?doc?

  real*8, parameter :: AU_ZLHEP(19) = (/&
   227.8,911.8,2050.6,3645.6,5696.2,8202.5,11164.5,14582.3,18455.7,22784.8,27569.6, &
   32810.1,38506.3,44658.2,51265.8,58329.0,65848.0,73822.6,82253.0/) ! ?doc?


  integer, parameter :: G3D_DIM_RHO = 12, &! length in the rho direction
   G3D_DIM_ETA = 18,                      &! length in the eta direction
   G3DX = 9                                ! half the length in the eta direction

  ! equivalence with first G3DX columns of au_g3d
  real*8 :: g3d1(G3D_DIM_RHO,G3DX)
  data g3d1 &
   /2.885,2.419,2.047,1.679,1.468,1.323,1.212,1.124,1.051,0.989,       &
    0.810,0.693,2.906,2.420,2.049,1.684,1.474,1.330,1.220,1.133,1.061, &
    1.000,0.824,0.708,2.912,2.430,2.072,1.723,1.527,1.395,1.296,1.218, &
    1.155,1.102,0.951,0.856,2.892,2.423,2.082,1.760,1.583,1.469,1.385, &
    1.320,1.268,1.226,1.111,1.045,2.815,2.365,2.046,1.755,1.604,1.507, &
    1.438,1.387,1.346,1.314,1.230,1.185,2.715,2.280,1.978,1.709,1.573, &
    1.488,1.428,1.383,1.348,1.320,1.249,1.208,2.615,2.194,1.906,1.654, &
    1.530,1.452,1.398,1.357,1.326,1.303,1.237,1.202,2.231,1.868,1.629, &
    1.440,1.352,1.298,1.261,1.235,1.215,1.198,1.158,1.136,1.955,1.635, &
    1.445,1.303,1.238,1.201,1.175,1.157,1.144,1.133,1.106,1.091/

  ! equivalente with second G3DX columns of au_g3d
  real*8 :: g3d2(G3D_DIM_RHO,G3DX)
  data g3d2 &
   /1.807,1.518,1.357,1.239,1.187,1.157,1.137,1.123,1.112,1.104,1.082, &
   1.065,1.707,1.446,1.303,1.201,1.157,1.131,1.115,1.103,1.094,1.087,  &
   1.069,1.054,1.634,1.394,1.266,1.175,1.136,1.114,1.100,1.089,1.081,  &
   1.075,1.056,1.046,1.579,1.357,1.239,1.157,1.121,1.102,1.088,1.079,  &
   1.072,1.067,1.049,1.042,1.497,1.302,1.201,1.131,1.101,1.085,1.073,  &
   1.066,1.060,1.055,1.042,1.034,1.442,1.265,1.175,1.113,1.088,1.073,  &
   1.064,1.057,1.052,1.046,1.035,1.030,1.400,1.237,1.156,1.101,1.078,  &
   1.065,1.057,1.051,1.045,1.042,1.032,1.026,1.367,1.217,1.142,1.091,  &
   1.071,1.059,1.051,1.045,1.041,1.037,1.029,1.024,1.342,1.200,1.130,  &
   1.084,1.065,1.053,1.047,1.042,1.037,1.034,1.026,1.022/

  ! au_g3d(i,j) = facteur de gaunt en fonction de AU_RHOG et de log(-eta)
  real*8, dimension(G3D_DIM_RHO,G3D_DIM_ETA) :: au_g3d
  equivalence (au_g3d(1,1),g3d1(1,1)),(au_g3d(1,G3DX+1),g3d2(1,1))

  real*8, parameter :: AU_RHOG(G3D_DIM_RHO) = (/&
   1.010,1.025,1.050,1.100,1.150,1.200,1.250,1.300,1.350,1.400,1.600,1.800/) ! ?doc?

  real*8, parameter :: AU_ZLETAG(G3D_DIM_ETA) = (/&
   -3.0000,-2.0000,-1.0000,-0.6021,-0.3010,-0.1249,0.0000,0.3979,0.6990, &
   0.8751,1.0000,1.0969,1.1761,1.3010,1.3979,1.4771,1.5441,1.6021/) ! ?doc?

  real*8, parameter :: AU_YY(4) = (/0.3225,1.7458,4.5366,9.3951/) ! ?doc?
  real*8, parameter :: AU_AA(4) = (/0.6032,0.3574,0.0389,0.0005/) ! ?doc?


  !=====
  ! Variables calculated by absoru_()
  !=====
  ! POPULATION POUR CHAQUE ABSORBANT M (H,HE OU METAUX).
  ! ISSUE Why 12? (MT): It seems that it should be longer.
  real*8, public, dimension(12) :: absoru_znh
  ! Calculated by absoru_(). ?doc?
  real*8, public, dimension(2) :: absoru_totkap
  ! NOMBRE DE NOYAUX D'HYDROGENE PAR cm^3. HYDRO2 uses this.
  real*8, public :: absoru_toc

  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  integer, dimension(2) :: au_jshyd
  integer au_jh, au_jfz

  real*8 au_ahe, & ! ?doc?
   au_ah, & ! ?doc?
   au_ahep, & ! ?doc?
   au_uh1, & ! ?doc?
   au_zemh, & ! ?doc?
   au_uhep1, & ! ?doc?
   au_uhe1, & ! ?doc?
   au_zeuhe1, & ! ?doc?
   au_ul, & ! ?doc?
   au_stimu, & ! ?doc?
   au_znu1, & ! ?doc?
   au_znu2, & ! ?doc?
   au_znu3, & ! ?doc?
   au_zmuze, & ! ?doc?
   au_pe

  real*8 au_avm  ! MASSE ATOMIQUE MOYENNE DES ELEMENTS PLUS LOURDS QUE L'HELIUM
  ! real*8 au_zmu  ! POIDS MOLECULAIRE MOYEN
  real*8 au_pg   ! PRESSION TOTALE EN DYNES/cm**2
  real*8 au_rho  ! DENSITE (G-CM-3)
  real*8 au_ac   ! DEGRE D'IONISATION MOYEN

  real*8 :: au_g2d(2, 19)  ! FACTEUR DE GAUNT BOUND FREE
  real*8, dimension(5) :: au_zexpm  ! ?doc?
  real*8, dimension(10) :: au_zexp ! ?doc?
  real*8, dimension(20) :: au_zeuh, & ! ?doc?
   au_zeuhep ! ?doc?
  real*8, dimension(11) :: au_zk  ! ?doc?
  real*8 :: au_zkm(MAX_ABSORU2_NM, MAX_ABSORU2_NRR) ! ?doc?
  real*8 :: au_ac2(MAX_ABSORU2_NM, MAX_ABSORU2_NRR) ! DEGRE D'IONISATION DES METAUX

  real*8, dimension(3) :: au_ac1  ! Ionization degrees of H, He+ and He
                                  !   - au_AC1(1): DEGRE D'IONIZATION DE H
                                  !   - au_AC1(2): DEGRE D'IONIZATION DE HE+
                                  !   - au_AC1(3): DEGRE D'IONIZATION DE HE

  real*8, dimension(MAX_ABSORU2_NM) :: au_znu  ! ?doc?


  real*8 :: at_zzk(11,2) ! Calculated by athyhe()

contains

  !-------------------------------------------------------------------------------
  ! Calculates the "continuum absorption"
  !
  ! Routine name has trailing underscore to differentiate from module name.
  !
  ! Note: (BLB) 1/3 things that need to be changed to include scattering (other software
  !       e.g. Upsalla already have this)
  !
  ! Note: (BLB) 1/3 atmospheric models 50e6 cannot be calculated, would tyake months.
  !        So, one idea is to include opacity model tables (Upsalla; MARCS model).
  !
  ! A.M.Colle

  subroutine absoru_(wl,th,zlpe,callam,calth,calpe,calmet,calu,flag_hydro2)
    real*8, intent(in) :: &
     wl,       & ! wavelength
     th,       & ! teta = 5040./teff
     zlpe        ! log of electron pressure (log(pe), pe given in dyn/cm^2)
    integer, intent(in) :: &
     callam, & ! flag ?doc?
     calth,  & ! flag ?doc?
     calpe,  & ! flag ?doc?
     calmet, & ! innefective flag
     calu      ! ?doc?
    ! Affects value of MAX_WLH_I2: 1200 or 14110
    logical, intent(in) :: flag_hydro2

    real*8 zzkk(11,2), dif(2,3),scath(2),at_zzk(11,2),scat(2), &
     scatel, sum1, sum2, unit, wl4, wlh

    integer i, ilt, ith, m, min, mm, mmm, nset, kkk

    data dif /5.799e-13,8.14e-13,1.422e-6,1.28e-6,2.784,1.61/

    !call log_debug(ENTERING//'absoru_()')

    ilt = callam
    if (calpe .eq. 2) go to 9003

    ! SUM1: SOMME DES ABONDANCES DES METAUX
    sum1 = 0.0
    sum2 = 0.0
    do i = 1,absoru2_nm
      sum1 = sum1+absoru2_zp(i)
      sum2 = sum2+absoru2_zp(i)*absoru2_zm(i)
    end do
    au_avm = sum2/sum1

    ! au_ZNU1,au_ZNU2,au_ZNU3: SUCCESSIVEMENT FRACTION D'(H,HE,METAUX) PAR NOMBRE T

    do i = 1,absoru2_nm
      au_znu(i) = absoru2_zp(i)/sum1
    end do

    au_znu1 = 1.0/(1.0+absoru2_abmet+absoru2_abhel)
    au_znu2 = au_znu1*absoru2_abhel
    au_znu3 = au_znu1*absoru2_abmet
    au_zmuze = 1.008*au_znu1+4.003*au_znu2+au_avm*au_znu3

    if ((calth.eq.2).and.(callam.eq.2)) go to 5016
    if (calth.eq.2) go to 9001
    if (th.le.0.8) ith=1
    if (th.gt.0.8) ith=2
    nset = absoru2_numset(ith)-1

    9001 continue
    do i = 1,nset
      if (abs(wl-absoru2_wi(i+1,ith)).le.0.50) go to 8000
      if (wl.lt.absoru2_wi(i+1,ith)) go to 7000
    end do

    7000 continue
    au_jfz=1
    go to 9002

    8000 continue
    au_jfz=2

    ! DIFFUSION DE RAYLEIGH PAR H ET H2 (DALGARNO) HARVARD JUIN 1964
    ! SCATH(1)=DIFFUSION DE H
    ! SCATH(2)=DIFFUSION DE H2
    9002 continue
    do 9023 i=1,2
      if (i.eq.2) go to 9020
      if (wl.gt.1026.0) go to 9021
      scath(1)=4.0e-24
      go to 9023

      9020 continue
      if (wl .gt. 1200.0) go to 9021
      wlh = 1200.0
      go to 9022

      9021 continue
      wlh=wl
      9022 continue
      wl4=wlh**4
      scath(i)=(dif(i,1)+dif(i,2)/sqrt(wl4)+dif(i,3)/wl4)/wl4
    9023 continue

    go to 5018

    5016 continue
    if ((au_jfz.ne.2).or.(ilt.eq.1)) go to 5017
    ilt=callam-1

    5018 continue
    call gaunth(wl, flag_hydro2)

    5017  continue
    call tempa(wl,th,calth,callam)
    if (calth.eq.2) go to 9007
    call sahath(th, flag_hydro2)

    9007 continue
    if ((calth.eq.2).and.(callam.eq.2)) go to 9006
    call athyhe(wl, th, callam, at_zzk, flag_hydro2)

    9006 continue
    if (calmet.eq.1) go to 9003 ! note: will go to 9003 anyway

    9003 continue
    call ionipe (th,zlpe,calth)

    ! ISSUE I gotta test variable spill and probably dimension variables
    mm=absoru2_nmeta+1
    mmm=absoru2_nmeta+6
    scatel=9.559063e-13*au_pe*th
    ! 9.559063E-13=4.81815E-9/5040.39 ET 4.81815E-9=6.625E-25/1.38024E-16
    ! =ELECTRON SCATTERING/(K*T)  UNSOLD P. 180 1955

    !86 format ('0LAMBDA KKK   c1'7x'mg'7x'si1'7x'al1'8x'h-'7x'h2-'7x'h2+'9x'h'7x'he+'8x'he'5x'k total/',2a4)
    !write (lll,86) (absoru2_iunite(i),i=1,2)
    !call log_debug(lll)

    kkk=au_jfz
    min=mm

    do i=1,au_jfz
      do m=1,absoru2_nmeta
        zzkk(m,i)=0.0
      end do
    end do

    absoru_totkap(2) = 0.
    if (calu.eq.1) unit=au_rho
    If (calu.eq.2) unit=absoru_toc ! RAPPEL  absoru_toc=NBRE DE NOYAUX DE H PAR CM3

    scatel=scatel/unit
    do i=1,kkk
      absoru_totkap(i)=0.0
      scat(1)=scath(1)*absoru_znh(absoru2_nmeta+4)/unit
      scat(2)=scath(2)*absoru_znh(absoru2_nmeta+2)/unit
      do m = min,mmm
        ! les absoru_znh pour les metaux sont en cm-3*1.0e-18
        if ((m.ne.(absoru2_nmeta+1)).and.(m.ne.(absoru2_nmeta+3))) go to 4222

        if (m.eq.(absoru2_nmeta+1)) &
         zzkk(m,i)=at_zzk(m,i)*(absoru_znh(absoru2_nmeta+4)*au_pe*1.e-26)/unit
        if (m.eq.(absoru2_nmeta+3)) &
         zzkk(m,i)=at_zzk(m,i)*((absoru_znh(absoru2_nmeta+4)*1.e-19)*(absoru_znh(absoru2_nmeta+7)*1.0e-20))/unit
        go to 4221

        4222 continue
        zzkk(m,i)=at_zzk(m,i)*absoru_znh(m)/unit
        if (m.eq.(absoru2_nmeta+2)) zzkk(m,i)=zzkk(m,i)*au_pe

        4221 continue
        absoru_totkap(i) = absoru_totkap(i)+zzkk(m,i)
      end do
      absoru_totkap(i) = (absoru_totkap(i)+scatel+scat(1)+scat(2))

      !87 format ('wl,kkk,zzkk,totkap =>',f9.1,i2,1p12e10.2)
      !write (lll,87) wl,kkk,(zzkk(m,i),m=1,mmm),absoru_totkap(i)
      !call log_debug(lll)
    end do

    !89 format ('0SIG(E)='1PE11.4,' SIG(H)='E11.4,' SIG(H2)='E11.4,' DENSITE='E11.4,' NBR.NOYAU D H/CM3='E11.4, &
    !           ' LOG10PE='0PF5.2,' TETA='F5.2)
    !write (lll,89) scatel,scat(1),scat(2),au_rho,absoru_toc,zlpe,th
    !call log_debug(lll)


    !call log_debug(LEAVING//'absoru_()')
  end


  !-------------------------------------------------------------------------------
  !
  ! Calculates the "Gaunth factor": multiplicative correction to the continuous absorption
  ! (i.e., a statistical weight)
  !
  ! "DETERMINATION DU FACTEUR DE GAUNT BOUND FREE POUR L HYDROGENE"
  !
  ! Reference:
  ! J.A.Gaunth 1930. http://en.wikipedia.org/wiki/Gaunt_factor http://dx.doi.org/10.1098%2Frsta.1930.0005
  !
  ! A.M COLLE   19/8/69

  subroutine gaunth(wl, flag_hydro2)
    ! whether to use PFANT or HYDRO2 logic. The difference is just some extra care with small argument to sqrt() in hydro2
    logical, intent(in) :: flag_hydro2
    real*8, intent(in) :: wl ! ?doc?
    real*8 cond, delta, rk, zj, zp, zq
    integer i, j, jj, js
    real*8, parameter :: VARIAVEL = 1e-37

    au_jh = 0
    do 1410 i=1,au_jfz
      do j=1,19
        jj=j
        if (abs(wl-AU_ZLH(j)) .le. 0.5) go to 1335
        if (wl .lt. AU_ZLH(j)) go to 1333
      end do

      1333  continue
      if (i .ne. 2) go to 1334

      !
      ! CE N'EST PAS UNE DISCONTINUITE DE L'HYDROGENE
      !

      do j = 1,19
        au_g2d(2,j)=au_g2d(1,j)
      end do

      go to 1420

      1334 continue
      js=jj
      go to 1340

      1335 continue
      !
      ! C'EST UNE DISCONTINUITE DE L'HYDROGENE
      !
      au_jh=1
      if (i .eq. 1) go to 1334

      js = jj+1

      1340 continue
      au_jshyd(i) = js

      do 1410 j=js,19
        zj=j
        if (j.gt.7) go to 1400
        cond=AU_ZLH(j)-wl
        if (abs(cond).le.0.50) go to 1122
        if (cond.lt.0.0) go to 1410

        !=====
        ! assignment of au_g2d(i,j), alternative 1
        !=====

        zq=wl*j**2/cond


        if (flag_hydro2) then
          ! #hydro2_mode
          if(zq .gt. VARIAVEL) then
            rk = sqrt(zq)
          else
            zq = 0
            rk = 0
          end if
        else
          rk = sqrt(zq)
        end if

        go to (1111,1113,1115,1117,1119,2000,2010), j

        ! menzel et pekeris=mon. not. vol. 96 p. 77 1935

        1111 delta=8.*rk/sqrt(zq+1.0)
        go to 1120

        1113 delta=(16.*rk*(3.*zq+4.)*(5.*zq+4.))/(zq+4.)**2.5
        go to 1120

        1115 delta=(24.*rk*((13.*zq+78.)*zq+81.)*((29.*zq+126.)*zq+81.))/(zq+9.)**4.5
        go to 1120

        1117 delta=32.*rk*(((197.*zq+3152.)*zq+13056.)*zq+12288.)*(((539.*zq+6800.)* &
                   zq+20736.)*zq+12288.)/(9.*(zq+16.)**6.5)
        go to 1120

        1119 delta=40.*rk*((((1083.*zq+36100.)*zq+372250.)*zq+1312500.)*zq+1171875.)* &
                   ((((3467.*zq+95700.)*zq+786250.)*zq+2062500.)*zq+1171875.)/(9.*(zq+25.)**8.5)
        go to 1120

        ! hagihara and soma=j.of astr. and geophys. japanese vol. 20 p. 59 1

        2000 zp=(zq+36.)**5.25
        delta=48.*rk*((((((38081.*zq+1953540.)*zq+3348086.e1)*zq+ &
        2262816.e2)*zq+5458752.e2)*zq+3023309.e2)/zp)*((((((10471.*zq+628260.)*zq+ &
        1290902.e1)*zq+1087085.e2)*zq+34992.0e4)*zq+3023309.e2)/25./zp)
        go to 1120

        2010 zp=(zq+49.)**6.25
        delta=56.*rk*(((((((56740.9*zq+5560608.)*zq+1993433.e2)*zq+3248060.e3)*zq+ &
        2428999.e4)*zq+7372604.e4)*zq+6228579.e4)/zp)*(((((((229742.5*zq+1968907.e1)* &
        zq+6067219.e2)*zq+8290160.e3)*zq+5002406.e4)*zq &
        +1144025.e5)*zq+6228579.e4)/20.25/zp)

        1120 au_g2d(i,j)=5.441398*rk*j*exp(-4.*rk*atan(zj/rk))*delta/ &
                         (sqrt(zq+zj**2)*(1.-exp(-6.283185*rk)))
        go to 1410


        !=====
        ! Assignment of au_G2D(I,J), alternative 2
        !=====
        1122 continue
        go to (1123,1125,1127,1129,1131,2020,2030), j
        1123 au_g2d(i,j)=0.7973
        go to 1410
        1125 au_g2d(i,j)=0.8762
        go to 1410
        1127 au_g2d(i,j)=0.9075
        go to 1410
        1129 au_g2d(i,j)=0.9247
        go to 1410
        1131 au_g2d(i,j)=0.9358
        go to 1410
        2020 au_g2d(i,j)=0.9436
        go to 1410
        2030 au_g2d(i,j)=0.9494
        go to 1410
        1400 au_g2d(i,j)=1.0

    1410 continue  ! This works as the "END DO" for two loops
    1420 return
  end


  !-------------------------------------------------------------------------------
  ! ?doc? subroutine has no description line
  !
  !
  ! HCBKTM=(H*C/K*T)*1.0E8
  ! 0.0010967876=CONSTANTE DE RYDBERG POUR H  *1.0E-8  ALLEN 1963
  ! 0.0043890867=CONSTANTE DE RYDBERG POUR HE+*1.0E-8  MOORE 1950 (HE4
  ! au_AHE =POUR HE 4*C/T**3
  ! au_AH  =POUR H   C*Z**4/T**3  AVEC Z=1
  ! au_AHEP=POUR HE+ C*Z**4/T**3  AVEC Z=2
  ! C=64*PI**4*ME*E**10/(3*RAC(3)*C*H**3*K**3)
  ! ME=9.10E-28,E**10=4.8E-10,K=1.38024E-16,H=6.6237E-27,C=2.99791E+10
  !
  !
  ! *Author* A.M COLLE   8/5/69

  subroutine tempa(wl,th,calth,callam)
    implicit none
    real*8, intent(in) :: wl, & ! ?doc?
     th  ! ?doc?
    real*8 comhe, hcbktm, uh, uhep
    integer j, k, l
    integer, intent(in) :: callam, & ! ?doc?
     calth  ! ?doc?

    if (calth.eq.2) go to 1001

    hcbktm  = 0.2854306e-3*th*1.0e8
    au_ahe  = 0.9717088e-12*th**3
    au_ah   = 0.2429272e-12*th**3
    au_ahep = 16.*au_ah
    au_uh1  = 1.096788e-3*hcbktm
    au_zemh = exp(-au_uh1)

    if (th.gt.1.4) go to 1001

    do j = 1,20
      uh=au_uh1/j**2
      au_zeuh(j)=exp(uh-au_uh1)/j**3
    enddo

    au_zeuh(20) = au_zeuh(20)*8000.  ! ISSUE big why this (ask mt)? (MT): Why 20? Why 8000.?
    au_uhep1 = 4.389087e-3*hcbktm
    if (th .gt. 0.3) go to 5290

    do j=1,20
      uhep=au_uhep1/j**2
      au_zeuhep(j) = exp(uhep-au_uhep1)/j**3
    end do

    au_zeuhep(20) = au_zeuhep(20)*8000.

    5290 continue
    au_uhe1 = hcbktm/504.3
    au_zeuhe1 = exp(-au_uhe1)
    if (th .gt. 0.8) go to 1001

    comhe=-hcbktm*(1.0/AU_ZLHEM(1))
    do k = 1,5
      au_zexpm(k)=exp(comhe+hcbktm*(1.0/AU_ZLHEM(k)))*AU_STWTM(k)
    end do

    do l=3,10
      au_zexp(l)= exp(comhe+hcbktm*(1.0/AU_ZLHE(l)))/l**3
    end do

    1001 if ((callam.eq.2).and.(calth.eq.2)) go to 5010

    au_ul = hcbktm/wl

    5010 return
  end



  !-------------------------------------------------------------------------------
  ! SAHA's equation: ionization equilibrium: relative number of atoms in each
  ! ionization state
  !
  !
  ! LOI DE SAHA=LOG((absoru2_NR+1)/absoru2_NR)*modele%pe= -POT.ION.*TH+5/2*LOG(T)-0.4772+FONC
  ! LES FONCTIONS DE PARTITION (L0G(2UR+1)/UR) SONT INCLUSES DANS LES
  ! CONSTANTES AJOUTEES A TEMPOR POUR H ET HE LES AUTRES SONT LUES
  ! 31.303644,1.7200311,56.597541,125.26753,SONT RESPECTIVEMENT LES
  ! POTENTIELS D'IONISATION DE (H,H-,HE,HE+)*2.3025851
  !
  !
  !     A.M COLLE   13/5/69
  !
  ! ISSUE logic is different for PFANT and HYDRO2: solve conflict. For the time,
  !       there is a flag_hydro2. Actually it seems that the differences are in sparing
  !       a few exponential calculations where the argument to exp() is < -38 or -100.
  !       Come back to this later.
  !
  ! ISSUE logic suggests a few "absoru2_nmeta+5" here should be "absoru2_nmeta+n"

  subroutine sahath(th, flag_hydro2)
    real*8, intent(in) :: th  ! ?doc?
    ! whether to use pfant or hydro2 logic
    logical, intent(in) :: flag_hydro2
    real*8 :: tempo, tempor, potepo
    integer i, j, n, nrr
    real*8, parameter :: &
     POTION(6) = (/-1.720031, 0.0, 0.0, 31.30364, 125.2675, -56.59754/), &
     C1(3) = (/0.0,-7.526612E-3,5.708280E-2/), &
     C2(3) = (/0.0,1.293852E-1,-1.823574E-1/), &
     C3(3) = (/0.0,-11.34061,-6.434060/), &
     C4(3) = (/0.0,28.85946,25.80507/)

    do n = 2,3
      au_zk(absoru2_nmeta+n)=exp(((C1(n)*th+C2(n))*th+C3(n))*th+C4(n))
    end do

    tempor=2.5*log(5040.39/th)
    tempo=tempor-1.098794

    if (flag_hydro2) then
      ! #hydro2_mode
      do n = 4,5
        au_zk(absoru2_nmeta+n) = potion(n)*th-tempo
        if (au_zk(absoru2_nmeta+5) .lt. 38) then
          au_zk(absoru2_nmeta+n) = exp(-au_zk(absoru2_nmeta+n))
        else
          au_zk(absoru2_nmeta+n) = 0.0
        end if
      end do

      do  j = 1,absoru2_nm
        nrr=absoru2_nr(j)
        do  i=1,nrr
          au_zkm(j,i)=th*absoru2_xi(j,i)-absoru2_pf(j,i)-tempo
          if (au_zkm(j,i) .lt. 38) then
            au_zkm(j,i) = exp(-au_zkm(j,i))
          else
            au_zkm(j,i)=0.0
          end if
        end do
      end do

      tempo=tempor+0.2875929
      do  n=1,6,5
        potepo = potion(n)*th+tempo
        if(potepo .gt. -38) then
          au_zk(absoru2_nmeta+n) = exp(potepo)
        else
          au_zk(absoru2_nmeta+n) = 0  ! saving exponential calculation or improving visual output?
        end if
      end do

    else
      ! PFANT mode
      do 2 n = 4,5
        au_zk(absoru2_nmeta+n) = potion(n)*th-tempo
        if (n .eq. 4) go to 12
        if  (au_zk(absoru2_nmeta+5) .lt. 100.0) go to 12
        au_zk(absoru2_nmeta+5) = 0.0
        go to 2
        12 continue
        au_zk(absoru2_nmeta+n) = exp(-au_zk(absoru2_nmeta+n))
      2 continue

      do 2270 j=1,absoru2_nm
        nrr=absoru2_nr(j)
        do 2270 i=1,nrr
          au_zkm(j,i)=th*absoru2_xi(j,i)-absoru2_pf(j,i)-tempo
          if (au_zkm(j,i) .lt. 100.0) go to 2269
          au_zkm(j,i)=0.0
          go to 2270
          2269 au_zkm(j,i) = exp(-au_zkm(j,i))
      2270 continue

      tempo=tempor+0.2875929
      do n=1,6,5
        au_zk(absoru2_nmeta+n) = exp(potion(n)*th+tempo)
      end do
    end if
  end




  !-------------------------------------------------------------------------------
  ! CE SSP CALCULE LE COEFFICIENT D'ABSORPTION PAR ATOME NEUTRE POUR
  ! L'HYDROGENE ET L'HELIUM, ON SORT 2 VALEURS DE at_zzk SI WL= A UNE
  ! DISCONTINUITE DE L'UN DE CES ABSORBANTS
  !
  ! *Author* A.M COLLE  07/12/1970
  !
  ! Note variable named "zk_"
  !   - local "zk" renamed to "zk_"
  !   - old COMMON "zk" so far is a module variable named au_zk

  subroutine athyhe(wl,th,callam,at_zzk, flag_hydro2)
    real*8,  intent(in) :: wl, & ! ?doc?
     th ! ?doc?
    integer, intent(in) :: callam ! ?doc?
    ! whether to use PFANT or HYDRO2 logic. The difference is just some extra care with small argument to exp() in hydro2
    ! ISSUE decide upon a single logic, I think
    logical, intent(in) :: flag_hydro2
    real*8,  intent(out) :: at_zzk(11,2) ! ?doc?

    real*8 althmb, althml, anu, any, bh, bhe, bhem, bhep, bkt, caeta, caro, difeta, &
     difro, dksq, fact, g3, gconst, rhog1, rhog2, rk, sigh, sighe, sighem, sighep, &
     stimu3, tempor, uk, wlm, zkas, zlamin, zleta1, zleta2, znl, znl1, zk_
    integer i, ie, indth, ir, j, je, jhyt, jj, jjs, jr, js, k, kk, kks, l, ll, lls, n
    integer jhe, jhep, jhem
    real*8 :: tgaunt(5),trhog(5),opnu(46), expon(2)
    real*8 :: ezut1, ezut2, zut1, zut2
    real*8, parameter ::                                  &
     expo(2) = (/-68.88230,-71.45087/),                   &
     cons(2) = (/3.3,3.6/),                               &
     cote(2) = (/3.136954e-23,8.195952e-24/),             &
     sniv(2) = (/0.55,0.485/),                            &
     cuk(2)  = (/0.3025,0.235225/),                       &
     an(2)   = (/0.3099204e-21, 0.22849203e-21/),         &
     c1(3)   = (/-2.850692e-2,-7.056869e-3,3.591294e-3/), &
     c2(3)   = (/0.2080816,0.1809394,-0.1959804/),        &
     c3(3)   = (/2.549101,-1.828635,4.233733/),           &
     c4(3)   = (/-14.97997,8.900841,-20.84862/),          &
     c5(3)   = (/0.0,-17.78231,0.0/),                     &
     c6(3)   = (/0.0,-7.89472e-2,0.0/)

    jhe  = 0
    jhep = 0
    jhem = 0

    if (callam.eq.1) indth = 0
    jhyt = 1

    ! 31.3213   = 157871.62/5040.39;
    ! 157871.62 = M*Z**2*E**4/(2*K*(H/2*PI)**
    ! M ET E SONT LA MASSE ET LA CHARGE DE L'ELECTRON,H ET K LES CONSTAN
    ! DE PLANCK ET DE BOLTZMANN
    gconst = 31.3213*th

    if (au_ul .lt. 100.0) go to 1333

    ! au_UL=H*NU/(K*T)

    au_STIMU=1.0
    GO TO 1334

    1333 au_stimu = 1.0-exp(-au_ul)
    1334 stimu3   = au_stimu/au_ul**3
    if (callam.eq.2) go to 1335

    znl = log(wl)
    zlamin = 1.0e8/wl
    do n = 1,2
      expon(n)=exp(expo(n)+cons(n)*znl)
    end do

    ! -- I --  H-
    ! H- GINGERICH: HARVARD JUIN 1964 (RESULTATS*1.0E-26)

    1335 if (th .ge. 0.3) go to 6060

    at_zzk(absoru2_nmeta+1,1)=0.0
    go to 6210

    6060 if ((callam.eq.2).and.(indth.eq.1)) go to 6100

    indth = 1
    if (wl.le.16419.0) go to 6070

    althmb = 0.0
    go to 6190

    6070 wlm = wl/1.0e3
    if (wl.gt.14200.0) go to 6090

    zkas =(((5.95244e-4*wlm-0.0204842)*wlm+0.164790)*wlm+0.178708)*wlm+0.680133e-2
    go to 6100

    6090 wlm=16.149-wlm
    zkas = ((0.273236e-2*wlm-0.411288e-1)*wlm+0.220190)*wlm**2+0.269818

    6100 fact = 1.0-exp((-th)*28.54310e+3/wl)

    !
    ! TRANSITION BOUND-FREE= GELTMAN APJ. VOL. 136 P. 935 1962
    !

    althmb = zkas*4.158e-1*th**2.5*exp(1.726*th)*fact

    !
    ! TRANSITION FREE-FREE=T L JOHN (OCT.1963)
    !

    6190 althml=(wl/1.0e6)*(((-5.939*th+11.934)*th-3.2062)+(wl/1.0e3)* &
     ((-0.34592*th+7.0355)*th-0.40192))+((0.027039*th-0.011493)*th+0.0053666)

    ! ISSUE: check spill!!!!!! if using index +1, perhaps I should dimension the relevant vectors with dimension MAX_absoru2_NMETA+1
    at_zzk(absoru2_nmeta+1,1) = althmb+althml

    ! -- II --  H2-
    ! H2- SOMMERVILLE: APJ. VOL. 139 P. 195 1963
    6210 if (th .lt. 0.5) go to 2050
    if (wl .ge. 3040.0) go to 2070

    2050 at_zzk(absoru2_nmeta+2,1)=0.0
    go to 2080

    2070 dksq=911.27/wl
    at_zzk(absoru2_nmeta+2,1)=(((0.09319*th+2.857-0.9316/th)/dksq-(2.6*th+6.831-4.993/th))/ &
     dksq+(35.29*th-9.804-10.62/th)-(74.52*th-62.48+0.4679/th)*dksq)*1.0e-29

    ! -- III --  H2+
    ! H2+ BATES: HARVARD JUIN 1964  (RESULTATS *1.0E+39)

    2080 if ((th.lt.0.25).or.((zlamin.lt.AU_WINV(1)).or.(zlamin.gt.AU_WINV(46)))) go to 1012

    bkt=3.19286e-2/th  ! BKT=K*T EN RYDBERGS POUR H2+

    do j=1,46
      if (flag_hydro2) then
        ! #hydro2_mode
        zut1 = AU_U1(j)/bkt
        zut2 = -AU_U2(j)/bkt
        if (zut2 .gt. -38) then
          ezut1 = exp(zut1)
          ezut2 = exp(zut2)
          opnu(j)=2.51e-3*AU_GRDM(j)*(ezut1-ezut2)
        else
          if(zut1 .gt. -38) then
            ezut1 = exp(zut1)
            opnu(j) = 2.51e-3*AU_GRDM(j)*ezut1
          else
            opnu(j) = 0
          end if
        end if
      else
        opnu(j)=2.51e-3*AU_GRDM(j)*(exp(AU_U1(j)/bkt)-(exp(-AU_U2(j)/bkt)))
      end if
    end do

    do j = 1,46
      jj=j
      if (abs(zlamin-AU_WINV(j)) .le. 0.5) go to 1014
      if (zlamin .lt. AU_WINV(j)) go to 1015
    end do

    1014 at_zzk(absoru2_nmeta+3,1)=opnu(jj)
    go to 1016

    ! INTERPOLATION LINEAIRE
    1015 at_zzk(absoru2_nmeta+3,1)=(opnu(jj-1)*AU_WINV(jj)-opnu(jj)*AU_WINV(jj-1)+ &
     (opnu(jj)-opnu(jj-1))*zlamin)/(AU_WINV(jj)-AU_WINV(jj-1))
    go to 1016

    1012 at_zzk(absoru2_nmeta+3,1)=0.0

    ! CAS OU WL EST UNE DISCONTINUITE
    1016 if (au_jfz.ne.2) go to 1017
    do n = 1,3
      at_zzk(absoru2_nmeta+n,2)=at_zzk(absoru2_nmeta+n,1)
    end do

    ! -- IV --  H
    ! H UNSOLD (1955) PAGE 168
    ! FACTEUR DE GAUNT FREE-FREE POUR H=GRANT M.N.,VOL.118
    ! SYMBOLES CF VARDYA APJ.SUP. VOL. 8,P.277,1964
    1017  if (th.gt.1.4) go to 1809
    do 1855 k=1,4
      trhog(k)=sqrt(1.0+au_ul/AU_YY(k))
      if (trhog(k).ge.1.01) go to 1820
      if (trhog(k).ne.1.0) tgaunt(k)=2.0/(trhog(k)-1.0)
      if (trhog(k).eq.1.0) go to 1856

      tgaunt(k)=0.5513289*log(tgaunt(k))  ! 0.5513289=SQRT(3)/PI
      go to 1855

      1856 tgaunt(k)=0.0
      go to 1855

      1820 if (trhog(k).le.1.8) go to 1830

      tempor=(trhog(k)-1.0)*sqrt(gconst/(AU_YY(k)+au_ul))
      any=tempor**(-0.6666667)
      tgaunt(k)=(-0.01312*any+0.21775)*any+1.0
      go to 1855

      1830 tempor=0.2171473*log(gconst/(AU_YY(k)+au_ul))  ! 0.2171473=0.434294482/2
      if ((tempor.lt.AU_ZLETAG(1)).or.(tempor.gt.AU_ZLETAG(18))) go to 1847

      ! INTERPOLATION A PARTIR DE LA TABLE 1 DE GRANT (1958)
      do ir=1,12
        jr=ir
        if (abs(trhog(k)-AU_RHOG(ir)).le.1.0e-4) go to 1836
        if  (trhog(k).lt.AU_RHOG(ir)) go to 1837
      end do

      1836 caro=1.0
      !
      ! INTERPOLATION SUR LOG(-ETA) SEULEMENT
      !
      go to 1838

      1837 rhog1=AU_RHOG(jr-1)
      caro=trhog(k)-rhog1

      1838  rhog2=AU_RHOG(jr)
      if (caro.eq.1.0) difro=1.0
      if (caro.ne.1.0) difro=rhog2-rhog1
      do ie=1,18
        je=ie
        if (abs(tempor-AU_ZLETAG(ie)).le.1.0e-4) go to 1846
        if  (tempor.lt.AU_ZLETAG(ie)) go to 1848
      end do

      1846 if (caro .eq. 1.0) go to 1850
      caeta=1.0

      !
      ! INTERPOLATION SUR au_RHO SEULEMENT
      !
      go to 1849

      1848 zleta1=AU_ZLETAG(je-1)
      caeta=tempor-zleta1

      1849  zleta2=AU_ZLETAG(je)
      if(caeta.eq.1.0)  difeta=1.0
      if(caeta.ne.1.0)  difeta=zleta2-zleta1
      go to 1851

      1850 tgaunt(k)=au_g3d(jr,je)
      go to 1855

      1851 tgaunt(k)=((au_g3d(jr-1,je-1)*(rhog2-trhog(k))+au_g3d(jr,je-1)*caro)* &
       (zleta2-tempor)+(au_g3d(jr,je)*caro+au_g3d(jr-1,je)*(rhog2-trhog(k)))*caeta)/ &
       difro/difeta
      go to 1855

      1847 continue
      call log_critical('0 on sort de la table de gff')
    1855 continue

    g3=0.0
    do k=1,4
      g3=g3+tgaunt(k)*AU_AA(k)  ! G3: FACTEUR DE GAUNT FREE FREE
    end do
    go to 4199

    1809 at_zzk(absoru2_nmeta+4,1)=0.0
    jhyt=0

    4199 continue
    do 4200 i=1,au_jfz
      if (((i.eq.1).and.(jhyt.ne.0)).or.(au_jh.eq.1)) go to 4201
      !
      ! WL N'EST PAS UNE DISCONTINUITE DE H
      !
      if (i.eq.2) at_zzk(absoru2_nmeta+4,2)=at_zzk(absoru2_nmeta+4,1)
      go to 1451

      4201 sigh=0.0
      js=au_jshyd(i)
      do j=js,19
        sigh=sigh+au_g2d(i,j)*au_zeuh(j)  ! RAPPEL: au_G2D: FACTEUR DE GAUNT BOUND FREE
      end do

      bh=sigh+(au_zeuh(20)-(1.0-g3)*au_zemh)/(2*au_uh1)
      at_zzk(absoru2_nmeta+4,i)=au_ah*bh*stimu3

      !
      ! -- V -- HE+
      ! HE+  VARDYA APJ.SUP. VOL. 8,P.277,1964
      1451 if (th.gt.0.3) go to 1552
      sighep=0.0
      do j=1,19
        jj=j
        if (abs(wl-AU_ZLHEP(j)).le.0.50) go to 1465
        if (wl.lt.AU_ZLHEP(j)) go to 1463
      end do

      1463 jjs=jj
      go to 1470

      1465 jhep=1
      if (i.eq.1) go to 1463

      jjs=jj+1

      1470 if ((i.eq.1).or.(jhep.eq.1)) go to 1471

      !
      ! WL N'EST PAS UNE DISCONTINUITE DE HE+
      !

      at_zzk(absoru2_nmeta+5,2)=at_zzk(absoru2_nmeta+5,1)
      go to 1554

      1471 continue
      do jj=jjs,19
        sighep=sighep+au_zeuhep(jj)
      end do
      bhep=sighep+au_zeuhep(20)/(2*au_uhep1)
      at_zzk(absoru2_nmeta+5,i)=au_ahep*bhep*stimu3
      go to 1554

      1552 at_zzk(absoru2_nmeta+5,i)=0.0

      !
      ! -- VI -- HE
      ! HE  VARDYA: APJ. SUP. 80 VOL. 8 P. 277 JANVIER 1964
      1554 if (th.le.0.8) go to 5400
      at_zzk(absoru2_nmeta+6,i)=0.0
      go to 4200

      5400 if ((i.eq.2).and.(au_jh.eq.1)) go to 5872

      sighem=0.0
      sighe=0.0
      if ((wl-AU_ZLHEM(5)).gt.0.50) go to 5740

      do k=1,5
        kk=k
        if (abs(wl-AU_ZLHEM(k)).le.0.50) go to 5490
        if (wl.lt.AU_ZLHEM(k)) go to 5470
      end do

      5470 kks=kk
      go to 5540

      5490 jhem=1
      if (i.eq.1) go to 5470

      kks=kk+1
      if (kks.gt.5) go to 5740

      5540 if ((jhem.eq.1).or.(i.eq.1)) go to 5541
      !
      ! WL N'EST PAS = A UNE VALEUR DE AU_ZLHEM
      ! RAPPEL  AU_ZLHEM=504,2601,3122,3422,3680 A.
      !
      go to 5741

      5541 continue
      do 5730 k=kks,5
        go to (5560,5560,5560,5620,5680),k

        5560 if (k.eq.2) znl1=znl

        if (k.ne.2) znl1=1.0
        anu=exp(((((c1(k)*znl+c2(k))*znl+c3(k))*znl+c4(k))*znl1+c5(k))*znl1+c6(k))*1.0e-18
        go to 5730

        5620 n=1

        !
        ! GOLDBERG APJ. VOL. 90 P. 414 1939 ET UNDERHILL PUB. COP. OBS. N0.
        !

        5621 if (abs(wl-AU_ZLHEM(3+n)).gt.0.50) go to 5640
        ! NIVEAUX 4 A 7 DE HE1

        anu=an(n)/wl+expon(n)
        go to 5730

        5640 zk_=1.097224e-3*AU_ZLHEM(3+n)*wl/(AU_ZLHEM(3+n)-wl)
        rk=sqrt(zk_)
        uk=1.0+cuk(n)*zk_
        anu=(cote(n)/(wl*(1.0-exp(-6.283185*rk)))*(zk_/uk   )**6*((1.0+zk_)/ &
         uk)*((4.0+zk_)/uk)*exp(-4.0*rk*atan(1.0/(sniv(n)*rk))))+expon(n)
        go to 5730

        5680 n=2
        go to 5621

        sighem=sighem+anu*au_zexpm(k)
      5730 continue

      bhem=sighem*au_stimu
      go to 5741

      5740 bhem=0.0
      ! NIVEAUX 8 ET SQ (N.GE.3)
      5741 continue
      do l=3,9
        ll=l
        if (abs(wl-AU_ZLHE(l)).le.0.50) go to 5810
        if  (wl.lt.AU_ZLHE(l)) go to 5790
      end do

      5790 lls=ll
      go to 5860

      5810 jhe=1
      if (i.eq.1) go to 5790

      lls=ll+1

      5860 if ((i.eq.1).or.(jhe.eq.1)) go to 5861
      !
      ! WL N'EST PAS = A UNE VALEUR DE AU_ZLHE
      !
      go to 5871

      5861 continue
      do l=lls,9
        sighe=sighe+au_zexp(l)*AU_ZEFF4(l)
      end do
      bhe=sighe+(1807.240*au_zexp(10)-0.8072399*au_zeuhe1)/(2*au_uhe1)

      5871 at_zzk(absoru2_nmeta+6,i)=au_ahe*bhe*stimu3+bhem
      go to 4200

      !
      ! WL N'EST PAS UNE DISCONTINUITE DE HE
      !
      5872 at_zzk(absoru2_nmeta+6,2)=at_zzk(absoru2_nmeta+6,1)
    4200 continue
  end






  !-------------------------------------------------------------------------------
  ! Ionization degree by hydrogen atoms & electrons (???; to be confirmed) ISSUE
  !
  ! SSP CALCULANT LES QUANTITES SUIVANTES: PARTH, au_PG, au_ZMU, au_RHO, absoru_toc,
  ! au_AC, au_AC1, au_AC2, PHI, absoru_ZNH
  !
  ! Reference: 'VARDYA' APJ VOL.133,P.107,1961
  !
  ! *Author* A.M COLLE  18/01/1971

  subroutine ionipe(th,zlpe,calth)
    real*8, intent(in) :: th, & ! ?doc?
     zlpe ! log of electron pressure
    integer, intent(in) :: calth ! ?doc?

    real*8 any, cond, den, fun1, fun2, pa, parth, ph, phi, ppar, s, sigm1, &
     sigm2, sigm3, tempor, tp1, tp2, w1, w2, w3, w4, w5, w6
    integer i, j, nrr
    real*8 kth
    dimension phi(MAX_ABSORU2_NM), &  ! PHI(J) = DEGRE D'IONIZATION DE LELEMENT J POUR MULTIPLE IONISATION
              pa(MAX_ABSORU2_NRR+1)

    kth = 6.956948e-13/th  ! 6.956948E-13 = 1.38024E-16*5040.39
    au_pe=exp(zlpe*2.302585)
    sigm3=0.0

    ! call log_debug("absoru2_nm is = "//int2str(absoru2_nm))

    do j=1,absoru2_nm
      nrr=absoru2_nr(j)
      sigm1=0.0
      sigm2=0.0
      pa(1)=1.0

      ! ISSUE check this, I think the programmer was considering PA initialized to zero when this is not necessarily true. I am initializing it
      do i = 2, nrr
        pa(i) = 0
      end do

      do i = 1,nrr
        if ((pa(i) .le. 0.0) .or. (au_zkm(j,i) .le. 0.0)) exit

        pa(i+1)=pa(i)*(au_zkm(j,i)/au_pe)
        sigm1=i*pa(i+1)+sigm1
        sigm2=sigm2+pa(i+1)
      end do

      den=1.0+sigm2
      phi(j)=sigm1/den
      do i=1,nrr
        au_ac2(j,i)=pa(i)/den
      end do
      sigm3=sigm3+au_znu(j)*phi(j)
    end do

    if (calth.eq.2) go to 2390
    if (th.ge.0.25) go to 2382

    tempor=0.0
    any=0.0
    cond=0.0
    go to 2390

    2382 continue
    any=1.0/au_zk(absoru2_nmeta+3)
    tempor=1.0/au_zk(absoru2_nmeta+2)
    cond=1.0/au_zk(absoru2_nmeta+1)

    2390 continue
    w2=au_zk(absoru2_nmeta+4)/au_pe
    w1=w2*any
    w3=au_pe*cond
    w4=au_znu2*au_zk(absoru2_nmeta+6)*(au_pe+2*au_zk(absoru2_nmeta+5))/ &
       ((au_pe+au_zk(absoru2_nmeta+6))*au_pe+au_zk(absoru2_nmeta+6)*au_zk(absoru2_nmeta+5))+ &
       (au_znu3*sigm3)
    fun1=au_znu1*w1+2*(tempor+w1)*w4
    fun2=au_znu1*(w2-w3)+(1.0+w2+w3)*w4
    ph=2*au_znu1*au_pe/(fun2+sqrt(fun2**2+4*fun1*au_znu1*au_pe))
    absoru_znh(absoru2_nmeta+4)=ph/kth
    absoru_znh(absoru2_nmeta+2)=ph*tempor*absoru_znh(absoru2_nmeta+4)
    absoru_znh(absoru2_nmeta+1)=absoru_znh(absoru2_nmeta+4)*w3
    absoru_znh(absoru2_nmeta+7)=absoru_znh(absoru2_nmeta+4)*w2
    absoru_znh(absoru2_nmeta+3)=absoru_znh(absoru2_nmeta+7)*ph*any
    tp1=absoru_znh(absoru2_nmeta+1)+absoru_znh(absoru2_nmeta+4)+absoru_znh(absoru2_nmeta+7)
    tp2=absoru_znh(absoru2_nmeta+2)+absoru_znh(absoru2_nmeta+3)
    absoru_toc=2*tp2+tp1
    parth=absoru_toc/au_znu1 ! NBRE TOTAL DE NOYAUX PAR CM3
    ppar=(tp1+tp2+parth*(au_znu2+au_znu3))*kth
    au_pg=ppar+au_pe
    au_ac=au_pe/ppar
    w5=au_zk(absoru2_nmeta+6)/au_pe
    w6=au_zk(absoru2_nmeta+5)*w5/au_pe
    s=1.0+w5+w6
    au_ac1(1)=w2/(1.0+w2)
    au_ac1(2)=w6/s
    au_ac1(3)=w5/s
    absoru_znh(absoru2_nmeta+6)=au_znu2*parth/(1.0+w5+w6)
    absoru_znh(absoru2_nmeta+5)=absoru_znh(absoru2_nmeta+6)*w5
    au_rho = 1.6602e-24*parth*au_zmuze  ! 1.6602E-24: MASSE DE L'UNITE DE POIDS
    ! not used au_zmu=au_rho*41904.28e+7/(th*au_pg)  ! 41904.275E+7: 8.313697E+7*5040.39, OU
    !                                       ! 8.313697e+7: constante des gaz


!print *, 'absoru_toc=', absoru_toc
!print *, 'parth=', parth
!print *, 'au_rho=', au_rho


  end subroutine ionipe
end module absoru




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Module containing the subtourine turbul_()
!
! Prefix:
!   turbul_* calculated by turbul_()
!
! Note that this module is used by pfant and hydro2 now.

module turbul
  use dimensions
  use file_main
  use file_models
  use config
  use misc_math
  implicit none

  real*8, dimension(MAX_MODELES_NTOT) :: turbul_vt
contains
  !======================================================================================================================
  ! This routine synchronizes the velocity(ies) of microturbulence informed in *main file*
  ! with the number of layers in the atmospheric model, interpolating the velocities
  ! if necessary.
  !
  ! (MT): Related to line broadening due to Doppler effect caused by microturbulent velocity.

  subroutine turbul_()
    integer i, nt2, n

    call log_debug('entree des turbul')
    if(main_ivtot .eq. 1)   then
      call log_debug('vt constant')
      do n = 1, modele%ntot
        turbul_vt(n) = main_vvt(1)*1e5
      end do
    else
      101 format(10f8.3)
      call log_debug('vt variable avec la profondeur')
      call log_debug('    log to')
      write(lll,101) (main_tolv(i),i=1,main_ivtot)
      call log_debug(lll)
      call log_debug('    vt')
      write(lll,101) (main_vvt(i),i=1,main_ivtot)
      call log_debug(lll)

      if(config_interp .eq. 1) then
        call ftlin3(main_ivtot, main_tolv, main_vvt, modele%ntot, modele%log_tau_ross, turbul_vt)
      elseif (config_interp .eq. 2) then
        ! ISSUE config_interp was hard-switched to 1, config_interp=2 needs testing.
        ! However, now nulbad is using ft2() to re-sample the spectrum and it is working fine!!
        call ft2(main_ivtot, main_tolv, main_vvt, modele%ntot, modele%log_tau_ross, turbul_vt)
      end if


      nt2 = modele%ntot-2
      do n = 1, nt2, 3
        102 format(3(i5,2f8.3,5x))
        write(lll,102) n,modele%log_tau_ross(n),turbul_vt(n),(n+1), modele%log_tau_ross(n+1), &
         turbul_vt(n+1),(n+2),modele%log_tau_ross(n+2),turbul_vt(n+2)
        call log_debug(lll)
      end do

      do n = 1, modele%ntot
        turbul_vt(n) = turbul_vt(n)*1e5
      end do
    end if

    if(main_ivtot .eq. 1) then
      131 format(' v microturbulence constante  =',f6.1,'km/s')
      write(lll,131) main_vvt(1)
      call log_debug(lll)
    else
      call log_debug('v microturbulence variable avec profondeur')
    end if

    return
  end subroutine
end


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Equilibre dissociatif
!
! This module contains subroutine sat4(), which calculates the sat4_* variables
! used by other modules.
!
! Prefixes:
!   - sat4_ -- variables calculated by sat4() (or indirectly, die())

module dissoc
  use file_models
  use file_dissoc
  implicit none

  ! Pressure vectors of elements listed in Table 1 of dissoc.dat
  real*8, target, dimension(MAX_MODELES_NTOT, MAX_DISSOC_NMETAL) :: dissoc_xp


  ! They will be pointer targets at molecules::point_ppa_pb()
  real*8, public, dimension(MAX_MODELES_NTOT) ::  &
   sat4_pph, &  ! pressure: hydrogen (used in kapmol_())
   sat4_po      ! pressure: oxygen (used in popadelh())

!  ! They will be pointer targets at molecules::point_ppa_pb()
!  real*8, public, target, dimension(MAX_MODELES_NTOT) ::  &
!   sat4_pph,  & ! pressure: hydrogen
!   sat4_ppc2, & ! pressure: 12carbon
!   sat4_pn,   & ! pressure: nytrogen
!   sat4_pc13, & ! pressure: 13carbon
!   sat4_pmg,  & ! pressure: magnesium
!   sat4_po,   & ! pressure: oxygen
!   sat4_pti,  & ! pressure: titanium
!   sat4_pfe     ! pressure: iron


  private :: die ! private subroutine

  integer, private, parameter :: &
   Z_ELECTRON = 99,  &  ! Fictional atomic number of electron.
                        ! *Note*: this is OK, Z=99 will never be used as a physical element
   Z_H_STAR   = 100, &  ! Fictional atomic number of "H*"
                        ! *Note*: this is OK, Z=99 will never be used as a physical element
   Z_H        = 1,   &  ! Atomic number of Hydrogen
   Z_HE       = 2       ! Atomic number of Helium

  ! Note that indexes of these arrays are atomic numbers
  ! For example: m_ip(z) contains information related to atomic number z
  real*8, private, dimension(MAX_DISSOC_Z) :: &
   m_ip,     & ! values come from dissoc_ip, but index is the atomic number
   m_ccomp,  & ! ?doc?
   m_uiidui, & ! ?doc?
   m_fp,     & ! ?doc?
   m_kp,     & ! ?doc?
   m_p         ! ?doc? Pressure

  real*8, private, dimension(MAX_DISSOC_NMOL) :: &
   m_ppmol, & ! ?doc?
   m_apmlog   ! ?doc?

  real*8, private :: m_pe ! Electron pressure

  real*8, private, parameter :: ECONST = 4.342945e-1 ! ?doc?
contains

  !=======================================================================================
  ! Subroutine d'equilibre dissociatif

  subroutine sat4()
    real*8  kplog, fplog, &
     pdfpl, pelog, pglog, pionl, plog, pmoll, tem, pg, theta, xlog
    real*8 cclogi
    integer i, ig0i, ig1i, iq, ir, irl, irr, ito, j, jcount, nbl, &
     nelemi, nelemxi, k1, k2, k3, kd, kf, iz

    !
    !*****INPUT A

    ! Infers other variables from variables dissoc_*
    do i = 1, dissoc_nmetal
      cclogi = dissoc_cclog(i)+main_afstar
      cclogi = cclogi
      if(i .eq. 1) cclogi = 0.0
      if(i .eq. 2) cclogi = -1.0

      nelemxi = dissoc_nelemx(i)
      ig0i = dissoc_ig0(i)
      ig1i = dissoc_ig1(i)

      m_ip(nelemxi) = dissoc_ip(i)
      m_uiidui(nelemxi) = ig1i * 0.661 / ig0i
      m_ccomp(nelemxi) = exp(cclogi/ECONST)

      !~     !--debugging--!
      !~     WRITE(LLL, '(1H ,5X,A4,8X,I5,3X, F10.3,5X, 2I5,3X,F10.5)')
      !~+     dissoc_ELEMS(I), NELEMXI, dissoc_IP(I),
      !~+     IG0I, IG1I, CCLOGI-main_AFSTAR
      !~     CALL LOG_DEBUG(LLL)
    end do

    !
    !*****INPUT D

    ! STARTING VALUE OF THE SOLUTION
    do 1400 i = 1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      m_p(nelemi) = 1.0e-20
    1400 continue

    m_p(Z_ELECTRON) = 1.0e-10

    !
    !*****INPUT E

    do 1020 ito = 1,modele%ntot
      theta = modele%teta(ito)
      tem = 5040.0/theta
      pg = modele%pg(ito)
      pglog = log10(pg)

      call die(tem,pg)

      m_pe = m_p(Z_ELECTRON)
      pelog = log10(m_pe)

      do 1303 i=1,dissoc_nmetal
        nelemi = dissoc_nelemx(i)

        fplog  = log10(m_fp(nelemi))
        dissoc_xp(ito,i) = m_p(nelemi)+1.0e-30
        plog   = log10( dissoc_xp(ito,i) )
        pdfpl  = plog - fplog
        if (mod(i,5)) 1303,1304,1303
        1304 continue
      1303 continue

      irl = 120
      do 1184 i=1,dissoc_nmetal
        nelemi = dissoc_nelemx(i)

        ! *Note* 1e-30 added to terms to avoid log(0)
        !        (MT): m_p,m_kp=0 should be avoided.
        plog   = log10(m_p(nelemi)+1.0e-30)
        kplog  = log10(m_kp(nelemi)+1.0e-30)
        pionl  = plog + kplog - pelog
        xlog   = pionl - pglog

        if (i .ne. dissoc_nmetal ) go to 1450
        iq  = i / 120
        ir  = dissoc_nmetal - iq * 120
        irl = ir / 3
        go to 1460

        1450 if (mod(i,120))  1184,1460,1184

        1460 nbl = 0

        do 1470  k1=1,120,3
          nbl = nbl + 1
          k2 = k1 + 1
          k3 = k1 + 2
          if ( nbl.eq.irl + 1)  go to 1480
          continue

          if (mod(nbl,5)) 1470,1500,1470
          1500 continue
        1470 continue

        go to 1184

        1480 continue
        irr = ir - irl*3
        if (irr .eq. 0)  go to 1184
        go to (1482,1484), irr

        1482 continue

        go to 1184

        1484 continue
      1184 continue

      irl = 120
      kd =-119
      do 1084 j=1,dissoc_nmol
        jcount = jcount + 1
        pmoll  = log10(m_ppmol(j)+1.0e-30)
        xlog   = pmoll - pglog

        if (j .ne. dissoc_nmol) go to 2450
        iq = j/120
        ir =  dissoc_nmol - iq*120
        irl = ir/3
        go to 2460

        2450 if (mod(j,120)) 2184,2460,2184

        2460 nbl = 0

        kd = kd + 120
        kf = kd + 119
        do 2470  k1=kd,kf,3
          nbl = nbl + 1
          k2 = k1 + 1
          k3 = k1 + 2
          if ( nbl.eq.irl + 1)  go to 2480
          continue

          if (mod(nbl,5)) 2470,2500,2470

          2500 continue
        2470 continue
        go to 2184

        2480 continue

        irr = ir - irl*3

        if (irr .eq. 0)  go to 2184

        go to (2482,2484), irr

        2482 continue

        go to 2184

        2484 continue
        2184 continue
      1084 continue
    1020 continue

    !_logging__
    !do i=1,4
    !  write(*,'(7e11.4)') (dissoc_xp(itx,i),itx=1,modele%ntot)
    !end do

    ! These pressure vectors are used explicitly in calculations special cases
    iz = find_atomic_symbol_dissoc('H ')
    sat4_pph = dissoc_xp(:, iz)
    iz = find_atomic_symbol_dissoc('O ')
    sat4_po = dissoc_xp(:, iz)


!20160920-    iz = find_atomic_symbol_dissoc('C ')
!20160920-    sat4_ppc2 = dissoc_xp(:, iz)
!20160920-    iz = find_atomic_symbol_dissoc('N ')
!20160920-    sat4_pn = dissoc_xp(:, iz)
!20160920-    iz = find_atomic_symbol_dissoc('O ')
!20160920-    sat4_po = dissoc_xp(:, iz)
!20160920-    iz = find_atomic_symbol_dissoc('A ')  ! Yeah, "A" is 13C
!20160920-    sat4_pc13 = dissoc_xp(:, iz)
!20160920-    iz = find_atomic_symbol_dissoc('TI')
!20160920-    sat4_pti = dissoc_xp(:, iz)
!20160920-    iz = find_atomic_symbol_dissoc('MG')
!20160920-    sat4_pmg = dissoc_xp(:, iz)
!20160920-    iz = find_atomic_symbol_dissoc('FE')
!20160920-    sat4_pfe = dissoc_xp(:, iz)


!original      do itx = 1,modele%ntot
!original        sat4_pph(itx)=dissoc_xp(itx,1)
!original        sat4_ppc2(itx)=dissoc_xp(itx,3)
!original        sat4_pn(itx)=dissoc_xp(itx,4)
!original        sat4_po(itx)=dissoc_xp(itx,5)
!original        sat4_pc13(itx)=dissoc_xp(itx,6)
!original        sat4_pti(itx)=dissoc_xp(itx,15)
!original        sat4_pmg(itx)=dissoc_xp(itx,8)
!original        sat4_pfe(itx)=dissoc_xp(itx,16)
!original      end do
  end


  !=======================================================================================
  ! DIE9 ?what?

  subroutine die(tem, pg)
    real*8, intent(in) :: tem, & ! ?doc?
     pg ! ?doc?
    real*8, dimension(MAX_DISSOC_Z) :: fx, dfx, z, prev
    real*8, dimension(MAX_DISSOC_NMETAL) :: wa
    real*8 aplogj, atomj, delta, df, dhh, EPSDIE, &
     f, fph, heh, hkp, perev, pglog, ph, pmolj, pmoljl, q, r, s, &
     spnion, t, tem25, u, x, xr, pph, phh
    integer i, imaxp1, iterat, j, k, km5, m, mmaxj, nelemi, nelemj, &
     natomj, niter

    EPSDIE = 5.0e-3
    t      = 5040.0/tem
    pglog  = log10(pg)

    heh    = m_ccomp(Z_HE)/m_ccomp(Z_H)  ! Helium-to-Hydrogen ratio by number

    ! EVALUATION OF LOG m_kp(MOL)
    do 1025 j =1, dissoc_nmol
      aplogj = dissoc_c(j,5)
      do 1026 k=1,4
        km5 = 5-k
        aplogj = aplogj*t + dissoc_c(j,km5)
      1026 continue
      m_apmlog(j) = aplogj
    1025 continue

    dhh = (((0.1196952e-02*t-0.2125713e-01)*t+0.1545253e+00)*(-0.5161452e+01))*t+0.1277356e+02
    dhh = exp(dhh/ECONST)

    ! EVALUATION OF THE IONIZATION CONSTANTS
    tem25 = tem**2*sqrt(tem)
    do 1060 i = 1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      m_kp(nelemi) =m_uiidui(nelemi)*tem25*exp(-m_ip(nelemi)*t/ECONST)
    1060 continue

    hkp = m_kp(Z_H)
    if (t-0.6) 1084, 1072, 1072

    ! PRELIMINARY VALUE OF PH AT HIGH TEMPERATURES
    1084 continue
    pph = sqrt(hkp *(pg/(1.0+heh)+hkp ))-hkp
    ph  = pph**2/hkp
    go to 1102

    ! PRELIMINARY VALUE OF PH AT LOW TEMPERATURES
    1072 continue
    if (pg/dhh - 0.1) 1073, 1073, 1074

    1073 continue
    ph = pg/(1.0+heh)
    go to 1102


    1074 continue
    ph = 0.5*(sqrt(dhh*(dhh+4.0*pg/(1.0+heh)))-dhh)

    ! EVALUATION OF THE FICTITIOUS PRESSURES OF HYDROGEN
    1102 continue
    u = (1.0+2.0*heh)/dhh
    q = 1.0+heh
    r = (2.0+heh)*sqrt(hkp )
    s = -1.0*pg
    x = sqrt(ph)
    iterat = 0

    1103 continue
    f  = ((u*x**2+q)*x+r)*x+s
    df = 2.0*(2.0*u*x**2+q)*x+r
    xr = x-f/df
    if (abs((x-xr)/xr)-EPSDIE) 1105, 1105, 1106

    1106 continue
    iterat=iterat+1
    if (iterat-50) 1104,1104,1107

    1107 continue

    6108 format(1h1,'Not converge in die  tem=', f9.2, 5x, 'pg=', e12.5, 5x 'x1=', &
                e12.5, 5x,'x2=', e12.5, 5x, 'ph=', e12.5)
    write(lll, 6108) tem,pg,x,xr,ph
    call log_warning(lll)

    go to 1105

    1104 continue
    x = xr

    go to 1103

    1105 continue
    ph  = xr**2
    phh = ph**2/dhh
    pph = sqrt(hkp *ph)
    fph = ph+2.0*phh+pph

    ! ISSUE Z=100 within dissoc.dat is only possible at the metals part (at the molecules part the Z slots have only 2 digits).
    ! THe current dissoc.dat has no Z=100 (neither 99).
    ! Is this a remaining fragment of code? My hint comes from the fact that Z_ELECTRON=99 is addressed several times, but Z_H_STAR=100 is not.
    m_p(Z_H_STAR) = pph


    ! EVALUATION OF THE FICTITIOUS PRESSURE OF EACH ELEMENT
    do 1070 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      m_fp(nelemi) = m_ccomp(nelemi)*fph
    1070 continue

    ! CHECK OF INITIALIZATION
    m_pe = m_p(Z_ELECTRON)



    if(ph-m_p(Z_H)) 1402,1402,1401

    1401 continue
    do 1403 i=1,dissoc_nmetal
      ! ISSUE: what if some NELEMI=Z_ELECTRON=99? THen m_p(99) will no longer be equal to m_pe
      nelemi=dissoc_nelemx(i)
      m_p(nelemi) = m_fp(nelemi)*exp(-5.0*t/ECONST)
    1403 continue
    m_p(Z_H) = ph   ! ISSUE: overwriting m_p(1)

    ! Update: kept as-was
    ! *Note* m_p was being divided by 100 over and over at each j (molecule).
    ! This division has been taken out of loop, but is still an issue, since it is unclear *why* this division is being done.
    ! ISSUE ask blb being divided by 100 is still an issue
    ! do m =1,MAX_DISSOC_Z
    !   m_p(m)=1.0e-2*m_p(m)
    ! end do


    ! RUSSELL EQUATIONS
    1402 continue
    niter = 0
    1040 continue
    do 1030 i =1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      fx(nelemi) = -m_fp(nelemi)+m_p(nelemi)*(1.0 + m_kp(nelemi)/m_pe)  ! ISSUE if NELEMI=99, m_p(99) and m_pe are potentially not the same thing! Is this alright?
      dfx(nelemi) = 1.0 + m_kp(nelemi)/m_pe
    1030 continue

    spnion = 0.0
    do 1041 j=1,dissoc_nmol
      mmaxj  = dissoc_mmax(j)
      pmoljl = -m_apmlog(j)
      do 1042 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        pmoljl = pmoljl + float(natomj)*log10(m_p(nelemj))
      1042 continue

      if(pmoljl - (pglog+1.0) ) 1046,1046,1047

      1047 continue
      do 1048 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        pmoljl = pmoljl + float(natomj)*(-2.0)

        ! For each j, divides all used elements in m_p by 100.
        ! This is necessary for convergence of the molecular equilibrium.
        m_p(nelemj) = 1.0e-2*m_p(nelemj)

      1048 continue

      1046 pmolj = exp(pmoljl/ECONST)
      do 1044 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        atomj = float(natomj)

        if (nelemj .eq. Z_ELECTRON) then  ! ISSUE This bit suggests that Z=99 is allowed in the molecules part
          spnion = spnion + pmolj
        end if

        do 1043 i=1,dissoc_nmetal
          nelemi = dissoc_nelemx(i)
          if(nelemj .eq. nelemi) go to 1045
          go to 1043
          1045 fx(nelemi) = fx(nelemi) + atomj*pmolj
          dfx(nelemi) = dfx(nelemi) + atomj**2*pmolj/m_p(nelemi)
        1043 continue
      1044 continue
      m_ppmol(j) = pmolj
    1041 continue

    ! SOLUTION OF THE RUSSELL EQUATIONS BY NEWTON-RAPHSON METHOD
    do 2001 i=1,dissoc_nmetal
      nelemi=dissoc_nelemx(i)
      wa(i)=log10(m_p(nelemi)+1.0e-30)
    2001 continue

    imaxp1 = dissoc_nmetal+1
    wa(imaxp1) = log10(m_pe+1.0e-30)
    delta = 0.0
    do 1050 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      prev(nelemi) = m_p(nelemi) - fx(nelemi)/dfx(nelemi)
      prev(nelemi) = abs(prev(nelemi))

      if (prev(nelemi) .lt. 1.0e-30) prev(nelemi)=1.0e-30

      z(nelemi) = prev(nelemi)/m_p(nelemi)
      delta = delta + abs(z(nelemi) - 1.0)

      if (dissoc_switer) 2500,2500,2501

      2501 continue
      m_p(nelemi) = (prev(nelemi) + m_p(nelemi) )*0.5
      go to 1050

      2500 continue
      m_p(nelemi) = prev(nelemi)
    1050 continue


    ! IONIZATION EQUILIBRIUM
    perev = 0.0
    do 1061 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      perev = perev + m_kp(nelemi)*m_p(nelemi)
    1061 continue

    perev = sqrt(perev/(1.0+spnion/m_pe))
    delta = delta + abs((m_pe-perev)/m_pe)
    m_pe = (perev + m_pe)*0.5  ! Note that it has an equivalence with the last element of m_p
    m_p(Z_ELECTRON)=m_pe

    if (delta - dissoc_eps) 1051,1051,1052

    1052 continue
    niter = niter+1
    if (niter-dissoc_nimax) 1040,1040,1054

    1054 continue
    6055 format(1h0,39h *Does not converge after iterations of,i4)
    write(lll,6055) dissoc_nimax
    call log_warning(lll)

    1051 continue
  end
end





!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Contains subroutines filter_molecules() and filter_atoms()
!
! Prefixes:
!   - km_f_ -- calculated by filter_molecules()
!   - atoms_f_ -- calculated by filter_atoms()

module filters
  use file_atoms
  use file_molecules
  implicit none


  ! Analogue to atoms_zinf
  ! All molecular lines will be calculated until this value (angstrom) to the
  ! left and to the right of the line centre.
  real*8, parameter :: KM_ALARGM = 0.1

  !=====
  ! km_f_*Variables filled by filter_molecules()
  !=====

  integer km_f_mblend  ! Total number of spectral lines *filtered in*

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_f_lmbdam, & ! ?doc?
    km_f_sj,     & ! ?doc?
    km_f_jj,     & ! ?doc?
    km_f_mm        ! Replicates km_mm(molidx) for all selected lines of molecule molidx.
                   ! Redundant information but simplifies use. Used in synthesis::selekfh()
  integer, dimension(MAX_KM_F_MBLEND) :: &
    km_f_molidx, & ! Molecule index for each line; allows access to any molecule-wise data
    km_f_transidx  ! Transition index for each line; allows access to any transition-wise data


  !------
  ! These two arrays contain indexes pointing at km_f_lmbdam, km_f_sj, km_f_jj, km_f_mm
  !------

  ! Contains the index of the last line of each set of lines within km_f_lmbdam, km_f_sj and km_f_jj
  ! **for the current molecule** molidx
  !
  ! Augmented: first row is 0 (ZERO) or repeats last element of previous column
  !
  ! km_f_ln(i+1, j) represents to i-th transition of j-th molecule
  integer :: km_f_ln(MAX_KM_NV_PER_MOL+1, MAX_NUM_MOL)

  !=====
  ! atoms_f_*Variables filled by filter_atoms()
  !=====

  ! *atoms file*, filtered variables
  ! Very similar to above; differences are
  ! - single underscore
  ! - additional variable "gf", which equals 10**algf
  integer atoms_f_nblend ! ?doc?
  character*2 atoms_f_elem(MAX_ATOMS_F_NBLEND) ! atomic symbol (right-alignes, uppercase)
  integer, dimension(MAX_ATOMS_F_NBLEND) :: &
   atoms_f_ioni ! ?doc?
  real*8, dimension(MAX_ATOMS_NBLEND) :: &
   atoms_f_lambda,       & ! ?doc?
   atoms_f_kiex,         & ! ?doc?
   atoms_f_algf,         & ! ?doc?
   atoms_f_ch,           & ! ?doc?
   atoms_f_gr,           & ! ?doc?
   atoms_f_ge,           & ! ?doc?
   atoms_f_zinf,         & ! ?doc?
   atoms_f_abondr_dummy, & ! ?doc?
   atoms_f_gf,           & ! ?doc?
   atoms_f_abonds_abo      ! ?doc?

contains

  !=======================================================================================
  ! Sweeps km_* to populate a few km_f_* depending on the interval lzero-lfin

  subroutine filter_molecules(lzero, lfin)
    ! Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    ! Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    real*8 :: lambda
    integer molidx,          &  ! Counts molecule id, from 1 to km_number
            j_dummy, j_set,  &
            i_line,          &  ! Index of km_lmbdam, km_sj, km_jj
            i_filtered          ! Counts number of filtered lines (molecule-independent);
                                ! index of km_f_lmbdam, km_f_sj, km_f_jj
    integer num_lambdas
    logical flag_in

    write(lll, *) ENTERING, 'filter_molecules()'
    call log_debug(lll)

    i_filtered = 0  ! Current *filtered-in* spectral line. Keeps growing (not reset when the molecule changes). Related to old "L"
    i_line = 1
    do molidx = 1, km_number
      km_f_ln(1, molidx) = i_filtered  ! first row contain number of lines already filtered

      write(lll, *) 'molecule idx', molidx, '; titulo: ',  trim(km_titulo(molidx)), &
       '; number of prospective lambdas: ', km_lines_per_mol(molidx)
      call log_debug(lll)


      ! Counters starting with "j_" restart at each molecule
      j_set = 1   ! Current "set-of-lines"
      flag_in = .FALSE.  ! Whether has filtered in at least one line
      do j_dummy = 1, km_lines_per_mol(molidx)
        lambda = km_lmbdam(i_line)

        if ((lambda .ge. lzero-KM_ALARGM) .and. (lambda .le. lfin+KM_ALARGM)) then
          ! Filters in a new spectral line!
          i_filtered = i_filtered+1

          ! spill check
          if (i_filtered .gt. MAX_KM_F_MBLEND) then
            call log_and_halt('filter_molecules(): number of filtered lines '//&
             'exceeded maximum of MAX_KM_F_MBLEND='//int2str(MAX_KM_F_MBLEND), .true.)
          end if

          km_f_lmbdam(i_filtered) = lambda
          km_f_sj(i_filtered) = km_sj(i_line)
          km_f_jj(i_filtered) = km_jj(i_line)
          km_f_mm(i_filtered) = km_mm(molidx)  ! TODO perhaps this will not be
          km_f_molidx(i_filtered) = molidx
          km_f_transidx(i_filtered) = j_set
        end if

        if (i_line .eq. km_ln(j_set, molidx)) then
          ! Reached last line of current set of lines
          km_f_ln(j_set+1, molidx) = i_filtered  ! Yes, j_set+1, not j_set, remember km_f_ln first row is set apart.

          num_lambdas = km_f_ln(j_set+1, molidx)-km_f_ln(j_set, molidx)

          !write(lll, *) 'number of SELECTED lambdas for transition ', j_set, ': ', num_lambdas
          !call log_debug(lll)


          j_set = j_set+1
        end if

        i_line = i_line+1
      end do
    end do !--end of MOLID loop--!

    km_f_mblend = i_filtered

    write(lll, *) LEAVING, 'filter_molecules() summary: [', lzero, ', ', lfin, '] --> ', i_filtered, '/', km_lines_total
    call log_debug(lll)
  end


  !=======================================================================================
  ! Selects only spectral lines within range lzero, lfin + performs "inner join".
  !
  ! Populates variables atoms_f_*

  subroutine filter_atoms(lzero, lfin)
    ! Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    ! Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    integer j, k

    k = 0
    do j = 1, atoms_nblend
      if((atoms_lambda(j) .le. lfin+atoms_zinf(j)) .and. (atoms_lambda(j) .ge. lzero-atoms_zinf(j))) then
        k = k+1

        ! spill check: checks if exceeds maximum number of elements allowed
        if (k .gt. MAX_ATOMS_F_NBLEND) then
          call log_and_halt('filter_atoms(): exceeded maximum of MAX_ATOMS_F_NBLEND='//&
           int2str(MAX_ATOMS_F_NBLEND)//' spectral lines')
        end if

        !Filters in
        atoms_f_elem(k)   = atoms_elem(j)
        atoms_f_ioni(k)   = atoms_ioni(j)
        atoms_f_lambda(k) = atoms_lambda(j)
        atoms_f_kiex(k)   = atoms_kiex(j)
        atoms_f_algf(k)   = atoms_algf(j)
        atoms_f_gf(k)     = 10.**atoms_algf(j)
        atoms_f_ch(k)     = atoms_ch(j)
        atoms_f_gr(k)     = atoms_gr(j)
        atoms_f_ge(k)     = atoms_ge(j)
        atoms_f_zinf(k)   = atoms_zinf(j)
        atoms_f_abonds_abo(k) = atoms_abonds_abo(j)
        atoms_f_abondr_dummy(k) = atoms_abondr_dummy(j)
      end if
    end do
    atoms_f_nblend = k
  end
end




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Contains subroutine kapmol_()
!
! Calculated variables have prefix "km_c_"

module kapmol
  use dissoc
  use filters
  use misc_math
  implicit none

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_c_gfm ! (BLB) moleculer "gf", corresponding to Eistein's coefficient (?doc?); in sync with km_f_sj etc

  real*8, dimension(MAX_KM_F_MBLEND, MAX_MODELES_NTOT) :: &
   km_c_pnvj ! (BLB) partial pressure; In sync with km_f_sj etc

  real*8, private, pointer, dimension(:) :: ppa, pb
!   private point_ppa_pb

contains

  !=======================================================================================
  ! Calculates the molecular absorption coefficient.

  subroutine kapmol_()
    real*8 t5040, psi
    real*8 csc
    real*8 fe, do_, mm, am, bm, ua, ub, te, cro, rm
    real*8 qv, gv, bv, dv, facto
    integer j_set, l, l_ini, l_fin, n, nnv, molidx, iz0, iz1

    real*8, parameter :: C2 = 8.8525E-13

    call log_debug(ENTERING//' kapmol()')

    do molidx = 1, km_number

      ! Assigns address of variable PPA and PB depending on atomic symbols of molecule
      iz0 = find_atomic_symbol_dissoc(km_symbols(1, molidx))
      ppa => dissoc_xp(:, iz0)
      iz1 = find_atomic_symbol_dissoc(km_symbols(2, molidx))
      pb => dissoc_xp(:, iz1)

      ! print *, 'WWXWW molidx', molidx, '; iz0', iz0, '; iz1', iz1, '; symbol0 ', &
      ! km_symbols(1, molidx), '; symbol1 ', km_symbols(2, molidx)

      nnv = km_nv(molidx)
      fe  = km_fe(molidx)
      do_ = km_do(molidx)
      mm  = km_mm(molidx)
      am  = km_am(molidx)
      bm  = km_bm(molidx)
      ua  = km_ua(molidx)
      ub  = km_ub(molidx)
      te  = km_te(molidx)
      cro = km_cro(molidx)

      !======
      ! This part of the code calculates km_PNVL
      rm = am*bm/mm
      do n = 1,modele%ntot
        t5040 = modele%teta(n)/5040
        psi = do_*modele%teta(n)+2.5*log10(modele%teta(n))-1.5*log10(rm)-&
              log10(ua*ub)-13.670
        psi = 10.**psi

        do j_set = 1,nnv
          qv = km_qqv(j_set, molidx)
          gv = km_ggv(j_set, molidx)
          bv = km_bbv(j_set, molidx)
          dv = km_ddv(j_set, molidx)

          ! Note that l_ini may be > l_fin, meaning that the current set-of-lines has no
          ! selected lines for current lzero-lfin intervfal
          l_ini = km_f_ln(j_set, molidx)+1
          l_fin = km_f_ln(j_set+1, molidx)

          ! l is index within km_f_lmbdam, km_f_sj and km_f_jj
          do l= l_ini, l_fin
            ! PC2003: default value for CSC does not exist physically
            csc = exp(-H*C/KB*t5040*(te+gv+bv*(km_f_jj(l)+1)*km_f_jj(l)))*   &
                  (2.-cro)*(2.*km_f_jj(l)+1.)*                                             &
                  exp(H*C/KB*t5040*(dv*(km_f_jj(l)*(km_f_jj(l)+1))**2+2.*bv))

            km_c_pnvj(l,n) = csc*psi*ppa(n)*pb(n)/sat4_pph(n)
          end do


          ! Takes advantage of current j_set loop so it is not necessary to create
          ! another double loop as in the original KAPMOL() to calculate km_c_gfm
          if (n .eq. 1) then
            ! Because gfm does not depend on n (i.e., it is atmospheric-layer-independent),
            ! this part runs only once, when n is 1.
            facto = km_fact(j_set, molidx)
            do l= l_ini, l_fin
              km_c_gfm(l) = C2*((1.e-8*km_f_lmbdam(l))**2)*fe*qv*km_f_sj(l)*facto
              if (km_c_gfm(l) .eq. 0.) then
                ! I think this is actually not a problem at all
                ! write (*,*) "MOLECULAR GF IS ZERO!!!! HOW COME???"
                ! write (*,*) trim(km_comments(km_f_molidx(l)))
                ! write (*,*) 'set-of-lines:', km_f_transidx(l)
                ! write (*,*) facto, C2, fe, qv, km_f_lmbdam(l), km_f_sj(l)
                ! stop -1
              end if
            end do
          end if
        end do
      end do
    end do


    call log_debug(LEAVING//' kapmol()')
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module pfantlib
  use dimensions
  use logging
  use misc
  use welcome
  use misc_math
  use flin
  use config

  use file_main
  use file_dissoc
  use file_models
  use file_hmap
  use file_toh
  use file_absoru2
  use file_abonds
  use file_partit
  use file_atoms
  use file_molecules

  use absoru
  use turbul

  use dissoc
  use filters
  use kapmol
end

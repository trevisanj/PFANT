!> Miscellanea of [*not* mathematical] functions
!>
!> @note Misc math functions are in module misc_math

module misc
  use logging
  implicit none

  !> Maximum number of rows in a file with comments, e.g. thmap.dat.
  !> Used in misc.f90
  integer, parameter :: MAX_FILE_ROWS=1000
contains
  !> Converts a string to lower case.
  !>
  !> @note Works on A-Z letters only (does not handle letter modifiers such as acute,
  !>       tilde etc)
  !>
  !> Source: http://stackoverflow.com/questions/10759375/how-can-i-write-a-to-upper-or-to-lower-function-in-f90

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

  !> Converts a string to upper case.
  !>
  !> @note Works on a-z letters only (does not handle letter modifiers such as acute,
  !>       tilde etc)
  !>
  !> Source: http://stackoverflow.com/questions/10759375/how-can-i-write-a-to-upper-or-to-lower-function-in-f90

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

  !> Converts an integer to string
  !>
  !> @note @c x is limited to 80 digits

  pure function int2str(x) result (string)
    integer, intent(in) :: x
    character(:), allocatable :: string
    character(80) :: ch

    write(ch,*) x
    string = trim(adjustl(ch))
  end function


  !> Converts a real*8 number to string
  !>
  !> @todo improve representation

  pure function real82str(x) result (string)
    real*8, intent(in) :: x
    character(:), allocatable :: string
    character(80) :: ch

    write(ch,*) x
    string = trim(adjustl(ch))
  end function

  !> Converts a float number to string
  !>
  !> @todo improve representation

  pure function real42str(x) result (string)
    real*4, intent(in) :: x
    character(:), allocatable :: string
    character(80) :: ch

    write(ch,*) x
    string = trim(adjustl(ch))
  end function


  !> Converts a logical value to string
  !>
  !> @li .true. is converted to "T"
  !> @li .false. is converted to "F"

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
  !> Converts a logical value to 0/1

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
  !> Converts an integer to logical. Accepts only 0 or 1
  !>
  !> If argument is not (0 or 1), halts the program

  function int2logical(x) result (y)
    integer, intent(in) :: x
    logical :: y

    if (x .eq. 0) then
      y = .false.
    elseif (x .eq. 1) then
      y = .true.
    else
      call pfant_halt('int2logical() accepts only 0/1, not '//int2str(x))
    end if
  end function

  !=======================================================================================  !> Trims spaces at the right and adds a final slash, if the latter is not present.
  !>
  !> Examples:
  !> @verbatim
  !> input             --> result
  !> --------------------------------
  !> "qwe/asd        " --> "qwe/asd/"
  !> "qwe/asd/       " --> "qwe/asd/"
  !> "qwe/asd/"        --> "qwe/asd/"
  !> @endverbatim

  function trim_and_add_slash(x) result(y)
    character(1) :: BACKSLASH = char(92)  ! Doxygen doesn't like a backslash appearing in the code
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
  !> Creates a logical vector indicating which rows should be skipped in a text file.
  !>
  !> This subroutine opens a text file and sweeps it until the end, then closes it.
  !> If row i starts with a "#" or is blank, skip_row(i) will be .true., otherwise
  !> .false.
  !>
  !> n is the total number of rows in file.

  subroutine map_file_comments(path_to_file, skip_row, n)
    integer, parameter :: UNIT_=195
    character(len=*), intent(in) :: path_to_file
    !> element i  (i=1,n) will be .true. if row i is a comment line (starts with "#"),
    !> or is blank
    logical, intent(out) :: skip_row(MAX_FILE_ROWS)
    !> Number of rows in text file
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
        call pfant_halt('Increase MAX_FILE_ROWS (='//int2str(MAX_FILE_ROWS)//')')

      s_temp = trim(adjustl(s_temp0))  ! tolerant with rows starting with spaces

      if (len(s_temp) .eq. 0) then
        skip = .true.
      else if (s_temp(1:1) .eq. '#') then
        skip = .true.
      else
        skip = .false.
      end if
      skip_row(n_temp) = skip

      write(*,*) 'skip row ', n_temp, '? ', skip
    end do

    10 continue
    n = n_temp
    close(UNIT_)
  end
end module misc




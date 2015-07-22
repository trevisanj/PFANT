!> Miscellanea of [*not* mathematical] functions
!>
!> @note Misc math functions are in module misc_math

module misc
  use logging
  implicit none
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
  end function to_lower

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


  !> Converts a logical value to 0/1

  pure function logical2int(x) result (y)
    logical, intent(in) :: x
    integer, intent(out) :: y

    if (x) then
      y = 1
    else
      y = 2
    end if
  end function

  !> Converts an integer to logical. Accepts only 0 or 1
  !>
  !> If argument is not (0 or 1), halts the program

  pure function int2logical(x) result (y)
    integer, intent(in) :: x
    logical, intent(out) :: y

    if (x .eq. 0) then
      y = .false.
    elseif (x .eq. 1) then
      y = .true.
    else
      call pfant_halt('int2logical() accepts only 0/1, not '//int2str(x))
    end if
  end function



  !> Trims spaces at the right and adds a final slash, if the latter is not present.
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
end module misc




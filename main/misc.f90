!> Miscellanea of [*not* mathematical] functions
!>
!> @note Misc math functions are in module misc_math

module misc
contains
  !> Converts a string to lower case.
  !>
  !> @note Works on A-Z letters only (does not handle letter modifiers such as acute,
  !>       tilde etc)
  !>
  !> Source: http://stackoverflow.com/questions/10759375/how-can-i-write-a-to-upper-or-to-lower-function-in-f90

  pure function to_lower(str) result (string)
      implicit none
      character(*), intent(in) :: str
      character(len(str))      :: string
      integer :: ic, i
      character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

      string = str
      do i = 1, len_trim(str)
          ic = index(cap, str(i:i))
          if (ic > 0) string(i:i) = low(ic:ic)
      end do
  end function to_lower

  !> Converts an integer to string
  !>
  !> @note @c i is limited to 80 digits

  pure function int2str(i) result (string)
    implicit none
    integer, intent(in) :: i
    character(:), allocatable :: string
    character(80) :: ch

    write(ch,*) i
    string = trim(adjustl(ch))
  end function


  !> Converts a float number to string

  pure function float2str(f) result (string)
    implicit none
    real*8, intent(in) :: f
    character(:), allocatable :: string
    character(80) :: ch

    write(ch,*) f
    string = trim(adjustl(ch))
  end function

end module misc

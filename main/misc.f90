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

  pure function to_lower(x) result (string)
      implicit none
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

  !> Converts an integer to string
  !>
  !> @note @c x is limited to 80 digits

  pure function int2str(x) result (string)
    implicit none
    integer, intent(in) :: x
    character(:), allocatable :: string
    character(80) :: ch

    write(ch,*) x
    string = trim(adjustl(ch))
  end function


  !> Converts a float number to string
  !>
  !> @todo improve representation

  pure function float2str(x) result (string)
    implicit none
    real*8, intent(in) :: x
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
    implicit none
    logical, intent(in) :: x
    character(:), allocatable :: string
    character(80) :: ch

    if (x) then
      string = 'T'
    else
      string = 'F'
    end if
  end function



end module misc

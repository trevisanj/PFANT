!> Command-line parser.
!>
!> I took the original from the Fortran wiki:
!> http://fortranwiki.org/fortran/show/Command-line+arguments
!>
!> T

module options
  implicit none

  type option
    !> Long name.
    character(len=100) :: name
    !> Does the option require an argument?
    logical :: has_arg
    !> Corresponding short name.
    character :: chr
    !> Description.
    character(len=500) :: descr
    !> Argument name, if required.
    character(len=20) :: argname
!  contains
!    procedure :: print => print_opt
  end type

  !> Configurable unit to output command-line parsing errors
  integer :: error_unit = 6


  private give_error  ! logs and halts

  integer, private, parameter :: MAX_TEMP_PTR=300  ! In practice: maximum size of options_ string

contains

! TODO UPDATE DOCUMENTATION

  !> Parse command line options. Options and their arguments must come before
  !> all non-option arguments. Short options have the form "-X", long options
  !> have the form "--XXXX..." where "X" is any character. Parsing can be
  !> stopped with the option '--'.
  !> The following code snippet illustrates the intended use:
  !> \code
  !> do
  !>   ! things changed
  !> end do
  !> \endcode
  subroutine getopt (options, chr, name, arg, arglen, stat, &
      offset, remain)
    use iso_fortran_env, only: error_unit

    !> Array, items are of option type. Each option may have either a long name
    !> (accessible in  the form '--XXXX...'), a single-char short name ("-X"),
    !> or both. Be careful not to repeat yourself, uniqueness of name is not
    !> checked.
    type(option), intent(in) :: options(:)

    !> Contains the short name of the option that was parsed.
    character, intent(out) :: chr

    !> If stat is not 1, chr contains the option character that was parsed.
    !> Otherwise its value is undefined.
    character(len=*), intent(out) :: name

    !> If the parsed option requires an argument, arg contains
    !> the first len(arg) (but at most 500) characters of that argument.
    !> Otherwise its value is undefined. If the arguments length exceeds 500
    !> characters and err is .true., a warning is issued.
    character(len=*), intent(out) :: arg

    !> If the parsed option requires an argument, arglen contains
    !> the actual length of that argument. Otherwise its value is undefined.
    !> This can be used to make sure the argument was not truncated by the
    !> limited length of arg.
    integer, intent(out) :: arglen

    !> Status indicator. Can have the following values:
    !>   -  0: An option was successfully parsed.
    !>   -  1: Parsing stopped because a '--' was encountered
    !>         (not an error, but nothing was parsed).
    !>   -  2: Parsing stopped because a non-option was encountered
    !>         (not an error, but nothing was parsed).
    !>   -  3: Parsing stopped because there are no arguments left
    !>         (not an error, but nothing was parsed).
    !>   .
    !> Its value is never undefined.
    integer, intent(out) :: stat

    !> If stat is 1, offset contains the number of the argument before the
    !> first non-option argument, i.e. offset+n is the nth non-option argument.
    !> If stat is not 1, offset contains the number of the argument that would
    !> be parsed in the next call to getopt. This number can be greater than
    !> the actual number of arguments.
    integer, intent(out), optional :: offset

    !> If stat is 1, remain contains the number of remaining non-option
    !> arguments, i.e. the non-option arguments are in the range
    !> (offset+1:offset+remain). If stat is not 1, remain is undefined.
    integer, intent(out), optional :: remain

    integer, save :: pos = 1, cnt = 0
    character(len=64), save :: arg0  ! used for option name
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

    call get_command_argument (pos, arg0, length)
    flagLong = .false.


    !-- This part tries to find the "option id"

    if (arg0(1:1) == '-') then
      ! Argument is an option

      ch = arg0(2:2)


      if (ch /= '-' .and. len_trim(arg0) > 2) then
        ! single dash, but name too long ('-xxxx...')
        write(lll, *) 'Invalid name for short option: "', trim(arg0(2:))
        call give_error(lll)
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
        call give_error(lll)
      end if

      if (.not. options(id)%has_arg) then
        st = 0
        goto 10
      end if

      if (pos == cnt) then
        write(lll, *) 'Option "', trim(arg0), '" requires an argument, but none found'
        call give_error(lll)
      end if

      pos = pos + 1
      call get_command_argument(pos, arg1, length)

      ! make sure argument is not an option
      if (arg1(1:1) == '-') then
        write(lll, *) 'Option "', trim(arg0), '" requires an argument, but option found'
        call give_error(lll)
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
    if (stat == 0) then
      chr    = options(id)%chr
      name   = options(id)%name
      stat   = st
      remain = cnt-pos
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

  !============================================================================

  !> Print an option in the style of a man page. I.e.
  !> \code
  !> -o arg
  !> --option arg
  !>    description.................................................................
  !>    ............................................................................
  !> \endcode
  subroutine print_opt (opt, unit)
    !> the option
    type(option), intent(in) :: opt
    !> logical unit number
    integer, intent(in) :: unit

    integer :: l, c1, c2

    if (opt%has_arg) then
      write (unit, '(1x,"-",a,1x,a)') opt%chr, trim(opt%argname)
      write (unit, '(1x,"--",a,1x,a)') trim(opt%name), trim(opt%argname)
    else
      write (unit, '(1x,"-",a)') opt%chr
      write (unit, '(1x,"--",a)') trim(opt%name)
    end if
    l = len_trim(opt%descr)

    ! c1 is the first character of the line
    ! c2 is one past the last character of the line
    c1 = 1
    do
      if (c1 > l) exit
      ! print at maximum 4+76 = 80 characters
      c2 = min(c1 + 76, 500)
      ! if not at the end of the whole string
      if (c2 /= 500) then
        ! find the end of a word
        do
          if (opt%descr(c2:c2) == ' ') exit
          c2 = c2-1
        end do
      end if
      write (unit, '(4x,a)') opt%descr(c1:c2-1)
      c1 = c2+1
    end do

  end subroutine

  !> Logs error and halts
  !> This routine was created to standardize the "giving error" behaviuor
  subroutine give_error(lll)
    !> Error message
    character(len=*) :: lll

    write(error_unit, *) lll
    stop
  end subroutine

end module




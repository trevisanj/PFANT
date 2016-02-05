!> Miscellanea of routines.
!>
module misc
  use logging
  use dimensions
  implicit none

  !> Maximum number of rows in a file with comments, e.g. hmap.dat.
  !> Used in misc.f90
  integer, parameter :: MAX_FILE_ROWS=1000
contains

  !> Checks if hydrogen line interval overlaps with calculation interval
  !>
  !> Overlap is checked for these two intervals:
  !> @verbatim
  !> [clam-H_LINE_WIDTH, clam+H_LINE_WIDTH]  and
  !>
  !> [llzero-LAMBDA_STRETCH, llfin+LAMBDA_STRETCH]
  logical function h_line_is_inside(clam, llzero, llfin) result(res)
    real*8, intent(in) :: &
     clam,   & !< central lambda of a hydrogen line
     llzero, & !< lower boundary of calculation interval, probably taken from dfile:main
     llfin     !< lower boundary of calculation interval, probably taken from dfile:main

    res = .false.
    if (clam+H_LINE_WIDTH .ge. llzero-LAMBDA_STRETCH .and. &
        clam-H_LINE_WIDTH .le. llfin+LAMBDA_STRETCH) then
      res = .true.
    end if
  end

  !> Adjusts a 2-character string containing atomic symbol to right-aligned uppercase
  !>
  !> This is used to prevent against left-alignment/lovercase  in specifying the atomic
  !> symbols in several input data files.
  !>
  !> ExampleL: "O ", " o", "o ", all will result in " O"

  function adjust_atomic_symbol(elem) result(res)
    character(len=*), intent(in) :: elem
    character(len=2) :: res
    if (len(elem) .ne. 2) &
      call pfant_halt('adjust_atomic_symbol(): got '//int2str(len(elem))//&
       '-char string (wants 2-char string)', is_assertion=.true.)
    res = to_upper(adjustr(elem))
  end

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

  pure function real82str(x, dec) result (string)
    real*8, intent(in) :: x
    !> Number of decimal places (optional). If not passed, will use Fortran "*" formatting.
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

  !> Converts a real*4 number to string

  pure function real42str(x, dec) result (string)
    real*4, intent(in) :: x
    !> Number of decimal places (optional). If not passed, will use Fortran "*" formatting.
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
    end do

    10 continue
    n = n_temp
    close(UNIT_)
  end


  !> Parses string into array of integers
  !>
  !> Examples of valid strings:
  !> @verbatim
  !> '[1, 2, 3, 4]'
  !> '1, 2, 3, 4'
  !> '1  2 3     4'
  !> 'kdjd1sdkljfsd2shjdfkl3sdfklsdfjkl4sdlk'
  !> @endverbatim
  !> The examples above all result in <code>(1, 2, 3, 4)</code>.
  !>
  !> Very tolerant: everything that is not 0-9 is considered a separator


  subroutine parse_int_array(str, molidxs, n)
    use logging
    !> String to be parsed, e.g. '[3, 4, 5]'
    character(*), intent(in) :: str
    !> Integer numbers found in string
    integer, intent(out) :: molidxs(:)
    !> Number of integer numbers found
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

end module misc






!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Module containing quicksort for array of real numbers
!>
!> @verbatim
!> ! Quick sort routine from:
!> Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
!> Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
!> Modified by Alan Miller to include an associated integer array which gives
!> the positions of the elements in the original order.
!> @endverbatim

module qsort
  implicit none
contains

  !=======================================================================================
  !> Credits (verbatim from @ref http://jblevins.org/mirror/amiller/qsort.f90):
  !> @verbatim
  !> Quick sort routine from:
  !> Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
  !> Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
  !> Modified by Alan Miller to include an associated integer array which gives
  !> the positions of the elements in the original order.
  !> @endverbatim
  !>
  !> Further changed by JT to use argument "n" instead of size(list).

  RECURSIVE SUBROUTINE quick_sort(list, order, n)
    REAL*8, DIMENSION (:), INTENT(IN OUT)  :: list
    INTEGER, DIMENSION (:), INTENT(OUT)  :: order
    !> Operation is restricted to elements 1 to n
    INTEGER, INTENT(IN) :: n

    ! Local variable
    INTEGER :: i

    DO i = 1, n
      order(i) = i
    END DO

    CALL quick_sort_1(1, n)

  CONTAINS

    !-------------------------------------------------------------------------------------
    !> Quick sort auxiliary internal.
    !>
    !> Does the actual hard work (the main routine is mostly initialization).

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
    !> Quick sort auxiliary internal, does the swapping

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

!> Maintains a list of molecules that are on/off
!>
!> Has routines to maintain the list and retrieve information.

module molecules_idxs
  use logging
  use misc
  use dimensions
  implicit none

  type molidx_list
    integer :: n_off, & !< number "off"
               n_on     !< number "on"

    integer, dimension(NUM_MOL) :: &
      on,  & !< molecule ids "on"
      off    !< molecule ids "off"
  end type

  type(molidx_list) :: molidxs

  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  ! There is a calling order to be observed. These flags + assertions inforce that
  logical, private :: &
   flag_molecules_idxs_init = .false.   !< molecules_idxs_init() has been called?

  private make_molidxs_on
contains

  !> Module initialization:
  !> Must be called at some point of system startup.

  subroutine molecules_idxs_init()
    call make_molidxs_on()
    flag_molecules_idxs_init = .true.
  end

  !=======================================================================================
  !> Returns molecule id given index
  !>
  !> Molecule id is a number from 1 to NUM_MOL, which is uniquely related to a chemical molecule within pfant.

  function get_molidx(i_mol)
    integer i_mol, get_molidx

    if (.not. flag_molecules_idxs_init) then
      call pfant_halt('get_molidx(): forgot to call molecules_idxs_init()', is_assertion=.true.)
    end if

    ! spill check
    if (i_mol .gt. molidxs%n_on) then
      write (lll, *) 'get_molidx(): invalid molecule index i_mol (', &
       i_mol, ') must be maximum ', molidxs%n_on
      call pfant_halt(lll)
    end if

    get_molidx = molidxs%on(i_mol)
  end

  !=======================================================================================
  !> Returns .TRUE. or .FALSE. depending on whether molecule represented by molidx is "on"
  !> or "off"
  !>
  !> Can be called anytime

  function molecule_is_on(molidx)
    integer molidx, j
    logical molecule_is_on

    if (.not. flag_molecules_idxs_init) then
      call pfant_halt('get_molidx(): forgot to call molecules_idxs_init()', is_assertion=.true.)
    end if

    molecule_is_on = .true.
    do j = 1, molidxs%n_off
      if (molidx .eq. molidxs%off(j)) then
        molecule_is_on = .false.
        exit
      end if
    end do
  end

  !=======================================================================================
  !> Parses string into integer array and adds molecule ids to list of "off" molecules

  subroutine set_molidxs_off(str)
    character(*), intent(in) :: str

    integer :: molidxs(NUM_MOL), n, i

    call parse_int_array(str, molidxs, n)

    do i = 1, n
      call add_molidx_off(molidxs(i))
    end do
  end

  !=======================================================================================
  !> Adds molecule id to list of "off" molecules

  subroutine add_molidx_off(molidx)
    integer, intent(in) :: molidx !< molecule id

    if (.not. flag_molecules_idxs_init) then
      call pfant_halt('get_molidx(): forgot to call molecules_idxs_init()', is_assertion=.true.)
    end if

    ! spill check
    if (molidx .gt. NUM_MOL .or. molidx .lt. 1) then
      call pfant_halt('Invalid molecule id: '//int2str(molidx)//' (valid: 1 to '//&
       int2str(NUM_MOL)//')')
    end if

    if (molecule_is_on(molidx)) then
      ! The condition we-re in prevents duplication
      molidxs%n_off = molidxs%n_off+1
      molidxs%off(molidxs%n_off) = molidx
      call log_info('molecule id '//int2str(molidx)//' added to molidxs%off')
    else
      call log_warning('molecule id '//int2str(molidx)//' *already turned off*')
    end if

    call make_molidxs_on()
  end

  !=======================================================================================
  !> Fills molidxs%on and molidxs%n_on based on their complements.

  subroutine make_molidxs_on()
    integer i_mol, j, molidx
    logical is_off

    i_mol = 0
    do molidx = 1, NUM_MOL
      is_off = .false.  ! Whether molecule I_MOL is off
      do j = 1, molidxs%n_off
        if (molidx .eq. molidxs%off(j)) then
          is_off = .true.
          exit
        end if
      end do
      if (.not. is_off) then
        i_mol = i_mol+1
        molidxs%on(i_mol) = molidx
      end if
    end do
    molidxs%n_on = i_mol
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
module welcome
  use logging
  use misc
contains
  !> Returns string representing current PFANT version

  function pfant_version() result(v)
    character(:), allocatable :: v
    v = ' v16.2.5-beta'
  end

  !> Displays welcome message
  !>
  !> Banner created by patorjk.com Text to ASCII Art Generator (TAAG) @ref IvritFont.
  !>
  !> @note This is considered logging at INFO level; therefore nothing will be outputted
  !>       if the logging level is greater than this.
  !>
  !> @todo this is just an idea: I could make a bigger sky and select a random rectangle of it. At least 2x visible width and height to get uniform probability of given pixel being in the rectangle

  subroutine print_welcome(unit_)
    integer, intent(in) :: unit_

    if (logging_level .gt. LOGGING_INFO) return


  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !! Shadow
  !  write(unit_,*) '   _ \   ____|  \      \  | __ __| '
  !  write(unit_,*) '  |   |  |     _ \      \ |    |   '
  !  write(unit_,*) '  ___/   __|  ___ \   |\  |    |   '
  !  write(unit_,*) ' _|     _|  _/    _\ _| \_|   _|   '
  !  write(unit_,*) '                                   '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !! Italic
  !  write(unit_,*) '     ____  _________    _   ________'
  !  write(unit_,*) '    / __ \/ ____/   |  / | / /_  __/'
  !  write(unit_,*) '   / /_/ / /_  / /| | /  |/ / / /   '
  !  write(unit_,*) '  / ____/ __/ / ___ |/ /|  / / /    '
  !  write(unit_,*) ' /_/   /_/   /_/  |_/_/ |_/ /_/     '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !! Glenyn
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
    write(unit_,*) '`                     `           `    Bugs/crashes: please report at   '
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

!> Allows printing with coordinates on a "canvas"
!>
!> ASCII image is mounted, then printed on-screen at once

module ascii_canvas
  use logging
  use misc
  implicit none

  integer, parameter, private :: MAX_DIM=100

  type canvas
    character(len=MAX_DIM) :: rows(MAX_DIM)
    integer :: nr !< number of rows
    integer :: nc !< number of columns
  contains
    procedure :: init => canvas_init
    procedure :: paint => canvas_paint
    procedure :: print => canvas_print
  end type

contains

  !> Initializes with given number of rows and columns

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

  !> "paints" string in canvas

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
    !> Does the "painting"

    subroutine replace_part(row)
      character(len=*) :: row !< row of the canvas
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

  !> prints to given unit

  subroutine canvas_print(this, flag_frame, level)
    class(canvas) :: this
    !> Whether to print a "frame" around (optional). Default: .false.
    logical, intent(in), optional :: flag_frame
    !> Logging level (optional). Default: LOGGING_INFO
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

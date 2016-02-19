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

!> Command-line parsing and respective global variable declarations
!> @li Configuration globals with their default values
!> @li Routines to parse command-line arguments
!>z
!> Prefixes:
!> @li "config_" have corresponding command-line options
!> @li "ex_config_" are unitialized and must be set by particular executable. Exception is "options"
!>
!> @note There are many variables in this module that have a unique relation with a
!>       command-line option. These variables will not be documented in comments, but
!>       in the help text associated with their command-line options (the reason is to
!>       avoid text duplication). There are two ways to access this documentation:
!>       @li a) view the source code for subroutine config::init_options()
!>       @li b) execute the program with the --help option
!>
!> @todo explain how to create new option, including that flag options (argumentless) are forbidden bcz pyfant is not prepared for them (for simplicity)


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Command-line parser.
!>
!> Originally from the Fortran wiki @ref FortranWiki

module options2
  implicit none

  integer, parameter :: MAX_LEN_DESCR = 500 !< Maximum length of option description

  type option
    !> Initials of executable(s) where option is applicable.
    !> [i]nnewmarcs, [h]ydro2, [p]fant, [n]ulbad
    character(len=4) :: ihpn
    !> Long name.
    character(len=100) :: name
    !> Corresponding short name.
    character :: chr
    !> Does the option require an argument?
    logical :: has_arg
    !> Argument name, if required.
    character(len=20) :: argname
    !> Default value. For the purpose of displaying the help text only (not used to set
    !> the default values of config_* variables; in fact, string versions of the default values
    !> of these variables are used to fill this field)
    character(len=100) :: default_
    !> Description.
    !> @note The newline marker "<br>" is recognized.
    character(len=MAX_LEN_DESCR) :: descr
    !> Whether the option appears or not in --help text
    !> Some options may be secret
    logical :: appears
!  contains
!    procedure :: print => print_opt
  end type


  !> Configurable unit to output command-line parsing errors
  integer :: error_unit = 6

  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  private give_error  ! logs and halts

  integer, private, parameter :: MAX_TEMP_PTR=300  ! In practice: maximum size of options_ string

contains

  !> Parse command line options. Options and their arguments must come before
  !> all non-option arguments. Short options have the form "-X", long options
  !> have the form "--XXXX..." where "X" is any character.

  subroutine getopt(options, optindex, arg, arglen, stat, &
      offset, remain)
    use iso_fortran_env, only: error_unit

    !> Array, items are of option type. Each option may have either a long name
    !> (accessible in  the form '--XXXX...'), a single-char short name ("-X"),
    !> or both. Be careful not to repeat yourself, uniqueness of name is not
    !> checked.
    type(option), intent(in) :: options(:)

    !> If stat is 0, contains the id (index) of the option that was parsed.
    integer, intent(out) :: optindex

    !> If the parsed option requires an argument, arg contains
    !> the first len(arg) (but at most MAX_LEN_DESCR) characters of that argument.
    !> Otherwise its value is undefined. If the arguments length exceeds MAX_LEN_DESCR
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

  !> Print an option in the style of a man page. I.e.
  !> <pre>
  !> -o <arg>
  !> --option <arg>
  !>    [=default_value]
  !>    description.................................................................
  !>    ............................................................................
  !> </pre>
  subroutine print_opt(opt, unit)
    !> the option
    type(option), intent(in) :: opt
    !> logical unit number
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

  !> Logs error and halts
  !> This routine was created to standardize the "giving error" behaviuor

  subroutine give_error(lll)
    !> Error message
    character(len=*) :: lll

    write(error_unit, *) lll
    stop
  end subroutine



  !=======================================================================================


  !> Returns option name with single quotes.
  !>
  function get_option_name(opt) result(res)
    character(len=:), allocatable :: res
    !> Option, will be used only in case of error
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

!> Configuration module for all executables
!>
!> Prefixes defined here:
!> @li HANDLER_
!> @li config_ -- throughout configuration variables
!> @li execonf_ -- executable-specific variables


module config
  use logging
  use options2
  use misc
  use molecules_idxs
  use welcome
  implicit none

  ! Possible return values of option handler
  integer, parameter ::    &
   HANDLER_OK = 0,         & !< option was handled successfully
   HANDLER_ERROR = 1,      & !< error handling option, e.g. invalid argument
   HANDLER_DONT_CARE = 2     !< handler not responsible for handling that option


  !=====
  ! Executable-specific options that must be set programatically
  !=====
  !> Name of executable. Case doesn't matter. It will be converted to all uppercase or
  !> lowercave, depending on the use.
  character*16 :: execonf_name = '?'


  !=====
  ! Variables configurable by command line
  !=====
  ! Variable order is the roughly in the same sequence as that of running the executables.
  ! I.e., executables are run in this order: innewmarcs, hydro2, pfant, nulbad

  !---
  ! all executables
  !---
  character*64 :: config_fn_main     = 'main.dat'      !< option: --fn_main
  character*64 :: config_fn_progress = 'progress.txt'  !< option: --fn_progress
  character*64 :: config_logging_fn_dump = '?'         !< option: --logging_fn_dump
  logical :: config_logging_screen = .true., &  !< option --logging_screen
             config_logging_dump   = .false., & !< option --logging_dump
             config_explain        = .false., & !< option --explain
             config_no_molecules   = .false., & !< option --no_molecules
             config_no_atoms       = .false., & !< option --no_atoms
             config_no_h           = .false.    !< option --no_ah

  !---
  ! innewmarcs, hydro2, pfant
  !---
  character*64 :: config_fn_modeles = 'modeles.mod' !< option: --fn_modeles

  !
  ! hydro2, pfant
  !
  character*64 :: &
   config_fn_hmap    = 'hmap.dat', &  !< option: --fn_hmap
   config_fn_absoru2 = 'absoru2.dat'  !< option: --fn_absoru2
  real*8 :: config_llzero = -1 !< option: --llzero
  real*8 :: config_llfin  = -1 !< option: --llfin
  real*8 :: config_pas    = -1 !< option: --pas
  real*8 :: config_aint   = -1 !< option: --aint

  !---
  ! innewmarcs-only
  !---
  integer, parameter :: LEN_TIRB = 15 ! size of variable config_tirb
  character*20 :: config_open_status = 'unknown' !< option: --open_status
  character*64 :: &
   config_fn_moddat = 'modeles.dat', &      !< option: --fn_moddat
   config_fn_gridsmap = 'gridsmap.dat'      !<
  character*25 :: config_modcode = 'NoName' !< option: --modcode
  !> option: --tirb
  character(LEN_TIRB) :: config_tirb = '?'
  !> option: --teff
  real*8 :: config_teff = -1
  !> option: --glog
  real*8 :: config_glog = -1
  !> option: --asalog.
  real*8 :: config_asalog = -1
  !> option: --inum
  integer :: config_inum = 0

  !---
  ! hydro2-only
  !---
  !> Option: --zph
  !> @note (historical note) This value was being read from an altered-format
  !> dfile:absoru2 which was incompatible with the pfant executable. Therefore,
  !> it has been assigned a default value and this command-line option was added
  real*8 :: config_zph = 12
  !> option: --ptdisk
  !>
  !> This is about the "number of points" for subroutine fluxis(). Feeds into variable
  !> x _ptdisk, which is actually logical. But I needed to make it integer here because
  !> I need a tristate variable to flag whether it is kept unitialized (-1).
  !> @li -1 unitialized (hydro2_init() will use main_ptdisk instead)
  !> @li 0 false
  !> @li 1 true
  integer :: config_ptdisk = -1
  !> option: --kik; affects subroutine flin_()
  integer :: config_kik = 0
  !> option: --amores
  !>
  !> This is a tristate variable, as config_ptdisk.
  !>
  !> @note Default value taken from M.Trevisan's pfant12.R script
  integer :: config_amores = 1
  !> option: --kq
  !> @note Default taken from M.Trevisan's pfant12.R script
  integer :: config_kq = 1
  !> option: --vvt
  real*8 :: config_vvt = -1

  !---
  ! pfant-only
  !---
  character*64 :: &
   config_fn_dissoc        = 'dissoc.dat',        & !< option: --fn_dissoc
   config_fn_partit        = 'partit.dat',        & !< option: --fn_partit
   config_fn_abonds        = 'abonds.dat',        & !< option: --fn_abonds
   config_fn_atoms     = 'atomgrade.dat',     & !< option: --fn_atoms
   config_fn_molecules     = 'moleculagrade.dat',     & !< option: --fn_molecules
   config_fn_lines         = 'lines.pfant',       & !< option: --fn_lines
   config_fn_log           = 'log.log',           & !< option: --fn_log
   config_flprefix         = '?'                    !< option: --flprefix
  integer :: config_interp = 1  !< option: --interp

  !---
  ! nulbad-only
  !---
  logical :: &
   config_norm = .true., &                       !< option: --norm
   config_flam = .true., &                       !< option: --flam
   config_convol = .true.                        !< option: --convol
  ! These variables are "uninitialized". If left so, nulbad_calc::nulbad_init() will
  ! take values within dfile:main
  real*8 :: &
   config_fwhm = -1, &               !< option: --fwhm
   config_pat = -1                   !< option: --pat
  character*64 :: &
    config_fn_flux = '?', &         !< option: --fn_flux
    config_fn_cv = '?'              !< option: --fn_cv


  real*8 :: config_zinf = -1        !< option: --zinf
  !===== end of command-line variables declarations



  !> Unit to write to "explain" file (file containing debugging information)
  integer, parameter :: UNIT_EXPLAIN = 145


  !=========^ PUBLIC  ^==========
  !=========v PRIVATE v==========

  !=====
  ! Command-line options definition
  !=====
  !> List of command-line options
  type(option), private :: options(MAX_NUM_OPTIONS)
  !> Maximum valid index of the options variable
  integer, private :: num_options = 0

  !=====
  ! Other stuff
  !=====

  !> indentation string to be used in help text at will
  character(3), private, parameter :: IND = '.. '
  !> Message to be used in help text at will.
  character(:), private, parameter :: FROM_MAIN = ' (read from main configuration file)'

  private :: validate_options, &
   init_options, parse_args, show_help, exe_wants, handle_option

contains

  !=======================================================================================
  !> Initialization of this module
  !>
  !> @note Must be called *after* module-specific initialization

  subroutine config_init()
    if (execonf_name .eq. '?') &
     call pfant_halt('Executable name not set', is_assertion=.true.)

    write(*,*) to_upper(execonf_name)//pfant_version()
    write(*,*) ''

    call init_options()
    call validate_options()
    call parse_args()

    ! logging module...
    logging_fn_progress = config_fn_progress
    if (config_logging_fn_dump .eq. '?') then
      config_logging_fn_dump = to_lower(execonf_name)//'_dump.log'
    end if
    logging_fn_dump = config_logging_fn_dump

    call print_welcome(6)
  end


  !=======================================================================================
  !> Creates option and adds to options variable
  !>
  !> @note character arguments are declared with len=*, truncation may occur when
  !> option structure is created.

  subroutine add_option(ihpn, name, chr, has_arg, argname, default_, descr, appears)
    character(len=*), intent(in) :: ihpn  !< initials of executables where option is valid
    character(len=*), intent(in) :: name, argname, default_, descr
    character(len=1), intent(in) :: chr
    logical, intent(in) :: has_arg
    logical, intent(in), optional :: appears
    logical :: appears_

    num_options = num_options+1

    if (num_options .gt. MAX_NUM_OPTIONS) then
      call pfant_halt('Reached maximum number of command-line options, increase '//&
       'value of MAX_NUM_OPTIONS', is_assertion=.true.)
    end if

    appears_ = .true.
    if (present(appears)) then
      appears_ = appears
    end if

    options(num_options) = option(ihpn, name, chr, has_arg, argname, default_, descr, &
      appears_)
  end

  !=======================================================================================
  !> Initialization of command-line options
  !>
  !> - initializes all options for all executables
  !> - assertions to try to catch options initialization errors (e.g. repeated option(s))
  !>
  !> @note It is possible to break description lines using &lt;br&gt;
  !>
  !> @note To indent 2nd, 3rd etc. lines of a paragraph, use the @c IND constant after a
  !> &lt;br&gt;
  !>
  !> @par Important:
  !> If you add options here, you must change the config_NUM_OPTIONS constant
  !> accordingly.
  !>
  !> @todo where to put the explanation on option text formatting

  subroutine init_options()
    ! Note that the order of calling add_option() doesn't matter for the program,
    ! but will affect the option printing order when you invoke with "--help"

    !
    ! All executables
    !
    call add_option('ihpn', 'help', 'h', .false., '', '', &
      'Displays this help text.')
    call add_option('ihpn', 'logging_level', 'l', .true., 'level', 'debug', &
     'logging level<br>'//&
     IND//'debug<br>'//&
     IND//'info<br>'//&
     IND//'warning<br>'//&
     IND//'error<br>'//&
     IND//'critical<br>'//&
     IND//'halt')
    call add_option('ihpn', 'logging_screen',   ' ', .true., 'T/F', logical2str(config_logging_screen), &
     'Print log messages to standard output (usually monitor screen)?')
    call add_option('ihpn', 'logging_dump',     ' ', .true., 'T/F', logical2str(config_logging_dump), &
      'Print log messages to dump log file?')
    call add_option('ihpn', 'logging_fn_dump',   ' ', .true., 'file name', '<executable name>_dump.log', &
     'output file name - dump log file')
    call add_option('ihpn', 'fn_main',          ' ', .true., 'file name', config_fn_main, &
     'input file name - main configuration')
    call add_option('ihpn', 'fn_progress',      ' ', .true., 'file name', config_fn_progress, &
     'output file name - progress indicator')
    call add_option('ihpn', 'explain',     ' ', .true., 'T/F', logical2str(config_explain), &
      'Save additional information in file explain.txt (debugging purposes; output varies, or flag may be ignored)', .false.)
    call add_option('ihpn', 'play',     ' ', .false., '', '', &
      'Fed up of calculating spectra', .false.)

    !
    ! innewmarcs, hydro2, pfant
    !
    call add_option('ihp', 'fn_modeles',' ', .true., 'file name', config_fn_modeles, &
     'Binary file containing information about atmospheric model (created by innewmarcs)')

    !
    ! innewmarcs, hydro2
    !
    call add_option('ih', 'teff',' ', .true., 'real value', '<main_teff> '//FROM_MAIN, &
     '"Teff"')
    call add_option('ih', 'glog',' ', .true., 'real value', '<main_glog> '//FROM_MAIN, &
     '"log g"')
    call add_option('ih', 'asalog',' ', .true., 'real value', '<main_asalog> '//FROM_MAIN, &
     '"[M/H]"')
    call add_option('ih', 'inum',' ', .true., 'real value', '<main_inum> '//FROM_MAIN, &
     'Record id within atmospheric model binary file')

    !
    ! hydro2, pfant
    !
    call add_option('hp', 'fn_absoru2',       ' ', .true., 'file name', config_fn_absoru2, &
     'input file name - absoru2')
    call add_option('hp', 'fn_hmap',       ' ', .true., 'file name', config_fn_hmap, &
     'input file name - table containing table with<br>'//&
     IND//'(filename, niv inf, niv sup, central lambda, kiex, c1)')
     !>@todo erplace "calculus interval" with "synthesis interval"
    call add_option('hp', 'llzero',' ', .true., 'real value', '<main_llzero> '//FROM_MAIN, &
     'Lower boundary of calculation interval (angstrom)')
    call add_option('hp', 'llfin',' ', .true., 'real value', '<main_llfin> '//FROM_MAIN, &
     'Upper boundary of calculation interval (angstrom)')

    !
    ! innewmarcs-only
    !
    call add_option('i', 'open_status',' ', .true., 'string', config_open_status, &
     'File open mode for binary file<br>'//&
     IND//'new: file must not exist<br>'//&
     IND//'old: file must exist<br>'//&
     IND//'replace: replaces file if exists, otherwise creates new')
    call add_option('i', 'fn_moddat',' ', .true., 'file name', config_fn_moddat, &
     'ASCII file containing information about atmospheric model (created by innewmarcs)')
    call add_option('i', 'modcode',' ', .true., 'string up to 25 characters', config_modcode, &
     '"Model name"')
    call add_option('i', 'tirb',' ', .true., 'string up to 15 characters', config_tirb, &
     '"Titre"')
    call add_option('hp', 'fn_gridsmap',       ' ', .true., 'file name', config_fn_gridsmap, &
     'input file name - file containing list of MARCS models file names')

    !
    ! hydro2-only
    !
    call add_option('h', 'ptdisk',' ', .true., 'T/F', '<main_ptdisk> '//FROM_MAIN, &
     'option for interpolation subroutines<br>'//&
     IND//'T: 7-point integration<br>'//&
     IND//'F: 6- or 26-point integration, depending on option kik')
    call add_option('hp', 'kik', ' ', .TRUE., '0/1', int2str(config_kik), &
     'option for interpolation subroutines<br>'//&
     IND//'0: integration using 6/7 points depending on option ptdisk;<br>'//&
     IND//'1: 26-point integration)')
    call add_option('h', 'amores',' ', .true., 'T/F', logical2str(int2logical(config_amores)), &
     'AMOrtissement de RESonnance?')

    call add_option('h', 'kq', ' ', .true., '0/1', int2str(config_kq), &
     '"Theorie"<br>'//&
     IND//'0: THEORIE DE GRIEM;<br>'//&
     IND//'1: THEORIE QUASISTATIQUE')

    call add_option('h', 'vvt', ' ', .true., 'real value', '<main_vvt(1)> '//FROM_MAIN, &
     'velocity of microturbulence')

    call add_option('h', 'zph', ' ', .true., 'real value', real82str(config_zph, 2), &
     'abondance d''H pour laquelle sont donnees les abondances metalliques')

    !
    ! pfant-only
    !
    call add_option('p', 'interp', 'i', .true., 'type', int2str(config_interp), &
     'interpolation type for subroutine turbul()<br>'//&
     IND//'1: linear;<br>'//&
     IND//'2: parabolic)')
    !> @todo Find names for each file and update options help
    call add_option('p', 'fn_dissoc',        ' ', .true., 'file name', config_fn_dissoc, &
     'input file name - dissociative equilibrium')
    call add_option('p', 'fn_partit',        ' ', .true., 'file name', config_fn_partit, &
     'input file name - partition functions')
    call add_option('p', 'fn_abonds',        ' ', .true., 'file name', config_fn_abonds, &
     'input file name - atomic abundances')
    call add_option('p', 'fn_atoms',     ' ', .true., 'file name', config_fn_atoms, &
     'input file name - atomic lines')
    call add_option('p', 'fn_molecules', ' ', .true., 'file name', config_fn_molecules, &
     'input file name - molecular lines')
    call add_option('p', 'molidxs_off',        ' ', .true., 'molecule ids', '', &
     'comma-separated ids of molecules to be "turned off" (1 to '//int2str(NUM_MOL)//').')
    call add_option('p', 'no_molecules',' ', .true., 'T/F', logical2str(config_no_molecules), &
     'If set, skips the calculation of molecular lines')
    call add_option('p', 'no_atoms',' ', .true., 'T/F', logical2str(config_no_atoms), &
     'If set, skips the calculation of atomic lines')
    call add_option('p', 'no_h',' ', .true., 'T/F', logical2str(config_no_h), &
     'If set, skips the calculation of hydrogen lines')
    call add_option('p', 'zinf', ' ', .true., 'real value', '(zinf per-line in dfile:atoms)', &
     'distance from center of line to consider in atomic line calculation.<br>'//&
     IND//'If this option is used, will bypass the zinf defined for each atomic line<br>'//&
     IND//'of dfine:atoms and use the value passed', .false.)  ! option will not appear in --help printout
    call add_option('p', 'pas', ' ', .true., 'real value', '<main_pas> '//FROM_MAIN, &
     'Calculation step (angstrom)')
    call add_option('p', 'aint', ' ', .true., 'real value', '<main_aint> '//FROM_MAIN, &
     'Interval length per iteration (angstrom)')


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
     'Flux file name')
    call add_option('n', 'norm',     ' ', .true., 'T/F', logical2str(config_norm), &
      'Is spectrum normalized?')
    call add_option('n', 'flam',     ' ', .true., 'T/F', logical2str(config_flam), &
      'Fnu to FLambda transformation?')
    call add_option('n', 'fn_cv',     ' ', .true., 'file name', '<flux file name>.nulbad', &
      'output file name, which will have the convolved spectrum')
    call add_option('n', 'pat',      ' ', .true., 'real value', '<main_pas> '//FROM_MAIN, &
      'Wavelength step of the output spectrum (angstrom)')
    call add_option('n', 'convol',   ' ', .true., 'T/F', logical2str(config_convol), &
      'Apply convolution?')
    call add_option('n', 'fwhm',     ' ', .true., 'real value', '<main_fwhm> '//FROM_MAIN, &
      'Full-width-half-maximum of Gaussian function')
  end



  !=======================================================================================
  !> Handles single option
  !>
  !> @note Must be called by executable-specific option handler when the latter does not
  !>       recognize the option.
  !>
  !> @note If finds "-h" or "--help", will display help text and halt.

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
      case ('logging_screen')
        config_logging_screen = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_logging_screen', logical2str(config_logging_screen))
      case ('logging_dump')
        config_logging_dump = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_logging_dump', logical2str(config_logging_dump))
      case ('explain')
        config_explain = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_explain', logical2str(config_explain))
      case ('fn_main')
        call parse_aux_assign_fn(o_arg, config_fn_main, 'config_fn_main')
      case ('logging_fn_dump')
        call parse_aux_assign_fn(o_arg, config_logging_fn_dump, 'config_logging_fn_dump')
      case ('fn_progress')
        call parse_aux_assign_fn(o_arg, config_fn_progress, 'config_fn_progress')
      case ('open_status')
        call parse_aux_assign_fn(o_arg, config_open_status, 'open_status')
      case ('fn_modeles')
        call parse_aux_assign_fn(o_arg, config_fn_modeles, 'fn_modeles')
      case ('fn_moddat')
        call parse_aux_assign_fn(o_arg, config_fn_moddat, 'fn_moddat')
      case ('modcode')
        call parse_aux_assign_fn(o_arg, config_modcode, 'modcode')
      case ('tirb')
        call parse_aux_assign_fn(o_arg, config_tirb, 'tirb')
      case ('teff')
        config_teff = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_teff', real82str(config_teff, 1))
      case ('glog')
        config_glog = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_glog', real82str(config_glog, 3))
      case ('asalog')
        config_asalog = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_asalog', real82str(config_asalog, 3))
      case ('inum')
        config_inum = parse_aux_str2int(opt, o_arg)
        if (config_inum .lt. 1) then !#validation
          res = HANDLER_ERROR
        else
          call parse_aux_log_assignment('config_inum', int2str(config_inum))
        end if
      case ('ptdisk')
        ! This conversion to/from integer is because config_ptdisk is a tristate variable,
        ! but the user may think it is just a logical variable
        ! See hydro2_init() to see how it is treated
        config_ptdisk = logical2int(parse_aux_str2logical(opt, o_arg))
        call parse_aux_log_assignment('config_ptdisk', logical2str(int2logical(config_ptdisk)))
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
        ! config_amores is also tristate, as config_ptdisk
        config_amores = logical2int(parse_aux_str2logical(opt, o_arg))
        call parse_aux_log_assignment('config_amores', logical2str(int2logical(config_amores)))
      case ('vvt')
        config_vvt = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_vvt', real82str(config_vvt, 3))
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
!            case ('fn_lines')
!              call parse_aux_assign_fn(o_arg, config_fn_lines)
!            case ('fn_log')
!              call parse_aux_assign_fn(o_arg, config_fn_log)
      case ('molidxs_off')
        call set_molidxs_off(o_arg)
      case ('flprefix')
        call parse_aux_assign_fn(o_arg, config_flprefix, 'config_flprefix')
      case ('no_molecules')
        config_no_molecules = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_no_molecules', logical2str(config_no_molecules))
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
      case ('norm')
        config_norm = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_norm', logical2str(config_norm))
      case ('fn_gridsmap')
        call parse_aux_assign_fn(o_arg, config_fn_gridsmap, 'config_fn_gridsmap')

      case default
        res = HANDLER_DONT_CARE
    end select
  end

  ! 88888 .d88b. .d88b. 8    .d88b.
  !   8   8P  Y8 8P  Y8 8    YPwww.
  !   8   8b  d8 8b  d8 8        d8
  !   8   `Y88P' `Y88P' 8888 `Y88P'  Tools

  ! Nobody wants to use this
  ! !=======================================================================================
  ! !> Concatenates config_wdir with specific filename: <working directory>/<filename>.
  ! !> Result is clean of leading/trailling spaces
  !
  ! function join_with_wdir(filename) result(res)
  !   character(len=*), intent(in) :: filename  !< File name
  !   character(len=:), allocatable :: res
  !
  !   res = wdir_trim // trim(filename)
  ! end

  !=======================================================================================
  !> logging routine: prints variable and assigned value

  subroutine parse_aux_log_assignment(varname, value)
    character(*), intent(in) :: varname, value
    call log_info('set '//varname//' = '''//trim(adjustl(value))//'''')
  end

  !=======================================================================================
  !> Assigns option argument to filename variable
  !>
  !> @todo Filename validation could be added here.

  subroutine parse_aux_assign_fn(arg, dest, varname)
    character(len=*), intent(in)  :: arg !< command-line option argument
    character(len=*), intent(out) :: dest!< One of config_fn_* variables
    !> name of vairable being assigned, for logging purpose
    character(len=*), intent(in) :: varname
    dest = arg
    call parse_aux_log_assignment(varname, arg)
  end

  !=======================================================================================
  !> Converts string to integer, halting the program if conversion fails.
  !>
  !> This function takes an option as argument in order to form a comprehensible
  !> error message if the conversion to integer fails.

  integer function parse_aux_str2int(opt, s)
    !> Option, will be used only in case of error
    type(option), intent(in) :: opt
    !> String to be converted to integer
    character(len=*), intent(in) :: s

    read(s, *, err=20) parse_aux_str2int
    go to 30

    20 continue
    call pfant_halt('Error parsing option '//get_option_name(opt)//&
     ': invalid integer argument: '''//trim(s)//'''')

    30 continue
  end

  !=======================================================================================
  !> Converts string to real*8, halting the program if conversion fails.
  !>
  !> This function takes an option as argument in order to form a comprehensible
  !> error message if the conversion to real fails.

  real*8 function parse_aux_str2real8(opt, s)
    !> Option, will be used only in case of error
    type(option), intent(in) :: opt
    !> String to be converted to integer
    character(len=*), intent(in) :: s

    read(s, *, err=20) parse_aux_str2real8
    go to 30

    20 continue
    call pfant_halt('Error parsing option '//get_option_name(opt)//&
     ': invalid real argument: '''//trim(s)//'''')

    30 continue
  end

  !=======================================================================================
  !> Converts string to real*4, halting the program if conversion fails.
  !>
  !> This function takes an option as argument in order to form a comprehensible
  !> error message if the conversion to real fails.

  real*4 function parse_aux_str2real4(opt, s)
    !> Option, will be used only in case of error
    type(option), intent(in) :: opt
    !> String to be converted to integer
    character(len=*), intent(in) :: s

    read(s, *, err=20) parse_aux_str2real4
    go to 30

    20 continue
    call pfant_halt('Error parsing option '//get_option_name(opt)//&
     ': invalid real argument: '''//trim(s)//'''')

    30 continue
  end

  !=======================================================================================
  !> Converts string to logical, halting the program if conversion fails.
  !>
  !> This function takes an option as argument in order to form a comprehensible
  !> error message if the conversion to logical  fails.
  !>
  !> Please check the source code for recognized representations of a logical value.
  !>
  !> @note Conversion is case insensitive.

  logical function parse_aux_str2logical(opt, s)
    !> Option, will be used only in case of error
    type(option), intent(in) :: opt
    !> String to be converted to logical
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

    call pfant_halt('Error parsing option '//get_option_name(opt)//&
     ': invalid logical argument: '''//trim(s)//'''')
  end


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888


  !=======================================================================================
  !> Parses and validates all command-line arguments.
  !>
  !> @note If finds "-h" or "--help", will display help text and halt.

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
                call pfant_halt('Forgot to handle option '//get_option_name(opt), is_assertion=.true.)
              case (HANDLER_ERROR)
                call pfant_halt('Invalid argument for option '//get_option_name(opt)//': "'//o_arg//'"')
            end select
          else
            ! Executable will ignore options that are not meant for it
            call log_debug(execonf_name//' ignoring option '//get_option_name(opt))
          end if
      end select
    end do
  end


  !=======================================================================================
  !> Returns whether or not the option is applicable to the executable running.

  logical function exe_wants(opt) result(res)
    type(option), intent(in) :: opt
    res = .true.
    if (index(opt%ihpn, execonf_name(1:1)) .eq. 0) then
      res = .false.
    end if
  end

  !=======================================================================================
  !> Writes help to particular unit

  subroutine show_help(unit)
    !> logical unit number, e.g., 6=screen
    integer, intent(in) :: unit

    integer i

    write(*,*) 'Command-line options'
    write(*,*) '----------------------'
    write(*,*) ' Legend:'
    write(*,*) '  [=xxx]     default value'
    write(*,*) '  <arg name> argument that must be specified'

    do i = 1, num_options
      if (exe_wants(options(i))) then
        write(unit,*) ''
        call print_opt(options(i), unit)
      end if
    end do
  end

  !=======================================================================================
  !> Performs a series of checks to avoid programming errors while defining the
  !> command-line options
  !>
  !> This subroutine must be called at the end of the init_options() of a config module

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
          call pfant_halt('Repeated long option: "'//trim(name)//'"', is_assertion=.true.)
        end if
        if (chr .ne. ' ' .and. chr .eq. options(j)%chr) then
          call pfant_halt('Repeated short option: "'//chr//'"', is_assertion=.true.)
        end if
      end do
    end do
  end
end




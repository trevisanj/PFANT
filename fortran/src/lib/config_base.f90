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
!>
!> Prefixes:
!> @li "config_" have corresponding command-line options
!> @li "ex_config_" are unitialized and must be set by particular executable. Exception is "options"
!>
!> @note There are many variables in this module that have a unique relation with a
!>       command-line option. These variables will not be documented in comments, but
!>       in the help text associated with their command-line options (the reason is to
!>       avoid text duplication). There are two ways to access this documentation:
!>       @li a) view the source code for subroutine config::init_common_options()
!>       @li b) execute the program with the --help option
!>
!> @attention Subroutines starting with "config_base_" must be called by particular
!>            executable, see for example config_pfant.f90
!>
!> @todo explain how to create new option


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!vvv ANOTHER MODULE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

!> Contains the interfaces for the execonf_* routine pointers
!>
!> Formalities kept separate to keep config_base module more clean.
!>

module config_base_handler_interfaces
  use options2
  use max_

  !> executable-specific routine to handle option must have this interface
  interface
    ! Result value must be a HANDLER_* value

    function handle_option_template(opt, o_arg) result(res)
      import :: option
      type(option), intent(in) :: opt
      character(len=*), intent(in) :: o_arg
      integer :: res
    end
  end interface

  !> executable-specific routine to initialize options must have this interface
  interface
    ! Result value must be a HANDLER_* value

    subroutine init_options_template()
    end
  end interface
end


module config_base
  use logging
  use options2
  use misc
  use config_base_handler_interfaces
  implicit none

  !> indentation string to be used in help text at will
  character(3), parameter :: IND = '.. '

  !> Message to be used in help text at will.
  character(:), parameter :: FROM_MAIN = ' (read from main configuration file)'


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

  !> Variable that points to function to handle command-line option. Each executable
  !> has a specific handler

  procedure(handle_option_template), pointer :: &
   execonf_handle_option => handle_option_give_error

  procedure(init_options_template), pointer :: &
    execonf_init_options => init_options_give_error

  !=====
  ! Variables configurable by command line
  !=====

  character*192 :: &
   config_wdir    = './'  !< command-line option --wdir

  character*64 :: config_fn_main     = 'main.dat'      !< option: --fn_main
  character*64 :: config_fn_progress = 'progress.txt'  !< option: --fn_progress
  character*64 :: config_logging_fn_dump = '?'         !< option: --logging_fn_dump

  logical :: config_logging_screen = .true., & !< option --logging_screen
             config_logging_dump   = .false.   !< option --logging_dump


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  !=====
  ! Command-line options definition
  !=====
  !> List of command-line options
  type(option), private :: options(MAX_NUM_OPTIONS)
  !> Maximum valid index of the options variable
  integer, private :: num_options = 0

!!!!!!!!!!!!!  !> Number of options defined here
  !!!!!!!!!!!!! integer, private :: CONFIG_BASE_NUM_OPTIONS = 9

  character(len=:), private, allocatable :: &
   wdir_trim  !< Input directory without trailling spaces and ending with a "/"

  private :: handle_option_give_error, init_options_give_error, validate_options, &
   init_common_options, parse_args, show_help

contains

  ! 8b   d8 8    8 .d88b. 88888      .d88b    db    8    8
  ! 8YbmdP8 8    8 YPwww.   8        8P      dPYb   8    8
  ! 8  "  8 8b..d8     d8   8   wwww 8b     dPwwYb  8    8
  ! 8     8 `Y88P' `Y88P'   8        `Y88P dP    Yb 8888 8888
  !
  ! Routines that *must* be called from executable-specific configuration module.

  !=======================================================================================
  !> Initialization of this module
  !>
  !> @note Must be called *after* module-specific initialization

  subroutine config_base_init()
    use welcome
    if (execonf_name .eq. '?') &
     call pfant_halt('Executable name not set', is_assertion=.true.)

    write(*,*) to_upper(execonf_name)//pfant_version()
    write(*,*) ''

    call init_common_options()
    call execonf_init_options()
    call validate_options()
    call parse_args()

    ! working directory...
    wdir_trim = trim_and_add_slash(config_wdir)
    ! logging module...
    logging_path_progress = full_path_w(config_fn_progress)
    if (config_logging_fn_dump .eq. '?') then
      config_logging_fn_dump = to_lower(execonf_name)//'_dump.log'
    end if

    logging_path_dump = full_path_w(config_fn_progress)

    call print_welcome(6)
  end


  !=======================================================================================
  !> Creates option and adds to options variable
  !>
  !> @note character arguments are declared with len=*, truncation may occur when
  !> option structure is created.

  subroutine add_option(name, chr, has_arg, argname, default_, descr)
    character(len=*), intent(in) :: name, argname, default_, descr
    character(len=1), intent(in) :: chr
    logical, intent(in) :: has_arg

    num_options = num_options+1

    if (num_options .gt. MAX_NUM_OPTIONS) then
      call pfant_halt('Reached maximum number of command-line options, increase '//&
       'value of MAX_NUM_OPTIONS', is_assertion=.true.)
    end if

    options(num_options) = option(name, chr, has_arg, argname, default_, descr)
  end



  !=======================================================================================
  !> Handles limited set of options
  !>
  !> @note Must be called by executable-specific option handler when the latter does not
  !>       recognize the option.
  !>
  !> @note If finds "-h" or "--help", will display help text and halt.

  function config_base_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res

    res = HANDLER_OK

    select case(opt%name)
      case ('help')
          call show_help(6)
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
      case ('wdir')
        ! Note: using same routine parse_aux_assign_fn() to assign directory
        call parse_aux_assign_fn(o_arg, config_wdir, 'config_wdir')
      case ('fn_main')
        call parse_aux_assign_fn(o_arg, config_fn_main, 'config_fn_main')
      case ('logging_fn_dump')
        call parse_aux_assign_fn(o_arg, config_logging_fn_dump, 'config_logging_fn_dump')
      case ('fn_progress')
        call parse_aux_assign_fn(o_arg, config_fn_progress, 'config_fn_progress')
      case default
        res = HANDLER_DONT_CARE
    end select
  end

  ! 88888 .d88b. .d88b. 8    .d88b.
  !   8   8P  Y8 8P  Y8 8    YPwww.
  !   8   8b  d8 8b  d8 8        d8
  !   8   `Y88P' `Y88P' 8888 `Y88P'  Tools

  !=======================================================================================
  !> Concatenates config_wdir with specific filename

  function full_path_w(filename) result(res)
    character(len=*), intent(in) :: filename  !< File name
    character(len=:), allocatable :: res

    res = wdir_trim // trim(filename)
  end

  !=======================================================================================
  !> logging routine: prints variable and assigned value

  subroutine parse_aux_log_assignment(varname, value)
    character(*), intent(in) :: varname, value
    call log_info('set '//varname//' = '//trim(adjustl(value)))
  end

  !=======================================================================================
  !> Assigns option argument to filename variable
  !>
  !> @todo This subroutine is currently not doing much but in the future some filename
  !> validation could be added.

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
  !> @todo will inform ' ' for options with no short equivalent, but I don't know if options2.f90 is prepared for this
  !>
  !> @todo Documentation: somehow think how to link option descriptions below, their default values, and the documentation for their respective config_* at their declarations.
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
          res = execonf_handle_option(opt, o_arg)
          select case(res)
            case(HANDLER_DONT_CARE)
              call pfant_halt('Forgot to handle option '//get_option_name(opt), is_assertion=.true.)
            case (HANDLER_ERROR)
              call pfant_halt('Invalid argument for option '//get_option_name(opt)//': "'//o_arg//'"')
          end select
      end select
    end do
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
      write(unit,*) ''
      call print_opt(options(i), unit)
    end do
  end


  !=======================================================================================
  !> This routine exists for assertion purpose only

  function handle_option_give_error(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res
    res = 0  ! shuts up one warning
    call pfant_halt('Forgot to set function pointer "execonf_handle_option"', &
     is_assertion=.true.)
  end

  !=======================================================================================
  !> This routine exists for assertion purpose only

  subroutine init_options_give_error()
    call pfant_halt('Forgot to set subroutine pointer "execonf_init_options"', &
     is_assertion=.true.)
  end

  !=======================================================================================
  !> Initialization of command-line options
  !>
  !> - allocates options variable
  !> - initializes options common to all executables
  !> - calls executable-specific options initializer
  !> - assertions to try to catch options initialization errors
  !>
  !> @note It is possible to break description lines using &lt;br&gt;
  !>
  !> @note To indent 2nd, 3rd etc. lines of a paragraph, use the @c IND constant after a
  !> &lt;br&gt;
  !>
  !> @par Important:
  !> If you add options here, you must change the CONFIG_BASE_NUM_OPTIONS constant
  !> accordingly.
  !>
  !> @todo where to put the explanation on option text formatting

  subroutine init_common_options()

    call add_option('help', 'h', .false., '', '', &
      'Displays this help text.')
    call add_option('wdir',         ' ', .true., 'directory name', config_wdir, &
      'working directory (directory for all input/output files')

    ! Logging options
    call add_option('logging_level', 'l', .TRUE., 'level', 'debug', &
     'logging level<br>'//&
     IND//'debug<br>'//&
     IND//'info<br>'//&
     IND//'warning<br>'//&
     IND//'error<br>'//&
     IND//'critical<br>'//&
     IND//'halt')
    call add_option('logging_screen',   ' ', .true., 'T/F', logical2str(config_logging_screen), &
     'Print log messages to standard output (usually monitor screen)?')
    call add_option('logging_dump',     ' ', .true., 'T/F', logical2str(config_logging_dump), &
      'Print log messages to dump log file?')
    call add_option('logging_fn_dump',   ' ', .true., 'file name', '<executable name>_dump.log', &
     'output file name - dump log file')

    ! Files that all executables will use
    call add_option('fn_main',          ' ', .true., 'file name', config_fn_main, &
     'input file name - main configuration')
    call add_option('fn_progress',      ' ', .true., 'file name', config_fn_progress, &
     'output file name - progress indicator')
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



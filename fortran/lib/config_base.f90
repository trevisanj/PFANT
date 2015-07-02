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

!> @ingroup gr_config
!>
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
!>       @li a) view the source code for subroutine config::init_options()
!>       @li b) execute the program with the --help option
!>
!> @attention Particular executable must set (at least) ex_config_num_options, ex_config_option_handler
!>
!> @attention Subroutines starting with "config_base_" must be called by particular
!>            executable, see for example config_pfant.f90

module config_base
  use logging
  use options2
  use misc
  implicit none

  character(3), parameter :: IND = '.. ' !< indentation string to be used in help text

  ! Possible return values of option handler
  integer, parameter ::    &
   HANDLER_OK = 0,         & !< option was handled successfully
   HANDLER_ERROR = 1,      & !< error handling option, e.g. invalid argument
   HANDLER_DONT_CARE = 2     !< handler not responsible for handling that option


  character*192 :: &
   config_inputdir    = './', &  !< command-line option --inputdir
   config_outputdir   = './'     !< command-line option --outputdir

  character*64 :: config_fn_progress = 'progress.txt'   !< option: --fn_progress


  !> Number of options of particular executable. Set to -1 here to
  !> allow for assertion.
  integer :: ex_config_num_options = 5

  type(option) coco

  ! routines to handle option must have this interface
  interface
    function handle_option_template(opt, o_arg) result(res)
      import :: option
      type(option), intent(in) :: opt
      character(len=*), intent(in) :: o_arg
      integer :: res
    end
  end interface

  procedure(handle_option_template), pointer :: &
   ex_config_option_handler => handle_option_give_error

  !> List of command-line options
  type(option), allocatable :: options(:)


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  character(len=:), private, allocatable :: &
   inputdir_trim  !< Input directory without trailling spaces and ending with a "/"
  character(len=:), private, allocatable :: &
   outputdir_trim   !< Output directory without trailling spaces and ending with a "/"

  private :: handle_option_give_error

   save

contains
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
          res = ex_config_option_handler(opt, o_arg)
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
  !> Performs a series of checks to avoid programming errors while defining the
  !> command-line options
  !>
  !> This subroutine must be called at the end of the init_options() of a config module

  subroutine validate_options(k)
    integer, intent(in) :: k !< Number of options initialized
    integer i, j
    character*1 :: chr
    character*100 :: name

    ! Fortran will give no error trying to assign options(k), k > ex_config_num_options,
    ! so I put this protection here
    if (k .gt. ex_config_num_options) then
      call pfant_halt('Assigned options('//int2str(k)//' > ex_config_num_options='//&
       int2str(ex_config_num_options)//')', is_assertion=.true.)
    end if

    !__assertion__: make sure that there are no repeated options. Checks all against all
    do i = 1, ex_config_num_options
      name = options(i)%name
      ! write(*,*) i, name
      chr = options(i)%chr
      do j = i+1, ex_config_num_options
        if (name .eq. options(j)%name) then
          call pfant_halt('Repeated long option: "'//trim(name)//'"', is_assertion=.true.)
        end if
        if (chr .ne. ' ' .and. chr .eq. options(j)%chr) then
          call pfant_halt('Repeated short option: "'//chr//'"', is_assertion=.true.)
        end if
      end do
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

    do i = 1, ex_config_num_options
      write(unit,*) ''
      call print_opt(options(i), unit)
    end do
  end



  ! .d88b. Yb    dP 8888 888b. 888b. 888 888b. 8888 
  ! 8P  Y8  Yb  dP  8www 8  .8 8  .8  8  8   8 8www 
  ! 8b  d8   YbdP   8    8wwK' 8wwK'  8  8   8 8    
  ! `Y88P'    YP    8888 8  Yb 8  Yb 888 888P' 8888
  !
  ! Routines requiring "override" (OOP terminology)
  !
  ! Here, config_base.f90 is seen as the definition of a base class, and a executable-
  ! specific module, e.g., config_pfant.f90 is seen as an extension of this class.
  !
  ! All config_base_*() routines have their counterparts in modules config_pfant.f90,
  ! config_nulbad.f90 etc. Their names however change: the "base_" will be dropped.
  !
  ! This is a design to allow both for common and executable-specific configuration.

  !=======================================================================================
  !> Initialization of this module
  !>
  !> @note Must be called *after* module-specific initialization

  subroutine config_base_init()
    call parse_args()

    ! data directories...
    inputdir_trim = trim_and_add_slash(config_inputdir)
    outputdir_trim = trim_and_add_slash(config_outputdir)
    ! logging module...
    logging_path_progress = full_path_o(config_fn_progress)
  end

  !=======================================================================================
  !> Allocates options variables and initializes a few options commont to all executables
  !>
  !> Returns then number of options initialized
  !>
  !> @note It is possible to break description lines using &lt;br&gt;
  !>
  !> @note To indent 2nd, 3rd etc. lines of a paragraph, use the @c IND constant after a
  !> &lt;br&gt;

  function config_base_init_options() result(k)
    integer k !< Number of initialized options

    if (ex_config_num_options .eq. -1) then
      call pfant_halt('Forgot to set ex_config_num_options', is_assertion=.true.)
    end if

    allocate(options(1:ex_config_num_options))

    k = 1
    options(k) = option('help', 'h', .false., '', '', &
      'Displays this help text.')
    k = k+1
    options(k) = option('loglevel', 'l', .TRUE., 'level', 'debug', &
     'logging level<br>'//&
     IND//'debug<br>'//&
     IND//'info<br>'//&
     IND//'warning<br>'//&
     IND//'error<br>'//&
     IND//'critical<br>'//&
     IND//'halt')
    k = k+1
    options(k) = option('inputdir',         ' ', .true., 'directory name', config_inputdir, &
      'directory containing input files')
    k = k+1
    options(k) = option('outputdir',         ' ', .true., 'directory name', config_outputdir, &
      'directory for output files')
    k = k+1
    options(k) = option('fn_progress',      ' ', .true., 'file name', config_fn_progress, &
     'output file name - progress indicator')
  end

  !=======================================================================================
  !> Handles limited set of options
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
      case ('loglevel')
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

      case ('inputdir')
        ! Note: using same routine parse_aux_assign_fn() to assign directory
        call parse_aux_assign_fn(o_arg, config_inputdir, 'config_inputdir')

      case ('outputdir')
        ! Note: using same routine parse_aux_assign_fn() to assign directory
        call parse_aux_assign_fn(o_arg, config_outputdir, 'config_outputdir')

      case ('fn_progress')
        call parse_aux_assign_fn(o_arg, config_fn_progress, 'config_fn_progress')

      case default
        res = HANDLER_DONT_CARE
    end select
  end



  ! 888b.    db    888b. .d88b. 888 8b  8 .d88b  
  ! 8  .8   dPYb   8  .8 YPwww.  8  8Ybm8 8P www 
  ! 8wwP'  dPwwYb  8wwK'     d8  8  8  "8 8b  d8 
  ! 8     dP    Yb 8  Yb `Y88P' 888 8   8 `Y88P' 
  !
  ! The following routines aid with parsing of command-line options.

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
  !> Converts string to real, halting the program if conversion fails.
  !>
  !> This function takes an option as argument in order to form a comprehensible
  !> error message if the conversion to real fails.

  real*8 function parse_aux_str2real(opt, s)
    !> Option, will be used only in case of error
    type(option), intent(in) :: opt
    !> String to be converted to integer
    character(len=*), intent(in) :: s

    read(s, *, err=20) parse_aux_str2real
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

  !=======================================================================================
  !> Concatenates config_inputdir with specific filename

  function full_path_i(filename) result(res)
    character(len=*), intent(in) :: filename  !< File name
    character(len=:), allocatable :: res

    res = inputdir_trim // trim(filename)
  end

  !=======================================================================================
  !> Concatenates config_outputdir with specific filename

  function full_path_o(filename) result(res)
    character(len=*), intent(in) :: filename  !< File name
    character(len=:), allocatable :: res

    res = outputdir_trim // trim(filename)
  end


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols


  !=======================================================================================
  !> This routine exists for assertion purpose only
  !>
  !> @note ex_config_option_handler could be pointed to config_base_handle_option(), however it is
  !>       better to give an error so that the developer knows that they forgot to set
  !>       that variable.

  function handle_option_give_error(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res
    call pfant_halt('Forgot to set function pointer "ex_config_option_handler"', is_assertion=.true.)
  end 
end

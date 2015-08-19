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
  ! Variable order is the same as sequence of running executables.
  ! So, variables used by several executables will appear below in the section of the
  ! executable that first uses ("introduces") it.

  ! ┌─┐┌─┐┌┬┐┌┬┐┌─┐┌┐┌
  ! │  │ ││││││││ ││││
  ! └─┘└─┘┴ ┴┴ ┴└─┘┘└┘  options useful to all executables

  character*192 :: &
   config_wdir    = './'  !< command-line option --wdir

  character*64 :: config_fn_main     = 'main.dat'      !< option: --fn_main
  character*64 :: config_fn_progress = 'progress.txt'  !< option: --fn_progress
  character*64 :: config_logging_fn_dump = '?'         !< option: --logging_fn_dump

  logical :: config_logging_screen = .true., & !< option --logging_screen
             config_logging_dump   = .false.   !< option --logging_dump


  ! ┬┌┐┌┌┐┌┌─┐┬ ┬┌┬┐┌─┐┬─┐┌─┐┌─┐
  ! │││││││├┤ ││││││├─┤├┬┘│  └─┐
  ! ┴┘└┘┘└┘└─┘└┴┘┴ ┴┴ ┴┴└─└─┘└─┘ innewmarcs-introduced options

  integer, parameter :: LEN_TIRB = 15 ! size of variable config_tirb

  ! note: maintained variable names found in original innewmarcs.f
  ! (with "config_" prefix)

  character*20 :: config_open_status = 'unknown' !< option: --open_status

  character*64 :: &
   config_fn_modeles = 'modeles.mod', &    !< option: --fn_modeles
   config_fn_moddat = 'modeles.dat', &     !< option: --fn_moddat
   config_fn_gridslist = 'gridsmap.dat' !< option: --fn_gridslist

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

  ! ┬ ┬┬ ┬┌┬┐┬─┐┌─┐┌─┐
  ! ├─┤└┬┘ ││├┬┘│ │┌─┘
  ! ┴ ┴ ┴ ─┴┘┴└─└─┘└─┘ hydro2-introduced options

  logical :: config_hmap = .false. !< option: --hmap

  !> Option: --zph
  !> @note (historical note) This value was being read from an altered-format
  !> infile:absoru2 which was incompatible with the pfant executable. Therefore,
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

  !> option: --kik
  !> @note This was called "IOP" but was changed to match variable name in pfant executable
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

  !> option: --nomplot
  character*16 :: config_nomplot = '?'

  !> option: --vvt
  real*8 :: config_vvt = -1


  character*64 :: &
   config_fn_hmap   = 'hmap.dat'     !< option: --fn_hmap


  integer :: &
   config_na = -1, &   !< option: --na
   config_nb = -1      !< option: --nb
  real*8 :: &
   config_clam = -1, & !< option: --clam
   config_kiex = -1, & !< option: --kiex
   config_c1   = -1    !< option: --c1


  ! ┌─┐┌─┐┌─┐┌┐┌┌┬┐
  ! ├─┘├┤ ├─┤│││ │
  ! ┴  └  ┴ ┴┘└┘ ┴  pfant-introduced options

  character*64 :: &
   config_fn_dissoc        = 'dissoc.dat',        & !< option: --fn_dissoc
   config_fn_partit        = 'partit.dat',        & !< option: --fn_partit
   config_fn_absoru2       = 'absoru2.dat',       & !< option: --fn_absoru2
   config_fn_modeles       = 'modeles.mod',       & !< option: --fn_modeles
   config_fn_abonds        = 'abonds.dat',        & !< option: --fn_abonds
   config_fn_atomgrade     = 'atomgrade.dat',     & !< option: --fn_atomgrade
   config_fn_molecules     = 'molecules.dat',     & !< option: --fn_molecules
   config_fn_lines         = 'lines.pfant',       & !< option: --fn_lines
   config_fn_log           = 'log.log',           & !< option: --fn_log
   config_fn_hmap         = 'hmap.dat'            !< option: --fn_hmap

  integer :: config_interp = 1  !< option: --interp


  ! ┌┐┌┬ ┬┬  ┌┐ ┌─┐┌┬┐
  ! ││││ ││  ├┴┐├─┤ ││
  ! ┘└┘└─┘┴─┘└─┘┴ ┴─┴┘ nulbad-introduced options

  ! These variables are set to their default values
  logical :: &
   config_norm = .true., &                       !< option: --norm
   config_flam = .true., &                       !< option: --flam
   config_convol = .true.                        !< option: --convol
  ! These variables are "uninitialized". If left so, nulbad_calc::nulbad_init() will
  ! take values within infile:main
  real*8 :: &
   config_fwhm = -1, &               !< option: --fwhm
   config_pat = -1                   !< option: --pat
  character*64 :: &
    config_fn_flux = '?', &         !< option: --fn_flux
    config_fn_cv = '?'                !< option: --fn_cv

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


      !=====
      ! innewmarcs
      !=====
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
        call parse_aux_log_assignment('config_teff', real42str(config_teff))
      case ('glog')
        config_glog = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_glog', real42str(config_glog))
      case ('asalog')
        config_asalog = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_asalog', real42str(config_asalog))
      case ('inum')
        config_inum = parse_aux_str2int(opt, o_arg)
        if (config_inum .lt. 1) then !#validation
          res = HANDLER_ERROR
        else
          call parse_aux_log_assignment('config_inum', int2str(config_inum))
        end if


      !=====
      ! hydro2
      !=====
      !> @todo issue duplicated in innwemarcs, don't like this; better read only from
      !> infile:main
      case ('teff')
        config_teff = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_teff', real82str(config_teff))
      case ('glog')
        config_glog = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_glog', real82str(config_glog))
      case ('asalog')
        config_asalog = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_asalog', real82str(config_asalog))
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
      case ('nomplot')
        call parse_aux_assign_fn(o_arg, config_nomplot, 'config_nomplot')
      case ('vvt')
        config_vvt = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_vvt', real82str(config_vvt))
      case ('zph')
        config_zph = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_zph', real82str(config_zph))
      case ('fn_absoru2')
        call parse_aux_assign_fn(o_arg, config_fn_absoru2, 'config_fn_absoru2')
      case ('fn_modeles')
        call parse_aux_assign_fn(o_arg, config_fn_modeles, 'config_fn_modeles')
      case ('fn_hmap')
        call parse_aux_assign_fn(o_arg, config_fn_hmap, 'config_fn_hmap')
      case ('na')
        config_na = parse_aux_str2int(opt, o_arg)
        if (config_na .eq. 0 .or. config_na .eq. 1) then !#validation
          call parse_aux_log_assignment('config_na', int2str(config_na))
        else
          res = HANDLER_ERROR
        end if
      case ('nb')
        config_nb = parse_aux_str2int(opt, o_arg)
        if (config_nb .eq. 0 .or. config_nb .eq. 1) then !#validation
          call parse_aux_log_assignment('config_nb', int2str(config_nb))
        else
          res = HANDLER_ERROR
        end if
      case ('clam')
        config_clam = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_clam', real82str(config_clam))
      case ('kiex')
        config_kiex = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_kiex', real82str(config_kiex))
      case ('c1')
        config_c1 = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_c1', real82str(config_c1))
      case ('hmap')
        config_hmap = .true.
        call parse_aux_log_assignment('config_hmap', '.true.')


      !=====
      ! pfant
      !=====
      case ('interp')
        iTemp = parse_aux_str2int(opt, o_arg)
        select case (iTemp)
          case (1, 2)
            config_INTERP = iTemp
            call parse_aux_log_assignment('config_interp', int2str(config_interp))
          case default
            res = HANDLER_ERROR
        end select

      case ('kik')
        iTemp = parse_aux_str2int(opt, o_arg)
        select case(iTemp)
          case (0, 1)
            config_kik = iTemp
            call parse_aux_log_assignment('config_kik', int2str(config_kik))
          case default
            res = HANDLER_ERROR
        end select

      case ('fn_dissoc')
        call parse_aux_assign_fn(o_arg, config_fn_dissoc, 'config_fn_dissoc')
      case ('fn_partit')
        call parse_aux_assign_fn(o_arg, config_fn_partit, 'config_fn_partit')
      case ('fn_absoru2')
        call parse_aux_assign_fn(o_arg, config_fn_absoru2, 'config_fn_absoru2')
      case ('fn_modeles')
        call parse_aux_assign_fn(o_arg, config_fn_modeles, 'config_fn_modeles')
      case ('fn_abonds')
        call parse_aux_assign_fn(o_arg, config_fn_abonds, 'config_fn_abonds')
      case ('fn_atomgrade')
        call parse_aux_assign_fn(o_arg, config_fn_atomgrade, 'config_fn_atomgrade')
      case ('fn_molecules')
        call parse_aux_assign_fn(o_arg, config_fn_molecules, 'config_fn_molecules')
!            case ('fn_lines')
!              call parse_aux_assign_fn(o_arg, config_fn_lines)
!            case ('fn_log')
!              call parse_aux_assign_fn(o_arg, config_fn_log)
      case ('fn_hmap')
        call parse_aux_assign_fn(o_arg, config_fn_hmap, 'config_fn_hmap')
      case ('hmap')
        config_hmap = .true.
        call parse_aux_log_assignment('config_hmap', '.true.')

      case ('molid_off')
        iTemp = parse_aux_str2int(opt, o_arg)
        call add_molid_off(iTemp)


      !=====
      ! nulbad
      !=====
      case ('fwhm')
        config_fwhm = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_fwhm', real82str(config_fwhm))
      case ('convol')
        config_convol = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_convol', logical2str(config_convol))
      case ('pat')
        config_pat = parse_aux_str2real8(opt, o_arg)
        call parse_aux_log_assignment('config_pat', real82str(config_pat))
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
    character*1 :: initial  ! executable initial

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
    ! ┌─┐┌─┐┌┬┐┌┬┐┌─┐┌┐┌
    ! │  │ ││││││││ ││││
    ! └─┘└─┘┴ ┴┴ ┴└─┘┘└┘
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

    ! ┬┌┐┌┌┐┌┌─┐┬ ┬┌┬┐┌─┐┬─┐┌─┐┌─┐
    ! │││││││├┤ ││││││├─┤├┬┘│  └─┐
    ! ┴┘└┘┘└┘└─┘└┴┘┴ ┴┴ ┴┴└─└─┘└─┘
    call add_option('open_status',' ', .true., 'string', config_open_status, &
     'File open mode for binary file<br>'//&
     IND//'new: file must not exist<br>'//&
     IND//'old: file must exist<br>'//&
     IND//'replace: replaces file if exists, otherwise creates new')

    call add_option('fn_modeles',' ', .true., 'file name', config_fn_modeles, &
     'Name of binary file')

    call add_option('fn_moddat',' ', .true., 'file name', config_fn_moddat, &
     'Name of ASCII file')

    call add_option('modcode',' ', .true., 'string up to 25 characters', config_modcode, &
     '"Model name"')

    call add_option('tirb',' ', .true., 'string up to 15 characters', config_tirb, &
     '"Titre"')

    call add_option('teff',' ', .true., 'real value', '<main_teff> '//FROM_MAIN, &
     '"Teff"')

    call add_option('glog',' ', .true., 'real value', '<main_glog> '//FROM_MAIN, &
     '"log g"')

    call add_option('asalog',' ', .true., 'real value', '<main_asalog> '//FROM_MAIN, &
     '"[M/H]"')

    call add_option('inum',' ', .true., 'real value', '<main_inum> '//FROM_MAIN, &
     'Record id within atmospheric model binary file')

    call add_option('interp', 'i', .TRUE., 'type', int2str(config_interp), &
     'interpolation type for subroutine turbul()<br>'//&
     IND//'1: linear;<br>'//&
     IND//'2: parabolic)')
    call add_option('kik', 'k', .TRUE., 'type', int2str(config_kik), &
     'selector for subroutines flin1() and flinh()<br>'//&
     IND//'0: integration using 6/7 points depending on main_ptdisk;<br>'//&
     IND//'1: 26-point integration)')


    ! ┬ ┬┬ ┬┌┬┐┬─┐┌─┐┌─┐
    ! ├─┤└┬┘ ││├┬┘│ │┌─┘
    ! ┴ ┴ ┴ ─┴┘┴└─└─┘└─┘
    call add_option('teff',' ', .true., 'real value', '<main_teff> '//FROM_MAIN, &
     '"Teff"')

    call add_option('glog',' ', .true., 'real value', '<main_glog> '//FROM_MAIN, &
     '"log g"')

    call add_option('asalog',' ', .true., 'real value', '<main_asalog> '//FROM_MAIN, &
     '"[M/H]"')

    call add_option('inum',' ', .true., 'real value', '<main_inum> '//FROM_MAIN, &
     'Record id within atmospheric model binary file')

    call add_option('ptdisk',' ', .true., 'T/F', '<main_ptdisk> '//FROM_MAIN, &
     'option for subroutine fluxis()<br>'//&
     IND//'T: 7-point integration<br>'//&
     IND//'F: 6- or 26-point integration, depending on option kik')

    call add_option('kik', ' ', .TRUE., '0/1', int2str(config_kik), &
     'option for subroutine fluxis()<br>'//&
     IND//'0: integration using 6/7 points depending on option ptdisk;<br>'//&
     IND//'1: 26-point integration)')

    call add_option('amores',' ', .true., 'T/F', logical2str(int2logical(config_amores)), &
     'AMOrtissement de RESonnance?')

    call add_option('kq', ' ', .true., '0/1', int2str(config_kq), &
     '"Theorie"<br>'//&
     IND//'0: THEORIE DE GRIEM;<br>'//&
     IND//'1: THEORIE QUASISTATIQUE')

    call add_option('nomplot', ' ', .true., 'file name', config_nomplot, &
     'output file name - hydrogen lines')

    call add_option('vvt', ' ', .true., 'real value', '<main_vvt(1)> '//FROM_MAIN, &
     'velocity of microturbulence')

    call add_option('zph', ' ', .true., 'real value', real82str(config_zph), &
     'abondance d''H pour laquelle sont donnees les abondances metalliques')

    call add_option('fn_absoru2',       ' ', .true., 'file name', config_fn_absoru2, &
     'input file name - absoru2')
    call add_option('fn_modeles',       ' ', .true., 'file name', config_fn_modeles, &
     'input file name - model')
    call add_option('fn_hmap',       ' ', .true., 'file name', config_fn_hmap, &
     'input file name - table containing table with<br>'//&
     IND//'(filename, niv inf, niv sup, central lambda, kiex, c1)')
    call add_option('hmap', ' ', .false., '', '', &
      'If set, will read wavelength interval from main configuration file and<br>'//&
      'determine automatically which hydrogen lines to calculate according to<br>'//&
      'hmap file')
    call add_option('na', ' ', .true., 'integer', '(no default)', &
      'NIV INF')
    call add_option('nb', ' ', .true., 'integer', '(no default)', &
      'NIV SUP')
    call add_option('clam', ' ', .true., 'real', '(no default)', &
      'Central wavelength')
    call add_option('kiex', ' ', .true., 'real', '(no default)', &
      'KIEX ?doc?')
    call add_option('c1', ' ', .true., 'real', '(no default)', &
      'C1 ?doc?')

    ! ┌─┐┌─┐┌─┐┌┐┌┌┬┐
    ! ├─┘├┤ ├─┤│││ │
    ! ┴  └  ┴ ┴┘└┘ ┴ 
    !> @todo Find names for each file and update options help
    call add_option('fn_dissoc',        ' ', .true., 'file name', config_fn_dissoc, &
     'input file name - dissociative equilibrium')
    call add_option('fn_partit',        ' ', .true., 'file name', config_fn_partit, &
     'input file name - partition functions')
    call add_option('fn_absoru2',       ' ', .true., 'file name', config_fn_absoru2, &
     'input file name - absoru2')
    call add_option('fn_modeles',       ' ', .true., 'file name', config_fn_modeles, &
     'input file name - model')
    call add_option('fn_abonds',        ' ', .true., 'file name', config_fn_abonds, &
     'input file name - atomic abundances')
    call add_option('fn_atomgrade',     ' ', .true., 'file name', config_fn_atomgrade, &
     'input file name - atomic lines')
    call add_option('fn_molecules', ' ', .true., 'file name', config_fn_molecules, &
     'input file name - molecular lines')

    call add_option('fn_hmap',       ' ', .true., 'file name', config_fn_hmap, &
     'input file name - table containing table with<br>'//&
     IND//'(filename, niv inf, niv sup, central lambda, kiex, c1)')
    call add_option('hmap', ' ', .false., '', '', &
      'If set, will read hydrogen lines filenames from hmap file instead of from<br>'//&
      'main configuration file')  ! This behavious is likely to become default soon...or not

    call add_option('molid_off',        ' ', .true., 'molecule id', '', &
     'id of molecule to be "turned off" (1 to '//int2str(NUM_MOL)//').<br>'//&
     '*Note*: This option may be repeated as many times as necessary.')

    ! ┌┐┌┬ ┬┬  ┌┐ ┌─┐┌┬┐
    ! ││││ ││  ├┴┐├─┤ ││
    ! ┘└┘└─┘┴─┘└─┘┴ ┴─┴┘
    call add_option('fn_flux', ' ', .true., 'file name', &
     '<"main_flprefix" variable>.norm (taken from main configuration file)>', &
     'Flux file name')
    call add_option('norm',     ' ', .true., 'T/F', logical2str(config_norm), &
      'Is spectrum normalized?')
    call add_option('flam',     ' ', .true., 'T/F', logical2str(config_flam), &
      'Fnu to FLambda transformation?')
    call add_option('fn_cv',     ' ', .true., 'file name', '<flux file name>.nulbad', &
      'output file name, which will have the convolved spectrum')
    call add_option('pat',      ' ', .true., 'real value', '<"main_pas" variable> (taken from main configuration file)', &
      'step ?doc?')
    call add_option('convol',   ' ', .true., 'T/F', logical2str(config_convol), &
      'Apply convolution?')
    call add_option('fwhm',     ' ', .true., 'real value', '<"main_fwhm" variable> (taken from main configuration file)', &
      'Full-width-half-maximum of Gaussian function')
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



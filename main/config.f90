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
!> @li All globals have prefix "config_"
!>
!> @todo config_fn_* documentation is important because it is a good place to "define" what each file is
!>
!> @note There are many variables in this module that have a unique relation with a
!>       command-line option. These variables will not be documented in comments, but
!>       in the help text associated with their command-line options (the reason is to
!>       avoid text duplication). There are two ways to access this documentation:
!>       @li a) view the source code for subroutine config::init_options()
!>       @li b) execute the program with the --help option

module config
  use logging
  use options2
  use misc
  implicit none

  integer, parameter :: NUM_MOL=21  !< Number of molecules configured in the program.
                                    !< Conceptually, this should be defined in molecules.f, but there would be cyclic USEs
                                    !< @todo maybe create a module called "dimensions"...not before I check the other modules to incorporate: hydro2 and inewmarcs

  !=====
  ! Mode of operation
  !=====
  character*10 :: config_mode = 'pfant' !< option: --mode


  !=====
  ! File names
  !=====
  character*256 :: &
   config_fn_dissoc        = 'dissoc.dat',        & !< option: --fn_dissoc
   config_fn_main          = 'main.dat',          & !< option: --fn_main
   config_fn_partit        = 'partit.dat',        & !< option: --fn_partit
   config_fn_absoru2       = 'absoru2.dat',       & !< option: --fn_absoru2
   config_fn_modeles       = 'modeles.mod',       & !< option: --fn_modeles
   config_fn_abonds        = 'abonds.dat',        & !< option: --fn_abonds
   config_fn_atomgrade     = 'atomgrade.dat',     & !< option: --fn_atomgrade
   config_fn_moleculagrade = 'moleculagrade.dat', & !< option: --fn_moleculagrade
   config_fn_lines         = 'lines.pfant',       & !< option: --fn_lines
   config_fn_log           = 'log.log'              !< option: --fn_log

  character*256 :: config_inputdir = './'   !< command-line option --inputdir
  character*256 :: config_outputdir = './'  !< command-line option --outputdir

  !=====
  ! Variables related to molecules
  !=====
  !> Number of molecules switched off (excluded from calculations)
  integer :: config_num_mol_off = 0
  !> List of molecule ids that are be switched off. Complement of config_molids_on.
  integer :: config_molids_off (NUM_MOL)


  !=====
  ! Misc
  !=====
  integer :: config_interp = 1  !< option: --interp
  integer :: config_kik = 0     !< option: --kik

  !> Whether to allow PFANT to run if none of the specified filetoh files is found
  logical :: config_allow_no_filetoh = .false.


  !=====
  ! nulbad configuration
  !=====
  ! note: maintained variable names found in original nulbadgrade.f
  !       (however with "config_nulbad_") prefix
  logical :: &
   config_nulbad_norm = .true., &                       !< option: --nulbad_norm
   config_nulbad_flam = .true., &                       !< option: --nulbad_flam
   config_nulbad_convol = .true.                        !< option: --nulbad_convol
  real*8 :: &
   config_nulbad_fwhm = 0.13, &                         !< option: --nulbad_fwhm
   config_nulbad_pat = 0.02                             !< option: --nulbad_pat
  character*64 :: &
    config_nulbad_fileflux = 'norm.fileflux', &         !< option: --nulbad_fileflux
    config_nulbad_flcv = ''                             !< option: --nulbad_flcv






  !====================================

  !=====
  ! Calculated variables
  !=====

  !> List of molecule ids that are switched on. Complement of config_molids_off. Calculated by make_molids_on()
  integer, dimension(NUM_MOL) :: config_molids_on
  !> This is actually <code> = NUM_MOL-config_num_mol_off </code>
  integer config_num_mol_on


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  ! There is a calling order to be observed. These flags + assertions inforce that
  logical, private :: &
   flag_setup = .false.,         & !< config_setup() has been called?
   flag_make_molids_on = .false.   !< make_molids_on() has been called?

  ! ╔═╗┌─┐┌┬┐┬┌─┐┌┐┌┌─┐
  ! ║ ║├─┘ │ ││ ││││└─┐
  ! ╚═╝┴   ┴ ┴└─┘┘└┘└─┘ command-line options

  integer, parameter, private :: NUM_OPTIONS = 23       !< Number of command-line options
  type(option), private :: options(NUM_OPTIONS), opt    !< Options

  private make_molids_on

  character(len=:), private, allocatable :: &
   inputdir_trim  !< Input directory without trailling spaces and ending with a "/"
  character(len=:), private, allocatable :: &
   outputdir_trim   !< Output directory without trailling spaces and ending with a "/"


  save
contains
  !=======================================================================================
  !> Initializes the options list
  !> @note It is possible to break description lines using &lt;br&gt;
  !>
  !> @note To indent 2nd, 3rd etc. lines of a paragraph, use the @c IND constant after a
  !> &lt;br&gt;

  subroutine init_options()
    integer i, j, k
    character(:), allocatable :: name
    character(1) :: chr
    character(3), parameter :: IND = '.. ' ! indentation string

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
    options(k) = option('interp', 'i', .TRUE., 'type', int2str(config_interp), &
     'interpolation type for subroutine turbul()<br>'//&
     IND//'1: linear;<br>'//&
     IND//'2: parabolic)')
    k = k+1
    options(k) = option('kik', 'k', .TRUE., 'type', int2str(config_kik), &
     'selector for subroutines flin1() and flinh()<br>'//&
     IND//'0: integration using 6/7 points depending on main_ptdisk;<br>'//&
     IND//'1: 26-point integration)')
    k = k+1

    !> @todo Find names for each file and update options help

    options(k) = option('fn_dissoc',        ' ', .true., 'file name', config_fn_dissoc, &
     'input file name - dissociative equilibrium')
    k = k+1
    options(k) = option('fn_main',          ' ', .true., 'file name', config_fn_main, &
     'input file name - main configuration')
    k = k+1
    options(k) = option('fn_partit',        ' ', .true., 'file name', config_fn_partit, &
     'input file name - partition functions')
    k = k+1
    options(k) = option('fn_absoru2',       ' ', .true., 'file name', config_fn_absoru2, &
     'input file name - absoru2')
    k = k+1
    options(k) = option('fn_modeles',       ' ', .true., 'file name', config_fn_modeles, &
     'input file name - model')
    k = k+1
    options(k) = option('fn_abonds',        ' ', .true., 'file name', config_fn_abonds, &
     'input file name - atomic abundances')
    k = k+1
    options(k) = option('fn_atomgrade',     ' ', .true., 'file name', config_fn_atomgrade, &
     'input file name - atomic lines')
    k = k+1
    options(k) = option('fn_moleculagrade', ' ', .true., 'file name', config_fn_moleculagrade, &
     'input file name - molecular lines')
    k = k+1
    options(k) = option('inputdir',         ' ', .true., 'directory name', config_inputdir, &
      'directory containing input files')
    k = k+1
    options(k) = option('outputdir',         ' ', .true., 'directory name', config_outputdir, &
      'directory for output files')
    k = k+1
    options(k) = option('molid_off',        ' ', .true., 'molecule id', '', &
     'id of molecule to be "turned off" (1 to '//int2str(NUM_MOL)//').<br>'//&
     '*Note*: This option can be repeated.')
    k = k+1
    ! Program "modes" of operation
    options(k) = option('mode', 'm', .true., '', '', &  ! is mode, "m" but I want to test the assertion below
     'Program operational mode<br>'//&
     IND//'pfant: spectral synthesis mode<br>'//&
     IND//'nulbad: reads output from pfant mode and saves convolved spectrum<br>'//&
     IND//'pfant-nulbad: cascade pfant and nulbad operations')
    k = k+1
    ! nulbad options
    options(k) = option('nulbad_fileflux', ' ', .true., 'file name', config_nulbad_fileflux, &
      'NULBAD Flux file name<br>'//&
      '*Note*: looks for file in *output* directory, because the flux'//&
      '        file is a PFANT output.')
    k = k+1
    options(k) = option('nulbad_norm',     ' ', .true., 'T/F', logical2str(config_nulbad_norm), &
      'NULBAD Is spectrum normalized?<br>'//&
      '*Note*: this setting is ignored in ''pfant-nulbad'' mode.')
    k = k+1
    options(k) = option('nulbad_flam',     ' ', .true., 'T/F', logical2str(config_nulbad_flam), &
      'NULBAD Fnu to FLambda transformation?')
    k = k+1
    options(k) = option('nulbad_flcv',     ' ', .true., 'file name', '<flux file name>.nulbad', &
      'NULBAD output file name')
    k = k+1
    options(k) = option('nulbad_pat',      ' ', .true., 'real value', real2str(config_nulbad_pat), &
      'NULBAD step ?doc?')
    k = k+1
    options(k) = option('nulbad_convol',   ' ', .true., 'T/F', logical2str(config_nulbad_convol), &
      'NULBAD Apply convolution?')
    k = k+1
    options(k) = option('nulbad_fwhm',     ' ', .true., 'real value', real2str(config_nulbad_fwhm), &
      'NULBAD full-width-half-maximum of gaussian')

    !__assertion__
    ! Fortran will give no error trying to assign options(k), k > NUM_OPTIONS,
    ! so I put this protection here
    if (k .gt. NUM_OPTIONS) then
      call pfant_halt('Assigned options('//int2str(k)//' > NUM_OPTIONS='//&
       int2str(NUM_OPTIONS)//')', is_assertion=.true.)
    end if

    !__assertion__: make sure that there are no repeated options. Checks all against all
    do i = 1, NUM_OPTIONS
      name = options(i)%name
      ! write(*,*) i, name
      chr = options(i)%chr
      do j = i+1, NUM_OPTIONS
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
  !> Does various setup operations.
  !>
  !>   - sets up configuration defaults,
  !>   - parses command-line arguments, and
  !>   - does other necessary operations.
  !>
  !> Must be called at system startup

  subroutine config_setup()
    call init_options()
    call parse_args()

    ! Configures data directories
    inputdir_trim = trim_slash(config_inputdir)
    outputdir_trim = trim_slash(config_outputdir)

    ! Default value for config_nulbad_flcv
    if (config_nulbad_flcv .eq. '') then
      config_nulbad_flcv = trim(config_nulbad_fileflux)//'.nulbad'
    end if

    call make_molids_on()

    flag_setup = .true.
  contains
    function trim_slash(x) result(y)
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
  end

  !=======================================================================================
  !> Initializes the options list

  subroutine show_help(unit)
    !> logical unit number, e.g., 6=screen
    integer, intent(in) :: unit

    integer i

    write(*,*) 'Command-line options'
    write(*,*) '----------------------'
    write(*,*) ' Legend:'
    write(*,*) '  [=xxx]     default value'
    write(*,*) '  <arg name> argument that must be specified'

    do i = 1, NUM_OPTIONS
      write(unit,*) ''
      call print_opt(options(i), unit)
    end do
  end

  !=======================================================================================
  !> Parses and validates all command-line arguments.
  !>
  !> @todo will inform ' ' for options with no short equivalent, but I don't know if options2.f90 is prepared for this
  !>
  !> @todo Documentation: somehow think how to link option descriptions below, their default values, and the documentation for their respective config_* at their declarations.
  !>
  !> @note If finds "-h" or "--help", will display help text and halt.

  subroutine parse_args()
    integer k  !> @todo for debugging, take it out
    integer o_len, o_stat, o_remain, o_offset, o_index, iTemp
    character*500 o_arg
    logical err_out
    type(option) opt

    err_out = .FALSE.

    do while (.TRUE.)
      call getopt(options, o_index, o_arg, o_len, o_stat, o_offset, o_remain)

      ! Debugging. Note that log_debug() cannot be used we don't know yet the logging level
      ! that the user wants.
      if (.false.) then
        write(*,*) 'o_index = ', o_index
        write(*,*) 'o_arg = ', o_arg
        write(*,*) 'o_len = ', o_len
        write(*,*) 'o_stat = ', o_stat
        write(*,*) 'o_offset = ', o_offset
        write(*,*) 'o_remain = ', o_remain
        write(*,*) '---------------------------'
      end if

      select case(o_stat)
        case (1,2,3)  ! parsing stopped (no error)
           exit
        case (0)  ! option successfully parsed
          opt = options(o_index)

          ! "Uses" config options: validates and assigns to proper config_* variables.
          select case(opt%name)  ! It is more legible select by option name than by index
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
                  err_out = .TRUE.
              end select
              if (.NOT. err_out) then
                call log_assignment('logging level', to_lower(o_arg))
              end if
            case ('interp')
              iTemp = str2int(opt, o_arg)
              select case (iTemp)
                case (1, 2)
                  config_INTERP = iTemp
                  call log_assignment('config_interp', int2str(config_interp))
                case default
                  err_out = .TRUE.
              end select

            case ('kik')
              iTemp = str2int(opt, o_arg)
              select case(iTemp)
                case (0, 1)
                  config_kik = iTemp
                  call log_assignment('config_kik', int2str(config_kik))
                case default
                  err_out = .TRUE.
              end select

            case ('fn_dissoc')
              call assign_fn(o_arg, config_fn_dissoc, 'config_fn_dissoc')
            case ('fn_main')
              call assign_fn(o_arg, config_fn_main, 'config_fn_main')
            case ('fn_partit')
              call assign_fn(o_arg, config_fn_partit, 'config_fn_partit')
            case ('fn_absoru2')
              call assign_fn(o_arg, config_fn_absoru2, 'config_fn_absoru2')
            case ('fn_modeles')
              call assign_fn(o_arg, config_fn_modeles, 'config_fn_modeles')
            case ('fn_abonds')
              call assign_fn(o_arg, config_fn_abonds, 'config_fn_abonds')
            case ('fn_atomgrade')
              call assign_fn(o_arg, config_fn_atomgrade, 'config_fn_atomgrade')
            case ('fn_moleculagrade')
              call assign_fn(o_arg, config_fn_moleculagrade, 'config_fn_moleculagrade')
!            case ('fn_lines')
!              call assign_fn(o_arg, config_fn_lines)
!            case ('fn_log')
!              call assign_fn(o_arg, config_fn_log)

            case ('inputdir')
              ! Note: using same routine assign_fn() to assign directory
              call assign_fn(o_arg, config_inputdir, 'config_inputdir')

            case ('outputdir')
              ! Note: using same routine assign_fn() to assign directory
              call assign_fn(o_arg, config_outputdir, 'config_outputdir')

            case ('molid_off')
              iTemp = str2int(opt, o_arg)
              call add_molid_off(iTemp)

            case ('mode')
              select case (to_lower(o_arg))
                case ('pfant', 'nulbad', 'pfant-nulbad')
                case default
                  err_out = .true.
              end select
              if (.not. err_out) then
                config_mode = to_lower(o_arg)
                call log_assignment('operation mode', to_lower(o_arg))
              end if

            case ('nulbad_fwhm')
              config_nulbad_fwhm = str2real(opt, o_arg)
              call log_assignment('config_nulbad_fwhm', real2str(config_nulbad_fwhm))
            case ('nulbad_convol')
              config_nulbad_convol = str2logical(opt, o_arg)
              call log_assignment('config_nulbad_convol', logical2str(config_nulbad_convol))
            case ('nulbad_pat')
              config_nulbad_pat = str2real(opt, o_arg)
              call log_assignment('config_nulbad_pat', real2str(config_nulbad_pat))
            case ('nulbad_flcv')
              call assign_fn(o_arg, config_nulbad_flcv, 'config_nulbad_flcv')
            case ('nulbad_flam')
              config_nulbad_flam = str2logical(opt, o_arg)
              call log_assignment('config_nulbad_flam', logical2str(config_nulbad_flam))
            case ('nulbad_fileflux')
              call assign_fn(o_arg, config_nulbad_fileflux, 'config_nulbad_fileflux')
            case ('nulbad_norm')
              config_nulbad_norm = str2logical(opt, o_arg)
              call log_assignment('config_nulbad_norm', logical2str(config_nulbad_norm))
            case default
              call pfant_halt('Forgot to treat option '//get_option_name(opt), is_assertion=.true.)
          end select

          if (err_out) then
            call pfant_halt('Invalid argument for option '//get_option_name(opt)//': "'//o_arg//'"')
          end if
      end select

      k = k+1
      if (k == 500) then
        stop 'sort this: parse_args looping forever'
      end if
    end do

  contains

    !> logging routine: prints variable and assigned value

    subroutine log_assignment(varname, value)
      character(*), intent(in) :: varname, value
      call log_info('set '//varname//' = '//trim(adjustl(value)))
    end

    !-------------------------------------------------------------------------------------
    !> Assigns option argument to filename variable
    !>
    !> @todo This subroutine is currently not doing much but in the future some filename
    !> validation could be added.

    subroutine assign_fn(arg, dest, varname)
      character(len=*), intent(in)  :: arg !< command-line option argument
      character(len=*), intent(out) :: dest!< One of config_fn_* variables
      !> name of vairable being assigned, for logging purpose
      character(len=*), intent(in) :: varname
      dest = arg
      call log_assignment(varname, arg)
    end

    !-------------------------------------------------------------------------------------
    !> Converts string to integer, halting the program if conversion fails.
    !>
    !> This function takes an option as argument in order to form a comprehensible
    !> error message if the conversion to integer fails.

    integer function str2int(opt, s)
      !> Option, will be used only in case of error
      type(option), intent(in) :: opt
      !> String to be converted to integer
      character(len=*), intent(in) :: s

      read(s, *, err=20) str2int
      go to 30

      20 continue
      call pfant_halt('Error parsing option '//get_option_name(opt)//&
       ': invalid integer argument: '''//trim(s)//'''')

      30 continue
    end

    !-------------------------------------------------------------------------------------
    !> Converts string to real, halting the program if conversion fails.
    !>
    !> This function takes an option as argument in order to form a comprehensible
    !> error message if the conversion to real fails.

    real*8 function str2real(opt, s)
      !> Option, will be used only in case of error
      type(option), intent(in) :: opt
      !> String to be converted to integer
      character(len=*), intent(in) :: s

      read(s, *, err=20) str2real
      go to 30

      20 continue
      call pfant_halt('Error parsing option '//get_option_name(opt)//&
       ': invalid real argument: '''//trim(s)//'''')

      30 continue
    end

    !-------------------------------------------------------------------------------------
    !> Converts string to logical, halting the program if conversion fails.
    !>
    !> This function takes an option as argument in order to form a comprehensible
    !> error message if the conversion to logical  fails.
    !>
    !> Please check the source code for recognized representations of a logical value.
    !>
    !> @note Conversion is case insensitive.

    logical function str2logical(opt, s)
      !> Option, will be used only in case of error
      type(option), intent(in) :: opt
      !> String to be converted to logical
      character(len=*), intent(in) :: s

      select case (to_lower(s))
        case ('.true.', 'true', 't', 'on', '1')
          str2logical = .true.
          return
      end select

      select case (to_lower(s))
        case ('.false.', 'false', 'f', 'off', '0')
          str2logical = .false.
          return
      end select

      call pfant_halt('Error parsing option '//get_option_name(opt)//&
       ': invalid logical argument: '''//trim(s)//'''')
    end

  end subroutine parse_args

  !=======================================================================================
  !> Concatenates config_inputdir with specific filename

  function fullpath_i(filename) result(res)
    character(len=*), intent(in) :: filename  !< File name
    character(len=:), allocatable :: res

    res = inputdir_trim // trim(filename)
  end

  !=======================================================================================
  !> Concatenates config_outputdir with specific filename

  function fullpath_o(filename) result(res)
    character(len=*), intent(in) :: filename  !< File name
    character(len=:), allocatable :: res

    res = outputdir_trim // trim(filename)
  end

  !=======================================================================================
  !> Returns molecule id given index
  !>
  !> Molecule id is a number from 1 to NUM_MOL, which is uniquely related to a chemical molecule within pfant.

  function get_molid(i_mol)
    integer i_mol, get_molid
    character*80 lll  !__logging__

    !__assertion__
    if (.not. flag_make_molids_on) then
      call pfant_halt('get_molid(): forgot to call make_molids_on()', is_assertion=.true.)
    end if

    !__spill check__
    if (i_mol .gt. config_num_mol_on) then
      write (lll, *) 'get_molid(): invalid molecule index i_mol (', &
       i_mol, ') must be maximum ', config_num_mol_on
      call pfant_halt(lll)
    end if

    get_molid = config_molids_on(i_mol)
  end

  !=======================================================================================
  !> Returns .TRUE. or .FALSE. depending on whether molecule represented by molid is "on"
  !> or "off"
  !>
  !> Can be called anytime

  function molecule_is_on(molid)
    integer molid, j
    logical molecule_is_on

    molecule_is_on = .true.
    do j = 1, config_num_mol_off
      if (molid .eq. config_molids_off(j)) then
        molecule_is_on = .false.
        exit
      end if
    end do
  end

  !-------------------------------------------------------------------------------------
  !> Adds molecule id to list of "off" molecules
  !>
  !> Can be called anytime

  subroutine add_molid_off(molid)
    integer, intent(in) :: molid !< molecule id

    !__spill check__
    if (molid .gt. NUM_MOL .or. molid .lt. 1) then
      call pfant_halt('Invalid molecule id: '//int2str(molid)//' (valid: 1 to '//&
       int2str(NUM_MOL)//')')
    end if

    if (molecule_is_on(molid)) then
      ! The condition we-re in prevents duplication
      config_num_mol_off = config_num_mol_off+1
      config_molids_off(config_num_mol_off) = molid
      call log_info('molecule id '//int2str(molid)//' added to config_molids_off')
    else
      call log_warning('molecule id '//int2str(molid)//' *already turned off*')
    end if

    call make_molids_on()
  end

  !=======================================================================================
  !> Fills config_molids_on and config_num_mol_on based on their complements.
  !>
  !> @todo see what is public and what is private in this module

  subroutine make_molids_on()
    integer i_mol, j, molid
    logical is_off

    i_mol = 0
    do molid = 1, NUM_MOL
      is_off = .false.  ! Whether molecule I_MOL is off
      do j = 1, config_num_mol_off
        if (molid .eq. config_molids_off(j)) then
          is_off = .true.
          exit
        end if
      end do
      if (.not. is_off) then
        i_mol = i_mol+1
        config_molids_on(i_mol) = molid
      end if
    end do
    config_num_mol_on = i_mol

    flag_make_molids_on = .true.
  end
end module config

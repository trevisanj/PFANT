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

module config
  use logging
  use options2
  use misc
  implicit none

  integer, parameter :: NUM_MOL=21  !< Number of molecules configured in the program.
                                    !< Conceptually, this should be defined in molecules.f, but there would be cyclic USEs
                                    !< @todo maybe create a module called "dimensions"...not before I check the other modules to incorporate: hydro2 and inewmarcs

  !=====
  ! "Tasks"
  !=====


  !=====
  ! File names
  !=====
  character*256 :: &
   config_fn_dissoc        = 'dissoc.dat',        & !< ?doc?
   config_fn_main          = 'main.dat',          & !< ?doc?
   config_fn_partit        = 'partit.dat',        & !< ?doc?
   config_fn_absoru2       = 'absoru2.dat',       & !< ?doc?
   config_fn_modeles       = 'modeles.mod',       & !< ?doc?
   config_fn_abonds        = 'abonds.dat',        & !< ?doc?
   config_fn_atomgrade     = 'atomgrade.dat',     & !< ?doc?
   config_fn_moleculagrade = 'moleculagrade.dat', & !< ?doc?
   config_fn_lines         = 'lines.pfant',       & !< ?doc?
   config_fn_log           = 'log.log'              !< ?doc?

  character*256 :: config_inputdir = './'  !< Directory prefix for all files

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
  !> Interpolation type of turbul_VT
  !> @li 1: (default) linear
  !> @li 2: parabolic
  integer :: config_interp = 1
  !> Selector for subroutines FLIN1() and FLINH():
  !> @li 0: (default) integration using 6/7 points depending on main_PTDISK;
  !> @li 1: 26-point integration
  integer :: config_kik = 0

  !> Whether to allow PFANT to run if none of the specified filetoh files is found
  logical :: config_allow_no_filetoh = .false.


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
  !> Input directory without trailling spaces and ending with a "/"
  character(len=:), private, allocatable :: inputdir_trim
  !> Number of possible command-line options
  integer, parameter, private :: NUM_OPTIONS = 14
  !> options
  type(option), private :: options(NUM_OPTIONS), opt

  private make_molids_on
contains
  !=======================================================================================
  !> Initializes the options list
  !> @note It is possible to break description lines using &lt;br&gt;
  !>
  !> @note To indent 2nd, 3rd etc. lines of a paragraph, use the @c IND constant after a
  !> &lt;br&gt;

  subroutine init_options()
    character(3), parameter :: IND = '.. ' ! indentation string

    options( 1) = option('help', 'h', .false., '', '', &
      'Displays this help text.')
    options( 2) = option('loglevel', 'l', .TRUE., 'level', 'debug', &
      'logging level (debug/info/warning/error/critical/halt)')
    options( 3) = option('interp', 'i', .TRUE., 'type', int2str(config_interp), &
     'interpolation type for subroutine turbul() (1: linear; 2: parabolic)')
    options( 4) = option('kik', 'k', .TRUE., 'type', int2str(config_kik), &
     'selector for subroutines flin1() and flinh()<br>'//&
     IND//'0: integration using 6/7 points depending on main_ptdisk;<br>'//&
     IND//'1: 26-point integration)')

    !> @todo Find names for each file and update options help

    ! easier to see and edit in single line
    options( 5) = option('fn_dissoc',        ' ', .true., 'file name', config_fn_dissoc, &
     'input file name - dissociative equilibrium')
    options( 6) = option('fn_main',          ' ', .true., 'file name', config_fn_main, &
      'input file name - main configuration')
    options( 7) = option('fn_partit',        ' ', .true., 'file name', config_fn_partit, &
      'input file name - partition functions')
    options( 8) = option('fn_absoru2',       ' ', .true., 'file name', config_fn_absoru2, &
      'input file name - absoru2')
    options( 9) = option('fn_modeles',       ' ', .true., 'file name', config_fn_modeles, &
      'input file name - model')
    options(10) = option('fn_abonds',        ' ', .true., 'file name', config_fn_abonds, &
      'input file name - atomic abundances')
    options(11) = option('fn_atomgrade',     ' ', .true., 'file name', config_fn_atomgrade, &
      'input file name - atomic lines')
    options(12) = option('fn_moleculagrade', ' ', .true., 'file name', config_fn_moleculagrade, &
      'input file name - molecular lines')
    options(13) = option('inputdir',         ' ', .true., 'directory name', config_inputdir, &
      'directory containing input files')
    options(14) = option('molid_off',        ' ', .true., 'molecule id', '', &
      'id of molecule to be "turned off" (1 to '//int2str(NUM_MOL)//').<br>'//&
      'Note: This option can be repeated.')

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
    integer i, n
    character(1) :: BACKSLASH = char(92)  ! Doxygen doesn't like a backslash appearing in the code


    call init_options()
    call parse_args()

    ! Configures data directory
    inputdir_trim = trim(config_inputdir)
    n = len(inputdir_trim)
    do i = 1, n  ! Replaces backslash by forward slash
      if (inputdir_trim(i:i) .eq. BACKSLASH) inputdir_trim(i:i) = '/'
    end do
    if (inputdir_trim(n:n) .ne. '/') inputdir_trim = inputdir_trim // '/'

    call make_molids_on()

    flag_setup = .true.
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
    character*128 lll
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
              iTemp = parseint(opt, o_arg)
              select case (iTemp)
                case (1, 2)
                  config_INTERP = iTemp
                  call log_assignment('config_interp', int2str(config_interp))
                case default
                  err_out = .TRUE.
              end select

            case ('kik')
              iTemp = parseint(opt, o_arg)
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

            case ('molid_off')
              iTemp = parseint(opt, o_arg)
              call add_molid_off(iTemp)
          end select

          if (err_out) then
            write (lll, *) 'Argument out of range for option ', get_option_name(opt)
            call pfant_halt(lll)
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
    !> Converts string to integer with error logging

    integer function parseint(opt, s)
      !> Option, will be used only in case of error
      type(option), intent(in) :: opt
      !> If the parsed option requires an argument, arg contains
      !> the first len(arg) (but at most 500) characters of that argument.
      !> Otherwise its value is undefined. If the arguments length exceeds 500
      !> characters and err is .true., a warning is issued.
      character(len=*), intent(in) :: s
      character*128 lll

      read(s, *, err=20) parseint
      go to 30

      20 write(lll, *) 'Error parsing option ', get_option_name(opt), &
       ': invalid integer argument: ''', trim(s), ''''
      call pfant_halt(lll)

      30 continue
    end

  end subroutine parse_args

  !=======================================================================================
  !> Concatenates config_inputdir with specific filename
  !>
  !>   - sets up configuration defaults,
  !>   - parses command-line arguments, and
  !>   - does other necessary operations.
  !>
  !> Must be called at system startup

  function fullpath(filename) result(res)
    character(len=*), intent(in) :: filename  !< File name
    character(len=:), allocatable :: res ! trimmed inputdir

    res = inputdir_trim // trim(filename)
  end

  !=======================================================================================
  !> Returns molecule id given index
  !>
  !> Molecule id is a number from 1 to NUM_MOL, which is uniquely related to a chemical molecule within pfant.

  function get_molid(i_mol)
    integer i_mol, get_molid
    character*80 lll  !__logging__

    !__assertion__
    if (.not. flag_make_molids_on) call pfant_halt('get_molid(): forgot to call make_molids_on()')

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

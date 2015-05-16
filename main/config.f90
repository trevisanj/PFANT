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
!> Command-line parsing and respective global variable declarations
!> - Configuration globals with their default values
!> - Routines to parse command-line arguments
!> - All globals have prefix "config_"
!>
!> @todo config_fn_* documentation is important because it is a good place to "define" what each file is

module config
  use logging
  implicit none

  integer, parameter :: NUM_MOL=21  ! Number of molecules configured in the program.
                                    ! Conceptually, this should be defined in molecula.f, but there would be cyclic USEs

  !> Logging level (from @ref logging module).
  !>
  !> Possible values: logging::LOGGING_HALT, logging::LOGGING_CRITICAL, logging::LOGGING_ERROR,
  !> logging::LOGGING_WARNING, logging::LOGGING_INFO (default), logging::LOGGING_DEBUG
  integer :: config_loglevel = LOGGING_INFO

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

  !=====
  ! Calculated variables
  !=====
  !> List of molecule ids that are switched on. Complement of config_molids_off. Calculated by make_molids()
  integer, dimension(NUM_MOL) :: config_molids_on
  !> This is actually <code> = NUM_MOL-config_num_mol_off </code>
  integer config_num_mol_on

  !=====
  ! Private variables
  !=====
  logical, private :: flag_setup = .false.
contains


  !================================================================================================================================
  !> Does various setup operations.
  !>
  !>   - sets up configuration defaults,
  !>   - parses command-line arguments, and
  !>   - does other necessary operations.
  !>
  !> Must be called at system startup

  subroutine config_setup()
    ! Parses command line
    call parseargs()

    ! Configures modules
    logging_level = config_loglevel  ! sets logging level at logging module based on config variable


    call make_molids()

    flag_setup = .true.
  end



  !================================================================================================================================
  !> Returns molecule id given index
  !>
  !> Molecule id is a number from 1 to NUM_MOL, which is uniquely related to a chemical molecule within pfant.

  function get_molid(i_mol)
    integer i_mol, get_molid
    character*80 s  !__logging__

    !--assertion--!
    if (.not. flag_setup) call pfant_halt('get_molid(): forgot to call config_setup()')

    !__spill check__
    if (i_mol .gt. config_num_mol_on) then
      write (s, *) 'get_molid(): invalid molecule index i_mol (', &
       i_mol, ') must be maximum ', config_num_mol_on
      call pfant_halt(s)
    end if

    get_molid = config_molids_on(i_mol)
    return
  end

  !================================================================================================================================
  !> Returns .TRUE. or .FALSE. depending on whether molecule represented by molid is "on" or "off"

  function molecule_is_on(molid)
    integer molid, j
    logical molecule_is_on

    !--assertion--!
    if (.not. flag_setup) &
     call pfant_halt('molecule_is_on(): forgot to call config_setup()')

    molecule_is_on = .true.
    do j = 1, config_num_mol_off
      if (molid .eq. config_molids_off(j)) then
        molecule_is_on = .false.
        exit
      end if
    end do
  end


  !================================================================================================================================
  !> Fills config_molids_on and config_num_mol_on

  subroutine make_molids()
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
  end subroutine


  !================================================================================================================================
  !> Parses and validates all command-line arguments.
  !>
  !> @todo will inform ' ' for options with no short equivalent, but I don't know if options2.f90 is prepared for this
  !>
  !> @todo Documentation: somehow think how to link option descriptions below, their default values, and the documentation for their respective config_* at their declarations.
  !>

  subroutine parseargs()
    use options2
    integer k  !> @todo for debugging, take it out
    integer o_len, o_stat, o_remain, o_offset, o_index, iTemp
    character*500 o_arg
    character*128 lll
    logical err_out
    type(option) options(11), opt




    options( 1) = option('loglevel', 'l', .TRUE., 'Logging level (1: debug; 2: info; 3: '//&
     'warning; 4: error; 5: critical; 6: halt)', 'level')
    options( 2) = option('interp', 'i', .TRUE., 'Interpolation type for subroutine '//&
     'TURBUL() (1: linear; 2: parabolic)', 'type')
    options( 3) = option('kik', 'k', .TRUE., 'Selector for subroutines FLIN1() and '//&
     'FLINH() (0 (default): integration using 6/7 points depending on main_PTDISK; '//&
     '1: 26-point integration)', 'type')
    options( 4) = option('fn_dissoc',    ' ', .true., 'file name', 'file name')
    options( 5) = option('fn_main',      ' ', .true., 'file name', 'file name')
    options( 6) = option('fn_partit',    ' ', .true., 'file name', 'file name')
    options( 7) = option('fn_absoru2',   ' ', .true., 'file name', 'file name')
    options( 8) = option('fn_modeles',   ' ', .true., 'file name', 'file name')
    options( 9) = option('fn_abonds',    ' ', .true., 'file name', 'file name')
    options(10) = option('fn_atomgrade', ' ', .true., 'file name', 'file name')
    options(11) = option('fn_atomgrade', ' ', .true., 'file name', 'file name')

    err_out = .FALSE.

    do while (.TRUE.)
      call getopt(options, o_index, o_arg, o_len, o_stat, o_offset, o_remain)

      write(*,*) 'o_index = ', o_index
      write(*,*) 'o_arg = ', o_arg
      write(*,*) 'o_len = ', o_len
      write(*,*) 'o_stat = ', o_stat
      write(*,*) 'o_offset = ', o_offset
      write(*,*) 'o_remain = ', o_remain
      write(*,*) '---------------------------'

      select case(o_stat)
        case (1,2,3)  ! parsing stopped (no error)
           exit
        case (0)  ! option successfully parsed
          opt = options(o_index)

          ! "Uses" config options: validates and assigns to proper config_* variables.
          select case(opt%name)  ! It is more legible select by option name than by index
            case ('loglevel')
              iTemp = parseint(opt, o_arg)
              select case (iTemp)
                case (10, 20, 30, 40, 50, 60)
                  config_loglevel = iTemp*10
                  write(*,*) 'setting logging level to ', config_loglevel
                case default
                  err_out = .TRUE.
              end select

            case ('interp')
              iTemp = parseint(opt, o_arg)
              select case (iTemp)
                case (1, 2)
                  config_INTERP = iTemp
                  write(*,*) 'setting config_interp to ', config_interp
                case default
                  err_out = .TRUE.
              end select

            case ('kik')
              iTemp = parseint(opt, o_arg)
              select case(iTemp)
                case (0, 1)
                  config_KIK = iTemp
                case default
                  err_out = .TRUE.
              end select

            case ('fn_dissoc')
              call assign_fn(o_arg, config_fn_dissoc)
            case ('fn_main')
              call assign_fn(o_arg, config_fn_main)
            case ('fn_partit')
              call assign_fn(o_arg, config_fn_partit)
            case ('fn_absoru2')
              call assign_fn(o_arg, config_fn_absoru2)
            case ('fn_modeles')
              call assign_fn(o_arg, config_fn_modeles)
            case ('fn_abonds')
              call assign_fn(o_arg, config_fn_abonds)
            case ('fn_atomgrade')
              call assign_fn(o_arg, config_fn_atomgrade)
            case ('fn_moleculagrade')
              call assign_fn(o_arg, config_fn_moleculagrade)
            case ('fn_lines')
              call assign_fn(o_arg, config_fn_lines)
            case ('fn_log')
              call assign_fn(o_arg, config_fn_log)
          end select

          if (err_out) then
            write (lll, *) 'Argument out of range for option ', get_option_name(opt)
            call pfant_halt(lll)
          end if

      end select

      k = k+1
      if (k == 20) then
        stop 'sort this shit'
      end if
    end do

  contains
    !----------
    !> Assigns option argument to filename variable
    !>
    !> @todo This subroutine is currently not doing much but in the future some filename
    !> validation could be added.

    subroutine assign_fn(arg, dest)
      character(len=*), intent(in)  :: arg !< command-line option argument
      character(len=*), intent(out) :: dest!< One of config_fn_* variables
      dest = arg
    end

    !----------
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
      call give_error(lll)

      30 continue
    end


  end subroutine parseargs





END MODULE

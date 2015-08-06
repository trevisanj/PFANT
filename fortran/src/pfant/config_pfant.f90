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

!> @todo config_fn_* documentation is important because it is a good place to "define" what each file is

module config_pfant
  use config_base
  use misc
  use molecules_ids
  implicit none

  !=====
  ! File names
  !=====
  character*64 :: &
   config_fn_dissoc        = 'dissoc.dat',        & !< option: --fn_dissoc
   config_fn_partit        = 'partit.dat',        & !< option: --fn_partit
   config_fn_absoru2       = 'absoru2.dat',       & !< option: --fn_absoru2
   config_fn_modeles       = 'modeles.mod',       & !< option: --fn_modeles
   config_fn_abonds        = 'abonds.dat',        & !< option: --fn_abonds
   config_fn_atomgrade     = 'atomgrade.dat',     & !< option: --fn_atomgrade
   config_fn_molecules = 'molecules.dat', & !< option: --fn_molecules
   config_fn_lines         = 'lines.pfant',       & !< option: --fn_lines
   config_fn_log           = 'log.log'              !< option: --fn_log


  !=====
  ! Misc
  !=====
  integer :: config_interp = 1  !< option: --interp
  integer :: config_kik = 0     !< option: --kik

  !> Whether to allow PFANT to run if none of the specified filetoh files is found
  logical :: config_allow_no_filetoh = .false.

  save
contains

  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_pfant_init()
    execonf_name = 'pfant'
    execonf_handle_option => config_pfant_handle_option
    execonf_init_options => config_pfant_init_options
    call config_base_init()
  end

  !=======================================================================================
  !> Initializes executable-specific options

  subroutine config_pfant_init_options()
    call add_option('interp', 'i', .TRUE., 'type', int2str(config_interp), &
     'interpolation type for subroutine turbul()<br>'//&
     IND//'1: linear;<br>'//&
     IND//'2: parabolic)')
    call add_option('kik', 'k', .TRUE., 'type', int2str(config_kik), &
     'selector for subroutines flin1() and flinh()<br>'//&
     IND//'0: integration using 6/7 points depending on main_ptdisk;<br>'//&
     IND//'1: 26-point integration)')

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

    call add_option('molid_off',        ' ', .true., 'molecule id', '', &
     'id of molecule to be "turned off" (1 to '//int2str(NUM_MOL)//').<br>'//&
     '*Note*: This option may be repeated as many times as necessary.')
  end

  !=======================================================================================
  !> Handles options for PFANT main executable

  function config_pfant_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res, iTemp

    res = HANDLER_OK

    select case(opt%name)
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

      case ('molid_off')
        iTemp = parse_aux_str2int(opt, o_arg)
        call add_molid_off(iTemp)

      case default
        ! if does not handle here, passes on to base handler
        res = config_base_handle_option(opt, o_arg)
    end select
  end
end

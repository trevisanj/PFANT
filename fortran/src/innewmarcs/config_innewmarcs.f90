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

!> innewmarcs-specific configuration

module config_innewmarcs
  use config_base
  use misc
  implicit none

  integer, parameter :: LEN_TIRB = 15 ! size of variable config_tirb

  ! note: maintained variable names found in original innewmarcs.f
  ! (with "config_" prefix)

  character*20 :: config_open_status = 'unknown' !< option: --open_status

  character*64 :: &
   config_fn_modeles = 'modeles.mod', &    !< option: --fn_modeles
   config_fn_moddat = 'modeles.dat', &     !< option: --fn_moddat
   config_fn_gridslist = 'gridslist.dat' !< option: --fn_gridslist

  character*25 :: config_modcode = 'NoName' !< option: --modcode
  !> option: --tirb
  !> @sa innewmarcs_init()
  character(LEN_TIRB) :: config_tirb = '?'
  !> option: --teff
  !> @sa get_teff()
  real*4 :: config_teff = -1
  !> option: --glog
  !> @sa get_teff()
  real*4 :: config_glog = -1
  !> option: --asalog.
  !> @note Name changed from "amet" to "asalog" to conform with pfant and hydro2
  !> @sa get_asalog()
  real*4 :: config_asalog = -1
  !> option: --inum
  !> @sa get_id()
  integer :: config_inum = 0
contains
  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_innewmarcs_init()
    execonf_name = 'innwemarcs'
    execonf_handle_option => config_innewmarcs_handle_option
    execonf_init_options => config_innewmarcs_init_options
    call config_base_init()
  end

  !=======================================================================================
  !> Initializes innewmarcs-specific options

  subroutine config_innewmarcs_init_options()
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
  end

  !=======================================================================================
  !> Handles options for innewmarcs executable

  function config_innewmarcs_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res

    res = HANDLER_OK

    select case(opt%name)
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
      case default
        ! if does not handle here, passes on to base handler
        res = config_base_handle_option(opt, o_arg)
    end select
  end
end

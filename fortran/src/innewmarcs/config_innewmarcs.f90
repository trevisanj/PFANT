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

  character*192 :: config_refdir = '.' !< option: --refdir

  character*64 :: &
   config_nomfimod = 'modeles.mod', & !< option: --nomfimod
   config_nomfidat = 'modeles.dat'    !< option: --nomfidat
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
  !> Initializes innewmarcs-specific options

  integer function config_init_options(k)
    !> k is the index of the last initialized option
    integer, intent(in) :: k

    k = k+1
    options(k) = option('refdir',' ', .true., 'directory name', config_refdir, &
     'Directory containing reference atmospheric models.<br>'//&
     'This directory must contain a file named "modelmap.dat" and<br>'//&
     'several ".mod" binary files. See inewmarcs.f90::read_modelmap() for more info.')

    k = k+1
    options(k) = option('open_status',' ', .true., 'string', config_open_status, &
     'File open mode for binary file<br>'//&
     IND//'new: file must not exist<br>'//&
     IND//'old: file must exist<br>'//&
     IND//'replace: replaces file if exists, otherwise creates new')

    k = k+1
    options(k) = option('nomfimod',' ', .true., 'file name', config_nomfimod, &
     'Name of binary file<br>'//&
     '*Note*: file is opened in directory specified in --inputdir')

    k = k+1
    options(k) = option('nomfidat',' ', .true., 'file name', config_nomfidat, &
     'Name of ASCII file<br>'//&
     '*Note*: file is opened in directory specified in --inputdir')

    k = k+1
    options(k) = option('modcode',' ', .true., 'string up to 25 characters', config_modcode, &
     '"Model name"')

    k = k+1
    options(k) = option('tirb',' ', .true., 'string up to 15 characters', config_tirb, &
     '"Titre"')

    k = k+1
    options(k) = option('teff',' ', .true., 'real value', real42str(config_teff), &
     '"Teff"')

    k = k+1
    options(k) = option('glog',' ', .true., 'real value', real42str(config_glog), &
     '"log g"')

    k = k+1
    options(k) = option('asalog',' ', .true., 'real value', real42str(config_asalog), &
     '"[M/H]"')

    k = k+1
    options(k) = option('inum',' ', .true., 'real value', '<"main_inum" variable (taken from main configuration file)>', &
     'Record id within atmospheric model binary file')

    config_init_options = k
  end

  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_init()
    execonf_name = 'innwemarcs'
    execonf_handle_option => config_handle_option
    execonf_init_options => config_init_options
    execonf_num_options = 10
    call config_base_init()
  end

  !=======================================================================================
  !> Handles options for innewmarcs executable

  function config_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res

    res = HANDLER_OK

    select case(opt%name)
      case ('refdir')
        call parse_aux_assign_fn(o_arg, config_refdir, 'refdir')
      case ('open_status')
        call parse_aux_assign_fn(o_arg, config_open_status, 'open_status')
      case ('nomfimod')
        call parse_aux_assign_fn(o_arg, config_nomfimod, 'nomfimod')
      case ('nomfidat')
        call parse_aux_assign_fn(o_arg, config_nomfidat, 'nomfidat')
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

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

!> nulbad-specific configuration

module config_nulbad
  use config_base
  use misc
  implicit none


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
contains

  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_nulbad_init()
    execonf_name = 'nulbad'
    execonf_handle_option => config_nulbad_handle_option
    execonf_init_options => config_nulbad_init_options
    call config_base_init()
  end

  !=======================================================================================
  !> Initializes nulbad-specific options

  subroutine config_nulbad_init_options()

    call add_option('fn_flux', ' ', .true., 'file name', &
     '<"main_fn_flux" variable>.norm (taken from main configuration file)>', &
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
  !> Handles options for nulbad executable

  function config_nulbad_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res

    res = HANDLER_OK

    select case(opt%name)

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
        ! if does not handle here, passes on to base handler
        res = config_base_handle_option(opt, o_arg)
    end select
  end
end

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
    config_fileflux = '?', &         !< option: --fileflux
    config_filecv = '?'                !< option: --filecv
contains

  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_nulbad_init()
    execonf_name = 'nulbad'
    execonf_handle_option => config_nulbad_handle_option
    execonf_init_options => config_nulbad_init_options
    execonf_num_options = 7
    call config_base_init()
  end

  !=======================================================================================
  !> Initializes nulbad-specific options

  function config_nulbad_init_options(j) result (k)
    !> k is the index of the last initialized option
    integer, intent(in) :: j
    integer :: k

    k = j+1
    options(k) = option('fileflux', ' ', .true., 'file name', &
     '<"main_fileflux" variable>.norm (taken from main configuration file)>', &
     'Flux file name')
    k = k+1
    options(k) = option('norm',     ' ', .true., 'T/F', logical2str(config_norm), &
      'Is spectrum normalized?')
    k = k+1
    options(k) = option('flam',     ' ', .true., 'T/F', logical2str(config_flam), &
      'Fnu to FLambda transformation?')
    k = k+1
    options(k) = option('filecv',     ' ', .true., 'file name', '<flux file name>.nulbad', &
      'output file name, which will have the convolved spectrum')
    k = k+1
    options(k) = option('pat',      ' ', .true., 'real value', '<"main_pas" variable> (taken from main configuration file)', &
      'step ?doc?')
    k = k+1
    options(k) = option('convol',   ' ', .true., 'T/F', logical2str(config_convol), &
      'Apply convolution?')
    k = k+1
    options(k) = option('fwhm',     ' ', .true., 'real value', '<"main_fwhm" variable> (taken from main configuration file)', &
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
      case ('filecv')
        call parse_aux_assign_fn(o_arg, config_filecv, 'config_filecv')
      case ('flam')
        config_flam = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_flam', logical2str(config_flam))
      case ('fileflux')
        call parse_aux_assign_fn(o_arg, config_fileflux, 'config_fileflux')
      case ('norm')
        config_norm = parse_aux_str2logical(opt, o_arg)
        call parse_aux_log_assignment('config_norm', logical2str(config_norm))
      case default
        ! if does not handle here, passes on to base handler
        res = config_base_handle_option(opt, o_arg)
    end select
  end
end

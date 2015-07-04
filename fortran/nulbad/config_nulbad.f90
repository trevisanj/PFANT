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

  ! note: maintained variable names found in original nulbadgrade.f
  !       (however with "config_") prefix
  
  
  some variables witll have a default, others won't
  
  
  logical :: &
   config_norm = .true., &                       !< option: --norm
   config_flam = .true., &                       !< option: --flam
   config_convol = .true.                        !< option: --convol
  real*8 :: &
   config_fwhm = 0.13, &                         !< option: --fwhm
   config_pat = 0.02                             !< option: --pat
  character*64 :: &
    config_fileflux = 'norm.fileflux', &         !< option: --fileflux
    config_flcv = ''                             !< option: --flcv
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


    k = k+1
    options(k) = option('fileflux', ' ', .true., 'file name', config_fileflux, &
      'Flux file name<br>'//&
      '*Note*: looks for file in *output* directory, because the flux'//&
      '        file is a PFANT output.')
    k = k+1
    options(k) = option('norm',     ' ', .true., 'T/F', logical2str(config_norm), &
      'Is spectrum normalized?<br>'//&
      '*Note*: this setting is ignored in ''pfant-nulbad'' mode.')
    k = k+1
    options(k) = option('flam',     ' ', .true., 'T/F', logical2str(config_flam), &
      'Fnu to FLambda transformation?')
    k = k+1
    options(k) = option('flcv',     ' ', .true., 'file name', '<flux file name>.nulbad', &
      'output file name')
    k = k+1
    options(k) = option('pat',      ' ', .true., 'real value', real82str(config_pat), &
      'step ?doc?')
    k = k+1
    options(k) = option('convol',   ' ', .true., 'T/F', logical2str(config_convol), &
      'Apply convolution?')
    k = k+1
    options(k) = option('fwhm',     ' ', .true., 'real value', real82str(config_fwhm), &
      'Full-width-half-maximum of Gaussian function')
  end


  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_init()
    ex_config_option_handler => config_handle_option
    ex_config_num_options = ex_config_num_options+7
    call config_init_options()
    call config_base_init()
  end



  !=======================================================================================
  !> Handles options for nulbad executable

  function config_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res, iTemp

    res = HANDLER_OK

    select case(opt%name)

      case ('fwhm')
        config_fwhm = str2real(opt, o_arg)
        call log_assignment('config_fwhm', real82str(config_fwhm))
      case ('convol')
        config_convol = str2logical(opt, o_arg)
        call log_assignment('config_convol', logical2str(config_convol))
      case ('pat')
        config_pat = str2real(opt, o_arg)
        call log_assignment('config_pat', real82str(config_pat))
      case ('flcv')
        call assign_fn(o_arg, config_flcv, 'config_flcv')
      case ('flam')
        config_flam = str2logical(opt, o_arg)
        call log_assignment('config_flam', logical2str(config_flam))
      case ('fileflux')
        call assign_fn(o_arg, config_fileflux, 'config_fileflux')
      case ('norm')
        config_norm = str2logical(opt, o_arg)
        call log_assignment('config_norm', logical2str(config_norm))
      case default
        ! if does not handle here, passes on to base handler
        res = config_base_handle_option(opt, o_arg)
    end select
  end
end

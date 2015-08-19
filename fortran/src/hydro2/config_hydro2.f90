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

!> hydro2-specific configuration
!>
!> @todo issue ask blb lots of options in infile:main are also configurable through command-line
!> and repeated in innewmarcs. Ít could be defined that the source for teff, glog, asalog, inum, ptdisk
!> will be infile:main and remove these config options. But time will tell. With pypfant we will be able
!> to see how the program will be used etc.

module config_hydro2
  use config_base
  use misc
  implicit none




contains

  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_hydro2_init()
    execonf_name = 'hydro2'
    execonf_handle_option => config_hydro2_handle_option
    execonf_init_options => config_hydro2_init_options
    call config_base_init()
  end

  !=======================================================================================
  !> Initializes hydro2-specific options

  subroutine config_hydro2_init_options()
  end

  !=======================================================================================
  !> Handles options for hydro2 executable

  function config_hydro2_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res

    res = HANDLER_OK

    select case(opt%name)



      case default
        ! if does not handle here, passes on to base handler
        res = config_base_handle_option(opt, o_arg)
    end select
  end
end

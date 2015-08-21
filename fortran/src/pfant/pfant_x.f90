!> Declaration and initialization of x_* variables
!>
!> These variable values may come either from infile:main or command-line options.

module pfant_x
  use logging
  use reader_main
  use config
  implicit none

  character*64 :: x_flprefix

contains

  !> Initializes x_* variables
  !>
  !> Note: to be called after read_main()

  subroutine pfant_init_x()
    if (config_flprefix .eq. '?') then
      x_flprefix = main_flprefix
      call parse_aux_log_assignment('x_flprefix', x_flprefix)
    else
      x_flprefix = config_flprefix
    end if
  end
end

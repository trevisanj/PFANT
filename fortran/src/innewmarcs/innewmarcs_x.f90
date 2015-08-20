!> Declaration and initialization of x_* variables
!>
!> These variable values may come either from infile:main or command-line options.

module innewmarcs_x
  use logging
  use reader_main
  use config
  implicit none

  ! x_* values may come either from command line or infile:main
  real*4 :: x_teff, x_glog, x_asalog
  character(LEN_TIRB) :: x_tirb
  integer :: x_inum

contains

  !> Initializes x_* variables

  subroutine innewmarcs_init_x()

    ! values in config_* variables have preference, but if they are uninitialized, will
    ! pick values from infile:main
     x_teff = real(config_teff)
     x_glog = real(config_glog)
     x_asalog = real(config_asalog)
     x_inum   = config_inum

     x_tirb = config_tirb
    if (config_inum .lt. 1) then
      call assure_read_main()
      if (main_inum .lt. 1) then
        ! note: here this consistency check is considered an assertion, because it should
        ! be validated upon file reading.
        call pfant_halt('Invalid value for main_inum: '//int2str(main_inum), is_assertion=.true.)
      end if
      x_inum = main_inum
      call parse_aux_log_assignment('x_inum', int2str(x_inum))
    end if
    if (config_tirb .eq. '?') then
      call assure_read_main()
      x_tirb = main_titrav
      call parse_aux_log_assignment('x_tirb', trim(x_tirb))
    end if
    if (config_teff .eq. -1) then
      call assure_read_main()
      x_teff = real(main_teff)  ! explicit real(8)-to-real(4) conversion to shut up warning
      call parse_aux_log_assignment('x_teff', real42str(x_teff))
    end if
    if (config_glog .eq. -1)  then
      call assure_read_main()
      x_glog = real(main_glog)
      call parse_aux_log_assignment('x_glog', real42str(x_glog))
    end if
    if (config_asalog .eq. -1) then
      call assure_read_main()
      x_asalog = real(main_asalog)
      call parse_aux_log_assignment('x_asalog', real42str(x_asalog))
    end if
  end
end

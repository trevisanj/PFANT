!> Declaration and initialization of x_* variables
!>
!> These variable values may come either from infile:main or command-line options.

module hydro2_x
  use logging
  use reader_main
  use config_hydro2
  implicit none

  !> Option for subroutine fluxis() (6/7/26 points for integration).
  !> @note This was originally set to .FALSE. but I opened it for configuration
  !>       because this information is available inside infile:main
  logical :: x_ptdisk

  !> @todo at least I could write a module shared between innewmarcs and hydro2 to avoid all this code duplication

  real*8 :: x_teff, x_glog, x_asalog
  integer :: x_inum

  real*8 :: x_vvt
  logical :: x_amores

contains!> Initializes x_* variables

  subroutine hydro2_init_x()

    if (config_ptdisk .eq. -1) then
      call assure_read_main()
      x_ptdisk = main_ptdisk
    else
      x_ptdisk = int2logical(config_ptdisk)
    end if


    if (config_amores .eq. -1) then
      ! No default because it was originally asking user
      call pfant_halt('Option --amores has not been set')
    else
      x_amores = int2logical(config_amores)
    end if

    if (config_kq .eq. -1) then
      ! No default because it was originally asking user
      call pfant_halt('Option --amores has not been set')
    else
      x_amores = int2logical(config_amores)
    end if

    ! duplicated in innewmarcs
    x_teff = config_teff
    x_glog = config_glog
    x_asalog = config_asalog
    x_inum = config_inum
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
    if (config_teff .eq. -1) then
      call assure_read_main()
      x_teff = main_teff
      call parse_aux_log_assignment('x_teff', real82str(x_teff))
    end if
    if (config_glog .eq. -1)  then
      call assure_read_main()
      x_glog = main_glog
      call parse_aux_log_assignment('x_glog', real82str(x_glog))
    end if
    if (config_asalog .eq. -1) then
      call assure_read_main()
      x_asalog = main_asalog
      call parse_aux_log_assignment('x_asalog', real82str(x_asalog))
    end if


    x_vvt = config_vvt
    if (config_vvt .eq. -1) then
      call assure_read_main()

      if (main_ivtot .gt. 1) then
        call pfant_halt('Tried to read vvt from main configuration file, '//&
         'but not prepared for multiple microturbulence velocities')
      end if
      x_vvt = main_vvt(1)
      call parse_aux_log_assignment('x_vvt', real82str(x_vvt))
    end if
  end
end

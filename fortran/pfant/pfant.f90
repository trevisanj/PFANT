!> PFANT main executable: spectral synthesis
!>
!> @todo Canonic cases: Sun, Arcturus

program pfant
  use config_pfant
  use logging
  use synthesis
  use welcome
  implicit none

  ! startup
  write(*,*) pfant_version('PFANT')
  write(*,*) ''
  call molecules_ids_init()
  call config_init()
  call print_welcome(6)

  ! Spectral synthesis
  call synthesis_()

!  select case (config_mode)
!    case ('pfant')
!      call synthesis_()
!    case ('nulbad')
!      call nulbad_complete()
!    case ('pfant-nulbad')
!      synthesis_flag_ffnu = .true.
!      ! This overrides possible setting from command-line because synthesis_ffnu
!      ! is the normalized spectrum.
!      config_nulbad_norm = .true.
!      call synthesis_()
!      call nulbad_calc(synthesis_ffnu, synthesis_ktot)
!    case default
!      call pfant_halt('Unknown mode: "'//config_mode//'"', is_assertion=.true.)
!  end select

end program pfant

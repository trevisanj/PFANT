!> INNEWMARCS
!>
!> Interpolation d'un modele dans les grilles de modeles de
!> NEWMARCS (2005) en fonction de Teff  log g et Fe/H
!> (lit newmarcsm200.mod  newmarcsm150.mod newmarcsm100.mod
!> newmarcsm075.mod newmarcsm050.mod  newmarcsm025.mod
!> newmarcsp000.mod newmarcsp025.mod  newmarcsp050.mod
!> newmarcsp075.mod newmarcsp100.mod
!>
!> @todo issue ask someone what the following means:
!> Si dans une autre grille les Teff log g sont differents
!> il faudrait modifier le SP locatab. (Une generalisation
!> est possible en introduisant les caracteristiques des
!> tables de modele dans un fichier separe).
!>
!> Le point critique de ce programme est le SP locatab qui
!> determine entre quels modeles on doit interpoler
!> ce SP peut se tester avec le programme locat.f
!> INPLEZ peut marcher en manuel (Manuel=.true.) on dit
!> alors entre quels modeles on veut interpoler.
!>

program innewmarcs
  use config
  use logging
  use synthesis
  use nulbad
  implicit none

  !=====
  ! Startup section
  !=====
  write(*,*) 'PFANT version: 15.5.21-alpha'
  write(*,*) ''
  call config_setup()
  if (logging_level .le. LOGGING_INFO) then
    call print_welcome(6)
  end if
  call log_info('Begin calculus')

  !=====
  ! Task(s)
  !=====
  select case (config_mode)
    case ('pfant')
      call synthesis_()
    case ('nulbad')
      call nulbad_complete()
    case ('pfant-nulbad')
      synthesis_flag_ffnu = .true.
      ! This overrides possible setting from command-line because synthesis_ffnu
      ! is the normalized spectrum.
      config_nulbad_norm = .true.
      call synthesis_()
      call nulbad_calc(synthesis_ffnu, synthesis_ktot)
    case default
      call pfant_halt('Unknown mode: "'//config_mode//'"', is_assertion=.true.)
  end select



  call log_info('End calculus')
end program pfant


!> Displays welcome message

subroutine print_welcome(unit_)
  integer, intent(in) :: unit_
  write(unit_,*) 'Welcome to PFANT - INEWMARCS module'
end

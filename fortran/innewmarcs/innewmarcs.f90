!> INNEWMARCS
!>
!> Interpolation d'un modele dans les grilles de modeles de
!> NEWMARCS (2005) en fonction de Teff, log g et [Fe/H]
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

program innewmarcs
  use config_innewmarcs
  use logging
  use welcome
  use innewmarcs_calc
  implicit none

  ! startup
  write(*,*) pfant_version('INNEWMARCS')
  write(*,*) ''
  call config_init()
  call print_welcome(6)

  ! calculation
  call innewmarcs_calc_()
end

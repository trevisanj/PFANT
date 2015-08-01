! New plan for nulbad
! 1) Get rid of internal setup
! 2) Implement command-line + main.dat setup
! 3) Write nulbad.f90 file

!> programme NULBT=NULBAD_SQ
!>
!>  lecture et convolution sortie fant93
!>
!>  Programme de lecture des flux en binaire, sortant de
!>      Fantomol93 , calculs de 100A en 100A (ou inferieur a 100A)
!>
!> Paula, julho de 2003:
!> - modifiquei para nao normalizar o espectro
!> - a opcao config_convol=F nao estava imprimindo o lambda, corrigi
!> P.Coelho, dez 2003
!> - se config_norm = .TRUE. (saida do pfant ja eh normalizada), nao
!>  altera o valor do fluxo


program nulbad
  use config_nulbad
  use logging
  use welcome
  use nulbad_calc
  implicit none

  call config_nulbad_init()
  call nulbad_calc_()
end

!> HYD2
!>
!> MEUDON OBSERVATORY
!>
!> CALCUL DU PROFIL D UNE RAIE DE L HYDROGENE
!>
!> CE PROGRAMME A ETE ECRIT PAR F.PRADERIE (ANN D AP 1967)
!> TALAVERA Y A INTRODUIT L ELARGISSEMENT DE SELFRESONNANCE (1970)
!> G.HERNANDEZ ET M.SPITE L ONT ADAPTE AU VAX PUIS SUR LA STATION DEC
!>
!> En sortie:
!> @li Un fichier de trace compatible avec GRAFIC (Nom demande)

program hydro2
  use config_innewmarcs
  use logging
  use welcome
  use innewmarcs_calc
  implicit none

  call config_hydro2_init()
  call hydro2_calc_()
end

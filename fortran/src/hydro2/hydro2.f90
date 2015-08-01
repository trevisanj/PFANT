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
  use config_hydro2
  use hydro2_x
  use reader_absoru2
  use reader_modeles
  use hydro2_calc
  implicit none

  call config_hydro2_init()
  call hydro2_init_x()

  call read_absoru2(full_path_i(config_fn_absoru2))  ! LECTURE DES DONNEES ABSORPTION CONTINUE
  call read_modele(full_path_i(config_fn_modeles))   ! LECTURE DU MODELE


  ok ok ok ok ok
  gotta make the call loop
  arg_th

  call hydro2_calc_()
end

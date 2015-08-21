!> PFANT main executable: spectral synthesis
!>
!> @todo Canonic cases: Sun, Arcturus

program pfant
  use config
  use logging
  use synthesis
  use welcome
  use reader_atomgrade
  use reader_filetoh
  use reader_main
  use reader_abonds
  use reader_dissoc
  use reader_partit
  use reader_molecules
  use reader_absoru2
  use reader_hmap
  use pfant_x

  implicit none

  !=====
  ! Startup
  !=====
  execonf_name = 'pfant'
  call molecules_ids_init()
  call config_init()


  call read_dissoc(full_path_w(config_fn_dissoc))
  call read_main(full_path_w(config_fn_main))


  !=====
  ! File reading
  !=====
  call read_dissoc(full_path_w(config_fn_dissoc))
  call read_partit(full_path_w(config_fn_partit))  ! LECTURE DES FCTS DE PARTITION
  call read_absoru2(full_path_w(config_fn_absoru2))  ! LECTURE DES DONNEES ABSORPTION CONTINUE
  call read_modele(full_path_w(config_fn_modeles))  ! LECTURE DU MODELE
  call read_abonds(full_path_w(config_fn_abonds))
  call read_atomgrade(full_path_w(config_fn_atomgrade))
  ! Gets list of hydrogen lines filenames either from infile:main or infile:hmap.
  ! The latter is not the preferred way.
  if (config_hmap) then
    call read_hmap(full_path_w(config_fn_hmap))
  else
    call hmap_copy_from_main()
  end if

  call read_filetoh(main_llzero, main_llfin)
  call read_molecules(full_path_w(config_fn_molecules))


  !=====
  ! Spectral synthesis
  !=====
  ! Initializes variables whose values may come either from infile:main or
  ! command-line argument
  call pfant_init_x()
  ! Does the calculus
  call synthesis_()

end program pfant

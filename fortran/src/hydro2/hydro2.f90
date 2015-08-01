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
  use reader_thmap
  use hydro2_calc
  use logging
  implicit none
  integer i
  type(thmap_row) :: th

  call config_hydro2_init()
  call validate_config()
  call hydro2_init_x()

  call read_absoru2(full_path_i(config_fn_absoru2))  ! LECTURE DES DONNEES ABSORPTION CONTINUE
  call read_modele(full_path_i(config_fn_modeles))   ! LECTURE DU MODELE

  if (config_thmap) then
    call assure_read_main()
    call read_thmap(full_path_i(config_fn_thmap))

    call log_info('Using thmap file')
    write(lll, 10) main_llzero, main_llfin
    10 format ('Interval in main configuration file: [',F14.3,',',F14.3,']')
    call log_info(lll)

    do i = 1, thmap_n
      th = thmap_rows(i)
      if (th%clam .ge. main_llzero .and. th%clam .le. main_llfin) then
        write(lll,20) i, th
        20 format('*** selected row from thmap file'/,&
         '*    row #: ',I2/, &
         '* filename: ',A16/,&
         '*       na: ',I4/,&
         '*       nb: ',I4/,&
         '*     clam: ',F14.3/,&
         '*     kiex: ',E12.7/,&
         '*       c1: ',E12.7)

        call hydro2_calc_(th)
      end if
    end do

  else
    th%fn = config_nomplot
    th%na = config_na
    th%nb = config_nb
    th%clam = config_clam
    th%kiex = config_kiex
    th%c1 = config_c1

    call hydro2_calc_(th)
  end if

contains
  subroutine validate_config()
    logical flag_error
    character(:), parameter :: NOT_SET = ' option has not been set'


    ! (na, nb, clam, kiex, c1) taken from
    !   - command-line argument, or
    !   - infile:thmap??
    if (.not. config_thmap) then
      flag_error = .false.

      ! If not thmap, all the following six options must be set from command line

      if (config_nomplot .eq. '?') then
        call log_halt('--nomplot'//NOT_SET)
        flag_error = .true.
      end if
      ! If not thmap, all five arguments must be set from command line
      if (config_na .eq. -1) then
        call log_halt('--na'//NOT_SET)
        flag_error = .true.
      end if
      if (config_nb .eq. -1) then
        call log_halt('--nb'//NOT_SET)
        flag_error = .true.
      end if
      if (config_clam .eq. -1) then
        call log_halt('--clam'//NOT_SET)
        flag_error = .true.
      end if
      if (config_kiex .eq. -1) then
        call log_halt('--kiex'//NOT_SET)
        flag_error = .true.
      end if
      if (config_c1 .eq. -1) then
        call log_halt('--c1'//NOT_SET)
        flag_error = .true.
      end if

      if (flag_error) then
        call pfant_halt('Set all (--nomplot,--na, --nb, --clam, --kiex, --c1), or use --thmap.')
      end if
    end if
  end
end

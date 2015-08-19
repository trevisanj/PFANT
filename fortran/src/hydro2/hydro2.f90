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
  use reader_hmap
  use hydro2_calc
  use logging
  use max_
  use misc
  implicit none
  integer i, cnt_in
  type(hmap_row) :: th

  call config_hydro2_init()
  call validate_config()
  call hydro2_init_x()

  call read_absoru2(full_path_w(config_fn_absoru2))  ! LECTURE DES DONNEES ABSORPTION CONTINUE
  call read_modele(full_path_w(config_fn_modeles))   ! LECTURE DU MODELE

  if (config_hmap) then
    call assure_read_main()
    call read_hmap(full_path_w(config_fn_hmap))

    call log_info('Using hmap file')
    write(lll, 10) main_llzero, main_llfin
    10 format ('Interval in main configuration file: [',F14.3,',',F14.3,']')
    call log_info(lll)

    cnt_in = 0
    do i = 1, hmap_n
      th = hmap_rows(i)
      if (h_line_is_inside(th%clam, main_llzero, main_llfin)) then
        cnt_in = cnt_in+1
        call log_info('*** selected row from hmap file')
        write(lll,'(''*    row #: '',I2)') i
        call log_info(lll)
        write(lll,'(''* filename: '',A16)') th%fn
        call log_info(lll)
        write(lll,'(''*       na: '',I4)') th%na
        call log_info(lll)
        write(lll,'(''*       nb: '',I4)') th%nb
        call log_info(lll)
        write(lll,'(''*     clam: '',F14.3)') th%clam
        call log_info(lll)
        write(lll,'(''*     kiex: '',E12.7)') th%kiex
        call log_info(lll)
        write(lll,'(''*       c1: '',E12.7)') th%c1
        call log_info(lll)

        call hydro2_calc_(th)
      end if
    end do

    call log_info('Summary: '//int2str(cnt_in)//'/'//int2str(hmap_n)//&
     ' hydrogen lines calculated')

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
    !   - infile:hmap??
    if (.not. config_hmap) then
      flag_error = .false.

      ! If not hmap, all the following six options must be set from command line

      if (config_nomplot .eq. '?') then
        call log_halt('--nomplot'//NOT_SET)
        flag_error = .true.
      end if
      ! If not hmap, all five arguments must be set from command line
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
        call pfant_halt('Set all (--nomplot,--na, --nb, --clam, --kiex, --c1), or use --hmap.')
      end if
    end if
  end
end

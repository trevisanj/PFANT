!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Declaration and initialization of x_* variables
!>
!> These variable values may come either from dfile:main or command-line options.

module innewmarcs_x
  use logging
  use reader_main
  use config
  implicit none

  ! x_* values may come either from command line or dfile:main
  real*4 :: x_teff, x_glog, x_asalog
  character(LEN_TIRB) :: x_tirb
  integer :: x_inum

contains

  !> Initializes x_* variables

  subroutine innewmarcs_init_x()

    ! values in config_* variables have preference, but if they are uninitialized, will
    ! pick values from dfile:main
     x_teff = real(config_teff)
     x_glog = real(config_glog)
     x_asalog = real(config_asalog)
     x_inum   = config_inum

     x_tirb = config_tirb
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
    if (config_tirb .eq. '?') then
      call assure_read_main()
      x_tirb = main_titrav
      call parse_aux_log_assignment('x_tirb', trim(x_tirb))
    end if
    if (config_teff .eq. -1) then
      call assure_read_main()
      x_teff = real(main_teff)  ! explicit real(8)-to-real(4) conversion to shut up warning
      call parse_aux_log_assignment('x_teff', real42str(x_teff, 1))
    end if
    if (config_glog .eq. -1)  then
      call assure_read_main()
      x_glog = real(main_glog)
      call parse_aux_log_assignment('x_glog', real42str(x_glog, 3))
    end if
    if (config_asalog .eq. -1) then
      call assure_read_main()
      x_asalog = real(main_asalog)
      call parse_aux_log_assignment('x_asalog', real42str(x_asalog, 3))
    end if
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
module innewmarcs_calc
  use dimensions
  use reader_modeles
  use config
  use innewmarcs_x
  use reader_gridsmap
  implicit none

  public innewmarcs_calc_

  private

  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  ! Calculated by locatab()
  ! "Dans cette table les modeles entre lesquels on va interpoler ont les numeros:"
  integer id11, id12, id21, id22

  ! Variables calculated by find_two_grid_files()
  character*64 :: nomfipl(2)
  real*4 asalog1, asalog2

  private find_two_grid_files, rangmod, interpol, locatab, readerbn

contains

  !=======================================================================================
  !> Main routine of this module
  !>
  !> @note ASCII file is always opened in status "unknown"

  subroutine innewmarcs_calc_()
    integer, parameter :: UNIT_MOD = 20, UNIT_DAT = 21  ! file units
    type(modele_record) :: &
     aa, bb, cc, dd, &   ! input records
     ee, ff, z1, z2, zz  ! records that are calculated by interpol()

    character*5 :: tira = ' Ple '  !> @todo ask Elvis what does "Ple" mean?
    character*20 tir    ! titre du modele interpole
    character*65 nomfiple
    real*4 bid0, vvt, tostand, t0, t1, tau, to0, tttt
    integer*4 iabon, in, k, n, nna, nnb, nnc, nnd, nntot, nz1, nz2, ntot(2)
    ! Variables that must be real*4, as they are going to be written to .mod file
    real*4 asalalf
    real*4, dimension(MAX_MODELES_NTOT) :: zle, zlp
    real*4 :: a(MAX_MODELES_NTOT*5)  ! modele resultant
    real*4 :: rteff(2,2), rglog(2,2), ralfa(2)

    character(len=:), allocatable :: path  ! full path to .mod file

    !=====
    ! Initialization
    !=====
    if (config_explain) then
      open(unit=UNIT_EXPLAIN, file=join_with_wdir('innewmarcs_explain.txt'), status='replace')
    end if
    call find_two_grid_files()  ! calculates nomfipl, asalog1, asalog2


    !=====
    ! Calculation
    !=====

    write(lll,*) 'Creating ASCII file ', join_with_wdir(config_fn_moddat)
    call log_info(lll)

    open(unit=UNIT_DAT,file=join_with_wdir(config_fn_moddat),status='unknown')

    call log_info('Opening binary file '//join_with_wdir(config_fn_modeles)//&
     ' in status='//trim(config_open_status))

    open(unit=UNIT_MOD,access='direct',status=config_open_status, &
     file=join_with_wdir(config_fn_modeles), recl=1200)

    ! **********************Boucle sur l'abondance*******************
    do iabon = 1,2 ! on interpole dans 2 grilles d'abondance
      nomfiple = nomfipl(iabon)
      path = join_with_wdir(nomfiple)

      !-------------------------------------------------------------
      ! On cherche ou se trouvent (teff, glog)  par rapport a la table
      call locatab(path) ! calculates id11, id12, id21, id22

      call log_info('On va interpoler dans la table de Plez '//nomfiple//&
       '; indice d''abondance: '//int2str(iabon))

      !-------------------------------------------------------------
      ! Lecture des modeles
      !
      ! Note: last two logical values control whether to open and close the file
      !       File is opened and kept open; then closed at the last call to readerbn()
      call open_mod_file(path)

      call readerbn(id11, aa)
      rteff(1,1) = aa%teff
      rglog(1,1) = aa%glog

      call readerbn(id12, bb)
      rteff(1,2) = bb%teff
      rglog(1,2) = bb%glog

      call readerbn(id21, cc)
      rteff(2,1) = cc%teff
      rglog(2,1) = cc%glog

      call readerbn(id22, dd)
      rteff(2,2) = dd%teff
      rglog(2,2) = dd%glog

      call close_mod_file()

      ! sets the alpha abundance of the models for each iabon
      !> @todo issue assumption: asalalf is the same for aa, bb, cc, dd????
      ralfa(iabon)=dd%asalalf  ! takes asalalf from record d

      ! write(6,*) ' '
      ! write(6,*) ' Valeur des rglog'
      ! write(6,*) ' rglog(1,i)', (rglog(1,i),i=1,2)
      ! write(6,*) ' rglog(2,i)', (rglog(2,i),i=1,2)

      to0 = amax1(aa%t5l(1), bb%t5l(1), cc%t5l(1), dd%t5l(1))
      nna = int((to0-aa%t5l(1))*10+0.1)
      nnb = int((to0-bb%t5l(1))*10+0.1)
      nnc = int((to0-cc%t5l(1))*10+0.1)
      nnd = int((to0-dd%t5l(1))*10+0.1)

      ! write(lll,*) ' to0max=', to0
      ! call log_debug(lll)
      ! write(lll,*) aa%t5l(1),bb%t5l(1),cc%t5l(1),dd%t5l(1)
      ! call log_debug(lll)
      ! write(lll,*) ' couches a oter'
      ! call log_debug(lll)
      ! write(lll,*)  nna, nnb, nnc, nnd
      ! call log_debug(lll)

      ! les 4 modeles doivent commencer au meme niveau en log toR
      if(nna .gt. 0) call rangmod(aa, aa%ntot, nna)
      if(nnb .gt. 0) call rangmod(bb, bb%ntot, nnb)
      if(nnc .gt. 0) call rangmod(cc, cc%ntot, nnc)
      if(nnd .gt. 0) call rangmod(dd, dd%ntot, nnd)

      ntot(iabon) = min0(aa%ntot, bb%ntot, cc%ntot, dd%ntot)

      !!!! write(lll,*) '   ntot(',iabon,')=', ntot(iabon)
      !!!! call log_debug(lll)

      ! interpolation sur log g   pour les 2 valeurs de teta
      t0 = rglog(1,2)-rglog(1,1)
      t1 = x_glog-rglog(1,1)

      ! write(6,*) '  t0,t1 ',t0,t1
      ! write(6,102)  rteff(1,1), x_glog
      ! 102 format('  modele interpole avec   Teff=',f9.3,' log g=', f9.3)

      call interpol(t0, t1, aa, bb, ee, ntot(iabon))
      call interpol(t0, t1, cc, dd, ff, ntot(iabon))

      ! interpolation sur t
      t0 = rteff(2,1)-rteff(1,1)
      t1 = x_teff-rteff(1,1)

      ! write(6,*) '  t0,t1 ',t0,t1
      ! write(6,102) x_teff, x_glog

      if (iabon .eq. 1) then
        call interpol(t0, t1, ee, ff, z1, ntot(iabon))
      else
        call interpol(t0, t1, ee, ff, z2, ntot(iabon))
      end if

      if (asalog1 .eq. asalog2) then
        exit
      end if
    end do

    if (asalog1 .eq. asalog2) then
      ! New feature(2015):
      ! If the two metallicity files are the same, then does not need extra interpolation
      zz = z1
      asalalf = ralfa(1)
      nntot = ntot(1)
      call log_info("Using only one metallicity file")
    else
      ! On a 2 modeles l'un interpole ds grille a asalog1 et asalog2
      !
      !     interpolation sur l'abondance
      !     les 2 modeles doivent commencer au meme niveau en log to
      !     et avoir la meme longueur
      to0 = amax1(z1%t5l(1), z2%t5l(1))
      nz1 = int((to0-z1%t5l(1))*10+0.1)
      nz2 = int((to0-z2%t5l(1))*10+0.1)

      ! write(lll,*)' to0max=', to0
      ! call log_debug(lll)
      ! write(lll,*) z1%t5l(1),z2%t5l(1)
      ! call log_debug(lll)
      ! call log_debug(' couches a oter:')
      ! write(lll,*) nz1, nz2
      ! call log_debug(lll)

      if(ntot(1) .gt. 0) call rangmod(z1, ntot(1), nz1)
      if(ntot(2) .gt. 0) call rangmod(z2, ntot(2), nz2)

      !> @todo issue look at original lines: ZP1 repeated, doesn't look right; ntot(2) repeated, doesn't look right
      !> I changed this
      !>       if(ntot(2).gt.0)call rangmod(ZH1,ZT1,ZE1,ZP1,ZR1,NTOT(1),nz1)
      !>       if(ntot(2).gt.0)call rangmod(ZH2,ZT2,ZE2,ZP1,ZR2,NTOT(2),nz2)

      nntot = min0(ntot(1), ntot(2))

      ! write(lll,*) '   nntot=',nntot
      ! call log_debug(lll)

      t0 = asalog2-asalog1
      t1 = x_asalog-asalog1

      !!!! call log_debug(' interpolation sur l''abondance avec')
      !!!! write(lll,*) ' asalog2=',asalog2,'       asalog1=',asalog1
      !!!! call log_debug(lll)
      !!!! write(lll,*) ' t0=', t0, '       t1=',t1
      !!!! call log_debug(lll)

      call interpol(t0, t1, z1, z2, zz, nntot)

      ! calcule les elements alpha resultants
      asalalf = ralfa(1) + t1/t0*(ralfa(2)-ralfa(1))
    end if

    ! write(lll,*) 'model 1 asalog, alpha=',asalog1,ralfa(1)
    ! call log_debug(lll)
    ! write(lll,*) 'model 2 asalog, alpha=',asalog2,ralfa(2)
    ! call log_debug(lll)
    ! write(lll,*) 'result: asalog, alpha=',x_asalog,asalalf
    ! call log_debug(lll)


    ! ***********************Ecriture du résultat*********************
    do n = 1,nntot
      zle(n) = zz%pe(n)
      zlp(n) = zz%pg(n)
      zz%nh(n) = 10**zz%nh(n)
      zz%pe(n) = 10**zz%pe(n)
      zz%pg(n) = 10**zz%pg(n)
    end do

    tir=tira//x_tirb
    in = 0

    do n = 1,nntot
      in = in+1
      k = (in-1)*5+1
      a(k)  =zz%nh(n)   ! NH
      a(k+1)=zz%teta(n) ! T
      a(k+2)=zz%pe(n)   ! Pe
      a(k+3)=zz%pg(n)   ! Pg
      a(k+4)=zz%t5l(n)  ! log to
    end do

    ! Writes binary file
    write(UNIT_MOD, rec=x_inum) &
     nntot,          &
     x_teff, &
     x_glog, &
     x_asalog, &
     asalalf,        &
     dd%nhe,         &  !> @todo issue  note: takes nhe from last record. Correct?
     tir,            &
     dd%tiabs,       &  !> @todo issue  note: takes tiabs from last record. Correct?
     (a(k),k=1,nntot*5)
    write(UNIT_MOD,rec=x_inum+1) 9999  ! 4 bytes, i guess

    ! Writes ASCII file
    bid0 = 0.0
    vvt = 2.0E+5 ! on prend vt constant
    tostand = 5000 ! ?
    write(UNIT_DAT,'(a30,i8,f10.0,f8.2,2f5.0)') &
     config_modcode, &
     nntot,             &
     tostand,           &
     x_glog,    &
     bid0,              &
     bid0
    do n = 1,nntot
      tau=10**zz%t5l(n)
      tttt=5040/zz%teta(n)
      write(UNIT_DAT,'(e15.5,f10.0,3e15.6)') zz%t5l(n), tttt, zle(n), zlp(n), vvt
    end do

    close(unit=UNIT_MOD)
    close(unit=UNIT_DAT)

    call log_info('File '''//trim(join_with_wdir(config_fn_modeles))//''' was successfully created.')
  end


  !> Fill variables nomfipl, asalog1, asalog2 based on x_asalog
  !>
  !> Note that intervals are open on upper boundary, i.e.,
  !> @verbatim
  !> [ gridsmap_asalog(i), gridsmap_asalog(i+1) [
  !>
  !> (1 <= i < gridsmap_num_files)
  !> @endverbatim

  subroutine find_two_grid_files()
    integer i, n
    logical :: flag_found = .false.

    n = gridsmap_num_files

    do i = 1, n-1
      if (x_asalog .ge. gridsmap_asalog(i) .and. &
          x_asalog .lt. gridsmap_asalog(i+1)) then
        nomfipl(1) = gridsmap_fn(i)
        nomfipl(2) = gridsmap_fn(i+1)
        asalog1 = gridsmap_asalog(i) !sert a l'interpolation sur la metallicite
        asalog2 = gridsmap_asalog(i+1)
        flag_found = .true.
        exit
      end if
    end do

    if (.not. flag_found) then
      if (x_asalog .eq. gridsmap_asalog(n)) then
        ! This is the case where metallicity matches that of last file.
        ! Will use same file twice
        nomfipl(1) = gridsmap_fn(n)
        nomfipl(2) = gridsmap_fn(n)
        asalog1 = gridsmap_asalog(n)
        asalog2 = gridsmap_asalog(n)
      else
        ! Will not longer give error if metallicity falls outsize the range. Instead, will do the best it can
        nomfipl(1) = gridsmap_fn(n)
        nomfipl(2) = gridsmap_fn(n)
        asalog1 = gridsmap_asalog(n)
        asalog2 = gridsmap_asalog(n)
        call log_warning('Metallicity '//real42str(x_asalog, 3)//' is out of interval ['//&
         real42str(gridsmap_asalog(1), 3)//', '//real42str(gridsmap_asalog(gridsmap_num_files), 3)//']')
      end if
    end if

    call log_info('Chosen file 1: '''//trim(nomfipl(1))//'''')
    call log_info('Chosen file 2: '''//trim(nomfipl(2))//'''')

  end



  !=======================================================================================
  !> Interpolation ?doc?

  subroutine interpol(t0, t1, r1, r2, rr, ntot)
    real*4, intent(in) :: t0, t1
    type(modele_record), intent(in) :: r1, r2
    type(modele_record), intent(out) :: rr
    real*4 u0, t2
    integer ntot, n


    t2=t1/t0
    do n=1,ntot
      u0 = r2%nh(n)-r1%nh(n)
      rr%nh(n) = r1%nh(n) + (u0*t2)

      u0 = r2%teta(n)-r1%teta(n)
      rr%teta(n)= r1%teta(n) + (u0*t2)

      u0 = r2%pe(n)-r1%pe(n)
      rr%pe(n)= r1%pe(n) + (u0*t2)

      u0 = r2%pg(n)-r1%pg(n)
      rr%pg(n) = r1%pg(n) + (u0*t2)

      rr%t5l(n) = r1%t5l(n)

      ! if(n.le.5) then
      !   write(6,'(i4,5f14.4)')n,rr%nh(n),rr%teta(n),rr%pe(n),rr%pg(n),rr%t5l(n)
      ! end if
    end do
  end


  !=======================================================================================
  !> Enlever les n premieres couches a un modele
  !>
  !> Note that ntot may or may not be rr%ntot

  subroutine rangmod(rr, ntot, n)
    type(modele_record), intent(inout) :: rr
    integer, intent(inout) :: ntot
    integer, intent(in) :: n
    integer k

    ntot = ntot-n
    do k = 1, ntot
      rr%nh(k) = rr%nh(k+n)
      rr%teta(k) = rr%teta(k+n)
      rr%pe(k) = rr%pe(k+n)
      rr%pg(k) = rr%pg(k+n)
      rr%t5l(k) = rr%t5l(k+n)
    end do
  end



  !=======================================================================================
  !> On cherche les numeros des 4 modeles de la table entre lesquels
  !> le programme devra interpoler. On donne les limites en T et g
  !> des modeles.
  !> Les modeles doivent etre ranges en temperature croissante
  !> a l'interieur de chaque temp les gravites doivent croitre
  !>
  !> Outputs are in module variables id11, id12, id21, id22
  !>
  !> @note This routine has been re-designed to sweep the whole models file to mount the
  !> tables (there was a table hard-coded before)

  subroutine locatab(path)
    character(len=*), intent(in) :: path

    integer, parameter :: MAX_NT=20, &  ! Maximum number of different temperatures
                          MAX_NG=20     ! Maximum number of different glog or each temperature

    integer*4 :: idt(MAX_NT), & ! index of first record for each tempearture
                 ing(MAX_NG), & ! number of glog for each temperature
                 jg1(MAX_NG), &
                 jg2(MAX_NG)
    !!!!!!!             idta(MAX_NT), & ! index of first record for each tempearture
    !!!!!!!             inga(MAX_NT)    ! number of glog for each temperature

    real*8 :: rteff(MAX_NT), rglog(MAX_NG)
    !!!!!!! ,r1teff(MAX_NT)
    !!!!!!! real*8 :: agloga1(MAX_NG),agloga2(MAX_NG),agloga3(MAX_NG),agloga4(MAX_NG), &
    !!!!!!! agloga5(MAX_NG),agloga6(MAX_NT),agloga7(MAX_NT)

    real*8 :: aglog(MAX_NT,MAX_NG)

    integer i, j, jjt, jt1, jt2, n, ng, ngg, nt

    type(modele_record) :: r  ! filled by read_mod_record()
    integer inum, num_rec
    real*8 teff_last

    call log_debug(ENTERING//'Entree dans le SP locatab')


    !=====
    ! Mounts data variables from file
    !=====
    ! First task is to
    ! mount (nt, rteff, idt, ing, aglog) from file

    teff_last = -1
    nt = 0
    num_rec = get_num_records(path)
    call log_debug('Number of records: '//int2str(num_rec))
    call open_mod_file(path)

    do inum = 1, num_rec
      call read_mod_record(inum, r)

      if (inum .eq. 1 .or. abs(teff_last-r%teff) .gt. 0.001) then
        if (inum .gt. 1) then
          ing(nt) = ng
        end if

        teff_last = r%teff
        nt = nt+1

        call assert_le(nt, MAX_NT, 'locatab()', 'nt', 'MAX_NT')

        ng = 0
        rteff(nt) = r%teff
        idt(nt) = inum
      end if

      ng = ng+1
      aglog(nt,ng) = r%glog
    end do
    ing(inum) = ng

    call close_mod_file()

    call log_debug('teff='//real42str(x_teff, 1))
    call log_debug('glog='//real42str(x_glog, 3))
    call log_debug('nt='//int2str(nt))
    write(lll,*) 'idt=', (idt(i), i=1,nt)
    call log_debug(lll)
    write(lll,*) 'ing=', (ing(i), i=1,nt)
    call log_debug(lll)

    !do i = 1, nt
    !  !write(lll,*) 'rteff(', i, ')=', rteff(i)
    !  !call log_debug(lll)
    !  !write(lll,*) 'aglog(i,:)=', (aglog(i,j), j=1,ing(i))
    !  !call log_debug(lll)
    !end do


    call log_debug('Liste du nbre de g en fonction de t')
    write(lll,*) (ing(n),n=1,nt)
    call log_debug(lll)

    ! sera suivi d'ordres equivalent si plusieurs abond donc autres IABON
    ! etc...

    do i=1,nt
      jt2 = i
      if(x_teff .lt. rteff(i)) go to 11
    end do

    11 continue
    if (jt2 .eq. 1) jt2 = 2

    jt1 = jt2-1
    write(lll,*) 'Indices de temperatures', jt1,jt2
    call log_debug(lll)

    do jjt=jt1,jt2
      ngg=ing(jjt)

      write(lll,*)'jtt=',jjt,'   ngg=',ngg
      call log_debug(lll)
      write(lll,*) 'Nbre de gravite pour la temp ngg=',ngg
      call log_debug(lll)

      do i=1,ngg
        rglog(i)=aglog(jjt,i)
      end do

      write(lll,*) ' rglog(i) pour jjt=', jjt
      call log_debug(lll)
      write(lll,'(10f5.1)') (rglog(i),i=1,ngg)
      call log_debug(lll)

      do i = 1,ngg
        jg2(jjt)=i
        if(x_glog .lt. rglog(i)) go to 12
      end do

      12 continue
      if (jg2(jjt) .eq. 1) jg2(jjt) = 2

      jg1(jjt)=jg2(jjt)-1

      write(lll,*) ' jjt jg1 jg2', jjt, jg1(jjt), jg2(jjt)
      call log_debug(lll)
    end do


    write(lll,*) ' jjt, idt jg1(jjt), jg2(jjt) ',jt1,idt(jt1), jg1(jt1), jg2(jt1)
    call log_debug(lll)
    write(lll,*) ' jjt, idt jg1(jjt), jg2(jjt) ',jt2,idt(jt2), jg1(jt2) , jg2(jt2)
    call log_debug(lll)


    ! ID des modeles dans la table
    id11 = idt(jt1)+jg1(jt1)-1
    id12 = idt(jt1)+jg2(jt1)-1
    id21 = idt(jt2)+jg1(jt2)-1
    id22 = idt(jt2)+jg2(jt2)-1
    call log_info('Dans cette table les modeles entre lesquels on va interpoler ont les numeros:')
    write(lll,*)  id11,id12,id21,id22
    call log_info(lll)

    if (config_explain) then
      write(UNIT_EXPLAIN, *) 'locatab: ''', trim(path), ''', ', int2str(id11)
      write(UNIT_EXPLAIN, *) 'locatab: ''', trim(path), ''', ', int2str(id12)
      write(UNIT_EXPLAIN, *) 'locatab: ''', trim(path), ''', ', int2str(id21)
      write(UNIT_EXPLAIN, *) 'locatab: ''', trim(path), ''', ', int2str(id22)
    end if

    call log_debug(LEAVING//'Sortie de locatab')
  end


  !=======================================================================================
  !> Lit sur disque acces direct nh,teta,pe,pg,t5l,ntot
  !>
  !> Lit sur le fichier de type .mod nh,teta,pe,pg,t5l,ntot

  subroutine readerbn(rec_id, r)
    integer, intent(in) :: rec_id
    type(modele_record), intent(out) :: r
    integer i

    call read_mod_record(rec_id, r)

    write(lll,'(f10.0,4f10.2,5a4)') r%teff, r%glog, r%asalog, r%asalalf, r%nhe, r%tit

    do i=1,r%ntot
      r%nh(i) = alog10(r%nh(i))
      r%pe(i) = alog10(r%pe(i))
      r%pg(i) = alog10(r%pg(i))
    end do

    !!!! call log_debug('        log NH           TETA          log PE         log PG     To(5000)')
    !!!! do i = 1, 3
    !!!!   write(lll,'(e16.4,f15.4,2e16.4,f12.4)') r%nh(i), r%teta(i), r%pe(i), r%pg(i), r%t5l(i)
    !!!!   call log_debug(lll)
    !!!! end do
    !!!! call log_debug('     ETC.....')
  end
end

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> INNEWMARCS
!>
!> Interpolation d'un modele dans les grilles de modeles de
!> NEWMARCS (2005) en fonction de Teff, log g et [Fe/H].
!>

program innewmarcs
  use config
  use logging
  use welcome
  use innewmarcs_calc
  use innewmarcs_x
  use reader_gridsmap
  implicit none

  execonf_name = 'inneWmarcs'
  call config_init()
  call read_gridsmap()
  call innewmarcs_init_x()
  call innewmarcs_calc_()
end

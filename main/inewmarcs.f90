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

module innewmarcs
  use read_most_files
  implicit none
  private

  ! .d88b .d88b. 8b  8 8888 888 .d88b
  ! 8P    8P  Y8 8Ybm8 8www  8  8P www
  ! 8b    8b  d8 8  "8 8     8  8b  d8
  ! `Y88P `Y88P' 8   8 8    888 `Y88P'  module configuration
  !
  ! Note: This module has kept the independence of the original INWEMARCS executable.
  !       The main difference is that it does not ask for user input

  !> Value Fortran OPEN(..., status=)
  !> E.g.: 'unknown', 'old', 'new', 'replace'
  character*20, public :: inewmarcs_open_status

  character*64, public :: &
   inewmarcs_nomfimod, & !< Full path to output binary file, e.g. "./modeles.mod"
   inewmarcs_nomfidat   !< Full path to output ascii file, e.g., "./modeles.da%teta"
  !> Name of directory containing file modelmap.dat and ".mod" files
  !> referenced within modelmap.dat.
  character*192, public :: inewmarcs_refdir = "."
  !> Name of model to be recorded inside output files, e.g., "BWF1 star"
  character*25, public :: inewmarcs_modcode !< nom du modele
  ! These variables must be real*4, as they are going to be written to .mod file
  real*4 :: &
   inewmarcs_teff, & !< Teff
   inewmarcs_glog, & !< log g
   inewmarcs_amet    !< [M/H]
  integer, public :: inewmarcs_jd !< ligne du modele en sortie
  character*15, public :: inewmarcs_tirb !< Titre


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  ! Variables related to the reference models
  integer, parameter:: MAX_NUM_REFMODELS = 10
  integer num_refmodels
  real*4, dimension(MAX_NUM_REFMODELS) :: modelmap_met
  character*64, dimension(MAX_NUM_REFMODELS) :: modelmap_fn

  ! Calculated by locatab()
  ! "Dans cette table les modeles entre lesquels on va interpoler ont les numeros:"
  integer id11, id12, id21, id22

  ! Variables calculated by find_refmodels()
  character*64 :: nomfipl(2)
  real*4 amet1, amet2

  private model_path, read_modelmap, find_refmodels, rangmod, interpol, locatab, readerbn
  character*192 :: lll  ! __logging__
contains

  !=======================================================================================
  !> Main routine of this module
  !>
  !> @note ASCII file is always opened in status "unknown"

  subroutine inewmarcs_calc()
    integer, parameter :: UNIT_MOD = 20, UNIT_DAT = 21  ! file units
    type(modele_record) :: &
     aa, bb, cc, dd, &   ! input records
     ee, ff, z1, z2, zz  ! records that are calculated by interpol()

    character*5 :: tira = ' Ple '
    character*20 tir    ! titre du modele interpole
    character*65 nomfiple
    real*4 bid0, vvt, tostand, t0, t1, tau, to0, tttt
    integer*4 iabon, in, k, n, nna, nnb, nnc, nnd, nntot, nz1, nz2, ntot(2)
    ! Variables that must be real*4, as they are going to be written to .mod file
    real*4 asalalf
    real*4, dimension(MAX_MODELES_NTOT) :: zle, zlp
    real*4 :: a(MAX_MODELES_NTOT*5)  ! modele resultant
    real*4 :: rteff(2,2), rglog(2,2), ralfa(2)


    !> @todo implement flag_initialized to call this only once if I call inewmarcs_calc() more than once
    call read_modelmap()


    write(lll,*) 'Creating ASCII file ', inewmarcs_nomfidat
    call log_debug(lll)

    open(unit=UNIT_DAT,file=inewmarcs_nomfidat,status='unknown')

    call log_debug('Opening binary file '//trim(inewmarcs_nomfimod)//' in status='//&
     trim(inewmarcs_open_status))

    open(unit=UNIT_MOD,access='direct',status=inewmarcs_open_status, &
     file=inewmarcs_nomfimod, recl=1200)

    call find_refmodels()  ! calculates nomfipl, amet1, amet2

    !-------------------------------------------------------------
    ! On cherche ou se trouvent (teff, glog)  par rapport a la table
    call locatab() ! calculates id11, id12, id21, id22


    ! **********************Boucle sur l'abondance*******************
    do iabon = 1,2 ! on interpole dans 2 grilles d'abondance
      nomfiple = nomfipl(iabon)

      call log_debug('On va interpoler dans la table de Plez '//nomfiple)
      call log_debug('Indice d''abondance: '//int2str(iabon))

      ! all values are wasted, no point in the following call:
      ! call readerbn(id,aa%nh,aa%teta,aa%pe,aa%pg,aa%t5l,ntot(iabon),.true.,.false.) ! Open du fichier

      !-------------------------------------------------------------
      ! Lecture des modeles
      !
      ! Note: last two logical values control whether to open and close the file
      !       File is opened and kept open; then closed at the last call to readerbn()

      call readerbn(nomfiple, id11, .true., .false., aa)
      rteff(1,1) = aa%teff
      rglog(1,1) = aa%glog

      call readerbn(nomfiple, id12, .false., .false., bb)
      rteff(1,2) = bb%teff
      rglog(1,2) = bb%glog

      call readerbn(nomfiple, id21, .false., .false., cc)
      rteff(2,1) = cc%teff
      rglog(2,1) = cc%glog

      call readerbn(nomfiple, id22, .false., .true., dd)
      rteff(2,2) = dd%teff
      rglog(2,2) = dd%glog

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

      write(lll,*) ' to0max=', to0
      call log_debug(lll)
      write(lll,*) aa%t5l(1),bb%t5l(1),cc%t5l(1),dd%t5l(1)
      call log_debug(lll)
      write(lll,*) ' couches a oter'
      call log_debug(lll)
      write(lll,*)  nna, nnb, nnc, nnd
      call log_debug(lll)

      ! les 4 modeles doivent commencer au meme niveau en log toR
      if(nna .gt. 0) call rangmod(aa, aa%ntot, nna)
      if(nnb .gt. 0) call rangmod(bb, bb%ntot, nnb)
      if(nnc .gt. 0) call rangmod(cc, cc%ntot, nnc)
      if(nnd .gt. 0) call rangmod(dd, dd%ntot, nnd)

      ntot(iabon) = min0(aa%ntot, bb%ntot, cc%ntot, dd%ntot)

      write(lll,*) '   ntot(',iabon,')=', ntot(iabon)
      call log_debug(lll)

      ! interpolation sur log g   pour les 2 valeurs de teta
      t0 = rglog(1,2)-rglog(1,1)
      t1 = inewmarcs_glog-rglog(1,1)

      ! write(6,*) '  t0,t1 ',t0,t1
      ! write(6,102)  rteff(1,1), inewmarcs_glog
      ! 102 format('  modele interpole avec   Teff=',f9.3,' log g=', f9.3)

      call interpol(t0, t1, aa, bb, ee, ntot(iabon))
      call interpol(t0, t1, cc, dd, ff, ntot(iabon))

      ! interpolation sur t
      t0 = rteff(2,1)-rteff(1,1)
      t1 = inewmarcs_teff-rteff(1,1)

      ! write(6,*) '  t0,t1 ',t0,t1
      ! write(6,102) inewmarcs_teff, inewmarcs_glog

      if (iabon .eq. 1) then
        call interpol(t0, t1, ee, ff, z1, ntot(iabon))
      else
        call interpol(t0, t1, ee, ff, z2, ntot(iabon))
      end if
    end do

    ! On a 2 modeles l'un interpole ds grille a amet1 et amet2
    !
    !     interpolation sur l'abondance
    !     les 2 modeles doivent commencer au meme niveau en log to
    !     et avoir la meme longueur
    to0 = amax1(z1%t5l(1), z2%t5l(1))
    nz1 = int((to0-z1%t5l(1))*10+0.1)
    nz2 = int((to0-z2%t5l(1))*10+0.1)

    write(lll,*)' to0max=', to0
    call log_debug(lll)
    write(lll,*) z1%t5l(1),z2%t5l(1)
    call log_debug(lll)
    call log_debug(' couches a oter:')
    write(lll,*) nz1, nz2
    call log_debug(lll)

    if(ntot(1) .gt. 0) call rangmod(z1, ntot(1), nz1)
    if(ntot(2) .gt. 0) call rangmod(z2, ntot(2), nz2)

    !> @todo issue look at original lines: ZP1 repeated, doesn't look right; ntot(2) repeated, doesn't look right
    !> I changed this
    !>       if(ntot(2).gt.0)call rangmod(ZH1,ZT1,ZE1,ZP1,ZR1,NTOT(1),nz1)
    !>       if(ntot(2).gt.0)call rangmod(ZH2,ZT2,ZE2,ZP1,ZR2,NTOT(2),nz2)

    nntot = min0(ntot(1), ntot(2))

    write(lll,*) '   nntot=',nntot
    call log_debug(lll)

    t0 = amet2-amet1
    t1 = inewmarcs_amet-amet1

    call log_debug(' interpolation sur l''abondance avec')
    write(lll,*) ' amet2=',amet2,'       amet1=',amet1
    call log_debug(lll)
    write(lll,*) ' t0=', t0, '       t1=',t1
    call log_debug(lll)

    call interpol(t0, t1, z1, z2, zz, nntot)

    ! calcule les elements alpha resultants
    asalalf = ralfa(1) + t1/t0*(ralfa(2)-ralfa(1))

    !__logging__
    write(lll,*) 'model 1 inewmarcs_amet, alpha=',amet1,ralfa(1)
    call log_debug(lll)
    write(lll,*) 'model 2 inewmarcs_amet, alpha=',amet2,ralfa(2)
    call log_debug(lll)
    write(lll,*) 'result: inewmarcs_amet, alpha=',inewmarcs_amet,asalalf
    call log_debug(lll)


    ! ***********************Ecriture du résultat*********************
    do n = 1,nntot
      zle(n) = zz%pe(n)
      zlp(n) = zz%pg(n)
      zz%nh(n) = 10**zz%nh(n)
      zz%pe(n) = 10**zz%pe(n)
      zz%pg(n) = 10**zz%pg(n)
    end do

    tir=tira//inewmarcs_tirb
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
    write(UNIT_MOD, rec=inewmarcs_jd) &
     nntot,          &
     inewmarcs_teff, &
     inewmarcs_glog, &
     inewmarcs_amet, &
     asalalf,        &
     dd%nhe,         &  !> @todo issue  note: takes nhe from last record. Correct?
     tir,            &
     dd%tiabs,       &  !> @todo issue  note: takes tiabs from last record. Correct?
     (a(k),k=1,nntot*5)
    write(UNIT_MOD,rec=inewmarcs_jd+1) 9999  ! 4 bytes, i guess

    ! Writes ASCII file
    bid0 = 0.0
    vvt = 2.0E+5 ! on prend vt constant
    tostand = 5000 ! ?
    write(UNIT_DAT,'(a30,i8,f10.0,f8.2,2f5.0)') &
     inewmarcs_modcode, &
     nntot,             &
     tostand,           &
     inewmarcs_glog,    &
     bid0,              &
     bid0
    do n = 1,nntot
      tau=10**zz%t5l(n)
      tttt=5040/zz%teta(n)
      write(UNIT_DAT,'(e15.5,f10.0,3e15.6)') zz%t5l(n), tttt, zle(n), zlp(n), vvt
    end do

    close(unit=UNIT_MOD)
    close(unit=UNIT_DAT)
  end


  !=======================================================================================
  !> Returns full path to file within models directory

  function model_path(filename) result(res)
    character(len=*), intent(in) :: filename  !< File name
    character(len=:), allocatable :: res

    res = trim_and_add_slash(inewmarcs_refdir) // trim(filename)
  end


  !=======================================================================================
  !> Reads map of models: file "modelmap.da%teta"
  !>
  !> By convention, this file is called "modelmap.da%teta". The information it contains is
  !> a list of metalicity ranges and corresponding reference models to be used.
  !>

  subroutine read_modelmap()
    character*64 :: t_fn
    real*4 t_met
    character(len=:), allocatable :: path_to_file
    integer, parameter :: UNIT_ = 199

    path_to_file = model_path('modelmap.dat')

    open(unit=UNIT_,file=path_to_file, status='old')

    num_refmodels = 0
    do while (.true.)
      read(UNIT_, *, end=10) t_met, t_fn

      num_refmodels = num_refmodels+1
      modelmap_met(num_refmodels) = t_met
      modelmap_fn(num_refmodels) = t_fn
    end do

    10 continue  ! reached EOF

    close(unit=UNIT_)
  end


  !> Fill variables nomfipl, amet1, amet2 based on inewmarcs_amet
  !>
  !> Note that intervals are open on upper boundary, i.e.,
  !> @verbatim
  !> [ modelmap_met(i), modelmap_met(i+1) [
  !>
  !> (1 <= i < num_refmodels)
  !> @endverbatim

  subroutine find_refmodels()
    integer i
    logical :: flag_found = .false.

    do i = 1, num_refmodels-1
      if (inewmarcs_amet .ge. modelmap_met(i) .and. &
          inewmarcs_amet .lt. modelmap_met(i+1)) then
        nomfipl(1) = modelmap_fn(i)
        nomfipl(2) = modelmap_fn(i+1)
        amet1 = modelmap_met(i) !sert a l'interpolation sur la metallicite
        amet2 = modelmap_met(i+1)
        flag_found = .true.
        exit
      end if
    end do

    if (.not. flag_found) then
      call pfant_halt('Metallicity '//real42str(inewmarcs_amet)//' is out of interval ['//&
       real42str(modelmap_met(1))//', '//real42str(modelmap_met(num_refmodels))//'[')
    end if
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
  !> NEWMARCS 2005 metallicities -1.5 to +1.00 included
  !>
  !> Outputs are in module variables id11, id12, id21, id22
  !>

  subroutine locatab()
    integer*4 :: idt(7),ing(8),jg1(8),jg2(8), &
                 idta(7), & ! indice des 1ers mod de chque temp
                 inga(7)    ! nbre de logg pour chaq temp
    real*8 :: rteff(7),rglog(8),r1teff(7)
    real*8 :: agloga1(8),agloga2(8),agloga3(8),agloga4(8), &
     agloga5(8),agloga6(7),agloga7(7),aglog(7,8)
    character*128 lll
    integer i, jjt, jt1, jt2, n, ng, ngg, nt

    data r1teff /4000.,4250.,4500.,4750.,5000.,5250.,5500./
    data agloga1 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
    data agloga2 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
    data agloga3 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
    data agloga4 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
    data agloga5 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
    data agloga6 /0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
    data agloga7 /0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
    ! data agloga8 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/

    data idta /1, 9, 17, 25, 33, 41, 48/
    data inga /8, 8, 8, 8, 8, 7, 7/

    call log_debug(ENTERING//'Entree dans le SP locatab')

    nt=7    ! nbre total de temperatures
    ng=7    ! nbre maximum de log g
    do n=1,nt
    rteff(n)=r1teff(n)
    ing(n)=inga(n)
    idt(n)=idta(n)
    end do
    do n=1,ng
    aglog(1,n)=agloga1(n)
    aglog(2,n)=agloga2(n)
    aglog(3,n)=agloga3(n)
    aglog(4,n)=agloga4(n)
    aglog(5,n)=agloga5(n)
    aglog(6,n)=agloga6(n)
    aglog(7,n)=agloga7(n)
    ! aglog(8,n)=agloga8(n)
    ! aglog(9,n)=agloga9(n)
    ! aglog(10,n)=agloga10(n)
    ! aglog(11,n)=agloga11(n)
    end do

    call log_debug('Liste du nbre de g en fonction de t')
    write(lll,*) (ing(n),n=1,nt)
    call log_debug(lll)

    ! sera suivi d'ordres equivalent si plusieurs abond donc autres IABON
    ! etc...

    do i=1,nt
      jt2 = i
      if(inewmarcs_teff .lt. rteff(i)) go to 11
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
        if(inewmarcs_glog.lt.rglog(i)) go to 12
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
    call log_debug('Dans cette table les modeles entre lesquels on va interpoler ont les numeros:')
    write(lll,*)  id11,id12,id21,id22
    call log_debug(lll)
    call log_debug(LEAVING//'Sortie de locatab')
  end

  !=======================================================================================
  !> Lit sur disque acces direct nh,teta,pe,pg,t5l,ntot
  !>
  !> Lit sur le fichier de type .mod nh,teta,pe,pg,t5l,ntot

  subroutine readerbn(path_to_file, rec_id, flag_open, flag_close, r)
    character(len=*), intent(in) :: path_to_file
    integer, intent(in) :: rec_id
    logical, intent(in) :: flag_open, flag_close
    type(modele_record), intent(out) :: r
    integer i


    call read_mod_record(path_to_file, rec_id, flag_open, flag_close, r)


    write(lll,'(f10.0,4f10.2,5a4)') r%teff, r%glog, r%asalog, r%asalalf, r%nhe, r%tit

    do i=1,r%ntot
      r%nh(i) = alog10(r%nh(i))
      r%pe(i) = alog10(r%pe(i))
      r%pg(i) = alog10(r%pg(i))
    end do

    call log_debug('        log NH           TETA          log PE         log PG     To(5000)')
    do i = 1, 3
      write(lll,'(e16.4,f15.4,2e16.4,f12.4)') r%nh(i), r%teta(i), r%pe(i), r%pg(i), r%t5l(i)
      call log_debug(lll)
    end do
    call log_debug('     ETC.....')
  end
end


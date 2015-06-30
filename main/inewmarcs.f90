!> INNEWMARCS
!>
!> Interpolation d'un modele dans les grilles de modeles de
!> NEWMARCS (2005) en fonction de Teff  log g et Fe/H
!> (lit newmarcsm200.mod  newmarcsm150.mod newmarcsm100.mod
!> newmarcsm075.mod newmarcsm050.mod  newmarcsm025.mod
!> newmarcsp000.mod newmarcsp025.mod  newmarcsp050.mod
!> newmarcsp075.mod newmarcsp100.mod
!>
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

  private


  ! .d88b .d88b. 8b  8 8888 888 .d88b 
  ! 8P    8P  Y8 8Ybm8 8www  8  8P www
  ! 8b    8b  d8 8  "8 8     8  8b  d8
  ! `Y88P `Y88P' 8   8 8    888 `Y88P'  module configuration
  !
  ! Note: This module has kept the independence of the original INWEMARCS executable.
  !       The main difference is that it does not ask for user input


  character*64, public :: &
   inewmarcs_nomfimod, & !< Full path to output binary file, e.g. "./modeles.mod"
   inewmarcs_nomfidat   !< Full path to output ascii file, e.g., "./modeles.dat"
  !> Name of directory containing file modelmap.dat and ".mod" files references within modelmap.dat.
  character*192, public :: inewmarcs_model_dirname = "."
  !> Name of model to be recorded inside output files, e.g., "BWF1 star" 
  character*25, public :: inewmarcs_modcode !< nom du modele
  real*8 :: &
   inewmarcs_teff, & !< Teff
   inewmarcs_glog, & !< log g
   inewmarcs_amet  & !< [M/H]
  integer, public :: inewmarcs_jd !< ligne du modele en sortie
  character*15, public :: inewmarcs_tirb !< Titre


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols


  real*8, dimension(MAX_MODELES_NTOT) :: &
   aah,aat,aae,aap,aar, &
   bbh,bbt,bbe,bbp,bbr, &
   cch,cct,cce,ccp,ccr, &
   ddh,ddt,dde,ddp,ddr, &
   eeh,eet,eee,eep,eer, &
   ffh,fft,ffe,ffp,ffr, &
   zh1,zt1,ze1,zp1,zr1, &
   zh2,zt2,ze2,zp2,zr2, &
   zzh,zzt,zze,zzp,zzr, &
   zle,zlp

  real*8 :: A(MAX_MODELES_NTOT*5)  ! modele resultant
  real*8 :: RTeff(2,2),RGlog(2,2),Ralfa(2),ntot(2)
  character*4 chteff
  character*3 chlogg
  character*14 chmodz
  Character*5 :: tira = ' Ple '
  Character*20 tir    ! titre du modele interpole

  common/COM1/Nomfiple
  COMMON/COM6/TEFK,GLOGK,ASAK,ASALK,BHEK,TIABS(5),TITK(5)


      tostand=5000


  integer, parameter:: MAX_NUM_REFMODELS = 10
  integer num_refmodels
  real*8, dimension(MAX_NUM_REFMODELS) :: modelmap_met
  character*64, dimension(MAX_NUM_REFMODELS) :: modelmap_fn

  character*64 :: nomfipl(2)


  ! Calculated by locatab()
  ! "Dans cette table les modeles entre lesquels on va interpoler ont les numeros:"
  integer id11, id12, id21, id22

  private input_path, read_modelmap
contains

  !=======================================================================================
  !> Main routine of this module

  subroutine inewmarcs_calc()
    character*192 :: lll
    integer, parameter :: UNIT_MOD = 20, UNIT_DAT = 21

    ! WRITE(6,*) 'Nom des fich ou mettre les modeles interpoles'
    ! WRITE(6,*) ' .mod     et     ascii'
    ! read(5,*) inewmarcs_nomfimod, config_inewmarcs_nomfidat

    write(lll,*) 'Creating ASCII file ', inewmarcs_nomfidat
    call log_debug(lll)

    open(unit=UNIT_DAT,file=inewmarcs_nomfidat,status='unknown')

    call log_debug('Opening binary file '//trim(inewmarcs_nomfimod)//' in status='//&
     trim(inewmarcs_model_status))

    open(unit=UNIT_MOD,access='direct',status=inewmarcs_model_status, &
     file=config_inewmarcs_nomfimod, recl=1200) 

    call find_refmodels()  ! fills nomfipl, amet1, amet2


    ! **********************Boucle sur l'abondance*******************

    do iabon = 1,2 ! on interpole dans 2 grilles d'abondance
      nomfiple = nomfipl(iabon)

      10 continue
      call pfant_debug('On va interpoler dans la table de Plez'//nomfiple)
      call pfant_debug('Indice d''abondance'//int2str(iabon))

      !-------------------------------------------------------------
      iop=0
      id=1

      ! Note: last two logical values control whether to open and close the file
      !       File is opened and kept open; then closed at the last call to readerbn()
      call readerbn(id,aah,aat,aae,aap,aar,ntot(iabon),.true.,.false.) ! Open du fichier

      iop=1

      !-------------------------------------------------------------
      ! On cherche ou se trouvent inewmarcs_teff inewmarcs_glog  par rapport a la table

      call locatab(iabon,nomfiple)

      !-------------------------------------------------------------
      ! Lecture des modeles

      call readerbn(id11,aah,aat,aae,aap,aar,natot,.false.,.false.)
      ! write(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
      rteff(1,1) = tefk
      rglog(1,1) = glogk

      call readerbn(iop,id12,bbh,bbt,bbe,bbp,bbr,nbtot,.false.,.false.)
      ! write(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
      rteff(1,2)=tefk
      rglog(1,2)=glogk

      call readerbn(iop,id21,cch,cct,cce,ccp,ccr,nctot,.false.,.false.)
      ! write(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
      rteff(2,1)=tefk
      rglog(2,1)=glogk

      call readerbn(iop,id22,ddh,ddt,dde,ddp,ddr,ndtot,.false.,.true.)
      ! write(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
      rteff(2,2)=tefk
      rglog(2,2)=glogk

      ! sets the alpha abundance of the models for each iabon
      ralfa(iabon)=asalk

      ! write(6,*) ' '
      ! write(6,*) ' Valeur des rglog'
      ! write(6,*) ' rglog(1,i)', (rglog(1,i),i=1,2)
      ! write(6,*) ' rglog(2,i)', (rglog(2,i),i=1,2)

      to0 = amax1(aar(1),bbr(1),ccr(1),ddr(1))
      nna = (to0-aar(1))*10+0.1
      nnb = (to0-bbr(1))*10+0.1
      nnc = (to0-ccr(1))*10+0.1
      nnd = (to0-ddr(1))*10+0.1

      write(lll,*) ' to0max=', to0
      call log_debug(lll)
      write(lll,*) aar(1),bbr(1),ccr(1),ddr(1)
      call log_debug(lll)
      write(lll,*) ' couches a oter'
      call log_debug(lll)
      write(lll,*)  nna, nnb, nnc, nnd
      call log_debug(lll)

      ! les 4 modeles doivent commencer au meme niveau en log toR
      if(nna .gt. 0) call rangmod(aah,aat,aae,aap,aar,natot,nna)
      if(nnb .gt. 0) call rangmod(bbh,bbt,bbe,bbp,bbr,nbtot,nnb)
      if(nnc .gt. 0) call rangmod(cch,cct,cce,ccp,ccr,nctot,nnc)
      if(nnd .gt. 0) call rangmod(ddh,ddt,dde,ddp,ddr,ndtot,nnd)

      ntot(iabon)=min0(natot,nbtot,nctot,ndtot)

      write(lll,*) '   ntot(',iabon,')=',ntot(iabon)
      call log_debug(lll)
      
      ! interpolation sur log g   pour les 2 valeurs de teta
      t0=rglog(1,2)-rglog(1,1)
      t1 =inewmarcs_glog-rglog(1,1)

      ! write(6,*) '  t0,t1 ',t0,t1
      ! write(6,102)  rteff(1,1), inewmarcs_glog
      ! 102 format('  modele interpole avec   Teff=',f9.3,' log g=', f9.3)

      call interpol(t0,t1,aah,bbh,eeh,aat,bbt,eet, &
       aae,bbe,eee,aap,bbp,eep,aar,bbr,eer,ntot(iabon))
      ! write(6,102)  rteff(2,1), inewmarcs_glog
      call interpol(t0,t1,cch,ddh,ffh,cct,ddt,fft, &
       cce,dde,ffe,ccp,ddp,ffp,ccr,ddr,ffr,ntot(iabon))

      ! interpolation sur t
      t0=rteff(2,1)-rteff(1,1)
      t1=inewmarcs_teff-rteff(1,1)
      ! write(6,*) '  t0,t1 ',t0,t1
      ! write(6,102) inewmarcs_teff, inewmarcs_glog
      if (iabon.eq.1) then
        call interpol(t0,t1,eeh,ffh,zh1,eet,fft,zt1, &
         eee,ffe,ze1,eep,ffp,zp1,eer,ffr,zr1,ntot(iabon))
      end if

      if (iabon.eq.2) then
        call interpol(t0,t1,eeh,ffh,zh2,eet,fft,zt2, &
         eee,ffe,ze2,eep,ffp,zp2,eer,ffr,zr2,ntot(iabon))
      end if
    end do  ! fin du do iabon

    ! On a 2 modeles l'un interpole ds grille a amet1 et amet2
    !
    !     interpolation sur l'abondance
    !     les 2 modeles doivent commencer au meme niveau en log to
    !     et avoir la meme longueur
    to0 = amax1(zr1(1),zr2(1))
    nz1 = (to0-zr1(1))*10+0.1
    nz2 = (to0-zr2(1))*10+0.1

    write(lll,*)' to0max=', to0
    call log_debug(lll)
    write(lll,*) zr1(1),zr2(1)
    call log_debug(lll)
    call log_debug(' couches a oter:')
    write(lll,*) nz1, nz2
    call log_debug(lll)

    if(ntot(2) .gt. 0) call rangmod(zh1,zt1,ze1,zp1,zr1,ntot(1),nz1)
    if(ntot(2) .gt. 0) call rangmod(zh2,zt2,ze2,zp1,zr2,ntot(2),nz2)
    nntot=min0(ntot(1),ntot(2))

    write(lll,*) '   nntot=',nntot
    call log_debug(lll)

    t0 = amet2-amet1
    t1 = inewmarcs_amet-amet1

    call log_debug(' interpolation sur l''abondance avec')
    write(lll,*) ' amet2=',amet2,'       amet1=',amet1
    call log_debug(lll)
    write(lll,*) ' t0=', t0, '       t1=',t1
    call log_debug(lll)

    call interpol(t0,t1,zh1,zh2,zzh,zt1,zt2,zzt, &
     ze1,ze2,zze,zp1,zp2,zzp,zr1,zr2,zzr,nntot)

    ! calcule les elements alpha resultants
    asalk = ralfa(1) + t1/t0*(ralfa(2)-ralfa(1))

    !__logging__
    write(lll,*) 'model 1 inewmarcs_amet, alpha=',amet1,ralfa(1)
    call log_debug(lll)
    write(lll,*) 'model 2 inewmarcs_amet, alpha=',amet2,ralfa(2)
    call log_debug(lll)
    write(lll,*) 'result: inewmarcs_amet, alpha=',inewmarcs_amet,asalk
    call log_debug(lll)


    ! ***********************Ecriture du résultat*********************
    do n = 1,nntot
      zle(n)=zze(n)
      zlp(n)=zzp(n)
      zzh(n)=10**zzh(n)
      zze(n)=10**zze(n)
      zzp(n)=10**zzp(n)
    end do

    tir=tira//inewmarcs_tirb
    in = 0

    do n = 1,nntot
      in = in+1
      k = (in-1)*5+1
      a(k)  =zzh(n)  ! NH
      a(k+1)=zzt(n)  ! T
      a(k+2)=zze(n)  ! Pe
      a(k+3)=zzp(n)  ! Pg
      a(k+4)=zzr(n)  ! log to
    end do

    ! Writes binary file
    write(UNIT_MOD, rec=inewmarcs_jd) &
     nntot,          &
     inewmarcs_teff, &
     inewmarcs_glog, &
     inewmarcs_amet, &
     asalk,          &
     bhek,           &
     tir,            &
     tiabs,          &
     (a(k),k=1,nntot*5)
    write(UNIT_MOD,rec=inewmarcs_jd+1) 9999  ! 4 bytes, i guess

    ! Writes ASCII file
    bid0 = 0.0
    vvt = 2.0E+5 ! on prend vt constant
    write(UNIT_DAT,'(A30,I8,f10.0,f8.2,2f5.0)') &
     inewmarcs_modcode, &
     nntot,             &
     tostand,           &
     inewmarcs_glog,    &
     bid0,              &
     bid0
    do n = 1,nntot
      tau=10**zzr(n)
      tttt=5040/zzt(n)
      write(UNIT_DAT,'(E15.5,f10.0,3E15.6)') zzr(n), tttt, zle(n), zlp(n), vvt
    end do

    close(unit=UNIT_MOD)
    close(unit=UNIT_DAT)
  end


  !=======================================================================================
  !> Returns full path to file within models directory

  function model_path(filename) result(res)
    character(len=*), intent(in) :: filename  !< File name
    character(len=:), allocatable :: res

    res = trim_and_add_slash(inewmarcs_model_dirname) // trim(filename)
  end


  !=======================================================================================
  !> Reads map of models: file "modelmap.dat"
  !>
  !> By convention, this file is called "modelmap.dat". The information it contains is
  !> a list of metalicity ranges and corresponding reference models to be used.
  !>

  subroutine read_modelmap()
    character*128 :: t_fn
    real*8 t_met
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

    if (.not. flag_found)
      call pfant_halt('Metallicity '//real2str(inewmarcs_amet)//' is out of interval ['//&
       real2str(modelmap_met(1)//', '//modelmap_met(num_refmodels)//'['))
    end if
  end




  !---------------------------------------------------------------------------------------

  subroutine interpol(t0,t1,aah,bbh,eeh,aat,bbt,eet,aae,bbe,eee,aap,bbp,eep,aar,bbr,eer,&
   ntot)
    real*8, intent(in) :: t0, t1
    real*8, intent(in), dimension(MAX_MODELES_NTOT) :: &
     aah, aat, aae, aap, aar, bbh, bbt, bbe, bbp, bbr
    real*8, intent(out), dimension(MAX_MODELES_NTOT) :: &
     eeh, eet, eee, eep, eer
    real*8 u0 

    t2=t1/t0
    do n=1,ntot
      u0 = bbh(n)-aah(n)
      eeh(n) = aah(n) + (u0*t2)

      u0 = bbt(n)-aat(n)
      eet(n)= aat(n) + (u0*t2)

      u0 = bbe(n)-aae(n)
      eee(n)= aae(n) + (u0*t2)

      u0 = bbp(n)-aap(n)
      eep(n) = aap(n) + (u0*t2)

      eer(n) = aar(n)

      ! if(n.le.5) then
      !   write(6,'(i4,5f14.4)')n,eeh(n),eet(n),eee(n),eep(n),eer(n)
      ! end if
    end do
  end


  !--------------------------------------------------------------------------------------- 
  !> Enlever les nna premieres couches a un modele

  subroutine rangmod(aah,aat,aae,aap,aar,natot,nna)
    real*8, intent(in, out), dimension(MAX_MODELES_NTOT) :: aah, aat, aae, aap, aar
    integer, intent(in) nna
    integer, intent(in, out) natot
    natot = natot-nna
    do k = 1,natot
      aah(k) = aah(k+nna)
      aat(k) = aat(k+nna)
      aae(k) = aae(k+nna)
      aap(k) = aap(k+nna)
      aar(k) = aar(k+nna)
    end do
  end


  !---------------------------------------------------------------------------------------
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
  
  subroutine locatab(iabon)
    real*8 :: rteff(7),idt(7),ing(8),jg1(8),jg2(8),rglog(8),r1teff(7)
    real*8 :: idta(7), & ! indice des 1ers mod de chque temp
              inga(7)    ! nbre de logg pour chaq temp

    real*8 :: agloga1(8),agloga2(8),agloga3(8),agloga4(8), &
     agloga5(8),agloga6(7),agloga7(7),aglog(7,8)
    character*128 lll

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
    if ((iabon .eq. 1) .or. (iabon .eq. 2) )then

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
    else
      call pfant_halt('iabon equals '//int2str(iabon))

    end if

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
    id11=idt(jt1)+jg1(jt1)-1
    id12=idt(jt1)+jg2(jt1)-1
    id21=idt(jt2)+jg1(jt2)-1
    id22=idt(jt2)+jg2(jt2)-1
    call log_debug('Dans cette table les modeles entre lesquels on va interpoler ont les numeros:')
    write(lll,*)  id11,id12,id21,id22
    call log_debug(lll)
    call log_debug(LEAVING//'Sortie de locatab')
  end


  !> Lit sur disque acces direct nh,teta,pe,pg,t5l,ntot
  !> 
  !> Lit sur le fichier de type .mod nh,teta,pe,pg,t5l,ntot

  subroutine readerbn(rec_id,nh,teta,pe,pg,t5l,ntot, flag_open, flag_close)
    logical flag_open, flag_close
    real*4, intent(out), dimension(MAX_MODELES_NTOT) :: teta, nh, pe, pg, t5l
    real*4 bid(16), nhe

    common /com6/teff,inewmarcs_glog,asalog,asalalf,nhe,tiabs(5),tit(5)

    id = inewmarcs_jd

    call read_mod_record(nomfiple, inewmarcs_jd, &
      ntot,teff,inewmarcs_glog,asalog,asalalf,nhe,tit,tiabs, &
      nh,teta,pe,pg,t5l, flag_open, flag_close)



    open(unit=18,access='direct',status='old',file=nomfiple,
    &       recl=1200) ! linux f77
    return

    10 continue

    read(18,rec=id) ntot,teff,inewmarcs_glog,asalog,asalalf,nhe,tit,tiabs
    write(6,'(F10.0,4F10.2,5A4)') teff,inewmarcs_glog,asalog,asalalf,nhe,tit

    read(18,rec=id)bid,(nh(i),teta(i),pe(i),pg(i),t5l(i),i=1,ntot)


    do I=1,NTOT
    NH(I)=alog10(NH(i))
    PE(I)=alog10(PE(i))
    PG(I)=alog10(PG(i))
    end do

    call log_debug('        log NH           TETA          log PE         log PG     To(5000)')
    do i=1,3
      write(lll, ,'(e16.4,f15.4,2e16.4,f12.4)') nh(i),teta(i),pe(i),pg(i),t5l(i)
      call log_debug(lll)
    end do
    call log_debug('     ETC.....')

    106    FORMAT(2X,4F5.2,I5)
    107    FORMAT()
    108    FORMAT(' Modele calcule avec la table ',5A4)
  END



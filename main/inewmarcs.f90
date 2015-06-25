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
  character*25, public :: inewmarcs_modcode
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


  Logical Manuel
  real*8, dimension(MAX_MODELES_NTOT) :: &
   AAH,AAT,AAE,AAP,AAR, &
   BBH,BBT,BBE,BBP,BBR, &
   CCH,CCT,CCE,CCP,CCR, &
   DDH,DDT,DDE,DDP,DDR, &
   EEH,EET,EEE,EEP,EER, &
   FFH,FFT,FFE,FFP,FFR, &
   ZH1,ZT1,ZE1,ZP1,ZR1, &
   ZH2,ZT2,ZE2,ZP2,ZR2, &
   ZZH,ZZT,ZZE,ZZP,ZZR, &
   ZLE,ZLP

  real*8 :: A(MAX_MODELES_NTOT*5)  ! modele resultant
  real*8 :: RTeff(2,2),RGlog(2,2),Ralfa(2),ntot(2)
  character*4 chteff
  character*3 chlogg
  character*14 chmodz
  Character*5  tira
  Character*20 tir    ! titre du modele interpole
  Logical AUTO,aiguill
  data tira/' Ple '/
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




  subroutine inewmarcs_calc()
    character*192 :: lll

    ! WRITE(6,*) 'Nom des fich ou mettre les modeles interpoles'
    ! WRITE(6,*) ' .mod     et     ascii'
    ! read(5,*) inewmarcs_nomfimod, config_inewmarcs_nomfidat

    write(lll,*) 'Creating file ', inewmarcs_nomfidat
    call log_debug(lll)

    open(unit=21,file=inewmarcs_nomfidat,status='unknown')

    call log_debug('Opening file '//trim(inewmarcs_nomfimod)//' in status='//&
     trim(inewmarcs_model_status))

    open(unit=20,access='direct',status=inewmarcs_model_status, &
     file=config_inewmarcs_nomfimod, recl=1200) 

    ! write(6,*) ' Entrer Teff  log g   [M/H] nom du modele:inewmarcs_modcode '
    ! read(5,*) inewmarcs_teff, inewmarcs_glog,  inewmarcs_amet, inewmarcs_modcode
    ! write(6,*) ' ligne du modele en sortie, Titre (I3,A15)'
    ! read(5,101) inewmarcs_jd, inewmarcs_tirb
    ! write(6,103) inewmarcs_teff, inewmarcs_glog, inewmarcs_amet, inewmarcs_jd, inewmarcs_tirb

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
      call readerbn(iop,id,aah,aat,aae,aap,aar,ntot(iabon)) ! Open du fichier

      iop=1

      !-------------------------------------------------------------
      ! On cherche ou se trouvent inewmarcs_teff inewmarcs_glog  par rapport a la table

      call locatab(iabon,nomfiple)

      !-------------------------------------------------------------
      ! Lecture des modeles

      call readerbn(iop,id11,aah,aat,aae,aap,aar,natot)
      ! write(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
      rteff(1,1) = tefk
      rglog(1,1) = glogk

      call readerbn(iop,id12,bbh,bbt,bbe,bbp,bbr,nbtot)
      ! write(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
      rteff(1,2)=tefk
      rglog(1,2)=glogk

      call readerbn(IOP,ID21,CCH,CCT,CCE,CCP,CCR,NCTOT)
      ! write(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
      rteff(2,1)=tefk
      rglog(2,1)=glogk

      call readerbn(IOP,ID22,DDH,DDT,DDE,DDP,DDR,NDTOT)
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
      T0=RGLOG(1,2)-RGLOG(1,1)
      T1 =inewmarcs_glog     -RGLOG(1,1)

      ! write(6,*) '  t0,t1 ',t0,t1
      ! write(6,102)  rteff(1,1), inewmarcs_glog

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

    t0=amet2-amet1
    t1=inewmarcs_amet -amet1
    call log_debug(' interpolation sur l''abondance avec')
    write(lll,*) ' amet2=',amet2,'       amet1=',amet1
    call log_debug(lll)
    write(lll,*) ' t0=', t0, '       t1=',t1
    call log_debug(lll)
    call interpol(t0,t1,zh1,zh2,zzh,zt1,zt2,zzt, &
     ze1,ze2,zze,zp1,zp2,zzp,zr1,zr2,zzr,nntot)

    ! calcule les elements alpha resultants
    asalk = ralfa(1) + t1/t0*(ralfa(2)-ralfa(1))
    write(lll,*) 'model 1 inewmarcs_amet, alpha=',amet1,ralfa(1)
    call log_debug(lll)
    write(lll,*) 'model 2 inewmarcs_amet, alpha=',amet2,ralfa(2)
    call log_debug(lll)
    write(lll,*) 'result: inewmarcs_amet, alpha=',inewmarcs_amet,asalk
    call log_debug(lll)


    !   ***********************Ecriture du résultat*********************
    !
                do n=1,nntot
            ZLE(n)=ZZE(n)
            ZLP(n)=ZZP(n)
                ZZH(n)=10**ZZH(n)
                ZZE(n)=10**ZZE(n)
                ZZP(n)=10**ZZP(n)
                end do
         tir=tira//inewmarcs_tirb
         IN=0
              do n=1,nntot
              in=in+1
              k=(in-1)*5+1
              A(k)  =ZZH(n)  ! NH
              A(k+1)=ZZT(n)  ! T
              A(k+2)=ZZE(n)  ! Pe
              A(k+3)=ZZP(n)  ! Pg
              A(k+4)=ZZR(n)  ! log to
              end do
         kto=nntot*5
         write(20,REC=inewmarcs_jd)nntot,inewmarcs_teff,inewmarcs_glog,inewmarcs_amet,asalk,BHEK,tir,tiabs,
     &        (A(k),k=1,kto)
         inewmarcs_jd=inewmarcs_jd+1 ! Linux f77
       bid0=0.0
       vvt=2.0E+5    ! on prend vt constant
       tteff=5040/inewmarcs_teff
       write(21,104) inewmarcs_modcode, nntot, tostand, inewmarcs_glog, bid0, bid0
        do n=1,nntot
        tau=10**ZZR(n)
        TTTT=5040/ZZT(n)
        write(21,105) ZZR(n), TTTT, ZLE(n), ZLP(n), vvt
        end do
         WRITE(6,*) ' Autre interpolation ? (T/F)'
         READ(5,*)   aiguill
c         read(15,*) aiguill
         if (aiguill) go to 1
         write(20,REC=inewmarcs_jd) 9999
       inewmarcs_jd=inewmarcs_jd+1


 100   format(5f14.4)
 101     format(I3,A15)
 102     format('  modele interpole avec   Teff=',f9.3,' log g=',
     1      f9.3)
 103     format(F15.0,2f8.2,I8,A15)
 104   format(A30,I8,f10.0,f8.2,2f5.0)
 105   format(E15.5,f10.0,3E15.6)
           end


        subroutine interpol(T0,T1,AAH,BBH,EEH,AAT,BBT,EET,
     1  AAE,BBE,EEE,AAP,BBP,EEP,AAR,BBR,EER,NTOT)
        Dimension AAH(50),AAT(50),AAE(50),AAP(50),AAR(50)
        Dimension BBH(50),BBT(50),BBE(50),BBP(50),BBR(50)
        Dimension EEH(50),EET(50),EEE(50),EEP(50),EER(50)
c
c        write(6,*)'          log  NH        TETA         log  PE',
c     1   '        log PG        logtoR'
c
        T2=T1/T0
           DO N=1,NTOT
           U0 =BBH(N)-AAH(N)
           EEH(N)= AAH(N) + (U0*T2)
           U0= BBT(N)-AAT(N)
           EET(N)= AAT(N) + (U0*T2)
           U0= BBE(N)-AAE(N)
           EEE(N)= AAE(N) + (U0*T2)
           U0= BBP(N)-AAP(N)
           EEP(N)= AAP(N) + (U0*T2)
           EER(N)=AAR(N)
c              if(n.le.5) then
c              write(6,100)N,EEH(N),EET(N),EEE(N),EEP(N),EER(N)
c              end if
           END DO
           return
 100       format(I4,5f14.4)
           end


  !--------------------------------------------------------------------------------------- 
  !> Enlever les nna premieres couches a un modele

  subroutine rangmod(aah,aat,aae,aap,aar,natot,nna)
    real*8, intent(in), dimension(MAX_MODELES_NTOT) :: aah, aat, aae, aap, aar
    natot = natot-nna
    do k = 1,natot
      aah(k)=aah(k+nna)
      aat(k)=aat(k+nna)
      aae(k)=aae(k+nna)
      aap(k)=aap(k+nna)
      aar(k)=aar(k+nna)
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

    ! if (manuel) then
    !   write(6,*) ' Entrer le numero des 4 modeles entre lesquels vous'
    !   write(6,*) ' voulez interpoler'
    !   read(5,*)  id11,id12,id21,id22
    !   return
    ! endif

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

        SUBROUTINE READERBN (IOP,inewmarcs_jd,NH,TETA,PE,PG,T5L,NTOT)
        DIMENSION TETA(50),NH(50),PE(50),PG(50),T5L(50),BID(16)
        REAL NH,NHE
        COMMON /COM6/TEFF,inewmarcs_glog,ASALOG,ASALALF,NHE,TIABS(5),TIT(5)
        COMMON /COM1/Nomfiple
        Character*52 Nomfiple
c        DATA   BLC/'    '/
c             DO  I=1,5
c             TIT(I)=BLC
c             END DO
             ID=inewmarcs_jd
c
        IF(IOP.GE.1)   GO TO 10
c        WRITE(6,*) ' On fait l''open du fichier ',Nomfiple
        OPEN(UNIT=18,ACCESS='DIRECT',STATUS='OLD',FILE=Nomfiple,
     &       RECL=1200) ! Linux f77
        return
c
 10     Continue
c       WRITE(6,*) ' Lecture du modele', inewmarcs_jd
        READ(18,REC=ID) NTOT,TEFF,inewmarcs_glog,ASALOG,ASALALF,NHE,TIT,TIABS
        WRITE(6,105) TEFF,inewmarcs_glog,ASALOG,ASALALF,NHE,TIT
c        write(6,108) TIABS
c         ID=ID-1 ! Linux f77
        READ(18,REC=ID)BID,(NH(I),TETA(I),PE(I),PG(I),T5L(I),I=1,NTOT)
        write(6,*)'        log NH           TETA          log PE',
     &       '         log PG     To(5000)'
           do I=1,NTOT
           NH(I)=alog10(NH(i))
           PE(I)=alog10(PE(i))
           PG(I)=alog10(PG(i))
           end do
c
           do i=1,3
           WRITE(6,103) NH(I),TETA(I),PE(I),PG(I),T5L(I)
           end do
c
        WRITE(6,107)
        ID=1
        RETURN
 102    FORMAT('   MODELE A LIRE ',F8.0,3F8.2,F8.3,4X,'LIGNE',I3,
     &         //' MODELES SUR LE DISQUE')
 103    FORMAT(E16.4,F15.4,2E16.4,F12.4)
 104    FORMAT('   ECRITURE DE TIT(1) ',A4,5X,Z4)
 105    FORMAT(F10.0,4F10.2,5A4)
 106    FORMAT(2X,4F5.2,I5)
 107    FORMAT('     ETC.....')
 108    FORMAT(' Modele calcule avec la table ',5A4)
        END



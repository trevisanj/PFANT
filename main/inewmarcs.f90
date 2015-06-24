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
  Character*52 config_inewmarcs_nomfimod, config_inewmarcs_nomfidat, Nomfiple, Nomfipl(2)
  Character*25 inewmarcs_modcode
  Character*5  tira
  Character*15 inewmarcs_tirb
  Character*20 tir    ! titre du modele interpole
  Logical AUTO,aiguill
  data tira/' Ple '/
  common/COM1/Nomfiple
  COMMON/COM6/TEFK,GLOGK,ASAK,ASALK,BHEK,TIABS(5),TITK(5)


      tostand=5000


  integer, parameter:: MAX_NUM_REFMODELS = 10
  integer num_refmodels
  real*8, dimension(MAX_NUM_REFMODELS) :: modelmap_met
  character*128, dimension(MAX_NUM_REFMODELS) :: modelmap_fn
contains



  !=======================================================================================
  !> Reads map of models: file "modelmap.dat"
  !>
  !> By convention, this file is called "modelmap.dat". The information it contains is
  !> a list of metalicity ranges and corresponding reference models to be used.
  !>

  subroutine read_modelmap(dirname) ! @todo actually I think the dirname will be configured at module level
    !> Directory containing ascii file "modelmap.dat"
    character(len=*), intent(in) :: filename
    character*128 :: t_fn
    real*8 t_met
    integer i
    parameter(unit_=199)

    open(unit=unit_,file=filename, status='old')

    i = 1
    num_refmodels = 0
    do while (.true.)
      read(unit_, *, end=10) t_met, t_fn

      if (to_lower(trim(t_fn)) .eq. 'none') exit

      modelmap_met(i) = t_met
      modelmap_fn(i) = t_fn
      num_refmodels = num_refmodels+1
    end do
    goto 11

    10 continue
    call pfant_halt('Last filename in modelmap.da must be "none"')

    11 continue
    close(unit=unit_)
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


      Manuel=.false. !> @todo this was removed; may re-implement using command-line option if necessary










!        WRITE(6,*) 'Nom des fich ou mettre les modeles interpoles'
!      WRITE(6,*) ' .mod     et     ascii'
!        read(5,*) config_inewmarcs_nomfimod, config_inewmarcs_nomfidat

      WRITE(6,*) ' creation de ',config_inewmarcs_nomfidat
      OPEN(UNIT=21,FILE=config_inewmarcs_nomfidat,status='unknown')
        WRITE(6,*) ' Le fichier ', config_inewmarcs_nomfimod



        call log_debug('Opening file '//trim(config_inewmarcs_nomfimod)//' in status='//&
         trim(inewmarcs_model_status))
         open(unit=20,access='direct',status=inewmarcs_model_status, &
          file=config_inewmarcs_nomfimod, recl=1200) 

c
 1      WRITE(6,*) ' Entrer Teff  log g   [M/H] nom du modele:inewmarcs_modcode '
         read(5,*) inewmarcs_teaff, inewmarcs_glog,  inewmarcs_amet, inewmarcs_modcode
         WRITE(6,*) ' ligne du modele en sortie, Titre (I3,A15)'
         read(5,101) inewmarcs_jd, inewmarcs_tirb
         WRITE(6,103) inewmarcs_teaff, inewmarcs_glog, inewmarcs_amet, inewmarcs_jd, inewmarcs_tirb
!c
!c
!c
!      if (inewmarcs_amet .GE. -2 .AND. inewmarcs_amet .LT. -1.5) then
!      nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newnewm200.mod'
!      nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newnewm150.mod'
!      AMET1=-2.00 !sert a l'interpolation sur la metallicite
!      AMET2=-1.50
!      else if (inewmarcs_amet .GE. -1.5 .AND. inewmarcs_amet .LT. -1) then
!C        if (inewmarcs_amet .GE. -1.5 .AND. inewmarcs_amet .LT. -1.0) then
!      nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newnewm150.mod'
!      nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newnewm100.mod'
!      AMET1=-1.50 !sert a l'interpolation sur la metallicite
!      AMET2=-1.00
!      else if (inewmarcs_amet .GE. -1. .AND. inewmarcs_amet .LT. -0.50) then
!      nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newnewm100.mod'
!      nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newnewm050.mod'
!      AMET1=-1.00 !sert a l'interpolation sur la metallicite
!      AMET2=-0.50
!C%    else if (inewmarcs_amet .GE. -0.75 .AND. inewmarcs_amet .LT. -0.5) then
!C%    nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newm075.mod'
!C%    nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newm050.mod'
!C%    AMET1=-0.75 !sert a l'interpolation sur la metallicite
!C%    AMET2=-0.50
!      else if (inewmarcs_amet .GE. -0.5 .AND. inewmarcs_amet .LT. 0.00) then
!      nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newnewm050.mod'
!      nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newnewp000.mod'
!      AMET1=-0.50 !sert a l'interpolation sur la metallicite
!      AMET2=+0.00
!C%    else if (inewmarcs_amet .GE. -0.25 .AND. inewmarcs_amet .LT. 0.00) then
!C%    nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newm025.mod'
!C%    nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newp000.mod'
!C%    AMET1=-0.25 !sert a l'interpolation sur la metallicite
!C%    AMET2=+0.00
!      else if (inewmarcs_amet .GE. 0.00 .AND. inewmarcs_amet .LT. 0.25) then
!      nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newnewp000.mod'
!      nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newnewp025.mod'
!      AMET1=+0.00 !sert a l'interpolation sur la metallicite
!      AMET2=+0.25
!C%    else if (inewmarcs_amet .GE. 0.25 .AND. inewmarcs_amet .LT. 0.50) then
!C%    nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newmarcsp025corr3.mod'
!C%    nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newmarcsp050corr.mod'
!C%    AMET1=+0.25 !sert a l'interpolation sur la metallicite
!C%    AMET2=+0.50
!C%    else if (inewmarcs_amet .GE. 0.50 .AND. inewmarcs_amet .LT. 0.75) then
!C%    nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newmarcsp050corr.mod'
!C%    nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newmarcsp075corr.mod'
!C%    AMET1=+0.50 !sert a l'interpolation sur la metallicite
!C%    AMET2=+0.75
!C%    else if (inewmarcs_amet .GE. 0.75 .AND. inewmarcs_amet .LE. 1) then
!C%    nomfipl(1)='C:\pfant06_newmarcs\newmarcs\newmarcsp075corr.mod'
!C%    nomfipl(2)='C:\pfant06_newmarcs\newmarcs\newp100.mod'
!C%    AMET1=+0.75 !sert a l'interpolation sur la metallicite
!C%    AMET2=+1.00
!      else
!         write(*,*) "not in the grid metallicity boundaries"
!         STOP
!      endif
C
C**********************Boucle sur l'abondance*******************
      do iabon=1,2    ! on interpole dans 2 grilles d'abondance
      nomfiple = nomfipl(iabon)
c
10 continue
      call pfant_debug('On va interpoler dans la table de Plez'//nomfiple)
      call pfant_debug('Indice d''abondance'//int2str(iabon))
c   -------------------------------------------------------------
        IOP=0
        ID=1
        call READERBN(IOP,ID,AAH,AAT,AAE,AAP,AAR,NTOT(IABON)) ! Open du fichier
        IOP=1
c   -------------------------------------------------------------
c       On cherche ou se trouvent inewmarcs_teaff inewmarcs_glog  par rapport a la table
        call locatab(inewmarcs_teaff,inewmarcs_glog,IABON,Nomfiple,Manuel,
     1               ID11,ID12,ID21,ID22)
          WRITE(6,*) 'Dans cette table les modeles entre lesquels on va'
          WRITE(6,*) 'interpoler ont les numeros:'
          WRITE(6,*)  ID11,ID12,ID21,ID22
c
c     Lecture des modeles
c           WRITE(6,*) ' '
c           WRITE(6,*) ' Modeles entre lesquels on va interpoler'
           call readerbn(IOP,ID11,AAH,AAT,AAE,AAP,AAR,NATOT)
c           WRITE(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
           rteff(1,1)=tefk
           rglog(1,1)=glogk
           call readerbn(IOP,ID12,BBH,BBT,BBE,BBP,BBR,NBTOT)
c           WRITE(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
           rteff(1,2)=tefk
           rglog(1,2)=glogk
           call readerbn(IOP,ID21,CCH,CCT,CCE,CCP,CCR,NCTOT)
c           WRITE(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
           rteff(2,1)=tefk
           rglog(2,1)=glogk
           call readerbn(IOP,ID22,DDH,DDT,DDE,DDP,DDR,NDTOT)
c           WRITE(6,*) ' tefk glogk,met,alfa=',tefk,glogk ,asak,asalk
           rteff(2,2)=tefk
           rglog(2,2)=glogk
c   sets the alpha abundance of the models for each IABON
         ralfa(IABON)=ASALK
c
c           WRITE(6,*) ' '
c           WRITE(6,*) ' Valeur des rglog'
c           WRITE(6,*) ' rglog(1,i)', (rglog(1,i),i=1,2)
c           WRITE(6,*) ' rglog(2,i)', (rglog(2,i),i=1,2)
           to0=AMAX1(AAR(1),BBR(1),CCR(1),DDR(1))
           nna=(to0-AAR(1))*10+0.1
           nnb=(to0-BBR(1))*10+0.1
           nnc=(to0-CCR(1))*10+0.1
           nnd=(to0-DDR(1))*10+0.1
           WRITE(6,*) ' to0max=', to0
           WRITE(6,*) AAR(1),BBR(1),CCR(1),DDR(1)
           WRITE(6,*) ' couches a oter'
           WRITE(6,*)  nna, nnb, nnc, nnd
c     les 4 modeles doivent commencer au meme niveau en log toR
           if(nna.gt.0)call rangmod(AAH,AAT,AAE,AAP,AAR,NATOT,nna)
           if(nnb.gt.0)call rangmod(BBH,BBT,BBE,BBP,BBR,NBTOT,nnb)
           if(nnc.gt.0)call rangmod(CCH,CCT,CCE,CCP,CCR,NCTOT,nnc)
           if(nnd.gt.0)call rangmod(DDH,DDT,DDE,DDP,DDR,NDTOT,nnd)
           ntot(IABON)=MIN0(NATOT,NBTOT,NCTOT,NDTOT)
           WRITE(6,*) '   ntot(',IABON,')=',ntot(IABON)
c
c
c          interpolation sur log g   pour les 2 valeurs de teta
           T0=RGLOG(1,2)-RGLOG(1,1)
           T1 =inewmarcs_glog     -RGLOG(1,1)
c           WRITE(6,*) '  T0,T1 ',T0,T1
c           WRITE(6,102)  RTeff(1,1), inewmarcs_glog
           CALL interpol(T0,T1,AAH,BBH,EEH,AAT,BBT,EET,
     &         AAE,BBE,EEE,AAP,BBP,EEP,AAR,BBR,EER,ntot(IABON))
c           WRITE(6,102)  RTeff(2,1), inewmarcs_glog
           CALL interpol(T0,T1,CCH,DDH,FFH,CCT,DDT,FFT,
     &         CCE,DDE,FFE,CCP,DDP,FFP,CCR,DDR,FFR,ntot(IABON))

c          interpolation sur T
           T0=RTeff(2,1)-RTeff(1,1)
           T1=inewmarcs_teaff     -RTeff(1,1)
c           WRITE(6,*) '  T0,T1 ',T0,T1
c           WRITE(6,102) inewmarcs_teaff, inewmarcs_glog
           if (IABON.EQ.1) then
           CALL interpol(T0,T1,EEH,FFH,ZH1,EET,FFT,ZT1,
     &          EEE,FFE,ZE1,EEP,FFP,ZP1,EER,FFR,ZR1,ntot(IABON))
            end if
           if (IABON.EQ.2) then
           CALL interpol(T0,T1,EEH,FFH,ZH2,EET,FFT,ZT2,
     &          EEE,FFE,ZE2,EEP,FFP,ZP2,EER,FFR,ZR2,ntot(IABON))
            end if
          END DO  ! Fin du DO IABON

c On a 2 modeles l'un interpole ds grille a AMET1 et AMET2
c
c     interpolation sur l'abondance
c     les 2 modeles doivent commencer au meme niveau en log to
c     et avoir la meme longueur
           to0=AMAX1(ZR1(1),ZR2(1))
           nz1=(to0-ZR1(1))*10+0.1
           nz2=(to0-ZR2(1))*10+0.1
           write(6,*)' to0max=', to0
           write(6,*)ZR1(1),ZR2(1)
           write(6,*)' couches a oter:'
           write(6,*) nz1, nz2
           if(ntot(2).gt.0)call rangmod(ZH1,ZT1,ZE1,ZP1,ZR1,NTOT(1),nz1)
           if(ntot(2).gt.0)call rangmod(ZH2,ZT2,ZE2,ZP1,ZR2,NTOT(2),nz2)
           nntot=MIN0(NTOT(1),NTOT(2))
           write(6,*)'   nntot=',nntot

        T0=AMET2-AMET1
        T1=inewmarcs_amet -AMET1
      WRITE(6,*) ' interpolation sur l''abondance avec'
      WRITE(6,*) ' AMET2=',AMET2,'       AMET1=',AMET1
      WRITE(6,*) ' T0=', T0, '       T1=',T1
        CALL interpol(T0,T1,ZH1,ZH2,ZZH,ZT1,ZT2,ZZT,
     &          ZE1,ZE2,ZZE,ZP1,ZP2,ZZP,ZR1,ZR2,ZZR,nntot)
c calcule les elements alpha resultants
      asalk=ralfa(1) + T1/T0*(ralfa(2)-ralfa(1))
      write(6,*) 'model 1 inewmarcs_amet, alpha=',AMET1,ralfa(1)
      write(6,*) 'model 2 inewmarcs_amet, alpha=',AMET2,ralfa(2)
      write(6,*) 'Result: inewmarcs_amet, alpha=',inewmarcs_amet,asalk
c
c
c   ***********************Ecriture du résultat*********************
c
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
         write(20,REC=inewmarcs_jd)nntot,inewmarcs_teaff,inewmarcs_glog,inewmarcs_amet,asalk,BHEK,tir,tiabs,
     &        (A(k),k=1,kto)
         inewmarcs_jd=inewmarcs_jd+1 ! Linux f77
       bid0=0.0
       vvt=2.0E+5    ! on prend vt constant
       tteff=5040/inewmarcs_teaff
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

      subroutine locatab (inewmarcs_teaff,inewmarcs_glog,IABON,Nomfiple,Manuel,
     1      ID11,ID12,ID21,ID22)
c On cherche les numeros des 4 modeles de la table entre lesquels
c le programme devra interpoler. On donne les limites en T et g
c des modeles.
c Les modeles doivent etre ranges en temperature croissante
c a l'interieur de chaque temp les gravites doivent croitre
c       NEWMARCS 2005 metallicities -1.5 to +1.00 included
c
      Logical Manuel
      Character*52 Nomfiple
        Dimension RTeff(7),IDT(7),ING(8),JG1(8),JG2(8),RGlog(8),
     1            R1Teff(7)
      Dimension IDTA(7),INGA(7)
      Dimension AGloga1(8),AGloga2(8),AGloga3(8),AGloga4(8),
     1            AGloga5(8),AGloga6(7),AGloga7(7),
     2              AGlog(7,8)

        data R1teff/4000.,4250.,4500.,4750.,5000.,5250.,5500./
c
c
        data AGloga1 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
        data AGloga2 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
        data AGloga3 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
        data AGloga4 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
        data AGloga5 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
        data AGloga6 /0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
        data AGloga7 /0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
c        data AGloga8 /0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5/
c
        data IDTa /1, 9, 17, 25, 33, 41, 48/
c     IDTA indice des 1ers mod de chque temp
        data INGa /8, 8, 8, 8, 8, 7, 7/
c     INGA nbre de logg pour chaq temp
c
      if (Manuel) then
      WRITE(6,*) ' Entrer le numero des 4 modeles entre lesquels vous'
      WRITE(6,*) ' voulez interpoler'
      READ(5,*)  ID11,ID12,ID21,ID22
      return
      endif
c
c
c        Entree dans le SP locatab
      WRITE(6,*) ' Entree dans le SP locatab'
      If ((IABON.eq.1) .or. (IABON.eq.2) )then
      NT=7    ! nbre total de temperatures
      NG=7    ! nbre maximum de log g
      Do n=1,nt
      Rteff(n)=R1teff(n)
      ING(n)=INGa(n)
      IDT(n)=IDTa(n)
      end do
      do n=1,ng
      AGlog(1,n)=AGloga1(n)
      AGlog(2,n)=AGloga2(n)
      AGlog(3,n)=AGloga3(n)
      AGlog(4,n)=AGloga4(n)
      AGlog(5,n)=AGloga5(n)
      AGlog(6,n)=AGloga6(n)
      AGlog(7,n)=AGloga7(n)
c     AGlog(8,n)=AGloga8(n)
c     AGlog(9,n)=AGloga9(n)
c     AGlog(10,n)=AGloga10(n)
c     AGlog(11,n)=AGloga11(n)
      end do
      WRITE(6,*) ' liste du nbre de G en fonction de T'
      WRITE(6,*) (ING(n),n=1,nt)
      end if
c sera suivi d'ordres equivalent si plusieurs abond donc autres IABON
c etc...
c
           Do I=1,NT
           jT2=I
           if(inewmarcs_teaff.lt.Rteff(I)) go to 11
           end do

 11        if (JT2.EQ.1) JT2=2
           JT1=JT2-1
           WRITE(6,*)  'Indices de temperatures', JT1,JT2

           DO JJT=JT1,JT2
         NGG=ING(JJT)
         write(6,*)'JTT=',JJT,'   NGG=',NGG
         WRITE(6,*) 'Nbre de gravite pour la temp NGG=',NGG
              do i=1,NGG
              rglog(I)=AGLOG(jjt,i)
              end do
            write(6,*) ' rglog(i) pour jjt=', jjt
            write(6,100) (rglog(i),i=1,NGG)
c
              DO I=1,NGG
              JG2(JJT)=I
              if(inewmarcs_glog.lt.Rglog(I))  go to 12
              end do
 12           if (JG2(JJT).EQ.1) JG2(JJT)=2
              JG1(JJT)=JG2(JJT)-1
c
          WRITE(6,*) ' jjt jg1 jg2', jjt, jg1(jjt), jg2(jjt)
          WRITE(6,*)
           end do ! fin de la bcle sur JJT
c
           WRITE(6,*) ' jjt, idt jg1(jjt), jg2(jjt) ',JT1,idt(JT1),
     &            jg1(jt1) , jg2(jt1)
           WRITE(6,*) ' jjt, idt jg1(jjt), jg2(jjt) ',JT2,idt(JT2),
     &            jg1(jt2) , jg2(jt2)
c
c     ID des modeles dans la table
           ID11=IDT(JT1)+JG1(JT1)-1
           ID12=IDT(JT1)+jG2(JT1)-1
           ID21=IDT(JT2)+JG1(JT2)-1
           ID22=IDT(JT2)+JG2(JT2)-1
          WRITE(6,*) 'Dans cette table les modeles entre lesquels on va'
          WRITE(6,*) 'interpoler ont les numeros:'
          WRITE(6,*)  ID11,ID12,ID21,ID22
          WRITE(6,*) '   Sortie de locatab'
        return
100   format(10f5.1)
        end

        SUBROUTINE READERBN (IOP,inewmarcs_jd,NH,TETA,PE,PG,T5L,NTOT)
C       CE S.P. LIT SUR DISQUE ACCES DIRECT NH,TETA,PE,PG,T5L,NTOT
c       lit sur le fichier de type .mod  NH,TETA,PE,PG,T5L,NTOT
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



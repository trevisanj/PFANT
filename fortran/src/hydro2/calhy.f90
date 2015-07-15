!> Sous-programmes a linker avec HYDRO et ABSORU /Janvier 1994

module hydro2_calhy
  use logging
  implicit none
  character*256 lll

   real*8 :: RPI = 1.772454, &
             PI =  3.141593



  real*8 :: modif_var(220),modif_f1(220),modif_phi(300),modif_v(300), modif_v
  integer :: modif_ih,modif_ii




contains
    !> L EST LE COEFFICIENT D'ABSORPTION PAR PARTICULE ABSORBANTE
    !>   
    !>
    !> LL=1,CALCUL EN UN POINT SPECIFIE PAR SA LONGUEUR D'ONDE
    !> SI DANS CE CAS,J1=2 ON SOMME LES COEFF. D'ABSORPTION DE PLUSIEURS
    !> RAIES D'UNE MEME SERIE
    !> LL=2,CALCUL EN UN OU PLUSIEURS POINTS SPECIFIES PAR DELTA LAMBDA

    !> SI DANS CE CAS,J1=1,LA CONVOLUTION FINALE EST SAUTEE.

    !> config_kq=1, CALCUL D UN PROFIL PUREMEMT QUASISTATIQUE POUR IONS + ELETRO

  subroutine raiehu(ll,na,nb,nbmin,nbmax,clam,c1,c,dlam,l,j1)
    logical d3_stark
    real*4 l,lim,lac,lv
    real*8 clam,xdp(50),xbdp

    ! Verbose flag that was kept, but set internally, not as a parameter as originally
    ! SI IND=0,ON ECRIT LE DETAIL DES APPROX. PAR LESQUELLES L EST CALCU
    ! SI IND=1, PAS D'ECRITURE
    logical, parameter :: ind = 1


    ! try to make them paRAMETERS, if doesnt work, keep upper case names anyway
    real*8, dimension(20), parameter :: V1, V2, V3, V4, V5, R1, R2, R3, R4, R5
    real*8, dimension(20,5) :: T1, T2
    real*8, parameter :: CK,R,CKP,CL,CMH



    real*8 :: az(99)

    DIMENSION BIDON(50,99)
    DIMENSION L(50,99),C(20),DLAM(50),ALFA(50),LV(50),AL(50),VX(50),STOC(99),VAL(5),RES(20),U(20),Q(5),,X(50),d3_hyn(99)
    COMMON /D2/VT(99),d2%cmu,NZ,IMAX,JMAX,d2%iqm,IJ(10)
    COMMON/D3/d3_hyn,az,d3_stark

    !     T1 PROFIL QUASISTAT. MOYEN POUR H ALPHA ET H BETA (DISTR. DU CHAMP
    !     ELEC. DE MOZER ET BARANGER) T1= CAB*S(ALPHA)
    !     T2 ID POUR LES AUTRES RAIES

    equivalence(V1(1),T1(1,1)),(V2(1),T1(1,2)),(V3(1),T1(1,3)),(V4(1),&
     T1(1,4)),(V5(1),T1(1,5))
    equivalence(R1(1),T2(1,1)),(R2(1),T2(1,2)),(R3(1),T2(1,3)),(R4(1),
     T2(1,4)),(R5(1),T2(1,5))
    data U/0.,0.5,1.,1.5,2.,2.5,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,14.,16.,18.,20./
    data V1/0.11133,0.11133,0.11000,0.10384,0.09049,0.07881,0.06708,0.04860,0.03363,&
     0.02326,0.01573,0.01089,0.00775,0.00571,0.00433,0.00336,0.00215,0.00147,0.00106,0.00079/
    data V2/0.12401,0.12267,0.11849,0.10935,0.09736,0.08100,0.06584,0.04503,0.02941,&
     0.01943,0.01313,0.00930,0.00681,0.00511,0.00388,0.00308,0.00203,0.00141,0.00103,0.00078/
    data V3/0.13908,0.13753,0.13130,0.11699,0.09511,0.07911,0.06285,0.03965,0.02501,&
     0.01653,0.01144,0.00828,0.00614,0.00466,0.00362,0.00291,0.00194,0.00138,0.00102,0.00077/
    data V4/0.17067,0.16488,0.15037,0.11669,0.09705,0.07365,0.05465,0.03289,0.02076,&
     0.01371,0.00971,0.00703,0.00521,0.00405,0.00314,0.00255,0.00173,0.00132,0.00091,0.00070/
    data V5/0.18045,0.17633,0.16263,0.12921,0.09578,0.06703,0.04913,0.02893,0.01783,&
     0.01192,0.00844,0.00618,0.00469,0.00364,0.00289,0.00235,0.00163,0.00118,0.00078,0.00068/
    data R1/0.09671,0.09586,0.09454,0.09029,0.08330,0.07596,0.06854,0.05219,0.03848,&
     0.02789,0.01944,0.01344,0.00967,0.00718,0.00544,0.00420,0.00267,0.00181,0.00131,0.00099/
    data R2/0.10882,0.10815,0.10757,0.10048,0.09052,0.07975,0.06709,0.04805,0.03324,&
     0.02258,0.01592,0.01127,0.00820,0.00619,0.00481,0.00377,0.00246,0.00170,0.00126,0.00098/
    data R3/0.12412,0.12267,0.11988,0.11054,0.09409,0.07826,0.06435,0.04301,0.02841,&
     0.01921,0.01361,0.00974,0.00722,0.00541,0.00430,0.00342,0.00228,0.00160,0.00120,0.00094/
    data R4/0.14759,0.14595,0.13621,0.11587,0.09529,0.07484,0.05809,0.03665,0.02367,&
     0.01619,0.01133,0.00840,0.00630,0.00489,0.00384,0.00305,0.00206,0.00149,0.00111,0.00085/
    data R5/0.17122,0.16737,0.14580,0.11845,0.09197,0.07139,0.05415,0.03188,0.02012,&
     0.01361,0.00985,0.00731,0.00555,0.00430,0.00342,0.00278,0.00192,0.00139,0.00104,0.00081/     
    data VAL/0.0,0.2,0.4,0.6,0.8/,&
         CK,R,CKP,CL,CMH /1.38E-16,109708.3,6.9952E-13,2.9978E10,1.673E-24/

    acte = 2.**0.6666667
    bcte = 2.**0.1666667
    dcte = 2.*ck/(cmh*d2%cmu)
    ecte = 1.5127e-27
    ar = na
    zr = nz
    z2 = zr**2
    z3 = z2*zr
    z5 = z3*z2
    zdem = zr**2.5
    rzr = r*zr
    a2 = ar**2
    go to (1,2),ll

    1 rad = clam*rzr*1.0e-08
    s = ar*sqrt(rad/(rad-a2))
    n = int(s)
    ! n = jint(s)
    n2 = n+1

    do 70 j = n,n2
      rj = j
      rj2 = rj**2
      x(j)=rj2*a2*1.0e+8/(rzr*(rj2-a2))
      xdp(j)=dble(x(j))
      dlam(j)=dabs(clam-xdp(j))
    70 continue


    if (dlam(n+1)-dlam(n))11,12,12
    11 nb1 = n+1
    go to 13

    12 nb1 = n

    13 if (j1 .eq. 2) go to 14

    write(lll,'(8(1P,E15.7))') dlam(j)
    call log_debug(lll)
    go to 15

    !
    !     CALCUL D'UN BLEND
    14 do 55 i = 1,imax
      55 stoc(i)=0.

    if ((nbmax.eq.0).and.(nbmin.eq.0)) go to 15

    43 k1 = nbmax-nbmin+1
    if (nbmin.eq.nb1)ik1 = 3
    if ((nbmax.gt.nb1).and.(nbmin.lt.nb1))ik1 = 2
    if ((nbmax.eq.nb1).and.(nbmin.lt.nb1)) ik1 = 1

    !     K1=NBRE DE RAIES CONTRIBUANT AU BLEND, K= INDICE DE COMPTAGE
    !     IK1=1 BLEND AVEC RAIES MOINS ELEVEES DANS LA SERIE
    !     IK1=2 BLEND AVEC RAIES DE PART ET D'AUTRE DE LA RAIE CONSIDEREE
    !     IK1=3 BLEND AVEC RAIES PLUS  ELEVEES DANS LA SERIE
    15 k = 1
    ibou = 1
    nb = nb1

    !
    !     REPRISE DE LA SEQUENCE NORMALE
    16 br = nb
    b2 = br**2
    xb = b2*a2*1.0e+8/(r*(b2-a2))
    xbdp = dble(xb)
    delta = dabs(clam-xbdp)
    c1 = c(nb)
    dlam(1)=delta
    cl2 = xb**2
    go to 80

    2 br = nb
    b2 = br**2
    cl2 = clam**2

    !     CALCUL DE QUANTITES DEPENDANT DE A,B,Z
    80 f = b2-a2
    b2a2 = b2*a2
    br4 = b2**2
    g = br4*br+a2**2*ar
    cab = 5.5e-5*b2a2**2/(z5*f)
    ct = 1.496*cab**1.5
    if ((na.eq.1).and.(nb.eq.2))ct = 3.4e-6
    if ((na.eq.1).and.(nb.eq.3))ct = 1.78e-5
    if ((na.eq.2).and.(nb.eq.3))ct = 1.3e-3
    if ((na.eq.2).and.(nb.eq.4))ct = 3.57e-3
    if ((na.eq.2).and.(nb.eq.5))ct = 6.0e-3
    if ((na.eq.2).and.(nb.eq.6))ct = 9.81e-3

    !     CT DE GRIEM,KOLB,SHEN,NRL REPORT 5455
    bic = g/(zr*f)
    ho = sqrt(f)
    din = b2a2/ho
    dan = g/(b2a2*ho)

    ! CALCUL DE QUANTITES NECESSAIRES A CONV4 INDEPENDANTES DU MODELE ET DU
    ! PROFIL
    pas = 0.001
    modif_var(1)=0.
    id = 2
    i4 = 11
    ical = 0

    260 do 250 i = id,i4
      modif_var(i)=modif_var(i-1)+pas
    250 continue

    ical = ical+1
    id = i4+1
    go to(200,201,202,203,204,205),ical

    200 i4 = 21
    pas = 0.01
    go to 260
    
    201 i4 = 23
    pas = 0.045
    go to 260
    
    202 i4 = 171
    pas = 0.10
    go to 260
    
    203 i4 = 191
    pas = 0.25
    go to 260
    
    204 i4 = 211
    pas = 0.5
    go to 260
    
    !
    !     CALCUL DE QUANTITES DEPENDANT DU MODELE
    205 modif_ih = i4
    call log_debug('VOUS ENTREZ DANS LA BOUCLE LA PLUS EXTERIEURE QUI '//&
     'PORTE SUR L INDICE DU NIVEAU DU MODELE'
    do 17 i = 1,imax
      write(lll,*)'      CALCUL AU NIVEAU',i,' DU MODELE'
      call log_debug(lll)
      t = 5040.39/modeles_teta(i)
      cne = modeles_teta(i)*exp(2.3026*pe_log(i))/ckp
      cam = cne**0.6666667
      cam1 = cne**0.1666667
      fo = 1.2532e-9*cam
      fac = 1.0e8*c1/fo
      alfad = clam*sqrt(dcte*t+vt(i)*vt(i))/(fo*cl)
      dld = alfad*fo
      az(i)=ecte*d3_hyn(i)*cl2/(alfad*fo)
      ddop = 0.8325*alfad
      rsurl = 0.0898*cam1*t**(-0.5)
      kp = 0
      delie = 4.736e-06*cl2/(br*(br-1.)*modeles_teta(i))
      delom = zr*cl2*0.3503192 e-4/(b2*modeles_teta(i))
      delp = 2.9953e-15*cl2*cne**0.5
      lim = 1.23e-18*cl2*b2*cam
      dd1 = alog10(delom/delp)
      if (config_kq.eq.1) go to 24

      !     QUANTITES UTILES UNIQUEMENT POUR ELARGISSEMENT IMPACT
      !     SIGNIFICATION DE CES QUANTITES  VOIR ARTICLES DE GRIEM ET DE MOZER
      cam2 = cne**0.3333333
      tdeg = 1.39e4/cam1
      timp = 2.1e10*t/cam
      tdens = 1.277e5*t**0.5/cam2
      ens = b2-tdens
      rnt = 4.6*(z3/t)**0.5*alog10(4.0e6*t*zr/(b2*cne**0.5))*dan
      gam1 = 1.217e-6*rnt*din*cam2/zdem
      bom = delom/(fo*cab)
      gams = 1.5*pi/(bom**0.5)
      dmoin = 1./sqrt(delom)
      dd = dd1*2.305855
      ceg = b2-tdeg
      aldp = ddop
      go to 120

      24 aldp = ddop/bcte
      if (ind.eq.1) go to 94

      call log_debug('1 RSURL        FZERO        ALFAD        NE DELIE        DELOM        DELP         LIM           I')
      write (lll, '(3X,1P,8E13.5,4X,I2)') rsurl,fo,alfad,cne,delie,delom,delp,lim,i
      call log_debug(lll)     
      go to 94

      120 if (ind.eq.1) go to 94

      call log_debug('1 DELOM        DELP        DELIE        RSURL R(N,T)       FZERO        LIM         ALFAD         NEI')
      write(lll,'(3X,9(E12.5,1X),3X,I2/)')DELOM,DELP,DELIE,RSURL,RNT,FO,LIM,ALFAD,CNE,I
      call log_debug(lll)

      94 if (ll-1) 19,19,20

      !     CALCUL AUX DIFFERENTS POINTS DU PROFIL

      !> @todo issue whaaaaat??? resetting JMAX???
      19 jmax = 1


      call log_debug(' VOUS ENTEZ DANS LA BOUCLE INTERIEURE QUI PORTE SUR L INDICE DE POINT DU PROFIL')
      20 do 1010 j = 1,jmax
        alfa(j)=dlam(j)/fo
        beta = alfa(j)/cab
        iz = 1
        if (alfa(j).lt.1.e-07) go to 23

        s1 = ct/alfa(j)**2.5
        if ((dlam(j).eq.0.).or.(config_kq.eq.1)) go to 23

        rac = dlam(j)**0.5
        corr = alog(delom/dlam(j))/dd

        23 if (dlam(j)-lim) 21,22,22

        !     FORMULES ASYMPTOTIQUES
        22 if (ind.eq.1) go to 95
        call log_debug('   FORMULES ASYMPTOTIQUES')

        95 afi = 1.

        110 if (config_kq.eq.1) go to 1060

        if (br4.lt.timp) go to 1061

        1060 if (ind.eq.1) go to 100
        call log_debug('   IMPACT   NON')

        !     TESTS POUR TRAITEMENT QUASISTATIQUE
        100 if (dlam(j)-delom) 3004,3001,3001

        1012 alfa(j)=alfa(j)/acte

        2011 if (ind.eq.1) go to 2015      
        call log_debug(' APPROX.QUASISTATIQUE POUR IONS + ELECTRONS')

        2015 ci = 2.*s1
        go to 27

        2014 if (ind.eq.1) go to 2013
        call log_debug('   APPROXIMATION   QUASISTATIQUE POUR IONS SEULS')

        2013 ci = s1
        go to 27

        !     IMPACT VALABLE SELON HVR (CR 259,3979,1964)
        1061 if (ind.eq.1) go to 96
        call log_debug('   IMPACT   OUI')

        96 if ((ceg.ge.0.).and.(ens.ge.0.)) go to 1018

        if (ind.eq.1) go to 97
        call log_debug('   DEGENERESCENCE')

        97 if (dlam(j)-delom) 2010,2010,1012

        2010 go to(52,53),iz

        52 if (dlam(j)-delp)1019,1020,1020

        1020 if (ind.eq.1) go to 81
        call log_debug('   LEWIS')

        81 ci= s1*(1.+(dmoin+rnt*corr)*rac)
        go to 27

        1019 if (ind.eq.1) go to 82
        call log_debug('   GRIEM 2 , 26')

        82 ci= s1*(1.+(dmoin+rnt)*rac)*afi
        go to 27

        1018 if (ind.eq.1) go to 100
        call log_debug('   DEGENERESCENCE  NON  VALABLE')
        go to 100

        3001 alfa(j)=alfa(j)/acte
        qbeta = beta/acte
        if (qbeta.gt.20.) go to 2011

        qrsurl = rsurl*bcte
        rvar = qrsurl
        ig = 1
        go to 3003

        3004 if (dlam(j)-delp) 3002,3005,3005

        3002 qbeta = beta
        if (qbeta.gt.20.) go to 2014

        rvar = rsurl
        ig = 3
        go to 3003

        3005 corr1 = 2.+alog10(dlam(j)/delom)/dd1

        !     NE=NE*CORR1(SELON SCHLUTER ET AVILA APJ 144,785,1966)
        corr2 = corr1**0.6666667
        alfa(j)=alfa(j)/corr2
        qbeta = beta/corr2
        if (qbeta.gt.20.) go to 2015

        rvar = rsurl*corr1**0.1666667
        ig = 2
        3003 iy = 1
        go to 1026

        !     FORMULES NON ASYMPTOTIQUES
        21 iz = 2
        if (config_kq.eq.1) go to 100
        
        go to 110

        53 if (na.eq.3) go to 84

        if (dlam(j)-delp)1028,1029,1029

        1029 if (ind.eq.1) go to 83
        call log_debug('   LEWIS')

        83 gam = gam1*corr+gams
        go to 1031

        1028 if (ind.eq.1) go to 84
        call log_debug('   GRIEM   SIMPLE')

        84 gam = gam1+gams

        1031 if (beta-15.)1034,1033,1033

        1033 if (ind.eq.1) go to 2012
        call log_debug('   BETA.GT.15')
)
        2012 afi = lim/dlam(j)
        go to 52

        1034 if (ind.eq.1) go to 86
        call log_debug('   BETA.LT.15')

        86 if (gam-10.)1039,1038,1038

        1038 if (ind.eq.1) go to 87
        write(lll,'(''   GAMMA.GT.10 GAMMA='',E17.7)') gam
        call log_debug(lll)

        87 t = gam/(pi*(beta**2+gam**2))
        go to 1036

        1039 if (ind.eq.1) go to 88
        write(6,'(''   GAMMA.LT.10 GAMMA='',E17.7)') gam
        call log_debug(lll)

        !     INTERPOLATION DANS T1 OU T2 POUR CONSTRUCTION DU PROFIL QUASISTAT.
        !     (VARIABLE RSURL)
        88 iy = 2
        rvar = rsurl
        1026 if (rvar .gt.0.8) go to 1025

        if (kp.eq.1) go to 18

        kp = 1
        do 90 il = 1,20
          if ((na.eq.2).and.((nb.eq.3).or.(nb.eq.4))) go to 92
          
          do 91 m = 1,5
            91 q(m)=t2(il,m)
          go to 90

          92 do 93 m = 1,5
            93 q(m)=t1(il,m)

          90 res(il)=ft(rvar,5,val,q)

        18 go to (1090,1091), iy

        !     CALCUL DE T(BETA,GAM)
        1091 call malt(res,u,beta,gam,t)
        go to 1036

        !     INTERPOLATION DANS TABLE DU PROFIL QUASIST. (VARIABLE BETA)
        1090 tv = ft(qbeta,20,u,res)
        go to (6001,6002,6003), ig

        6001 t = tv/acte
        go to 1094

        6002 t = tv/corr2
        go to 1094

        6003 t = tv

        1094 if (ind.eq.1) go to 1036
        call log_debug('   DISTR. MOZER BARANGER')

        1036 ci = t/cab

        27 l(j,i)=fac*ci
        if (ind.eq.1) go to 1010
        ! WRITE (6,1043) J,DLAM(J),ALFA(J),BETA,L(J,I)
      1010 continue

      if ((ll.eq.1).or.(j1.eq.1)) go to 17
      
      !     CALCUL DE LA DEMI LARGEUR STARK DS
      do 1013 j = 1,jmax
        lv(j)=l(j,i)
        l1 = jmax-j+1
        vx(l1)=alfa(j)
        1013 al(l1)=alog10(lv(j))
      if (.not.config_amores) go to 4013
      call pronor(jmax,d2%iqm,ij,lv,alfa,fac,ax)
      do 4006 j = 1,jmax
      lv(j)=lv(j)*ax
      4006 bidon(j,i)=lv(j)
      if (ind.eq.0) then
        write(lll,'(/,6X,'' AX='',1PE13.5)') ax
        call log_debug(lll)
      end if

      4013 lac = l(1,i)/2.
      truc = alog10(lac)
      j = 2
      
      1053 if (lac-l(j,i)) 1050,1051,1052
      
      1050 j = j+1
      go to 1053
      
      1051 ds = alfa(j)
      go to 1057
      
      1052 if (j.eq.2) go to 6000
      
      ds = ft(truc,jmax,al,vx)
      go to 1057
      
      6000 ds = alfa(2)+(alfa(2)-alfa(1))*(truc-al(jmax-1))/(al(jmax-1)-al(jmax))
      
      1057 if (.not.d3_stark) go to 71
      if (ind.eq.0) then
        write(6,'(/,''   DEMI LARGEUR DU PROFIL STARK  DS='',E17.7)') ds
        call log_debug(lll)
      end if

      !     CONVOLUTION DES PROFILS STARK ET DOPPLER
      if (ind.eq.1) go to 71
      write(lll, '(''1'',///''     RESULTATS DU PRODUIT DE CONVOLUTION''//)')
      call log_debug(lll)

      71 continue
      if (config_amores) go to 4100

      if (ds-ddop) 1070,1070,1071

      1070 ib = 2
      go to 1072

      1071 ib = 1

      1072 call pronor(jmax,d2%iqm,ij,lv,alfa,fac,ax)
      if (ind.eq.1) go to 28
      write(lll,'('' FACTEUR DE NORM.='',1P,E15.7)') ax
      call log_debug(lll)
      ! call log_debug('VOUS PASSEZ AU NIVEAU SUIVANT')

      28 do 1006 j = 1,jmax
        lv(j) = lv(j)*ax
        1006 bidon(j,i)=lv(j)

      tempor = alfa(jmax)-0.7071068*alfad
      atest = 4.*aldp
      do 1011 j = 1,jmax
        if (alfa(j) .ge. atest) ib = 1
        go to (1073,1074), ib

        1073 if (tempor-alfa(j)) 1011,1011,1048

        1048 call conf(dlam(j),dld,jmax,ris,lv,dlam)
        go to 1075

        1074 call conv2(dlam(j),dld,jmax,d2%iqm,ij,ris,lv,dlam)

        1075 l(j,i)=ris
        if (ind.eq.1) go to 1011
        if (ib.eq.1) go to 1080
        write(6,'(''   CONV2='',1P,E16.5)')l(j,i)
        call log_debug(lll)
        go to 1011
        1080 write(lll,'(''   CONF ='',1P,E16.5)') l(j,i)
        call log_debug(lll)
      1011 continue
      go to 17

      ! CALCUL POUR UN NIVEAU DU MODELE DE LA TABLE:PHI(A,V) DANS LAQUELLE ON
      ! ERA POUR CHAQUE POINT DU PROFIL
      ! ON RESSERRE LA TABLE DE PHI,POUR V COMPRIS ENTRE 1 ET 4,SOIT modif_var COMPR
      ! ENTRE DLD/2 ET 2*DLD
      4100 call ft2_hydro2(jmax,dlam,lv,modif_ih,modif_var,modif_f1)
      ! call log_debug(' VOUS PASSEZ AU NIVEAU SUIVANT')
      b1 = dld/2
      b2 = 2*dld
      cty = 2./dld
      do 229 ik = 1,modif_ih
        if (modif_var(ik).lt.b1) go to 219

        ikd = ik-1
        go to 228

        219 modif_v(ik)=modif_var(ik)*cty
      229 continue

      228 do 227 ik = ikd,modif_ih
        if (modif_var(ik).lt.b2) go to 227
        ikf = ik
        go to 226
      227 continue

      226 ikt = 4*(ikf-ikd)
      pak = (modif_var(ikf)-modif_var(ikd))/float(ikt)
      pak = pak*cty
      do 225 ik = 1,ikt
        modif_ii = ik+ikd
        modif_v(modif_ii)=modif_v(ikd)+pak*float(ik)
      225 continue
      ikf = ikf+1
      do 218 ik = ikf,modif_ih
        modif_ii = modif_ii+1
        modif_v(modif_ii)=cty*modif_var(ik)
      218 continue
      modif_ii = modif_ii+1
      modif_v(modif_ii)=modif_v(modif_ii-1)+5*cty
      call hjen(az(i),modif_v,dld,modif_phi,modif_ii)
      call conv4(dlam,dld,resc,jmax)
      do 1017 k = 1,jmax
        l(k,i)=resc(k)
      1017 continue
    17 continue ! end of loop opened 400 lines above!

    call log_debug(,*)'LE DERNIER NIVEAU DU MODELE EST ATTEINT'
    if (ind .eq. 0)   then
      if (.not. d3_stark) go to 4011
      write(lll,'(''1'',''STARK='',//)')
      call log_debug(lll)
      do 4010 i = 1,imax,5
        write(6,'('' COUCHE'',I4)') i
        call log_debug(lll)
        write(6,'(8X,10E12.5)') (bidon(k,i),k=1,jmax)
        call log_debug(lll)
      4010 continue
      4011 continue
    end if
    
    
    if (j1.ne.2) go to 1027
    if (k1.eq.1) go to 1027

    !     SEQUENCE POUR CALCUL DE BLEND
    do 54 i = 1,imax
      54 stoc(i)=stoc(i)+l(1,i)
  
    !***************************
    !  IMPRESSIONS SUPPLEMENTAIRES
    write(6,'(5X,''NA='',I5,5X,''NB='',I5)')na,nb
    call log_debug(lll)
    write(6,'(8(1P,E15.7))')(l(1,i),i=1,imax)
    call log_debug(lll)

    !***************************
    !***************************
    k = k+1
    if (k1-k)48,49,49
    49 if (ik1.eq.1) go to 45
    if ((ik1.eq.2).and.(ibou.eq.1)) go to 46
    if (ik1.eq.2) go to 3
    if (ik1.eq.3) go to 47
    go to 1027
    45 nb = nb-1
    go to 16
    47 nb = nb+1
    go to 16
    46 if (nbmax -nb)58,58,59
    59 nb = nb+1
    go to 16
    58 if (ibou.ne.1) go to 3
    nb = nb1-1
    ibou = ibou+1
    go to 16
    3 nb = nb-1
    go to 16
    48 do 4 i = 1,imax
    4 l(1,i)=stoc(i)

    !***************************
    !  IMPRESSIONS SUPPLEMENTAIRES
    write(lll,'(//10X,''COEF. SOMME (BLEND)'')')
    call log_debug(lll)
    write(6,'(8(1P,E15.7))')(L(1,I),I=1,IMAX)
    call log_debug(lll)

    !***************************
    go to 1027
    1025 write(lll,1046) I,J
    1046 format('      RZERO/LAMBDA SUP. A 0.8     I=',I4,'     J=',I4,'ON SORT DU SSP'/)
    call log_debug(lll)

    1027 return
  end

================================================================================================================
================================================================================================================
================================================================================================================
================================================================================================================
================================================================================================================


  !> CALCUL DE LA PROFONDEUR OPTIQUE SELECTIVE TAU PAR INTEGRATION
  !> EXPONENTIELLE.modeles_teta=5040./T,pe_log=LOG10PE,modeles_nh=VARIABLE DE PROFONDEUR
  !> DANS LE MODELE,LAMB=LONGUEUR D'ONDE
  !>
  give attention below
  !> @todo check nmax, mmax, may be tied with other values, e.g. tab(9,7)

  subroutine ameru (modeles_teta,pe_log,ppg,modeles_nh,lamb,pds, &
   x,imax,jmax,al,bpl,tau,ix)
    real*4 modeles_nh,lamb
    dimension p(0:99), tau(50,0:99), bpl(0:99)
    dimension modeles_teta(99),pe_log(99),al(50,99),modeles_nh(99),dnh(99),ppg(99)
    dimension tet(7),alp(9),tab(9,7)
    dimension y(99)  

    ! tab=log 10(u), u=fonction de partition de h
    data alp/5.,4.,3.,2.,1.,0.,-1.,-2.,-3./,mmax,nmax/9,7/,tet/0.1,0.2,0.3,0.4,0.5,0.6,0.7/
    data tab/1.43,1.83,2.28,2.77,3.27,3.77,4.27,4.77,5.27,   & 
     0.47,0.62,0.91,1.31,1.78,2.26,2.75,3.25,3.76, &
     0.31,0.32,0.35,0.42,0.61,0.93,1.35,1.82,2.32, &
     0.30,0.30,0.30,0.31,0.32,0.35,0.44,0.65,0.99, &
     0.30,0.30,0.30,0.30,0.30,0.30,0.31,0.32,0.36, &
     18*0.30/
    data cte,c1/3.972257e+8,2.8546e+4/

    !
    ! CALCUL DU COEFFICIENT D'ABSORPTION PAR NOYAU DE H
    !
    if (ix.eq.2) go to 4000

    write(lll,*)'1'
    call log_debug(lll)
    write(lll,*)'      COEFFICIENTS D ABSORPTION PAR NOYAU D''H'
    call log_debug(lll)
    write(lll6,116)lamb,jmax
    format(3x,'lambda=  ',f10.3,10x,'jmax=  ',i5)
    call log_debug(lll)

    !***************************
    4000 continue
    do 1000 i = 1,imax
      pe = exp(2.302585*pe_log(i))
      temp = 2.5*alog(5040.39/modeles_teta(i))
      zkh = exp(-31.30364 *modeles_teta(i)+temp-1.098794 )
      f1 = 1./(1.+zkh/pe)
      f2 = exp(-c1*modeles_teta(i)/lamb)
      f3 = 1.-f2

      if ((modeles_teta(i).ge.0.7).or.(pe_log(i).le.-3.)) go to 5
      
      uv = pipe(modeles_teta(i),pe_log(i),tet,alp,tab,nmax,mmax)
      u = exp(2.302585 *uv)
      go to 6

      5 u = 2.

      6 f = pds*f1*f3/u*10.**(-x*modeles_teta(i))

      bpl(i)=cte*f2/(f3*lamb**3)
      
      do j = 1,jmax
        al(j,i)=f*al(j,i)
      end do

      if (ix.eq.2) go to 1000


      write(lll,110)i,f,u
      110 format(3x,'i= ',i4,3x,'n2/nhtot=',1p,e13.5,3x,'u=',1p,e13.5)
      call log_debug(lll)
      write(lll,'(5E15.7)')(al(i,j),j = 1,jmax)
      call log_debug(lll)

    1000 continue

    write(lll,*)' Calcul de la fonction de Plank au niveau zero'
    call log_debug(lll)

    tet0 = fteta0(ppg,modeles_teta)   ! on extrapole modeles_teta pour modeles_nh=0
    f20 = exp(-c1*tet0/lamb)
    f30 = 1-f20
    bpl(0) = cte*f20/(f30*lamb**3)

    !
    ! CALCUL DE TAU
    p(0)=0
    do  j = 1,jmax
      do i = 1,imax
        y(i)=al(j,i)
      end do
      p(1)=modeles_nh(1)*(y(1)-(y(2)-y(1)) / (modeles_nh(2)-modeles_nh(1))*modeles_nh(1)/2.)
      call integra(modeles_nh,y,p,imax,p(1))
      do i = 0,imax
        tau(j,i)=p(i)*1.e-24
      end do
    end do
    write(lll,*) LEAVING, '   Sortie de AMERU'
    call log_debug(lll)
  end


  !> PRODUIT DE CONVOLUTION EFFECTUE PAR GAUSS HERMITE (N=2)

  subroutine conf(x,y,jmax,res,tab,ab)
    integer*2 ix
    dimension tab(50),ab(50)  
    real*8 :: v, h
    data v, h /.7071068, .8862269/

    res = 0.
    q = 1.
    ix = 1
    2002 continue
    arg = x-v*y*q
    avu = abs(arg)

    if (avu .gt. ab(jmax)) go to 10

    to_ = ft(avu,jmax,ab,tab)
    res = res+to_*h
    go to (2003,2004), ix

    2003 continue
    q = -1.
    ix = 2
    go to 2002

    2004 continue
    res = res/rpi
    go to 11

    10 continue
    call pfant_halt('conf() says: "impossible"')  ! note: wasn't halting before 2015+
  end


  !> PRODUIT DE CONVOLUTION POUR LE CAS OU LE PROFIL STARK VARIE PLUS
  !> VITE QUE LE PROFIL DOPPLER. INTEGRATION PAR SIMPSON

  subroutine conv2(x,y,jm,d2%iqm,ij,res,t,ab)
    integer*2 ir
    dimension ij(10),f(50),ab(50),h(10),v(50),t(50),ac(50)
    bol = x/y
    h(1) = ab(2)
    do 10 i = 2,d2%iqm
       i1 = ij(i-1)
       10 h(i)=ab(i1+1)-ab(i1)

    do 11 n = 1,jm
      11 v(n)=ab(n)/y

    q = 1.
    ir = 1

    40 do 15 n = 1,jm
      ac(n)=-(bol-q*v(n))**2
      15 f(n)=t(n)*exp(ac(n))

    som = 0.
    do 12 i = 1,d2%iqm
      if (i.gt.1) go to 20
      k1 = 1
      k2 = ij(1)
      go to 21
      20 k1 = ij(i-1)
      k2 = ij(i)
      21 k3 = k1+1
      k4 = k2-3
      sig = f(k1)+f(k2)+4.*f(k2-1)
      if (k4.lt.k3) go to 16
      do 13 k = k3,k4,2
      13 sig = sig+4.*f(k)+2.*f(k+1)
      16 s = sig*h(i)/3.
      12 som = som+s

    go to(60,61),ir
    60 s1 = som
    q=-1.
    ir = 2
    go to 40
    61 s2 = som
    res = (s1+s2)/(rpi*y)
  end


  !> CONVOLUTION AVEC H%A,V<,INTEGRATION PAR SIMPSON

  subroutine conv4(x,y,tab,jmax)
    dimension n(6),pas(6)
    dimension x(50),tab(50),ac(220),phit(220)
    data n/11,21,23,171,191,211/
    data pas/0.001,0.01,0.045,0.1,0.25,0.5/

    epsi = 1.e-06
    do 1017 kk = 1,jmax
      bol = x(kk)/y
      ir = 1
      q = 1.
      qy = q/y
      25 res = 0.
      resg = 1.
      do 50 i = 1,modif_ih
        ac(i)=abs(bol-qy*modif_var(i))
      50 continue

      call ft2_hydro2(modif_ii,modif_v,modif_phi,modif_ih,ac,phit)

      i = 1
      k = 1
      som = modif_f1(1)*phit(1)
      
      do 10 i = 2,modif_ih,2
        ff4 = 4*modif_f1(i)*phit(i)
        j = i+1
        if ((i+1).eq.n(k)) go to 15
        ff2 = 2*modif_f1(i+1)*phit(i+1)
        som = som+ff4+ff2
        go to 10

        15 ff1 = modif_f1(i+1)*phit(i+1)
        som = som+ff4+ff1
        res = res+som*pas(k)/3
        if (abs(1.-res/resg).lt.epsi) go to 45
        som = ff1
        resg = res
        k = k+1
      10 continue

      45 go to (20,35),ir

      20 if (x(kk).eq.0.) go to 30
      
      qy=-qy
      res1 = res
      ir = 2
      go to 25

      35 res = res1+res
      tab(kk)=res
      go to 1017

      30 res = 2.*res
      tab(kk)=res
    1017 continue
  end


  subroutine fluxis (t1,t2,b,modeles_ntot,ptdisk,mu,jm,f,fc,iop)
  !> ***
    real mu
    logical ptdisk
    dimension t1(50,0:99),t2(0:99),b(0:99),t(0:99)
    dimension f(50),cc(26),tt(26),tta(6),cca(6),ttb(26),ccb(26), &
     ttp(7),ccp(7)
    DATA CCA/0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
    DATA TTA /0.038,0.154,0.335,0.793,1.467,3.890 /
    DATA CCP/0.176273,0.153405,0.167016,0.135428,0.210244,0.107848, 0.049787/
    DATA TTP/0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
    DATA CCB/0.032517,0.111077,0.071279,0.154237,0.076944,0.143783, &
     0.063174,0.108330,0.038767,0.059794,0.021983,0.034293,0.012815, &
     0.020169,0.007616,0.012060,0.004595,0.007308,0.002802,0.004473, &
     0.001724,0.002761,0.001578,0.002757,0.000396,0.002768/
    DATA TTB/0.,0.05,0.1,0.20,0.30,0.45,0.60,0.80,1.,1.2,1.4,1.6,1.8, &
     12.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
    if (ptdisk) then
      ipoint = 7
      do i = 1,ipoint
        cc(i)=ccp(i)
        tt(i)=ttp(i)*mu
        end do
    else
      if (iop.eq.0) then
        ipoint = 6
        do i = 1,ipoint
          cc(i)=cca(i)
          tt(i)=tta(i)
        end do
      else
        ipoint = 26
        do i = 1,ipoint
          cc(i)=ccb(i)
          tt(i)=ttb(i)
        end do
      end if
    end if

    tolim = tt(ipoint)
    if (t2(modeles_ntot).lt.tolim) then
      call log_halt(' Modele trop court ')
      write(lll,103) modeles_ntot,t2(modeles_ntot))
      103 format(i10,5x,'modeles_t5l=',f10.4)
      call pfant_halt(lll)
    end if
    !
    fc = 0
    do k = 1,ipoint
      bbc = faitk30(tt(k),t2,b,modeles_ntot)
      fc = fc+cc(k)*bbc
    end do

    do j = 1,jm
      fl = 0
      do i = 0,modeles_ntot
        t(i)=t1(j,i)+t2(i)
      end do

      do k = 1,ipoint
        bb = faitk30(tt(k),t,b,modeles_ntot)
        fl = fl+cc(k)*bb
      end do
      f(j)=fl
    end do
  end


  !>  CALCUL DE LA FONCTION DE HJERTING = H(A,V)

  subroutine hjen(a,vh,del,modif_phi,modif_ii)
    dimension h11(41),h12(43), v1(43),v2(43)
    dimension vh(300)
    data H11/-1.128380,-1.105960,-1.040480,-0.937030,-0.803460, &
             -0.649450,-0.485520,-0.321920,-0.167720,-0.030120, &
             +0.085940,+0.177890, 0.245370, 0.289810, 0.313940, &
              0.321300, 0.315730, 0.300940, 0.280270, 0.256480, &
              0.231726, 0.207528, 0.184882, 0.164341, 0.146128, &
              0.130236, 0.116515, 0.104739, 0.094653, 0.086005, &
              0.078565, 0.072129, 0.066526, 0.061615, 0.057281, &
              0.053430, 0.049988, 0.046894, 0.044098, 0.041561, &
              0.039250/
    data H12/0.0440980,0.0392500,0.0351950,0.0317620,0.0288240, &
             0.0262880,0.0240810,0.0221460,0.0204410,0.0189290, &
             0.0175820,0.0163750,0.0152910,0.0143120,0.0134260, &
             0.0126200,0.0118860,0.0112145,0.0105990,0.0100332, &
             0.0095119,0.0090306,0.0085852,0.0081722,0.0077885, &
             0.0074314,0.0070985,0.0067875,0.0064967,0.0062243, &
             0.0059688,0.0057287,0.0055030,0.0052903,0.0050898, &
             0.0049006,0.0047217,0.0045526,0.0043924,0.0042405, &
             0.0040964,0.0039595,0.0038308/

    ! SI V>3.9 LE CALCUL DE EXP(-V**2) EST INUTILE
    do 100 k = 1,modif_ii
      v = vh(k)
      if (v-12)1,1,2
      
      2 w = 1./(2*v**2)
      h1 = 2.*w*(1.+w*(3.+15.*w*(1.+7.*w)))
      h1 = h1/rpi
      go to 10
      
      1 v1(1)=0.
      v2(1)=03.8
      
      do 5 i = 1,42
        v1(i+1)=v1(i)+0.1
        v2(i+1)=v2(i)+0.2
      5 continue

      if (v-3.9)3,3,4
      
      4 h1 = ft(v,43,v2,h12)
      
      10 modif_phi(k) = (a*h1)/(rpi*del)
      go to 100
      
      3 h1 = ft(v,41,v1,h11)
      modif_phi(k) = (exp(-v**2)+a*h1)/(rpi*del)
    100 continue
  END


  !> CE PROGRAMME INTEGRE NUMERIQUEMENT UNE FONCTION RELLE, DONNEE PAR
  !> UN TABLEAU DE VALEURS X(I),Y(I).
  !>
  !> LA METHODE CONSISTE A CALCULER
  !> L' INTEGRALE DEFINIE SUR CHAQUE INTERVALLE (X(I),X(I+1)) D'ABORD
  !> PAR SIMPSON,PUIS PAR GAUSS A DEUX POINTS. LES VALEURS DE Y AU
  !> POINT MILIEU ET AU DEUX POINTS DE GAUSS SONT CALCULES PAR INTER-
  !> POLATION CUBIQUE A QUATRE POINTS EN UTILISANT LES VALEURS DE LA
  !> TABLE.ON FORME ENSUITE UNE MOYENNE PONDEREE DES DEUX RESULTATS
  !> QUI ANNULE L'ERREUR DU QUATRIEME (ET AUUSI DU CINQUIEME PAR RAI-
  !> SON DE PARITE) ORDRE.L' ERREUR SUR L'INTEGRATION EST DONC
  !> GENERALEMENT NEGLIGEABLE PAR RAPPORT A L'ERREUR SUR L'INTERPOLA-
  !> TION DU TROSIEME ORDRE.CETTE DERNIERE EST EVALUEE EN COMPARANT
  !> LA VALEUR INTERPOLEE SUR LA FONCTION ELLE MEME ET SUR LA VALEUR
  !> INTERPOLEE EN PASSANT PAR SON LOGARITHME.IL FAUT QUE LA FONCTION
  !> SOIT POSITIVE PAR NATURE POUR QUE LA TRANSFORMATION LOGARITHMI-
  !> QUE SOIT POSSIBLE.

  subroutine inait(x,y,p,err,n)
    real*8 :: dimension x(99),y(99),aly(200),p(99),err(99)
    real*8 aly(200), xmilieu, xgauss1, xgauss2, del, ym, yg1, yg2, const
    integer i, j

    p(1)=0.
    const = 1./sqrt(3.)
    do i = 1,n
      aly(i)=alog(y(i))
    end do
    i = 1
    j = 1
    call step()

    n2 = n-2
    do i = 2,n2
      j = i-1
      call step()

      k = i+1
    end do
    i = n-1
    j = n-3
    call step()

  contains
      subroutine step()
        real*8 :: out_

        xmilieu = (x(i)+x(i+1))/2.
        del = x(i+1)-x(i)
        xgauss1 = xmilieu-del*const/2.
        xgauss2 = xmilieu+del*const/2.
        call naitk3(x(j),x(j+1),x(j+2),x(j+3), &
         aly(j),aly(j+1),aly(j+2),aly(j+3),xmilieu,out_)
        yml = exp(out_)

        call naitk3(x(j),x(j+1),x(j+2),x(j+3), &
         aly(j),aly(j+1),aly(j+2),aly(j+3),xgauss1,out_)
        yg1l = exp(out_)

        call naitk3(x(j),x(j+1),x(j+2),x(j+3), &
         aly(j),aly(j+1),aly(j+2),aly(j+3),xgauss2,out_)
        yg2l = exp(out_)

        call naitk3(x(j),x(j+1),x(j+2),x(j+3), &
         y(j),y(j+1),y(j+2),y(j+3),xmilieu,ym)
        call naitk3(x(j),x(j+1),x(j+2),x(j+3), &
         y(j),y(j+1),y(j+2),y(j+3),xgauss1,yg1)
        call naitk3(x(j),x(j+1),x(j+2),x(j+3), &
         y(j),y(j+1),y(j+2),y(j+3),xgauss2,yg2)
        amys = (y(i)+4.*ym+y(i+1))/6.
        amyg = (yg1+yg2)/2.
        amy = 0.6*amyg+0.4*amys
        amyls = (y(i)+4.*yml+y(i+1))/6.
        amylg = (yg1l+yg2l)/2.
        amyl = 0.6*amylg+0.4*amyls
        err(i+1) = (amyl-amy)/amy
        amyruse = 0.0*amy+1.0*amyl
        p(i+1) = p(i)+del*amyruse
      end
  end


  !> INTERPOLATION DANS UNE TABLE A PAS CONSTANT

  subroutine inpol1(x,xa,dx,n,yt,y)
    dimension yt(1)

    i = 1
    1 if (x-xa-dx*float(i-1)) 4,3,2

    2 if (i.ge.n) go to 5
    i = i+1
    go to 1

    3 y = yt(i)
    return

    4 if (i.eq.1) i = 2

    5 if (i.ge.n) i = n-1   
    a = x-xa-dx*float(i-2)
    b = a-dx
    c = b-dx
    y = (b*(a*yt(i+1) + c*yt(i-1))/2. - a*c*yt(i) )/dx**2
  end

  subroutine malt(th,u,beta,gam,t)
    dimension u(20),th(20),asm(150)
    DATA PI/3.141593/, IBORN/20/

    t1 = ta(1.,beta,gam)
    t2 = ta(-1.,beta,gam)
    vu = 0.
    sigma = 0.
    avu = vu
    to_ = ft(avu,iborn,u,th)
    aso = as(gam,beta,0.,to_)
    pp = beta+gam

    if (pp-20.) 48,47,47

    47 h1 = pp/100.
    h2 = 0.
    go to 606

    48 if (pp-15.)50,49,49
    
    49 ih1 = 100
    ih2 = 10
    go to 55
    
    50 if (pp-10.)52,51,51
    
    51 ih1 = 76
    ih2 = 20
    go to 55
    
    52 if (pp-5.)54,53,53
    
    53 ih1 = 50
    ih2 = 30
    go to 55
    
    54 ih1 = 30
    ih2 = 40
    
    55 h1 = pp/ih1
    h2 = (20.-pp)/ih2
    
    606 ah = h1
    in = 1
    im = ih1-1
    ir = ih1
    
    59 do 56 i = in,ir
      vu = vu+ah
      avu = abs(vu)
      to_ = ft(avu,iborn,u,th)
      56 asm(i)=as(gam,beta,vu,to_)

    do 57 i = in,im,2
      sig = (1.333333*asm(i)+0.6666667*asm(i+1))*abs(ah)
      57 sigma = sig+sigma

    if (in-1)81,81,58

    81 if (pp-20.)60,58,58

    60 sigma = asm(ir)*(h2-h1)/3.+sigma
    ah = h2
    in = ih1+1
    im = ih2+ih1-1
    ir = im+1
    go to 59

    58 sigma = sigma-asm(ir)*abs(ah)/3.
    if (ah) 63,62,62

    62 sigma1 = sigma
    vu = 0.
    sigma = 0.
    im = 40
    in = 2
    ir = im+1
    ah=-0.5
    go to 59

    63 tb = (sigma+sigma1+aso*(h1+0.5)/3.)/pi
    t = t1+t2+tb
  end


  !> CALCUL DE LA PROFONDEUR OPTIQUE TAUC.FORMULES VOIR THESE CAYREL.
  !> modeles_nh VARIABLE DE PROFONDEUR DANS LE MODELE.KAP COEFFT D'ABSORPTION
  !> PAR NOYAU D'HYDROGENE.

  subroutine optic1(kap,modeles_nh,modeles_ntot,tauc)
    real modeles_nh,kap
    dimension tauc(0:99)
    dimension modeles_nh(99),kap(99)
    tauc(0)=0.
    tauc(1)=modeles_nh(1)*(kap(1) - (kap(2)-kap(1))/(modeles_nh(2)-modeles_nh(1))*modeles_nh(1)/2.)
    call integra(modeles_nh,kap,tauc,modeles_ntot,tauc(1))
  end


  !> NORMALISATION DU PROFIL DE STARK,INTEGRATION PAR SIMPSON

  subroutine pronor(jm,d2%iqm,ij,f,ab,z,ax)
    dimension ij(10),ab(50),h(10),f(50)
    h(1)=ab(2)
    do10i = 2,d2%iqm
    i1 = ij(i-1)

    10 h(i)=ab(i1+1)-ab(i1)
    som = 0.

    do 12 i = 1,d2%iqm
      if (i.gt.1) go to 20
      k1 = 1
      k2 = ij(1)
      go to 21

      20 k1 = ij(i-1)
      k2 = ij(i)

      21 k3 = k1+1
      k4 = k2-3
      sig = f(k1)+f(k2)+4.*f(k2-1)

      if (k4.lt.k3) go to 16

      do 13 k = k3,k4,2
        13 sig = sig+4.*f(k)+2.*f(k+1)

      16 s = sig*h(i)/3.

      12 som = som+s

    anor = 2.*som
    ax = z/anor
  end


  function as(gam,beta,vu,to_)
    as = to_*gam/(gam**2+(beta-vu)**2)
  end

  !> INTERPOLATION DANS UN TABLEAU A DOUBLE ENTREE NOTE TAB
  !> XX=TABLE DE LA VARIABLE LIGNE.  YY=TABLE DE LA VARIABLE COLONNE.
  !> RESULTAT=VALEUR DE LA FONCTION POUR LES ENTREES X ET Y

  function pipe(x,y,xx,yy,tab,nmax,mmax)
    integer*2 j
    real mu,nu
    dimension u(3),tab(mmax,nmax),xx(nmax),yy(mmax)
    n = 1
    80 if (x-xx (n))72,71,70

    70 n = n+1
    go to 80

    71 nn = n
    go to 90

    72 if (nmax-n)74,74,73

    73 nn = n-1
    go to 90

    74 nn = n-2

    90 m = 1

    91 if (yy (m)-y) 92,93,94

    93 mm = m
    go to 95

    94 m = m+1
    go to 91

    92 if (mmax-m)97,97,96

    96 mm = m-1
    go to 95

    97 mm = m-2

    95 s0 = y-yy (mm)
    phi = s0*(y-yy (mm+1))/(yy (mm+2)-yy (mm))
    inn = nn+2
    j = 0
    do 200 nc = nn,inn
      j = j+1
      mu = (tab  (mm+1,nc)-tab  (mm,nc))/(yy (mm+1)-yy (mm))
      nu = (tab  (mm+2,nc)-tab  (mm+1,nc))/(yy (mm+2)-yy (mm+1))
      200 u(j)=tab(mm,nc)+s0*mu+phi*(nu-mu)

    s0 = x-xx (nn)
    phi = s0*(x-xx (nn+1))/(xx (nn+2)-xx (nn))
    mu = (u(2)-u(1))/(xx (nn+1)-xx (nn))
    nu = (u(3)-u(2))/(xx (nn+2)-xx(nn+1))
    pipe = u(1)+s0*mu+phi*(nu-mu)
  end


  function ta(x,beta,gam)
    bb = 2.*beta*x
    aa = beta**2+gam**2
    q = sqrt(aa)
    rm = sqrt(2.*q-bb)
    en = sqrt(2.*q+bb)
    bca = bb**2
    aca = aa**2
    rca = sqrt(20.)
    fac = (bca-aa)/(aca*q)
    phi = bb/(4.*aca*rm)-fac/(4.*rm)
    ta = (3.*gam/pi)*((-bb/aa+.1666667e-1)/(aa*rca)+phi*alog((20.+rca*rm+q)/&
     (20.-rca*rm+q))+(fac/(2.*en))*(pi/2.-atan((20.-q)/(rca*en)))+&
     (bb/(2.*aca*en))*(pi-atan((2.*rca+rm)/en)-atan((2.*rca-rm)/en)))
  end
end



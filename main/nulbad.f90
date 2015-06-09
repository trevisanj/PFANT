! Plan for nulbadgrade
!
! 1) convert to .f90
! 2) format
! 3) make it run with new libraries (I may want PFANT in NULBADGRADE mode)
! 4) separate the calculation from file reading
! 5) prepare to inject PFANT output vectors (I think I will need to update
!    PFANT because it currently doesn't store the whole output in the memory).
! 6) See the dimensions
! 7) Implement command-line options
! 8) Incorporate into PFANT


!  programme NULBT=NULBAD_SQ
!        lecture et convol sortie fant93
!   Programme de lecture des flux en binaire, sortant de
!       Fantomol93 , calculs de 100A en 100A (ou inferieur a 100A)
!
!  Paula, julho de 2003:
!  - modifiquei para nao normalizar o espectro
!  - a opcao CONVOL=F nao estava imprimindo o lambda, corrigi
!  P.Coelho, dez 2003
!  - se NORM = .TRUE. (saida do pfant ja eh normalizada), nao
!  altera o valor do fluxo
!

module nulbad
  subroutine nulbad_()
  
  parameter (NP=2000000,NI=2000000)
  character fileflux*20,flcv*30
  character titc*20
  real*8 lzero,lfin,lambd,l0,lf
  logical convol,flam,norm
  real nhe
  integer d,dmj,dtotc

  dimension ffl(np),fnu(ni),ffnu(np),lambd(np)
  dimension fi(1501),tfi(1501)
  dimension alfl(np),afl(np),fl(np),tl(np)

  norm = .false.

  print *,'Binary file name:'
  read(5,200) fileflux
  ik=1
  open(unit=17,file=fileflux, status='unknown')

  icle=1
  ktot=0

  12 continue


  !!!! --material for a subroutine read_spectrum(fn)
  read(17, 1130) ikeytot,titc,tetaeff,glog,asalog,nhe,amg,&
   l0,lf,lzero,lfin,itot,dpas,echx,echy,fwhm
  1130 format(i5, a20, 5f15.5, 4f10.1, i10, 4f15.5)

  read(17, *)(fnu(d),d=1,itot)

  write(6,*) ikeytot,ik-1,itot
  if(icle.eq.ikeytot) itot=itot+1
  do d=1,itot-1
    k=ktot+d
    ffnu(k)=fnu(d)
  end do
  ktot=k
  icle=icle+1
  if(icle.gt.ikeytot) go to 14
  go to 12

  14 continue
  npt=k



  !!!! it seems that this question doesn't matter if the user answers "no" to flam below
  print *,'Is spectra normalized? (T/F)'
  read(5,*) norm


  lambd(1)=l0
  do k=1,ktot
    lambd(k)=l0 + (k-1)*dpas
  end do


  ! transformation de Fnu en Flambda
  print *,'Fnu to FLambda transformation? (T or F)'
  read(5,*) flam
  if(.not. flam) go to 60

  !!!! transforms the thing
  C = 2.997929E+10
  do k=1,ktot
    ca=1.e+11/(lambd(k)**2)
    !  ffnu(10(-5) etait x 10(5), donc cte=10(11) et pas 10(16)
    cb=ca*c
    if (.not. norm) then
      ffl(k)=ffnu(k)*cb
    else
      ffl(k)=ffnu(k)
    end if
  end do

  write(6,122) dpas,KTOT
  122  format(2x,'pas=',f8.3,2x,'ktot=', i10)

  60 continue

  print *,'ASC file name to create:'
  read(5,200) flcv
  open(unit=8,status='new',file=flcv)

  PRINT *,'Step:'
  read(5,*) pat
  ip=int(pat/dpas)

  PRINT *,' CONVOL (T ou F), FWHM(A)'
  read(5,*) convol,fwhm

  !
  !  Convolution sp synthetique avec profil instrumental
  !
  if(.not.convol) go to 65

  call cafconvh(dpas,fwhm,ift,tfi,fi)
  write(6,101) ift
  j=(ift-1)/2
  jp1=j+1
  dmj=ktot-j
  dtotc=dmj-jp1+1

  m=0
  do d = jp1,dmj,ip
    m=m+1
    tl(m)=lambd(d)
  end do
  kktot=m

  if(flam) then
    do k=1,ktot
      fl(k)=ffl(k)
    end do
  else
    do k=1,ktot
      fl(k)=ffnu(k)
    end do
  end if
  call volut(fl,ktot,fi,j,ip,dpas,alfl)
  k=0
  do i=jp1,dmj,ip
    k=k+1
    afl(k)=alfl(i)
  end do
  kktot=k

  65 continue

  if((.not.convol) .and. (.not.flam)) then
    kktot=ktot
    do k=1,kktot
      afl(k)=ffnu(k)
    end do
  end if

  if((.not.convol) .and. (flam)) then
    kktot=ktot
    do k=1,kktot
      afl(k)=ffl(k)
      tl(k)=lambd(k)
    end do
  end if

  if(convol) then
    alz=tl(1)
    alf=tl(kktot)
  else
    alz=lambd(1)
    alf=lambd(kktot)
  end if

  write(8,201) titc,tetaeff,glog,asalog,amg
  write(8,202) kktot,l0,lf,pat,fwhm
  do k=1,kktot
    write(8,*) tl(k), afl(k)
  end do

  write(6,110) tetaeff,glog,asalog,nhe,amg
  write(6,130) alz,alf,kktot,pat,fwhm
  write(6,120) l0,lf,ktot,dpas
  write(6,240) (afl(k),k=1,12)



  
101  FORMAT(' IFT=',I4)
110  FORMAT(2X,'tetaeff=',F8.3,2X,'log g=',F6.2,2X,'[M/H]=',F6.2, &
   2X,'NHE=',F5.2,2X,'[Mg/Fe]=',F6.3)
120  FORMAT(2X,'Lzero=',F8.3,2x,'Lfin=',F8.2,2x,'KTOT =',I7, &
   2X,'PAS original='F5.2)
130  FORMAT(2X,'Lzero=',F8.3,2x,'Lfin=',F8.2,2x,'KKTOT=',I7, &
   2X,'PAS nouveau =',F5.2,2x,'FWHM=',F5.2)
200     FORMAT(A)
201     FORMAT('#',A,'Tef=',F6.3,X,'log g=',F4.1,X,'[M/H]=',F5.2,X,F5.2)
202     FORMAT('#',I6,2X,'0. 0. 1. 1. Lzero =',F10.2,2x,'Lfin =', &
   F10.2,2X,'PAS =',F5.2,2x,'FWHM =',F5.2)
240  FORMAT(12F6.3)
  STOP
end


  !>  ON CALCULE FI(TT) la fonction de convolution EN IFT PTS
  !>
  !>  LA FONCTION DE CONVOLUTION PEUT AVOIR 1501 PTS
  !>   SI L ON CHANGE CE NBRE DE PTS CHANGER AUSSI IPPTOT=750
  !>   DANS LES DATA QUELQUES LIGNES PLUS BAS.
  !>   (LA MOITIE DU NBRE TOT DE PTS POUR LE CALCUL DES AT)

  subroutine cafconvh(pat,fwhm,ift,tt,fi)
    dimension tt(1501),fi(1501),at(-750:+750)
    real*8, parameter :: C7 = 1.772453
    ipptot=750

    !
    ! GAUSS: PROFIL GAUSSIEN DE 1/2 LARG AA
    sigma=fwhm/2.35482
    aa=1.414214*sigma
    write(6,119) fwhm,sigma,aa
    totlarg=3.0 * aa
    write(6,134)totlarg
    at(0)=0
    do i=1,ipptot   ! IPPTOT TAILLE MAX DE LA FCT DE CONV
      at(i)=pat * i
      at(-i)=-at(i)
      if(at(i).gt.totlarg) go to 40
    end do
    bb=3*aa
    write(6,133) bb
    stop

    40 continue
    ifd=i-1
    ift= 1 + 2*ifd

    if(ift.gt.1501)  then
      write(6,137)
      stop
    end if

    do i=1,ift
      tt(i)=at(i-ifd-1)
    end do

    z = c7*aa

    do  i=1,ift
      fi(i) = exp( -(tt(i)/aa)**2)
    end do

    do  i=1,ift
      fi(i) = fi(i) / z
    end do


  return
119  FORMAT(1X,'Profil instrumental gaussien'/, &
   1X,'FWHM =',F7.3,' (A)'/,1X,'Sigma=',F7.3,' (A)'/, &
   1X,'1/2 Largeur AA =',F7.3,' (A)')
133  FORMAT(' LE PROFIL GAUSSIEN PAR LEQUEL ON CONVOLE NE PEUT', &
  ' AVOIR PLUS DE 121 PTS  (60 DE CHAQUE COTE DU CENTRE)', &
  /' CETTE GAUSSIENNE EST CALCULEE JUSQU A UNE DISTANCE DE', &
  ' 3 DEMI-LARGEUR DU CENTRE', &
  /' SOIT:',F7.3,' ANGSTROM', &
  /'   ELARGISSEZ LE PAS DU CALCUL')
134  FORMAT(' Gaussienne calculee jusqu a une distance du centre:', &
   /,' 3(SIGMA*1.414)=',F7.3,' (A)')
137  FORMAT(5X,'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER A ', &
   /' PLUS DE 500 PTS -CHANGEZ LE PAS-. (LE NBRE DE PTS TOTAL ' &
   /' SUR LA FCTION S OBTIENT EN MULTIPLIANT PAR 6 LE NBRE ' &
   /' DE PTS SUR LA DEMI LARGEUR')
  END

  subroutine volut(s,itot,fi,j,ip,pa,ps)
    dimension ps(itot),s(itot),fi(1501)
    do i=1,itot
      ps(i)=0.
    end do

    jp1 =j+1
    imj=itot-j
    j2p =  2*j +1
    do i = jp1,imj,ip
      do k = 1,j2p
        ps(i) =ps(i)  +  s(i-j+k-1) *fi(k)*pa
      end do
    end do

    jjp1 = jp1+1
    iimj = imj+1
    do i = 1,jjp1
      ps(i) = s(i)
    end do

    do i = iimj,itot
      ps(i)=s(i)
    end do
  end

  !!!! misc_math
  FUNCTION ISUP(FR,ITOT,IA,IZ)
!  UNE FONCTION FR EST CONNUE EN ITOT POINTS. ON CHERCHE ENTRE
!  LES POINTS IA ET IZ QUEL EST L INDICE I OU CETTE FONCTION
!  EST MAXIMUM.
  DIMENSION FR(ITOT)
  IA2=IA+1
  ISUP=IA
  FMAX=FR(IA)
  DO 1 I=IA2,IZ
  IF(FR(I).LT.FMAX)   GO TO 1
  FMAX=FR(I)
  ISUP=I
1  CONTINUE
  RETURN
END

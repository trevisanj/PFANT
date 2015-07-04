module nulbad_calc
  use misc_math
  use logging
  use read_most_files
  use filetoh
  implicit none


  !=====
  ! Module configuration
  !=====
  ! These variables are either set from outside or by subroutine read_spectrum()

  character nulbad_titc*20

  real*8 &
   nulbad_tetaeff, & !<
   nulbad_glog,    & !<
   nulbad_asalog,  & !<
   nulbad_amg,     & !<
   nulbad_l0,      & !<
   nulbad_lf,      & !<
   nulbad_dpas,    & !<
   nulbad_nhe_bid


  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  ! Private variables have "p_" prefix

  ! Attention: MAX_P_IFT *must* be an odd number!
  !> @todo assertion MAX_P_IFT *must* be an odd number!
  integer, private, parameter :: MAX_P_IFT = 1501 !< length of "convolution function"
  integer, private, parameter :: IPPTOT = (MAX_P_IFT-1)/2!< "TAILLE MAX DE LA FCT DE CONV"

  ! Gaussian function variables, calculated by cafconvh()
  real*8, private :: &
   p_tfi(MAX_P_IFT), &  !< x-axis values of gaussian function (calculated by nulbad::cafconvh())
   p_fi(MAX_P_IFT)      !< y-axis values of gaussian function (calculated by nulbad::cafconvh())
  integer, private :: p_ift !< Number of points of gaussian function (calculated by nulbad::cafconvh())

  ! Variables set by read_spectrum()
  real*8, private, allocatable :: p_ffnu(:)
  integer, private :: p_ktot

  integer, private, parameter :: UNIT_=199 !< unit for file I/O

contains
  !=======================================================================================
  !> Reads spectrum file and does the calculation

  subroutine nulbad_complete()
    call read_spectrum()
    call nulbad_calc(p_ffnu, p_ktot)
  end

  !=======================================================================================
  !> Reads spectrum file
  !>
  !> @note FWHM from spectrum file is not used, config_fwhm is used instead!

  subroutine read_spectrum()
    ! variables with suffix "_bid" are read from file, but used for nothing
    real*8 echx_bid, echy_bid, fwhm_bid, lzero_bid, lfin_bid
    integer :: itot, ikeytot, icle, d, k
    real*8 fnu(FILETOH_NP)
    ! character*92  lll

    open(unit=UNIT_,file=full_path_o(config_fileflux), status='unknown')

    icle = 1
    p_ktot = 0

    do while (.true.)
      read(UNIT_, 1130) &
       ikeytot,        & ! used locally
       nulbad_titc,    & ! written back in output file
       nulbad_tetaeff, & ! written back
       nulbad_glog,    & ! written back
       nulbad_asalog,  & ! written back
       nulbad_nhe_bid,        & ! read and used in logging only
       nulbad_amg,     & ! written back
       nulbad_l0,      & ! used by nulbad_calc() & written back
       nulbad_lf,      & ! written back
       lzero_bid,      & ! read and discarded
       lfin_bid,       & ! read and discarded
       itot,           & ! used locally
       nulbad_dpas,    & ! used by nulbad_calc()
       echx_bid,       & ! read and discarded
       echy_bid,       & ! read and discarded
       fwhm_bid          ! read and discarded
      1130 format(i5, a20, 5f15.5, 4f10.1, i10, 4f15.5)

      ! allocates ffnu at first iteration
      if (icle .eq. 1) then
        allocate(p_ffnu(ikeytot*itot)) ! This is probably a slight overallocation
      end if

      read(UNIT_, *)(fnu(d),d=1,itot)

      ! write(lll,*) ikeytot,itot
      ! call log_debug(lll)

      if (icle .eq. ikeytot) itot = itot+1

      !> @todo hmmmm I have to test if I am mounting ffnu in synthesis_() correctly, why itot-1? Perhaps it skips one repeated lambda?? Will have to check this out
      do d = 1,itot-1
        k=p_ktot+d
        p_ffnu(k)=fnu(d)
      end do
      p_ktot = k
      icle = icle+1
      if(icle .gt. ikeytot) exit
    end do
    close(unit=UNIT_)
    !> @todo check if Fortran needs explicit deallocation
  end

  !=======================================================================================


  subroutine nulbad_calc(ffnu, ktot)
    real*8, intent(in) :: ffnu(ktot)  !< Flux Fnu of ktot valid elements
    integer, intent(in) :: ktot       !< Number of valid elements in ffnu(:)
    real*8, parameter :: C = 2.997929E+10
    integer d, dmj, dtotc, k, i, ip, j, jp1, kktot, m
    real*8, dimension(ktot) :: ffl, lambd, alfl, afl, fl, tl
    real*8 alf, alz, ca, cb
    character*92 lll

    ! Note: will now replace output file if already existent
    open(unit=UNIT_,status='replace',file=full_path_o(config_flcv))

    do k = 1, ktot
      lambd(k) = nulbad_l0+(k-1)*nulbad_dpas
    end do

    ! transformation de Fnu en Flambda
    if(config_flam) then
      do k = 1, ktot
        if (.not. config_norm) then
          ca = 1.e+11/(lambd(k)**2)
          !  ffnu(10(-5) etait x 10(5), donc cte=10(11) et pas 10(16)
          cb = ca*c
          ffl(k) = ffnu(k)*cb
        else
          ffl(k) = ffnu(k)
        end if
      end do

      write(lll,122) nulbad_dpas, ktot
      122  format(2x,'pas=',f8.3,2x,'ktot=', i10)
      call log_debug(lll)
    end if

    ip = int(config_pat/nulbad_dpas)

    if (ip .lt. 1) then
      call log_warning('New step ('//real82str(config_pat)//&
       ') lower than old step ('//real82str(nulbad_dpas)//'), ip forced to 1')
      ip = 1
    end if

    !
    !  Convolution sp synthetique avec profil instrumental
    !
    if(config_convol) then
      call cafconvh()
      call log_debug('p_ift='//int2str(p_ift))

      j = (p_ift-1)/2
      jp1 = j+1
      dmj = ktot-j
      dtotc = dmj-jp1+1

      m = 0
      do d = jp1,dmj,ip
        m = m+1
        tl(m) = lambd(d)
      end do
      kktot = m

      if(config_flam) then
        do k = 1,ktot
          fl(k) = ffl(k)
        end do
      else
        do k = 1,ktot
          fl(k) = ffnu(k)
        end do
      end if

      call volut() ! calculates alfl

      k=0
      do i = jp1, dmj, ip
        k = k+1
        afl(k) = alfl(i)
      end do
      kktot = k
    end if

    if((.not. config_convol) .and. (.not. config_flam)) then
      kktot = ktot
      do k = 1, kktot
        afl(k) = ffnu(k)
      end do
    end if

    if((.not. config_convol) .and. (config_flam)) then
      kktot = ktot
      do k = 1,kktot
        afl(k) = ffl(k)
        tl(k) = lambd(k)
      end do
    end if

    if(config_convol) then
      alz = tl(1)
      alf = tl(kktot)
    else
      alz = lambd(1)
      alf = lambd(kktot)
    end if

    write(UNIT_,201) nulbad_titc,nulbad_tetaeff,nulbad_glog,nulbad_asalog,nulbad_amg
    201 format('#',A,'Tef=',F6.3,X,'log g=',F4.1,X,'[M/H]=',F5.2,X,F5.2)

    write(UNIT_,202) kktot,nulbad_l0,nulbad_lf,config_pat,config_fwhm
    202 format('#',I6,2X,'0. 0. 1. 1. Lzero =',F10.2,2x,'Lfin =', &
               F10.2,2X,'PAS =',F5.2,2x,'FWHM =',F5.2)

    write(*,*) 'ppppppppppppp', afl(1), alfl(1)

    do k=1,kktot
      write(UNIT_,*) tl(k), afl(k)
    end do

    close(unit=UNIT_)

    !#loggingx4
    write(lll,110) nulbad_tetaeff,nulbad_glog,nulbad_asalog,nulbad_nhe_bid,nulbad_amg
    110 format(2X,'tetaeff=',F8.3,2X,'log g=',F6.2,2X,'[M/H]=',F6.2, &
               2X,'NHE=',F5.2,2X,'[Mg/Fe]=',F6.3)
    call log_debug(lll)

    write(lll,130) alz,alf,kktot,config_pat,config_fwhm
    130 format(2X,'Lzero=',F8.3,2x,'Lfin=',F8.2,2x,'KKTOT=',I7, &
               2X,'PAS nouveau =',F5.2,2x,'FWHM=',F5.2)
    call log_debug(lll)

    write(lll,120) nulbad_l0,nulbad_lf,ktot,nulbad_dpas
    120 format(2X,'Lzero=',F8.3,2x,'Lfin=',F8.2,2x,'KTOT =',I7, &
               2X,'PAS original='F5.2)
    call log_debug(lll)

    write(6,'(12F6.3)') (afl(k),k=1,12)
    call log_debug(lll)
  contains

    !=======================================================================================
    !> Calculates alfl

    subroutine volut()
      integer i, imj, j2p, k, jjp1, iimj
      do i = 1,ktot
        alfl(i) = 0.
      end do

      imj = ktot-j
      j2p =  2*j +1
      do i = jp1, imj, ip
        do k = 1, j2p
          alfl(i) = alfl(i) + fl(i-j+k-1) * p_fi(k)*nulbad_dpas
        end do
      end do

      jjp1 = jp1+1
      iimj = imj+1
      do i = 1,jjp1
        alfl(i) = fl(i)
      end do
      do i = iimj,ktot
        alfl(i)=fl(i)
      end do
    end

  end subroutine nulbad_calc


  !=======================================================================================
  !>  ON CALCULE FI(tfi) la fonction de convolution EN p_ift PTS
  !>
  !> @verbatim
  !>  LA FONCTION DE CONVOLUTION PEUT AVOIR MAX_P_IFT PTS
  !>   SI L ON CHANGE CE NBRE DE PTS CHANGER AUSSI IPPTOT=750
  !>   DANS LES DATA QUELQUES LIGNES PLUS BAS.
  !>   (LA MOITIE DU NBRE TOT DE PTS POUR LE CALCUL DES AT)
  !> @endverbatim

  subroutine cafconvh()
    real*8 :: at(-IPPTOT:+IPPTOT)
    real*8, parameter :: C7 = 1.772453
    real*8 :: sigma, aa, totlarg, bb, z
    integer i, ifd
    character*392 lll

    ! GAUSS: PROFIL GAUSSIEN DE 1/2 LARG AA
    sigma = config_fwhm/2.35482
    aa = 1.414214*sigma
    totlarg=3.0 * aa

    !#logging x2
    write(lll,119) config_fwhm,sigma,aa
    119  format(1X,'Profil instrumental gaussien; ', &
     'FWHM =',F7.3,' (A); Sigma=',F7.3,' (A); ', &
     '1/2 Largeur AA =',F7.3,' (A)')
    call log_debug(lll)
    write(lll,134) totlarg
    134 format(' Gaussienne calculee jusqu a une distance du centre:', &
     ' 3(SIGMA*1.414)=',F7.3,' S(A)')
    call log_debug(lll)

    at(0)=0
    do i = 1,IPPTOT
      at(i) = config_pat * i
      at(-i) = -at(i)
      if(at(i) .gt. totlarg) go to 40
    end do

    bb = 3*aa
    write(lll,133) bb
    !> @todo numbers in message probably wrong
    133  FORMAT(' LE PROFIL GAUSSIEN PAR LEQUEL ON CONVOLE NE PEUT', &
      ' AVOIR PLUS DE 121 PTS (60 DE CHAQUE COTE DU CENTRE); ', &
      ' CETTE GAUSSIENNE EST CALCULEE JUSQU A UNE DISTANCE DE', &
      ' 3 DEMI-LARGEUR DU CENTRE; ', &
      ' SOIT:',F7.3,' ANGSTROM; ', &
      ' ELARGISSEZ LE PAS DU CALCUL')
    call pfant_halt(lll)

    40 continue
    ifd = i-1
    p_ift = 1 + 2*ifd

    if(p_ift .gt. MAX_P_IFT)  then
      write(lll,137)
      !> @todo numbers in message probably wrong
      137  FORMAT(5X,'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER A ', &
       ' PLUS DE 500 PTS -CHANGEZ LE PAS-. (LE NBRE DE PTS TOTAL ', &
       ' SUR LA FCTION S OBTIENT EN MULTIPLIANT PAR 6 LE NBRE ', &
       ' DE PTS SUR LA DEMI LARGEUR')
      call pfant_halt(lll)
    end if

    do i = 1,p_ift
      p_tfi(i) = at(i-ifd-1)
    end do

    z = c7*aa

    do i = 1,p_ift
      p_fi(i) = exp( -(p_tfi(i)/aa)**2)  ! here's gaussian exponential
    end do

    do i = 1,p_ift
      p_fi(i) = p_fi(i) / z
    end do
  end
end

! This file is part of PFANT.
!
! PFANT is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! PFANT is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with PFANT.  If not, see <http://www.gnu.org/licenses/>.

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
! Prefixes:
!   - rs_* -- calculated by read_spectrum()
!   - p_*  -- Gaussian function variables, calculated by cafconvh()
module nulbad_calc
  use pfantlib
  implicit none

  public nulbad_calc_

  private

  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  integer, parameter :: MAX_P_IFT = 1501 ! length of "convolution function" (odd number)
  integer, parameter :: IPPTOT = (MAX_P_IFT-1)/2! "TAILLE MAX DE LA FCT DE CONV"

  real*8 :: &
   p_tfi(MAX_P_IFT), &  ! x-axis values of gaussian function (calculated by nulbad::cafconvh())
   p_fi(MAX_P_IFT)      ! y-axis values of gaussian function (calculated by nulbad::cafconvh())
  integer, private :: p_ift ! Number of points of gaussian function (calculated by nulbad::cafconvh())

  real*8, allocatable :: rs_ffnu(:), rs_lambd(:)
  integer :: rs_ktot
  character :: rs_titc*20
  real*8 :: &
   rs_tetaeff, & ! 5040./teff
   rs_glog,    & ! g log
   rs_asalog,  & ! metallicity
   rs_amg,     & ! used to be xxcor(8) but now just zero
   rs_l0,      & ! initial wavelength
   rs_lf,      & ! final wavelength
   rs_dpas,    & ! calculation "delta lambda"
   rs_nhe_bid    ! abundance of helium

  integer, parameter :: UNIT_=199 ! unit for file I/O

  ! x_* values may come either from command line or *main file*
  real*8 :: x_fwhm, x_pat
  character*128 :: x_fn_flux, x_fn_cv, x_flprefix
contains

  !=======================================================================================
  ! Initialization of this module
  !
  ! One of the tasks if the initialization of the x_* variables, whose values may be
  ! either set from the command line or taken from *main file*

  subroutine nulbad_init()
    logical :: main_exists  ! whether or not main configuration file exists

    inquire(file=config_fn_main, exist=main_exists)

    ! MAX_P_IFT must be an odd number
    if (mod(MAX_P_IFT, 2) .eq. 0) then
      call log_and_halt('MAX_P_IFT='//int2str(MAX_P_IFT)//' must be an odd number', &
       is_assertion=.true.)
    end if

    !=====
    ! Assigns x_*
    !=====
    ! values in config_* variables have preference, but if they are uninitialized, will
    ! pick values from *main file*
    x_fwhm = config_fwhm
    x_pat = config_pat
    x_fn_flux = config_fn_flux
    x_fn_cv = config_fn_cv
    if (config_fwhm .eq. -1) then
      if(.not. main_exists) &
        call log_and_halt('--fwhm option not set and '''//&
         trim(config_fn_main)//''' does not exist')
      call assure_read_main(config_fn_main)
      x_fwhm = main_fwhm
      call parse_aux_log_assignment('x_fwhm', real82str(x_fwhm, 3))
    end if

    if (config_flprefix .eq. '?' .and. config_fn_flux .eq. '?') then
      if(.not. main_exists) &
        call log_and_halt('Neither --flprefix nor --fn_flux was set and '''//&
         trim(config_fn_main)//''' does not exist')
      ! Note: x_flprefix only used to make x_fn_flux, in case the latter is not specified
      !       through --fn_flux option.
      call assure_read_main(config_fn_main)
      x_flprefix = main_flprefix
      call parse_aux_log_assignment('x_flprefix', x_flprefix)
    else
      x_flprefix = config_flprefix
    end if
    if (config_fn_flux .eq. '?') then
      x_fn_flux = trim(x_flprefix)//'.norm'
      call parse_aux_log_assignment('x_fn_flux', trim(x_fn_flux))
    end if
    if (config_fn_cv .eq. '?') then
      x_fn_cv = trim(x_fn_flux)//'.nulbad.'//real82str(x_fwhm, 3)
      call parse_aux_log_assignment('x_fn_cv', trim(x_fn_cv))
    end if

    ! Reads spectrum now to make output delta-lambda equal to input delta-lambda
    ! in case --pat not specified
    call read_spectrum()

    if (config_pat .eq. -1) then
      x_pat = rs_dpas
      call parse_aux_log_assignment('x_pat', real82str(x_pat, 3))
    end if
  end

  !=======================================================================================
  ! Reads spectrum file

  subroutine read_spectrum()
    ! variables with suffix "_bid" are read from file, but used for nothing
    real*8 echx_bid, echy_bid, fwhm_bid, lzero_bid, lfin_bid
    integer :: itot, ikeytot, icle, d, k, size_ffnu
    real*8 fnu(MAX_DTOT)  ! temporary, just for reading one "icle" iteration

    open(unit=UNIT_,file=x_fn_flux, status='unknown')

    icle = 1
    rs_ktot = 0

    do while (.true.)
      read(UNIT_, 1130) &
       ikeytot,        & ! used locally
       rs_titc,    & ! written back in output file
       rs_tetaeff, & ! written back
       rs_glog,    & ! written back
       rs_asalog,  & ! written back
       rs_nhe_bid,        & ! read and used in logging only
       rs_amg,     & ! written back
       rs_l0,      & ! used by nulbad_calc() & written back
       rs_lf,      & ! written back
       lzero_bid,      & ! read and discarded
       lfin_bid,       & ! read and discarded
       itot,           & ! used locally
       rs_dpas,    & ! used by nulbad_calc()
       echx_bid,       & ! read and discarded
       echy_bid,       & ! read and discarded
       fwhm_bid          ! read and discarded
      1130 format(i5, a20, 5f15.5, 4f10.1, i10, 4f15.5)

      ! allocates rs_ffnu at first iteration
      if (icle .eq. 1) then
        size_ffnu = ikeytot*itot
        allocate(rs_ffnu(size_ffnu), rs_lambd(size_ffnu)) ! This is probably a slight overallocation
      end if

      read(UNIT_, *)(fnu(d),d=1,itot)

      ! write(lll,*) ikeytot,itot
      ! call log_debug(lll)

      ! pfant writes redundant data: last value of iteration ikey equals first value of
      ! iteration ikey+1. So the following tweak on itot is to account for this.
      ! In the last iteration, the last point is not discarded.
      if (icle .eq. ikeytot) itot = itot+1
      do d = 1,itot-1
        k = rs_ktot+d
        rs_ffnu(k) = fnu(d)
      end do
      rs_ktot = k

      icle = icle+1
      if(icle .gt. ikeytot) exit
    end do

    do k = 1, rs_ktot
      rs_lambd(k) = rs_l0+(k-1)*rs_dpas
    end do


    ! print *, 'AAAAAAA rs_ktot', rs_ktot,' size_ffnu', size_ffnu

    close(unit=UNIT_)
  end

  !=======================================================================================
  ! Main routine of this module

  subroutine nulbad_calc_()
    integer :: d, &
               dmj, &
               dtotc, &
               k, &
               i, &
               j, &
               jp1, &
               kktot, &
               m, &
               ktot  ! size of lambda and flux after resampling
    real*8, dimension(:), allocatable :: alfl, afl, fl, tl, lambd, ffnu
    real*8 alf, alz, ca, cb
    logical :: flag_resample

    call nulbad_init()

    ! # Allocation & re-sampling
    flag_resample = abs(rs_dpas-x_pat) .ge. 1e-10

    if (flag_resample) then
      ! performs a resampling in case the new and old steps are different
      ktot = int(rs_ktot*rs_dpas/x_pat)
    else
      ktot = rs_ktot
    end if

    allocate(lambd(ktot), alfl(ktot), afl(ktot), fl(ktot), &
     tl(ktot), ffnu(ktot))


    if (flag_resample) then
      ! performs a resampling in case the new and old steps are different
      ktot = int(rs_ktot*rs_dpas/x_pat)
      do i = 1, ktot
        lambd(i) = rs_l0+(i-1)*x_pat
      end do
      call log_info('Resampling...')
      call ft2(rs_ktot, rs_lambd, rs_ffnu, ktot, lambd, ffnu)
      call log_info('Resampling...done')
    else
      lambd = rs_lambd
      ffnu = rs_ffnu
    end if

    ! # Creates output file
    ! Note: will now replace output file if already existent
    open(unit=UNIT_,status='replace',file=x_fn_cv)

    ! # Transformation de Fnu en Flambda
    if(config_flam) then
      do k = 1, ktot
        ca = 1.e+11/(lambd(k)**2)
        !  ffnu(10(-5) etait x 10(5), donc cte=10(11) et pas 10(16)
        cb = ca*C
        fl(k) = ffnu(k)*cb
      end do

      write(lll,122) rs_dpas, rs_ktot
      122  format(2x,'pas=',f8.3,2x,'ktot=', i10)
      call log_debug(lll)
    else
      fl = ffnu
    end if

    ! # Convolution sp synthetique avec profil instrumental
    call cafconvh()

    j = (p_ift-1)/2
    jp1 = j+1
    dmj = ktot-j
    dtotc = dmj-jp1+1

    m = 0
    do d = jp1,dmj
      m = m+1
      tl(m) = lambd(d)
    end do
    kktot = m

    call volut() ! calculates alfl

    k=0
    do i = jp1, dmj
      k = k+1
      afl(k) = alfl(i)
    end do
    kktot = k

    alz = tl(1)
    alf = tl(kktot)


    ! # Writes output
    write(UNIT_,201) rs_titc,rs_tetaeff,rs_glog,rs_asalog
    201 format('#',A,'Tef=',F6.3,X,'log g=',F4.1,X,'[M/H]=',F5.2)

    write(UNIT_,202) kktot,rs_l0,rs_lf,x_pat,x_fwhm
    202 format('#',I6,2X,'0. 0. 1. 1. Lzero =',F10.2,2x,'Lfin =', &
               F10.2,2X,'PAS =',F5.2,2x,'FWHM =',F5.2)

    do k=1,kktot
      write(UNIT_,*) tl(k), afl(k)
    end do

    close(unit=UNIT_)  ! xxx

    write(lll,110) rs_tetaeff,rs_glog,rs_asalog,rs_nhe_bid
    110 format(2X,'tetaeff=',F8.3,2X,'log g=',F6.2,2X,'[M/H]=',F6.2, &
               2X,'NHE=',F5.2,2X)
    call log_info(lll)
    write(lll,130) alz,alf,kktot,x_pat,x_fwhm
    130 format(2X,'Lzero=',F9.3,2x,'Lfin=',F8.2,2x,'KKTOT=',I7, &
               2X,'PAS nouveau =',F5.2,2x,'FWHM=',F5.2)
    call log_info(lll)
    write(lll,120) rs_l0,rs_lf,ktot,rs_dpas
    120 format(2X,'Lzero=',F9.3,2x,'Lfin=',F8.2,2x,'KTOT =',I7, &
               2X,'PAS original='F7.4)
    call log_info(lll)
    write(6,'(12F6.3)') (afl(k),k=1,12)
    call log_info(lll)
    call log_info('File '//trim(x_fn_cv)//' successfully created.')
  contains

    !=======================================================================================
    ! Convolution: calculates alfl

    subroutine volut()
      integer i, imj, k, jjp1, iimj, j2p

      do i = 1,ktot
        alfl(i) = 0.
      end do

      imj = ktot-j
      j2p = 2*j+1
      do i = jp1, imj
        do k = 1, j2p
          alfl(i) = alfl(i) + fl(i-j+k-1) * p_fi(k)
        end do
      end do

      jjp1 = jp1-1
      iimj = imj+1
      do i = 1,jjp1
        alfl(i) = fl(i)
      end do
      do i = iimj,ktot
        alfl(i) = fl(i)
      end do
    end
  end


  !=======================================================================================
  !  ON CALCULE FI(tfi) la fonction de convolution EN p_ift PTS
  !
  ! 
  ! LA FONCTION DE CONVOLUTION PEUT AVOIR MAX_P_IFT PTS
  ! SI L ON CHANGE CE NBRE DE PTS CHANGER AUSSI IPPTOT=750
  ! DANS LES DATA QUELQUES LIGNES PLUS BAS.
  ! (LA MOITIE DU NBRE TOT DE PTS POUR LE CALCUL DES AT)
  ! 

  subroutine cafconvh()
    real*8 :: at(-IPPTOT:+IPPTOT)
    real*8 :: sigma, aa, totlarg, z
    integer i, ifd
    real*8, parameter :: SQRT_2 = sqrt(2.)
    real*8 sum_


    ! GAUSS: PROFIL GAUSSIEN DE 1/2 LARG AA
    sigma = x_fwhm/2.35482
    aa = SQRT_2*sigma
    totlarg = 3.0 * aa

    !#logging x2
    write(lll,119) x_fwhm,sigma,aa
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
      at(i) = x_pat * i
      at(-i) = -at(i)
      if(at(i) .gt. totlarg) go to 40
    end do

    write(lll,133) totlarg
    133  FORMAT(' LE PROFIL GAUSSIEN PAR LEQUEL ON CONVOLE NE PEUT', &
      ' AVOIR PLUS DE 121 PTS (60 DE CHAQUE COTE DU CENTRE); ', &
      ' CETTE GAUSSIENNE EST CALCULEE JUSQU A UNE DISTANCE DE', &
      ' 3 DEMI-LARGEUR DU CENTRE; ', &
      ' SOIT:',F7.3,' ANGSTROM; ', &
      ' ELARGISSEZ LE PAS DU CALCUL')
    call log_and_halt(lll)

    40 continue
    ifd = i-1
    p_ift = 1 + 2*ifd
    write(lll, *) 'Number of point of Gaussian curve: '//int2str(p_ift)
    call log_info(lll)

    if(p_ift .gt. MAX_P_IFT)  then
      write(lll,137) MAX_P_IFT, p_ift
      137  FORMAT(5X,'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER A ', &
       ' PLUS DE ',i4,' PTS (ift=)',i4,': -CHANGEZ LE PAS-. (LE NBRE DE PTS TOTAL ', &
       ' SUR LA FCTION S OBTIENT EN MULTIPLIANT PAR 6 LE NBRE DE PTS SUR LA DEMI LARGEUR')
      call log_and_halt(lll)
    end if

    do i = 1,p_ift
      p_tfi(i) = at(i-ifd-1)
    end do

    z = RPI*aa

    sum_ = 0
    do i = 1,p_ift
      p_fi(i) = exp( -(p_tfi(i)/aa)**2) / z  ! here's gaussian exponential
      sum_ = sum_ +p_fi(i)
    end do

    ! (JT) Normalizes the Gaussian function so that its sum_ equals one
    !      This ensures that if the step changes (rs_dpas <> x_pat), then
    !      the amplitude of the resulting spectrum will not change.
    do i = 1,p_ift
      p_fi(i) = p_fi(i)/sum_
    end do

  end
end


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! programme NULBT=NULBAD_SQ
!
!  lecture et convolution sortie fant93
!
!  Programme de lecture des flux en binaire, sortant de
!      Fantomol93 , calculs de 100A en 100A (ou inferieur a 100A)
!
!  (JT) Re-samples spectrum when the input (rs_dpas) and output (x_pat) steps differ.
!
! 
! Paula, julho de 2003:
! - modifiquei para nao normalizar o espectro
! - a opcao config_convol=F nao estava imprimindo o lambda, corrigi
! P.Coelho, dez 2003
! - se norm = .TRUE. (saida do pfant ja eh normalizada), nao altera o valor do fluxo.
!   (JT) ficou so a opcao "flam", opcao "norm" foi removida por razao de redundancia (IF (norm) dentro de IF (flam))
! 

program nulbad
  use pfantlib
  use nulbad_calc
  implicit none

  execonf_name = 'nulbad'
  call config_init()
  call nulbad_calc_()
end




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Math routines used in hydro2_calc.f90 + MAX_* declarations
!>
!> Routines in callhy.f that 1) didn't use COMMONs; 2) didn't take COMMON as argument
!> were moved here.
!>
!> @note Arrays passed to routines now have assumed-shape declarations + assertions against
!> access beyond boundaries.
!>
!> @todo Routines that have the size of arrays passed to them now have assertions. This may
!> slow down the calculations (depending on the number of calls to given routine)
!> and may be subject to re-visit.

module hydro2_math
  use misc_math
  use misc
  implicit none


  ! Dimensions of some constants and variables
  integer, parameter :: MAX_IL = 20, MAX_M = 5

  ! Variables that were declared with dimension 220. modif_ih is the last valid index of
  ! these vectors
  integer, parameter :: MAX_MODIF_IH = 220

  !> Maximum possible value for hjen() ii argument
  integer, parameter :: MAX_MODIF_II = 300



contains
  !=======================================================================================
  !>  CALCUL DE LA FONCTION DE HJERTING = H(A,V)

  subroutine hjen(a,vh,del,phi,ii)
    real*8, intent(in) :: a, vh(:), del
    integer, intent(in) :: ii !< Length of vh and phi vectors
    real*8, intent(out) :: phi(:)

    real*8 :: v1(43),v2(43)
    real*8, parameter :: &
     H11(41) = (/ &
      -1.128380,-1.105960,-1.040480,-0.937030,-0.803460, &
      -0.649450,-0.485520,-0.321920,-0.167720,-0.030120, &
      +0.085940,+0.177890, 0.245370, 0.289810, 0.313940, &
       0.321300, 0.315730, 0.300940, 0.280270, 0.256480, &
       0.231726, 0.207528, 0.184882, 0.164341, 0.146128, &
       0.130236, 0.116515, 0.104739, 0.094653, 0.086005, &
       0.078565, 0.072129, 0.066526, 0.061615, 0.057281, &
       0.053430, 0.049988, 0.046894, 0.044098, 0.041561, &
       0.039250/), &
     H12(43) = (/ &
       0.0440980,0.0392500,0.0351950,0.0317620,0.0288240, &
       0.0262880,0.0240810,0.0221460,0.0204410,0.0189290, &
       0.0175820,0.0163750,0.0152910,0.0143120,0.0134260, &
       0.0126200,0.0118860,0.0112145,0.0105990,0.0100332, &
       0.0095119,0.0090306,0.0085852,0.0081722,0.0077885, &
       0.007\4314,0.0070985,0.0067875,0.0064967,0.0062243, &
       0.0059688,0.0057287,0.0055030,0.0052903,0.0050898, &
       0.0049006,0.0047217,0.0045526,0.0043924,0.0042405, &
       0.0040964,0.0039595,0.0038308/)

    real*8 :: h1, v, w
    integer :: i, k

    call assert_le(ii, size(vh), 'hjen()', 'ii', 'size(vh)')
    call assert_le(ii, size(phi), 'hjen()', 'ii', 'size(phi)')

    ! SI V>3.9 LE CALCUL DE EXP(-V**2) EST INUTILE
    do 100 k = 1,ii
      v = vh(k)
      if (v-12)1,1,2

      2 w = 1./(2*v**2)
      h1 = 2.*w*(1.+w*(3.+15.*w*(1.+7.*w)))
      h1 = h1/RPI
      go to 10

      1 v1(1)=0.
      v2(1)=03.8

      do 5 i = 1,42
        v1(i+1)=v1(i)+0.1
        v2(i+1)=v2(i)+0.2
      5 continue

      if (v-3.9)3,3,4

      4 h1 = ft(v,43,v2,H12)

      10 phi(k) = (a*h1)/(RPI*del)
      go to 100

      3 h1 = ft(v,41,v1,H11)
      phi(k) = (exp(-v**2)+a*h1)/(RPI*del)
    100 continue
  END




  !=======================================================================================
  !> ?doc?
  !>
  !> Output: t

  subroutine malt(th, u, beta, gam, t, iii)
    integer, intent(in) :: iii !< maximum valid index of th and u
    real*8, intent(in), dimension(:) :: th, u
    real*8, intent(in) :: beta, gam
    real*8, intent(out) :: t

    real*8, dimension(150) :: asm ! Can't track down why 150

    real*8 :: ah, aso, avu, h1, h2, pp, sig, sigma, sigma1, t1, t2, tb, to_, vu
    integer :: i, ih1, ih2, im, in, ir

    call assert_le(iii, size(th), 'malt()', 'iii', 'size(th)')
    call assert_le(iii, size(u), 'malt()', 'iii', 'size(u)')

    t1 = f_ta(dble(1.))
    t2 = f_ta(dble(-1.))
    vu = 0.
    sigma = 0.
    avu = vu
    to_ = ft(avu,MAX_IL,u,th)
    aso = f_as(dble(0.))
    pp = beta+gam

    if (pp-20.) 48,47,47  ! dunno if "20." is same as MAX_IL, but I think not, hope not

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
      to_ = ft(avu,MAX_IL,u,th)
      56 asm(i)=f_as(vu)

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

    63 tb = (sigma+sigma1+aso*(h1+0.5)/3.)/PI
    t = t1+t2+tb

  contains
    !> ?doc?

    real*8 function f_as(vu_)
      real*8, intent(in) :: vu_
      f_as = to_*gam/(gam**2+(beta-vu_)**2)
    end

    !> ?doc?

    real*8 function f_ta(x)
      real*8, intent(in) :: x
      real*8 :: aa, bb, q, rm, en, bca, aca, rca, fac, phi
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
      f_ta = (3.*gam/PI)*((-bb/aa+.1666667e-1)/(aa*rca)+phi*log((20.+rca*rm+q)/&
       (20.-rca*rm+q))+(fac/(2.*en))*(PI/2.-atan((20.-q)/(rca*en)))+&
       (bb/(2.*aca*en))*(PI-atan((2.*rca+rm)/en)-atan((2.*rca-rm)/en)))
    end

  end


  !=======================================================================================
  !> INTERPOLATION PARABOLIQUE
  !>  DANS LA TABLE X Y (N POINTS) ON INTERPOLE LES FTT CORRESPONDANT
  !>  AUX TT  (ITOT POINTS) POUR TOUTE LA LISTE DES TT
  !>
  !>  ON ADMET UNE EXTRAPOLATION JUSQU A 1/10 DE X2-X1 ET XN-X(N-1)
  !>
  !> @note This routine is extremely similar to misc_math::ft2()

  subroutine ft2_hydro2(n,x,y,itot,tt,ftt)
    integer, intent(in) :: &
     n, & !< Last valid element of vectors x and y
     itot !< Size valid element of vectors tt and ftt
    real*8, intent(in) :: &
     x(:),     & !< ?doc?
     y(:),     & !< ?doc?
     tt(:)    !< ?doc?
    real*8, intent(out) :: &
     ftt(:)   !< ?doc?
    real*8 ft, a, b, c, d, e, t, t0, t1, t2, u0, u1, u2
    integer i, inv, j, k
    real*8 dxa, dxz, xx1, xxn

    call assert_le(n, size(x), 'ft2_hydro2()', 'n', 'size(x)')
    call assert_le(n, size(y), 'ft2_hydro2()', 'n', 'size(y)')
    call assert_le(itot, size(tt), 'ft2_hydro2()', 'itot', 'size(tt)')
    call assert_le(itot, size(ftt), 'ft2_hydro2()', 'itot', 'size(ftt)')

    inv = -1
    if(x(n).lt.x(1) ) inv=1

    ! DXA ET DXZ  : LIMITES SUPPORTABLES D EXTRAPOLATION
    dxa=(x(2)-x(1))/10
    dxz=(x(n)-x(n-1))/10
    xx1=x(1)-dxa
    xxn=x(n)+dxz

    do k = 1,itot
      t = tt(k)
      if (inv) 5, 6, 6
      5 continue
      if ((t.lt.xx1) .or. (t.gt.xxn)) go to 10
      do 1 j=1,n
        i=j
        if (t-x(j)) 3, 2, 1
      1 continue
      go to 3  !> @todo ISSUE differs from ft2() above (should be go to 10?),
               !> seems it is overriding something that should give an error

      6 continue
      if((t.gt.xx1) .or. (t.lt.xxn)) go to 10
      do 7 j=1,n
        i = j
        if (t-x(j)) 7,2,3
      7 continue
      go to 3  !> @todo ISSUE differs from ft2() above (should be go to 10?),
               !> seems it is overriding something that should give an error

      2 continue
      ft=y(j)
      go to 4

3     continue
      if(i .eq. 1) i = 2
      if(i .ge. n) i = n-1
      t0=t-x(i-1)
      t1=t-x(i)
      t2=t-x(i+1)
      u0=x(i+1)-x(i-1)
      u1=x(i+1)-x(i)
      u2=x(i)-x(i-1)
      a=t0/u0
      b=t1/u1
      c=t2/u2
      d=t0/u1
      e=t1/u0
      ft=y(i+1)*a*b - y(i)*d*c + y(i-1)*e*c

      4 continue
      ftt(k) = ft
    end do
    return

    10 write(lll,100) X(1),X(N),T
    100 format(5X,'ON SORT DE LA TABLE D INTERPOLATION :', &
     /5X,'X(1)=',E15.7,3X,'X(N)=',E15.7,5X,'T=',E15.7)
    call pfant_halt(lll)
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Declaration and initialization of x_* variables
!>
!> These variable values may come either from dfile:main or command-line options.

module hydro2_x
  use logging
  use reader_main
  use config
  implicit none

  !> Option for subroutine fluxis() (6/7/26 points for integration).
  !> @note This was originally set to .FALSE. but I opened it for configuration
  !>       because this information is available inside dfile:main
  logical :: x_ptdisk

  real*8 :: x_teff, x_glog, x_asalog
  integer :: x_inum

  real*8 :: x_vvt
  logical :: x_amores

  real*8 :: x_llzero, x_llfin

contains!> Initializes x_* variables

  subroutine hydro2_init_x()

    if (config_ptdisk .eq. -1) then
      call assure_read_main()
      x_ptdisk = main_ptdisk
    else
      x_ptdisk = int2logical(config_ptdisk)
    end if


    if (config_amores .eq. -1) then
      ! No default because it was originally asking user
      call pfant_halt('Option --amores has not been set')
    else
      x_amores = int2logical(config_amores)
    end if

    if (config_kq .eq. -1) then
      ! No default because it was originally asking user
      call pfant_halt('Option --amores has not been set')
    else
      x_amores = int2logical(config_amores)
    end if

    ! duplicated in innewmarcs
    x_teff = config_teff
    x_glog = config_glog
    x_asalog = config_asalog
    x_inum = config_inum
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
    if (config_teff .eq. -1) then
      call assure_read_main()
      x_teff = main_teff
      call parse_aux_log_assignment('x_teff', real82str(x_teff, 1))
    end if
    if (config_glog .eq. -1)  then
      call assure_read_main()
      x_glog = main_glog
      call parse_aux_log_assignment('x_glog', real82str(x_glog, 3))
    end if
    if (config_asalog .eq. -1) then
      call assure_read_main()
      x_asalog = main_asalog
      call parse_aux_log_assignment('x_asalog', real82str(x_asalog, 3))
    end if


    x_vvt = config_vvt
    if (config_vvt .eq. -1) then
      call assure_read_main()

      if (main_ivtot .gt. 1) then
        call pfant_halt('Tried to read vvt from main configuration file, '//&
         'but not prepared for multiple microturbulence velocities')
      end if
      x_vvt = main_vvt(1)
      call parse_aux_log_assignment('x_vvt', real82str(x_vvt, 3))
    end if

    x_llzero = config_llzero
    if (config_llzero .eq. -1) then
      call assure_read_main()
      x_llzero = main_llzero
      call parse_aux_log_assignment('x_llzero', real82str(x_llzero, 2))
    else
      x_llzero = config_llzero
    end if

    x_llfin = config_llfin
    if (config_llfin .eq. -1) then
      call assure_read_main()
      x_llfin = main_llfin
      call parse_aux_log_assignment('x_llfin', real82str(x_llfin, 2))
    else
      x_llfin = config_llfin
    end if
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Hydro2 main calculation
!>
!> (si modeles_tit va bien modeles_tiabs du modele = absoru2_titre de absoru)

module hydro2_calc
  use logging
  use readers
  use misc_math
  use hydro2_x
  use hydro2_math
  use absoru

  implicit none

  public hydro2_calc_

  private

  ! command-line option: config_zph (default=12) because it will no longer be taken from file




  real*8, parameter :: &
   CSTE = 6.958258E-13

  !=====
  ! Hard-wired configuration
  !=====

  integer, parameter :: LL = 2 !< OPTION DANS RAIEHU (old "IX"). If 1, sets m_jmax=1
  integer, parameter :: J1 = 0 !< OPTION DANS RAIEHU (CONVOLUTION STARK-DOPPLER)

  !> ?doc? configuration option
  logical, parameter :: IS_STARK = .TRUE.


  ! MASSE ET CHARGE DE L ATOME D HYDROGENE
  !> ?doc?
  !> Hydrogen mass?
  real*8, parameter :: CMU = 1.
  !> Hydrogen charge?
  integer, parameter :: NZ = 1



  !=====
  ! p_* variables, shared between at least 2 of these: hydro2(), raiehu() and ameru()
  !=====

  !> ?doc?
  !>
  !> @todo Gotta investigate if this m_jmax has the same meaning as many 46 in absoru_data.f and absoru.f90
  !> gotta add parameter to absoru_data.f and absoru.f90
  integer :: m_jmax = 46

  !> POINTS DE LA RAIE OU ON EFFECTUE LE CALCUL
  real*8 m_dlam(MAX_FILETOH_JMAX)

  data m_dlam /0.,0.01,.02,.03,.04,.05,.06,.08,.1,.125,.15,.175, &
   .20,.25,.30,.35,.40,.50,.60,.70,.80,.90,1.,1.1,1.2,1.3, &
   1.4,1.5,1.6,1.8,2.,2.5,3.,3.5,4.,4.5,5.,7.5,10.,12.5, &
   15.,17.5,20.,25.,30.,35.,0.,0.,0.,0./



  integer, parameter :: IQM = 9  !< Apparently, (the number of discontinuities within m_dlam)
  !< NUMEROS DES DISCONTINUITES DANS LES m_dlam
  integer, parameter :: IJ(IQM+1) = (/7,9,13,17,29,31,37,43,46,0/)

  !> This information is passed to hydro2_calc_() and copied into m_th
  type(hmap_row) :: m_th


  real*8 :: m_tau(MAX_FILETOH_JMAX,0:MAX_MODELES_NTOT)
  real*8 :: m_al(MAX_FILETOH_JMAX, MAX_MODELES_NTOT)
  real*8, dimension(0:MAX_MODELES_NTOT) :: m_bpl, m_tauc
  real*8, dimension(MAX_FILETOH_JMAX) :: r
  real*8, dimension(MAX_MODELES_NTOT) :: m_kc, m_toth, m_pelog, m_hyn, m_vt


  !=====
  ! Variables that were unitialized, filling them with zeroes
  !=====

  !> Maximum value for na/nb (superior/inferior level)
  integer, parameter :: MAX_LEVEL = 20

  !> ?doc? (note: this was uninitialized, has been initialized to zero)
  real*8, parameter :: C(MAX_LEVEL) = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)

  !> ?doc? (note: this was uninitialized, has been initialized to zero)
  integer :: nbmin = 0
  !> ?doc? (note: this was uninitialized, has been initialized to zero)
  integer :: nbmax = 0

contains


  !=======================================================================================
  ! Note: all write(6, ...) have been converted to logging at INFO level. Some of them
  !       could be later removed or changed to DEBUG level.
  !

  subroutine hydro2_calc_(arg_th)
    !> Hydrogen line specification
    type(hmap_row), intent(in) :: arg_th

    ! Enabled/disables lots of log_info() calls.
    ! This was originally being asked as "SORTIES INTERMEDIARIES (T/F)".
    ! This could be made into a config option or removed completely, if someone wishes either.
    logical, parameter :: ECRIT = .TRUE.

    real*8, dimension(MAX_MODELES_NTOT) :: ne, fl
    real*8 :: fc, zut1, zut2, zut3

    integer :: i, iq, j, jj, mmu, n


    ! Note that elements within structure are copied
    ! http://h21007.www2.hp.com/portal/download/files/unprot/fortran/docs/lrm/lrm0076.htm
    m_th = arg_th

    write(lll,'(''      KQ='',I4,10X,A)') config_kq, absoru2_titre
    call log_debug(lll)

    write (lll,75) CMU,NZ
    75 format('  CMU=',F5.2,'  NZ=',I5)
    call log_debug(lll)
    do j = 1, m_jmax
      write (lll,79) j, m_dlam(j)
      79 format('dlam(',i2,')=',F7.3)
      call log_debug(lll)
    end do

    call log_info('  C1=(PI*E**2)/(M*C**2) * CLAM**2 * F*10E24 ')
    call log_info('  F=FORCE D OSCILLATEUR TOTALE DE LA RAIE')
    call log_info('      VALEURS PARTICULIERES DE C1 :')
    call log_info(' ')
    call log_info(' HA:2442.326   HB:249.628   HG:74.4776')
    call log_info(' HD:32.8903    HE:17.72642')
    call log_info(' ')
    call log_info(' ex H alfa  2 3 6562.817 10.15 2442.326 ')
    call log_info('    H beta  2 4 4861.342 10.15  249.628 ')
    call log_info('    H gamma 2 5 4340.475 10.15   74.4776')

    if(ECRIT)   then
      write(lll,72) m_th%na,m_th%nb,m_th%clam,m_th%c1,m_th%kiex,J1,IQM
      72 format('  NA=',I4,' NB=',I4,' CLAM=',F14.3,' C1=',E12.7, ' X=',E12.7,&
       ' J1=',I4,' IQM=',I4,10X,'SI J1=0, CONVOLUTION PAR PROFIL DOPPLER')
      call log_info(lll)
      write(lll,86) (IJ(iq),iq=1,IQM)
      86 format('  IJ    =',10I5)
      call log_info(lll)

      if(x_amores) call log_info(' AMORTISSEMENT RESONANCE')
      if(IS_STARK) call log_info(' ECRITURE ELARG. STARK')
    end if


    call log_debug('   sortie de READER   modeles_pg(1 a 5)')
    write(lll,*)(modeles_pg(I),I=1,5)
    call log_debug(lll)

    do  i=1,modeles_ntot
      m_pelog(i)=log10(modeles_pe(i))
      m_vt(i)=x_vvt*1.e+5
    end do

    call abonio()



    if(ECRIT) then
      call log_info(modeles_tit)
      write(lll,201)
      201 format(13X,'modeles_nh',1X,'modeles_teta', 1X,'LOG PE',12X,'m_tau')
      call log_info(lll)
      do i=1,modeles_ntot
        write(lll,202) i,modeles_nh(i),modeles_teta(i),m_pelog(i),modeles_t5l(i)
        202 format( 2X,I5,3X,1P,E13.5,0P,2F10.4,5X,1P,E12.5)
        call log_info(lll)
      end do
      call log_info('')
    end if

    write(lll,*)'appel de ABSORU pour clam=', m_th%clam
    call log_debug(lll)

    do i = 1,modeles_ntot
      !> @todo I probably changed the parameters of this routine
      call absoru_(m_th%clam,modeles_teta(i),m_pelog(i),1,1,1,1,2,.true.)
      m_toth(i) = absoru_toc
      m_kc(i) = absoru_totkap(1)
      m_hyn(i) = absoru_znh(absoru2_nmeta+4)
      ne(i) = modeles_teta(i)*modeles_pe(i)/cste
    end do


    call raiehu()

    call log_debug(' VOUS CALCULEZ LE m_tau SELECTIF')
    write(lll,*) 'APPEL AMERU pour CLAM=',m_th%clam
    call log_debug(lll)
    call ameru()

    if(ECRIT) then
      write(lll,'(1H1,40X,''CALCUL DE m_tau SELECTIF'')')
      call log_info(lll)
      do  i=1,modeles_ntot,5
        write(lll,'(''NIVEAU '',I5,'' -- m_tau='')') i
        call log_info(lll)
        write (lll,'('//int2str(m_jmax)//'F10.3)') (m_tau(j,i),j=1,m_jmax)
        call log_info(lll)
      end do
      write(lll,'(10X,A)') modeles_tit
      call log_info(lll)

      write(lll,121)
      121 format(7X,'m_tau',5X,'modeles_nh',1X,'modeles_teta LOGPE  KC/NOYAU_DE_H',&
       '            VT           NE         TOTH')
      call log_info(lll)

      do i = 1, modeles_ntot
        write(lll,11) modeles_t5l(i),modeles_nh(i),modeles_teta(i),m_pelog(i),m_kc(i),&
         m_vt(i),ne(i),m_toth(i),i
        11 format(E12.5,2X, E13.5,2F9.4,E16.5,4X,F10.0,2E13.5,I8)
        call log_info(lll)
      end do
    end if

    call log_debug('   APPEL OPTIC1  ')
    call optic1()

    if(m_tauc(modeles_ntot).lt.3.89) then
      write(lll,*) 'Maximum value of tauc (=',m_tauc(modeles_ntot), ') is lower than 3.89'
      call pfant_halt(lll)
    end if

    mmu = 0 ! (note: this was uninitialized, has been initialized to zero)
    call fluxis(mmu, fl,fc)
    do j=1,m_jmax
      r(j)=fl(j)/fc
    end do

    call log_info(' Parametres du modele')
    write(lll,70) x_teff,x_glog,x_asalog,modeles_asalalf,modeles_nhe
    70 format(' TEFF=',F7.0,3X,'LOG G=',F5.2, 3X,'[M/H]=',F6.2,3X,'[alfa/A]=',f6.2,'  NHE=',F6.3)
    call log_info(lll)
    write (lll,*) modeles_tit
    call log_info(lll)
    call log_info(' ')
    write(lll,'('' Absorption calculee avec table '',A)') absoru2_titre
    call log_info(lll)
    write(lll,4) m_th%clam, m_th%na,m_th%nb,m_th%kiex, m_th%c1
    4 format('Lambda=',F10.3,'A','   NA=',I2,' NB=',I2,4X, 'KIEX=',F7.3,4X,'C1=',E12.4)
    call log_info(lll)

    write(lll,21) fc
    21 format('      FLUX CONTINU=',E16.7,'CGS')
    call log_info(lll)
    if(config_kq .eq. 1) call log_info('PROFIL QUASI-STATIQUE')
    if(config_kq .ne. 1) call log_info('THEORIE DE GRIEM')
    if(x_amores) call log_info('CALCUL DE L AMORTISSEMENT DE RESONNANCE')
    if(.not. x_amores) call log_info('ON NEGLIGE AMORTISSEMENT DE RESONNANCE')
    if(J1 .eq. 1) call log_info('PAS DE CONVOLUTION AVEC NOYAU DOPPLER')
    if(J1 .eq. 0) then
      write(lll,406)x_vvt
      406 format(30X,'CONVOLUTION STARK-DOPPLER',10X,'VT=', F5.1,'KM/S')
      call log_info(lll)
    end if

    call log_info('DELTA_LAMBDA(A) FLUX_DANS_LA_RAIE            F/FC')
    do j = 1, m_jmax
      write(lll,'(F15.3,3X,E15.7,1X,F15.5)') m_dlam(j), fl(j), r(j)
      call log_info(lll)
    end do

    !=====
    ! FICHIER POUR TRACE
    !=====
    ! (creates the "th" file, e.g., "thalpha")

    open(unit=17, file=full_path_w(m_th%fn), status='replace')

    zut1 = 0.
    zut2 = 1.
    zut3 = 0.0001

    write(17,100) modeles_tit,x_teff,x_glog,x_asalog,modeles_nhe
    100 format(1X,A,2X,F6.0,3(1X,F6.2))
    write(17,'(I6,2F6.1,2X,2F8.4)') modeles_ntot, zut1, zut1, zut2, zut3
    write(17,'(i4)')m_jmax
    write(17,'(5f14.3)')((m_dlam(jj)+m_th%clam),jj=1,m_jmax)
    write(17,'(5e12.4)')((m_tau(jj,n),jj=1,m_jmax),n=1,modeles_ntot)

    call log_info('TRAVAIL TERMINE')
  end


  !---------------------------------------------------------------------------------------

  !    #    ######  ####### #     # ### #######
  !   # #   #     # #     # ##    #  #  #     #
  !  #   #  #     # #     # # #   #  #  #     #
  ! #     # ######  #     # #  #  #  #  #     #
  ! ####### #     # #     # #   # #  #  #     #
  ! #     # #     # #     # #    ##  #  #     #
  ! #     # ######  ####### #     # ### #######

  !> Calcule l'abondance des elements en tenant compte
  !> d'une eventuelle surabondance des elements alfa
  !>
  !> On entre avec les valeurs lues par lectur, le coef de deficience
  !> et la surabondance des alfa on en deduit les abondances des
  !> differents elements intervenant dans l'ionisation ZP et ABMET
  !> l'abondance des elements lourds pour 1at d'H
  !> Les elements alfa sont reconnus par leur masse
  !>
  !> @warning This routine overwrites variables absoru2_zp absoru2_abmet,
  !>          which are initially read from dfile:absoru2

  SUBROUTINE abonio()
    !> nbre d'elements alfa reconnus par leur masse ALFAM
    integer, parameter :: NALF = 7
    real*8, parameter :: ALFAM(NALF) = (/16.,20.2,24.3,27.,28.,32.,40./)

    real*8 coefalf, asasol, a0, ddm, som1
    integer :: ialfa, j, k


    call log_debug(ENTERING//' abonio')

    coefalf = 10**modeles_asalalf
    asasol = 10**x_asalog

    do j = 1,absoru2_nm
      ialfa = 0
      do K = 1, NALF
        ddm = abs(absoru2_zm(j)-ALFAM(k))
        if (ddm .lt. 0.3) then
          ialfa = ialfa+1
        end if
      end do  !fin de la bcke sur k

      if (ialfa .gt. 0) then
        ! Overwrites value read from file if ialfa > 0
        absoru2_zp(J) = absoru2_zp(J) * coefalf
      end if
    end do   ! fin de la bcle sur j

    som1 = 0

    do j = 1,absoru2_nm
      som1 = som1+absoru2_zp(j)
    end do

    a0 = som1/config_zph

    ! Changing value that was read from dfile:absoru2
    absoru2_abmet = a0*asasol

    write(lll, *) 'in abonio ABMET=',absoru2_abmet
    call log_info(lll)

  end


  !---------------------------------------------------------------------------------------

  ! ######     #    ### ####### #     # #     #
  ! #     #   # #    #  #       #     # #     #
  ! #     #  #   #   #  #       #     # #     #
  ! ######  #     #  #  #####   ####### #     #
  ! #   #   #######  #  #       #     # #     #
  ! #    #  #     #  #  #       #     # #     #
  ! #     # #     # ### ####### #     #  #####

  !> L EST LE COEFFICIENT D'ABSORPTION PAR PARTICULE ABSORBANTE
  !>
  !> LL=1,CALCUL EN UN POINT SPECIFIE PAR SA LONGUEUR D'ONDE
  !> SI DANS CE CAS,J1=2 ON SOMME LES COEFF. D'ABSORPTION DE PLUSIEURS
  !> RAIES D'UNE MEME SERIE
  !> LL=2,CALCUL EN UN OU PLUSIEURS POINTS SPECIFIES PAR DELTA LAMBDA
  !>
  !> SI DANS CE CAS,J1=1,LA CONVOLUTION FINALE EST SAUTEE.
  !>
  !> config_kq=1, CALCUL D UN PROFIL PUREMEMT QUASISTATIQUE POUR IONS + ELETRO

  subroutine raiehu()


    ! Hard-wired configuration
    ! Verbose flag that was kept, but set internally, not as a parameter as originally
    ! SI IND=0,ON ECRIT LE DETAIL DES APPROX. PAR LESQUELLES m_al EST CALCU
    ! SI IND=1, PAS D'ECRITURE
    integer, parameter :: IND = 1


    !=====
    ! Old common "MODIF"
    !=====
    real*8 :: &
     modif_var(MAX_MODIF_IH),modif_f1(MAX_MODIF_IH), &
     modif_phi(MAX_MODIF_II),modif_v(MAX_MODIF_II)
    integer :: modif_ih, modif_ii

    !=====
    ! Other local variables
    !=====
    real*8, dimension(MAX_FILETOH_JMAX, MAX_MODELES_NTOT) :: bidon
    real*8, dimension(MAX_FILETOH_JMAX) :: al_, alfa, lv, vx, x, xdp, resc
    real*8, dimension(MAX_MODELES_NTOT) :: stoc, az
    real*8, dimension(MAX_IL) ::res
    real*8, dimension(MAX_M) :: q
    real*8 :: lim, lac, xbdp, ax, a2, acte, afi, aldp, ar, atest, alfad, b1, b2, b2a2, &
     bcte, beta, bic, bom, br, br4, cab, cam, cam1, cam2, ceg, ci, cl2, cne, corr, corr1,&
     corr2, ct, cty, dan, dcte, dd, dd1, ddop, delie, delom, delp, delta, din, dld, &
     dmoin, ds, ecte, ens, f, fac, g, gam, gam1, gams, fo, ho
    integer :: i, i4, ib, ibou, ical, id, ig, ik, ik1, ikd, ikf, ikt, il, iy, iz, j, k, &
     k1, kp, l1, m, n, n2, nb1
    real*8 :: pak, pas, qbeta, qrsurl, rac, rad, ris, rj, rj2, rnt, rsurl, rvar, rzr, s, &
     s1, t, tdeg, tdens, tempor, timp, truc, tv, xb, z2, z3, z5, zdem, zr


    !=====
    ! Constants
    !=====

    real*8, dimension(MAX_IL) :: V1, V2, V3, V4, V5, R1, R2, R3, R4, R5, U
    real*8, dimension(MAX_IL,MAX_M) :: T1, T2
    real*8 :: CK, R, CKP, CL, CMH
    real*8 :: VAL(MAX_M)
    !     T1 PROFIL QUASISTAT. MOYEN POUR H ALPHA ET H BETA (DISTR. DU CHAMP
    !     ELEC. DE MOZER ET BARANGER) T1= CAB*S(ALPHA)
    !     T2 ID POUR LES AUTRES RAIES
    equivalence(V1(1),T1(1,1)),(V2(1),T1(1,2)),(V3(1),T1(1,3)),(V4(1),&
     T1(1,4)),(V5(1),T1(1,5))
    equivalence(R1(1),T2(1,1)),(R2(1),T2(1,2)),(R3(1),T2(1,3)),(R4(1),&
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


    call log_debug(ENTERING//' PATIENCE VOUS ENTRER DANS RAIEHU')

    acte = 2.**0.6666667
    bcte = 2.**0.1666667
    dcte = 2.*ck/(cmh*CMU)
    ecte = 1.5127e-27
    ar = m_th%na
    zr = NZ
    z2 = zr**2
    z3 = z2*zr
    z5 = z3*z2
    zdem = zr**2.5
    rzr = r*zr
    a2 = ar**2
    go to (1,2),LL

    1 rad = m_th%clam*rzr*1.0e-08
    s = ar*sqrt(rad/(rad-a2))
    n = int(s)
    ! n = jint(s)
    n2 = n+1

    do 70 j = n,n2
      rj = j
      rj2 = rj**2
      x(j)=rj2*a2*1.0e+8/(rzr*(rj2-a2))
      xdp(j)=dble(x(j))
      m_dlam(j)=dabs(m_th%clam-xdp(j))
    70 continue


    if (m_dlam(n+1)-m_dlam(n))11,12,12
    11 nb1 = n+1
    go to 13

    12 nb1 = n

    13 if (j1 .eq. 2) go to 14

    write(lll,'(8(1P,E15.7))') m_dlam(j)
    call log_debug(lll)
    go to 15

    !
    !     CALCUL D'UN BLEND
    14 do 55 i = 1,modeles_ntot
      55 stoc(i)=0.

    if ((nbmax.eq.0).and.(nbmin.eq.0)) go to 15

    k1 = nbmax-nbmin+1
    if (nbmin.eq.nb1) ik1 = 3
    if ((nbmax.gt.nb1).and.(nbmin.lt.nb1)) ik1 = 2
    if ((nbmax.eq.nb1).and.(nbmin.lt.nb1)) ik1 = 1

    !     K1=NBRE DE RAIES CONTRIBUANT AU BLEND, K= INDICE DE COMPTAGE
    !     IK1=1 BLEND AVEC RAIES MOINS ELEVEES DANS LA SERIE
    !     IK1=2 BLEND AVEC RAIES DE PART ET D'AUTRE DE LA RAIE CONSIDEREE
    !     IK1=3 BLEND AVEC RAIES PLUS  ELEVEES DANS LA SERIE
    15 k = 1
    ibou = 1

    !> @todo issue nb being overwritten even before being used elsewhere
    m_th%nb = nb1

    !
    !     REPRISE DE LA SEQUENCE NORMALE
    16 br = m_th%nb
    b2 = br**2
    xb = b2*a2*1.0e+8/(r*(b2-a2))
    xbdp = dble(xb)
    delta = dabs(m_th%clam-xbdp)

    !> @todo issue c1 being overwritten even before being used elsewhere
    !>
    !> @todo issue c being used but nothing was ever assigned to this variable
    !>
    m_th%c1 = c(m_th%nb)

    !> @todo issue overwriting data vector???
    m_dlam(1) = delta

    cl2 = xb**2
    go to 80

    2 br = m_th%nb
    b2 = br**2
    cl2 = m_th%clam**2

    !     CALCUL DE QUANTITES DEPENDANT DE A,B,Z
    80 f = b2-a2
    b2a2 = b2*a2
    br4 = b2**2
    g = br4*br+a2**2*ar
    cab = 5.5e-5*b2a2**2/(z5*f)
    ct = 1.496*cab**1.5
    if ((m_th%na.eq.1).and.(m_th%nb.eq.2))ct = 3.4e-6
    if ((m_th%na.eq.1).and.(m_th%nb.eq.3))ct = 1.78e-5
    if ((m_th%na.eq.2).and.(m_th%nb.eq.3))ct = 1.3e-3
    if ((m_th%na.eq.2).and.(m_th%nb.eq.4))ct = 3.57e-3
    if ((m_th%na.eq.2).and.(m_th%nb.eq.5))ct = 6.0e-3
    if ((m_th%na.eq.2).and.(m_th%nb.eq.6))ct = 9.81e-3

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
     'PORTE SUR L INDICE DU NIVEAU DU MODELE')
    do 17 i = 1,modeles_ntot
      write(lll,*)'      CALCUL AU NIVEAU',i,' DU MODELE'
      call log_debug(lll)
      t = 5040.39/modeles_teta(i)
      cne = modeles_teta(i)*exp(2.3026*m_pelog(i))/ckp
      cam = cne**0.6666667
      cam1 = cne**0.1666667
      fo = 1.2532e-9*cam
      fac = 1.0e8*m_th%c1/fo
      alfad = m_th%clam*sqrt(dcte*t+m_vt(i)*m_vt(i))/(fo*cl)
      dld = alfad*fo
      az(i)=ecte*m_hyn(i)*cl2/(alfad*fo)
      ddop = 0.8325*alfad
      rsurl = 0.0898*cam1*t**(-0.5)
      kp = 0
      delie = 4.736e-06*cl2/(br*(br-1.)*modeles_teta(i))
      delom = zr*cl2*0.3503192e-4/(b2*modeles_teta(i))
      delp = 2.9953e-15*cl2*cne**0.5
      lim = 1.23e-18*cl2*b2*cam
      dd1 = log10(delom/delp)
      if (config_kq.eq.1) go to 24

      !     QUANTITES UTILES UNIQUEMENT POUR ELARGISSEMENT IMPACT
      !     SIGNIFICATION DE CES QUANTITES  VOIR ARTICLES DE GRIEM ET DE MOZER
      cam2 = cne**0.3333333
      tdeg = 1.39e4/cam1
      timp = 2.1e10*t/cam
      tdens = 1.277e5*t**0.5/cam2
      ens = b2-tdens
      rnt = 4.6*(z3/t)**0.5*log10(4.0e6*t*zr/(b2*cne**0.5))*dan
      gam1 = 1.217e-6*rnt*din*cam2/zdem
      bom = delom/(fo*cab)
      gams = 1.5*pi/(bom**0.5)
      dmoin = 1./sqrt(delom)
      dd = dd1*2.305855
      ceg = b2-tdeg
      aldp = ddop
      go to 120

      24 aldp = ddop/bcte
      if (IND.eq.1) go to 94

      call log_debug('1 RSURL        FZERO        ALFAD        NE DELIE        DELOM        DELP         LIM           I')
      write (lll, '(3X,1P,8E13.5,4X,I2)') rsurl,fo,alfad,cne,delie,delom,delp,lim,i
      call log_debug(lll)
      go to 94

      120 if (IND.eq.1) go to 94

      call log_debug('1 DELOM        DELP        DELIE        RSURL R(N,T)       FZERO        LIM         ALFAD         NEI')
      write(lll,'(3X,9(E12.5,1X),3X,I2)')DELOM,DELP,DELIE,RSURL,RNT,FO,LIM,ALFAD,CNE,I
      call log_debug(lll)

      94 if (LL-1) 19,19,20

      !     CALCUL AUX DIFFERENTS POINTS DU PROFIL

      19 m_jmax = 1  ! this is OK, this is not executed as long as LL>=2


      call log_debug(' VOUS ENTEZ DANS LA BOUCLE INTERIEURE QUI PORTE SUR L INDICE DE POINT DU PROFIL')
      20 do 1010 j = 1,m_jmax
        alfa(j)=m_dlam(j)/fo
        beta = alfa(j)/cab
        iz = 1
        if (alfa(j).lt.1.e-07) go to 23

        s1 = ct/alfa(j)**2.5
        if ((m_dlam(j).eq.0.).or.(config_kq.eq.1)) go to 23

        rac = m_dlam(j)**0.5
        corr = log(delom/m_dlam(j))/dd

        23 if (m_dlam(j)-lim) 21,22,22

        !     FORMULES ASYMPTOTIQUES
        22 if (IND.eq.1) go to 95
        call log_debug('   FORMULES ASYMPTOTIQUES')

        95 afi = 1.

        110 if (config_kq.eq.1) go to 1060

        if (br4.lt.timp) go to 1061

        1060 if (IND.eq.1) go to 100
        call log_debug('   IMPACT   NON')

        !     TESTS POUR TRAITEMENT QUASISTATIQUE
        100 if (m_dlam(j)-delom) 3004,3001,3001

        1012 alfa(j)=alfa(j)/acte

        2011 if (IND.eq.1) go to 2015
        call log_debug(' APPROX.QUASISTATIQUE POUR IONS + ELECTRONS')

        2015 ci = 2.*s1
        go to 27

        2014 if (IND.eq.1) go to 2013
        call log_debug('   APPROXIMATION   QUASISTATIQUE POUR IONS SEULS')

        2013 ci = s1
        go to 27

        !     IMPACT VALABLE SELON HVR (CR 259,3979,1964)
        1061 if (IND.eq.1) go to 96
        call log_debug('   IMPACT   OUI')

        96 if ((ceg.ge.0.).and.(ens.ge.0.)) go to 1018

        if (IND.eq.1) go to 97
        call log_debug('   DEGENERESCENCE')

        97 if (m_dlam(j)-delom) 2010,2010,1012

        2010 go to(52,53),iz

        52 if (m_dlam(j)-delp)1019,1020,1020

        1020 if (IND.eq.1) go to 81
        call log_debug('   LEWIS')

        81 ci= s1*(1.+(dmoin+rnt*corr)*rac)
        go to 27

        1019 if (IND.eq.1) go to 82
        call log_debug('   GRIEM 2 , 26')

        82 ci= s1*(1.+(dmoin+rnt)*rac)*afi
        go to 27

        1018 if (IND.eq.1) go to 100
        call log_debug('   DEGENERESCENCE  NON  VALABLE')
        go to 100

        3001 alfa(j)=alfa(j)/acte
        qbeta = beta/acte
        if (qbeta.gt.20.) go to 2011

        qrsurl = rsurl*bcte
        rvar = qrsurl
        ig = 1
        go to 3003

        3004 if (m_dlam(j)-delp) 3002,3005,3005

        3002 qbeta = beta
        if (qbeta.gt.20.) go to 2014

        rvar = rsurl
        ig = 3
        go to 3003

        3005 corr1 = 2.+log10(m_dlam(j)/delom)/dd1

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

        53 if (m_th%na.eq.3) go to 84

        if (m_dlam(j)-delp)1028,1029,1029

        1029 if (IND.eq.1) go to 83
        call log_debug('   LEWIS')

        83 gam = gam1*corr+gams
        go to 1031

        1028 if (IND.eq.1) go to 84
        call log_debug('   GRIEM   SIMPLE')

        84 gam = gam1+gams

        1031 if (beta-15.)1034,1033,1033

        1033 if (IND.eq.1) go to 2012
        call log_debug('   BETA.GT.15')

        2012 afi = lim/m_dlam(j)
        go to 52

        1034 if (IND.eq.1) go to 86
        call log_debug('   BETA.LT.15')

        86 if (gam-10.)1039,1038,1038

        1038 if (IND.eq.1) go to 87
        write(lll,'(''   GAMMA.GT.10 GAMMA='',E17.7)') gam
        call log_debug(lll)

        87 t = gam/(pi*(beta**2+gam**2))
        go to 1036

        1039 if (IND.eq.1) go to 88
        write(6,'(''   GAMMA.LT.10 GAMMA='',E17.7)') gam
        call log_debug(lll)

        !     INTERPOLATION DANS T1 OU T2 POUR CONSTRUCTION DU PROFIL QUASISTAT.
        !     (VARIABLE RSURL)
        88 iy = 2
        rvar = rsurl
        1026 if (rvar .gt.0.8) go to 1025

        if (kp.eq.1) go to 18

        kp = 1
        do 90 il = 1,MAX_IL
          if ((m_th%na.eq.2).and.((m_th%nb.eq.3).or.(m_th%nb.eq.4))) go to 92

          do 91 m = 1,MAX_M
            91 q(m)=t2(il,m)
          go to 90

          92 do 93 m = 1,MAX_M
            93 q(m)=t1(il,m)

          90 res(il)=ft(rvar,MAX_M,VAL,q)

        18 go to (1090,1091), iy

        !     CALCUL DE T(BETA,GAM)
        1091 call malt(res, U, beta, gam, t, MAX_IL)
        go to 1036

        !     INTERPOLATION DANS TABLE DU PROFIL QUASIST. (VARIABLE BETA)
        1090 tv = ft(qbeta, MAX_IL, U, res)
        go to (6001,6002,6003), ig

        6001 t = tv/acte
        go to 1094

        6002 t = tv/corr2
        go to 1094

        6003 t = tv

        1094 if (IND.eq.1) go to 1036
        call log_debug('   DISTR. MOZER BARANGER')

        1036 ci = t/cab

        27 m_al(j,i)=fac*ci
        if (IND.eq.1) go to 1010
        ! WRITE (6,1043) J,m_dlam(J),ALFA(J),BETA,m_al(J,I)
      1010 continue

      if ((LL.eq.1).or.(j1.eq.1)) go to 17

      !     CALCUL DE LA DEMI LARGEUR STARK DS
      do 1013 j = 1,m_jmax
        lv(j)=m_al(j,i)
        l1 = m_jmax-j+1
        vx(l1)=alfa(j)
        1013 al_(l1)=log10(lv(j))

      if (.not.x_amores) go to 4013

      call pronor() ! calculates ax

      do 4006 j = 1,m_jmax
        lv(j)=lv(j)*ax
        4006 bidon(j,i)=lv(j)

      if (IND.eq.0) then
        write(lll,'(6X,'' AX='',1PE13.5)') ax
        call log_debug(lll)
      end if

      4013 lac = m_al(1,i)/2.
      truc = log10(lac)
      j = 2

      1053 if (lac-m_al(j,i)) 1050,1051,1052

      1050 j = j+1
      go to 1053

      1051 ds = alfa(j)
      go to 1057

      1052 if (j.eq.2) go to 6000

      ds = ft(truc,m_jmax,al_,vx)
      go to 1057

      6000 ds = alfa(2)+(alfa(2)-alfa(1))*(truc-al_(m_jmax-1))/(al_(m_jmax-1)-al_(m_jmax))

      1057 if (.not.IS_STARK) go to 71
      if (IND.eq.0) then
        write(6,'(''   DEMI LARGEUR DU PROFIL STARK  DS='',E17.7)') ds
        call log_debug(lll)
      end if

      !     CONVOLUTION DES PROFILS STARK ET DOPPLER
      if (IND.eq.1) go to 71
      write(lll, '(''1'',''     RESULTATS DU PRODUIT DE CONVOLUTION'')')
      call log_debug(lll)

      71 continue
      if (x_amores) go to 4100

      if (ds-ddop) 1070,1070,1071

      1070 ib = 2
      go to 1072

      1071 ib = 1

      1072 call pronor() ! calculates ax
      if (IND.eq.1) go to 28
      write(lll,'('' FACTEUR DE NORM.='',1P,E15.7)') ax
      call log_debug(lll)
      ! call log_debug('VOUS PASSEZ AU NIVEAU SUIVANT')

      28 do 1006 j = 1,m_jmax
        lv(j) = lv(j)*ax
        1006 bidon(j,i)=lv(j)

      tempor = alfa(m_jmax)-0.7071068*alfad
      atest = 4.*aldp
      do 1011 j = 1,m_jmax
        if (alfa(j) .ge. atest) ib = 1
        go to (1073,1074), ib

        1073 if (tempor-alfa(j)) 1011,1011,1048

        1048 call conf(j)
        go to 1075

        1074 call conv2(j)

        1075 m_al(j,i)=ris
        if (IND.eq.1) go to 1011
        if (ib.eq.1) go to 1080
        write(6,'(''   CONV2='',1P,E16.5)')m_al(j,i)
        call log_debug(lll)
        go to 1011
        1080 write(lll,'(''   CONF ='',1P,E16.5)') m_al(j,i)
        call log_debug(lll)
      1011 continue
      go to 17

      ! CALCUL POUR UN NIVEAU DU MODELE DE LA TABLE:PHI(A,V) DANS LAQUELLE ON
      ! ERA POUR CHAQUE POINT DU PROFIL
      ! ON RESSERRE LA TABLE DE PHI,POUR V COMPRIS ENTRE 1 ET 4,SOIT modif_var COMPR
      ! ENTRE DLD/2 ET 2*DLD
      4100 call ft2_hydro2(m_jmax,m_dlam,lv,modif_ih,modif_var,modif_f1)
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
      modif_v(modif_ii) = modif_v(modif_ii-1)+5*cty
      call hjen(az(i), modif_v, dld, modif_phi, modif_ii)
      call conv4()
      do 1017 k = 1,m_jmax
        m_al(k,i)=resc(k)
      1017 continue
    17 continue ! end of loop opened 400 lines above!

    call log_debug('LE DERNIER NIVEAU DU MODELE EST ATTEINT')
    if (IND .eq. 0)   then
      if (.not. IS_STARK) go to 4011
      write(lll,'(''1'',''STARK='')')
      call log_debug(lll)
      do 4010 i = 1,modeles_ntot,5
        write(6,'('' COUCHE'',I4)') i
        call log_debug(lll)
        write(6,'(8X,10E12.5)') (bidon(k,i),k=1,m_jmax)
        call log_debug(lll)
      4010 continue
      4011 continue
    end if


    if (j1.ne.2) go to 1027
    if (k1.eq.1) go to 1027

    !     SEQUENCE POUR CALCUL DE BLEND
    do 54 i = 1,modeles_ntot
      54 stoc(i)=stoc(i)+m_al(1,i)

    !***************************
    !  IMPRESSIONS SUPPLEMENTAIRES
    write(6,'(5X,''NA='',I5,5X,''NB='',I5)')m_th%na,m_th%nb
    call log_debug(lll)
    write(6,'(8(1P,E15.7))')(m_al(1,i),i=1,modeles_ntot)
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
    45 m_th%nb = m_th%nb-1
    go to 16
    47 m_th%nb = m_th%nb+1
    go to 16
    46 if (nbmax -m_th%nb)58,58,59
    59 m_th%nb = m_th%nb+1
    go to 16
    58 if (ibou.ne.1) go to 3
    m_th%nb = nb1-1
    ibou = ibou+1
    go to 16
    3 m_th%nb = m_th%nb-1
    go to 16
    48 do 4 i = 1,modeles_ntot
    4 m_al(1,i)=stoc(i)

    !***************************
    !  IMPRESSIONS SUPPLEMENTAIRES
    write(lll,'(10X,''COEF. SOMME (BLEND)'')')
    call log_debug(lll)
    write(6,'(8(1P,E15.7))')(m_al(1,I),I=1,modeles_ntot)
    call log_debug(lll)

    !***************************
    go to 1027
    1025 write(lll,1046) I,J
    1046 format('      RZERO/LAMBDA SUP. A 0.8     I=',I4,'     J=',I4,'ON SORT DU SSP'/)
    call log_debug(lll)


    1027 continue
    call log_debug(LEAVING//' VOUS ETES SORTI DE RAIEHU')
    return
  contains



    !---------------------------------------------------------------------------------------
    !> CONVOLUTION AVEC H(A,V),INTEGRATION PAR SIMPSON
    !>
    !> Contained inside raiehu() to share variables modif_*
    !>
    !> Calculates variable resc

    subroutine conv4()
      integer, parameter :: N(6) = (/11,21,23,171,191,211/)
      real*8, parameter :: PAS(6) = (/0.001,0.01,0.045,0.1,0.25,0.5/)

      real*8, dimension(MAX_MODIF_IH) :: ac, phit
      real*8 :: bol, epsi, q, qy, ff1, ff2, ff4, res, res1, resg, som
      integer :: i, ir, j, k, kk

      epsi = 1.e-06
      do 1017 kk = 1,m_jmax
        bol = m_dlam(kk)/dld
        ir = 1
        q = 1.
        qy = q/dld
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

        20 if (m_dlam(kk).eq.0.) go to 30

        qy=-qy
        res1 = res
        ir = 2
        go to 25

        35 res = res1+res
        resc(kk) = res
        go to 1017

        30 res = 2.*res
        resc(kk) = res
      1017 continue
    end


    !---------------------------------------------------------------------------------------
    !> PRODUIT DE CONVOLUTION POUR LE CAS OU LE PROFIL STARK VARIE PLUS
    !> VITE QUE LE PROFIL DOPPLER. INTEGRATION PAR SIMPSON
    !>
    !> Result goes in variable ris

    subroutine conv2(j)
      integer, intent(in) :: j

      real*8 :: h(10), q, bol, s, som, s1, s2, sig
      real*8, dimension(MAX_FILETOH_JMAX) :: v, ac, f
      integer :: ir, n, i, i1, k, k1, k2, k3, k4

      bol = m_dlam(j)/dld
      h(1) = m_dlam(2)
      do 10 i = 2,IQM
         i1 = IJ(i-1)
         10 h(i)=m_dlam(i1+1)-m_dlam(i1)

      do 11 n = 1, m_jmax
        11 v(n) = m_dlam(n)/dld

      q = 1.
      ir = 1

      40 do 15 n = 1,m_jmax
        ac(n) = -(bol-q*v(n))**2
        15 f(n) = lv(n)*exp(ac(n))

      som = 0.
      do 12 i = 1,IQM
        if (i.gt.1) go to 20
        k1 = 1
        k2 = IJ(1)
        go to 21
        20 k1 = IJ(i-1)
        k2 = IJ(i)
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
      ris = (s1+s2)/(RPI*dld)
    end

    !-------------------------------------------------------------------------------------
    !> NORMALISATION DU PROFIL DE STARK,INTEGRATION PAR SIMPSON
    !>
    !> Result goes in ax

    subroutine pronor()
      real*8 :: h(IQM), anor, s, sig, som
      integer :: i, i1, k, k1, k2, k3, k4

      h(1)=alfa(2)
      do 10 i = 2,IQM
        i1 = IJ(i-1)
        10 h(i)=alfa(i1+1)-alfa(i1)

      som = 0.

      do 12 i = 1,IQM
        if (i.gt.1) go to 20
        k1 = 1
        k2 = IJ(1)
        go to 21

        20 k1 = IJ(i-1)
        k2 = IJ(i)

        21 k3 = k1+1
        k4 = k2-3
        sig = lv(k1)+lv(k2)+4.*lv(k2-1)

        if (k4.lt.k3) go to 16

        do 13 k = k3,k4,2
          13 sig = sig+4.*lv(k)+2.*lv(k+1)

        16 s = sig*h(i)/3.

        12 som = som+s

      anor = 2.*som
      ax = fac/anor
    end

    !-------------------------------------------------------------------------------------
    !> PRODUIT DE CONVOLUTION EFFECTUE PAR GAUSS HERMITE (N=2)
    !>
    !> Results goes in variable ris


    subroutine conf(j)
      integer, intent(in) :: j
      integer*2 ix
      real*8 :: v, h, arg, avu, to_, q
      data v, h /.7071068, .8862269/

      ris = 0.
      q = 1.
      ix = 1
      2002 continue
      arg = m_dlam(j)-v*dld*q
      avu = abs(arg)

      if (avu .gt. m_dlam(m_jmax)) go to 10

      to_ = ft(avu,m_jmax,m_dlam,lv)
      ris = ris+to_*h
      go to (2003,2004), ix

      2003 continue
      q = -1.
      ix = 2
      go to 2002

      2004 continue
      ris = ris/RPI
      go to 11

      10 continue
      call pfant_halt('conf() says: "impossible"')  ! note: wasn't halting before 2015+
      11 return
    end

  end


  !---------------------------------------------------------------------------------------

  !    #    #     # ####### ######  #     #
  !   # #   ##   ## #       #     # #     #
  !  #   #  # # # # #       #     # #     #
  ! #     # #  #  # #####   ######  #     #
  ! ####### #     # #       #   #   #     #
  ! #     # #     # #       #    #  #     #
  ! #     # #     # ####### #     #  #####

  !> CALCUL DE LA PROFONDEUR OPTIQUE SELECTIVE m_tau PAR INTEGRATION
  !> EXPONENTIELLE.modeles_teta=5040./T,m_pelog=LOG10PE,modeles_nh=VARIABLE DE PROFONDEUR
  !> DANS LE MODELE,LAMB=LONGUEUR D'ONDE
  !>

  subroutine ameru()
    real*8, parameter :: CTE = 3.972257e+8, C1_ = 2.8546e+4

    real*8 :: p(0:MAX_MODELES_NTOT), y(MAX_MODELES_NTOT), &
     pds, f, f1, f2, f20, f3, f30, pe, temp, tet0, u, uv, zkh
    integer :: i, j

    pds = 2*m_th%na**2

    !
    ! CALCUL DU COEFFICIENT D'ABSORPTION PAR NOYAU DE H
    !
    if (LL.eq.2) go to 4000

    write(lll,*)'1'
    call log_debug(lll)
    write(lll,*)'      COEFFICIENTS D ABSORPTION PAR NOYAU D''H'
    call log_debug(lll)
    write(lll,116) m_th%clam,m_jmax
    116 format(3x,'lambda=  ',f10.3,10x,'m_jmax=  ',i5)
    call log_debug(lll)

    !***************************
    4000 continue
    do 1000 i = 1,modeles_ntot
      pe = exp(2.302585*m_pelog(i))
      temp = 2.5*log(5040.39/modeles_teta(i))
      zkh = exp(-31.30364 *modeles_teta(i)+temp-1.098794 )
      f1 = 1./(1.+zkh/pe)
      f2 = exp(-C1_*modeles_teta(i)/m_th%clam)
      f3 = 1.-f2

      if ((modeles_teta(i).ge.0.7).or.(m_pelog(i).le.-3.)) go to 5

      uv = pipe(modeles_teta(i),m_pelog(i))
      u = exp(2.302585 *uv)
      go to 6

      5 u = 2.

      6 f = pds*f1*f3/u*10.**(-m_th%kiex*modeles_teta(i))

      m_bpl(i)=CTE*f2/(f3*m_th%clam**3)

      do j = 1,m_jmax
        m_al(j,i)=f*m_al(j,i)
      end do

      if (LL.eq.2) go to 1000


      write(lll,110)i,f,u
      110 format(3x,'i= ',i4,3x,'n2/nhtot=',1p,e13.5,3x,'u=',1p,e13.5)
      call log_debug(lll)
      write(lll,'(5E15.7)')(m_al(i,j),j = 1,m_jmax)
      call log_debug(lll)

    1000 continue

    write(lll,*)' Calcul de la fonction de Plank au niveau zero'
    call log_debug(lll)

    tet0 = fteta0(modeles_pg,modeles_teta,modeles_ntot)   ! on extrapole modeles_teta pour modeles_nh=0
    f20 = exp(-C1_*tet0/m_th%clam)
    f30 = 1-f20
    m_bpl(0) = CTE*f20/(f30*m_th%clam**3)

    !
    ! CALCUL DE m_tau
    p(0)=0
    do  j = 1,m_jmax
      do i = 1,modeles_ntot
        y(i)=m_al(j,i)
      end do
      p(1)=modeles_nh(1)*(y(1)-(y(2)-y(1)) / (modeles_nh(2)-modeles_nh(1))*modeles_nh(1)/2.)
      call integra(modeles_nh,y,p,modeles_ntot,p(1))
      do i = 0,modeles_ntot
        m_tau(j,i)=p(i)*1.e-24
      end do
    end do
    write(lll,*) LEAVING, '   Sortie de AMERU'
    call log_debug(lll)
  contains

    !-------------------------------------------------------------------------------------
    !> INTERPOLATION DANS UN TABLEAU A DOUBLE ENTREE NOTE TAB
    !>
    !> TET=TABLE DE LA VARIABLE LIGNE.  ALP=TABLE DE LA VARIABLE COLONNE.
    !> RESULTAT=VALEUR DE LA FONCTION POUR LES ENTREES X ET Y

    real*8 function pipe(x,y)
      real*8, intent(in) :: x, y
      integer, parameter :: MMAX = 9, NMAX = 7
      real*8, parameter :: &
       TET(NMAX) = (/0.1,0.2,0.3,0.4,0.5,0.6,0.7/), &
       ALP(MMAX) = (/5.,4.,3.,2.,1.,0.,-1.,-2.,-3./), &
       TAB(MMAX,NMAX) = reshape( (/ &
        1.43,1.83,2.28,2.77,3.27,3.77,4.27,4.77,5.27, &
        0.47,0.62,0.91,1.31,1.78,2.26,2.75,3.25,3.76, &
        0.31,0.32,0.35,0.42,0.61,0.93,1.35,1.82,2.32, &
        0.30,0.30,0.30,0.31,0.32,0.35,0.44,0.65,0.99, &
        0.30,0.30,0.30,0.30,0.30,0.30,0.31,0.32,0.36, &
        0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30, &
        0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30/), (/MMAX,NMAX/) )

      real*8 :: u(3), mu, nu, phi, s0
      integer j, n, inn, nn, m, mm, nc

      n = 1
      80 if (x-TET (n))72,71,70

      70 n = n+1
      go to 80

      71 nn = n
      go to 90

      72 if (nmax-n)74,74,73

      73 nn = n-1
      go to 90

      74 nn = n-2

      90 m = 1

      91 if (ALP (m)-y) 92,93,94

      93 mm = m
      go to 95

      94 m = m+1
      go to 91

      92 if (mmax-m)97,97,96

      96 mm = m-1
      go to 95

      97 mm = m-2

      95 s0 = y-ALP(mm)
      phi = s0*(y-ALP (mm+1))/(ALP(mm+2)-ALP(mm))
      inn = nn+2
      j = 0
      do 200 nc = nn,inn
        j = j+1
        mu = (TAB(mm+1,nc)-TAB(mm,nc))/(ALP(mm+1)-ALP(mm))
        nu = (TAB(mm+2,nc)-TAB(mm+1,nc))/(ALP(mm+2)-ALP(mm+1))
        200 u(j)=TAB(mm,nc)+s0*mu+phi*(nu-mu)

      s0 = x-TET (nn)
      phi = s0*(x-TET(nn+1))/(TET (nn+2)-TET(nn))
      mu = (u(2)-u(1))/(TET (nn+1)-TET (nn))
      nu = (u(3)-u(2))/(TET (nn+2)-TET(nn+1))
      pipe = u(1)+s0*mu+phi*(nu-mu)
    end


  end


  !---------------------------------------------------------------------------------------
  !> CALCUL DE LA PROFONDEUR OPTIQUE m_tauc.FORMULES VOIR THESE CAYREL.
  !> modeles_nh VARIABLE DE PROFONDEUR DANS LE MODELE.m_kc COEFFT D'ABSORPTION
  !> PAR NOYAU D'HYDROGENE.
  !>
  !>
  !> Output goes

  subroutine optic1()
    m_tauc(0)=0.
    m_tauc(1)=modeles_nh(1)*(m_kc(1) - (m_kc(2)-m_kc(1))/(modeles_nh(2)-modeles_nh(1))*modeles_nh(1)/2.)
    call integra(modeles_nh,m_kc,m_tauc,modeles_ntot,m_tauc(1))
  end



  !---------------------------------------------------------------------------------------
  !> Calculates m_fl and m_fc
  !>
  !> Similar to routines in flin.f90
  !>
  !>


  subroutine fluxis(mmu, fl, fc)
    integer, intent(in) :: mmu
    real*8, intent(out), dimension(MAX_MODELES_NTOT) :: fl
    real*8, intent(out) :: fc

    real*8 :: cc(26),tt(26)
    real*8, dimension(0:MAX_MODELES_NTOT) :: t
    real*8 :: bb, bbc, fl_, tolim
    integer :: i, ipoint, j, k

    real*8, parameter :: &
     CCA(6) = (/0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/), &
     TTA(6) = (/0.038,0.154,0.335,0.793,1.467,3.890/), &
     CCP(7) = (/0.176273,0.153405,0.167016,0.135428,0.210244,0.107848, 0.049787/), &
     TTP(7) = (/0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/), &
     CCB(26) = (/0.032517,0.111077,0.071279,0.154237,0.076944,0.143783, &
      0.063174,0.108330,0.038767,0.059794,0.021983,0.034293,0.012815, &
      0.020169,0.007616,0.012060,0.004595,0.007308,0.002802,0.004473, &
      0.001724,0.002761,0.001578,0.002757,0.000396,0.002768/), &
     TTB(26) = (/0.,0.05,0.1,0.20,0.30,0.45,0.60,0.80,1.,1.2,1.4,1.6,1.8, &
      12.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6,3.8,4.2,4.6,5.487/)

    if (x_ptdisk) then
      ipoint = 7
      do i = 1,ipoint
        cc(i)=ccp(i)
        tt(i)=ttp(i)*mmu
        end do
    else
      if (config_kik.eq.0) then
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
    if (m_tauc(modeles_ntot).lt.tolim) then
      call log_halt(' Modele trop court ')
      write(lll,103) modeles_ntot,m_tauc(modeles_ntot)
      103 format(i10,5x,'modeles_t5l=',f10.4)
      call pfant_halt(lll)
    end if
    !
    fc = 0
    do k = 1,ipoint
      bbc = faitk30(tt(k),m_tauc,m_bpl,modeles_ntot)
      fc = fc+cc(k)*bbc
    end do

    do j = 1,m_jmax
      fl_ = 0
      do i = 0,modeles_ntot
        t(i)=m_tau(j,i)+m_tauc(i)
      end do

      do k = 1,ipoint
        bb = faitk30(tt(k),t,m_bpl,modeles_ntot)
        fl_ = fl_+cc(k)*bb
      end do
      fl(j)=fl_
    end do
  end
end




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||ŝ
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> @verbatim
!> HYD2
!>
!> MEUDON OBSERVATORY
!>
!> CALCUL DU PROFIL D UNE RAIE DE L HYDROGENE
!>
!> CE PROGRAMME A ETE ECRIT PAR F.PRADERIE (ANN D AP 1967)
!> TALAVERA Y A INTRODUIT L ELARGISSEMENT DE SELFRESONNANCE (1970)
!> G.HERNANDEZ ET M.SPITE L ONT ADAPTE AU VAX PUIS SUR LA STATION DEC
!> @endverbatim
!>
!> En sortie:
!> @li Un fichier de trace compatible avec GRAFIC (Nom demande)

program hydro2
  use config
  use hydro2_x
  use reader_absoru2
  use reader_modeles
  use reader_hmap
  use hydro2_calc
  use logging
  use dimensions
  use misc
  implicit none
  integer i, cnt_in
  type(hmap_row) :: th

  execonf_name = 'hydro2'
  call config_init()
  call validate_config()
  call hydro2_init_x()

  call read_absoru2(full_path_w(config_fn_absoru2))  ! LECTURE DES DONNEES ABSORPTION CONTINUE
  call read_modele(full_path_w(config_fn_modeles))   ! LECTURE DU MODELE

  if (config_hmap) then
    call assure_read_main()
    call read_hmap(full_path_w(config_fn_hmap))

    call log_info('Using hmap file')
    write(lll, 10) x_llzero, x_llfin
    10 format ('Calculation interval: [',F14.3,',',F14.3,']')
    call log_info(lll)

    cnt_in = 0
    do i = 1, hmap_n
      th = hmap_rows(i)
      if (h_line_is_inside(th%clam, x_llzero, x_llfin)) then
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
    !   - dfile:hmap??
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

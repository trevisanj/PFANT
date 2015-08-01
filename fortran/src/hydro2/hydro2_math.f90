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
       0.0074314,0.0070985,0.0067875,0.0064967,0.0062243, &
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

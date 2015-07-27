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

!> @ingroup gr_math
!> MISCellaneous MATHs: re-usable Math library

module misc_math
  use logging
  implicit none
contains

  !> Computes the Voight function.
  !>
  !> @verbatim
  !> COMPUTES THE VOIGHT FUNCTION  Y/PI*INTEGRAL FROM
  !> - TO + INFINITY OF  EXP(-T*T)/(Y*Y+(X-T)*(X-T)) DT
  !> LA FONCTION EST ENSUITE NORMALISEE
  !> @endverbatim
  !>
  !> @todo sort this reference Reference: Q.S.R.T. VOL16,611 (1976)

  subroutine hjenor(y,x,del,phi)
    real*8, intent(in) :: &
     x,   & !< ?doc?
     y,   & !< ?doc?
     del    !< ?doc?
    real*8, intent(out) :: phi    !< ?doc?
    real*8 voigt
    real*8 vv,uu
    real*8 b(22),ri(15),xn(15)/10.,9.,2*8.,7.,6.,5.,4.,7*3./,        &
     yn(15)/3*.6,.5,2*.4,4*.3,1.,.9,.8,2*.7/,d0(35),d1(35),d2(35)    &
     ,d3(35),d4(35),hn(35),h/.201/,xx(3)/.5246476,1.65068,.7071068/  &
     ,hh(3)/.2562121,.2588268e-1,.2820948/,nby2(19)/9.5,9.,8.5,8.,   &
     7.5,7.,6.5,6.,5.5,5.,4.5,4.,3.5,3.,2.5,2.,1.5,1.,.5/,c(21)/     &
     .7093602e-7,-.2518434e-6,.8566874e-6,-.2787638e-5,.866074e-5,   &
     -.2565551e-4,.7228775e-4,-.1933631e-3,.4899520e-3,-.1173267e-2, &
     .2648762e-2,-.5623190e-2, .1119601e-1,-.2084976e-1,.3621573e-1, &
     -.5851412e-1,.8770816e-1, -.121664,.15584,-.184,.2/
    logical tru/.false./
    real*8 c0, dx, v, u, y2
    integer i, j, min, max, n

    tru=.false.
    b(1)=0.
    b(2)=0.7093602e-7

    if (tru) go to 104

    ! REGION I. COMPUTE DAWSON'S FUNCTION AT MESH POINTS
    tru=.true.

    do i=1,15
      ri(i)=-i/2.
    end do

    do i=1,25
      hn(i)=h*(i-.5)
      c0=4.*hn(i)*hn(i)/25.-2.

      do j = 2,21
        b(j+1)=c0*b(j)-b(j-1)+c(j)
      end do

      d0(i)=hn(i)*(b(22)-b(21))/5.
      d1(i)=1.-2.*hn(i)*d0(i)
      d2(i)=(hn(i)*d1(i)+d0(i))/ri(2)
      d3(i)=(hn(i)*d2(i)+d1(i))/ri(3)
      d4(i)=(hn(i)*d3(i)+d2(i))/ri(4)

      ! write(6,*)i,d0(i),d1(i),d2(i),d3(i),d4(i)
    end do

    104 if (x-5.) 105,112,112
    105 if (y-1.) 110,110,106
    106 if (x.gt.1.85*(3.6-y)) go to 112

    ! REGION II CONTINUED FRACTION .COMPUTE NUMBER OF TERMS NEEDED
    ! write(6,*)'region II'
    if (y.lt.1.45) go to 107
    i=int(y+y) !> @todo issue added int conversion but not sure if this is the logic
    go to 108

    107 continue
    i=int(11.*y)

    108 continue
    j=int(x+x+1.85) !> @todo issue added int conversion but not sure if this is the logic
    max=int(xn(j)*yn(i)+.46) !> @todo issue added int conversion but not sure if this is the logic
    min=min0(16,21-2*max)

    ! EVALUATED CONTINUED FRACTION
    uu=y
    vv=x
    do 109 j=min,19
      u=nby2(j)/(uu*uu+vv*vv)
      uu=y+u*uu
      vv=x-u*vv
    109 continue
    voigt=uu/(uu*uu+vv*vv)/1.772454
    go to 10

    110 continue
    y2=y*y
    if (x+y.ge.5.) go to 113

    ! REGION I. COMMPUTE DAWSON'S FUNCTION AT X FROM TAYLOR SERIES
    n=int(x/h)
    dx=x-hn(n+1)
    u=(((d4(n+1)*dx+d3(n+1))*dx+d2(n+1))*dx+d1(n+1))*dx+d0(n+1)
    v=1.-2.*x*u

    ! TAYLOR SERIES EXPANSION ABOUT Y=0.0
    vv=exp(y2-x*x)*cos(2.*x*y)/1.128379-y*v
    ! write(6,*) n,u,dx,d0(n+1),d1(n+1),d2(n+1),d3(n+1),d4(n+1)
    uu=-y
    max=int(5.+(12.5-x)*.8*y) !> @todo issue added int conversion but not sure if this is the logic
    do 111 i=2,max,2
      u=(x*v+u)/ri(i)
      v=(x*u+v)/ri(i+1)
      uu=-uu*y2
      vv=vv+v*uu
    111 continue
    voigt=1.128379*vv
    ! write(6,*)'region i ',voigt,vv,x,y,del
    go to 10

    112 continue
    y2=y*y
    if (y.lt.11.-.6875*x) go to 113

    !  REGION IIIB  2 POINT GAUSS-HERMITE QUADRATURE
    u=x-xx(3)
    v=x+xx(3)
    voigt=y*(hh(3)/(y2+u*u)+hh(3)/(y2+v*v))
    ! write(6,*)'region IIIb ', voigt
    go to 10

    !  REGION IIIA 4-POINT GAUSS-HERMITE QUADRATURE.
    113 continue
    u=x-xx(1)
    v=x+xx(1)
    uu=x-xx(2)
    vv=x+xx(2)
    voigt=y*(hh(1)/(y2+u*u)+hh(1)/(y2+v*v)+hh(2)/(y2+uu*uu)+hh(2)/(y2+vv*vv))
    ! write(6,*)'region IIIa',voigt

    10 continue
    phi = voigt /  (1.772454 * del)
    ! write(6,*)phi
  end

  !---------------------------------------------------------------------------------------
  !> Returns index of minimum value of fr within interval [ia, iz]
  !>
  !> Returns
  !> i* (ia <= i* <= iz) such that
  !> fr(i*) = minimum( fr(i), ia <= i <= iz)
  integer function iinf(fr,itot,ia,iz)
    real*8, intent(in):: fr(itot) !< search vector
    integer, intent(in) :: &
     itot, & !< size of vector fr
     ia,   & !< interval lower index
     iz      !< interval upper index
    integer i, ia2
    real*8 fmin

    ia2=ia+1
    iinf=ia
    fmin=fr(ia)
    do 1 i=ia2,iz
      if(fr(i).gt.fmin) go to 1
      fmin=fr(i)
      iinf=i
    1 continue
  end


  !---------------------------------------------------------------------------------------
  !> Returns index of maximum value of fr within interval [ia, iz]
  !>
  !> UNE FONCTION FR EST CONNUE EN ITOT POINTS. ON CHERCHE ENTRE
  !> LES POINTS IA ET IZ QUEL EST L INDICE I OU CETTE FONCTION
  !> EST MAXIMUM.

  integer function isup(fr, itot, ia, iz)
    real*8, intent(in):: fr(itot) !< search vector
    integer, intent(in) :: &
     itot, & !< size of vector fr
     ia,   & !< interval lower index
     iz      !< interval upper index
    integer i, ia2
    real*8 fmax

    ia2=ia+1
    isup=ia
    fmax=fr(ia)
    do 1 i=ia2,iz
      if(fr(i) .lt. fmax) go to 1
      fmax=fr(i)
      isup=i
    1 continue
  end

  !---------------------------------------------------------------------------------------
  !> Returns minimum value of ifa within interval [ia, iz]
  !>
  !> This function is designed for integer vectors.

  integer function mini(ifa, ntot, ia, iz)
    integer, intent(in) :: &
     ifa(ntot), & !< search vector
     ntot,      & !< size of vector ifa
     ia,        & !< interval lower index
     iz           !< interval upper index
    integer i, ia2

    mini=ifa(ia)
    ia2=ia+1
    do i=ia2,iz
      if(ifa(i) .lt. mini) then
        mini=ifa(i)
      end if
    end do
  end

  !---------------------------------------------------------------------------------------
  !> Returns maximum value of ifa within interval [ia, iz]

  integer function maxi(ifa, ntot, ia, iz)
    integer, intent(in) :: &
     ifa(ntot), & !< search vector
     ntot,      & !< size of vector ifa
     ia,        & !< interval lower index
     iz           !< interval upper index
    integer i, ia2

    maxi=ifa(ia)
    ia2=ia+1
    do i=ia2,iz
      if(ifa(i).gt.maxi) then
        maxi=ifa(i)
      end if
    end do
  end

  !---------------------------------------------------------------------------------------
  !> Numerical integration
  !>
  !> *METHODE*: LA VALEUR DE L'INTEGRALE SUR L'INTERVALLE X(I), X(I+1)
  !>   EST CALCULEE PAR LA FORMULE DE SIMPSON, LA VALEUR DE Y AU POINT
  !>   MILIEU ETANT CALCULEE PAR INTERPOLATION CUBIQUE, PAR LA ROUTINE
  !>   naitk3()
  subroutine integra(x, y, p, n, pdeb)
    real*8, intent(in) :: &
     x(n), & !< TABLEAU DE VALEURS DE LA VARIABLE INDEPENDANTE, PAR VALEURS CROISSANTES
     y(n), & !< TABLEAU DES VALEURS ASSOCIEES DE LA FONCTION A INTEGRER
     pdeb    !< VALEUR DE LA PRIMITIVE POUR X(1),PREMIERE VALEUR DU TABLEAU
    integer, intent(in) :: n !< Size of x, y, and p
    real*8, intent(out) :: p(0:n) !< TABLEAU DES VALEURS DE LA PRIMITIVE AUX POINTS X(I)
    real*8 fx, xmilieu
    integer i, j

    p(1) = pdeb

    ! CAS SPECIAL DU PREMIER INTERVALLE
    xmilieu=(x(1)+x(2))/2.
    call naitk3(x(1),x(2),x(3),x(4), y(1),y(2),y(3),y(4),xmilieu,fx)
    p(2) = p(1)+((x(2)-x(1))/6.)*(y(1)+y(2)+4.*fx)

    ! CAS GENERAL
    do i=2,n-2
      xmilieu = (x(i)+x(i+1))/2.
      j = i-1
      call naitk3(x(j),x(j+1),x(j+2),x(j+3),y(j),y(j+1),y(j+2),y(j+3),xmilieu,fx)
      p(i+1) = p(i)+((y(i)+y(i+1)+4.*fx)/6.)*(x(i+1)-x(i))
    end do

    ! CAS SPECIAL DERNIER INTERVALLE
    xmilieu = (x(n-1)+x(n))/2.
    j = n-3
    call naitk3(x(j),x(j+1),x(j+2),x(j+3),y(j),y(j+1),y(j+2),y(j+3),xmilieu,fx)
    p(n) = p(n-1)+((x(n)-x(n-1))/6.)*(y(n-1)+y(n)+4.*fx)
  end

  !---------------------------------------------------------------------------------------
  !> CALCUL DE CH VAN DER WAALS  APPROXIMATIONS D UNSOLD (1955)
  !>
  !> SI IS1 ET IS2 SONT EGAUX A S P D F FORMULE 82-54, SINON 82-55
  !> @todo improve documentation ?doc?
  real*8 function calch(kii, iz, kiex1, is1, kiex2, is2)
    real*8, intent(in) :: &
     kii,   & !< ?doc?
     kiex1, & !< ?doc?
     kiex2    !< ?doc?
    integer, intent(in) :: iz !< ?doc?
    character*1, intent(in) :: &
     is1, & !< ?doc?
     is2    !< ?doc?
    real*8 net1c, net2c, c61, c62
    integer, parameter :: il(4) = (/1,-5,-17,-35/)
    character*1, parameter :: &
     is(4) = (/'s','p','d','f'/), &
     ibl = ' '
    integer il1, il2, izc, i

    if (is1 .ne. ibl) go to 1
    c61 = 1.61e-33*(13.5*iz / (kii-kiex1))**2
    c62 = 1.61e-33*(13.5*iz / (kii-kiex2))**2
    go to 10

    1 continue
    do i=1,4
      if(is1 .eq. is(i)) exit
    end do

    il1=il(i)
    do i=1,4
      if(is2 .eq. is(i)) exit
    end do

    il2 = il(i)
    izc = iz**2
    net1c = 13.5 * izc / (kii-kiex1)
    net2c = 13.5 * izc / (kii-kiex2)
    c61 = 3.22e-34 * net1c *(5*net1c+il1)/ izc
    c62 = 3.22e-34 * net2c *(5*net2c+il2)/ izc

    10 calch = c62-c61
    return
  end



  !===============================================================================
  ! INTERPOLATION ROUTINES
  !===============================================================================


  !---------------------------------------------------------------------------------------
  !> INTERPOLATION D UNE LISTE A PAS NON CONSTANT
  !>
  !> EXTRAPOLE TETA(PG) POUR PG=0
  !> @verbatim
  !> ***TETA1 extrapolation parabolique sur les 3 derniers pts
  !> ***TETA2      "             "      sur les pts 2 3 et 4
  !> ***TETA3      "        lineaire    sur les 2 derniers pts
  !> (ici seul TETA3 est utilise)
  !> @endverbatim
  !>
  !> @todo reference
  !>
  !> @todo Function used in specific context, called by BK only. I don't know if not better there.
  real*8 function fteta0(pg, teta, n)
    !> Size of vectors pf and teta; example source is reader_modeles::modeles_ntot
    integer, intent(in) :: n
    real*8, intent(in), dimension(n) :: &
      pg, & !< Example source is reader_modeles::modeles_pg
      teta  !< Example source is reader_modeles::modeles_teta
    real*8, dimension(5) :: pp1,tt1,pp2,tt2
    integer i
    real*8 teta3

    !~logical ecrit  i think this has been tested already, no need to verbose flag, would slow down the maths
    !~ecrit=.false.
    pp1(1)=pg(1)
    tt1(1)=teta(1)
    !~if(ecrit) write(6,*) pg(1), teta(1)
    do i=2,5
      pp1(i)=pg(i)
      pp2(i-1)=pg(i)
      tt1(i)=teta(i)
      tt2(i-1)=teta(i)
      !~if(ecrit) write(6,*) pg(i), teta(i)
    end do
    ! teta1=ft(0.,5,pp1,tt1)
    ! teta2=ft(0.,4,pp2,tt2)
    teta3=tt1(1) - pp1(1) * (tt1(1)-tt1(2)) / (pp1(1)-pp1(2))
    !~if(ecrit) write(6,*)teta3
    fteta0= teta3
    return
  end


  !---------------------------------------------------------------------------------------
  !> INTERPOLATION D UNE LISTE A PAS NON CONSTANT
  !> @todo improve documentation ?doc?

  real*8 function ft(t,n,x,f)
    integer, intent(in) :: n !< Size of vectors x and f
    real*8, intent(in) :: &
     t,    & !< ?doc?
     x(n), & !< ?doc?
     f(n)    !< ?doc?
    real*8 t0, t1, t2, u0, u1, u2, a, b, c, d, e
    integer i, j

    do 1 j = 1,n
      i = j
      if(t-x(j)) 3, 2, 1
      2 continue
      ft = f(j)
      return
    1 continue

    3 continue
    if (i .eq. 1) i = 2
    if (i .ge. n) i = n-1

    t0 = t-x(i-1)
    t1 = t-x(i)
    t2 = t-x(i+1)
    u0 = x(i+1)-x(i-1)
    u1 = x(i+1)-x(i)
    u2 = x(i)-x(i-1)
    a  = t0/u0
    b  = t1/u1
    c  = t2/u2
    d  = t0/u1
    e  = t1/u0
    ft = f(i+1)*a*b - f(i)*d*c + f(i-1)*e*c
  end


  !---------------------------------------------------------------------------------------
  !> @todo ?what? ?doc?
  !>
  !> @todo This routine is very similar to filetoh::ftlin3h(). Explain why ftlin3h() exists, what the differences are, etc.
  !>
  !> @todo actually all these ft routines are similar. I don't know what to do. A lot of code duplication, but each routine is a bit different

  subroutine ftlin3(n,x,y,itot,tt,ftt)
    integer, intent(in) :: &
     n, & !< Size of vectors x and y
     itot !< Size of vectors tt and ftt
    real*8, intent(in) :: &
     x(n),     & !< ?doc?
     y(n),     & !< ?doc?
     tt(itot)    !< ?doc?
    real*8, intent(out) :: &
     ftt(itot)   !< ?doc?

    real*8 ft, dy, t0, t1, t2, u0, t
    integer i, j, jj, k

!     write(6,*) n
!     105 format(7f10.3)
!     write (6,105) (x(j),j=1,n)
!     write (6,105) (y(j),j=1,n)
!

    j=2
    do k=1,itot
      t=tt(k)
      ! 103 format(5x,f10.3)
      ! write(6,103)t
      jj=j-1
      do 1 j=jj,n
        if(t-x(j) ) 3,2,1
      1 continue

      go to 10

      2 continue
      ft=y(j)
      if(j .eq. 1) j=j+1
      go to 4
      ! 3   write(6,*) '   j=',j

      3 continue
      if(j .eq. 1) go to 10

      u0= y(j)-y(j-1)
      t0= x(j)-x(j-1)
      t1=   t -x(j-1)
      ! 104 format(i5,7f9.3)
      ! write(6,104) j, x(j-1), y(j-1), x(j),y(j), u0,t0,t1
      t2= t1/t0
      dy= u0*t2
      ft= y(j-1) + dy

      4 continue
      ftt(k)=ft
    end do

    return

    !> @todo test this label "10", somehow make it fall here
    10 continue
    100 format('On sort de la table d interpolation avec t=',e15.7, '. liste des x: ', 8e15.7)
    write(lll,100) t, (x(i),i=1,n)
    call pfant_halt(lll)
  end



  !---------------------------------------------------------------------------------------
  !> interpolation
  !> @ todo ?what? ?doc? maybe will be found in BLB's book "Numerical Recipes"

  real*8 function faitk30(xx, x, y, n)
    integer, intent(in) :: n !< Size of vectors x and y
    real*8, intent(in) :: &
     xx,     & !< ?doc?
     x(0:n), & !< ?doc?
     y(0:n)    !< ?doc?
    integer i, j
    real*8 resulta
    if (xx .lt. x(2)) then
      i=0
      goto 200
    else if (xx.gt.x(n-2)) then
      i = n-3
      goto 200
    else
      do j = 2, n-2
        if(xx .le. x(j)) go to 100
      end do
    endif

    100 continue
    i = j-2

    200 call naitk3(x(i),x(i+1),x(i+2),x(i+3),y(i),y(i+1),y(i+2),y(i+3),xx,resulta)
    faitk30 = resulta
  end


  !---------------------------------------------------------------------------------------
  !> INTERPOLATION PARABOLIQUE
  !> DANS LA TABLE X Y (N POINTS) ON INTERPOLE LES FTT CORRESPONDANT
  !> AUX TT  (ITOT POINTS) POUR TOUTE LA LISTE DES TT
  !>
  !> @todo issue This was hard-coded switched off and hydro2 has a very similar routine
  !> (just below). However, in some situations, ft2_hydro2 jumps to 3 instead of 10
  !> I keep both routines close due to their similarity.

  subroutine ft2(n,x,y,itot,tt,ftt)
    integer, intent(in) :: &
     n, & !< Size of vectors x and y
     itot !< Size of vectors tt and ftt
    real*8, intent(in) :: &
     x(n),     & !< ?doc?
     y(n),     & !< ?doc?
     tt(itot)    !< ?doc?
    real*8, intent(out) :: &
     ftt(itot)   !< ?doc?
    real*8 ft, a, b, c, d, e, t, t0, t1, t2, u0, u1, u2
    integer i, inv, j, k


    inv = -1
    if (x(n).lt.x(1)) inv = 1
    do k = 1,itot
      t = tt(k)
      if (inv) 5, 6, 6
      5 continue
      do 1 j = 1,n
        i = j
        if (t-x(j)) 3, 2, 1
      1 continue
      go to 10

      6 continue
      do 7 j = 1, n
        i = j
        if(t-x(j)) 7, 2, 3
      7 continue
      go to 10

      2 continue
      ft = y(j)
      go to 4

      3 continue
      if (i .eq. 1) i = 2
      if (i .ge. n) i = n-1
      t0 = t-x(i-1)
      t1 = t-x(i)
      t2 = t-x(i+1)
      u0 = x(i+1)-x(i-1)
      u1 = x(i+1)-x(i)
      u2 = x(i)-x(i-1)
      a  = t0/u0
      b  = t1/u1
      c  = t2/u2
      d  = t0/u1
      e  = t1/u0
      ft = y(i+1)*a*b - y(i)*d*c + y(i-1)*e*c

      4 continue
      ftt(k) = ft
    end do
    return

    !> @todo Document this error situation
    10 continue
    100 format(5x,'On sort de la table d interpolation avec t=',e15.7)
    write(lll,100) t
    call pfant_halt(lll)
  end


  !> INTERPOLATION PARABOLIQUE
  !>  DANS LA TABLE X Y (N POINTS) ON INTERPOLE LES FTT CORRESPONDANT
  !>  AUX TT  (ITOT POINTS) POUR TOUTE LA LISTE DES TT
  !>
  !>  ON ADMET UNE EXTRAPOLATION JUSQU A 1/10 DE X2-X1 ET XN-X(N-1)
  !>
  !> @note Even though it is used in hydro2 only, this subroutine was
  !>       kept close to ft2() due to their similarity

  subroutine ft2_hydro2(n,x,y,itot,tt,ftt)
    integer, intent(in) :: &
     n, & !< Size of vectors x and y
     itot !< Size of vectors tt and ftt
    real*8, intent(in) :: &
     x(n),     & !< ?doc?
     y(n),     & !< ?doc?
     tt(itot)    !< ?doc?
    real*8, intent(out) :: &
     ftt(itot)   !< ?doc?
    real*8 ft, a, b, c, d, e, t, t0, t1, t2, u0, u1, u2
    integer i, inv, j, k
    real*8 dxa, dxz, xx1, xxn

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




  !---------------------------------------------------------------------------------------
  !> Nouvelle subroutine naitk3, remplace aitk3 et aitk30.
  !> @todo issue ?what? ?doc?

  subroutine naitk3(xdi, xdip1, xdip2, xdip3, ydi, ydip1, ydip2, ydip3, xx, fx)
    real*8, intent(in) :: &
     xdi,   & !< ?doc?
     xdip1, & !< ?doc?
     xdip2, & !< ?doc?
     xdip3, & !< ?doc?
     ydi,   & !< ?doc?
     ydip1, & !< ?doc?
     ydip2, & !< ?doc?
     ydip3, & !< ?doc?
     xx       !< ?doc?
    real*8, intent(out) :: &
     fx       !< ?doc?
    real*8 f01, f12, f13, f012, f123, f0123, fu, fv, u, v

    u   = xdi
    v   = xdip1
    fu  = ydi
    fv  = ydip1
    f01 =(fu*(v-xx)-fv*(u-xx))/(v-u)

    u   = xdip2
    fu  = ydip2
    f12 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    u   = xdip3
    fu  = ydip3
    f13 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    u    = xdi
    fu   = f01
    v    = xdip2
    fv   = f12
    f012 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    u    = xdip2
    fu   = f12
    v    = xdip3
    fv   = f13
    f123 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    u     = xdi
    v     = xdip3
    fu    = f012
    fv    = f123
    f0123 = (fu*(v-xx)-fv*(u-xx))/(v-u)

    fx = f0123
  end
end module misc_math

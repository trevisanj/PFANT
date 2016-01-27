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

!> MISCellaneous MATHs: re-usable Math library
!>
!> This module contains routines existing in the 2015- code base .
!>
!> Arrays passed to routines now have assumed-shape declarations, e.g.,
!> @code
!> real*8 :: fr(:)
!> @endcode
!>
!> instead of
!>
!> @code
!> real*8 :: fr(itot)
!> @endcode
!>
!> In the example latter, Fortran won't give a runtime error if
!> <code>itot > size(fr)</code>. Therefore, declaring <code>fr(ntot)</code>
!> provides *no* error protection.
!>
!> On the other hand, declaring <code>fr(:)</code> gives access to the array size
!> using function <code>size()</code>, allows for placing assertions inside the
!> routines.
!>
!> @note However, assertions may slow down the code. Once the code is tested enough,
!> assertions should be taken out from routines that are called many times. *However*,
!> assertions are good documentation, so don't delete them; rather, comment them out.!!


module misc_math
  use logging
  implicit none

  ! Mathematical constants used throughout. Better to use these than define them every
  ! time they are needed inside a routine
  real*8, parameter :: PI = acos(-1.) !< "pi" constant
  real*8, parameter :: RPI = sqrt(PI) !< square root of pi, approx 1.772453851

contains
  !> Computes the Voigt function.
  !>
  !> @verbatim
  !> COMPUTES THE VOIGHT FUNCTION  Y/PI*INTEGRAL FROM
  !> - TO + INFINITY OF  EXP(-T*T)/(Y*Y+(X-T)*(X-T)) DT
  !> LA FONCTION EST ENSUITE NORMALISEE
  !> @endverbatim
  !>
  !> The code seems to originate from Appendix A of [1]
  !>
  !> @par References
  !> [1] Drayson, S. Roland. "Rapid computation of the Voigt profile." Journal of
  !> Quantitative Spectroscopy and Radiative Transfer 16.7 (1976): 611-614.
  !>
  !> [2] A. Belafhal "The shape of spectral lines: widths and equivalent widths of the
  !>     Voigt profile". Optics Communications 177 (2000). 111–118

  function hjenor(y,x,del) result (phi)
    real*8, intent(in) :: &
     y,   & !< ?doc? "Relative importance of the Lorentzian and Gaussian contributions" (?) [2]
     x,   & !< ?doc? The actual variable here (?)
     del    !< ?doc? Normalization term (?)
    real*8 :: phi !< Result value
    real*8 voigt ! un-normalized Voigt function value
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
    logical :: flag_first = .true.
    logical tru/.false./
    real*8 c0, dx, v, u, y2
    integer i, j, min, max, n
    save :: flag_first, b, ri, xn, yn, d0, d1, d2, d3, d4, hn, h, xx, hh, nby2, c


    if (flag_first) then
      ! print *, 'first'
      ! initialization executed at first call

      flag_first = .false.

      tru=.false.
      b(1)=0.
      b(2)=0.7093602e-7

      if (tru) go to 104

      ! ! REGION I. COMPUTE DAWSON'S FUNCTION AT MESH POINTS
      ! tru = .true.

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
      flag_first = .false.
    else
      ! print *, 'NOT FIRST'
      ! pass
    end if

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
    vv = exp(y2-x*x)*cos(2.*x*y)/1.128379-y*v
    ! write(6,*) n,u,dx,d0(n+1),d1(n+1),d2(n+1),d3(n+1),d4(n+1)
    uu = -y
    max = int(5.+(12.5-x)*.8*y) !> @todo issue added int conversion but not sure if this is the logic
    do 111 i = 2,max,2
      u = (x*v+u)/ri(i)
      v = (x*u+v)/ri(i+1)
      uu = -uu*y2
      vv = vv+v*uu
    111 continue
    voigt = 1.128379*vv
    ! write(6,*)'region i ',voigt,vv,x,y,del
    go to 10

    112 continue
    y2 = y*y
    if (y .lt. 11.-.6875*x) go to 113

    !  REGION IIIB  2 POINT GAUSS-HERMITE QUADRATURE
    u = x-xx(3)
    v = x+xx(3)
    voigt = y*(hh(3)/(y2+u*u)+hh(3)/(y2+v*v))
    ! write(6,*)'region IIIb ', voigt
    go to 10

    !  REGION IIIA 4-POINT GAUSS-HERMITE QUADRATURE.
    113 continue
    u = x-xx(1)
    v = x+xx(1)
    uu = x-xx(2)
    vv = x+xx(2)
    voigt = y*(hh(1)/(y2+u*u)+hh(1)/(y2+v*v)+hh(2)/(y2+uu*uu)+hh(2)/(y2+vv*vv))
    ! write(6,*)'region IIIa',voigt

    10 continue
    phi = voigt /  (1.772454 * del)
  end


!!!!  !> Computes the Voight function.
!!!!  !>
!!!!  !> @verbatim
!!!!  !> COMPUTES THE VOIGHT FUNCTION  Y/PI*INTEGRAL FROM
!!!!  !> - TO + INFINITY OF  EXP(-T*T)/(Y*Y+(X-T)*(X-T)) DT
!!!!  !> LA FONCTION EST ENSUITE NORMALISEE
!!!!  !> @endverbatim
!!!!  !>
!!!!  !> @par Reference
!!!!  !> Drayson, S. Roland. "Rapid computation of the Voigt profile." Journal of
!!!!  !> Quantitative Spectroscopy and Radiative Transfer 16.7 (1976): 611-614.
!!!!
!!!!  subroutine hjenor(y,x,del,phi)
!!!!    real*8, intent(in) :: &
!!!!     x,   & !< ?doc?
!!!!     y,   & !< ?doc?
!!!!     del    !< ?doc?
!!!!    real*8, intent(out) :: phi    !< ?doc?
!!!!    real*8 voigt
!!!!    real*8 vv,uu
!!!!    real*8, b(22),ri(15),xn(15)/10.,9.,2*8.,7.,6.,5.,4.,7*3./,        &
!!!!     yn(15)/3*.6,.5,2*.4,4*.3,1.,.9,.8,2*.7/,d0(35),d1(35),d2(35)    &
!!!!     ,d3(35),d4(35),hn(35),h/.201/,xx(3)/.5246476,1.65068,.7071068/  &
!!!!     ,hh(3)/.2562121,.2588268e-1,.2820948/,nby2(19)/9.5,9.,8.5,8.,   &
!!!!     7.5,7.,6.5,6.,5.5,5.,4.5,4.,3.5,3.,2.5,2.,1.5,1.,.5/,c(21)/     &
!!!!     .7093602e-7,-.2518434e-6,.8566874e-6,-.2787638e-5,.866074e-5,   &
!!!!     -.2565551e-4,.7228775e-4,-.1933631e-3,.4899520e-3,-.1173267e-2, &
!!!!     .2648762e-2,-.5623190e-2, .1119601e-1,-.2084976e-1,.3621573e-1, &
!!!!     -.5851412e-1,.8770816e-1, -.121664,.15584,-.184,.2/
!!!!    logical, save :: flag_first = .true.
!!!!    logical tru/.false./
!!!!    real*8 c0, dx, v, u, y2
!!!!    integer i, j, min, max, n
!!!!
!!!!    tru=.false.
!!!!    b(1)=0.
!!!!    b(2)=0.7093602e-7
!!!!
!!!!    if (tru) go to 104
!!!!
!!!!    ! REGION I. COMPUTE DAWSON'S FUNCTION AT MESH POINTS
!!!!    tru = .true.
!!!!
!!!!    do i=1,15
!!!!      ri(i)=-i/2.
!!!!    end do
!!!!
!!!!    do i=1,25
!!!!      hn(i)=h*(i-.5)
!!!!      c0=4.*hn(i)*hn(i)/25.-2.
!!!!
!!!!      do j = 2,21
!!!!        b(j+1)=c0*b(j)-b(j-1)+c(j)
!!!!      end do
!!!!
!!!!      d0(i)=hn(i)*(b(22)-b(21))/5.
!!!!      d1(i)=1.-2.*hn(i)*d0(i)
!!!!      d2(i)=(hn(i)*d1(i)+d0(i))/ri(2)
!!!!      d3(i)=(hn(i)*d2(i)+d1(i))/ri(3)
!!!!      d4(i)=(hn(i)*d3(i)+d2(i))/ri(4)
!!!!
!!!!      ! write(6,*)i,d0(i),d1(i),d2(i),d3(i),d4(i)
!!!!    end do
!!!!
!!!!    104 if (x-5.) 105,112,112
!!!!    105 if (y-1.) 110,110,106
!!!!    106 if (x.gt.1.85*(3.6-y)) go to 112
!!!!
!!!!    ! REGION II CONTINUED FRACTION .COMPUTE NUMBER OF TERMS NEEDED
!!!!    ! write(6,*)'region II'
!!!!    if (y.lt.1.45) go to 107
!!!!    i=int(y+y) !> @todo issue added int conversion but not sure if this is the logic
!!!!    go to 108
!!!!
!!!!    107 continue
!!!!    i=int(11.*y)
!!!!
!!!!    108 continue
!!!!    j=int(x+x+1.85) !> @todo issue added int conversion but not sure if this is the logic
!!!!    max=int(xn(j)*yn(i)+.46) !> @todo issue added int conversion but not sure if this is the logic
!!!!    min=min0(16,21-2*max)
!!!!
!!!!    ! EVALUATED CONTINUED FRACTION
!!!!    uu=y
!!!!    vv=x
!!!!    do 109 j=min,19
!!!!      u=nby2(j)/(uu*uu+vv*vv)
!!!!      uu=y+u*uu
!!!!      vv=x-u*vv
!!!!    109 continue
!!!!    voigt=uu/(uu*uu+vv*vv)/1.772454
!!!!    go to 10
!!!!
!!!!    110 continue
!!!!    y2=y*y
!!!!    if (x+y.ge.5.) go to 113
!!!!
!!!!    ! REGION I. COMMPUTE DAWSON'S FUNCTION AT X FROM TAYLOR SERIES
!!!!    n=int(x/h)
!!!!    dx=x-hn(n+1)
!!!!    u=(((d4(n+1)*dx+d3(n+1))*dx+d2(n+1))*dx+d1(n+1))*dx+d0(n+1)
!!!!    v=1.-2.*x*u
!!!!
!!!!    ! TAYLOR SERIES EXPANSION ABOUT Y=0.0
!!!!    vv = exp(y2-x*x)*cos(2.*x*y)/1.128379-y*v
!!!!    ! write(6,*) n,u,dx,d0(n+1),d1(n+1),d2(n+1),d3(n+1),d4(n+1)
!!!!    uu = -y
!!!!    max = int(5.+(12.5-x)*.8*y) !> @todo issue added int conversion but not sure if this is the logic
!!!!    do 111 i = 2,max,2
!!!!      u = (x*v+u)/ri(i)
!!!!      v = (x*u+v)/ri(i+1)
!!!!      uu = -uu*y2
!!!!      vv = vv+v*uu
!!!!    111 continue
!!!!    voigt = 1.128379*vv
!!!!    ! write(6,*)'region i ',voigt,vv,x,y,del
!!!!    go to 10
!!!!
!!!!    112 continue
!!!!    y2 = y*y
!!!!    if (y .lt. 11.-.6875*x) go to 113
!!!!
!!!!    !  REGION IIIB  2 POINT GAUSS-HERMITE QUADRATURE
!!!!    u = x-xx(3)
!!!!    v = x+xx(3)
!!!!    voigt = y*(hh(3)/(y2+u*u)+hh(3)/(y2+v*v))
!!!!    ! write(6,*)'region IIIb ', voigt
!!!!    go to 10
!!!!
!!!!    !  REGION IIIA 4-POINT GAUSS-HERMITE QUADRATURE.
!!!!    113 continue
!!!!    u = x-xx(1)
!!!!    v = x+xx(1)
!!!!    uu = x-xx(2)
!!!!    vv = x+xx(2)
!!!!    voigt = y*(hh(1)/(y2+u*u)+hh(1)/(y2+v*v)+hh(2)/(y2+uu*uu)+hh(2)/(y2+vv*vv))
!!!!    ! write(6,*)'region IIIa',voigt
!!!!
!!!!    10 continue
!!!!    phi = voigt /  (1.772454 * del)
!!!!    ! write(6,*)phi
!!!!  end


  !---------------------------------------------------------------------------------------
  !> Returns index of minimum value of fr within interval [ia, iz]
  !>
  !> Returns
  !> i* (ia <= i* <= iz) such that
  !> fr(i*) = minimum( fr(i), ia <= i <= iz)
  integer function iinf(fr,itot,ia,iz)
    real*8, intent(in):: fr(:) !< search vector
    integer, intent(in) :: &
     itot, & !< size of vector fr
     ia,   & !< interval lower index
     iz      !< interval upper index
    integer i, ia2
    real*8 fmin

    call assert_le(itot, size(fr), 'iinf()', 'itot', 'size(fr)')

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
    real*8, intent(in):: fr(:) !< search vector
    integer, intent(in) :: &
     itot, & !< size of vector fr
     ia,   & !< interval lower index
     iz      !< interval upper index
    integer i, ia2
    real*8 fmax

    call assert_le(itot, size(fr), 'isup()', 'itot', 'size(fr)')

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
     ifa(:), & !< search vector
     ntot,      & !< size of vector ifa
     ia,        & !< interval lower index
     iz           !< interval upper index
    integer i, ia2

    call assert_le(ntot, size(ifa), 'mini()', 'ntot', 'size(ifa)')

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
     ifa(:), & !< search vector
     ntot,      & !< size of vector ifa
     ia,        & !< interval lower index
     iz           !< interval upper index
    integer i, ia2

    call assert_le(ntot, size(ifa), 'maxi()', 'ntot', 'size(ifa)')

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
     x(:), & !< TABLEAU DE VALEURS DE LA VARIABLE INDEPENDANTE, PAR VALEURS CROISSANTES
     y(:), & !< TABLEAU DES VALEURS ASSOCIEES DE LA FONCTION A INTEGRER
     pdeb    !< VALEUR DE LA PRIMITIVE POUR X(1),PREMIERE VALEUR DU TABLEAU
    integer, intent(in) :: n !< maximum indexes in x, y, and p
    real*8, intent(out) :: p(0:) !< TABLEAU DES VALEURS DE LA PRIMITIVE AUX POINTS X(I)
    real*8 fx, xmilieu
    integer i, j

    call assert_le(n, size(p)-1, 'integra()', 'n', 'size(p)-1')
    call assert_le(n, size(x), 'integra()', 'n', 'size(x)')
    call assert_le(n, size(y), 'integra()', 'n', 'size(y)')

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
  !> @verbatim
  !> SI IS1 ET IS2 SONT EGAUX A S P D F FORMULE 82-54, SINON 82-55
  !> @endverbatim

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
    real*8, intent(in) :: &
      pg(:), & !< Example source is reader_modeles::modeles_pg
      teta(:)  !< Example source is reader_modeles::modeles_teta
    real*8, dimension(5) :: pp1,tt1,pp2,tt2
    integer i
    real*8 teta3

    call assert_le(n, size(pg), 'fteta0()', 'n', 'size(pg)')
    call assert_le(n, size(teta), 'fteta0()', 'n', 'size(teta)')

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
     x(:), & !< ?doc?
     f(:)    !< ?doc?
    real*8 t0, t1, t2, u0, u1, u2, a, b, c, d, e
    integer i, j

    call assert_le(n, size(x), 'ft()', 'n', 'size(x)')
    call assert_le(n, size(f), 'ft()', 'n', 'size(f)')

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
  !> @todo ?doc?
  !>
  !> @todo This routine is very similar to filetoh::ftlin3h(). Explain why ftlin3h() exists, what the differences are, etc.
  !>
  !> @todo actually all these ft routines are similar. I don't know what to do. A lot of code duplication, but each routine is a bit different

  subroutine ftlin3(n, x, y, itot, tt, ftt)
    integer, intent(in) :: &
     n, & !< Size of vectors x and y
     itot !< Size of vectors tt and ftt
    real*8, intent(in) :: &
     x(:),     & !< ?doc?
     y(:),     & !< ?doc?
     tt(:)    !< ?doc?
    real*8, intent(out) :: &
     ftt(:)   !< ?doc?

    real*8 ft, dy, t0, t1, t2, u0, t
    integer i, j, jj, k

    call assert_le(n, size(x), 'ftlin3()', 'n', 'size(x)')
    call assert_le(n, size(y), 'ftlin3()', 'n', 'size(y)')
    call assert_le(itot, size(tt), 'ftlin3()', 'itot', 'size(tt)')
    call assert_le(itot, size(ftt), 'ftlin3()', 'itot', 'size(ftt)')

    j=2
    do k=1,itot
      t=tt(k)
      ! 103 format(5x,f10.3)
      ! write(6,103)t
      jj = j-1
      do 1 j = jj, n
        if (t-x(j)) 3, 2, 1
      1 continue

      !if (abs(t-x(j)) .le. 1e-38) goto 2  ! tolerance because numbers may not match because of tolerance
      !go to 10

      2 continue
      ft = y(j)
      if (j .eq. 1) j = j+1
      go to 4

      3 continue
      ! write(6,*) '   j=',j
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

    10 continue
    100 format('ftlin3(): On sort de la table d interpolation avec t=',e15.7, '. liste des x: ', 8e15.7)
    write(lll,100) t, (x(i),i=1,n)
    call pfant_halt(lll)
  end



  !---------------------------------------------------------------------------------------
  !> interpolation
  !> @ todo ?doc? maybe will be found in BLB's book "Numerical Recipes"

  real*8 function faitk30(xx, x, y, n)
    integer, intent(in) :: n !< Size of vectors x and y
    real*8, intent(in) :: &
     xx,     & !< ?doc?
     x(0:), & !< ?doc?; 0-based
     y(0:)    !< ?doc?; 0-based
    integer i, j
    real*8 resulta

    call assert_le(n, size(x)-1, 'faitk30()', 'n', 'size(x)-1')
    call assert_le(n, size(y)-1, 'faitk30()', 'n', 'size(y)-1')

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
     x(:),     & !< ?doc?
     y(:),     & !< ?doc?
     tt(:)    !< ?doc?
    real*8, intent(out) :: &
     ftt(:)   !< ?doc?
    real*8 ft, a, b, c, d, e, t, t0, t1, t2, u0, u1, u2
    integer i, inv, j, k

    call assert_le(n, size(x), 'ft2_hydro2()', 'n', 'size(x)')
    call assert_le(n, size(y), 'ft2_hydro2()', 'n', 'size(y)')
    call assert_le(itot, size(tt), 'ft2_hydro2()', 'itot', 'size(tt)')
    call assert_le(itot, size(ftt), 'ft2_hydro2()', 'itot', 'size(ftt)')

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
    100 format(5x,'ft2(): On sort de la table d interpolation avec t=',e15.7)
    write(lll,100) t
    call pfant_halt(lll)
  end


  !---------------------------------------------------------------------------------------
  !> Nouvelle subroutine naitk3, remplace aitk3 et aitk30.
  !> @todo issue ?doc?

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


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Functions flin1() and flinh()
!>
!> Calcul du flux ou de l'intensite par la methode d'integration
!> a 6 pts (ou 13pts) de R.Cayrel (these).
!> nouvelle methode de calcul de to . to(1) est calcule et
!> est different de 0 (On pose to(0)=0)   -Avril 1988-
!>
!> @note in flin_(), MT+JT ipoint was being set to 7 regardless of ptdisk. This has been
!>       changed and now works as described: kik .true.: ptdisk .true. -> ipoint = 7; else 6
!>
!> @author Roger Cayrel
!>
!> @todo gotta check, to is never used, but I am pretty sure it used to be, in the original,
!> I may have deleted sth

module flin
  use misc_math
  use misc
  use dimensions
  implicit none
  private

  public flinh, flin1

  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  real*8 :: TD2, TD, TP, CD, CP, C1, C2, C3
  dimension TD2(26),TD(6),TP(7),CD(6),CP(7), C1(13),C2(12),C3(12)

  data TD /0.038,0.154,0.335,0.793,1.467,3.890 /
  data CD /0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
  data TP /0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
  data CP /0.176273,0.153405,0.167016,0.135428,0.210244,0.107848, 0.049787/
  data TD2 /0.,.05,.1,.15,.3,.45,.60,.80,1.,1.2,1.4,1.6,1.8,2., &
   2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
  data C1 /.032517,.047456,.046138,.036113,.019493,.011037,.006425, &
   .003820,.002303,.001404,.000864,.001045,.002769/
  data C2 /.111077,.154237,.143783,.108330,.059794,.034293, &
   .020169,.012060,.007308,.004473,.002761,.002757/
  data C3 /.023823,.030806,.027061,.019274,.010946,.006390, &
   .003796,.002292,.001398,.000860,.000533,.000396/

  logical, parameter :: MODE_FLINH = .true., &
                        MODE_FLIN1 = .false.

  private flin_
contains
  !---------------------------------------------------------------------------------------------------

  !> See routine flin_() for description of parameters

  real*8 function flin1(kap, b, nh, ntot, ptdisk, mu, kik)
    real*8, intent(in), dimension(MAX_MODELES_NTOT) :: kap, nh
    real*8, dimension(0:MAX_MODELES_NTOT), intent(in) :: b
    logical, intent(in) :: ptdisk
    real*8, intent(in) :: mu
    integer, intent(in) :: ntot, kik
    ! This variable is needed just to fill in the allocation requisites for FLIN_() in FLIN1 mode
    real*8, dimension(MAX_MODELES_NTOT) :: dummy_tauhd
    flin1 = flin_(kap, b, nh, ntot, ptdisk, mu, kik, dummy_tauhd, MODE_FLIN1)
  end


  !---------------------------------------------------------------------------------------------------

  !> See routine flin_() for description of parameters
  !>
  !> Differences from FLIN1(): adds tauhd vector to to_

  real*8 function flinh(kap, b, nh, ntot, ptdisk, mu, kik, tauhd)
    real*8, intent(in), dimension(MAX_MODELES_NTOT) :: kap, nh, tauhd
    real*8, dimension(0:MAX_MODELES_NTOT), intent(in) :: b
    logical, intent(in) :: ptdisk
    real*8, intent(in) :: mu
    integer, intent(in) :: ntot, kik

    flinh = flin_(kap, b, nh, ntot, ptdisk, mu, kik, tauhd, MODE_FLINH)
  end


  !> Generic routine, called by flin1() and flinh()

  real*8 function flin_(kap, b, nh, ntot, ptdisk, mu, kik, tauhd, mode_)
    implicit none
    !> ?doc?
    real*8, intent(in) :: kap(MAX_MODELES_NTOT)
    !> ?doc?
    real*8, intent(in) :: b(0:MAX_MODELES_NTOT)
    !> Source is probably @ref reader_modeles::modeles_nh
    real*8, intent(in) :: nh(MAX_MODELES_NTOT)
    !> Source is probably @ref reader_modeles::modeles_ntot
    integer, intent(in) :: ntot
    !> Source is probably @ref reader_main::main_pfdisk
    !> @li if .TRUE. : 7 points
    !> @li .FALSE.   : 6 points
    logical, intent(in) :: ptdisk
    !> cos(angle). Source is probably reader_main::main_mu
    real*8, intent(in) :: mu
    !> (old "IOP") accepts 0 or 1.
    !> @li if 0, uses the 6/7 point formulation
    !> @li if 1, uses the 26-point formulation
    integer, intent(in) :: kik
    !> Used only in FLINH mode
    real*8, intent(in) :: tauhd(MAX_MODELES_NTOT)
    !> Internal, either @ref MODE_FLIN1 or @ref MODE_FLINH
    logical, intent(in) :: mode_
    ! Optical depth
    real*8 :: to_(0:MAX_MODELES_NTOT)

    real*8, dimension(13) :: fp, cc, bb, tt
    real*8, dimension(26) :: bbb

    real*8 tolim
    integer ipoint, l, m, n

    ! Calcul de to_
    to_(0) = 0.
    to_(1) = nh(1)*(kap(1)-(kap(2)-kap(1))/(nh(2)-nh(1))*nh(1)/2.)
    call integra(nh, kap, to_, ntot, to_(1))
    if (mode_ .eqv. MODE_FLINH) then  ! flinh() mode only!!!
      do n = 1,ntot
        to_(n) = to_(n)+tauhd(n)
      end do
    end if

    ! Calcul du flux
    if (kik .eq. 0) then
      ! Formule a 6 ou 7 pts
      if(ptdisk) then
        ipoint = 7
        tolim = 4.0
      else
        ipoint = 6
        tolim = 3.89
      end if

      call check_modele_trop_court(1)

      continue
      do l = 1, ipoint
        if (ptdisk) then
          tt(l) = TP(l)*mu
          cc(l) = CP(l)
        else
          tt(l) = TD(l)
          cc(l) = CD(l)
        end if
      end do

      flin_ = 0.
      do  l = 1, ipoint
        bb(l) = faitk30(tt(l), to_, b, ntot)
        fp(l) = cc(l)*bb(l)
        flin_ = flin_+fp(l)
      end do
      return

    elseif (kik .eq. 1) then
      ! Formule a 26 pts (ne marche que pour le flux!)
      ! (13pts +pts milieu)

      !> @todo test this error condition, better: put this verification in somewhere at startup, but has to be after READ_main()
      if(ptdisk) then
        call pfant_halt('Le sp flin_ ne peut calculer l intensite en 1 pt '// &
         'du disque avec la formule a 26pts (utiliser 6pts/7pts: kik=0)', is_assertion=.true.)
      end if
      tolim = 5.487  ! Le modele doit aller au moins a une prof tolim

      call check_modele_trop_court(2)

      do l = 1,26
        bbb(l) = faitk30(TD2(l), to_, b, ntot)
      end do

      do m = 1, 12
        l = 2*m - 1
        bb(m) = bbb(l+1)
        fp(m) = C1(m)*bbb(l) + C2(m)*bbb(l+1) + C3(m)*bbb(l+2)
        cc(m) = C2(m)
      end do

      fp(13) = C1(13)*bbb(26)
      bb(13) = bbb(26)
      cc(13) = C1(13)
      ! Ces bb et cc ne servent que pour les sorties (pas au calcul)

      flin_ = 0.
      do l = 1,13
        flin_ = flin_+fp(l)
      end do
      return
    else
      call pfant_halt('Bad kik (must be 0 or 1)')
    end if  !(fin du if kik)
  contains
    !> Error verification, called twice

    subroutine check_modele_trop_court(i_call)
      !> Indicates where it was called from, used in error message: facilitates debugging
      integer, intent(in) :: i_call
      if(to_(ntot) .lt. tolim) then
        call pfant_halt('Modele too short (call #'//int2str(i_call)//'): ntot=' //&
         int2str(ntot) //'; to_(' //&
         int2str(ntot) // ') = ' // real82str(to_(ntot), 7) // ' (must be >= '//&
          real82str(tolim, 3) // ')')
      end if
    end
  end
end module flin

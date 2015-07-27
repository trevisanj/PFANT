!> Math routines used only in hydro2 and not other programs
!>
!> Routines in callhy.f that 1) didn't use COMMONs; 2) didn't take COMMON as argument
!> were moved here.

module hydro2_math
  implicit none

  !> Maximum possible value for hjen() ii argument
  integer, parameter :: MAX_HJEN_II = 300

contains
  !>  CALCUL DE LA FONCTION DE HJERTING = H(A,V)

  subroutine hjen(a,vh,del,phi,ii)
    real*8, intent(in) :: a, vh(MAX_HJEN_II), del
    integer, intent(in) :: ii !< Length of vh and phi vectors
    real*8, intent(out) :: phi(MAX_HJEN_II)

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

    if (ii .gt. MAX_HJEN_II) &
      call pfant_halt('hjen(): ii exceeds maximum: '//int2str(ii)//' > '//&
       int2str(MAX_HJEN_II), is_assertion=.true.)

    ! SI V>3.9 LE CALCUL DE EXP(-V**2) EST INUTILE
    do 100 k = 1,ii
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

      4 h1 = ft(v,43,v2,H12)

      10 phi(k) = (a*h1)/(rpi*del)
      go to 100

      3 h1 = ft(v,41,v1,H11)
      phi(k) = (exp(-v**2)+a*h1)/(rpi*del)
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
    real*8 :: x(99),y(99),aly(200),p(99),err(99), xmilieu, xgauss1, xgauss2, &
     del, ym, yg1, yg2, const
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



  !> ?doc?

  subroutine malt(th, u, beta, gam, t, max_il)
    !> length of th and u
    integer, intent(in) :: max_il
    real*8, intent(in), dimension(max_il) :: th, u
    real*8, intent(in) :: beta, gam
    real*8, intent(out) :: t
    real*8, dimension(150) :: asm ! Can't track down why 150
    DATA PI/3.141593/

    t1 = f_ta(1.,beta,gam)
    t2 = f_ta(-1.,beta,gam)
    vu = 0.
    sigma = 0.
    avu = vu
    to_ = ft(avu,MAX_IL,u,th)
    aso = f_as(gam,beta,0.,to_)
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
      56 asm(i)=f_as(gam,beta,vu,to_)

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
      f_ta = (3.*gam/PI)*((-bb/aa+.1666667e-1)/(aa*rca)+phi*alog((20.+rca*rm+q)/&
       (20.-rca*rm+q))+(fac/(2.*en))*(PI/2.-atan((20.-q)/(rca*en)))+&
       (bb/(2.*aca*en))*(PI-atan((2.*rca+rm)/en)-atan((2.*rca-rm)/en)))
    end

  end




  !> INTERPOLATION DANS UN TABLEAU A DOUBLE ENTREE NOTE TAB
  !> XX=TABLE DE LA VARIABLE LIGNE.  YY=TABLE DE LA VARIABLE COLONNE.
  !> RESULTAT=VALEUR DE LA FONCTION POUR LES ENTREES X ET Y

  real*8 function pipe(x,y,xx,yy,tab,nmax,mmax)
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
end

!> @todo remove au_* prefixes


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
!>
!> Subroutine absoru_() and related sub-subroutines
!>
!> Original comment block:
!> @verbatim
!> PARTH =NBRE TOTAL DE NOYAUX PAR CM3
!> PG    =PRESSION TOTALE EN DYNES/CM2
!> ZMU   =POIDS MOLECULAIRE MOYEN
!> RHO   =DENSITE (G-CM-3)
!> TOC   =NOMBRE DE NOYAUX D'HYDROGENE PAR CM3
!> AC    =DEGRE D'IONISATION MOYEN
!> AC1(1)=  ''        ''     DE H
!> AC1(2)=  ''        ''     DE HE+
!> AC1(3)=  ''        ''     DE HE
!> AC2   =  ''        ''     DES METAUX
!> PHI(J)=  ''        ''     DE L ELEMENT J POUR MULTIPLE IONISATION
!> ZNH(M)=POPULATION POUR CHAQUE ABSORBANT M (H,HE OU METAUX)
!> VOIR ARTICLE DE 'VARDYA' APJ VOL.133,P.107,1961
!> @endverbatim
!>
!> Public: only subroutine absoru() and variable absoru_znh
!> @todo Identify outputs and inputs
!> @todo CHECK ALL TYPES
!> @todo CHECK ALL SIZES
!> @todo probably get rid of au_ prefix

module absoru
  use absoru_data
  implicit none

  private  ! This statement makes all symbols private by default

  !> Public subroutine
  public absoru_

  !=====
  ! Variables calculated by absoru_()
  !=====
  !> POPULATION POUR CHAQUE ABSORBANT M (H,HE OU METAUX).
  !> @todo ISSUE Why 12? (MT): It seems that it should be longer.
  real*8, public, dimension(12) :: absoru_znh
  !> Calculated by absoru_(). ?doc?
  real*8, public, dimension(2) :: absoru_totkap

  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  integer, dimension(2) :: au_jshyd
  integer au_jh, au_jfz

  real*8 au_ahe, au_ah, au_ahep, au_uh1, au_zemh, au_uhep1, au_uhe1, au_zeuhe1, &
   au_ul, au_stimu, au_znu1, au_znu2, au_znu3, au_zmuze, au_pe

  real*8 au_avm  !< MASSE ATOMIQUE MOYENNE DES ELEMENTS PLUS LOURDS QUE L'HELIUM
  real*8 au_zmu  !< POIDS MOLECULAIRE MOYEN
  real*8 au_pg   !< PRESSION TOTALE EN DYNES/cm**2
  real*8 au_rho  !< DENSITE (G-CM-3)
  real*8 au_toc  !< NOMBRE DE NOYAUX D'HYDROGENE PAR cm^3
  real*8 au_ac   !< DEGRE D'IONISATION MOYEN

  real*8 :: au_g2d(2, 19)  !< FACTEUR DE GAUNT BOUND FREE
  real*8, dimension(5) :: au_zexpm
  real*8, dimension(10) :: au_zexp
  real*8, dimension(20) :: au_zeuh, au_zeuhep
  real*8, dimension(11) :: au_zk
  real*8 :: au_zkm(30, 9)
  real*8 :: au_ac2(30, 9) !< DEGRE D'IONISATION DES METAUX

  real*8, dimension(3) :: au_ac1  !< Ionization degrees of H, He+ and He
                                  !! @li au_AC1(1): DEGRE D'IONIZATION DE H
                                  !! @li au_AC1(2): DEGRE D'IONIZATION DE HE+
                                  !! @li au_AC1(3): DEGRE D'IONIZATION DE HE


  real*8, dimension(30) :: au_znu
  character*192 lll

contains

  !-------------------------------------------------------------------------------
  !> @ingroup data
  !> Calculates the "continuum absorption"
  !>
  !> Routine name has trailing underscore to differentiate from module name.
  !>
  !> @note 1/3 things that need to be changed to include scattering (other software
  !>       e.g. Upsalla already have this)
  !>
  !> @note 1/3 atmospheric models 50e6 cannot be calculates, would tyake months.
  !>        So, one idea is to include opacity model tables (Upsalla; MARCS model).
  !> @todo structure arguments for all routines in this file

  subroutine absoru_(wl,th,zlpe,callam,calth,calpe,calmet,calu)
    use read_most_files
    implicit none
    real*8, intent(in) :: &
     wl,       & !< ?doc?
     th,       & !< ?doc?
     zlpe        !< ?doc?
    integer, intent(in) :: &
     callam, & !< ?doc?
     calth,  & !< ?doc?
     calpe,  & !< ?doc?
     calmet, & !< ?doc?
     calu      !< ?doc?

    real*8 zzkk(11,2), dif(2,3),scath(2),zzk(11,2),scat(2), &
     scatel, sum1, sum2, unit, wl4, wlh

    integer i, ilt, ith, m, min, mm, mmm, nset, kkk

    data dif /5.799e-13,8.14e-13,1.422e-6,1.28e-6,2.784,1.61/

    !call log_debug(ENTERING//'absoru_()')

    ilt = callam
    if (calpe .eq. 2) go to 9003

    ! SUM1: SOMME DES ABONDANCES DES METAUX
    sum1 = 0.0
    sum2 = 0.0
    do i = 1,absoru2_nm
      sum1 = sum1+absoru2_zp(i)
    end do
    sum2 = sum2+absoru2_zp(i)*absoru2_zm(i)
    au_avm = sum2/sum1

    ! au_ZNU1,au_ZNU2,au_ZNU3=SUCCESSIVEMENT FRACTION D'(H,HE,METAL) PAR NOMBRE T

    do i = 1,absoru2_nm
      au_znu(i) = absoru2_zp(i)/sum1
    end do

    au_znu1 = 1.0/(1.0+absoru2_abmet+absoru2_abhel)
    au_znu2 = au_znu1*absoru2_abhel
    au_znu3 = au_znu1*absoru2_abmet
    au_zmuze = 1.008*au_znu1+4.003*au_znu2+au_avm*au_znu3

    if ((calth.eq.2).and.(callam.eq.2)) go to 5016
    if (calth.eq.2) go to 9001
    if (th.le.0.8) ith=1
    if (th.gt.0.8) ith=2
    nset = absoru2_numset(ith)-1

    9001 continue
    do i = 1,nset
      if (abs(wl-absoru2_wi(i+1,ith)).le.0.50) go to 8000
      if (wl.lt.absoru2_wi(i+1,ith)) go to 7000
    end do

    7000 continue
    au_jfz=1
    go to 9002

    8000 continue
    au_jfz=2

    ! DIFFUSION DE RAYLEIGH PAR H ET H2 (DALGARNO) HARVARD JUIN 1964
    ! SCATH(1)=DIFFUSION DE H
    ! SCATH(2)=DIFFUSION DE H2
    9002 continue
    do 9023 i=1,2
      if (i.eq.2) go to 9020
      if (wl.gt.1026.0) go to 9021
      scath(1)=4.0e-24
      go to 9023

      9020 continue
      if (wl.gt.1200.0) go to 9021
      wlh=1200.0
      go to 9022

      9021 continue
      wlh=wl
      9022 continue
      wl4=wlh**4
      scath(i)=(dif(i,1)+dif(i,2)/sqrt(wl4)+dif(i,3)/wl4)/wl4
    9023 continue

    go to 5018

    5016 continue
    if ((au_jfz.ne.2).or.(ilt.eq.1)) go to 5017
    ilt=callam-1

    5018 continue
    call gaunth(wl)

    5017  continue
    call tempa(wl,th,calth,callam)
    if (calth.eq.2) go to 9007
    call sahath(th)

    9007 continue
    if ((calth.eq.2).and.(callam.eq.2)) go to 9006
    call athyhe (wl,th,callam,zzk)

    9006 continue
    if (calmet.eq.1) go to 9003 !> @todo ISSUE line doing nothing (MT): Very clever!!!

    9003 continue
    call ionipe (th,zlpe,calth)
    mm=absoru2_nmeta+1
    mmm=absoru2_nmeta+6
    scatel=9.559063e-13*au_pe*th
    ! 9.559063E-13=4.81815E-9/5040.39 ET 4.81815E-9=6.625E-25/1.38024E-1
    ! =ELECTRON SCATTERING/(K*T)  UNSOLD P. 180 1955

    !!__logging__
    !86 format ('0LAMBDA KKK   c1'7x'mg'7x'si1'7x'al1'8x'h-'7x'h2-'7x'h2+'9x'h'7x'he+'8x'he'5x'k total/',2a4)
    !write (lll,86) (absoru2_iunite(i),i=1,2)
    !call log_debug(lll)

    kkk=au_jfz
    min=mm

    do i=1,au_jfz
      do m=1,absoru2_nmeta
        zzkk(m,i)=0.0
      end do
    end do

    !> @todo investigate whether absoru_totkap is actually integer or real!
    absoru_totkap(2)=0.
    if (calu.eq.1) unit=au_rho
    If (calu.eq.2) unit=au_toc ! RAPPEL  au_TOC=NBRE DE NOYAUX DE H PAR CM3

    scatel=scatel/unit
    do i=1,kkk
      absoru_totkap(i)=0.0
      scat(1)=scath(1)*absoru_znh(absoru2_nmeta+4)/unit
      scat(2)=scath(2)*absoru_znh(absoru2_nmeta+2)/unit
      do m = min,mmm
        ! les absoru_znh pour les metaux sont en cm-3*1.0e-18
        if ((m.ne.(absoru2_nmeta+1)).and.(m.ne.(absoru2_nmeta+3))) go to 4222

        if (m.eq.(absoru2_nmeta+1)) &
         zzkk(m,i)=zzk(m,i)*(absoru_znh(absoru2_nmeta+4)*au_pe*1.e-26)/unit
        if (m.eq.(absoru2_nmeta+3)) &
         zzkk(m,i)=zzk(m,i)*((absoru_znh(absoru2_nmeta+4)*1.e-19)*(absoru_znh(absoru2_nmeta+7)*1.0e-20))/unit
        go to 4221

        4222 continue
        zzkk(m,i)=zzk(m,i)*absoru_znh(m)/unit
        if (m.eq.(absoru2_nmeta+2)) zzkk(m,i)=zzkk(m,i)*au_pe

        4221 continue
        absoru_totkap(i)=absoru_totkap(i)+zzkk(m,i)
      end do
      absoru_totkap(i)=(absoru_totkap(i)+scatel+scat(1)+scat(2))

      !!__logging__
      !87 format ('wl,kkk,zzkk,totkap =>',f9.1,i2,1p12e10.2)
      !write (lll,87) wl,kkk,(zzkk(m,i),m=1,mmm),absoru_totkap(i)
      !call log_debug(lll)
    end do

    !!__logging__
    !89 format ('0SIG(E)='1PE11.4,' SIG(H)='E11.4,' SIG(H2)='E11.4,' DENSITE='E11.4,' NBR.NOYAU D H/CM3='E11.4, &
    !           ' LOG10PE='0PF5.2,' TETA='F5.2)
    !write (lll,89) scatel,scat(1),scat(2),au_rho,au_toc,zlpe,th
    !call log_debug(lll)


    !call log_debug(LEAVING//'absoru_()')
  end


  !-------------------------------------------------------------------------------
  !>
  !> Calcalutes the "Gaunth factor": multiplicative correction to the continuous absorption
  !> (i.e., a statistical weight)
  !>
  !> "DETERMINATION DU FACTEUR DE GAUNT BOUND FREE POUR L HYDROGENE" @ref Gaunth1930
  !>
  !> A.M COLLE   19/8/69

  subroutine gaunth (wl)
    implicit none
    real*8 wl, cond, delta, rk, zj, zp, zq
    integer i, j, jj, js
    au_jh = 0
    do 1410 i=1,au_jfz


      do j=1,19
        jj=j
        if (abs(wl-au_zlh(j)) .le. 0.5) go to 1335
        if (wl .lt. au_zlh(j)) go to 1333
      end do

      1333  continue
      if (i .ne. 2) go to 1334

      !

      ! CE N'EST PAS UNE DISCONTINUITE DE L'HYDROGENE
      !

      do j = 1,19
        au_g2d(2,j)=au_g2d(1,j)
      end do

      go to 1420

      1334 continue
      js=jj
      go to 1340

      1335 continue
      !
      ! C'EST UNE DISCONTINUITE DE L'HYDROGENE
      !
      au_jh=1
      if (i .eq. 1) go to 1334

      js = jj+1

      1340 continue
      au_jshyd(i) = js

      do 1410 j=js,19
        zj=j
        if (j.gt.7) go to 1400
        cond=au_zlh(j)-wl
        if (abs(cond).le.0.50) go to 1122
        if (cond.lt.0.0) go to 1410

        !=====
        ! assignment of au_g2d(i,j), alternative 1
        !=====

        zq=wl*j**2/cond
        rk=sqrt(zq)
        go to (1111,1113,1115,1117,1119,2000,2010), j

        ! menzel et pekeris=mon. not. vol. 96 p. 77 1935

        1111 delta=8.*rk/sqrt(zq+1.0)
        go to 1120

        1113 delta=(16.*rk*(3.*zq+4.)*(5.*zq+4.))/(zq+4.)**2.5
        go to 1120

        1115 delta=(24.*rk*((13.*zq+78.)*zq+81.)*((29.*zq+126.)*zq+81.))/(zq+9.)**4.5
        go to 1120

        1117 delta=32.*rk*(((197.*zq+3152.)*zq+13056.)*zq+12288.)*(((539.*zq+6800.)* &
                   zq+20736.)*zq+12288.)/(9.*(zq+16.)**6.5)
        go to 1120

        1119 delta=40.*rk*((((1083.*zq+36100.)*zq+372250.)*zq+1312500.)*zq+1171875.)* &
                   ((((3467.*zq+95700.)*zq+786250.)*zq+2062500.)*zq+1171875.)/(9.*(zq+25.)**8.5)
        go to 1120

        ! hagihara and soma=j.of astr. and geophys. japanese vol. 20 p. 59 1

        2000 zp=(zq+36.)**5.25
        delta=48.*rk*((((((38081.*zq+1953540.)*zq+3348086.e1)*zq+ &
        2262816.e2)*zq+5458752.e2)*zq+3023309.e2)/zp)*((((((10471.*zq+628260.)*zq+ &
        1290902.e1)*zq+1087085.e2)*zq+34992.0e4)*zq+3023309.e2)/25./zp)
        go to 1120

        2010 zp=(zq+49.)**6.25
        delta=56.*rk*(((((((56740.9*zq+5560608.)*zq+1993433.e2)*zq+3248060.e3)*zq+ &
        2428999.e4)*zq+7372604.e4)*zq+6228579.e4)/zp)*(((((((229742.5*zq+1968907.e1)* &
        zq+6067219.e2)*zq+8290160.e3)*zq+5002406.e4)*zq &
        +1144025.e5)*zq+6228579.e4)/20.25/zp)

        1120 au_g2d(i,j)=5.441398*rk*j*exp(-4.*rk*atan(zj/rk))*delta/ &
                         (sqrt(zq+zj**2)*(1.-exp(-6.283185*rk)))
        go to 1410


        !=====
        ! Assignment of au_G2D(I,J), alternative 2
        !=====
        1122 continue
        go to (1123,1125,1127,1129,1131,2020,2030), j
        1123 au_g2d(i,j)=0.7973
        go to 1410
        1125 au_g2d(i,j)=0.8762
        go to 1410
        1127 au_g2d(i,j)=0.9075
        go to 1410
        1129 au_g2d(i,j)=0.9247
        go to 1410
        1131 au_g2d(i,j)=0.9358
        go to 1410
        2020 au_g2d(i,j)=0.9436
        go to 1410
        2030 au_g2d(i,j)=0.9494
        go to 1410
        1400 au_g2d(i,j)=1.0

    1410 continue  ! This works as the "END DO" for two loops
    1420 return
  end


  !-------------------------------------------------------------------------------
  !> @todo issue ?what? ?doc?
  !>
  !> @verbatim
  !> HCBKTM=(H*C/K*T)*1.0E8
  !> 0.0010967876=CONSTANTE DE RYDBERG POUR H  *1.0E-8  ALLEN 1963
  !> 0.0043890867=CONSTANTE DE RYDBERG POUR HE+*1.0E-8  MOORE 1950 (HE4
  !> au_AHE =POUR HE 4*C/T**3
  !> au_AH  =POUR H   C*Z**4/T**3  AVEC Z=1
  !> au_AHEP=POUR HE+ C*Z**4/T**3  AVEC Z=2
  !> C=64*PI**4*ME*E**10/(3*RAC(3)*C*H**3*K**3)
  !> ME=9.10E-28,E**10=4.8E-10,K=1.38024E-16,H=6.6237E-27,C=2.99791E+10
  !> @endverbatim
  !>
  !> @author A.M COLLE   8/5/69

  subroutine tempa(wl,th,calth,callam)
    implicit none
    real*8 wl, th, comhe, hcbktm, uh, uhep
    integer j, k, l
    integer callam,calth

    if (calth.eq.2) go to 1001

    hcbktm  = 0.2854306e-3*th*1.0e8
    au_ahe  = 0.9717088e-12*th**3
    au_ah   = 0.2429272e-12*th**3
    au_ahep = 16.*au_ah
    au_uh1  = 1.096788e-3*hcbktm
    au_zemh = exp(-au_uh1)

    if (th.gt.1.4) go to 1001

    do j = 1,20
      uh=au_uh1/j**2
      au_zeuh(j)=exp(uh-au_uh1)/j**3
    enddo

    au_zeuh(20) = au_zeuh(20)*8000.  !> @todo ISSUE big why this (ask mt)? (MT): Why 20? Why 8000.?
    au_uhep1 = 4.389087e-3*hcbktm
    if (th .gt. 0.3) go to 5290

    do j=1,20
      uhep=au_uhep1/j**2
      au_zeuhep(j) = exp(uhep-au_uhep1)/j**3
    end do

    au_zeuhep(20) = au_zeuhep(20)*8000.

    5290 continue
    au_uhe1 = hcbktm/504.3
    au_zeuhe1 = exp(-au_uhe1)
    if (th .gt. 0.8) go to 1001

    comhe=-hcbktm*(1.0/au_zlhem(1))
    do k = 1,5
      au_zexpm(k)=exp(comhe+hcbktm*(1.0/au_zlhem(k)))*au_stwtm(k)
    end do

    do l=3,10
      au_zexp(l)= exp(comhe+hcbktm*(1.0/au_zlhe(l)))/l**3
    end do

    1001 if ((callam.eq.2).and.(calth.eq.2)) go to 5010

    au_ul = hcbktm/wl

    5010 return
  end



  !-------------------------------------------------------------------------------
  !> @ingroup gr_data
  !> SAHA's equation: ionization equilibrium: relative number of atoms in each
  !> ionization state
  !>
  !> @verbatim
  !> LOI DE SAHA=LOG((absoru2_NR+1)/absoru2_NR)*modeles_PE= -POT.ION.*TH+5/2*LOG(T)-0.4772+FONC
  !> LES FONCTIONS DE PARTITION (L0G(2UR+1)/UR) SONT INCLUSES DANS LES
  !> CONSTANTES AJOUTEES A TEMPOR POUR H ET HE LES AUTRES SONT LUES
  !> 31.303644,1.7200311,56.597541,125.26753,SONT RESPECTIVEMENT LES
  !> POTENTIELS D'IONISATION DE (H,H-,HE,HE+)*2.3025851
  !> @endverbatim
  !>
  !>     A.M COLLE   13/5/69

  SUBROUTINE SAHATH(TH)
    use read_most_files
    implicit none
    real*8 th, tempo, tempor
    integer i, j, n, nrr
    real*8, PARAMETER :: &
     POTION(6) = (/2-1.720031, 0.0, 0.0, 31.30364, 125.2675, -56.59754/), &
     C1(3) = (/0.0,-7.526612E-3,5.708280E-2/), &
     C2(3) = (/0.0,1.293852E-1,-1.823574E-1/), &
     C3(3) = (/0.0,-11.34061,-6.434060/), &
     C4(3) = (/0.0,28.85946,25.80507/)

    do n = 2,3
      au_zk(absoru2_nmeta+n)=exp(((c1(n)*th+c2(n))*th+c3(n))*th+c4(n))
    end do

    tempor=2.5*log(5040.39/th)
    tempo=tempor-1.098794
    do 2 n = 4,5
      au_zk(absoru2_nmeta+n)=potion(n)*th-tempo
      if (n .eq. 4) go to 12
      if  (au_zk(absoru2_nmeta+5).lt.100.0) go to 12

      au_zk(absoru2_nmeta+5)=0.0
      go to 2

      12 continue
      au_zk(absoru2_nmeta+n)=exp(-au_zk(absoru2_nmeta+n))
    2 continue

    do 2270 j=1,absoru2_nm
      nrr=absoru2_nr(j)
      do 2270 i=1,nrr
        au_zkm(j,i)=th*absoru2_xi(j,i)-absoru2_pf(j,i)-tempo
        if (au_zkm(j,i).lt.100.0) go to 2269
        au_zkm(j,i)=0.0
        go to 2270
        2269 au_zkm(j,i)=exp(-au_zkm(j,i))
    2270 continue

    tempo=tempor+0.2875929
    do n=1,6,5
      au_zk(absoru2_nmeta+n)=exp( potion(n)*th+tempo)
    end do
    return
  end




  !-------------------------------------------------------------------------------
  !> @ingroup gr_data
  !> CE SSP CALCULE LE COEFFICIENT D'ABSORPTION PAR ATOME NEUTRE POUR
  !> L'HYDROGENE ET L'HELIUM, ON SORT 2 VALEURS DE ZZK SI WL= A UNE
  !> DISCONTINUITE DE L'UN DE CES ABSORBANTS
  !>
  !> @author A.M COLLE  07/12/1970
  !>

  subroutine athyhe (wl,th,callam,zzk)
    use read_most_files
    use logging
    implicit none


    ! Note variable named "zk_"
    !   - local "zk" renamed to "zk_"
    !   - old COMMON "zk" so far is a module variable named au_zk
    real*8 althmb, althml, anu, any, bh, bhe, bhem, bhep, bkt, caeta, caro, difeta, &
     difro, dksq, fact, g3, gconst, rhog1, rhog2, rk, sigh, sighe, sighem, sighep, &
     stimu3, tempor, uk, wlm, zkas, zlamin, zleta1, zleta2, znl, znl1, zk_
    integer i, ie, indth, ir, j, je, jhyt, jj, jjs, jr, js, k, kk, kks, l, ll, lls, n
    integer callam, jhe, jhep, jhem
    real*8 wl, th
    real*8 :: tgaunt(5),trhog(5),opnu(46),zzk(11,2), expon(2)
    real*8, parameter ::                                  &
     expo(2) = (/-68.88230,-71.45087/),                   &
     cons(2) = (/3.3,3.6/),                               &
     cote(2) = (/3.136954e-23,8.195952e-24/),             &
     sniv(2) = (/0.55,0.485/),                            &
     cuk(2)  = (/0.3025,0.235225/),                       &
     an(2)   = (/0.3099204e-21, 0.22849203e-21/),         &
     c1(3)   = (/-2.850692e-2,-7.056869e-3,3.591294e-3/), &
     c2(3)   = (/0.2080816,0.1809394,-0.1959804/),        &
     c3(3)   = (/2.549101,-1.828635,4.233733/),           &
     c4(3)   = (/-14.97997,8.900841,-20.84862/),          &
     c5(3)   = (/0.0,-17.78231,0.0/),                     &
     c6(3)   = (/0.0,-7.89472e-2,0.0/)
    jhe  = 0
    jhep = 0
    jhem = 0
    if (callam.eq.1) indth = 0
    jhyt = 1

    ! 31.3213   = 157871.62/5040.39;
    ! 157871.62 = M*Z**2*E**4/(2*K*(H/2*PI)**
    ! M ET E SONT LA MASSE ET LA CHARGE DE L'ELECTRON,H ET K LES CONSTAN
    ! DE PLANCK ET DE BOLTZMANN
    gconst = 31.3213*th

    if (au_ul .lt. 100.0) go to 1333

    ! au_UL=H*NU/(K*T)

    au_STIMU=1.0
    GO TO 1334

    1333 au_stimu = 1.0-exp(-au_ul)
    1334 stimu3   = au_stimu/au_ul**3
    if (callam.eq.2) go to 1335

    znl = log(wl)
    zlamin = 1.0e8/wl
    do n = 1,2
      expon(n)=exp(expo(n)+cons(n)*znl)
    end do

    ! -- I --  H-
    ! H- GINGERICH: HARVARD JUIN 1964 (RESULTATS*1.0E-26)

    1335 if (th .ge. 0.3) go to 6060

    zzk(absoru2_nmeta+1,1)=0.0
    go to 6210

    6060 if ((callam.eq.2).and.(indth.eq.1)) go to 6100

    indth = 1
    if (wl.le.16419.0) go to 6070

    althmb = 0.0
    go to 6190

    6070 wlm = wl/1.0e3
    if (wl.gt.14200.0) go to 6090

    zkas =(((5.95244e-4*wlm-0.0204842)*wlm+0.164790)*wlm+0.178708)*wlm+0.680133e-2
    go to 6100

    6090 wlm=16.149-wlm
    zkas = ((0.273236e-2*wlm-0.411288e-1)*wlm+0.220190)*wlm**2+0.269818

    6100 fact = 1.0-exp((-th)*28.54310e+3/wl)

    !
    ! TRANSITION BOUND-FREE= GELTMAN APJ. VOL. 136 P. 935 1962
    !

    althmb = zkas*4.158e-1*th**2.5*exp(1.726*th)*fact

    !
    ! TRANSITION FREE-FREE=T L JOHN (OCT.1963)
    !

    6190 althml=(wl/1.0e6)*(((-5.939*th+11.934)*th-3.2062)+(wl/1.0e3)* &
     ((-0.34592*th+7.0355)*th-0.40192))+((0.027039*th-0.011493)*th+0.0053666)

    !> @todo ISSUE: check spill!!!!!!!!!!! if using index +1, perhaps I should dimension the relevant vectors with dimension MAX_absoru2_NMETA+1
    zzk(absoru2_nmeta+1,1) = althmb+althml

    ! -- II --  H2-
    ! H2- SOMMERVILLE: APJ. VOL. 139 P. 195 1963
    6210 if (th .lt. 0.5) go to 2050
    if (wl .ge. 3040.0) go to 2070

    2050 zzk(absoru2_nmeta+2,1)=0.0
    go to 2080

    2070 dksq=911.27/wl
    zzk(absoru2_nmeta+2,1)=(((0.09319*th+2.857-0.9316/th)/dksq-(2.6*th+6.831-4.993/th))/ &
     dksq+(35.29*th-9.804-10.62/th)-(74.52*th-62.48+0.4679/th)*dksq)*1.0e-29

    ! -- III --  H2+
    ! H2+ BATES: HARVARD JUIN 1964  (RESULTATS *1.0E+39)

    2080 if ((th.lt.0.25).or.((zlamin.lt.au_winv(1)).or.(zlamin.gt.au_winv(46)))) go to 1012

    bkt=3.19286e-2/th  ! BKT=K*T EN RYDBERGS POUR H2+

    do j=1,46
      opnu(j)=2.51e-3*au_grdm(j)*(exp(u1(j)/bkt)-(exp(-au_u2(j)/bkt)))
    end do

    do j = 1,46
      jj=j
      if (abs(zlamin-au_winv(j)) .le. 0.5) go to 1014
      if (zlamin .lt. au_winv(j)) go to 1015
    end do

    1014 zzk(absoru2_nmeta+3,1)=opnu(jj)
    go to 1016

    ! INTERPOLATION LINEAIRE
    1015 zzk(absoru2_nmeta+3,1)=(opnu(jj-1)*au_winv(jj)-opnu(jj)*au_winv(jj-1)+ &
     (opnu(jj)-opnu(jj-1))*zlamin)/(au_winv(jj)-au_winv(jj-1))
    go to 1016

    1012 zzk(absoru2_nmeta+3,1)=0.0

    ! CAS OU WL EST UNE DISCONTINUITE
    1016 if (au_jfz.ne.2) go to 1017
    do n = 1,3
      zzk(absoru2_nmeta+n,2)=zzk(absoru2_nmeta+n,1)
    end do

    ! -- IV --  H
    ! H UNSOLD (1955) PAGE 168
    ! FACTEUR DE GAUNT FREE-FREE POUR H=GRANT M.N.,VOL.118
    ! SYMBOLES CF VARDYA APJ.SUP. VOL. 8,P.277,1964
    1017  if (th.gt.1.4) go to 1809
    do 1855 k=1,4
      trhog(k)=sqrt(1.0+au_ul/au_yy(k))
      if (trhog(k).ge.1.01) go to 1820
      if (trhog(k).ne.1.0) tgaunt(k)=2.0/(trhog(k)-1.0)
      if (trhog(k).eq.1.0) go to 1856

      tgaunt(k)=0.5513289*log(tgaunt(k))  ! 0.5513289=SQRT(3)/PI
      go to 1855

      1856 tgaunt(k)=0.0
      go to 1855

      1820 if (trhog(k).le.1.8) go to 1830

      tempor=(trhog(k)-1.0)*sqrt(gconst/(au_yy(k)+au_ul))
      any=tempor**(-0.6666667)
      tgaunt(k)=(-0.01312*any+0.21775)*any+1.0
      go to 1855

      1830 tempor=0.2171473*log(gconst/(au_yy(k)+au_ul))  ! 0.2171473=0.434294482/2
      if ((tempor.lt.au_zletag(1)).or.(tempor.gt.au_zletag(18))) go to 1847

      ! INTERPOLATION A PARTIR DE LA TABLE 1 DE GRANT (1958)
      do ir=1,12
        jr=ir
        if (abs(trhog(k)-au_rhog(ir)).le.1.0e-4) go to 1836
        if  (trhog(k).lt.au_rhog(ir)) go to 1837
      end do

      1836 caro=1.0
      !
      ! INTERPOLATION SUR LOG(-ETA) SEULEMENT
      !
      go to 1838

      1837 rhog1=au_rhog(jr-1)
      caro=trhog(k)-rhog1

      1838  rhog2=au_rhog(jr)
      if (caro.eq.1.0) difro=1.0
      if (caro.ne.1.0) difro=rhog2-rhog1
      do ie=1,18
        je=ie
        if (abs(tempor-au_zletag(ie)).le.1.0e-4) go to 1846
        if  (tempor.lt.au_zletag(ie)) go to 1848
      end do

      1846 if (caro .eq. 1.0) go to 1850
      caeta=1.0

      !
      ! INTERPOLATION SUR au_RHO SEULEMENT
      !
      go to 1849

      1848 zleta1=au_zletag(je-1)
      caeta=tempor-zleta1

      1849  zleta2=au_zletag(je)
      if(caeta.eq.1.0)  difeta=1.0
      if(caeta.ne.1.0)  difeta=zleta2-zleta1
      go to 1851

      1850 tgaunt(k)=au_g3d(jr,je)
      go to 1855

      1851 tgaunt(k)=((au_g3d(jr-1,je-1)*(rhog2-trhog(k))+au_g3d(jr,je-1)*caro)* &
       (zleta2-tempor)+(au_g3d(jr,je)*caro+au_g3d(jr-1,je)*(rhog2-trhog(k)))*caeta)/ &
       difro/difeta
      go to 1855

      1847 continue
      call log_critical('0 on sort de la table de gff')
    1855 continue

    g3=0.0
    do k=1,4
      g3=g3+tgaunt(k)*au_aa(k)  ! G3: FACTEUR DE GAUNT FREE FREE
    end do
    go to 4199

    1809 zzk(absoru2_nmeta+4,1)=0.0
    jhyt=0

    4199 continue
    do 4200 i=1,au_jfz
      if (((i.eq.1).and.(jhyt.ne.0)).or.(au_jh.eq.1)) go to 4201
      !
      ! WL N'EST PAS UNE DISCONTINUITE DE H
      !
      if (i.eq.2) zzk(absoru2_nmeta+4,2)=zzk(absoru2_nmeta+4,1)
      go to 1451

      4201 sigh=0.0
      js=au_jshyd(i)
      do j=js,19
        sigh=sigh+au_g2d(i,j)*au_zeuh(j)  ! RAPPEL: au_G2D: FACTEUR DE GAUNT BOUND FREE
      end do

      bh=sigh+(au_zeuh(20)-(1.0-g3)*au_zemh)/(2*au_uh1)
      zzk(absoru2_nmeta+4,i)=au_ah*bh*stimu3

      !
      ! -- V -- HE+
      ! HE+  VARDYA APJ.SUP. VOL. 8,P.277,1964
      1451 if (th.gt.0.3) go to 1552
      sighep=0.0
      do j=1,19
        jj=j
        if (abs(wl-au_zlhep(j)).le.0.50) go to 1465
        if (wl.lt.au_zlhep(j)) go to 1463
      end do

      1463 jjs=jj
      go to 1470

      1465 jhep=1
      if (i.eq.1) go to 1463

      jjs=jj+1

      1470 if ((i.eq.1).or.(jhep.eq.1)) go to 1471

      !
      ! WL N'EST PAS UNE DISCONTINUITE DE HE+
      !

      zzk(absoru2_nmeta+5,2)=zzk(absoru2_nmeta+5,1)
      go to 1554

      1471 continue
      do jj=jjs,19
        sighep=sighep+au_zeuhep(jj)
      end do
      bhep=sighep+au_zeuhep(20)/(2*au_uhep1)
      zzk(absoru2_nmeta+5,i)=au_ahep*bhep*stimu3
      go to 1554

      1552 zzk(absoru2_nmeta+5,i)=0.0

      !
      ! -- VI -- HE
      ! HE  VARDYA: APJ. SUP. 80 VOL. 8 P. 277 JANVIER 1964
      1554 if (th.le.0.8) go to 5400
      zzk(absoru2_nmeta+6,i)=0.0
      go to 4200

      5400 if ((i.eq.2).and.(au_jh.eq.1)) go to 5872

      sighem=0.0
      sighe=0.0
      if ((wl-au_zlhem(5)).gt.0.50) go to 5740

      do k=1,5
        kk=k
        if (abs(wl-au_zlhem(k)).le.0.50) go to 5490
        if (wl.lt.au_zlhem(k)) go to 5470
      end do

      5470 kks=kk
      go to 5540

      5490 jhem=1
      if (i.eq.1) go to 5470

      kks=kk+1
      if (kks.gt.5) go to 5740

      5540 if ((jhem.eq.1).or.(i.eq.1)) go to 5541
      !
      ! WL N'EST PAS = A UNE VALEUR DE au_ZLHEM
      ! RAPPEL  au_ZLHEM=504,2601,3122,3422,3680 A.
      !
      go to 5741

      5541 continue
      do 5730 k=kks,5
        go to (5560,5560,5560,5620,5680),k

        5560 if (k.eq.2) znl1=znl

        if (k.ne.2) znl1=1.0
        anu=exp(((((c1(k)*znl+c2(k))*znl+c3(k))*znl+c4(k))*znl1+c5(k))*znl1+c6(k))*1.0e-18
        go to 5730

        5620 n=1

        !
        ! GOLDBERG APJ. VOL. 90 P. 414 1939 ET UNDERHILL PUB. COP. OBS. N0.
        !

        5621 if (abs(wl-au_zlhem(3+n)).gt.0.50) go to 5640
        ! NIVEAUX 4 A 7 DE HE1

        anu=an(n)/wl+expon(n)
        go to 5730

        5640 zk_=1.097224e-3*au_zlhem(3+n)*wl/(au_zlhem(3+n)-wl)
        rk=sqrt(zk_)
        uk=1.0+cuk(n)*zk_
        anu=(cote(n)/(wl*(1.0-exp(-6.283185*rk)))*(zk_/uk   )**6*((1.0+zk_)/ &
         uk)*((4.0+zk_)/uk)*exp(-4.0*rk*atan(1.0/(sniv(n)*rk))))+expon(n)
        go to 5730

        5680 n=2
        go to 5621

        sighem=sighem+anu*au_zexpm(k)
      5730 continue

      bhem=sighem*au_stimu
      go to 5741

      5740 bhem=0.0
      ! NIVEAUX 8 ET SQ (N.GE.3)
      5741 continue
      do l=3,9
        ll=l
        if (abs(wl-au_zlhe(l)).le.0.50) go to 5810
        if  (wl.lt.au_zlhe(l)) go to 5790
      end do

      5790 lls=ll
      go to 5860

      5810 jhe=1
      if (i.eq.1) go to 5790

      lls=ll+1

      5860 if ((i.eq.1).or.(jhe.eq.1)) go to 5861
      !
      ! WL N'EST PAS = A UNE VALEUR DE au_ZLHE
      !
      go to 5871

      5861 continue
      do l=lls,9
        sighe=sighe+au_zexp(l)*au_zeff4(l)
      end do
      bhe=sighe+(1807.240*au_zexp(10)-0.8072399*au_zeuhe1)/(2*au_uhe1)

      5871 zzk(absoru2_nmeta+6,i)=au_ahe*bhe*stimu3+bhem
      go to 4200

      !
      ! WL N'EST PAS UNE DISCONTINUITE DE HE
      !
      5872 zzk(absoru2_nmeta+6,2)=zzk(absoru2_nmeta+6,1)
    4200 continue
  end






  !-------------------------------------------------------------------------------
  !> Ionization degree by hydrogen atoms & electrons (???; to be confirmed) ISSUE
  !>
  !> SSP CALCULANT LES QUANTITES SUIVANTES: PARTH, au_PG, au_ZMU, au_RHO, au_TOC,
  !> au_AC, au_AC1, au_AC2, PHI, absoru_ZNH
  !>
  !> Reference: 'VARDYA' APJ VOL.133,P.107,1961
  !>
  !> @author A.M COLLE  18/01/1971
  !>
  !> @todo consider creating module variables to avoid passing parameter to subroutine

  subroutine ionipe(th,zlpe,calth)
    use read_most_files
    implicit none
    real*8 th, zlpe, any, cond, den, fun1, fun2, pa, parth, ph, phi, ppar, s, sigm1, &
     sigm2, sigm3, tempor, tp1, tp2, w1, w2, w3, w4, w5, w6
    integer i, j, nrr

    integer*4 calth
    real*8 kth
    dimension phi(30), &  ! PHI(J) = DEGRE D'IONIZATION DE LELEMENT J POUR MULTIPLE IONISATION
     pa(10)

    kth = 6.956948e-13/th  ! 6.956948E-13 = 1.38024E-16*5040.39
    au_pe=exp(zlpe*2.302585)
    sigm3=0.0

    do j=1,absoru2_nm
      nrr=absoru2_nr(j)
      sigm1=0.0
      sigm2=0.0
      pa(1)=1.0
      do i = 1,nrr
        if ((pa(i).le.0.0).or.(au_zkm(j,i).le.0.0)) go to 2375
        pa(i+1)=pa(i)*(au_zkm(j,i)/au_pe)
        sigm1=i*pa(i+1)+sigm1
        sigm2=sigm2+pa(i+1)
      end do

      2375 continue
      den=1.0+sigm2
      phi(j)=sigm1/den
      do i=1,nrr
        au_ac2(j,i)=pa(i)/den
      end do
      sigm3=sigm3+au_znu(j)*phi(j)
    end do

    if (calth.eq.2) go to 2390
    if (th.ge.0.25) go to 2382

    tempor=0.0
    any=0.0
    cond=0.0
    go to 2390

    2382 continue
    any=1.0/au_zk(absoru2_nmeta+3)
    tempor=1.0/au_zk(absoru2_nmeta+2)
    cond=1.0/au_zk(absoru2_nmeta+1)

    2390 continue
    w2=au_zk(absoru2_nmeta+4)/au_pe
    w1=w2*any
    w3=au_pe*cond
    w4=au_znu2*au_zk(absoru2_nmeta+6)*(au_pe+2*au_zk(absoru2_nmeta+5))/ &
       ((au_pe+au_zk(absoru2_nmeta+6))*au_pe+au_zk(absoru2_nmeta+6)*au_zk(absoru2_nmeta+5))+ &
       (au_znu3*sigm3)
    fun1=au_znu1*w1+2*(tempor+w1)*w4
    fun2=au_znu1*(w2-w3)+(1.0+w2+w3)*w4
    ph=2*au_znu1*au_pe/(fun2+sqrt(fun2**2+4*fun1*au_znu1*au_pe))
    absoru_znh(absoru2_nmeta+4)=ph/kth
    absoru_znh(absoru2_nmeta+2)=ph*tempor*absoru_znh(absoru2_nmeta+4)
    absoru_znh(absoru2_nmeta+1)=absoru_znh(absoru2_nmeta+4)*w3
    absoru_znh(absoru2_nmeta+7)=absoru_znh(absoru2_nmeta+4)*w2
    absoru_znh(absoru2_nmeta+3)=absoru_znh(absoru2_nmeta+7)*ph*any
    tp1=absoru_znh(absoru2_nmeta+1)+absoru_znh(absoru2_nmeta+4)+absoru_znh(absoru2_nmeta+7)
    tp2=absoru_znh(absoru2_nmeta+2)+absoru_znh(absoru2_nmeta+3)
    au_toc=2*tp2+tp1
    parth=au_toc/au_znu1 ! NBRE TOTAL DE NOYAUX PAR CM3
    ppar=(tp1+tp2+parth*(au_znu2+au_znu3))*kth
    au_pg=ppar+au_pe
    au_ac=au_pe/ppar
    w5=au_zk(absoru2_nmeta+6)/au_pe
    w6=au_zk(absoru2_nmeta+5)*w5/au_pe
    s=1.0+w5+w6
    au_ac1(1)=w2/(1.0+w2)
    au_ac1(2)=w6/s
    au_ac1(3)=w5/s
    absoru_znh(absoru2_nmeta+6)=au_znu2*parth/(1.0+w5+w6)
    absoru_znh(absoru2_nmeta+5)=absoru_znh(absoru2_nmeta+6)*w5
    au_rho=1.6602e-24*parth*au_zmuze  ! 1.6602E-24: MASSE DE L'UNITE DE POIDS
    au_zmu=au_rho*41904.28e+7/(th*au_pg)  ! 41904.275E+7: 8.313697E+7*5040.39, OU
                                          ! 8.313697e+7: constante des gaz
  end subroutine ionipe
end module absoru

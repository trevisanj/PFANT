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
!> Module ABSORU
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
!> Public: only subroutine ABSORU() and variable absoru_ZNH

module mod_absoru
  use absoru_data

  !=====
  ! Outputs
  !=====
  !> POPULATION POUR CHAQUE ABSORBANT M (H,HE OU METAUX). Calculated by absoru()
  real*8, dimension(12) :: absoru_znh  ! ISSUE Why 12?


  ! Down here all private
  ! vvvvvvvvvvvvvvvvvvvvv

  ! Subroutines
  private gaunth, tempa, sahath, athyhe, ionipe

  private

  ! TODO Identify outputs and inputs
  ! TODO CHECK ALL TYPES
  ! TODO CHECK ALL SIZES


  integer, dimension(2) :: au_jshyd
  integer au_jh, au_jfz


  real*8 au_ahe, au_ah, au_ahep, au_uh1, au_zemh, au_uhep1, au_uhe1, au_zeuhe1, &
   au_ul, &
   au_stimu, au_znu1, au_znu2, au_znu3, au_zmuze, &
   au_pe


   !> MASSE ATOMIQUE MOYENNE DES ELEMENTS PLUS LOURDS QUE L'HELIUM
   real*8 au_avm
   !> POIDS MOLECULAIRE MOYEN
   real*8 au_zmu
   !> PRESSION TOTALE EN DYNES/cm**2
   real*8 au_pg
   !> DENSITE (G-CM-3)
   real*8 au_rho
   !> NOMBRE DE NOYAUX D'HYDROGENE PAR cm^3
   real*8 au_toc 
   !> DEGRE D'IONISATION MOYEN
   real*8 au_ac


  !> FACTEUR DE GAUNT BOUND FREE
  real*8, DIMENSION(2, 19) :: au_G2D  
  real*8, DIMENSION(5) :: au_ZEXPM
  real*8, DIMENSION(10) :: au_ZEXP
  real*8, DIMENSION(20) :: au_ZEUH, au_ZEUHEP
  real*8, DIMENSION(11) :: au_ZK
  real*8 :: au_ZKM(30, 9)
  !> DEGRE D'IONISATION DES METAUX
  real*8 :: au_AC2(30, 9)

  !> @li au_AC1(1): DEGRE D'IONIZATION DE H
  !> @li au_AC1(2): DEGRE D'IONIZATION DE HE+
  !> @li au_AC1(3): DEGRE D'IONIZATION DE HE
  real*8, DIMENSION(3) :: au_AC1  

  real*8, DIMENSION(30) :: au_ZNU
  CHARACTER*80 LLL

CONTAINS

  !-------------------------------------------------------------------------------
  !> @ingroup data
  !> Calculates the "continuum absorption"
  !>
  !> Note1: 1/3 things that need to be changed to include scattering (other software
  !>       e.g. Upsalla already have this)
  !>
  !> Note2: 1/3 atmospheric models 50e6 cannot be calculates, would tyake months.
  !>        So, one idea is to include opacity model tables (Upsalla; MARCS model).
 
  subroutine absoru(wl,th,zlpe,callam,calth,calpe,calmet,calu,kkk,totkap)
    integer*4 calu,calmet,callam,calth,calpe,pmax,calsor
    dimension zzkk(11,2),totkap(2),dif(2,3),scath(2),zzk(11,2),scat(2)

    data dif /5.799e-13,8.14e-13,1.422e-6,1.28e-6,2.784,1.61/

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
    call athyhe (wl,th,calth,callam,zzk)

    9006 continue
    if (calmet.eq.1) go to 9003 ! issue line doing nothing

    9003 continue
    call ionipe (th,zlpe,calth,calmet)
    mm=absoru2_nmeta+1
    mmm=absoru2_nmeta+6
    scatel=9.559063e-13*au_pe*th
    ! 9.559063E-13=4.81815E-9/5040.39 ET 4.81815E-9=6.625E-25/1.38024E-1
    ! =ELECTRON SCATTERING/(K*T)  UNSOLD P. 180 1955

    !__logging__
    86 format ('0LAMBDA KKK   c1'7x'mg'7x'si1'7x'al1'8x'h-'7x'h2-'7x'h2+'9x'h'7x'he+'8x'he'5x'k total/',2a4)
    write (lll,86) (absoru2_iunite(i),i=1,2)
    call log_debug(lll)

    kkk=au_jfz
    min=mm

    do i=1,au_jfz
      do m=1,absoru2_nmeta
        zzkk(m,i)=0.0
      end do
    end do

    totkap(2)=0.
    if (calu.eq.1) unit=au_rho
    IF (CALU.EQ.2) UNIT=au_TOC ! RAPPEL  au_TOC=NBRE DE NOYAUX DE H PAR CM3

    scatel=scatel/unit
    do i=1,kkk
      totkap(i)=0.0
      scat(1)=scath(1)*absoru_znh(absoru2_nmeta+4)/unit
      scat(2)=scath(2)*absoru_znh(absoru2_nmeta+2)/unit
      do m = min,mmm
        ! les absoru_znh pour les metaux sont en cm-3*1.0e-18
        if ((m.ne.(absoru2_nmeta+1)).and.(m.ne.(absoru2_nmeta+3))) go to 4222

        if (m.eq.(absoru2_nmeta+1)) &
         zzkk(m,i)=zzk(m,i)*(absoru_znh(absoru2_nmeta+4)*au_pe*1.e-26)/unit
        if (m.eq.(absoru2_nmeta+3)) &
         zzkk(m,i)=zzk(m,i)*((absoru_znh(absoru2_nmeta+4)*1.e-19)*(absoru_znh(mmax+7)*1.0e-20))/unit
        go to 4221

        4222 continue
        zzkk(m,i)=zzk(m,i)*absoru_znh(m)/unit
        if (m.eq.(absoru2_nmeta+2)) zzkk(m,i)=zzkk(m,i)*au_pe

        4221 continue
        totkap(i)=totkap(i)+zzkk(m,i)
      end do
      totkap(i)=(totkap(i)+scatel+scat(1)+scat(2))

      !__logging__
      87 format (f9.1,i2,1p12e10.2)
      write (lll,87) wl,kkk,(zzkk(m,i),m=1,mmm),totkap(i)
      call log_debug(lll)
    end do

    !__logging__
    89 format ('0SIG(E)='1PE11.4,' SIG(H)='E11.4,' SIG(H2)='E11.4,' DENSITE='E11.4,' NBR.NOYAU D H/CM3='E11.4, &
               ' LOG10PE='0PF5.2,' TETA='F5.2)
    write (lll,89) scatel,scat(1),scat(2),au_rho,au_toc,zlpe,th
    call log_debug(lll)
  end


  !-------------------------------------------------------------------------------
  !>
  !> Calcalutes the "Gaunth factor": multiplicative correction to the continuous absorption
  !> (i.e., a statistical weight)
  !>
  !> "DETERMINATION DU FACTEUR DE GAUNT BOUND FREE POUR L HYDROGENE" @ref Gaunth1930 
  !>
  !> A.M COLLE   19/8/69

  SUBROUTINE GAUNTH (WL)
    au_JH = 0
    DO 1410 I=1,au_JFZ


      DO J=1,19
        JJ=J
        IF (ABS(WL-au_ZLH(J)) .LE. 0.5) GO TO 1335
        IF (WL .LT. au_ZLH(J)) GO TO 1333
      END DO

      1333  CONTINUE
      IF (I .NE. 2) GO TO 1334

      !

      ! CE N'EST PAS UNE DISCONTINUITE DE L'HYDROGENE
      !

      DO J = 1,19
        au_G2D(2,J)=au_G2D(1,J)
      END DO

      GO TO 1420

      1334 CONTINUE
      JS=JJ
      GO TO 1340

      1335 CONTINUE
      !
      ! C'EST UNE DISCONTINUITE DE L'HYDROGENE
      !
      au_JH=1
      IF (I .EQ. 1) GO TO 1334

      JS = JJ+1

      1340 CONTINUE
      au_JSHYD(I) = JS

      DO 1410 J=JS,19
        ZJ=J
        IF (J.GT.7) GO TO 1400
        COND=au_ZLH(J)-WL
        IF (ABS(COND).LE.0.50) GO TO 1122
        IF (COND.LT.0.0) GO TO 1410

        !=====
        ! Assignment of au_G2D(I,J), alternative 1
        !=====

        ZQ=WL*J**2/COND
        RK=SQRT(ZQ)
        GO TO (1111,1113,1115,1117,1119,2000,2010), J

        ! MENZEL ET PEKERIS=MON. NOT. VOL. 96 P. 77 1935

        1111 DELTA=8.*RK/SQRT(ZQ+1.0)
        GO TO 1120

        1113 DELTA=(16.*RK*(3.*ZQ+4.)*(5.*ZQ+4.))/(ZQ+4.)**2.5
        GO TO 1120

        1115 DELTA=(24.*RK*((13.*ZQ+78.)*ZQ+81.)*((29.*ZQ+126.)*ZQ+81.))/(ZQ+9.)**4.5
        GO TO 1120

        1117 DELTA=32.*RK*(((197.*ZQ+3152.)*ZQ+13056.)*ZQ+12288.)*(((539.*ZQ+6800.)* &
                   ZQ+20736.)*ZQ+12288.)/(9.*(ZQ+16.)**6.5)
        GO TO 1120

        1119 DELTA=40.*RK*((((1083.*ZQ+36100.)*ZQ+372250.)*ZQ+1312500.)*ZQ+1171875.)* &
                   ((((3467.*ZQ+95700.)*ZQ+786250.)*ZQ+2062500.)*ZQ+1171875.)/(9.*(ZQ+25.)**8.5)
        GO TO 1120

        ! HAGIHARA AND SOMA=J.OF ASTR. AND GEOPHYS. JAPANESE VOL. 20 P. 59 1

        2000 ZP=(ZQ+36.)**5.25
        DELTA=48.*RK*((((((38081.*ZQ+1953540.)*ZQ+3348086.E1)*ZQ+ &
        2262816.E2)*ZQ+5458752.E2)*ZQ+3023309.E2)/ZP)*((((((10471.*ZQ+628260.)*ZQ+ &
        1290902.E1)*ZQ+1087085.E2)*ZQ+34992.0E4)*ZQ+3023309.E2)/25./ZP)
        GO TO 1120

        2010 ZP=(ZQ+49.)**6.25
        DELTA=56.*RK*(((((((56740.9*ZQ+5560608.)*ZQ+1993433.E2)*ZQ+3248060.E3)*ZQ+ &
        2428999.E4)*ZQ+7372604.E4)*ZQ+6228579.E4)/ZP)*(((((((229742.5*ZQ+1968907.E1)* &
        ZQ+6067219.E2)*ZQ+8290160.E3)*ZQ+5002406.E4)*ZQ &
        +1144025.E5)*ZQ+6228579.E4)/20.25/ZP)

        1120 au_G2D(I,J)=5.441398*RK*J*EXP(-4.*RK*ATAN(ZJ/RK))*DELTA/ &
                         (SQRT(ZQ+ZJ**2)*(1.-EXP(-6.283185*RK)))
        GO TO 1410


        !=====
        ! Assignment of au_G2D(I,J), alternative 2
        !=====
        1122 CONTINUE
        GO TO (1123,1125,1127,1129,1131,2020,2030), J
        1123 au_G2D(I,J)=0.7973
        GO TO 1410
        1125 au_G2D(I,J)=0.8762
        GO TO 1410
        1127 au_G2D(I,J)=0.9075
        GO TO 1410
        1129 au_G2D(I,J)=0.9247
        GO TO 1410
        1131 au_G2D(I,J)=0.9358
        GO TO 1410
        2020 au_G2D(I,J)=0.9436
        GO TO 1410
        2030 au_G2D(I,J)=0.9494
        GO TO 1410
        1400 au_G2D(I,J)=1.0

    1410 CONTINUE  ! This works as the "END DO" for two loops
    1420 RETURN
  END


  !-------------------------------------------------------------------------------
  !> @todo ISSUE WHAT
  !>
  !> A.M COLLE   8/5/69
  !>
  !> HCBKTM=(H*C/K*T)*1.0E8
  !> 0.0010967876=CONSTANTE DE RYDBERG POUR H  *1.0E-8  ALLEN 1963
  !> 0.0043890867=CONSTANTE DE RYDBERG POUR HE+*1.0E-8  MOORE 1950 (HE4
  !> au_AHE =POUR HE 4*C/T**3
  !> au_AH  =POUR H   C*Z**4/T**3  AVEC Z=1
  !> au_AHEP=POUR HE+ C*Z**4/T**3  AVEC Z=2
  !> C=64*PI**4*ME*E**10/(3*RAC(3)*C*H**3*K**3)
  !> ME=9.10E-28,E**10=4.8E-10,K=1.38024E-16,H=6.6237E-27,C=2.99791E+10
  !
  SUBROUTINE TEMPA(WL,TH,CALTH,CALLAM)
    INTEGER*4 CALLAM,CALTH

    IF (CALTH.EQ.2) GO TO 1001

    HCBKTM  = 0.2854306E-3*TH*1.0E8
    au_AHE  = 0.9717088E-12*TH**3
    au_AH   = 0.2429272E-12*TH**3
    au_AHEP = 16.*au_AH
    au_UH1  = 1.096788E-3*HCBKTM
    au_ZEMH = EXP(-au_UH1)

    IF (TH.GT.1.4) GO TO 1001

    DO J = 1,20
      UH=au_UH1/J**2
      au_ZEUH(J)=EXP(UH-au_UH1)/J**3
    ENDDO

    au_ZEUH(20) = au_ZEUH(20)*8000.  ! ISSUE why this (ask MT)?
    au_UHEP1 = 4.389087E-3*HCBKTM
    IF (TH .GT. 0.3) GO TO 5290

    DO J=1,20
      UHEP=au_UHEP1/J**2
      au_ZEUHEP(J) = EXP(UHEP-au_UHEP1)/J**3
    END DO

    au_ZEUHEP(20) = au_ZEUHEP(20)*8000.

    5290 CONTINUE
    au_UHE1 = HCBKTM/504.3
    au_ZEUHE1 = EXP(-au_UHE1)
    IF (TH .GT. 0.8) GO TO 1001

    COMHE=-HCBKTM*(1.0/au_ZLHEM(1))
    DO K = 1,5
      au_ZEXPM(K)=EXP(COMHE+HCBKTM*(1.0/au_ZLHEM(K)))*au_STWTM(K)
    END DO

    DO L=3,10
      au_ZEXP(L)= EXP(COMHE+HCBKTM*(1.0/au_ZLHE(L)))/L**3
    END DO

    1001 IF ((CALLAM.EQ.2).AND.(CALTH.EQ.2)) GO TO 5010

    au_UL = HCBKTM/WL

    5010 RETURN
  END



  !-------------------------------------------------------------------------------
  !> SAHA's equation: ionization equilibrium: relative number of atoms in each
  !> ionization state
  !>
  !> LOI DE SAHA=LOG((absoru2_NR+1)/absoru2_NR)*modeles_PE= -POT.ION.*TH+5/2*LOG(T)-0.4772+FONC
  !> LES FONCTIONS DE PARTITION (L0G(2UR+1)/UR) SONT INCLUSES DANS LES
  !> CONSTANTES AJOUTEES A TEMPOR POUR H ET HE LES AUTRES SONT LUES
  !> 31.303644,1.7200311,56.597541,125.26753,SONT RESPECTIVEMENT LES
  !> POTENTIELS D'IONISATION DE (H,H-,HE,HE+)*2.3025851
  !>
  !>     A.M COLLE   13/5/69

  SUBROUTINE SAHATH(TH)
    real*8, PARAMETER :: POTION(6) = (/2-1.720031, 0.0, 0.0, 31.30364, 125.2675, -56.59754/)
    real*8, PARAMETER :: C1(3) = (/0.0,-7.526612E-3,5.708280E-2/)
    real*8, PARAMETER :: C2(3) = (/0.0,1.293852E-1,-1.823574E-1/)
    real*8, PARAMETER :: C3(3) = (/0.0,-11.34061,-6.434060/)
    real*8, PARAMETER :: C4(3) = (/0.0,28.85946,25.80507/)

    DO N = 2,3
      au_ZK(absoru2_NMETA+N)=EXP(((C1(N)*TH+C2(N))*TH+C3(N))*TH+C4(N))
    END DO

    TEMPOR=2.5*ALOG(5040.39/TH)
    TEMPO=TEMPOR-1.098794
    DO 2 N = 4,5
      au_ZK(absoru2_NMETA+N)=POTION(N)*TH-TEMPO
      IF (N .EQ. 4) GO TO 12
      IF  (au_ZK(absoru2_NMETA+5).LT.100.0) GO TO 12

      au_ZK(absoru2_NMETA+5)=0.0
      GO TO 2

      12 CONTINUE
      au_ZK(absoru2_NMETA+N)=EXP(-au_ZK(absoru2_NMETA+N))
    2 CONTINUE

    DO 2270 J=1,absoru2_NM
      NRR=absoru2_NR(J)
      DO 2270 I=1,NRR
        au_ZKM(J,I)=TH*absoru2_XI(J,I)-absoru2_PF(J,I)-TEMPO
        IF (au_ZKM(J,I).LT.100.0) GO TO 2269
        au_ZKM(J,I)=0.0
        GO TO 2270
        2269 au_ZKM(J,I)=EXP(-au_ZKM(J,I))
    2270 CONTINUE
    TEMPO=TEMPOR+0.2875929
    DO N=1,6,5
      au_ZK(absoru2_NMETA+N)=EXP( POTION(N)*TH+TEMPO)
    END DO
    RETURN
  END




  !-------------------------------------------------------------------------------
  !> CE SSP CALCULE LE COEFFICIENT D'ABSORPTION PAR ATOME NEUTRE POUR
  !> L'HYDROGENE ET L'HELIUM, ON SORT 2 VALEURS DE ZZK SI WL= A UNE
  !> DISCONTINUITE DE L'UN DE CES ABSORBANTS
  !>
  !> A.M COLLE  07/12/1970

  SUBROUTINE ATHYHE (WL,TH,CALTH,CALLAM,ZZK)
    USE READ_FILES
    IMPLICIT NONE
    INTEGER*4 CALLAM,CALTH, JHE, JHEP, JHEM
    REAL*8 WL, TH
    REAL*8 :: TGAUNT(5),TRHOG(5),OPNU(46),ZZK(11,2), EXPON(2)
    REAL*8, PARAMETER ::                                  &
     EXPO(2) = (/-68.88230,-71.45087/),                   &
     CONS(2) = (/3.3,3.6/),                               &
     COTE(2) = (/3.136954E-23,8.195952E-24/),             &
     SNIV(2) = (/0.55,0.485/),                            &
     CUK(2)  = (/0.3025,0.235225/),                       &
     AN(2)   = (/0.3099204E-21, 0.22849203E-21/),         &
     C1(3)   = (/-2.850692E-2,-7.056869E-3,3.591294E-3/), &
     C2(3)   = (/0.2080816,0.1809394,-0.1959804/),        &
     C3(3)   = (/2.549101,-1.828635,4.233733/),           &
     C4(3)   = (/-14.97997,8.900841,-20.84862/),          &
     C5(3)   = (/0.0,-17.78231,0.0/),                     &
     C6(3)   = (/0.0,-7.89472E-2,0.0/)
    JHE  = 0
    JHEP = 0
    JHEM = 0
    IF (CALLAM.EQ.1) INDTH = 0
    JHYT = 1

    ! 31.3213   = 157871.62/5040.39;
    ! 157871.62 = M*Z**2*E**4/(2*K*(H/2*PI)**
    ! M ET E SONT LA MASSE ET LA CHARGE DE L'ELECTRON,H ET K LES CONSTAN
    ! DE PLANCK ET DE BOLTZMANN
    GCONST = 31.3213*TH

    IF (au_UL .LT. 100.0) GO TO 1333

    ! au_UL=H*NU/(K*T)
    au_STIMU=1.0
    GO TO 1334

    1333 au_STIMU = 1.0-EXP(-au_UL)
    1334 STIMU3   = au_STIMU/au_UL**3
    IF (CALLAM.EQ.2) GO TO 1335

    ZNL = ALOG(WL)
    ZLAMIN = 1.0E8/WL
    DO N = 1,2
      EXPON(N)=EXP(EXPO(N)+CONS(N)*ZNL)
    END DO

    ! -- I --  H-
    ! H- GINGERICH: HARVARD JUIN 1964 (RESULTATS*1.0E-26)

    1335 IF (TH .GE. 0.3) GO TO 6060

    ZZK(absoru2_NMETA+1,1)=0.0
    GO TO 6210

    6060 IF ((CALLAM.EQ.2).AND.(INDTH.EQ.1)) GO TO 6100

    INDTH = 1
    IF (WL.LE.16419.0) GO TO 6070

    ALTHMB = 0.0
    GO TO 6190

    6070 WLM = WL/1.0E3
    IF (WL.GT.14200.0) GO TO 6090

    ZKAS =(((5.95244E-4*WLM-0.0204842)*WLM+0.164790)*WLM+0.178708)*WLM+0.680133E-2
    GO TO 6100

    6090 WLM=16.149-WLM
    ZKAS = ((0.273236E-2*WLM-0.411288E-1)*WLM+0.220190)*WLM**2+0.269818

    6100 FACT = 1.0-EXP((-TH)*28.54310E+3/WL)

    !
    ! TRANSITION BOUND-FREE= GELTMAN APJ. VOL. 136 P. 935 1962
    !

    ALTHMB = ZKAS*4.158E-1*TH**2.5*EXP(1.726*TH)*FACT

    !
    ! TRANSITION FREE-FREE=T L JOHN (OCT.1963)
    !

    6190 ALTHML=(WL/1.0E6)*(((-5.939*TH+11.934)*TH-3.2062)+(WL/1.0E3)* &
     ((-0.34592*TH+7.0355)*TH-0.40192))+((0.027039*TH-0.011493)*TH+0.0053666)

    ! ISSUE: check spill!!!!!!!!!!! if using index +1, perhaps I should dimension the relevant vectors with dimension MAX_absoru2_NMETA+1
    ZZK(absoru2_NMETA+1,1) = ALTHMB+ALTHML

    ! -- II --  H2-
    ! H2- SOMMERVILLE: APJ. VOL. 139 P. 195 1963
    6210 IF (TH .LT. 0.5) GO TO 2050
    IF (WL .GE. 3040.0) GO TO 2070

    2050 ZZK(absoru2_NMETA+2,1)=0.0
    GO TO 2080

    2070 DKSQ=911.27/WL
    ZZK(absoru2_NMETA+2,1)=(((0.09319*TH+2.857-0.9316/TH)/DKSQ-(2.6*TH+6.831-4.993/TH))/ &
     DKSQ+(35.29*TH-9.804-10.62/TH)-(74.52*TH-62.48+0.4679/TH)*DKSQ)*1.0E-29

    ! -- III --  H2+
    ! H2+ BATES: HARVARD JUIN 1964  (RESULTATS *1.0E+39)

    2080 IF ((TH.LT.0.25).OR.((ZLAMIN.LT.au_WINV(1)).OR.(ZLAMIN.GT.au_WINV(46)))) GO TO 1012

    BKT=3.19286E-2/TH  ! BKT=K*T EN RYDBERGS POUR H2+

    DO J=1,46
      OPNU(J)=2.51E-3*au_GRDM(J)*(EXP(U1(J)/BKT)-(EXP(-au_U2(J)/BKT)))
    END DO

    DO J = 1,46
      JJ=J
      IF (ABS(ZLAMIN-au_WINV(J)) .LE. 0.5) GO TO 1014
      IF (ZLAMIN .LT. au_WINV(J)) GO TO 1015
    END DO

    1014 ZZK(absoru2_NMETA+3,1)=OPNU(JJ)
    GO TO 1016

    ! INTERPOLATION LINEAIRE
    1015 ZZK(absoru2_NMETA+3,1)=(OPNU(JJ-1)*au_WINV(JJ)-OPNU(JJ)*au_WINV(JJ-1)+ &
     (OPNU(JJ)-OPNU(JJ-1))*ZLAMIN)/(au_WINV(JJ)-au_WINV(JJ-1))
    GO TO 1016

    1012 ZZK(absoru2_NMETA+3,1)=0.0

    ! CAS OU WL EST UNE DISCONTINUITE
    1016 IF (au_JFZ.NE.2) GO TO 1017
    DO N = 1,3
      ZZK(absoru2_NMETA+N,2)=ZZK(absoru2_NMETA+N,1)
    END DO

    ! -- IV --  H
    ! H UNSOLD (1955) PAGE 168
    ! FACTEUR DE GAUNT FREE-FREE POUR H=GRANT M.N.,VOL.118
    ! SYMBOLES CF VARDYA APJ.SUP. VOL. 8,P.277,1964
    1017  IF (TH.GT.1.4) GO TO 1809
    DO 1855 K=1,4
      TRHOG(K)=SQRT(1.0+au_UL/au_YY(K))
      IF (TRHOG(K).GE.1.01) GO TO 1820
      IF (TRHOG(K).NE.1.0) TGAUNT(K)=2.0/(TRHOG(K)-1.0)
      IF (TRHOG(K).EQ.1.0) GO TO 1856

      TGAUNT(K)=0.5513289*ALOG(TGAUNT(K))  ! 0.5513289=SQRT(3)/PI
      GO TO 1855

      1856 TGAUNT(K)=0.0
      GO TO 1855

      1820 IF (TRHOG(K).LE.1.8) GO TO 1830

      TEMPOR=(TRHOG(K)-1.0)*SQRT(GCONST/(au_YY(K)+au_UL))
      ANY=TEMPOR**(-0.6666667)
      TGAUNT(K)=(-0.01312*ANY+0.21775)*ANY+1.0
      GO TO 1855

      1830 TEMPOR=0.2171473*ALOG(GCONST/(au_YY(K)+au_UL))  ! 0.2171473=0.434294482/2
      IF ((TEMPOR.LT.au_ZLETAG(1)).OR.(TEMPOR.GT.au_ZLETAG(18))) GO TO 1847

      ! INTERPOLATION A PARTIR DE LA TABLE 1 DE GRANT (1958)
      DO IR=1,12
        JR=IR
        IF (ABS(TRHOG(K)-au_RHOG(IR)).LE.1.0E-4) GO TO 1836
        IF  (TRHOG(K).LT.au_RHOG(IR)) GO TO 1837
      END DO

      1836 CARO=1.0
      !
      ! INTERPOLATION SUR LOG(-ETA) SEULEMENT
      !
      GO TO 1838

      1837 RHOG1=au_RHOG(JR-1)
      CARO=TRHOG(K)-RHOG1

      1838  RHOG2=au_RHOG(JR)
      IF (CARO.EQ.1.0) DIFRO=1.0
      IF (CARO.NE.1.0) DIFRO=RHOG2-RHOG1
      DO IE=1,18
        JE=IE
        IF (ABS(TEMPOR-au_ZLETAG(IE)).LE.1.0E-4) GO TO 1846
        IF  (TEMPOR.LT.au_ZLETAG(IE)) GO TO 1848
      END DO

      1846 IF (CARO .EQ. 1.0) GO TO 1850
      CAETA=1.0

      !
      ! INTERPOLATION SUR au_RHO SEULEMENT
      !
      GO TO 1849

      1848 ZLETA1=au_ZLETAG(JE-1)
      CAETA=TEMPOR-ZLETA1

      1849  ZLETA2=au_ZLETAG(JE)
      IF(CAETA.EQ.1.0)  DIFETA=1.0
      IF(CAETA.NE.1.0)  DIFETA=ZLETA2-ZLETA1
      GO TO 1851

      1850 TGAUNT(K)=au_G3D(JR,JE)
      GO TO 1855

      1851 TGAUNT(K)=((au_G3D(JR-1,JE-1)*(RHOG2-TRHOG(K))+au_G3D(JR,JE-1)*CARO)* &
       (ZLETA2-TEMPOR)+(au_G3D(JR,JE)*CARO+au_G3D(JR-1,JE)*(RHOG2-TRHOG(K)))*CAETA)/ &
       DIFRO/DIFETA
      GO TO 1855

      1847 CALL LOG_CRITICAL('0 ON SORT DE LA TABLE DE GFF')
    1855 CONTINUE

    G3=0.0
    DO K=1,4
      G3=G3+TGAUNT(K)*au_AA(K)  ! G3: FACTEUR DE GAUNT FREE FREE
    END DO
    GO TO 4199

    1809 ZZK(absoru2_NMETA+4,1)=0.0
    JHYT=0

    4199 CONTINUE
    DO 4200 I=1,au_JFZ
      IF (((I.EQ.1).AND.(JHYT.NE.0)).OR.(au_JH.EQ.1)) GO TO 4201
      !
      ! WL N'EST PAS UNE DISCONTINUITE DE H
      !
      IF (I.EQ.2) ZZK(absoru2_NMETA+4,2)=ZZK(absoru2_NMETA+4,1)
      GO TO 1451

      4201 SIGH=0.0
      JS=au_JSHYD(I)
      DO J=JS,19
        SIGH=SIGH+au_G2D(I,J)*au_ZEUH(J)  ! RAPPEL: au_G2D: FACTEUR DE GAUNT BOUND FREE
      END DO

      BH=SIGH+(au_ZEUH(20)-(1.0-G3)*au_ZEMH)/(2*au_UH1)
      ZZK(absoru2_NMETA+4,I)=au_AH*BH*STIMU3

      !
      ! -- V -- HE+
      ! HE+  VARDYA APJ.SUP. VOL. 8,P.277,1964
      1451 IF (TH.GT.0.3) GO TO 1552
      SIGHEP=0.0
      DO J=1,19
        JJ=J
        IF (ABS(WL-au_ZLHEP(J)).LE.0.50) GO TO 1465
        IF (WL.LT.au_ZLHEP(J)) GO TO 1463
      END DO

      1463 JJS=JJ
      GO TO 1470

      1465 JHEP=1
      IF (I.EQ.1) GO TO 1463

      JJS=JJ+1

      1470 IF ((I.EQ.1).OR.(JHEP.EQ.1)) GO TO 1471

      !
      ! WL N'EST PAS UNE DISCONTINUITE DE HE+
      !

      ZZK(absoru2_NMETA+5,2)=ZZK(absoru2_NMETA+5,1)
      GO TO 1554

      1471 CONTINUE
      DO JJ=JJS,19
        SIGHEP=SIGHEP+au_ZEUHEP(JJ)
      END DO
      BHEP=SIGHEP+au_ZEUHEP(20)/(2*au_UHEP1)
      ZZK(absoru2_NMETA+5,I)=au_AHEP*BHEP*STIMU3
      GO TO 1554

      1552 ZZK(absoru2_NMETA+5,I)=0.0

      !
      ! -- VI -- HE
      ! HE  VARDYA: APJ. SUP. 80 VOL. 8 P. 277 JANVIER 1964
      1554 IF (TH.LE.0.8) GO TO 5400
      ZZK(absoru2_NMETA+6,I)=0.0
      GO TO 4200

      5400 IF ((I.EQ.2).AND.(au_JH.EQ.1)) GO TO 5872

      SIGHEM=0.0
      SIGHE=0.0
      IF ((WL-au_ZLHEM(5)).GT.0.50) GO TO 5740

      DO K=1,5
        KK=K
        IF (ABS(WL-au_ZLHEM(K)).LE.0.50) GO TO 5490
        IF (WL.LT.au_ZLHEM(K)) GO TO 5470
      END DO

      5470 KKS=KK
      GO TO 5540

      5490 JHEM=1
      IF (I.EQ.1) GO TO 5470

      KKS=KK+1
      IF (KKS.GT.5) GO TO 5740

      5540 IF ((JHEM.EQ.1).OR.(I.EQ.1)) GO TO 5541
      !
      ! WL N'EST PAS = A UNE VALEUR DE au_ZLHEM
      ! RAPPEL  au_ZLHEM=504,2601,3122,3422,3680 A.
      !
      GO TO 5741

      5541 CONTINUE
      DO 5730 K=KKS,5
        GO TO (5560,5560,5560,5620,5680),K

        5560 IF (K.EQ.2) ZNL1=ZNL

        IF (K.NE.2) ZNL1=1.0
        ANU=EXP(((((C1(K)*ZNL+C2(K))*ZNL+C3(K))*ZNL+C4(K))*ZNL1+C5(K))*ZNL1+C6(K))*1.0E-18
        GO TO 5730

        5620 N=1

        !
        ! GOLDBERG APJ. VOL. 90 P. 414 1939 ET UNDERHILL PUB. COP. OBS. N0.
        !

        5621 IF (ABS(WL-au_ZLHEM(3+N)).GT.0.50) GO TO 5640
        ! NIVEAUX 4 A 7 DE HE1

        ANU=AN(N)/WL+EXPON(N)
        GO TO 5730

        5640 au_ZK=1.097224E-3*au_ZLHEM(3+N)*WL/(au_ZLHEM(3+N)-WL)
        RK=SQRT(au_ZK)
        UK=1.0+CUK(N)*au_ZK
        ANU=(COTE(N)/(WL*(1.0-EXP(-6.283185*RK)))*(au_ZK/UK   )**6*((1.0+au_ZK)/ &
         UK)*((4.0+au_ZK)/UK)*EXP(-4.0*RK*ATAN(1.0/(SNIV(N)*RK))))+EXPON(N)
        GO TO 5730

        5680 N=2
        GO TO 5621

        SIGHEM=SIGHEM+ANU*au_ZEXPM(K)
      5730 CONTINUE

      BHEM=SIGHEM*au_STIMU
      GO TO 5741

      5740 BHEM=0.0
      ! NIVEAUX 8 ET SQ (N.GE.3)
      5741 CONTINUE
      DO L=3,9
        LL=L
        IF (ABS(WL-au_ZLHE(L)).LE.0.50) GO TO 5810
        IF  (WL.LT.au_ZLHE(L)) GO TO 5790
      END DO

      5790 LLS=LL
      GO TO 5860

      5810 JHE=1
      IF (I.EQ.1) GO TO 5790

      5840 LLS=LL+1

      5860 IF ((I.EQ.1).OR.(JHE.EQ.1)) GO TO 5861
      !
      ! WL N'EST PAS = A UNE VALEUR DE au_ZLHE
      !
      GO TO 5871

      5861 CONTINUE
      DO L=LLS,9
        SIGHE=SIGHE+au_ZEXP(L)*au_ZEFF4(L)
      END DO
      BHE=SIGHE+(1807.240*au_ZEXP(10)-0.8072399*au_ZEUHE1)/(2*au_UHE1)

      5871 ZZK(absoru2_NMETA+6,I)=au_AHE*BHE*STIMU3+BHEM
      GO TO 4200

      !
      ! WL N'EST PAS UNE DISCONTINUITE DE HE
      !
      5872 ZZK(absoru2_NMETA+6,2)=ZZK(absoru2_NMETA+6,1)
    4200 CONTINUE
  END






  !-------------------------------------------------------------------------------
  !> Ionization degree by hydrogen atoms & electrons (???; to be confirmed) ISSUE
  !>
  !> SSP CALCULANT LES QUANTITES SUIVANTES: PARTH, au_PG, au_ZMU, au_RHO, au_TOC,
  !> au_AC, au_AC1, au_AC2, PHI, absoru_ZNH
  !>
  !> Reference: 'VARDYA' APJ VOL.133,P.107,1961
  !>
  !> A.M COLLE  18/01/1971

  SUBROUTINE IONIPE(TH,ZLPE,CALTH,CALMET)
    INTEGER*4 CALTH,CALMET
    REAL KTH
    DIMENSION PHI(30), &  ! PHI(J) = DEGRE D'IONIZATION DE LELEMENT J POUR MULTIPLE IONISATION
     PA(10)

    KTH = 6.956948E-13/TH  ! 6.956948E-13 = 1.38024E-16*5040.39
    au_PE=EXP(ZLPE*2.302585)
    SIGM3=0.0

    DO J=1,absoru2_NM
      NRR=absoru2_NR(J)
      SIGM1=0.0
      SIGM2=0.0
      PA(1)=1.0
      DO I = 1,NRR
        IF ((PA(I).LE.0.0).OR.(au_ZKM(J,I).LE.0.0)) GO TO 2375
        PA(I+1)=PA(I)*(au_ZKM(J,I)/au_PE)
        SIGM1=I*PA(I+1)+SIGM1
        SIGM2=SIGM2+PA(I+1)
      END DO

      2375 CONTINUE
      DEN=1.0+SIGM2
      PHI(J)=SIGM1/DEN
      DO I=1,NRR
        au_AC2(J,I)=PA(I)/DEN
      END DO
      SIGM3=SIGM3+au_ZNU(J)*PHI(J)
    END DO

    IF (CALTH.EQ.2) GO TO 2390
    IF (TH.GE.0.25) GO TO 2382

    TEMPOR=0.0
    ANY=0.0
    COND=0.0
    GO TO 2390

    2382 CONTINUE
    ANY=1.0/au_ZK(absoru2_NMETA+3)
    TEMPOR=1.0/au_ZK(absoru2_NMETA+2)
    COND=1.0/au_ZK(absoru2_NMETA+1)

    2390 CONTINUE
    W2=au_ZK(absoru2_NMETA+4)/au_PE
    W1=W2*ANY
    W3=au_PE*COND
    W4=au_ZNU2*au_ZK(absoru2_NMETA+6)*(au_PE+2*au_ZK(absoru2_NMETA+5))/ &
       ((au_PE+au_ZK(absoru2_NMETA+6))*au_PE+au_ZK(absoru2_NMETA+6)*au_ZK(absoru2_NMETA+5))+ &
       (au_ZNU3*SIGM3)
    FUN1=au_ZNU1*W1+2*(TEMPOR+W1)*W4
    FUN2=au_ZNU1*(W2-W3)+(1.0+W2+W3)*W4
    PH=2*au_ZNU1*au_PE/(FUN2+SQRT(FUN2**2+4*FUN1*au_ZNU1*au_PE))
    absoru_ZNH(absoru2_NMETA+4)=PH/KTH
    absoru_ZNH(absoru2_NMETA+2)=PH*TEMPOR*absoru_ZNH(absoru2_NMETA+4)
    absoru_ZNH(absoru2_NMETA+1)=absoru_ZNH(absoru2_NMETA+4)*W3
    absoru_ZNH(absoru2_NMETA+7)=absoru_ZNH(absoru2_NMETA+4)*W2
    absoru_ZNH(absoru2_NMETA+3)=absoru_ZNH(absoru2_NMETA+7)*PH*ANY
    TP1=absoru_ZNH(absoru2_NMETA+1)+absoru_ZNH(absoru2_NMETA+4)+absoru_ZNH(absoru2_NMETA+7)
    TP2=absoru_ZNH(absoru2_NMETA+2)+absoru_ZNH(absoru2_NMETA+3)
    au_TOC=2*TP2+TP1
    PARTH=au_TOC/au_ZNU1 ! NBRE TOTAL DE NOYAUX PAR CM3
    PPAR=(TP1+TP2+PARTH*(au_ZNU2+au_ZNU3))*KTH
    au_PG=PPAR+au_PE
    au_AC=au_PE/PPAR
    W5=au_ZK(absoru2_NMETA+6)/au_PE
    W6=au_ZK(absoru2_NMETA+5)*W5/au_PE
    S=1.0+W5+W6
    au_AC1(1)=W2/(1.0+W2)
    au_AC1(2)=W6/S
    au_AC1(3)=W5/S
    absoru_ZNH(absoru2_NMETA+6)=au_ZNU2*PARTH/(1.0+W5+W6)
    absoru_ZNH(absoru2_NMETA+5)=absoru_ZNH(absoru2_NMETA+6)*W5
    au_RHO=1.6602E-24*PARTH*au_ZMUZE  ! 1.6602E-24: MASSE DE L'UNITE DE POIDS
    au_ZMU=au_RHO*41904.28E+7/(TH*au_PG)  ! 41904.275E+7: 8.313697E+7*5040.39, OU
                                          ! 8.313697E+7: CONSTANTE DES GAZ
  END SUBROUTINE IONIPE
END MODULE MOD_ABSORU

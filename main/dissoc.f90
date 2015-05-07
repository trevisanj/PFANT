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
!> Subroutines SAT4 and DIE
!>
!> Prefix "sat4_" denotes variables filled by SAT4() (or indirectly, DIE())
module dissoc
  use read_files
  implicit none


  integer, private, parameter :: &
   z_electron = 99,  &  ! Fictitious atomic number of electron
   z_h_star   = 100, &  ! Fictitious atomic number of "H*"
   z_h        = 1,   &  ! Atomic number of Hydrogen
   z_he       = 2       ! Atomic number of Helium



  ! They will be pointer targets at molecula.f:POINT_PPA_PB()
  real*8, target, dimension(max_modeles_ntot) :: sat4_pph, sat4_ppc2, &
   sat4_pn, &
   sat4_pc13, sat4_pmg, sat4_po, sat4_pti, sat4_pfe

  real*8, private, dimension(max_z) :: &
   ip,     & ! ?
   ccomp,  & ! ?
   uiidui, & ! ?
   fp,     & ! ?
   kp,     & ! ?
   p         ! Pressure

  real*8, private, dimension(max_dissoc_nmol) :: &
   ppmol, apmlog

  real pe ! Fictitious pressure of electron?? ISSUE: is it?

  ! ISSUE I won't do it this way until I sort the conflicts in DIE
  !~REAL PE ! Fictitious pressure of the electron?? is it? ISSUE
  !~EQUIVALENCE (P(Z_ELECTRON), P_ELECTRON)


contains


  !================================================================================================================================
  !> Subroutine d'equilibre dissociatif
  ! ISSUE WHAT

  subroutine sat4()
    use config
    implicit none
    real*8, dimension(max_modeles_ntot, max_dissoc_nmetal) :: xp
    real  kplog, econst, fplog, &
     pdfpl, pelog, pglog, pionl, plog, pmoll, tem, pg, theta, xlog
    real*8 cclogi
    integer i, ig0i, ig1i, iq, ir, irl, irr, ito, itx, j, jcount, nbl, &
     nelemi, nelemxi, k1, k2, k3, kd, kf
    character*128 lll


    !
    !*****IMPUT A

    econst = 4.342945e-1

    ! Infers other variables from variables dissoc__* (notice the double underscore)
    do i = 1, dissoc_nmetal
      cclogi = dissoc__cclog(i)+main_afstar
      ! ISSUE This is the thing that Beatriz mentioned that it is not used anymore, I think
      cclogi = cclogi+main_xxcor(i)
      if(i .eq .1) cclogi = 0.0
      if(i .eq .2) cclogi = -1.0

      nelemxi = dissoc_nelemx(i)
      ig0i = dissoc__ig0(i)
      ig1i = dissoc__ig1(i)

      ip(nelemxi) = dissoc__ip(i)
      uiidui(nelemxi) = ig1i * 0.661 / ig0i
      ccomp(nelemxi) = exp(cclogi/econst)

      !~     !--debugging--!
      !~     WRITE(LLL, '(1H ,5X,A4,8X,I5,3X, F10.3,5X, 2I5,3X,F10.5)')
      !~+     dissoc_ELEMS(I), NELEMXI, dissoc__IP(I),
      !~+     IG0I, IG1I, CCLOGI-main_AFSTAR
      !~     CALL LOG_DEBUG(LLL)
    end do

    !
    !*****INPUT D

    ! STARTING VALUE OF THE SOLUTION
    do 1400 i = 1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      p(nelemi) = 1.0e-20
    1400 continue

    ! ISSUE What if atomic number 99 was already in dissoc.dat?
    p(z_electron) = 1.0e-10
    ! ISSUE: what about 100?

    !*****INPUT E

    ! @todo issue this block has no comments
    do 1020 ito = 1,modeles_ntot
      theta = modeles_teta(ito)
      tem = 5040.0/theta
      pg = modeles_pg(ito)
      pglog = alog10(pg)

      call die(tem,pg)

      pe = p(z_electron)
      pelog = alog10(pe)

      do 1303 i=1,dissoc_nmetal
        nelemi = dissoc_nelemx(i)

        fplog  = alog10(fp(nelemi))
        xp(ito,i) = p(nelemi)+1.0e-30
        plog   = alog10( xp(ito,i) )
        pdfpl  = plog - fplog
        if (mod(i,5)) 1303,1304,1303
        1304 continue
      1303 continue

      irl = 120
      do 1184 i=1,dissoc_nmetal
        nelemi = dissoc_nelemx(i)

        plog   = alog10(p(nelemi)+1.0e-30)
        kplog  = alog10(kp(nelemi)+1.0e-30)
        pionl  = plog + kplog - pelog
        xlog   = pionl - pglog

        if (i .ne. dissoc_nmetal ) go to 1450
        iq  = i / 120
        ir  = dissoc_nmetal - iq * 120
        irl = ir / 3
        go to 1460

        1450 if (mod(i,120))  1184,1460,1184

        1460 nbl = 0
        
        do 1470  k1=1,120,3
          nbl = nbl + 1
          k2 = k1 + 1
          k3 = k1 + 2
          if ( nbl.eq.irl + 1)  go to 1480
          continue

          if (mod(nbl,5)) 1470,1500,1470
          1500 continue
        1470 continue

        go to 1184

        1480 continue
        irr = ir - irl*3
        if (irr .eq. 0)  go to 1184
        go to (1482,1484), irr
        
        1482 continue
        
        go to 1184

        1484 continue
      1184 continue

      irl = 120
      kd =-119
      do 1084 j=1,dissoc_nmol
        jcount = jcount + 1
        pmoll  = alog10(ppmol(j)+1.0e-30)
        xlog   = pmoll - pglog

        if (j .ne. dissoc_nmol) go to 2450
        iq = j/120
        ir =  dissoc_nmol - iq*120
        irl = ir/3
        go to 2460

        2450 if (mod(j,120)) 2184,2460,2184

        2460 nbl = 0

        kd = kd + 120
        kf = kd + 119
        do 2470  k1=kd,kf,3
          nbl = nbl + 1
          k2 = k1 + 1
          k3 = k1 + 2
          if ( nbl.eq.irl + 1)  go to 2480
          continue

          if (mod(nbl,5)) 2470,2500,2470

          2500 continue
        2470 continue
        go to 2184

        2480 continue
        
        irr = ir - irl*3
        
        if (irr .eq. 0)  go to 2184
        
        go to (2482,2484), irr
        
        2482 continue

        go to 2184
        
        2484 continue
        2184 continue
      1084 continue
    1020 continue

    !--debugging--!
    do i=1,4
      write(lll,'(7e11.4)') (xp(itx,i),itx=1,modeles_ntot)
      call log_debug(lll)
    end do

    do 51 itx=1,modeles_ntot
      sat4_pph(itx)=xp(itx,1)
      sat4_ppc2(itx)=xp(itx,3)
      sat4_pn(itx)=xp(itx,4)
      sat4_po(itx)=xp(itx,5)
      sat4_pc13(itx)=xp(itx,6)
      sat4_pti(itx)=xp(itx,15)
      sat4_pmg(itx)=xp(itx,8)
      sat4_pfe(itx)=xp(itx,16)
    51 continue
  end


  !================================================================================================================================
  !> DIE9
  ! ISSUE WHAT

  subroutine die(tem, pg)
    use config
    use read_files
    real*8 tem, pg
    real*8, dimension(max_z) :: fx, dfx, z, prev
    real*8, dimension(max_dissoc_nmetal) :: wa
    real aplogj, atomj, delta, df, dhh, econst, epsdie, &
     f, fph, heh, hkp, perev, pglog, ph, pmolj, pmoljl, q, r, s, &
     spnion, t, tem25, u, x, xr, pph, phh
    integer i, imaxp1, iterat, j, k, km5, m, mmaxj, nelemi, nelemj, &
     natomj, niter
    character*128 lll

    econst = 4.342945e-1
    epsdie = 5.0e-3
    t      = 5040.0/tem
    pglog  = alog10(pg)
    
    heh    = ccomp(z_he)/ccomp(z_h)  ! Helium-to-Hydrogen ratio by number

    ! Evaluation of log kp(mol)
    do 1025 j =1, dissoc_nmol
      aplogj = dissoc_c(j,5)
      do 1026 k=1,4
        km5 = 5-k
        aplogj = aplogj*t + dissoc_c(j,km5)
      1026 continue
      apmlog(j) = aplogj
    1025 continue

    dhh = (((0.1196952e-02*t-0.2125713e-01)*t+0.1545253e+00)*(-0.5161452e+01))*t+0.1277356e+02
    dhh = exp(dhh/econst)

    ! Evaluation of the ionization constants
    tem25 = tem**2*sqrt(tem)
    do 1060 i = 1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      kp(nelemi) =uiidui(nelemi)*tem25*exp(-ip(nelemi)*t/econst)
    1060 continue

    hkp = kp(z_h)
    if (t-0.6) 1084, 1072, 1072

    ! Preliminary value of pH at high temperatures (ISSUE is this potential hidrogenionico??)
    1084 continue
    pph = sqrt(hkp *(pg/(1.0+heh)+hkp ))-hkp
    ph  = pph**2/hkp
    go to 1102

    ! Preliminary value of ph at low temperatures
    1072 continue
    if (pg/dhh - 0.1) 1073, 1073, 1074
    
    1073 continue
    ph = pg/(1.0+heh)
    go to 1102


    1074 continue
    ph = 0.5*(sqrt(dhh*(dhh+4.0*pg/(1.0+heh)))-dhh)

    ! Evaluation of the fictitious pressures of hydrogen
    ! pg = ph+phh+2.0*pph+heh*(ph+2.0*phh+pph)  ! issue i may have commented this by accident
    1102 CONTINUE
    U = (1.0+2.0*HEH)/DHH
    Q = 1.0+HEH
    R = (2.0+HEH)*SQRT(HKP )
    S = -1.0*PG
    X = SQRT(PH)
    ITERAT = 0
    
    1103 CONTINUE
    F  = ((U*X**2+Q)*X+R)*X+S
    DF = 2.0*(2.0*U*X**2+Q)*X+R
    XR = X-F/DF
    IF (ABS((X-XR)/XR)-EPSDIE) 1105, 1105, 1106
    
    1106 CONTINUE
    ITERAT=ITERAT+1
    IF (ITERAT-50) 1104,1104,1107

    1107 CONTINUE
    
    6108 FORMAT(1H1,'NOT CONVERGE IN DIE  TEM=', F9.2, 5X, 'PG=', E12.5, 5X 'X1=', &
                E12.5, 5X,'X2=', E12.5, 5X, 'PH=', E12.5)
    WRITE(LLL, 6108) TEM,PG,X,XR,PH
    CALL LOG_WARNING(LLL)
    
    GO TO 1105

    1104 CONTINUE
    X = XR
    
    GO TO 1103

    1105 CONTINUE
    PH  = XR**2
    PHH = PH**2/DHH
    PPH = SQRT(HKP *PH)
    FPH = PH+2.0*PHH+PPH

    ! ISSUE Z=100 within dissoc.dat is only possible at the metals part (at the molecules part the Z slots have only 2 digits).
    ! THe current dissoc.dat has no Z=100 (neither 99).
    ! Is this a remaining fragment of code? My hint comes from the fact that Z_ELECTRON=99 is addressed several times, but Z_H_STAR=100 is not.
    P(Z_H_STAR) = PPH


    ! EVALUATION OF THE FICTITIOUS PRESSURE OF EACH ELEMENT
    DO 1070 I=1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      FP(NELEMI) = CCOMP(NELEMI)*FPH
    1070 CONTINUE

    ! CHECK OF INITIALIZATION
    PE = P(Z_ELECTRON)

    IF(PH-P(Z_H)) 1402,1402,1401
    
    1401 CONTINUE
    DO 1403 I=1,dissoc_NMETAL
      ! ISSUE: what if some NELEMI=Z_ELECTRON=99? THen P(99) will no longer be equal to PE
      NELEMI=dissoc_NELEMX(I)
      P(NELEMI) = FP(NELEMI)*EXP(-5.0*T/ECONST)
    1403 CONTINUE
    P(Z_H) = PH   ! ISSUE: overwriting P(1)

    ! RUSSELL EQUATIONS
    1402 CONTINUE
    NITER = 0
    1040 CONTINUE
    DO 1030 I =1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      FX(NELEMI) = -FP(NELEMI)+P(NELEMI)*(1.0 + KP(NELEMI)/PE)  ! ISSUE if NELEMI=99, P(99) and PE are potentially not the same thing! Is this alright?
      DFX(NELEMI) = 1.0 + KP(NELEMI)/PE
    1030 CONTINUE

    SPNION = 0.0
    DO 1041 J=1,dissoc_NMOL
      MMAXJ  = dissoc_MMAX(J)
      PMOLJL = -APMLOG(J)
      DO 1042 M =1,MMAXJ
        NELEMJ = dissoc_NELEM(M,J)
        NATOMJ = dissoc_NATOM(M,J)
        PMOLJL = PMOLJL + FLOAT(NATOMJ)*ALOG10(P(NELEMJ))
      1042 CONTINUE
      
      IF(PMOLJL - (PGLOG+1.0) ) 1046,1046,1047
      
      1047 CONTINUE
      DO 1048 M =1,MMAXJ
        NELEMJ = dissoc_NELEM(M,J)
        NATOMJ = dissoc_NATOM(M,J)

        ! ISSUE BIG! at each iteration of the J loop, P gets divided by 100, is this correct??? Doesn't look like
        P(NELEMJ)=1.0E-2*P(NELEMJ)
        PMOLJL = PMOLJL + FLOAT(NATOMJ)*(-2.0)
      1048 CONTINUE

      1046 PMOLJ = EXP(PMOLJL/ECONST)
      DO 1044 M =1,MMAXJ
        NELEMJ = dissoc_NELEM(M,J)
        NATOMJ = dissoc_NATOM(M,J)
        ATOMJ = FLOAT(NATOMJ)

        IF (NELEMJ .EQ. Z_ELECTRON) THEN  ! ISSUE This bit suggests that Z=99 is allowed in the molecules part
          SPNION = SPNION + PMOLJ
        END IF

        DO 1043 I=1,dissoc_NMETAL
          NELEMI = dissoc_NELEMX(I)
          IF(NELEMJ .EQ. NELEMI) GO TO 1045
          GO TO 1043
          1045 FX(NELEMI) = FX(NELEMI) + ATOMJ*PMOLJ
          DFX(NELEMI) = DFX(NELEMI) + ATOMJ**2*PMOLJ/P(NELEMI)
        1043 CONTINUE
      1044 CONTINUE
      PPMOL(J) = PMOLJ
    1041 CONTINUE

    ! SOLUTION OF THE RUSSELL EQUATIONS BY NEWTON-RAPHSON METHOD
    DO 2001 I=1,dissoc_NMETAL
      NELEMI=dissoc_NELEMX(I)
      WA(I)=ALOG10(P(NELEMI)+1.0E-30)
    2001 CONTINUE

    IMAXP1 = dissoc_NMETAL+1
    WA(IMAXP1) = ALOG10(PE+1.0E-30)
    DELTA = 0.0
    DO 1050 I=1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      PREV(NELEMI) = P(NELEMI) - FX(NELEMI)/DFX(NELEMI)
      PREV(NELEMI) = ABS(PREV(NELEMI))

      IF (PREV(NELEMI) .LT. 1.0E-30) PREV(NELEMI)=1.0E-30

      Z(NELEMI) = PREV(NELEMI)/P(NELEMI)
      DELTA = DELTA + ABS(Z(NELEMI) - 1.0)

      IF (dissoc_SWITER) 2500,2500,2501

      2501 CONTINUE
      P(NELEMI) = (PREV(NELEMI) + P(NELEMI) )*0.5
      GO TO 1050

      2500 CONTINUE
      P(NELEMI) = PREV(NELEMI)
    1050 CONTINUE


    ! IONIZATION EQUILIBRIUM
    PEREV = 0.0
    DO 1061 I=1,dissoc_NMETAL
      NELEMI = dissoc_NELEMX(I)
      PEREV = PEREV + KP(NELEMI)*P(NELEMI)
    1061 CONTINUE

    PEREV = SQRT(PEREV/(1.0+SPNION/PE))
    DELTA = DELTA + ABS((PE-PEREV)/PE)
    PE = (PEREV + PE)*0.5  ! Note that it has an equivalence with the last element of P
    P(Z_ELECTRON)=PE

    IF (DELTA - dissoc_EPS) 1051,1051,1052

    1052 CONTINUE
    NITER = NITER+1
    IF (NITER-dissoc_NIMAX) 1040,1040,1054

    1054 CONTINUE
    6055 FORMAT(1H0,39H *DOES NOT CONVERGE AFTER ITERATIONS OF,I4/////)
    WRITE(LLL,6055) dissoc_NIMAX
    CALL LOG_WARNING(LLL)

    1051 CONTINUE
  END
END MODULE DISSOC

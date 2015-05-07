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

  !> @todo ISSUE I won't do it this way until I sort the conflicts in DIE
  !~REAL PE ! Fictitious pressure of the electron?? is it? ISSUE
  !~EQUIVALENCE (P(Z_ELECTRON), P_ELECTRON)


contains

  !================================================================================================================================
  !> Subroutine d'equilibre dissociatif
  !> @todo ISSUE WHAT

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
      !> @todo ISSUE This is the thing that Beatriz mentioned that it is not used anymore, I think
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

    !> @todo ISSUE What if atomic number 99 was already in dissoc.dat?
    p(z_electron) = 1.0e-10
    !> @todo ISSUE: what about 100?

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
  !> @todo ISSUE WHAT

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

    ! EVALUATION OF LOG KP(MOL)
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

    ! EVALUATION OF THE IONIZATION CONSTANTS
    tem25 = tem**2*sqrt(tem)
    do 1060 i = 1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      kp(nelemi) =uiidui(nelemi)*tem25*exp(-ip(nelemi)*t/econst)
    1060 continue

    hkp = kp(z_h)
    if (t-0.6) 1084, 1072, 1072

    ! PRELIMINARY VALUE OF PH AT HIGH TEMPERATURES (ISSUE is this potential hidrogenionico??)
    1084 continue
    pph = sqrt(hkp *(pg/(1.0+heh)+hkp ))-hkp
    ph  = pph**2/hkp
    go to 1102

    ! PRELIMINARY VALUE OF PH AT LOW TEMPERATURES
    1072 continue
    if (pg/dhh - 0.1) 1073, 1073, 1074
    
    1073 continue
    ph = pg/(1.0+heh)
    go to 1102


    1074 continue
    ph = 0.5*(sqrt(dhh*(dhh+4.0*pg/(1.0+heh)))-dhh)

    ! EVALUATION OF THE FICTITIOUS PRESSURES OF HYDROGEN
    ! PG = PH+PHH+2.0*PPH+HEH*(PH+2.0*PHH+PPH)  !> @todo ISSUE i may have commented this by accident
    1102 continue
    u = (1.0+2.0*heh)/dhh
    q = 1.0+heh
    r = (2.0+heh)*sqrt(hkp )
    s = -1.0*pg
    x = sqrt(ph)
    iterat = 0
    
    1103 continue
    f  = ((u*x**2+q)*x+r)*x+s
    df = 2.0*(2.0*u*x**2+q)*x+r
    xr = x-f/df
    if (abs((x-xr)/xr)-epsdie) 1105, 1105, 1106
    
    1106 continue
    iterat=iterat+1
    if (iterat-50) 1104,1104,1107

    1107 continue
    
    6108 format(1h1,'Not converge in die  tem=', f9.2, 5x, 'pg=', e12.5, 5x 'x1=', &
                e12.5, 5x,'x2=', e12.5, 5x, 'ph=', e12.5)
    write(lll, 6108) tem,pg,x,xr,ph
    call log_warning(lll)
    
    go to 1105

    1104 continue
    x = xr
    
    go to 1103

    1105 continue
    ph  = xr**2
    phh = ph**2/dhh
    pph = sqrt(hkp *ph)
    fph = ph+2.0*phh+pph

    !> @todo ISSUE Z=100 within dissoc.dat is only possible at the metals part (at the molecules part the Z slots have only 2 digits).
    ! THe current dissoc.dat has no Z=100 (neither 99).
    ! Is this a remaining fragment of code? My hint comes from the fact that Z_ELECTRON=99 is addressed several times, but Z_H_STAR=100 is not.
    p(z_h_star) = pph


    ! EVALUATION OF THE FICTITIOUS PRESSURE OF EACH ELEMENT
    do 1070 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      fp(nelemi) = ccomp(nelemi)*fph
    1070 continue

    ! CHECK OF INITIALIZATION
    pe = p(z_electron)

    if(ph-p(z_h)) 1402,1402,1401
    
    1401 continue
    do 1403 i=1,dissoc_nmetal
      !> @todo ISSUE: what if some NELEMI=Z_ELECTRON=99? THen P(99) will no longer be equal to PE
      nelemi=dissoc_nelemx(i)
      p(nelemi) = fp(nelemi)*exp(-5.0*t/econst)
    1403 continue
    p(z_h) = ph   !> @todo ISSUE: overwriting P(1)

    ! RUSSELL EQUATIONS
    1402 continue
    niter = 0
    1040 continue
    do 1030 i =1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      fx(nelemi) = -fp(nelemi)+p(nelemi)*(1.0 + kp(nelemi)/pe)  !> @todo ISSUE if NELEMI=99, P(99) and PE are potentially not the same thing! Is this alright?
      dfx(nelemi) = 1.0 + kp(nelemi)/pe
    1030 continue

    spnion = 0.0
    do 1041 j=1,dissoc_nmol
      mmaxj  = dissoc_mmax(j)
      pmoljl = -apmlog(j)
      do 1042 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        pmoljl = pmoljl + float(natomj)*alog10(p(nelemj))
      1042 continue
      
      if(pmoljl - (pglog+1.0) ) 1046,1046,1047
      
      1047 continue
      do 1048 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)

        !> @todo ISSUE BIG! at each iteration of the J loop, P gets divided by 100, is this correct??? Doesn't look like
        p(nelemj)=1.0e-2*p(nelemj)
        pmoljl = pmoljl + float(natomj)*(-2.0)
      1048 continue

      1046 pmolj = exp(pmoljl/econst)
      do 1044 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        atomj = float(natomj)

        if (nelemj .eq. z_electron) then  !> @todo ISSUE This bit suggests that Z=99 is allowed in the molecules part
          spnion = spnion + pmolj
        end if

        do 1043 i=1,dissoc_nmetal
          nelemi = dissoc_nelemx(i)
          if(nelemj .eq. nelemi) go to 1045
          go to 1043
          1045 fx(nelemi) = fx(nelemi) + atomj*pmolj
          dfx(nelemi) = dfx(nelemi) + atomj**2*pmolj/p(nelemi)
        1043 continue
      1044 continue
      ppmol(j) = pmolj
    1041 continue

    ! SOLUTION OF THE RUSSELL EQUATIONS BY NEWTON-RAPHSON METHOD
    do 2001 i=1,dissoc_nmetal
      nelemi=dissoc_nelemx(i)
      wa(i)=alog10(p(nelemi)+1.0e-30)
    2001 continue

    imaxp1 = dissoc_nmetal+1
    wa(imaxp1) = alog10(pe+1.0e-30)
    delta = 0.0
    do 1050 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      prev(nelemi) = p(nelemi) - fx(nelemi)/dfx(nelemi)
      prev(nelemi) = abs(prev(nelemi))

      if (prev(nelemi) .lt. 1.0e-30) prev(nelemi)=1.0e-30

      z(nelemi) = prev(nelemi)/p(nelemi)
      delta = delta + abs(z(nelemi) - 1.0)

      if (dissoc_switer) 2500,2500,2501

      2501 continue
      p(nelemi) = (prev(nelemi) + p(nelemi) )*0.5
      go to 1050

      2500 continue
      p(nelemi) = prev(nelemi)
    1050 continue


    ! IONIZATION EQUILIBRIUM
    perev = 0.0
    do 1061 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      perev = perev + kp(nelemi)*p(nelemi)
    1061 continue

    perev = sqrt(perev/(1.0+spnion/pe))
    delta = delta + abs((pe-perev)/pe)
    pe = (perev + pe)*0.5  ! Note that it has an equivalence with the last element of P
    p(z_electron)=pe

    if (delta - dissoc_eps) 1051,1051,1052

    1052 continue
    niter = niter+1
    if (niter-dissoc_nimax) 1040,1040,1054

    1054 continue
    6055 format(1h0,39h *Does not converge after iterations of,i4/////)
    write(lll,6055) dissoc_nimax
    call log_warning(lll)

    1051 continue
  end
end module dissoc

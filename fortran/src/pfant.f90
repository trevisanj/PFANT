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
!> Equilibre dissociatif
!>
!> This module contains subroutine sat4(), which calculates the sat4_* variables
!> used by other modules.
!>
!> Prefixes:
!> @li sat4_ -- variables calculated by sat4() (or indirectly, die())
!>
!> @todo plot the sat4_*

!
!> @todo consider eliminating the +-20 angstrom from calculations

module dissoc
  use config
  use readers
  implicit none

  ! They will be pointer targets at molecules::point_ppa_pb()
  real*8, target, dimension(MAX_MODELES_NTOT) ::  &
   sat4_pph,  & !< pressure: hydrogen ?doc?
   sat4_ppc2, & !< pressure: 12carbon ?doc?
   sat4_pn,   & !< pressure: nytrogen ?doc?
   sat4_pc13, & !< pressure: 13carbon ?doc?
   sat4_pmg,  & !< pressure: magnesium ?doc?
   sat4_po,   & !< pressure: oxygen ?doc?
   sat4_pti,  & !< pressure: titanium ?doc?
   sat4_pfe     !< pressure: iron ?doc?

  private :: die ! private subroutine

  integer, private, parameter :: &
   Z_ELECTRON = 99,  &  !< Fictitious atomic number of electron
   Z_H_STAR   = 100, &  !< Fictitious atomic number of "H*"
   Z_H        = 1,   &  !< Atomic number of Hydrogen
   Z_HE       = 2       !< Atomic number of Helium

  ! Note that indexes of these arrays are atomic numbers
  ! For example: m_ip(z) contains information related to atomic number z
  real*8, private, dimension(MAX_DISSOC_Z) :: &
   m_ip,     & ! ?doc?
   m_ccomp,  & ! ?doc?
   m_uiidui, & ! ?doc?
   m_fp,     & ! ?doc?
   m_kp,     & ! ?doc?
   m_p         ! ?doc? Pressure

  real*8, private, dimension(MAX_DISSOC_NMOL) :: &
   m_ppmol, & ! ?doc?
   m_apmlog   ! ?doc?

  real*8, private :: m_pe !< Fictitious pressure of electron ?doc?

  real*8, private, parameter :: ECONST = 4.342945e-1 !< ?doc?
contains

  !=======================================================================================
  !> Subroutine d'equilibre dissociatif

  subroutine sat4()
    real*8, dimension(MAX_MODELES_NTOT, MAX_DISSOC_NMETAL) :: xp
    real*8  kplog, fplog, &
     pdfpl, pelog, pglog, pionl, plog, pmoll, tem, pg, theta, xlog
    real*8 cclogi
    integer i, ig0i, ig1i, iq, ir, irl, irr, ito, j, jcount, nbl, &
     nelemi, nelemxi, k1, k2, k3, kd, kf, iz

    !
    !*****INPUT A


    ! Infers other variables from variables dissoc_*
    do i = 1, dissoc_nmetal
      cclogi = dissoc_cclog(i)+main_afstar
      !> @todo xxcor on the way. MT: Get rid of it.
      cclogi = cclogi+main_xxcor(i)
      if(i .eq. 1) cclogi = 0.0
      if(i .eq. 2) cclogi = -1.0

      nelemxi = dissoc_nelemx(i)
      ig0i = dissoc_ig0(i)
      ig1i = dissoc_ig1(i)

      m_ip(nelemxi) = dissoc_ip(i)
      m_uiidui(nelemxi) = ig1i * 0.661 / ig0i
      m_ccomp(nelemxi) = exp(cclogi/ECONST)

      !~     !--debugging--!
      !~     WRITE(LLL, '(1H ,5X,A4,8X,I5,3X, F10.3,5X, 2I5,3X,F10.5)')
      !~+     dissoc_ELEMS(I), NELEMXI, dissoc_IP(I),
      !~+     IG0I, IG1I, CCLOGI-main_AFSTAR
      !~     CALL LOG_DEBUG(LLL)
    end do

    !
    !*****INPUT D

    ! STARTING VALUE OF THE SOLUTION
    do 1400 i = 1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      m_p(nelemi) = 1.0e-20
    1400 continue

    !> @todo issue ?doc? if atomic number 99 was already in dissoc.dat?
    m_p(Z_ELECTRON) = 1.0e-10

    !
    !*****INPUT E

    do 1020 ito = 1,modeles_ntot
      theta = modeles_teta(ito)
      tem = 5040.0/theta
      pg = modeles_pg(ito)
      pglog = log10(pg)

      call die(tem,pg)

      m_pe = m_p(Z_ELECTRON)
      pelog = log10(m_pe)

      do 1303 i=1,dissoc_nmetal
        nelemi = dissoc_nelemx(i)

        fplog  = log10(m_fp(nelemi))
        xp(ito,i) = m_p(nelemi)+1.0e-30
        plog   = log10( xp(ito,i) )
        pdfpl  = plog - fplog
        if (mod(i,5)) 1303,1304,1303
        1304 continue
      1303 continue

      irl = 120
      do 1184 i=1,dissoc_nmetal
        nelemi = dissoc_nelemx(i)

        !> @todo issue is it OK to avoid log(0) by adding 1e-30 here??? (MT): m_p,m_kp=0 should be avoided.
        plog   = log10(m_p(nelemi)+1.0e-30)
        kplog  = log10(m_kp(nelemi)+1.0e-30)
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
        pmoll  = log10(m_ppmol(j)+1.0e-30)
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

    !!_logging__
    !do i=1,4
    !  write(*,'(7e11.4)') (xp(itx,i),itx=1,modeles_ntot)
    !end do

    iz = find_atomic_symbol_dissoc('H ')
    sat4_pph = xp(:, iz)
    iz = find_atomic_symbol_dissoc('C ')
    sat4_ppc2 = xp(:, iz)
    iz = find_atomic_symbol_dissoc('N ')
    sat4_pn = xp(:, iz)
    iz = find_atomic_symbol_dissoc('O ')
    sat4_po = xp(:, iz)
    iz = find_atomic_symbol_dissoc('A ')  ! @todo issue taking fluorine ('A') instead of 13C
    sat4_pc13 = xp(:, iz)
    iz = find_atomic_symbol_dissoc('TI')
    sat4_pti = xp(:, iz)
    iz = find_atomic_symbol_dissoc('MG')
    sat4_pmg = xp(:, iz)
    iz = find_atomic_symbol_dissoc('FE')
    sat4_pfe = xp(:, iz)


!original      do itx = 1,modeles_ntot
!original        sat4_pph(itx)=xp(itx,1)
!original        sat4_ppc2(itx)=xp(itx,3)
!original        sat4_pn(itx)=xp(itx,4)
!original        sat4_po(itx)=xp(itx,5)
!original        sat4_pc13(itx)=xp(itx,6)
!original        sat4_pti(itx)=xp(itx,15)
!original        sat4_pmg(itx)=xp(itx,8)
!original        sat4_pfe(itx)=xp(itx,16)
!original      end do
  end


  !=======================================================================================
  !> DIE9 ?what?

  subroutine die(tem, pg)
    real*8, intent(in) :: tem, & !< ?doc?
     pg !< ?doc?
    real*8, dimension(MAX_DISSOC_Z) :: fx, dfx, z, prev
    real*8, dimension(MAX_DISSOC_NMETAL) :: wa
    real*8 aplogj, atomj, delta, df, dhh, EPSDIE, &
     f, fph, heh, hkp, perev, pglog, ph, pmolj, pmoljl, q, r, s, &
     spnion, t, tem25, u, x, xr, pph, phh
    integer i, imaxp1, iterat, j, k, km5, m, mmaxj, nelemi, nelemj, &
     natomj, niter

    EPSDIE = 5.0e-3
    t      = 5040.0/tem
    pglog  = log10(pg)

    heh    = m_ccomp(Z_HE)/m_ccomp(Z_H)  ! Helium-to-Hydrogen ratio by number

    ! EVALUATION OF LOG m_kp(MOL)
    do 1025 j =1, dissoc_nmol
      aplogj = dissoc_c(j,5)
      do 1026 k=1,4
        km5 = 5-k
        aplogj = aplogj*t + dissoc_c(j,km5)
      1026 continue
      m_apmlog(j) = aplogj
    1025 continue

    dhh = (((0.1196952e-02*t-0.2125713e-01)*t+0.1545253e+00)*(-0.5161452e+01))*t+0.1277356e+02
    dhh = exp(dhh/ECONST)

    ! EVALUATION OF THE IONIZATION CONSTANTS
    tem25 = tem**2*sqrt(tem)
    do 1060 i = 1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      m_kp(nelemi) =m_uiidui(nelemi)*tem25*exp(-m_ip(nelemi)*t/ECONST)
    1060 continue

    hkp = m_kp(Z_H)
    if (t-0.6) 1084, 1072, 1072

    ! PRELIMINARY VALUE OF PH AT HIGH TEMPERATURES
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
    if (abs((x-xr)/xr)-EPSDIE) 1105, 1105, 1106

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
    !> THe current dissoc.dat has no Z=100 (neither 99).
    !> Is this a remaining fragment of code? My hint comes from the fact that Z_ELECTRON=99 is addressed several times, but Z_H_STAR=100 is not.
    m_p(Z_H_STAR) = pph


    ! EVALUATION OF THE FICTITIOUS PRESSURE OF EACH ELEMENT
    do 1070 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      m_fp(nelemi) = m_ccomp(nelemi)*fph
    1070 continue

    ! CHECK OF INITIALIZATION
    m_pe = m_p(Z_ELECTRON)



    if(ph-m_p(Z_H)) 1402,1402,1401

    1401 continue
    do 1403 i=1,dissoc_nmetal
      !> @todo ISSUE: what if some NELEMI=Z_ELECTRON=99? THen m_p(99) will no longer be equal to m_pe
      nelemi=dissoc_nelemx(i)
      m_p(nelemi) = m_fp(nelemi)*exp(-5.0*t/ECONST)
    1403 continue
    m_p(Z_H) = ph   !> @todo ISSUE: overwriting m_p(1)

    ! Update: kept as-was
    ! !> @note m_p was being divided by 100 over and over at each j (molecule). This division has been taken out of loop, but is still an issue, since it is unclear *why* this division is being done.
    ! !> @todo issue ask blb being divided by 100 is still an issue
    ! do m =1,MAX_DISSOC_Z
    !   m_p(m)=1.0e-2*m_p(m)
    ! end do


    ! RUSSELL EQUATIONS
    1402 continue
    niter = 0
    1040 continue
    do 1030 i =1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      fx(nelemi) = -m_fp(nelemi)+m_p(nelemi)*(1.0 + m_kp(nelemi)/m_pe)  !> @todo ISSUE if NELEMI=99, m_p(99) and m_pe are potentially not the same thing! Is this alright?
      dfx(nelemi) = 1.0 + m_kp(nelemi)/m_pe
    1030 continue

    spnion = 0.0
    do 1041 j=1,dissoc_nmol
      mmaxj  = dissoc_mmax(j)
      pmoljl = -m_apmlog(j)
      do 1042 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        !> @todo log(m_p()) called many times, should try to optimize
        pmoljl = pmoljl + float(natomj)*log10(m_p(nelemj))
      1042 continue

      if(pmoljl - (pglog+1.0) ) 1046,1046,1047

      1047 continue
      do 1048 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        pmoljl = pmoljl + float(natomj)*(-2.0)

        ! For each j, divides all used elements in m_p by 100.
        ! This is necessary for convergence of the molecular equilibrium.
        m_p(nelemj) = 1.0e-2*m_p(nelemj)

      1048 continue

      1046 pmolj = exp(pmoljl/ECONST)
      do 1044 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        atomj = float(natomj)

        if (nelemj .eq. Z_ELECTRON) then  !> @todo ISSUE This bit suggests that Z=99 is allowed in the molecules part
          spnion = spnion + pmolj
        end if

        do 1043 i=1,dissoc_nmetal
          nelemi = dissoc_nelemx(i)
          if(nelemj .eq. nelemi) go to 1045
          go to 1043
          1045 fx(nelemi) = fx(nelemi) + atomj*pmolj
          dfx(nelemi) = dfx(nelemi) + atomj**2*pmolj/m_p(nelemi)
        1043 continue
      1044 continue
      m_ppmol(j) = pmolj
    1041 continue

    ! SOLUTION OF THE RUSSELL EQUATIONS BY NEWTON-RAPHSON METHOD
    do 2001 i=1,dissoc_nmetal
      nelemi=dissoc_nelemx(i)
      wa(i)=log10(m_p(nelemi)+1.0e-30)
    2001 continue

    imaxp1 = dissoc_nmetal+1
    wa(imaxp1) = log10(m_pe+1.0e-30)
    delta = 0.0
    do 1050 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      prev(nelemi) = m_p(nelemi) - fx(nelemi)/dfx(nelemi)
      prev(nelemi) = abs(prev(nelemi))

      if (prev(nelemi) .lt. 1.0e-30) prev(nelemi)=1.0e-30

      z(nelemi) = prev(nelemi)/m_p(nelemi)
      delta = delta + abs(z(nelemi) - 1.0)

      if (dissoc_switer) 2500,2500,2501

      2501 continue
      m_p(nelemi) = (prev(nelemi) + m_p(nelemi) )*0.5
      go to 1050

      2500 continue
      m_p(nelemi) = prev(nelemi)
    1050 continue


    ! IONIZATION EQUILIBRIUM
    perev = 0.0
    do 1061 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      perev = perev + m_kp(nelemi)*m_p(nelemi)
    1061 continue

    perev = sqrt(perev/(1.0+spnion/m_pe))
    delta = delta + abs((m_pe-perev)/m_pe)
    m_pe = (perev + m_pe)*0.5  ! Note that it has an equivalence with the last element of m_p
    m_p(Z_ELECTRON)=m_pe

    if (delta - dissoc_eps) 1051,1051,1052

    1052 continue
    niter = niter+1
    if (niter-dissoc_nimax) 1040,1040,1054

    1054 continue
    6055 format(1h0,39h *Does not converge after iterations of,i4)
    write(lll,6055) dissoc_nimax
    call log_warning(lll)

    1051 continue
  end
end





!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Contains subroutines filter_molecules() and filter_atoms()
!>
!> Prefixes:
!> @li km_f_ -- calculated by filter_molecules()
!> @li atoms_f_ -- calculated by filter_atoms()

module filters
  use molecules_idxs
  use dimensions
  use reader_atoms
  use reader_molecules
  implicit none


  !=====
  ! km_f_*Variables filled by filter_molecules()
  !=====

  integer km_f_mblend  ! Total number of spectral lines *filtered in*

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_f_lmbdam, & !< ?doc?
    km_f_sj,     & !< ?doc?
    km_f_jj,     & !< ?doc?
    km_f_mm        !< Replicates km_mm(molidx) for all selected lines of molecule molidx.
                   !< Redundant information but simplifies use. Used in synthesis::selekfh()

  !------
  ! These two arrays contain indexes pointing at km_f_lmbdam, km_f_sj, km_f_jj, km_f_mm
  !------

  !> Contains the index of the last line of each set of lines within km_f_lmbdam, km_f_sj and km_f_jj
  !> **for the current molecule** I_MOL
  !>
  !> Augmented: first row is 0 (ZERO) or repeats last element of previous column
  !>
  !> km_f_ln(i+1, j) represents to i-th transition of j-th molecule, 1=1,molidxs%n_on
  integer :: km_f_ln(MAX_NV_PER_MOL+1, NUM_MOL)

  !=====
  ! atoms_f_*Variables filled by filter_atoms()
  !=====

  ! dfile:atoms, filtered variables
  ! Very similar to above; differences are
  ! - single underscore
  ! - additional variable "gf", which equals 10**algf
  integer atoms_f_nblend !< ?doc?
  character*2 atoms_f_elem(MAX_ATOMS_F_NBLEND) !< atomic symbol (right-alignes, uppercase)
  integer, dimension(MAX_ATOMS_F_NBLEND) :: &
   atoms_f_ioni !< ?doc?
  real*8, dimension(MAX_ATOMS_NBLEND) :: &
   atoms_f_lambda,       & !< ?doc?
   atoms_f_kiex,         & !< ?doc?
   atoms_f_algf,         & !< ?doc?
   atoms_f_ch,           & !< ?doc?
   atoms_f_gr,           & !< ?doc?
   atoms_f_ge,           & !< ?doc?
   atoms_f_zinf,         & !< ?doc?
   atoms_f_abondr_dummy, & !< ?doc?
   atoms_f_gf,           & !< ?doc?
   atoms_f_abonds_abo      !< ?doc?

contains

  !=======================================================================================
  !> Sweeps km_* to populate a few km_f_* depending on the interval lzero-lfin
  !>
  !> @todo test

  subroutine filter_molecules(lzero, lfin)
    !> Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    !> Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    real*8 :: lambda
    integer molidx,          &  ! Counts molecule id, from 1 to NUM_MOL
            i_mol,           &  ! Counts molecules that are "switched on"
            j_dummy, j_set,  &
            i_line,          &  ! Index of km_lmbdam, km_sj, km_jj
            i_filtered          ! Counts number of filtered lines (molecule-independent);
                                ! index of km_f_lmbdam, km_f_sj, km_f_jj
    integer num_lambdas
    logical flag_in

    write(lll, *) ENTERING, 'filter_molecules()', 'molidxs%n_on = ', molidxs%n_on
    call log_debug(lll)

    i_filtered = 0  ! Current *filtered-in* spectral line. Keeps growing (not reset when the molecule changes). Related to old "L"
    i_line = 1
    i_mol = 0
    do molidx = 1, km_number
      if (.not. molecule_is_on(molidx)) then
        i_line = i_line+km_lines_per_mol(molidx)
        cycle
      end if

      i_mol = i_mol+1

      km_f_ln(1, i_mol) = i_filtered  ! first row contain number of lines already filtered

      write(lll, *) 'molecule idx', molidx, '; titulo: ',  km_titulo(molidx), &
       '; number of prospective lambdas: ', km_lines_per_mol(molidx)
      call log_debug(lll)


      ! Counters starting with "j_" restart at each molecule
      j_set = 1   ! Current "set-of-lines"
      flag_in = .FALSE.  ! Whether has filtered in at least one line
      do j_dummy = 1, km_lines_per_mol(molidx)
        lambda = km_lmbdam(i_line)

        if ((lambda .ge. lzero) .and. (lambda .le. lfin)) then
          ! Filters in a new spectral line!
          i_filtered = i_filtered+1

          ! spill check
          if (i_filtered .gt. MAX_KM_F_MBLEND) then
            call pfant_halt('filter_molecules(): number of filtered lines '//&
             'exceeded maximum of MAX_KM_F_MBLEND='//int2str(MAX_KM_F_MBLEND), .true.)
          end if

          km_f_lmbdam(i_filtered) = lambda
          km_f_sj(i_filtered) = km_sj(i_line)
          km_f_jj(i_filtered) = km_jj(i_line)
          km_f_mm(i_filtered) = km_mm(molidx)
        end if

        if (i_line .eq. km_ln(j_set, molidx)) then
          ! Reached last line of current set of lines
          km_f_ln(j_set+1, i_mol) = i_filtered  ! Yes, j_set+1, not j_set, remember km_f_ln first row is set apart.

          num_lambdas = km_f_ln(j_set+1, i_mol)-km_f_ln(j_set, i_mol)
          write(lll, *) 'number of SELECTED lambdas for transition ', j_set, ': ', num_lambdas
          call log_debug(lll)


          j_set = j_set+1
        end if

        i_line = i_line+1
      end do
    end do !--end of MOLID loop--!

    km_f_mblend = i_filtered

    !print *, '*****************************************************************'
    !do i = 1, i_mol
    !  print *, '#', get_molidx(i), ' == ', km_f_ln(1:km_nv(get_molidx(i_mol)),i)
    !end do
    !print *, '*****************************************************************'
    write(lll, *) LEAVING, 'filter_molecules() summary: [', lzero, ', ', lfin, '] --> ', i_filtered, '/', km_lines_total
    call log_debug(lll)
  end


  !=======================================================================================
  !> Selects only spectral lines within range lzero, lfin + performs "inner join".
  !>
  !> Populates variables atoms_f_*

  subroutine filter_atoms(lzero, lfin)
    !> Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    !> Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    integer j, k

    k = 0
    do j = 1, atoms_nblend
      if((atoms_lambda(j).le.lfin) .and. (atoms_lambda(j) .ge. lzero)) then
        k = k+1


        ! spill check: checks if exceeds maximum number of elements allowed
        if (k .gt. MAX_ATOMS_F_NBLEND) then
          call pfant_halt('filter_atoms(): exceeded maximum of MAX_ATOMS_F_NBLEND='//&
           int2str(MAX_ATOMS_F_NBLEND)//' spectral lines')
        end if

        !Filters in
        atoms_f_elem(k)   = atoms_elem(j)
        atoms_f_ioni(k)   = atoms_ioni(j)
        atoms_f_lambda(k) = atoms_lambda(j)
        atoms_f_kiex(k)   = atoms_kiex(j)
        atoms_f_algf(k)   = atoms_algf(j)
        atoms_f_gf(k)     = 10.**atoms_algf(j)
        atoms_f_ch(k)     = atoms_ch(j)
        atoms_f_gr(k)     = atoms_gr(j)
        atoms_f_ge(k)     = atoms_ge(j)
        atoms_f_zinf(k)   = atoms_zinf(j)
        atoms_f_abonds_abo(k) = atoms_abonds_abo(j)
        atoms_f_abondr_dummy(k) = atoms_abondr_dummy(j)
      end if
    end do
    atoms_f_nblend = k
  end
end




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Contains subroutine kapmol_()
!>
!> Calculated variables have prefix "km_c_"

module kapmol
  use molecules_idxs
  use dimensions
  use dissoc
  use reader_molecules
  use filters

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_c_gfm,    & !< ?doc? in sync with km_f_sj etc
    km_c_alargm    !< ?doc? in sync with km_f_sj etc

  real*8, dimension(MAX_KM_F_MBLEND, MAX_MODELES_NTOT) :: km_c_pnvj !< ?doc? in sync with km_f_sj etc

  real*8, private, pointer, dimension(:) :: ppa, pb
  private point_ppa_pb

contains

  !=======================================================================================
  !> Calculates the molecular absorption coefficient.

  subroutine kapmol_()
    real*8 t5040, psi
    real*8 csc
    real*8 fe, do_, mm, am, bm, ua, ub, te, cro, rm
    real*8 qv, gv, bv, dv, facto
    integer i_mol, j_set, l, l_ini, l_fin, n, nnv, molidx

    real*8, parameter :: H  = 6.6252E-27,   &
                         C  = 2.997929E+10, &
                         KB = 1.38046E-16,  &
                         C2 = 8.8525E-13

    call log_debug(ENTERING//' kapmol()')

    do i_mol = 1, molidxs%n_on
      molidx = get_molidx(i_mol)

      call point_ppa_pb(km_formula_id(molidx))

      ! print *, 'molidx', molidx, '; formula_id', km_formula_id(molidx), '; nnv'

      nnv = km_nv(molidx)
      fe  = km_fe(molidx)
      do_ = km_do(molidx)
      mm  = km_mm(molidx)
      am  = km_am(molidx)
      bm  = km_bm(molidx)
      ua  = km_ua(molidx)
      ub  = km_ub(molidx)
      te  = km_te(molidx)
      cro = km_cro(molidx)

      !======
      ! This part of the code calculates km_PNVL
      rm = am*bm/mm
      do n = 1,modeles_ntot
        t5040 = modeles_teta(n)/5040
        psi = do_*modeles_teta(n)+2.5*log10(modeles_teta(n))-1.5*log10(rm)-&
              log10(ua*ub)-13.670
        psi = 10.**psi

        do j_set = 1,nnv
          qv = km_qqv(j_set, molidx)
          gv = km_ggv(j_set, molidx)
          bv = km_bbv(j_set, molidx)
          dv = km_ddv(j_set, molidx)

          ! Note that l_ini may be > l_ini, meaning that the current set-of-lines has no
          ! selected lines for current lzero-lfin intervfal
          l_ini = km_f_ln(j_set, i_mol)+1
          l_fin = km_f_ln(j_set+1, i_mol)

          ! l is index within km_f_lmbdam, km_f_sj and km_f_jj
          do l= l_ini, l_fin
            ! PC2003: default value for CSC does not exist physically
            csc = exp(-H*C/KB*modeles_teta(n)/5040.*(te+gv+bv*(km_f_jj(l)+1)*km_f_jj(l)))*   &
                  (2.-cro)*(2.*km_f_jj(l)+1.)*                                             &
                  exp(H*C/KB*modeles_teta(n)/5040.*(dv*(km_f_jj(l)*(km_f_jj(l)+1))**2+2.*bv))

            km_c_pnvj(l,n) = csc*psi*ppa(n)*pb(n)/sat4_pph(n)
          end do


          ! Takes advantage of current j_set loop so it is not necessary to create
          ! another double loop as in the original KAPMOL() to calculate km_c_gfm
          if (n .eq. 1) then
            ! Because gfm does not depend on n, runs this part just once, when n is 1.
            facto = km_fact(j_set, molidx)
            do l= l_ini, l_fin
              km_c_gfm(l) = C2*((1.e-8*km_f_lmbdam(l))**2)*fe*qv*km_f_sj(l)*facto
            end do
          end if
        end do
      end do
    end do ! end of i_mol loop


    do l = 1, km_f_mblend
      km_c_alargm(l) = 0.1
    end do

    call log_debug(LEAVING//' kapmol()')
  end


  !=======================================================================================
  !> Assigns address of variable PPA and PB depending on the molecule formula ID
  !>
  !> This was originally a vector copy element-by-element in old routine KAPMOL. However, as
  !> PPA and PB contents are not changed after the assignment, it is reasonable to just point
  !> to the source vectors (way faster).
  !>
  !> @sa reader_molecules::find_formula_id, reader_molecules::read_molecules

  subroutine point_ppa_pb(formula_id)
    integer, intent(in) :: formula_id

    if (formula_id .gt. NUM_FORMULAE) then
      write (lll, *) 'point_ppa_pb(): invalid formula id (', formula_id, ') must be maximum ', NUM_FORMULAE
      call pfant_halt(lll)
    end if

    select case (formula_id)
      case (1)            ! MgH
        ppa => sat4_pmg
        pb  => sat4_pph
      case (2)            ! C2
        ppa => sat4_ppc2
        pb  => sat4_ppc2
      case (3)            ! CN
        ppa => sat4_ppc2
        pb  => sat4_pn
      case (4)            ! CH
        ppa => sat4_ppc2
        pb  => sat4_pph
      case (5)            ! 13CH
        ppa => sat4_pc13
        pb  => sat4_pph
      case (6)            ! CO
        ppa => sat4_ppc2
        pb  => sat4_po
      case (7)            ! NH
        ppa => sat4_pn
        pb  => sat4_pph
      case (8)            ! OH
        ppa => sat4_po
        pb  => sat4_pph
      case (9)            ! FeH
        ppa => sat4_pfe
        pb  => sat4_pph
      case (10)           ! TiO
        ppa => sat4_pti
        pb  => sat4_po
      case default
        call pfant_halt('Formula id not handled by point_ppa_pb(): '//int2str(formula_id), is_assertion=.true.)
    end select
  end
end


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Declaration and initialization of x_* variables
!>
!> Prefixes:
!> @li x_ -- these variable values may come either from dfile:main or command-line options.

module pfant_x
  use reader_main
  use config
  implicit none

  character*64 :: x_flprefix
  real*8 :: x_llzero, x_llfin

contains

  !> Initializes x_* variables
  !>
  !> Note: to be called after read_main()

  subroutine pfant_init_x()
    if (config_flprefix .eq. '?') then
      x_flprefix = main_flprefix
      call parse_aux_log_assignment('x_flprefix', x_flprefix)
    else
      x_flprefix = config_flprefix
    end if
    if (config_llzero .eq. -1) then
      x_llzero = main_llzero
      call parse_aux_log_assignment('x_llzero', real82str(x_llzero, 2))
    else
      x_llzero = config_llzero
    end if
    if (config_llfin .eq. -1) then
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

!>@verbatim
!> Fantomol avec sous-programmes (MNP) -
!> Calcul possible de 100 angstrom en 100 angstrom.
!>@endverbatim
!>
!> @note Flux sortant est en nu: lambda (x-axis) vs. F(nu) (y-axis)
!>
!> @note Unit of flux: erg*s^-1*cm^-2/(Hz*ster), however (see next note)
!>
!> @note Actually what is called "flux" would be more accurately called "Specific intensity" [Gray Stellar Photospheres 3rd Ed. Eq 5.1]
!>
!> @note Flux absolu sortant a ete multiplie par 10**5
!>
!> @note Existing files are replaced
!>
!>

!>
!> @todo If I find any of the constants being used in another module, I shall move them to a separate module called "constants"
!>
!> Variable patterns
!> @verbatim
!>        m_* module internal variables shared among routines
!>       hy_* hydrogen line-related, calculated by calc_tauh()
!> popadelh_* calculated by popadelh()
!>   turbul_* calculated by turbul()
!>  selekfh_* calculated by selekfh()
!>       bk_* calculated by bk()
!>    popul_* calculated by popul()
!> @endverbatim

module synthesis
  use logging
  use config
  use flin
  use misc_math
  use absoru
  use dissoc
  use filters
  use kapmol
  use readers
  use pfant_x
  implicit none

  !private  ! This statement makes all symbols private by default

  public :: synthesis_ ! subroutine


  !^^^^^ PUBLIC  ^^^^^
  !vvvvv PRIVATE vvvvv

  !=====
  ! Subroutine outputs
  !=====
  ! The following variables have the prefix of the subroutine that is responsible for them.

  !> Calculated by subroutine popul
  real*8, dimension(3,MAX_PARTIT_NPAR, MAX_MODELES_NTOT) :: popul_p

  !> Calculated by subroutine turbul
  real*8, dimension(MAX_MODELES_NTOT) :: turbul_vt

  !> calculated by subroutine popadelh
  real*8, dimension(MAX_ATOMS_F_NBLEND) :: popadelh_corch, popadelh_cvdw
  !> Calculated by subroutine popadelh
  real*8, dimension(MAX_ATOMS_F_NBLEND,MAX_MODELES_NTOT) :: &
   popadelh_pop, popadelh_a, popadelh_delta

  !> Calculated by subroutine selekfh
  real*8, dimension(MAX_DTOT) :: selekfh_fl, selekfh_fcont

  !> Calculated by subroutine bk
  real*8, dimension(0:MAX_MODELES_NTOT) :: bk_b, bk_b1, bk_b2
  !> Calculated by subroutine bk
  real*8, dimension(MAX_MODELES_NTOT) :: bk_kc, bk_kc1, bk_kc2, bk_phn, bk_ph2
  !> Calculated by subroutine bk
  real*8, dimension(MAX_DTOT, MAX_MODELES_NTOT) :: bk_kcd
  !> Calculated by subroutine bk
  real*8, dimension(MAX_DTOT) :: bk_fc

  ! Calculated by calc_tauh(), hydrogen lines-related
  real*8, public :: hy_tauh(MAX_MODELES_NTOT, MAX_DTOT)
  integer, public :: &
   hy_dhp, & !< maximum value of dhpy
   hy_dhm    !< minimum value of dhmy

  ! Calculated by calc_tauhi(), auxiliary of calc_tauh()
  integer :: &
   m_dhmi, & !< Calculated by subroutine calc_tauhi
   m_dhpi    !< Calculated by subroutine calc_tauhi

  !=====
  ! Constants available to all subroutines within this module
  !=====

  real*8, parameter :: & !< ?doc?
    C = 2.997929E+10,  & !< ?doc?
    H = 6.6252E-27,    & !< ?doc?
   KB = 1.38046E-16,   & !< ?doc?
   C1 = 4.8298E+15,    & !< ?doc?
   C2 = 8.8525E-13,    & !< ?doc?
   C4 = 2.1179E+8,     & !< ?doc?
   C6 = 3.76727E+11,   & !< ?doc?
   DEUXR = 1.6634E+8     !< ?doc?

  real*8, parameter :: &
   C5 = 2.*PI* (3.*PI**2/2.44)**0.4   !< ?doc?


  !=====
  ! Module variables initialized in subroutine synthesis_() and shared among other routines.
  !=====
  real*8 :: &
   m_lzero, &                 !< initial lambda of current ikey-th iteration
   m_lfin, &                  !< final lambda of current ikey-th iteration
   m_lambd, &                 !< lambda 1/2
   m_ttd(MAX_DTOT), &
   m_ecart(MAX_ATOMS_F_NBLEND), & !< MT: some sort of delta lambda
   m_gfal(MAX_ATOMS_F_NBLEND), &
   m_ecartm(MAX_KM_F_MBLEND), &
   m_ilzero
  integer :: &
    m_dtot!,    &  !< number of different wavelenghts for which flux will be calculated at each ikey-iteration
    !m_ilzero      !< closest integer multiple of 100 that is <= m_lzero

contains

  !======================================================================================================================
  !> @todo make file replacing clear somewhere because it was not the original behaviour

  subroutine synthesis_()
    ! Units for output files
    integer, parameter :: &
     UNIT_SPEC  = 17, &
     UNIT_CONT  = 19, &
     UNIT_NORM  = 20, &
     UNIT_LINES = 32, &
     UNIT_LOG   = 31

    real*8 fn(MAX_DTOT)

    integer i, i1, i2, k, d, &
     ikey,    & ! ikey-th main_aint-large calculation interval
     ikeytot    ! total number of main_aint-large calculation intervals

    real*8 l0, lf, alzero, tetaef, xlfin, xlzero

    ! auxiliary variables to time the different parts
    real :: start, finish, start0, finish0

    ! output filenames with relative path included
    character(len=:), allocatable :: fn_spec, fn_cont, fn_norm

    call cpu_time(start0)

    !=====
    ! Setup
    !=====

    !> @todo ?doc? The reason why values read from file are being overwritten should be explained.
    absoru2_abhel = modeles_nhe
    !> @todo issue using modeles_asalog instead of main_afstar
    absoru2_abmet = absoru2_abmet*10.**modeles_asalog

    tetaef = 5040/main_teff

    !-----
    ! Output files opened here and left open until the end
    !-----
    ! Note that existing files are replaced
    fn_spec = trim(x_flprefix)//'.spec'
    fn_cont = trim(x_flprefix)//'.cont'
    fn_norm = trim(x_flprefix)//'.norm'
    open(unit=UNIT_SPEC, file=fn_spec, status='replace')  ! spectrum
    open(unit=UNIT_CONT, file=fn_cont, status='replace')  ! continuum
    open(unit=UNIT_NORM, file=fn_norm, status='replace')  ! normalized
    !--- open(unit=UNIT_LINES,file=config_fn_lines, status='replace')               ! outfile:lines
    !--- open(unit=UNIT_LOG,  file=config_fn_log, status='replace')                 ! log.log


    !=====
    ! Calculation begins!
    !=====

    call cpu_time(start)

    call turbul()

    call log_debug('turbul() OK!')
    call cpu_time(finish)
    print '("TURBUL() Time = ",f6.3," seconds.")',finish-start

    ! -- III --
    ! Calcul de quant  ne dependant que du metal et du modele
    ! Population du niv fond des ions
    call cpu_time(start)

    call popul()

    call cpu_time(finish)
    print '("POPUL() Time = ",f6.3," seconds.")',finish-start

    ! -- IV --
    ! Calcul des quantites ne dependant que du
    ! Modele et de lambda : bk_b(n)   bk_kc(n)   bk_fc ISSUE probably wrong comment
    call cpu_time(start)

    call sat4()
    call log_debug('sat4() OK!')

    call cpu_time(finish)
    print '("SAT4() Time = ",f6.3," seconds.")',finish-start


    ! initial calculation sub-interval
    xlzero = x_llzero-LAMBDA_STRETCH
    xlfin = xlzero+main_aint+LAMBDA_STRETCH

    ! discovers the number of iterations
    if(xlfin .ge. (x_llfin+LAMBDA_STRETCH)) then
      ikeytot = 1
    else
      do i = 2,25000
        xlfin = xlfin+main_aint
        if(xlfin .ge. (x_llfin+LAMBDA_STRETCH)) exit
      end do
      ikeytot = i
    end if

    !m_lzero = x_llzero-LAMBDA_STRETCH
    !m_lfin = m_lzero+main_aint+LAMBDA_STRETCH
    ikey = 1


    !l0 = x_llzero-LAMBDA_STRETCH/2
    !lf = x_llfin+LAMBDA_STRETCH/2
    l0 = x_llzero-LAMBDA_STRETCH
    lf = x_llfin+LAMBDA_STRETCH


    !=====
    ! Main loop
    !=====
    call log_progress(0, ikeytot)
    do while (.true.)
      !=====
      ! Initialization of lambdas / delta lambdas for current iteration
      !=====

      ! Determines the calculation interval [m_lzero, m_lfin]
      ! Note: two extra points are added to the left and to the right, as without this
      ! some interpolation routines (such as ftlin3) may crash. 
      if (ikey .eq. 1) then
        m_lzero = l0-main_pas
      else
        m_lzero = x_llzero+main_aint*(ikey-1)+main_pas-main_pas
      end if

      if (ikey .eq. ikeytot) then
        m_lfin = lf+main_pas
      else
        m_lfin = x_llzero+main_aint*ikey+main_pas
      end if

      ! Note: (m_lfin-m_lzero) is constant except in the last iteration where m_lfin may be corrected
      m_dtot = int((m_lfin-m_lzero)/main_pas + 1.0005)

      ! spill check
      if(m_dtot .gt. MAX_DTOT) then
        call pfant_halt('dtot = '//int2str(m_dtot)//' exceeds maximum of MAX_DTOT='//&
         int2str(MAX_DTOT))
      end if

      m_lambd = (m_lzero+m_lfin)/2
      m_ilzero = int((m_lzero-main_pas)/main_pas)*main_pas  ! some reference ilzero smaller than lzero
      alzero = m_lzero-m_ilzero
      do d = 1,m_dtot
        m_ttd(d) = alzero+main_pas*(d-1)
      end do

      !#logging
      call log_info('/\/\/\ Calculation step '//int2str(ikey)//'/'//int2str(ikeytot)//&
        ' /\/\/\')
      501 format(2x,2x,'m_lzero=',f10.3,2x,'m_lfin=',&
       f10.3,2x,'m_dtot=',i7,2x,'m_lambd 1/2=',f10.3, 2x, 'm_ilzero=',f10.3)
      write(lll,501) m_lzero, m_lfin, m_dtot, m_lambd, m_ilzero
      call log_info(lll)

      !=====
      ! Beginning of iteration
      !=====

      ! ????????????????
      call cpu_time(start)

      call bk()

      call cpu_time(finish)
      print '("BK() Time = ",f6.3," seconds.")',finish-start


      ! hydrogen lines
      call cpu_time(start)

      call calc_tauh()

      call cpu_time(finish)
      print '("CALC_TAUH() Time = ",f6.3," seconds.")',finish-start

      ! -- V --
      ! Quantites dependant de la raie et du modele
      call cpu_time(start)

      call filter_atoms(m_lzero, m_lfin)

      call cpu_time(finish)
      print '("FILTER_ATOMS() Time = ",f6.3," seconds.")',finish-start


      if(atoms_f_nblend .gt. 0) then
        call cpu_time(start)

        call popadelh()

        call cpu_time(finish)
        print '("POPADELH() Time = ",f6.3," seconds.")',finish-start


        ! -- VI --
        ! Calcul du coefficient d absorption selectif et calcul du spectre
        do k = 1, atoms_f_nblend
          m_gfal(k) = atoms_f_gf(k)*C2*(atoms_f_lambda(k)*1.e-8)**2
          m_ecart(k) = atoms_f_lambda(k)-m_lzero+main_pas
        end do
      end if

      call cpu_time(start)

      call filter_molecules(m_lzero, m_lfin)

      call cpu_time(finish)
      print '("FILTER_MOLECULES() Time = ",f6.3," seconds.")',finish-start


      call cpu_time(start)

      call kapmol_()

      call cpu_time(finish)
      print '("KAPMOL_() Time = ",f6.3," seconds.")',finish-start



      call cpu_time(start)

      call selekfh()

      call cpu_time(finish)
      print '("SELEKFH() Time = ",f6.3," seconds.")',finish-start


      !=====
      ! Saving...
      !=====

      call cpu_time(start)


      !li = int(dble(LAMBDA_STRETCH)/2/main_pas)
      !i1 = li+1  ! initial recording index
      !i2 = m_dtot - li
      !if (m_lfin .ge. (x_llfin+LAMBDA_STRETCH)) then
      !  i2 = int((x_llfin+10.-m_lzero)/main_pas + 1.0005)
      !end if
      !itot = i2-i1+1
      !do d = i1,i2

      ! i1 and i2 are the initial and final indexes of selekfh_fl, selekfh_fcont, and fd
      ! that will be written to output file.
      i1 = 2
      if (ikey .eq. ikeytot) then 
        i2 = m_dtot - 1
      else
        ! The last value of flux in iteration ikey equals
        ! the first value of flux in iteration ikey+1.
        ! Note that nulbad is aware of this and discards these redundant points.
        ! PyFANT is also aware of this.
        i2 = m_dtot
      end if
      !if (m_lfin .ge. (x_llfin+LAMBDA_STRETCH)) then
      !  i2 = int((x_llfin+10.-m_lzero)/main_pas + 1.0005)
      !end if
      ! itot = i2-i1+1
      !do d = i1,i2
      do d = 1, m_dtot
        selekfh_fl(d) = selekfh_fl(d)*(10.**5)
        selekfh_fcont(d) = selekfh_fcont(d)*(10.**5)
        fn(d) = selekfh_fl(d) / selekfh_fcont(d)  ! normalized spectrum
      end do

      ! Writes results for current iteration into open files
      ! call write_lines_fort91()                        ! outfile:lines and fort.91
      ! call write_log()                                ! log.log
      call write_spec_item(UNIT_SPEC, selekfh_fl)     ! spectrum
      call write_spec_item(UNIT_CONT, selekfh_fcont)  ! continuum
      call write_spec_item(UNIT_NORM, fn)             ! normalized

      !707 format(1x,'ikey=',i10,2x,'lzero=',f10.3,2x,'lfin=',f10.3, 2x,'i1=',i7,2x,'i2=',i7)
      !write(lll,707) ikey, m_lzero, m_lfin, i1, i2
      !call log_info(lll)

      call cpu_time(finish)
      print '("SAVING Time = ",f6.3," seconds.")',finish-start

      !=====
      ! Preparing for next iteration
      !=====

      call log_progress(ikey, ikeytot)

      ikey = ikey+1
      if (ikey .gt. ikeytot) exit  ! main loop smooth exit

      !m_lzero = m_lzero+main_aint
      !m_lfin = m_lfin+main_aint
      !if(m_lfin .gt. (x_llfin+LAMBDA_STRETCH)) m_lfin = x_llfin+LAMBDA_STRETCH
    end do  ! main loop

    close(UNIT_SPEC)
    close(UNIT_CONT)
    close(UNIT_NORM)
    !--- close(UNIT_LOG)
    !--- close(UNIT_LINES)

    !#logging
    call log_info('Note: flux sortant est en nu: Fnu x lambda')
    call log_info('Note: flux absolu sortant a ete multiplie par 10**5')

    call log_info('File '//trim(fn_spec)//' successfully created.')
    call log_info('File '//trim(fn_cont)//' successfully created.')
    call log_info('File '//trim(fn_norm)//' successfully created.')

    call log_debug(LEAVING//'synthesis_()')

    call cpu_time(finish0)
    print '("SYNTHESIS_() Time = ",f6.3," seconds.")',finish0-start0

  contains

    ! These subroutines have total knowledge of the variable names and values that appear inside
    ! their parent subroutine synthesis_().
    ! http://www.personal.psu.edu/jhm/f90/statements/contains.html

    !> Used to write the "spectrum", "continuum", and "normalized".
    !> Their writing pattern is very similar. THe header is the same,
    !> only the "ITEM" changes from file to file.

    subroutine write_spec_item(unit_, item)
      !> unit number, either UNIT_SPEC, UNIT_CONT, UNIT_NORM
      integer, intent(in) :: unit_
      !> either selekfh_fl, selekfh_fcont, or fn
      real*8, intent(in) :: item(:)
      real*8 amg
      amg = main_xxcor(8)  ! ?doc? MT: I think that somebody did this because the "alpha-enhanced" is specified nowhere. See also in write_log()

      1130 format(i5, a20, 5f15.5, 4f10.1, i10, 4f15.5)
!      write(unit_, 1130)       &
!       ikeytot,                &  ! fixed (same value for all iterations)
!       modeles_tit,            &  ! fixed
!       tetaef,                 &  ! fixed
!       main_glog,              &  ! fixed
!       main_asalog,            &  ! fixed
!       modeles_nhe,            &  ! fixed
!       amg,                    &  ! fixed
!       l0,                     &  ! fixed
!       lf,                     &  ! fixed
!       m_lzero,                &  ! changes (value changes with each iteration)
!       m_lfin,                 &  ! changes
!       itot,                   &  ! changes
!       main_pas,               &  ! fixed
!       main_echx,              &  ! fixed
!       main_echy,              &  ! fixed
!       main_fwhm                  ! fixed
      write(unit_, 1130)       &
       ikeytot,                &  ! fixed (same value for all iterations)
       modeles_tit,            &  ! fixed
       tetaef,                 &  ! fixed
       main_glog,              &  ! fixed
       main_asalog,            &  ! fixed
       modeles_nhe,            &  ! fixed
       amg,                    &  ! fixed
       l0,                     &  ! fixed
       lf,                     &  ! fixed
       m_lzero+main_pas*(i1-1),                &  ! changes (value changes with each iteration)
       m_lzero+main_pas*(i2-1),                 &  ! changes
       i2-i1+1,                   &  ! old nulbad, not used
       main_pas,               &  ! fixed
       main_echx,              &  ! fixed
       main_echy,              &  ! fixed
       main_fwhm                  ! fixed
      write(unit_,'('//int2str(MAX_DTOT)//'f15.5)') (item(d), d=i1,i2)  !(item(d), d=i1,i2)
    end

  end subroutine synthesis_

  !======================================================================================================================
  !> @todo issue ?doc? (MT): Related to line broadening due to Doppler effect caused by microturbulent velocity.

  subroutine turbul()
    integer i, nt2, n

    call log_debug('entree des turbul')
    if(main_ivtot .eq. 1)   then
      call log_debug('vt constant')
      do n = 1, modeles_ntot
        turbul_vt(n) = main_vvt(1)*1e5
      end do
    else
      101 format(10f8.3)
      call log_debug('vt variable avec la profondeur')
      call log_debug('    log to')
      write(lll,101) (main_tolv(i),i=1,main_ivtot)
      call log_debug(lll)
      call log_debug('    vt')
      write(lll,101) (main_vvt(i),i=1,main_ivtot)
      call log_debug(lll)

      if(config_interp .eq. 1) then
        call ftlin3(main_ivtot, main_tolv, main_vvt, modeles_ntot, modeles_t5l, turbul_vt)
      elseif (config_interp .eq. 2) then
        !> @todo issue ask blb config_interp was hard-switched to 1, config_interp=2 path needs testing
        call ft2(main_ivtot, main_tolv, main_vvt, modeles_ntot, modeles_t5l,turbul_vt)
      end if


      nt2 = modeles_ntot-2
      do n = 1, nt2, 3
        102 format(3(i5,2f8.3,5x))
        write(lll,102) n,modeles_t5l(n),turbul_vt(n),(n+1), modeles_t5l(n+1), &
         turbul_vt(n+1),(n+2),modeles_t5l(n+2),turbul_vt(n+2)
        call log_debug(lll)
      end do

      do n = 1, modeles_ntot
        turbul_vt(n) = turbul_vt(n)*1e5
      end do
    end if


    if(main_ivtot .eq. 1) then
      131 format(' v microturbulence constante  =',f6.1,'km/s')
      write(lll,131) main_vvt(1)
      call log_debug(lll)
    else
      call log_debug('v microturbulence variable avec profondeur')
    end if

    return
  end subroutine


  !======================================================================================================================
  !> Calcule la pop du niv fond de l'ion pour tous les partit_NPAR atomes de
  !> la table des fonctions de partition ,a tous les niv du modele
  !>
  !> 40 elements, 50 niveaux de modele, 3 niv d'ionisation par elem.
  !> Partit donnee pour 33 temperatures au plus ds la table.

  subroutine popul()
    real*8 u(3), alistu(MAX_MODELES_NTOT*2), ue(MAX_MODELES_NTOT), tt(MAX_MODELES_NTOT*2), &
     aa, bb, uuu, x, y, t, tki2
    integer j, k, kmax, l, n

    do n = 1, modeles_ntot
      t = 5040./modeles_teta(n)  !> @todo I think the program deserves a modeles_T5040 because this is calculated everywhere!!!
      ue(n) = C1*KB*t/modeles_pe(n)*t**1.5
      do j = 1, partit_npar
        kmax = partit_jkmax(j)
        tt(1) = partit_tini(j)
        do  l=1,3
          do  k=1,kmax
            tt(k+1) = tt(k) + partit_pa(j)
            alistu(k) = partit_tabu(j,l,k)
          end do

          if (modeles_teta(n) .lt. tt(kmax-1) ) then
            ! interpolation parabolique
            uuu = ft(modeles_teta(n),kmax,tt,alistu)
          else
            ! interpolation lineaire entre 2 derniers pts
            aa = (alistu(kmax)-alistu(kmax-1)) / partit_pa(j)
            bb = alistu(kmax-1) - aa * tt(kmax-1)
            uuu = aa*modeles_teta(n) + bb
          end if

          u(l) = exp(2.302585*uuu)
        end do

        x=u(1) / (u(2)*ue(n)) * 10.**(partit_ki1(j)*modeles_teta(n))
        tki2= partit_ki2(j) * modeles_teta(n)

        !> @todo ?doc? ask blb why 77? Needs some comment on it.
        if (tki2 .ge. 77.) then
          y = 0.
          popul_p(3,j,n) = 0.
        else
          y = u(3)*ue(n)/u(2) * 10.**(-partit_ki2(j)*modeles_teta(n))
          popul_p(3,j,n) = (1./u(3))*(y/(1.+x+y))
        end if
        popul_p(2,j,n) = (1./u(2))*(1./(1.+x+y))
        popul_p(1,j,n) =  (1./u(1))*(x/(1.+x+y))
        end do
      end do
    return
  end subroutine


  !======================================================================================================================
  !> Calcule la population au niveau inferieur de la transition
  !> la largeur doppler popadelh_DELTA et le coefficient d'elargissement
  !> le "popadelh_a" utilise dans le calcul de H(popadelh_a,v)
  !

  subroutine popadelh()
    implicit none
    character*1 isi, iss
    integer j, k, ioo, iopi, n
    real*8 kies,kii,nul, ahnul, alphl(MAX_MODELES_NTOT), gamma, gh, t, tap, top, vrel
    data isi/' '/, iss/' '/

    do k = 1, atoms_f_nblend
      ! Search: finds j-th atomic symbol in partit_el matching atoms_f_elem(k)
      ! This is a "inner join"
      do j = 1,partit_npar
        if(partit_el(j) .eq. atoms_f_elem(k)) go to 15
      end do

      104 format('Manque les fcts de partition du ', a2)
      write(lll,104) atoms_f_elem(k)
      call pfant_halt(lll)

      15 continue

      popadelh_corch(k) = 0.
      popadelh_cvdw(k) = 0
      ioo = atoms_f_ioni(k)

      ! fort.77 disabled until someone misses it.
      ! write (77,*) atoms_f_elem(k),atoms_f_lambda(k)

      ! ?doc?
      ! If "ch" variable from dfile:atoms is zero, overwrites it with a calculated value.
      ! See also read_atoms(), variable atoms_gr, which is also overwritten.
      if(atoms_f_ch(k) .lt. 1.e-37)  then
        !> @todo optimize create atoms_partit_ki1, atoms_partit_ki2 to be filled by inner join upon reading atoms

        kies = (12398.54/atoms_f_lambda(k)) + atoms_f_kiex(k)
        if (ioo .eq. 1) then
          kii = partit_ki1(j)
        else if (ioo .eq. 2) then
          kii = partit_ki2(j)
        else
          ! Better to give error than to silently do wrong calculations
          call pfant_halt('popadelh() found invalid ionization level: '//int2str(ioo), is_assertion=.true.)
        end if

        if(popadelh_corch(k) .lt. 1.e-37)   then
          popadelh_corch(k) = 0.67 * atoms_f_kiex(k) +1
        end if

        ! 125 format(3x ,' pour',f9.3,'   on calcule ch ', 'van der waals et on multiplie par ',f7.1)
        ! write(6,125)  atoms_f_lambda(k), popadelh_corch(k)
        popadelh_cvdw(k)= calch(kii, ioo, atoms_f_kiex(k), isi, kies, iss)

        atoms_f_ch(k) = popadelh_cvdw(k) * popadelh_corch(k)
      end if

!
      if(atoms_f_ch(k) .lt. 1.e-20) then
        ! This will be the case most of the time
        iopi = 1
      else
        iopi = 2
      end if

      do n = 1, modeles_ntot
        t = 5040./modeles_teta(n)
        nul = C* 1.e+8 /atoms_f_lambda(k)
        ahnul = H*nul
        alphl(n) = exp(-ahnul/(KB*t))

        tap = 1.-alphl(n)
        top = 10.**(-atoms_f_kiex(k)*modeles_teta(n))

        if(atoms_f_elem(k) .eq. ' O') then
          ! #NOXIG: oxygen is treated differently
          popadelh_pop(k,n) = top*tap*popul_p(ioo,j,n)*sat4_po(n)/sat4_pph(n)
        else
          popadelh_pop(k,n) = popul_p(ioo,j,n)*top*tap
        end if

        popadelh_delta(k,n) =(1.e-8*atoms_f_lambda(k))/C*sqrt(turbul_vt(n)**2+DEUXR*t/partit_m(j))
        vrel = sqrt(C4*t*(1.+1./partit_m(j)))
        if (iopi .eq. 1) then
          gh = C5*atoms_f_ch(k)**0.4*vrel**0.6
        else
          gh = atoms_f_ch(k) + popadelh_corch(k)*t
        end if
        gamma = atoms_f_gr(k)+(atoms_f_ge(k)*modeles_pe(n)+gh*(bk_phn(n)+1.0146*bk_ph2(n)))/(KB*t)
        popadelh_a(k,n) = gamma*(1.e-8*atoms_f_lambda(k))**2 / (C6*popadelh_delta(k,n))
        write(45,*) popadelh_a(k,n)
      end do
    end do
  end



  !======================================================================================================================
  !> Sets the Voigt profile using Hjertings' constants; calculates the flux and continuum
  !>
  !> @note Most of the calculation time is spent inside this routine
  !>
  !> @note Convolution for molecules uses Gaussian profile.
  !>
  !> @todo MT+JT Decision on variable MM: logic suggests that there should be one MM per molecule, so we are going to make

  subroutine selekfh()
    integer d, k, l, n
    real*8 :: bi(0:MAX_MODELES_NTOT)
    real*8, dimension(MAX_ATOMS_F_NBLEND) :: &
     ecar  !, & !     ecartl, &
    real*8, dimension(MAX_MODELES_NTOT) :: &
     kap,    &
     kci,    &
     kappt
    real*8, dimension(MAX_KM_F_MBLEND) :: &
     ecarm !, ecartlm
    real*8 :: &
     deltam(max_km_f_mblend,MAX_MODELES_NTOT), &
     phi, t, v, vm, &
     kam, kappam, kappa, kak

    if (atoms_f_nblend .ne. 0) then
      do k = 1,atoms_f_nblend
        ecar(k) = m_ecart(k)
      end do
    end if

    if (km_f_mblend .ne. 0) then
      ! do l = 1, km_f_mblend
      !   m_ecartm(l) = km_f_lmbdam(l)-m_lzero + main_pas
      ! end do
      ! do k=1,km_f_mblend
      !  ecarm(k) = m_ecartm(k)
      ! end do
      do k=1,km_f_mblend
        ecarm(k) = km_f_lmbdam(k)-m_lzero + main_pas
      end do
    end if

    do d = 1, m_dtot
      ! Shifts the ecar and ecarm delta lambda vectors
      if (atoms_f_nblend .ne. 0) then
        do k = 1, atoms_f_nblend
          ecar(k) = ecar(k)-main_pas
        end do
      end if
      if (km_f_mblend .ne. 0) then
        do k = 1,km_f_mblend
          ecarm(k) = ecarm(k)-main_pas
        end do
      end if

      do n = 1,modeles_ntot
        kappa = 0.
        kappam = 0.
        t = 5040./modeles_teta(n)

        ! atomes
        if(atoms_f_nblend .eq. 0) go to 260

        do  k = 1,atoms_f_nblend
          if(abs(ecar(k)) .gt. atoms_f_zinf(k)) then
            kak = 0.
          else
            v = abs(ecar(k)*1.e-8/popadelh_delta(k,n))
            phi = hjenor(popadelh_a(k,n), v, popadelh_delta(k,n))

            if(atoms_f_elem(k) .eq. ' O') then
              ! #NOXIG: oxygen is a particular case here
              kak = phi * popadelh_pop(k,n) * m_gfal(k)
            else
              kak = phi * popadelh_pop(k,n) * m_gfal(k) * atoms_f_abonds_abo(k)
            end if
          end if

          kappa = kappa + kak
        end do   !  fin bcle sur k

        260 continue

        ! molecules
        if(km_f_mblend .eq. 0) go to 250
        do l = 1, km_f_mblend
          !if(abs(ecartlm(l)) .gt. km_c_alargm(l))  then
          if(abs(ecarm(l)) .gt. km_c_alargm(l))  then
            kam = 0.
          else
            deltam(l,n) = (1.e-8*km_f_lmbdam(l))/C*sqrt(turbul_vt(n)**2+DEUXR*t/km_f_mm(l))
            vm = abs(ecarm(l)*1.e-08/deltam(l,n))
            phi = (exp(-vm**2))/(RPI*deltam(l,n))
            kam = phi*km_c_gfm(l)*km_c_pnvj(l,n)
          end if
          kappam = kappam+kam
        end do   !  fin bcle sur l

        250 continue
        kappt(n) = kappa+kappam

        kci(n) = bk_kcd(d,n)
        kap(n) = kappt(n)+kci(n)
        bi(n) = ((bk_b2(n)-bk_b1(n))*(float(d-1)))/(float(m_dtot-1)) + bk_b1(n)
      end do


      bi(0) = ((bk_b2(0)-bk_b1(0))*(float(d-1)))/(float(m_dtot-1)) + bk_b1(0)

      !if (d .eq. 1 .or. d .eq. m_dtot) then
      !  150 format(' d=',i5,2x,'kci(1)=',e14.7,2x,'kci(ntot)=',e14.7,10x,'kappa(1)=',e14.7,2x,'kappa(ntot)=',e14.7)
      !  152 format(10x,'kappam(1)=',e14.7,2x,'kappam(ntot)=',e14.7)
      !  151 format(' d=',i5,2x,'bi(0)=',e14.7,2x,'bi(1)=',e14.7,2x,'bi(ntot)=',e14.7)
      !
      !  write(lll,151) d,bi(0),bi(1),bi(modeles_ntot)
      !  call log_debug(lll)
      !  write(lll,150) d,kci(1),kci(modeles_ntot),kappa(1),kappa(modeles_ntot)
      !  call log_debug(lll)
      !  !write(lll,152)kappam(1),kappam(modeles_ntot)
      !  !call log_debug(lll)
      !end if

      if ((d .lt. hy_dhm) .or. (d .ge. hy_dhp)) then
        ! without hydrogen lines
        ! write(*,*) 'NO H LINES BECAUSE d=',d,'; hy_dhm=',hy_dhm,'; hy_dhp=',hy_dhp
        call flin1(kap,bi,modeles_nh,modeles_ntot,main_ptdisk,main_mu, config_kik)
        selekfh_fl(d) = flin_f
      else
        ! with hydrogen lines
        ! write(*,*) 'YES H LINES BECAUSE d=',d,'; hy_dhm=',hy_dhm,'; hy_dhp=',hy_dhp
        call flinh(kap, bi, modeles_nh, modeles_ntot, main_ptdisk, main_mu, config_kik, hy_tauh(:, d))
        selekfh_fl(d) = flin_f
      end if

      ! Dez 03-P. Coelho - calculate the continuum and normalized spectra
      call flin1(kci,bi,modeles_nh,modeles_ntot,main_ptdisk,main_mu, config_kik)
      selekfh_fcont(d) = flin_f
    end do  ! fin bcle sur d
  end


  !======================================================================================================================
  !> Calculates the flux in the continuum.
  !> @todo There is a lot of calculation here that is independent from m_lzero and m_lfin (optimize?)
  !>
  !> @todo log_pe not used yet

  subroutine bk()
    real*8 nu, llzero, llfin, nu1, nu2, &
     alph_n ! old ALPH, which was a vector, but I realized it is used only inside loop, no need for vector
    real*8, dimension(2, MAX_MODELES_NTOT) :: kcj
    real*8, dimension(2) :: kcn, lambdc
    real*8 :: fttc(MAX_DTOT)
    real*8 c3, c31, c32, fc1, fc2, t, tet0, ahnu, ahnu1, ahnu2, alph0, alph01, alph02
    integer d, j, n

    call log_debug(ENTERING//'bk()')

    llzero = m_lzero
    llfin  = m_lfin
    nu1 = C* 1.e+8 /m_lzero
    ahnu1 = h*nu1
    c31 = (2*ahnu1) * (nu1/C)**2

    do n = 1,modeles_ntot
      t = 5040./modeles_teta(n)
      alph_n = exp(-ahnu1/(KB*t))
      bk_b1(n) = c31 * (alph_n/(1.-alph_n))
      call absoru_(llzero,modeles_teta(n),log10(modeles_pe(n)),1,1,1,1,2, .false.)
      bk_kc1(n) = absoru_totkap(1)
    end do

    nu2 = C* 1.e+8 /m_lfin
    ahnu2 = H*nu2
    c32 =(2*ahnu2) * (nu2/C)**2
    do n = 1,modeles_ntot
      !> @todo: calculate this "T" somewhere else, this is calculated all the time (optimize)
      t = 5040./modeles_teta(n)
      alph_n = exp(-ahnu2/(KB*t))
      bk_b2(n) = c32 * (alph_n/(1.-alph_n))
      call absoru_(llfin,modeles_teta(n),log10(modeles_pe(n)),1,1,1,1,2, .false.)
      bk_kc2(n) = absoru_totkap(1)
    end do

    nu = C* 1.e+8 /m_lambd
    ahnu = H*nu
    c3 =(2*ahnu) * (nu/C)**2
    do n=1,modeles_ntot
      t=5040./modeles_teta(n)
      alph_n = exp(-ahnu/(KB*t))
      bk_b(n) = c3 * (alph_n/(1.-alph_n))
      call absoru_(m_lambd,modeles_teta(n),log10(modeles_pe(n)),1,1,1,1,2, .false.)
      bk_phn(n) = absoru_znh(absoru2_nmeta+4) *KB * t
      bk_ph2(n) = absoru_znh(absoru2_nmeta+2) *KB * t
      bk_kc(n) = absoru_totkap(1)
    end do

    tet0 = fteta0(modeles_pg, modeles_teta, modeles_ntot)     !on extrapole modeles_teta pour modeles_nh=0
    t = 5040./tet0

    alph01 = exp(-ahnu1/(KB*t))
    bk_b1(0) = c31 * (alph01/(1.-alph01))
    call flin1(bk_kc1,bk_b1,modeles_nh,modeles_ntot,main_ptdisk,main_mu,config_kik)
    fc1 = flin_f

    alph02 = exp(-ahnu2/(KB*t))
    bk_b2(0) = c32 * (alph02/(1.-alph02))
    call flin1(bk_kc2,bk_b2,modeles_nh,modeles_ntot,main_ptdisk,main_mu,config_kik)
    fc2 = flin_f

    alph0 = exp(-ahnu/(KB*t))
    bk_b(0) = c3 * (alph0/(1.-alph0))
    call flin1(bk_kc,bk_b,modeles_nh,modeles_ntot,main_ptdisk,main_mu,config_kik)
    bk_fc = flin_f

    lambdc(1) = m_lzero-m_ilzero
    lambdc(2) = m_lfin-m_ilzero
    do n=1,modeles_ntot
      kcj(1,n)=bk_kc1(n)
      kcj(2,n)=bk_kc2(n)
    end do
    do n=1,modeles_ntot
      do j=1,2
        kcn(j)=kcj(j,n)
      end do
!write(*,*)'AQUI QCHEGOU'
!write(*,*) 'lambdc', lambdc
!write(*,*) 'kcn', kcn
!write(*,*) 'm_dtot', m_dtot
!write(*,*) 'm_ttd', m_ttd(1), m_ttd(m_dtot)
!write(*,*) 'fttc', fttc(1), fttc(m_dtot)
!write(*,*)'---------------'
      call ftlin3(2,lambdc,kcn,m_dtot,m_ttd,fttc)
!      write(*,*)'MAS PASSOU DO FTLIN3!!!!!!!!!!!!!!!!!!!!'
      do d = 1,m_dtot
        bk_kcd(d,n) = fttc(d)  !> @todo these vector copies... pointer operations could speed up considerably here (optimize)
      end do
    end do

    !#logging
    153 format(' bk_kcd(1,1)=',e14.7,2x,'bk_kcd(1,ntot)=',e14.7)
    154 format(' bk_kcd(dtot,1)=',e14.7,2x,'bk_kcd(dtot,ntot)=',e14.7)
    write(lll,153) bk_kcd(1,1),bk_kcd(1,modeles_ntot)
    call log_debug(lll)
    write(lll,154) bk_kcd(m_dtot,1),bk_kcd(m_dtot,modeles_ntot)
    call log_debug(lll)

    call log_debug(LEAVING//'bk()')
  end


  !=======================================================================================
  !> Hydrogen lines-related calculation: calculates hy_tauh, hy_dhp, hy_dhm

  subroutine calc_tauh()
    integer :: im, dhmy(MAX_FILETOH_NUM_FILES), dhpy(MAX_FILETOH_NUM_FILES), ih, iht, imy
    real*8 :: allhy

    hy_tauh = 0.  ! initializes all elements to zero

    im = 0
    do ih = 1,filetoh_num_files
      allhy = filetoh_llhy(ih)-m_lzero

      if (((allhy .gt. 0) .and. (allhy .le. (main_aint+H_LINE_WIDTH+LAMBDA_STRETCH))) .or. &
          ((allhy .lt. 0.) .and. (allhy .ge. (-H_LINE_WIDTH)))) then
        im = im+1
        iht = ih

        !#logging
        712 format(1x,'im=',i3,2x,'lambda h=',f8.3,2x,'filename=',a,2x,'ih=',i5)
        write(lll,712) im, filetoh_llhy(ih), ''''//trim(filetoh_filenames(iht))//'''', iht
        call log_info(lll)

        call calc_tauhi(ih)

        dhmy(im) = m_dhmi
        dhpy(im) = m_dhpi
      end if
    end do

    imy = im
    if(imy .ne. 0) then
      write(lll,*) (dhmy(im), im=1,imy)
      call log_debug('DHMY ==> '//lll)
      write(lll,*) (dhpy(im), im=1,imy)
      call log_debug('DHPY ==> '//lll)

      hy_dhm = mini(dhmy, imy, 1, imy)
      hy_dhp = maxi(dhpy, imy, 1, imy)

      !!!!!do n = 1,modeles_ntot
      !!!!!  do d = 1,dtot
      !!!!!    tauh(n, d) = 0.0
      !!!!!    do im = 1,imy
      !!!!!      tauh(n, d) = tauh(d,n)+tauhy(im,d,n)
      !!!!!    end do
      !!!!!  end do
      !!!!!end do
    else
      hy_dhm = 0
      hy_dhp = 0
    end if
  end

  !=======================================================================================
  !> Adds contribution of single file to hy_tauh; also calculates m_dhmi and m_dhpi
  !>
  !> @todo top test the pointers

  subroutine calc_tauhi(i_file)
    integer, intent(in) :: i_file !< index pointing to element of the filetoh_* arrays
    integer d, j, jj, jma1, n, &
     jjmax, &
     now_jmax ! jmax of file i_file

    real*8, dimension(MAX_FILETOH_JJMAX) :: llambdh, allh, tauhn
    real*8 :: tth(MAX_FILETOH_JJMAX, MAX_MODELES_NTOT)
    real*8 :: ftth(MAX_DTOT)

    real*8 del
    ! pointers, point to information within filetoh_* matrices at the beginning of
    ! a specific file.
    ! This simplifies the notation within the loop below and is probably faster than
    ! accessing the variables filetoh_* directly
    real*8, pointer, dimension(:,:) :: now_th
    real*8, pointer, dimension(:)   :: now_lambdh

    ! write(*,*) 'CALCULATING TAUHI FOR FILE ', filetoh_filenames(i_file)

    now_jmax   = filetoh_jmax(i_file)
    now_th     => filetoh_th(i_file, :, :)
    now_lambdh => filetoh_lambdh(i_file, :)

    jjmax = 2*now_jmax-1
    jma1 = now_jmax-1
    do jj = 1, now_jmax
      del = now_lambdh(now_jmax+1-jj)-now_lambdh(1)
      llambdh(jj) = now_lambdh(now_jmax+1-jj)-2*del
    end do
    do jj = now_jmax+1, jjmax
      llambdh(jj) = now_lambdh(jj-jma1)
    end do
    do n = 1, modeles_ntot
      do jj = 1, now_jmax
        tth(jj, n) = now_th(now_jmax+1-jj, n)
      end do
      do jj = now_jmax+1, jjmax
        tth(jj, n) = now_th(jj-jma1, n)
      end do
    end do

    do j = 1,jjmax
      allh(j) = llambdh(j)-m_ilzero
    end do

    do n = 1,modeles_ntot
      do j = 1,jjmax
        tauhn(j) = tth(j,n)
      end do

      call ftlin3h()

      do d = 1,m_dtot
        hy_tauh(n, d) = hy_tauh(n, d)+ftth(d)
      end do
    end do
  contains

    !-------------------------------------------------------------------------------------
    !> Interpolaton specially for hydrogen lines calculation.
    !>
    !> Linear interpolation.
    !>
    !> @note When seen in pfant output (e.g. flux.norm), the hydrogen line is
    !> a sequence of joined convex lines. This is probably due to hy_tauh undergoing
    !> something like -log(hy_tauh). These small convex lines are more easily noticed
    !> if a hydrogen line is calculated alone, without other abundances.
    !>
    !> @note This routine is very similar to misc_math::ftlin3()*.
    !>

    subroutine ftlin3h()
      real*8 dy, ft, t, t0, t1, t2, u0
      integer j, k, kk, jj, kk1, kq

      j = 2
      kk = 1
      24 continue
      do 4 k = kk,m_dtot
        kq = k
        t = m_ttd(k)

        jj = j-1
        do 1  j = jj,jjmax
          if (t-allh(j)) 3,2,1
        1 continue
        go to 10
        2 ftth(k) = tauhn(j)
        if (j .eq. 1) j = j+1
        go to 4

        3 if (j .eq. 1) go to 10
        ! linear interpolation.
        u0 = tauhn(j)-tauhn(j-1)
        t0 = allh(j)-allh(j-1)
        t1 = t-allh(j-1)
        t2 = t1/t0
        dy = u0*t2
        ft = tauhn(j-1) + dy
        ftth(k) = ft
      4 continue

      14 continue

      do k = 1,m_dtot
        if(ftth(k).gt. 1e-38) go to 20
      end do

      20 m_dhmi = k

      ! ?doc?
      if (m_dhmi .eq. m_dtot) m_dhmi = 1


      kk1 = m_dhmi+1
      do k = kk1,m_dtot
        if (ftth(k) .le. 1e-38) go to 30
      end do

      30 m_dhpi = k

      ! (Paula Coelho 21/11/04) instrucao da Marie Noel
      ! ?doc?
      if (ftth(m_dtot) .ne. 0.0) m_dhpi = m_dtot

      return

      10 ftth(k) = 0.
      j = j+1

      kk = kq
      kk = kk+1
      if (kq .gt. m_dtot) go to 14
      go to 24
    end
  end

end module


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> PFANT main executable: spectral synthesis
!>
!> Creates three files such as (flux.spec, flux.cont, flux.norm).
!> These files can be inputted into nulbad
!> or visualized with plot_spectra.py
!>
!> Takes as input most of the data files.
!>
!> @verbatim
!> @note Flux absolu sortant a ete multiplie par 10**5
!> @endverbatim

program pfant
  use config
  use logging
  use synthesis
  use welcome
  use readers
  use pfant_x
  use misc

  implicit none

  !=====
  ! Startup
  !=====
  execonf_name = 'pfant'
  call molecules_idxs_init()
  call config_init()

  !=====
  ! File reading
  !=====
  call read_dissoc(config_fn_dissoc)
  call read_main(config_fn_main)

  !---
  ! (intermission)
  ! After reading dfile:dissoc and dfile:main,
  ! initializes variables whose values may come either from dfile:main or
  ! command-line option
  !---
  call pfant_init_x()

  ! continues file reading
  call read_partit(config_fn_partit)  ! LECTURE DES FCTS DE PARTITION
  call read_absoru2(config_fn_absoru2)  ! LECTURE DES DONNEES ABSORPTION CONTINUE
  call read_modele(config_fn_modeles)  ! LECTURE DU MODELE
  call read_abonds(config_fn_abonds)
  call read_atoms(config_fn_atoms)
  ! Gets list of hydrogen lines filenames either from dfile:main or dfile:hmap.
  ! The latter is not the preferred way.
  if (config_hmap) then
    call read_hmap(config_fn_hmap)
  else
    call hmap_copy_from_main()
  end if

  call read_filetoh(x_llzero, x_llfin)
  call read_molecules(config_fn_molecules)

  if (abs(modeles_asalog-main_afstar) > 0.01) then
    call log_warning('asalog from model ('//real82str(modeles_asalog, 2)//&
     ') does not match afstar in main configuration file ('//real82str(main_afstar, 2)//')')
  end if


  !=====
  ! Spectral synthesis
  !=====
  ! Does the calculus
  call synthesis_()

end program pfant






! Disabled until someone misses
! MT: Either get rid of it or include it as an optional output.
!    !> Writes into file log.log
!
!    subroutine write_log()
!      integer d
!      real*8 amg
!      amg = main_xxcor(8)
!
!      1130  format(i5, a20, 5f15.5, 4f10.1, i10, 4f15.5)
!      write(UNIT_LOG, 1130) &
!       ikeytot, &
!       modeles_tit, &
!       tetaef, &
!       main_glog, &
!       main_asalog, &
!       modeles_nhe, &
!       amg, &
!       l0, &
!       lf, &
!       m_lzero, &
!       m_lfin, &
!       itot, &
!       main_pas, &
!       main_echx, &
!       main_echy, &
!       main_fwhm
!
!      do d = i1,i2
!        write(UNIT_LOG, *) l0+(d-1)*main_pas, selekfh_fl(d)
!      end do
!    end
!
!    !> Writes into outfile:lines and fort.91
!
!    subroutine write_lines_fort91()
!      real*8 log_abond
!      122 FORMAT(6X,'# LAMBDA',4X,'KIEX',5X,'L GF',3X,'L ABOND',6X,'CH',10X,'GR',10X,'GE',5X,'ZINF',4X,'CORCH')
!      write(UNIT_LINES, 122)
!      do k=1,atoms_f_nblend
!        log_abond = log10(atoms_f_abonds_abo(k))
!
!        125 format(a2,1x,i1,1x,f08.3,1x,f6.3,f09.3,f09.3,1x,3e12.3,f5.1, f7.1)
!        write(UNIT_LINES, 125)     &
!         atoms_f_elem(k),        &
!         atoms_f_ioni(k),        &
!         atoms_f_lambda(k),      &
!         atoms_f_kiex(k),        &
!         atoms_f_algf(k),        &
!         log_abond-main_afstar+12, &
!         atoms_f_ch(k),          &
!         atoms_f_gr(k),          &
!         atoms_f_ge(k),          &
!         atoms_f_zinf(k),        &
!         popadelh_corch(k)
!
! MT: fort.91 is not necessary to me.
!       121 FORMAT(1X,A2,I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1,F7.1)
!        write(91,121)              &
!         atoms_f_elem(k),        &
!         atoms_f_ioni(k),        &
!         atoms_f_lambda(k),      &
!         atoms_f_kiex(k),        &
!         atoms_f_algf(k),        &
!         log_abond-main_afstar+12, &
!         atoms_f_ch(k),          &
!         atoms_f_gr(k),          &
!         atoms_f_ge(k),          &
!         atoms_f_zinf(k),        &
!         popadelh_corch(k)
!      end do
!    end


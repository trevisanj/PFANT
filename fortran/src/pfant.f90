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
! Equilibre dissociatif
!
! This module contains subroutine sat4(), which calculates the sat4_* variables
! used by other modules.
!
! Prefixes:
!   - sat4_ -- variables calculated by sat4() (or indirectly, die())

module dissoc
  use pfantlib
  implicit none

  ! They will be pointer targets at molecules::point_ppa_pb()
  real*8, public, target, dimension(MAX_MODELES_NTOT) ::  &
   sat4_pph,  & ! pressure: hydrogen
   sat4_ppc2, & ! pressure: 12carbon
   sat4_pn,   & ! pressure: nytrogen
   sat4_pc13, & ! pressure: 13carbon
   sat4_pmg,  & ! pressure: magnesium
   sat4_po,   & ! pressure: oxygen
   sat4_pti,  & ! pressure: titanium
   sat4_pfe     ! pressure: iron

  private :: die ! private subroutine

  integer, private, parameter :: &
   Z_ELECTRON = 99,  &  ! Fictional atomic number of electron.
                        ! *Note*: this is OK, Z=99 will never be used as a physical element
   Z_H_STAR   = 100, &  ! Fictional atomic number of "H*"
                        ! *Note*: this is OK, Z=99 will never be used as a physical element
   Z_H        = 1,   &  ! Atomic number of Hydrogen
   Z_HE       = 2       ! Atomic number of Helium

  ! Note that indexes of these arrays are atomic numbers
  ! For example: m_ip(z) contains information related to atomic number z
  real*8, private, dimension(MAX_DISSOC_Z) :: &
   m_ip,     & ! values come from dissoc_ip, but index is the atomic number
   m_ccomp,  & ! ?doc?
   m_uiidui, & ! ?doc?
   m_fp,     & ! ?doc?
   m_kp,     & ! ?doc?
   m_p         ! ?doc? Pressure

  real*8, private, dimension(MAX_DISSOC_NMOL) :: &
   m_ppmol, & ! ?doc?
   m_apmlog   ! ?doc?

  real*8, private :: m_pe ! Electron pressure

  real*8, private, parameter :: ECONST = 4.342945e-1 ! ?doc?
contains

  !=======================================================================================
  ! Subroutine d'equilibre dissociatif

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
      cclogi = cclogi
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

    m_p(Z_ELECTRON) = 1.0e-10

    !
    !*****INPUT E

    do 1020 ito = 1,modele%ntot
      theta = modele%teta(ito)
      tem = 5040.0/theta
      pg = modele%pg(ito)
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

        ! *Note* 1e-30 added to terms to avoid log(0)
        !        (MT): m_p,m_kp=0 should be avoided.
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

    !_logging__
    !do i=1,4
    !  write(*,'(7e11.4)') (xp(itx,i),itx=1,modele%ntot)
    !end do

    iz = find_atomic_symbol_dissoc('H ')
    sat4_pph = xp(:, iz)
    iz = find_atomic_symbol_dissoc('C ')
    sat4_ppc2 = xp(:, iz)
    iz = find_atomic_symbol_dissoc('N ')
    sat4_pn = xp(:, iz)
    iz = find_atomic_symbol_dissoc('O ')
    sat4_po = xp(:, iz)
    iz = find_atomic_symbol_dissoc('A ')  ! Yeah, "A" is 13C
    sat4_pc13 = xp(:, iz)
    iz = find_atomic_symbol_dissoc('TI')
    sat4_pti = xp(:, iz)
    iz = find_atomic_symbol_dissoc('MG')
    sat4_pmg = xp(:, iz)
    iz = find_atomic_symbol_dissoc('FE')
    sat4_pfe = xp(:, iz)


!original      do itx = 1,modele%ntot
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
  ! DIE9 ?what?

  subroutine die(tem, pg)
    real*8, intent(in) :: tem, & ! ?doc?
     pg ! ?doc?
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

    ! ISSUE Z=100 within dissoc.dat is only possible at the metals part (at the molecules part the Z slots have only 2 digits).
    ! THe current dissoc.dat has no Z=100 (neither 99).
    ! Is this a remaining fragment of code? My hint comes from the fact that Z_ELECTRON=99 is addressed several times, but Z_H_STAR=100 is not.
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
      ! ISSUE: what if some NELEMI=Z_ELECTRON=99? THen m_p(99) will no longer be equal to m_pe
      nelemi=dissoc_nelemx(i)
      m_p(nelemi) = m_fp(nelemi)*exp(-5.0*t/ECONST)
    1403 continue
    m_p(Z_H) = ph   ! ISSUE: overwriting m_p(1)

    ! Update: kept as-was
    ! *Note* m_p was being divided by 100 over and over at each j (molecule).
    ! This division has been taken out of loop, but is still an issue, since it is unclear *why* this division is being done.
    ! ISSUE ask blb being divided by 100 is still an issue
    ! do m =1,MAX_DISSOC_Z
    !   m_p(m)=1.0e-2*m_p(m)
    ! end do


    ! RUSSELL EQUATIONS
    1402 continue
    niter = 0
    1040 continue
    do 1030 i =1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      fx(nelemi) = -m_fp(nelemi)+m_p(nelemi)*(1.0 + m_kp(nelemi)/m_pe)  ! ISSUE if NELEMI=99, m_p(99) and m_pe are potentially not the same thing! Is this alright?
      dfx(nelemi) = 1.0 + m_kp(nelemi)/m_pe
    1030 continue

    spnion = 0.0
    do 1041 j=1,dissoc_nmol
      mmaxj  = dissoc_mmax(j)
      pmoljl = -m_apmlog(j)
      do 1042 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
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

        if (nelemj .eq. Z_ELECTRON) then  ! ISSUE This bit suggests that Z=99 is allowed in the molecules part
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
! Contains subroutines filter_molecules() and filter_atoms()
!
! Prefixes:
!   - km_f_ -- calculated by filter_molecules()
!   - atoms_f_ -- calculated by filter_atoms()

module filters
  use pfantlib
  implicit none


  ! Analogue to atoms_zinf
  ! All molecular lines will be calculated until this value (angstrom) to the
  ! left and to the right of the line centre.
  real*8, parameter :: KM_ALARGM = 0.1

  !=====
  ! km_f_*Variables filled by filter_molecules()
  !=====

  integer km_f_mblend  ! Total number of spectral lines *filtered in*

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_f_lmbdam, & ! ?doc?
    km_f_sj,     & ! ?doc?
    km_f_jj,     & ! ?doc?
    km_f_mm        ! Replicates km_mm(molidx) for all selected lines of molecule molidx.
                   ! Redundant information but simplifies use. Used in synthesis::selekfh()

  !------
  ! These two arrays contain indexes pointing at km_f_lmbdam, km_f_sj, km_f_jj, km_f_mm
  !------

  ! Contains the index of the last line of each set of lines within km_f_lmbdam, km_f_sj and km_f_jj
  ! **for the current molecule** I_MOL
  !
  ! Augmented: first row is 0 (ZERO) or repeats last element of previous column
  !
  ! km_f_ln(i+1, j) represents to i-th transition of j-th molecule, 1=1,molidxs%n_on
  integer :: km_f_ln(MAX_KM_NV_PER_MOL+1, NUM_MOL)

  !=====
  ! atoms_f_*Variables filled by filter_atoms()
  !=====

  ! *atoms file*, filtered variables
  ! Very similar to above; differences are
  ! - single underscore
  ! - additional variable "gf", which equals 10**algf
  integer atoms_f_nblend ! ?doc?
  character*2 atoms_f_elem(MAX_ATOMS_F_NBLEND) ! atomic symbol (right-alignes, uppercase)
  integer, dimension(MAX_ATOMS_F_NBLEND) :: &
   atoms_f_ioni ! ?doc?
  real*8, dimension(MAX_ATOMS_NBLEND) :: &
   atoms_f_lambda,       & ! ?doc?
   atoms_f_kiex,         & ! ?doc?
   atoms_f_algf,         & ! ?doc?
   atoms_f_ch,           & ! ?doc?
   atoms_f_gr,           & ! ?doc?
   atoms_f_ge,           & ! ?doc?
   atoms_f_zinf,         & ! ?doc?
   atoms_f_abondr_dummy, & ! ?doc?
   atoms_f_gf,           & ! ?doc?
   atoms_f_abonds_abo      ! ?doc?

contains

  !=======================================================================================
  ! Sweeps km_* to populate a few km_f_* depending on the interval lzero-lfin

  subroutine filter_molecules(lzero, lfin)
    ! Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    ! Upper edge of wavelength interval
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

        if ((lambda .ge. lzero-KM_ALARGM) .and. (lambda .le. lfin+KM_ALARGM)) then
          ! Filters in a new spectral line!
          i_filtered = i_filtered+1

          ! spill check
          if (i_filtered .gt. MAX_KM_F_MBLEND) then
            call log_and_halt('filter_molecules(): number of filtered lines '//&
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

          !write(lll, *) 'number of SELECTED lambdas for transition ', j_set, ': ', num_lambdas
          !call log_debug(lll)


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
  ! Selects only spectral lines within range lzero, lfin + performs "inner join".
  !
  ! Populates variables atoms_f_*

  subroutine filter_atoms(lzero, lfin)
    ! Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    ! Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    integer j, k

    k = 0
    do j = 1, atoms_nblend
      if((atoms_lambda(j) .le. lfin+atoms_zinf(j)) .and. (atoms_lambda(j) .ge. lzero-atoms_zinf(j))) then
        k = k+1

        ! spill check: checks if exceeds maximum number of elements allowed
        if (k .gt. MAX_ATOMS_F_NBLEND) then
          call log_and_halt('filter_atoms(): exceeded maximum of MAX_ATOMS_F_NBLEND='//&
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
! Contains subroutine kapmol_()
!
! Calculated variables have prefix "km_c_"

module kapmol
  use pfantlib
  use dissoc
  use filters
  implicit none

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_c_gfm ! ?doc? in sync with km_f_sj etc

  real*8, dimension(MAX_KM_F_MBLEND, MAX_MODELES_NTOT) :: km_c_pnvj ! ?doc? in sync with km_f_sj etc

  real*8, private, pointer, dimension(:) :: ppa, pb
  private point_ppa_pb

contains

  !=======================================================================================
  ! Calculates the molecular absorption coefficient.

  subroutine kapmol_()
    real*8 t5040, psi
    real*8 csc
    real*8 fe, do_, mm, am, bm, ua, ub, te, cro, rm
    real*8 qv, gv, bv, dv, facto
    integer i_mol, j_set, l, l_ini, l_fin, n, nnv, molidx

    real*8, parameter :: C2 = 8.8525E-13

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
      do n = 1,modele%ntot
        t5040 = modele%teta(n)/5040
        psi = do_*modele%teta(n)+2.5*log10(modele%teta(n))-1.5*log10(rm)-&
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
            csc = exp(-H*C/KB*t5040*(te+gv+bv*(km_f_jj(l)+1)*km_f_jj(l)))*   &
                  (2.-cro)*(2.*km_f_jj(l)+1.)*                                             &
                  exp(H*C/KB*t5040*(dv*(km_f_jj(l)*(km_f_jj(l)+1))**2+2.*bv))

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


    call log_debug(LEAVING//' kapmol()')
  end


  !=======================================================================================
  ! Assigns address of variable PPA and PB depending on the molecule formula ID
  !
  ! This was originally a vector copy element-by-element in old routine KAPMOL. However, as
  ! PPA and PB contents are not changed after the assignment, it is reasonable to just point
  ! to the source vectors (way faster).
  !
  ! @sa reader_molecules::find_formula_id, reader_molecules::read_molecules

  subroutine point_ppa_pb(formula_id)
    integer, intent(in) :: formula_id

    if (formula_id .gt. NUM_FORMULAE) then
      write (lll, *) 'point_ppa_pb(): invalid formula id (', formula_id, ') must be maximum ', NUM_FORMULAE
      call log_and_halt(lll)
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
        call log_and_halt('Formula id not handled by point_ppa_pb(): '//int2str(formula_id), is_assertion=.true.)
    end select
  end
end


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Declaration and initialization of x_* variables
!
! This module deals with the variables whose values may come either from *main file* or
! command-line arguments (the latter has precedence).
!
! Prefixes:
!   - x_ -- these variable values may come either from *main file* or command-line options.

module pfant_x
  use pfantlib
  implicit none

  character*128 :: x_flprefix
  real*8 :: x_llzero, x_llfin, x_pas, x_aint
contains

  ! Initializes x_* variables
  !
  ! Note: to be called after read_main()

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
    if (config_pas .eq. -1) then
      x_pas = main_pas
      call parse_aux_log_assignment('x_pas', real82str(x_pas, 2))
    else
      x_pas = config_pas
    end if
    if (config_aint .eq. -1) then
      x_aint = main_aint
      call parse_aux_log_assignment('x_aint', real82str(x_aint, 0))
    else
      x_aint = config_aint
    end if
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

!
! Fantomol avec sous-programmes (MNP) -
! Calcul possible de 100 angstrom en 100 angstrom.
!
! **Notes**
! - Flux absolu sortant a ete multiplie par 10**5
! - Flux sortant est en nu: lambda (x-axis) vs. F(nu) (y-axis)
! - Existing files are replaced
!
! (MT) Unit of flux: erg*s^-1*cm^-2/(Hz*ster), however (see next note)
! Actually what is called "flux" would be more accurately called "Specific intensity"
! [Gray Stellar Photospheres 3rd Ed. Eq 5.1]
!

! **Variable prefixes**
!        m_* module internal variables shared among routines
!       hy_* hydrogen line-related, calculated by calc_tauh()
! popadelh_* calculated by popadelh()
!  selekfh_* calculated by selekfh()
!       bk_* calculated by bk()
!    popul_* calculated by popul()
!

module synthesis
  use pfantlib
  use dissoc
  use filters
  use kapmol
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

  ! Calculated by subroutine popul
  real*8, dimension(3,MAX_PARTIT_NPAR, MAX_MODELES_NTOT) :: popul_p

  ! Calculated by subroutine popadelh
  real*8, dimension(MAX_ATOMS_F_NBLEND) :: popadelh_corch, popadelh_cvdw, &
   popadelh_zinf  ! limit distance from center of line to calculate the Voigt profile
                  ! This information was once in *atoms file* and being set manually;
                  ! now it is calculated automatically.

  real*8, dimension(MAX_ATOMS_F_NBLEND,MAX_MODELES_NTOT) :: &
   popadelh_pop, popadelh_a, popadelh_delta



  ! Calculated by subroutine popadelh
  !real*8, dimension(MAX_ATOMS_F_NBLEND,MAX_MODELES_NTOT) :: &
  ! popadelh_pop, popadelh_a, popadelh_delta, &
  ! popadelh_zinf  ! limit distance from center of line to calculate the Voigt profile
  !                ! This information was once in *atoms file* and being set manually;
  !                ! now it is calculated automatically.




  ! Calculated by selekfh()
  real*8, dimension(MAX_DTOT) :: selekfh_fl, selekfh_fcont

  ! Calculated by bk()
  real*8, dimension(0:MAX_MODELES_NTOT) :: bk_b1, bk_b2
  real*8, dimension(MAX_MODELES_NTOT) :: bk_phn, bk_ph2
  real*8, dimension(MAX_DTOT, MAX_MODELES_NTOT) :: bk_kcd

  ! Opacity-related, calculated by calc_opa()
  real*8, dimension(MAX_DTOT) :: m_lambda ! lambda for each point
  real*8, dimension(MAX_DTOT, MAX_MODELES_NTOT) :: &
   opa_sca, &  ! opacities calculated by interpolation of the available MARCs model (scattering)
   opa_abs     ! opacities calculated by interpolation of the available MARCs model (absorption)
  real*8 :: opa_mass_factor ! factor to convert cm^2/g --> cm^2/(hydrogen particle)

  ! Calculated by calc_tauh(), hydrogen lines-related
  real*8, public :: hy_tauh(MAX_MODELES_NTOT, MAX_DTOT)
  integer, public :: &
   hy_dhp, & ! maximum value of dhpy
   hy_dhm    ! minimum value of dhmy

  ! Calculated by calc_tauhi(), auxiliary of calc_tauh()
  integer :: &
   m_dhmi, & ! Calculated by subroutine calc_tauhi
   m_dhpi    ! Calculated by subroutine calc_tauhi

  !=====
  ! Constants available to all subroutines within this module
  !=====

  real*8, parameter :: &
   C1 = 4.8298E+15,    & ! ?doc?
   C2 = 8.8525E-13,    & ! ?doc?
   C4 = 2.1179E+8,     & ! ?doc?
   C6 = 3.76727E+11,   & ! ?doc?
   DEUXR = 1.6634E+8     ! ?doc?

  real*8, parameter :: &
   C5 = 2.*PI* (3.*PI**2/2.44)**0.4   ! ?doc?


  !=====
  ! Module variables initialized in subroutine synthesis_() and shared among other routines.
  !=====
  real*8 :: &
   m_lzero, &                 ! initial lambda of current ikey-th iteration
   m_lfin, &                  ! final lambda of current ikey-th iteration
   m_lambd, &                 ! lambda 1/2
   m_ttd(MAX_DTOT), &
   m_ecart(MAX_ATOMS_F_NBLEND), & ! MT: some sort of delta lambda
   m_gfal(MAX_ATOMS_F_NBLEND), &
   m_ecartm(MAX_KM_F_MBLEND)
  integer :: &
    m_dtot,    &  ! number of different wavelenghts for which flux will be calculated at each ikey-iteration
    m_ilzero      ! closest integer multiple of 100 that is <= m_lzero

contains

  !======================================================================================================================
  ! Spectral synthesis subroutine

  subroutine synthesis_()
    ! Units for output files
    integer unit_spec, unit_cont, unit_norm, unit_lines, unit_log
    real*8 fn(MAX_DTOT)
    integer i, i1, i2, k, d, &
     ikey,    & ! ikey-th aint-large calculation interval
     ikeytot    ! total number of aint-large calculation intervals
    real*8 l0, lf, alzero, xlfin, xlzero, incr
    ! auxiliary variables to time the different parts
    real :: start, finish, start0, finish0
    ! output filenames with relative path included
    character(len=:), allocatable :: fn_spec, fn_cont, fn_norm

    call cpu_time(start0)

    !=====
    ! Setup
    !=====

    ! ASK BLB why overwriting absoru2_abhel?
    absoru2_abhel = modele%nhe
    ! ASK BLB why using modele%asalog instead of main_afstar? why multiplying this?
    absoru2_abmet = absoru2_abmet*10.**modele%asalog

    !-----
    ! Output files opened here and left open until the end
    !-----
    ! Note that existing files are replaced
    fn_spec = trim(x_flprefix)//'.spec'
    fn_cont = trim(x_flprefix)//'.cont'
    fn_norm = trim(x_flprefix)//'.norm'
    open(newunit=unit_spec, file=fn_spec, status='replace')  ! spectrum
    open(newunit=unit_cont, file=fn_cont, status='replace')  ! continuum
    open(newunit=unit_norm, file=fn_norm, status='replace')  ! normalized
    !--- open(newunit=unit_lines,file=config_fn_lines, status='replace')               ! outfile:lines
    !--- open(newunit=unit_log,  file=config_fn_log, status='replace')                 ! log.log


    !=====
    ! Calculation begins!
    !=====

    call turbul_()  ! no need to time, very quick

    call cpu_time(start)
    call popul()
    call cpu_time(finish)
    call log_debug("POPUL() Time = "//real42str(finish-start, 3)//" seconds.")

    call cpu_time(start)
    call sat4()
    call cpu_time(finish)
    call log_debug("SAT4() Time = "//real42str(finish-start, 3)//" seconds.")


    ! initial calculation sub-interval
    xlzero = x_llzero-LAMBDA_STRETCH
    xlfin = xlzero+x_aint+LAMBDA_STRETCH

    ! discovers the number of iterations
    if(xlfin .ge. (x_llfin+LAMBDA_STRETCH)) then
      ikeytot = 1
    else
      do i = 2,25000
        xlfin = xlfin+x_aint
        if(xlfin .ge. (x_llfin+LAMBDA_STRETCH)) exit
      end do
      ikeytot = i
    end if

    !m_lzero = x_llzero-LAMBDA_STRETCH
    !m_lfin = m_lzero+x_aint+LAMBDA_STRETCH
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
      if (ikey .eq. 1) then
        m_lzero = l0
      else
        m_lzero = x_llzero+x_aint*(ikey-1)
      end if

      ! Note0: The last value of flux in iteration ikey equals
      !  the first value of flux in iteration ikey+1.
      !  Hence the "+x_pas"  in the expression
      if (ikey .eq. ikeytot) then
        m_lfin = lf+x_pas
      else
        m_lfin = x_llzero+x_aint*ikey-x_pas+x_pas
      end if

      ! Note: (m_lfin-m_lzero) is constant except in the last iteration where m_lfin may be corrected
      m_dtot = int((m_lfin-m_lzero)/x_pas + 1.0005)

      ! spill check
      if(m_dtot .gt. MAX_DTOT) then
        call log_and_halt('dtot = '//int2str(m_dtot)//' exceeds maximum of MAX_DTOT='//&
         int2str(MAX_DTOT))
      end if

      m_lambd = (m_lzero+m_lfin)/2
      m_ilzero = floor(m_lzero/100)*100
      alzero = m_lzero-m_ilzero
      do d = 1, m_dtot
        incr = x_pas*(d-1)
        m_ttd(d) = alzero+incr
        m_lambda(d) = m_lzero+incr ! lambda for each point
      end do

      call log_info('/\/\/\ Calculation step '//int2str(ikey)//'/'//int2str(ikeytot)//&
        ' /\/\/\')
      501 format(2x,2x,'m_lzero=',f10.3,2x,'m_lfin=',&
       f10.3,2x,'m_dtot=',i7,2x,'m_lambd 1/2=',f10.3, 2x, 'm_ilzero=',i6)
      write(lll,501) m_lzero, m_lfin, m_dtot, m_lambd, m_ilzero
      call log_info(lll)

      !=====
      ! Beginning of iteration
      !=====

      ! ????????????????
      call cpu_time(start)
      call bk()
      call cpu_time(finish)
      ! call log_info("BK() Time = "//real42str(finish-start, 3)//" seconds.")

      ! opacities
      if (config_opa) then
        call calc_opa()
      end if

      ! hydrogen lines
      if (.not. config_no_h) then
        call cpu_time(start)
        call calc_tauh()
        call cpu_time(finish)
        call log_debug("CALC_TAUH() Time = "//real42str(finish-start, 3)//" seconds.")
      end if

      if (.not. config_no_atoms) then
        ! -- V --
        ! Quantites dependant de la raie et du modele
        !call cpu_time(start)
        call filter_atoms(m_lzero, m_lfin)
        !call cpu_time(finish)
        !call log_debug("FILTER_ATOMS() Time = "//real42str(finish-start, 3)//" seconds.")

        if (atoms_f_nblend .gt. 0) then
          !call cpu_time(start)
          call popadelh()
          !call cpu_time(finish)
          !call log_debug("POPADELH() Time = "//real42str(finish-start, 3)//" seconds.")


          ! -- VI --
          ! Calcul du coefficient d absorption selectif et calcul du spectre
          do k = 1, atoms_f_nblend
            m_gfal(k) = atoms_f_gf(k)*C2*(atoms_f_lambda(k)*1.e-8)**2
            m_ecart(k) = atoms_f_lambda(k)-m_lzero+x_pas
          end do
        end if
      end if

      if (.not. config_no_molecules) then
        !call cpu_time(start)
        call filter_molecules(m_lzero, m_lfin)
        !call cpu_time(finish)
        !call log_debug("FILTER_MOLECULES() Time = "//real42str(finish-start, 3)//" seconds.")

        !call cpu_time(start)
        call kapmol_()
        !call cpu_time(finish)
        !call log_debug("KAPMOL_() Time = "//real42str(finish-start, 3)//" seconds.")
      end if

      call cpu_time(start)
      call selekfh()
      call cpu_time(finish)
      call log_info("SELEKFH() Time = "//real42str(finish-start, 3)//" seconds.")

      !=====
      ! Saving...
      !=====

      call cpu_time(start)
      ! i1 and i2 are the initial and final indexes of selekfh_fl, selekfh_fcont, and fd
      ! that will be written to output file.
      i1 = 1
      if (ikey .eq. ikeytot) then
        i2 = m_dtot - 1
      else
        ! Note0: The last value of flux in iteration ikey equals
        !  the first value of flux in iteration ikey+1.
        !  nulbad is aware of this and discards these redundant points.
        !  pyfant is also aware of this.
        i2 = m_dtot
      end if
      do d = 1, m_dtot
        selekfh_fl(d) = selekfh_fl(d)*(10.**5)
        selekfh_fcont(d) = selekfh_fcont(d)*(10.**5)
        fn(d) = selekfh_fl(d) / selekfh_fcont(d)  ! normalized spectrum
      end do

      ! Writes results for current iteration into open files
      ! call write_lines_fort91()                        ! outfile:lines and fort.91
      ! call write_log()                                ! log.log
      call write_spec_item(unit_spec, selekfh_fl)     ! spectrum
      call write_spec_item(unit_cont, selekfh_fcont)  ! continuum
      call write_spec_item(unit_norm, fn)             ! normalized

      call cpu_time(finish)
      call log_debug("SAVING Time = "//real42str(finish-start, 3)//" seconds.")

      !=====
      ! Preparing for next iteration
      !=====

      call log_progress(ikey, ikeytot)

      ikey = ikey+1
      if (ikey .gt. ikeytot) exit  ! main loop smooth exit

      !m_lzero = m_lzero+x_aint
      !m_lfin = m_lfin+x_aint
      !if(m_lfin .gt. (x_llfin+LAMBDA_STRETCH)) m_lfin = x_llfin+LAMBDA_STRETCH
    end do  ! main loop

    close(unit_spec)
    close(unit_cont)
    close(unit_norm)
    !--- close(unit_log)
    !--- close(unit_lines)

    !#logging
    call log_info('Note: flux sortant est en nu: Fnu x lambda')
    call log_info('Note: flux absolu sortant a ete multiplie par 10**5')

    call log_info('File '//trim(fn_spec)//' successfully created.')
    call log_info('File '//trim(fn_cont)//' successfully created.')
    call log_info('File '//trim(fn_norm)//' successfully created.')

    call log_debug(LEAVING//'synthesis_()')

    call cpu_time(finish0)
    print '("SYNTHESIS_() Time = ",f10.3," seconds.")',finish0-start0

  contains

    ! These subroutines have total knowledge of the variable names and values that appear inside
    ! their parent subroutine synthesis_().
    ! http://www.personal.psu.edu/jhm/f90/statements/contains.html

    ! Used to write the "spectrum", "continuum", and "normalized".
    ! Their writing pattern is very similar. THe header is the same,
    ! only the "ITEM" changes from file to file.

    subroutine write_spec_item(unit_, item)
      ! unit number, either UNIT_SPEC, UNIT_CONT, UNIT_NORM
      integer, intent(in) :: unit_
      ! either selekfh_fl, selekfh_fcont, or fn
      real*8, intent(in) :: item(:)
      real*8 amg
      amg = 0

      1130 format(i5, a20, 5f15.5, 4f10.1, i10, 4f15.5)
      write(unit_, 1130)       &
       ikeytot,                &  ! fixed (same value for all iterations)
       modele%tit,            &  ! fixed
       5040/main_teff,         &  ! fixed
       main_glog,              &  ! fixed
       main_asalog,            &  ! fixed
       modele%nhe,            &  ! fixed
       amg,                    &  ! fixed
       l0,                     &  ! fixed
       lf,                     &  ! fixed
       m_lzero+x_pas*(i1-1),   &  ! changes (value changes with each iteration)
       m_lzero+x_pas*(i2-1),   &  ! changes
       i2-i1+1,                &  ! old nulbad, not used
       x_pas,                  &  ! fixed
       main_echx,              &  ! fixed
       main_echy,              &  ! fixed
       main_fwhm                  ! fixed
      write(unit_,'('//int2str(MAX_DTOT)//'f15.5)') (item(d), d=i1,i2)  !(item(d), d=i1,i2)
    end

  end subroutine synthesis_

  !======================================================================================================================
  ! Calcule la pop du niv fond de l'ion pour tous les partit_NPAR atomes de
  ! la table des fonctions de partition ,a tous les niv du modele
  !
  ! 40 elements, 50 niveaux de modele, 3 niv d'ionisation par elem.
  ! Partit donnee pour 33 temperatures au plus ds la table.

  subroutine popul()
    real*8 u(3), alistu(MAX_MODELES_NTOT*2), ue(MAX_MODELES_NTOT), tt(MAX_MODELES_NTOT*2), &
     aa, bb, uuu, x, y, t, tki2
    integer j, k, kmax, l, n

    do n = 1, modele%ntot
      t = 5040./modele%teta(n)
      ue(n) = C1*KB*t/modele%pe(n)*t**1.5
      do j = 1, partit_npar
        kmax = partit_jkmax(j)
        tt(1) = partit_tini(j)
        do  l=1,3
          do  k=1,kmax
            tt(k+1) = tt(k) + partit_pa(j)
            alistu(k) = partit_tabu(j,l,k)
          end do

          if (modele%teta(n) .lt. tt(kmax-1) ) then
            ! interpolation parabolique
            uuu = ft(modele%teta(n),kmax,tt,alistu)
          else
            ! interpolation lineaire entre 2 derniers pts
            aa = (alistu(kmax)-alistu(kmax-1)) / partit_pa(j)
            bb = alistu(kmax-1) - aa * tt(kmax-1)
            uuu = aa*modele%teta(n) + bb
          end if

          u(l) = exp(2.302585*uuu)
        end do

        x=u(1) / (u(2)*ue(n)) * 10.**(partit_ki1(j)*modele%teta(n))
        tki2= partit_ki2(j) * modele%teta(n)

        ! ASK BLB Why 77?
        if (tki2 .ge. 77.) then
          y = 0.
          popul_p(3,j,n) = 0.
        else
          y = u(3)*ue(n)/u(2) * 10.**(-partit_ki2(j)*modele%teta(n))
          popul_p(3,j,n) = (1./u(3))*(y/(1.+x+y))
        end if
        popul_p(2,j,n) = (1./u(2))*(1./(1.+x+y))
        popul_p(1,j,n) =  (1./u(1))*(x/(1.+x+y))
        end do
      end do
    return
  end subroutine


  ! Calculates opa_*: interpolated abs and sca, "mass factor"

  subroutine calc_opa()
    ! Atomic weights of all elements from hydrogen to uranium
    real*8, parameter :: weights(92) = (/1.0079, 4.0026, 6.941, 9.0122, 10.811, 12.0107, &
     14.0067, 15.9994, 18.9984, 20.1797, 22.9897, 24.305, 26.9815, 28.0855, 30.9738, &
     32.065, 35.453, 39.948, 39.0983, 40.078, 44.9559, 47.867, 50.9415, 51.9961, 54.938, &
     55.845, 58.9332, 58.6934, 63.546, 65.39, 69.723, 72.64, 74.9216, 78.96, 79.904, &
     83.8, 85.4678, 87.62, 88.9059, 91.224, 92.9064, 95.94, 98., 101.07, 102.9055, 106.42, &
     107.8682, 112.411, 114.818, 118.71, 121.76, 127.6, 126.9045, 131.293, 132.9055, &
     137.327, 138.9055, 140.116, 140.9077, 144.24, 145., 150.36, 151.964, 157.25, &
     158.9253, 162.5, 164.9303, 167.259, 168.9342, 173.04, 174.967, 178.49, 180.9479, &
     183.84, 186.207, 190.23, 192.217, 195.078, 196.9665, 200.59, 204.3833, 207.2, &
     208.9804, 209., 210., 222., 223., 226., 227., 232.0381, 231.0359, 238.0289/)

    integer n, i, j
    real*8 ab_h, temp

    ! Mass factor is calculated to convert cm^2/g to cm^2/(hydrogen nucleus).
    ! MARCS values are given in the former unit but we need the values in the latter unit,
    ! because absoru_() calculates kappa in unit cm^2/(hydrogen nucleus).
    !
    ! This is calculates according to Gray 1st ed. formula (8-17)
    !
    ! We use the abundances given as part of the MARCS model because they were the ones
    ! used by the MARCS code to calculate the model.
    opa_mass_factor = 0
    ab_h = 10**modele%abund(1)
    do i = 1, 92
      temp = weights(i)*(10**modele%abund(i)/ab_h)
      opa_mass_factor = opa_mass_factor+temp
      ! print *, 'mass of element ', i, ': ', weights(i), '; coiso: ', temp, '; abund: ', modele%abund(i)
    end do
    opa_mass_factor = opa_mass_factor*1.6606e-24

    ! print *, 'opa_mass_factor ', opa_mass_factor

    ! write(10,*) (m_lambda(i), i=1,modele%nwav)
    ! write(10,*) (modele%wav(i), i=1,modele%nwav)
    ! do n = 1, modele%ntot  ! modele%ntot
    ! ! todo cleanup
    !  write(11, *) (modele%sca(i,n),i=1,modele%nwav)
    !  write(12, *) (modele%abs(i,n),i=1,modele%nwav)
    ! end do
    !write(13,*) (m_lambda(i), i=1,m_dtot)

    do n = 1, modele%ntot  ! modele%ntot
    ! ft2 does not perform well here, ftlin3 is ok
      !call   ft2(modele%nwav, modele%wav, modele%sca(:, n), m_dtot, m_lambda, opa_sca(:, n))


      ! todo cleanup

      ! print *, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAaa"


      if (config_sca) &
       call ftlin3(modele%nwav, modele%wav, modele%sca(:, n), m_dtot, m_lambda, opa_sca(:, n))


      ! print *, "___A___", n, m_lambda(1), m_lambda(m_dtot)


      ! todo cleanup
      !  write(14, *) (opa_sca(i,n),i=1,m_dtot)



      !call   ft2(modele%nwav, modele%wav, modele%abs(:, n), m_dtot, m_lambda, opa_abs(:, n))
      ! print *, "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      if (config_abs) &
       call ftlin3(modele%nwav, modele%wav, modele%abs(:, n), m_dtot, m_lambda, opa_abs(:, n))
      ! print *, "___B___"


      ! todo cleanup
      !  write(15, *) (opa_abs(i,n),i=1,m_dtot)

    end do


    !stop 'clean up this'
  end


  !======================================================================================================================
  ! Calcule la population au niveau inferieur de la transition
  ! la largeur doppler popadelh_delta et le coefficient d'elargissement
  ! le "popadelh_a" utilise dans le calcul de H(popadelh_a,v)

  subroutine popadelh()
    implicit none
    character*1 isi, iss
    integer j, k, ioo, iopi, n
    real*8 kies,kii,nul, ahnul, alphl(MAX_MODELES_NTOT), gamma, gh, t, tap, top, vrel, &
     a, delta
    data isi/' '/, iss/' '/

    do k = 1, atoms_f_nblend
      ! Search: finds j-th atomic symbol in partit_el matching atoms_f_elem(k)
      ! This is a "inner join"
      do j = 1,partit_npar
        if(partit_el(j) .eq. atoms_f_elem(k)) go to 15
      end do

      104 format('Manque les fcts de partition du ', a2)
      write(lll,104) atoms_f_elem(k)
      call log_and_halt(lll)

      15 continue

      popadelh_corch(k) = 0.
      popadelh_cvdw(k) = 0
      ioo = atoms_f_ioni(k)

      ! If "ch" variable from *atoms file* is zero, overwrites it with a calculated value.
      ! See also read_atoms(), variable atoms_gr, which is also overwritten.
      if(atoms_f_ch(k) .lt. 1.e-37)  then
        kies = (12398.54/atoms_f_lambda(k)) + atoms_f_kiex(k)
        if (ioo .eq. 1) then
          kii = partit_ki1(j)
        else if (ioo .eq. 2) then
          kii = partit_ki2(j)
        else
          ! Better to give error than to silently do wrong calculations
          call log_and_halt('popadelh() found invalid ionization level: '//int2str(ioo), is_assertion=.true.)
        end if

        if(popadelh_corch(k) .lt. 1.e-37)   then
          popadelh_corch(k) = 0.67 * atoms_f_kiex(k) +1
        end if

        ! 125 format(3x ,' pour',f9.3,'   on calcule ch ', 'van der waals et on multiplie par ',f7.1)
        ! write(6,125)  atoms_f_lambda(k), popadelh_corch(k)
        popadelh_cvdw(k)= calch(kii, ioo, atoms_f_kiex(k), isi, kies, iss)

        atoms_f_ch(k) = popadelh_cvdw(k) * popadelh_corch(k)
        if (atoms_f_ch(k)  .lt. 0) then
          ! ch cannot be < 0, as it will be powered to 0.4 later
          ! If this happens, "gh" below will be NaN, and hjenor() will crash with a segmentation fault.
          write(*,*) 'popadelh_cvdw(k) = ', popadelh_cvdw(k)
          write(*,*) 'popadelh_corch(k) = ', popadelh_corch(k)
          write(*,*) 'atoms_f_kiex(k) = ', atoms_f_kiex(k)
          write(*,*) 'atoms_f_elem(k) = ', atoms_f_elem(k)
          write(*,*) 'atoms_f_lambda(k) = ', atoms_f_lambda(k)
          call log_halt('popadelh(): atoms_f_ch(k)  calculated is lower than ZERO')
          call log_and_halt('******'//real82str(atoms_f_ch(k))//'******')
        end if
      end if

!
      if(atoms_f_ch(k) .lt. 1.e-20) then
        ! This will be the case most of the time
        iopi = 1
      else
        iopi = 2
      end if

      do n = 1, modele%ntot
        t = 5040./modele%teta(n)
        nul = C* 1.e+8 /atoms_f_lambda(k)
        ahnul = H*nul
        alphl(n) = exp(-ahnul/(KB*t))

        tap = 1.-alphl(n)
        top = 10.**(-atoms_f_kiex(k)*modele%teta(n))

        if(atoms_f_elem(k) .eq. ' O') then
          ! #NOXIG: oxygen is treated differently
          popadelh_pop(k,n) = top*tap*popul_p(ioo,j,n)*sat4_po(n)/sat4_pph(n)
        else
          popadelh_pop(k,n) = popul_p(ioo,j,n)*top*tap
        end if

        delta = (1.e-8*atoms_f_lambda(k))/C*sqrt(turbul_vt(n)**2+DEUXR*t/partit_m(j))
        popadelh_delta(k,n) = delta

        vrel = sqrt(C4*t*(1.+1./partit_m(j)))
        if (iopi .eq. 1) then
          gh = C5*atoms_f_ch(k)**0.4*vrel**0.6
        else
          gh = atoms_f_ch(k) + popadelh_corch(k)*t
        end if
        gamma = atoms_f_gr(k)+(atoms_f_ge(k)*modele%pe(n)+gh*(bk_phn(n)+1.0146*bk_ph2(n)))/(KB*t)

        a = gamma*(1.e-8*atoms_f_lambda(k))**2 / (C6*popadelh_delta(k,n))

        ! if (isnan(a)) then
        !   call log_halt('popadelh(): A is not a number!')
        !   call log_halt('atomic "ch" = '//real82str(atoms_f_ch(k)))
        !   call log_halt('gh = '//real82str(gh))
        ! end if


        popadelh_a(k,n) = a


      end do
    end do
  end



  !======================================================================================================================
  ! Sets the Voigt profile using Hjertings' constants; calculates the flux and continuum
  !
  ! *Note* Most of the calculation time is spent inside this routine
  !
  ! *Note* Convolution for molecules uses Gaussian profile.
  !
  ! *Note* (MT)+(JT) Decision on variable MM: logic suggests that there should be one MM per molecule, so now using km_f_mm
  !                  instead of old scalar MM

  subroutine selekfh()
    integer d, k, l, n
    real*8 :: bi(0:MAX_MODELES_NTOT)
    real*8, dimension(MAX_ATOMS_F_NBLEND) :: &
     ecar  !, & !     ecart,l &
    real*8, dimension(MAX_MODELES_NTOT) :: &
     kap,    &
     kci
    real*8, dimension(MAX_KM_F_MBLEND) :: &
     ecarm !, ecartlm
    real*8 :: &
     deltam(max_km_f_mblend,MAX_MODELES_NTOT), &
     phi, t, v, vm, &
     kam, kappam, kappa, kak, &
     kappa_opa, &    ! continuum absorption coefficient, origin MARCS website
     kappa_absoru    ! continuum absorption coefficient


    kappa_opa = 0
    kappa_absoru = 0

    if (atoms_f_nblend .ne. 0) then
      do k = 1,atoms_f_nblend
        ecar(k) = m_ecart(k)
      end do
    end if

    if (km_f_mblend .ne. 0) then
      ! do l = 1, km_f_mblend
      !   m_ecartm(l) = km_f_lmbdam(l)-m_lzero + x_pas
      ! end do
      ! do k=1,km_f_mblend
      !  ecarm(k) = m_ecartm(k)
      ! end do
      do k=1,km_f_mblend
        ecarm(k) = km_f_lmbdam(k)-m_lzero + x_pas
      end do
    end if

    do d = 1, m_dtot
      ! Shifts the ecar and ecarm delta lambda vectors
      if (atoms_f_nblend .ne. 0) then
        do k = 1, atoms_f_nblend
          ecar(k) = ecar(k)-x_pas
        end do
      end if
      if (km_f_mblend .ne. 0) then
        do k = 1,km_f_mblend
          ecarm(k) = ecarm(k)-x_pas
        end do
      end if

      do n = 1,modele%ntot
        kappa = 0.
        kappam = 0.
        t = 5040./modele%teta(n)


        ! atomes
        if(config_no_atoms) go to 260
        do  k = 1, atoms_f_nblend
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
        if (config_no_molecules) go to 250
        do l = 1, km_f_mblend
          ! TODO exponential is easier to know where it finishes, no need to use KM_ALARGM. however, will it be faster?
          !      changing this may be easier to import from TurboSpectrum molecular data
          if(abs(ecarm(l)) .gt. KM_ALARGM)  then
            kam = 0.
          else
            deltam(l,n) = (1.e-8*km_f_lmbdam(l))/C*sqrt(turbul_vt(n)**2+DEUXR*t/km_f_mm(l))
            vm = abs(ecarm(l)*1.e-08/deltam(l,n))
            phi = (exp(-vm**2))/(RPI*deltam(l,n))
            kam = phi*km_c_gfm(l)*km_c_pnvj(l,n)
          end if
          kappam = kappam + kam
        end do   !  fin bcle sur l

        250 continue

        ! opacities

        if (config_abs .or. config_sca) then
            ! todo issue
            ! Scattering and absorption are given in MARCS ".opa" files in cm^2/g
            !
            ! Apparently they need to be converted back to cm^2 per hydrogen nucleous
            !
            ! It is doing so by multiplying by 2e-24, but this should be
            ! a weighted sum from 1 to 92 of (relative abundance)*(atomic weight)*1.66e-24
            !
            ! According to Gray this gives a typical value of around 2e-24
            !
            ! Another thing to investigate is whether MARCS opacities do not already represent everything for the
            ! kappa of the continuum

            kappa_opa = 0

            if (config_abs) kappa_opa = kappa_opa+opa_abs(d, n)
            if (config_sca) kappa_opa = kappa_opa+opa_sca(d, n)


            kappa_opa = kappa_opa*opa_mass_factor
        end if

        if (config_absoru) kappa_absoru = bk_kcd(d, n)

        kci(n) = kappa_absoru+kappa_opa  ! kappa for the continuous
        kap(n) = kappa+kappam+kci(n)     ! continuous + lines
      end do

      do n = 0,modele%ntot
        bi(n) = ((bk_b2(n)-bk_b1(n))*(float(d-1)))/(float(m_dtot-1)) + bk_b1(n)
      end do

      if (config_no_h .or. (d .lt. hy_dhm) .or. (d .ge. hy_dhp)) then
        ! without hydrogen lines
        selekfh_fl(d) = flin1(kap, bi, modele%nh, modele%ntot, main_ptdisk, main_mu, config_kik)
      else
        ! with hydrogen lines
        selekfh_fl(d) = flinh(kap, bi, modele%nh, modele%ntot, main_ptdisk, main_mu, config_kik, hy_tauh(:, d))
      end if

      ! Dez 03-P. Coelho - calculate the continuum and normalized spectra
      selekfh_fcont(d) = flin1(kci, bi, modele%nh, modele%ntot, main_ptdisk, main_mu, config_kik)
    end do
  end


  !======================================================================================================================
  ! Calculates the flux in the continuum.
  !
  ! Issue ASK BLB Only two values are calculated for each "ikey" iteration and the intermediate values are filled by
  !               linear interpolation. The continuum is dependent on aint.

  subroutine bk()
    real*8 nu, nu1, nu2, &
     alph1, alph2 ! old ALPH, which was a vector, but I realized it is used only inside loop, no need for vector
    real*8, dimension(2, MAX_MODELES_NTOT) :: kcj
    real*8, dimension(2) :: kcn, lambdc
    real*8, dimension(MAX_MODELES_NTOT) :: kc1, kc2
    real*8 :: fttc(MAX_DTOT)
    real*8 c31, c32, t, tet0, ahnu, ahnu1, ahnu2, alph01, alph02
    integer d, j, n

    call log_debug(ENTERING//'bk()')

    nu1 = C* 1.e+8 /m_lzero
    ahnu1 = H*nu1
    c31 = (2*ahnu1) * (nu1/C)**2
    nu2 = C* 1.e+8 /m_lfin
    ahnu2 = H*nu2
    c32 = (2*ahnu2) * (nu2/C)**2
    nu = C* 1.e+8 /m_lambd
    ahnu = H*nu
    do n = 1,modele%ntot
      t = 5040./modele%teta(n)
      alph1 = exp(-ahnu1/(KB*t))
      bk_b1(n) = c31 * (alph1/(1.-alph1))
      alph2 = exp(-ahnu2/(KB*t))
      bk_b2(n) = c32 * (alph2/(1.-alph2))

      call absoru_(m_lzero, modele%teta(n),log10(modele%pe(n)),1,1,1,1,2, .false.)
      kc1(n) = absoru_totkap(1)
      call absoru_(m_lfin, modele%teta(n),log10(modele%pe(n)),1,1,1,1,2, .false.)
      kc2(n) = absoru_totkap(1)
      call absoru_(m_lambd, modele%teta(n),log10(modele%pe(n)),1,1,1,1,2, .false.)
      bk_phn(n) = absoru_znh(absoru2_nmeta+4) *KB * t
      bk_ph2(n) = absoru_znh(absoru2_nmeta+2) *KB * t
    end do

    tet0 = fteta0(modele%pg, modele%teta, modele%ntot)     !on extrapole modele%teta pour modele%nh=0
    t = 5040./tet0
    alph01 = exp(-ahnu1/(KB*t))
    bk_b1(0) = c31 * (alph01/(1.-alph01))
    alph02 = exp(-ahnu2/(KB*t))
    bk_b2(0) = c32 * (alph02/(1.-alph02))

    ! lambdc(1) and lambdc(2) forced to be equal to m_ttd(1) and m_ttd(m_tdod)
    ! because I was experiencing numerical errors here
    ! where m_ttd(1) was lower than lambdc(1) by ~1e-14 causing ftlin3() to crash and
    ! flin_() was raising "modele trop court" because of lambdc(2) apparently.
    ! Sorry but I don't know why exactly. But works this way
    lambdc(1) = m_ttd(1) ! m_lzero-m_ilzero
    lambdc(2) = m_ttd(m_dtot)  !  m_lfin-m_ilzero
    do n=1,modele%ntot
      kcj(1,n) = kc1(n)
      kcj(2,n) = kc2(n)
    end do
    do n = 1, modele%ntot
      do j = 1, 2
        kcn(j) = kcj(j, n)
      end do
      call ftlin3(2, lambdc, kcj(:, n), m_dtot, m_ttd, bk_kcd(:, n))
    end do

    153 format(' bk_kcd(1,1)=',e14.7,2x,'bk_kcd(1,ntot)=',e14.7)
    154 format(' bk_kcd(dtot,1)=',e14.7,2x,'bk_kcd(dtot,ntot)=',e14.7)
    write(lll,153) bk_kcd(1,1),bk_kcd(1,modele%ntot)
    call log_debug(lll)
    write(lll,154) bk_kcd(m_dtot,1),bk_kcd(m_dtot,modele%ntot)
    call log_debug(lll)

    call log_debug(LEAVING//'bk()')
  end


  !=======================================================================================
  ! Hydrogen lines-related calculation: calculates hy_tauh, hy_dhp, hy_dhm

  subroutine calc_tauh()
    integer :: im, dhmy(MAX_FILETOH_NUM_FILES), dhpy(MAX_FILETOH_NUM_FILES), ih, iht, imy
    real*8 :: allhy

    hy_tauh = 0.  ! initializes all elements to zero

    im = 0
    do ih = 1,filetoh_num_files
      allhy = filetoh_llhy(ih)-m_lzero

      ! TODO top priority H_LINE_WIDTH do be taken from file!!!
      if (((allhy .gt. 0) .and. (allhy .le. (x_aint+H_LINE_WIDTH+LAMBDA_STRETCH))) .or. &
          ((allhy .lt. 0.) .and. (allhy .ge. (-H_LINE_WIDTH)))) then
        im = im+1
        iht = ih

        !#logging
        712 format(1x,'im=',i3,2x,'lambda h=',f8.3,2x,'filename=',a,2x,'ih=',i5)
        write(lll,712) im, filetoh_llhy(ih), ''''//trim(filetoh_filenames(iht))//'''', iht
        call log_debug(lll)

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

      !!!do n = 1,modele%ntot
      !!!  do d = 1,dtot
      !!!    tauh(n, d) = 0.0
      !!!    do im = 1,imy
      !!!      tauh(n, d) = tauh(d,n)+tauhy(im,d,n)
      !!!    end do
      !!!  end do
      !!!end do
    else
      hy_dhm = 0
      hy_dhp = 0
    end if

    !write(20, '(50e12.4)') hy_tauh(1:modele%ntot, 1:m_dtot)
    !stop -100
  end

  !=======================================================================================
  ! Adds contribution of single file to hy_tauh; also calculates m_dhmi and m_dhpi

  subroutine calc_tauhi(i_file)
    integer, intent(in) :: i_file ! index pointing to element of the filetoh_* arrays
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
    print *, now_jmax
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
    do n = 1, modele%ntot
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

    do n = 1,modele%ntot
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
    ! Interpolaton specially for hydrogen lines calculation.
    !
    ! Linear interpolation.
    !
    ! *Note* When seen in pfant output (e.g. flux.norm), the hydrogen line is
    ! a sequence of joined convex lines. This is probably due to hy_tauh undergoing
    ! something like -log(hy_tauh). These small convex lines are more easily noticed
    ! if a hydrogen line is calculated alone, without other abundances.
    !
    ! *Note* This routine is very similar to misc_math::ftlin3()*.
    !

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
! PFANT main executable: spectral synthesis
!
! Creates three files such as (flux.spec, flux.cont, flux.norm).
! These files can be inputted into nulbad
! or visualized with plot_spectra.py
!
! Takes as input most of the data files.
!
!
! *Note* Flux absolu sortant a ete multiplie par 10**5
!

program pfant
  use pfantlib
  use synthesis
  use pfant_x
  implicit none
  integer i

  !=====
  ! Startup
  !=====
  execonf_name = 'pfant'
  call molecules_idxs_init()
  call config_init()

  !=====
  ! File reading
  !=====
  call read_main(config_fn_main)
  call read_dissoc(config_fn_dissoc)

  !---
  ! (intermission)
  ! After reading *dissoc file* and *main file*,
  ! initializes variables whose values may come either from *main file* or
  ! command-line option
  !---
  call pfant_init_x()

  ! continues file reading
  call read_partit(config_fn_partit)  ! LECTURE DES FCTS DE PARTITION
  call read_absoru2(config_fn_absoru2)  ! LECTURE DES DONNEES ABSORPTION CONTINUE
  call read_modele(config_fn_modeles)  ! LECTURE DU MODELE
  if (config_opa) call read_opa(config_fn_opa)
  call read_abonds(config_fn_abonds)
  if (.not. config_no_atoms) then
    call read_atoms(config_fn_atoms)
    if (config_zinf .ne. -1) then
      do i = 1, atoms_nblend
        atoms_zinf(i) = config_zinf
      end do
    end if
  end if
  if (.not. config_no_h) then
    call read_hmap(config_fn_hmap)
    call read_filetoh(x_llzero, x_llfin)
  end if
  if (.not. config_no_molecules) call read_molecules(config_fn_molecules)

  if (abs(modele%asalog-main_afstar) > 0.01) then
    call log_and_halt('asalog from model ('//real82str(modele%asalog, 2)//&
     ') does not match afstar in main configuration file ('//real82str(main_afstar, 2)//')')
  end if


  !=====
  ! Spectral synthesis
  !=====
  ! Does the calculus
  call synthesis_()
end program pfant

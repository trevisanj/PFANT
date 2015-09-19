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
!> Prefix "sat4_" denotes variables filled by sat4() (or indirectly, die())
!>

module dissoc
  use config
  use readers
  implicit none

  ! They will be pointer targets at molecules::point_ppa_pb()
  real*8, target, dimension(MAX_MODELES_NTOT) ::  &
   sat4_pph,  & !< ?doc?
   sat4_ppc2, & !< ?doc?
   sat4_pn,   & !< ?doc?
   sat4_pc13, & !< ?doc?
   sat4_pmg,  & !< ?doc?
   sat4_po,   & !< ?doc?
   sat4_pti,  & !< ?doc?
   sat4_pfe     !< ?doc?

  private :: die ! private subroutine

  integer, private, parameter :: &
   Z_ELECTRON = 99,  &  !< Fictitious atomic number of electron
   Z_H_STAR   = 100, &  !< Fictitious atomic number of "H*"
   Z_H        = 1,   &  !< Atomic number of Hydrogen
   Z_HE       = 2       !< Atomic number of Helium

  real*8, private, dimension(MAX_DISSOC_Z) :: &
   ip,     & ! ?doc?
   ccomp,  & ! ?doc?
   uiidui, & ! ?doc?
   fp,     & ! ?doc?
   kp,     & ! ?doc?
   p         ! ?doc? Pressure

  real*8, private, dimension(MAX_DISSOC_NMOL) :: &
   ppmol, & ! ?doc?
   apmlog   ! ?doc?

  real*8, private :: pe !< Fictitious pressure of electron ?doc?

  real*8, private, parameter :: econst = 4.342945e-1 !< ?doc?
contains

  !=======================================================================================
  !> Subroutine d'equilibre dissociatif

  subroutine sat4()
    real*8, dimension(MAX_MODELES_NTOT, MAX_DISSOC_NMETAL) :: xp
    real*8  kplog, fplog, &
     pdfpl, pelog, pglog, pionl, plog, pmoll, tem, pg, theta, xlog
    real*8 cclogi
    integer i, ig0i, ig1i, iq, ir, irl, irr, ito, itx, j, jcount, nbl, &
     nelemi, nelemxi, k1, k2, k3, kd, kf

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

      ip(nelemxi) = dissoc_ip(i)
      uiidui(nelemxi) = ig1i * 0.661 / ig0i
      ccomp(nelemxi) = exp(cclogi/econst)

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
      p(nelemi) = 1.0e-20
    1400 continue

    !> @todo issue ?doc? if atomic number 99 was already in dissoc.dat?
    p(Z_ELECTRON) = 1.0e-10

    !
    !*****INPUT E

    do 1020 ito = 1,modeles_ntot
      theta = modeles_teta(ito)
      tem = 5040.0/theta
      pg = modeles_pg(ito)
      pglog = log10(pg)

      call die(tem,pg)

      pe = p(Z_ELECTRON)
      pelog = log10(pe)

      do 1303 i=1,dissoc_nmetal
        nelemi = dissoc_nelemx(i)

        fplog  = log10(fp(nelemi))
        xp(ito,i) = p(nelemi)+1.0e-30
        plog   = log10( xp(ito,i) )
        pdfpl  = plog - fplog
        if (mod(i,5)) 1303,1304,1303
        1304 continue
      1303 continue

      irl = 120
      do 1184 i=1,dissoc_nmetal
        nelemi = dissoc_nelemx(i)

        !> @todo issue is it OK to avoid log(0) by adding 1e-30 here??? (MT): p,kp=0 should be avoided.
        plog   = log10(p(nelemi)+1.0e-30)
        kplog  = log10(kp(nelemi)+1.0e-30)
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
        pmoll  = log10(ppmol(j)+1.0e-30)
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


  !=======================================================================================
  !> DIE9
  !>
  !> ?doc?

  subroutine die(tem, pg)
    real*8 tem, pg
    real*8, dimension(MAX_DISSOC_Z) :: fx, dfx, z, prev
    real*8, dimension(MAX_DISSOC_NMETAL) :: wa
    real*8 aplogj, atomj, delta, df, dhh, epsdie, &
     f, fph, heh, hkp, perev, pglog, ph, pmolj, pmoljl, q, r, s, &
     spnion, t, tem25, u, x, xr, pph, phh
    integer i, imaxp1, iterat, j, k, km5, m, mmaxj, nelemi, nelemj, &
     natomj, niter

    epsdie = 5.0e-3
    t      = 5040.0/tem
    pglog  = log10(pg)

    heh    = ccomp(Z_HE)/ccomp(Z_H)  ! Helium-to-Hydrogen ratio by number

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

    hkp = kp(Z_H)
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
    !> THe current dissoc.dat has no Z=100 (neither 99).
    !> Is this a remaining fragment of code? My hint comes from the fact that Z_ELECTRON=99 is addressed several times, but Z_H_STAR=100 is not.
    p(Z_H_STAR) = pph


    ! EVALUATION OF THE FICTITIOUS PRESSURE OF EACH ELEMENT
    do 1070 i=1,dissoc_nmetal
      nelemi = dissoc_nelemx(i)
      fp(nelemi) = ccomp(nelemi)*fph
    1070 continue

    ! CHECK OF INITIALIZATION
    pe = p(Z_ELECTRON)



    if(ph-p(Z_H)) 1402,1402,1401

    1401 continue
    do 1403 i=1,dissoc_nmetal
      !> @todo ISSUE: what if some NELEMI=Z_ELECTRON=99? THen P(99) will no longer be equal to PE
      nelemi=dissoc_nelemx(i)
      p(nelemi) = fp(nelemi)*exp(-5.0*t/econst)
    1403 continue
    p(Z_H) = ph   !> @todo ISSUE: overwriting P(1)

    ! Update: kept as-was
    ! !> @note P was being divided by 100 over and over at each j (molecule). This division has been taken out of loop, but is still an issue, since it is unclear *why* this division is being done.
    ! !> @todo issue ask blb being divided by 100 is still an issue
    ! do m =1,MAX_DISSOC_Z
    !   p(m)=1.0e-2*p(m)
    ! end do


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
        !> @todo log(p) called many times, should try to optimize
        pmoljl = pmoljl + float(natomj)*log10(p(nelemj))
      1042 continue

      if(pmoljl - (pglog+1.0) ) 1046,1046,1047

      1047 continue
      do 1048 m =1,mmaxj
        nelemj = dissoc_nelem(m,j)
        natomj = dissoc_natom(m,j)
        pmoljl = pmoljl + float(natomj)*(-2.0)

        ! For each j, divides all used elements in p by 100.
        ! This is necessary for convergence of the molecular equilibrium.
        p(nelemj) = 1.0e-2*p(nelemj)

      1048 continue

      1046 pmolj = exp(pmoljl/econst)
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
          dfx(nelemi) = dfx(nelemi) + atomj**2*pmolj/p(nelemi)
        1043 continue
      1044 continue
      ppmol(j) = pmolj
    1041 continue

    ! SOLUTION OF THE RUSSELL EQUATIONS BY NEWTON-RAPHSON METHOD
    do 2001 i=1,dissoc_nmetal
      nelemi=dissoc_nelemx(i)
      wa(i)=log10(p(nelemi)+1.0e-30)
    2001 continue

    imaxp1 = dissoc_nmetal+1
    wa(imaxp1) = log10(pe+1.0e-30)
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
    p(Z_ELECTRON)=pe

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
!> Contains subroutines filter_molecules_() and filter_atoms_()
!>
!> Variables calculated have prefixes km_f_ or atoms_f_

module filters
  use molecules_ids
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
    km_f_mm        !< Replicates km_mm(molid) for all selected lines of molecule molid.
                   !< Redundant information but simplifies use. Used in synthesis::selekfh()

  !------
  ! These two arrays contain indexes pointing at km_f_lmbdam, km_f_sj, km_f_jj, km_f_mm
  !------

  !> This one points to the last index of each molecule within
  !> km_f_lmbdam, km_f_sj and km_f_jj (after the filtering)
  !>
  !> @par Augmented vectors
  !> First element is 0 (ZERO) -- facilitates the algorithm implementation
  !> (first molecule corresponds to 2nd element).
  integer :: km_f_mblenq(NUM_MOL+1)

  !> This has a use "similar" to km_f_mblenq, but is a "local" one, it contains
  !> the index of the last
  !> line of each set of lines within km_f_lmbdam, km_f_sj and km_f_jj
  !> **for the current molecule** I_MOL
  !>
  !> @par Augmented
  !> First column is 0 (ZERO) -- facilitates the algorithm
  !> (second column of matrix corresponds to first molecule)
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
  !> Sweeps km_* to populate a few km_* depending on the interval LZERO-LFIN
  !>
  !> @todo test

  subroutine filter_molecules(lzero, lfin)
    !> Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    !> Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    real*8 :: lambda
    integer i, &
            molid,          &  ! Counts molecule id, from 1 to NUM_MOL
            i_mol,          &  ! Counts molecules that are "switched on"
            j_dummy, j_set, &
            i_line,         &  ! Index of km_lmbdam, km_sj, km_jj
            i_filtered         ! Counts number of filtered lines (molecule-independent);
                               !  index of km_f_lmbdam, km_f_sj, km_f_jj
    logical flag_in

    write(lll, *) 'molids%n_on = ', molids%n_on
    call log_debug(lll)

    ! Initializes the zero elements of the augmented matrices
    km_f_mblenq(1) = 0
    do i = 1, num_mol
      km_f_ln(1, i) = 0
    end do

    i_filtered = 0  ! Current *filtered-in* spectral line. Keeps growing (not reset when the molecule changes). Related to old "L"
    i_line = 1
    i_mol = 0
    do molid = 1, km_number
      if (.not. molecule_is_on(molid)) cycle

      i_mol = i_mol+1

      !#logging
      write(lll, *) 'molecule id', molid, ': ',  km_titulo(molid)
      call log_debug(lll)
      write(lll, *) 'number of prospective lambdas ------>', km_lines_per_mol(molid)
      call log_debug(lll)


      ! Counters starting with "J_" restart at each molecule
      j_set = 1   ! Current "set-of-lines"
      flag_in = .FALSE.  ! Whether has filtered in at least one line
      do j_dummy = 1, km_lines_per_mol(molid)
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

          km_f_mm(i_filtered) = km_mm(molid)

          flag_in = .true.

        end if

        if (i_line .eq. km_iollosol(j_set, molid)) then
          ! Reached last line of current set of lines


!           !> @todo ISSUE Should we think about preparing it for not having a single line within LZERO-LFIN for set J_SET, J_SET=1,NNV?????
!           IF (.NOT. FLAG_IN) THEN
!             !> @todo IDEA Actually I think that it might work without having lines within a given lambda range, because the routines that use the calculations just don't care which molecule it is
!             !> but I can give a *WARNING*, more for testing than for anything else, actually
!
!             !--error checking--!
!             !> @todo test this error
!             WRITE (*, *) 'FILTER_molecules(): Molecule ID ',MOLID,
!    +            ' titled  "', km_TITULO(MOLID), '"'
!             WRITE (*, *) 'Set of lines ', (J_SET), 'has no lambda '
!    +            //'within ', LZERO, ' <= lambda <= ', LFIN
!             WRITE (*, *) 'The algorithm is not prepared for this, '
!    +            //'sorry!'
!             STOP ERROR_BAD_VALUE
!           END IF

          !> @todo issue test number of filtered items against original pfantgrade and write testing suite for this

          km_f_ln(j_set+1, i_mol) = i_filtered  ! Yes, j_set+1, not j_set, remember km_f_ln first row is all ZEROes.
          j_set = j_set+1
        end if

        i_line = i_line+1
      end do

      km_f_mblenq(i_mol+1) = i_filtered  ! Yes, i_mol+1, not i_mol, remember km_f_mblenq(1) is ZERO.
    end do !--end of MOLID loop--!

    km_f_mblend = i_filtered
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
  use molecules_ids
  use dimensions
  use dissoc
  use reader_molecules
  use filters

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_c_gfm,    & !< ?doc?
    km_c_alargm    !< ?doc?

  real*8, dimension(MAX_KM_LINES_TOTAL, MAX_MODELES_NTOT) :: km_c_pnvj

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
    integer i_mol, j_set, l, l_ini, l_fin, n, nnv, molid

    real*8, parameter :: H  = 6.6252E-27,   &
                         C  = 2.997929E+10, &
                         KB = 1.38046E-16,  &
                         C2 = 8.8525E-13

    do i_mol = 1, molids%n_on
      molid = get_molid(i_mol)

      call point_ppa_pb(molid)

      nnv = km_nv(molid)

      fe  = km_fe(molid)
      do_ = km_do(molid)
      mm  = km_mm(molid)
      am  = km_am(molid)
      bm  = km_bm(molid)
      ua  = km_ua(molid)
      ub  = km_ub(molid)
      te  = km_te(molid)
      cro = km_cro(molid)


      !======
      ! This part of the code calculates km_PNVL
      rm = am*bm/mm
      do n = 1,modeles_ntot
        t5040 = modeles_teta(n)/5040
        psi = do_*modeles_teta(n)+2.5*log10(modeles_teta(n))-1.5*log10(rm)-&
              log10(ua*ub)-13.670
        psi = 10.**psi

        do j_set = 1,nnv
          qv = km_qqv(j_set, molid)
          gv = km_ggv(j_set, molid)
          bv = km_bbv(j_set, molid)
          dv = km_ddv(j_set, molid)

          l_ini = km_f_ln(j_set, molid)+1
          l_fin = km_f_ln(j_set+1, molid)

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
            facto = km_fact(j_set, molid)
            km_c_gfm(l) = C2*((1.e-8*km_f_lmbdam(l))**2)*fe*qv*km_f_sj(l)*facto
          end if
        end do
      end do
    end do ! end of i_mol loop

    do l = 1, km_f_mblend
      km_c_alargm(l) = 0.1
    end do
  end



  !=======================================================================================
  !> Private subroutine; pointer operation; assigns address of variable PPA and PB depending on the molecule ID.
  !>
  !> This was originally a vector copy element-by-element in old routine KAPMOL. However, as
  !> PPA and PB contents are not changed after the assignment, it is reasonable to just point
  !> to the source vectors (way faster).

  subroutine point_ppa_pb(molid)
    implicit none
    integer molid
    character*192 s

    if (molid .gt. num_mol) then
      write (s, *) 'point_ppa_pb(): invalid molecule id (', molid, ') must be maximum ', num_mol
      call pfant_halt(s)
    end if

    select case (molid)
      case (1)  ! MgH
        ppa => sat4_pmg
        pb  => sat4_pph
      case (2)  ! C2
        ppa => sat4_ppc2
        pb  => sat4_ppc2
      case (3, 4, 5)  ! CN blue,red, nir
        ppa => sat4_ppc2
        pb  => sat4_pn
      case (6, 7, 8)  ! CH AX, BX, CX
        ppa => sat4_ppc2
        pb  => sat4_pph
      case (9)  ! 13
        ppa => sat4_pc13
        pb  => sat4_pph
      case (10)  ! CO nir
        ppa => sat4_ppc2
        pb  => sat4_po
      case (11)  ! NH blue
        ppa => sat4_pn
        pb  => sat4_pph
      case (12, 13)  ! OH blue,nir
        ppa => sat4_po
        pb  => sat4_pph
      case (14)  ! FeH
        ppa => sat4_pfe
        pb  => sat4_pph
      case (15, 16, 17, 18, 19, 20, 21)  ! Tio Gama,Gama linha,alfa,beta,delta,epsilon,phi
        ppa => sat4_pti
        pb  => sat4_po
    end select
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Declaration and initialization of x_* variables
!>
!> These variable values may come either from dfile:main or command-line options.

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

  private  ! This statement makes all symbols private by default

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


  !> Calculated by subroutine calc_tauh
  real*8 ct_tauhi(MAX_DTOT, MAX_MODELES_NTOT)
  integer :: &
   ct_dhmi, & !< Calculated by subroutine calc_tauh
   ct_dhpi    !< Calculated by subroutine calc_tauh

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

    real*8 lzero, lfin

    integer :: &
     d,        &
     dtot,     & ! number of different wavelenghts for which flux will be calculated at each ikey-iteration
     dhmy(MAX_FILETOH_NUM_FILES), &
     dhpy(MAX_FILETOH_NUM_FILES)
    integer dhm,dhp

    real*8 gfal(MAX_ATOMS_F_NBLEND), &
           ecart(MAX_ATOMS_F_NBLEND), &  ! MT: some sort of delta lambda
           ecartm(MAX_KM_F_MBLEND)

    real*8 ttd(MAX_DTOT), &
           fn(MAX_DTOT), &
           tauh(MAX_DTOT, MAX_MODELES_NTOT), &
           tauhy(MAX_FILETOH_NUM_FILES, MAX_DTOT, MAX_MODELES_NTOT)

    integer i, i1, i2, ih, &
     ikey,    & ! ikey-th main_aint-large calculation interval
     ikeytot, & ! total number of main_aint-large calculation intervals
     iht, ilzero, im, imy, irh, itot, k, l, li,&
     n
    real*8 lambd, l0, lf, allhy, alzero, tetaef, xlfin, xlzero, ahnu, ahnu1, &
     ahnu2, alph0, alph01, alph02


    !=====
    ! Setup
    !=====

    ! @todo ?doc? The reason why values read from file are being overwritten should be explained.
    absoru2_abhel = modeles_nhe
    absoru2_abmet = absoru2_abmet*10.**modeles_asalog

    tetaef = 5040/main_teff

    !-----
    ! Output files opened here and left open until the end
    !-----
    ! Note that existing files are replaced
    open(unit=UNIT_SPEC, file=full_path_w(trim(x_flprefix)//'.spec'), status='replace')  ! spectrum
    open(unit=UNIT_CONT, file=full_path_w(trim(x_flprefix)//'.cont'), status='replace')  ! continuum
    open(unit=UNIT_NORM, file=full_path_w(trim(x_flprefix)//'.norm'), status='replace')  ! normalized
    !--- open(unit=UNIT_LINES,file=full_path_w(config_fn_lines), status='replace')               ! outfile:lines
    !--- open(unit=UNIT_LOG,  file=full_path_w(config_fn_log), status='replace')                 ! log.log


    !=====
    ! Calculation begins!
    !=====

    call turbul()

    ! -- III --
    ! Calcul de quant  ne dependant que du metal et du modele
    ! Population du niv fond des ions
    call popul()

    ! -- IV --
    ! Calcul des quantites ne dependant que du
    ! Modele et de lambda : bk_b(n)   bk_kc(n)   bk_fc ISSUE probably wrong comment
    call sat4()


    ! initial calculation sub-interval
    xlzero = x_llzero-LAMBDA_STRETCH
    xlfin = xlzero+main_aint+LAMBDA_STRETCH

    ! discovers the number of iterations
    if(xlfin .ge. (x_llfin+LAMBDA_STRETCH)) then
      ikeytot = 1
    else
      do i = 2,250
        xlfin = xlfin+main_aint
        if(xlfin .ge. (x_llfin+LAMBDA_STRETCH)) exit
      end do
      ikeytot = i
    end if

    lzero = x_llzero-LAMBDA_STRETCH
    lfin = lzero+main_aint+LAMBDA_STRETCH
    ikey = 1

    !> @todo check if 10 is LAMBDA_STRETCH/2.
    !
    !> @todo This is used by nulbad. Gotta plot non-convolved on top of convolved and see if the lambdas are right
    l0 = x_llzero-10.
    lf = x_llfin+10.



    !=====
    ! Main loop
    !=====
    call log_progress(0, ikeytot)
    do while (.true.)
      call log_info('/\/\/\ Calculation step '//int2str(ikey)//'/'//int2str(ikeytot)//&
        ' /\/\/\')

      ! Note: (lfin-lzero) is constant except in the last iteration where lfin may be corrected
      dtot = int((lfin-lzero)/main_pas + 1.0005)

      ! spill check
      if(dtot .gt. MAX_DTOT) then
        call pfant_halt('dtot = '//int2str(dtot)//' exceeds maximum of MAX_DTOT='//&
         int2str(MAX_DTOT))
      end if

      !#logging
      117 format(5x,'lzero=',f10.3,10x,'lfin=',f10.3,5x,'dtot=',i7)
      write(lll, 117) lzero, lfin, dtot
      call log_info(lll)

      lambd = (lzero+lfin)/2
      ilzero = int(lzero/100.)*100
      alzero = lzero-ilzero

      do d = 1,dtot
        ttd(d) = alzero+main_pas*(d-1)
      end do

      call bk()

      !#logging
      501 format(2x,2x,'llzero=',f10.3,2x,'llfin=',f10.3,  2x,'lzero=',f10.3,2x,'lfin=',f10.3,2x,'lambd 1/2=',f10.3)
      write(lll,501) x_llzero,x_llfin,lzero,lfin,lambd
      call log_info(lll)


      im = 0
      do ih = 1,filetoh_num_files
        allhy = filetoh_llhy(ih)-lzero

        if (((allhy .gt. 0) .and. (allhy .le. (main_aint+H_LINE_WIDTH+LAMBDA_STRETCH))) .or. &
            ((allhy .lt. 0.) .and. (allhy .ge. (-H_LINE_WIDTH)))) then


          im = im+1
          irh = 1
          iht = ih

          !#logging
          712 format(1x,'im=',i3,2x,'lambda h=',f8.3,2x,'filename=',a,2x,'ih=',i5)
          write(lll,712) im, filetoh_llhy(ih), ''''//trim(filetoh_filenames(iht))//'''', iht
          call log_info(lll)


          ! Lecture tau raie hydrogene et interpolation de tauh
          ! Type *,' nom des fichiers TAU raies Hydrogene'
          !
          ! Old "LECTAUH()". Calculates tauh, dhmi and dhpi for file identified by ih.
          call calc_tauh(ih, dtot, ttd, ilzero)

          dhmy(im) = ct_dhmi
          dhpy(im) = ct_dhpi
          do n = 1,modeles_ntot
            do d = 1,dtot
              tauhy(im, d, n) = ct_tauhi(d,n)
            end do
          end do
        end if
      end do

      imy = im
      if(imy .ne. 0) then
        !#logging
        write(lll,*) (dhmy(im), im=1,imy)
        call log_debug('DHMY ==> '//lll)
        write(lll,*) (dhpy(im), im=1,imy)
        call log_debug('DHPY ==> '//lll)

        dhp = maxi(dhpy, imy, 1, imy)
        dhm = mini(dhmy, imy, 1, imy)
        do n = 1,modeles_ntot
          do d = 1,dtot
            tauh(d,n) = 0.0
          end do
        end do

        do n = 1,modeles_ntot
          do d = 1,dtot
            do im = 1,imy
              tauh(d,n) = tauh(d,n)+tauhy(im,d,n)
            end do
          end do
        end do
      else
        irh=0
        dhm=0
        dhp=0
      end if


      ! -- V --
      ! Quantites dependant de la raie et du modele
      call filter_atoms(lzero, lfin)

      if(atoms_f_nblend .gt. 0) then
        call popadelh()

        ! -- VI --
        ! Calcul du coefficient d absorption selectif et calcul du spectre
        do k = 1, atoms_f_nblend
          gfal(k) = atoms_f_gf(k)*C2*(atoms_f_lambda(k)*1.e-8)**2
          ecart(k) = atoms_f_lambda(k)-lzero+main_pas
        end do
      end if

      call filter_molecules(lzero, lfin)
      call kapmol_()

      !--debugging--!
      704 format(1x,'mblend=',i10)
      write(lll, 704) km_f_mblend
      call log_debug(lll)

      do l = 1, km_f_mblend
        ecartm(l) = km_f_lmbdam(l)-lzero + main_pas
      end do

      call selekfh()



      li = int(10./main_pas)
      i1 = li+1
      i2 = dtot - li
      if (lfin .ge. (x_llfin+LAMBDA_STRETCH)) then
        i2 = int((x_llfin+10.-lzero)/main_pas + 1.0005)
      end if
      itot = i2-i1+1
      do d = i1,i2
        selekfh_fl(d) = selekfh_fl(d)*(10.**5)
        selekfh_fcont(d) = selekfh_fcont(d)*(10.**5)
        fn(d) = selekfh_fl(d) / selekfh_fcont(d)  ! normalized spectrum
      end do

      !=====
      ! Writes results for current iteration into open files
      !=====
      ! call write_lines_fort91()                        ! outfile:lines and fort.91
      ! call write_log()                                ! log.log
      call write_spec_item(UNIT_SPEC, selekfh_fl)     ! spectrum
      call write_spec_item(UNIT_CONT, selekfh_fcont)  ! continuum
      call write_spec_item(UNIT_NORM, fn)             ! normalized

      !#logging
      707 format(1x,'ikey=',i10,2x,'lzero=',f10.3,2x,'lfin=',f10.3, 2x,'i1=',i7,2x,'i2=',i7)
      write(lll,707) ikey, lzero, lfin, i1, i2
      call log_info(lll)

      call log_progress(ikey, ikeytot)

      ikey = ikey+1
      if (ikey .gt. ikeytot) exit  ! main loop smooth exit

      !#logging
      708 format(1x,'ikey=',i10,2x,'irh=',i6)
      write(lll, 708) ikey, irh
      call log_info(lll)

      lzero = lzero+main_aint
      lfin = lfin+main_aint
      if(lfin .gt. (x_llfin+LAMBDA_STRETCH)) lfin = x_llfin+LAMBDA_STRETCH

    end do  ! main loop

    close(UNIT_SPEC)
    close(UNIT_CONT)
    close(UNIT_NORM)
    !--- close(UNIT_LOG)
    !--- close(UNIT_LINES)

    !#logging
    call log_info('Flux sortant est en nu: Fnu x lambda')
    call log_info('Flux absolu sortant a ete multiplie par 10**5')


    call log_debug(LEAVING//'synthesis_()')


  contains  !--still pfant_calculate()

    ! These subroutines have total knowledge of the variable names and values that appear inside
    ! their parent subroutine PFANT_CALCULATE(). http://www.personal.psu.edu/jhm/f90/statements/contains.html

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
       lzero,                  &  ! changes (value changes with each iteration)
       lfin,                   &  ! changes
       itot,                   &  ! changes
       main_pas,               &  ! fixed
       main_echx,              &  ! fixed
       main_echy,              &  ! fixed
       main_fwhm                  ! fixed

      write(unit_,'(40000f15.5)') (item(d), d=i1,i2)
    end

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
!       lzero, &
!       lfin, &
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



    !======================================================================================================================
    !> Sets the Voigt profile using Hjertings' constants.
    !>
    !> @note Convolution for molecules uses Gaussian profile.
    !>
    !> @todo MT+JT Decision on variable MM: logic suggests that there should be one MM per molecule, so we are going to make

    subroutine selekfh()
      integer d
      real*8 :: bi(0:MAX_MODELES_NTOT)
      real*8, dimension(MAX_ATOMS_F_NBLEND) :: &
       ecar, &
       ecartl, &
       ka
      real*8, dimension(MAX_MODELES_NTOT) :: &
       kap,    &
       kappa,  &
       kci,    &
       tauhd,  &
       kappam, &
       kappt
      real*8, dimension(MAX_KM_F_MBLEND) :: &
       ecarm,   &
       ecartlm, &
       kam
      real*8 :: &
       deltam(max_km_f_mblend,MAX_MODELES_NTOT), &
       phi, t, v, vm, lambi


      if (atoms_f_nblend .ne. 0) then
        do k = 1,atoms_f_nblend
          ecar(k) = ecart(k)
        end do
      end if

      if (km_f_mblend .ne. 0) then
        do k=1,km_f_mblend
          ecarm(k) = ecartm(k)
        end do
      end if


      do d = 1, dtot
        lambi = (6270+(d-1)*0.02)
        if (atoms_f_nblend .ne. 0) then
          do k=1,atoms_f_nblend
            ecar(k)=ecar(k)-main_pas
            ecartl(k)=ecar(k)
          end do
        end if

        if(km_f_mblend .ne. 0) then
          do k=1,km_f_mblend
            ecarm(k) = ecarm(k)-main_pas
            ecartlm(k) = ecarm(k)
          end do
        end if

        do n = 1,modeles_ntot
          kappa(n) =0.
          kappam(n) =0.
          t = 5040./modeles_teta(n)

          ! atomes
          if(atoms_f_nblend .eq. 0) go to 260

          do  k=1,atoms_f_nblend
            if(abs(ecartl(k)) .gt. atoms_f_zinf(k)) then
              ka(k) = 0.
            else
              v = abs(ecar(k)*1.e-8/popadelh_delta(k,n))
              call hjenor(popadelh_a(k,n), v, popadelh_delta(k,n), phi)

              if(atoms_f_elem(k) .eq. ' O') then
                ! #NOXIG: oxygen is a particular case here
                ka(k) = phi * popadelh_pop(k,n) * gfal(k)
              else
                ka(k) = phi * popadelh_pop(k,n) * gfal(k) * atoms_f_abonds_abo(k)
              end if

            end if
            kappa(n) = kappa(n) + ka(k)
          end do   !  fin bcle sur k

          260 continue

          ! molecule
          if(km_f_mblend.eq.0) go to 250
          do l=1,km_f_mblend
            if( abs(ecartlm(l)) .gt. km_c_alargm(l) )  then
              kam(l)=0.
            else
              deltam(l,n) = (1.e-8*km_f_lmbdam(l))/C*sqrt(turbul_vt(n)**2+DEUXR*t/km_f_mm(l))
              vm = abs(ecarm(l)*1.e-08/deltam(l,n))
              phi = (exp(-vm**2))/(RPI*deltam(l,n))
              kam(l) = phi*km_c_gfm(l)*km_c_pnvj(l,n)
            end if
            kappam(n)=kappam(n)+kam(l)
          end do   !  fin bcle sur l

          250 continue
          kappt(n) = kappa(n)+kappam(n)
          kci(n) = bk_kcd(d,n)
          kap(n) = kappt(n)+kci(n)
          bi(n) = ((bk_b2(n)-bk_b1(n))*(float(d-1)))/(float(dtot-1)) + bk_b1(n)
        end do    ! fin bcle sur n

        bi(0) = ((bk_b2(0)-bk_b1(0))*(float(d-1)))/(float(dtot-1)) + bk_b1(0)

        !#logging
        if (d .eq. 1 .or. d .eq. dtot) then
          150 format(' d=',i5,2x,'kci(1)=',e14.7,2x,'kci(ntot)=',e14.7,10x,'kappa(1)=',e14.7,2x,'kappa(ntot)=',e14.7)
          152 format(10x,'kappam(1)=',e14.7,2x,'kappam(ntot)=',e14.7)
          151 format(' d=',i5,2x,'bi(0)=',e14.7,2x,'bi(1)=',e14.7,2x,'bi(ntot)=',e14.7)

          write(lll,151) d,bi(0),bi(1),bi(modeles_ntot)
          call log_debug(lll)
          write(lll,150) d,kci(1),kci(modeles_ntot),kappa(1),kappa(modeles_ntot)
          call log_debug(lll)
          write(lll,152)kappam(1),kappam(modeles_ntot)
          call log_debug(lll)
        end if

        if((d .lt. dhm) .or. (d .ge. dhp)) then
          call flin1(kap,bi,modeles_nh,modeles_ntot,main_ptdisk,main_mu, config_kik)
          selekfh_fl(d) = flin_f
        else
          do n = 1,modeles_ntot
              tauhd(n) = tauh(d,n)
          end do
          call flinh(kap,bi,modeles_nh,modeles_ntot,main_ptdisk,main_mu, config_kik,tauhd)
          selekfh_fl(d) = flin_f
        end if

        ! Dez 03-P. Coelho - calculate the continuum and normalized spectra
        call flin1(kci,bi,modeles_nh,modeles_ntot,main_ptdisk,main_mu, config_kik)
        selekfh_fcont(d) = flin_f
      end do  ! fin bcle sur d
    end


    !======================================================================================================================
    !> Calculates the flux in the continuum.
    !> @todo There is a lot of calculation here that is independent from lzero and lfin (optimize?)
    !>
    !> @todo log_pe not used yet

    subroutine bk()
      real*8 nu, llzero, llfin, nu1, nu2, &
       alph_n, &! old ALPH, which was a vector, but I realized it is used only inside loop, no need for vector
       log_pe   ! Created to avoid calculating ALOG10(PE) 3x
      real*8, dimension(2, max_modeles_ntot) :: kcj
      real*8, dimension(2) :: kcn, lambdc
      real*8 ::  fttc(MAX_DTOT)
      real*8 c3, c31, c32, fc1, fc2, t, tet0
      integer d, j

      call log_debug(ENTERING//'bk()')

      llzero = lzero
      llfin  = lfin
      nu1 = C* 1.e+8 /lzero
      ahnu1 = h*nu1
      c31 = (2*ahnu1) * (nu1/C)**2

      do n = 1,modeles_ntot
        t = 5040./modeles_teta(n)
        alph_n = exp(-ahnu1/(KB*t))
        bk_b1(n) = c31 * (alph_n/(1.-alph_n))
        call absoru_(llzero,modeles_teta(n),log10(modeles_pe(n)),1,1,1,1,2, .false.)
        bk_kc1(n) = absoru_totkap(1)
      end do

      nu2 = C* 1.e+8 /lfin
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

      nu = C* 1.e+8 /lambd
      ahnu = H*nu
      c3 =(2*ahnu) * (nu/C)**2
      do n=1,modeles_ntot
        t=5040./modeles_teta(n)
        alph_n = exp(-ahnu/(KB*t))
        bk_b(n) = c3 * (alph_n/(1.-alph_n))
        call absoru_(lambd,modeles_teta(n),log10(modeles_pe(n)),1,1,1,1,2, .false.)
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

      ilzero = int(lzero/100.)*100
      lambdc(1) = lzero-ilzero
      lambdc(2) = lfin-ilzero
      do n=1,modeles_ntot
        kcj(1,n)=bk_kc1(n)
        kcj(2,n)=bk_kc2(n)
      end do
      do n=1,modeles_ntot
        do j=1,2
          kcn(j)=kcj(j,n)
        end do
        call ftlin3(2,lambdc,kcn,dtot,ttd,fttc)
        do d=1,dtot
          bk_kcd(d,n)=fttc(d)  !> @todo these vector copies... pointer operations could speed up considerably here (optimize)
        end do
      end do

      !#logging
      153 format(' bk_kcd(1,1)=',e14.7,2x,'bk_kcd(1,ntot)=',e14.7)
      154 format(' bk_kcd(dtot,1)=',e14.7,2x,'bk_kcd(dtot,ntot)=',e14.7)
      write(lll,153) bk_kcd(1,1),bk_kcd(1,modeles_ntot)
      call log_debug(lll)
      write(lll,154) bk_kcd(dtot,1),bk_kcd(dtot,modeles_ntot)
      call log_debug(lll)

      call log_debug(LEAVING//'bk()')
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
      131 format(' v micro constante  =',f6.1,'km/s')
      write(lll,131) main_vvt(1)
      call log_debug(lll)
    else
      call log_debug('v micro variable avec profondeur')
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
    real*8 u(3), alistu(63), ue(50), tt(51), &
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
  !> le "popadelh_A" utilise dans le calcul de H(popadelh_A,V)
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
      if(atoms_f_ch(k).lt.1.e-37)  then
        !> @todo optimize create atoms_partit_ki1, atoms_partit_ki2 to be filled by inner join upon reading atoms

        kies = (12398.54/atoms_f_lambda(k)) + atoms_f_kiex(k)
        if(ioo.eq.1) kii = partit_ki1(j)
        if(ioo.eq.2) kii = partit_ki2(j)

        if(popadelh_corch(k).lt.1.e-37)   then
          popadelh_corch(k) = 0.67 * atoms_f_kiex(k) +1
        end if

        ! 125 format(3x ,' pour',f9.3,'   on calcule ch ', 'van der waals et on multiplie par ',f7.1)
        ! write(6,125)  atoms_f_lambda(k), popadelh_corch(k)
        popadelh_cvdw(k)= calch(kii,ioo,atoms_f_kiex(k),isi,kies,iss)

        atoms_f_ch(k) = popadelh_cvdw(k) * popadelh_corch(k)
      end if

!
      if(atoms_f_ch(k) .lt. 1.e-20) then
        iopi=1
      else
        iopi=2
      end if

      do n = 1, modeles_ntot
        t=5040./modeles_teta(n)
        nul= C* 1.e+8 /atoms_f_lambda(k)
        ahnul= H*nul
        alphl(n)=exp(-ahnul/(KB*t))

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
      end do
    end do
  end

  !=======================================================================================
  !> Calculates tauhi, dhmi and dhpi for file specified
  !>
  !> @note this is originally subroutine "LECTAUH" without the file reading part
  !>
  !> @todo top test the pointers

  subroutine calc_tauh(i_file, dtot, ttd, ilzero)
    integer, intent(in) :: i_file !< index pointing to element of the filetoh_* arrays
    !> ?doc? Number of calculation steps, I think. ISSUE: better explanation
    !> Calculated as: @code dtot = (lfin-lzero)/main_pas + 1.0005 @endcode
    integer, intent(in) :: dtot
    !> integer version of variable lzero in main module
    integer, intent(in) :: ilzero
    !> ?doc? Calculated as: ttd(d) = alzero+main_pas*(d-1)
    real*8, intent(in) :: ttd(MAX_DTOT)
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

    !~WRITE(6,'(A80)') filetoh_TITRE
    !~WRITE(6,'(A11)') filetoh_TTT
    !~WRITE(6,'('' now_jmax='',I3)') now_jmax
    !~WRITE(6,'(2X,5F14.3)') (LLAMBDH(JJ), JJ=1,JJMAX)
    !~WRITE(6,'(2X,5F14.3)') (LLAMBDH(JJ), JJ=1,JJMAX)
    !~
    !~DO N = 1,modeles_NTOT,5
    !~  WRITE(6,'('' N='',I3)') N
    !~  WRITE(6,'(2X,5E12.4)') (TTH(JJ,N), JJ=1,JJMAX)
    !~END DO


    do j = 1,jjmax
      allh(j) = llambdh(j)-ilzero
    end do

    !~ WRITE(6, '('' ALLH(1)='',F8.3,2X,''ALLH(JJMAX)='',F8.3,2X)')
    !~+      ALLH(1),ALLH(JJMAX)
    !~ WRITE(6, '('' JJMAX='',I3,2X,''NTOT='',I3,2X,''DTOT='',I5)')
    !~       JJMAX, modeles_NTOT, DTOT

    do n = 1,modeles_ntot
      do j = 1,jjmax
        tauhn(j) = tth(j,n)
      end do

      call ftlin3h()

      do d = 1,dtot
        ct_tauhi(d, n) = ftth(d)
      end do
    end do


    !~ !--debugging--!
    !~ WRITE(6,'('' TAUHI(1,1)='',E14.7,2X,''TAUHI(1,NTOT)='',E14.7)')
    !~+ ct_tauhi(1,1), ct_tauhi(1,modeles_NTOT)
    !~ WRITE(6,'('' TAUHI(DTOT,1)='',E14.7,2X,'
    !~+ //'''TAUHI(DTOT,NTOT)='',E14.7)')
    !~+ ct_tauhi(DTOT,1), ct_tauhi(DTOT,modeles_NTOT)

  contains
    !-------------------------------------------------------------------------------
    !> ?doc?
    !>
    !> @note This routine is very similar to misc_math::ftlin3()*.
    !>
    !> Uses variables from parent filetoh_auh():
    !> @li dtot
    !> @li ttd
    !> @li jjmax
    !>
    subroutine ftlin3h()
      real*8 dy, ft, t, t0, t1, t2, u0
      integer j, k, kk, jj, kk1, kq

      j=2
      kk=1
      24 continue
      do 4 k = kk,dtot
        kq=k
        t=ttd(k)

        jj=j-1
        do 1  j=jj,jjmax
          if(t-allh(j) ) 3,2,1
          1 continue
          go to 10
          2 ft=tauhn(j)
        if(j .eq. 1) j = j+1
        go to 4

        3 if (j .eq. 1) go to 10
        u0 = tauhn(j)-tauhn(j-1)
        t0 = allh(j)-allh(j-1)
        t1 = t-allh(j-1)

        t2= t1/t0
        dy= u0*t2
        ft= tauhn(j-1) + dy
        ftth(k) = ft
      4 continue

      14 continue

      do k=1,dtot
        if(ftth(k).ne.0.0) go to 20
      end do

      20 ct_dhmi = k

      ! ?doc?
      if (ct_dhmi .eq. dtot) ct_dhmi = 1


      kk1 = ct_dhmi+1
      do k = kk1,dtot
        if (ftth(k) .eq. 0.0) go to 30
      end do

      30 ct_dhpi = k

      ! (Paula Coelho 21/11/04) instrucao da Marie Noel
      ! ?doc?
      if (ftth(dtot) .ne. 0.0) ct_dhpi = dtot

      return

      10 ftth(k) = 0.
      j = j+1

      kk = kq
      kk = kk+1
      if (kq .gt. dtot) go to 14
      go to 24
    end
  end
end module

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module pfant_lib
end

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> PFANT main executable: spectral synthesis
!>
!> @todo List of prefixes somewhere

program pfant
  use config
  use logging
  use synthesis
  use welcome
  use readers
  use pfant_x

  implicit none

  !=====
  ! Startup
  !=====
  execonf_name = 'pfant'
  call molecules_ids_init()
  call config_init()

  !=====
  ! File reading
  !=====
  call read_dissoc(full_path_w(config_fn_dissoc))
  call read_main(full_path_w(config_fn_main))

  !---
  ! (intermission)
  ! Initializes variables whose values may come either from dfile:main or
  ! command-line argument as soon as it reads the main file
  !---
  call pfant_init_x()

  ! continues file reading
  call read_dissoc(full_path_w(config_fn_dissoc))
  call read_partit(full_path_w(config_fn_partit))  ! LECTURE DES FCTS DE PARTITION
  call read_absoru2(full_path_w(config_fn_absoru2))  ! LECTURE DES DONNEES ABSORPTION CONTINUE
  call read_modele(full_path_w(config_fn_modeles))  ! LECTURE DU MODELE
  call read_abonds(full_path_w(config_fn_abonds))
  call read_atoms(full_path_w(config_fn_atoms))
  ! Gets list of hydrogen lines filenames either from dfile:main or dfile:hmap.
  ! The latter is not the preferred way.
  if (config_hmap) then
    call read_hmap(full_path_w(config_fn_hmap))
  else
    call hmap_copy_from_main()
  end if

  call read_filetoh(x_llzero, x_llfin)
  call read_molecules(full_path_w(config_fn_molecules))


  !=====
  ! Spectral synthesis
  !=====
  ! Does the calculus
  call synthesis_()

end program pfant


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

!> Contains subroutine kapmol_()
!>
!> Calculated variables have prefix "km_c_"

module kapmol
  use molecules_ids
  use max_
  use dissoc
  use reader_molecules
  use filters

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_c_gfm,    & !< ?doc?
    km_c_alargm    !< ?doc?

  real*8, dimension(MAX_KM_R_LINES_TOTAL, MAX_MODELES_NTOT) :: km_c_pnvj

  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  !> @todo test the pointers
  real*8, private, pointer, dimension(:) :: ppa, pb

  private point_ppa_pb
  save
contains


!=======================================================================================
!> Calculates the molecular absorption coefficient.
!>
!> Uses km_* filled by FILTER_molecules()

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

      nnv = km_r_nv(molid)

      fe  = km_r_fe(molid)
      do_ = km_r_do(molid)
      mm  = km_r_mm(molid)
      am  = km_r_am(molid)
      bm  = km_r_bm(molid)
      ua  = km_r_ua(molid)
      ub  = km_r_ub(molid)
      te  = km_r_te(molid)
      cro = km_r_cro(molid)


      !======
      ! This part of the code calculates km_PNVL
      rm = am*bm/mm
      do n = 1,modeles_ntot
        t5040 = modeles_teta(n)/5040
        psi = do_*modeles_teta(n)+2.5*log10(modeles_teta(n))-1.5*log10(rm)-&
              log10(ua*ub)-13.670
        psi = 10.**psi

        do j_set = 1,nnv
          qv = km_r_qqv(j_set, molid)
          gv = km_r_ggv(j_set, molid)
          bv = km_r_bbv(j_set, molid)
          dv = km_r_ddv(j_set, molid)

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
            facto = km_r_fact(j_set, molid)
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

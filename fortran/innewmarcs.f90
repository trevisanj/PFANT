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
!||| PROGRAM |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! INNEWMARCS
!
! Interpolation d'un modele dans les grilles de modeles de
! NEWMARCS (2005) en fonction de Teff, log g et [Fe/H].
!

program innewmarcs
  use pfantlib
  implicit none

  real*8, parameter :: K_EQ = 1e-5  ! value to use to compare two numbers for equality, e.q..,
                                    ! "if (abs(a-b) .lt. K_EQ)"

  ! # Grid information
  ! all grid records (points) in sequence. Must be order by (asalog, teff, glog)
  type(moo_record), allocatable :: recs(:)
  ! lists extracted from the records
  real*8, allocatable :: glogs(:), teffs(:), asalogs(:)
  ! number of grid points
  integer n
  ! information about the records found/used for interpolation
  integer, dimension(8) :: xs, ys, zs, ii
  ! selected records
  type(moo_record) :: rr(8)

  ! # interpolated records
  !       glog    teff    asalog  <-- dimension of interpolation
  !        |       |        |
  !
  ! rr(1)  \_  aa  \
  ! rr(2)  /        |- ee
  ! rr(3)  \_  bb  /        \
  ! rr(4)  /                 |-   zz
  ! rr(5)  \_  cc  \        /
  ! rr(6)  /        |- ff
  ! rr(7)  \_  dd  /
  ! rr(8)  /
  type(moo_record) :: aa, bb, cc, dd, ee, ff, zz

  ! # Other variables
  integer i, ntot

  ! # Initialization, reads files, opens outputs
  execonf_name = 'innewmarcs'
  call config_init()
  call read_main(config_fn_main)
  if (config_opa) then
    call read_moo(config_fn_moo, recs)
  else
    call read_mod_grid(config_fn_modgrid, recs)
  end if
  if (config_explain) then
    open(unit=UNIT_EXPLAIN, file='innewmarcs_explain.txt', status='replace')
  end if

  !  Extracts glogs, teffs, asalogs
  n = size(recs)
  allocate(glogs(n), teffs(n), asalogs(n))
  do i = 1, n
    glogs(i) = recs(i)%glog
    teffs(i) = recs(i)%teff
    asalogs(i) = recs(i)%asalog
  end do

  ! # Finds and prepares records to use
  call locatab(main_glog, main_teff, main_asalog, glogs, teffs, asalogs, xs, ys, zs, ii)
  ! Separates 8 records
  do i = 1, 8
    rr(i) = recs(ii(i))
! todo cleanup    print *, 'ntot', rr(i)%ntot
  end do
  call align_log_tau_ross(rr)
  ! Converts nh, pe, pg to log
  ntot = rr(1)%ntot
  do i = 1, 8
    rr(i)%nh(1:ntot) = log10(rr(i)%nh(1:ntot))
    rr(i)%pe(1:ntot) = log10(rr(i)%pe(1:ntot))
    rr(i)%pg(1:ntot) = log10(rr(i)%pg(1:ntot))
  end do

  ! # Now the interpolation
  call interpol_two_models(glogs(ii(1)), glogs(ii(2)), main_glog, rr(1), rr(2), aa)
  call interpol_two_models(glogs(ii(3)), glogs(ii(4)), main_glog, rr(3), rr(4), bb)
  call interpol_two_models(glogs(ii(5)), glogs(ii(6)), main_glog, rr(5), rr(6), cc)
  call interpol_two_models(glogs(ii(7)), glogs(ii(8)), main_glog, rr(7), rr(8), dd)
  call interpol_two_models(teffs(ii(1)), teffs(ii(3)), main_teff, aa, bb, ee)
  call interpol_two_models(teffs(ii(5)), teffs(ii(7)), main_teff, cc, dd, ff)
  zz = rr(1)  ! copies scalar information such as ntot, asalalf, nhe, tit, tiabs to result
  ! todo cleanup print *, 'zz ntot', zz%ntot
  call interpol_two_models(asalogs(ii(1)), asalogs(ii(5)), main_asalog, ee, ff, zz)

  ! # Final adjustments
  zz%glog = main_glog
  zz%teff = main_teff
  zz%asalog = main_asalog
  zz%nh(1:ntot) = 10**zz%nh(1:ntot)
  zz%pe(1:ntot) = 10**zz%pe(1:ntot)
  zz%pg(1:ntot) = 10**zz%pg(1:ntot)

  ! # Writes outputs
  ! Writes ".mod" mod, e.g., "modeles.mod"
  call write_modele(config_fn_modeles, zz)
  call log_info('File '''//trim(config_fn_modeles)//''' was successfully created.')
  ! Writes ".opa" file, e.g., "opacities.opa"
  if (config_opa) then
    call write_opa(config_fn_opa, zz)
    call log_info('File '''//trim(config_fn_opa)//''' was successfully created.')
  end if
  if (config_explain) then
    if (config_opa) then
      write(UNIT_EXPLAIN, *) 'innewmarcs grid file: '''//trim(config_fn_moo)//''''
    else
      write(UNIT_EXPLAIN, *) 'innewmarcs grid file: '''//trim(config_fn_modgrid)//''''
    end if
    write(UNIT_EXPLAIN, *) 'innewmarcs output file: '''//trim(config_fn_modeles)//''''
    write(UNIT_EXPLAIN, '(1x, a, 8i4, a)') 'innewmarcs indexes: ''', ii, ''''
    close(UNIT_EXPLAIN)
    call log_info('File ''innewmarcs_explain.txt'' was successfully created.')
  end if
contains
  !=======================================================================================
  ! On cherche les numeros des 4 modeles de la table entre lesquels
  ! le programme devra interpoler. On donne les limites en T et g
  ! des modeles.
  ! Les modeles doivent etre ranges en temperature croissante
  ! a l'interieur de chaque temp les gravites doivent croitre
  !
  ! Outputs are in module variables id11, id12, id21, id22
  !
  ! **Note** This routine has been re-designed to sweep the whole models file to mount the
  ! tables (there was a table hard-coded before)

  subroutine locatab(glog, teff, asalog, glogs, teffs, asalogs, xs, ys, zs, indexes)
    real*8, intent(in) :: glog, teff, asalog, glogs(:), teffs(:), asalogs(:)
    ! (xs, ys, zs) are coordinates (from 1 to ...) of (glog, teff, asalog) respectively
    integer, intent(out) :: xs(8), ys(8), zs(8)
    ! 1D sequential indexes, i.e., valid indexes of glogs/teffs/asalogs
    integer, intent(out) :: indexes(8)
    ! # These variables will be first filled in, then used in calculations
    integer, parameter :: MAX_NG = 20, MAX_NT = 20, MAX_NA = 20
    ! Number of different asalog's
    integer n_asalogs
    ! Number of different teffs for each asalog
    integer n_teffs(MAX_NA)
    ! Number of different glogs for each (teff, asalog)
    integer n_glogs(MAX_NT, MAX_NA)
    ! list of asalog's
    real*8 v_asalogs(MAX_NA)
    ! list of teff's for each asalog
    real*8 v_teffs(MAX_NT, MAX_NA)
    ! list of asalog's
    real*8 v_glogs(MAX_NG, MAX_NT, MAX_NA)
    ! 1D sequential index of each point in the grid
    integer v_indexes(MAX_NG, MAX_NT, MAX_NA)
    ! # Other variables
    integer num_rec, iid, it, ia, ig, i1, i2, n, i
    real*8 last_teff, last_asalog

    num_rec = size(glogs)
    ig = 0
    it = 0
    ia = 0
    last_teff = -9999
    last_asalog = -9999
    do iid = 1, num_rec
      if (last_asalog .ne. asalogs(iid)) then
        ia = ia+1
        it = 1
        ig = 1
        last_asalog = asalogs(iid)
        last_teff = teffs(iid)
        v_asalogs(ia) = asalogs(iid)
        v_teffs(it, ia) = teffs(iid)
        n_teffs(ia) = it
      elseif (last_teff .ne. teffs(iid)) then
        it = it+1
        ig = 1
        last_teff = teffs(iid)
        v_teffs(it, ia) = teffs(iid)
        n_teffs(ia) = it
      else
        ig = ig+1
      end if
      v_glogs(ig, it, ia) = glogs(iid)
      v_indexes(ig, it, ia) = iid
      n_glogs(it, ia) = ig
    end do
    n_asalogs = ia

    call search('asalog', v_asalogs, ia, asalog, i1, i2)
    zs(1:4) = i1
    zs(5:8) = i2

    call search('teff', v_teffs(:,zs(1)), n_teffs(zs(1)), teff, i1, i2)
    ys(1:2) = i1
    ys(3:4) = i2

    call search('teff', v_teffs(:,zs(5)), n_teffs(zs(5)), teff, i1, i2)
    ys(5:6) = i1
    ys(7:8) = i2

    call search('glog', v_glogs(:,ys(1),zs(1)), n_glogs(ys(1),zs(1)), glog, i1, i2)
    xs(1) = i1
    xs(2) = i2

    call search('glog', v_glogs(:,ys(3),zs(3)), n_glogs(ys(3),zs(3)), glog, i1, i2)
    xs(3) = i1
    xs(4) = i2

    call search('glog', v_glogs(:,ys(5),zs(5)), n_glogs(ys(5),zs(5)), glog, i1, i2)
    xs(5) = i1
    xs(6) = i2

    call search('glog', v_glogs(:,ys(7),zs(7)), n_glogs(ys(7),zs(7)), glog, i1, i2)
    xs(7) = i1
    xs(8) = i2

    do i = 1, 8
      iid = v_indexes(xs(i), ys(i), zs(i))
      indexes(i) = iid

      write(lll,1212) i, iid, xs(i), ys(i), zs(i), glogs(iid), teffs(iid), asalogs(iid)
      call log_debug(lll)
      1212 format('#', i1, ': ', i3, ' (', i2, ',', i2, ',', i2, ') -- ', f5.1, f7.1, f5.1)
    end do
  end

  !=======================================================================================
  ! Search routine: finds interval containing x

  subroutine search(name, v, n, x, i1, i2)
    character(*), intent(in) :: name  ! meaning of a/b/x, e.g., "teff", "glog", to figure
                                      ! in log message
    real*8, intent(in) :: v(:) ! vector to search inside
    real*8, intent(in) :: x
    integer, intent(in) :: n  ! valid values within v range from 1 to n
    integer, intent(out) :: i1, i2  ! indexes of boundaries of interval
    integer i

    i1 = 0
    do i = 1, n
      if (abs(x-v(i)) .lt. K_EQ) then
        i1 = i
        i2 = i
        exit
      else if (x .lt. v(i)) then
        if (i .eq. 1) then
          i1 = 1
          i2 = 1
        elseif (x .gt. v(i-1)) then
          i1 = i-1
          i2 = i
        end if
      end if
    end do
    if (i1 .eq. 0) then
      if (x .lt. v(1)) then
        i1 = 1
        i2 = 1
      else
        i1 = n
        i2 = n
      end if
    end if

    call check_interval(name, v(1), v(n), x)
  end

  !=======================================================================================
  ! Auxiliary routine: checks if x is inside [a, b]
  !
  ! If x is outside [a, b], it will log a warning or halt the program,
  ! depending on config_allow

  subroutine check_interval(name, a, b, x)
    real*8, intent(in) :: a, b, x
    character(*), intent(in) :: name ! for logging purposes
    if (a-x .gt. K_EQ .or. x-b .gt. K_EQ) then
      write (lll, *) name//' '//real82str(x, 1)//' is outside interval ['//&
       real82str(a, 1)//', '//real82str(b, 1)//']'
      !print *, "AAAAAAAAAAAAAAAAA", config_allow
      if (config_allow) then
        call log_warning(lll)
      else
        call log_and_halt(lll)
      end if
    end if
  end


  !=======================================================================================
  ! Aligns all atmospheric layer vectors so that the log_tau_ross vector matches exactly
  ! across all models
  !
  ! This implies reducing the number of layers of some models

  subroutine align_log_tau_ross(recs)
    type(moo_record), intent(inout) :: recs(:)
    integer num_rec, i, ishift, j, ntot_min, isup_
    real*8, allocatable :: tos(:)
    real*8 :: to0

    num_rec = size(recs)
    allocate(tos(num_rec))

    ! # finds maximum log_tau_ross(1) across all records
    do i = 1, num_rec
      tos(i) = recs(i)%log_tau_ross(1)
    end do
! todo cleanup   
!    print *, 'TOSSSSSSSSSSSS', tos
    isup_ = isup(tos, num_rec, 1, num_rec)
    to0 = tos(isup_)
!    print *, 'TO000000000000000', to0, isup_

    ntot_min = 9999
    do i = 1, num_rec
      ishift = int((to0-recs(i)%log_tau_ross(1))*10+0.1)
      if(ishift .gt. 0) then
!        print *, ' O SHIFT EH '
!        print *, ' O SHIFT EH '
!        print *, ' O SHIFT EH '
!        print *, ' O SHIFT EH ', ishift
!        print *, ' O SHIFT EH '
!        print *, ' O SHIFT EH '
!        print *, ' O SHIFT EH '
!        print *, ' O SHIFT EH '
        call shift_left(ishift, recs(i)%nh)
        call shift_left(ishift, recs(i)%teta)
        call shift_left(ishift, recs(i)%pe)
        call shift_left(ishift, recs(i)%pg)
        call shift_left(ishift, recs(i)%log_tau_ross)

        if (config_opa) then
          call shift_left(ishift, recs(i)%rad)
          call shift_left(ishift, recs(i)%tau)
          call shift_left(ishift, recs(i)%t)
          call shift_left(ishift, recs(i)%pe)
          call shift_left(ishift, recs(i)%pg)
          call shift_left(ishift, recs(i)%rho)
          call shift_left(ishift, recs(i)%xi)
          call shift_left(ishift, recs(i)%ops)

          do j = 1, recs(i)%nwav
            call shift_left(ishift, recs(i)%abs(j,:))
            call shift_left(ishift, recs(i)%sca(j,:))
          end do
        end if

        recs(i)%ntot = recs(i)%ntot-ishift
      end if

      if (ntot_min .gt. recs(i)%ntot) &
        ntot_min = recs(i)%ntot
    end do

    ! This will force all models to have the same number of layers
    do i = 1, num_rec
      recs(i)%ntot = ntot_min
    end do
  end

  !=======================================================================================
  ! Shifts values of vector to the left

  subroutine shift_left(ishift, v)
    integer, intent(in) :: ishift
    real*8, intent(inout) :: v(:)
    integer n
    n = size(v)
    v(1:n-ishift) = v(ishift+1:n)
  end

  subroutine interpol_two_models(a, b, x, reca, recb, recx)
    real*8, intent(in)  :: a, b         ! boundaries of interval where x is inside
    real*8, intent(in)  :: x            ! point between a and b
    type(moo_record), intent(in)  :: reca, recb ! models corresponding to a and b
    type(moo_record), intent(out) :: recx       ! model corresponding to x (interpolated)

    real*8 :: ka, kb ! Calculated weights for each model
    integer j
    real*8 div

    ! print *, 'rexxxxxxxxxxxxxx ntot', recx%ntot

    if (abs(b-a) .lt. K_EQ) then
      ka = 1
      kb = 0
    else
      div = b-a
      ka = (b-x)/div
      kb = 1-ka
      ! ka = (x-a)/div
      ! kb = (b-x)/div
    end if

    ! todo cleanup print *, "----------------- a=",a,"; b=", b, "; x=", x
    ! print *, "----------------- ka=",ka,"; kb=", kb


    call interpol(ka, kb, reca%nh, recb%nh, recx%nh)
    call interpol(ka, kb, reca%teta, recb%teta, recx%teta)
    call interpol(ka, kb, reca%pe, recb%pe, recx%pe)
    call interpol(ka, kb, reca%pg, recb%pg, recx%pg)
    ! call interpol(ka, kb, reca%log_tau_ross, recb%log_tau_ross, recx%log_tau_ross)

    if (config_opa) then
      call interpol(ka, kb, reca%rad, recb%rad, recx%rad)
      call interpol(ka, kb, reca%tau, recb%tau, recx%tau)
      call interpol(ka, kb, reca%t, recb%t, recx%t)
      call interpol(ka, kb, reca%pe, recb%pe, recx%pe)
      call interpol(ka, kb, reca%pg, recb%pg, recx%pg)
      call interpol(ka, kb, reca%rho, recb%rho, recx%rho)
      call interpol(ka, kb, reca%xi, recb%xi, recx%xi)
      call interpol(ka, kb, reca%ops, recb%ops, recx%ops)

      do j = 1, reca%nwav
        call interpol(ka, kb, reca%abs(j,:), recb%abs(j,:), recx%abs(j,:))
        call interpol(ka, kb, reca%sca(j,:), recb%sca(j,:), recx%sca(j,:))
      end do
    end if

    ! print *, 'rexxxxxxxxxxxxxx ntot', recx%ntot

  end

  !=======================================================================================
  ! Linear interpolation for the five vectors: nh, teta, pe, pg, log_tau_ross

  subroutine interpol(ka, kb, va, vb, vx)
    real*8, intent(in)  :: ka, kb       ! weights for va and vb respectively
    real*8, intent(in)  :: va(:), vb(:) ! input vectors
    real*8, intent(out) :: vx(:)        ! output vector
    if (ka .eq. 1) then
      vx = va
      return
    end if
    vx = va*ka+vb*kb
  end

end

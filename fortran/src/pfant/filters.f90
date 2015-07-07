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

!> Contains subroutines filter_molecules_() and filter_atomgrade_()
!>
!> Variables calculated have prefixes km_f_ or atomgrade_f_

module filters
  use molecules_ids
  use max_


  !=====
  ! km_f_*Variables filled by filter_molecules()
  !=====

  integer km_f_mblend  ! Total number of spectral lines *filtered in*

  ! Valid elements of these are from 1 to km_f_mblend
  real*8, dimension(MAX_KM_F_MBLEND) :: &
    km_f_lmbdam, & !< ?doc?
    km_f_sj,     & !< ?doc?
    km_f_jj,     & !< ?doc?
    km_f_mm        !< Replicates km_f_mm(molid) for all selected lines of molecule molid.
                 !< Redundant information but simplifies use. Used in synthesis::selekfh()

  !------
  ! These two arrays contain indexes pointing at km_LMBDAM, km_SJ, and km_JJ
  !------

  !> This one points to the last index of the lines of each molecule within
  !> km_f_lmbdam, km_f_sj and km_f_jj (after the filtering)
  !>
  !> Update: **augmented!** -- first element is 0 (ZERO) -- facilitates the algorithm
  dimension km_f_mblenq(NUM_MOL+1)
  !> This is similar but is a "local" one, it contains index of the last
  !> line of each set of lines within km_f_lmbdam, km_f_sj and km_f_jj
  !> **for the current molecule** I_MOL
  !>
  !> Update: **augmented!** -- first row is 0 (ZERO) -- facilitates the algorithm
  !>
  !> @todo Explain better
  dimension km_f_ln(MAX_SOL_PER_MOL+1, NUM_MOL)


  !=====
  ! atomgrade_f_*Variables filled by filter_atomgrade()
  !=====

  ! infile:atomgrade, filtered variables
  ! Very similar to above; differences are
  ! - single underscore
  ! - additional variable "gf", which equals 10**algf
  integer atomgrade_f_nblend !< ?doc?
  character*2 atomgrade_f_elem(MAX_ATOMGRADE_NBLEND) !< ?doc?
  integer, dimension(MAX_ATOMGRADE_NBLEND) :: &
   atomgrade_f_ioni !< ?doc?
  real*8, dimension(MAX_ATOMGRADE_R_NBLEND) :: &
   atomgrade_f_lambda,       & !< ?doc?
   atomgrade_f_kiex,         & !< ?doc?
   atomgrade_f_algf,         & !< ?doc?
   atomgrade_f_ch,           & !< ?doc?
   atomgrade_f_gr,           & !< ?doc?
   atomgrade_f_ge,           & !< ?doc?
   atomgrade_f_zinf,         & !< ?doc?
   atomgrade_f_abondr_dummy, & !< ?doc?
   atomgrade_f_gf,           & !< ?doc?
   atomgrade_f_abonds_abo      !< ?doc?

contains

  !=======================================================================================
  !> Sweeps km_r_* to populate a few km_* depending on the interval LZERO-LFIN

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
            i_line,         &  ! Index of km_r_lmbdam, km_r_sj, km_r_jj
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
    do molid = 1, km_r_number
      if (.not. molecule_is_on(molid)) cycle

      i_mol = i_mol+1

      !#logging
      write(lll, *) 'molecule id', molid, ': ',  km_r_titulo(molid)
      call log_debug(lll)
      write(lll, *) 'number of prospective lambdas ------>', km_r_lines_per_mol(molid)
      call log_debug(lll)


      ! Counters starting with "J_" restart at each molecule
      j_set = 1   ! Current "set-of-lines"
      flag_in = .FALSE.  ! Whether has filtered in at least one line
      do j_dummy = 1, km_r_lines_per_mol(molid)
        lambda = km_r_lmbdam(i_line)

        if ((lambda .ge. lzero) .and. (lambda .le. lfin)) then
          ! Filters in a new spectral line!
          i_filtered = i_filtered+1

          !#spill_check
          if (i_filtered .gt. MAX_KM_F_MBLEND) then
            call pfant_halt('filter_molecules(): number of filtered lines '//&
             'exceeded maximum of MAX_KM_F_MBLEND='//int2str(MAX_KM_F_MBLEND), .true.)
          end if



          km_f_lmbdam(i_filtered) = lambda
          km_f_sj(i_filtered) = km_r_sj(i_line)
          km_f_jj(i_filtered) = km_r_jj(i_line)

          km_f_mm(i_filtered) = km_r_mm(molid)

          flag_in = .true.

        end if

        if (i_line .eq. km_r_iollosol(j_set, molid)) then
          ! Reached last line of current set of lines


!           !> @todo ISSUE Should we think about preparing it for not having a single line within LZERO-LFIN for set J_SET, J_SET=1,NNV?????
!           IF (.NOT. FLAG_IN) THEN
!             !> @todo, IDEA Actually I think that it might work without having lines within a given lambda range, because the routines that use the calculations just don't care which molecule it is
!             !> @todo but I can give a *WARNING*, more for testing than for anything else, actually
!
!             !--error checking--!
!             !> @todo test this error
!             WRITE (*, *) 'FILTER_molecules(): Molecule ID ',MOLID,
!    +            ' titled  "', km_r_TITULO(MOLID), '"'
!             WRITE (*, *) 'Set of lines ', (J_SET), 'has no lambda '
!    +            //'within ', LZERO, ' <= lambda <= ', LFIN
!             WRITE (*, *) 'The algorithm is not prepared for this, '
!    +            //'sorry!'
!             STOP ERROR_BAD_VALUE
!           END IF

          km_f_ln(j_set+1, i_mol) = i_filtered  ! Yes, J_SET+1, not J_SET, remember km_LN first row is all ZEROes.
          j_set = j_set+1
        end if

        i_line = i_line+1
      end do

      km_f_mblenq(i_mol+1) = i_filtered  ! Yes, I_MOL+1, not I_MOL, remember km_MBLENQ(1) is ZERO.
    end do !--end of MOLID loop--!

    km_f_mblend = i_filtered
  end


  !=======================================================================================
  !> Selects only spectral lines within range lzero, lfin + performs "inner join".
  !>
  !> Populates variables atomgrade_f_*

  subroutine filter_atomgrade(lzero, lfin)
    !> Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    !> Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    integer j, k

    k = 0
    do j = 1, atomgrade_r_nblend
      if((atomgrade_r_lambda(j).le.lfin) .and. (atomgrade_r_lambda(j) .ge. lzero)) then
        k = k+1


        !#spill_check: checks if exceeds maximum number of elements allowed
        if (k .gt. MAX_ATOMGRADE_NBLEND) then
          call pfant_halt('filter_atomgrade(): exceeded maximum of MAX_ATOMGRADE_NBLEND='//&
           int2str(MAX_ATOMGRADE_NBLEND)//' spectral lines')
        end if

        !Filters in
        atomgrade_f_elem(k)   = atomgrade_r_elem(j)
        atomgrade_f_ioni(k)   = atomgrade_r_ioni(j)
        atomgrade_f_lambda(k) = atomgrade_r_lambda(j)
        atomgrade_f_kiex(k)   = atomgrade_r_kiex(j)
        atomgrade_f_algf(k)   = atomgrade_r_algf(j)
        atomgrade_f_gf(k)     = 10.**atomgrade_r_algf(j)
        atomgrade_f_ch(k)     = atomgrade_r_ch(j)
        atomgrade_f_gr(k)     = atomgrade_r_gr(j)
        atomgrade_f_ge(k)     = atomgrade_r_ge(j)
        atomgrade_f_zinf(k)   = atomgrade_r_zinf(j)

        atomgrade_f_abonds_abo(k) = atomgrade_r_abonds_abo(j)

        atomgrade_f_abondr_dummy(k) = atomgrade_r_abondr_dummy(j)

      end if
    end do

    atomgrade_f_nblend = k
  end

end

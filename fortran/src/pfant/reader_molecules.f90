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

!> Reading routines and variable declarations for infile:molecules

module reader_molecules
  use logging
  use max_
  use misc
  use molecules_ids
  implicit none


  ! Specifies how many molecules to read
  integer km_r_number

  integer km_r_lines_total  ! Total number of spectral line, counting all molecules

  character*80 km_r_titm, km_r_titulo

  dimension km_r_titulo(NUM_MOL)

  real*8, dimension(NUM_MOL) :: km_r_fe, km_r_do, &
   km_r_mm, km_r_am, km_r_bm, km_r_ua, km_r_ub, km_r_te, km_r_cro, &
   km_r_a0, km_r_a1, km_r_a2, km_r_a3, km_r_a4, km_r_als, km_r_s

  integer, dimension(NUM_MOL)  :: km_r_ise, km_r_nv, &
   km_r_lines_per_mol  !> This stores the number of spectral lines for each molecule

  real*8, dimension(MAX_SOL_PER_MOL, NUM_MOL) :: km_r_qqv, km_r_ggv, km_r_bbv, km_r_ddv, km_r_fact


  !> "Index of Last Lambda Of Set-Of-Lines"
  !> Points to km_r_lmbdam, km_r_sj, km_r_jj; km_r_last
  !> This is mounted at reading to help with the filtering and avoid
  !> allocating 2 dimensions for (lm__lmbdam, km_r_sj, km_r_jj)
  real*8, dimension(MAX_SOL_PER_MOL, NUM_MOL) :: km_r_iollosol

  real*8,  dimension(MAX_KM_R_LINES_TOTAL) :: &
   km_r_lmbdam, &
   km_r_sj,     &
   km_r_jj

contains
  !=======================================================================================
  !> Reads file infile:molecules to fill variables km_r_*
  !>
  !> Reads molecular lines
  !>
  !> Note: The whole file is read into memory, independent of which molecules are "switched on".
  !>       There is not much of a point in skipping molecules here, because the space is already
  !>       pre-allocated, and I have to read the whole file anyway, so it is much easier
  !>       programming-wise (and not time-costly either) to filter
  !>       molecules in filter_molecules() when they are already in memory.
  !>
  !> @todo TOP PRIORITY Number of molecules ON: consider km_r_number; search for km_r_number; also wrong references to NUM_MOL, search for NUM_MOL

  subroutine read_molecules(filename)
    character(len=*) :: filename

    integer unit_, i, &
     molid,   &  ! Old "NMOL", index/ID of molecule, ranges from 1 to NUM_MOL
     i_line,  &  ! Counts lines within each molecule (reset at each new molecule)
     nnv, iz, &
     numlin , &  ! Temporary variable
     j_set,   &
     j_line
    parameter(unit_=199)

    open(unit=unit_,file=filename, status='old')


    !#logging
    write (lll,*) 'MAX_KM_R_LINES_TOTAL = ', MAX_KM_R_LINES_TOTAL
    call log_debug(lll)


    ! row 01:
    ! BLB: NUMBER -- number of molecules do be considered
    ! Note: This is no longer used for anything, now the molecules to be switched on/off are configured

    read(unit_,*) km_r_number

    !#spill_check
    if (km_r_number .gt. NUM_MOL) then
      call pfant_halt("Number of molecules ("//int2str(km_r_number)// &
       ") exceeds maximum allowed ("//int2str(NUM_MOL)//")")
    end if

    ! Deactivates molecules not wanted or not present in the file
    do molid = km_r_number+1, NUM_MOL
      call add_molid_off(molid)
    end do


    ! row 02: string containing list of names of all molecules
    read(unit_,'(a)') km_r_titm
    !~READ(UNIT_,'(20A4)') km_r_TITM

    !#logging
    write(lll, *) 'titm--------------', km_r_titm
    call log_debug(lll)

    ! BLB:
    ! BLB: km_r_NV -- number of transitions (v', v'') for each molecule
    ! BLB: Example: if (0,0)(1,1)(2,2) are considered for CH
    ! BLB:             (1,1)(2,2) are considered for CN
    ! BLB:             NV(J) = 3 2
    read(unit_,*) (km_r_nv(molid), molid=1,km_r_number)

    !#spill_check
    do molid = 1, km_r_number
      if (km_r_nv(molid) .gt. MAX_SOL_PER_MOL) then
          call pfant_halt('read_molecules(): molecule id '//int2str(molid)//&
           ' has nv = '//int2str(km_r_nv(molid))//' (maximum is MAX_SOL_PER_MOL='//&
           int2str(MAX_SOL_PER_MOL)//')')
        end if
    end do

    i_line = 0
    do molid = 1, km_r_number

      !> @todo check spill in each element in km_r_NV

      ! BLB:
      ! BLB: title -- specifying the molecule to follow
      ! BLB:          format: 20A4
      read(unit_,'(a)') km_r_titulo(molid)

      !#logging
      write(lll,*) 'molecule id ', molid
      call log_debug(lll)
      write(lll,*) 'titulo:  ', km_r_titulo(molid)
      call log_debug(lll)

      ! BLB: FE, DO, MM, AM, BM, UA, UB, Te, CRO
      ! BLB: Format: free
      ! BLB: FE -- molecular oscillator strength fel
      ! BLB: DO -- dissociation constant (eV)
      ! BLB:    | MM -- [mass of A + mass of B] for molecule AB
      ! BLB:  +-| AM -- mass of A
      ! BLB:  | | BM -- mass of B
      ! BLB:  |
      ! BLB:  +---> where (12)C = 12, H = 1.008
      ! BLB:
      ! BLB: UA -- value of partition function for element A
      ! BLB: UB -- value of partition function for element B
      ! BLB: TE -- eletronic term.
      ! BLB: CRO - delta Kronecker (2-delta_{Sigma, 0})
      ! BLB:       delta_{Sigma, 0} = 0 for Sigma transitions
      ! BLB:                          1 for non-Sigma transitions

      read(unit_,*) km_r_fe(molid), km_r_do(molid), km_r_mm(molid), &
       km_r_am(molid), km_r_bm(molid), km_r_ua(molid), &
       km_r_ub(molid), km_r_te(molid), km_r_cro(molid)


      !> @todo ISSUE Documentation
      !> @todo ISSUE !P! My sample file is blank here
      read(unit_,'(2x,i3, 5f10.6, 10x, f6.3)') km_r_ise(molid), &
       km_r_a0(molid), km_r_a1(molid), km_r_a2(molid), &
       km_r_a3(molid), km_r_a4(molid), km_r_als(molid)

      !> @todo issue ?what? ?doc? is S??
      read(unit_,*) km_r_s(molid)

      nnv = km_r_nv(molid)

      !#logging
      write(lll,*) 'nv=', nnv
      call log_debug(lll)

      !> @todo type in documentation
      read(unit_,*) (km_r_qqv(i, molid), i=1,nnv)
      read(unit_,*) (km_r_ggv(i, molid), i=1,nnv)
      read(unit_,*) (km_r_bbv(i, molid), i=1,nnv)
      read(unit_,*) (km_r_ddv(i, molid), i=1,nnv)
      read(unit_,*) (km_r_fact(i, molid),i=1,nnv)

      do i = 1,nnv
        km_r_ddv(i, molid)=1.e-6*km_r_ddv(i, molid)
      end do


      !~L = (MOLID-1)*MAX_LINES_PER_MOL+1  ! Initial index for all vectors inside this loop
      j_set = 0
      j_line = 0 ! Counts how many lines per molecule and stores.
      do while (.true.)
        i_line = i_line+1

        !#spill_check: checks if exceeds maximum number of elements allowed
        if (i_line .gt. MAX_KM_R_LINES_TOTAL) then
          call pfant_halt('read_molecules(): exceeded maximum number of total '//&
            'spectral lines  MAX_KM_R_LINES_TOTAL= '//int2str(MAX_KM_R_LINES_TOTAL)//&
            ' (at molecule id '//int2str(molid)//')')
        end if

        ! BLB: LMBDAM(L), SJ(L), JJ(L), IZ, ITRANS(L), NUMLIN
        ! BLB: Format: free
        ! BLB:
        ! BLB: LMBDAM -- wavelength in angstron
        ! BLB: Sj -- H�n-London factor calculated such that sum(S_j/(2*j+1)) = 1 ISSUE: H�n-London?
        ! BLB: JJ -- rotational quantum number
        ! BLB: IZ -- branch as table:  ISSUE not used
        ! BLB:       P  -  1
        ! BLB:       Q  -  2
        ! BLB:       R  -  3
        ! BLB:       P1 -  4
        ! BLB:       Q1 -  5
        ! BLB:       R1 -  6
        ! BLB:       P2 -  7
        ! BLB:       Q2 -  8
        ! BLB:       R2 -  9
        ! BLB:       P3 - 10
        ! BLB:       Q3 - 11
        ! BLB:       R3 - 12
        ! BLB: ITRANS -- key to indicate which is the (v',v'') -- only used in isotropic calculations ISSUE: !P! missing from sample file infile:molecules
        !> @todo ISSUE (question) Where does infile:molecules come from?
        ! BLB: NUMLIN -- key as table:
        ! BLB:           = 1 for the last line of the first (v',v'') set of lines
        ! BLB:           = 2 for the last line of the second (v', v'') set of lines  ISSUE never used
        ! BLB:           .
        ! BLB:           . (etc)  ISSUE never used
        ! BLB:           .
        ! BLB:
        ! BLB:           = 9 for the last line of the last (v', v'') set of lines of a certain molecule
        read(unit_,*) km_r_lmbdam(i_line), km_r_sj(i_line), km_r_jj(i_line), iz, numlin

        !~km_r_NUMLIN(J_LAMBDA, MOLID) = NUMLIN

        if (numlin .ne. 0) then
          j_set = j_set+1
          km_r_iollosol(j_set, molid) = i_line
        end if

        j_line = j_line+1

        if (numlin .eq. 9) exit
      end do

      !__consistency check__: J_SET must match NNV
      if(j_set .ne. nnv) then
        call pfant_halt('read_molecules():  incorrect number of set-of-lines: '//&
         int2str(j_set)//' (should be '//int2str(nnv)//') (in molecule number '//&
         int2str(molid)//')')
      end if

      km_r_lines_per_mol(molid) = j_line

      !#logging
      write(lll,*) 'This molecule has ', j_line, ' lines'
      call log_debug(lll)
    end do

    km_r_lines_total = i_line

    close(unit_)
  end
end

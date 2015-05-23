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
!> This module corresponds to 2015- subroutine KAPMOL, which was split in three parts.
!>
!> - READ_MOLECULAGRADE: Reads file
!> - FILTER_MOLECULAGRADE: Selects spectral lines within LZERO-LFIN lambda interval
!> - USE_MOLECULAGRADE: performs the calculations using the selected spectral lines

module molecula
  use read_files
  use config
  use dissoc

  !~integer, parameter :: max_lines_per_mol=300000
  ! Old "NTR"; Maximum number of transitions ("Set-Of-Lines") for each molecule

  integer, parameter :: MAX_SOL_PER_MOL=200

  !~ integer, parameter ::
  !~+  MAX_KM__LINES_TOTAL = max_lines_per_mol*NUM_MOL
  integer, parameter :: &
   MAX_KM__LINES_TOTAL=1400000, & !< Maximum number of spectral lines in moleculagrade.dat
                                  !< pertaining all molecules
   MAX_KM_MBLEND=200000 !< Maximum number of spectral lines that can be filtered in at a
                        !< filtering operation performed by filter_moleculagrade()


  ! Specifies how many molecules to read
  !> @todo ISSUE: According to EC, 16-21 are hydrogen lines which are used somewhere else, gotta check this, it is taking 7 seconds to read the whole file
  ! (MT) Gonna ask BLB this
  integer km__number

  integer km__lines_total  ! Total number of spectral line, counting all molecules

  character*256 km__titm, km__titulo

  dimension km__titulo(NUM_MOL)

  real*8, dimension(NUM_MOL) :: km__fe, km__do, &
   km__mm, km__am, km__bm, km__ua, km__ub, km__te, km__cro, &
   km__a0, km__a1, km__a2, km__a3, km__a4, km__als, km__s

  integer, dimension(NUM_MOL)  :: km__ise, km__nv, &
   km__lines_per_mol  !> This stores the number of spectral lines for each molecule

  real*8, dimension(MAX_SOL_PER_MOL, NUM_MOL) :: km__qqv, km__ggv, km__bbv, km__ddv, km__fact


  !> "Index of Last Lambda Of Set-Of-Lines"
  !> Points to km__lmbdam, km__sj, km__jj; km__last
  !> This is mounted at reading to help with the filtering and avoid
  !> allocating 2 dimensions for (lm__lmbdam, km__sj, km__jj)
  real*8, dimension(MAX_SOL_PER_MOL, NUM_MOL) :: km__iollosol

  real*8,  dimension(MAX_KM__LINES_TOTAL) :: &
   km__lmbdam, &
   km__sj,     &
   km__jj

  !~ integer, dimension(max_lines_per_mol, NUM_MOL) ::
  !~+  km__numlin


  !=====
  ! Variables filled by filter_moleculagrade()
  !=====

  integer km_mblend  ! Total number of spectral lines *filtered in*

  ! Valid elements of these are from 1 to km_mblend
  real*8, dimension(MAX_KM_MBLEND) :: &
    km_lmbdam, & !< ?doc?
    km_sj,     & !< ?doc?
    km_jj,     & !< ?doc?
    km_gfm,    & !< ?doc?
    km_alargm    !< ?doc?


  !------
  ! These two arrays contain indexes pointing at km_LMBDAM, km_SJ, and km_JJ
  !------
  ! This one points to the last index of the lines of each molecule within
  ! km_lmbdam, km_sj and km_jj (after the filtering)
  ! Update: **augmented!** -- first element is 0 (ZERO) -- facilitates the algorithm
  dimension km_mblenq(NUM_MOL+1)
  ! This is similar but is a "local" one, it contains index of the last
  ! line of each set of lines within km_lmbdam, km_sj and km_jj
  ! **for the current molecule** I_MOL
  ! Update: **augmented!** -- first row is 0 (ZERO) -- facilitates the algorithm
  !> @todo Explain better
  dimension km_ln(MAX_SOL_PER_MOL+1, NUM_MOL)

  !> @todo ISSUE: "Warning: possible change of value in conversion from real(8) to real(4)"
  real*8, dimension(MAX_KM__LINES_TOTAL, MAX_MODELES_NTOT) :: km_pnvj

  !> @todo test the pointers
  real*8, private, pointer, dimension(:) :: ppa, pb

  private point_ppa_pb

  save
contains



  !================================================================================================================================
  !> Reads file moleculagrade.dat to fill variables km_*
  !>
  !> Reads molecular lines
  !>
  !> Note: The whole file is read into memory, independent of which molecules are "switched on".
  !>       There is not much of a point in skipping molecules here, because the space is already
  !>       pre-allocated, and I have to read the whole file anyway, so it is much easier
  !>       programming-wise (and not time-costly either) to filter
  !>       molecules in FILTER_MOLECULAGRADE() when they are already in memory.
  !>

  subroutine read_moleculagrade(filename)
    character(len=*) :: filename

    integer unit_, i, &
     molid,   &  ! Old "NMOL", index/ID of molecule, ranges from 1 to NUM_MOL
     i_line,  &  ! Counts lines within each molecule (reset at each new molecule)
     nnv, iz, &
     numlin , &  ! Temporary variable
     j_set,   &
     j_line
    parameter(unit_=199)
    character*128 lll

    open(unit=unit_,file=filename, status='old')


    !__logging__
    write (lll,*) 'MAX_KM__LINES_TOTAL = ', MAX_KM__LINES_TOTAL
    call log_debug(lll)


    ! BLB: NUMBER -- number of molecules do be considered
    ! Note: This is no longer used for anything, now the molecules to be switched on/off are configured

    read(unit_,*) km__number

    read(unit_,'(a)') km__titm
    !~READ(UNIT_,'(20A4)') km__TITM

    !__logging__
    write(lll, *) 'titm--------------', km__titm
    call log_debug(lll)

    ! BLB:
    ! BLB: km__NV -- number of transitions (v', v'') for each molecule
    ! BLB: Example: if (0,0)(1,1)(2,2) are considered for CH
    ! BLB:             (1,1)(2,2) are considered for CN
    ! BLB:             NV(J) = 3 2
    read(unit_,*) (km__nv(molid), molid=1,NUM_MOL)

    !__spill check__
    do molid = 1, NUM_MOL
      if (km__nv(molid) .gt. MAX_SOL_PER_MOL) then
          write(lll,*) 'read_moleculagrade(): molecule id ', molid, &
           ' has nv = ', km__nv(molid), ' (maximum is ', MAX_SOL_PER_MOL, ')'
          call pfant_halt(lll)
        end if
    end do

    i_line = 0
    do molid = 1, km__number

      !> @todo check spill in each element in km__NV

      ! BLB:
      ! BLB: title -- specifying the molecule to follow
      ! BLB:          format: 20A4
      read(unit_,'(a)') km__titulo(molid)

      !__logging__
      write(lll,*) 'molecule id ', molid
      call log_debug(lll)
      write(lll,*) 'titulo:  ', km__titulo(molid)
      call log_debug(lll)

      ! BLB: FE, DO, MM, AM, BM, UA, UB, Te, CRO
      ! BLB: Format: free
      ! BLB: FE -- molecular oscillator strength fel
      ! BLB: DO -- dissociation constant (eV)
      ! BLB:    | MM -- [mass of A + mass of B] for molecula AB
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

      read(unit_,*) km__fe(molid), km__do(molid), km__mm(molid), &
       km__am(molid), km__bm(molid), km__ua(molid), &
       km__ub(molid), km__te(molid), km__cro(molid)


      !> @todo ISSUE Documentation
      !> @todo ISSUE !P! My sample file is blank here
      read(unit_,'(2x,i3, 5f10.6, 10x, f6.3)') km__ise(molid), &
       km__a0(molid), km__a1(molid), km__a2(molid), &
       km__a3(molid), km__a4(molid), km__als(molid)

      !> @todo issue ?what? ?doc? is S??
      read(unit_,*) km__s(molid)

      nnv = km__nv(molid)

      !__logging__
      write(lll,*) 'nv=', nnv
      call log_debug(lll)

      !> @todo type in documentation
      read(unit_,*) (km__qqv(i, molid), i=1,nnv)
      read(unit_,*) (km__ggv(i, molid), i=1,nnv)
      read(unit_,*) (km__bbv(i, molid), i=1,nnv)
      read(unit_,*) (km__ddv(i, molid), i=1,nnv)
      read(unit_,*) (km__fact(i, molid),i=1,nnv)

      do i = 1,nnv
        km__ddv(i, molid)=1.e-6*km__ddv(i, molid)
      end do


      !~L = (MOLID-1)*MAX_LINES_PER_MOL+1  ! Initial index for all vectors inside this loop
      j_set = 0
      j_line = 0 ! Counts how many lines per molecule and stores.
      do while (.true.)
        i_line = i_line+1

        !__spill check__: checks if exceeds maximum number of elements allowed
        !> @todo ISSUE This wasn't being checked and I got an error when I tried to include all the 21 molecules
        if (i_line .gt. MAX_KM__LINES_TOTAL) then
          write(lll,*) 'read_moleculagrade(): exceeded maximum number of spectral lines total = ', &
           MAX_KM__LINES_TOTAL, ' (at molecule id ', molid, ')'
          call pfant_halt(lll)
        end if

        ! BLB: LMBDAM(L), SJ(L), JJ(L), IZ, ITRANS(L), NUMLIN
        ! BLB: Format: free
        ! BLB:
        ! BLB: LMBDAM -- wavelength in angstron
        ! BLB: Sj -- Hön-London factor calculated such that sum(S_j/(2*j+1)) = 1 ISSUE: Hön-London?
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
        ! BLB: ITRANS -- key to indicate which is the (v',v'') -- only used in isotropic calculations ISSUE: !P! missing from sample file moleculagrade.dat
        !> @todo ISSUE (question) Where does moleculagrade.dat come from?
        ! BLB: NUMLIN -- key as table:
        ! BLB:           = 1 for the last line of the first (v',v'') set of lines
        ! BLB:           = 2 for the last line of the second (v', v'') set of lines  ISSUE never used
        ! BLB:           .
        ! BLB:           . (etc)  ISSUE never used
        ! BLB:           .
        ! BLB:
        ! BLB:           = 9 for the last line of the last (v', v'') set of lines of a certain molecula
        read(unit_,*) km__lmbdam(i_line), km__sj(i_line), km__jj(i_line), iz, numlin

        !~km__NUMLIN(J_LAMBDA, MOLID) = NUMLIN

        if (numlin .ne. 0) then
          j_set = j_set+1
          km__iollosol(j_set, molid) = i_line
        end if

        j_line = j_line+1

        if (numlin .eq. 9) exit
      end do

      !__consistency check__: J_SET must match NNV
      if(j_set .ne. nnv) then
        write(lll,*) 'read_moleculagrade():  incorrect number of set-of-lines: ', j_set, &
         '(should be ', nnv, ')  (in molecule number ', molid, ')'
        call pfant_halt(lll)
      end if

      km__lines_per_mol(molid) = j_line

      !__logging__
      write(lll,*) 'This molecule has ', j_line, ' lines'
      call log_debug(lll)
    end do

    km__lines_total = i_line

    close(unit_)
  end



  !================================================================================================================================
  !> @ingroup gr_filter
  !> Sweeps km__* to populate a few km_* depending on the interval LZERO-LFIN

  subroutine filter_moleculagrade(lzero, lfin)
    !> Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    !> Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    real*8 :: lambda
    integer i, &
            molid,          &  ! Counts molecule id, from 1 to NUM_MOL
            i_mol,          &  ! Counts molecules that are "switched on"
            j_dummy, j_set, &
            i_line,         &  ! Index of km__lmbdam, km__sj, km__jj
            i_filtered         ! Counts number of filtered lines (molecule-independent);
                               !  index of km_lmbdam, km_sj, km_jj
    logical flag_in
    character*100 lll


    !__logging__
    write(lll, *) 'config_num_mol_on = ', config_num_mol_on
    call log_debug(lll)


    ! Initializes the zero elements of the augmented matrices
    km_mblenq(1) = 0
    do i = 1, num_mol
      km_ln(1, i) = 0
    end do

    i_filtered = 0  ! Current *filtered-in* spectral line. Keeps growing (not reset when the molecule changes). Related to old "L"
    i_line = 1
    i_mol = 0
    do molid = 1, num_mol
      if (.not. molecule_is_on(molid)) cycle

      i_mol = i_mol+1

      !__logging__
      write(lll, *) 'molecule id', molid, ': ',  km__titulo(molid)
      call log_debug(lll)
      write(lll, *) 'number of prospective lambdas ------>', km__lines_per_mol(molid)
      call log_debug(lll)


      ! Counters starting with "J_" restart at each molecule
      J_SET = 1   ! Current "set-of-lines"
      FLAG_IN = .FALSE.  ! Whether has filtered in at least one line
      DO J_DUMMY = 1, km__LINES_PER_MOL(MOLID)
        LAMBDA = km__LMBDAM(I_LINE)

        if ((lambda .ge. lzero) .and. (lambda .le. lfin)) then
          ! Filters in a new spectral line!
          i_filtered = i_filtered+1

          !__spill check__
          if (i_filtered .gt. max_km_mblend) then
            write(lll, *) 'filter_moleculagrade(): number of filtered lines '//&
             'exceeded maximum of ', max_km_mblend
            call pfant_halt(lll, .true.)
          end if



          km_lmbdam(i_filtered) = lambda
          km_sj(i_filtered) = km__sj(i_line)
          km_jj(i_filtered) = km__jj(i_line)

          flag_in = .true.

        end if

        if (i_line .eq. km__iollosol(j_set, molid)) then
          ! Reached last line of current set of lines


!           !> @todo ISSUE Should we think about preparing it for not having a single line within LZERO-LFIN for set J_SET, J_SET=1,NNV?????
!           IF (.NOT. FLAG_IN) THEN
!             !> @todo, IDEA Actually I think that it might work without having lines within a given lambda range, because the routines that use the calculations just don't care which molecule it is
!             !> @todo but I can give a *WARNING*, more for testing than for anything else, actually
!
!             !--error checking--!
!             !> @todo test this error
!             WRITE (*, *) 'FILTER_MOLECULAGRADE(): Molecule ID ',MOLID,
!    +            ' titled  "', km__TITULO(MOLID), '"'
!             WRITE (*, *) 'Set of lines ', (J_SET), 'has no lambda '
!    +            //'within ', LZERO, ' <= lambda <= ', LFIN
!             WRITE (*, *) 'The algorithm is not prepared for this, '
!    +            //'sorry!'
!             STOP ERROR_BAD_VALUE
!           END IF

          km_ln(j_set+1, i_mol) = i_filtered  ! Yes, J_SET+1, not J_SET, remember km_LN first row is all ZEROes.
          j_set = j_set+1
        end if

        i_line = i_line+1
      end do

      km_mblenq(i_mol+1) = i_filtered  ! Yes, I_MOL+1, not I_MOL, remember km_MBLENQ(1) is ZERO.
    end do !--end of MOLID loop--!

    km_mblend = i_filtered
  end







!================================================================================================================================
!> Calculates the molecular absorption coefficient.
!>
!> Uses km_* filled by FILTER_MOLECULAGRADE()

  subroutine use_moleculagrade()
    real*8 t5040, psi
    real*8 csc
    real*8 fe, do_, mm, am, bm, ua, ub, te, cro, rm
    real*8 qv, gv, bv, dv, facto
    integer i_mol, j_set, l, l_ini, l_fin, n, nnv, molid
    character*80 lll

    real*8, parameter :: H  = 6.6252E-27,   &
                         C  = 2.997929E+10, &
                         KB = 1.38046E-16,  &
                         C2 = 8.8525E-13

    do i_mol = 1, config_num_mol_on
      molid = get_molid(i_mol)

      !__logging__
      write(lll, *) 'molecule id ', molid, ': ', km__titulo(molid)
      call log_debug(lll)

      call point_ppa_pb(molid)

      nnv = km__nv(molid)

      fe  = km__fe(molid)
      do_ = km__do(molid)
      mm  = km__mm(molid)
      am  = km__am(molid)
      bm  = km__bm(molid)
      ua  = km__ua(molid)
      ub  = km__ub(molid)
      te  = km__te(molid)
      cro = km__cro(molid)


      !======
      ! This part of the code calculates km_PNVL
      rm = am*bm/mm
      do n = 1,modeles_ntot
        t5040 = modeles_teta(n)/5040
        psi = do_*modeles_teta(n)+2.5*log10(modeles_teta(n))-1.5*log10(rm)-&
              log10(ua*ub)-13.670
        psi = 10.**psi

        do j_set = 1,nnv
          qv = km__qqv(j_set, molid)
          gv = km__ggv(j_set, molid)
          bv = km__bbv(j_set, molid)
          dv = km__ddv(j_set, molid)

          l_ini = km_ln(j_set, molid)+1
          l_fin = km_ln(j_set+1, molid)

          ! l is index within km_lmbdam, km_sj and km_jj
          do l= l_ini, l_fin
            ! PC2003: default value for CSC does not exist physically
            csc = exp(-H*C/KB*modeles_teta(n)/5040.*(te+gv+bv*(km_jj(l)+1)*km_jj(l)))*   &
                  (2.-cro)*(2.*km_jj(l)+1.)*                                             &
                  exp(H*C/KB*modeles_teta(n)/5040.*(dv*(km_jj(l)*(km_jj(l)+1))**2+2.*bv))

            km_PNVJ(L,N) = CSC*PSI*PPA(N)*PB(N)/sat4_PPH(N)
          end do


          ! Takes advantage of current j_set loop so it is not necessary to create
          ! another double loop as in the original KAPMOL() to calculate km_gfm
          if (n .eq. 1) then
            ! Because gfm does not depend on n, runs this part just once, when n is 1.
            facto = km__fact(j_set, molid)
            km_gfm(l) = C2*((1.e-8*km_lmbdam(l))**2)*fe*qv*km_sj(l)*facto
          end if
        end do
      end do
    end do ! end of i_mol loop

    do l = 1, km_mblend
      km_alargm(l) = 0.1
    end do
  end



  !================================================================================================================================

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

    !> @todo Original select, remove when the above workds
    !~SELECT CASE (I_MOL)  !> @todo ISSUE Check molecule names and cases
    !~  CASE (1)  ! MgH
    !~    !~DO N = 1,modeles_NTOT
    !~    !~  PPA(N)=sat4_PMG(N)
    !~    !~  PB(N)=sat4_PPH(N)
    !~    !~END DO
    !~    PPA => sat4_PMG
    !~    PB => sat4_PPH
    !~  CASE (2)  ! C2
    !~    DO N=1,modeles_NTOT
    !~      PPA(N)=sat4_PPC2(N)
    !~      PB(N)=sat4_PPC2(N)
    !~    END DO
    !~  CASE (3, 4, 5)  ! CN blue,red, nir
    !~    DO N=1,modeles_NTOT
    !~      PPA(N)=sat4_PPC2(N)
    !~      PB(N)=sat4_PN(N)
    !~    END DO
    !~  CASE (6, 7, 8)  ! CH AX, BX, CX
    !~    DO  N=1,modeles_NTOT
    !~      PPA(N)=sat4_PPC2(N)
    !~      PB(N)=sat4_PPH(N)
    !~    END DO
    !~  CASE (9)  ! 13
    !~    DO N=1,modeles_NTOT
    !~      PPA(N)=sat4_PC13(N)
    !~      PB(N)=sat4_PPH(N)
    !~    END DO
    !~  CASE (10)  ! CO nir
    !~    DO N=1,modeles_NTOT
    !~      PPA(N)=sat4_PPC2(N)
    !~      PB(N)=sat4_PO(N)
    !~    END DO
    !~  CASE (11)  ! NH blue
    !~    DO N=1,modeles_NTOT
    !~      PPA(N)=sat4_PN(N)
    !~      PB(N)=sat4_PPH(N)
    !~    END DO
    !~  CASE (12, 13)  ! OH blue,nir
    !~    DO N=1,modeles_NTOT
    !~      PPA(N)=sat4_PO(N)
    !~      PB(N)=sat4_PPH(N)
    !~    END DO
    !~  CASE (14)  ! FeH
    !~    DO N=1,modeles_NTOT
    !~      PPA(N)=sat4_PFE(N)
    !~      PB(N)=sat4_PPH(N)
    !~    END DO
    !~  CASE (15, 16, 17, 18, 19, 20, 21)  ! Tio Gama,Gama linha,alfa,beta,delta,epsilon,phi
    !~    DO N=1,modeles_NTOT
    !~      PPA(N)=sat4_PTI(N)
    !~      PB(N)=sat4_PO(N)
    !~    END DO
    !~END SELECT
  end

end module molecula

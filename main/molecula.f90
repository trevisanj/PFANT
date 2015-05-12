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

  !~integer, parameter :: max_lines_per_mol=300000
  ! Old "NTR"; Maximum number of transitions ("Set-Of-Lines") for each molecule

  integer, parameter :: max_sol_per_mol=200

  !~ integer, parameter ::
  !~+  max_km__lines_total = max_lines_per_mol*num_mol
  integer, parameter :: max_km__lines_total=1400000

  ! Specifies how many molecules to read
  !> @todo ISSUE: According to EC, 16-21 are hydrogen lines which are used somewhere else, gotta check this, it is taking 7 seconds to read the whole file
  ! (MT) Gonna ask BLB this
  integer km__number

  integer km__lines_total  ! Total number of spectral line, counting all molecules

  character*256 km__titm, km__titulo

  dimension km__titulo(num_mol)

  real*8, dimension(num_mol) :: km__fe, km__do, &
   km__mm, km__am, km__bm, km__ua, km__ub, km__te, km__cro, &
   km__a0, km__a1, km__a2, km__a3, km__a4, km__als, km__s

  integer, dimension(num_mol)  :: km__ise, km__nv, &
   km__lines_per_mol  !> This stores the number of spectral lines for each molecule

  real*8, dimension(max_sol_per_mol, num_mol) :: km__qqv, km__ggv, km__bbv, km__ddv, km__fact


  !> "Index of Last Lambda Of Set-Of-Lines"
  !> Points to km__lmbdam, km__sj, km__jj; km__last
  !> This is mounted at reading to help with the filtering and avoid
  !> allocating 2 dimensions for (lm__lmbdam, km__sj, km__jj)
  real*8, dimension(max_sol_per_mol, num_mol) :: km__iollosol 

  real*8,  dimension(max_km__lines_total) :: km__lmbdam
  real*8,    dimension(max_km__lines_total) :: km__sj, km__jj

  !~ integer, dimension(max_lines_per_mol, num_mol) ::
  !~+  km__numlin


  !=====
  ! Variables filled by filter_moleculagrade()

  integer km_mblend  ! Total number of spectral lines *filtered in*

  ! Valid elements of these are from 1 to km_mblend
  real*8, dimension(max_km__lines_total) :: km_lmbdam
  real*8, dimension(max_km__lines_total) :: km_sj, km_jj, km_gfm, km_alargm


  !------
  ! These two arrays contain indexes pointing at km_LMBDAM, km_SJ, and km_JJ
  !------
  ! This one points to the last index of the lines of each molecule within
  ! km_lmbdam, km_sj and km_jj (after the filtering)
  ! Update: **augmented!** -- first element is 0 (ZERO) -- facilitates the algorithm
  dimension km_mblenq(num_mol+1)
  ! This is similar but is a "local" one, it contains index of the last
  ! line of each set of lines within km_lmbdam, km_sj and km_jj
  ! **for the current molecule** I_MOL
  ! Update: **augmented!** -- first row is 0 (ZERO) -- facilitates the algorithm
  !> @todo Explain better
  dimension km_ln(max_sol_per_mol+1, num_mol)

  !> @todo ISSUE: "Warning: possible change of value in conversion from real(8) to real(4)"
  real*8, dimension(max_km__lines_total, max_modeles_ntot) :: km_pnvj

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
    use errors
    use config
    implicit none
    character(len=*) :: filename
    
    integer unit_, i,
     molid,   &  ! Old "NMOL", index/ID of molecule, ranges from 1 to NUM_MOL
     i_line,  &  ! Counts lines within each molecule (reset at each new molecule)
     nnv, iz, &
     numlin , &  ! Temporary variable
     j_set,   &
     j_line
    parameter(unit_=199)
    character*80 lll

    open(unit=unit_,file=filename, status='old')


    !__logging__
    write (lll,*) 'max_km__lines_total = ', max_km__lines_total
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
    read(unit_,*) (km__nv(molid), molid=1,num_mol)

    !__spill check__
    do molid = 1, num_mol
      if (km__nv(molid) .gt. max_sol_per_mol) then
          write(lll,*) 'read_moleculagrade(): molecule id ', molid,
   +       ' has nv = ', km__nv(molid), ' (maximum is ',
   +       max_sol_per_mol, ')'
          stop error_exceeded
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

      read(unit_,*) km__fe(molid), km__do(molid), km__mm(molid),
   +             km__am(molid), km__bm(molid), km__ua(molid),
   +             km__ub(molid), km__te(molid), km__cro(molid)


      !> @todo ISSUE Documentation
      !> @todo ISSUE !P! My sample file is blank here
      read(unit_,'(2x,i3, 5f10.6, 10x, f6.3)') km__ise(molid),
   +    km__a0(molid), km__a1(molid), km__a2(molid),
   +    km__a3(molid), km__a4(molid), km__als(molid)

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
        if (i_line .gt. max_km__lines_total) then
          write(*,*) 'read_moleculagrade(): exceeded maximum number of spectral lines total = ',
           max_km__lines_total, ' (at molecule id ', molid, ')'
          stop error_exceeded
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
        write(lll,*) 'read_moleculagrade():  incorrect number of set-of-lines: ', j_set,
   +     '(should be ', nnv, ')  (in molecule number ', molid, ')'
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
    use config
    use errors
    implicit none
    !> Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    !> Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    real*8 :: lambda
    integer i,
            MOLID,          &  ! Counts molecule ID, from 1 to NUM_MOL
            I_MOL,          &  ! Counts molecules that are "switched on"
            J_DUMMY, J_SET, &
            I_LINE,         &  ! Index of km__LMBDAM, km__SJ, km__JJ
            I_FILTERED      &  ! Counts number of filtered lines (molecule-independent);
                               !  index of km_LMBDAM, km_SJ, km_JJ
    LOGICAL FLAG_IN
    CHARACTER*80 LLL


    !__logging__
    WRITE(LLL, *) 'config_NUM_MOL_ON = ', config_NUM_MOL_ON
    CALL LOG_DEBUG(LLL)


    ! Initializes the zero elements of the augmented matrices
    km_MBLENQ(1) = 0
    DO I = 1, NUM_MOL
      km_LN(1, I) = 0
    END DO

    I_FILTERED = 0  ! Current *filtered-in* spectral line. Keeps growing (not reset when the molecule changes). Related to old "L"
    I_LINE = 1
    I_MOL = 0
    DO MOLID = 1, NUM_MOL
      IF (.NOT. MOLECULE_IS_ON(MOLID)) CYCLE

      I_MOL = I_MOL+1

      !__logging__
      WRITE(LLL, *) 'MOLECULE ID', MOLID, ': ',  km__TITULO(MOLID)
      CALL LOG_DEBUG(LLL)
      WRITE(LLL, *) 'Number of prospective Lambdas ------>', km__LINES_PER_MOL(MOLID)
      CALL LOG_DEBUG(LLL)


      ! Counters starting with "J_" restart at each molecule
      J_SET = 1   ! Current "set-of-lines"
      FLAG_IN = .FALSE.  ! Whether has filtered in at least one line
      DO J_DUMMY = 1, km__LINES_PER_MOL(MOLID)
        LAMBDA = km__LMBDAM(I_LINE)

        IF ((LAMBDA .GE. LZERO) .AND. (LAMBDA .LE. LFIN)) THEN
          ! Filters in a new spectral line!
          I_FILTERED = I_FILTERED+1

          km_LMBDAM(I_FILTERED) = LAMBDA
          km_SJ(I_FILTERED) = km__SJ(I_LINE)
          km_JJ(I_FILTERED) = km__JJ(I_LINE)

          FLAG_IN = .TRUE.

        END IF

        IF (I_LINE .EQ. km__IOLLOSOL(J_SET, MOLID)) THEN
          ! Reached last line of current set of lines


*            !> @todo ISSUE Should we think about preparing it for not having a single line within LZERO-LFIN for set J_SET, J_SET=1,NNV?????
*            IF (.NOT. FLAG_IN) THEN
*              !> @todo, IDEA Actually I think that it might work without having lines within a given lambda range, because the routines that use the calculations just don't care which molecule it is
*              !> @todo but I can give a *WARNING*, more for testing than for anything else, actually
*
*              !--error checking--!
*              !> @todo test this error
*              WRITE (*, *) 'FILTER_MOLECULAGRADE(): Molecule ID ',MOLID,
*     +            ' titled  "', km__TITULO(MOLID), '"'
*              WRITE (*, *) 'Set of lines ', (J_SET), 'has no lambda '
*     +            //'within ', LZERO, ' <= lambda <= ', LFIN
*              WRITE (*, *) 'The algorithm is not prepared for this, '
*     +            //'sorry!'
*              STOP ERROR_BAD_VALUE
*            END IF

          km_LN(J_SET+1, I_MOL) = I_FILTERED  ! Yes, J_SET+1, not J_SET, remember km_LN first row is all ZEROes.
          J_SET = J_SET+1
        END IF

        I_LINE = I_LINE+1
      END DO

      km_MBLENQ(I_MOL+1) = I_FILTERED  ! Yes, I_MOL+1, not I_MOL, remember km_MBLENQ(1) is ZERO.
    END DO !--end of MOLID loop--!

    km_MBLEND = I_FILTERED
  END







!================================================================================================================================
!> Calculates the molecular absorption coefficient.
!>
!> Uses km_* filled by FILTER_MOLECULAGRADE()

  SUBROUTINE USE_MOLECULAGRADE()
    USE CONFIG
    USE SAT4_DIE
    USE LOGGING
    IMPLICIT NONE

    REAL*8 T5040, PSI
    REAL CSC, H, C, KB, C2
    REAL FE, DO_, MM, AM, BM, UA, UB, TE, CRO, RM
    REAL QV, GV, BV, DV, FACTO
    INTEGER I_MOL, J_SET, L, L_INI, L_FIN, N, NNV, MOLID
    CHARACTER*80 LLL

    REAL*8, PARAMETER :: H  = 6.6252E-27,   &
                         C  = 2.997929E+10, &
                         KB = 1.38046E-16,  &
                         C2 = 8.8525E-13

    DO I_MOL = 1, config_NUM_MOL_ON
      MOLID = GET_MOLID(I_MOL)

      !__logging__
      WRITE(LLL, *) 'MOLECULE ID ', MOLID, ': ', km__TITULO(MOLID)
      CALL LOG_DEBUG(LLL)

      CALL POINT_PPA_PB(MOLID)

      NNV = km__NV(MOLID)

      FE  = km__FE(MOLID)
      DO_ = km__DO(MOLID)
      MM  = km__MM(MOLID)
      AM  = km__AM(MOLID)
      BM  = km__BM(MOLID)
      UA  = km__UA(MOLID)
      UB  = km__UB(MOLID)
      TE  = km__TE(MOLID)
      CRO = km__CRO(MOLID)


      !======
      ! This part of the code calculates km_PNVL(
      RM = AM*BM/MM
      DO N = 1,modeles_NTOT
        T5040 = modeles_TETA(N)/5040
        PSI = DO_*modeles_TETA(N)+2.5*ALOG10(modeles_TETA(N))-1.5*ALOG10(RM)-&
              ALOG10(UA*UB)-13.670
        PSI = 10.**PSI

        DO J_SET = 1,NNV
          QV = km__QQV(J_SET, MOLID)
          GV = km__GGV(J_SET, MOLID)
          BV = km__BBV(J_SET, MOLID)
          DV = km__DDV(J_SET, MOLID)

          L_INI = km_LN(J_SET, MOLID)+1
          L_FIN = km_LN(J_SET+1, MOLID)

          ! L is index within km_LMBDAM, km_SJ and km_JJ
          DO L= L_INI, L_FIN
            ! PC: default value for CSC does not exist physically
            CSC = EXP(-H*C/KB*modeles_TETA(N)/5040.*(TE+GV+BV*(km_JJ(L)+1)*km_JJ(L)))*   &
                  (2.-CRO)*(2.*km_JJ(L)+1.)*                                             &
                  EXP(H*C/KB*modeles_TETA(N)/5040.*(DV*(km_JJ(L)*(km_JJ(L)+1))**2+2.*BV))

            !> @todo issue ?what? ?doc? to do with this REAL*4 vs. REAL*8 thing?
            km_PNVJ(L,N) = CSC*PSI*PPA(N)*PB(N)/sat4_PPH(N)
          END DO


          ! Takes advantage of current J_SET loop so it is not necessary to create
          ! another double loop as in the original KAPMOL to calculate km_GFM
          IF (N .EQ. 1) THEN
            ! Because GFM does not depend on N, runs this part just once, when N is 1.
            FACTO = km__FACT(J_SET, MOLID)
            km_GFM(L) = C2*((1.E-8*km_LMBDAM(L))**2)*FE*QV*km_SJ(L)*FACTO
          END IF
        END DO
      END DO
    END DO !--end of I_MOL loop--!

    DO L = 1,km_MBLEND
      km_ALARGM(L) = 0.1
    END DO
  END



  !================================================================================================================================

  !> Private subroutine; pointer operation; assigns address of variable PPA and PB depending on the molecule ID.
  !>
  !> This was originally a vector copy element-by-element in old routine KAPMOL. However, as
  !> PPA and PB contents are not changed after the assignment, it is reasonable to just point
  !> to the source vectors (way faster).
  SUBROUTINE POINT_PPA_PB(MOLID)
    USE CONFIG
    USE ERRORS
    USE SAT4_DIE
    USE LOGGING
    IMPLICIT NONE
    INTEGER MOLID
    CHARACTER*192 S
    
    IF (MOLID .GT. NUM_MOL) THEN
      WRITE (S, *) 'FILL_PPA_PB(): Invalid molecule ID (', MOLID, ') must be maximum ', NUM_MOL
      CALL PFANT_HALT(S)
    END IF

    SELECT CASE (MOLID)  !> @todo ISSUE Check molecule names and cases
      CASE (1)  ! MgH
        PPA => sat4_PMG
        PB  => sat4_PPH
      CASE (2)  ! C2
        PPA => sat4_PPC2
        PB  => sat4_PPC2
      CASE (3, 4, 5)  ! CN blue,red, nir
        PPA => sat4_PPC2
        PB  => sat4_PN
      CASE (6, 7, 8)  ! CH AX, BX, CX
        PPA => sat4_PPC2
        PB  => sat4_PPH
      CASE (9)  ! 13
        PPA => sat4_PC13
        PB  => sat4_PPH
      CASE (10)  ! CO nir
        PPA => sat4_PPC2
        PB  => sat4_PO
      CASE (11)  ! NH blue
        PPA => sat4_PN
        PB  => sat4_PPH
      CASE (12, 13)  ! OH blue,nir
        PPA => sat4_PO
        PB  => sat4_PPH
      CASE (14)  ! FeH
        PPA => sat4_PFE
        PB  => sat4_PPH
      CASE (15, 16, 17, 18, 19, 20, 21)  ! Tio Gama,Gama linha,alfa,beta,delta,epsilon,phi
        PPA => sat4_PTI
        PB  => sat4_PO
    END SELECT

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
  END

END MODULE MOLECULA

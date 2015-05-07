! This file is part of PFANT.
!
! PFANT is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Foobar is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with PFANT.  If not, see <http://www.gnu.org/licenses/>.

!> @ingroup gr_io
module read_files
  implicit none
      ! Variables filled by READ_DISSOC() (file dissoc.dat)

      !> @todo ISSUE: I find very confusing NELEMX (metal atoms) versus NELEM (molecules)

      !> Number of elements actually used is specified by variable dissoc_NMETAL <= MAX_dissoc_NMETAL
      integer, parameter :: max_dissoc_nmetal=50  ! Limit number of metal rows in dissoc.dat


      integer, parameter :: max_z = 100  ! Maximum atomic number that can be found in dissoc.dat

      !> Maximum possible value of modeles_NTOT
      integer, parameter :: max_modeles_ntot=50



      ! dissoc.dat, metals part
      integer dissoc_nmetal, dissoc_nimax, dissoc_nelemx
      character*2 dissoc_elems
      integer dissoc__ig0, dissoc__ig1
      real dissoc__ip, dissoc__cclog
      dimension dissoc_elems(max_dissoc_nmetal),  & ! Only this ...
                dissoc_nelemx(max_dissoc_nmetal), & ! ... and this are used directly.
      ! JT2015 I introduced these variables, double underscore to emphasize that they
      !        are not just old variables that had a prefix added.
                dissoc__ip(max_dissoc_nmetal),    & ! These other 4 variables are used
                dissoc__ig0(max_dissoc_nmetal),   & ! for filling local variables
                dissoc__ig1(max_dissoc_nmetal),   & ! within SAT4()
                dissoc__cclog(max_dissoc_nmetal)    !

      ! dissoc.dat, molecules part
      integer, parameter:: max_dissoc_nmol=600  ! Limit number of molecule rows ISSUE: bit overdimensioned?? (considering the file has only about 30 molecules)
      character dissoc_mol*3
      integer dissoc_nmol, dissoc_mmax, dissoc_nelem, dissoc_natom
      real dissoc_c, dissoc_eps, dissoc_switer
      dimension dissoc_mol(max_dissoc_nmol), &
                dissoc_c(max_dissoc_nmol, 5), &
                dissoc_mmax(max_dissoc_nmol), &
                dissoc_nelem(5, max_dissoc_nmol), &
                dissoc_natom(5, max_dissoc_nmol)





! Variables filled by READ_MAIN() (file main.dat)
      character main_titrav*10, main_fileflux*64
      logical   main_ecrit, main_ptdisk
      real*8    main_pas, main_echx, main_echy, main_fwhm, &
                main_mu, main_afstar, main_llzero, &
                main_llfin, main_aint, &
                main_teff, main_glog, main_asalog, main_nhe
      integer   main_ivtot, & ! affects TURBUL() ISSUE what
                              ! = 1 -- "VT" constant
                              ! > 1 -- "VT" variable
                main_inum
      character main_filetohy*64

      dimension main_filetohy(10)
      dimension main_titrav(20)
      real*8, dimension(max_modeles_ntot) :: main_vvt, main_tolv
      real*8, dimension(max_dissoc_nmetal) :: main_xxcor   ! This vector goes along with dissoc_ELEMS and dissoc_NELEMX


! Variables filled by READ_ABONDS() (file abonds.dat)
      integer, parameter :: max_abonds_nabond=100  ! Limit number of abundances in abonds.dat
      integer abonds_nabond
      character*2 abonds_ele
      real*8 abonds_abol, abonds_abo
      dimension abonds_ele(max_abonds_nabond), &
                abonds_abol(max_abonds_nabond), &
                abonds_abo(max_abonds_nabond)   ! This is calculated


! Variables filled by READ_ATOMGRADE() (file atomgrade.dat)


      integer, parameter :: &
       max_atomgrade__nblend=13000, & ! Half of the maximum the number of rows in atomgrade.dat
       max_atomgrade_nblend=8000      ! Maximum number of spectral lines possible within the interval LZERO, LFIN

      integer*4 atomgrade_nblend, atomgrade__nblend
      character*2 atomgrade_elem, atomgrade__elem
      integer*4 atomgrade_ioni, atomgrade__ioni

      ! Only these variables had their sizes specified originally


!> @todo ISSUE: Can I just declare everything as DOUBLE PRECISION (by default!!)?? (MT) Yes and modeles.mod could become an ASCII file!!!

      real atomgrade_kiex, atomgrade__kiex
      real*8 atomgrade_gr, atomgrade_lambda, atomgrade__lambda


!> @todo ISSUE: nobody cares about ABONDR_DUMMY. Why? I might have the answer, this is taken from elsewere, file abonds.dat, maybe


      ! Filtered variables
      real*8, dimension(max_atomgrade__nblend) :: &
       atomgrade_ge, atomgrade_zinf, atomgrade_abondr_dummy, atomgrade_algf, &
       atomgrade_gf, atomgrade_ch
      real*8 atomgrade_abonds_abo
      dimension atomgrade_elem(max_atomgrade_nblend), &
                atomgrade_ioni(max_atomgrade_nblend), &
                atomgrade_lambda(max_atomgrade_nblend), &
                atomgrade_kiex(max_atomgrade_nblend), &
                atomgrade_gr(max_atomgrade_nblend), &
                atomgrade_abonds_abo(max_atomgrade_nblend)


      ! File originals
      real*8, dimension(max_atomgrade__nblend) :: atomgrade__abondr_dummy, &
       atomgrade__algf, atomgrade__ch, atomgrade__ge, atomgrade__zinf, &
       atomgrade__gr
      real*8 atomgrade__abonds_abo
      dimension atomgrade__elem(max_atomgrade__nblend), &
                atomgrade__ioni(max_atomgrade__nblend), &
                atomgrade__lambda(max_atomgrade__nblend), &
                atomgrade__kiex(max_atomgrade__nblend), &
                atomgrade__abonds_abo(max_atomgrade__nblend)  ! will be filled by searching atomgrade__ELEM within abonds_ELE



! Variables filled by READ_PARTIT() (file partit.dat)
! ---------------------------------------------------
      ! Limit number of "items" in partit.dat:
      ! - Maximum value for partit_npar
      ! - Second dimension of partit_tabu
      integer, parameter :: max_partit_npar=85
      ! Third dimension of partit_TABU
      integer, parameter :: max_partit_kmax=63

      character*2  partit_el
      integer partit_npar

      real*8, dimension (max_partit_npar) :: partit_ki1, partit_pa, &
       partit_m, partit_tini, partit_ki2
      integer, dimension (max_partit_npar) :: partit_jkmax
      real partit_tabu
      dimension partit_el(max_partit_npar), &
                partit_tabu(max_partit_npar, 3, max_partit_kmax)






        ! NION   =
        ! XI(J,I)=POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATIO
        ! PF(J,I)=FONCTION DE PARTITION         ''   ''     ''      ''   ''


! Variables filled by READ_ABSORU2() (file absoru2.dat)
! ---------------------------------------------------
      ! Maximum value for absoru_NM
      integer, parameter :: max_absoru2_nm=30
      ! Maximum value for absoru2_NR(J)
      integer, parameter :: max_absoru2_nrr=9
      ! Maximum value for each element of absoru2_NUMSET
      integer, parameter :: max_absoru2_numset_i=41

      integer absoru2_nm, absoru2_nmeta, absoru2_numset, absoru2_nr

      !> NOM DE L'ELEMENT
      character*2 :: absoru2_nomet(max_absoru2_nm)

      real*8 absoru2_abmet, absoru2_abhel
      real absoru2_pf, absoru2_wi, absoru2_xi, absoru2_zm, absoru2_zp

      !> @todo ISSUE: i am not sure about these character*4, made this from the format below
      character*4 absoru2_titre, absoru2_iunite

      dimension  absoru2_iunite(2), absoru2_titre(17), &
                 absoru2_nr(max_absoru2_nm), &
                 absoru2_zp(max_absoru2_nm), &
                 absoru2_zm(max_absoru2_nm)
                 

      !> XI(J,I) = POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATION
      real*8 :: absoru2_xi(max_absoru2_nm, max_absoru2_nrr)
      ! PF(J,I) = FONCTION DE PARTITION DE L'ELEMENT J AU STADE D'IONISATION
      real*8 :: absoru2_pf(max_absoru2_nm, max_absoru2_nrr)


      dimension absoru2_wi(max_absoru2_numset_i, 2), absoru2_numset(2)





!> Variables filled by READ_MODELE() (file modeles.mod)
!> ----------------------------------------------------

      ! Attention: one has to specify sizes of all the variables here, because
      ! this may change with compiler
      integer*4 modeles_ntot
      character*4 modeles_tit
      character*20 modeles_tiabs ! I just want to see this string at testing

      !> @todo create REAL*4 temp locals, then transfer to REAL*8 ones
      real*4 modeles_detef, modeles_dglog, modeles_dsalog, &
             modeles_asalalf, modeles_nhe, &
             modeles_nh, modeles_teta, modeles_pe, modeles_pg, &
             modeles_t5l, bid

      dimension modeles_nh(max_modeles_ntot), &
                modeles_teta(max_modeles_ntot), &
                modeles_pe(max_modeles_ntot), &
                modeles_pg(max_modeles_ntot), &
                modeles_t5l(max_modeles_ntot), &
                bid(16), &  !> @todo ISSUE: I counted 12... let's see, I have to see INEWMARCS... actually, it seems that it should be 12!! &
                modeles_tit(5)





      ! Flags indicating whether corresponding routine has already been called.
      ! There is a strict order in which files must be read, because there are
      ! consistency checks and inner joins being percormed while reading files.
      ! At the moment, this order is reinforces: DISSOC -> MAIN -> ABONDS -> ATOMGRADE
      logical, private :: flag_read_abonds = .false., &
                          flag_read_main   = .false., &
                          flag_read_dissoc = .false.



      SAVE

CONTAINS



!================================================================================================================================
!> Reads file main.dat to fill variables main_*
!>
!> Original UNIT: 4
!>
!> IMPORTANT: Depends on variable dissoc_NMETAL (this variable is filled
!>            by READ_DISSOC())

  subroutine read_main(filename)
    implicit none
    integer unit_
    parameter(unit_=4)
    character(len=*) :: filename
    integer ih, i
    character*80 lll

    if (.not. flag_read_dissoc) then
      call pfant_halt('read_dissoc() must be called before read_main()')
    end if

    open(unit=unit_,file=filename, status='old')

    !> row 01: object name, e.g. "sun"
    !> =======
    read(unit_, '(20a)') main_titrav

    !> row 02: ???
    !> =======
    !> Example:  T      0.02 5.0   1.    .12
    read(unit_, *) main_ecrit, main_pas, main_echx, main_echy, main_fwhm

    !> row 03
    !> =======
    !> Example: 0.9
    read(unit_, *) main_vvt(1)
    main_ivtot = 1

    !> rows 03.(0-2) (three conditional rows that MUST exist if and only if main_VVT(1) > 900)
    !> =============
    !> @todo ISSUE: VVT(1) is used throughout the program, so why reading VVT(I)??
    !> @todo ISSUE Explain this part VERY WELL because it is an "anomaly", i.e., main.dat will have MORE LINES
    !> @todo ISSUE Documentation
    !> (MT) Yes it makes sense, it specifies microturbulence velocities for each layer of the atmosphere
    if(main_vvt(1) .gt. 900)  then   ! vt variable avec la profondeur
      read(unit_, *) main_ivtot
      ! ivtot, affects subroutine turbul() issue what
      if (main_ivtot .gt. max_modeles_ntot) then
        write (lll, *) 'main_ivtot .gt. max_modeles_ntot (', &
         main_ivtot, ' .gt. ', max_modeles_ntot, ')'
         call pfant_halt(lll)
      end if

      read(unit_,*) (main_tolv(i), i=1, main_ivtot)
      read(unit_,*) (main_vvt(i) ,i=1, main_ivtot)
    end if


    !> line 04
    !> =======
    !> Example:      5777  4.44  0       0.1  1
    read(unit_, *) main_teff, main_glog, main_asalog, main_nhe,main_inum

    !> line 05
    !> =======
    !> Example:      F       1.
    read(unit_, *) main_ptdisk, main_mu

    !> line 06
    !> =======
    !> Example: 0
    read(unit_, *) main_afstar  ! metallicity of the star (in log scale)

    !> line 07 -- XXCOR(I)
    !> =======
    !> @todo ISSUE: Should be a column in dissoc.dat !!!!!
    !> (MT) I agree

    ! Feature "XXCOR" --
    read(unit_, *)(main_xxcor(i), i=1, dissoc_nmetal)

    !> line 08 -- part of filename
    !> =======
    !> This line will define the names of other three files to be read:
    !>   cont.<FILEFLUX>
    !>   norm.<FILEFLUX>
    !>   spec.<FILEFLUX>
    read(unit_, '(a)') main_fileflux

    !> line 09 --
    !> =======
    !>     AINT =intervalle de calcul

    !> Example:      4800    4820   50
    read(unit_, *) main_llzero, main_llfin, main_aint


  !> rows 10-19 -- file names, in sync with variable LLHY
  !> ===========
  !> Example: thkappa
  !>          thiota
  !>          ththeta
  !>          theta
  !>          thzeta
  !>          thepsilon
  !>          thdelta
  !>          thgamma
  !>          thbeta
  !>          thalpha

    do ih = 1, 10
      read(unit_, '(a)') main_filetohy(ih)
    end do



    close(unit=unit_)
    flag_read_main = .true.
  end




!================================================================================================================================
!> Reads file dissoc.dat to fill variables dissoc_*
!>
!> Original UNIT: 23
!>
!> This file must end with a blank row so that the routine can detect
!> the end of the file


!> TODO Various tests:
!> TODO - mismatched NMETAL and metal rows
!> TODO - check if NMETAL and NMOL match what they are supposed to (assertions in test)

!> PROPOSE: use READ()'s "END=" option

  subroutine read_dissoc(filename)
    integer unit_
    integer i, j, k, m, mmaxj
    parameter(unit_=199)
    character(len=*) :: filename
    character*192 lll
    character*2 symbol
    logical flag_found

    ! Auxiliary temp variables for reading file
    integer*4 natomm, nelemm
    dimension natomm(5), nelemm(5)

    open(unit=unit_,file=filename, status='old')

    !> row 01
    !> =======
    !> NMETAL - NUMBER OF ELEMENTS CONSIDERED IN CHEMICAL EQUILIBRIUM
    !> NIMAX  - MAXIMUM NUMBER OF ITERATION IN NEWTON-RAPSON METHOD
    !> EPS    - IF ABS((X(I+1)-X(I))/X(I)).LE. EPS; CONVERGED
    !> SWITER - IF SWITER .GT. 0;   X(I+1)=0.5*(X(I+1)+X(I))
    !>          IF SWITER .LE. 0;   X(I+1)=X(I+1)
    !>
    !> Example:    18  100    0.005   -1.0
    read(unit_,'(2i5, 2f10.5, i10)') dissoc_nmetal, dissoc_nimax, dissoc_eps, dissoc_switer

    !> rows 2 to NMETAL+1
    !> ==================
    !> 6 columns:
    !>   col 1 -- symbol of chemical element
    !>   col 2 -- atomic number "N"
    !>   col 3 -- (?)
    !>   col 4 -- (?)
    !>   col 5 -- (?)
    !>   col 6 -- (?)
    do i = 1, dissoc_nmetal
      read (unit_, '(a2, 2x, i6, f10.3, 2i5, f10.5)') &
       symbol, dissoc_nelemx(i), dissoc__ip(i), &
       dissoc__ig0(i), dissoc__ig1(i), dissoc__cclog(i)

        ! makes sure that elements first and second are h and he, respectively,
        ! because sat4() and die() count on this
        select case (i)
          case (1)
            if (symbol .ne. 'h ') &
             call pfant_halt('first element must be hydrogen ("h ")!')
          case (2)
            if (symbol .ne. 'he' .and. symbol .ne. 'he') &
             call pfant_halt('second element must be helium ("he")!')
        end select

        dissoc_elems(i) = symbol

        !__spill check__
        if (dissoc_nelemx(i) .gt. max_z) then
          write(lll,*) 'read_dissoc(): metal # ', i, ': nelemxi = ', &
           dissoc_nelemx(i), ' over maximum allowed (', &
           max_z, ')'
          call pfant_halt(lll)
        end if

      end do

      !> rows NMETAL+2 till end-of-file
      !> ==============================
      !>   col  1     -- "name" of molecule
      !>   cols 2-6   -- !>(J, 1-5)
      !>   col  7     -- MMAX(J) (number of subsequent columns)/2
      !>   cols 8-... -- Maximum of 8 columns here.
      !>                 Pairs (NELEM(M), NATOM(M)), M = 1 to MMAX(J) ISSUE NELEM(M) is atomic number, what about NATOM(M)???
      j = 0

      1010 continue
      j = j+1

      !> @todo ISSUE: This 1X does not appear in my sample dissoc.dat file
      !> @todo ISSUE: Atually the file that Beatriz sent me does not work under this format!!!!
      !> @todo ISSUE: THere is no 1X
      !      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')


      read(unit_, '(a3, 5x, e11.5, 4e12.5, i1, 4(i2,i1))') &
                   dissoc_mol(j), &
                   (dissoc_c(j, k), k=1,5), &
                   dissoc_mmax(j), &
                   (nelemm(m), natomm(m), m=1,4)


      mmaxj = dissoc_mmax(j)
      if(mmaxj .eq. 0) go to 1014  ! means end-of-file

      if (mmaxj .gt. 4) then
        write(lll,*) 'read_dissoc() molecule "', dissoc_mol(j), &
         '", mmaxj = ', mmaxj, ' cannot be greater than 4!'
        call pfant_halt(lll)
      end if


      do m = 1, 4
        flag_found = .false.
        do i = 1, dissoc_nmetal
          if (nelemm(m) .eq. dissoc_nelemx(i)) then
            flag_found = .true.
            exit
          end if
        end do

        if (.not. flag_found) then
          write(lll,*) 'read_dissoc() molecule "', dissoc_mol(j), &
           '" atomic number ', nelemm(m), 'not in atoms list above'
          call pfant_halt(lll)
        end if
      end do

      do m = 1, mmaxj
          dissoc_nelem(m,j) = nelemm(m)
          dissoc_natom(m,j) = natomm(m)
      end do

      go to 1010

    1014 dissoc_nmol = j-1

    close(unit=unit_)
    flag_read_dissoc = .true.
  end




  !================================================================================================================================
  !> Reads file abonds.dat to fill variables abonds_*
  !>
  !> In addition, searches elements in dissoc atoms table to take XXCOR values.
  !>
  !> The input file has 3 columns:
  !>   1) Empty space or "1"
  !>   2) 2-character atomic element symbol (?)
  !>   3) absolute abundance (a) (exponent; actual abundance is 10^a), unit "dex"
  !>
  !> The end of the file is signalled by two rows containing only "1" each at
  !> column 1 and nothing else at the others
  !>
  !> TODO Test one row!!!
  !> PROPOSE: use READ()'s "END=" option

  subroutine read_abonds(filename)
    implicit none
    integer unit_, finab, k, j
    parameter(unit_=199)
    character(len=*) :: filename
    character*80 lll
    logical flag_found
    real*8 fstar

    if (.not. flag_read_main) then
      call pfant_halt('read_main() must be called before read_abonds()')
    end if

    open(unit=unit_,file=filename, status='old')

    fstar = 10**main_afstar

    j = 1
    finab = 0
    do while (finab .lt. 1)
      read(unit_, '(i1,a2,f6.3)') finab, abonds_ele(j), abonds_abol(j)

      if (finab .lt. 1) then
        ! Extra tasks (i.e., apart from reading file) [1], [2]:
        !
        ! [1] Searches dissoc.dat' metals table by atomic symbol to get value of XXCOR variable
        !> @todo ISSUE: lot of effort to keep dubious feature
        !> @todo ISSUE: This is the thing that Beatriz mentioned that is not used anymore

        flag_found = .false.
        do k = 1, dissoc_nmetal
          if(abonds_ele(j) .eq. dissoc_elems(k)) then
            flag_found = .true.
            exit
          end if
        end do

        if (.not. flag_found) then
          write(lll,*) 'read_abonds() element "', abonds_ele(j), '" is not in dissoc atoms list!'
          call pfant_halt(lll)
        end if

        abonds_abol(j) = abonds_abol(j)+main_xxcor(k)

        ! [2] Calculates abonds_ABO based on abonds_ABOL

        !> @todo ISSUE Why "-12" ??
        abonds_abo(j) = 10.**(abonds_abol(j)-12.)
        abonds_abo(j) = abonds_abo(j)*fstar
      end if

      j = j+1
    end do
    abonds_nabond = j-2

    close(unit=unit_)
    flag_read_abonds = .true.
  end








  !================================================================================================================================
  !> Reads file atomgrade.dat to fill variables atomgrade__* (double underscore)
  !>
  !> Original UNIT: 14
  !>
  !> Depends on abonds_*, so must be called after READ_ABONDS()
  !>
  !> This file has 2 types of alternating rows:
  !> @verbatim
  !>   odd row
  !>     col 1 -- 2-letter atomgrade_ELEM(K) FILLDOC
  !>     col 2 -- atomgrade_IONI(K) FILLDOC
  !>     col 3 -- atomgrade_LAMBDA(K) FILLDOC
  !>   even row
  !>     col 1 --
  !>     col 2 --
  !>     col 3 --
  !>     col 4 --
  !>     col 5 --
  !>     col 6 --
  !>     col 7 --
  !>     col 8 -- signals end-of-file. If "1", reading stops
  !> @endverbatim
  !>
  !> TODO: give error if blows MAX!!!!!!!!!!!!!!!!!
  !> @todo ISSUE: Cannot cope with empty file (tested), however there are if's inside the program: IF NBLEND .EQ. 0 .........
  !> (MT) use READ()'s "END=" option IS A GOOD IDEA, and we will do the following: stop reading EITHER when the "1" is found or when the file ends!
  !> TODO Assertions in test to see if numbers match what they are supposed to
  !> PROPOSE: use READ()'s "END=" option

  subroutine read_atomgrade(filename)
    implicit none
    integer unit_
    parameter(unit_=199)
    character(len=*) :: filename
    character*192 lll
    integer finrai, k, j
    logical flag_found

    if (.not. flag_read_abonds) then
      call pfant_halt('read_abonds() must be called before read_atomgrade()')
    end if

    open(unit=unit_,file=filename, status='old')

    k = 1

    9 continue
    read(unit_, '(a2, i1, 1x, f10.3)') atomgrade__elem(k), &
                                       atomgrade__ioni(k), &
                                       atomgrade__lambda(k)

    read(unit_, *) atomgrade__kiex(k), &
     atomgrade__algf(k), &
     atomgrade__ch(k), &
     atomgrade__gr(k), &
     atomgrade__ge(k), &
     atomgrade__zinf(k), &
     atomgrade__abondr_dummy(k), finrai


    !> @todo ISSUE Why this? Document!!!
    if (atomgrade_gr(k) .lt. 1e-37) atomgrade_gr(k) = 2.21e15 / atomgrade_lambda(k)**2

    if (finrai .eq. 1) go to 10
    k = k+1

    !__spill check__: checks if exceeds maximum number of elements allowed
    if (k .gt. max_atomgrade__nblend) then
      write(lll,*) 'read_atomgrade(): exceeded maximum of', max_atomgrade__nblend, ' spectral lines'
      call pfant_halt(lll)
    end if

    ! Searches atomgrade's element within abonds' elements and copies corresponding
    ! abonds_ABO value into atomgrade_ABONDS_ABO. Halts program if element not found.
    ! This is a "inner join" (SQL terminology)!
    ! Historical note: this corresponds to old routine "ABONDRAIH". The search was being carried out
    !   every time (LZERO, LFIN) changed and only for the filtered atomgrade rows. I decided to
    !   perform this search for all atomgrade rows and then filter atomgrade_ABONDS_ABO together
    !   with other variables in FILTER_ATOMGRADE(), which is probably cheaper.

    !> @todo ISSUE Why name was "ABONDRAIH"?? Did it mean "abundances of hydrogen lines"? I ask this because if it has really a physical meaning, I shouldn't bury this inside read_files.f

    flag_found = .false.
    do  j = 1, abonds_nabond
      if (abonds_ele(j) .eq. atomgrade__elem(k)) then
        flag_found = .true.
        exit
      end if
    end do
    if (.not. flag_found) then
      write(lll,*)  'manque l abondance du ', atomgrade__elem(k)
      call pfant_halt(lll)
    end if
    atomgrade__abonds_abo(k) = abonds_abo(j)

    go to 9

    10 continue
    atomgrade__nblend = k

!> @todo ISSUE
! MENTION: last atomic line wasn't being used!! (this has been fixed/changed). Original code:
!~	K=1
!~9	READ(14,103)ELEM(K),IONI(K),LAMBDA(K)
!~	READ(14,*) KIEX(K),ALGF(K),CH(K),GR(K),GE(K),ZINF(K),
!~	1 ABONDR(K),FINRAI
!~	write(34,103)ELEM(K),IONI(K),LAMBDA(K)
!~	GF(K)=10.**ALGF(K)
!~C        IF(K.EQ.1) GF(K)=10**AGGF
!~	IF(GR(K).LT.1E-37)   GR(K)=2.21E15 / LAMBDA(K)**2
!~	IF(FINRAI.EQ.1) GO TO 10
!~	IF(((LAMBDA(K).GT.LFIN).OR.(LAMBDA(K).LT.LZERO))) GO TO 205
!~	K=K+1
!~205	CONTINUE
!~	GO TO 9
!~10	NBLEND=K-1

    close(unit=unit_)

    return

  end


  !================================================================================================================================
  !> @ingroup gr_filter
  !> selects only spectral lines within range LZERO, LFIN
  !>
  !> Populates variables atomgrade_* (single underscore)
  !>
  !> Also looks into abonds_ELE(:) to fill atomgrade_ABOND_ABONDS

  subroutine filter_atomgrade(lzero, lfin)
    !> Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    !> Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    integer j, k
    character*192 lll

    k = 0
    do j = 1, atomgrade__nblend
      if((atomgrade__lambda(j).le.lfin) .and. (atomgrade__lambda(j) .ge. lzero)) then
        k = k+1


        !__spill check__: checks if exceeds maximum number of elements allowed
        if (k .gt. max_atomgrade_nblend) then
          write(lll,*) 'filter_atomgrade(): exceeded maximum of', &
           max_atomgrade_nblend, ' spectral lines'
          call pfant_halt(lll)
        end if

        !Filters in
        atomgrade_elem(k)   = atomgrade__elem(j)
        atomgrade_ioni(k)   = atomgrade__ioni(j)
        atomgrade_lambda(k) = atomgrade__lambda(j)
        atomgrade_kiex(k)   = atomgrade__kiex(j)
        atomgrade_algf(k)   = atomgrade__algf(j)
        atomgrade_gf(k)     = 10.**atomgrade__algf(j)
        atomgrade_ch(k)     = atomgrade__ch(j)
        atomgrade_gr(k)     = atomgrade__gr(j)
        atomgrade_ge(k)     = atomgrade__ge(j)
        atomgrade_zinf(k)   = atomgrade__zinf(j)

        atomgrade_abonds_abo(k) = atomgrade__abonds_abo(j)

        atomgrade_abondr_dummy(k) = atomgrade__abondr_dummy(j)

      end if
    end do

    atomgrade_nblend = k

  end


  !================================================================================================================================
  !> Reads file partit.dat to fill variables partit_*
  !>
  !> LECTURE DES FCTS DE PARTITION
  !>
  !> Rows in this file alternate between:
  !> 1) 8-column row
  !>    col 8 -- signals end-of-file. If 1, it ignores the row and
  !>             stops reading
  !>
  !> 2) Series of rows to fill in partit_TABU(J, :, :)

  subroutine read_partit(filename)
    implicit none
    integer unit_
    parameter(unit_=199)
    character(len=*) :: filename
    character*192 lll

    integer finpar, j, kmax, l, k

    open(unit=unit_,file=filename, status='old')


    j = 1
    finpar = 0
    do while (finpar .lt. 1)
      read (unit_, '(a2, 2f5.2, i3, 3f10.2, 34x, i1)') &
       partit_el(j), &
       partit_tini(j), &
       partit_pa(j), &
       partit_jkmax(j), &
       partit_m(j), &
       partit_ki1(j), &
       partit_ki2(j), finpar

      if (finpar .ne. 1) then

        !__spill check__: checks if exceeds maximum number of elements allowed
        if (j .gt. max_partit_npar) then
          write(lll,*) 'read_partit(): par exceeded maximum of ', max_partit_npar
          call pfant_halt(lll)
        end if


        kmax = partit_jkmax(j)

        !__spill check__: checks if exceeds maximum number of elements allowed
        if (kmax .gt. max_partit_kmax) then
          write(lll,*) 'read_partit(): par number', j, 'kmax=', kmax, &
           ' exceeded maximum of', max_partit_kmax
          call pfant_halt(lll)
        end if


        read(unit_, '(13f6.4)') ((partit_tabu(j, l, k), l=1, 3), k=1, kmax)

        j = j+1
      end if
    end do

    partit_npar = j-1

    return
  end


  !================================================================================================================================
  !> Reads file absoru2.dat to fill variables absoru2_*
  !>
  !> Attention: variables absoru2_ZP, absoru2_XI, and absoru2_PF
  !>            undergo transformation!
  !>
  !> Obs: this routine is in essence the old routine called "LECTUR"
  !>
  !> I think SSP means "SubProgram" but it is a typo with an additional "S"
  !>
  !> CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
  !> PUIS LES DONNEES CORRESPONDANTS AUX ABSORBANTS METALLIQUES SI NECE
  !> CALMET=1 SI ON NE TIENT PAS COMPTE DES METAUX
  !> CALMET=2 SI ON    TIENT     COMPTE DES METAUX
  !>
  !> @todo ISSUE: as opposed to original description, CALMET was being ignored in original routine LECTUR()
  !> (MT) ???
  !> (JT) I guess it is a remain of sth and nowadays the metals are always taken into account
  !>

  subroutine read_absoru2(filename)
    implicit none
    integer unit_
    parameter(unit_=199)
    character(len=*) :: filename
    character*80 lll
    character*3 neant
    integer nion, i, ith, j, nrr, nset

    open(unit=unit_,file=filename, status='old')

    ! ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
    ! ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
    read (unit_,'(2e15.7)') absoru2_abmet, absoru2_abhel


    ! NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISA
    ! NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
    ! IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
    ! IUNITE=' NOYAU H'  ''    ''    ''       ''      NOYAU D'HYDROGENE
    read (unit_,'(2i2, 19a4)') absoru2_nm, absoru2_nmeta, &
          (absoru2_iunite(i),i=1,2), (absoru2_titre(i),i=1,17)


    !__spill check__: checks if exceeds maximum number of elements allowed
    if (absoru2_nm .gt. max_absoru2_nm) then
      write(lll,*) 'read_absoru2(): nm=', absoru2_nm, ' exceeded maximum of', max_absoru2_nm
      call pfant_halt(lll)
    end if


    ! LECTURE DE LA TABLE D'IONISATION CHOISIE
    do j = 1, absoru2_nm
      read (unit_, '(3x,i3,2e16.5)') absoru2_nr(j), absoru2_zp(j), absoru2_zm(j)
      absoru2_zp(j) = 10**absoru2_zp(j)

      ! NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
      ! ZP=NBR. D'ABONDANCE DE L'ELEMENT
      ! ZM=POIDS MOLECULAIRE DE L'ELEMENT
      nrr = absoru2_nr(j)

      ! checks if exceeds maximum number of elements allowed
      if (nrr .gt. max_absoru2_nrr) then
        write(lll,*) 'read_absoru2(): j = ', j, 'nr=', nrr, ' exceeded maximum of', max_absoru2_nrr
        call pfant_halt(lll)
      end if

      do i = 1, nrr
        ! neant="nothing"
        ! NION is also not used
        !> @todo ISSUE: NOMET is not used in the program
        read (unit_, '(a3,a2,i1,2e16.5)') neant, absoru2_nomet(j), &
         nion, absoru2_xi(j,i), absoru2_pf(j,i)

        ! ON LIT NR CARTES CONTENANT CHACUNE LE POTENTIEL D'IONISATION ET LA
        ! FONCTION DE PARTITION(LOG10(2UI+1)/UI)DE CHAQUE DEGRE D'IONISATION
        ! CES VALEURS SONT LUES DANS L'ORDRE CROISSANT DU DEGRE D'IONISATION
        ! NOMET  =NOM DE L'ELEMENT
        ! NION   =SON ETAT D'IONISATION
        ! XI(J,I)=POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATIO
        ! PF(J,I)=FONCTION DE PARTITION         ''   ''     ''      ''   ''

        absoru2_xi(j, i) = absoru2_xi(j, i)*2.302585
        absoru2_pf(j, i) = absoru2_pf(j, i)*2.302585
      end do
    END DO


    read (unit_, '(2i2)') (absoru2_numset(ith), ith=1,2)

!> @todo ISSUE: I am not sure if this last part is being read correcly. Perhaps I didn't de-spag right. Anyway, the test verbose is not good.


    !__spill check__: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(1) .gt. max_absoru2_numset_i) then
      write(lll,*) 'read_absoru2(): numset(1) = ', absoru2_numset(1), &
       ' exceeded maximum of', max_absoru2_numset_i
      call pfant_halt(lll)
    end if
    !__spill check__: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(2) .gt. max_absoru2_numset_i) then
      write(lll,*) 'read_absoru2(): numset(2) = ', absoru2_numset(2), &
       ' exceeded maximum of', max_absoru2_numset_i
      call pfant_halt(lll)
    end if


    ! NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
    ! POUR H,HE ET HE+
    ! PREMIERE LISTE POUR TH.LE.0.8  ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
    do ith = 1,2
      nset = absoru2_numset(ith)
      read (unit_,'(8f10.1)') (absoru2_wi(i,ith),i=1,nset)
    end do

  end


  !================================================================================================================================
  !> Reads single record from file modeles.mod into variables modeles_*
  !>
  !> SI L ON DESIRE IMPOSER UN MODELE ON MET EN INUM LE NUM DU MODELE
  !> SUR LE FICHIER ACCES DIRECT
  !>
  !> Depends on main_INUM
  !> @todo ISSUE: depends on other main_* but I think not for long

  subroutine read_modele(filename)
    implicit none
    integer unit_
    parameter(unit_=199)
    character(len=*) :: filename
    real*8 ddt, ddg, ddab
    integer i, &
            id_   !> @todo This could well be an input parameter, because it wouldn't have to rely on main.dat and would become MUCH more flexible
    character*92 lll


    open(unit=unit_, access='direct',status='old', file=filename, recl=1200)

    id_ = 1

    !> @todo better to give error if main_INUM is not set
    !> @todo Check if FORTRAN initializes variables to zero automatically: can I rely on this??
    !> @todo Maybe implement variable main_FLAG to FLAG that main.dat has been read already
    if (main_inum .gt. 0) id_ = main_inum  ! Selects record number

    !> @todo ISSUE: Variable NHE read again from a different file!!!!!!!!! I decided to opt for modeles_NHE because it overwrites main_NHE
    !> (MT) ASSERT main_NHE == modeles_NHE


    read(unit_, rec=id_) modeles_ntot, modeles_detef, modeles_dglog, &
     modeles_dsalog, modeles_asalalf, modeles_nhe, modeles_tit, &
     modeles_tiabs
    if (modeles_ntot .eq. 9999) then
      !> @todo ISSUE perhaps I should check the condition that leads to this error
      call pfant_halt('Le modele desire ne est pas sur le fichier')
    end if


    !__spill check__: Checks if exceeds maximum number of elements allowed
    if (modeles_ntot .gt. max_modeles_ntot) then
      write(lll,*) 'read_modeles(): numset(1) = ',absoru2_numset(1), &
       ' exceeded maximum of', max_absoru2_numset_i
      call pfant_halt(lll)
    end if

    write(lll, *) 'modeles_detef', modeles_detef
    call log_debug(lll)
    write(lll, *) 'modeles_dglog', modeles_dglog
    call log_debug(lll)
    write(lll, *) 'modeles_dsalog', modeles_dsalog
    call log_debug(lll)

    ddt  = abs(main_teff-modeles_detef)
    ddg = abs(main_glog-modeles_dglog)
    ddab = abs(main_asalog-modeles_dsalog)


    !> @todo ISSUE: this seems to be some kind of error check, but will loop forever, better to place it outside, also because of dependence on main_* variables
    !> @todo ISSUE: I don't get this; it will keep looping forever??? Look at the original code. What should happen if one of these three conditions hold?? Actually, got this. These are really consistency checks  (MT) It is annoying for the user to know exactly the index of the model that they are using. The code could have a "search feature"


!~9	READ(18, REC=ID) NTOT,DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT,TITABS
!~	WRITE(6,105)DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT
!~        write(6,108) TIABS
!~	IF(NTOT.EQ.9999)   GO TO 6
!~	DDT  = ABS(TEFF-DETEF)
!~C	DDTA  = ABS(TETAEF-DETAEF)
!~	DDG = ABS(GLOG-DGLOG)
!~	DDAB = ABS(ASALOG-DSALOG)
!~	DDHE= ABS(NHE-DNHE)
!~C	DDIF = DDTA+DDG+DDAB+DDHE
!~5	IF(DDT.GT.1.0)   GO TO 9
!~	IF(DDG.GT.0.01)   GO TO 9
!~	IF(DDAB.GT.0.01)   GO TO 9


    !> @todo ABS(main_NHE - modeles_NHE) <= 0.01     <--- this is the epsilon
    !> @todo Get the epsilon from MT
    if(ddt .gt. 1.0) then
      write(lll,*) 'abs(main_teff-modeles_detef) = ', ddt, ' > 1.0'
      call pfant_halt(lll)
    end if
    if(ddg .gt. 0.01) then
      write(lll,*) 'abs(main_glog-modeles_dglog) = ', ddg, ' > 0.01'
      call pfant_halt(lll)
    end if
    if(ddab .gt. 0.01) then
      write(lll,*) 'abs(main_asalog-modeles_dsalog) = ', ddab, ' > 0.01'
      call pfant_halt(lll)
    end if

    read(unit_, rec=id_) bid, &
         (modeles_nh(i), &
          modeles_teta(i), &
          modeles_pe(i), &
          modeles_pg(i), &
          modeles_t5l(i), i=1,modeles_ntot)

    close(unit_)
  end
end module read_files

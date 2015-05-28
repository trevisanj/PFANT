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

!> Most of file reading routines.
!>
!> This module contains most of the file reading routines
!> @sa filetoh, molecula
!> @todo create REAL*4 temp locals, then transfer to REAL*8 ones
!>
!> Note that all reading routines expect a @c filename parameter.
!> @todo create a "File reading" page to explain the situation of infile:main, dissoc.dat etc referring to a concept rather than a file of that name.

module read_files
  use logging
  use misc
  implicit none

      !=====
      ! Parameters
      !=====

      integer, parameter :: MAX_MAIN_FILETOH_NUMFILES=30   !< Maximum number of "filetoh" files


      !> Maximum number of metal rows in dissoc.dat
      !> (number of elements actually used is specified by variable
      !> dissoc_nmetal <= MAX_DISSOC_NMETAL)
      integer, parameter :: MAX_DISSOC_NMETAL=50
      !> Maximum number of molecule rows in dissoc.dat
      !> @todo ISSUE: bit overdimensioned?? (considering the file has only about 30 molecules)
      integer, parameter:: MAX_DISSOC_NMOL=600
      !> Maximum atomic number that can be found in dissoc.dat
      integer, parameter :: MAX_Z = 100
      !> Maximum possible value of modeles_ntot
      integer, parameter :: MAX_MODELES_NTOT=50
      !> Maximum number of abundances in abonds.dat
      integer, parameter :: MAX_ABONDS_NABOND=100
      integer, parameter :: &
       MAX_ATOMGRADE_R_NBLEND=13000, & !< Half of the maximum the number of rows in infile:atomgrade
       MAX_ATOMGRADE_NBLEND=8000      !< Maximum number of spectral lines possible within the interval LZERO, LFIN
      !> Maximum number of "items" in infile:partit:
      !> @li Maximum value for partit_npar
      !> @li Second dimension of partit_tabu
      integer, parameter :: MAX_PARTIT_NPAR=85
      !> Third dimension of partit_TABU
      integer, parameter :: MAX_PARTIT_KMAX=63
      !> Maximum value for absoru2_NM
      integer, parameter :: MAX_ABSORU2_NM=30
      !> Maximum value for absoru2_NR(J)
      integer, parameter :: MAX_ABSORU2_NRR=9
      !> Maximum value for each element of absoru2_NUMSET
      integer, parameter :: MAX_ABSORU2_NUMSET_I=41


      !=====
      ! Variables filled by read_dissoc() (file dissoc.dat)
      !=====

      ! dissoc.dat, metals part
      integer dissoc_nmetal !< number of elements considered in chemical equilibrium
      integer dissoc_nimax  !< maximum number of iterations in Newton-Rapson method
      character*2 dissoc_elems(MAX_DISSOC_NMETAL)    !< Elements table field 1/6: element symbol
      integer     dissoc_nelemx(MAX_DISSOC_NMETAL)   !< Elements table field 2/6: atomic number
      real*8      dissoc_ip(MAX_DISSOC_NMETAL)      !< Elements table field 3/6: ?
      integer     dissoc_ig0(MAX_DISSOC_NMETAL),  & !< Elements table field 4/6: ?
                  dissoc_ig1(MAX_DISSOC_NMETAL)     !< Elements table field 5/6: ?
      real*8      dissoc_cclog(MAX_DISSOC_NMETAL)   !< Elements table field 6/6: ?

      ! dissoc.dat, molecules part
      character*3 dissoc_mol(MAX_DISSOC_NMOL)     !< Molecules table field 01: molecule name
      real*8 dissoc_c(MAX_DISSOC_NMOL, 5)         !< Molecules table fields 02-06
      integer dissoc_mmax(MAX_DISSOC_NMOL), &     !< Molecules table field 07
              dissoc_nelem(5, MAX_DISSOC_NMOL), & !< Molecules table fields 08, 10, ...
              dissoc_natom(5, MAX_DISSOC_NMOL)    !< Molecules table fields 09, 11, ...
      integer dissoc_nmol
      real*8 dissoc_eps,   & !< if abs((x(i+1)-x(i))/x(i)) .le. eps: converged
             dissoc_switer   !< flag affecting x(i+1)
                             !< @li if switer .gt. 0 then x(i+1)=0.5*(x(i+1)+x(i))
                             !< @li if switer .le. 0 then x(i+1)=x(i+1)


      !=====
      ! Variables filled by read_main() (file infile:main)
      !=====
      character*64 main_fileflux  !< ?doc?
      logical   main_ecrit_obsolete, & !< obsolete (old verbose flag)
                main_ptdisk    !< ?doc?
      real*8    main_pas,    & !< ?doc?
                main_echx,   & !< ?doc?
                main_echy,   & !< ?doc?
                main_fwhm,   & !< ?doc?
                main_mu,     & !< ?doc?
                main_afstar, & !< ?doc?
                main_llzero, & !< ?doc?
                main_llfin,  & !< ?doc?
                main_aint,   & !< ?doc?
                main_teff,   & !< ?doc?
                main_glog,   & !< ?doc?
                main_asalog, & !< ?doc?
                main_nhe       !< ?doc?
      integer   main_ivtot, & !< affects turbul() issue ?what? ?doc?
                              !< = 1 -- "vt" constant
                              !< > 1 -- "vt" variable
                main_inum !< ?doc?
      character*64 main_filetohy(MAX_MAIN_FILETOH_NUMFILES) !< ?doc? Names of ten outputs files that will contain ???
      integer   main_filetoh_numfiles !< number of valid elements within filetohy

      character*10 main_titrav(20) !< ?doc?
      real*8 :: main_vvt(MAX_MODELES_NTOT), & !< ?doc?
                main_tolv(MAX_MODELES_NTOT)   !< ?doc?
      !> This vector goes along with dissoc_ELEMS and dissoc_NELEMX
      !> @todo would go better as a column in dissoc.dat or eliminated
      real*8 :: main_xxcor(MAX_DISSOC_NMETAL) !< ?doc?


      !=====
      ! Variables filled by read_abonds() (file abonds.dat)
      !=====
      integer abonds_nabond !< ?doc?
      character*2 abonds_ele(MAX_ABONDS_NABOND)     !< ?doc?
      real*8      abonds_abol(MAX_ABONDS_NABOND), & !< ?doc?
                  abonds_abo(MAX_ABONDS_NABOND)     !< ?doc? This is calculated


      !=====
      ! Variables filled by read_atomgrade() (file infile:atomgrade)
      !=====
      ! infile:atomgrade, file originals
      integer atomgrade_r_nblend !< ?doc?
      character*2 atomgrade_r_elem(MAX_ATOMGRADE_R_NBLEND) !< ?doc?
      integer, dimension(MAX_ATOMGRADE_R_NBLEND) :: &
       atomgrade_r_ioni !< ?doc?
      real*8, dimension(MAX_ATOMGRADE_R_NBLEND) :: &
       atomgrade_r_lambda,       & !< ?doc?
       atomgrade_r_kiex,         & !< ?doc?
       atomgrade_r_algf,         & !< ?doc?
       atomgrade_r_ch,           & !< ?doc?
       atomgrade_r_gr,           & !< ?doc?
       atomgrade_r_ge,           & !< ?doc?
       atomgrade_r_zinf,         & !< ?doc?
       atomgrade_r_abondr_dummy, & !< ?doc?
       atomgrade_r_abonds_abo  !< will be filled by "inner join" by searching
                              !< atomgrade_r_elem through abonds_ele

      ! infile:atomgrade, filtered variables
      ! Very similar to above; differences are
      ! - single underscore
      ! - additional variable "gf", which equals 10**algf
      integer atomgrade_nblend !< ?doc?
      character*2 atomgrade_elem(MAX_ATOMGRADE_NBLEND) !< ?doc?
      integer, dimension(MAX_ATOMGRADE_NBLEND) :: &
       atomgrade_ioni !< ?doc?
      real*8, dimension(MAX_ATOMGRADE_R_NBLEND) :: &
       atomgrade_lambda,       & !< ?doc?
       atomgrade_kiex,         & !< ?doc?
       atomgrade_algf,         & !< ?doc?
       atomgrade_ch,           & !< ?doc?
       atomgrade_gr,           & !< ?doc?
       atomgrade_ge,           & !< ?doc?
       atomgrade_zinf,         & !< ?doc?
       atomgrade_abondr_dummy, & !< ?doc?
       atomgrade_gf,           & !< ?doc?
       atomgrade_abonds_abo      !< ?doc?


      !=====
      ! Variables filled by read_partit() (file infile:partit)
      !=====
      character*2, dimension(MAX_PARTIT_NPAR) :: &
       partit_el !< ?doc?
      integer partit_npar !< ?doc?

      real*8, dimension (MAX_PARTIT_NPAR) :: &
       partit_ki1,  & !< ?doc?
       partit_pa,   & !< ?doc?
       partit_m,    & !< ?doc?
       partit_tini, & !< ?doc?
       partit_ki2
      integer, dimension (MAX_PARTIT_NPAR) :: &
       partit_jkmax !< ?doc?

      real*8, dimension (MAX_PARTIT_NPAR, 3, MAX_PARTIT_KMAX) :: &
       partit_tabu !< ?doc?


      !=====
      ! Variables filled by read_absoru2() (file infile:absoru2)
      !=====
      integer absoru2_nm,    & !< NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISATION
              absoru2_nmeta, & !< NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
              absoru2_numset(2) !< NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
                                !< POUR H,HE ET HE+

      character*2 :: absoru2_nomet(MAX_ABSORU2_NM) !< NOM DE L'ELEMENT; not used
      real*8 absoru2_abmet, & !< ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
             absoru2_abhel    !< ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
      !> @todo ISSUE: i am not sure about these character*4, made this from the format below
      character*4 absoru2_titre(17) !< ?doc?

      !> Two possibilities
      !> @li IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
      !> @li IUNITE=' NOYAU H' SI ON VEUT CALCULER KAPPA PAR NOYAU D'HYDROGENE
      character*4 absoru2_iunite(2)

      integer, dimension(MAX_ABSORU2_NM) :: &
       absoru2_nr !< NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
      real*8, dimension(MAX_ABSORU2_NM) :: &
       absoru2_zp, & !< ZP=NBR. D'ABONDANCE DE L'ELEMENT
       absoru2_zm    !< ZM=POIDS MOLECULAIRE DE L'ELEMENT
      real*8, dimension(MAX_ABSORU2_NM, MAX_ABSORU2_NRR) :: &
        absoru2_xi,&!< XI(J,I) = POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATION
        absoru2_pf  !< PF(J,I) = FONCTION DE PARTITION DE L'ELEMENT J AU STADE D'IONISATION
      real*8 absoru2_wi(MAX_ABSORU2_NUMSET_I, 2) !< ?doc?





      !=====
      ! Variables filled by READ_MODELE() (file infile:modeles)
      !=====
      !> @todo (MT) infile:modeles could become an ASCII file

      ! Attention: one has to specify sizes of all the variables here, because
      ! this may change with compiler
      integer modeles_ntot !< ?doc?
      character*4 modeles_tit(5) !< ?doc?
      character*20 modeles_tiabs !< ?doc? I just want to see this string at testing
      real*8 modeles_detef,   & !< ?doc?
             modeles_dglog,   & !< ?doc?
             modeles_dsalog,  & !< ?doc?
             modeles_asalalf, & !< ?doc?
             modeles_nhe        !< ?doc?
      real*8, dimension(MAX_MODELES_NTOT) :: &
       modeles_nh,   & !< ?doc?
       modeles_teta, & !< ?doc?
       modeles_pe,   & !< ?doc?
       modeles_pg,   & !< ?doc?
       modeles_t5l     !< ?doc?



      !=====
      ! Private variables (internal function)
      !=====

      ! Flags indicating whether corresponding routine has already been called.
      ! There is a strict order in which files must be read, because there are
      ! consistency checks and inner joins being percormed while reading files.
      ! At the moment, this order is reinforced:
      ! read_dissoc -> read_main -> read_abonds -> read_atomgrade
      logical, private :: &
       flag_read_abonds = .false., & !< Whether read_abonds() has already been called
       flag_read_main   = .false., & !< Whether read_main() has already been called
       flag_read_dissoc = .false.    !< Whether read_dissoc() has already been called


      SAVE
CONTAINS

  !================================================================================================================================
  !> Reads infile:main to fill variables main_*
  !>
  !> @note Must be called after read_dissoc().
  !> @todo ISSUE: VVT(1) is used throughout the program, so why reading VVT(I)??
  !> @todo ISSUE Explain this part VERY WELL because it is an "anomaly", i.e., infile:main will have MORE LINES
  !> @todo ISSUE Documentation
  !> (MT) Yes it makes sense, it specifies microturbulence velocities for each layer of the atmosphere

  subroutine read_main(filename)
    implicit none
    integer unit_
    parameter(unit_=4)
    character(len=*), intent(in) :: filename
    integer ih, i
    character*256 lll
    character*256 filetoh_temp

    if (.not. flag_read_dissoc) then
      call pfant_halt('read_dissoc() must be called before read_main()')
    end if

    open(unit=unit_,file=filename, status='old')

    ! row 01: object name, e.g. "sun"
    read(unit_, '(20a)') main_titrav

    ! row 02
    read(unit_, *) main_ecrit_obsolete, main_pas, main_echx, main_echy, main_fwhm

    ! row 03
    read(unit_, *) main_vvt(1)
    main_ivtot = 1

    ! rows 03.(1-3): (three conditional rows that MUST exist if and only if main_VVT(1) > 900) ?doc?
    if(main_vvt(1) .gt. 900)  then   ! vt variable avec la profondeur
      read(unit_, *) main_ivtot
      ! ivtot, affects subroutine turbul() issue ?what? ?doc?
      if (main_ivtot .gt. MAX_MODELES_NTOT) then
        write (lll, *) 'main_ivtot .gt. MAX_MODELES_NTOT (', &
         main_ivtot, ' .gt. ', MAX_MODELES_NTOT, ')'
         call pfant_halt(lll)
      end if

      read(unit_,*) (main_tolv(i), i=1, main_ivtot)
      read(unit_,*) (main_vvt(i) ,i=1, main_ivtot)
    end if

    ! row 04
    read(unit_, *) main_teff, main_glog, main_asalog, main_nhe,main_inum

    ! row 05
    read(unit_, *) main_ptdisk, main_mu

    ! row 06
    read(unit_, *) main_afstar  ! metallicity of the star (in log scale)

    ! row 07: XXCOR(i)
    ! @todo ISSUE: Should be a column in dissoc.dat !!!!!
    ! (MT) I agree
    read(unit_, *)(main_xxcor(i), i=1, dissoc_nmetal)

    ! row 08 -- part of a filename
    ! This line will define the names of three output files:
    !   FILEFLUX.cont
    !   FILEFLUX.norm
    !   FILEFLUX.spec
    read(unit_, '(a)') main_fileflux

    ! row 09
    read(unit_, *) main_llzero, main_llfin, main_aint

    write(*,*) main_llzero, main_llfin, main_aint

    ! row 10 - ....
    ! Considers the remaining rows as the filetoh file names
    ! Doesn't know yet the number of files
    ih = 1
    110 continue
    read(unit_, '(a)', end=111) filetoh_temp

    if (len_trim(filetoh_temp) .eq. 0) goto 110  ! skips blank rows

    !__spill check__
    if (ih .gt. MAX_MAIN_FILETOH_NUMFILES) then
      write(lll,*) 'Too many filetoh files specified (maximum is', MAX_MAIN_FILETOH_NUMFILES, ')'
      call pfant_halt(lll)
    end if

    main_filetohy(ih) = filetoh_temp

    write(lll,*) 'filetohy(', ih, ') = "', trim(main_filetohy(ih)), '"'
    call log_debug(lll)

    ih = ih+1
    goto 110

    111 continue
    main_filetoh_numfiles = ih-1

    write(lll,*) 'Number of filetoh files: ', main_filetoh_numfiles
    call log_debug(lll)

    close(unit=unit_)
    flag_read_main = .true.
  end




  !================================================================================================================================
  !> Reads dissoc.dat to fill variables dissoc_*
  !>
  !> @attention This file must end with a <b>blank row</b> so that the routine can detect
  !> the end of the file.

  !> @todo Various tests:
  !> @todo - mismatched NMETAL and metal rows
  !> @todo - check if NMETAL and NMOL match what they are supposed to (assertions in test)
  !> @todo ISSUE: This 1X does not appear in my sample dissoc.dat file
  !> @todo ISSUE: Atually the file that Beatriz sent me does not work under this format!!!!
  !> @todo ISSUE: THere is no 1X
  !<      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
  !> @todo PROPOSE: use READ()'s "END=" option

  subroutine read_dissoc(filename)
    integer unit_
    integer i, j, k, m, mmaxj
    parameter(unit_=199)
    character(len=*) :: filename
    character*128 lll
    character*2 symbol
    logical flag_found

    ! Auxiliary temp variables for reading file
    integer*4 natomm, nelemm
    dimension natomm(5), nelemm(5)

    open(unit=unit_,file=filename, status='old')

    ! row 01
    read(unit_,'(2i5, 2f10.5, i10)') dissoc_nmetal, dissoc_nimax, dissoc_eps, dissoc_switer

    ! rows 02 to NMETAL+1: 6-column rows
    do i = 1, dissoc_nmetal
      read (unit_, '(a2, 2x, i6, f10.3, 2i5, f10.5)') &
       symbol, dissoc_nelemx(i), dissoc_ip(i), &
       dissoc_ig0(i), dissoc_ig1(i), dissoc_cclog(i)

        ! makes sure that elements first and second are h and he, respectively,
        ! because sat4() and die() count on this
        select case (i)
          case (1)
            if (to_lower(trim(symbol)) .ne. 'h') then
              write(lll,*) 'First element must be hydrogen ("H"), not "', symbol, '"!'
              write(*,*) lll
              call pfant_halt(lll)
            end if
          case (2)
            if (to_lower(symbol) .ne. 'he') then
              write(lll,*) 'First element must be helium ("He"), not "', symbol, '"!'
              write(*,*) lll
              call pfant_halt(lll)
            end if
        end select

        dissoc_elems(i) = symbol

        !__spill check__
        if (dissoc_nelemx(i) .gt. MAX_Z) then
          write(lll,*) 'read_dissoc(): metal # ', i, ': nelemxi = ', &
           dissoc_nelemx(i), ' over maximum allowed (', &
           MAX_Z, ')'
          call pfant_halt(lll)
        end if

      end do



      ! rows NMETAL+2 till end-of-file
      !   col  1     -- "name" of molecule
      !   cols 2-6   -- c(J, 1-5)
      !   col  7     -- mmax(j) (number of subsequent columns)/2
      !   cols 8-... -- maximum of 8 columns here.
      !                 pairs (nelem(m), natom(m)), m = 1 to mmax(j)
      j = 0

      1010 continue
      j = j+1


      read(unit_, '(a3, 5x, e11.5, 4e12.5, i1, 4(i2,i1))') &
                   dissoc_mol(j), &
                   (dissoc_c(j, k), k=1,5), &
                   dissoc_mmax(j), &
                   (nelemm(m), natomm(m), m=1,4)


      mmaxj = dissoc_mmax(j)
      if(mmaxj .eq. 0) then
        ! note: interesting that Fortran accepts a blank line and reads everything blank or zero

        go to 1014  ! means end-of-file
      end if

      !__consistency check__:
      if (mmaxj .gt. 4) then
        write(lll,*) 'read_dissoc() molecule "', dissoc_mol(j), &
         '", mmaxj = ', mmaxj, ' cannot be greater than 4!'
        call pfant_halt(lll)
      end if

      !__consistency check__
      do m = 1, mmaxj
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

    write(lll,*) 'Last molecule considered in dissoc file is ', dissoc_mol(dissoc_nmol)
    call log_debug(lll)

    close(unit=unit_)
    flag_read_dissoc = .true.
  end




  !================================================================================================================================
  !> Reads file abonds.dat to fill variables abonds_*
  !>
  !> In addition, searches elements in dissoc atoms table to take XXCOR values.
  !>
  !> The input file has 3 columns:
  !> @li Empty space or "1"
  !> @li 2-character atomic element symbol (?)
  !> @li absolute abundance (a) (exponent; actual abundance is 10^a), unit "dex"
  !>
  !> @attention The end of the file is signalled by two rows containing only "1" each at
  !> column 1 and nothing else at the others, i.e.,
  !> @verbatim
  !> .......(last data row).......
  !> 1
  !> 1
  !> @endverbatim
  !>
  !> @todo Test one row!!!
  !> @todo use READ()'s "END=" option

  subroutine read_abonds(filename)
    implicit none
    integer unit_, finab, k, j
    parameter(unit_=199)
    character(len=*) :: filename
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

        if (flag_found) then
          abonds_abol(j) = abonds_abol(j)+main_xxcor(k)
        else
          ! if not found, doesn't bother
        end if


        ! [2] Calculates abonds_ABO based on abonds_ABOL

        !> @todo ISSUE ask MT Why "-12" ?? (MT) By definition, the "abundance of element X" is given by (X/H) = log10(NX/NH) - 12

        !> @note What is in abonds.dat is log10(NX/NH) - [Fe/H]

        !> @note What is in dissoc.dat is log10(NX/NH) - 12 - [Fe/H]

        !> @todo issue abundances in abonds.dat and dissoc.dat correspond to (X/H) - [Fe/H]. The purpose is to [X/Fe] = 0.0

        !> @todo issue dissoc.dat has redundant information: all abundances are these but with 12 subtracted and they should match!
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
  !> Reads file infile:atomgrade to fill variables atomgrade_r_* (double underscore)
  !>
  !> Depends on abonds_*, so must be called after READ_ABONDS()
  !>
  !> This file has 2 types of alternating rows:
  !> @verbatim
  !>   odd row
  !>     col 1 -- 2-letter atomgrade_elem(K) ?doc?
  !>     col 2 -- atomgrade_ioni(k) ?doc?
  !>     col 3 -- atomgrade_lambda(K) ?doc?
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
  !>  @note The line that has the end-of-file flag set is also taken into account.

  !>
  !> @todo issue ask blb is it supposed to consider the last row? considering now. Ask about all files.
  !>
  !> @todo give error if blows MAX!!!!!!!!!!!!!!!!!
  !> @todo ISSUE: Cannot cope with empty file (tested), however there are if's inside the program: IF NBLEND .EQ. 0 .........
  !> (MT) use READ()'s "END=" option IS A GOOD IDEA, and we will do the following: stop reading EITHER when the "1" is found or when the file ends!
  !> @todo Assertions in test to see if numbers match what they are supposed to
  !> @todo use READ()'s "END=" option

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
    do while (.true.)
      !__spill check__: checks if exceeds maximum number of elements allowed
      if (k .gt. MAX_ATOMGRADE_R_NBLEND) then
        write(lll,*) 'read_atomgrade(): exceeded maximum of', MAX_ATOMGRADE_R_NBLEND, ' spectral lines'
        call pfant_halt(lll)
      end if

      read(unit_, '(a2, i1, 1x, f10.3)') atomgrade_r_elem(k), &
                                         atomgrade_r_ioni(k), &
                                         atomgrade_r_lambda(k)

      read(unit_, *) atomgrade_r_kiex(k), &
       atomgrade_r_algf(k), &
       atomgrade_r_ch(k), &
       atomgrade_r_gr(k), &
       atomgrade_r_ge(k), &
       atomgrade_r_zinf(k), &
       atomgrade_r_abondr_dummy(k), finrai

      !> @todo ISSUE ask MT Why this? Document!!! (MT) If the "radiative broadening" is zero,
      !> it is calculated as a function of lambda; otherwise, it is assumed that it has been inputted manually.
      !>
      !> @todo issue ask BLB 2.21e15 stand for? + reference
      if (atomgrade_r_gr(k) .lt. 1e-37) atomgrade_r_gr(k) = 2.21e15 / atomgrade_r_lambda(k)**2


      !> Besides reading the file, this routine searches atomgrade's element within abonds'
      !> elements and copies corresponding
      !> abonds_abo value into atomgrade_abonds_abo. Halts program if element not found.
      !>
      !> In database terminology, this is sort of a "inner join".
      !>
      !> @note Historically, this corresponds to old routine "abondraih". The search was
      !>   being carried out every time (lzero, lfin) changed and only for the filtered
      !>   atomgrade rows. I decided to perform this search for all atomgrade rows and
      !>   then filter atomgrade_abonds_abo together with other variables in
      !>   filter_atomgrade(), which is probably cheaper.
      !> @todo issue why name was "abondraih"?? did it mean "abundances of hydrogen lines"? i ask this because if it has really a physical meaning, i shouldn't bury this inside read_files.f

      flag_found = .false.
      do  j = 1, abonds_nabond
        if (abonds_ele(j) .eq. atomgrade_r_elem(k)) then
          flag_found = .true.
          exit
        end if
      end do
      if (.not. flag_found) then
        write(lll,*)  'read_atomgrade(): element "', atomgrade_r_elem(k), &
         ' (spectral line number ', k, ') cannot be found in abundance file'
        call pfant_halt(lll)
      end if
      atomgrade_r_abonds_abo(k) = abonds_abo(j)

      if (finrai .eq. 1) exit !__end-of-file__

      k = k+1
    end do

    atomgrade_r_nblend = k

    write(lll,*) 'read_atomgrade(): last line taken: element: "', atomgrade_r_elem(atomgrade_r_nblend), &
      '"; lambda: ', atomgrade_r_lambda(atomgrade_r_nblend)
    call log_debug(lll)


!> @todo ISSUE
!> MENTION: last atomic line wasn't being used!! (this has been fixed/changed). Original code commented in subroutine
!~  K=1
!~9 READ(14,103)ELEM(K),IONI(K),LAMBDA(K)
!~  READ(14,*) KIEX(K),ALGF(K),CH(K),GR(K),GE(K),ZINF(K),
!~  1 ABONDR(K),FINRAI
!~  write(34,103)ELEM(K),IONI(K),LAMBDA(K)
!~  GF(K)=10.**ALGF(K)
!~C        IF(K.EQ.1) GF(K)=10**AGGF
!~  IF(GR(K).LT.1E-37)   GR(K)=2.21E15 / LAMBDA(K)**2
!~  IF(FINRAI.EQ.1) GO TO 10
!~  IF(((LAMBDA(K).GT.LFIN).OR.(LAMBDA(K).LT.LZERO))) GO TO 205
!~  K=K+1
!~205 CONTINUE
!~  GO TO 9
!~10  NBLEND=K-1

    close(unit=unit_)

    return

  end


  !================================================================================================================================
  !> Selects only spectral lines within range lzero, lfin + performs "inner join".
  !>
  !> Populates variables atomgrade_* (single underscore)

  subroutine filter_atomgrade(lzero, lfin)
    !> Lower edge of wavelength interval
    real*8, intent(in) :: lzero
    !> Upper edge of wavelength interval
    real*8, intent(in) :: lfin
    integer j, k
    character*192 lll

    k = 0
    do j = 1, atomgrade_r_nblend
      if((atomgrade_r_lambda(j).le.lfin) .and. (atomgrade_r_lambda(j) .ge. lzero)) then
        k = k+1


        !__spill check__: checks if exceeds maximum number of elements allowed
        if (k .gt. MAX_ATOMGRADE_NBLEND) then
          write(lll,*) 'filter_atomgrade(): exceeded maximum of', &
           MAX_ATOMGRADE_NBLEND, ' spectral lines'
          call pfant_halt(lll)
        end if

        !Filters in
        atomgrade_elem(k)   = atomgrade_r_elem(j)
        atomgrade_ioni(k)   = atomgrade_r_ioni(j)
        atomgrade_lambda(k) = atomgrade_r_lambda(j)
        atomgrade_kiex(k)   = atomgrade_r_kiex(j)
        atomgrade_algf(k)   = atomgrade_r_algf(j)
        atomgrade_gf(k)     = 10.**atomgrade_r_algf(j)
        atomgrade_ch(k)     = atomgrade_r_ch(j)
        atomgrade_gr(k)     = atomgrade_r_gr(j)
        atomgrade_ge(k)     = atomgrade_r_ge(j)
        atomgrade_zinf(k)   = atomgrade_r_zinf(j)

        atomgrade_abonds_abo(k) = atomgrade_r_abonds_abo(j)

        atomgrade_abondr_dummy(k) = atomgrade_r_abondr_dummy(j)

      end if
    end do

    atomgrade_nblend = k

  end


  !================================================================================================================================
  !> Reads file infile:partit to fill variables partit_*
  !>
  !> LECTURE DES FCTS DE PARTITION
  !>
  !> Rows in this file alternate between:
  !> @verbatim
  !> 1) 8-column row
  !>    col 8 -- signals end-of-file. If 1, it ignores the row and
  !>             stops reading
  !>
  !> 2) Series of rows to fill in partit_TABU(J, :, :)
  !> @endverbatim

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
        if (j .gt. MAX_PARTIT_NPAR) then
          write(lll,*) 'read_partit(): par exceeded maximum of ', MAX_PARTIT_NPAR
          call pfant_halt(lll)
        end if


        kmax = partit_jkmax(j)

        !__spill check__: checks if exceeds maximum number of elements allowed
        if (kmax .gt. MAX_PARTIT_KMAX) then
          write(lll,*) 'read_partit(): par number', j, 'kmax=', kmax, &
           ' exceeded maximum of', MAX_PARTIT_KMAX
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
  !> Reads file infile:absoru2 to fill variables absoru2_*
  !>
  !> Attention: variables absoru2_ZP, absoru2_XI, and absoru2_PF
  !>            undergo transformation!
  !>
  !> @note Historically, this routine is in essence the old routine called "LECTUR"
  !>
  !> CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
  !> PUIS LES DONNEES CORRESPONDANTS AUX ABSORBANTS METALLIQUES SI NECE
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
    if (absoru2_nm .gt. MAX_ABSORU2_NM) then
      write(lll,*) 'read_absoru2(): nm=', absoru2_nm, ' exceeded maximum of', MAX_ABSORU2_NM
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
      if (nrr .gt. MAX_ABSORU2_NRR) then
        write(lll,*) 'read_absoru2(): j = ', j, 'nr=', nrr, ' exceeded maximum of', MAX_ABSORU2_NRR
        call pfant_halt(lll)
      end if

      do i = 1, nrr
        ! neant="nothing"
        ! NION is also not used
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
    if (absoru2_numset(1) .gt. MAX_ABSORU2_NUMSET_I) then
      write(lll,*) 'read_absoru2(): numset(1) = ', absoru2_numset(1), &
       ' exceeded maximum of', MAX_ABSORU2_NUMSET_I
      call pfant_halt(lll)
    end if
    !__spill check__: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(2) .gt. MAX_ABSORU2_NUMSET_I) then
      write(lll,*) 'read_absoru2(): numset(2) = ', absoru2_numset(2), &
       ' exceeded maximum of', MAX_ABSORU2_NUMSET_I
      call pfant_halt(lll)
    end if


    ! NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
    ! POUR H,HE ET HE+
    ! PREMIERE LISTE POUR TH.LE.0.8 ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
    do ith = 1,2
      nset = absoru2_numset(ith)
      read (unit_,'(8f10.1)') (absoru2_wi(i,ith),i=1,nset)
    end do

  end


  !================================================================================================================================
  !> Reads single record from file infile:modeles into variables modeles_*
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
            id_   !> @todo This could well be an input parameter, because it wouldn't have to rely on infile:main and would become MUCH more flexible
    character*128 lll


    ! Variables declared only for reading from binary file, then their values will be copied to modeles_*
    ! Same names as modeles_* but have the "r_" prefix.
    integer*4 r_modeles_ntot
    real*4 r_modeles_detef,   &
           r_modeles_dglog,   &
           r_modeles_dsalog,  &
           r_modeles_asalalf, &
           r_modeles_nhe,     &
           bid(16)
    real*4, dimension(MAX_MODELES_NTOT) :: &
     r_modeles_nh,   &
     r_modeles_teta, &
     r_modeles_pe,   &
     r_modeles_pg,   &
     r_modeles_t5l




    open(unit=unit_, access='direct',status='old', file=filename, recl=1200)

    id_ = 1

    !> @todo better to give error if main_INUM is not set
    !> @todo Check if FORTRAN initializes variables to zero automatically: can I rely on this??
    !> @todo Maybe implement variable main_FLAG to FLAG that infile:main has been read already
    if (main_inum .gt. 0) id_ = main_inum  ! Selects record number


    read(unit_, rec=id_) r_modeles_ntot, r_modeles_detef, r_modeles_dglog, &
     r_modeles_dsalog, r_modeles_asalalf, r_modeles_nhe, modeles_tit, &
     modeles_tiabs  ! Note that modeles_tit and modeles_titabs are read directly because they are chars
    if (r_modeles_ntot .eq. 9999) then
      !> @todo ISSUE perhaps I should check the condition that leads to this error
      call pfant_halt('Le modele desire ne est pas sur le fichier')
    end if


    !__spill check__: Checks if exceeds maximum number of elements allowed
    if (r_modeles_ntot .gt. MAX_MODELES_NTOT) then
      write(lll,*) 'read_modele(): modeles_ntot = ',r_modeles_ntot, &
       ' exceeded maximum of', MAX_MODELES_NTOT
      call pfant_halt(lll)
    end if

    write(lll, *) 'modeles_ntot', r_modeles_ntot
    call log_debug(lll)
    write(lll, *) 'modeles_detef', r_modeles_detef
    call log_debug(lll)
    write(lll, *) 'modeles_dglog', r_modeles_dglog
    call log_debug(lll)
    write(lll, *) 'modeles_dsalog', r_modeles_dsalog
    call log_debug(lll)

    ddt  = abs(main_teff-r_modeles_detef)
    ddg = abs(main_glog-r_modeles_dglog)
    ddab = abs(main_asalog-r_modeles_dsalog)


    !> @todo ISSUE: this seems to be some kind of error check, but will loop forever, better to place it outside, also because of dependence on main_* variables
    !> @todo ISSUE: I don't get this; it will keep looping forever??? Look at the original code. What should happen if one of these three conditions hold?? Actually, got this. These are really consistency checks  (MT) It is annoying for the user to know exactly the index of the model that they are using. The code could have a "search feature"


!~9 READ(18, REC=ID) NTOT,DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT,TITABS
!~  WRITE(6,105)DETEF,DGLOG,DSALOG,ASALALF,NHE,TIT
!~        write(6,108) TIABS
!~  IF(NTOT.EQ.9999)   GO TO 6
!~  DDT  = ABS(TEFF-DETEF)
!~C DDTA  = ABS(TETAEF-DETAEF)
!~  DDG = ABS(GLOG-DGLOG)
!~  DDAB = ABS(ASALOG-DSALOG)
!~  DDHE= ABS(NHE-DNHE)
!~C DDIF = DDTA+DDG+DDAB+DDHE
!~5 IF(DDT.GT.1.0)   GO TO 9
!~  IF(DDG.GT.0.01)   GO TO 9
!~  IF(DDAB.GT.0.01)   GO TO 9

    !> @todo Get the proper (abs(main_nhe-modeles_nhe) .gt. 0.001) epsilon from MT

      !__consistency check__
    if (abs(main_nhe-r_modeles_nhe) .gt. 0.001) then
      write(lll, *) 'modeles nhe (', r_modeles_nhe, ') does not match main nhe (', main_nhe, ')'
      call pfant_halt(lll)
    end if
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
         (r_modeles_nh(i), &
          r_modeles_teta(i), &
          r_modeles_pe(i), &
          r_modeles_pg(i), &
          r_modeles_t5l(i), i=1,r_modeles_ntot)
    close(unit_)

    ! ready to copy (& convert) variables to their counterparts
    modeles_ntot    = r_modeles_ntot     ! integer(4)-to-integer(?)
    modeles_detef   = r_modeles_detef    ! real(4) to real(8)
    modeles_dglog   = r_modeles_dglog    ! "
    modeles_dsalog  = r_modeles_dsalog   ! "
    modeles_asalalf = r_modeles_asalalf  ! "
    modeles_nhe     = r_modeles_nhe      ! "
    do i = 1, modeles_ntot
      modeles_nh(i)   = r_modeles_nh(i)   ! real(4) to real(8)
      modeles_teta(i) = r_modeles_teta(i) ! "
      modeles_pe(i)   = r_modeles_pe(i)   ! "
      modeles_pg(i)   = r_modeles_pg(i)   ! "
      modeles_t5l(i)  = r_modeles_t5l(i)  ! "
    end do
  end
end module read_files

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

!> Most of file reading routines.
!>
!> This module contains most of the file reading routines
!> @sa filetoh, molecules
!> @todo create REAL*4 temp locals, then transfer to REAL*8 ones
!>
!> @todo create a "File reading" page to explain the situation of infile:main, dissoc.dat etc referring to a concept rather than a file of that name.

module read_most_files
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
  !> Maximum number of "items" in infile:partit:
  !> @li Maximum value for partit_npar
  !> @li Second dimension of partit_tabu
  integer, parameter :: MAX_PARTIT_NPAR=85
  !> Third dimension of partit_TABU
  integer, parameter :: MAX_PARTIT_KMAX=63
  !> Maximum value for absoru2_NM
  integer, parameter :: MAX_ABSORU2_NM=30
  !> Maximum value for absoru2_NR(J)
  integer, parameter :: MAX_ABSORU2_NRR=9  !> Maximum value for each element of absoru2_NUMSET
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
  logical   main_ptdisk    !< ?doc?
  real*8    main_pas,    & !< ?doc?
            main_echx,   & !< ?doc?
            main_echy,   & !< ?doc?
            main_fwhm,   & !< not used
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
            main_inum     !< record id within modeles.mod
  character*64 main_filetohy(MAX_MAIN_FILETOH_NUMFILES) !< ?doc? Names of ten outputs files that will contain ???
  integer   main_filetoh_numfiles !< number of valid elements within filetohy

  character*15 main_titrav                !< Title, e.g. "Sun"
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

  !> Structure to store atmospheric model read from binary file.
  !> Binary file follows "NewMarcs" structure containing real numbers stored as real*4.
  !> Hence the real*4 declarations.

  type modele_record
    integer*4 :: ntot

    real*4 :: teff, glog, asalog, asalalf, nhe

    real*4, dimension(MAX_MODELES_NTOT) :: &
     nh, teta, pe, pg, t5l

    character*20 :: tit, tiabs
  end type



  integer modeles_ntot !< ?doc?
  character*20 modeles_tit   !< titre du modele interpolé
  character*20 modeles_tiabs !< ?doc? I just want to see this string at testing
  real*8 modeles_teff,   & !< ?doc?
         modeles_glog,   & !< ?doc?
         modeles_asalog,  & !< ?doc?
         modeles_asalalf, & !< ?doc?
         modeles_nhe        !< ?doc?
  real*8, dimension(MAX_MODELES_NTOT) :: &
   modeles_nh,   & !< ?doc?
   modeles_teta, & !< ?doc?
   modeles_pe,   & !< ?doc?
   modeles_pg,   & !< ?doc?
   modeles_t5l     !< ?doc?


  ! Flags indicating whether corresponding routine has already been called.
  ! There is a strict order in which files must be read, because there are
  ! consistency checks and inner joins being percormed while reading files.
  ! At the moment, this order is reinforced:
  ! read_dissoc -> read_main -> read_abonds -> read_atomgrade
  logical :: &
   flag_read_abonds = .false., & !< Whether read_abonds() has already been called
   flag_read_main   = .false., & !< Whether read_main() has already been called
   flag_read_dissoc = .false.    !< Whether read_dissoc() has already been called


   character*256, private :: lll
  save
contains

  !=======================================================================================
  !> Reads infile:main to fill variables main_*
  !>
  !> @note Must be called after read_dissoc().
  !> @todo ISSUE: VVT(1) is used throughout the program, so why reading VVT(I)??
  !> @todo ISSUE Explain this part VERY WELL because it is an "anomaly", i.e., infile:main will have MORE LINES
  !> @todo ISSUE Documentation
  !> (MT) Yes it makes sense, it specifies microturbulence velocities for each layer of the atmosphere

  subroutine read_main(path_to_file, flag_care_about_dissoc)
    character(len=*), intent(in) :: path_to_file
    !> Defaults to .true. If .false., won't bother about reading infile:dissoc first and
    !> will skip the xxcor line. Only pfant bother about the xxcor line; other executables
    !> want to use infile:main without having to read infile:dissoc first.
    logical, intent(in), optional :: flag_care_about_dissoc
    integer, parameter :: UNIT_ = 4
    integer ih, i
    character*64 filetoh_temp
    logical :: fcad = .true.
    logical bid

    if (present(flag_care_about_dissoc)) fcad = flag_care_about_dissoc

    if (fcad .and. .not. flag_read_dissoc) then
      call pfant_halt('read_dissoc() must be called before read_main()')
    end if

    open(unit=UNIT_,file=path_to_file, status='old')

    ! row 01: object name, e.g. "sun"
    read(UNIT_, '(a15)') main_titrav

    ! row 02
    ! bid is old "ecrit" (obsolete)
    read(UNIT_, *) bid, main_pas, main_echx, main_echy, main_fwhm

    ! row 03
    read(UNIT_, *) main_vvt(1)
    main_ivtot = 1

    ! rows 03.(1-3): (three conditional rows that MUST exist if and only if main_VVT(1) > 900) ?doc?
    if(main_vvt(1) .gt. 900)  then   ! vt variable avec la profondeur
      read(UNIT_, *) main_ivtot
      ! ivtot, affects subroutine turbul() issue ?what? ?doc?
      if (main_ivtot .gt. MAX_MODELES_NTOT) then
        write (lll, *) 'main_ivtot .gt. MAX_MODELES_NTOT (', &
         main_ivtot, ' .gt. ', MAX_MODELES_NTOT, ')'
         call pfant_halt(lll)
      end if

      read(UNIT_,*) (main_tolv(i), i=1, main_ivtot)
      read(UNIT_,*) (main_vvt(i) ,i=1, main_ivtot)
    end if

    !> @todo issue Carlos Silveira about relation between main_asalog and main_afstar: they are both the metallicity

    ! row 04
    read(UNIT_, *) main_teff, main_glog, main_asalog, main_nhe, main_inum

    ! row 05
    read(UNIT_, *) main_ptdisk, main_mu

    ! row 06
    read(UNIT_, *) main_afstar  ! metallicity of the star (in log scale)

    ! row 07: XXCOR(i)
    ! @todo ISSUE: Should be a column in dissoc.dat !!!!!
    ! (MT) I agree
    if (fcad) then
      read(UNIT_, *) (main_xxcor(i), i=1, dissoc_nmetal)
    else
      read(UNIT_, *) ! skips the line
    end if

    ! row 08 -- part of a file name
    ! This line will define the names of three output files:
    !   fileflux.cont
    !   fileflux.norm
    !   fileflux.spec
    read(UNIT_, '(a)') main_fileflux

    ! row 09
    read(UNIT_, *) main_llzero, main_llfin, main_aint

    write(*,*) main_llzero, main_llfin, main_aint

    ! row 10 - ....
    ! Considers the remaining rows as the filetoh file names
    ! Doesn't know yet the number of files
    ih = 1
    110 continue
    read(UNIT_, '(a)', end=111) filetoh_temp

    if (len_trim(filetoh_temp) .eq. 0) goto 110  ! skips blank rows

    !#spill_check
    if (ih .gt. MAX_MAIN_FILETOH_NUMFILES) then
      call pfant_halt('Too many filetoh files specified (maximum is '//&
       'MAX_MAIN_FILETOH_NUMFILES='//int2str(MAX_MAIN_FILETOH_NUMFILES)//')')
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

    close(unit=UNIT_)
    flag_read_main = .true.
  end

  !=======================================================================================
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

  subroutine read_dissoc(path_to_file)
    integer, parameter :: UNIT_=199
    integer i, j, k, m, mmaxj
    character(len=*) :: path_to_file
    character*2 symbol
    logical flag_found

    ! Auxiliary temp variables for reading file
    integer*4 natomm, nelemm
    dimension natomm(5), nelemm(5)

    open(unit=UNIT_,file=path_to_file, status='old')

    ! row 01
    read(UNIT_,'(2i5, 2f10.5, i10)') dissoc_nmetal, dissoc_nimax, dissoc_eps, dissoc_switer

    ! rows 02 to NMETAL+1: 6-column rows
    !
    !
    !
    do i = 1, dissoc_nmetal
      read (UNIT_, '(a2, 2x, i6, f10.3, 2i5, f10.5)') &
       symbol, dissoc_nelemx(i), dissoc_ip(i), &
       dissoc_ig0(i), dissoc_ig1(i), dissoc_cclog(i)

        ! makes sure that elements first and second are h and he, respectively,
        ! because sat4() and die() count on this
        select case (i)
          case (1)
            if (to_lower(trim(symbol)) .ne. 'h') then
              write(lll,*) 'First element must be hydrogen ("H"), not "', symbol, '"!'
              call pfant_halt(lll)
            end if
          case (2)
            if (to_lower(symbol) .ne. 'he') then
              write(lll,*) 'First element must be helium ("He"), not "', symbol, '"!'
              call pfant_halt(lll)
            end if
        end select

        dissoc_elems(i) = symbol

        !#spill_check
        if (dissoc_nelemx(i) .gt. MAX_Z) then
          call pfant_halt('read_dissoc(): metal # '//int2str(i)//': nelemxi = '//&
           int2str(dissoc_nelemx(i))//' over maximum allowed (MAX_Z='//int2str(MAX_Z)//')')
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

      read(UNIT_, '(a3, 5x, e11.5, 4e12.5, i1, 4(i2,i1))') &
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

    close(unit=UNIT_)
    flag_read_dissoc = .true.
  end




  !=======================================================================================
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

  subroutine read_abonds(path_to_file)
    implicit none
    integer UNIT_, finab, k, j
    parameter(UNIT_=199)
    character(len=*) :: path_to_file
    logical flag_found
    real*8 fstar

    if (.not. flag_read_main) then
      call pfant_halt('read_main() must be called before read_abonds()')
    end if

    open(unit=UNIT_,file=path_to_file, status='old')

    fstar = 10**main_afstar

    j = 1
    finab = 0
    do while (finab .lt. 1)
      read(UNIT_, '(i1,a2,f6.3)') finab, abonds_ele(j), abonds_abol(j)

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

    close(unit=UNIT_)
    flag_read_abonds = .true.
  end




  !=======================================================================================
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

  subroutine read_partit(path_to_file)
    implicit none
    integer UNIT_
    parameter(UNIT_=199)
    character(len=*) :: path_to_file

    integer finpar, j, kmax, l, k

    open(unit=UNIT_,file=path_to_file, status='old')


    j = 1
    finpar = 0
    do while (finpar .lt. 1)
      read (UNIT_, '(a2, 2f5.2, i3, 3f10.2, 34x, i1)') &
       partit_el(j), &
       partit_tini(j), &
       partit_pa(j), &
       partit_jkmax(j), &
       partit_m(j), &
       partit_ki1(j), &
       partit_ki2(j), finpar

      if (finpar .ne. 1) then

        !#spill_check: checks if exceeds maximum number of elements allowed
        if (j .gt. MAX_PARTIT_NPAR) then
          call pfant_halt('read_partit(): par exceeded maximum of MAX_PARTIT_NPAR='//&
           int2str(MAX_PARTIT_NPAR))
        end if


        kmax = partit_jkmax(j)

        !#spill_check: checks if exceeds maximum number of elements allowed
        if (kmax .gt. MAX_PARTIT_KMAX) then
          call pfant_halt('read_partit(): par number '//int2str(j)//'; kmax='//&
           int2str(kmax)//' exceeded maximum of MAX_PARTIT_KMAX='//int2str(MAX_PARTIT_KMAX))
        end if


        read(UNIT_, '(13f6.4)') ((partit_tabu(j, l, k), l=1, 3), k=1, kmax)

        j = j+1
      end if
    end do

    partit_npar = j-1

    return
  end


  !=======================================================================================
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

  subroutine read_absoru2(path_to_file)
    implicit none
    integer UNIT_
    parameter(UNIT_=199)
    character(len=*) :: path_to_file
    character*3 neant
    integer nion, i, ith, j, nrr, nset

    open(unit=UNIT_,file=path_to_file, status='old')

    ! ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
    ! ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
    read (UNIT_,'(2e15.7)') absoru2_abmet, absoru2_abhel


    ! NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISATION
    ! NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
    ! IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
    ! IUNITE=' NOYAU H'  ''    ''    ''       ''      NOYAU D'HYDROGENE
    read (UNIT_,'(2i2, 19a4)') absoru2_nm, absoru2_nmeta, &
          (absoru2_iunite(i),i=1,2), (absoru2_titre(i),i=1,17)

    !#spill_check: checks if exceeds maximum number of elements allowed
    if (absoru2_nm .gt. MAX_ABSORU2_NM) then
      call pfant_halt('read_absoru2(): nm='//int2str(absoru2_nm)//&
         ' exceeded maximum of MAX_ABSORU2_NM='//int2str(MAX_ABSORU2_NM))
    end if


    ! LECTURE DE LA TABLE D'IONISATION CHOISIE
    do j = 1, absoru2_nm
      read (UNIT_, '(3x,i3,2e16.5)') absoru2_nr(j), absoru2_zp(j), absoru2_zm(j)
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
        read (UNIT_, '(a3,a2,i1,2e16.5)') neant, absoru2_nomet(j), &
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


    read (UNIT_, '(2i2)') (absoru2_numset(ith), ith=1,2)

  !> @todo ISSUE: I am not sure if this last part is being read correcly. Perhaps I didn't de-spag right. Anyway, the test verbose is not good.


    !#spill_check: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(1) .gt. MAX_ABSORU2_NUMSET_I) then
      call pfant_halt('read_absoru2(): numset(1) = '//int2str(absoru2_numset(1))//&
       ' exceeded maximum of MAX_ABSORU2_NUMSET_I='//int2str(MAX_ABSORU2_NUMSET_I))
    end if
    !#spill_check: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(2) .gt. MAX_ABSORU2_NUMSET_I) then
      call pfant_halt('read_absoru2(): numset(2) = '//int2str(absoru2_numset(2))//&
       ' exceeded maximum of MAX_ABSORU2_NUMSET_I='//int2str(MAX_ABSORU2_NUMSET_I))
    end if


    ! NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
    ! POUR H,HE ET HE+
    ! PREMIERE LISTE POUR TH.LE.0.8 ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
    do ith = 1,2
      nset = absoru2_numset(ith)
      read (UNIT_,'(8f10.1)') (absoru2_wi(i,ith),i=1,nset)
    end do

  end



  !=======================================================================================
  !> Reads single record from .mod file
  !>
  !> This routine is constructed in a way that it can be used by read_modele() and
  !> also within the inewmarcs module.
  !>
  !> @note Output goes within "record". This could be made as a function and return the
  !>       record, but then it wouldn't be so clear how the compiler would work out the
  !>       memory, i.e., would the whole structure be copied into the variable declared at
  !>       the caller?? Better to avoid doubts: we know that args are passed by reference.

  subroutine read_mod_record(path_to_file, rec_id, flag_open, flag_close, record)
    integer, parameter :: UNIT_=198
    character(len=*), intent(in) :: path_to_file
    integer, intent(in) :: rec_id       !< record identifier (>= 1)
    logical, intent(in) :: flag_open, & !< whether to open the file
                           flag_close   !< whether to close the file
    type(modele_record), intent(out) :: record
    integer i

    ! Record is read twice; second time bid "grabs" everything that was read before
    real*4 bid(16)

    if (flag_open) then
      open(unit=UNIT_, access='direct',status='old', file=path_to_file, recl=1200)
    end if

    read(UNIT_, rec=rec_id) &
     record%ntot,    &
     record%teff,   &
     record%glog,   &
     record%asalog,  &
     record%asalalf, &
     record%nhe,     &
     record%tit,     &
     record%tiabs

    if (record%ntot .eq. 9999) then
      !> @todo ISSUE perhaps I should check the condition that leads to this error
      call pfant_halt('Le modele desire ne est pas sur le fichier')
    end if

    !#spill_check: Checks if exceeds maximum number of elements allowed
    if (record%ntot .gt. MAX_MODELES_NTOT) then
      call pfant_halt('read_mod_record(): ntot = '//int2str(record%ntot)//&
       ' exceeded maximum of MAX_MODELES_NTOT='//int2str(MAX_MODELES_NTOT))
    end if

    read(UNIT_, rec=rec_id) bid, &
         (record%nh(i),   &
          record%teta(i), &
          record%pe(i),   &
          record%pg(i),   &
          record%t5l(i), i=1,record%ntot)

    write(lll, *) 'read_mod_record(): ntot=', record%ntot
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): teff=', record%teff
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): glog=', record%glog
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): asalog=', record%asalog
    call log_debug(lll)

    if (flag_close) then
      close(UNIT_)
    end if
  end


  !=======================================================================================
  !> Reads single record from file infile:modeles into variables modeles_*
  !>
  !> SI L ON DESIRE IMPOSER UN MODELE ON MET EN main_inum LE NUM DU MODELE
  !> SUR LE FICHIER ACCES DIRECT

  subroutine read_modele(path_to_file)
    implicit none
    integer UNIT_
    parameter(UNIT_=199)
    character(len=*) :: path_to_file
    real*8 ddt, ddg, ddab
    integer i, &
            id_   !> @todo This could well be an input parameter, because it wouldn't have to rely on infile:main and would become MUCH more flexible
    type(modele_record) :: r

    !> @todo better to give error if main_inum is not set
    !> @todo Check if FORTRAN initializes variables to zero automatically: can I rely on this??
    !> @todo Maybe implement variable main_FLAG to FLAG that infile:main has been read already
    id_ = 1
    if (main_inum .gt. 0) id_ = main_inum  ! Selects record number

    call read_mod_record(path_to_file, id_, .true., .true., r)


    !> @todo there is an intention here to *look for a model* that matches parameters in main.dat, but I am not sure it is implemented right. BLB mentionet to MT her intention to make this work as a feature.

    !> @todo ISSUE: this seems to be some kind of error check, but will loop forever, better to place it outside, also because of dependence on main_* variables
    !> @todo ISSUE: I don't get this; it will keep looping forever??? Look at the original code. What should happen if one of these three conditions hold?? Actually, got this. These are really consistency checks  (MT) It is annoying for the user to know exactly the index of the model that they are using. The code could have a "search feature"
    !~9 READ(18, REC=ID) NTOT,DETEF,glog,asalog,ASALALF,NHE,TIT,TITABS
    !~  WRITE(6,105)DETEF,glog,asalog,ASALALF,NHE,TIT
    !~        write(6,108) TIABS
    !~  IF(NTOT.EQ.9999)   GO TO 6
    !~  DDT  = ABS(TEFF-DETEF)
    !~C DDTA  = ABS(TETAEF-DETAEF)
    !~  DDG = ABS(GLOG-glog)
    !~  DDAB = ABS(ASALOG-asalog)
    !~  DDHE= ABS(NHE-DNHE)
    !~C DDIF = DDTA+DDG+DDAB+DDHE
    !~5 IF(DDT.GT.1.0)   GO TO 9
    !~  IF(DDG.GT.0.01)   GO TO 9
    !~  IF(DDAB.GT.0.01)   GO TO 9

    !__consistency check__
    ! series of consistency checks which were already present in the 2015- code
    ddt  = abs(main_teff-r%teff)
    ddg = abs(main_glog-r%glog)
    ddab = abs(main_asalog-r%asalog)
    if (abs(main_nhe-r%nhe) .gt. 0.001) then  !> @todo Get the proper (abs(main_nhe-modeles_nhe) .gt. 0.001) epsilon from MT
      write(lll, *) 'modele nhe (', r%nhe, ') does not match main nhe (', main_nhe, ')'
      call pfant_halt(lll)
    end if
    if(ddt .gt. 1.0) then
      write(lll,*) 'abs(main_teff-(model teff)) = ', ddt, ' > 1.0'
      call pfant_halt(lll)
    end if
    if(ddg .gt. 0.01) then
      write(lll,*) 'abs(main_glog-(model glog)) = ', ddg, ' > 0.01'
      call pfant_halt(lll)
    end if
    if(ddab .gt. 0.01) then
      write(lll,*) 'abs(main_asalog-(model asalog)) = ', ddab, ' > 0.01'
      call pfant_halt(lll)
    end if

    ! ready to copy (& convert) variables to their counterparts
    modeles_ntot    = r%ntot     ! integer(4)-to-integer(?)
    modeles_teff   = r%teff    ! real(4) to real(8)
    modeles_glog   = r%glog    ! "
    modeles_asalog  = r%asalog   ! "
    modeles_asalalf = r%asalalf  ! "
    modeles_nhe     = r%nhe      ! "
    do i = 1, modeles_ntot
      modeles_nh(i)   = r%nh(i)   ! real(4) to real(8)
      modeles_teta(i) = r%teta(i) ! "
      modeles_pe(i)   = r%pe(i)   ! "
      modeles_pg(i)   = r%pg(i)   ! "
      modeles_t5l(i)  = r%t5l(i)  ! "
    end do
    modeles_tit = r%tit
    modeles_tiabs = r%tiabs
  end
end module read_most_files

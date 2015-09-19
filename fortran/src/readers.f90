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
!> Reading routines and variable declarations for dfile:dissoc

module reader_dissoc
  use logging
  use dimensions
  use misc
  implicit none

  logical :: flag_read_dissoc = .false.    !< Whether read_dissoc() has already been called


  ! dissoc.dat, metals part
  integer dissoc_nmetal !< number of elements considered in chemical equilibrium
  integer dissoc_nimax  !< maximum number of iterations in Newton-Rapson method
  character*2 dissoc_elems(MAX_DISSOC_NMETAL)    !< Elements table field 1/6: atomic symbol
  integer     dissoc_nelemx(MAX_DISSOC_NMETAL)   !< Elements table field 2/6: atomic number
  real*8      dissoc_ip(MAX_DISSOC_NMETAL)      !< Elements table field 3/6: ?
  integer     dissoc_ig0(MAX_DISSOC_NMETAL),  & !< Elements table field 4/6: ?
              dissoc_ig1(MAX_DISSOC_NMETAL)     !< Elements table field 5/6: ?
  real*8      dissoc_cclog(MAX_DISSOC_NMETAL)   !< Elements table field 6/6: ?

  ! dissoc.dat, molecules part
  character*3 dissoc_mol(MAX_DISSOC_NMOL)     !< Molecules table field 01: molecule name
  real*8 dissoc_c(MAX_DISSOC_NMOL, 5)         !< Molecules table fields 02-06
  integer dissoc_mmax(MAX_DISSOC_NMOL), &     !< Molecules table field 07
          dissoc_nelem(5, MAX_DISSOC_NMOL), & !< Molecules table fields 08, 10, ... (atomic number)
          dissoc_natom(5, MAX_DISSOC_NMOL)    !< Molecules table fields 09, 11, ... (number of atoms)
  integer dissoc_nmol
  real*8 dissoc_eps,   & !< if abs((x(i+1)-x(i))/x(i)) .le. eps: converged
         dissoc_switer   !< flag affecting x(i+1)
                         !< @li if switer .gt. 0 then x(i+1)=0.5*(x(i+1)+x(i))
                         !< @li if switer .le. 0 then x(i+1)=x(i+1)

contains

  !=======================================================================================
  !> Reads dissoc.dat to fill variables dissoc_*
  !>
  !> @attention This file must end with a <b>blank row</b> so that the routine can detect
  !> the end of the file.

  !> @todo Various tests:
  !> @todo - mismatched NMETAL and metal rows
  !> @todo - check if NMETAL and NMOL match what they are supposed to (assertions in test)
  !<      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
  !> @todo PROPOSE: use READ()'s "END=" option

  subroutine read_dissoc(path_to_file)
    integer, parameter :: UNIT_=199
    integer i, j, k, m, mmaxj
    character(len=*) :: path_to_file
    character*2 symbol, symbol_
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
       symbol_, dissoc_nelemx(i), dissoc_ip(i), &
       dissoc_ig0(i), dissoc_ig1(i), dissoc_cclog(i)

      symbol = adjust_atomic_symbol(symbol_)

      ! makes sure that elements first and second are h and he, respectively,
      ! because sat4() and die() count on this
      select case (i)
        case (1)
          if (symbol .ne. ' H') then
            write(lll,*) 'First element must be hydrogen (" H"), not "', symbol_, '"!'
            call pfant_halt(lll)
          end if
        case (2)
          if (symbol .ne. 'HE') then
            write(lll,*) 'First element must be helium ("HE"), not "', symbol_, '"!'
            call pfant_halt(lll)
          end if
      end select

      dissoc_elems(i) = symbol

      ! spill check
      if (dissoc_nelemx(i) .gt. MAX_DISSOC_Z) then
        call pfant_halt('read_dissoc(): metal # '//int2str(i)//': nelemxi = '//&
         int2str(dissoc_nelemx(i))//' over maximum allowed (MAX_DISSOC_Z='//int2str(MAX_DISSOC_Z)//')')
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

    write(lll,*) dissoc_mol(j), &
                 (dissoc_c(j, k), k=1,5), &
                 dissoc_mmax(j), &
                 (nelemm(m), natomm(m), m=1,4)
    call log_debug(lll)

    mmaxj = dissoc_mmax(j)
    if(mmaxj .eq. 0) then
      ! note: interesting that Fortran accepts a blank line and reads everything blank or zero

      go to 1014  ! means end-of-file
    end if

    ! consistency check:
    if (mmaxj .gt. 4) then
      write(lll,*) 'read_dissoc() molecule "', dissoc_mol(j), &
       '", mmaxj = ', mmaxj, ' cannot be greater than 4!'
      call pfant_halt(lll)
    end if

    ! consistency check
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
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:main

module reader_main
  use logging
  use dimensions
  use reader_dissoc
  use config
  implicit none

  !> Flag indicating whether read_main() has already been called.
  logical :: flag_read_main   = .false.

  character*64 main_flprefix  !< prefix for flux files: <main_flprefix>.(spec, cont, norm)
  logical   main_ptdisk    !< ?doc?
  real*8 :: &
   main_pas,    & !< calculation step within each calculation sub-interval; this is a "delta lambda"
   main_echx,   & !< not used in calculation, only written to flux file
   main_echy,   & !< not used in calculation, only written to flux file
   main_mu,     & !< ?doc?
   main_afstar, & !< ?doc? metallicity of the star (in log scale)
   main_llzero, & !< lower boundary of calculation interval
   main_llfin,  & !< upper boundary of calculation interval
   main_aint,   & !< length of each calculation sub-interval (llfin-llzero) is divided into intervals of roughly aint
   main_teff,   & !< effective temperature of the star
   main_glog,   & !< log10 of gravity
   main_asalog, & !< log10 of metallicity
   main_nhe       !< only used to check if matches with modeles_nhe
  !> "Full-width-half-maximum" of Gaussian function for
  !> convolution of calculated spectrum; used only by nulbad executable
  real*8 :: main_fwhm

  integer   main_ivtot, & !< affects turbul() issue ?doc?
                          !< = 1 -- "vt" constant
                          !< > 1 -- "vt" variable
            main_inum     !< record id within modeles.mod
  character*64 main_filetohy(MAX_FILETOH_NUM_FILES) !< ?doc? Names of ten outputs files that will contain ???
  integer   main_filetoh_numfiles !< number of valid elements within filetohy

  character*15 main_titrav                !< Title, e.g. "Sun"
  real*8 :: main_vvt(MAX_MODELES_NTOT), & !< ?doc?
            main_tolv(MAX_MODELES_NTOT)   !< ?doc?
  !> This vector goes along with dissoc_ELEMS and dissoc_NELEMX
  !> @todo would go better as a column in dissoc.dat or eliminated
  real*8 :: main_xxcor(MAX_DISSOC_NMETAL) !< ?doc?
  save
contains

  !=======================================================================================
  !> Makes sure that read_main() has been called
  !>
  !> @note Calls read_main() with argument flag_care_about_dissoc set to .false.
  !> @note Calls read_main() with argument flag_read_filetoh set to .false.
  !>
  !> This routine is used by hydro2, innewmarcs, nulbad. None of these care about dissoc
  !> or filetoh variables.

  subroutine assure_read_main()
    if (.not. flag_read_main) then
      call read_main(full_path_w(config_fn_main), flag_care_about_dissoc=.false., &
       flag_read_filetoh=.false.)
    end if
  end

  !=======================================================================================
  !> Reads dfile:main to fill variables main_*
  !>
  !> @note Must be called after read_dissoc().
  !> @todo ISSUE Explain the vvt case VERY WELL because it is an "anomaly", i.e., dfile:main will have MORE LINES
  !> (MT) Yes it makes sense, it specifies microturbulence velocities for each layer of the atmosphere

  subroutine read_main(path_to_file, flag_care_about_dissoc, flag_read_filetoh)
    character(len=*), intent(in) :: path_to_file
    !> (=.true.) If .false., won't bother about reading dfile:dissoc first and
    !> will skip the xxcor line. Only pfant bother about the xxcor line; other executables
    !> want to use dfile:main without having to read dfile:dissoc first.
    logical, intent(in), optional :: flag_care_about_dissoc
    !> (=.true.) If .false., does not read the hydrogen lines filenames.
    logical, intent(in), optional :: flag_read_filetoh
    integer, parameter :: UNIT_ = 4
    integer ih, i
    character*64 filetoh_temp
    logical :: flag_care_about_dissoc_, flag_read_filetoh_
    logical ecrit_obsolete



    flag_care_about_dissoc_ = .true.
    if (present(flag_care_about_dissoc)) flag_care_about_dissoc_ = flag_care_about_dissoc

    flag_read_filetoh_ = .true.
    if (present(flag_read_filetoh)) flag_read_filetoh_ = flag_read_filetoh

    if (flag_care_about_dissoc_ .and. .not. flag_read_dissoc) then
      call pfant_halt('read_dissoc() must be called before read_main()')
    end if

    open(unit=UNIT_,file=path_to_file, status='old')

    ! row 01: object name, e.g. "sun"
    read(UNIT_, '(a15)') main_titrav

    ! row 02
    ! ecrit is obsolete
    read(UNIT_, *) ecrit_obsolete, main_pas, main_echx, main_echy, main_fwhm

    ! row 03
    read(UNIT_, *) main_vvt(1)
    main_ivtot = 1

    ! rows 03.(1-3): (three conditional rows that MUST exist if and only if main_VVT(1) > 900) ?doc?
    if(main_vvt(1) .gt. 900)  then   ! vt variable avec la profondeur
      read(UNIT_, *) main_ivtot
      ! ivtot, affects subroutine turbul() issue ?doc?
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
    read(UNIT_, *) main_afstar

    ! row 07: XXCOR(i)
    ! @todo ISSUE: Should be a column in dissoc.dat !!!!!
    ! (MT) I agree
    if (flag_care_about_dissoc_) then
      read(UNIT_, *) (main_xxcor(i), i=1, dissoc_nmetal)
    else
      read(UNIT_, *) ! skips the line
    end if

    ! row 08 -- part of a file name
    ! This line will define the names of three output files:
    !   <fn_flux>.cont
    !   <fn_flux>.norm
    !   <fn_flux>.spec
    read(UNIT_, '(a)') main_flprefix

    ! row 09
    read(UNIT_, *) main_llzero, main_llfin, main_aint

    101 format('reader_main(): llzero=',f8.2,'; llfin=',f8.2,'; aint=',f6.2)
    write(lll,101) main_llzero, main_llfin, main_aint
    call log_info(lll)

    if (flag_read_filetoh_) then
      ! row 10 - ....
      ! Considers the remaining rows as the filetoh file names
      ! Doesn't know yet the number of files
      ih = 1
      110 continue
      read(UNIT_, '(a)', end=111) filetoh_temp

      if (len_trim(filetoh_temp) .eq. 0) goto 110  ! skips blank rows

      ! spill check
      if (ih .gt. MAX_FILETOH_NUM_FILES) then
        call pfant_halt('Too many filetoh files specified (maximum is '//&
         'MAX_FILETOH_NUM_FILES='//int2str(MAX_FILETOH_NUM_FILES)//')')
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
    end if

    close(unit=UNIT_)
    flag_read_main = .true.
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:modeles
!>
!> @todo (MT) dfile:modeles could become an ASCII file

module reader_modeles
  use logging
  use dimensions
  use reader_main
  implicit none

  ! Attention: one has to specify sizes of all the variables here, because
  ! this may change with compiler

  !> Structure to store atmospheric model read from binary file.
  !> Binary file follows "NewMarcs" structure containing real numbers stored as real*4.
  !> Hence the real*4 declarations.

  type modele_record
    !> Size of variables nh, teta, pe, pg, t5l
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

   !> Unit to open dfile:modeles
   integer, parameter :: UNIT_MOD = 198

   !> Whether the file is open
   logical :: flag_open = .false.

   integer, parameter :: MOD_RECL = 1200 !< record length

contains
  !=======================================================================================
  !> Returns number of records in file

  integer function get_num_records(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer :: size

    inquire(FILE=path_to_file, SIZE=size)

    ! Note the "-1": last 1200 bytes are used as a end-of-file flag with only an integer
    ! value of 9999 recorded
    get_num_records = size/MOD_RECL-1
  end



  !=======================================================================================
  !> Opens existing models binary file

  subroutine open_mod_file(path_to_file)
    character(len=*), intent(in) :: path_to_file

    if (flag_open) then
      call pfant_halt('There is already a models file open', is_assertion=.true.)
    end if

    open(unit=UNIT_MOD, access='direct',status='old', file=path_to_file, recl=MOD_RECL)

    flag_open = .true.
  end

  !=======================================================================================
  !> Closes open file

  subroutine close_mod_file()
    if (.not. flag_open) then
      call pfant_halt('No models is open', is_assertion=.true.)
    end if

    close(UNIT_MOD)

    flag_open = .false.
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

  subroutine read_mod_record(rec_id, record)
    integer, intent(in) :: rec_id       !< record identifier (>= 1)
    type(modele_record), intent(out) :: record

    integer i

    ! Record is read twice; second time bid "grabs" everything that was read before
    real*4 bid(16)

    read(UNIT_MOD, rec=rec_id) &
     record%ntot,    &
     record%teff,   &
     record%glog,   &
     record%asalog,  &
     record%asalalf, &
     record%nhe,     &
     record%tit,     &
     record%tiabs

    !> @todo ISSUE I think this situation occurs when the last record of the file is read. This is a "flag" record
    !> created by innewmarcs.
    if (record%ntot .eq. 9999) then
      call pfant_halt('Le modele desire ne est pas sur le fichier')
    end if

    ! spill check: Checks if exceeds maximum number of elements allowed
    if (record%ntot .gt. MAX_MODELES_NTOT) then
      call pfant_halt('read_mod_record(): ntot = '//int2str(record%ntot)//&
       ' exceeded maximum of MAX_MODELES_NTOT='//int2str(MAX_MODELES_NTOT))
    end if

    read(UNIT_MOD, rec=rec_id) bid, &
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
    write(lll, *) 'read_mod_record(): asalalf=', record%asalalf
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): nhe=', record%nhe
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): tit=', record%tit
    call log_debug(lll)
    write(lll, *) 'read_mod_record(): tiabs=', record%tiabs
    call log_debug(lll)

  end


  !=======================================================================================
  !> Reads single record from file dfile:modeles into variables modeles_*
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
            id_   !> @todo This could well be an input parameter, because it wouldn't have to rely on dfile:main and would become MUCH more flexible
    type(modele_record) :: r

    !> @todo better to give error if main_inum is not set
    !> @todo Check if FORTRAN initializes variables to zero automatically: can I rely on this??
    !> @todo Maybe implement variable main_FLAG to FLAG that dfile:main has been read already
    id_ = 1
    if (main_inum .gt. 0) id_ = main_inum  ! Selects record number

    call open_mod_file(path_to_file)
    call read_mod_record(id_, r)
    call close_mod_file()

    !> @todo there was an intention here to *look for a model* that matches parameters in main.dat.
    !> hydro2 has the correct reader. but I am not sure it is implemented right. BLB mentionet to MT her intention to make this work as a feature.

    !> @todo will no longer compare with main_*, but probably with input variables because of hydro2, which uses x_teff, x_glog, x_asalog

    ! consistency check: these were already present in the 2015- code
    ddt  = abs(main_teff-r%teff)
    ddg = abs(main_glog-r%glog)
    ddab = abs(main_asalog-r%asalog)

    !> @todo issue I think that nhe doesn't need to match. Anyway, pfantgrade wasn't checking this.
    !> r%nhe comes straight from the newmarcs grid file and is not calculated.
    ! if (abs(main_nhe-r%nhe) .gt. 0.001) then
    !  write(lll, *) 'modele nhe (', r%nhe, ') does not match main nhe (', main_nhe, ')'
    !  call pfant_halt(lll)
    ! end if

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
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:hmap

module reader_hmap
  use logging
  use dimensions
  use misc
  use reader_main
  implicit none

  !> Structure to store one row of dfile:hmap
  type hmap_row
    !> file name
    character*64 fn
    !> lower level
    integer na
    !> upper level
    integer nb
    !> Central lambda
    real*8 clam
    !> excitation potential for the lower level of transition
    real*8 kiex
    !> C1 ?doc?
    real*8 c1
  end type

  !> Array to store all rows in dfile:hmap
  type(hmap_row) :: hmap_rows(MAX_FILETOH_NUM_FILES)

  !> Number of rows in dfile:hmap
  integer :: hmap_n

contains

  !=======================================================================================
  !> Reads dfile:hmap to fill variables hmap_*
  !>

  subroutine read_hmap(path_to_file)
    character(len=*), intent(in) :: path_to_file
    integer, parameter :: UNIT_ = 199
    logical :: skip_row(MAX_FILE_ROWS)
    integer :: num_rows, i

    ! Temporary auxiliary variables for reading file
    character*64 t_fn
    integer t_na
    integer t_nb
    real*8 t_clam
    real*8 t_kiex
    real*8 t_c1

    call map_file_comments(path_to_file, skip_row, num_rows)

    write(*,*) 'num_rows=', num_rows

    open(unit=UNIT_,file=path_to_file, status='old')

    hmap_n = 0
    do i = 1, num_rows
      if (skip_row(i)) then
        read(UNIT_,*)   ! skips comment row
      else
        read(UNIT_, *) t_fn, t_na, t_nb, t_clam, t_kiex, t_c1

        hmap_n = hmap_n+1

        hmap_rows(hmap_n)%fn = t_fn
        hmap_rows(hmap_n)%na = t_na
        hmap_rows(hmap_n)%nb = t_nb
        hmap_rows(hmap_n)%clam = t_clam
        hmap_rows(hmap_n)%kiex = t_kiex
        hmap_rows(hmap_n)%c1 = t_c1
      end if
    end do
    close(unit=UNIT_)


    !#logging
    call log_info('reader_hmap():')
    call log_info('filename         na nb c.lambda     kiex       c1')
    do i = 1, hmap_n
      11 format(a16,1x,i2,1x,i2,1x,f8.2,1x,f8.2,1x,f8.2)
      write(lll, 11) hmap_rows(i)
      call log_info(lll)
    end do
  end


  !========================================================================================
  !> Fills hmap_rows according with main_filetohy. For compatibility with old way of
  !> informing the "filetoh" filenames, which was inside dfile:main
  !>
  !> This routine fills only the "fn" (filename) field of the hmap_row structure because
  !> the filenames are all that's available in dfile:main.
  !>
  !> This routine is used only by pfant when not in "--hmap" mode.
  !>
  !> This routine is part of a mechanism to allow pfant to work in two ways.
  !> @li either use dfile:hmap for the list of hydrogen line files (likely to become the
  !>     standard way in the future)
  !> @li take this list from dfile:main (legacy)

  subroutine hmap_copy_from_main()
    integer :: i

    hmap_n = 0
    do i = 1, main_filetoh_numfiles
      hmap_n = hmap_n+1
      hmap_rows(hmap_n)%fn = main_filetohy(i)
      hmap_rows(hmap_n)%na = 0
      hmap_rows(hmap_n)%nb = 0
      hmap_rows(hmap_n)%clam = 0
      hmap_rows(hmap_n)%kiex = 0
      hmap_rows(hmap_n)%c1 = 0
    end do
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:filetoh

module reader_filetoh
  use dimensions
  use reader_modeles
  use config
  use reader_hmap
  implicit none

  !=====
  ! Read directly from file
  !=====
  ! These are read by read_filetoh() and processed by filetoh_auh()
  !> ?doc?
  character*80 filetoh_titre(MAX_FILETOH_NUM_FILES)
  !> ?doc?
  character*11 filetoh_ttt(MAX_FILETOH_NUM_FILES)
  !> Will be pointer target
  !> ?doc?
  real*8, target, dimension(MAX_FILETOH_NUM_FILES, MAX_FILETOH_JMAX, MAX_MODELES_NTOT) :: &
   filetoh_th
  !> Will be pointer target
  !> ?doc?
  real*8, target, dimension(MAX_FILETOH_NUM_FILES, MAX_FILETOH_JMAX) :: filetoh_lambdh
  !> ?doc?
  integer filetoh_jmax(MAX_FILETOH_NUM_FILES)

  !> This variable was a constant hard-coded as
  !> @code
  !> /3750.150, 3770.630, 3797.900, 3835.390, 3889.050, 3970.076, 4101.748, 4340.468, 4861.332, 6562.817/
  !> @endcode
  !> Now it is opening the files and taking the initial lambda for each file instead
  !> @todo line is symmetric; check if left-side is assumed
  real*8, dimension(MAX_FILETOH_NUM_FILES) :: filetoh_llhy
  !> Number of filetoh files that were actually found in disk
  integer :: filetoh_num_files = 0
  !> Names of filetoh files that were actually found in disk
  character*64 filetoh_filenames(MAX_FILETOH_NUM_FILES)

contains

  !=======================================================================================
  !> Tries to open and read all files listed in variable hmap_rows
  !>
  !> [llzero, llfin] is the calculation lambda interval. Here this interval is used for
  !> error checking: if it doesn't find a file that should exist, it will give an error
  !>
  !> @note For compatibility, it will allow to pass if the filetoh list came from inside
  !> dfile:main. This is assumed to have happened if the "clam" field of a given hmap
  !> row is zero.
  !>
  !> LECTURE DE LA PROFONDEUR OPTIQUE DANS LA RAIE D H

  !> @todo not tested; also the calctauh in synthesis needs testing

  subroutine read_filetoh(llzero, llfin)
    real*8, intent(in) :: llzero, llfin
    integer unit_
    parameter(unit_=199)
    integer i, j, n, i_file
    character(len=:), allocatable :: file_now
    real*8 :: clam
    logical :: must_exist, flag_inside

    i = 0

    do i_file = 1, hmap_n
      file_now = full_path_w(hmap_rows(i_file)%fn)
      clam = hmap_rows(i_file)%clam

      must_exist = .false.
      flag_inside = .true.
      if (clam .ne. 0) then
        if (h_line_is_inside(clam, llzero, llfin)) then
          must_exist = .true.
        else
          flag_inside = .false.  ! hydrogen line if outside calculation interval, skips it
        end if

      else
        ! list of nydrogen line files came from dfile:man and we don't know their central lambda unless we open the file
      end if

      if (flag_inside) then
        open(err=111, unit=unit_,file=file_now,status='old')

        i = i+1

        read(unit_,'(a80)') filetoh_titre(i)
        read(unit_,'(a11)') filetoh_ttt(i)
        read(unit_,*) filetoh_jmax(i)
        read(unit_,'(5f14.3)') (filetoh_lambdh(i,j), j=1,filetoh_jmax(i))
        read(unit_,'(5e12.4)') ((filetoh_th(i,j,n),&
         j=1,filetoh_jmax(i)), n=1,modeles_ntot)
        close(unit_)

        ! Takes first lambda of file as a reference
        clam = filetoh_lambdh(i, 1)

        if (.not. h_line_is_inside(clam, llzero, llfin)) then
          i = i-1  ! "rewinds" 1
          go to 112
        end if

        filetoh_llhy(i) = clam

        ! Registers filename in list of files that were found
        filetoh_filenames(i) = hmap_rows(i_file)%fn

        goto 112

        111 continue
        if (must_exist) then
          130 format('[',F7.1,'-',F5.1,',',F7.1,'+',F5.1,'] overlaps with [',&
           F7.1,'-',F5.1,',',F7.1,'+',F5.1,'], but cannot open file "',A,'"')
          write(lll,130) clam, H_LINE_WIDTH, clam, H_LINE_WIDTH, llzero, LAMBDA_STRETCH, &
           llfin, LAMBDA_STRETCH, file_now
          call pfant_halt(lll)
        end if
        call log_warning('Error opening file "' // file_now // '"')
      end if

      112 continue
    end do

    filetoh_num_files = i


    ! Note: when taking the "filetohy" from main configuration file, will not bother
    ! about hydrogen lines files not found, so bewhare (--hmap is the preferred mode anyway)
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:absoru2

module reader_absoru2
  use logging
  use dimensions
  use misc
  implicit none

  integer absoru2_nm,    & !< NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISATION
          absoru2_nmeta, & !< NMETA=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
          absoru2_numset(2) !< NUMSET=NUMBER DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
                            !< POUR H,HE ET HE+

  character*2 :: absoru2_nomet(MAX_ABSORU2_NM) !< NOM DE L'ELEMENT; not used
  real*8 absoru2_abmet, & !< ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
         absoru2_abhel    !< ABHEL=ABONDANCE NORMALE D'HELIUM (NHE/NH)
  !> @todo ISSUE: i am not sure about these character*4, made this from the format below
  character*68 absoru2_titre !< ?doc?

  !> Two possibilities
  !> @li IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
  !> @li IUNITE=' NOYAU H' SI ON VEUT CALCULER KAPPA PAR NOYAU D'HYDROGENE
  character*8 absoru2_iunite

  integer, dimension(MAX_ABSORU2_NM) :: &
   absoru2_nr !< NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
  real*8, dimension(MAX_ABSORU2_NM) :: &
   absoru2_zp, & !< ZP=NBR. D'ABONDANCE DE L'ELEMENT
   absoru2_zm    !< ZM=POIDS MOLECULAIRE DE L'ELEMENT
  real*8, dimension(MAX_ABSORU2_NM, MAX_ABSORU2_NRR) :: &
    absoru2_xi,&!< XI(J,I) = POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATION
    absoru2_pf  !< PF(J,I) = FONCTION DE PARTITION DE L'ELEMENT J AU STADE D'IONISATION
  real*8 absoru2_wi(MAX_ABSORU2_NUMSET_I, 2) !< ?doc?

contains
  !=======================================================================================
  !> Reads file dfile:absoru2 to fill variables absoru2_*
  !>
  !> @note Variables absoru2_ZP, absoru2_XI, and absoru2_PF
  !>       undergo transformation after their values are read from file.
  !>
  !> @note Historically, this routine is in essence the old routine called "LECTUR"
  !>
  !> CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
  !> PUIS LES DONNEES CORRESPONDANTS AUX ABSORBANTS METALLIQUES SI NECE

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
    read (UNIT_,'(2i2,a8,a)') absoru2_nm, absoru2_nmeta, absoru2_iunite, absoru2_titre

    ! spill check: checks if exceeds maximum number of elements allowed
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

        ! makes sure the atomic symbol looks OK (even thou this variable is not used so far)
        absoru2_nomet(j) = adjust_atomic_symbol(absoru2_nomet(j))

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


    ! spill check: Checks if exceeds maximum number of elements allowed
    if (absoru2_numset(1) .gt. MAX_ABSORU2_NUMSET_I) then
      call pfant_halt('read_absoru2(): numset(1) = '//int2str(absoru2_numset(1))//&
       ' exceeded maximum of MAX_ABSORU2_NUMSET_I='//int2str(MAX_ABSORU2_NUMSET_I))
    end if
    ! spill check: Checks if exceeds maximum number of elements allowed
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
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:abonds

module reader_abonds
  use logging
  use reader_dissoc
  use reader_main
  use dimensions
  implicit none

  !=====
  ! Variables filled by read_abonds() (file abonds.dat)
  !=====
  integer abonds_nabond !< ?doc?
  character*2 abonds_ele(MAX_ABONDS_NABOND)     !< ?doc?
  real*8      abonds_abol(MAX_ABONDS_NABOND), & !< ?doc?
              abonds_abo(MAX_ABONDS_NABOND)     !< ?doc? This is calculated

  !> Flag indicating whether read_abonds() has already been called
  logical :: flag_read_abonds = .false.

contains

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
        abonds_ele(j) = adjust_atomic_symbol(abonds_ele(j))

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
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:partit

module reader_partit
  use logging
  use misc
  use dimensions
  implicit none

  character*2, dimension(MAX_PARTIT_NPAR) :: &
   partit_el !< Element symbol. Must be right-aligned and uppercase.
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

contains
  !=======================================================================================
  !> Reads file dfile:partit to fill variables partit_*
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

        ! spill check: checks if exceeds maximum number of elements allowed
        if (j .gt. MAX_PARTIT_NPAR) then
          call pfant_halt('read_partit(): par exceeded maximum of MAX_PARTIT_NPAR='//&
           int2str(MAX_PARTIT_NPAR))
        end if

        partit_el(j) = adjust_atomic_symbol(partit_el(j))

        kmax = partit_jkmax(j)

        ! spill check: checks if exceeds maximum number of elements allowed
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
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:gridsmap (used by innewmarcs)
!>
!> @li gridsmap -


module reader_gridsmap
  use reader_modeles
  use qsort
  use logging
  use config
  implicit none

  ! Variables related to the reference models
  integer, parameter:: MAX_GRIDSMAP_NUM_FILES = 10
  integer gridsmap_num_files
  real*4, dimension(MAX_GRIDSMAP_NUM_FILES) :: gridsmap_asalog
  character*64, dimension(MAX_GRIDSMAP_NUM_FILES) :: gridsmap_fn

contains
  !=======================================================================================
  !> Reads map of models: dfile:gridsmap
  !>
  !> The information it contains is
  !> just a list of .mod files. Files needn't be sorted. Metallicities are taken from the
  !> first record of each file and the files are sorted in ascending order of metallicity
  !>
  !> This this is necessary because Fortran doesn't have a cross-platform solution for
  !> listing the files in a directory.
  !>

  subroutine read_gridsmap()
    character*64 :: t_fn0
    character(len=:), allocatable :: t_fn
    character(len=:), allocatable :: path_to_file
    integer, parameter :: UNIT_ = 199
    type(modele_record) :: rec
    character*64 :: temp_fn(MAX_GRIDSMAP_NUM_FILES)
    real*8 :: temp_asalog(MAX_GRIDSMAP_NUM_FILES) ! have to declare as real8 for the quicksort routine
    integer :: order(MAX_GRIDSMAP_NUM_FILES), i

    path_to_file = full_path_w(config_fn_gridslist)

    open(unit=UNIT_,file=path_to_file, status='old')


    gridsmap_num_files = 0
    do while (.true.)
      read(UNIT_, '(a)', end=10) t_fn0

      t_fn = adjustl(trim(t_fn0))  ! Tolerant with lines starting with spaces

      if (len(t_fn) .eq. 0) cycle
      if (t_fn(1:1) .eq. '#') cycle  ! Comment lines starting with a '#'

      ! Opens .mod file to get metallicity from its first record
      ! (metallicity should be the same for all records)
      call open_mod_file(full_path_w(t_fn))
      call read_mod_record(1, rec)
      call close_mod_file()

      gridsmap_num_files = gridsmap_num_files+1
      temp_asalog(gridsmap_num_files) = dble(rec%asalog)
      temp_fn(gridsmap_num_files) = t_fn  ! writes to temp because order will change
    end do

    10 continue  ! reached EOF

    ! sorts temp_asalog
    call quick_sort(temp_asalog, order, gridsmap_num_files)

    ! now mounts gridsmap_fn in sync with gridsmap_asalog
    do i = 1, gridsmap_num_files
      gridsmap_asalog(i) = real(temp_asalog(i))
      gridsmap_fn(i) = temp_fn(order(i))
    end do

    !#logging
    call log_info('reader_gridsmap():')
    call log_info('List of model grid files: # asalog filename')
    do i = 1, gridsmap_num_files
      11 format(26x,i1,' ', f6.3,' ',a)
      write(lll, 11) i, gridsmap_asalog(i), gridsmap_fn(i)
      call log_info(lll)
    end do

    close(unit=UNIT_)
  end
end




!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reader and variable declarations for dfile:atoms

module reader_atoms
  use dimensions
  use logging
  use reader_abonds
  implicit none

  !=====
  ! Variables filled by read_atoms() (file dfile:atoms)
  !=====
  ! dfile:atoms, file originals
  integer atoms_nblend !< ?doc?
  !> atomic symbol. Must be right-aligned and uppercase
  character*2 atoms_elem(MAX_ATOMS_NBLEND)
  integer, dimension(MAX_ATOMS_NBLEND) :: &
   atoms_ioni !< ?doc?
  real*8, dimension(MAX_ATOMS_NBLEND) :: &
   atoms_lambda,       & !< ?doc?
   atoms_kiex,         & !< ?doc? excitation potential for the lower level?
   atoms_algf,         & !< ?doc?
   atoms_ch,           & !< ?doc?
   atoms_gr,           & !< ?doc?
   atoms_ge,           & !< ?doc?
   atoms_zinf,         & !< ?doc?
   atoms_abondr_dummy, & !< ?doc?
   atoms_abonds_abo  !< will be filled by "inner join" by searching
                          !< atoms_elem through abonds_ele

contains
  !=======================================================================================
  !> Reads file dfile:atoms to fill variables atoms_* (double underscore)
  !>
  !> Depends on abonds_*, so must be called after READ_ABONDS()
  !>
  !> This file has 2 types of alternating rows:
  !> @verbatim
  !>   odd row
  !>     col 1 -- 2-letter atoms_elem(K) atomic symbol
  !>     col 2 -- atoms_ioni(k) ?doc?
  !>     col 3 -- atoms_lambda(K) ?doc?
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

  subroutine read_atoms(filename)
    implicit none
    integer unit_
    parameter(unit_=199)
    character(len=*) :: filename
    integer finrai, k, j
    logical flag_found

    if (.not. flag_read_abonds) then
      call pfant_halt('read_abonds() must be called before read_atoms()')
    end if

    open(unit=unit_,file=filename, status='old')

    k = 1
    do while (.true.)
      ! spill check: checks if exceeds maximum number of elements allowed
      if (k .gt. MAX_ATOMS_NBLEND) then
        call pfant_halt('read_atoms(): exceeded maximum of MAX_ATOMS_NBLEND='//&
         int2str(MAX_ATOMS_NBLEND)//' spectral lines')
      end if

      read(unit_, '(a2, i1, 1x, f10.3)') atoms_elem(k), &
                                         atoms_ioni(k), &
                                         atoms_lambda(k)
      atoms_elem(k) = adjust_atomic_symbol(atoms_elem(k))

      read(unit_, *) atoms_kiex(k), &
       atoms_algf(k), &
       atoms_ch(k), &
       atoms_gr(k), &
       atoms_ge(k), &
       atoms_zinf(k), &
       atoms_abondr_dummy(k), finrai

      !> @todo ISSUE ask MT Why this? Document!!! (MT) If the "radiative broadening" is zero,
      !> it is calculated as a function of lambda; otherwise, it is assumed that it has been inputted manually.
      !>
      !> @todo issue ask BLB 2.21e15 stand for? + reference
      if (atoms_gr(k) .lt. 1e-37) atoms_gr(k) = 2.21e15 / atoms_lambda(k)**2


      !> Besides reading the file, this routine searches atoms's element within abonds'
      !> elements and copies corresponding
      !> abonds_abo value into atoms_abonds_abo. Halts program if element not found.
      !>
      !> In database terminology, this is sort of a "inner join".
      !>
      !> @note Historically, this corresponds to old routine "abondraih". The search was
      !>   being carried out every time (lzero, lfin) changed and only for the filtered
      !>   atoms rows. I decided to perform this search for all atoms rows and
      !>   then filter atoms_abonds_abo together with other variables in
      !>   filter_atoms(), which is probably cheaper.
      !> @todo issue why name was "abondraih"?? did it mean "abundances of hydrogen lines"? i ask this because if it has really a physical meaning, i shouldn't bury this inside read_most_files.f

      flag_found = .false.
      do  j = 1, abonds_nabond
        if (abonds_ele(j) .eq. atoms_elem(k)) then
          flag_found = .true.
          exit
        end if
      end do
      if (.not. flag_found) then
        write(lll,*)  'read_atoms(): element "', atoms_elem(k), &
         ' (spectral line number ', k, ') cannot be found in abundance file'
        call pfant_halt(lll)
      end if
      atoms_abonds_abo(k) = abonds_abo(j)

      if (finrai .eq. 1) exit !__end-of-file__

      k = k+1
    end do

    atoms_nblend = k

    write(lll,*) 'read_atoms(): last line taken: element: "', atoms_elem(atoms_nblend), &
      '"; lambda: ', atoms_lambda(atoms_nblend)
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
  end
end



!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!> Reading routines and variable declarations for dfile:molecules
!>

module reader_molecules
  use logging
  use dimensions
  use misc
  use molecules_ids
  implicit none


  ! Specifies how many molecules to read
  integer km_number

  integer km_lines_total  ! Total number of spectral line, counting all molecules

  character*80 km_titm, km_titulo

  dimension km_titulo(NUM_MOL)

  real*8, dimension(NUM_MOL) :: km_fe, km_do, &
   km_mm, km_am, km_bm, km_ua, km_ub, km_te, km_cro, &
   km_a0, km_a1, km_a2, km_a3, km_a4, km_als, km_s

  integer, dimension(NUM_MOL)  :: km_ise, km_nv, &
   km_lines_per_mol  !> This stores the number of spectral lines for each molecule

  real*8, dimension(MAX_NV_PER_MOL, NUM_MOL) :: km_qqv, km_ggv, km_bbv, km_ddv, km_fact


  !> "Index of Last Lambda Of Set-Of-Lines"
  !> Points to km_lmbdam, km_sj, km_jj.
  !> This is mounted at reading to help with the filtering and avoid
  !> allocating 2 dimensions for (lm__lmbdam, km_sj, km_jj)
  real*8, dimension(MAX_NV_PER_MOL, NUM_MOL) :: km_iollosol

  real*8,  dimension(MAX_KM_LINES_TOTAL) :: &
   km_lmbdam, &
   km_sj,     &
   km_jj

contains
  !=======================================================================================
  !> Reads file dfile:molecules to fill variables km_*
  !>
  !> Reads molecular lines
  !>
  !> Note: The whole file is read into memory, independent of which molecules are "switched on".
  !>       There is not much of a point in skipping molecules here, because the space is already
  !>       pre-allocated, and I have to read the whole file anyway, so it is much easier
  !>       programming-wise (and not time-costly either) to filter
  !>       molecules in filter_molecules() when they are already in memory.
  !>
  !> @todo TOP PRIORITY Number of molecules ON: consider km_number; search for km_number; also wrong references to NUM_MOL, search for NUM_MOL

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
    write (lll,*) 'MAX_KM_LINES_TOTAL = ', MAX_KM_LINES_TOTAL
    call log_debug(lll)


    ! row 01:
    ! BLB: NUMBER -- number of molecules do be considered
    ! Note: This is no longer used for anything, now the molecules to be switched on/off are configured

    read(unit_,*) km_number

    ! spill check
    if (km_number .gt. NUM_MOL) then
      call pfant_halt("Number of molecules ("//int2str(km_number)// &
       ") exceeds maximum allowed ("//int2str(NUM_MOL)//")")
    end if

    ! Deactivates molecules not wanted or not present in the file
    do molid = km_number+1, NUM_MOL
      call add_molid_off(molid)
    end do


    ! row 02: string containing list of names of all molecules
    read(unit_,'(a)') km_titm
    !~READ(UNIT_,'(20A4)') km_TITM

    !#logging
    write(lll, *) 'titm--------------', km_titm
    call log_debug(lll)

    ! BLB:
    ! BLB: km_NV -- number of transitions (v', v'') for each molecule
    ! BLB: Example: if (0,0)(1,1)(2,2) are considered for CH
    ! BLB:             (1,1)(2,2) are considered for CN
    ! BLB:             NV(J) = 3 2
    read(unit_,*) (km_nv(molid), molid=1,km_number)

    ! spill check
    do molid = 1, km_number
      if (km_nv(molid) .gt. MAX_NV_PER_MOL) then
          call pfant_halt('read_molecules(): molecule id '//int2str(molid)//&
           ' has nv = '//int2str(km_nv(molid))//' (maximum is MAX_NV_PER_MOL='//&
           int2str(MAX_NV_PER_MOL)//')')
        end if
    end do

    i_line = 0
    do molid = 1, km_number

      !> @todo check spill in each element in km_NV

      ! BLB:
      ! BLB: title -- specifying the molecule to follow
      ! BLB:          format: 20A4
      read(unit_,'(a)') km_titulo(molid)

      !#logging
      write(lll,*) 'molecule id ', molid
      call log_debug(lll)
      write(lll,*) 'titulo:  ', km_titulo(molid)
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

      read(unit_,*) km_fe(molid), km_do(molid), km_mm(molid), &
       km_am(molid), km_bm(molid), km_ua(molid), &
       km_ub(molid), km_te(molid), km_cro(molid)


      !> @todo ISSUE Documentation
      !> @todo ISSUE !P! My sample file is blank here
      read(unit_,'(2x,i3, 5f10.6, 10x, f6.3)') km_ise(molid), &
       km_a0(molid), km_a1(molid), km_a2(molid), &
       km_a3(molid), km_a4(molid), km_als(molid)

      !> @todo issue ?doc? is S??
      read(unit_,*) km_s(molid)

      nnv = km_nv(molid)

      !#logging
      write(lll,*) 'nv=', nnv
      call log_debug(lll)

      !> @todo type in documentation
      read(unit_,*) (km_qqv(i, molid), i=1,nnv)
      read(unit_,*) (km_ggv(i, molid), i=1,nnv)
      read(unit_,*) (km_bbv(i, molid), i=1,nnv)
      read(unit_,*) (km_ddv(i, molid), i=1,nnv)
      read(unit_,*) (km_fact(i, molid),i=1,nnv)

      do i = 1,nnv
        km_ddv(i, molid)=1.e-6*km_ddv(i, molid)
      end do


      !~L = (MOLID-1)*MAX_LINES_PER_MOL+1  ! Initial index for all vectors inside this loop
      j_set = 0
      j_line = 0 ! Counts how many lines per molecule and stores.
      do while (.true.)
        i_line = i_line+1

        ! spill check: checks if exceeds maximum number of elements allowed
        if (i_line .gt. MAX_KM_LINES_TOTAL) then
          call pfant_halt('read_molecules(): exceeded maximum number of total '//&
            'spectral lines  MAX_KM_LINES_TOTAL= '//int2str(MAX_KM_LINES_TOTAL)//&
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
        ! BLB: ITRANS -- key to indicate which is the (v',v'') -- only used in isotropic calculations ISSUE: !P! missing from sample file dfile:molecules
        !> @todo ISSUE (question) Where does dfile:molecules come from?
        ! BLB: NUMLIN -- key as table:
        ! BLB:           = 1 for the last line of a given (v',v'') set of lines of a given molecule
        ! BLB:           = 9 for the last line of the last (v', v'') set of lines of a given molecule
        read(unit_,*) km_lmbdam(i_line), km_sj(i_line), km_jj(i_line), iz, numlin

        !~km_NUMLIN(J_LAMBDA, MOLID) = NUMLIN

        if (numlin .ne. 0) then
          j_set = j_set+1
          km_iollosol(j_set, molid) = i_line
        end if

        j_line = j_line+1

        if (numlin .eq. 9) exit
      end do

      ! consistency check: J_SET must match NNV
      if(j_set .ne. nnv) then
        call pfant_halt('read_molecules():  incorrect number of set-of-lines: '//&
         int2str(j_set)//' (should be '//int2str(nnv)//') (in molecule number '//&
         int2str(molid)//')')
      end if

      km_lines_per_mol(molid) = j_line

      !#logging
      write(lll,*) 'This molecule has ', j_line, ' lines'
      call log_debug(lll)
    end do

    km_lines_total = i_line

    close(unit_)
  end
end


!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!||| MODULE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

!> Imports all reader_* modules

module readers
  use reader_main
  use reader_dissoc
  use reader_modeles
  use reader_hmap
  use reader_filetoh
  use reader_absoru2
  use reader_abonds
  use reader_partit
  use reader_gridsmap
  use reader_atoms
  use reader_molecules
end

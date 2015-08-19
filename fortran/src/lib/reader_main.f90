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

!> Reading routines and variable declarations for infile:main

module reader_main
  use logging
  use max_
  use reader_dissoc
  use config_base
  implicit none

  !> Flag indicating whether read_main() has already been called.
  logical :: flag_read_main   = .false.

  character*64 main_fn_flux  !< ?doc?
  logical   main_ptdisk    !< ?doc?
  real*8 :: &
   main_pas,    & !< ?doc?
   main_echx,   & !< ?doc?
   main_echy,   & !< ?doc?
   main_mu,     & !< ?doc?
   main_afstar, & !< ?doc?
   main_llzero, & !< ?doc?
   main_llfin,  & !< ?doc?
   main_aint,   & !< ?doc?
   main_teff,   & !< ?doc?
   main_glog,   & !< ?doc?
   main_asalog, & !< ?doc?
   main_nhe       !< ?doc?
  !> "Full-width-half-maximum" of Gaussian function for
  !> convolution of calculated spectrum; used only by nulbad executable
  real*8 :: main_fwhm

  integer   main_ivtot, & !< affects turbul() issue ?what? ?doc?
                          !< = 1 -- "vt" constant
                          !< > 1 -- "vt" variable
            main_inum     !< record id within modeles.mod
  character*64 main_filetohy(MAX_FILETOH_NUMFILES) !< ?doc? Names of ten outputs files that will contain ???
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
  !> Reads infile:main to fill variables main_*
  !>
  !> @note Must be called after read_dissoc().
  !> @todo ISSUE Explain the vvt case VERY WELL because it is an "anomaly", i.e., infile:main will have MORE LINES
  !> (MT) Yes it makes sense, it specifies microturbulence velocities for each layer of the atmosphere

  subroutine read_main(path_to_file, flag_care_about_dissoc, flag_read_filetoh)
    character(len=*), intent(in) :: path_to_file
    !> (=.true.) If .false., won't bother about reading infile:dissoc first and
    !> will skip the xxcor line. Only pfant bother about the xxcor line; other executables
    !> want to use infile:main without having to read infile:dissoc first.
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
    if (flag_care_about_dissoc_) then
      read(UNIT_, *) (main_xxcor(i), i=1, dissoc_nmetal)
    else
      read(UNIT_, *) ! skips the line
    end if

    ! row 08 -- part of a file name
    ! This line will define the names of three output files:
    !   fn_flux.cont
    !   fn_flux.norm
    !   fn_flux.spec
    read(UNIT_, '(a)') main_fn_flux

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

      !#spill_check
      if (ih .gt. MAX_FILETOH_NUMFILES) then
        call pfant_halt('Too many filetoh files specified (maximum is '//&
         'MAX_FILETOH_NUMFILES='//int2str(MAX_FILETOH_NUMFILES)//')')
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

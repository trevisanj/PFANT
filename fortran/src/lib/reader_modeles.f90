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

!> Reading routines and variable declarations for infile:modeles
!>
!> @todo (MT) infile:modeles could become an ASCII file

module reader_modeles
  use logging
  use max_
  use reader_main
  implicit none

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

contains
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
end

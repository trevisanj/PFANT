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

!> Reads and filters infile:atomgrade

module atomgrade
  use read_most_files
  implicit none

  integer, parameter :: &
   MAX_ATOMGRADE_R_NBLEND=14000, & !< Half of the maximum the number of rows in infile:atomgrade
   MAX_ATOMGRADE_NBLEND=8000      !< Maximum number of spectral lines possible within the interval LZERO, LFIN

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

contains
  !=======================================================================================
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
        call pfant_halt('read_atomgrade(): exceeded maximum of MAX_ATOMGRADE_R_NBLEND='//&
         int2str(MAX_ATOMGRADE_R_NBLEND)//' spectral lines')
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
      !> @todo issue why name was "abondraih"?? did it mean "abundances of hydrogen lines"? i ask this because if it has really a physical meaning, i shouldn't bury this inside read_most_files.f

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


  !=======================================================================================
  !> Selects only spectral lines within range lzero, lfin + performs "inner join".
  !>
  !> Populates variables atomgrade_* (single underscore)

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


        !__spill check__: checks if exceeds maximum number of elements allowed
        if (k .gt. MAX_ATOMGRADE_NBLEND) then
          call pfant_halt('filter_atomgrade(): exceeded maximum of MAX_ATOMGRADE_NBLEND='//&
           int2str(MAX_ATOMGRADE_NBLEND)//' spectral lines')
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
end

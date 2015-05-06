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

C> CONFIG module:
C> - Configuration globals with their default values
C> - Routines to parse command-line arguments
C> - All globals have prefix "config_"
C>
MODULE CONFIG
  USE LOGGING

  INTEGER, PARAMETER :: NUM_MOL=21  ! Number of molecules configured in the program.
                                    ! Conceptually, this should be defined in molecula.f, but there would be cyclic USEs

  INTEGER config_logging_LEVEL /logging_INFO/
  LOGICAL config_DEBUG /.FALSE./    ! Allows for debugging messages



  !=====
  ! File names
  
  CHARACTER*256 config_FN_DISSOC


  !=====
  ! Variables related to molecules
  !=====

  ! These are configurable
  INTEGER config_NUM_MOL_OFF /0/ ! Number of molecules switched off (excluded from calculations)
  INTEGER, DIMENSION(NUM_MOL) :: config_MOLIDS_OFF  ! IDs=indexes of the molecules to be switched off

  ! These are filled by MAKE_MOLIDS()
  INTEGER, DIMENSION(NUM_MOL) :: config_MOLIDS_ON  ! List of molecule IDs. Valid indexes range from 1 to km__NUMBER
  INTEGER config_NUM_MOL_ON  ! This is actually = NUM_MOL-config_NUM_MOL_OFF


  !=====
  ! Misc
  !=====


  ! Interpolation type of turbul_VT
  ! 1: linear
  ! 2: parabolic
  INTEGER config_INTERP /1/


  ! Selector for subroutines FLIN1() and FLINH():
  ! 0: (default) integration using 6/7 points depending on main_PTDISK;
  ! 1: 26-point integration
  INTEGER config_KIK /0/



  !=====
  ! Private variables
  !=====
  LOGICAL, PRIVATE :: FLAG_SETUP = .FALSE.

  PRIVATE STR2INT

CONTAINS


  C================================================================================================================================
  C> Does various setup operations.
  C>
  C>   - sets up configuration defaults,
  C>   - parses command-line arguments, and
  C>   - does other necessary operations.
  C>
  C> Must be called at system startup
  C
  SUBROUTINE CONFIG_SETUP()
    USE LOGGING
    USE argparser
      IMPLICIT NONE

    ! TODO parse command-line here

    CALL parseargs()



    logging_LEVEL = config_logging_LEVEL  ! sets logging level at LOGGING module based on config variable


    CALL MAKE_MOLIDS()
    FLAG_SETUP = .TRUE.

  END



  !================================================================================================================================
  !> Returns molecule ID given index
  !>
  !> Molecule ID is a number from 1 to NUM_MOL, which is uniquely related to a chemical molecule within PFANT.
  !
  FUNCTION GET_MOLID(I_MOL)
    IMPLICIT NONE
    INTEGER I_MOL, GET_MOLID
    CHARACTER*128 S  !__logging__

    !--assertion--!
    IF (.NOT. FLAG_SETUP) CALL PFANT_HALT('GET_MOLID(): forgot to call CONFIG_SETUP()')

    !--spill check--!
    IF (I_MOL .GT. config_NUM_MOL_ON) THEN
      WRITE (S, *) 'GET_MOLID(): Invalid molecule index I_MOL (', &
       I_MOL, ') must be maximum ', config_NUM_MOL_ON
      CALL PFANT_HALT(S)
    END IF

    GET_MOLID = config_MOLIDS_ON(I_MOL)
    RETURN
  END

  !================================================================================================================================
  !> Returns .TRUE. or .FALSE. depending on whether molecule represented by MOLID is "on" or "off"

  FUNCTION MOLECULE_IS_ON(MOLID)
    IMPLICIT NONE
    INTEGER MOLID, J
    LOGICAL MOLECULE_IS_ON

    !--assertion--!
    IF (.NOT. FLAG_SETUP) &
     CALL PFANT_HALT('MOLECULE_IS_ON(): ' &
     //'forgot to call CONFIG_SETUP()')

    MOlECULE_IS_ON = .TRUE.
    DO J = 1, config_NUM_MOL_OFF
      IF (MOLID .EQ. config_MOLIDS_OFF(J)) THEN
        MOLECULE_IS_ON = .FALSE.
        EXIT
      END IF
    END DO
  END


  !================================================================================================================================
  !> Fills config_MOLIDS_ON and config_NUM_MOL_ON
  !
  SUBROUTINE MAKE_MOLIDS()
    IMPLICIT NONE
    INTEGER I_MOL, J, MOLID
    LOGICAL IS_OFF

    I_MOL = 0
    DO MOLID = 1, NUM_MOL
      IS_OFF = .FALSE.  ! Whether molecule I_MOL is off
      DO J = 1, config_NUM_MOL_OFF
        IF (MOLID .EQ. config_MOLIDS_OFF(J)) THEN
          IS_OFF = .TRUE.
          EXIT
        END IF
      END DO
      IF (.NOT. IS_OFF) THEN
        I_MOL = I_MOL+1
        config_MOLIDS_ON(I_MOL) = MOLID
      END IF
    END DO
    config_NUM_MOL_ON = I_MOL
  END


  !================================================================================================================================
  !> Parses and validates all command-line arguments.
  !>
  
  !
  subroutine parseargs()
    use options2
    use CONFIG
    use logging
    implicit none
    integer k  ! TODO for debugging, take it out
    integer o_len, o_stat, o_remain, o_offset, o_index, iTemp
    character*500 o_arg
    character*128 lll
    logical err_out
    type(option) options(3), opt

    options(1) = option('loglevel', 'l', .TRUE., 'Logging level (1: debug; 2: info; 3: warning; 4: error; 5: critical; 6: halt)', 'level')
    options(2) = option('interp', 'i', .TRUE., 'Interpolation type for subroutine TURBUL() (1: linear; 2: parabolic)', 'type')
    options(3) = option('kik', 'i', .TRUE., 'Selector for subroutines FLIN1() and FLINH() (0 (default): integration using 6/7 '//&
     'points depending on main_PTDISK; 1: 26-point integration)', 'type')


    err_out = .FALSE.

    do while (.TRUE.)
      call getopt(options, o_index, o_arg, o_len, o_stat, o_offset, o_remain)

      write(*,*) 'o_index = ', o_index
      write(*,*) 'o_arg = ', o_arg
      write(*,*) 'o_len = ', o_len
      write(*,*) 'o_stat = ', o_stat
      write(*,*) 'o_offset = ', o_offset
      write(*,*) 'o_remain = ', o_remain
      write(*,*) '---------------------------'

      select case(o_stat)
        case (1,2,3)  ! parsing stopped (no error)
           exit
        case (0)  ! option successfully parsed
          opt = options(o_index)

          ! "Uses" config options: validates and assigns to proper config_* variables.
          select case(opt%name)  ! It is more legible select by option name than by index
            case ('loglevel')
              iTemp = parseint(opt, o_arg)
              select case (iTemp)
                case (1, 2, 3, 4, 5, 6)
                  config_logging_LEVEL = iTemp*10
                  write(*,*) 'setting logging level to ', config_logging_LEVEL
                case default
                  err_out = .TRUE.
              end select

            case ('interp')
              iTemp = parseint(opt, o_arg)
              select case (iTemp)
                case (1, 2)
                  config_INTERP = iTemp
                  write(*,*) 'setting config_interp to ', config_INTERP
                case default
                  err_out = .TRUE.
              end select
              
            case ('kik')
              iTemp = parseint(opt, o_arg)
              select case(iTemp)
                case (0, 1)
                  config_KIK = iTemp
                case default
                  err_out = .TRUE.
          end select

          if (err_out) then
            write (lll, *) 'Argument out of range for option ', get_option_name(opt)
            call PFANT_HALT(lll)
          end if

      end select

      k = k+1
      if (k == 20) then
        stop 'sort this shit'
      end if
    end do
  end


END MODULE










!> Maintains a list of molecules that are on/off
!>
!> Has routines to maintain the list and retrieve information.

module molecules_ids
  use logging
  use misc
  use max_
  implicit none

  type molid_list
    integer :: n_off, & !< number "off"
               n_on     !< number "on"

    integer, dimension(NUM_MOL) :: &
      on,  & !< molecule ids "on"
      off    !< molecule ids "off"
  end type

  type(molid_list) :: molids

  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  ! There is a calling order to be observed. These flags + assertions inforce that
  logical, private :: &
   flag_molecules_ids_init = .false.   !< molecules_ids_init() has been called?

  private make_molids_on
contains

  !> Module initialization:
  !> Must be called at some point of system startup.

  subroutine molecules_ids_init()
    call make_molids_on()
    flag_molecules_ids_init = .true.
  end

  !=======================================================================================
  !> Returns molecule id given index
  !>
  !> Molecule id is a number from 1 to NUM_MOL, which is uniquely related to a chemical molecule within pfant.

  function get_molid(i_mol)
    integer i_mol, get_molid

    if (.not. flag_molecules_ids_init) then
      call pfant_halt('get_molid(): forgot to call molecules_ids_init()', is_assertion=.true.)
    end if

    !#spill_check
    if (i_mol .gt. molids%n_on) then
      write (lll, *) 'get_molid(): invalid molecule index i_mol (', &
       i_mol, ') must be maximum ', molids%n_on
      call pfant_halt(lll)
    end if

    get_molid = molids%on(i_mol)
  end

  !=======================================================================================
  !> Returns .TRUE. or .FALSE. depending on whether molecule represented by molid is "on"
  !> or "off"
  !>
  !> Can be called anytime

  function molecule_is_on(molid)
    integer molid, j
    logical molecule_is_on

    if (.not. flag_molecules_ids_init) then
      call pfant_halt('get_molid(): forgot to call molecules_ids_init()', is_assertion=.true.)
    end if

    molecule_is_on = .true.
    do j = 1, molids%n_off
      if (molid .eq. molids%off(j)) then
        molecule_is_on = .false.
        exit
      end if
    end do
  end

  !=======================================================================================
  !> Adds molecule id to list of "off" molecules

  subroutine add_molid_off(molid)
    integer, intent(in) :: molid !< molecule id

    if (.not. flag_molecules_ids_init) then
      call pfant_halt('get_molid(): forgot to call molecules_ids_init()', is_assertion=.true.)
    end if

    !#spill_check
    if (molid .gt. NUM_MOL .or. molid .lt. 1) then
      call pfant_halt('Invalid molecule id: '//int2str(molid)//' (valid: 1 to '//&
       int2str(NUM_MOL)//')')
    end if

    if (molecule_is_on(molid)) then
      ! The condition we-re in prevents duplication
      molids%n_off = molids%n_off+1
      molids%off(molids%n_off) = molid
      call log_info('molecule id '//int2str(molid)//' added to molids%off')
    else
      call log_warning('molecule id '//int2str(molid)//' *already turned off*')
    end if

    call make_molids_on()
  end

  !=======================================================================================
  !> Fills molids%on and molids%n_on based on their complements.
  !>
  !> @todo see what is public and what is private in this module

  subroutine make_molids_on()
    integer i_mol, j, molid
    logical is_off

    i_mol = 0
    do molid = 1, NUM_MOL
      is_off = .false.  ! Whether molecule I_MOL is off
      do j = 1, molids%n_off
        if (molid .eq. molids%off(j)) then
          is_off = .true.
          exit
        end if
      end do
      if (.not. is_off) then
        i_mol = i_mol+1
        molids%on(i_mol) = molid
      end if
    end do
    molids%n_on = i_mol
  end
end

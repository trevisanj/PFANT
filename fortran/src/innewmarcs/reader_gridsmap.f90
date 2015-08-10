!> Reading routines and variable declarations for infile:gridsmap
!>
!> @li gridsmap -


module reader_gridsmap
  use reader_modeles
  use qsort
  use logging
  use config_innewmarcs
  implicit none

  ! Variables related to the reference models
  integer, parameter:: MAX_GRIDSMAP_NUM_FILES = 10
  integer gridsmap_num_files
  real*4, dimension(MAX_GRIDSMAP_NUM_FILES) :: gridsmap_asalog
  character*64, dimension(MAX_GRIDSMAP_NUM_FILES) :: gridsmap_fn

contains
  !=======================================================================================
  !> Reads map of models: file "index.dat"
  !>
  !> By convention, this file is called "index.dat". The information it contains is
  !> just a list of .mod files. Files needn't be sorted. Metallicities are taken from the
  !> first record of each file and the files are sorted in ascending order of metallicity
  !>
  !> This this is necessary because Fortran doesn't have a cross-platform solution for
  !> listing the files in a directory.
  !>
  !> @note first and second rows of file are skipped.
  !>

  subroutine read_gridsmap()
    character*64 :: t_fn
    character(len=:), allocatable :: path_to_file
    integer, parameter :: UNIT_ = 199
    type(modele_record) :: rec
    character*64 :: temp_fn(MAX_GRIDSMAP_NUM_FILES)
    real*8 :: temp_asalog(MAX_GRIDSMAP_NUM_FILES) ! have to declare as real8 for the quicksort routine
    integer :: order(MAX_GRIDSMAP_NUM_FILES), i

    path_to_file = full_path_gridsdir('index.dat')

    open(unit=UNIT_,file=path_to_file, status='old')

    ! rows 01, 02: Skipped (may contain description)
    read(UNIT_, *)
    read(UNIT_, *)


    gridsmap_num_files = 0
    do while (.true.)
      ! rows 03...etc: filename
      read(UNIT_, *, end=10) t_fn

      ! Opens .mod file to get metallicity from its first record
      ! (metallicity should be the same for all records)
      call open_mod_file(full_path_gridsdir(t_fn))
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
    call log_info('List of model grid files: # asalog filename')
    do i = 1, gridsmap_num_files
      11 format(26x,i1,' ', f6.3,' ',a)
      write(lll, 11) i, gridsmap_asalog(i), gridsmap_fn(i)
      call log_info(lll)
    end do

    close(unit=UNIT_)
  end
end

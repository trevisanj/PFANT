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

!> innewmarcs-specific configuration

module config_innewmarcs
  use config_base
  use misc
  use read_most_files
  implicit none

  integer, parameter :: LEN_TIRB = 15 ! size of variable config_tirb

  ! note: maintained variable names found in original innewmarcs.f

  character*20 :: config_open_status = 'unknown' !< option: --open_status

  character*192 :: config_refdir = '.' !< option: --refdir

  character*64 :: &
   config_nomfimod = 'modeles.mod', & !< option: --nomfimod
   config_nomfidat = 'modeles.dat'    !< option: --nomfidat
  character*25 :: config_modcode = 'NoName' !< option: --modcode
  !> option: --tirb
  !> @sa get_tirb()
  character(LEN_TIRB) :: config_tirb = '?'
  !> option: --teff
  !> @sa get_teff()
  real*4 :: config_teff = -1
  !> option: --glog
  !> @sa get_teff()
  real*4 :: config_glog = -1
  !> option: --amet
  !> @sa get_amet()
  real*4 :: config_amet = -1
  !> option: --id
  !> @sa get_id()
  integer :: config_id = 0


  private assure_read_main
contains
  !=======================================================================================
  !> Initializes the options list
  !> @note It is possible to break description lines using &lt;br&gt;
  !>
  !> @note To indent 2nd, 3rd etc. lines of a paragraph, use the @c IND constant after a
  !> &lt;br&gt;

  subroutine init_options()
    integer i, j, k
    character(:), allocatable :: name
    character(1) :: chr


    k = k+1
    options(k) = option('refdir',' ', .true., 'directory name', config_refdir, &
     'Directory containing reference atmospheric models.<br>'//&
     'This directory must contain a file named "modelmap.dat" and<br>'//&
     'several ".mod" binary files. See inewmarcs.f90::read_modelmap() for more info.')

    k = k+1
    options(k) = option('open_status',' ', .true., 'string', config_open_status, &
     'File open mode for binary file<br>'//&
     IND//'new: file must not exist<br>'//&
     IND//'old: file must exist<br>'//&
     IND//'replace: replaces file if exists, otherwise creates new')

    k = k+1
    options(k) = option('nomfimod',' ', .true., 'file name', config_nomfimod, &
     'Name of binary file<br>'//&
     '*Note*: file is opened in directory specified in --inputdir')

    k = k+1
    options(k) = option('nomfidat',' ', .true., 'file name', config_nomfidat, &
     'Name of ASCII file<br>'//&
     '*Note*: file is opened in directory specified in --inputdir')

    k = k+1
    options(k) = option('modcode',' ', .true., 'string up to 25 characters', config_modcode, &
     '"Model name"')

    k = k+1
    options(k) = option('tirb',' ', .true., 'string up to 15 characters', config_tirb, &
     '"Titre"')

    k = k+1
    options(k) = option('teff',' ', .true., 'real value', real42str(config_teff), &
     '"Teff"')

    k = k+1
    options(k) = option('glog',' ', .true., 'real value', real42str(config_glog), &
     '"log g"')

    k = k+1
    options(k) = option('amet',' ', .true., 'real value', real42str(config_amet), &
     '"[M/H]"')

    k = k+1
    options(k) = option('id',' ', .true., 'real value', '<"inum" value in main configuration file>', &
     'Record id within binary file. If not specified, takes value of '//&
     'main_inum variable (last value of 4th row of main configuration file)')
  end


  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_init()
    ex_config_option_handler => config_handle_option
    ex_config_num_options = ex_config_num_options+10
    call config_init_options()
    call config_base_init()
  end

  !=======================================================================================
  !> Handles options for innewmarcs executable

  function config_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res, iTemp

    res = HANDLER_OK

    select case(opt%name)
      case ('refdir')
        call parse_aux_assign_fn(o_arg, config_refdir, 'refdir')
      case ('open_status')
        call parse_aux_assign_fn(o_arg, config_open_status, 'open_status')
      case ('nomfimod')
        call parse_aux_assign_fn(o_arg, config_nomfimod, 'nomfimod')
      case ('nomfidat')
        call parse_aux_assign_fn(o_arg, config_nomfidat, 'nomfidat')
      case ('modcode')
        call parse_aux_assign_fn(o_arg, config_modcode, 'modcode')
      case ('tirb')
        call parse_aux_assign_fn(o_arg, config_tirb, 'tirb')
      case ('teff')
        config_teff = parse_aux_str2real4(opt, o_arg)
        call log_assignment('config_teff', real42str(config_teff))
      case ('glog')
        config_glog = parse_aux_str2real4(opt, o_arg)
        call log_assignment('config_glog', real42str(config_glog))
      case ('amet')
        config_amet = parse_aux_str2real4(opt, o_arg)
        call log_assignment('config_amet', real42str(config_amet))
      case ('id')
        config_id = parse_aux_str2int(opt, o_arg)
        if (config_id .lt. 1) then !#validation
          res = HANDLER_ERROR
        else
          call log_assignment('config_id', int2str(config_id))
        end if
      case default
        ! if does not handle here, passes on to base handler
        res = config_base_handle_option(opt, o_arg)
    end select
  end










move logic to inewmarcs in sequential manner, no need for subroutines n shit
call read_main, yes, then do subsequent shit
but I may put everything inside a initialization subroutine

  !=======================================================================================
  !> Returns record id.
  !>
  !> config_id has preference, but if 0 (i.e., not set by command-line),
  !> then returns read_most_files::main_inum

  integer function get_id() result(res)
    if (config_id .lt. 1) then
      call assure_read_main()
      if (main_inum .lt. 1) then
        ! note: here this consistency check is considered an assertion, because it should
        ! be validated upon file reading.
        call pfant_halt('Invalid value for main_inum: '//int2str(main_inum), is_assertion=.true.)
      end if
      res = main_inum
    else
      res = config_id
    end if
  end

  !=======================================================================================
  !> Returns title "tirb".
  !>
  !> config_tirb has preference, but if '?' (i.e., not set by command-line),
  !> then returns read_most_files::main_titrav

  character(LEN_TIRB) function get_tirb() result(res)
    if (config_tirb .eq. '?') then
      call assure_read_main()
      res = main_titrav
    else
      res = config_tirb
    end if
  end

  !=======================================================================================
  !> Returns teff.
  !>
  !> config_teff has preference, but if -1 (i.e., not set by command-line),
  !> then returns read_most_files::main_teff

  real*4 function get_teff() result(res)
    if (config_teff .eq. -1) then
      call assure_read_main()
      res = real(main_teff)  ! explicit real(8)-to-real(4) conversion to shut up warning
    else
      res = config_teff
    end if
  end

  !=======================================================================================
  !> Returns log g
  !>
  !> config_glog has preference, but if -1 (i.e., not set by command-line),
  !> then returns main_glog

  real*4 function get_glog() result(res)
    if (config_glog .eq. -1) then
      call assure_read_main()
      res = real(main_glog) ! explicit real(8)-to-real(4) conversion to shut up warning
    else
      res = config_glog
    end if
  end

  !=======================================================================================
  !> Returns metallicity
  !>
  !> config_amet has preference, but if -1 (i.e., not set by command-line),
  !> then returns main_asalog

  real*4 function get_amet() result(res)
    if (config_amet .eq. -1) then
      call assure_read_main()
      res = real(main_asalog) ! explicit real(8)-to-real(4) conversion to shut up warning
    else
      res = config_amet
    end if
  end


  !=======================================================================================
  !> Makes sure that read_main() has been called

  subroutine assure_read_main()
    if (.not. flag_read_main) then
      call read_main(full_path_i(config_fn_main), flag_care_about_dissoc=.false.)
    end if
  end
end module config_innewmarcs

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

!> hydro2-specific configuration
!>
!> @todo issue ask blb lots of options in infile:main are also configurable through command-line
!> and repeated in innewmarcs. Ít could be defined that the source for teff, glog, asalog, inum, ptdisk
!> will be infile:main and remove these config options. But time will tell. With pypfant we will be able
!> to see how the program will be used etc.

module config_hydro2
  use config_base
  use misc
  implicit none

  !> option: --teff
  !> @sa get_teff()
  real*4 :: config_teff = -1
  !> option: --glog
  !> @sa get_teff()
  real*4 :: config_glog = -1
  !> option: --asalog.
  !> @note Name changed from "amet" to "asalog" to conform with pfant and hydro2
  !> @sa get_asalog()
  real*4 :: config_asalog = -1
  !> option: --inum
  !> @sa get_id()
  integer :: config_inum = 0

  !> option: --ptdisk
  !>
  !> This is about the "number of points" for subroutine fluxis(). Feeds into variable
  !> x _ptdisk, which is actually logical. But I needed to make it integer here because
  !> I need a tristate variable to flag whether it is kept unitialized (-1).
  !> @li -1 unitialized (hydro2_init() will use main_ptdisk instead)
  !> @li 0 false
  !> @li 1 true
  integer :: config_ptdisk = -1

  !> option: --kik
  !> @note This was called "IOP" but was changed to match variable name in pfant executable
  integer :: config_kik = 0

  !> option: --amores
  !>
  !> This is a tristate variable, as config_ptdisk, because it has no default and is
  !> initially set to -1, meaning that it is unitialized.
  integer config_amores = -1

  !> option: --kq
  integer config_kq = -1

  character*192 :: config_refdir = '.' !< option: --refdir
  character*64 :: &
   config_nomfimod = 'modeles.mod', & !< option: --nomfimod
   config_nomfidat = 'modeles.dat'    !< option: --nomfidat
  character*25 :: config_modcode = 'NoName' !< option: --modcode
  !> option: --tirb
  !> @sa hydro2_init()
  character(LEN_TIRB) :: config_tirb = '?'
  !> option: --teff
  !> @sa get_teff()
  real*4 :: config_teff = -1
  !> option: --glog
  !> @sa get_teff()
  real*4 :: config_glog = -1
  !> option: --asalog.
  !> @note Name changed from "amet" to "asalog" to conform with pfant and hydro2
  !> @sa get_asalog()
  real*4 :: config_asalog = -1
  !> option: --id
  !> @sa get_id()
  integer :: config_inum = 0

  !> option: --nomplot
  character*16 :: nomplot = '?'

contains
  !=======================================================================================
  !> Initializes hydro2-specific options

  integer function config_init_options(k)
    !> k is the index of the last initialized option
    integer, intent(in) :: k


    k = k+1
    options(k) = option('teff',' ', .true., 'real value', real42str(config_teff), &
     '"Teff"')

    k = k+1
    options(k) = option('glog',' ', .true., 'real value', real42str(config_glog), &
     '"log g"')

    k = k+1
    options(k) = option('asalog',' ', .true., 'real value', real42str(config_asalog), &
     '"[M/H]"')

    k = k+1
    options(k) = option('inum',' ', .true., 'real value', '<"main_inum" variable (taken from main configuration file)>', &
     'Record id within atmospheric model binary file')

    k = k+1
    options(k) = option('ptdisk',' ', .true., 'T/F', '<"main_ptdisk" variable from main configuration file>', &
     'option for subroutine fluxis()<br>'//&
     IND//'T: 7-point integration<br>'//&
     IND//'F: 6- or 26-point integration, depending on option kik')

    k = k+1
    options(k) = option('kik', ' ', .TRUE., '0/1', int2str(config_kik), &
     'option for subroutine fluxis()<br>'//&
     IND//'0: integration using 6/7 points depending on option ptdisk;<br>'//&
     IND//'1: 26-point integration)')

    k = k+1
    options(k) = option('amores',' ', .true., 'T/F', '(no default)', &
     'AMOrtissement de RESonnance?')

    k = k+1
    options(k) = option('kq', ' ', .true., '0/1', '(no default)', &
     '"Theorie"<br>'//&
     IND//'0: THEORIE DE GRIEM;<br>'//&
     IND//'1: THEORIE QUASISTATIQUE')

    config_init_options = k
  end

  !=======================================================================================
  !> Executable-specific initialization + calls config_base_init()

  subroutine config_init()
    execonf_name = 'innwemarcs'
    execonf_handle_option => config_handle_option
    execonf_init_options => config_init_options
    execonf_num_options = 10
    call config_base_init()
  end

  !=======================================================================================
  !> Handles options for hydro2 executable

  function config_handle_option(opt, o_arg) result(res)
    type(option), intent(in) :: opt
    character(len=*), intent(in) :: o_arg
    integer :: res

    res = HANDLER_OK

    select case(opt%name)


      !> @todo issue duplicated in innwemarcs, don't like this; better read only from
      !> infile:main
      case ('teff')
        config_teff = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_teff', real42str(config_teff))
      case ('glog')
        config_glog = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_glog', real42str(config_glog))
      case ('asalog')
        config_asalog = parse_aux_str2real4(opt, o_arg)
        call parse_aux_log_assignment('config_asalog', real42str(config_asalog))
      case ('inum')
        config_inum = parse_aux_str2int(opt, o_arg)
        if (config_inum .lt. 1) then !#validation
          res = HANDLER_ERROR
        else
          call parse_aux_log_assignment('config_inum', int2str(config_inum))
        end if



      !>
      !> @todo issue this is also available in infile:main
      case ('ptdisk')
        ! This conversion to/from integer is because config_ptdisk is a tristate variable,
        ! but the user doesn't need to know this
        ! See hydro2_init() to see how it is treated
        config_ptdisk = logical2integer(parse_aux_str2logical(opt, o_arg))
        call parse_aux_log_assignment('config_ptdisk', integer2logical(logical2str(config_ptdisk)))

      case ('kik')
        config_kik = parse_aux_str2int(opt, o_arg)
        if (config_kik .eq. 0 .or. config_kik .eq. 1) then !#validation
          call parse_aux_log_assignment('config_kik', int2str(config_kik))
        else
          res = HANDLER_ERROR
        end if
      case ('amores')
        ! config_amores is also tristate, as config_ptdisk
        config_amores = logical2integer(parse_aux_str2logical(opt, o_arg))
        call parse_aux_log_assignment('config_amores', integer2logical(logical2str(config_amores)))
      case default
        ! if does not handle here, passes on to base handler
        res = config_base_handle_option(opt, o_arg)
    end select
  end
end

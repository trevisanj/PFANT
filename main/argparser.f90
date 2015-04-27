module argparser

  private str2int

contains


  !================================================================================================================================
  !> Parses and validates all command-line arguments.
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
    type(option) options(2), opt

    options(1) = option('loglevel', 'l', .TRUE., 'Logging level (1-debug; 2-info; 3-warning; 4-error; 5-critical; 6-halt)', 'level')
    options(2) = option('interp', 'i', .TRUE., 'Interpolation type for subroutine TURBUL() (1-linear; 2-parabolic)', 'type')

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


end module

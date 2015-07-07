      ! A fortran2008 program for GFORTRAN
      ! By WQY
      program main
        use logging
        implicit none


        character(len=256) :: t
        integer shit /10/
        integer i


        write(t,*) 'banana', i, 'abacate', shit

        LOGging_level = logging_CRITICAL
        call critical(t)
        call ERROR(t)
        call WARNING(t)
        call INFO(t)
        call DEBUG(t)
        write(*,*) '================='

        LOGging_level = logging_ERROR
        call critical(t)
        call ERROR(t)
        call WARNING(t)
        call INFO(t)
        call DEBUG(t)
        write(*,*) '================='

        LOGging_level = logging_WARNING
        call critical(t)
        call ERROR(t)
        call WARNING(t)
        call INFO(t)
        call DEBUG(t)
        write(*,*) '================='

        LOGging_level = logging_INFO
        call critical(t)
        call ERROR(t)
        call WARNING(t)
        call INFO(t)
        call DEBUG(t)
        write(*,*) '================='

        LOGging_level = logging_DEBUG
        call critical(t)
        call ERROR(t)
        call WARNING(t)
        call INFO(t)
        call DEBUG(t)
        write(*,*) '================='


        call PFANT_HALT('------GAME OVER------')

        call INFO('Not gonna log this')

      end



*        character(len=:), allocatable :: s
*        character(len=256) :: t
*
*        i = 10232
*
*        allocate(s, source='shit')
*
*
*
*        write(t,*) 'banana', i, 'abacate', shit
*
*        call llog(s)
*
*        call llog2(t)
*
*
*      end
*
*
*      subroutine llog(s)
*            character(len=*) :: s
*
*            write(*,*) '#', s, '#'
*      end
*
*
*
*      subroutine llog2(t)
*            character(len=*) :: t
*
*            write(*,*) '#', trim(t), '#'
*      end

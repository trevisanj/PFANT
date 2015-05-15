!> Varying task (default: SYNTHESIS)
!>


!include 'read_files.f90'
!include 'read_files.f90'
!include 'read_files.f90'
!include 'read_files.f90'
!include 'read_files.f90'
include 'filetoh.f90'
include 'dissoc.f90'
include 'molecula.f90'
include 'read_files.f90'
include 'options2.f90'
include 'synthesis.f90'
include 'logging.f90'
include 'config.f90'
program pfant
  use config
  use logging
  use main_calc

  !=====
  ! Startup section
  !=====
  call config_setup()


  call log_debug('PFANT: about to execute task')

  !=====
  ! Task(s)
  !=====
  call synthesis_()


  call log_debug('PFANT: executed task without crashing!')
  call log_debug('PFANT: end')
end program pfant

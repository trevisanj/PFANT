!> Varying task (default: SYNTHESIS)
!>
PROGRAM PFANT
  USE CONFIG
  USE LOGGING

  !=====
  ! Startup section
  !=====
  CALL CONFIG_SETUP()


  CALL LOGGING_DEBUG('PFANT: about to execute task')

  !=====
  ! Task(s)
  !=====
  CALL SYNTHESIS()


  CALL LOGGING_DEBUG('PFANT: executed task without crashing!')
  CALL LOGGING_DEBUG('PFANT: end')
END PROGRAM PFANT

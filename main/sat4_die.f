      MODULE SAT4_DIE
      USE READ_FILES
      IMPLICIT NONE
      
      ! They will be pointer targets at molecula.f:POINT_PPA_PB()
      REAL, TARGET, DIMENSION(MAX_modeles_NTOT) :: sat4_PPH, sat4_PPC2,
     + sat4_PN,
     + sat4_PC13, sat4_PMG, sat4_PO, sat4_PTI, sat4_PFE
    
      
      END MODULE SAT4_DIE

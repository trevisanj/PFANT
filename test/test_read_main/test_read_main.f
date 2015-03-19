C     TEST THE READ_MAIN ROUTINE
      PROGRAM TEST_READ_MAIN
      USE READ_FILES

      NMETAL = 18  ! This must be set in order to read line 07

      CALL READ_MAIN('test_read_main.dat')

      WRITE(*, *)
      WRITE(*, *) 'TITRAV = ', main_TITRAV
      WRITE(*, *) 'ECRIT = ', main_ECRIT
      WRITE(*, *) 'PAS = ', main_PAS
      WRITE(*, *) 'ECHX = ', main_ECHX
      WRITE(*, *) 'ECHY = ', main_ECHY
      WRITE(*, *) 'FWHM = ', main_FWHM
      WRITE(*, *) 'VVT = ', main_VVT
      WRITE(*, *) 'IVTOT = ', main_IVTOT
      WRITE(*, *) 'TEFF = ', main_TEFF
      WRITE(*, *) 'GLOG = ', main_GLOG
      WRITE(*, *) 'ASALOG = ', main_ASALOG
      WRITE(*, *) 'PTDISK = ', main_PTDISK
      WRITE(*, *) 'MU = ', main_MU
      WRITE(*, *) 'AFSTAR = ', main_AFSTAR
      WRITE(*, *) 'XXCOR = ', main_XXCOR
      WRITE(*, *) 'FILEFLUX = ', main_FILEFLUX
      WRITE(*, *) 'LLZERO = ', main_LLZERO
      WRITE(*, *) 'LLFIN = ', main_LLFIN
      WRITE(*, *) 'AINT = ', main_AINT
      WRITE(*, *) 'INUM = ', main_INUM


      DO IH = 1, 10
        WRITE(*, *) 'FILETOHY(', IH, ') = ', main_FILETOHY(IH)
      END DO


      END


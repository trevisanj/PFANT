C     TEST THE READ_MAIN ROUTINE
      PROGRAM TEST_READ_MAIN
      USE COMMONS

      NMETAL = 18  ! This must be set in order to read line 07

      CALL READ_MAIN('test_read_main.dat')

      WRITE(*, *)
      WRITE(*, *) 'TITRAV = ', TITRAV
      WRITE(*, *) 'ECRIT = ', ECRIT
      WRITE(*, *) 'PAS = ', PAS
      WRITE(*, *) 'ECHX = ', ECHX
      WRITE(*, *) 'ECHY = ', ECHY
      WRITE(*, *) 'FWHM = ', FWHM
      WRITE(*, *) 'VVT = ', VVT
      WRITE(*, *) 'IVTOT = ', IVTOT
      WRITE(*, *) 'TEFF = ', TEFF
      WRITE(*, *) 'GLOG = ', GLOG
      WRITE(*, *) 'ASALOG = ', ASALOG
      WRITE(*, *) 'PTDISK = ', PTDISK
      WRITE(*, *) 'MU = ', MU
      WRITE(*, *) 'AFSTAR = ', AFSTAR
      WRITE(*, *) 'XXCOR = ', XXCOR
      WRITE(*, *) 'FILEFLUX = ', FILEFLUX
      WRITE(*, *) 'LLZERO = ', LLZERO
      WRITE(*, *) 'LLFIN = ', LLFIN
      WRITE(*, *) 'AINT = ', AINT


      DO IH = 1, 10
        WRITE(*, *) 'FILETOHY(', IH, ') = ', FILETOHY(IH)
      END DO


      END

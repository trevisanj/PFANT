C     TEST THE READ_MAIN ROUTINE      
      PROGRAM TEST_READ_MAIN
      USE COMMONS
      
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
      WRITE(*, *) 'FILETOHY = ', FILETOHY
 	  
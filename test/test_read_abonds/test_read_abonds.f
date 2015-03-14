
C     TEST THE READ_MAIN ROUTINE
      PROGRAM TEST_READ_ABONDS
      USE READ_FILES

      CALL READ_ABONDS('abonds.dat')

      WRITE (*, *) 'abonds_NABOND = ', abonds_NABOND
      WRITE (*, *) ''

      WRITE (*, *) 'EL Abundance'
      WRITE (*, *) '============'
      DO I = 1, abonds_NABOND
        WRITE(*, '(1X, A2, F10.3)') abonds_ELE(I), abonds_ABOL(I)
      END DO




      END

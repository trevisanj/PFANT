
C     TEST THE READ_MAIN ROUTINE
      PROGRAM TEST_READ_DISSOC
      USE COMMONS

      CALL READ_DISSOC('dissoc.dat')

      WRITE(*, *)
      WRITE(*, *) 'NMETAL = ', NMETAL
      WRITE(*, *) 'NIMAX = ', NIMAX
      WRITE(*, *) 'EPS = ', EPS
      WRITE(*, *) 'SWITER = ', SWITER

      WRITE (*, *) 'EL   AtomN      IP  IG0  IG1     CCLOG'
      WRITE (*, *) '======================================'
      DO I = 1, NMETAL
        WRITE(*, '(1X, A2, I6, F10.3, 2I5, F10.5)')
     1        DISSOC_SYMBOL(I), DISSOC_N(I), DISSOC_IP(I),
     2        DISSOC_IG0(I), DISSOC_IG1(I), DISSOC_CCLOG(I)
      END DO

      END


C     TEST THE READ_MAIN ROUTINE
      PROGRAM TEST_READ_DISSOC
      USE READ_FILES
      CHARACTER*256, PARAMETER :: fn='dissoc.dat'



      CALL READ_DISSOC(fn)

      WRITE(*, *)
      WRITE(*, *) 'NMETAL = ', dissoc_NMETAL
      WRITE(*, *) 'NIMAX = ', dissoc_NIMAX
      WRITE(*, *) 'EPS = ', dissoc_EPS
      WRITE(*, *) 'SWITER = ', dissoc_SWITER

      WRITE (*, *) 'EL   AtomN      IP  IG0  IG1     CCLOG'
      WRITE (*, *) '======================================'
      DO I = 1, dissoc_NMETAL
        WRITE(*, '(1X, A2, I6, F10.3, 2I5, F10.5)')
     1        dissoc_ELEMS(I), dissoc_NELEMX(I), dissoc__IP(I),
     2        dissoc__IG0(I), dissoc__IG1(I), dissoc__CCLOG(I)
      END DO


      WRITE(*, *) ''
      WRITE(*, *) '*=MMAX; ...=Subsequent specs'
      WRITE (*, '(2A)') 'MOL        C1        C2        C3        C4',
     1          '        C5 * ...'
      WRITE (*, 111)
111   FORMAT(79('='))
      DO J = 1, dissoc_NMOL
      WRITE(*,'(A3, 1X, E9.3, 4(1X, E9.3), 1X, I1, 4(1X, I2, 1X, I1))')
     1             dissoc_MOL(J),
     2             (dissoc_C(J, K), K=1,5),
     3             dissoc_MMAX(J),
     4             (dissoc_NELEM(M, J), dissoc_NATOM(M, J), M=1,4)
      END DO


      END

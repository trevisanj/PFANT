C     TEST THE READ_MOLECULAGRADE ROUTINE
      PROGRAM TEST_READ_MOLECULAGRADE
      USE MOLECULA
      IMPLICIT NONE

      CHARACTER*256 fn
      REAL*8 LZERO, LFIN

      fn = 'moleculagrade.dat'

      CALL READ_MOLECULAGRADE(fn)

      WRITE(*,*) '*****************************************************'
      WRITE(*, *) 'NUMBER = ', km__NUMBER
      WRITE(*, *) 'TITM = ', km__TITM

      CALL PRINT_1_MOLECULE(1)
      CALL PRINT_1_MOLECULE(15)

      LZERO = 1200
      LFIN = 1201

      CALL FILTER_MOLECULAGRADE(LZERO, LFIN)

      CALL PRINT_FILTERED()

      END





C-----------------------------------------------------------------------
C Writes original file variables (unfiltered)
C-----------------------------------------------------------------------
      SUBROUTINE PRINT_1_MOLECULE(I)
      USE MOLECULA
      IMPLICIT NONE
      INTEGER K, NNV, I


      WRITE(*, *) ''
      WRITE(*, *) '===== MOLECULE ', I, '====='
      WRITE(*, *) 'TITULO = ', km__TITULO(I)
      WRITE(*, *) '# NV = ', km__NV(I)
      WRITE(*, *) '! FE = ', km__FE(I)
      WRITE(*, *) '! DO = ', km__DO(I)
      WRITE(*, *) '! MM = ', km__MM(I)
      WRITE(*, *) '! AM = ', km__AM(I)
      WRITE(*, *) '! BM = ', km__BM(I)
      WRITE(*, *) '! UA = ', km__UA(I)
      WRITE(*, *) '! UB = ', km__UB(I)
      WRITE(*, *) '! TE = ', km__TE(I)
      WRITE(*, *) '! CRO = ', km__CRO(I)
      WRITE(*, *) '@ ISE = ', km__ISE(I)
      WRITE(*, *) '@ A0 = ', km__A0(I)
      WRITE(*, *) '@ A1 = ', km__A1(I)
      WRITE(*, *) '@ A2 = ', km__A2(I)
      WRITE(*, *) '@ A3 = ', km__A3(I)
      WRITE(*, *) '@ A4 = ', km__A4(I)
      WRITE(*, *) '@ ALS = ', km__ALS(I)
      WRITE(*, *) '$ S = ', km__S(I)

      NNV = km__NV(I)

      WRITE(*, *) '^ QQV(:, I) = ', (km__QQV(K, I), K=1,NNV)
      WRITE(*, *) '---'
      WRITE(*, *) '^ GGV(:, I) = ', (km__GGV(K, I), K=1,NNV)
      WRITE(*, *) '---'
      WRITE(*, *) '^ BBV(:, I) = ', (km__BBV(K, I), K=1,NNV)
      WRITE(*, *) '---'
      WRITE(*, *) '^ DDV(:, I) = ', (km__DDV(K, I), K=1,NNV)
      WRITE(*, *) '---'
      WRITE(*, *) '^ FACT(:, I) = ', (km__FACT(K, I), K=1,NNV)
*      READ(UNIT_,*) (km__GGV(I, I_MOL), I=1,NNV)
*      READ(UNIT_,*) (km__BBV(I, I_MOL), I=1,NNV)
*      READ(UNIT_,*) (km__DDV(I, I_MOL), I=1,NNV)
*      READ(UNIT_,*) (km__FACT(I, I_MOL),I=1,NNV)


*      WRITE(*,*) '>>> Press Enter >>>'
*      READ(*,*) X

      END

C-----------------------------------------------------------------------
C Writes filtered variables
C-----------------------------------------------------------------------
      SUBROUTINE PRINT_FILTERED()
      USE MOLECULA
      IMPLICIT NONE
      INTEGER I, J, K

      WRITE(*,*) ''
      WRITE(*,*) ''
      WRITE(*,*) ''
      WRITE(*,*) ''
      WRITE(*,*) ''
      WRITE(*,*) '************* FILTERED ******************************'
      WRITE(*,*) 'km_MBLEND = ', km_MBLEND
      WRITE(*, *) '^ km_MBLENQ = ', (km_MBLENQ(I), K=1,km__NUMBER)
      DO I = 1, km__NUMBER
        WRITE(*,*) '& km_LN(:,', I, ') = ',
     +   (km_LN(J, I), J=1, km__NV(I))
      END DO

      END




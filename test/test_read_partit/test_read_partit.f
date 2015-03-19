C     TEST THE READ_PARTIT ROUTINE

      PROGRAM TEST_READ_PARTIT
      USE READ_FILES

      CHARACTER*256 partit_KMAX_too_big,
     1               partit_NPAR_is_0,
     2               partit_NPAR_is_1,
     3               partit_original


      partit_KMAX_too_big = 'partit_KMAX_too_big.dat'
      partit_NPAR_is_0 = 'partit_NPAR_is_0.dat'
      partit_NPAR_is_1 = 'partit_NPAR_is_1.dat'
      partit_original = 'partit_original.dat'

      !-----------------------------------------------------------------------
      ! Original partit.dat by Marina/Beatriz; NPAR=81
      !
      ! Result:
      !-----------------------------------------------------------------------
      CALL TEST_FILE(partit_original)

      IF (partit_NPAR .NE. 81) THEN
        WRITE (*, *) 'NPAR = ', partit_NPAR, '(supposed to be', 1, ')'
        STOP -1
      END IF




      !-----------------------------------------------------------------------
      ! NPAR=0 (no data)
      !
      ! Result:
      !-----------------------------------------------------------------------
      CALL TEST_FILE(partit_NPAR_is_0)

      !-----------------------------------------------------------------------
      ! NPAR=1
      !
      ! Result:
      !-----------------------------------------------------------------------
      CALL TEST_FILE(partit_NPAR_is_1)

      IF (partit_NPAR .NE. 1) THEN
        WRITE (*, *) 'NPAR = ', partit_NPAR, '(supposed to be', 1, ')'
        STOP -1
      END IF


      !-----------------------------------------------------------------------
      ! KMAX in second "PAR" is 79
      !
      ! Result: "
      !  READ_PARTIT(): PAR number           2 KMAX=          79  exceeded maximum of
      !        63
      ! STOP 111
      ! "
      !-----------------------------------------------------------------------
* Gave error as expected
*      CALL TEST_FILE(partit_KMAX_too_big)

      END



C-----------------------------------------------------------------------
C Reads and writes original file variables (unfiltered)
C-----------------------------------------------------------------------
      SUBROUTINE TEST_FILE(fn)
      USE READ_FILES

      CHARACTER*256 fn
      WRITE (*, *) '***'
      WRITE (*, *) '*** File: ', fn

      CALL READ_PARTIT(fn)


      WRITE(*, *) '-----------------------------------'
      WRITE(*, *) 'partit_NPAR = ', partit_NPAR
      WRITE(*, *) '-----------------------------------'


      WRITE(*, *) ''
      WRITE(*, *) '*=JKMAX(J)'


      WRITE(*, *) 'El TINI   PA  *         M       KI1',
     1             '       KI2    ?'

      WRITE(*, *) '===================================',
     1             '==============='


      DO J = 1, partit_NPAR
          WRITE(*, '(A2, 2F5.2, I3, 3F10.2)')
     1         partit_EL(J),
     2         partit_TINI(J),
     3         partit_PA(J),
     4         partit_JKMAX(J),
     5         partit_M(J),
     6         partit_KI1(J),
     7         partit_KI2(J)
      END DO

      WRITE(*, *) ''
      WRITE(*, *) ''
      WRITE(*, *) ''
      WRITE(*, *) ''
      WRITE(*, *) ''
      WRITE(*, *) ''
      WRITE(*, *) ''
      WRITE(*, *) ''
      WRITE(*, *) ''


      END

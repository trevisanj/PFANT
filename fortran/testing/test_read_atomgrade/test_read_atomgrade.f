C     TEST THE READ_ATOMGRADE ROUTINE
      PROGRAM TEST_READ_ATOMGRADE
      USE READ_FILES



      CHARACTER*256 atomgrade_empty, atomgrade_one_line,
     1              atomgrade_too_big, atomgrade_data_too_big,
     2              atomgrade_no_eof_marker, atomgrade_pass

      REAL*8 LZERO, LFIN

      atomgrade_empty = 'atomgrade_empty.dat'
      atomgrade_one_line = 'atomgrade_one_line.dat'
      atomgrade_too_big = 'atomgrade_too_big.dat'
      atomgrade_data_too_big = 'atomgrade_data_too_big.dat'
      atomgrade_no_eof_marker = 'atomgrade_no_eof_marker.dat'
      atomgrade_pass = 'atomgrade_pass.dat'


      !-----------------------------------------------------------------------
      ! EMPTY FILE
      !
      ! Result: "Fortran runtime error: End of file"
      !-----------------------------------------------------------------------
* (gave error as supposed to)
*      CALL TEST_ORIGINAL(atomgrade_empty)


      !-----------------------------------------------------------------------
      ! ONLY ONE SPECTRAL LINE
      !-----------------------------------------------------------------------
      CALL TEST_ORIGINAL(atomgrade_one_line)


      !-----------------------------------------------------------------------
      ! NO END-OF-FILE MARKER
      !
      ! Result: "Fortran runtime error: End of file"
      !-----------------------------------------------------------------------
* (gave error as supposed to)
*      CALL TEST_ORIGINAL(atomgrade_no_eof_marker)


      !-----------------------------------------------------------------------
      ! FILE TOO BIG
      ! Result: "
      !  READ_ATOMGRADE(): blew maximum of       13000  spectral lines
      ! STOP 111
      ! Process returned 111 (0x6F)   execution time : 0.214 s
      ! "
      !-----------------------------------------------------------------------
* (gave error as supposed to)
*      CALL TEST_ORIGINAL(atomgrade_too_big)


      !-----------------------------------------------------------------------
      ! FILTERED DATA TOO BIG: file has repeated lambda=6300.311
      ! Result: "
      !  FILTER_ATOMGRADE(): exceeded maximum of        8000  spectral lines
      ! STOP 111
      !
      ! Process returned 111 (0x6F)   execution time : 0.139 s
      ! "
      !-----------------------------------------------------------------------
      CALL READ_FILE(atomgrade_data_too_big)
      LZERO = 6200
      LFIN = 6400
*(gave error as supposed to)
*      CALL TEST_FILTER(LZERO, LFIN)


      !-----------------------------------------------------------------------
      ! File that passes (but invalid!!! repeats lines,
      !                   but not supposed to give error, anyway)
      ! Has 18 lines total, 12 filtered using 6200-6400 lambda range.
      !-----------------------------------------------------------------------
      CALL TEST_ORIGINAL(atomgrade_pass)
      LZERO = 6200
      LFIN = 6400
      CALL TEST_FILTER(LZERO, LFIN)



      END



C-----------------------------------------------------------------------
C Reads and writes original file variables (unfiltered)
C-----------------------------------------------------------------------
      SUBROUTINE TEST_ORIGINAL(fn)
      USE READ_FILES
      CHARACTER*256 fn

      CALL READ_FILE(fn)

      CALL WRITE_ORIGINAL()

      END


C-----------------------------------------------------------------------
C Reads file, but not before printing the file name on screen
C-----------------------------------------------------------------------
      SUBROUTINE READ_FILE(fn)
      USE READ_FILES
      CHARACTER*256 fn
      WRITE (*, *) '***'
      WRITE (*, *) '*** File: ', fn

      CALL READ_ATOMGRADE(fn)
      END


C-----------------------------------------------------------------------
C Writes original file variables (unfiltered)
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_ORIGINAL()
      USE READ_FILES

      WRITE(*, *) '-----------------------------------'
      WRITE(*, *) 'Number of lines (atomgrade__NBLEND)'
      WRITE(*, *) atomgrade__NBLEND
      WRITE(*, *) '-----------------------------------'


      WRITE(*, *) 'E=Element'
      WRITE(*, *) '1=IONI(K)'
      WRITE(*, *) 'A=ABONDR(K)'
      WRITE(*, *) ''


      WRITE(*, *) 'E 1     Lambda'
      WRITE(*, *) '=============='
      WRITE(*, *) ''
      WRITE(*, '(2A)') '    KIEX    ALGF         CH         GR',
     1                 '         GE   ZINF   A'
      WRITE (*, 111)
111   FORMAT(79('='))

      DO K = 1, atomgrade__NBLEND
          WRITE(*, '(1X, A2, I1, 1X, F10.3)') atomgrade__ELEM(K),
     1                                    atomgrade__IONI(K),
     2                                    atomgrade__LAMBDA(K)

          WRITE(*, '(2F8.3, 3E11.3, F7.1, F4.1)')
     1                atomgrade__KIEX(K),
     2                atomgrade__ALGF(K),
     3                atomgrade__CH(K),
     4                atomgrade__GR(K),
     5                atomgrade__GE(K),
     6                atomgrade__ZINF(K),
     7                atomgrade__ABONDR(K)
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

C-----------------------------------------------------------------------
C Filters and writes filtered
C-----------------------------------------------------------------------
      SUBROUTINE TEST_FILTER(LZERO, LFIN)
      USE READ_FILES
      REAL*8 LZERO, LFIN

      CALL FILTER_ATOMGRADE(LZERO, LFIN)

      CALL WRITE_FILTERED()

      END


C-----------------------------------------------------------------------
C Writes filtered variables
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_FILTERED()
      USE READ_FILES

      WRITE(*, *) '-----------------------------------'
      WRITE(*, *) 'Number of lines (atomgrade_NBLEND)'
      WRITE(*, *) atomgrade_NBLEND
      WRITE(*, *) '-----------------------------------'


      WRITE(*, *) 'E=Element'
      WRITE(*, *) '1=IONI(K)'
      WRITE(*, *) 'A=ABONDR(K)'
      WRITE(*, *) ''


      WRITE(*, *) 'E 1     Lambda'
      WRITE(*, *) '=============='
      WRITE(*, *) ''
      WRITE(*, '(2A)') '    KIEX    ALGF         CH         GR',
     1                 '         GE   ZINF   A'
      WRITE (*, 111)
111   FORMAT(79('='))



      DO K = 1, atomgrade_NBLEND
          WRITE(*, '(1X, A2, I1, 1X, F10.3)') atomgrade_ELEM(K),
     1                                    atomgrade_IONI(K),
     2                                    atomgrade_LAMBDA(K)

          WRITE(*, '(2F8.3, 3E11.3, F7.1, F4.1)')
     1                atomgrade_KIEX(K),
     2                atomgrade_ALGF(K),
     3                atomgrade_CH(K),
     4                atomgrade_GR(K),
     5                atomgrade_GE(K),
     6                atomgrade_ZINF(K),
     7                atomgrade_ABONDR(K), FINRAI
      END DO

      END

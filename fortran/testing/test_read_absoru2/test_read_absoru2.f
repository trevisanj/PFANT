C     TEST THE READ_ABSORU2 ROUTINE


C ISSUE: NUMSET(2) is ZERO!!!! See below:
*=== NUMSET(1) = 40
*     100.0     227.8     504.3     911.8    2050.6    2601.0    3122.0    3422.0
*
*    3645.6    3647.0    3680.0    5696.2    7932.0    8202.5    8205.9   11164.5
*
*   14380.0   14582.3   14588.2   18455.7   22535.0   22784.8   22794.1   27569.6
*
*   32513.0   32810.1   32823.5   38506.3   44313.0   44658.2   44676.4   51265.8
*
*   57936.0   58329.0   58352.0   65845.1   73383.0   73822.6   82253.0   90603.0
*
*=== NUMSET(2) =  0




      PROGRAM TEST_READ_ABSORU2
      USE READ_FILES

      CHARACTER*256 absoru2_original

      absoru2_original = 'absoru2_original.dat'

      !-----------------------------------------------------------------------
      ! Original absoru2.dat
      !
      ! Result:
      !-----------------------------------------------------------------------
      CALL TEST_FILE(absoru2_original)
      END



C-----------------------------------------------------------------------
C Reads and writes original file variables (unfiltered)
C-----------------------------------------------------------------------
      SUBROUTINE TEST_FILE(fn)
      USE READ_FILES

      CHARACTER*256 fn
      WRITE (*, *) '***'
      WRITE (*, *) '*** File: ', fn

      CALL READ_ABSORU2(fn)


      WRITE (*, *) 'NM = ', absoru2_NM
      WRITE (*, *) 'NMETA = ', absoru2_NMETA



      DO J = 1, absoru2_NM
100     FORMAT('J=', I2, ' -- NR=', I3, '; ZP=', E16.5, '; ZM=', E16.5)
        WRITE(*, 100) J, absoru2_NR(J), absoru2_ZP(J), absoru2_ZM(J)

        NRR = absoru2_NR(J)

        DO I = 1, NRR
200       FORMAT('  I=', I2, ' -- XI=', E16.5, 'PF=', E16.5)
          WRITE(*, 200) I, absoru2_XI(J,I), absoru2_PF(J,I)
        END DO
      END DO


      DO ITH = 1,2
        NSET = absoru2_NUMSET(ITH)
300     FORMAT('=== NUMSET(', I1, ') = ', I2)
        WRITE(*, 300), ITH, NSET
        WRITE (*, '(8F10.1)') (absoru2_WI(I,ITH),I=1,NSET)
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





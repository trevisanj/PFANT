! This file is part of PFANT.
! 
! PFANT is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! Foobar is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with PFANT.  If not, see <http://www.gnu.org/licenses/>.

C TODO Most variables still cannot be found because they are local to the main module

      MODULE WRITE_FILES

      CONTAINS



C================================================================================================================================
C WRITE_LINES_PFANT(): Creates file "lines.pfant"

      SUBROUTINE WRITE_LINES_PFANT(filename)
      USE READ_FILES
      INTEGER UNIT_
      PARAMETER(UNIT_=199)
      CHARACTER*256 fileName

122   FORMAT(6X,'# LAMBDA',4X,'KIEX',5X,'L GF',3X,'L ABOND',6X,'CH',10X,
     1  'GR',10X,'GE',5X,'ZINF',4X,'CORCH')

125   FORMAT(A2,1X, I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1,
     1 F7.1)

      OPEN(UNIT=UNIT_, FILE=filename, STATUS='UNKNOWN')
      WRITE(UNIT_, 122)
      IF (atomgrade_NBLEND .NE. 0) then
        DO K=1,atomgrade_NBLEND
          WRITE(UNIT_,125)
     +          atomgrade_ELEM(K),
     +          atomgrade_IONI(K),
     +          atomgrade_LAMBDA(K),
     +          atomgrade_KIEX(K),
     +          atomgrade_ALGF(K),
     +          ALOG10(ABOND(K))-main_AFSTAR+12,
     +          atomgrade_CH(K),
     +          atomgrade_GR(K),
     +          atomgrade_GE(K),
     +          atomgrade_ZINF(K),
     +          CORCH(K)

          ! ISSUE: Is file "fort.91" still wanted????
*          WRITE(91,121) atomgrade_ELEM(K),atomgrade_IONI(K),atomgrade_LAMBDA(K),atomgrade_KIEX(K),atomgrade_ALGF(K),
*     1      alog10(ABOND(K))-main_AFSTAR+12,atomgrade_CH(K),atomgrade_GR(K),atomgrade_GE(K),atomgrade_ZINF(K),CORCH(K)
        END DO
      END IF

      CLOSE(UNIT_)
      END





C================================================================================================================================
C WRITE_LOG_LOG(): Creates file "log.log"

      SUBROUTINE WRITE_LOG_LOG(filename)
      USE READ_FILES
      INTEGER UNIT_
      PARAMETER(UNIT_=199)
      CHARACTER*256 fileName

1130  FORMAT(I5, 5A4, 5F15.5, 4F10.1, I10, 4F15.5)

      OPEN(UNIT=UNIT_, FILE=filename, STATUS='UNKNOWN')
      PRINT  *, DTOT, ITOT, I1, I2
      WRITE(UNIT_, 1130) I,
     +                    KEYtot,
     +                    (TIT(I),I=1,5),
     +                    TETAEF,
     +                    main_GLOG,
     +                    main_ASALOG,
     +                    NHE,
     +                    AMG,
     +                    L0,
     +                    LF,
     +                    LZERO,
     +                    LFIN,
     +                    ITOT,
     +                    DPAS,
     +                    main_ECHX,
     +                    main_ECHY,
     +                    main_FWHM

      DO D = I1,I2
        WRITE(UNIT_, *) L0+(D-1)*DPAS, FL(D)
      END DO

      CLOSE(UNIT_)
      END




      END MODULE WRITE_FILES

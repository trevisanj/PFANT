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

C TODO I will probably move most of these declarations to the main
C calculation file

      MODULE COMMONS
      USE READ_FILES


C     COMMON definitions
C
C     Rules for working with COMMON blocks:
C
C       1) Include all COMMON definitions here, even if used only in
C          subroutines!
C
C       2) Keep consistent!!!
C          Don't change variable names in subroutines!!!
C
C       3) Keep clean! Only one block definition per statement!




C main, ABSORU, IONIPE, BK
      COMMON /SAPE/   AVM, ZNU1, ZNU2, ZNU3, ZMUZE, ZNU(30)
C     Variant: COMMON /SAPE/  BIDB(5), ZNU(30)


C main, ABSORU, IONIPE, BK
      COMMON /SAPU/   PE, RHO, TOC, ZNH(12)
C     Variant: COMMON /SAPU/  BIDC(3), ZNH(12)




C     First appeared in subroutine LECTUR
c LECTUR, ABSORU, IONIPE, SAHATH
      COMMON /SAHT/  ZK(11), ZKM(30,9), NR_SAHT(30)

C First appeared in subroutine ABSORU
C ABSORU, GAUNTH, block data
      COMMON /GBF/   ZLH(19)
C ABSORU, ATHYHE, GAUNTH
      COMMON /GBFH/  G2D(2,19), JSHYD(2), JH
C ABSORU, TEMPA, ATHYHE
      COMMON /TETA/  AHE, AH, AHEP, UH1, ZEMH, UHEP1, UHE1, ZEUHE1
C ABSORU, TEMPA, ATHYHE, block data
      COMMON /TEHE/  ZLHEM(5), ZLHE(10)
C ABSORU, TEMPA, block data
      COMMON /TEMPE/ STWTM(5)
C ABSORU, TEMPA, ATHYHE
      COMMON /THE/   ZEUH(20), ZEUHEP(20), ZEXPM(5), ZEXP(10), UL
C ABSORU, ATHYHE, GAUNTH
      COMMON /ABSO3/ JFZ
C ABSORU, ATHYHE, block data
      COMMON /HYHE/  GRDM(46), V1(46), U2(46), WINV(46), YY(4),
     1               ZLETAG(18), G3D(12,18), AA(4), ZEFF4(10),
     2               RHOG(12), ZLHEP(19)
C ABSORU, IONIPE
      COMMON /ZION/  AC, AC1(3), AC2(30,9)
C ABSORU, ATHYHE
      COMMON /ABME/  STIMU
C ABSORU
      COMMON /SOMAB/  SUM1





      SAVE
      END MODULE COMMONS

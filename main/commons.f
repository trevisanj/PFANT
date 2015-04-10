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


C     COMMON ENTRE LE PROGRAMME PRINCIPAL ET LE SP FLIN1
C main, BK, SELEKFH, FLINH, FLIN1
      COMMON /TOTO/ TO_TOTO
      COMMON /FAIL/ ERR(50)
C main, BK, SELEKFH, FLINH, FLIN1
      DIMENSION TO_TOTO(0:50)


C main, SELEKFH
      COMMON /KAPM4/ ECARTM

C     COMMON'S AVEC LE SP KAPMOL ET POPADELH
c main, KAPMOL, POPADELH
      COMMON /KAPM3/ PPH, PPC2, PN, PC13, PMG, PO, PTI, PNG, PIG, PFE
c main, BK, LECTAUH, SELEKFH
      COMMON /PRT/   ECRIT
      LOGICAL ECRIT



C     COMMON AVEC SELEKFH ET POPADELH
c main, POPADELH, SELEKFH
      COMMON /CNO/ DM



C     First appeared in subroutine LECTUR
c LECTUR, ABSORU, IONIPE, SAHATH
      COMMON /SAHT/  ZK(11), ZKM(30,9), NR_SAHT(30)
c LECTUR, ABSORU, SAHATH
      COMMON /SAHTP/ XI(30,9), PF(30,9)
c LECTUR, ABSORU
      COMMON /UNI/   IUNITE(2)
c LECTUR, ABSORU
      COMMON /NOM/   NOMET(30)
c LECTUR, ABSORU
      COMMON /TIT/   TITRE(17)



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

C Not used!!!!, appears only in ABSORU's block data
C I think I can get rid of all these variables
      COMMON /D1/ TET(7), ALP(9), TAB(9,7), MMAX_D1, NMAX


C     First appeared in subroutine FLINH
C FLINH, FLIN1
      COMMON /FCO/  FP_FCO(13),CC(13),TT(13),BB(13)
C FLINH, FLIN1
      COMMON /CCC/  AMF(50), AMF2(50), FX1(50), FX2(50)



C     First appeared in subroutine SAT4
C SAT4, DIE
      COMMON /COMFH1/ C(MAX_dissoc_NMOL,5), NELEM(5,MAX_dissoc_NMOL), NATOM(5,MAX_dissoc_NMOL), MMAX(MAX_dissoc_NMOL),
     1                PPMOL(MAX_dissoc_NMOL), APMLOG(MAX_dissoc_NMOL),MOL(MAX_dissoc_NMOL), IP(100),
     2                CCOMP(100), UIIDUI(100), P(100), FP(100), KP(100),
     3                NELEMX(50), NIMAX, EPS, SWITER
C SAT4 only
      COMMON /VAL/    PPG(MAX_dissoc_NMOL,50)


      SAVE
      END MODULE COMMONS

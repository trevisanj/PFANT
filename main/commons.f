C TODO I will probably move most of these declarations to the main
C calculation file

      MODULE COMMONS


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



C     COMMON AVEC LE SP READER ET LE SP DE TRACE
C (main), READER06
      COMMON /COM6/ TETAEF, GLOG, ASASOL, NHE, TIT(5)
      REAL*8 NHE

C main, READER06
      COMMON /ALOG/ASALOG


C     COMMON AVEC LE SP D ABSORPTION CONTINUE
C main, ABSORU, BK, LECTUR
      COMMON /LECT1/ AMET, BHE
C main, ABSORU, BK, LECTUR
      COMMON /LECT2/ ZP(30), ZM(30), WI(41,2), NUMSET(2), CAL
C     History
C     =========
C     I found occurrences of the LECT2 block being defined as
C     The variable "BIDA"is never used, so I guess it just reserves
C     the allocation space for WI+NUMSET+C whose space indeed sum up
C     to 85
C     COMMON /LECT2/ZP(30),ZM(30),BIDA(85)



C main, LECTUR, ABSORU, IONIPE, SAHATH, BK
      COMMON /ABSO1/ NM
c main, LECTUR, ABSORU, IONIPE, SAHATH, ATHYHE, BK
      COMMON /ABSO2/ NMETA

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


C     COMMON AVEC LA SUBROUTINE D EQUILIBRE DISSOCIATIF SAT4
C main, SAT4, DIE
      COMMON /COM8/  NNH(50), TETA(50), PPE(50), PGG(50), T5L(50)
C     Variant: COMMON /COM8/   NH, TETA, PE, PG, T5L
C main, SAT4
      COMMON /COR/ ELEMS, XXCOR, NNMETAL
      DIMENSION XXCOR(50)
C main, SAT4
      COMMON /FANSAT/ FSTAR

C     COMMON'S AVEC LE SP KAPMOL
C main, SELEKFH, KAPMOL
      COMMON /KAPM1/ MM, MBLEND
C main, SELEKFH, KAPMOL
      COMMON /KAPM2/ LMBDAM, GFM, PNVJ, ALARGM
C main, SELEKFH
      COMMON /KAPM4/ ECARTM
C main, KAPMOL
      COMMON /OPTIM/ LZERO,LFIN
C main, KAPMOL
      COMMON /TOTAL/ MMC,MBLENQ

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
      COMMON /SAHT/  ZK(11), ZKM(30,9), NR(30)
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
      COMMON /COMFH1/ C(600,5), NELEM(5,600), NATOM(5,600), MMAX(600),
     1                PPMOL(600), APMLOG(600),MOL(600), IP(100),
     2                CCOMP(100), UIIDUI(100), P(100), FP(100), KP(100),
     3                NELEMX(50), NIMAX, EPS, SWITER, NMETAL, NMOL
C SAT4 only
      COMMON /VAL/    PPG(600,50)



C      Variables read by subroutine READ_MAIN and not present in other blocks
      COMMON /IO_MAIN/ TITRAV, PAS, ECHX, ECHY, FWHM, VVT(20),
     1                 TEFF, IVTOT, PTDISK, MU, AFSTAR,
     2                 FILEFLUX, LLZERO, LLFIN, AINT, FILETOHY
      DIMENSION TITRAV(20)
      CHARACTER TITRAV*10
      CHARACTER FILEFLUX*64
      LOGICAL   PTDISK
      REAL*8    PAS, ECHX, ECHY, FWHM, TEFF, MU, AFSTAR,
     1          LLZERO, LLFIN, AINT
      INTEGER   IVTOT
      CHARACTER FILETOHY*64
      DIMENSION FILETOHY(10)




C Variables filled by READ_DISSOC()
C Number of elements actually used is specified by variable NMETAL <= maxNMETAL

C ISSUE: I created new variables to decouple from logic while reading this file.
C        The original mixes reading and logic, and I don't want to track down the
C        variables that are read right now. It seems, BTW, that many of them aren't used.
C ISSUE: this may be temporary, or to test not using COMMON, or remain just like this


C RESULT: common blocks are really not needed!!!

      PARAMETER(maxNMETAL=50)  ! Limit number of element lines in dissoc.dat
      INTEGER NMETAL
      CHARACTER*2 DISSOC_SYMBOL
      INTEGER DISSOC_N, DISSOC_IG0, DISSOC_IG1
      REAL*8 DISSOC_IP, DISSOC_CCLOG
      DIMENSION DISSOC_SYMBOL(maxNMETAL), DISSOC_N(maxNMETAL),
     1          DISSOC_IP(maxNMETAL), DISSOC_IG0(maxNMETAL),
     2          DISSOC_IG1(maxNMETAL), DISSOC_CCLOG(maxNMETAL)


C Variables filled by READ_ABONDS
      PARAMETER(maxNABOND=100)  ! Limit number of abundances in abonds.dat
      INTEGER abonds_NABOND
      CHARACTER*2 abonds_ELE
      DIMENSION abonds_ELE(maxNABOND), abonds_ABOL(maxNABOND)


      SAVE
      END MODULE COMMONS

      MODULE COMMONS
C 	COMMON definitions
C
C     Rules for working with COMMON blocks:
C
C       1) Include all COMMON definitions here, even if used only in
C          subroutines!
C
C       2) Keep consistent!!!
C          Don't change variable names in subroutines!!!
C
C	  3) Keep clean! Only one block definition per statement!



C	COMMON AVEC LE SP READER ET LE SP DE TRACE
	COMMON /COM6/ TETAEF,GLOG, ASASOL, NHE, TIT(5)
	COMMON /ALOG/ASALOG


C	COMMON AVEC LE SP D ABSORPTION CONTINUE
	COMMON /LECT1/ AMET, BHE
	COMMON /LECT2/ ZP(30), ZM(30), WI(41,2), NUMSET(2), CAL
C	History
C	=========
C	I found occurrences of the LECT2 block being defined as
C	The variable "BIDA"is never used, so I guess it just reserves
C	the allocation space for WI+NUMSET+C whose space indeed sum up
C     to 85
C	COMMON /LECT2/ZP(30),ZM(30),BIDA(85)





      COMMON /ABSO1/ NM
	COMMON /ABSO2/ NMETA


	COMMON /SAPE/   AVM, ZNU1, ZNU2, ZNU3, ZMUZE, ZNU(30)
C     Variant: COMMON /SAPE/  BIDB(5), ZNU(30)



	COMMON /SAPU/   PE, RHO, TOC, ZNH(12)
C	Variant: COMMON /SAPU/  BIDC(3), ZNH(12)


C	COMMON ENTRE LE PROGRAMME PRINCIPAL ET LE SP FLIN1
	COMMON /TOTO/ TO
	COMMON /FAIL/ ERR(50)

C	COMMON AVEC LA SUBROUTINE D EQUILIBRE DISSOCIATIF SAT4

      COMMON /COM8/  NNH(50), TETA(50), PPE(50), PGG(50), T5L(50)
C	Variant: COMMON /COM8/   NH, TETA, PE, PG, T5L

      COMMON /COR/    ELEMS, XXCOR, NNMETAL
	COMMON /FANSAT/ FSTAR

C	COMMON'S AVEC LE SP KAPMOL
      COMMON /KAPM1/ MM, MBLEND
      COMMON /KAPM2/ LMBDAM, GFM, PNVJ, ALARGM
	COMMON /KAPM4/ ECARTM
      COMMON /OPTIM/ LZERO,LFIN
	COMMON /TOTAL/ MMC,MBLENQ

C     COMMON'S AVEC LE SP KAPMOL ET POPADELH
	COMMON /KAPM3/ PPH, PPC2, PN, PC13, PMG, PO, PTI, PNG, PIG, PFE
	COMMON /PRT/   ECRIT
	LOGICAL ECRIT



C	COMMON AVEC SELEKFH ET POPADELH
	COMMON /CNO/ DM



C	First appeared in subroutine LECTUR
      COMMON /SAHT/  ZK(11), ZKM(30,9), NR(30)
      COMMON /SAHTP/ XI(30,9), PF(30,9)
      COMMON /UNI/   IUNITE(2)
      COMMON /NOM/   NOMET(30)
      COMMON /TIT/   TITRE(17)



C	`First appeared in subroutine ABSORU
      COMMON /GBF/   ZLH(19)
      COMMON /GBFH/  G2D(2,19), JSHYD(2), JH
      COMMON /TETA/  AHE, AH, AHEP, UH1, ZEMH, UHEP1, UHE1, ZEUHE1
      COMMON /TEHE/  ZLHEM(5), ZLHE(10)
      COMMON /TEMPE/ STWTM(5)
      COMMON /THE/   ZEUH(20), ZEUHEP(20), ZEXPM(5), ZEXP(10), UL
      COMMON /ABSO3/ JFZ


C	ISSUE!!!! RHOG APPERS
      COMMON /HYHE/  GRDM(46), V1(46), U2(46), WINV(46), YY(4),
     1               ZLETAG(18), G3D(12,18), AA(4), ZEFF4(10),
     2               RHOG(12), ZLHEP(19)
	COMMON /ZION/  AC, AC1(3), AC2(30,9)
	COMMON /ABME/  STIMU
	COMMON /SOMAB/  SUM1

C	First appeared in subroutine SAHATH
	COMMON /D1/ TET(7), ALP(9), TAB(9,7), MMAX_D1, NMAX


C	First appeared in subroutine FLINH
	COMMON /FCO/  FP_FCO(13),CC(13),TT(13),BB(13)
	COMMON /CCC/  AMF(50), AMF2(50), FX1(50), FX2(50)


C	First appeared in subroutine SAT4
      COMMON /COMFH1/ C(600,5), NELEM(5,600), NATOM(5,600), MMAX(600),
     1                PPMOL(600), APMLOG(600),MOL(600), IP(100),
     2                CCOMP(100), UIIDUI(100), P(100), FP(100), KP(100),
     3                NELEMX(50), NIMAX, EPS, SWITER, NMETAL, NMOL
      COMMON /VAL/    PPG(600,50)



C      Variables read by subroutine READ_MAIN and not present in other blocks
      COMMON /IO_MAIN/ TITRAV, PAS, ECHX, ECHY, FWHM, VVT(20),
     1                 TEFF, IVTOT, PTDISK, MU, AFSTAR,
     2                 FILEFLUX, LLZERO, LLFIN, AINT, FILETOHY
      DIMENSION TITRAV(20)
      CHARACTER FILEFLUX*64
      LOGICAL   PTDISK
      REAL*8    PAS, ECHX, ECHY, FWHM, TEFF, MU, AFSTAR,
     1          LLZERO, LLFIN, AINT
      INTEGER   IVTOT
      DIMENSION FILETOHY(10)


      SAVE

      END MODULE COMMONS

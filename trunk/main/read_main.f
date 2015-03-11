
C  Variables "returned" (in reading order)
C /IO_MAIN/ TITRAV
C /PRT/     ECRIT
C /IO_MAIN/ PAS
C /IO_MAIN/ ECHX
C /IO_MAIN/ ECHY
C /IO_MAIN/ FWHM
C /IO_MAIN/ VVT
C /IO_MAIN/ IVTOT
C /IO_MAIN/ TEFF
C /COM6/    GLOG
C /ALOG/    ASALOG
C /IO_MAIN/ PTDISK
C /IO_MAIN/ MU
C /IO_MAIN/ AFSTAR
C /COR/     XXCOR
C /IO_MAIN/ FILEFLUX
C /IO_MAIN/ LLZERO
C /IO_MAIN/ LLFIN
C /IO_MAIN/ AINT
C /IO_MAIN/ FILETOHY

C IMPORTANT: You must set global NMETAL first


      SUBROUTINE READ_MAIN(fileName)
      USE COMMONS
      INTEGER UNIT_
      PARAMETER(UNIT_=4)
      CHARACTER*256 fileName

      OPEN(UNIT=UNIT_,FILE=fileName, STATUS='OLD')

C line 01: object name, e.g. "sun"
C =======
*      PRINT *,' ENTRER UN TITRE'
      READ(UNIT_, '(20A)') TITRAV
*      WRITE(6,'(1H1, 20X, 20A4)') TITRAV

C line 02: ???
C =======
C Example:  T      0.02 5.0   1.    .12
      READ(UNIT_, *) ECRIT, PAS, ECHX, ECHY, FWHM

C line 03:
C =======
C Example: 0.9

      READ(UNIT_, *) VVT(1)
      IVTOT=1
C ISSUE: it seems that here there are three conditional lines;
C ISSUE: these are present/read only if the value in the third line
C ISSUE: is greater than 900
C ISSUE: UPDATE: I checked pfantgrade.f and only VVT(1) is used
C ISSUE:         throughout the program, so I think I can safely
C ISSUE;         exclude these
C      IF(VVT(1) .GT. 900)  THEN   ! VT VARIABLE AVEC LA PROFONDEUR
C            READ(4,*) IVTOT
C            READ(4,*) (TOLV(I), I=1, IVTOT)
C            READ(4,*) (VVT(I) ,I=1, IVTOT)
C      END IF


C line 04
C =======
C Example:      5777  4.44  0       0.1  1
      READ(UNIT_, *) TEFF, GLOG, ASALOG, NHE, INUM
*      print *, TEFF, GLOG, ASALOG, NHE, INUM


C line 05
C =======
C Example:      F       1.
      READ(UNIT_, *) PTDISK, MU

C line 06
C =======
C Example: 0
      READ(UNIT_, *) AFSTAR  ! metallicity of the star (in log scale)



C line 07 -- XXCOR(I)
C =======
C ISSUE: Should be a column in dissoc.dat !!!!!
C ISSUE: Is this actually used?
      READ(UNIT_, *)(XXCOR(I), I=1, NMETAL)



C line 08 -- part of filename
C =======
C This line will define the names of other three files to be read:
C   cont.<FILEFLUX>
C   norm.<FILEFLUX>
C   spec.<FILEFLUX>
      READ(UNIT_, '(A)') FILEFLUX



C line 09 --
C =======
C Example:      4800    4820   50
      READ(UNIT_, *) LLZERO, LLFIN, AINT
*711   FORMAT(/,2X,'LLZERO=',F10.5,2X,'LLFIN=',F10.5,2X,'AINT=',F8.3,/)
*      write(6,711) LLZERO,LLFIN,AINT


C lines 10-19 -- file names, in sync with variable LLHY
C ===========
C Example: thkappa
C          thiota
C          ththeta
C          theta
C          thzeta
C          thepsilon
C          thdelta
C          thgamma
C          thbeta
C          thalpha

      DO IH = 1, 10
        READ(UNIT_, '(A)') FILETOHY(IH)
*        print *,IH,filetohy(IH)
      END DO

      CLOSE(UNIT=UNIT_)

      RETURN
      END

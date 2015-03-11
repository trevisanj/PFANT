C IO routines


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

      SUBROUTINE READ_MAIN(fileName)
      USE COMMONS
      CHARACTER*64 fileName


*      COMMON /COM6/ TETAEF, GLOG, ASASOL, NHE, TIT(5)
*
*      COMMON /ALOG/ ASALOG
*
*      COMMON /PRT/   ECRIT
*      LOGICAL ECRIT


      OPEN(UNIT=4,FILE=fileName, STATUS='OLD')

C line 01: object name, e.g. "sun"
C =======
*      PRINT *,' ENTRER UN TITRE'
      READ(4,'(20A4)') TITRAV
*      WRITE(6,'(1H1, 20X, 20A4)') TITRAV

C line 02: ???
C =======
C Example:  T      0.02 5.0   1.    .12
      READ(4,*) ECRIT, PAS, ECHX, ECHY, FWHM


C line 03:
C =======
C Example: 0.9

      READ(4,*) VVT(1)
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
      READ(4,*) TEFF, GLOG, ASALOG, NHE, INUM
*      print *, TEFF, GLOG, ASALOG, NHE, INUM


C line 05
C =======
C Example:      F       1.
      READ(4,*) PTDISK, MU

C line 06
C =======
C Example: 0
      READ(4,*) AFSTAR  ! metallicity of the star (in log scale)



C line 07 -- XXCOR(I)
C =======
      READ(4,*)(XXCOR(I), I=1, 10)



C line 08 -- part of filename
C =======
C This line will define the names of other three files to be read:
C   cont.<FILEFLUX>
C   norm.<FILEFLUX>
C   spec.<FILEFLUX>
      READ(4, '(A)') FILEFLUX



C line 09 --
C =======
C Example:      4800    4820   50
      READ(4,*) LLZERO, LLFIN, AINT
*711   FORMAT(/,2X,'LLZERO=',F10.5,2X,'LLFIN=',F10.5,2X,'AINT=',F8.3,/)
*      write(6,711) LLZERO,LLFIN,AINT


C      lines 10-19 -- file names in sync with variable LLHY
C      ===========
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
        READ(4, '(A)') FILETOHY(IH)
*        print *,IH,filetohy(IH)
      END DO

      RETURN
      END

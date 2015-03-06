C	IO routines

	SUBROUNTINE READ_MAIN(fileName)
	CHARACTER*64 fileName

	DIMENSION VT(50),TOLV(20),VVT(20)

	LOGICAL PTDISK, ECRIT,IDENTH,calfc


	OPEN(UNIT=4,FILE=fileName, STATUS='OLD')


C	line 01: object name, e.g. "sun"
C	=======
	PRINT *,' ENTRER UN TITRE'
	READ(4,'20A4') TITRAV
	WRITE(6,'(1H1, 20X, 20A4)') TITRAV

C	line 02: ???
C	=======
C	Example:  T      0.02 5.0   1.    .12
	READ(4,*) ECRIT, PAS, ECHX, ECHY, FWHM


C	line 03:
C	=======
C	Example: 0.9

	READ(4,*) VVT(1)
	IVTOT=1
C	ISSUE: it seems that here there are three conditional lines;
C	ISSUE: these are present/read only if the value in the third line
C	ISSUE: is greater than 900
C	ISSUE: UPDATE: I checked pfantgrade.f and only VVT(1) is used
C	ISSUE:         throughout the program, so I think I can safely
C	ISSUE;         exclude these
	IF(VVT(1) .GT. 900)  THEN   ! VT VARIABLE AVEC LA PROFONDEUR
		READ(4,*) IVTOT
		READ(4,*) (TOLV(I), I=1, IVTOT)
		READ(4,*) (VVT(I) ,I=1, IVTOT)
	END IF


C	line 04
C	=======
	READ(4,*) TEFF,GLOG,ASALOG,NHE,INUM
	print *, TEFF,GLOG,ASALOG,NHE,INUM


C	line 05
C	=======
      READ(4,*) PTDISK, MU

C	line 06
C	=======
      READ(4,*) afstar  ! metallicity of the star (in log scale)


C	ISSUE: Line 7 missing
C	line 07 -- XXCOR(I)
C	=======
	READ(4,*) (XXCOR(I),I=1,NMETAL)



C	line 08 -- part of filename
C	=======
C	This line will define the names of other three files to be read:
C	  cont.<FILEFLUX>
C	  norm.<FILEFLUX>
C	  spec.<FILEFLUX>
	READ(4, 'A20') FILEFLUX
	READ(4,*) LLZERO,LLFIN,AINT
	write(6,711) LLZERO,LLFIN,AINT
711	FORMAT(/,2X,'LLZERO=',F10.5,2X,'LLFIN=',F10.5,2X,'AINT=',F8.3,/)


C	lines 10-17
C	===========
	DO IH=1,8
        READ(4, 'A') FILETOHY(IH)
	  print *,IH,filetohy(IH)
	END DO


      RETURN
      END

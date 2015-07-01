c	programme NULBT=NULBAD_SQ
c      	lecture et convol sortie fant93
C 	Programme de lecture des flux en binaire, sortant de 
C       Fantomol93 , calculs de 100A en 100A (ou inferieur a 100A)
c	
c	Paula, julho de 2003: 
c	- modifiquei para nao normalizar o espectro
c	- a opcao CONVOL=F nao estava imprimindo o lambda, corrigi
c	P.Coelho, dez 2003
c	- se NORM = .TRUE. (saida do pfant ja eh normalizada), nao
c	altera o valor do fluxo
C  
        PARAMETER (NP=2000000,NI=2000000)
	CHARACTER FILEFLUX*20,flcv*30
	CHARACTER TITC*20
	REAL*8 LZERO,LFIN,LAMBD,L0,LF
	LOGICAL CONVOL,FLAM,NORM
	REAL NHE
	INTEGER D,DMJ,DTOTC

	DIMENSION FFL(NP),FNU(NI),FFNU(NP),LAMBD(NP)
        DIMENSION FI(1501),TFI(1501)
        DIMENSION ALFL(NP),AFL(NP),FL(NP),TL(NP)

	NORM = .FALSE.
	
	PRINT *,'Binary file name:'
	READ(5,200) FILEFLUX
	IK=1
c        OPEN(UNIT=17,FILE=FILEFLUX,ACCESS='DIRECT',STATUS='OLD',
c	1 RECL=6025)
	OPEN(UNIT=17,FILE=FILEFLUX, STATUS='UNKNOWN')
C	AINT =intervalle de calcul
        ICLE=1
        KTOT=0
   12   CONTINUE
	READ(17, 1130) IKEYtot,TITC,TETAEFF,GLOG,ASALOG,NHE,AMG,
	1 L0,LF,LZERO,LFIN,ITOT,DPAS,ECHX,ECHY,FWHM
	READ(17, *)(FNU(D),D=1,Itot)
1130	FORMAT(I5, A20, 5F15.5, 4F10.1, I10, 4F15.5)
1132	FORMAT(40000F15.5)
	WRITE(6,*) IKEYtot,IK-1,itot
	if(icle.eq.ikeytot) itot=itot+1
        DO D=1,ITOT-1
        K=KTOT+D
         FFNU(K)=FNU(D)
        END DO
        KTOT=K
        ICLE=ICLE+1
        IF(ICLE.GT.IKEYTOT) GO TO 14
        GO TO 12
   14   CONTINUE
        NPT=K

	PRINT *,'Is spectra normalized? (T/F)'
	READ(5,*) NORM 

c        imax=isup(ffnu,ktot,1,ktot)
c        fmax=ffnu(imax)
c        write(6,123)imax,fmax
c123	format(1x,'imax=',i8,2x,'fnumax=',e14.6)
c        do K=1,ktot
c        fffnu(K)=FFNU(K)/fmax
c        fffnu(K)=FFNU(K)
c        end do

        lambd(1)=L0
        do k=1,ktot
        lambd(k)=L0 + (k-1)*dpas
        end do	
C  transformation de Fnu en Flambda
	PRINT *,'Fnu to FLambda transformation? (T or F)'
	read(5,*) FLAM
        IF(.NOT.FLAM) GO TO 60
        C=2.997929E+10
        DO K=1,KTOT
        CA=1.E+11/(LAMBD(K)**2)
c  ffnu(10(-5) etait x 10(5), donc cte=10(11) et pas 10(16)
        CB=CA*C
        IF (.NOT.NORM) THEN
		FFL(K)=FFNU(K)*CB
	ELSE 
		FFL(K)=FFNU(K)
	END IF
        END DO
C
c        write(6,*) NPT,KTOT
        write(6,122) dpas,KTOT
122	format(2x,'pas=',f8.3,2x,'ktot=', i10)
c        iimax=isup(ffl,ktot,1,ktot)
c        ffmax=ffl(iimax)
c        write(6,121)iimax,ffmax
c121	format(1x,'iimax=', i8,2x,'fflmax=',e14.6)
c        do K=1,ktot
c        ffl(K)=FFl(K)/ffmax
c        ffl(K)=FFl(K)
c        end do
   60     CONTINUE
          
  
	PRINT *,'ASC file name to create:'
	read(5,200) flcv
	open(unit=8,status='new',file=flcv)

	PRINT *,'Step:'
	READ(5,*) PAT
        IP=PAT/DPAS
C	PRINT *,' CONVOL (T ou F),GAUSS (T),FWHM(A)'
C	READ(5,*) CONVOL,GAUSS,FWHM
	PRINT *,' CONVOL (T ou F),FWHM(A)'
	READ(5,*) CONVOL,FWHM
C
C	Convolution sp synthetique avec profil instrumental
C
	IF(.NOT.CONVOL) GO TO 65
C	CALL CAFCONVH(GAUSS,DPAS,FWHM,IFT,TFI,FI)
	CALL CAFCONVH(DPAS,FWHM,IFT,TFI,FI)
	WRITE(6,101) IFT
	J=(IFT-1)/2
	JP1=J+1
	DMJ=KTOT-J
	DTOTC=DMJ-JP1+1
C
        M=0
	DO D=JP1,DMJ,IP
	M=M+1
	TL(M)=LAMBD(D)
	END DO
	KKTOT=M
C
        IF(FLAM) THEN
        DO K=1,KTOT
        FL(K)=FFL(K)
        END DO
        ELSE
        DO K=1,KTOT
        FL(K)=FFNU(K)
        END DO
        END IF
	CALL VOLUT (FL,KTOT,FI,J,IP,DPAS,ALFL)
	K=0
	DO I=JP1,DMJ,IP
	K=K+1
	AFL(K)=ALFL(I)
	END DO
	KKTOT=K
65      CONTINUE

	IF((.NOT.CONVOL).AND.(.NOT.FLAM)) THEN
	KKTOT=KTOT
	DO K=1,KKTOT
	AFL(K)=FFNU(K)
	END DO
	END IF

	IF((.NOT.CONVOL).AND.(FLAM)) THEN
	KKTOT=KTOT
	DO K=1,KKTOT
	AFL(K)=FFL(K)
	TL(K)=LAMBD(K)
	END DO
	END IF

	IF(CONVOL) THEN
	ALZ=TL(1)
	ALF=TL(KKTOT)
	ELSE
	ALZ=LAMBD(1)
	ALF=LAMBD(KKTOT)
	END IF

	write(8,201) TITC,TETAEFF,GLOG,ASALOG,AMG
	WRIte(6,110) TETAEFF,GLOG,ASALOG,NHE,AMG
	WRITE(6,130) ALZ,ALF,KKTOT,PAT,FWHM
 	write(6,120) L0,LF,KTOT,DPAS
	WRITE(6,240) (AFL(K),K=1,12)
        write(8,202) KKTOT,L0,LF,PAT,FWHM
        DO K=1,KKTOT
 	 WRITE(8,*) TL(K), AFL(K)
        END DO
101	FORMAT(' IFT=',I4)
c102	FORMAT(2X,F9.3,2X,)
104	FORMAT(' JP1=',I3,2x,'DMJ=',I6,2X,'DTOTC=',I6,/,
	1 ' DTOT=',I6,2X,'KTOT=',I6)
105	FORMAT(1X,' TT(JP1)=',F8.3,/,' TL(1)=',F8.3,2X,'TL(KTOT)=',F8.3)
125	FORMAT(2X,'Lzero=',F8.3,2x,'Lfin=',F8.2,2x,'KTOT=',I7,2X,
	1 'PAS original='F5.2,2x,/,2X,'KKTOT=',I7,2X,'PAS nouveau=',F5.2,
	1 2X,'FWHM=',F5.2)
110	FORMAT(2X,'tetaeff=',F8.3,2X,'log g=',F6.2,2X,'[M/H]=',F6.2,
	1 2X,'NHE=',F5.2,2X,'[Mg/Fe]=',F6.3)
120	FORMAT(2X,'Lzero=',F8.3,2x,'Lfin=',F8.2,2x,'KTOT =',I7,
	1 2X,'PAS original='F5.2)
130	FORMAT(2X,'Lzero=',F8.3,2x,'Lfin=',F8.2,2x,'KKTOT=',I7,
	1 2X,'PAS nouveau =',F5.2,2x,'FWHM=',F5.2)
200     FORMAT(A)
201     FORMAT('#',A,'Tef=',F6.3,X,'log g=',F4.1,X,'[M/H]=',F5.2,X,F5.2)
202     FORMAT('#',I6,2X,'0. 0. 1. 1. Lzero =',F10.2,2x,'Lfin =',
	1 F10.2,2X,'PAS =',F5.2,2x,'FWHM =',F5.2)
210	FORMAT(2X,F6.3,2X,F5.2,2X,F5.2,2X,F5.2)
220	FORMAT(2X,F8.3,1X,F8.3,1X,I10,F5.2)
230	FORMAT(2X,F8.3,1X,F8.3,1X,I10,2F5.2)
240	FORMAT(12F6.3)
	STOP
	END
C
	SUBROUTINE CAFCONVH(PAT,FWHM,IFT,TT,FI)
C	ON CALCULE FI(TT) la fonction de convolution EN IFT PTS
C
C	LA FONCTION DE CONVOLUTION PEUT AVOIR 1501 PTS
C	 SI L ON CHANGE CE NBRE DE PTS CHANGER AUSSI IPPTOT=750
C	 DANS LES DATA QUELQUES LIGNES PLUS BAS.
C	 (LA MOITIE DU NBRE TOT DE PTS POUR LE CALCUL DES AT)
C
C	LOGICAL GAUSS
	DIMENSION TT(1501),FI(1501),AT(-750:+750)
C	DIMENSION XFI(1501),YFI(1501)
	IPPTOT=750
	C7=1.772453 
C
C	GAUSS: PROFIL GAUSSIEN DE 1/2 LARG AA
	SIGMA=FWHM/2.35482
	AA=1.414214*SIGMA
	WRITE(6,119) FWHM,SIGMA,AA
	TOTLARG=3.0 * AA
	WRITE(6,134)TOTLARG
	AT(0)=0
		DO I=1,IPPTOT   ! IPPTOT TAILLE MAX DE LA FCT DE CONV
		AT(I)=PAT * I
		AT(-I)=-AT(I)
		IF(AT(I).GT.TOTLARG)   GO TO 40
		END DO
	BB=3*AA
	WRITE(6,133)BB
	STOP
40	IFD=I-1
	IFT= 1 + 2*IFD
		IF(IFT.GT.1501)  THEN
		WRITE(6,137)
		STOP
		END IF
		DO I=1,IFT
		TT(I)=AT(I-IFD-1)
		END DO
	Z = C7*AA
		DO  I=1,IFT
		FI(I) = EXP ( -(TT(I)/AA)**2)
		END DO
C		WRITE(6,118) ((TT(I),FI(I) ),I=1,IFT )
C		WRITE(6,115)
C
C	WRITE(6,109)   Z
		DO  I=1,IFT
		FI(I) = FI(I) / Z
		END DO
C		WRITE(6,115)
C		WRITE(6,118) ((TT(I),FI(I) ),I=1,IFT )
C		WRITE(6,115)
	RETURN
109	FORMAT(7E15.4)
115	FORMAT(1H )
118	FORMAT(5(2X,F7.3,F7.3) )
119	FORMAT(1X,'Profil instrumental gaussien'/,
	1 1X,'FWHM =',F7.3,' (A)'/,1X,'Sigma=',F7.3,' (A)'/,
	1 1X,'1/2 Largeur AA =',F7.3,' (A)')
133	FORMAT(' LE PROFIL GAUSSIEN PAR LEQUEL ON CONVOLE NE PEUT',
	1' AVOIR PLUS DE 121 PTS  (60 DE CHAQUE COTE DU CENTRE)',
	2/' CETTE GAUSSIENNE EST CALCULEE JUSQU A UNE DISTANCE DE',
	3' 3 DEMI-LARGEUR DU CENTRE',
	4/' SOIT:',F7.3,' ANGSTROM',
	4/'   ELARGISSEZ LE PAS DU CALCUL')
134	FORMAT(' Gaussienne calculee jusqu a une distance du centre:',
	3 /,' 3(SIGMA*1.414)=',F7.3,' (A)')
137	FORMAT(5X,'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER A ',   
	1 /' PLUS DE 500 PTS -CHANGEZ LE PAS-. (LE NBRE DE PTS TOTAL '
	2 /' SUR LA FCTION S OBTIENT EN MULTIPLIANT PAR 6 LE NBRE '
	3 /' DE PTS SUR LA DEMI LARGEUR')
138	FORMAT( 5X, 'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER',
	1 ' A PLUS DE 500 PTS. C''EST TROP !')
139	FORMAT(5X,'-CHANGEZ LE PAS DU CALCUL !-')
	END
C
	SUBROUTINE VOLUT (S,ITOT,FI,J,IP,PA,PS)
	DIMENSION PS(ITOT),S(ITOT),FI(1501)
	DO 1 I=1,ITOT
1	PS(I)=0.
	JP1 =J+1
	IMJ=ITOT-J
	J2P =  2*J +1
	DO 2 I=JP1,IMJ,IP
	DO 2 K=1,J2P
2	PS(I) =PS(I)  +  S(I-J+K-1) *FI(K)*PA
	JJP1=JP1+1
	IIMJ=IMJ+1
		DO I=1,JJP1
		PS(I)=S(I)
		END DO
		DO I=IIMJ,ITOT
		PS(I)=S(I)
		END DO
	RETURN
102	FORMAT (10F10.3)
100	FORMAT(I20)
101	FORMAT(' MTOT=',I4)
	END
C
	FUNCTION ISUP(FR,ITOT,IA,IZ)
C	UNE FONCTION FR EST CONNUE EN ITOT POINTS. ON CHERCHE ENTRE
C	LES POINTS IA ET IZ QUEL EST L INDICE I OU CETTE FONCTION
C	EST MAXIMUM.
	DIMENSION FR(ITOT)
	IA2=IA+1
	ISUP=IA
	FMAX=FR(IA)
	DO 1 I=IA2,IZ
	IF(FR(I).LT.FMAX)   GO TO 1
	FMAX=FR(I)
	ISUP=I
1	CONTINUE
	RETURN
	END

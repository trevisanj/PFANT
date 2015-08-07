c                          LIRMOD /Janvier 1994  
c     Programme permettant de lire et/ou manipuler des fichiers de modeles 
c      cree par ex. par ECTOR
c
c       Modifie le 27 Oct 1998 pour lecture acces direct compatible SUN (ESO)
c       i.e.: - associatevariable n'existe pas (instruction OPEN)
c	      - recordsize=4400 (au lieu de 300) (instruction OPEN)
c             - ID n'est pas incremente apres une instruction READ(18,REC=ID)
c
	LOGICAL OK,newfich
        integer oplec,opclass
        integer TIntot(100),TIntot5(100)
	REAL NH(50),TETA(50),PE(50),PG(50),TOL(50),BID(16),mod(250)
        character*40 nomfich,nomfich2
        character*4 TIR(5),TIABS(5)
        character*4 TITIR(100,5)
        character*7 stat
C        dimension TIntot5(100),Ident(100),TIntot(100)
         dimension Ident(100)
        real TIteaf(100),TIglog(100),TIasalog(100),TInhe(100),
     1       TIamet(100)
	COMMON /COM8/ID
	ID=1
 1      WRITE(6,*) ' Nom du fichier a lire  (entre '' '')'
        READ(5,*) nomfich
        open(unit=18,access='direct',status='old',file=nomfich,
	1 RECL=1200)       ! modif 20/10/03
c
 50     WRITE(6,*) ' Lecture des titres              Entrez 1'
        WRITE(6,*) ' Lecture detaillee d''un modele   Entrez 2'
        WRITE(6,*) ' Re-classement des modeles       Entrez 3'
        WRITE(6,*) ' STOP                            Entrez 0'
        WRITE(6,*) ' Autre fichier                   Entrez 999'
        READ(5,*)  oplec
        if (oplec.eq.0)   STOP
        if (oplec.eq.1)   go to 51
        if (oplec.eq.2)   go to 52
        if (oplec.eq.3)   go to 53
        if (oplec.gt.900) go to 1
c
 51     WRITE(6,*) ' Premiere et Derniere ligne dont on veut le titre'
        READ(5,*) IDA,IDZ
	WRITE(6,129)
        ID=IDA
17	READ(18,REC=ID) N
	    IF(N.GT.9900)  then
            WRITE(6,*) ' Dernier modele atteint'
            WRITE(6,*) ' '
            go to 50
            end if
	READ(18,REC=ID) N,B,C,D,E,G,TIR,TIABS
	ID=ID+1       ! modif 27/10/98
	IDD=ID-1
	WRITE(6,130) IDD,B,C,D,E,G,N,TIR
        WRITE(6,*) '      Calcule avec table d''absorption',TIABS
           if (IDD.LT.IDZ) then
	   go to 17
           else
           WRITE(6,*) ' '
           go to 50
           end if
C
 52     WRITE(6,*) ' QUELLE LIGNE?'
	READ(5,*) IL
	ID=IL
	READ(18,REC=ID) NTOT,B,C,D,E,G,TIR,TIABS
	ID=IL
        write(6,*) ' modele a la ligne', IL
        write(6,133) TIR, TIABS
        WRITE(6,128) B,C,D,E,G
        write(6,*) NTOT,' niveaux'
	WRITE(6,132)
	READ(18,REC=ID)BID,(NH(I),TETA(I),PE(I),PG(I),TOL(I),I=1,NTOT)
		DO N=1,NTOT
		WRITE(6,131)N,NH(N),TETA(N),PE(N),PG(N),TOL(N)
		END DO
        WRITE(6,*) ' '
	Go to 50
c
 53     do ii=1,100
           ID=ii
           READ(18,REC=ID) TIntot(ii)
           write(6,*) TIntot(ii)
           write(22,*) TIntot(ii)
           ITT=TIntot(ii)
           IF(ITT.GT.9900) then
              ID=ii
              goto 1515
           end if
           READ(18,REC=ID) TIntot(ii),TIteaf(ii),TIglog(ii),
     1       TIasalog(ii),TInhe(ii),TIamet(ii),(TItir(ii,jj),jj=1,5)
           TIntot5(ii)=TIntot(ii)*5
        end do
        WRITE(6,*) 'on ne considere que les 100 premiers modeles'
 1515   ntmod=ID-1
        WRITE(6,*) ntmod,' modeles consideres'
        do ii=1,ntmod
           Ident(ii)=ii
        end do
c
        WRITE(6,*) 'Supprimer les lignes i a j                Entrez 1'
        WRITE(6,*) 'Copier les lignes i a j dans un fichier   Entrez 2'
c        WRITE(6,*) 'Ranger les modeles selon [M/H]            Entrez 3'
c        WRITE(6,*) 'Ranger les modeles selon Teff             Entrez 4'
        WRITE(6,*) 'Revenir au menu principal                 Entrez 0'
        READ(5,*)  opclass
c
        if (opclass.eq.1) go to 61
        if (opclass.eq.2) go to 62
        if (opclass.eq.3) go to 63
        if (opclass.eq.4) go to 63
        if (opclass.eq.0) go to 50
c
 61     WRITE(6,*) 'ATTENTION !!!! Le fichier resultant aura l''air d''avoir
     1 raccourci, mais il n''en sera rien !' 
        WRITE(6,*) 'Les (j-i) derniers modeles seront toujours presents a la
     1 fin du fichier, meme si LIRMOD n''est plus capable de les lire'
        WRITE(6,*) ' Entrez i et j'
        READ(5,*) i,j
        do ii=(j+1),ntmod
           ID=ii     !ID de lecture du modele
           READ(18,REC=ID)BID,(mod(k),k=1,TIntot5(ii))
           WRITE(6,*) 'titre du modele traite'
           WRITE(6,*) TIntot,TIteaf(ii),TIglog(ii),TIasalog(ii),
     1         TInhe(ii),TIamet(ii),(TItir(ii,jj),jj=1,5)
           ID=ii - j + i - 1   !ID d'ecriture de ce modele a la bonne place
           WRITE(6,*) 'id de re ecriture',ID
           TIntot(ii)=TIntot5(ii)/5
           write(18,REC=ID)TIntot(ii),TIteaf(ii),TIglog(ii),
     1       TIasalog(ii),
     1         TInhe(ii),TIamet(ii),(TItir(ii,jj),jj=1,5),
     1         (mod(k),k=1,TIntot5(ii))  
        end do
	ID=ID+1       ! modif 27/10/98
        write(18,REC=ID) 9999
        go to 50
c
c
 62     WRITE(6,*) ' Entrez i et j'
        READ(5,*) i,j
        WRITE(6,*) ' Entrez le nom du fichier et le numero de ligne ou 
     1  les copier;faut-il creer ce fichier? (T/F)'
        READ(5,*) Nomfich2,IID2,newfich
        if (newfich) then 
           stat='unknown'
           else
              stat='old'
        end if      
        open(unit=19,access='direct',STATUS=stat,file=nomfich2,
     1        RECL=1200)
        ID2=IID2
        do ii=i,j
           ID=ii   !ID de lecture du modele
           READ(18,REC=ID)BID,(mod(k),k=1,TIntot5(ii))
           WRITE(6,*) 'titre du modele traite'
           WRITE(6,*) TIntot,TIteaf(ii),TIglog(ii),TIasalog(ii),
     1         TInhe(ii),TIamet(ii),(TItir(ii,jj),jj=1,5)
           TIntot=TIntot5(ii)/5
           write(19,REC=ID2)TIntot,TIteaf(ii),TIglog(ii),TIasalog(ii),
     1         TInhe(ii),TIamet(ii),(TItir(ii,jj),jj=1,5),
     1         (mod(k),k=1,TIntot5(ii))
	   ID2=ID2+1       ! modif 27/10/98
        end do
        write(19,REC=ID2) 9999
        close(19)
        go to 50

c
 63     if (opclass.eq.3) then
           WRITE(6,*) 'Les modeles seront ranges suivant [M/H] croissant,
     1          Teff croissant et log g croissant'
        else
           WRITE(6,*) 'Les modeles seront ranges suivant Teff croissant,
     1          [M/H] croissant et log g croissant'
        end if
c        
        WRITE(6,*) ' Entrez le nom du fichier range et le numero de ligne ou 
     1      on commence a ecrire;faut-il creer ce fichier? (T/F)'
        READ(5,*) Nomfich2,IID2,newfich
        if (newfich) then 
           stat='unknown'
           else
              stat='old'
        end if      
        open(unit=19,access='direct',STATUS=stat,file=nomfich2,
     1        RECL=1200)
c
        WRITE(6,*) 'on range les numrero ID'
c       call rangx(TIglog,Ident,ntmod)
        if (opclass.eq.3) then     ! On range par [M/H]
c           call rangx(TIteaf,Ident,ntmod)
           call rangx(TIasalog,Ident,ntmod)
        else                   ! On range par Tetaeff
c           call rangx(TIasalog,Ident,ntmod)
           call rangx(TIteaf,Ident,ntmod)
        end if
        do ii=1,ntmod
           WRITE(6,*) 'ID',ii,'= ',Ident(ii)
        end do   
c
        WRITE(6,*) 'on ecrit le modele range'
        ID2=IID2   !ID d'ecriture du modele
        do ii=1,ntmod
           ID=Ident(ii)   !ID de lecture du modele
           READ(18,REC=ID) BID,(mod(I),I=1,TIntot5(ii))
           WRITE(6,*) 'test : compare BID et le titre suppose'
           WRITE(6,*) BID
           WRITE(6,*) TIntot,TIteaf(ii),TIglog(ii),TIasalog(ii),
     1         TInhe(ii),TIamet(ii),(TItir(ii,jj),jj=1,5)
           TIntot=TIntot5(ii)/5
           write(19,REC=ID2)TIntot,TIteaf(ii),TIglog(ii),TIasalog(ii),
     1         TInhe(ii),TIamet(ii),(TItir(ii,jj),jj=1,5),
     1         (mod(I),I=1,TIntot5(ii))
	   ID2=ID2+1       ! modif 27/10/98
        end do
        close(19)
        go to 50
        



c
 128	FORMAT(3X,F8.0,4F8.3)
 129	FORMAT('   MODELES SUR LE DISQUE')
 130    FORMAT(1X,I5,F8.0,4F8.3,I6,2X,5A4)
 131	FORMAT(I4,E20.4,F8.4,2E13.4,F9.3)
 132	FORMAT(15X,'NH',10X,'TETA',11X,'PE',11X,'PG',5X,'L TO')
 133    FORMAT(5A4,' Calcule avec la table d''absorption ',5A4)
	END



c -----------------------------------------------------------
	SUBROUTINE RANGX (X,Y,NTOT)
C	POUR PASSER DE RANGX A RANGD AJOUTER LA CARTE REAL*8
C	REAL*8 X,Y,TX,TY
C   POUR PASSER DE RANGI A RANGX RETIRER LA CARTE  INTEGER
C   INTEGERX,TX
	DIMENSION Y(NTOT)
        integer TY, Y
        real X(NTOT)
        real TX
	DO 1 I=2,NTOT
	DO 2 K=2,I
	J=I-K+2
	IF(X(J).GE.X(J-1))   GO TO 1
	TX=X(J)
	TY=Y(J)
	X(J)=X(J-1)
	Y(J)=Y(J-1)
	X(J-1)=TX
2	Y(J-1)=TY
1	CONTINUE
	RETURN
	END


c	                        TRANSOSMARCS
c     Ce programme interpole a pas constant en to 5000 un modele
c     de NEWMARCS.
c     Il calcule la masse puis  NH pour chaque couche
c     On range le modele interpole dans un fichier
c     de type modeles (lisible par lirmo2, ector2, rai12, fantom2 etc...)
c     Linker avec ABSOR
c     
c     Le modele OSMARCS est donne sous la forme donnee sur le web en 2005
c     Cette version traite une entete de la forme
c     1 1ere ligne pour rien
c     2 Tegg log g 
c     3 Nbre total de profondeurs
c     4 La longueur d'onde du Tau donne (5000, 12000...)
c     5 une ligne pour rien
c     6 le modele proprement debute en ligne 6 etc...
c
c     Dans ce programme on a ecrit dans le fichier de sortie le nom de la
c     table d'absorption utilisee comme "nom de modele" (ca se voit 
c     avec lirmo2)
	logical anal
	real*4  T(100),TET(100),P(100),Pe(100),Rossk(100),
     &          AMAS(0:100),TR(100),Nh(100),Nh1(100),rox(100)
        real*4 Trl(100),ZNH(100),ZTET(100),ZP(100)
        real*4  ZPe(100), K5(100),T5(0:100),Tl5(100),Tl12(100)
c       T=tau interpole, Trl=log(tauRoss), T5=tau5000, Tl5=log(tau5000)
        real*4  TOTKAP(2),ZZP0(30), PARAM
c       Nh=nbre atomes H au dessus, ZZP0=pour SP lectur
	real KB, TT, vt, lambdatau ,    xmum  
c TT=temperature, vt=microturb lambdatau=longueur d'onde ou le tau est calcule
c   xmum = mean atomic weight
        character*7  Stat
	character*40 Nomfia,Nomfib
        character*80 titcol,aa
	logical newfich
        COMMON/STATOU/ID,Stat,Nomfib
        COMMON/TIT/ TIR(5)   ! titre table d'absor, commmon avec ABSORU
	COMMON/LECT1/AMET,BHEa 
	COMMON/LECT2/  ZZP(30),VALR1(115)
        COMMON/COM2/ TEFF,GLOG,ASALOG,ASALALF,BHE
c
c	anal=.true.
	anal=.false.
	KB=1.38046E-16
c
c ************************ INITIALISATIONS ********************
	write(6,*)' Traite les modeles OSMARCS du web'
	write(6,*)' avec l''en tete des modeles 2005'
	write(6,*)' '
	write(6,*)'  Fichier de modeles a lire?   (entre '' '')'
	read(5,*) Nomfia
	open (unit=11,status='old',file=Nomfia)
	open (unit=10,status='unknown',file='sortie.dat')
        write(6,*)'  fichier de modeles a ecrire? (entre '' '')' 
        read(5,*) Nomfib
        write(6,*)' Faut il creer ce fichier  (T/F)'
        read(5,*) newfich
             if(newfich) then
             Stat='unknown'
             else
             Stat='old'
             end if
        call PRIMO4(.true.,nt,Trl,NH,TET,PE,P)   ! open du fich de sortie
	IMOD=0   ! numero du fichier que l'on calcule
c
c
 1      IMOD=IMOD+1
	read(11,101,END=10) aa    ! lecture de 1 ligne pour rien: code
        write(85,*)aa
        read(11,*,END=10) TEFF
        write(85,*)TEFF
        TETAEF=5040/TEFF
	read(11,101,END=10) aa    ! lecture de 1 ligne pour rien: flux
        write(85,*)aa
        read(11,*,END=10) G
	GLOG=ALOG10(G)
        write(85,*) G,GLOG
	read(11,*,END=10) vt
        write(85,*)vt 
	read(11,101,END=10) aa    ! lecture de 1 ligne pour rien: mass
        write(85,*)aa
	read(11,*,END=10) asalog, asalalf   ! lecture metallicite et alpha
	ASASOL=10.**Asalog
        write(85,*)asalog,asalalf,asasol
	do iii=1,3
	read(11,101,END=10) aa    ! lecture de 5 lignes pour rien
        write(85,*)aa
	end do
	read(11,*) PARAM
	read(11, 101, END=10) aa
	read(11,*,END=10) hhhh,hehe   ! lecture de logN(H) et logN(He)
        write(85,*)hhhh,hehe
	write(6,*) 'N(H), N(He)',hhhh,hehe   ! lecture de logN(H) et logN(He)
	BHE=10**(hehe-hhhh)
	write(6,*) "Teff  glog  met alfa  bhe  vt"
        write(6,*) Teff, glog, Asalog, asalalf, BHE, vt
	do iii=1,9
	read(11,101,END=10) aa    ! lecture de 9 lignes pour rien
        write(85,*)aa
	end do
        read(11,*)nx
        write(85,*)nx
        write(6,*)nx
	read(11,101,END=10) aa    ! lecture de 1 ligne pour rien: titre
        write(85,*)aa
	read(11,101,END=10) aa    ! lecture de 1 ligne pour rien: titrecol
        write(85,*)aa
c	print *, PARAM
c	STOP
c	PARAM=1.6733E-24*(1+4*BHE)
	PARAM=1/(6.022142e23*PARAM) 
	CT=PARAM * 10**GLOG
        write(6,*)' lecture du modele'
	 do k=1,nx
	   read(11,*) nbid,bid,Tl5(k),bid,TT, Pe(k),P(k)
	   TET(k)=5040./TT
	   T5(k)=10**(Tl5(k))
	 end do
 	read(11,101,END=10) aa    ! lecture de 1 ligne pour rien: titrecol
        write(6,*)' lecture du modele: 2ieme partie'
        write(85,*)aa
	 do k=1,nx
c                  Rox = masse au dessus de chaque couche
c                   Calcul de NH a partir de cette masse 
	  read(11,*) nbid,Trl(k),bid,bid,xmum,bid,bid,rox(k)
	  write(85,*) nbid,Trl(k),bid,bid,xmum,bid,bid,rox(k)
          Tr(k)=10**(Trl(k))
	  nh(k)=rox(k)/PARAM
c	  nh(k)=rox(k)/(1.6733E-24*xmum)
	 end do
       if(anal) then
       write(6,*)'      logTo5000  logToRoss    T     Pe      Pgas'
      write (6,109) (k,Tl5(k),TRl(k),5040/TET(k),Pe(k),P(k),k=1,NX)
       write(6,*)'      logToRoss      RhoX         NH'
	write (6,111) (k,TRl(k),rox(k),NH(k),
     &  k=1,NX)
       write(85,*)'      logTo5000  logToRoss    T     Pe      Pgas'
      write (85,109) (k,Tl5(k),TRl(k),5040/TET(k),Pe(k),P(k),k=1,NX)
       write(85,*)'      logToRoss      RhoX         NH'
	write (85,111) (k,TRl(k),rox(k),NH(k),
     &  k=1,NX)
	end if  ! fin du if anal
c     **********************************************************
	nnnn = nx*3+4
        do I=1,nnnn
        read(11,101)aa   ! 3nx+4 lignes pour rien a la fin d'1 modele
        write(85,101)aa
        end do
c     *** FIN de lecture du modele original   ***
c
c     **********   Interpolation a pas constant en log to5  **********
	n=1
	T(1)=-3.4
           if (T(1).lt.Tl5(1))then
           T10=Tl5(1) * 10
           IT10=T10
           T10=IT10*0.1
           T(1)=T10
           write(6,*)' Interpolation a partir de To(1)=', T(1)
           end if
		do while(T(n).lt.1.5)
		n=n+1
		T(n)=T(1)+(0.1*(n-1))
		end do
		nt=n
	call fmt2(nx,Tl5,TET,Pe,P,NH,nt,T,ZTET,ZPe,ZP,ZNH)


	do n=1, nx
	print 10101, TET(n), PE(n), P(n), NH(n)
	end do
10101   format(f10.4, 3e18.4 )
c
c       **********                 sorties              **********
c
c        write(6,*)
c        write(6,103)
        write(6,102)
	do k=1,nt
	write(6,105)k,ZNH(k),ZTET(k),ZPe(k),ZP(k),T(k)
	end do
c
        If(IMOD.EQ.1) then
        write(6,*)' Sur quelle ligne du fichier ',Nomfib
        write(6,*)'ecrit on ce premier modele ? (les autres a la suite)'
        read(5,*) JD
        else
        JD=JD+1
        End if   ! fin du if IMOD=1
        ID=JD
        call PRIMO4(.false.,nt,T,ZNH,ZTET,ZPE,ZP)
        goto 1
c
 10     stop
 100    format(I5,E12.4,2F10.3,2E12.4)
 101    format(A80)
 102	format(13X,'NH',10X,'TETA',11X,'PE',11X,'PG',4X,'l TO5',
     &         4X,'lTO12')
 103    format(12x,'nH',7x,'log tor',5x,'teta',6x,'Pe',10x,'Pg')
 104    format(f12.0,3F10.2)
 105	format(I4,E18.4,F8.4,2E13.4,F9.3, e18.5)  
 107	format(I5,3E14.5)   
 109	format(I3,2F6.2,F8.0,2E12.4)
 111	format(I5,F6.2,2E12.4)
       end
	


	subroutine fmt2(nx,X,Y1,Y2,Y3,Y4,nt,T,Z1,Z2,Z3,Z4)
	dimension X(100),Y1(100),Y2(100),Y3(100),Y4(100),
	1     T(100),Z1(100),Z2(100),Z3(100),Z4(100)
c     Dans une table, pour nx valeurs de X, on a nx valeurs de Y1,
c     de Y2 de Y3 et de Y4
c     [table X  Y1  Y2  Y3  Y4 de nx valeurs].
c     On interpole (par un polynome d'ordre 2) dans cette table les Y
c     correspondant aux nt valeurs de X rangees dans T(100). On en deduit
c     les nt valeurs de Y1, Y2, Y3 Y4 que l'on range dans Z1, Z2, Z3 et Z4.
c     D'ou la table 'interpolee' T  Z1  Z2  Z3 Z4 de nt valeurs.
	inv=-1
	if (X(nx).lt.X(1)) inv=1
c   ATTENTION !
c   valable poour table croissante en X sinon remplacer TT .lt. X(j) par 
c                                                       TT .gt. X(j)
c   (qques lignes + bas)
          do k=1,nt
          TT=T(k)
	        do j=1,nx
	        i=j
	          if  (TT .lt. X(j))  then
			if(i.eq.1)  i=3
			if(i.ge.nx) i=nx-1
	          call ftm2sub(TT,i,X,A,B,C,D,E)
		  Z1(k)=Y1(i-2)*A + Y1(i-1)*B + Y1(i)*C + Y1(i+1)*D
		  Z2(k)=Y2(i-2)*A + Y2(i-1)*B + Y2(i)*C + Y2(i+1)*D
		  Z3(k)=Y3(i-2)*A + Y3(i-1)*B + Y3(i)*C + Y3(i+1)*D
		  Z4(k)=Y4(i-2)*A + Y4(i-1)*B + Y4(i)*C + Y4(i+1)*D
		  go to 1
	          else
                       if ( TT .eq. X(j)) then  
		       Z1(k)=Y1(j)
		       Z2(k)=Y2(j)
                       Z3(k)=Y3(j)
		       Z4(k)=Y4(j)
	               go to 1
		       else
		       go to 2	
		       end if
	          end if
 2		print *, '  '
	        end do                ! fin bcle j(1,nx)
	        write(6,100) TT       ! on est en dehors de la table
 1	     print *, '  '	     
	    end do                   ! fin bcle k(1,nt)
	return
 100	format(3x,'On sort de la table d''interpolation pour T=',E15.7)
	end
c
c
c
	subroutine ftm2sub(TT,i,X,A,B,C,D,E)
c     calcul des coefficients de la formule d'interpolation
	dimension X(100)
	t1 = X(I-2)
	t2 = X(I-1)
	t3 = X(I)
	t4 = X(I+1)

	A=((tt-t2)*(tt-t3)*(tt-t4))/((t1-t2)*(t1-t3)*(t1-t4))
	B=((tt-t1)*(tt-t3)*(tt-t4))/((t2-t1)*(t2-t3)*(t2-t4))
	C=((tt-t1)*(tt-t2)*(tt-t4))/((t3-t1)*(t3-t2)*(t3-t4))
	D=((tt-t1)*(tt-t2)*(tt-t3))/((t4-t1)*(t4-t2)*(t4-t3))
	
	E=0.0
	return
	end

 

       	SUBROUTINE INAITB(X,Y,P,N)
C CE PROGRAMME INTEGRE NUMERIQUEMENT UNE FONCTION RELLE, DONNEE PAR
C UN TABLEAU DE VALEURS X(I),Y(I). LA METHODE CONSISTE A CALCULER
C L' INTEGRALE DEFINIE SUR CHAQUE INTERVALLE (X(I),X(I+1)) D'ABORD
C  PAR SIMPSON,PUIS PAR GAUSS A DEUX POINTS. LES VALEURS DE Y AU 
C  POINT MILIEU ET AU DEUX POINTS DE GAUSS SONT CALCULES PAR INTER-
C  POLATION CUBIQUE A QUATRE POINTS EN UTILISANT LES VALEURS DE LA
C  TABLE.ON FORME ENSUITE UNE MOYENNE PONDEREE DES DEUX RESULTATS
C  QUI ANNULE L'ERREUR DU QUATRIEME (ET AUUSI DU CINQUIEME PAR RAI-
C   SON DE PARITE) ORDRE.L' ERREUR SUR L'INTEGRATION EST DONC
C  GENERALEMENT NEGLIGEABLE PAR RAPPORT A L'ERREUR SUR L'INTERPOLA-
C  TION DU TROSIEME ORDRE.
C
C *******PAS DE CALCUL D'ERREUR DANS CETTE VERSION*****************
C
	DIMENSION X(N),Y(N),ALY(200),P(N)
	COMMON /GENE/ALY,XMILIEU,XGAUSS1,XGAUSS2,DEL,YM 
     1  ,YG1,YG2,CONST,I,J
c        write(6,*)'  Appel de INAITB'
	P(1)=0.
	CONST=1./SQRT(3.)
	I=1
	J=1
c        write(6,*)' 1er intervalle'
	CALL STEPB(X,Y,P,N)
	  DO I=2,N-2
	  J=I-1
c         write(6,*)' intervalle', I
	  CALL STEPB(X,Y,P,N)
	  END DO
	I=N-1
	J=N-3
c       write(6,*)' Dernier intervalle'
	CALL STEPB(X,Y,P,N)
	END



	SUBROUTINE STEPB(X,Y,P,N)
	DIMENSION X(N),Y(N),ALY(200),P(N)
c       COMMON avec INAIT
	COMMON /GENE/ALY,XMILIEU,XGAUSS1,XGAUSS2,DEL,YM 
     1  ,YG1,YG2,CONST,I,J
c        write(6,*)' Appel STEPB    I et J=',I,J
	XMILIEU=(X(I)+X(I+1))/2.
	DEL=X(I+1)-X(I)
	XGAUSS1=XMILIEU-DEL*CONST/2.
	XGAUSS2=XMILIEU+DEL*CONST/2.
c        write(6,*)' 1er appel NAITK3'
	call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
	1           Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,YM)
c        write(6,*)' YM=',YM
c        write(6,*)' 2eme appel NAITK3' 
	call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
	1           Y(J),Y(J+1),Y(J+2),Y(J+3),XGAUSS1,YG1)
c        write(6,*)' YG1=', YG1
c        write(6,*)' 3eme appel NAITK3' 
	call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
	1           Y(J),Y(J+1),Y(J+2),Y(J+3),XGAUSS2,YG2) 
c        write(6,*)' YG2=', YG2
	AMYS=(Y(I)+4.*YM+Y(I+1))/6.
	AMYG=(YG1+YG2)/2.
 	AMY=0.6*AMYG+0.4*AMYS
	P(I+1)=P(I)+DEL*AMY
	RETURN
	END


	SUBROUTINE INTEGRA(X,Y,P,N,PDEB)
C	X=TABLEAU DE VALEURS DE LA VARIABLE INDEPENDANTE, PAR VALEURS 
C	CROISSANTES
C	Y=TABLEAU DES VALEURS ASSOCIEES DE LA FONCTION A INTEGRER
C	PDEB=VALEUR DE LA PRIMITIVE POUR X(1),PREMIERE VALEUR
C	DU TABLEAU
C	P=TABLEAU DES VALEURS DE LA PRIMITIVE AUX POINTS X(I)
C	METHODE : LA VALEUR DE L'INTEGRALE SUR L'INTERVALLE X(I),X(I+1)
C	EST CALCULEE PAR LA FORMULE DE SIMPSON,LA VALEUR DE Y AU POINT
C	MILIEU ETANT CALCULEE PAR INTERPOLATION CUBIQUE ,PAR LA ROUTINE
C	AITK3
	DIMENSION X(N),Y(N),P(0:N)
	P(1)=PDEB
c         write(6,*)' Entree dans INTEGRA'
c
c	CAS SPECIAL DU PREMIER INTERVALLE
c         write(6,*)' Premier intervalle'
	XMILIEU=(X(1)+X(2))/2.
	CALL NAITK3(X(1),X(2),X(3),X(4),
	1           Y(1),Y(2),Y(3),Y(4),XMILIEU,FX)
	P(2)=P(1)+((X(2)-X(1))/6.)*(Y(1)+Y(2)+4.*FX)
C	********************************
C	CAS GENERAL
		DO I=2,N-2
c                write(6,*)' Intervalle I=',I
		XMILIEU=(X(I)+X(I+1))/2.
		J=I-1
		call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
	1                   Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,FX)
		P(I+1)=P(I)+((Y(I)+Y(I+1)+4.*FX)/6.)*(X(I+1)-X(I))
		END DO
C	*********************************
C	CAS SPECIAL DERNIER INTERVALLE
c        write(6,*)' Dernier intervalle'
	XMILIEU=(X(N-1)+X(N))/2.
	J=N-3
	call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
	1           Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,FX)
	P(N)=P(N-1)+((X(N)-X(N-1))/6.)*(Y(N-1)+Y(N)+4.*FX)
C	***********************************
	RETURN
	END


	SUBROUTINE NAITK3(XdI,XdIp1,XdIp2,XdIp3,
	1                 YdI,YdIp1,YdIp2,YdIp3,XX,FX)
c
c	Nouvelle subroutine NAITK3 remplace AITK3 et AITK30
c        write(6,*)' Entree dans Naitk3'
	U=XdI
	V=XdIp1
	FU=YdI
	FV=YdIp1
c        write(6,*)' U,V,FU,FV', U,V,FU,FV
		F01=(FU*(V-XX)-FV*(U-XX))/(V-U)
c 	write(6,*)' F01=',F01
        U=XdIp2
	FU=YdIp2
c        write(6,*)' U,V,FU,FV', U,V,FU,FV
		F12=(FU*(V-XX)-FV*(U-XX))/(V-U)
c        write(6,*)' F12=',F12
	U=XdIp3
	FU=YdIp3
c        write(6,*)' U,V,FU,FV', U,V,FU,FV
		F13=(FU*(V-XX)-FV*(U-XX))/(V-U)
c        write(6,*)' F13=',F13
	U=XdI
	FU=F01
	V=XdIp2
	FV=F12
		F012=(FU*(V-XX)-FV*(U-XX))/(V-U)
	U=XdIp2
	FU=F12
	V=XdIp3
	FV=F13
		F123=(FU*(V-XX)-FV*(U-XX))/(V-U)
	U=XdI
	V=XdIp3
	FU=F012
	FV=F123
		F0123=(FU*(V-XX)-FV*(U-XX))/(V-U)
	FX=F0123
	RETURN
	END	


 
	SUBROUTINE PRIMO4 (IDEF,NIV,TOL,NH,TETA,PE,PG)
c     on ecrit les donnes sous une forme compatible avec ECTOR, RAI11 etc...
	LOGICAL PU,DK,IDEF
	REAL NH
	CHARACTER*7 STATUS
	Character*4 TIRMOD(5)
        CHARACTER*40 Nomfich
	COMMON /STATOU/ID,STATUS,Nomfich
	COMMON /TIT/ TIRABS(5)
C	Dimension TIRMOD(5)
        COMMON/COM2/ TEFF,GLOG,ASALOG,ASALALF,BHE
	dimension TOL(100),NH(100),TETA(100),PG(100),PE(100),
	1         A(250)
	data TIRMOD/'    ','    ','    ','    ','    '/
c
	WRITE(6,114) IDEF
	KEP=9999
	IF (.NOT.IDEF)   GO TO 7
	OPEN (UNIT=18,ACCESS='DIRECT',status='unknown',file=Nomfich,
	1   RECL=1200)
	RETURN
c
 7      write(6,*)' Appel Primo4 avec ID=',ID
        If(ID.GT.0)   GO TO 6
	ID=1
 10	READ(18,REC=ID)   NTOT
	IF(NTOT.LT.9900)   GO TO 10
C	ID=ID-1
	
 6	K=1
	WRITE(6,115)   ID, NIV
           if (NIV.gt.50) then
           write(6,*)' Trop de niveaux au modele de sortie '
           write(6,*)' On ne garde que les 50 premiers'
           NIV=50
           end if
 4	WRITE(6,105) TEFF,GLOG,ASALOG,ASALALF,BHE
	WRITE(6,109)(TIRABS(I) ,I=1,5)
        WRITE(6,109)(TIRMOD(I) ,I=1,5)
 	WRITE(85,105) TEFF,GLOG,ASALOG,ASALALF,BHE
	WRITE(85,109)(TIRABS(I) ,I=1,5)
        WRITE(85,109)(TIRMOD(I) ,I=1,5)
c
c
	IN=0
		DO N=1,NIV
		IN=IN+1
		K=(IN-1)*5 + 1
		A(K)    =NH(N)
		A(K+1)  =TETA(N)
		A(K+2)  =PE(N)
		A(K+3)  =PG(N)
		A(K+4)  =TOL(N)
c                WRITE(6,100) NH(N),TETA(N),PE(N),PG(N),TOL(N)
		END DO
	KTOT=NIV*5
	WRITE(18,REC=ID) NIV,TEFF,GLOG,ASALOG,ASALALF,BHE,
	1           TIRABS,TIRMOD,(A(K),K=1,KTOT)
	KTOT=NIV*5
	WRITE(85,*) NIV,TEFF,GLOG,ASALOG,ASALALF,BHE,
	1           TIRABS,TIRMOD,(A(K),K=1,KTOT)
	ID=ID+1 ! Linux f77
	WRITE(18,REC=ID)   KEP
C	ID=ID-1     !Linux f77
	write(34,*) 'ID = ',ID
        write(85,*)KEP,ID
5	WRITE(6,102)
12	RETURN
c
100	FORMAT(E15.4,F10.3,2E15.4,F10.3)
102	FORMAT('       FIN PRIMO4')
105	FORMAT(' TEFF=',F6.0,2X,'LOG G=',F5.2,2X,'[A/A0]=',F7.4,2X,
	1  '[alf/Fe]=',F6.2,'  NHE=',F6.3)
109	FORMAT(1X,5A4)
114	FORMAT('     Appel PRIMO4 avec  IDEF=',L2)
115	FORMAT('   ID=',I5,'  NIV=',I5)
	END


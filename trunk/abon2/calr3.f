C                        CALR3 /Janvier 1994
C       CONTIENT TOUS LES SOUSPROGRAMMES DE PGPLOT
C     ET les programmes de lecture des fichiers RETIFILE ou SPECFILE
c     et aussi qques programmes de RENOIR

      SUBROUTINE ALLOC(ICONS)
      INTEGER PGBEGIN
      CHARACTER*80 DEVICE
C
         write(6,*)' POUR INITIALISER L ECRAN, REPONDRE:'
         write(6,*)'  sur une station XWINDOW        /xwindow'
         write(6,*)'  sur un Mac (Versaterm)         /tek'
         write(6,*)'  pour une double console (SIO)  TX??/TEK'
3        read(5, 101) DEVICE
*         IF(PGBEGIN(0,DEVICE,1,1).NE.1)GO TO 3
C
      ICONS=1
 100  FORMAT(4F10.2)
 101  format(a80)
      RETURN
      END



      SUBROUTINE ALLOC4
      LOGICAL STD
      INTEGER PGBEGIN
      CHARACTER*80 DEVICE
      COMMON/COMVIS/IVIS,DEVICE
      COMMON/COMCADR/STD,XG,XD,YB,YH
      STD=.TRUE.
      write(6,*)'VISUALISATION SUR ECRAN          TAPER 1'
      write(6,*)'VISUALISATION VERSATEC           TAPER 2'
      write(6,*)'VISUALISATION impri LASER VAX    TAPER 3'
      write(6,*)'VISUALISATION impri POSTCRIPT    TAPER 4'
      READ(5,*)IVIS
C
         IF(IVIS.LE.1)   THEN                     !  Ecran
         write(6,*)' POUR INITIALISER L ECRAN, REPONDRE:'
         write(6,*)'  sur une station XWINDOW        /xwindow'
         write(6,*)'  sur un Mac (Versaterm)         /tek'
         write(6,*)'  pour une double console (SIO)  TX??/TEK'
3        read(5, 101) DEVICE
*         IF(PGBEGIN(0,DEVICE,1,1).NE.1)GO TO 3
C              IF (PGOPEN('?') .LE. 0)GO TO 3
         ELSE
            if (IVIS.EQ.2)  then
            write(6,*)'ENTREZ A LA SUITE LE NOM DU FICHIER VERSATEC'
            write(6,*)'EX: VISAJ.DAT/VERSATEC (landscape)'
            write(6,*)'ou  VISAJ.DAT/VVERSATEC (portrait)'
            else    !  Cas IVIS=3
            if(IVIS.EQ.3)  then
                  write(6,*)'ENTREZ A LA SUITE LE NOM DU FICHIER LASER'
                  write(6,*)'EX: VISAJ.DAT/LASER  (landscape)'
            write(6,*)'ou  VISAJ.DAT/VLASER  (portrait)'
            else    !  Cas IVIS=4
            write(6,*)'ENTREZ A LA SUITE LE NOM DU FICHIER POSTCRIPT'
                write(6,*)'EX: VISAJ.POS/PS      (landscape)'
                write(6,*)'ou  VISAJ.POS/VPS      (portrait)'
            end if
            end if
4        read(5, 101) DEVICE
*         IF(PGBEGIN(0,DEVICE,1,1).NE.1)GO TO 4
C
      write(6,*)' EPAISSEUR DU TRAIT ? (REPONDRE 1 OU 2 OU 3
     &  ETC...[NORMAL: 2]'
      READ(5,*)IEPAI
*      CALL PGSLW(IEPAI)
*      call pgscr(0,1.,1.,1.)
*      call pgscr(1,0.5,0.5,0.5)
      END IF
 100  FORMAT(4F10.2)
 101  format(a80)
      RETURN
      END




        subroutine CALCDC2(JEL,ELEM,IONI,ALAT,KIEX,vth,ABTOT,AAL,WTL)
c       On calcule la CdC passant par l'origine [ABTOT pts WTL versus AAL],
c       pour l'element ELEM de rang JL dans la liste des CdC a faire,
c       degre d'ionisation ioni,longueur d'onde ALAT, potentiel
c       d'excitation KIEX
      integer CAVA,D,DTOT,DDTOT,DIVTOT,AB,ABTOT
      real KB,NH,KIEX,KIES,KKI1,KKI2,M,KI1,KI2,MM,KII,KC,KAPPA,KAP,
     &       KA,LAMDA,LZERO,MU
      real pg,pe,teta,b,P,phi
      real xx,aal,wtl,w,f,flu,ecart,delt
      logical XPDISK,PTDISK
      character*2 EL,Q,QQ,ELEM
      character*1 IBL, ISI, ISS
      dimension P(50),ALPH(50),PHN(50),PH2(50),KC(50),VT(50),
     &            A(50),DELTA(50)
      dimension XX(60),AAL(60),WTL(60),W(60),F(50,60),KAPPA(60,50)
      dimension NH(50),TETA(50),PE(50),PG(50),KAP(50)
      dimension B(0:50)
      dimension DELTL(29),ECART(50),FLU(50)
        COMMON/COM6/TEFF,GLOG,ASALOG,asalalf,NHE,TIABS(5),TITRE(5)
C*********
C       Commons avec CALGAM
        COMMON/COMM1/GALOG(2,5,40,4),AKI(4),LAMDA(5)
        COMMON/ZUT/NTOT,NH,TETA,PE,PG,XPDISK,XMU,KIK
        COMMON/COM11/EL(40),TINI(40),PA(40),JKMAX(40),MM(40),
     &  KKI1(40),KKI2(40),NPAR,TABU(40,3,33)
C     COMMON/TOTO/TO
C*********
      COMMON/LECT1/AMET,BHE
C     DATA IBL/' '/
        DATA DELTL/5.,0.,2.,0.,1.,0.,0.5,0.,0.5,0.,0.40,0.,0.2,0.,
     &      0.2,0.,0.05,0.,.05,0.,.02,0.,.02,0.,.02,0.,.02,0.,.02/
        DATA  C/2.997929E+10/,H/6.6252E-27/,KB/1.38046E-16/,R/8.3170E+7/,
     &        PI/3.141593/,C1/4.8298E+15/,C2/8.8525E-13/,C4/2.1179E+8/,
     &        C6/3.76727E+11/,DEUXR/1.6634E+8/
C       C1=2.*(2*PI*KB*ME)**1.5/ H**3
C       C2=PI*E**2 / (ME*C**2)
C       C4=8.*R/PI
        C5= 2.*PI* (3.*PI**2/2.44)**0.4
C       C6=4.*PI*C
        C7=12398.54
      IBL=' '
C
C
c     write(6,*)' Entree dans CALCDC'
      DTOT=29
      DDTOT=15
      DIVTOT=31
      S=0.
         do D=1,DTOT,2
         S=S+DELTL(D)
         end do
      LZERO=ALAT-S
      NBLEND=1
      CALL BKF(NH,TETA,PE,PG,NTOT,ALAT,
     &           B,ALPH,PHN,PH2,KC,XPDISK,XMU,KIK,FC)
         Do N=1,NTOT
         VT(N)=VTH*1E5
         END DO
C     write(6,*)' FC CdC =', FC
C     write(6,*)' pour ALAT=', ALAT
C ******************************************************************
C        Calcul des quantites dependant de ALAT du modele de l'elem
C *********************************************
         DO I=1,NPAR
         II=I
         Q=EL(II)
         QQ=ELEM
         IF(Q.EQ.QQ) GO TO 412
         END DO
c       Inutile de faire test d'existence (fait dans CALGAM)
 412     KI1=KKI1(II)
         KI2=KKI2(II)
         write(6,151) II, KI1,KI2
         M=MM(II)
         KII=KI1
         IF(IONI.GT.1) KII=KI2
         ISI=IBL
         ISS=IBL
         KIES=KIEX + C7/ALAT
         CH=CALCH(KII,IONI,KIEX,ISI,KIES,ISS)
         CORCH=0.67*KIEX + 1
         CH=CH*CORCH
         CALL POPUL2(TETA,PE,NTOT,TINI,PA,II,
     &                 JKMAX,IONI,KI1,KI2,TABU,P)
C
      AL=1.E-8*ALAT
      GAMMAE=0.
      GAMMAR=2.21E15 / ALAT**2
      GF=1
      GGF=GF*C2*(AL**2)
C
        DO  N=1,NTOT
        T=5040./TETA(N)
        TOP=10.**(-KIEX   *TETA(N))
        TAP= 1.-ALPH(N)
        P(N)=P(N) * TOP *TAP
        DELTA(N)  = AL   /C*SQRT(VT(N)**2+DEUXR*T/M   )
        VREL      = SQRT ( C4 *T * (1.+1./M ))
        GAMMAH    = C5 * CH**0.4 * VREL**0.6
        IF(N.EQ.10)   GAH=GAMMAH
        GAMMA=GAMMAR+(GAMMAE*PE(N)+GAMMAH*(PHN(N)+1.0146*PH2(N)))/(KB*T)
C       ON A SUPPOSE  GAMMA(H2) / GAMMA(H) = 1.0146   CE QUI EST VRAI
C       POUR  CA1  4227    CE RAPPORT EST INSENSIBLE A L ATOME ET A
C       LA TRANSITION CONSIDEREE
        A(N)  = GAMMA * AL**2 /(C6*DELTA(N)  )
        END DO                                  !fin du do n=1,ntot
C
C       Calcul de l'abondance initiale
C       interpolation du Gamma pour la raie de la CdC
      GAMMACDC=GAMM(JEL,IONI,ALAT,KIEX)
      ALOGAB1=-7-GAMMACDC ! Valeur abondance pour W/l = -7

      ALOGAB2=ALOGAB1 + 6
C     write(6,*)' ALOGAB1=',ALOGAB1,'   ALOGAB2=',ALOGAB2
C
      XX(1)=10**ALOGAB1
      ABFIN=10**ALOGAB2
      PR=2
                DO  AB=2,60
                XX(AB)=XX(AB-1)*PR
                IF(XX(AB).GT.ABFIN)   GO TO 30
                END DO
 30   ABTOT=AB-1
C
        ECART(1) = S
                DO D=3,DIVTOT,2
                ECART(D)=ECART(D-2) - DELTL(D-2)
                ECART(D-1) = (ECART(D) + ECART(D-2) )  / 2
                END DO
                DO  D=1,DIVTOT
                        DO  N=1,NTOT
                        V= ABS (ECART(D)*1.E-08/DELTA(N)   )
                        AA=A(N)
                        DEL=DELTA(N)
C                 WRITE(6,*)'v del ', V,DEL
                        CALL HJENOR(AA,V,DEL,PHI)
C                        PHI=H(A,V)/( DELTA(J,N) *  PI**0.5)

                        KA    = PHI * P(N)  *GGF
C                 write(6,*)d,n,a(n),v, ka, p(n)
                                DO AB=1,ABTOT
                                KAPPA(AB,N) = KA *XX(AB)
                                END DO
                        END DO
C
                        DO  AB =1,ABTOT
                                DO  N=1,NTOT
                                KAP(N)  =KAPPA (AB,N)+KC(N)
                                END DO
                       CALL FLIN1(KAP,B,NH,NTOT,XPDISK,XMU,FFF,KIK,CAVA)
C                       IF(CAVA.NE.0)   STOP
                        F(D,AB)=1-FFF/FC
                  END DO
                END DO  ! fin du do D=1,DIVTOT
c           do ab=1,abtot,2
c              do n=1,ntot,5
c                 write(6,*)ab,n,kap(n)
c                 end do
c                 end do

C *********************************************************************
C
C           write(6,*)' Calcul des W ,  Bcle sur AB'
      write(6,155)
        DO  AB=1,ABTOT
      AAL(AB)=12+ALOG10(XX(AB))
           do D=1,DIVTOT
           FLU(D)=F(D,AB)
C          write(6,*)FLU(D)
           END DO
C          write(6,*),'aal(ab)',AAL(AB),'FLU(D)',FLU(D)
      CALL LARGQ(DIVTOT,DELTL,FLU,W(AB))
        WEX1=FLU(1) * (ALAT-LZERO)
        W(AB)=2000 * (W(AB)+WEX1)
        WTL(AB)    = -3 + ALOG10(W(AB)/ALAT)
      WRITE(6,159) AAL(AB),W(AB),WTL(AB),F(1,AB),WEX1,F(DIVTOT,AB)
c     WRITE(6,159) AAL(AB),W(AB),WTL(AB)
        END DO                          !fin du DO AB=1,ABTOT
c
c     type *,' CdC passant par origine colonne3/colonne1'
c     type *,'    AAL(AB)   W(AB)   WTL(AB)'
      CSTAJ=WTL(1)-AAL(1)
         Do AB=1,ABTOT
         AAL(AB)=AAL(AB)+CSTAJ
C        WRITE(6,159) AAL(AB),W(AB),WTL(AB)
         end do
C       La CdC passant par l'origine est  WTL versus AAL
         RETURN
C
 100     FORMAT(10F7.3)
 101     FORMAT('  ELEM=',A2)
 151     FORMAT('II=',I3,'   KI1=',F7.2,'  KI2=',F7.2)
 152     FORMAT(5F10.3)
 153     FORMAT(4f8.2)
 154     FORMAT(5f10.1)
 155     FORMAT(8X,'AB',6X,'W',5X,'log W/AB',9X,'R1',12X,'W ex',9X,'RC')
 159     FORMAT(F11.3,F8.2,F10.3,2E15.2,E14.4)
         END




      SUBROUTINE CAFCONV(GAUSS,PAS,AA,GRAPH,IFT,TT,FI)
C     ON CALCULE FI(TT) la fonction de convolution EN IFT PTS
C
C     LA FONCTION DE CONVOLUTION PEUT AVOIR 500 PTS
C      SI L ON CHANGE CE NBRE DE PTS CHANGER AUSSI IPPTOT=250
C      DANS LES DATA QUELQUES LIGNES PLUS BAS.
C      (LA MOITIE DU NBRE TOT DE PTS POUR LE CALCUL DES AT)
C
      LOGICAL GAUSS, GRAPH
      DIMENSION XFI(501),YFI(501),TT(501),FI(501),AT(-250:+250)
      IPPTOT=250
      C7=1.772453
C
            IF (GAUSS) THEN       ! PROFIL GAUSSIEN DE 1/2 LARG AA
      WRITE(6,119) AA
      TOTLARG=3.0 * AA
      AT(0)=0
            DO I=1,IPPTOT   ! IPPTOT TAILLE MAX DE LA FCT DE CONV
            AT(I)=PAS * I
            AT(-I)=-AT(I)
            IF(AT(I).GT.TOTLARG)   GO TO 40
            END DO
      BB=3*AA
      WRITE(6,133)BB
      STOP
40    IFD=I-1
      IFT= 1 + 2*IFD
            IF(IFT.GT.501)  THEN
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
            IF(.NOT.GRAPH)   THEN
            WRITE(6,118) (TT(I),FI(I) ,I=1,IFT )
            WRITE(6,115)
            END IF
C
                        ELSE ! ( NON GAUSSIEN)------------------
            IF (GRAPH) write(6,*)' ENTRER NBRE DE PTS DE FCTION FI'
            READ(5,*) IFTOT
            IF(IFTOT.GT.500)   THEN
            WRITE(6,138)
            STOP
            END IF
            DO N=1,IFTOT
            IF(GRAPH) write(6,*)' ENTRER LES IFTOT  ( XFI   YFI)'
            READ(5,*) XFI(N),YFI(N)
            END DO
            IF(.NOT.GRAPH)   THEN
            WRITE(6,118)   (XFI(N),YFI(N)  ,N=1,IFTOT)
            WRITE(6,115)
            END IF
C
      I=1
      DO WHILE (TT(I).LE.XFI(IFTOT))
      TT(I)=XFI(1) + PAS*(I-1)
      I=I+1
            IF(I.GT.500) THEN
            WRITE(6,138)
            WRITE(6,139)
            STOP
            END IF
      END DO
      IFT=I-1
      CALL FT2(IFTOT,XFI,YFI,IFT,TT,FI)
            IF(.NOT.GRAPH)   THEN
            WRITE(6,118) (TT(I),FI(I) ,I=1,IFT )
            WRITE(6,115)
            END IF
C
      II = IFT/2
      III= 2*II
            IF(IFT.LE.III)   THEN
            IFT=IFT+1
            FI(IFT) = 0.
            END IF  ! FIN DU IF(IFT<III)
      Z=0.
      IF1 = IFT-1
            DO  I=2,IF1
            Z = Z +2.*FI(I)
            END DO
      Z= Z+FI(1) + FI(IFT)
      Z = Z*0.5*PAS
C
                  END IF ! FIN DU IF GAUSS
C   ------------------------------------------------------------------
      WRITE(6,109)   Z
            DO  I=1,IFT
            FI(I) = FI(I) / Z
            END DO
            IF(.NOT.GRAPH)   THEN
            WRITE(6,115)
            WRITE(6,118) (TT(I),FI(I) ,I=1,IFT )
            WRITE(6,115)
            END IF
C
C
      RETURN
109   FORMAT(7E15.4)
115   FORMAT(1H )
118   FORMAT(4(2X,F7.3,F7.3) )
119   FORMAT(10X,'PROFIL INSTRUMENTAL GAUSSIEN  1/2 LARGEUR=',
     1  F7.3,'ANGSTROM')
133   FORMAT(' LE PROFIL GAUSSIEN PAR LEQUEL ON CONVOLE NE PEUT',
     1' AVOIR PLUS DE 121 PTS  (60 DE CHAQUE COTE DU CENTRE)',
     2/' CETTE GAUSSIENNE EST CALCULEE JUSQU A UNE DISTANCE DE',
     3' 3 DEMI-LARGEUR DU CENTRE',
     4/' SOIT:',F7.3,' ANGSTROM',
     4/'   ELARGISSEZ LE PAS DU CALCUL')
137   FORMAT(5X,'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER A ',
     1 /' PLUS DE 500 PTS -CHANGEZ LE PAS-. (LE NBRE DE PTS TOTAL '
     2 /' SUR LA FCTION S OBTIENT EN MULTIPLIANT PAR 6 LE NBRE '
     3 /' DE PTS SUR LA DEMI LARGEUR')
138   FORMAT( 5X, 'LA FCTION PAR LAQUELLE VOUS VOULEZ CONVOLER',
     1 ' A PLUS DE 500 PTS. C''EST TROP !')
139   FORMAT(5X,'-CHANGEZ LE PAS DU CALCUL !-')
      END




      SUBROUTINE CALCON(BLAM,VIT,DL,POI,ITOT,ANAL)
      DIMENSION CALF(20),SALF(21),ALF(21),SCALO(21),S(21),
     1     V(40),POI(40),DL(40)
      LOGICAL ANAL
      DATA CALF/.95,.90,.85,.80,.75,.70,.65,.60,.55,.50,
     1       .45,.40,.35,.30,.25,.20,.15,.10,.05,.00/
C      write(6,*)' Appel de CALCON'
           DO I=1,20
           ALF(I)=ACOS(CALF(I))
           SALF(I)=SIN(ALF(I))
           SCALO(I)=ALF(I) - (SALF(I)*CALF(I))
           END DO
C
           S(1)=SCALO(1)
           DO I=2,20
           S(I)=SCALO(I)-SCALO(I-1)
           END DO
C
           IF(ANAL) THEN
              write(6,*)' '
              write(6,*)'      ALF        SCALO        S'
              DO I=1,20
              WRITE(6,100) ALF(I),SCALO(I),S(I)
              END DO
          END IF
C
      DO J=1,19
      POI(J)=S(J)
      V(J)=-VIT*(CALF(J)+0.025)
      END DO
      POI(20)=S(20)
      V(20)=0
c
      DO J=21,39
      I=40-J
      POI(J)=S(I)
      V(J)=VIT*(CALF(I)+0.025)
      END DO
C
      DO J=1,39
      DL(J)=BLAM*V(J)/3E+5
      END DO
C
      ITOT=39
           IF (ANAL) THEN
           write(6,*)' '
           write(6,*)'  POI        V          DL'
           DO I=1,ITOT
           WRITE(6,100) POI(I),V(I),DL(I)
           END DO
           END IF
      RETURN
C
 100  FORMAT(5E12.3)
      END


      SUBROUTINE CALGAM(ELMAX,EELEM,IOO,ABSOL)
c       lit les modeles les fonctions de partition et
c     calcule les gamma pour 5 lambda et 4 kiex
      REAL  LAMBDA,KI1,KI2,KIEX,LAMBD,KKI1,KKI2,KKIEX,NH,NU,MU
      REAL   KAP,KB,KA,KC,M,NHE,tto,t5l,phn,ph2,p,pop,absol,abso
C     REAL   PG,PE,TO
      LOGICAL   PTDISK
      LOGICAL XPDISK
      INTEGER FIN,ELMAX,CAVA
        INTEGER IOO
      CHARACTER*2 EELEM,ELEM,EL
      DIMENSION TETA(50),PE(50),ALPH(50),TOTKAP(2),
     2 PG(50),KA(50),KAP(50),KC(50),TTO(50),T5L(50),
     3 NH(50),PHN(50),PH2(50),P(50),POP(50),
     4 EELEM(40),IOO(40),ABSOL(40),ABSO(40)
        DIMENSION ZZP0(30) ! entre  Lectur et abonio
        DIMENSION XNH(50),XPG(50),XTETA(50),XPE(50)
        DIMENSION  B(0:50), TO(0:50)


c       Common avec le SP d'absorption continue
      COMMON/LECT1/ AMET,BHE
      COMMON/TIT/TITABS(5)
c     Common avec READER
      COMMON/COM6/TEFF,GLOG,ASALOG,asalalf,NHE,TIABS(5),TITRE(5)
C     Common AVEC LE SP FLIN1
      COMMON/TOTO/TO
      COMMON/FAIL/ERR(50)
C     Common avec CALCDC2
        COMMON /COMM1/ GALOG(2,5,40,4),KKIEX(4),LAMBDA(5)
        COMMON/ZUT/NTOT,NH,TETA,PE,PG,XPDISK,XMU,KIK
        COMMON/COM11/EL(40),TINI(40),PA(40),JKMAX(40),M(40),
     &  KKI1(40),KKI2(40),NPAR,TABU(40,3,33)

c
C     write(6,*)'   entree dans CALGAM'
      KIK=0  ! FORMULE A 6 OU 7 PTS POUR CALCUL DU FLUX OU INT
      VVT = 1.6E+6
      ABOND =2.E-08
      LMAX=5
      KIMAX=4
      DATA LAMBDA/4000.,5000.,6000.,7000.,8000./
      DATA KKIEX/0,2.5,5,8.5/

      DATA C/2.997929E+10/,H/6.6252E-27/,KB/1.38046E-16/,C2/8.8525E-13/,
     1C1/4.8298E+15/
C
C     LECTURE DES FONCTIONS DE PARTITION
C
      write(6,*)'  Lecture des fonctions de partition'
      OPEN (UNIT=25,FILE='partit.dat',STATUS='OLD')
      DO J=1,40
      READ(25,108)EL(J),TINI(J),PA(J),JKMAX(J),M(J),
     1          KKI1(J),KKI2(J),ABSO(J),FIN
      IF(FIN.EQ.1) GO TO 203
      KMAX=JKMAX(J)
      READ(25,102) ((TABU(J,L,K),L=1,3),K=1,KMAX)
      END DO
203   NPAR=J-1
C
      write(6,*)' Lecture des donnees ABSORU'
      CALL LECTUR(ZZP0,ZZPH)
      write(6,*)' PTDISK,  Mu  ?'
      READ(5,*) PTDISK,MU
      write(6,*)' Modele ? (Teff, log G, [M/H], Ligne)'
      CALL READER5N(  NH,TETA,PE,PG,TTO,NTOT)
        write(6,*)'  ASALOG=',ASALOG, ' asalalf=',asalalf
      ASASOL=10**ASALOG
        COEFALF=10**asalalf
      WRITE(6,*)  '   TEFF, GLOG ',   TEFF, GLOG
      WRITE(6,101)   ASASOL, COEFALF
        call abonio(ZZP0,ZZPH,ASASOL,COEFALF)
      BHE= NHE
        write(6,*)'  AMET=', AMET,' BHE=',BHE
C
C      Mise en memoire pour CALCDC
         NTOX=NTOT
         DO N=1,NTOT
         XNH(N)=NH(N)
         XTETA(N)=TETA(N)
         XPE(N)=PE(N)
         XPG(N)=PG(N)
         END DO
         XPDISK=PTDISK
         XMU=MU
C
C     DEBUT DE LA BOUCLE
C
1001  WRITE(6,*)' Debut  de  la  bcle sur lambda'
      DO 666   L=1,LMAX
      ICOMP=0
      LAMBD= LAMBDA(L)
      AL=1.E-8 * LAMBD
c     write(6,*) ' L=',L,'  LAMBDA=',LAMBD
      CALL BKF(NH,TETA,PE,PG,NTOT,LAMBD,
     1      B,ALPH,PHN,PH2,KC,PTDISK,MU,KIK,FC)
      WRITE(6,118) LAMBD,FC
      GG2 = C2 * AL**2
      DELTA = AL*VVT/C
      PHI   = 1. / (DELTA *  1.77245)
      DO 666   JL=1,ELMAX
      ELEM = EELEM(JL)
            DO I=1,NPAR
            II=I
            IF(EL(II).EQ.ELEM) GO TO 405
            end do
            WRITE(6,117)ELEM
            STOP
405   KI2=KKI2(II)
      KI1 = KKI1(II)
        ABSOL(JL)=ABSO(II)
c     write(6,107) ELEM, KI1, KI2, ABSOL(JL)
c
      IONI=IOO(JL)
      call POPUL2(TETA,PE,NTOT,TINI,PA,II,
     1          JKMAX,IONI,KI1,KI2,TABU,P)
c     type *,' Population au niveau 5', P(5)
      DO 666  K=1,KIMAX
      III=0
      KIEX = KKIEX(K)
c     type *,'  JL=',JL,'   KIEX=', KIEX
            DO  N=1,NTOT
            POP(N)=P(N) * (1-ALPH(N)) * 10.**(-KIEX*TETA(N))
            END DO
c
 561        DO N=1,NTOT
            KAP(N) = PHI * POP(N) * GG2 * ABOND
            KA(N) = KAP(N) + KC(N)
c           if(N.EQ.5) write(6,1103) POP(N),KAP(N),KA(N)
            END DO
      CALL FLIN1 (KA,B,NH,NTOT,PTDISK,MU,FFF,KIK,CAVA)
      R = 1.-(FFF/FC)
c     write(6,1104) FC,FFF,R
c
c       -------------------------------------------------------------
c       Modification de l'abondance si R est trop petit ou trop grand
c       Apres cette modification on retourne en 561 refaire le calcul
          IF(  (R.LT. 1.E-4)  .OR.  (R .GT. 1.E-3)  ) then
          III=III+1
            if(III.GT.20) then
            write(6,137) ELEM, IONI, ABOND, R
            STOP
            end if ! fin du if (III>20)
            if (R.LT.1.E-6) then
            ABOND=ABOND*10
            GO TO 561
            end if ! fin du if (R < 1.E-6)
          RAPR=R/5.E-4
          ABOND=ABOND/RAPR
        GO TO 561
          END IF   ! fin de if  R en dehors de intervalle 1E-4 1E-3
c       -------------------------------------------------------------
c
c
      WCM = R * 1.77245 * DELTA
      GAMMA = WCM/(AL*ABOND)
      GALOG(IONI,L,JL,K) = ALOG10 (GAMMA)
c     WRITE(6,100)   L,JL,IONI,K,GALOG(IONI,L,JL,K)
 666  CONTINUE
C
C     FIN DE LA BOUCLE
C
      WRITE(6,115)
      WRITE(6,104)   TEFF,GLOG,ASALOG,BHE,AMET,(TITRE(I),I=1,5)
      WRITE(6,140)   (LAMBDA(L),L=1,LMAX)
      WRITE(6,141)   (KKIEX(K),K=1,KIMAX)
      WRITE(6,116)
      IF(PTDISK) WRITE(6,103) MU
      IF(.NOT.PTDISK)   WRITE(6,106)
c
            do JL=1,ELMAX
            write(6,112) EELEM(JL), IOO(JL)
            IONI=IOO(JL)
            write(6,109)((GALOG(IONI,L,JL,K),L=1,5),K=1,4)
            end do
      RETURN
c
c
100   FORMAT(20X,4I10,F10.3)
101   FORMAT('   A/ASOL=',E12.4,'   COEFALF=',E12.4)
102   FORMAT(13F6.4)
103   FORMAT(20X,' CALCUL EN  UN POINT DU DISQUE     MU=',F5.1)
104   FORMAT(2X,'TEFF=',F5.0,2X,'GLOG=',F5.2,2X,'[A/A0]=',F6.2,
     1  2X,'NHE=',F6.2,2X,'AMET=',E10.3,1X,5A4)
105   FORMAT(79X,I1)
106   FORMAT(20X,' CALCUL DES GAMMA POUR LE DISQUE INTEGRE')
107   FORMAT(5X,A2,3F10.3)
108   FORMAT(A2,2F5.2,I3,4F10.2,24X,I1)
109   FORMAT( (5(1X,F6.3),4X,5(1X,F6.3)))
110   FORMAT(2I5, L5,F5.0,I5,L5,2X,A3)
111   FORMAT(8F10.3)
112   FORMAT(5X,A2,I1)
113   FORMAT(26(1X,A2))
115   FORMAT(1H1)
116   FORMAT(1H )
117   FORMAT(8X,30HMANQUE FCTION DE PARTITION DE ,A2)
118   FORMAT(2X,F10.2,5X,'FC=',E15.4)
122   FORMAT(31H LE MODELE A PLUS DE 49 NIVEAUX)
130   FORMAT( ' ENNUI DANS FLIN1 CONTINU   CAVA=',I3)
131   FORMAT(10F8.3)
132   FORMAT( 5(I4,F7.3,F5.2) )
137   FORMAT(10X,8HBOUCLAGE,10X,A2,I1,5X,6HABOND=,E16.7,5X,2HR=,E16.7)
140   FORMAT(/' LISTE DES LAMBDA  ',5F10.0)
141   FORMAT(/' LISTE DES KIEX    ',4F10.1)
160   FORMAT(8E16.7)
1103  format('  P(5)=',E15.4,'   KAP(5)=',E15.4,'     KA(5)=',E15.4)
1104  format(' FC=',E15.4,'  FFF=',E15.4,'   R=',F12.5)
      END




      SUBROUTINE CALVIT(BLAMB,VIT,PA,IFT,TT,FI)
c     Calcul du profil d'une raies stellaire elargie par rotation
c
      dimension XFI(500), YFI(500), FI(1000),FI2(1000),
     1 TT(1000),TT2(1000)
      logical anal
      character*50 titre,spectheo
      ANAL=.FALSE.
C******************************************************************
C     CALCUL DE LA FONCTION DE CONVOLUTION
      WRITE(6,112)  BLAMB,VIT
      call CALCON(BLAMB,VIT,XFI,YFI,IFTOT,ANAL)
C
C      write(6,*)'   On revient dans CALVIT'
C     interpolation de FI
      ALIM=XFI(IFTOT)
C      write(6,*)'  TT(1)=',TT(1),'   ALIM=', ALIM
c
      TT2(1)=XFI(20)  ! le pt central TTT(1)=0
         do i=2,500
         TT2(I)=XFI(20) + PA*(I-1)
         if (TT2(I).GT.ALIM) GO TO 501
         end do
 501     IFTT=I-1  ! il y aura IFTT points de chaque cote
      if (ANAL) then
      write(6,*)' IFTT=', IFTT
      write(6,*) ' ecriture des TT2'
      write(6,108) (TT2(I),I=1,IFTT)
      write(6,*)' '
      end if
c
       IFT=2*(IFTT-1)+1
          do i=1,IFT
          TT(i)=-TT2(IFTT)+(I-1)*PA
          end do
c
      if (ANAL) then
      write(6,*)' IFT=', IFT
      write(6,*)'  ecriture des TT'
      write(6,108) (TT(I),I=1,IFT)
      write(6,*)' '
      end if
c
      call ft2(IFTOT,XFI,YFI,IFT,TT,FI)
      IFT=IFT+1
         do I=2,IFT
         TT2(I)=TT(I-1)
         FI2(I)=FI(I-1)
         end do
         TT2(1)=TT2(2)-PA
         IFT=IFT+1
         TT2(IFT)=TT2(IFT-1)+PA
         FI2(1)=0.
         FI2(IFT)=0.
c
            DO I=1,IFT
            TT(I)=TT2(I)
            FI(I)=FI2(I)
            END DO
c
         if (anal) then
         write(6,*)' Fonction YFI calculee par CALCON'
         write(6,107) (XFI(I),YFI(I),I=1,IFTOT)
         write(6,*)' '
         write(6,*)' Fonction FI interpolee   IFT=',IFT
         write(6,107) (TT(I),FI(I),I=1,IFT)
         end if
C
C     verification que IFT est impair
         II=IFT/2
         III=2*II
         ddiff=ABS(IFT-III)
         write(6,*)  '   ddiff=', ddiff
            If ( ABS(IFT-III).LT.0.1)then
            IFT=IFT+1
            TT(IFT)=TT(IFT-1)+PA
            FI(IFT)=0
               if(anal) then
               write(6,*)' On donne un nbre impair de pts a FI'
               write(6,107) (TT(I),FI(I),I=1,IFT)
               end if
            end if
C
C      Normalisation
      Z=0
      IF1=IFT-1
         Do I=2,IF1
         Z=Z+2*FI(I)
         END DO
      Z=Z+FI(1)+FI(IFT)
      Z=Z*0.5*PA
      IF(anal) write(6,106) Z
         DO I=1,IFT
         FI(I)=FI(I)/Z
         END DO
              IF(ANAL) then
              write(6,*)' Fonction Fi apres normalisation'
              write(6,107) (TT(I),FI(I),I=1,IFT)
              end if
C ******************************************************************
      RETURN
C
C
 100    Format(A50)
 101    Format(4(2X,2F7.3))
 106    Format (5E14.5)
 107    Format(4(2X,2F8.3))
 108    Format(6f10.3)
 112    FORMAT(' LONGUEUR D''ONDE DE LA RAIE',F8.2,10X,'VITESSE',F8.1,
     1  'km/s')
      END




      SUBROUTINE DEBUTFIN(IOPDEBF,EPSILON,DJ1,DTOT,TL,FL,
     1  KTOT,TTS,FFS,D2,K1,K2)
c     On calcule les indices K1 et K2 du spectre observe FFS entre
c     lesquels le spectre calcule FL "decolle" du continu (c'est a
c     dire s'ecarte du continu de plus de 0.001) .
      INTEGER D,DTOT,DJ1,D1,D2
      CHARACTER*1 CH
      DIMENSION TL(1001),FL(1001),TTS(1001),FFS(1001)
      write(6,*)' entree dans DEBUTFIN avec DJ1=', DJ1,'  iopdebf=',
     1         IOPDEBF
C     type 100,(TTS(K),K=1,KTOT)
C
C     ------------------option K1 et K2 automatiques----------------
        IF (IOPDEBF.EQ.0) then
      DO d=DJ1,dtot
      if( (1-fl(d)).gt.EPSILON)  go to 5
      end do
      D2=0   ! valeur impossible indiquant que le spectre est plat
      return
c
 5    D1=D
      do d=D1,DTOT
      IF( (1-fl(D)).lt.EPSILON)  go to 6
      end do
C
 6    D2=D
      end if ! fin de if iopdebf=0
c
C     ------------option K1 et K2 pointes sur ecran--------------
        IF (IOPDEBF.GE.1) then
         IF (IOPDEBF.EQ.1)then !    IOPDEF=1
           write(6,*)' Pointez K1 et K2'
*           call PGCURSE(XK1,YK,CH)
           write(6,*)'XK1=', XK1
*           call PGCURSE(XK2,YK,CH)
           write(6,*)'XK2=', XK2
         else !                     IOPDEF>1
         write(6,*)' Entrer XK1, XK2'
         read(5,*)XK1, XK2
         end if  !                  fin du IOPDEBF=1
c
        D=1
           Do while (XK1.gt.TL(D))
           D=D+1
           END DO
        D1=D
           Do while (TL(D).lt.XK2)
           D=D+1
           END DO
        D2=D-1
        write(6,*)'D1=',D1,'   D2=',D2
        END IF ! fin du if(IOPDEF>=1)
C     -----------------------------------------------------------
C
      TL1=TL(D1)
      TL2=TL(D2)
      DO K=1,KTOT
      IF(TTS(K).GT.TL1)   go to 7
      END DO
c 444 FORMAT(F8.3,2x,A2,I1,2x,f5.2,2x,f4.2,2x,f6.2)

 7    K1=K
      DO K=K1,KTOT
      IF(TTS(K).GT.TL2)   go to 8
      END DO
8     K2=K-1
      write(6,*) D1,D2,K1,K2
      return
 100  FORMAT(5 F10.3)
      end



      SUBROUTINE EXTRAI3(DESSIN,NR,JJ,ELJ,IOJ,ABJ,KTOT,
     &  XCAR,YCAR,JCAR,
     1                  XMUL,YMUL,JMUL,XPLU,YPLU,JPLU)
      LOGICAL DESSIN
      character*2 ELJ,ELEM
      Integer IOJ
      DIMENSION XCAR(500),YCAR(500),XMUL(500),YMUL(500),
     1      XPLU(500),YPLU(500)
      COMMON /COM2/ ALBDA(800),ELEM(800),MULT(800),WSL(800)
     &  ,AKIEX(800),
     1  XSOL(800),IONI(800),ABSI(800),X(800),Y(3,800),
     2  WST(800),WSO(800),GFL(800)
      KTOT=0
      K=0
      write(6,*)' Appel EXTRAI3'
      write(6,345)ELJ,IOJ
 345  FORMAT(2x,'element : ',A2,I1)
      write(6,*)' Nbre tot de raies dans la table ', NR
            DO   M=1,NR
            X(M)=0.
            Y(1,M) =0.
            Y(2,M) =0.
            Y(3,M) =0.
        write(6,444)ALBDA(M),ELEM(M),IONI(M),GFL(M),AKIEX(M),wst(M)
            END DO
 444  FORMAT(F8.3,2x,A2,I1,2x,f5.2,2x,f4.2,2x,f6.2)

C
            DO  I=1,NR
            IF( (ELEM(I).EQ.ELJ ) .AND. (IONI(I) .EQ.IOJ)) THEN
            K=K+1                      ! Numero de la raie ds la CdC
            XSOL(I)=ABJ+GFL(I)
            ABSI(I)=XSOL(I)+ GAMM(JJ,IONI(I),ALBDA(I),AKIEX(I))
C
                  IF(ABSI(I).GT. 100.)  THEN    ! CAS OU PAS DE GAMMA
                  DO  M=I,NR
                  IF( (ELEM(M).EQ.ELJ ) .AND. (IONI(M) .EQ.IOJ))
     1            WRITE(6,105) ALBDA(M)
                  END DO
                  GO TO 7               ! On passe a elem suivant
                  END IF
C
            WRITE(6,105)ALBDA(I),MULT(I),AKIEX(I),GFL(I),
     1           WST(I),ABSI(I),WSL(I)
C
                  IF (DESSIN)   THEN
                  X(K)   = ABSI(I)
                  Y(1,K) = WSL(I)
                  Y(2,K) = AKIEX(I)
                  Y(3,K) = ALBDA(I)
                  END IF
C
            END IF  ! FIN DE IF(ELEMENT=ELEMENT CONSIDERE)
            END DO ! FIN DE LA BCLE SUR LES RAIES
      KTOT=K

      IF(dessin) then
      JCAR=0
      JMUL=0
      JPLU=0
            DO K=1,KTOT
            IF(Y(2,K).LE.1.5) THEN
            JCAR=JCAR+1
            XCAR(JCAR)=X(K)
            YCAR(JCAR)=Y(1,K)
            ELSE
                    IF(Y(2,K).LE.3.0) THEN
                  JMUL=JMUL+1
                  XMUL(JMUL)=X(K)
                  YMUL(JMUL)=Y(1,K)
                  ELSE
                  JPLU=JPLU+1
                  XPLU(JPLU)=X(K)
                  YPLU(JPLU)=Y(1,K)
                  END IF
            END IF
            end do
      end if       !fin du if dessin
 7    RETURN
C
100   FORMAT(5X,F10.3,2X,A2,I1,8X,A2,I1)
105   FORMAT(1X,F10.2,5X,I4,5X,F5.2,5X,F5.2,4X,F6.1,
     1 5X,F5.2,5X,F5.2)
      END



      Function GAMM(J,IONIS,LBDA,KIEX)
C     Double interpolation parabolique dans la table des Gamma
C     en fonction de KIEX et de LBDA
c     On a GALOG(IONIS,ILAMDA,IELEM,IKIEX)
C
      REAL LBDA,KIEX,LAMDA,f1,f2,f3,g3,all
      INTEGER ELMAX
      DIMENSION F1(4),F2(4),F3(4),G(3),ALL(3)
C     COMMON avec la subroutine CALGAM
      COMMON /COMM1/ GALOG(2,5,40,4),AKI(4),LAMDA(5)
      KIMAX=4   !Nbre de KIEX
C     LMAX=5    !Nbre de Lambdas
C
c     type *,'  appel de GAMM'
c     type *,'  IONIS=', IONIS,'  LBDA=',LBDA,'   KIEX=',KIEX
c     write(6,109)((GALOG(IONIS,L,J,K),L=1,5),K=1,4)
C     On cherche entre quels lambdas de la table est le LBDA de la raie
C     On interpolera entre LAMDA(L1) , LAMDA(L1+1) et LAMDA(L1+2)
            IF(LBDA.LT.LAMDA(3)) THEN
            L1=1
            ELSE
                  if(LBDA.LT.LAMDA(4)) THEN
                  L1=2
                  ELSE
                  L1=3
                  END IF
            END IF
      L2=L1+1
      L3=L1+2
c     type *,' L1=',L1,'  L2=',L2,'  L3=',L3
C
C     Interpolation sur Kiex dans la table AKI pour le 3 lambdas
            DO K=1,KIMAX
            F1(K)= GALOG(IONIS,L1,J,K)
            F2(K)= GALOG(IONIS,L2,J,K)
            F3(K)= GALOG(IONIS,L3,J,K)
c           write(6,*) F1(K),F2(K),F3(K)
            END DO
c           type *,'  '
      G(1)=FT(KIEX,KIMAX,AKI,F1)
      G(2)=FT(KIEX,KIMAX,AKI,F2)
      G(3)=FT(KIEX,KIMAX,AKI,F3)
c     write(6,*) G(1),G(2),G(3)
      write(6,*)'  '
      ALL(1)=LAMDA(L1)
      ALL(2)=LAMDA(L2)
      ALL(3)=LAMDA(L3)
C
C     Interpolation sur Lambda dans la table des G
      GAMM=FT(LBDA,3,ALL,G)
c     type *, '   GAMM interpole =', GAMM
      RETURN
109   FORMAT( (5(1X,F6.3),4X,5(1X,F6.3)))
      END



      FUNCTION HIORNER(IX,NTOT,CC)
      DIMENSION CC(NTOT)
c     calcul de la valeur d 1 polynome ; methode de HORNER
c     les X du polynome sont des entiers
      S=CC(NTOT)
      DO N= (NTOT-1), 1, -1
      S= S*IX + CC(N)
      END DO
      HIORNER=S
      RETURN
      END



      SUBROUTINE LECSPE(INIT,ITYPE,ANOM,ILZERO,ITOT,PPHOT,
     1                 XX,SSS)
      parameter (JPI=4000)
c
      INTEGER*2  MIS,NFICH,SSS(JPI)
      DIMENSION SS(JPI),XX(JPI),C1(6),C2(6)
        CHARACTER*156 TI
      CHARACTER*40  TI40
      CHARACTER*45 ANOM
      LOGICAL OK
      common/MINFI/TI40,IDS,MIS,NFICH
c     write(6,*)' Appel Lecspe avec Init=', INIT,'  ITYPE=',ITYPE
      IF( INIT.eq.1 ) then
            IF(ITYPE.EQ.2)   THEN
            OPEN(UNIT=10,ACCESS='DIRECT',status='OLD',FILE=ANOM,
     1      RECL=8400)
            END IF
            IF(ITYPE.EQ.1)   THEN
            CALL ROWRET(0,10,ANOM,NB,IS,TI,MIS,NFICH,ITOT,
     1                  ICONS,FL0,C1,C2,VR,SSS)
            END IF
            IF(ITYPE.EQ.3)   THEN
            OPEN(UNIT=10,file=ANOM,status='old')
            END IF
c     write(6,*)'on revient au prog principal'
      RETURN
      END IF
c
 1    if(ITYPE.LT.3) then       !fichier acces direct
      WRITE(6,*)'ENTRER NUMERO LIGNE DS FICHIER  ' ,ANOM
      READ(5,*)IS
      IDS=IS
      end if
C
      IF(ITYPE.EQ.1) then   !********** fichiers RETIFILE ******
      CALL ROWRET(2,10,ANOM,NB,IS,TI,MIS,NFICH,ITOT,
     1                  ICONS,FL0,C1,C2,VR,SSS)
      TI40=TI
      WRITE(6,103) MIS, NFICH, VR,TI40
      II=ITOT/100
      IX0=II*50
      WRITE(6,*)  'ITOT=', ITOT, '   FL0=',   FL0
      write(6,104) (C1(I),I=1,ICONS)
      FI0=FL0-ILZERO
            DO I=1,ITOT
            J=I-IX0
            XX(I)=HIORNER(J,ICONS,C1)
            DECA= ((XX(I)+FL0)/300000) * VR
            XX(I) = XX(I) - DECA + FI0
            END DO
      DEBLVRAI=XX(1)+ILZERO
      FINVRAI =XX(ITOT)+ILZERO
      end if
c
        IF(ITYPE.EQ.2) then   !********** fichiers SPECTRES ******
      READ(10,rec=IS) TI,MIS,NFICH,PA,FL0,AL1,ITOT,SSS
      TI40=TI
      write(6,100) TI40
      PPHOT=0.
      deblvrai=fl0+al1
      finvrai=deblvrai+ITOT*PA
      debl=(FL0-ILZERO)+AL1
            do i=1,itot
            XX(I)=DEBL + (I-1)*PA
            END DO
      END IF
c
        IF(ITYPE.EQ.3) then   !********** fichiers TABLES ******
      read(10,100) TI40
      write(6,100) TI40
      read(10,*)ITOT,PX,PY,QX,QY
      write(6,*)' Nbre total de pts du spectre', ITOT
         do I=1,ITOT
         read(10,*) XX(I),SS(I)
         XX(I)=XX(I)+PX-ILZERO
         END DO
c     write(6,*)' Lecture des XX SS'
c     do I=1,10
c     write (6,104) XX(I),SS(I)
c     end do
c       write(6,*)' Rangement des XX SS'
      CALL RANGX(XX,SS,ITOT)
c     do I=1,10
c       write (6,104) XX(I),SS(I)
c       end do
c
      do I=1,ITOT
      SSS(I)=SS(I)*10000
      end do
      deblvrai=XX(1)+ILZERO
      finvrai=XX(ITOT)+ILZERO
      rewind 10
      END IF
c
      write(6, 101) DEBLVRAI, FINVRAI
                WRITE(6,*)' OK ?'
                READ(5,*)OK
                IF (.NOT. OK) GO TO 1
        write(6,*)'entrez la precision photometrique (RMS) de '
        write(6,*)'l''Observation (si 0 on ne calcule pas likelihood)'
        READ(5,*)PPHOT

      RETURN
100   FORMAT(1X,A40)
101   FORMAT(' 1er Lambda du spectre:',f10.3,'  Dernier',f10.3)
103   FORMAT(' MIS=',I5,' NFICH=',I5,5X,'VR=',F8.2,/2X, A40)
104   FORMAT(4E15.4)
      END



      SUBROUTINE NBPS(ALZERO,ZLZERO,ITOT,TS,KTOT,I1,I2)
      DIMENSION TS(4001)
C     calcul du nbre KTOT de pts d'un spectre  entre
C     ALZERO et ZLZERO . Les lambda du spectre sont ranges
C     dans TS , le spectre a ITOT pts au total.
C
      DO I=1,ITOT
      IF(TS(I).GE.ALZERO)   THEN
      I1=I
      GO TO 1
      END IF
      END DO
C
 1    DO I=I1,ITOT
      IF(TS(I).GT.ZLZERO) THEN
      I2=I-1
      GO TO 2
      END IF
      END DO
C
 2    KTOT=I2-I1+1
      RETURN
      END


      SUBROUTINE PGENV1(IVIS,XA,XZ,YA,YZ,XG,XD,YB,YH)
      if (IVIS.LE.1)then
c       les 7 ordres suiv = PGENV(XA,XZ,YA,YZ,0,0) avec fond couleur
*      CALL PGADVANCE
*      CALL PGVSTAND
*      CALL PGWINDOW(XA,XZ,YA,YZ)
*      CALL PGSCR(0,1.,1.,1.)
*      CALL PGSCR(1,0.,0.,0.)
*      CALL PGSCI(1)
*      CALL PGBOX('BCNST',0.0,0,'BCNST',0.0,0)
      end if
      if(IVIS.GE.2)then
*      CALL PGADVANCE
      write(6,*),' XG=',XG,' XD=',XD,'     YB=',YB,' YH=',YH
*      CALL PGVSIZE(XG,XD,YB,YH)
*      CALL PGWINDOW(XA,XZ,YA,YZ)
*      CALL PGBOX('BCNST',0.0,0,'BCNST',0.0,0)
      end if

      RETURN
      END

      SUBROUTINE PGENV2(ICONS,XA,XZ,YA,YZ)
*      CALL PGADVANCE
C     call PGSCR(0, 1., 1., 1.)
      IF(ICONS.EQ.1)   THEN
*      CALL PGVSTAND
                  ELSE
*      CALL PGVPORT(0.04,0.98,0.2,0.98)
      END IF
*      CALL PGWINDOW(XA,XZ,YA,YZ)
C     call PGSCR(1,0.,0.,0.)
C     call PGSCR(0,1.,1.,1.)
*      call PGSCI(1)
*      CALL PGBOX('BCNST',0.0,0,'BCNST',0.0,0)
      RETURN
      END




      subroutine ROWRET(INDEX,IUNIT,NOMFICH,NB,NLD,TIT,MISS,NFICH,ITOPIX,
     1                  ICONS,Z0,C1,C2,VR,S)
        PARAMETER  (JPI=1030)
c****REMARQUES
c     -Dans le programme principal la dimension de NOMFICH doit etre
c     identique a celle declaree dans ROWRET. (Sinon concatenations bizarres)
c     -Si le fichier est cree en ecriture, il faut definir le nbre de blocs
c     en dehors du sous programme (le nbre de pix ne suffit pas)
c****
      CHARACTER*156 TIT
      CHARACTER*35 TIA
      CHARACTER*45 NOMFICH
      CHARACTER*15 CHVR,CHZ0,CHC1(6),CHC2(6)
      INTEGER*2 MISS,NFICH,S(JPI)
      DIMENSION C1(6), C2(6)
C       NLD est le numero du spectre dans le fichier
C     LD  est la position du premier bloc du spectre
C
c     INDEX=0 et 1 on fait un OPEN (si 0 vieux fich, 1 nveau fich)
c     INDEX=2 on lit
c     INDEX=3 on ecrit
      IF (INDEX.LT.2) then
            if (INDEX.EQ.1) then
            OPEN(UNIT=IUNIT,ACCESS='DIRECT',status='NEW',FILE=NOMFICH,
     1       RECL=512)
            else
            OPEN(UNIT=IUNIT,ACCESS='DIRECT',status='OLD',FILE=NOMFICH,
     1       RECL=512)
            ID=1 !lecture du nbre blocs  NB
            READ(IUNIT,rec=ID)TIT,MISS,NFICH,NB,ITOPIX,ICONS,
     1           CHZ0,CHC1,CHC2,CHVR
            NPOINMAX=(NB-1)*256
      write(6,*)' chaque spectre devra avoir au plus',NPOINMAX,' pixels'
            end if
      RETURN
      END IF
C
      LD=(NLD-1)*NB + 1
      ID=LD
      IF (INDEX.EQ.2) THEN  ! lecture
      READ(IUNIT,rec=ID,ERR=1)TIT,MISS,NFICH,NB,ITOPIX,ICONS,
     1    CHZ0,CHC1,CHC2,CHVR
      IF(ITOPIX.LT.1) GO TO 1
      NB1=NB-1
C     decode(15,100,CHZ0)Z0
C     decode(15,100,CHVR)VR
      write(CHZ0,100)Z0
      write(CHVR,100)VR

            do I=1,6
               write(CHC1(I),100)C1(I)
               write(CHC2(I),100)C2(I)
c           decode(15,100,CHC1(I))C1(I)
c           decode(15,100,CHC2(I))C2(I)
            end do
c
            DO N=1,NB1
            I1=(N-1)*256 + 1
            I2=I1+255
            IF(N.EQ.NB1) I2=ITOPIX
            READ(IUNIT,rec=ID) (S(I),I=I1,I2)
            END DO
      RETURN
      ELSE   ! ecriture   (INDEX=3)
C     encode(15,100,CHZ0)Z0
C     encode(15,100,CHVR)VR
      write(CHZ0,100)Z0
      write(CHVR,100)VR
            do I=1,6
C           encode(15,100,CHC1(I)) C1(I)
C           encode(15,100,CHC2(I)) C2(I)
               write(CHC1(I),100)C1(I)
                   write(CHC2(I),100)C2(I)
            end do
      write(IUNIT,rec=ID)TIT,MISS,NFICH,NB,ITOPIX,ICONS,
     1     CHZ0,CHC1,CHC2,CHVR
      NB1=NB-1
c
            DO N=1,NB1
            I1=(N-1)*256 + 1
            I2=I1+255
            IF(N.EQ.NB1) I2=ITOPIX
            write(IUNIT,rec=ID) (S(I),I=I1,I2)
            END DO
      RETURN
      END IF
 1    write(6,*)' Fin de fichier '
      STOP
 100  format(E15.7)
      END

        SUBROUTINE SPDECAL(DECAL)
        CHARACTER*1 CH
        COMMON /COM7/PA,IP,KTOT,TTS(1001),FS(1001),FFS(1001),VERSOB,GRAPH
        WRITE(6,*)' POINTEZ 1 PIXEL PUIS L ENDROIT OU IL DEVRAIT ETRE'
*        CALL PGCURSE(AL1,Y,CH)
*        CALL PGCURSE(AL2,Y,CH)
        DECAL=AL2-AL1
                DO K=1,KTOT
                TTS(K)=TTS(K)+DECAL
                END DO
        RETURN
        END


      SUBROUTINE SPDECAL2(DECAL)
      CHARACTER*1 CH
      COMMON /COM7/PA,IP,KTOT,TTS(1001),FS(1001),FFS(1001),VERSOB,GRAPH
      WRITE(6,*)' POINTEZ 1 PIXEL PUIS L ENDROIT OU IL DEVRAIT ETRE'
            DO K=1,KTOT
            TTS(K)=TTS(K)+DECAL
            END DO
      RETURN
      END



      SUBROUTINE VISU1(IOP,ICONS,PPHOT,RESM,
     1  DTOT,TL,FL,ITOT,TS,SSS,EPSILON)
C     ICONS : INDICE DE LA TAILLE DE LA FENETRE DE VISUALISATION
C     IOP=1 premier appel; on trace les spectre observe et calcule
C     IOP=2 on trace observe et calcule comme iop=1 mais on ne
C           ne refait pas les calculs relatifs au spectre observe
C     IOP=3 on ne trace qu 1 spectre calcule
C     IOP=4 on recommence un calcul d'erreur avec nouvel epsilon
      LOGICAL OK
      INTEGER*2  SSS(4000)
      INTEGER    D,DTOT,DD,DJ1
      REAL*4 TL(1001),FL(DTOT),AFS(1001),DELF(1001),TS(4001),
     1      XC(2),YC(2),LIKELIHOOD,
     2      YHPTS(50),YBPTS(50)  !dimens sur NBLEND
      CHARACTER*4 SFMIN
      CHARACTER*1 CH
      CHARACTER*2 NUMRAI
c     common avec le prog principal et le sous prog PECPLO
      COMMON /COM7/PA,IP,KTOT,TTS(1001),FS(1001),FFS(1001),VERSOB,GRAPH
c       common avec le prog princ pour reperage des raies en trace
        COMMON/COM8/IOPDEBF,NBLEND,NUMRAI(50),TLR(50)
C                 ********************
      IF (IOP.EQ.1) THEN
      CALL NBPS(TL(1),TL(DTOT),ITOT,TS,KTOT,I1,I2)
C     Nbre KTOT de pts entre lzero lfin; Le 1er a l'ind I1 le dern I2
      IP=1
C     SI IP=1 ON VISUALISE TS LES PTS DU SPECTRE OBSERVE
C     SI IP=2 "      "     1 PT SUR 2 ETC...
            IF(KTOT.GT.300)   THEN
            IP= 0.5+ KTOT/250.
            WRITE(6,*)' LE SPECTRE OBSERVE REPRESENTE ',KTOT,' PTS'
            WRITE(6,*)' ON VISUALISE 1 PT SUR ',IP
            END IF    ! FIN DE IF KTOT GT 300
C
            K=0
            DO I=I1,I2,IP
            K=K+1
            FS(K)=SSS(I)/10000.
            FFS(K)=FS(K)
            TTS(K)=TS(I)
            END DO
      KTOT=K
      write(6, 101) KTOT,I1,I2
      END IF          ! FIN DU IF IOP=1
C                 ********************
 3    IF(IOP.LT.3)   THEN
C     calcul du minimum (avec arrondi a 0.1) de 2 spectres
C     ranges dans FFS et FL
C     vrai minimium FMIN    minimum arrondi FMIN1
      JJS=IINF(FFS,1001,1,KTOT)   !OBSERVE
      JJT=IINF(FL,DTOT,1,DTOT)    !THEORIQUE
      FMIN=AMIN1(FFS(JJS),FL(JJT))
      FMIN1=FMIN*10
      IMIN=FMIN1
      FMIN1=IMIN/10.
c
      CALL PGENV2(ICONS,TL(1),TL(DTOT),FMIN1,1.1)
C
C           TRACE DU SPECTRE OBSERVE
            IF(KTOT.LT.200) THEN
            ISYMB=2
            ELSE
            ISYMB=1
            END IF
*            CALL PGPOINT(KTOT,TTS,FFS,ISYMB)
C     TRACE DU CONTINU SUR LE SPECTRE OBSERVE
      XC(1)=TTS(1)
      XC(2)=TTS(KTOT)
      YC(1)=1
      YC(2)=1
*      CALL PGLINE(2,XC,YC)
      END IF   ! FIN DU IF  IOP<3
C
C                 ********************
            IF (IOP.EQ.1)   THEN
C           ON NE RECENTRE LE CONTINU QUE LA PREMIERE FOIS
            WRITE(6,*)' CONTINU OK ?'
            READ(5,*)OK
            IF(OK)   GO TO 4
            WRITE(6,*)' POINTEZ LE NOUVEAU CONTINU'
*            CALL PGCURSE(X,FC,CH)
                  DO K=1,KTOT
                  FFS(K)=FFS(K)/FC
                  END DO
            GO TO 3
            END IF ! fin du IOP=1
C                 ********************
C
C                 TRACE DU SPECTRE CALCULE
4     CONTINUE
* 4    IF(IOP.LT.4) CALL PGLINE (DTOT,TL,FL) !iop=4 seulement calcul erreur
      DDY=(1.1-FMIN1)/20
      YNUM= FMIN1 + 1.2*DDY
      do K=1,nblend
      YBPTS(K)=FMIN1+DDY
      YHPTS(K)=1 + DDY
      end do
*      CALL PGPOINT(NBLEND,TLR,YBPTS,30)
*      CALL PGPOINT(NBLEND,TLR,YHPTS,31)
            DO K=1,NBLEND
*            CALL PGPTEXT(TLR(K),YNUM,90.,0.,NUMRAI(K))
            END DO
C
C                 CALCUL D'ERREUR
C     (calcul du domaine ou le spec observe "decolle" du continu)
      IF (PPHOT .LT. 0.00001) RETURN ! Pas de calcul d'erreur
      DJ1=1
c
 5    CALL DEBUTFIN(IOPDEBF,EPSILON,DJ1,DTOT,TL,FL,KTOT,TTS,AFS,
     1                DD,K1,K2)
      IF (DD .EQ. 0) return
      SRES=0.
      KKK=0
            DO K=K1,K2
            TSK=TTS(K)
              IF ((TSK.LT.TL(1)).OR.(TSK.GT.TL(DTOT))) THEN !hors limite
              DELF(K)=0
                                                       ELSE !cas normal
              AFS(K)=FT(TTS(K),DTOT,TL,FL)
              DELF(K)=FFS(K) - AFS(K)
              KKK=KKK+1
              END IF
            SRES=SRES+DELF(K)**2
            END DO
C
      RESM= sqrt(SRES / KKK)
      WRITE(6,104)TTS(K1),TTS(K2),RESM
      LIKELIHOOD=EXP(-0.5*SRES/(PPHOT**2))
      WRITE(6,106)LIKELIHOOD, KKK
c     DJ1=DD
c     GO TO 5
      return
c
101   FORMAT(' nbre de pts visualises ',I6,' entre i1=',I6,
     1'  et i2=' ,I6)
104   FORMAT(' Residu moyen entre lambda ',F7.3,' et',F7.3,5X,E10.4)
105   FORMAT(4F10.3)
106   FORMAT(' VRAISEMBLANCE ds ces limites ',E15.8,
     &         '     Nbre total de pts=',I3)
      END

















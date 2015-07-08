C     Sous-programmes a linker avec HYDRO et ABSORU /Janvier 1994
C

      SUBROUTINE AMERU (TETA,CLOG,PPG,NH,LAMB,PDS,
     1                 X,IMAX,JMAX,AL,BPL,TAU,IX)
C     CALCUL DE LA PROFONDEUR OPTIQUE SELECTIVE TAU PAR INTEGRATION
C     EXPONENTIELLE.TETA=5040./T,CLOG=LOG10PE,NH=VARIABLE DE PROFONDEUR
C     DANS LE MODELE,LAMB=LONGUEUR D'ONDE
C
      REAL*4 NH,LAMB
      DIMENSION P(0:99), TAU(50,0:99), BPL(0:99)
      DIMENSION TETA(99),CLOG(99),AL(50,99),NH(99),DNH(99),PPG(99)
      DIMENSION TET(7),ALP(9),TAB(9,7)
      DIMENSION Y(99)
C
C     TAB=LOG 10(U), U=FONCTION DE PARTITION DE H
      DATA ALP/5.,4.,3.,2.,1.,0.,-1.,-2.,-3./,MMAX,NMAX/9,7/,TET/0.1,0.2
     1,0.3,0.4,0.5,0.6,0.7/
      DATA TAB/1.43,1.83,2.28,2.77,3.27,3.77,4.27,4.77,5.27,
     1         0.47,0.62,0.91,1.31,1.78,2.26,2.75,3.25,3.76,
     2         0.31,0.32,0.35,0.42,0.61,0.93,1.35,1.82,2.32,
     3         0.30,0.30,0.30,0.31,0.32,0.35,0.44,0.65,0.99,
     4         0.30,0.30,0.30,0.30,0.30,0.30,0.31,0.32,0.36,
     5         18*0.30/
      DATA CTE,C1/3.972257E+8,2.8546E+4/
C
C     CALCUL DU COEFFICIENT D'ABSORPTION PAR NOYAU DE H
C
      IF(IX.EQ.2) GO TO 4000
C  IMPRESSIONS SUPPLEMENTAIRES
C***************************
      WRITE(6,*)'1'
      WRITE(6,*)'      COEFFICIENTS D ABSORPTION PAR NOYAU D''H'
      WRITE(6,116)LAMB,JMAX
C***************************
4000  DO 1000 I=1,IMAX
      PE=EXP(2.302585*CLOG(I))
      TEMP=2.5*ALOG(5040.39/TETA(I))
      ZKH=EXP(-31.30364 *TETA(I)+TEMP-1.098794 )
      F1=1./(1.+ZKH/PE)
      F2=EXP(-C1*TETA(I)/LAMB)
      F3=1.-F2
      IF((TETA(I).GE.0.7).OR.(CLOG(I).LE.-3.)) GO TO 5
      UV=PIPE(TETA(I),CLOG(I),TET,ALP,TAB,NMAX,MMAX)
      U=EXP(2.302585 *UV)
      GO TO 6
5     U=2.
6     F=PDS*F1   *F3/U*10.**(-X*TETA(I))
      BPL(I)=CTE*F2/(F3*LAMB**3)
            DO J=1,JMAX
            AL(J,I)=F*AL(J,I)
            END DO
      IF(IX.EQ.2) GO TO 1000
C***************************
C     IMPRESSIONS SUPPLEMENTAIRES
      WRITE(6,110)I,F,U
      WRITE(6,112)(AL(I,J),J=1,JMAX)
C***************************
1000  CONTINUE
C
      write(6,*)' Calcul de la fonction de Plank au niveau zero'
      TET0=FTETA0(PPG,TETA)   ! on extrapole TETA pour NH=0
      F20=EXP(-C1*TET0/LAMB)
      F30=1-F20
      BPL(0)=CTE*F20/(F30*LAMB**3)
C
C     CALCUL DE TAU
      P(0)=0
      DO  J=1,JMAX
                  DO I=1,IMAX
                  Y(I)=AL(J,I)
                  end do
      P(1)=NH(1)*(Y(1)-(Y(2)-Y(1)) / (NH(2)-NH(1))*NH(1)/2.)
      CALL INTEGRA(NH,Y,P,IMAX,P(1))
                  DO I=0,IMAX
                  TAU(J,I)=P(I)*1.E-24
                  END DO
      END DO
      WRITE(6,*)'   Sortie de AMERU'
      RETURN
110   FORMAT(3X,'I= ',I4,3X,'N2/NHTOT=',1P,E13.5,3X,'U=',1P,E13.5)
112   FORMAT(5E15.7)
116   FORMAT(3X,'LAMBDA=  ',F10.3,10X,'JMAX=  ',I5)
      END


      SUBROUTINE CONF(X,Y,JMAX,RES,TAB,AB)
      INTEGER*2 IX
C     PRODUIT DE CONVOLUTION EFFECTUE PAR GAUSS HERMITE (N=2)
C     ***
      DIMENSION TAB(50),AB(50)
      DATA RPI,V,H/1.772454, .7071068, .8862269/
      RES=0.
      Q=1.
      IX=1
2002  ARG=X-V*Y*Q
      AVU=ABS(ARG)
      IF(AVU.GT.AB (JMAX)) GO TO 10
      TO=FT(AVU,JMAX,AB,TAB)
      RES=RES+TO*H
      GO TO (2003,2004),IX
2003  Q=-1.
      IX=2
      GO TO 2002
2004  RES=RES/RPI
      GO TO 11
10    WRITE(6,1)
1     FORMAT(6X,12H  IMPOSSIBLE)
11    RETURN
      END


      SUBROUTINE CONV2(X,Y,JM,IQM,IJ,RES,T,AB)
      INTEGER*2 IR
C     PRODUIT DE CONVOLUTION POUR LE CAS OU LE PROFIL STARK VARIE PLUS
C     VITE QUE LE PROFIL DOPPLER. INTEGRATION PAR SIMPSON
C     ***
      DIMENSION IJ(10),F(50),AB(50),H(10),V(50),T(50),AC(50)
      DATA RPI/1.772454/
      BOL=X/Y
      H(1)=AB(2)
      DO 10I=2,IQM
      I1=IJ(I-1)
10    H(I)=AB(I1+1)-AB(I1)
      DO 11 N=1,JM
11    V(N)=AB(N)/Y
      Q=1.
      IR=1
40    DO 15 N=1,JM
      AC(N)=-(BOL-Q*V(N))**2
15    F(N)=T(N)*EXP(AC(N))
      SOM=0.
      DO 12 I=1,IQM
      IF(I.GT.1)GO TO 20
      K1=1
      K2=IJ(1)
      GO TO 21
20    K1=IJ(I-1)
      K2=IJ(I)
21    K3=K1+1
      K4=K2-3
      SIG=F(K1)+F(K2)+4.*F(K2-1)
      IF(K4.LT.K3) GO TO 16
      DO 13 K=K3,K4,2
13    SIG=SIG+4.*F(K)+2.*F(K+1)
16    S=SIG*H(I)/3.
12    SOM=SOM+S
      GO TO(60,61),IR
60    S1=SOM
      Q=-1.
      IR=2
      GO TO 40
61    S2=SOM
      RES=(S1+S2)/(RPI*Y)
      RETURN
      END


      SUBROUTINE CONV4(X,Y,TAB,JMAX)
C     CONVOLUTION AVEC H%A,V<,INTEGRATION PAR SIMPSON
      DIMENSION N(6),PAS(6),VAR(220),F1(220),PHI(300),V(300)
      DIMENSION X(50),TAB(50),AC(220),PHIT(220)
      COMMON/MODIF/VAR,F1,V,PHI,IH,II
      DATA N/11,21,23,171,191,211/
      DATA PAS/0.001,0.01,0.045,0.1,0.25,0.5/
      EPSI=1.E-06
      DO 1017 KK=1,JMAX
      BOL=X(KK)/Y
      IR=1
      Q=1.
      QY=Q/Y
25    RES=0.
      RESG=1.
      DO 50 I=1,IH
      AC(I)=ABS(BOL-QY*VAR(I))
 50   CONTINUE
      CALL FT2_hydro2(II,V,PHI,IH,AC,PHIT)
      I=1
      K=1
      SOM=F1(1)*PHIT(1)
      DO 10 I=2,IH,2
      FF4=4*F1(I)*PHIT(I)
      J=I+1
      IF((I+1).EQ.N(K))GO TO 15
      FF2=2*F1(I+1)*PHIT(I+1)
      SOM=SOM+FF4+FF2
      GO TO 10
15    FF1=F1(I+1)*PHIT(I+1)
      SOM=SOM+FF4+FF1
      RES=RES+SOM*PAS(K)/3
      IF(ABS(1.-RES/RESG).LT.EPSI)GO TO 45
      SOM=FF1
      RESG=RES
      K=K+1
10    CONTINUE
45    GO TO (20,35),IR
20    IF(X(KK).EQ.0.)GO TO 30
      QY=-QY
      RES1=RES
      IR=2
      GO TO 25
35    RES=RES1+RES
      TAB(KK)=RES
      GO TO 1017
30    RES=2.*RES
      TAB(KK)=RES
 1017 CONTINUE
      RETURN
      END


      SUBROUTINE FLUXIS (T1,T2,B,modeles_ntot,PTDISK,MU,JM,F,FC,IOP)
C     CALCUL DU FLUX SORTANT OU DE L'INTENSITE A MU DONNE
C     ***
      REAL MU
      LOGICAL PTDISK
      DIMENSION T1(50,0:99),T2(0:99),B(0:99),T(0:99)
      DIMENSION F(50),CC(26),TT(26),TTA(6),CCA(6),TTB(26),CCB(26),
     1         TTP(7),CCP(7)
      DATA CCA/0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
      DATA TTA /0.038,0.154,0.335,0.793,1.467,3.890 /
      DATA CCP/0.176273,0.153405,0.167016,0.135428,0.210244,0.107848,
     1 0.049787/
      DATA TTP/0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
      DATA CCB/0.032517,0.111077,0.071279,0.154237,0.076944,0.143783,
     10.063174,0.108330,0.038767,0.059794,0.021983,0.034293,0.012815,
     20.020169,0.007616,0.012060,0.004595,0.007308,0.002802,0.004473,
     30.001724,0.002761,0.001578,0.002757,0.000396,0.002768/
      DATA TTB/0.,0.05,0.1,0.20,0.30,0.45,0.60,0.80,1.,1.2,1.4,1.6,1.8,
     12.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
            IF(PTDISK) THEN
            IPOINT=7
                  DO I=1,IPOINT
                  CC(I)=CCP(I)
                  TT(I)=TTP(I)*MU
                  END DO
            ELSE
                  IF(IOP.EQ.0) THEN
                  IPOINT=6
                        DO I=1,IPOINT
                        CC(I)=CCA(I)
                        TT(I)=TTA(I)
                        END DO
                  ELSE
                  IPOINT=26
                        DO I=1,IPOINT
                        CC(I)=CCB(I)
                        TT(I)=TTB(I)
                        END DO
                  END IF
            END IF
      TOLIM=TT(IPOINT)
      IF(T2(modeles_ntot).LT.TOLIM) THEN
      WRITE(6,104)
      WRITE(6,103) modeles_ntot,T2(modeles_ntot)
      WRITE(6,101)
      END IF
C
      FC=0
      DO K=1,IPOINT
      BBC=FAITK30(TT(K),T2,B,modeles_ntot)
      FC=FC+CC(K)*BBC
      END DO
C
      DO J=1,JM
      FL=0
            DO I=0,modeles_ntot
            T(I)=T1(J,I)+T2(I)
            END DO
C
            DO K=1,IPOINT
            BB=FAITK30(TT(K),T,B,modeles_ntot)
            FL=FL+CC(K)*BB
            END DO
      F(J)=FL
      END DO
      RETURN
101   FORMAT(1H//)
103   FORMAT(I10,5X,'TO=',F10.4)
104   FORMAT(' Modele trop court ')
      END


      SUBROUTINE HJEN(A,VH,DEL,PHI,IW)
C      CALCUL DE LA FONCTION DE HJERTING = H(A,V)
        DIMENSION H11(41),H12(43), V1(43),V2(43)
      DIMENSION VH(300),PHI(300)
      RPI=SQRT(3.14159)
      DATA H11/-1.128380,-1.105960,-1.040480,-0.937030,-0.803460,
     1         -0.649450,-0.485520,-0.321920,-0.167720,-0.030120,
     2         +0.085940,+0.177890, 0.245370, 0.289810, 0.313940,
     3          0.321300, 0.315730, 0.300940, 0.280270, 0.256480,
     4          0.231726, 0.207528, 0.184882, 0.164341, 0.146128,
     5          0.130236, 0.116515, 0.104739, 0.094653, 0.086005,
     6          0.078565, 0.072129, 0.066526, 0.061615, 0.057281,
     7          0.053430, 0.049988, 0.046894, 0.044098, 0.041561,
     8          0.039250/
      DATA H12/0.0440980,0.0392500,0.0351950,0.0317620,0.0288240,
     1         0.0262880,0.0240810,0.0221460,0.0204410,0.0189290,
     2         0.0175820,0.0163750,0.0152910,0.0143120,0.0134260,
     3         0.0126200,0.0118860,0.0112145,0.0105990,0.0100332,
     4         0.0095119,0.0090306,0.0085852,0.0081722,0.0077885,
     5         0.0074314,0.0070985,0.0067875,0.0064967,0.0062243,
     6         0.0059688,0.0057287,0.0055030,0.0052903,0.0050898,
     7         0.0049006,0.0047217,0.0045526,0.0043924,0.0042405,
     8         0.0040964,0.0039595,0.0038308/,RPI/1.772454/
C SI V>3.9 LE CALCUL DE EXP(-V**2) EST INUTILE
      DO 100 K=1,IW
      V=VH(K)
      IF(V-12)1,1,2
    2 W=1./(2*V**2)
      H1=2.*W*(1.+W*(3.+15.*W*(1.+7.*W)))
      H1=H1/RPI
      GO TO 10
    1 V1(1)=0.
      V2(1)=03.8
      DO 5 I=1,42
      V1(I+1)=V1(I)+0.1
      V2(I+1)=V2(I)+0.2
    5 CONTINUE
      IF(V-3.9)3,3,4
    4 H1=FT(V,43,V2,H12)
   10 PHI(K)         =(A*H1)/(RPI*DEL)
      GO TO 100
    3 H1=FT(V,41,V1,H11)
      PHI(K)         =(EXP(-V**2)+A*H1)/(RPI*DEL)
 100  CONTINUE
      RETURN
      END


      SUBROUTINE INAIT(X,Y,P,ERR,N)
C  CE PROGRAMME INTEGRE NUMERIQUEMENT UNE FONCTION RELLE, DONNEE PAR
C  UN TABLEAU DE VALEURS X(I),Y(I). LA METHODE CONSISTE A CALCULER
C  L' INTEGRALE DEFINIE SUR CHAQUE INTERVALLE (X(I),X(I+1)) D'ABORD
C  PAR SIMPSON,PUIS PAR GAUSS A DEUX POINTS. LES VALEURS DE Y AU
C  POINT MILIEU ET AU DEUX POINTS DE GAUSS SONT CALCULES PAR INTER-
C  POLATION CUBIQUE A QUATRE POINTS EN UTILISANT LES VALEURS DE LA
C  TABLE.ON FORME ENSUITE UNE MOYENNE PONDEREE DES DEUX RESULTATS
C  QUI ANNULE L'ERREUR DU QUATRIEME (ET AUUSI DU CINQUIEME PAR RAI-
C  SON DE PARITE) ORDRE.L' ERREUR SUR L'INTEGRATION EST DONC
C  GENERALEMENT NEGLIGEABLE PAR RAPPORT A L'ERREUR SUR L'INTERPOLA-
C  TION DU TROSIEME ORDRE.CETTE DERNIERE EST EVALUEE EN COMPARANT
C  LA VALEUR INTERPOLEE SUR LA FONCTION ELLE MEME ET SUR LA VALEUR
C  INTERPOLEE EN PASSANT PAR SON LOGARITHME.IL FAUT QUE LA FONCTION
C  SOIT POSITIVE PAR NATURE POUR QUE LA TRANSFORMATION LOGARITHMI-
C  QUE SOIT POSSIBLE.
      DIMENSION X(99),Y(99),ALY(200),P(99),ERR(99)
      COMMON /GENE/ALY,XMILIEU,XGAUSS1,XGAUSS2,DEL,YM,
     1  YG1,YG2,CONST,I,J
      P(1)=0.
      CONST=1./SQRT(3.)
       DO I=1,N
       ALY(I)=ALOG(Y(I))
       END DO
      I=1
      J=1
      CALL STEP(X,Y,P,ERR,N)
      N2=N-2
        DO I=2,N2
        J=I-1
      CALL STEP(X,Y,P,ERR,N)
      K=I+1
        END DO
      I=N-1
      J=N-3
      CALL STEP(X,Y,P,ERR,N)
      END


      SUBROUTINE INPOL1(X,XA,DX,N,YT,Y)
C     INTERPOLATION DANS UNE TABLE A PAS CONSTANT
      DIMENSION YT(1)
      I=1
1     IF(X-XA-DX*FLOAT(I-1))4,3,2
2     IF(I.GE.N) GO TO 5
      I=I+1
      GO TO 1
3     Y=YT(I)
      RETURN
4     IF(I.EQ.1) I=2
5     IF(I.GE.N) I=N-1
      A=X-XA-DX*FLOAT(I-2)
      B=A-DX
      C=B-DX
      Y=(B*(A*YT(I+1) + C*YT(I-1))/2. - A*C*YT(I) )/DX**2
      RETURN
      END

      SUBROUTINE MALT(TH,U,BETA,GAM,T)
      DIMENSION U(20),TH(20),ASM(150)
      DATA PI/3.141593/,IBORN/20/
      T1=TA(1.,BETA,GAM)
      T2=TA(-1.,BETA,GAM)
      VU=0.
      SIGMA=0.
      AVU=VU
      TO=FT(AVU,IBORN,U,TH)
      ASO=AS(GAM,BETA,0.,TO)
      PP=BETA+GAM
      IF(PP-20.)48,47,47
47    H1=PP/100.
      H2=0.
      GO TO 606
48    IF(PP-15.)50,49,49
49    IH1=100
      IH2=10
      GO TO 55
50    IF(PP-10.)52,51,51
51    IH1=76
      IH2=20
      GO TO 55
52    IF(PP-5.)54,53,53
53    IH1=50
      IH2=30
      GO TO 55
54    IH1=30
      IH2=40
55    H1=PP/IH1
      H2=(20.-PP)/IH2
606   AH=H1
      IN=1
      IM=IH1-1
      IR=IH1
59    DO 56 I=IN,IR
      VU=VU+AH
      AVU=ABS(VU)
      TO=FT(AVU,IBORN,U,TH)
56    ASM(I)=AS(GAM,BETA,VU,TO)
      DO 57 I=IN,IM,2
      SIG=(1.333333*ASM(I)+0.6666667*ASM(I+1))*ABS(AH)
57    SIGMA=SIG+SIGMA
      IF(IN-1)81,81,58
81    IF(PP-20.)60,58,58
60    SIGMA=ASM(IR)*(H2-H1)/3.+SIGMA
      AH=H2
      IN=IH1+1
      IM=IH2+IH1-1
      IR=IM+1
      GO TO 59
58    SIGMA=SIGMA-ASM(IR)*ABS(AH)/3.
      IF(AH)63,62,62
62    SIGMA1=SIGMA
      VU=0.
      SIGMA=0.
      IM=40
      IN=2
      IR=IM+1
      AH=-0.5
      GO TO 59
63    TB=(SIGMA+SIGMA1+ASO*(H1+0.5)/3.)/PI
      T=T1+T2+TB
      RETURN
      END


      SUBROUTINE OPTIC1(KAP,NH,modeles_ntot,TO)
C     CALCUL DE LA PROFONDEUR OPTIQUE TO.FORMULES VOIR THESE CAYREL.
C     NH VARIABLE DE PROFONDEUR DANS LE MODELE.KAP COEFFT D'ABSORPTION
C     PAR NOYAU D'HYDROGENE.
C     ***
      REAL   NH,KAP
      DIMENSION TO(0:99)
      DIMENSION NH(99),KAP(99)
      TO(0)=0.
      TO(1)=NH(1)*(KAP(1) - (KAP(2)-KAP(1))/(NH(2)-NH(1))*NH(1)/2.)
      call INTEGRA(NH,KAP,TO,modeles_ntot,TO(1))
      RETURN
      END


      SUBROUTINE PRONOR(JM,IQM,IJ,F,AB,Z,AZ)
C     NORMALISATION DU PROFIL DE STARK,INTEGRATION PAR SIMPSON
C     ***
      DIMENSION IJ(10),AB(50),H(10),F(50)
      H(1)=AB(2)
      DO10I=2,IQM
      I1=IJ(I-1)
10    H(I)=AB(I1+1)-AB(I1)
      SOM=0.
      DO 12 I=1,IQM
      IF(I.GT.1)GO TO 20
      K1=1
      K2=IJ(1)
      GO TO 21
20    K1=IJ(I-1)
      K2=IJ(I)
21    K3=K1+1
      K4=K2-3
      SIG=F(K1)+F(K2)+4.*F(K2-1)
      IF(K4.LT.K3) GO TO 16
      DO 13 K=K3,K4,2
13    SIG=SIG+4.*F(K)+2.*F(K+1)
16    S=SIG*H(I)/3.
12    SOM=SOM+S
      ANOR=2.*SOM
      AZ=Z/ANOR
      RETURN
      END


      SUBROUTINE RAIEHU(LL,NA,NB,NBMIN,NBMAX,CLAM,C1,C,DLAM,L,J1,IND,config_kq)
C
C     L EST LE COEFFICIENT D'ABSORPTION PAR PARTICULE ABSORBANTE
C     SI IND=0,ON ECRIT LE DETAIL DES APPROX. PAR LESQUELLES L EST CALCU
C     SI IND=1, PAS D'ECRITURE
C     LL=1,CALCUL EN UN POINT SPECIFIE PAR SA LONGUEUR D'ONDE
C     SI DANS CE CAS,J1=2 ON SOMME LES COEFF. D'ABSORPTION DE PLUSIEURS
C     RAIES D'UNE MEME SERIE
C     LL=2,CALCUL EN UN OU PLUSIEURS POINTS SPECIFIES PAR DELTA LAMBDA
C     SI DANS CE CAS,J1=1,LA CONVOLUTION FINALE EST SAUTEE.
C     config_kq=1, CALCUL D UN PROFIL PUREMEMT QUASISTATIQUE POUR IONS + ELETRO
C
      LOGICAL config_amores,STARK
      REAL*4 L,LIM,LAC,LV
      REAL*8 CLAM,XDP(50),XBDP
      DIMENSION F1(220),VAR(220),V(300),PHI(300),RESC(50)
      DIMENSION BIDON(50,99)
      DIMENSION L(50,99),C(20),DLAM(50),ALFA(50),LV(50),AL(
     150),VX(50),STOC(99),AZ(99),VAL(5),T1(20,5),T2(20,5),RES(20),U(20),
     2Q(5),V1(20),V2(20),V3(20),V4(20),V5(20),R1(20),R2(20),R3(20),R4(20
     3),R5(20),X(50),HYD(99)
      COMMON /D2/VT(99),CMU,TETA(99),CLOG(99),NZ,IMAX,JMAX,IQM,IJ(10)
      COMMON/D3/HYD,AZ,config_amores,STARK
C
C     T1 PROFIL QUASISTAT. MOYEN POUR H ALPHA ET H BETA (DISTR. DU CHAMP
C     ELEC. DE MOZER ET BARANGER) T1= CAB*S(ALPHA)
C     T2 ID POUR LES AUTRES RAIES
C
      COMMON/MODIF/VAR,F1,V,PHI,IH,IW
      EQUIVALENCE(V1(1),T1(1,1)),(V2(1),T1(1,2)),(V3(1),T1(1,3)),(V4(1),
     1T1(1,4)),(V5(1),T1(1,5))
      EQUIVALENCE(R1(1),T2(1,1)),(R2(1),T2(1,2)),(R3(1),T2(1,3)),(R4(1),
     1T2(1,4)),(R5(1),T2(1,5))
      DATA U/0.,0.5,1.,1.5,2.,2.5,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,14.,1
     16.,18.,20./
      DATA V1/0.11133,0.11133,0.11000,0.10384,0.09049,0.07881,0.06708,0.
     104860,0.03363,0.02326,0.01573,0.01089,0.00775,0.00571,0.00433,0.00
     2336,0.00215,0.00147,0.00106,0.00079/
      DATA V2/0.12401,0.12267,0.11849,0.10935,0.09736,0.08100,0.06584,0.
     104503,0.02941,0.01943,0.01313,0.00930,0.00681,0.00511,0.00388,0.00
     2308,0.00203,0.00141,0.00103,0.00078/
      DATA V3/0.13908,0.13753,0.13130,0.11699,0.09511,0.07911,0.06285,0.
     103965,0.02501,0.01653,0.01144,0.00828,0.00614,0.00466,0.00362,0.00
     2291,0.00194,0.00138,0.00102,0.00077/
      DATA V4/0.17067,0.16488,0.15037,0.11669,0.09705,0.07365,0.05465,0.
     103289,0.02076,0.01371,0.00971,0.00703,0.00521,0.00405,0.00314,0.00
     2255,0.00173,0.00132,0.00091,0.00070/
      DATA V5/0.18045,0.17633,0.16263,0.12921,0.09578,0.06703,0.04913,0.
     102893,0.01783,0.01192,0.00844,0.00618,0.00469,0.00364,0.00289,0.00
     2235,0.00163,0.00118,0.00078,0.00068/
      DATA R1/0.09671,0.09586,0.09454,0.09029,0.08330,0.07596,0.06854,0.
     105219,0.03848,0.02789,0.01944,0.01344,0.00967,0.00718,0.00544,0.00
     2420,0.00267,0.00181,0.00131,0.00099/
      DATA R2/0.10882,0.10815,0.10757,0.10048,0.09052,0.07975,0.06709,0.
     104805,0.03324,0.02258,0.01592,0.01127,0.00820,0.00619,0.00481,0.00
     2377,0.00246,0.00170,0.00126,0.00098/
      DATA R3/0.12412,0.12267,0.11988,0.11054,0.09409,0.07826,0.06435,0.
     104301,0.02841,0.01921,0.01361,0.00974,0.00722,0.00541,0.00430,0.00
     2342,0.00228,0.00160,0.00120,0.00094/
      DATA R4/0.14759,0.14595,0.13621,0.11587,0.09529,0.07484,0.05809,0.
     103665,0.02367,0.01619,0.01133,0.00840,0.00630,0.00489,0.00384,0.00
     2305,0.00206,0.00149,0.00111,0.00085/
      DATA R5/0.17122,0.16737,0.14580,0.11845,0.09197,0.07139,0.05415,0.
     103188,0.02012,0.01361,0.00985,0.00731,0.00555,0.00430,0.00342,0.00
     2278,0.00192,0.00139,0.00104,0.00081/
      DATA VAL/0.0,0.2,0.4,0.6,0.8/,CK,PI,R,CKP,CL,CMH/1.38E-16,3.141593
     1,109708.3,6.9952E-13,2.9978E10,1.673E-24/
      ACTE=2.**0.6666667
      BCTE=2.**0.1666667
      DCTE=2.*CK/(CMH*CMU)
      ECTE=1.5127E-27
      AR=NA
      ZR=NZ
      Z2=ZR**2
      Z3=Z2*ZR
      Z5=Z3*Z2
      ZDEM=ZR**2.5
      RZR=R*ZR
      A2=AR**2
      GO TO (1,2),LL
C
1     RAD=CLAM*RZR*1.0E-08
      S=AR*SQRT(RAD/(RAD-A2))
      N=INT(S)
C     N=JINT(S)
      N2=N+1
      DO 70 J=N,N2
      RJ=J
      RJ2=RJ**2
      X(J)=RJ2*A2*1.0E+8/(RZR*(RJ2-A2))
      XDP(J)=DBLE(X(J))
      DLAM(J)=DABS(CLAM-XDP(J))
70    CONTINUE
      IF(DLAM(N+1)-DLAM(N))11,12,12
11    NB1=N+1
      GO TO 13
12    NB1=N
13    IF(J1.EQ.2) GO TO 14
      WRITE(6,1064) DLAM(J)
      GO TO 15
C
C     CALCUL D'UN BLEND
14    DO 55 I=1,IMAX
55    STOC(I)=0.
      IF((NBMAX.EQ.0).AND.(NBMIN.EQ.0)) GO TO 15
43    K1=NBMAX-NBMIN+1
      IF(NBMIN.EQ.NB1)IK1=3
      IF((NBMAX.GT.NB1).AND.(NBMIN.LT.NB1))IK1=2
      IF((NBMAX.EQ.NB1).AND.(NBMIN.LT.NB1)) IK1=1
C     K1=NBRE DE RAIES CONTRIBUANT AU BLEND, K= INDICE DE COMPTAGE
C     IK1=1 BLEND AVEC RAIES MOINS ELEVEES DANS LA SERIE
C     IK1=2 BLEND AVEC RAIES DE PART ET D'AUTRE DE LA RAIE CONSIDEREE
C     IK1=3 BLEND AVEC RAIES PLUS  ELEVEES DANS LA SERIE
15    K=1
      IBOU=1
      NB=NB1
C
C     REPRISE DE LA SEQUENCE NORMALE
16    BR=NB
      B2=BR**2
      XB=B2*A2*1.0E+8/(R*(B2-A2))
      XBDP=DBLE(XB)
      DELTA=DABS(CLAM-XBDP)
      C1=C(NB)
      DLAM(1)=DELTA
      CL2=XB**2
      GO TO 80
C
2     BR=NB
      B2=BR**2
      CL2=CLAM**2
C     CALCUL DE QUANTITES DEPENDANT DE A,B,Z
80    F=B2-A2
      B2A2=B2*A2
      BR4=B2**2
      G=BR4*BR+A2**2*AR
      CAB=5.5E-5*B2A2**2/(Z5*F)
      CT=1.496*CAB**1.5
      IF((NA.EQ.1).AND.(NB.EQ.2))CT=3.4E-6
      IF((NA.EQ.1).AND.(NB.EQ.3))CT=1.78E-5
      IF((NA.EQ.2).AND.(NB.EQ.3))CT=1.3E-3
      IF((NA.EQ.2).AND.(NB.EQ.4))CT=3.57E-3
      IF((NA.EQ.2).AND.(NB.EQ.5))CT=6.0E-3
      IF((NA.EQ.2).AND.(NB.EQ.6))CT=9.81E-3
C     CT DE GRIEM,KOLB,SHEN,NRL REPORT 5455
      BIC=G/(ZR*F)
      HO=SQRT(F)
      DIN=B2A2/HO
      DAN=G/(B2A2*HO)
C CALCUL DE QUANTITES NECESSAIRES A CONV4 INDEPENDANTES DU MODELE ET DU
C PROFIL
      PAS=0.001
      VAR(1)=0.
      ID=2
      I4=11
      ICAL=0
260   DO 250 I=ID,I4
      VAR(I)=VAR(I-1)+PAS
250   CONTINUE
      ICAL=ICAL+1
      ID=I4+1
      GO TO(200,201,202,203,204,205),ICAL
200   I4=21
      PAS=0.01
      GO TO 260
201   I4=23
      PAS=0.045
      GO TO 260
202   I4=171
      PAS=0.10
      GO TO 260
203   I4=191
      PAS=0.25
      GO TO 260
204   I4=211
      PAS=0.5
      GO TO 260
C
C     CALCUL DE QUANTITES DEPENDANT DU MODELE
205   IH=I4
      WRITE(6,*)'VOUS ENTREZ DANS LA BOUCLE LA PLUS EXTERIEURE QUI
     1 PORTE SUR L INDICE DU NIVEAU DU MODELE'
      DO 17 I=1,IMAX
      write(6,*)'      CALCUL AU NIVEAU',I,' DU MODELE'
      T=5040.39/TETA(I)
      CNE=TETA(I)*EXP(2.3026*CLOG(I))/CKP
      CAM=CNE**0.6666667
      CAM1=CNE**0.1666667
      FO=1.2532E-9*CAM
      FAC=1.0E8*C1/FO
      ALFAD=CLAM*SQRT(DCTE*T+VT(I)*VT(I))/(FO*CL)
      DLD=ALFAD*FO
      AZ(I)=ECTE*HYD(I)*CL2/(ALFAD*FO)
      DDOP=0.8325*ALFAD
      RSURL=0.0898*CAM1*T**(-0.5)
      KP=0
      DELIE=4.736E-06*CL2/(BR*(BR-1.)*TETA(I))
      DELOM=ZR*CL2*0.3503192 E-4/(B2*TETA(I))
      DELP=2.9953E-15*CL2*CNE**0.5
      LIM=1.23E-18*CL2*B2*CAM
      DD1=ALOG10(DELOM/DELP)
      IF(config_kq.EQ.1) GO TO 24
C     QUANTITES UTILES UNIQUEMENT POUR ELARGISSEMENT IMPACT
C     SIGNIFICATION DE CES QUANTITES  VOIR ARTICLES DE GRIEM ET DE MOZER
      CAM2=CNE**0.3333333
      TDEG=1.39E4/CAM1
      TIMP=2.1E10*T/CAM
      TDENS=1.277E5*T**0.5/CAM2
      ENS=B2-TDENS
      RNT=4.6*(Z3/T)**0.5*ALOG10(4.0E6*T*ZR/(B2*CNE**0.5))*DAN
      GAM1=1.217E-6*RNT*DIN*CAM2/ZDEM
      BOM=DELOM/(FO*CAB)
      GAMS=1.5*PI/(BOM**0.5)
      DMOIN=1./SQRT(DELOM)
      DD=DD1*2.305855
      CEG=B2-TDEG
      ALDP=DDOP
      GO TO 120
24    ALDP=DDOP/BCTE
      IF(IND.EQ.1) GO TO 94
      WRITE(6,121)
      WRITE (6,122) RSURL,FO,ALFAD,CNE,DELIE,DELOM,DELP,LIM,I
      GO TO 94
120   IF(IND.EQ.1) GO TO 94
      WRITE(6,99)
      WRITE(6,1042)DELOM,DELP,DELIE,RSURL,RNT,FO,LIM,ALFAD,CNE,I
C
94    IF(LL-1)19,19,20
19    JMAX=1
C     CALCUL AUX DIFFERENTS POINTS DU PROFIL
      WRITE(6,*)' VOUS ENTEZ DANS LA BOUCLE INTERIEURE QUI PORTE
     1 SUR L INDICE DE POINT DU PROFIL'
20    DO 1010 J=1,JMAX
      ALFA(J)=DLAM(J)/FO
      BETA=ALFA(J)/CAB
      IZ=1
      IF(ALFA(J).LT.1.E-07)GO TO 23
      S1=CT/ALFA(J)**2.5
      IF ((DLAM(J).EQ.0.).OR.(config_kq.EQ.1)) GO TO 23
      RAC=DLAM(J)**0.5
      CORR=ALOG(DELOM/DLAM(J))/DD
23    IF(DLAM(J)-LIM)21,22,22
C
C     FORMULES ASYMPTOTIQUES
22    IF(IND.EQ.1) GO TO 95
      WRITE(6,1009)
95    AFI=1.
110   IF(config_kq.EQ.1) GO TO 1060
      IF(BR4.LT.TIMP) GO TO 1061
1060  IF(IND.EQ.1) GO TO 100
      WRITE(6,1014)
C     TESTS POUR TRAITEMENT QUASISTATIQUE
100   IF(DLAM(J)-DELOM) 3004,3001,3001
1012  ALFA(J)=ALFA(J)/ACTE
2011  IF(IND.EQ.1) GO TO 2015
      WRITE (6,115)
2015  CI=2.*S1
      GO TO 27
2014  IF(IND.EQ.1) GO TO 2013
      WRITE (6,1015)
2013  CI=S1
      GO TO 27
C     IMPACT VALABLE SELON HVR (CR 259,3979,1964)
1061  IF(IND.EQ.1) GO TO 96
      WRITE(6,1016)
96    IF((CEG.GE.0.).AND.(ENS.GE.0.)) GO TO 1018
      IF(IND.EQ.1) GO TO 97
      WRITE(6,1022)
97    IF(DLAM(J)-DELOM) 2010,2010,1012
2010  GO TO(52,53),IZ
52    IF(DLAM(J)-DELP)1019,1020,1020
1020  IF(IND.EQ.1) GO TO 81
      WRITE(6,1021)
81    CI= S1*(1.+(DMOIN+RNT*CORR)*RAC)
      GO TO 27
1019  IF(IND.EQ.1) GO TO 82
      WRITE(6,1023)
82    CI= S1*(1.+(DMOIN+RNT)*RAC)*AFI
      GO TO 27
1018  IF(IND.EQ.1) GO TO 100
      WRITE(6,1024)
      GO TO 100
C
3001  ALFA(J)=ALFA(J)/ACTE
      QBETA=BETA/ACTE
      IF(QBETA.GT.20.) GO TO 2011
      QRSURL=RSURL*BCTE
      RVAR=QRSURL
      IG=1
      GO TO 3003
3004  IF(DLAM(J)-DELP) 3002,3005,3005
3002  QBETA=BETA
      IF(QBETA.GT.20.) GO TO 2014
      RVAR=RSURL
      IG=3
      GO TO 3003
3005  CORR1=2.+ALOG10(DLAM(J)/DELOM)/DD1
C     NE=NE*CORR1(SELON SCHLUTER ET AVILA APJ 144,785,1966)
      CORR2=CORR1**0.6666667
      ALFA(J)=ALFA(J)/CORR2
      QBETA=BETA/CORR2
      IF(QBETA.GT.20.) GO TO 2015
      RVAR=RSURL*CORR1**0.1666667
      IG=2
3003  IY=1
      GO TO 1026
C
C     FORMULES NON ASYMPTOTIQUES
21    IZ=2
      IF(config_kq.EQ.1) GO TO 100
      GO TO 110
53    IF(NA.EQ.3) GO TO 84
      IF(DLAM(J)-DELP)1028,1029,1029
1029  IF(IND.EQ.1) GO TO 83
      WRITE (6,1021)
83    GAM=GAM1*CORR+GAMS
      GO TO 1031
1028  IF(IND.EQ.1) GO TO 84
      WRITE(6,1032)
84    GAM=GAM1+GAMS
1031  IF(BETA-15.)1034,1033,1033
1033  IF(IND.EQ.1) GO TO 2012
      WRITE(6,1035)
2012  AFI=LIM/DLAM(J)
      GO TO 52
1034  IF (IND.EQ.1) GO TO 86
      WRITE(6,1037)
86    IF(GAM-10.)1039,1038,1038
1038  IF (IND.EQ.1) GO TO 87
      WRITE(6,1040)GAM
87    T=GAM/(PI*(BETA**2+GAM**2))
      GO TO 1036
1039  IF (IND.EQ.1) GO TO 88
      WRITE(6,1041)GAM
C
C     INTERPOLATION DANS T1 OU T2 POUR CONSTRUCTION DU PROFIL QUASISTAT.
C     (VARIABLE RSURL)
88    IY=2
      RVAR=RSURL
1026  IF(RVAR .GT.0.8)GO TO 1025
      IF(KP.EQ.1) GO TO 18
      KP=1
      DO 90 IL=1,20
      IF((NA.EQ.2).AND.((NB.EQ.3).OR.(NB.EQ.4))) GO TO 92
      DO 91 M=1,5
91    Q(M)=T2(IL,M)
      GO TO 90
92    DO 93 M=1,5
93    Q(M)=T1(IL,M)
90    RES(IL)=FT(RVAR,5,VAL,Q)
18    GO TO(1090,1091),IY
C     CALCUL DE T(BETA,GAM)
1091  CALL MALT(RES,U,BETA,GAM,T)
      GO TO 1036
C     INTERPOLATION DANS TABLE DU PROFIL QUASIST. (VARIABLE BETA)
1090  TV=FT(QBETA,20,U,RES)
      GO TO(6001,6002,6003),IG
6001  T=TV/ACTE
      GO TO 1094
6002  T=TV/CORR2
      GO TO 1094
6003  T=TV
1094  IF(IND.EQ.1) GO TO 1036
      WRITE(6,1092)
C
1036  CI=T/CAB
27    L(J,I)=FAC*CI
      IF(IND.EQ.1) GO TO 1010
C     WRITE (6,1043) J,DLAM(J),ALFA(J),BETA,L(J,I)
1010  CONTINUE
      IF((LL.EQ.1).OR.(J1.EQ.1)) GO TO 17
C
C     CALCUL DE LA DEMI LARGEUR STARK DS
      DO 1013 J=1,JMAX
      LV(J)=L(J,I)
      L1=JMAX-J+1
      VX(L1)=ALFA(J)
1013  AL(L1)=ALOG10(LV(J))
      IF(.NOT.config_amores) GO TO 4013
      CALL PRONOR(JMAX,IQM,IJ,LV,ALFA,FAC,AX)
      DO 4006 J=1,JMAX
      LV(J)=LV(J)*AX
4006  BIDON(J,I)=LV(J)
      IF(IND.EQ.0)WRITE(6,4004)AX
4004  FORMAT(/,6X,' AX=',1PE13.5)
4013  LAC=L(1,I)/2.
      TRUC=ALOG10(LAC)
      J=2
1053  IF(LAC-L(J,I))1050,1051,1052
1050  J=J+1
      GO TO 1053
1051  DS=ALFA(J)
      GO TO 1057
1052  IF(J.EQ.2) GO TO 6000
      DS=FT(TRUC,JMAX,AL,VX)
      GO TO 1057
6000  DS=ALFA(2)+(ALFA(2)-ALFA(1))*(TRUC-AL(JMAX-1))/(AL(JMAX-1)-AL(JMAX
     1))
1057  IF(.NOT.STARK) GO TO 71
      IF(IND.EQ.0)   WRITE(6,1054)DS
C
C     CONVOLUTION DES PROFILS STARK ET DOPPLER
      IF(IND.EQ.1) GO TO 71
      WRITE(6,1058)
71    CONTINUE
      IF(config_amores) GO TO 4100
      IF(DS-DDOP )1070,1070,1071
1070  IB=2
      GO TO 1072
1071  IB=1
1072  CALL PRONOR(JMAX,IQM,IJ,LV,ALFA,FAC,AX)
      IF(IND.EQ.1) GO TO 28
      WRITE(6,1059)AX
c     WRITE(6,*)'VOUS PASSEZ AU NIVEAU SUIVANT'
28    DO 1006 J=1,JMAX
      LV(J)=LV(J)*AX
1006  BIDON(J,I)=LV(J)
C
      TEMPOR=ALFA(JMAX)-0.7071068*ALFAD
      ATEST=4.*ALDP
      DO 1011 J=1,JMAX
      IF(ALFA(J).GE.ATEST) IB=1
      GO TO (1073,1074),IB
1073  IF(TEMPOR-ALFA(J)) 1011,1011,1048
1048  CALL CONF(DLAM(J),DLD,JMAX,RIS,LV,DLAM)
      GO TO 1075
1074  CALL CONV2(DLAM(J),DLD,JMAX,IQM,IJ,RIS,LV,DLAM)
1075  L(J,I)=RIS
      IF(IND.EQ.1) GO TO 1011
      IF(IB.EQ.1)GO TO 1080
      WRITE(6,1055)L(J,I)
      GO TO 1011
1080  WRITE(6,1056)L(J,I)
1011  CONTINUE
      GO TO 17
C CALCUL POUR UN NIVEAU DU MODELE DE LA TABLE:PHI(A,V) DANS LAQUELLE ON
C ERA POUR CHAQUE POINT DU PROFIL
C ON RESSERRE LA TABLE DE PHI,POUR V COMPRIS ENTRE 1 ET 4,SOIT VAR COMPR
C ENTRE DLD/2 ET 2*DLD
 4100 CALL FT2(JMAX,DLAM,LV,IH,VAR,F1)
c     WRITE(6,*)' VOUS PASSEZ AU NIVEAU SUIVANT'
      B1=DLD/2
      B2=2*DLD
      CTY=2./DLD
      DO 229 IK=1,IH
      IF(VAR(IK).LT.B1) GO TO 219
      IKD=IK-1
      GO TO 228
219   V(IK)=VAR(IK)*CTY
229   CONTINUE
228   DO 227 IK=IKD,IH
      IF(VAR(IK).LT.B2) GO TO 227
      IKF=IK
      GO TO 226
227   CONTINUE
220   FORMAT(1X,I5,2E15.8)
226   IKT=4*(IKF-IKD)
      PAK=(VAR(IKF)-VAR(IKD))/FLOAT(IKT)
      PAK=PAK*CTY
      DO 225 IK=1,IKT
      IW=IK+IKD
      V(IW)=V(IKD)+PAK*FLOAT(IK)
225   CONTINUE
      IKF=IKF+1
      DO 218 IK=IKF,IH
      IW=IW+1
      V(IW)=CTY*VAR(IK)
218   CONTINUE
      IW=IW+1
      V(IW)=V(IW-1)+5*CTY
      CALL HJEN(AZ(I),V,DLD,PHI,IW)
      CALL CONV4(DLAM,DLD,RESC,JMAX)
      DO 1017 K=1,JMAX
      L(K,I)=RESC(K)
1017  CONTINUE
C
17    CONTINUE
      WRITE(6,*)'LE DERNIER NIVEAU DU MODELE EST ATTEINT'
            IF(IND.EQ.0)   THEN
      IF(.NOT.STARK) GO TO 4011
      WRITE(6,4050)
      DO 4010 I=1,IMAX,5
      WRITE(6,4051) I
      WRITE(6,4065) (BIDON(K,I),K=1,JMAX)
4010  CONTINUE
 4011 CONTINUE
            END IF
4050  FORMAT('1','STARK=',//)
4051  FORMAT(' COUCHE',I4)
4065  FORMAT(8X,10E12.5)
      IF(J1.NE.2) GO TO 1027
      IF(K1.EQ.1) GO TO 1027
C
C     SEQUENCE POUR CALCUL DE BLEND
      DO 54 I=1,IMAX
54    STOC(I)=STOC(I)+L(1,I)
C***************************
C  IMPRESSIONS SUPPLEMENTAIRES
      WRITE(6,1063)NA,NB
      WRITE(6,1064)(L(1,I),I=1,IMAX)
C***************************
C***************************
      K=K+1
      IF(K1-K)48,49,49
49    IF(IK1.EQ.1) GO TO 45
      IF((IK1.EQ.2).AND.(IBOU.EQ.1))GO TO 46
      IF(IK1.EQ.2) GO TO 3
      IF(IK1.EQ.3) GO TO 47
      GO TO 1027
45    NB=NB-1
      GO TO 16
47    NB=NB+1
      GO TO 16
46    IF(NBMAX -NB)58,58,59
59    NB=NB+1
      GO TO 16
58    IF(IBOU.NE.1) GO TO 3
      NB=NB1-1
      IBOU=IBOU+1
      GO TO 16
3     NB=NB-1
      GO TO 16
48    DO 4 I=1,IMAX
4     L(1,I)=STOC(I)
C***************************
C  IMPRESSIONS SUPPLEMENTAIRES
      WRITE(6,1062)
      WRITE(6,1064)(L(1,I),I=1,IMAX)
C***************************
      GO TO 1027
1025  WRITE(6,1046) I,J
1027  RETURN
C
C     FORMATS
99    FORMAT ('1      DELOM        DELP        DELIE        RSURL
     1 R(N,T)       FZERO        LIM         ALFAD         NE
     2I'/)
115   FORMAT (' APPROX.QUASISTATIQUE POUR IONS + ELECTRONS')
121   FORMAT('1      RSURL        FZERO        ALFAD        NE
     1 DELIE        DELOM        DELP         LIM           I'/)
122   FORMAT (3X,1P,8E13.5,4X,I2)
1009  FORMAT ('   FORMULES ASYMPTOTIQUES')
1014  FORMAT ('   IMPACT   NON')
1015  FORMAT ('   APPROXIMATION   QUASISTATIQUE POUR IONS SEULS')
1016  FORMAT ('   IMPACT   OUI')
1021  FORMAT ('   LEWIS')
1022  FORMAT ('   DEGENERESCENCE')
1023  FORMAT ('   GRIEM 2 , 26')
1024  FORMAT ('   DEGENERESCENCE  NON  VALABLE')
1032  FORMAT ('   GRIEM   SIMPLE')
1035  FORMAT ('   BETA.GT.15')
1037  FORMAT ('   BETA.LT.15')
1040  FORMAT ('   GAMMA.GT.10   GAMMA=',E17.7)
1041  FORMAT ('   GAMMA.LT.10   GAMMA=',E17.7)
1042  FORMAT (3X,9(E12.5,1X),3X,I2/)
C1043 FORMAT ('  J=',I3,'  DLAMBDA=',F10.3,'   ALPHA=',E16.7,'   BETA=',
C     1     E16.7,'    L=',E16.7/)
1046  FORMAT('      RZERO/LAMBDA SUP. A 0.8     I=',I4,'     J=',I4,
     1'ON SORT DU SSP'/)
1054  FORMAT(/,'   DEMI LARGEUR DU PROFIL STARK  DS=',E17.7)
1055  FORMAT ('   CONV2=',1P,E16.5)
1056  FORMAT ('   CONF =',1P,E16.5)
1058  FORMAT ('1',///'     RESULTATS DU PRODUIT DE CONVOLUTION'//)
1059  FORMAT (' FACTEUR DE NORM.=',1P,E15.7)
1062  FORMAT(//10X,'COEF. SOMME (BLEND)')
1063  FORMAT(5X,'NA=',I5,5X,'NB=',I5)
1064  FORMAT(8(1P,E15.7))
1092  FORMAT ('   DISTR. MOZER BARANGER')
      END






      SUBROUTINE STEP(X,Y,P,ERR,N)
      DIMENSION X(N),Y(N),ALY(200),P(N),ERR(N)
c     COMMON avec INAIT
      COMMON /GENE/ALY,XMILIEU,XGAUSS1,XGAUSS2,DEL,YM,
     1  YG1,YG2,CONST,I,J
      XMILIEU=(X(I)+X(I+1))/2.
      DEL=X(I+1)-X(I)
      XGAUSS1=XMILIEU-DEL*CONST/2.
      XGAUSS2=XMILIEU+DEL*CONST/2.
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           ALY(J),ALY(J+1),ALY(J+2),ALY(J+3),XMILIEU,OUT)
      YML =EXP(OUT)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           ALY(J),ALY(J+1),ALY(J+2),ALY(J+3),XGAUSS1,OUT)
      YG1L=EXP(OUT)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           ALY(J),ALY(J+1),ALY(J+2),ALY(J+3),XGAUSS2,OUT)
      YG2L=EXP(OUT)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,YM)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XGAUSS1,YG1)
      call NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1           Y(J),Y(J+1),Y(J+2),Y(J+3),XGAUSS2,YG2)
      AMYS=(Y(I)+4.*YM+Y(I+1))/6.
      AMYG=(YG1+YG2)/2.
      AMY=0.6*AMYG+0.4*AMYS
      AMYLS=(Y(I)+4.*YML+Y(I+1))/6.
      AMYLG=(YG1L+YG2L)/2.
      AMYL=0.6*AMYLG+0.4*AMYLS
      ERR(I+1)=(AMYL-AMY)/AMY
      AMYRUSE=0.0*AMY+1.0*AMYL
      P(I+1)=P(I)+DEL*AMYRUSE
      RETURN
      END


      FUNCTION AS(GAM,BETA,VU,TO)
      AS=TO*GAM/(GAM**2+(BETA-VU)**2)
      RETURN
      END

      FUNCTION PIPE(X,Y,XX,YY,TAB,NMAX,MMAX)
C     INTERPOLATION DANS UN TABLEAU A DOUBLE ENTREE NOTE TAB
C     XX=TABLE DE LA VARIABLE LIGNE.  YY=TABLE DE LA VARIABLE COLONNE.
C     RESULTAT=VALEUR DE LA FONCTION POUR LES ENTREES X ET Y
C     ***
      INTEGER*2 J
      REAL MU,NU
      DIMENSION U(3),TAB(MMAX,NMAX),XX(NMAX),YY(MMAX)
      N=1
80    IF(X-XX (N))72,71,70
70    N=N+1
      GO TO 80
71    NN=N
      GO TO 90
72    IF(NMAX-N)74,74,73
73    NN=N-1
      GO TO 90
74    NN=N-2
90    M=1
91    IF(YY (M)-Y)92,93,94
93    MM=M
      GO TO 95
94    M=M+1
      GO TO 91
92    IF(MMAX-M)97,97,96
96    MM=M-1
      GO TO 95
97    MM=M-2
95    S0=Y-YY (MM)
      PHI=S0*(Y-YY (MM+1))/(YY (MM+2)-YY (MM))
      INN=NN+2
      J=0
      DO 200 NC=NN,INN
      J=J+1
      MU=(TAB  (MM+1,NC)-TAB  (MM,NC))/(YY (MM+1)-YY (MM))
      NU=(TAB  (MM+2,NC)-TAB  (MM+1,NC))/(YY (MM+2)-YY (MM+1))
200   U(J)=TAB(MM,NC)+S0*MU+PHI*(NU-MU)
      S0=X-XX (NN)
      PHI=S0*(X-XX (NN+1))/(XX (NN+2)-XX (NN))
      MU=(U(2)-U(1))/(XX (NN+1)-XX (NN))
      NU=(U(3)-U(2))/(XX (NN+2)-XX(NN+1))
      PIPE=U(1)+S0*MU+PHI*(NU-MU)
      RETURN
      END


      FUNCTION TA(X,BETA,GAM)
      DATA PI/3.141593/
      BB=2.*BETA*X
      AA=BETA**2+GAM**2
      Q=SQRT(AA)
      RM=SQRT(2.*Q-BB)
      EN=SQRT(2.*Q+BB)
      BCA=BB**2
      ACA=AA**2
      RCA=SQRT(20.)
      FAC=(BCA-AA)/(ACA*Q)
      PHI=BB/(4.*ACA*RM)-FAC/(4.*RM)
      TA=(3.*GAM/PI)*((-BB/AA+.1666667E-1)/(AA*RCA)+PHI*ALOG((20.+RCA*RM
     1+Q)/(20.-RCA*RM+Q))+(FAC/(2.*EN))*(PI/2.-ATAN((20.-Q)/(RCA*EN)))
     2+(BB/(2.*ACA*EN))*(PI-ATAN((2.*RCA+RM)/EN)-ATAN((2.*RCA-RM)/EN)))
      RETURN
      END

      FUNCTION ALIN(XX,U,V,FU,FV)
      ALIN=(FU*(V-XX)-FV*(U-XX))/(V-U)
      RETURN
      END


!!! Reads modele. Gotta comform this
        SUBROUTINE READER5N (NH,TETA,PE,PG,T5L,modeles_ntot)
c     identique a READERN mais lit le titre du modele sur l'unite 5
C       CE S.P. LIT SUR DISQUE ACCES DIRECT NH,TETA,PE,PG,T5L,modeles_ntot
C       il lit toujours NH de 1 a 50 (et non 0:50)
        DIMENSION TETA(50),NH(50),PE(50),PG(50),T5L(50),BID(16)
        REAL NH,modeles_nhe
      CHARACTER*4 BLC,modeles_tit
        COMMON /COM6/x_teff,GLOG,x_asalog,modeles_asalalf,modeles_nhe,modeles_tiabs(5),modeles_tit(5)
C     COMMON/TOTO/TO
        DATA   BLC/'    '/
c        type *,' entree dans readern'
              DO  I=1,5
             modeles_tit(I)=BLC
             END DO
        IF(IDEF.EQ.211939)   GO TO 10
c        type *,' On fait l''open du fichier  modeles.mod'
        OPEN(UNIT=18,ACCESS='DIRECT',status='OLD',FILE='modeles.mod',
     &       RECL=1200)
        ID=1
        IDEF=211939
c
 10   READ(5,*) x_teff,GLOG,x_asalog,INUM
        IF(INUM.GT.0)   ID=INUM
        WRITE(6,102)x_teff,GLOG,x_asalog,modeles_asalalf,modeles_nhe,INUM
C   SI L ON DESIRE IMPOSER UN MODELE  ON MET EN INUM LE NUM DU MODELE
C   SUR LE FICHIER ACCES DIRECT
 9      READ(18,rec=ID) modeles_ntot,modeles_teff,modeles_glog,modeles_asalog,modeles_asalalf,modeles_nhe,modeles_tit,modeles_tiabs
        WRITE(6,105) modeles_teff,modeles_glog,modeles_asalog,modeles_asalalf,modeles_nhe,modeles_tit
        write(6,108) modeles_tiabs
        IF(modeles_ntot.EQ.9999)   GO TO 6
        DDT  = ABS(x_teff-modeles_teff)
        DDG  = ABS(GLOG-modeles_glog)
        DDAB = ABS(x_asalog-modeles_asalog)
c        write(6,*)' D''s=',DDT,DDG,DDAB
 5          IF(DDT.GT.1.0) then
             ID=ID+1
            GO TO 9
            END IF
        IF(DDG.GT.0.01) THEN
         ID=ID+1
        GO TO 9
      END IF
        IF(DDAB.GT.0.01) THEN
         ID=ID+1
           GO TO 9
         END IF

        READ(18,REC=ID)BID,(NH(I),TETA(I),PE(I),
     &  PG(I),T5L(I),I=1,modeles_ntot)
        write(6,*)'            NH           TETA              PE',
     &       '             PG     To(5000)'
        DO I=1,modeles_ntot
        WRITE(6,103) NH(I),TETA(I),PE(I),PG(I),T5L(I)
        END DO
        WRITE(6,107)
        ID=1
        RETURN
 6          WRITE(6,101)
        STOP
 101  FORMAT('   LE MODELE DESIRE N EST PAS SUR LE FICHIER'/)
 102  FORMAT('   MODELE A LIRE ',F8.0,3F8.2,F8.3,4X,'LIGNE',I3,
     &  //' MODELES SUR LE DISQUE')
 103  FORMAT(E16.4,F15.4,2E16.4,F12.4)
 105  FORMAT(F10.0,4F10.2,5A4)
 106  FORMAT(2X,4F5.2,I5)
 107  FORMAT('     ETC.....')
 108  FORMAT(' Modele calcule avec la table ',5A4)


        END


C                          Janvier 1994
C
      SUBROUTINE LECTUR(ZP0,ZPH)
      INTEGER*4 PMAX,PNAX,P,CAL,CALMET
      DIMENSION ZP0(30)
      COMMON/LECT2/ZP(30),ZM(30),WI(41,2),NUMSET(2),CAL
      COMMON/TITIOLINE/TITRE(5)
      COMMON/SAHT/ZK(11),ZKM(30,9),NR(30) /SAHTP/XI(30,9),PF(30,9)
      COMMON/ABSO1/NM
      COMMON/NOM/NOMET(30)
C
C     CE SSP PERMET DE LIRE LES ABONDANCES ET LA TABLE D'IONISATION CHOI
C       Dans cette version on ne tient pas compte de l'absor des metaux
c
      OPEN(UNIT=15,STATUS='OLD',FILE='absoru2.dat')
C
C     ABMET=ABONDANCE TOTALE DES METAUX (NMET/NH)
C     ABHEL=ABONDANCE NORMALE D'HELIUM (modeles_nhe/NH)
C
      READ (15,39) NM, (TITRE(I),I=1,5)
C
C     NM=NBR. D'ELEMENTS(+LOURD QUE HE)CONSIDERES DANS LA TABLE D'IONISA
C     MMAX=NOMBRE D'ABSORBANTS METALLIQUES CONSIDERES
C     IUNITE=' GR.MAT.' SI ON VEUT CALCULER KAPPA PAR GRAMME DE MATIERE
C     IUNITE=' NOYAU H'  ''    ''    ''       ''      NOYAU D'HYDROGENE
C
      WRITE (6,80) (TITRE(I),I=1,5)
C
C     LECTURE DE LA TABLE D'IONISATION CHOISIE
C     ----------------------------------------
        READ (15,*) ZPH ! ab d'H pour laquelle sont donnees les ab metall
        ZPH=10**ZPH
        write(*,*) '  abondance d''H', ZPH
C
      DO  J=1,NM
      READ (15,*) NR(J),ZP0(J),ZM(J)
        ZP0(J)=10**ZP0(J)
        write(6,*) ' liste des 4 premiers ZP0', (ZP0(I),I=1,4)
C
C     NR=DEGRE MAXIMUM D'IONISATION CONSIDERE
C     ZP0=ABONDANCE standard DE L'ELEMENT
C     ZM=POIDS MOLECULAIRE DE L'ELEMENT
C
      NRR=NR(J)
         DO I=1,NRR
         READ (15,46) NOMET(J),NION,XI(J,I),PF(J,I)
C
C        ON LIT NR CARTES CONTENANT CHACUNE LE POTENTIEL D'IONISATION ET LA
C        FONCTION DE PARTITION(LOG10(2UI+1)/UI)DE CHAQUE DEGRE D'IONISATION
C          CES VALEURS SONT LUES DANS L'ORDRE CROISSANT DU DEGRE D'IONISATION
C        NOMET  =NOM DE L'ELEMENT
C        NION   =SON ETAT D'IONISATION
C        XI(J,I)=POTENTIEL D'IONISATION DE L'ELEMENT J AU STADE D'IONISATIO
C        PF(J,I)=FONCTION DE PARTITION         ''   ''     ''      ''   ''
C
         XI(J,I)=XI(J,I)*2.302585
         PF(J,I)=PF(J,I)*2.302585
           END DO
        END DO
        write(6,84) (NOMET(J),J=1,NM)
        write(6,83) (ZP0(J),J=1,NM)
c
c   ******  fin de lecture de la table d'ionisation ******
c   ******   Lecture de la table des discontinuites ******
c
c
c     NUMSET=NBR.DE LAMBDAS CONSIDERES POUR LA LISTE DES DISCONTINUITES
c     POUR H,HE ET HE+
c     PREMIERE LISTE POUR TH.LE.0.8  ITH=1,DEUXIEME LISTE POUR TH.GT.0.8
c
           do ITH=1,2
           read(15,*) NUMSET(ITH)
           NSET=NUMSET(ITH)
           READ (15,*) (WI(I,ITH),I=1,NSET)
c          type 83, (WI(I,ITH),I=1,NSET)
           end do
c
      CAL=1
      RETURN
c
c             formats
 39     FORMAT (I2,5A4)
 46     FORMAT (3X,A2,I1,2E16.5)
 80   FORMAT(1X,5A4)
 83     FORMAT(5E10.3)
 84     FORMAT(5(8X,A2))
      END



        SUBROUTINE abonio(ZP0,ZPH,asasol,coefalf)
c     Calcule l'abondance des elements en tenant compte
c     d'une eventuelle surabondance des elements alfa
c     On entre avec les valeurs lues par lectur, le coef de deficience
c     et la surabondance des alfa on en deduit les abondances des
c     differents elements intervenant dans l'ionisation ZP et ABMET
c     l'abondance des elements lourds pour 1at d'H
c     Les elements alfa sont reconnus par leur masse
        Dimension ALFAM(7),ZP0(30)
C      COMMONS AVEC LE SP D ABSORPTION CONTINUE
       COMMON/LECT1/ABMET,ABHEL /SAHTP/XI(30,9),PF(30,9)
       COMMON/LECT2/ZP(30),ZM(30),WI(41,2),NUMSET(2),CAL
       COMMON/SAHT/ZK(11),ZKM(30,9),NR(30)
       COMMON/ABSO2/MMAX
       COMMON/ABSO1/NM
       COMMON/NOM/NOMET(30)
       COMMON/TITIOLINE/TITRE(5)
c
       DATA ALFAM/16,20.2,24.3,27,28,32,40/
       nalf=7   ! nbre d'elements alfa reconnus par leur masse ALFAM
       write(*,*) '    Appel abonio'
c
           do J=1,NM
           ialfa=0
             do K=1, nalf
             DDM=ABS(ZM(J)-ALFAM(K))
                if (DDM.LT.0.3) then
                ialfa=ialfa+1
                end if
             end do  !fin de la bcke sur k
c            type 101,NOMET(J), ialfa
c
             if (ialfa.eq.0) then
             ZP(J)=ZP0(J)
             else
             ZP(J)=ZP0(J) * coefalf
             end if
          end do   ! fin de la bcle sur j
c
        SOM1=0
           DO J=1,NM
           SOM1=SOM1+ZP(J)
           End do
        A0=SOM1/ZPH
        ABMET=A0*ASASOL
c        write(6,*) ' liste des 4 premiers ZP0', (ZP0(I),I=1,4)
c        write(6,*) ' liste des 4 premiers ZP', (ZP(I),I=1,4)
        write(6,*) 'in abonio AMET=',ABMET
        RETURN
        END



      SUBROUTINE ABSORU(Wl,Th,Zlpe,Callam,Calth,Calpe,Calmet,Calu,
     &                  Calsor,Kkk,Totkap)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL AA , ABHel , ABMet , AC , AC1 , AC2 , AH , AHE , AHEp , AVM ,
     &     dif , G2D , G3D , GRDm , PE , PF , PG , RHO , RHOg , scat
      REAL scatel , scath , STImu , STWtm , SUM1 , sum2 , Th , TITre ,
     &     TOC , Totkap , U2 , UH1 , UHE1 , UHEp1 , UL , unit , V1 ,
     &     WI , WINv , Wl
      REAL wl4 , wlh , XI , YY , ZEFf4 , ZEHm , ZEUh , ZEUhe1 , ZEUhep ,
     &     ZEXp , ZEXpm , ZK , ZKM , ZLEtag , ZLH , ZLHe , ZLHem ,
     &     ZLHep , Zlpe , ZM
      REAL ZMU , ZMUze , ZNH , ZNU , ZNU1 , ZNU2 , ZNU3 , ZP , zzk ,
     &     zzkk
      INTEGER i , ilt , ith , JFZ , JH , JSHyd , Kkk , min , mm , MMAx ,
     &        mmm , NM , NOMet , NR , nset , NUMset , m
      CHARACTER*4 IUNite
C*** End of declarations inserted by SPAG
      INTEGER*4 CAL , Calu , Calmet , Callam , Calth , Calpe , Calsor
      DIMENSION zzkk(11,2) , Totkap(2) , dif(2,3) , scath(2) , zzk(11,2)
     &          , scat(2)
      COMMON /LECT1 / ABMet , ABHel
      COMMON /LECT2 / ZP(30) , ZM(30) , WI(41,2) , NUMset(2) , CAL
      COMMON /GBF   / ZLH(19)
      COMMON /GBFH  / G2D(2,19), JSHyd(2) , JH
      COMMON /TETA  / AHE, AH, AHEp, UH1, ZEHm, UHEp1, UHE1, ZEUhe1
      COMMON /TEHE  / ZLHem(5) , ZLHe(10)
      COMMON /TEMPE / STWtm(5)
      COMMON /THE   / ZEUh(20) , ZEUhep(20) , ZEXpm(5) , ZEXp(10) , UL
      COMMON /SAHT  / ZK(11) , ZKM(30,9) , NR(30)
      COMMON /SAHTP / XI(30,9) , PF(30,9)
      COMMON /ABSO2 / MMAx
      COMMON /ABSO3 / JFZ
      COMMON /HYHE  / GRDm(46), V1(46), U2(46), WINv(46), YY(4),
     &                ZLEtag(18) , G3D(12,18) , AA(4) , ZEFf4(10) ,
     &                RHOg(12) , ZLHep(19)
      COMMON /ZION  / AC , AC1(3) , AC2(30,9)
      COMMON /SAPE  / AVM , ZNU1 , ZNU2 , ZNU3 , ZMUze , ZNU(30)
      COMMON /SAPDIV/ ZMU , PG
      COMMON /SAPU  / PE , RHO , TOC , ZNH(12)
      COMMON /UNI   / IUNite(2)
      COMMON /ABSO1 / NM
      COMMON /NOM   / NOMet(30)
      COMMON /TITIOLINE/ TITre(5)
      COMMON /SOMAB / SUM1
      DATA dif/5.799E-13 , 8.14E-13 , 1.422E-6 , 1.28E-6 , 2.784 , 1.61/
      DATA IUNite/' Noy' , 'au H'/
      ilt = Callam
      IF ( Calpe.EQ.2 ) GOTO 800
      SUM1 = 0.0
      sum2 = 0.0
      DO 100 i = 1 , NM
         SUM1 = SUM1 + ZP(i)
         sum2 = sum2 + ZP(i)*ZM(i)
 100  CONTINUE
      AVM = sum2/SUM1
C
C     AVM=MASSE ATOMIQUE MOYENNE DES ELEMENTS PLUS LOURDS QUE L'HELIUM
C     SUM1=SOMME DES ABONDANCES DES METAUX
      DO 200 i = 1 , NM
         ZNU(i) = ZP(i)/SUM1
 200  CONTINUE
C
C     ZNU1,ZNU2,ZNU3=SUCCESSIVEMENT FRACTION D'(H,HE,METAL) PAR NOMBRE T
C
      ZNU1 = 1.0/(1.0+ABMet+ABHel)
      ZNU2 = ZNU1*ABHel
      ZNU3 = ZNU1*ABMet
      ZMUze = 1.008*ZNU1 + 4.003*ZNU2 + AVM*ZNU3
      IF ( (Calth.EQ.2) .AND. (Callam.EQ.2) ) THEN
         IF ( (JFZ.EQ.2) .AND. (ilt.NE.1) ) THEN
            ilt = Callam - 1
            CALL GAUNTH(Wl)
         ENDIF
         GOTO 700
      ELSE
         IF ( Calth.NE.2 ) THEN
            IF ( Th.LE.0.8 ) ith = 1
            IF ( Th.GT.0.8 ) ith = 2
            nset = NUMset(ith) - 1
         ENDIF
         DO 250 i = 1 , nset
            IF ( ABS(Wl-WI(i+1,ith)).LE.0.50 ) GOTO 400
            IF ( Wl.LT.WI(i+1,ith) ) GOTO 300
 250     CONTINUE
 300     JFZ = 1
         GOTO 500
      ENDIF
 400  JFZ = 2
C
C     DIFFUSION DE RAYLEIGH PAR H ET H2 (DALGARNO) HARVARD JUIN 1964
C     SCATH(1)=DIFFUSION DE H
C     SCATH(2)=DIFFUSION DE H2
C
 500  DO 600 i = 1 , 2
         IF ( i.EQ.2 ) THEN
            IF ( Wl.GT.1200.0 ) THEN
               wlh = Wl
            ELSE
               wlh = 1200.0
            ENDIF
         ELSEIF ( Wl.GT.1026.0 ) THEN
            wlh = Wl
         ELSE
            scath(1) = 4.0E-24
            GOTO 600
         ENDIF
         wl4 = wlh**4
         scath(i) = (dif(i,1)+dif(i,2)/SQRT(wl4)+dif(i,3)/wl4)/wl4
 600  CONTINUE
      CALL GAUNTH(Wl)
 700  CALL TEMPA(Wl,Th,Calth,Callam)
      IF ( Calth.NE.2 ) CALL SAHATH(Th)
      IF ( (Calth.NE.2) .OR. (Callam.NE.2) )
     &     CALL ATHYHE(Wl,Th,Calth,Callam,zzk)
      IF ( Calmet.EQ.1 ) THEN
      ENDIF
 800  CALL IONIPE(Th,Zlpe,Calth,Calmet)
      mm = MMAx + 1
      mmm = MMAx + 6
      scatel = 9.559063E-13*PE*Th
C
C     9.559063E-13=4.81815E-9/5040.39 ET 4.81815E-9=6.625E-25/1.38024E-1
C     =ELECTRON SCATTERING/(K*T)  UNSOLD P. 180 1955
C
      IF ( Calsor.NE.1 ) WRITE (6,99001) (IUNite(i),i=1,2)
      Kkk = JFZ
      min = mm
      DO 900 i = 1 , JFZ
         DO 850 m = 1 , MMAx
            zzkk(m,i) = 0.0
 850     CONTINUE
 900  CONTINUE
      Totkap(2) = 0.
      IF ( Calu.EQ.1 ) unit = RHO
      IF ( Calu.EQ.2 ) unit = TOC
C     RAPPEL  TOC=NBRE DE NOYAUX DE H PAR CM3
      scatel = scatel/unit
      DO 1000 i = 1 , Kkk
         Totkap(i) = 0.0
         scat(1) = scath(1)*ZNH(MMAx+4)/unit
         scat(2) = scath(2)*ZNH(MMAx+2)/unit
c         WRITE (*,*) 'scat=' , scat
         DO 950 m = min , mmm
C     LES ZNH POUR LES METAUX SONT EN CM-3*1.0E-18
            IF ( (m.NE.(MMAx+1)) .AND. (m.NE.(MMAx+3)) ) THEN
               zzkk(m,i) = zzk(m,i)*ZNH(m)/unit
               IF ( m.EQ.(MMAx+2) ) zzkk(m,i) = zzkk(m,i)*PE
            ELSE
               IF ( m.EQ.(MMAx+1) ) zzkk(m,i) = zzk(m,i)
     &              *(ZNH(MMAx+4)*PE*1.E-26)/unit
               IF ( m.EQ.(MMAx+3) ) zzkk(m,i) = zzk(m,i)
     &              *((ZNH(MMAx+4)*1.E-19)*(ZNH(MMAx+7)*1.0E-20))/unit
            ENDIF
            Totkap(i) = Totkap(i) + zzkk(m,i)
 950     CONTINUE
         Totkap(i) = (Totkap(i)+scatel+scat(1)+scat(2))
         IF ( Calsor.NE.1 ) WRITE (6,99002) Wl , Kkk ,
     &                             (zzkk(m,i),m=1,mmm) , Totkap(i)
 1000 CONTINUE
      IF ( Calsor.NE.1 ) WRITE (6,99003) scatel , scat(1) , scat(2) ,
     &                          RHO , TOC , Zlpe , Th
      CAL = 2
      RETURN
99001 FORMAT ('0LAMBDA KKK   C1',7X,'MG',7X,'SI1',7X,'AL1',8X,'H-',7X,
     &        'H2-',7X,'H2+',9X,'H',7X,'HE+',8X,'HE',5X,'K TOTAL/',2A4)
99002 FORMAT (F9.1,I2,1P12E10.2)
99003 FORMAT ('0SIG(E)=',1PE11.4,' SIG(H)=',E11.4,' SIG(H2)=',E11.4,
     &        ' DENSITE=',E11.4,' NBR.NOYAU D H/CM3=',E11.4,' LOG10PE=',
     &        0PF5.2,' TETA=',F5.2)
      END


      SUBROUTINE IONIPE(TH,ZLPE,CALTH,CALMET)
C
C     A.M COLLE  18/01/1971
C
      INTEGER*4 CALTH,CALMET
      REAL KTH
      DIMENSION PHI(30),PA(10)
        COMMON/SAPE/AVM,ZNU1,ZNU2,ZNU3,ZMUZE,ZNU(30)/ZION/AC,AC1(3),
     &   AC2(30,9)/SAHT/ZK(11),ZKM(30,9),NR(30)/SAPU/PE,RHO,TOC,
     &   ZNH(12)/SAPDIV/ZMU,PG/ABSO1/NM/ABSO2/MMAX
C
C     SSP CALCULANT LES QUANTITES SUIVANTES
C     PARTH =NBRE TOTAL DE NOYAUX PAR CM3
C     PG    =PRESSION TOTALE EN DYNES/CM2
C     ZMU   =POIDS MOLECULAIRE MOYEN
C     RHO   =DENSITE (G-CM-3)
C     TOC   =NOMBRE DE NOYAUX D'HYDROGENE PAR CM3
C     AC    =DEGRE D'IONISATION MOYEN
C     AC1(1)=  ''        ''     DE H
C     AC1(2)=  ''        ''     DE HE+
C     AC1(3)=  ''        ''     DE HE
C     AC2   =  ''        ''     DES METAUX
C     PHI(J)=  ''        ''     DE L ELEMENT J POUR MULTIPLE IONISATION
C     ZNH(M)=POPULATION POUR CHAQUE ABSORBANT M (H,HE OU METAUX)
C     VOIR ARTICLE DE 'VARDYA' APJ VOL.133,P.107,1961
C
      KTH=6.956948E-13/TH
C     6.956948E-13=1.38024E-16*5040.39
      PE=EXP(ZLPE*2.302585)
      SIGM3=0.0
      DO 2380 J=1,NM
      NRR=NR(J)
      SIGM1=0.0
      SIGM2=0.0
      PA(1)=1.0
      DO 2370 I=1,NRR
      IF ((PA(I).LE.0.0).OR.(ZKM(J,I).LE.0.0)) GO TO 2375
      PA(I+1)=PA(I)*(ZKM(J,I)/PE)
      SIGM1=I*PA(I+1)+SIGM1
2370  SIGM2=SIGM2+PA(I+1)
2375  DEN=1.0+SIGM2
      PHI(J)=SIGM1/DEN
      DO 1 I=1,NRR
1     AC2(J,I)=PA(I)/DEN
2380  SIGM3=SIGM3+ZNU(J)*PHI(J)
      IF (CALTH.EQ.2) GO TO 2390
      IF (TH.GE.0.25) GO TO 2382
      TEMPOR=0.0
      ANY=0.0
      COND=0.0
      GO TO 2390
2382  ANY=1.0/ZK(MMAX+3)
      TEMPOR=1.0/ZK(MMAX+2)
      COND=1.0/ZK(MMAX+1)
2390  W2=ZK(MMAX+4)/PE
      W1=W2*ANY
      W3=PE*COND
      W4=ZNU2*ZK(MMAX+6)*(PE+2*ZK(MMAX+5))/
     &   ((PE+ZK(MMAX+6))*PE+ZK(MMAX+6)*ZK(MMAX+5))
     &   +(ZNU3*SIGM3)
      FUN1=ZNU1*W1+2*(TEMPOR+W1)*W4
      FUN2=ZNU1*(W2-W3)+(1.0+W2+W3)*W4
      PH=2*ZNU1*PE/(FUN2+SQRT(FUN2**2+4*FUN1*ZNU1*PE))
      ZNH(MMAX+4)=PH/KTH
      ZNH(MMAX+2)=PH*TEMPOR*ZNH(MMAX+4)
      ZNH(MMAX+1)=ZNH(MMAX+4)*W3
      ZNH(MMAX+7)=ZNH(MMAX+4)*W2
      ZNH(MMAX+3)=ZNH(MMAX+7)*PH*ANY
      TP1=ZNH(MMAX+1)+ZNH(MMAX+4)+ZNH(MMAX+7)
      TP2=ZNH(MMAX+2)+ZNH(MMAX+3)
      TOC=2*TP2+TP1
      PARTH=TOC/ZNU1
      PPAR=(TP1+TP2+PARTH*(ZNU2+ZNU3))*KTH
      PG=PPAR+PE
      AC=PE/PPAR
      W5=ZK(MMAX+6)/PE
      W6=ZK(MMAX+5)*W5/PE
      S=1.0+W5+W6
      AC1(1)=W2/(1.0+W2)
      AC1(2)=W6/S
      AC1(3)=W5/S
      ZNH(MMAX+6)=ZNU2*PARTH/(1.0+W5+W6)
      ZNH(MMAX+5)=ZNH(MMAX+6)*W5
      RHO=1.6602E-24*PARTH*ZMUZE
C     1.6602E-24=MASSE DE L'UNITE DE POIDS
      ZMU=RHO*41904.28E+7/(TH*PG)
C     41904.275E+7=8.313697E+7*5040.39,OU 8.313697E+7=CONSTANTE DES GAZ
      RETURN
      END



      SUBROUTINE  TEMPA(WL,TH,CALTH,CALLAM)
C     A.M COLLE   8/5/69
C
      INTEGER*4 CALLAM,CALTH
        COMMON /TETA/AHE,AH,AHEP,UH1,ZEMH,UHEP1,UHE1,
     &   ZEUHE1/TEHE/ZLHEM(5),ZLHE(10)/TEMPE/STWTM(5)/THE/ZEUH(20),
     &   ZEUHEP(20),ZEXPM(5),ZEXP(10),UL
C
C     HCBKTM=(H*C/K*T)*1.0E8
C     0.0010967876=CONSTANTE DE RYDBERG POUR H  *1.0E-8  ALLEN 1963
C     0.0043890867=CONSTANTE DE RYDBERG POUR HE+*1.0E-8  MOORE 1950 (HE4
C     AHE =POUR HE 4*C/T**3
C     AH  =POUR H   C*Z**4/T**3  AVEC Z=1
C     AHEP=POUR HE+ C*Z**4/T**3  AVEC Z=2
C     C=64*PI**4*ME*E**10/(3*RAC(3)*C*H**3*K**3)
C     ME=9.10E-28,E**10=4.8E-10,K=1.38024E-16,H=6.6237E-27,C=2.99791E+10
C
      IF (CALTH.EQ.2) GO TO 1001
      HCBKTM=0.2854306E-3*TH*1.0E8
      AHE=0.9717088E-12*TH**3
      AH=0.2429272E-12*TH**3
      AHEP=16.*AH
      UH1=1.096788E-3*HCBKTM
      ZEMH=EXP(-UH1)
      IF (TH.GT.1.4) GO TO 1001
      DO 1304 J=1,20
      UH=UH1/J**2
1304  ZEUH(J)=EXP(UH-UH1)/J**3
      ZEUH(20)=ZEUH(20)*8000.
      UHEP1=4.389087E-3*HCBKTM
      IF (TH.GT.0.3) GO TO 5290
      DO 1313 J=1,20
      UHEP=UHEP1/J**2
1313  ZEUHEP(J)=EXP(UHEP-UHEP1)/J**3
      ZEUHEP(20)=ZEUHEP(20)*8000.
5290  UHE1=HCBKTM/504.3
      ZEUHE1=EXP(-UHE1)
      IF (TH.GT.0.8) GO TO 1001
      COMHE=-HCBKTM*(1.0/ZLHEM(1))
      DO 5310 K=1,5
5310  ZEXPM(K)=EXP(COMHE+HCBKTM*(1.0/ZLHEM(K)))*STWTM(K)
      DO 5340 L=3,10
5340  ZEXP(L)= EXP(COMHE+HCBKTM*(1.0/ZLHE(L)))/L**3
1001  IF ((CALLAM.EQ.2).AND.(CALTH.EQ.2)) GO TO 5010
      UL=HCBKTM/WL
5010  RETURN
      END



      SUBROUTINE SAHATH(TH)
C     A.M COLLE   13/5/69
C     LOI DE SAHA=LOG((NR+1)/NR)*PE= -POT.ION.*TH+5/2*LOG(T)-0.4772+FONC
C     LES FONCTIONS DE PARTITION (L0G(2UR+1)/UR) SONT INCLUSES DANS LES
C     CONSTANTES AJOUTEES A TEMPOR POUR H ET HE LES AUTRES SONT LUES
C     31.303644,1.7200311,56.597541,125.26753,SONT RESPECTIVEMENT LES
C     POTENTIELS D'IONISATION DE (H,H-,HE,HE+)*2.3025851
C
      DIMENSION POTION(6),C1(3),C2(3),C3(3),C4(3)
        COMMON /ABSO2/MMAX
        COMMON /SAHT/ZK(11),ZKM(30,9),NR(30)
        COMMON /SAHTP/XI(30,9),PF(30,9)/ABSO1/NM
        DATA C1/0.0,-7.526612E-3,5.708280E-2/,C2/0.0,1.293852E-1,
     &   -1.823574E-1/,C3/0.0,-11.34061,-6.434060/,C4/0.0,28.85946,
     &   25.80507/,POTION/-1.720031,0.0,0.0,31.30364,125.2675,-56.59754/
C

            DO  N=2,3
            ZK(MMAX+N)=EXP(((C1(N)*TH+C2(N))*TH+C3(N))*TH+C4(N))
            END DO
C
      TEMPOR=2.5*ALOG(5040.39/TH)
      TEMPO=TEMPOR-1.098794
            DO N=4,5
            ZK(MMAX+N)=POTION(N)*TH-TEMPO
                  if  (ZK(MMAX+5).LT.38) then
                  ZK(MMAX+N)=EXP(-ZK(MMAX+N))
                  else
                  ZK(MMAX+N)=0.0
                  end if
            END DO
C
      DO  J=1,NM
      NRR=NR(J)
            DO  I=1,NRR
            ZKM(J,I)=TH*XI(J,I)-PF(J,I)-TEMPO
                  IF (ZKM(J,I).LT.38)then
                  ZKM(J,I)=EXP(-ZKM(J,I))
                  else
                  ZKM(J,I)=0.0
                  END IF
            END DO
      END DO
c
      TEMPO=TEMPOR+0.2875929
      DO  N=1,6,5
      POTEPO=POTION(N)*TH+TEMPO
            if(POTEPO.GT.-38) then
            ZK(MMAX+N)=EXP(POTEPO)
            else
            ZK(MMAX+N)=0
            end if
      END DO
c
      RETURN
      END



      BLOCK DATA
C     BLOCK DATA POUR LE SOUS PROGRAMME  ABSORU POUR LES RAIES D'HYDROGE
C
C     A.M COLLE  12/08/70
      DIMENSION G3D1(12,9),G3D2(12,9)
C
      COMMON /D1/TET(7),ALP(9),TAB(9,7),MMAX,NMAX
        COMMON /GBF/ZLH(19)
        COMMON/HYHE/GRDM(46),U1(46),U2(46),WINV(46),YY(4),
     &   ZLETAG(18),G3D(12,18),AA(4),ZEFF4(10),RHOG(12),
     &   ZLHEP(19)
        COMMON/TEHE/ZLHEM(5),ZLHE(10)
        COMMON/TEMPE/STWTM(5)
      EQUIVALENCE (G3D(1,1),G3D1(1,1)),(G3D(1,10),G3D2(1,1))
        DATA ALP/5.,4.,3.,2.,1.,0.,-1.,-2.,-3./,MMAX,NMAX/9,7/,
     &   TET/0.1,0.2,0.3,0.4,0.5,0.6,0.7/
      DATA TAB/1.43,1.83,2.28,2.77,3.27,3.77,4.27,4.77,5.27,
     &         0.47,0.62,0.91,1.31,1.78,2.26,2.75,3.25,3.76,
     &         0.31,0.32,0.35,0.42,0.61,0.93,1.35,1.82,2.32,
     &         0.30,0.30,0.30,0.31,0.32,0.35,0.44,0.65,0.99,
     &         0.30,0.30,0.30,0.30,0.30,0.30,0.31,0.32,0.36,
     &         18*0.30/
        DATA WINV/361.9,429.9,514.4,615.3,733.7,869.7,1028.7,1226.2,
     &       1460.9,1737.3,2044.4,2407.4,2851.6,3378.1,3986.8,4677.7,
     &       5477.3,6442.5,7574.3,8872.9,10338.2,12064.6,14049.7,
     &       16352.9,18996.2,22056.2,25576.8,29634.9,34307.2,39703.3,
     &       45943.9,53171.7,61529.1,71213.6,82433.7,95441.4,110445.3,
     &       127774.4,147406.7,169671.2,194568.0,221877.7,251600.4,
     &       282968.2,312800.5,329032.7/
        DATA GRDM/1729.881 ,1591.634 ,1450.598 ,1317.928 ,1198.805 ,
     &       1091.634 ,994.4223,896.8127,805.5777,722.7092,649.4024,
     &       583.2669,517.9283,457.7689,405.1793,358.9641,316.3347,
     &       275.7371,238.9641,206.6136,180.4781,153.5857,130.4781,
     &       109.6813,91.9920,76.1753,62.7092,50.9960,40.9562,32.5498,
     &       25.4263,19.6693,14.9920,11.2470,8.2869,5.9960,4.2311,
     &       2.8900,1.9020,1.1733,0.6781,0.3544,0.1605,0.0602,0.0171,
     &       0.0000/
        DATA U1/0.00263,0.00286,0.00322,0.00372,0.00435,0.00511,0.00600,
     &       0.00701,0.00821,0.00958,0.01190,0.01305,0.01522,0.01772,
     &       0.02061,0.02394,0.02775,0.03207,0.03699,0.04257,0.04884,
     &       0.05584,0.06367,0.07229,0.08180,0.09216,0.10340,0.11542,
     &       0.12815,0.14147,0.15511,0.16871,0.18167,0.19309,0.20167,
     &       0.20525,0.20052,0.18186,0.13996,0.05794,-05.09644,-0.39105,
     &       -0.99032,-2.39840,-7.14260,-85.0/
        DATA U2/0.00114,0.00124,0.00145,0.00178,0.00221,0.00277,
     &       0.00342,0.00416,0.00508,0.00615,0.00745,0.00899,0.01083,
     &       0.01302,0.01561,0.01869,0.02237,0.02676,0.03195,0.03810,
     &       0.04540,0.05412,0.06445,0.07676,0.09140,0.10889,0.12977,
     &       0.15473,0.18466,0.22057,0.26382,0.31606,0.37932,0.45618,
     &       0.54997,0.66493,0.80665,0.98279,1.20442,1.48940,1.
     &       7040,2.41450,3.28470,4.97840,9.99460,85.0/
        DATA ZLHEM/504.3,2601.0,3122.0,3422.0,3680.0/,STWTM/1.0,3.0,
     &       1.0,9.0,3.0/,ZLHE/0.0,0.0,7932.0,14380.0,22535.0,32513.0,
     &       44313.0,57936.0,73383.0,90603.0/,ZEFF4/0.0,0.0,1.069373,
     &       1.028328 ,1.022291 ,1.018358,1.015639,1.013614,1.012019,
     &       1.011869/,ZLH/911.8,3647.0,8205.9,14588.2,22794.1,32823.5,
     &       44676.4,58352.9,73852.8,91176.3,110323.4,131293.9,154088.0,
     &       178705.6,205146.7,233411.4,263499.6,295411.3,329146.5/,
     &       ZLHEP/227.8,911.8,2050.6,3645.6,5696.2,8202.5,11164.5,
     &       14582.3,18455.7,22784.8,27569.6,32810.1,38506.3,44658.2,
     &       51265.8,58329.0,65848.0,73822.6,82253.0/
        DATAG3D1/2.885,2.419,2.047,1.679,1.468,1.323,1.212,1.124,1.051,
     &       0.989,0.810,0.693,2.906,2.420,2.049,1.684,1.474,1.330,
     &       1.220,1.133,1.061,1.000,0.824,0.708,2.912,2.430,2.072,
     &       1.723,1.527,1.395,1.296,1.218,1.155,1.102,0.951,0.856,
     &       2.892,2.423,2.082,1.760,1.583,1.469,1.385,1.320,1.268,
     &       1.226,1.111,1.045,2.815,2.365,2.046,1.755,1.604,1.507,
     &       1.438,1.387,1.346,1.314,1.230,1.185,2.715,2.280,1.978,
     &       1.709,1.573,1.488,1.428,1.383,1.348,1.320,1.249,1.208,
     &       2.615,2.194,1.906,1.654,1.530,1.452,1.398,1.357,1.326,
     &       1.303,1.237,1.202,2.231,1.868,1.629,1.440,1.352,1.298,
     &       1.261,1.235,1.215,1.198,1.158,1.136,1.955,1.635,1.445,
     &       1.303,1.238,1.201,1.175,1.157,1.144,1.133,1.106,1.091/
        DATA G3D2/1.807,1.518,1.357,1.239,1.187,1.157,1.137,1.123,
     &       1.112,1.104,1.082,1.065,1.707,1.446,1.303,1.201,1.157,
     &       1.131,1.115,1.103,1.094,1.087,1.069,1.054,1.634,1.394,
     &       1.266,1.175,1.136,1.114,1.100,1.089,1.081,1.075,1.056,
     &       1.046,1.579,1.357,1.239,1.157,1.121,1.102,1.088,1.079,
     &       1.072,1.067,1.049,1.042,1.497,1.302,1.201,1.131,1.101,
     &       1.085,1.073,1.066,1.060,1.055,1.042,1.034,1.442,1.265,
     &       1.175,1.113,1.088,1.073,1.064,1.057,1.052,1.046,1.035,
     &       1.030,1.400,1.237,1.156,1.101,1.078,1.065,1.057,1.051,
     &       1.045,1.042,1.032,1.026,1.367,1.217,1.142,1.091,1.071,
     &       1.059,1.051,1.045,1.041,1.037,1.029,1.024,1.342,1.200,
     &       1.130,1.084,1.065,1.053,1.047,1.042,1.037,1.034,1.026,
     &       1.022/
        DATA RHOG/1.010,1.025,1.050,1.100,1.150,1.200,1.250,1.300,1.350,
     &       1.400,1.600,1.800/,ZLETAG/-3.0000,-2.0000,-1.0000,-0.6021,
     &       -0.3010,-0.1249,0.0000,0.3979,0.6990,0.8751,1.0000,1.0969,
     &       1.1761,1.3010,1.3979,1.4771,1.5441,1.6021/,YY/0.3225,
     &       1.7458,4.5366,9.3951/,AA/0.6032,0.3574,0.0389,0.0005/
C
C     DONNEES POUR H2+ =TABLE DE BATES(1952) COMPLETEE PAR HARVARD(1964)
C     ------------------
C     WINV=NU/C,GRDM=(-NU/DNU/DR)*R*D(R),U1=-U(1S SIGMA/R),U2=U(+2P SIGM
C     DONNEES POUR L'HELIUM NEUTRE
C     ----------------------------
C     ZLHEM(K)ET STWTM(K)=LAMBDAS DES DISCONTINUITES  ET VALEUR DU POIDS
C     STATISTIQUE CORRESPONDANT
C     ZLHE(L) ET ZEFF4(L)=SUITE DES DISCONTINUITEES ET SECTIONS EFFICACE
C     CORRESPONDANTES
C     DONNEES POUR L'HYDROGENE GRANT=MON. NOT. VOL. 118 P. 241 1958
C     ------------------------
C     G3D(I,J) =FACTEUR DE GAUNT EN FONCTION DE RHO ET DE LOG(-ETA)
C     RHO(I)  =VALEUR DE RHO CORRESPONDANTES
C     ZLETAG(J)=VALEUR DE LOG(-ETA)
C     YY=ZEROS DU POLYNOME DE LAGUERRE=CHANDRASEKHAR RADIATIVE TRANSFER
C     AA=NBR. DE CHRISTOFFEL CORRESPONDANT
C     DONNEES POUR QUASI MOLECULE H+H (DOYLE,APJ,153,187.1968)
      END



      SUBROUTINE ATHYHE (WL,TH,CALTH,CALLAM,ZZK)
C     A.M COLLE  07/12/1970
Cpatrick
      INTEGER*4 CALLAM,CALTH
        DIMENSION TGAUNT(5),TRHOG(5),OPNU(46),ZZK(11,2),COTE(2),
     &       SNIV(2),EXPO(2),CUK(2),CONS(2),AN(2),C1(3),C2(3),C3(3),
     &       C4(3),C5(3),C6(3),EXPON(2)
        COMMON /ABSO2/MMAX
        COMMON/ABSO3/JFZ
        COMMON/HYHE / GRDM(46),U1(46),U2(46),WINV(46),YY(4),
     &  ZLETAG(18),G3D(12,18),AA(4),ZEFF4(10),RHOG(12),ZLHEP(19)
        COMMON/ABME/STIMU
        COMMON/GBFH/G2D(2,19),JSHYD(2),JH
        COMMON/TETA/AHE,AH,
     &   AHEP,UH1,ZEMH,UHEP1,UHE1,ZEUHE1/TEHE/ZLHEM(5),
     &   ZLHE(10)
        COMMON/THE/ZEUH(20),ZEUHEP(20),ZEXPM(5),ZEXP(10),UL
        DATA EXPO/-68.88230,-71.45087/,CONS/3.3,3.6/,COTE/3.136954E-23,
     &       8.195952E-24/,SNIV/0.55,0.485/,CUK/0.3025,0.235225/,
     &       AN/0.3099204E-21,0.22849203E-21/,C1/-2.850692E-2,
     &       -7.056869E-3,3.591294E-3/,C2/0.2080816,0.1809394,-0.1959804
     &       /,C3/2.549101,-1.828635,4.233733/,C4/-14.97997,8.900841,
     &       -20.84862/,C5/0.0,-17.78231,0.0/,C6/0.0,-7.89472E-2,0.0/
C
C     CE SSP CALCULE LE COEFFICIENT D'ABSORPTION PAR ATOME NEUTRE POUR
C     L'HYDROGENE ET L'HELIUM, ON SORT 2 VALEURS DE ZZK SI WL= A UNE
C     DISCONTINUITE DE L'UN DE CES ABSORBANTS
C
      JHE=0
      JHEP=0
      JHEM=0
      IF (CALLAM.EQ.1) INDTH=0
      JHYT=1
      GCONST=31.3213*TH
C     31.3213= 157871.62/5040.39 , 157871.62=M*Z**2*E**4/(2*K*(H/2*PI)**
C     M ET E SONT LA MASSE ET LA CHARGE DE L'ELECTRON,H ET K LES CONSTAN
C     DE PLANCK ET DE BOLTZMANN
C
      IF (UL.LT.100.0) GO TO 1333
C     UL=H*NU/(K*T)
      STIMU=1.0
      GO TO 1334
1333  STIMU=1.0-EXP(-UL)
1334  STIMU3=STIMU/UL**3
      IF (CALLAM.EQ.2) GO TO 1335
      ZNL=ALOG(WL)
      ZLAMIN=1.0E8/WL
      DO 3 N=1,2
3     EXPON(N)=EXP(EXPO(N)+CONS(N)*ZNL)
C
C     I   H-
C     H- GINGERICH= HARVARD JUIN 1964 (RESULTATS*1.0E-26)
1335  IF (TH.GE.0.3) GO TO 6060
      ZZK(MMAX+1,1)=0.0
      GO TO 6210
6060  IF ((CALLAM.EQ.2).AND.(INDTH.EQ.1)) GO TO 6100
      INDTH=1
      IF (WL.LE.16419.0) GO TO 6070
      ALTHMB=0.0
      GO TO 6190
6070  WLM=WL/1.0E3
      IF (WL.GT.14200.0) GO TO 6090
        ZKAS=(((5.95244E-4*WLM-0.0204842)*WLM+0.164790)*WLM+0.178708)
     &       *WLM+0.680133E-2
      GO TO 6100
6090  WLM=16.149-WLM
      ZKAS=((0.273236E-2*WLM-0.411288E-1)*WLM+0.220190)*WLM**2+0.269818
6100  FACT=1.0-EXP((-TH)*28.54310E+3/WL)
C
C     TRANSITION BOUND-FREE= GELTMAN APJ. VOL. 136 P. 935 1962
C
      ALTHMB=ZKAS*4.158E-1*TH**2.5*EXP(1.726*TH)*FACT
C
C     TRANSITION FREE-FREE=T L JOHN (OCT.1963)
C
 6190   ALTHML=(WL/1.0E6)*(((-5.939*TH+11.934)*TH-3.2062)+(WL/1.0E3)
     &       *((-0.34592*TH+7.0355)*TH-0.40192))+((0.027039*TH-0.011493)
     &       *TH+0.0053666)
      ZZK(MMAX+1,1)=ALTHMB+ALTHML
C
C     II   H2-
C     H2- SOMMERVILLE= APJ. VOL. 139 P. 195 1963
6210  IF (TH.LT.0.5) GO TO 2050
      IF (WL.GE.3040.0) GO TO 2070
2050  ZZK(MMAX+2,1)=0.0
      GO TO 2080
2070  DKSQ=911.27/WL
        ZZK(MMAX+2,1)=(((0.09319*TH+2.857-0.9316/TH)/DKSQ-(2.6*TH+6.831
     &       -4.993/TH))/DKSQ+(35.29*TH-9.804-10.62/TH)-(74.52*TH-62.48
     &       +0.4679/TH)*DKSQ)*1.0E-29
C
C     III   H2+
C     H2+ BATES=HARVARD JUIN 1964  (RESULTATS *1.0E+39)
 2080 IF ((TH.LT.0.25).OR.((ZLAMIN.LT.WINV(1)).OR.(ZLAMIN.GT.WINV(46))))
     &      GO TO 1012
      BKT=3.19286E-2/TH
C     BKT=K*T EN RYDBERGS POUR H2+
C
      DO  J=1,46
      Zut1=U1(J)/BKT
      Zut2=-U2(J)/BKT
            if (Zut2.GT.-38) then
            EZut1=exp(Zut1)
            EZut2=exp(Zut2)
            OPNU(J)=2.51E-3*GRDM(J)*(EZut1-EZut2)
            else
                  if(Zut1.GT.-38) then
                  EZut1=exp(Zut1)
                  OPNU(J)=2.51E-3*GRDM(J)*EZut1
                  else
                  OPNU(J)=0
                  end if
            end if
      END DO
c
      DO 1013 J=1,46
      JJ=J
      IF (ABS(ZLAMIN-WINV(J)).LE.0.5) GO TO 1014
      IF (ZLAMIN.LT.WINV(J)) GO TO 1015
1013  CONTINUE
1014  ZZK(MMAX+3,1)=OPNU(JJ)
      GO TO 1016
C     INTERPOLATION LINEAIRE
 1015   ZZK(MMAX+3,1)=(OPNU(JJ-1)*WINV(JJ)-OPNU(JJ)*WINV(JJ-1)+(OPNU(JJ)
     &       -OPNU(JJ-1))*ZLAMIN)/(WINV(JJ)-WINV(JJ-1))
      GO TO 1016
1012  ZZK(MMAX+3,1)=0.0
C
C     CAS OU WL EST UNE DISCONTINUITE
1016  IF (JFZ.NE.2) GO TO 1017
      DO 1019 N=1,3
1019  ZZK(MMAX+N,2)=ZZK(MMAX+N,1)
C
C     IV   H
C     H UNSOLD (1955) PAGE 168
C     FACTEUR DE GAUNT FREE-FREE POUR H=GRANT M.N.,VOL.118
C     SYMBOLES CF VARDYA APJ.SUP. VOL. 8,P.277,1964
1017  IF (TH.GT.1.4) GO TO 1809
      DO 1855 K=1,4
      TRHOG(K)=SQRT(1.0+UL/YY(K))
      IF (TRHOG(K).GE.1.01) GO TO 1820
      IF (TRHOG(K).NE.1.0) TGAUNT(K)=2.0/(TRHOG(K)-1.0)
      IF (TRHOG(K).EQ.1.0) GO TO 1856
      TGAUNT(K)=0.5513289*ALOG(TGAUNT(K))
C     0.5513289=SQRT(3)/PI
      GO TO 1855
1856  TGAUNT(K)=0.0
      GO TO 1855
1820  IF (TRHOG(K).LE.1.8) GO TO 1830
      TEMPOR=(TRHOG(K)-1.0)*SQRT(GCONST/(YY(K)+UL))
      ANY=TEMPOR**(-0.6666667)
      TGAUNT(K)=(-0.01312*ANY+0.21775)*ANY+1.0
      GO TO 1855
1830  TEMPOR=0.2171473*ALOG(GCONST/(YY(K)+UL))
C
C     0.2171473=0.434294482/2
C
      IF ((TEMPOR.LT.ZLETAG(1)).OR.(TEMPOR.GT.ZLETAG(18))) GO TO 1847
C     INTERPOLATION A PARTIR DE LA TABLE 1 DE GRANT (1958)
      DO 1835 IR=1,12
      JR=IR
      IF (ABS(TRHOG(K)-RHOG(IR)).LE.1.0E-4) GO TO 1836
      IF  (TRHOG(K).LT.RHOG(IR)) GO TO 1837
1835  CONTINUE
1836  CARO=1.0
C
C     INTERPOLATION SUR LOG(-ETA) SEULEMENT
C
      GO TO 1838
1837  RHOG1=RHOG(JR-1)
      CARO=TRHOG(K)-RHOG1
1838  RHOG2=RHOG(JR)
      IF (CARO.EQ.1.0) DIFRO=1.0
      IF (CARO.NE.1.0) DIFRO=RHOG2-RHOG1
      DO 1845 IE=1,18
      JE=IE
      IF (ABS(TEMPOR-ZLETAG(IE)).LE.1.0E-4) GO TO 1846
      IF  (TEMPOR.LT.ZLETAG(IE)) GO TO 1848
1845  CONTINUE
1846  IF (CARO.EQ.1.0) GO TO 1850
      CAETA=1.0
C
C     INTERPOLATION SUR RHO SEULEMENT
C
      GO TO 1849
1848  ZLETA1=ZLETAG(JE-1)
      CAETA=TEMPOR-ZLETA1
1849  ZLETA2=ZLETAG(JE)
      IF(CAETA.EQ.1.0)  DIFETA=1.0
      IF(CAETA.NE.1.0)  DIFETA=ZLETA2-ZLETA1
      GO TO 1851
1850  TGAUNT(K)=G3D(JR,JE)
      GO TO 1855
 1851   TGAUNT(K)=((G3D(JR-1,JE-1)*(RHOG2-TRHOG(K))+G3D(JR,JE-1)*CARO)
     &       *(ZLETA2-TEMPOR)+(G3D(JR,JE)*CARO+G3D(JR-1,JE)
     &       *(RHOG2-TRHOG(K)))*CAETA)/DIFRO/DIFETA
      GO TO 1855
1847  WRITE (6,100)
100   FORMAT ('0 ON SORT DE LA TABLE DE GFF')
1855  CONTINUE
      G3=0.0
      DO 1860 K=1,4
1860  G3=G3+TGAUNT(K)*AA(K)
C     G3=FACTEUR DE GAUNT FREE FREE
      GO TO 4199
1809  ZZK(MMAX+4,1)=0.0
      JHYT=0
4199  DO 4200 I=1,JFZ
      IF (((I.EQ.1).AND.(JHYT.NE.0)).OR.(JH.EQ.1)) GO TO 4201
C
C     WL N'EST PAS UNE DISCONTINUITE DE H
C
      IF (I.EQ.2) ZZK(MMAX+4,2)=ZZK(MMAX+4,1)
      GO TO 1451
4201  SIGH=0.0
      JS=JSHYD(I)
      DO 1410 J=JS,19
1410  SIGH=SIGH+G2D(I,J)*ZEUH(J)
C     RAPPEL  G2D= FACTEUR DE GAUNT BOUND FREE
      BH=SIGH+(ZEUH(20)-(1.0-G3)*ZEMH)/(2*UH1)
      ZZK(MMAX+4,I)=AH*BH*STIMU3
C
C     V   HE+
C     HE+  VARDYA APJ.SUP. VOL. 8,P.277,1964
1451  IF (TH.GT.0.3) GO TO 1552
      SIGHEP=0.0
      DO 1462 J=1,19
      JJ=J
      IF (ABS(WL-ZLHEP(J)).LE.0.50) GO TO 1465
      IF (WL.LT.ZLHEP(J)) GO TO 1463
1462  CONTINUE
1463  JJS=JJ
      GO TO 1470
1465  JHEP=1
      IF (I.EQ.1) GO TO 1463
      JJS=JJ+1
1470  IF ((I.EQ.1).OR.(JHEP.EQ.1)) GO TO 1471
C
C     WL N'EST PAS UNE DISCONTINUITE DE HE+
C
      ZZK(MMAX+5,2)=ZZK(MMAX+5,1)
      GO TO 1554
1471  DO 1520 JJ=JJS,19
1520  SIGHEP=SIGHEP+ZEUHEP(JJ)
      BHEP=SIGHEP+ZEUHEP(20)/(2*UHEP1)
      ZZK(MMAX+5,I)=AHEP*BHEP*STIMU3
      GO TO 1554
1552  ZZK(MMAX+5,I)=0.0
C
C     VI   HE
C     HE  VARDYA=APJ. SUP. 80 VOL. 8 P. 277 JANVIER 1964
1554  IF (TH.LE.0.8) GO TO 5400
      ZZK(MMAX+6,I)=0.0
      GO TO 4200
5400  IF ((I.EQ.2).AND.(JH.EQ.1)) GO TO 5872
      SIGHEM=0.0
      SIGHE=0.0
      IF ((WL-ZLHEM(5)).GT.0.50) GO TO 5740
      DO 5460 K=1,5
      KK=K
      IF (ABS(WL-ZLHEM(K)).LE.0.50) GO TO 5490
      IF (WL.LT.ZLHEM(K)) GO TO 5470
5460  CONTINUE
5470  KKS=KK
      GO TO 5540
5490  JHEM=1
      IF (I.EQ.1) GO TO 5470
      KKS=KK+1
      IF (KKS.GT.5) GO TO 5740
5540  IF ((JHEM.EQ.1).OR.(I.EQ.1)) GO TO 5541
C
C     WL N'EST PAS = A UNE VALEUR DE ZLHEM
C     RAPPEL  ZLHEM=504,2601,3122,3422,3680 A.
C
      GO TO 5741
5541  DO 5730 K=KKS,5
      GO TO (5560,5560,5560,5620,5680),K
5560  IF (K.EQ.2) ZNL1=ZNL
      IF (K.NE.2) ZNL1=1.0
        ANU=EXP(((((C1(K)*ZNL+C2(K))*ZNL+C3(K))*ZNL+C4(K))*ZNL1+C5(K))
     &       *ZNL1+C6(K))*1.0E-18
      GO TO 5730
5620  N=1
C
C     GOLDBERG APJ. VOL. 90 P. 414 1939 ET UNDERHILL PUB. COP. OBS. N0.
C
5621  IF (ABS(WL-ZLHEM(3+N)).GT.0.50) GO TO 5640
C     NIVEAUX 4 A 7 DE HE1
      ANU=AN(N)/WL+EXPON(N)
      GO TO 5730
5640  ZK=1.097224E-3*ZLHEM(3+N)*WL/(ZLHEM(3+N)-WL)
      RK=SQRT(ZK)
      UK=1.0+CUK(N)*ZK
        ANU=(COTE(N)/(WL*(1.0-EXP(-6.283185*RK)))*(ZK/UK   )**6
     &       *((1.0+ZK)/UK)*((4.0+ZK)/UK)*EXP(-4.0*RK*ATAN(1.0
     &       /(SNIV(N)*RK))))+EXPON(N)
      GO TO 5730
5680  N=2
      GO TO 5621
5730  SIGHEM=SIGHEM+ANU*ZEXPM(K)
      BHEM=SIGHEM*STIMU
      GO TO 5741
5740  BHEM=0.0
C     NIVEAUX 8 ET SQ (N.GE.3)
5741  DO 5780 L=3,9
      LL=L
      IF (ABS(WL-ZLHE(L)).LE.0.50) GO TO 5810
      IF  (WL.LT.ZLHE(L)) GO TO 5790
5780  CONTINUE
5790  LLS=LL
      GO TO 5860
5810  JHE=1
      IF (I.EQ.1) GO TO 5790
      LLS=LL+1
5860  IF ((I.EQ.1).OR.(JHE.EQ.1)) GO TO 5861
C
C     WL N'EST PAS = A UNE VALEUR DE ZLHE
C
      GO TO 5871
5861  DO 5870 L=LLS,9
5870  SIGHE=SIGHE+ZEXP(L)*ZEFF4(L)
      BHE=SIGHE+(1807.240*ZEXP(10)-0.8072399*ZEUHE1)/(2*UHE1)
5871  ZZK(MMAX+6,I)=AHE*BHE*STIMU3+BHEM
      GO TO 4200
C
C     WL N'EST PAS UNE DISCONTINUITE DE HE
C
5872  ZZK(MMAX+6,2)=ZZK(MMAX+6,1)
4200  CONTINUE
      RETURN
      END


      SUBROUTINE GAUNTH (WL)
C     A.M COLLE   19/8/69
C     DETERMINATION DU FACTEUR DE GAUNT BOUND FREE POUR L HYDROGENE
      REAL*8 VARIAVEL
      COMMON /GBF/ZLH(19)/GBFH/G2D(2,19),JSHYD(2),JH/ABSO3/JFZ
      JH=0
      DO 1410 I=1,JFZ
      DO 1332 J=1,19
      JJ=J
      IF (ABS(WL-ZLH(J)).LE.0.5) GO TO 1335
      IF (WL.LT.ZLH(J)) GO TO 1333
1332  CONTINUE
1333  IF (I.NE.2) GO TO 1334
C
C     CE N'EST PAS UNE DISCONTINUITE DE L'HYDROGENE
C
      DO 1336 J=1,19
1336  G2D(2,J)=G2D(1,J)
      GO TO 1420
1334  JS=JJ
      GO TO 1340
1335  JH=1
C
C     C'EST UNE DISCONTINUITE DE L'HYDROGENE
C
      IF (I.EQ.1) GO TO 1334
      JS=JJ+1
1340  JSHYD(I)=JS
      DO 1410 J=JS,19
      ZJ=J
      IF (J.GT.7) GO TO 1400
      COND=ZLH(J)-WL
      IF (ABS(COND).LE.0.50) GO TO 1122
      IF (COND.LT.0.0) GO TO 1410
      ZQ=WL*J**2/COND
      VARIAVEL = 1E-37
            IF(ZQ.GT.VARIAVEL) then
            RK=SQRT(ZQ)
            else
            ZQ=0
            RK=0
            END IF
      GO TO (1111,1113,1115,1117,1119,2000,2010),J
C
C     MENZEL ET PEKERIS=MON. NOT. VOL. 96 P. 77 1935
C
1111  DELTA=8.*RK/SQRT(ZQ+1.0)
      GO TO 1120
1113  DELTA=(16.*RK*(3.*ZQ+4.)*(5.*ZQ+4.))/(ZQ+4.)**2.5
      GO TO 1120
 1115   DELTA=(24.*RK*((13.*ZQ+78.)*ZQ+81.)*((29.*ZQ+126.)*ZQ+81.))
     &       /(ZQ+9.)**4.5
      GO TO 1120
 1117   DELTA=32.*RK*(((197.*ZQ+3152.)*ZQ+13056.)*ZQ+12288.)*
     &       (((539.*ZQ+6800.)*ZQ+20736.)*ZQ+12288.)/(9.*(ZQ+16.)**6.5)
      GO TO 1120
 1119   DELTA=40.*RK*((((1083.*ZQ+36100.)*ZQ+372250.)*ZQ+1312500.)*ZQ
     &       +1171875.)*((((3467.*ZQ+95700.)*ZQ+786250.)*ZQ+2062500.)*ZQ
     &       +1171875.)/(9.*(ZQ+25.)**8.5)
      GO TO 1120
C
C     HAGIHARA AND SOMA=J.OF ASTR. AND GEOPHYS. JAPANESE VOL. 20 P. 59 1
C
2000  ZP=(ZQ+36.)**5.25
        DELTA=48.*RK*((((((38081.*ZQ+1953540.)*ZQ+3348086.E1)*ZQ+
     &       2262816.E2)*ZQ+5458752.E2)*ZQ+3023309.E2)/ZP)
     &       *((((((10471.*ZQ+628260.)*ZQ+1290902.E1)*ZQ+1087085.E2)
     &       *ZQ+34992.0E4)*ZQ+3023309.E2)/25./ZP)
      GO TO 1120
2010  ZP=(ZQ+49.)**6.25
        DELTA=56.*RK*(((((((56740.9*ZQ+5560608.)*ZQ+1993433.E2)*ZQ
     &   +3248060.E3)*ZQ+2428999.E4)*ZQ+7372604.E4)*ZQ+6228579.E4)/ZP)
     &   *(((((((229742.5*ZQ+1968907.E1)*ZQ+6067219.E2)*ZQ+8290160.E3)
     &   *ZQ+5002406.E4)*ZQ+1144025.E5)*ZQ+6228579.E4)/20.25/ZP)
1120    G2D(I,J)=5.441398*RK*J*EXP(-4.*RK*ATAN(ZJ/RK))*DELTA
     &    /(SQRT(ZQ+ZJ**2)*(1.-EXP(-6.283185*RK)))
      GO TO 1410
1122  GO TO (1123,1125,1127,1129,1131,2020,2030),J
1123  G2D(I,J)=0.7973
      GO TO 1410
1125  G2D(I,J)=0.8762
      GO TO 1410
1127  G2D(I,J)=0.9075
      GO TO 1410
1129  G2D(I,J)=0.9247
      GO TO 1410
1131  G2D(I,J)=0.9358
      GO TO 1410
2020  G2D(I,J)=0.9436
      GO TO 1410
2030  G2D(I,J)=0.9494
      GO TO 1410
1400  G2D(I,J)=1.0
1410  CONTINUE
1420  RETURN
      END

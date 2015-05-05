! This file is part of PFANT.
! 
! PFANT is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! PFANT is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with PFANT.  If not, see <http://www.gnu.org/licenses/>.

C> Module ABSORU
C>
C> Public: only subroutine ABSORU() and variable absoru_ZNH
      MODULE ABSORU

      !=====
      ! Outputs
      !=====
      REAL, DIMENSION(12) :: absoru_ZNH  ! [SAPU]



      ! Down here all private
      ! vvvvvvvvvvvvvvvvvvvvv

      ! Subroutines
      PRIVATE GAUNTH, TEMPA, SAHATH, ATHYHE, IONIPE

      PRIVATE

      ! TODO Identify outputs and inputs
      ! TODO CHECK ALL TYPES
      ! TODO CHECK ALL SIZES
      ! TODO COMMENT INDICATING ORIGINAL COMMON BLOCK FOR EACH VARIABLE
      
      
      ! Name of original COMMON block was put between square brackets ("[]"). These annotations
      ! can be removed with time; they are mostly for developers who were familiar with the COMMON
      ! blocks that existed.
      
      INTEGER, DIMENSION(2) :: au_JSHYD  ! [GBFH]
      INTEGER au_JH,  ! [GBFH]
     +        au_JFZ  ! [ABSO3]
      
      REAL au_AHE, au_AH, au_AHEP, au_UH1, au_ZEMH, au_UHEP1, au_UHE1, ! [TETA]
     + au_ZEUHE1,  ! [TETA]
     + au_UL,  ! [THE]
     + au_AC, au_STIMU,
     + au_AVM, au_ZNU1, au_ZNU2, au_ZNU3, au_ZMUZE,
     + au_ZMU, au_PG,  ! [SAPDIV]
     + au_PE, au_RHO, au_TOC  ! [SAPU]
     
      REAL, DIMENSION(19) :: au_ZLH,  ! [GBF]
     + au_ZLHEP
      REAL, DIMENSION(2, 19) :: au_G2D  ! [GBFH]
      
      REAL, DIMENSION(5) :: au_ZLHEM,  ! [TEHE]
     + au_STWTM, ! [TEMPE]
     + au_ZEXPM  ! [THE]
      REAL, DIMENSION(10) :: au_ZLHE,  ! [TEHE]
     + au_ZEXP,  ! [THE]
     + au_ZEFF4
      REAL, DIMENSION(20) :: au_ZEUH, au_ZEUHEP  ! [THE]
      REAL, DIMENSION(11) :: au_ZK
      REAL, DIMENSION(30, 9) :: au_ZKM, au_AC2
      REAL, DIMENSION(46) :: au_GRDM, au_U2, au_WINV
      REAL, DIMENSION(4) :: au_YY, au_AA
      REAL, DIMENSION(18) :: au_ZLETAG
      REAL, DIMENSION(12,18) :: au_G3D
      REAL, DIMENSION(126) :: au_RHOG
      REAL, DIMENSION(3) :: au_AC1
      REAL, DIMENSION(30) :: au_ZNU  ! [SAPE]





      CONTAINS

C>-------------------------------------------------------------------------------
C> Calculates the "continuum absorption"
C>
C> Note1: 1/3 things that need to be changed to include scattering (other software
C>       e.g. Upsalla already have this)
C>
C> Note2: 1/3 atmospheric models 50e6 cannot be calculates, would tyake months.
C>        So, one idea is to include opacity model tables (Upsalla; MARCS model).
C>
      SUBROUTINE ABSORU(WL,TH,ZLPE,CALLAM,CALTH,CALPE,CALMET,CALU,CALSO
     1                  R,KKK,TOTKAP)
      INTEGER*4 CALU,CALMET,CALLAM,CALTH,CALPE,PMAX,CALSOR
      DIMENSION ZZKK(11,2),TOTKAP(2),DIF(2,3),SCATH(2),ZZK(11,2),SCAT(2)


      DATA DIF/5.799E-13,8.14E-13,1.422E-6,1.28E-6,2.784,1.61/
      
      ILT = CALLAM
      IF (CALPE .EQ. 2) GO TO 9003

C>
C>     au_AVM=MASSE ATOMIQUE MOYENNE DES ELEMENTS PLUS LOURDS QUE L'HELIUM
C>     SUM1=SOMME DES ABONDANCES DES METAUX
C>
      SUM1 = 0.0
      SUM2 = 0.0
      DO 4100 I=1,absoru2_NM
        SUM1 = SUM1+absoru2_ZP(I)
4100    SUM2 = SUM2+absoru2_ZP(I)*absoru2_ZM(I)
      au_AVM = SUM2/SUM1

C>
C>     au_ZNU1,au_ZNU2,au_ZNU3=SUCCESSIVEMENT FRACTION D'(H,HE,METAL) PAR NOMBRE T
C>
      DO 4110 I = 1,absoru2_NM
4110    au_ZNU(I) = absoru2_ZP(I)/SUM1

      au_ZNU1 = 1.0/(1.0+absoru2_ABMET+absoru2_ABHEL)
      au_ZNU2 = au_ZNU1*absoru2_ABHEL
      au_ZNU3 = au_ZNU1*absoru2_ABMET
      au_ZMUZE = 1.008*au_ZNU1+4.003*au_ZNU2+au_AVM*au_ZNU3
      
9005  IF ((CALTH.EQ.2).AND.(CALLAM.EQ.2)) GO TO 5016
      IF (CALTH.EQ.2) GO TO 9001
      IF (TH.LE.0.8) ITH=1
      IF (TH.GT.0.8) ITH=2
      NSET = absoru2_NUMSET(ITH)-1
      
9001  DO 6500 I=1,NSET
        IF (ABS(WL-absoru2_WI(I+1,ITH)).LE.0.50) GO TO 8000
        IF (WL.LT.absoru2_WI(I+1,ITH)) GO TO 7000
6500  CONTINUE

7000  au_JFZ=1
      GO TO 9002
8000  au_JFZ=2

C
C     DIFFUSION DE RAYLEIGH PAR H ET H2 (DALGARNO) HARVARD JUIN 1964
C     SCATH(1)=DIFFUSION DE H
C     SCATH(2)=DIFFUSION DE H2
C
9002  DO 9023 I=1,2
        IF (I.EQ.2) GO TO 9020
        IF (WL.GT.1026.0) GO TO 9021
        SCATH(1)=4.0E-24
        GO TO 9023
        
9020    IF (WL.GT.1200.0) GO TO 9021
        WLH=1200.0
        GO TO 9022
9021    WLH=WL
9022    WL4=WLH**4
        SCATH(I)=(DIF(I,1)+DIF(I,2)/SQRT(WL4)+DIF(I,3)/WL4)/WL4
9023  CONTINUE

      GO TO 5018
      
5016  IF ((au_JFZ.NE.2).OR.(ILT.EQ.1)) GO TO 5017
      ILT=CALLAM-1
5018  CALL GAUNTH (WL)
5017  CALL TEMPA(WL,TH,CALTH,CALLAM)
      IF (CALTH.EQ.2) GO TO 9007
      CALL SAHATH(TH)
9007  IF ((CALTH.EQ.2).AND.(CALLAM.EQ.2)) GO TO 9006
      CALL ATHYHE (WL,TH,CALTH,CALLAM,ZZK)
9006  IF (CALMET.EQ.1) GO TO 9003
9003  CALL IONIPE (TH,ZLPE,CALTH,CALMET)
      MM=absoru2_NMETA+1
      MMM=absoru2_NMETA+6
      SCATEL=9.559063E-13*au_PE*TH
      
C
C     9.559063E-13=4.81815E-9/5040.39 ET 4.81815E-9=6.625E-25/1.38024E-1
C     =ELECTRON SCATTERING/(K*T)  UNSOLD P. 180 1955
C
      GO TO (9008,9009), CALSOR
9009  WRITE (6,86) (absoru2_IUNITE(I),I=1,2)
86    FORMAT ('0LAMBDA KKK   C1'7X'MG'7X'SI1'7X'AL1'8X'H-'7X'H2-'7X'H2+'
     19X'H'7X'HE+'8X'HE'5X'K TOTAL/',2A4)
9008  KKK=au_JFZ
      MIN=MM
      
      DO 9015 I=1,au_JFZ
        DO 9015 M=1,absoru2_NMETA
9015      ZZKK(M,I)=0.0

1750  TOTKAP(2)=0.
      IF (CALU.EQ.1) UNIT=au_RHO
      IF (CALU.EQ.2) UNIT=au_TOC

C     RAPPEL  au_TOC=NBRE DE NOYAUX DE H PAR CM3

      SCATEL=SCATEL/UNIT
      DO 4220 I=1,KKK
        TOTKAP(I)=0.0
        SCAT(1)=SCATH(1)*absoru_ZNH(absoru2_NMETA+4)/UNIT
        SCAT(2)=SCATH(2)*absoru_ZNH(absoru2_NMETA+2)/UNIT
        DO 4221 M=MIN,MMM
C         LES absoru_ZNH POUR LES METAUX SONT EN CM-3*1.0E-18
          IF ((M.NE.(absoru2_NMETA+1)).AND.(M.NE.(absoru2_NMETA+3)))
     +     GO TO 4222
          IF (M.EQ.(absoru2_NMETA+1))
     +     ZZKK(M,I)=ZZK(M,I)*(absoru_ZNH(absoru2_NMETA+4)*au_PE*1.E-26)/UNIT
          IF (M.EQ.(absoru2_NMETA+3))
     +     ZZKK(M,I)=ZZK(M,I)*((absoru_ZNH(absoru2_NMETA+4)*1.E-19)*(absoru_ZNH(MMAX+7)*1.0E-20))/UNIT
          GO TO 4221
4222      ZZKK(M,I)=ZZK(M,I)*absoru_ZNH(M)/UNIT
          IF (M.EQ.(absoru2_NMETA+2)) ZZKK(M,I)=ZZKK(M,I)*au_PE
4221      TOTKAP(I)=TOTKAP(I)+ZZKK(M,I)
        TOTKAP(I)=(TOTKAP(I)+SCATEL+SCAT(1)+SCAT(2))
        GO TO (4220,9010),CALSOR
9010    WRITE (6,87) WL,KKK,(ZZKK(M,I),M=1,MMM),TOTKAP(I)
87      FORMAT (F9.1,I2,1P12E10.2)
4220  CONTINUE

      GO TO (9011,9012),CALSOR
9012  WRITE (6,89) SCATEL,SCAT(1),SCAT(2),au_RHO,au_TOC,ZLPE,TH
89    FORMAT ('0SIG(E)='1PE11.4,' SIG(H)='E11.4,' SIG(H2)='E11.4,' DENSI
     1TE='E11.4,' NBR.NOYAU D H/CM3='E11.4,' LOG10PE='0PF5.2,' TETA='F5.
     22)
9011  CONTINUE
      RETURN
      END











C>-------------------------------------------------------------------------------
C>
C> Calcalutes the "Gaunth factor": multiplicative correction to the continuous absorption
C> (i.e., a statistical weight)
C>
C> Reference: J.A.Gaunth 1930.
C>
C> A.M COLLE   19/8/69
C>
      SUBROUTINE GAUNTH (WL)

C
C     DETERMINATION DU FACTEUR DE GAUNT BOUND FREE POUR L HYDROGENE
C
      au_JH = 0
      DO 1410 I=1,au_JFZ
      
        DO 1332 J=1,19
          JJ=J
          IF (ABS(WL-au_ZLH(J)).LE.0.5) GO TO 1335
          IF (WL.LT.au_ZLH(J)) GO TO 1333
1332    CONTINUE

1333    IF (I .NE. 2) GO TO 1334
C       
C       CE N'EST PAS UNE DISCONTINUITE DE L'HYDROGENE
C       

        DO 1336 J=1,19
1336      au_G2D(2,J)=au_G2D(1,J)

        GO TO 1420
        
1334    JS=JJ
        GO TO 1340
        
1335    au_JH=1
C       
C       C'EST UNE DISCONTINUITE DE L'HYDROGENE
C       
        IF (I .EQ. 1) GO TO 1334
        
        JS = JJ+1
        
1340    au_JSHYD(I) = JS

        DO 1410 J=JS,19
          ZJ=J
          IF (J.GT.7) GO TO 1400
          COND=au_ZLH(J)-WL
          IF (ABS(COND).LE.0.50) GO TO 1122
          IF (COND.LT.0.0) GO TO 1410
          
          
          !=====
          ! Assignment of au_G2D(I,J), alternative 1
          !=====
          
          ZQ=WL*J**2/COND
          RK=SQRT(ZQ)
          GO TO (1111,1113,1115,1117,1119,2000,2010), J
          
C         
C         MENZEL ET PEKERIS=MON. NOT. VOL. 96 P. 77 1935
C         
1111      DELTA=8.*RK/SQRT(ZQ+1.0)
          GO TO 1120
          
1113      DELTA=(16.*RK*(3.*ZQ+4.)*(5.*ZQ+4.))/(ZQ+4.)**2.5
          GO TO 1120
          
1115      DELTA=(24.*RK*((13.*ZQ+78.)*ZQ+81.)*((29.*ZQ+126.)*ZQ+81.))/(ZQ+9.
     1    )**4.5
          GO TO 1120
          
1117      DELTA=32.*RK*(((197.*ZQ+3152.)*ZQ+13056.)*ZQ+12288.)*(((539.*ZQ+68
     1    00.)*ZQ+20736.)*ZQ+12288.)/(9.*(ZQ+16.)**6.5)
          GO TO 1120
          
1119      DELTA=40.*RK*((((1083.*ZQ+36100.)*ZQ+372250.)*ZQ+1312500.)*ZQ+1171
     1    875.)*((((3467.*ZQ+95700.)*ZQ+786250.)*ZQ+2062500.)*ZQ+1171875.)/(
     2    9.*(ZQ+25.)**8.5)
          GO TO 1120
          
C         
C         HAGIHARA AND SOMA=J.OF ASTR. AND GEOPHYS. JAPANESE VOL. 20 P. 59 1
C         
2000      ZP=(ZQ+36.)**5.25
          DELTA=48.*RK*((((((38081.*ZQ+1953540.)*ZQ+3348086.E1)*ZQ+2262816.E
     1    2)*ZQ+5458752.E2)*ZQ+3023309.E2)/ZP)*((((((10471.*ZQ+628260.)*ZQ+1
     2    290902.E1)*ZQ+1087085.E2)*ZQ+34992.0E4)*ZQ+3023309.E2)/25./ZP)
          GO TO 1120
          
2010      ZP=(ZQ+49.)**6.25
          DELTA=56.*RK*(((((((56740.9*ZQ+5560608.)*ZQ+1993433.E2)*ZQ+3248060
     1    .E3)*ZQ+2428999.E4)*ZQ+7372604.E4)*ZQ+6228579.E4)/ZP)*(((((((22974
     2    2.5*ZQ+1968907.E1)*ZQ+6067219.E2)*ZQ+8290160.E3)*ZQ+5002406.E4)*ZQ
     3    +1144025.E5)*ZQ+6228579.E4)/20.25/ZP)
     
1120      au_G2D(I,J)=5.441398*RK*J*EXP(-4.*RK*ATAN(ZJ/RK))*DELTA/(SQRT(ZQ+ZJ**
     1    2)*(1.-EXP(-6.283185*RK)))
          GO TO 1410
          
          
          !=====
          ! Assignment of au_G2D(I,J), alternative 2
          !=====
          
1122      GO TO (1123,1125,1127,1129,1131,2020,2030), J
1123      au_G2D(I,J)=0.7973
          GO TO 1410
1125      au_G2D(I,J)=0.8762
          GO TO 1410
1127      au_G2D(I,J)=0.9075
          GO TO 1410
1129      au_G2D(I,J)=0.9247
          GO TO 1410
1131      au_G2D(I,J)=0.9358
          GO TO 1410
2020      au_G2D(I,J)=0.9436
          GO TO 1410
2030      au_G2D(I,J)=0.9494
          GO TO 1410
1400      au_G2D(I,J)=1.0

1410  CONTINUE
1420  RETURN
      END


      
      
      
      
      




C-------------------------------------------------------------------------------
C>
C>     A.M COLLE   8/5/69
C>
C>     HCBKTM=(H*C/K*T)*1.0E8
C>     0.0010967876=CONSTANTE DE RYDBERG POUR H  *1.0E-8  ALLEN 1963
C>     0.0043890867=CONSTANTE DE RYDBERG POUR HE+*1.0E-8  MOORE 1950 (HE4
C>     au_AHE =POUR HE 4*C/T**3
C>     au_AH  =POUR H   C*Z**4/T**3  AVEC Z=1
C>     au_AHEP=POUR HE+ C*Z**4/T**3  AVEC Z=2
C>     C=64*PI**4*ME*E**10/(3*RAC(3)*C*H**3*K**3)
C>     ME=9.10E-28,E**10=4.8E-10,K=1.38024E-16,H=6.6237E-27,C=2.99791E+10
C>
      SUBROUTINE TEMPA(WL,TH,CALTH,CALLAM)
      INTEGER*4 CALLAM,CALTH

      IF (CALTH.EQ.2) GO TO 1001
      
      HCBKTM  = 0.2854306E-3*TH*1.0E8
      au_AHE  = 0.9717088E-12*TH**3
      au_AH   = 0.2429272E-12*TH**3
      au_AHEP = 16.*au_AH
      au_UH1  = 1.096788E-3*HCBKTM
      au_ZEMH = EXP(-au_UH1)
      
      IF (TH.GT.1.4) GO TO 1001
      
      DO 1304 J=1,20
        UH=au_UH1/J**2
1304    au_ZEUH(J)=EXP(UH-au_UH1)/J**3

      au_ZEUH(20) = au_ZEUH(20)*8000.  ! ISSUE why this (ask MT)?     
      au_UHEP1 = 4.389087E-3*HCBKTM
      IF (TH .GT. 0.3) GO TO 5290
      
      DO 1313 J=1,20
        UHEP=au_UHEP1/J**2
1313    au_ZEUHEP(J) = EXP(UHEP-au_UHEP1)/J**3

      au_ZEUHEP(20) = au_ZEUHEP(20)*8000.
      
5290  au_UHE1 = HCBKTM/504.3
      au_ZEUHE1 = EXP(-au_UHE1)
      IF (TH .GT. 0.8) GO TO 1001
      
      COMHE=-HCBKTM*(1.0/au_ZLHEM(1))
      DO 5310 K=1,5
5310    au_ZEXPM(K)=EXP(COMHE+HCBKTM*(1.0/au_ZLHEM(K)))*au_STWTM(K)
      DO 5340 L=3,10
5340    au_ZEXP(L)= EXP(COMHE+HCBKTM*(1.0/au_ZLHE(L)))/L**3

1001  IF ((CALLAM.EQ.2).AND.(CALTH.EQ.2)) GO TO 5010

      au_UL = HCBKTM/WL
      
5010  RETURN
      END

      
      
      
      
      
      




C-------------------------------------------------------------------------------
C
C> SAHA's equation: ionization equilibrium: relative number of atoms in each
C> ionization state
C>
C> LOI DE SAHA=LOG((absoru2_NR+1)/absoru2_NR)*modeles_PE= -POT.ION.*TH+5/2*LOG(T)-0.4772+FONC
C> LES FONCTIONS DE PARTITION (L0G(2UR+1)/UR) SONT INCLUSES DANS LES
C> CONSTANTES AJOUTEES A TEMPOR POUR H ET HE LES AUTRES SONT LUES
C> 31.303644,1.7200311,56.597541,125.26753,SONT RESPECTIVEMENT LES
C> POTENTIELS D'IONISATION DE (H,H-,HE,HE+)*2.3025851
C>
C>     A.M COLLE   13/5/69
C>
      SUBROUTINE SAHATH(TH)
      DIMENSION POTION(6),C1(3),C2(3),C3(3),C4(3)

      COMMON /SAHT/ au_ZK(11), au_ZKM(30,9)

      DATA C1 /0.0,-7.526612E-3,5.708280E-2/,
     1     C2 /0.0,1.293852E-1,-1.823574E-1/,
     2     C3 /0.0,-11.34061,-6.434060/,
     3     C4 /0.0,28.85946,25.80507/,
     4 POTION /2-1.720031, 0.0, 0.0, 31.30364, 125.2675, -56.59754/

      DO 1 N=2,3
1       au_ZK(absoru2_NMETA+N)=EXP(((C1(N)*TH+C2(N))*TH+C3(N))*TH+C4(N))

      TEMPOR=2.5*ALOG(5040.39/TH)
      TEMPO=TEMPOR-1.098794
      DO 2 N=4,5
      au_ZK(absoru2_NMETA+N)=POTION(N)*TH-TEMPO
      IF (N.EQ.4) GO TO 12
      IF  (au_ZK(absoru2_NMETA+5).LT.100.0) GO TO 12
      au_ZK(absoru2_NMETA+5)=0.0
      GO TO 2
12    au_ZK(absoru2_NMETA+N)=EXP(-au_ZK(absoru2_NMETA+N))
2     CONTINUE
      DO 2270 J=1,absoru2_NM
      NRR=absoru2_NR(J)
      DO 2270 I=1,NRR
      au_ZKM(J,I)=TH*absoru2_XI(J,I)-absoru2_PF(J,I)-TEMPO
      IF (au_ZKM(J,I).LT.100.0) GO TO 2269
      au_ZKM(J,I)=0.0
      GO TO 2270
2269  au_ZKM(J,I)=EXP(-au_ZKM(J,I))
2270  CONTINUE
      TEMPO=TEMPOR+0.2875929
      DO 3 N=1,6,5
3     au_ZK(absoru2_NMETA+N)=EXP( POTION(N)*TH+TEMPO)
      RETURN
      END



I AM TIREd!!!!!!
COntinuation:
make header for this module
with all data
figure out what bk and main wants (maybe shouldn't have replaced things here...)
no worries, since I have to track down BK()'s inputs and outputs anyway
remove au_* prefixes

      BLOCK DATA
C
C     BLOCK DATA POUR LE SOUS PROGRAMME  ABSORU POUR LES RAIES D'HYDROGE
C     A.M COLLE  12/08/70
      DIMENSION G3D1(12,9), G3D2(12,9)

      COMMON /HYHE/ au_GRDM(46), U1(46), au_U2(46), au_WINV(46), au_YY(4),
     1              au_ZLETAG(18), au_G3D(12,18), au_AA(4), au_ZEFF4(10),
     2              au_RHOG(12), au_ZLHEP(19)
      COMMON /TEHE/ au_ZLHEM(5), au_ZLHE(10)
      COMMON /TEMPE/ au_STWTM(5)

      EQUIVALENCE (au_G3D(1,1),G3D1(1,1)),(au_G3D(1,10),G3D2(1,1))

      DATA au_WINV/361.9,429.9,514.4,615.3,733.7,869.7,1028.7,1226.2,1460.9
     1,1737.3,2044.4,2407.4,2851.6,3378.1,3986.8,4677.7,5477.3,6442.5,75
     274.3,8872.9,10338.2,12064.6,14049.7,16352.9,18996.2,22056.2,25576.
     38,29634.9,34307.2,39703.3,45943.9,53171.7,61529.1,71213.6,82433.7,
     495441.4,110445.3,127774.4,147406.7,169671.2,194568.0,221877.7,2516
     500.4,282968.2,312800.5,329032.7/
      DATA au_GRDM/1729.881 ,1591.634 ,1450.598 ,1317.928 ,1198.805 ,1091.6
     134 ,994.4223,896.8127,805.5777,722.7092,649.4024,583.2669,517.9283
     2,457.7689,405.1793,358.9641,316.3347,275.7371,238.9641,206.6136,18
     30.4781,153.5857,130.4781,109.6813,91.9920,76.1753,62.7092,50.9960,
     440.9562,32.5498,25.4263,19.6693,14.9920,11.2470,8.2869,5.9960,4.23
     511,2.8900,1.9020,1.1733,0.6781,0.3544,0.1605,0.0602,0.0171,0.0000/
      DATA U1/0.00263,0.00286,0.00322,0.00372,0.00435,0.00511,0.00600,0.
     100701,0.00821,0.00958,0.01190,0.01305,0.01522,0.01772,0.02061,0.02
     2394,0.02775,0.03207,0.03699,0.04257,0.04884,0.05584,0.06367,0.0722
     39,0.08180,0.09216,0.10340,0.11542,0.12815,0.14147,0.15511,0.16871,
     40.18167,0.19309,0.20167,0.20525,0.20052,0.18186,0.13996,0.05794,-0
     5.09644,-0.39105,-0.99032,-2.39840,-7.14260,-85.0/
      DATA au_U2/0.00114,0.00124,0.00145,0.00178,0.00221,0.00277,0.00342,0.
     100416,0.00508,0.00615,0.00745,0.00899,0.01083,0.01302,0.01561,0.01
     2869,0.02237,0.02676,0.03195,0.03810,0.04540,0.05412,0.06445,0.0767
     36,0.09140,0.10889,0.12977,0.15473,0.18466,0.22057,0.26382,0.31606,
     40.37932,0.45618,0.54997,0.66493,0.80665,0.98279,1.20442,1.48940,1.
     587040,2.41450,3.28470,4.97840,9.99460,85.0/
      DATA au_ZLHEM/504.3,2601.0,3122.0,3422.0,3680.0/,au_STWTM/1.0,3.0,1.0,9.
     10,3.0/,au_ZLHE/0.0,0.0,7932.0,14380.0,22535.0,32513.0,44313.0,57936.0
     2,73383.0,90603.0/,au_ZEFF4/0.0,0.0,1.069373 ,1.028328 ,1.022291 ,1.01
     38358,1.015639,1.013614,1.012019,1.011869/,au_ZLH/911.8,3647.0,8205.9,
     414588.2,22794.1,32823.5,44676.4,58352.9,73852.8,91176.3,110323.4,1
     531293.9,154088.0,178705.6,205146.7,233411.4,263499.6,295411.3,3291
     646.5/,au_ZLHEP/227.8,911.8,2050.6,3645.6,5696.2,8202.5,11164.5,14582.
     73,18455.7,22784.8,27569.6,32810.1,38506.3,44658.2,51265.8,58329.0,
     865848.0,73822.6,82253.0/
      DATAG3D1/2.885,2.419,2.047,1.679,1.468,1.323,1.212,1.124,1.051,0.9
     189,0.810,0.693,2.906,2.420,2.049,1.684,1.474,1.330,1.220,1.133,1.0
     261,1.000,0.824,0.708,2.912,2.430,2.072,1.723,1.527,1.395,1.296,1.2
     318,1.155,1.102,0.951,0.856,2.892,2.423,2.082,1.760,1.583,1.469,1.3
     485,1.320,1.268,1.226,1.111,1.045,2.815,2.365,2.046,1.755,1.604,1.5
     507,1.438,1.387,1.346,1.314,1.230,1.185,2.715,2.280,1.978,1.709,1.5
     673,1.488,1.428,1.383,1.348,1.320,1.249,1.208,2.615,2.194,1.906,1.6
     754,1.530,1.452,1.398,1.357,1.326,1.303,1.237,1.202,2.231,1.868,1.6
     829,1.440,1.352,1.298,1.261,1.235,1.215,1.198,1.158,1.136,1.955,1.6
     935,1.445,1.303,1.238,1.201,1.175,1.157,1.144,1.133,1.106,1.091/
      DATA G3D2/1.807,1.518,1.357,1.239,1.187,1.157,1.137,1.123,1.112,1.
     1104,1.082,1.065,1.707,1.446,1.303,1.201,1.157,1.131,1.115,1.103,1.
     2094,1.087,1.069,1.054,1.634,1.394,1.266,1.175,1.136,1.114,1.100,1.
     3089,1.081,1.075,1.056,1.046,1.579,1.357,1.239,1.157,1.121,1.102,1.
     4088,1.079,1.072,1.067,1.049,1.042,1.497,1.302,1.201,1.131,1.101,1.
     5085,1.073,1.066,1.060,1.055,1.042,1.034,1.442,1.265,1.175,1.113,1.
     6088,1.073,1.064,1.057,1.052,1.046,1.035,1.030,1.400,1.237,1.156,1.
     7101,1.078,1.065,1.057,1.051,1.045,1.042,1.032,1.026,1.367,1.217,1.
     8142,1.091,1.071,1.059,1.051,1.045,1.041,1.037,1.029,1.024,1.342,1.
     9200,1.130,1.084,1.065,1.053,1.047,1.042,1.037,1.034,1.026,1.022/
      DATA au_RHOG/1.010,1.025,1.050,1.100,1.150,1.200,1.250,1.300,1.350,1.
     1400,1.600,1.800/,au_ZLETAG/-3.0000,-2.0000,-1.0000,-0.6021,-0.3010,-0
     2.1249,0.0000,0.3979,0.6990,0.8751,1.0000,1.0969,1.1761,1.3010,1.39
     379,1.4771,1.5441,1.6021/,au_YY/0.3225,1.7458,4.5366,9.3951/,au_AA/0.6032
     4,0.3574,0.0389,0.0005/
C
C     DONNEES POUR H2+ =TABLE DE BATES(1952) COMPLETEE PAR HARVARD(1964)
C     ------------------
C     au_WINV=NU/C,au_GRDM=(-NU/DNU/DR)*R*D(R),U1=-U(1S SIGMA/R),au_U2=U(+2P SIGM
C     DONNEES POUR L'HELIUM NEUTRE
C     ----------------------------
C     au_ZLHEM(K)ET au_STWTM(K)=LAMBDAS DES DISCONTINUITES  ET VALEUR DU POIDS
C     STATISTIQUE CORRESPONDANT
C     au_ZLHE(L) ET au_ZEFF4(L)=SUITE DES DISCONTINUITEES ET SECTIONS EFFICACE
C     CORRESPONDANTES
C     DONNEES POUR L'HYDROGENE GRANT=MON. NOT. VOL. 118 P. 241 1958
C     ------------------------
C     au_G3D(I,J) =FACTEUR DE GAUNT EN FONCTION DE au_RHO ET DE LOG(-ETA)
C     au_RHO(I)  =VALEUR DE au_RHO CORRESPONDANTES
C     au_ZLETAG(J)=VALEUR DE LOG(-ETA)
C     au_YY=ZEROS DU POLYNOME DE LAGUERRE=CHANDRASEKHAR RADIATIVE TRANSFER
C     au_AA=NBR. DE CHRISTOFFEL CORRESPONDANT
C     DONNEES POUR QUASI MOLECULE H+H (DOYLE,APJ,153,187.1968)
C
      END















C>-------------------------------------------------------------------------------
C> ATHYHE():
C> CE SSP CALCULE LE COEFFICIENT D'ABSORPTION PAR ATOME NEUTRE POUR
C> L'HYDROGENE ET L'HELIUM, ON SORT 2 VALEURS DE ZZK SI WL= A UNE
C> DISCONTINUITE DE L'UN DE CES ABSORBANTS
C>
C> A.M COLLE  07/12/1970
C>
      SUBROUTINE ATHYHE (WL,TH,CALTH,CALLAM,ZZK)

      INTEGER*4 CALLAM,CALTH
      DIMENSION TGAUNT(5),TRHOG(5),OPNU(46),ZZK(11,2),COTE(2),SNIV(2),EX
     1PO(2),CUK(2),CONS(2),AN(2),C1(3),C2(3),C3(3),C4(3),C5(3),C6(3),EXP
     2ON(2)

      COMMON /ABSO3/ au_JFZ
      COMMON /HYHE/  au_GRDM(46), U1(46), au_U2(46), au_WINV(46), au_YY(4),
     1               au_ZLETAG(18), au_G3D(12,18), au_AA(4), au_ZEFF4(10),
     2               au_RHOG(12), au_ZLHEP(19)

      COMMON /ABME/  au_STIMU
      COMMON /GBFH/  au_G2D(2,19), au_JSHYD(2), au_JH
      COMMON /TETA/  au_AHE, au_AH, au_AHEP, au_UH1, au_ZEMH, au_UHEP1, au_UHE1,au_ZEUHE1
      COMMON /TEHE/  au_ZLHEM(5), au_ZLHE(10)
      COMMON /THE/   au_ZEUH(20), au_ZEUHEP(20), au_ZEXPM(5), au_ZEXP(10), au_UL

      DATA EXPO/-68.88230,-71.45087/,CONS/3.3,3.6/,COTE/3.136954E-23,8.1
     195952E-24/,SNIV/0.55,0.485/,CUK/0.3025,0.235225/,AN/0.3099204E-21,
     20.22849203E-21/,C1/-2.850692E-2,-7.056869E-3,3.591294E-3/,C2/0.208
     30816,0.1809394,-0.1959804/,C3/2.549101,-1.828635,4.233733/,C4/-14.
     497997,8.900841,-20.84862/,C5/0.0,-17.78231,0.0/,C6/0.0,-7.89472E-2
     5,0.0/
      JHE  = 0
      JHEP = 0
      JHEM = 0
      IF (CALLAM.EQ.1) INDTH = 0
      JHYT = 1
      GCONST = 31.3213*TH     
C     31.3213= 157871.62/5040.39 , 157871.62=M*Z**2*E**4/(2*K*(H/2*PI)**
C     M ET E SONT LA MASSE ET LA CHARGE DE L'ELECTRON,H ET K LES CONSTAN
C     DE PLANCK ET DE BOLTZMANN
C

      IF (au_UL .LT. 100.0) GO TO 1333
      
C     au_UL=H*NU/(K*T)
      au_STIMU=1.0
      GO TO 1334
      
1333  au_STIMU = 1.0-EXP(-au_UL)

1334  STIMU3 = au_STIMU/au_UL**3

      IF (CALLAM.EQ.2) GO TO 1335
      
      ZNL = ALOG(WL)
      ZLAMIN = 1.0E8/WL
      DO 3 N = 1,2
3       EXPON(N)=EXP(EXPO(N)+CONS(N)*ZNL)
C
C     I   H-
C     H- GINGERICH= HARVARD JUIN 1964 (RESULTATS*1.0E-26)

1335  IF (TH .GE. 0.3) GO TO 6060

      ZZK(absoru2_NMETA+1,1)=0.0
      GO TO 6210
      
6060  IF ((CALLAM.EQ.2).AND.(INDTH.EQ.1)) GO TO 6100

      INDTH=1
      IF (WL.LE.16419.0) GO TO 6070
      
      ALTHMB = 0.0
      GO TO 6190
      
6070  WLM = WL/1.0E3
      IF (WL.GT.14200.0) GO TO 6090
      
      ZKAS =(((5.95244E-4*WLM-0.0204842)*WLM+0.164790)*WLM+0.178708)*WLM+
     10.680133E-2
      GO TO 6100
      
6090  WLM=16.149-WLM
      ZKAS = ((0.273236E-2*WLM-0.411288E-1)*WLM+0.220190)*WLM**2+0.269818
      
6100  FACT = 1.0-EXP((-TH)*28.54310E+3/WL)
C
C     TRANSITION BOUND-FREE= GELTMAN APJ. VOL. 136 P. 935 1962
C

      ALTHMB = ZKAS*4.158E-1*TH**2.5*EXP(1.726*TH)*FACT
C
C     TRANSITION FREE-FREE=T L JOHN (OCT.1963)
C

6190  ALTHML=(WL/1.0E6)*(((-5.939*TH+11.934)*TH-3.2062)+(WL/1.0E3)*((-0.
     134592*TH+7.0355)*TH-0.40192))+((0.027039*TH-0.011493)*TH+0.0053666
     2)

! ISSUE: check spill!!!!!!!!!!! if using index +1, perhaps I should dimension the relevant vectors with dimension MAX_absoru2_NMETA+1     
      ZZK(absoru2_NMETA+1,1) = ALTHMB+ALTHML
C
C     II   H2-
C     H2- SOMMERVILLE= APJ. VOL. 139 P. 195 1963

6210  IF (TH.LT.0.5) GO TO 2050
      IF (WL.GE.3040.0) GO TO 2070
      
2050  ZZK(absoru2_NMETA+2,1)=0.0
      GO TO 2080
      
2070  DKSQ=911.27/WL
      ZZK(absoru2_NMETA+2,1)=(((0.09319*TH+2.857-0.9316/TH)/DKSQ-(2.6*TH+6.831-4.
     1993/TH))/DKSQ+(35.29*TH-9.804-10.62/TH)-(74.52*TH-62.48+0.4679/TH)
     2*DKSQ)*1.0E-29
C
C     III   H2+
C     H2+ BATES=HARVARD JUIN 1964  (RESULTATS *1.0E+39)

2080  IF ((TH.LT.0.25).OR.((ZLAMIN.LT.au_WINV(1)).OR.(ZLAMIN.GT.au_WINV(46))))
     1 GO TO 1012
     
      BKT=3.19286E-2/TH
C     BKT=K*T EN RYDBERGS POUR H2+
C

      DO 1006 J=1,46
1006  OPNU(J)=2.51E-3*au_GRDM(J)*(EXP(U1(J)/BKT)-(EXP(-au_U2(J)/BKT)))
      DO 1013 J=1,46
      JJ=J
      IF (ABS(ZLAMIN-au_WINV(J)).LE.0.5) GO TO 1014
      IF (ZLAMIN.LT.au_WINV(J)) GO TO 1015
1013  CONTINUE
1014  ZZK(absoru2_NMETA+3,1)=OPNU(JJ)
      GO TO 1016
C     INTERPOLATION LINEAIRE
1015  ZZK(absoru2_NMETA+3,1)=(OPNU(JJ-1)*au_WINV(JJ)-OPNU(JJ)*au_WINV(JJ-1)+(OPNU(JJ)-O
     1PNU(JJ-1))*ZLAMIN)/(au_WINV(JJ)-au_WINV(JJ-1))
      GO TO 1016
1012  ZZK(absoru2_NMETA+3,1)=0.0
C
C     CAS OU WL EST UNE DISCONTINUITE
1016  IF (au_JFZ.NE.2) GO TO 1017
      DO 1019 N=1,3
1019  ZZK(absoru2_NMETA+N,2)=ZZK(absoru2_NMETA+N,1)
C
C     IV   H
C     H UNSOLD (1955) PAGE 168
C     FACTEUR DE GAUNT FREE-FREE POUR H=GRANT M.N.,VOL.118
C     SYMBOLES CF VARDYA APJ.SUP. VOL. 8,P.277,1964
1017  IF (TH.GT.1.4) GO TO 1809
      DO 1855 K=1,4
      TRHOG(K)=SQRT(1.0+au_UL/au_YY(K))
      IF (TRHOG(K).GE.1.01) GO TO 1820
      IF (TRHOG(K).NE.1.0) TGAUNT(K)=2.0/(TRHOG(K)-1.0)
      IF (TRHOG(K).EQ.1.0) GO TO 1856
      TGAUNT(K)=0.5513289*ALOG(TGAUNT(K))
C     0.5513289=SQRT(3)/PI
      GO TO 1855
1856  TGAUNT(K)=0.0
      GO TO 1855
1820  IF (TRHOG(K).LE.1.8) GO TO 1830
      TEMPOR=(TRHOG(K)-1.0)*SQRT(GCONST/(au_YY(K)+au_UL))
      ANY=TEMPOR**(-0.6666667)
      TGAUNT(K)=(-0.01312*ANY+0.21775)*ANY+1.0
      GO TO 1855
1830  TEMPOR=0.2171473*ALOG(GCONST/(au_YY(K)+au_UL))
C
C     0.2171473=0.434294482/2
C
      IF ((TEMPOR.LT.au_ZLETAG(1)).OR.(TEMPOR.GT.au_ZLETAG(18))) GO TO 1847
C     INTERPOLATION A PARTIR DE LA TABLE 1 DE GRANT (1958)
      DO 1835 IR=1,12
      JR=IR
      IF (ABS(TRHOG(K)-au_RHOG(IR)).LE.1.0E-4) GO TO 1836
      IF  (TRHOG(K).LT.au_RHOG(IR)) GO TO 1837
1835  CONTINUE
1836  CARO=1.0
C
C     INTERPOLATION SUR LOG(-ETA) SEULEMENT
C
      GO TO 1838
1837  RHOG1=au_RHOG(JR-1)
      CARO=TRHOG(K)-RHOG1
1838  RHOG2=au_RHOG(JR)
      IF (CARO.EQ.1.0) DIFRO=1.0
      IF (CARO.NE.1.0) DIFRO=RHOG2-RHOG1
      DO 1845 IE=1,18
      JE=IE
      IF (ABS(TEMPOR-au_ZLETAG(IE)).LE.1.0E-4) GO TO 1846
      IF  (TEMPOR.LT.au_ZLETAG(IE)) GO TO 1848
1845  CONTINUE
1846  IF (CARO.EQ.1.0) GO TO 1850
      CAETA=1.0
C
C     INTERPOLATION SUR au_RHO SEULEMENT
C
      GO TO 1849
1848  ZLETA1=au_ZLETAG(JE-1)
      CAETA=TEMPOR-ZLETA1
1849  ZLETA2=au_ZLETAG(JE)
      IF(CAETA.EQ.1.0)  DIFETA=1.0
      IF(CAETA.NE.1.0)  DIFETA=ZLETA2-ZLETA1
      GO TO 1851
1850  TGAUNT(K)=au_G3D(JR,JE)
      GO TO 1855
1851  TGAUNT(K)=((au_G3D(JR-1,JE-1)*(RHOG2-TRHOG(K))+au_G3D(JR,JE-1)*CARO)*(ZL
     1ETA2-TEMPOR)+(au_G3D(JR,JE)*CARO+au_G3D(JR-1,JE)*(RHOG2-TRHOG(K)))*CAETA
     2)/DIFRO/DIFETA
      GO TO 1855
1847  WRITE (6,100)
100   FORMAT ('0 ON SORT DE LA TABLE DE GFF')
1855  CONTINUE
      G3=0.0
      DO 1860 K=1,4
1860  G3=G3+TGAUNT(K)*au_AA(K)
C     G3=FACTEUR DE GAUNT FREE FREE
      GO TO 4199
1809  ZZK(absoru2_NMETA+4,1)=0.0
      JHYT=0
4199  DO 4200 I=1,au_JFZ
      IF (((I.EQ.1).AND.(JHYT.NE.0)).OR.(au_JH.EQ.1)) GO TO 4201
C
C     WL N'EST PAS UNE DISCONTINUITE DE H
C
      IF (I.EQ.2) ZZK(absoru2_NMETA+4,2)=ZZK(absoru2_NMETA+4,1)
      GO TO 1451
4201  SIGH=0.0
      JS=au_JSHYD(I)
      DO 1410 J=JS,19
1410  SIGH=SIGH+au_G2D(I,J)*au_ZEUH(J)
C     RAPPEL  au_G2D= FACTEUR DE GAUNT BOUND FREE
      BH=SIGH+(au_ZEUH(20)-(1.0-G3)*au_ZEMH)/(2*au_UH1)
      ZZK(absoru2_NMETA+4,I)=au_AH*BH*STIMU3
C
C     V   HE+
C     HE+  VARDYA APJ.SUP. VOL. 8,P.277,1964
1451  IF (TH.GT.0.3) GO TO 1552
      SIGHEP=0.0
      DO 1462 J=1,19
      JJ=J
      IF (ABS(WL-au_ZLHEP(J)).LE.0.50) GO TO 1465
      IF (WL.LT.au_ZLHEP(J)) GO TO 1463
1462  CONTINUE
1463  JJS=JJ
      GO TO 1470
1465  JHEP=1
      IF (I.EQ.1) GO TO 1463
1468  JJS=JJ+1
1470  IF ((I.EQ.1).OR.(JHEP.EQ.1)) GO TO 1471
C
C     WL N'EST PAS UNE DISCONTINUITE DE HE+
C
      ZZK(absoru2_NMETA+5,2)=ZZK(absoru2_NMETA+5,1)
      GO TO 1554
1471  DO 1520 JJ=JJS,19
1520  SIGHEP=SIGHEP+au_ZEUHEP(JJ)
      BHEP=SIGHEP+au_ZEUHEP(20)/(2*au_UHEP1)
      ZZK(absoru2_NMETA+5,I)=au_AHEP*BHEP*STIMU3
      GO TO 1554
1552  ZZK(absoru2_NMETA+5,I)=0.0
C
C     VI   HE
C     HE  VARDYA=APJ. SUP. 80 VOL. 8 P. 277 JANVIER 1964
1554  IF (TH.LE.0.8) GO TO 5400
      ZZK(absoru2_NMETA+6,I)=0.0
      GO TO 4200
5400  IF ((I.EQ.2).AND.(au_JH.EQ.1)) GO TO 5872
      SIGHEM=0.0
      SIGHE=0.0
      IF ((WL-au_ZLHEM(5)).GT.0.50) GO TO 5740
      DO 5460 K=1,5
      KK=K
      IF (ABS(WL-au_ZLHEM(K)).LE.0.50) GO TO 5490
      IF (WL.LT.au_ZLHEM(K)) GO TO 5470
5460  CONTINUE
5470  KKS=KK
      GO TO 5540
5490  JHEM=1
      IF (I.EQ.1) GO TO 5470
5520  KKS=KK+1
      IF (KKS.GT.5) GO TO 5740
5540  IF ((JHEM.EQ.1).OR.(I.EQ.1)) GO TO 5541
C
C     WL N'EST PAS = A UNE VALEUR DE au_ZLHEM
C     RAPPEL  au_ZLHEM=504,2601,3122,3422,3680 A.
C
      GO TO 5741
5541  DO 5730 K=KKS,5
      GO TO (5560,5560,5560,5620,5680),K
5560  IF (K.EQ.2) ZNL1=ZNL
      IF (K.NE.2) ZNL1=1.0
      ANU=EXP(((((C1(K)*ZNL+C2(K))*ZNL+C3(K))*ZNL+C4(K))*ZNL1+C5(K))*ZNL
     11+C6(K))*1.0E-18
      GO TO 5730
5620  N=1
C
C     GOLDBERG APJ. VOL. 90 P. 414 1939 ET UNDERHILL PUB. COP. OBS. N0.
C
5621  IF (ABS(WL-au_ZLHEM(3+N)).GT.0.50) GO TO 5640
C     NIVEAUX 4 A 7 DE HE1
      ANU=AN(N)/WL+EXPON(N)
      GO TO 5730
5640  au_ZK=1.097224E-3*au_ZLHEM(3+N)*WL/(au_ZLHEM(3+N)-WL)
      RK=SQRT(au_ZK)
      UK=1.0+CUK(N)*au_ZK
      ANU=(COTE(N)/(WL*(1.0-EXP(-6.283185*RK)))*(au_ZK/UK   )**6*((1.0+au_ZK)/
     1UK)*((4.0+au_ZK)/UK)*EXP(-4.0*RK*ATAN(1.0/(SNIV(N)*RK))))+EXPON(N)
      GO TO 5730
5680  N=2
      GO TO 5621
5730  SIGHEM=SIGHEM+ANU*au_ZEXPM(K)
      BHEM=SIGHEM*au_STIMU
      GO TO 5741
5740  BHEM=0.0
C     NIVEAUX 8 ET SQ (N.GE.3)
5741  DO 5780 L=3,9
      LL=L
      IF (ABS(WL-au_ZLHE(L)).LE.0.50) GO TO 5810
      IF  (WL.LT.au_ZLHE(L)) GO TO 5790
5780  CONTINUE
5790  LLS=LL
      GO TO 5860
5810  JHE=1
      IF (I.EQ.1) GO TO 5790
5840  LLS=LL+1
5860  IF ((I.EQ.1).OR.(JHE.EQ.1)) GO TO 5861
C
C     WL N'EST PAS = A UNE VALEUR DE au_ZLHE
C
      GO TO 5871
5861  DO 5870 L=LLS,9
5870  SIGHE=SIGHE+au_ZEXP(L)*au_ZEFF4(L)
      BHE=SIGHE+(1807.240*au_ZEXP(10)-0.8072399*au_ZEUHE1)/(2*au_UHE1)
5871  ZZK(absoru2_NMETA+6,I)=au_AHE*BHE*STIMU3+BHEM
      GO TO 4200
C
C     WL N'EST PAS UNE DISCONTINUITE DE HE
C
5872  ZZK(absoru2_NMETA+6,2)=ZZK(absoru2_NMETA+6,1)
4200  CONTINUE
      RETURN
      END






C-------------------------------------------------------------------------------
C> Ionization degree by hydrogen atoms & electrons (???; to be confirmed) ISSUE
C>
C>     SSP CALCULANT LES QUANTITES SUIVANTES
C>     PARTH =NBRE TOTAL DE NOYAUX PAR CM3
C>     au_PG    =PRESSION TOTALE EN DYNES/CM2
C>     au_ZMU   =POIDS MOLECULAIRE MOYEN
C>     au_RHO   =DENSITE (G-CM-3)
C>     au_TOC   =NOMBRE DE NOYAUX D'HYDROGENE PAR CM3
C>     au_AC    =DEGRE D'IONISATION MOYEN
C>     au_AC1(1)=  ''        ''     DE H
C>     au_AC1(2)=  ''        ''     DE HE+
C>     au_AC1(3)=  ''        ''     DE HE
C>     au_AC2   =  ''        ''     DES METAUX
C>     PHI(J)=  ''        ''     DE L ELEMENT J POUR MULTIPLE IONISATION
C>     absoru_ZNH(M)=POPULATION POUR CHAQUE ABSORBANT M (H,HE OU METAUX)
C>     VOIR ARTICLE DE 'VARDYA' APJ VOL.133,P.107,1961
C>
C> A.M COLLE  18/01/1971
C>
      SUBROUTINE IONIPE(TH,ZLPE,CALTH,CALMET)
      INTEGER*4 CALTH,CALMET
      REAL KTH
      DIMENSION PHI(30),PA(10)
      COMMON /SAPE/ au_AVM, au_ZNU1, au_ZNU2, au_ZNU3, au_ZMUZE, au_ZNU(30)
      COMMON /ZION/ au_AC, au_AC1(3), au_AC2(30,9)
      COMMON /SAHT/ au_ZK(11), au_ZKM(30,9), absoru2_NR(30)
      COMMON /SAPU/ au_PE, au_RHO, au_TOC, absoru_ZNH(12)
      COMMON /SAPDIV/ au_ZMU, au_PG
      KTH=6.956948E-13/TH
C     6.956948E-13=1.38024E-16*5040.39
      au_PE=EXP(ZLPE*2.302585)
      SIGM3=0.0
      DO 2380 J=1,absoru2_NM
      NRR=absoru2_NR(J)
      SIGM1=0.0
      SIGM2=0.0
      PA(1)=1.0
      DO 2370 I=1,NRR
      IF ((PA(I).LE.0.0).OR.(au_ZKM(J,I).LE.0.0)) GO TO 2375
      PA(I+1)=PA(I)*(au_ZKM(J,I)/au_PE)
      SIGM1=I*PA(I+1)+SIGM1
2370  SIGM2=SIGM2+PA(I+1)
2375  DEN=1.0+SIGM2
      PHI(J)=SIGM1/DEN
      DO 1 I=1,NRR
1     au_AC2(J,I)=PA(I)/DEN
2380  SIGM3=SIGM3+au_ZNU(J)*PHI(J)
      IF (CALTH.EQ.2) GO TO 2390
      IF (TH.GE.0.25) GO TO 2382
      TEMPOR=0.0
      ANY=0.0
      COND=0.0
      GO TO 2390
2382  ANY=1.0/au_ZK(absoru2_NMETA+3)
      TEMPOR=1.0/au_ZK(absoru2_NMETA+2)
      COND=1.0/au_ZK(absoru2_NMETA+1)
2390  W2=au_ZK(absoru2_NMETA+4)/au_PE
      W1=W2*ANY
      W3=au_PE*COND
      W4=au_ZNU2*au_ZK(absoru2_NMETA+6)*(au_PE+2*au_ZK(absoru2_NMETA+5))/((au_PE+au_ZK(absoru2_NMETA+6))*au_PE+au_ZK(absoru2_NMETA+6
     1)*au_ZK(absoru2_NMETA+5))+(au_ZNU3*SIGM3)
      FUN1=au_ZNU1*W1+2*(TEMPOR+W1)*W4
      FUN2=au_ZNU1*(W2-W3)+(1.0+W2+W3)*W4
      PH=2*au_ZNU1*au_PE/(FUN2+SQRT(FUN2**2+4*FUN1*au_ZNU1*au_PE))
      absoru_ZNH(absoru2_NMETA+4)=PH/KTH
      absoru_ZNH(absoru2_NMETA+2)=PH*TEMPOR*absoru_ZNH(absoru2_NMETA+4)
      absoru_ZNH(absoru2_NMETA+1)=absoru_ZNH(absoru2_NMETA+4)*W3
      absoru_ZNH(absoru2_NMETA+7)=absoru_ZNH(absoru2_NMETA+4)*W2
      absoru_ZNH(absoru2_NMETA+3)=absoru_ZNH(absoru2_NMETA+7)*PH*ANY
      TP1=absoru_ZNH(absoru2_NMETA+1)+absoru_ZNH(absoru2_NMETA+4)+absoru_ZNH(absoru2_NMETA+7)
      TP2=absoru_ZNH(absoru2_NMETA+2)+absoru_ZNH(absoru2_NMETA+3)
      au_TOC=2*TP2+TP1
      PARTH=au_TOC/au_ZNU1
      PPAR=(TP1+TP2+PARTH*(au_ZNU2+au_ZNU3))*KTH
      au_PG=PPAR+au_PE
      au_AC=au_PE/PPAR
      W5=au_ZK(absoru2_NMETA+6)/au_PE
      W6=au_ZK(absoru2_NMETA+5)*W5/au_PE
      S=1.0+W5+W6
      au_AC1(1)=W2/(1.0+W2)
      au_AC1(2)=W6/S
      au_AC1(3)=W5/S
      absoru_ZNH(absoru2_NMETA+6)=au_ZNU2*PARTH/(1.0+W5+W6)
      absoru_ZNH(absoru2_NMETA+5)=absoru_ZNH(absoru2_NMETA+6)*W5
      au_RHO=1.6602E-24*PARTH*au_ZMUZE
C     1.6602E-24=MASSE DE L'UNITE DE POIDS
      au_ZMU=au_RHO*41904.28E+7/(TH*au_PG)
C     41904.275E+7=8.313697E+7*5040.39,OU 8.313697E+7=CONSTANTE DES GAZ
11    RETURN
      END



      END MODULE ABSORU
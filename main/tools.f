C MISCellaneous MATHs: re-usable Math library
      MODULE MISC_MATH
      
      CONTAINS
      

C-------------------------------------------------------------------------------
      FUNCTION IINF(FR,ITOT,IA,IZ)
      DIMENSION FR(ITOT)
      IA2=IA+1
      IINF=IA
      FMIN=FR(IA)
      DO 1 I=IA2,IZ
        IF(FR(I).GT.FMIN) GO TO 1
        FMIN=FR(I)
        IINF=I
1     CONTINUE
      RETURN
      END


C-------------------------------------------------------------------------------
C     UNE FONCTION FR EST CONNUE EN ITOT POINTS. ON CHERCHE ENTRE
C     LES POINTS IA ET IZ QUEL EST L INDICE I OU CETTE FONCTION
C     EST MAXIMUM.
      FUNCTION ISUP(FR, ITOT, IA, IZ)
      DIMENSION FR(ITOT)
      IA2=IA+1
      ISUP=IA
      FMAX=FR(IA)
      DO 1 I=IA2,IZ
        IF(FR(I) .LT. FMAX) GO TO 1
        FMAX=FR(I)
        ISUP=I
1     CONTINUE
      RETURN
      END





C-------------------------------------------------------------------------------
      FUNCTION MINI(IFA, NTOT, IA, IZ)
      INTEGER NTOT, IA, IZ
      INTEGER, DIMENSION(NTOT) :: IFA
      MINI=IFA(IA)
      IA2=IA+1
      DO I=IA2,IZ
        IF(IFA(I) .LT. MINI) THEN
          MINI=IFA(I)
        END IF
      END DO
      RETURN
      END




C-------------------------------------------------------------------------------
      FUNCTION MAXI(IFA, NTOT, IA, IZ)
      INTEGER NTOT, IA, IZ
      INTEGER, DIMENSION(NTOT) :: IFA
      MAXI=IFA(IA)
      IA2=IA+1
      DO I=IA2,IZ
        IF(IFA(I).GT.MAXI) THEN
          MAXI=IFA(I)
        END IF
      END DO
      RETURN
      END






C-------------------------------------------------------------------------------
C X -- TABLEAU DE VALEURS DE LA VARIABLE INDEPENDANTE, PAR VALEURS
C      CROISSANTES
C Y -- TABLEAU DES VALEURS ASSOCIEES DE LA FONCTION A INTEGRER
C P -- TABLEAU DES VALEURS DE LA PRIMITIVE AUX POINTS X(I)
C N --
C PDEB -- VALEUR DE LA PRIMITIVE POUR X(1),PREMIERE VALEUR
C         DU TABLEAU
C
C METHODE: LA VALEUR DE L'INTEGRALE SUR L'INTERVALLE X(I), X(I+1)
C   EST CALCULEE PAR LA FORMULE DE SIMPSON, LA VALEUR DE Y AU POINT
C   MILIEU ETANT CALCULEE PAR INTERPOLATION CUBIQUE, PAR LA ROUTINE
C   AITK3
      SUBROUTINE INTEGRA(X, Y, P, N, PDEB)
      DIMENSION X(N),Y(N), P(0:N)
      P(1) = PDEB
      
C     CAS SPECIAL DU PREMIER INTERVALLE
      XMILIEU=(X(1)+X(2))/2.
      CALL NAITK3(X(1),X(2),X(3),X(4),
     1           Y(1),Y(2),Y(3),Y(4),XMILIEU,FX)
      P(2) = P(1)+((X(2)-X(1))/6.)*(Y(1)+Y(2)+4.*FX)
      
C     ********************************
C     CAS GENERAL
      DO I=2,N-2
        XMILIEU = (X(I)+X(I+1))/2.
        J = I-1
        CALL NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1              Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,FX)
        P(I+1) = P(I)+((Y(I)+Y(I+1)+4.*FX)/6.)*(X(I+1)-X(I))
      END DO
      
C     *********************************
C     CAS SPECIAL DERNIER INTERVALLE
      XMILIEU = (X(N-1)+X(N))/2.
      J = N-3
      CALL NAITK3(X(J),X(J+1),X(J+2),X(J+3),
     1            Y(J),Y(J+1),Y(J+2),Y(J+3),XMILIEU,FX)
      P(N) = P(N-1)+((X(N)-X(N-1))/6.)*(Y(N-1)+Y(N)+4.*FX)
C     ***********************************
      RETURN
      END




C-------------------------------------------------------------------------------
C     CALCUL DE CH VAN DER WAALS  APPROXIMATIONS D UNSOLD (1955)
C     SI IS1 ET IS2 SONT EGAUX A S P D F FORMULE 82-54  SINON 82-55
      FUNCTION CALCH(KII, IZ, KIEX1, IS1, KIEX2, IS2)
      REAL KII,KIEX1,KIEX2,NET1C,NET2C
      DIMENSION IS(4), IL(4)
      CHARACTER*1 IS, IBL
      CHARACTER*1 IS1, IS2
      
      DATA IBL/' '/
      DATA IS/'S','P','D','F'/
      DATA IL/1,-5,-17,-35/
      
      IF (IS1 .NE. IBL) GO TO 1
      C61 = 1.61E-33*(13.5*IZ / (KII-KIEX1))**2
      C62 = 1.61E-33*(13.5*IZ / (KII-KIEX2))**2
      GO TO 10
      
1     DO 2 I=1,4
        IF(IS1.EQ.IS(I)) GO TO 3
2     CONTINUE

3     IL1=IL(I)
      DO 4 I=1,4
        IF(IS2.EQ.IS(I)) GO TO 5
4     CONTINUE

5     IL2 = IL(I)
      IZC = IZ**2
      NET1C = 13.5 * IZC / (KII-KIEX1)
      NET2C = 13.5 * IZC / (KII-KIEX2)
      C61 = 3.22E-34 * NET1C *(5*NET1C+IL1)/ IZC
      C62 = 3.22E-34 * NET2C *(5*NET2C+IL2)/ IZC
      
10    CALCH = C62-C61
      RETURN
      
100   FORMAT('  NET1C=',F5.2,'  NET2C=',F5.2,5X,2E14.4)
      END












C===============================================================================
C INTERPOLATION ROUTINES
C===============================================================================


      
C-------------------------------------------------------------------------------
C     INTERPOLATION D UNE LISTE A PAS NON CONSTANT
C
C     EXTRAPOLE TETA(PG) POUR PG=0
c     ***TETA1 extrapolation parabolique sur les 3 derniers pts
c     ***TETA2      "             "      sur les pts 2 3 et 4
c     ***TETA3      "        lineaire    sur les 2 derniers pts
c     (ici seul TETA3 est utilise)
      FUNCTION FTETA0(PG,TETA)
      REAL*4 PP1(5),TT1(5),PP2(5),TT2(5)

      !~LOGICAL ECRIT  I think this has been tested already, no need to verbose flag, would slow down the maths
      !~ECRIT=.FALSE.
      PP1(1)=PG(1)
      TT1(1)=TETA(1)
      IF(ECRIT) write(6,*) PG(1), TETA(1)
      DO I=2,5
        PP1(I)=PG(I)
        PP2(I-1)=PG(I)
        TT1(I)=TETA(I)
        TT2(I-1)=TETA(I)
      !~IF(ECRIT) write(6,*) PG(I), TETA(I)
      END DO
c     TETA1=FT(0.,5,PP1,TT1)
c     TETA2=FT(0.,4,PP2,TT2)
      TETA3=TT1(1) - PP1(1) * (TT1(1)-TT1(2)) / (PP1(1)-PP1(2))
      !~IF(ECRIT) WRITE(6,*)TETA3
      FTETA0= TETA3
      RETURN
      END


C-------------------------------------------------------------------------------
C     INTERPOLATION D UNE LISTE A PAS NON CONSTANT
      FUNCTION FT(T,N,X,F)
      DIMENSION X(N),F(N)
      DO 1 J = 1,N
        I = J
        IF(T-X(J)) 3, 2, 1
2       FT = F(J)
        RETURN
1     CONTINUE

3     IF (I. EQ. 1) I = 2
      IF (I. GE. N) I = N-1
      T0 = T-X(I-1)
      T1 = T-X(I)
      T2 = T-X(I+1)
      U0 = X(I+1)-X(I-1)
      U1 = X(I+1)-X(I)
      U2 = X(I)-X(I-1)
      A  = T0/U0
      B  = T1/U1
      C  = T2/U2
      D  = T0/U1
      E  = T1/U0
      FT = F(I+1)*A*B - F(I)*D*C + F(I-1)*E*C
      RETURN
      END


C-------------------------------------------------------------------------------
      SUBROUTINE FTLIN3(N,X,Y,ITOT,TT,FTT)
      DIMENSION X(N),Y(N),TT(ITOT),FTT(ITOT)
C     WRITE(6,*) N
C     WRITE (6,105) (X(J),J=1,N)
C     WRITE (6,105) (Y(J),J=1,N)
C
      J=2
      DO  4 K=1,ITOT
      T=TT(K)
C     WRITE(6,103)T
      JJ=J-1
            DO 1  J=JJ,N
            IF(T-X(J) ) 3,2,1
1           CONTINUE
            GO TO 10
2     FT=Y(J)
      IF(J.EQ.1) J=J+1
      GO TO 4
C 3   WRITE(6,*) '   J=',J
3     IF(J.EQ.1)   GO TO 10
      U0= Y(J)-Y(J-1)
      T0= X(J)-X(J-1)
      T1=   T -X(J-1)
C     WRITE(6,104) J, X(J-1), Y(J-1), X(J),Y(J), U0,T0,T1
      T2= T1/T0
      DY= U0*T2
      FT= Y(J-1) + DY
4     FTT(K)=FT
      RETURN
10    WRITE(6,100) T
      WRITE(6,101)
      WRITE(6,102) (X(I),I=1,N)
      STOP
100   FORMAT(' ON SORT DE LA TABLE D INTERPOLATION    T=',E15.7)
101   FORMAT(/'   LISTE DES X')
102   FORMAT(8E15.7)
103   FORMAT(5X,F10.3)
104   FORMAT(I5,7F9.3)
C105  FORMAT(7F10.3)
      END



C-------------------------------------------------------------------------------
C ISSUE WHAT
C
C       interpolation
C
      FUNCTION FAITK30(XX, X, Y, N)
      DIMENSION X(0:N), Y(0:N)
      IF (XX .LT. X(2)) THEN
        I=0
        GOTO 200
      ELSE 
        IF (XX.GT.X(N-2)) THEN
          I = N-3
          GOTO 200
        ELSE
        DO J = 2, N-2
          IF(XX .LE. X(J)) GO TO 100
        END DO
      ENDIF
100   CONTINUE
      I = J-2
c
200   CALL NAITK3(X(I),X(I+1),X(I+2),X(I+3),
     1           Y(I),Y(I+1),Y(I+2),Y(I+3),XX,RESULTA)
      FAITK30 = RESULTA
      RETURN
      END


! ISSUE: is this still useful?? (NOT SWITCHED ON!!!)
C-------------------------------------------------------------------------------
C     INTERPOLATION PARABOLIQUE
C     DANS LA TABLE X Y (N POINTS) ON INTERPOLE LES FTT CORRESPONDANT
C     AUX TT  (ITOT POINTS) POUR TOUTE LA LISTE DES TT
      SUBROUTINE FT2(N,X,Y,ITOT,TT,FTT)
      DIMENSION  X(N),Y(N),TT(ITOT),FTT(ITOT)
      INV = -1
      IF (X(N).LT.X(1)) INV = 1
      DO 4 K = 1,ITOT
      T = TT(K)
      IF (INV) 5, 6, 6
5     DO 1 J = 1,N
        I = J
        IF (T-X(J)) 3, 2, 1
1     CONTINUE
      GO TO 10
      
6     DO 7 J = 1, N
        I = J
        IF(T-X(J)) 7, 2, 3
7     CONTINUE
      GO TO 10
      
2     FT = Y(J)
      GO TO 4
      
3     IF (I .EQ. 1) I = 2
      IF (I .GE. N) I = N-1
      T0 = T-X(I-1)
      T1 = T-X(I)
      T2 = T-X(I+1)
      U0 = X(I+1)-X(I-1)
      U1 = X(I+1)-X(I)
      U2 = X(I)-X(I-1)
      A  = T0/U0
      B  = T1/U1
      C  = T2/U2
      D  = T0/U1
      E  = T1/U0
      FT = Y(I+1)*A*B - Y(I)*D*C + Y(I-1)*E*C
4     FTT(K) = FT
      RETURN
10    WRITE(6,100) T
      STOP
100   FORMAT(5X,'ON SORT DE LA TABLE D INTERPOLATION       T=',E15.7)
      END




C-------------------------------------------------------------------------------
C ISSUE WHAT
C
c     Nouvelle subroutine NAITK3 remplace AITK3 et AITK30
C
      SUBROUTINE NAITK3(XdI, XdIp1, XdIp2, XdIp3,
     1                  YdI, YdIp1, YdIp2, YdIp3, XX, FX)
      IMPLICIT NONE
      REAL XdI, XdIp1, XdIp2, XdIp3, YdI, YdIp1, YdIp2, YdIp3, XX, FX
     
      U   = XdI
      V   = XdIp1
      FU  = YdI
      FV  = YdIp1
      F01 =(FU*(V-XX)-FV*(U-XX))/(V-U)
      
      U   = XdIp2
      FU  = YdIp2
      F12 = (FU*(V-XX)-FV*(U-XX))/(V-U)
      
      U   = XdIp3
      FU  = YdIp3
      F13 = (FU*(V-XX)-FV*(U-XX))/(V-U)
      
      U    = XdI
      FU   = F01
      V    = XdIp2
      FV   = F12
      F012 = (FU*(V-XX)-FV*(U-XX))/(V-U)
      
      U    = XdIp2
      FU   = F12
      V    = XdIp3
      FV   = F13
      F123 = (FU*(V-XX)-FV*(U-XX))/(V-U)
      
      U     = XdI
      V     = XdIp3
      FU    = F012
      FV    = F123
      F0123 = (FU*(V-XX)-FV*(U-XX))/(V-U)
      
      FX = F0123
      RETURN
      END



      END MODULE MISC_MATH

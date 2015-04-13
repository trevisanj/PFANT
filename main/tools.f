C Various standalone functions.
      MODULE TOOLS
      
      CONTAINS

C-------------------------------------------------------------------------------
C     ***      REFERENCE   Q.S.R.T.   VOL16,611 (1976)
C     ***ROUTINE COMPUTES THE VOIGHT FUNCTION  Y/PI*INTEGRAL FROM
C     ***- TO + INFINITY OF  EXP(-T*T)/(Y*Y+(X-T)*(X-T)) DT
C     *** LA FONCTION EST ENSUITE NORMALISEE
C
C Arguments:
C   X --
C   Y --
C   DEL --
C   PHI -- output
      SUBROUTINE HJENOR(Y,X,DEL,PHI)

      REAL X,Y
      real VOIGT
      real    VV,UU,CO
      REAL    B(22),RI(15),XN(15)/10.,9.,2*8.,7.,6.,5.,4.,7*3./,
     &  YN(15)/3*.6,.5,2*.4,4*.3,1.,.9,.8,2*.7/,D0(35),D1(35),D2(35)
     &  ,D3(35),D4(35),HN(35),H/.201/,XX(3)/.5246476,1.65068,.7071068/
     &  ,HH(3)/.2562121,.2588268E-1,.2820948/,NBY2(19)/9.5,9.,8.5,8.,
     &  7.5,7.,6.5,6.,5.5,5.,4.5,4.,3.5,3.,2.5,2.,1.5,1.,.5/,C(21)/
     &  .7093602E-7,-.2518434E-6,.8566874E-6,-.2787638E-5,.866074E-5,
     &  -.2565551E-4,.7228775E-4,-.1933631E-3,.4899520E-3,-.1173267E-2,
     &  .2648762E-2,-.5623190E-2, .1119601E-1,-.2084976E-1,.3621573E-1,
     &  -.5851412E-1,.8770816E-1, -.121664,.15584,-.184,.2/
      LOGICAL TRU/.FALSE./
      TRU=.FALSE.
      B(1)=0.
      B(2)=0.7093602E-7
      IF (TRU) GO TO 104
C  REGION I. COMPUTE DAWSON'S FUNCTION AT MESH POINTS
      TRU=.TRUE.
      DO 101 I=1,15
101   RI(I)=-I/2.
      DO 103 I=1,25
      HN(I)=H*(I-.5)
      C0=4.*HN(I)*HN(I)/25.-2.
      DO 102 J=2,21
102   B(J+1)=C0*B(J)-B(J-1)+C(J)
      D0(I)=HN(I)*(B(22)-B(21))/5.
      D1(I)=1.-2.*HN(I)*D0(I)
      D2(I)=(HN(I)*D1(I)+D0(I))/RI(2)
      D3(I)=(HN(I)*D2(I)+D1(I))/RI(3)
      D4(I)=(HN(I)*D3(I)+D2(I))/RI(4)
C     write(6,*)i,d0(i),d1(i),d2(i),d3(i),d4(i)
103   CONTINUE
104   IF (X-5.) 105,112,112
105   IF (Y-1.) 110,110,106
106   IF (X.GT.1.85*(3.6-Y)) GO TO 112
C   REGION II CONTINUED FRACTION .COMPUTE NUMBER OF TERMS NEEDED
C     write(6,*)'region II'
      IF (Y.LT.1.45) GO TO 107
      I=Y+Y
      GO TO 108
107   I=11.*Y
108   J=X+X+1.85
      MAX=XN(J)*YN(I)+.46
      MIN=MIN0(16,21-2*MAX)
C  EVALUATED CONTINUED FRACTION
      UU=Y
      VV=X
      DO 109 J=MIN,19
      U=NBY2(J)/(UU*UU+VV*VV)
      UU=Y+U*UU
109   VV=X-U*VV
      VOIGT=UU/(UU*UU+VV*VV)/1.772454
      GO TO 10
110   Y2=Y*Y
      IF (X+Y.GE.5.) GO TO 113
C REGION I. COMMPUTE DAWSON'S FUNCTION AT X FROM TAYLOR SERIES
      N=INT(X/H)
      DX=X-HN(N+1)
      U=(((D4(N+1)*DX+D3(N+1))*DX+D2(N+1))*DX+D1(N+1))*DX+D0(N+1)
      V=1.-2.*X*U
C  TAYLOR SERIES EXPANSION ABOUT Y=0.0
      VV=EXP(Y2-X*X)*COS(2.*X*Y)/1.128379-Y*V
C     write(6,*) n,u,dx,d0(n+1),d1(n+1),d2(n+1),d3(n+1),d4(n+1)
      UU=-Y
      MAX=5.+(12.5-X)*.8*Y
      DO 111 I=2,MAX,2
      U=(X*V+U)/RI(I)
      V=(X*U+V)/RI(I+1)
      UU=-UU*Y2
111   VV=VV+V*UU
      VOIGT=1.128379*VV
C     write(6,*)'region I ',voigt,vv,x,y,del
      GO TO 10
112   Y2=Y*Y
      IF (Y.LT.11.-.6875*X) GO TO 113
C  REGION IIIB  2 POINT GAUSS-HERMITE QUADRATURE
      U=X-XX(3)
      V=X+XX(3)
      VOIGT=Y*(HH(3)/(Y2+U*U)+HH(3)/(Y2+V*V))
C     write(6,*)'region IIIb ', voigt
      GO TO 10
C  REGION IIIA 4-POINT GAUSS-HERMITE QUADRATURE.
113   U=X-XX(1)
      V=X+XX(1)
      UU=X-XX(2)
      VV=X+XX(2)
      VOIGT=Y*(HH(1)/(Y2+U*U)+HH(1)/(Y2+V*V)+HH(2)/(Y2+UU*UU)+HH(2)/
     1(Y2+VV*VV))
C     write(6,*)'region IIIa',voigt
10    PHI = VOIGT /  (1.772454 * DEL)
C     write(6,*)phi
      RETURN
      END



      END MODULE TOOLS

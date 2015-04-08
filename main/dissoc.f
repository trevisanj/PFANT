
C Subroutines SAT4 and DIE


      MODULE DISSOC
      IMPLICIT NONE





C Variables filled by READ_DISSOC() (file dissoc.dat)
C Number of elements actually used is specified by variable dissoc_NMETAL <= MAX_dissoc_NMETAL

C ISSUE: I created new variables to decouple from logic while reading this file.
C        The original mixes reading and logic, and I don't want to track down the
C        variables that are read right now. It seems, BTW, that many of them aren't used.
C ISSUE: this may be temporary, or to test not using COMMON, or remain just like this


C RESULT: common blocks are really not needed!!!




      ! TODO if MAX_Z ceases to be private, then it must become MAX_dissoc_Z or sth
      INTEGER, PRIVATE, PARAMETER ::
     + MAX_Z      = 100,  ! Maximum atomic number that can be found in dissoc.dat
     + Z_ELECTRON = 99,   ! Fictitious atomic number of electron
     + Z_H_STAR   = 100,  ! Fictitious atomic number of "H*"
     + Z_H        = 1,    ! Atomic number of Hydrogen
     + Z_HE       = 2     ! Atomic number of Helium



C Variables filled by READ_DISSOC()
C ISSUE: I find very confusing NELEMX (metal atoms) versus NELEM (molecules)

      ! dissoc.dat, metals part

      INTEGER, PARAMETER :: MAX_dissoc_NMETAL=50  ! Limit number of metal rows in dissoc.dat



      INTEGER dissoc_NMETAL, dissoc_NIMAX, dissoc_NELEMX
      CHARACTER*2 dissoc_ELEMS
      INTEGER dissoc__IG0, dissoc__IG1
      REAL dissoc__IP, dissoc__CCLOG
      DIMENSION dissoc_ELEMS(MAX_dissoc_NMETAL),   ! Only this ...
     1          dissoc_NELEMX(MAX_dissoc_NMETAL),  ! ... and this are used directly.
      ! JT2015 I introduced these variables, double underscore to emphasize that they
      !        are not just old variables that had a prefix added.
     2          dissoc__IP(MAX_dissoc_NMETAL),      ! These other 4 variables are used
     3          dissoc__IG0(MAX_dissoc_NMETAL),     ! for filling local variables
     4          dissoc__IG1(MAX_dissoc_NMETAL),     ! within SAT4()
     5          dissoc__CCLOG(MAX_dissoc_NMETAL)    !

      ! dissoc.dat, molecules part
      INTEGER, PARAMETER:: MAX_dissoc_NMOL=600  ! Limit number of molecule rows ISSUE: bit overdimensioned?? (considering the file has only about 30 molecules)
      CHARACTER dissoc_MOL*3
      INTEGER dissoc_NMOL, dissoc_MMAX, dissoc_NELEM, dissoc_NATOM
      REAL dissoc_C
      DIMENSION dissoc_MOL(MAX_dissoc_NMOL),
     1          dissoc_C(MAX_dissoc_NMOL, 5),
     2          dissoc_MMAX(MAX_dissoc_NMOL),
     3          dissoc_NELEM(5, MAX_dissoc_NMOL),
     4          dissoc_NATOM(5, MAX_dissoc_NMOL)








      ! Prefix "sat4_" denotes variables filled by SAT4() (or indirectly, DIE())


      ! They will be pointer targets at molecula.f:POINT_PPA_PB()
      REAL, TARGET, DIMENSION(MAX_modeles_NTOT) :: sat4_PPH, sat4_PPC2,
     + sat4_PN,
     + sat4_PC13, sat4_PMG, sat4_PO, sat4_PTI, sat4_PFE


      ! ISSUE These variables are assigned but never used!!!
      REAL, DIMENSION(MAX_modeles_NTOT) :: sat4_PNG, sat4_PIG

      REAL, PRIVATE, DIMENSION(MAX_Z) ::
     + IP,     ! ?
     + CCOMP,  ! ?
     + UIIDUI, ! ?
     + FP,     ! ?
     + KP,     ! ?
     + P       ! Pressure

      REAL, PRIVATE, DIMENSION(MAX_dissoc_NMOL) ::
     + PPMOL, APMLOG





      REAL PE ! Fictitious pressure of electron?? ISSUE: is it?

      ! ISSUE I won't do it this way until I sort the conflicts in DIE
      !~REAL PE ! Fictitious pressure of the electron?? is it? ISSUE
      !~EQUIVALENCE (P(Z_ELECTRON), P_ELECTRON)


C     ========
      CONTAINS
C     ========


C================================================================================================================================
C "SUBROUTINE D'EQUILIBRE DISSOCIATIF"
      SUBROUTINE SAT4()
      ! USE READ_FILES
      IMPLICIT NONE
      REAL  KPLOG, IPI, ECONST

C ISSUE Careful with all this ELE<something> variables, this is messy
      REAL, DIMENSION(MAX_modeles_NTOT, MAX_dissoc_NMETAL) :: XP

C
C*****IMPUT A

      ECONST = 4.342945E-1

      ! Infers other variables from variables dissoc__* (notice the double underscore)
      DO I = 1, dissoc_NMETAL
          CCLOGI = dissoc__CCLOG(I)+main_AFSTAR
C ISSUE This is the thing that Beatriz mentioned that it is not used anymore, I think
          CCLOGI = CCLOGI+main_XXCOR(I)
          IF(I .EQ .1) CCLOGI = 0.0
          IF(I .EQ .2) CCLOGI = -1.0

          NELEMXI = dissoc_NELEMX(I)
          IG0I = dissoc__IG0(I)
          IG1I = dissoc__IG1(I)

          IP(NELEMXI) = dissoc__IP(I)
          UIIDUI(NELEMXI) = IG1I * 0.661 / IG0I
          CCOMP(NELEMXI) = EXP(CCLOGI/ECONST)

          IF (VERBOSE) THEN !--verbose--!
            WRITE(*, '(1H ,5X,A4,8X,I5,3X, F10.3,5X, 2I5,3X,F10.5)')
     +          dissoc_ELEMS(I), NELEMXI, dissoc__IP(I),
     +          IG0I, IG1I, CCLOGI-main_AFSTAR
          END IF
      END DO

C
C orig *****INPUT D

C orig STARTING VALUE OF THE SOLUTION

      DO 1400 I = 1,dissoc_NMETAL
        NELEMI = dissoc_NELEMX(I)
        P(NELEMI) = 1.0E-20
 1400 CONTINUE

      ! ISSUE What if atomic number 99 was already in dissoc.dat?
      P(Z_ELECTRON) = 1.0E-10
      ! ISSUE: what about 100?


C orig *****INPUT E
      DO 1020 ITO = 1,modeles_NTOT
 1023   THETA = modeles_TETA(ITO)
        TEM = 5040.0/THETA
 1024   PG = modeles_PG(ITO)
        PGLOG = ALOG10(PG)

        CALL DIE(TEM,PG)

        PE = P(Z_ELECTRON)
        PELOG = ALOG10(PE)

        DO 1303 I=1,dissoc_NMETAL
          NELEMI = dissoc_NELEMX(I)

          FPLOG  = ALOG10(FP(NELEMI))
          XP(ITO,I) = P(NELEMI)+1.0E-30
          PLOG   = ALOG10( XP(ITO,I) )
          PDFPL  = PLOG - FPLOG
          IF (MOD(I,5)) 1303,1304,1303
 1304   CONTINUE
 1303   CONTINUE

        IRL = 120
        DO 1184 I=1,dissoc_NMETAL
          NELEMI = dissoc_NELEMX(I)

          PLOG   = ALOG10(P(NELEMI)+1.0E-30)
          KPLOG  = ALOG10(KP(NELEMI)+1.0E-30)
          PIONL  = PLOG + KPLOG - PELOG
          XLOG   = PIONL - PGLOG

          IF (I .NE. dissoc_NMETAL ) GO TO 1450
          IQ  = I / 120
          IR  = dissoc_NMETAL - IQ * 120
          IRL = IR / 3
          GO TO 1460
 1450     IF (MOD(I,120))  1184,1460,1184

 1460     NBL = 0
          DO 1470  K1=1,120,3
            NBL = NBL + 1
            K2 = K1 + 1
            K3 = K1 + 2
            IF ( NBL.EQ.IRL + 1)  GO TO 1480
 1475       CONTINUE

            IF (MOD(NBL,5)) 1470,1500,1470
 1500       CONTINUE
 1470     CONTINUE
          GO TO 1184


 1480     IRR = IR - IRL*3
          IF (IRR .EQ. 0)  GO TO 1184
          GO TO (1482,1484), IRR
 1482     CONTINUE
          GO TO 1184

 1484     CONTINUE
 1184   CONTINUE

        IRL = 120
        KD =-119
        DO 1084 J=1,dissoc_NMOL
          JCOUNT = JCOUNT + 1
          PMOLL  = ALOG10(PPMOL(J)+1.0E-30)
          XLOG   = PMOLL - PGLOG

          IF (J .NE. dissoc_NMOL) GO TO 2450
          IQ = J/120
          IR =  dissoc_NMOL - IQ*120
          IRL = IR/3
          GO TO 2460

 2450     IF (MOD(J,120)) 2184,2460,2184

 2460     NBL = 0

          KD = KD + 120
          KF = KD + 119
          DO 2470  K1=KD,KF,3
            NBL = NBL + 1
            K2 = K1 + 1
            K3 = K1 + 2
            IF ( NBL.EQ.IRL + 1)  GO TO 2480
 2475       CONTINUE

            IF (MOD(NBL,5)) 2470,2500,2470

 2500       CONTINUE
 2470     CONTINUE
          GO TO 2184

 2480     IRR = IR - IRL*3
          IF (IRR .EQ. 0)  GO TO 2184
          GO TO (2482,2484), IRR
 2482     CONTINUE

          GO TO 2184
 2484     CONTINUE
 2184     CONTINUE
 1084   CONTINUE
 1020 CONTINUE

  100 FORMAT(' TO=',5E15.5)

      IF (VERBOSE) THEN !--verbose--!
        DO I=1,4
          WRITE(*,'(7E11.4)') (XP(ITX,I),ITX=1,modeles_NTOT)
        END DO
      END IF


      DO 51 ITX=1,modeles_NTOT
        sat4_PPH(ITX)=XP(ITX,1)
        sat4_PPC2(ITX)=XP(ITX,3)
        sat4_PN(ITX)=XP(ITX,4)
        sat4_PO(ITX)=XP(ITX,5)
        sat4_PC13(ITX)=XP(ITX,6)
        sat4_PTI(ITX)=XP(ITX,15)
        sat4_PMG(ITX)=XP(ITX,8)
        sat4_PNG(ITX)=XP(ITX,9)   ! ISSUE Not used
        sat4_PIG(ITX)=XP(ITX,10)  ! ISSUE Not used
        sat4_PFE(ITX)=XP(ITX,16)
   51  CONTINUE


      !=====
      ! Formats
      !=====

      RETURN
      END












C*****DIE9
C================================================================================================================================
      SUBROUTINE DIE(TEM,PG)
      REAL TEM, PG
      REAL PLOG, IPI

      REAL, DIMENSION(Z_MAX) :: FX, DFX, Z, PREV
      REAL, DIMENSION(MAX_dissoc_NMETAL) :: WA

      LOGICAL VERBOSE

      VERBOSE = config_DEBUG .AND. .FALSE.

      ECONST = 4.342945E-1
      EPSDIE = 5.0E-3
      T      = 5040.0/TEM
      PGLOG  = ALOG10(PG)
      ! orig HEH=HELIUM-TO-HYDROGEN RATIO BY NUMBER
      HEH    = CCOMP(Z_HE)/CCOMP(Z_H)

C     EVALUATION OF LOG KP(MOL)
      DO 1025 J =1, dissoc_NMOL
        APLOGJ = dissoc_C(J,5)
        DO 1026 K=1,4
          KM5 = 5-K
          APLOGJ = APLOGJ*T + dissoc_C(J,KM5)
 1026   CONTINUE
        APMLOG(J) = APLOGJ
 1025 CONTINUE

      DHH = (((0.1196952E-02*T-0.2125713E-01)*T+0.1545253E+00)*
     1  (-0.5161452E+01))*T+0.1277356E+02
      DHH = EXP(DHH/ECONST)

C     EVALUATION OF THE IONIZATION CONSTANTS
      TEM25 = TEM**2*SQRT(TEM)
      DO 1060 I = 1,dissoc_NMETAL
        NELEMI = dissoc_NELEMX(I)
        KP(NELEMI) =UIIDUI(NELEMI)*TEM25*EXP(-IP(NELEMI)*T/ECONST)
 1060 CONTINUE

      HKP = KP(Z_H)
      IF (T-0.6) 1084,1072,1072

C     PRELIMINARY VALUE OF PH AT HIGH TEMPERATURES
 1084 PPH = SQRT(HKP *(PG/(1.0+HEH)+HKP ))-HKP
      PH  = PPH**2/HKP
      GO TO 1102

C     PRELIMINARY VALUE OF PH AT LOW TEMPERATURES
 1072 IF (PG/DHH - 0.1) 1073,1073,1074
 1073 PH = PG/(1.0+HEH)
      GO TO 1102


 1074 PH = 0.5*(SQRT(DHH*(DHH+4.0*PG/(1.0+HEH)))-DHH)

C
C     EVALUATION OF THE FICTITIOUS PRESSURES OF HYDROGEN
C     PG=PH+PHH+2.0*PPH+HEH*(PH+2.0*PHH+PPH)
 1102 U = (1.0+2.0*HEH)/DHH
      Q = 1.0+HEH
      R = (2.0+HEH)*SQRT(HKP )
      S = -1.0*PG
      X = SQRT(PH)
      ITERAT = 0
 1103 F  = ((U*X**2+Q)*X+R)*X+S
      DF = 2.0*(2.0*U*X**2+Q)*X+R
      XR = X-F/DF
      IF (ABS((X-XR)/XR)-EPSDIE) 1105,1105,1106
 1106 ITERAT=ITERAT+1
      IF (ITERAT-50) 1104,1104,1107

 1107 IF (VERBOSE) PRINT 6108, TEM,PG,X,XR,PH !--verbose--!
 6108 FORMAT(1H1,'NOT CONVERGE IN DIE  TEM=', F9.2, 5X, 'PG=', E12.5, 5X
     1 'X1=', E12.5, 5X,'X2=', E12.5, 5X, 'PH=', E12.5)
      GO TO 1105

 1104 X = XR
      GO TO 1103

 1105 PH  = XR**2
      PHH = PH**2/DHH
      PPH = SQRT(HKP *PH)
      FPH = PH+2.0*PHH+PPH


      ! ISSUE Z=100 within dissoc.dat is only possible at the metals part (at the molecules part the Z slots have only 2 digits).
      ! THe current dissoc.dat has no Z=100 (neither 99).
      ! Is this a remaining fragment of code? My hint comes from the fact that Z_ELECTRON=99 is addressed several times, but Z_H_STAR=100 is not.
      P(Z_H_STAR) = PPH

C
C     EVALUATION OF THE FICTITIOUS PRESSURE OF EACH ELEMENT
      DO 1070 I=1,dissoc_NMETAL
        NELEMI = dissoc_NELEMX(I)
        FP(NELEMI) = CCOMP(NELEMI)*FPH
 1070 CONTINUE

C
C     CHECK OF INITIALIZATION
      PE = P(Z_ELECTRON)


      IF(PH-P(Z_H)) 1402,1402,1401
 1401 DO 1403 I=1,dissoc_NMETAL
        ! ISSUE: what if some NELEMI=Z_ELECTRON=99? THen P(99) will no longer be equal to PE
        NELEMI=dissoc_NELEMX(I)
        P(NELEMI) = FP(NELEMI)*EXP(-5.0*T/ECONST)
 1403 CONTINUE
      P(Z_H) = PH   ! ISSUE: overwriting P(1)

C
C     RUSSELL EQUATIONS
 1402 CONTINUE

      NITER = 0
 1040 DO 1030 I =1,dissoc_NMETAL
        NELEMI = dissoc_NELEMX(I)
        FX(NELEMI) = -FP(NELEMI) +
     @               P(NELEMI)*(1.0 + KP(NELEMI)/PE)  ! ISSUE if NELEMI=99, P(99) and PE are potentially not the same thing! Is this alright?
        DFX(NELEMI) = 1.0 + KP(NELEMI)/PE
 1030 CONTINUE

      SPNION = 0.0
      DO 1041 J=1,dissoc_NMOL
        MMAXJ  = dissoc_MMAX(J)
        PMOLJL = -APMLOG(J)
        DO 1042 M =1,MMAXJ
          NELEMJ = dissoc_NELEM(M,J)
          NATOMJ = dissoc_NATOM(M,J)
          PMOLJL = PMOLJL + FLOAT(NATOMJ)*ALOG10(P(NELEMJ))
 1042   CONTINUE
        IF(PMOLJL - (PGLOG+1.0) ) 1046,1046,1047
 1047   DO 1048 M =1,MMAXJ
          NELEMJ = dissoc_NELEM(M,J)
          NATOMJ = dissoc_NATOM(M,J)

          ! ISSUE BIG! at each iteration of the J loop, P gets divided by 100, is this correct??? Doesn't look like
          P(NELEMJ)=1.0E-2*P(NELEMJ)
          PMOLJL = PMOLJL + FLOAT(NATOMJ)*(-2.0)
 1048   CONTINUE

 1046   PMOLJ = EXP(PMOLJL/ECONST)
        DO 1044 M =1,MMAXJ
          NELEMJ = dissoc_NELEM(M,J)
          NATOMJ = dissoc_NATOM(M,J)
          ATOMJ = FLOAT(NATOMJ)

          IF (NELEMJ .EQ. Z_ELECTRON) THEN  ! ISSUE This bit suggests that Z=99 is allowed in the molecules part
            SPNION = SPNION + PMOLJ
          END IF

          DO 1043 I=1,dissoc_NMETAL
            NELEMI = dissoc_NELEMX(I)
            IF(NELEMJ .EQ. NELEMI) GO TO 1045
            GO TO 1043
 1045       FX(NELEMI) = FX(NELEMI) + ATOMJ*PMOLJ
            DFX(NELEMI) = DFX(NELEMI) + ATOMJ**2*PMOLJ/P(NELEMI)
 1043     CONTINUE
 1044   CONTINUE
        PPMOL(J) = PMOLJ
 1041 CONTINUE

C
C     SOLUTION OF THE RUSSELL EQUATIONS BY NEWTON-RAPHSON METHOD
      DO 2001 I=1,dissoc_NMETAL
        NELEMI=dissoc_NELEMX(I)
        WA(I)=ALOG10(P(NELEMI)+1.0E-30)
 2001 CONTINUE

      IMAXP1 = dissoc_NMETAL+1
      WA(IMAXP1) = ALOG10(PE+1.0E-30)
      DELTA = 0.0
      DO 1050 I=1,dissoc_NMETAL
        NELEMI = dissoc_NELEMX(I)
        PREV(NELEMI) = P(NELEMI) - FX(NELEMI)/DFX(NELEMI)
        PREV(NELEMI) = ABS(PREV(NELEMI))

        IF (PREV(NELEMI) .LT. 1.0E-30) PREV(NELEMI)=1.0E-30

        Z(NELEMI) = PREV(NELEMI)/P(NELEMI)
        DELTA = DELTA + ABS(Z(NELEMI) - 1.0)

        IF (dissoc_SWITER) 2500,2500,2501

 2501   P(NELEMI) = (PREV(NELEMI) + P(NELEMI) )*0.5
        GO TO 1050

 2500   P(NELEMI) = PREV(NELEMI)
 1050 CONTINUE

C
C     IONIZATION EQUILIBRIUM
      PEREV = 0.0
      DO 1061 I=1,dissoc_NMETAL
        NELEMI = dissoc_NELEMX(I)
        PEREV = PEREV + KP(NELEMI)*P(NELEMI)
 1061 CONTINUE

      PEREV = SQRT(PEREV/(1.0+SPNION/PE))
      DELTA = DELTA + ABS((PE-PEREV)/PE)
      PE = (PEREV + PE)*0.5  ! Note that it has an equivalence with the last element of P
      P(Z_ELECTRON)=PE

      IF (DELTA - dissoc_EPS) 1051,1051,1052

 1052 NITER = NITER + 1
      IF (NITER-dissoc_NIMAX) 1040,1040,1054

 1054 IF (VERBOSE) PRINT 6055, dissoc_NIMAX

 6055 FORMAT(1H0,39H *DOES NOT CONVERGE AFTER ITERATIONS OF,I4/////)



 1051 RETURN
      END







C================================================================================================================================
C READ_DISSOC(): reads file dissoc.dat to fill variables dissoc_*
C
C Original UNIT: 23
C
C This file must end with a blank row so that the routine can detect
C the end of the file


C TODO Various tests:
C TODO - mismatched NMETAL and metal rows
C TODO - check if NMETAL and NMOL match what they are supposed to (assertions in test)

C PROPOSE: use READ()'s "END=" option

      SUBROUTINE READ_DISSOC(filename)
      USE ERRORS
      INTEGER UNIT_
      INTEGER I
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename
      CHARACTER*128 S
      CHARACTER*2 SYMBOl

      ! Auxiliary temp variables for reading file
      INTEGER*4 NATOMM, NELEMM
      DIMENSION NATOMM(5), NELEMM(5)

      OPEN(UNIT=UNIT_,FILE=filename, STATUS='OLD')

C row 01
C =======
C NMETAL - NUMBER OF ELEMENTS CONSIDERED IN CHEMICAL EQUILIBRIUM
C NIMAX  - MAXIMUM NUMBER OF ITERATION IN NEWTON-RAPSON METHOD
C EPS    - IF ABS((X(I+1)-X(I))/X(I)).LE. EPS; CONVERGED
C SWITER - IF SWITER .GT. 0;   X(I+1)=0.5*(X(I+1)+X(I))
C          IF SWITER .LE. 0;   X(I+1)=X(I+1)
C
C Example:    18  100    0.005   -1.0
      READ(UNIT_,'(2I5, 2F10.5, I10)')
     1     dissoc_NMETAL, dissoc_NIMAX, dissoc_EPS, dissoc_SWITER

C rows 2 to NMETAL+1
C ==================
C 6 columns:
C   col 1 -- symbol of chemical element
C   col 2 -- atomic number "N"
C   col 3 -- (?)
C   col 4 -- (?)
C   col 5 -- (?)
C   col 6 -- (?)
      DO I = 1, dissoc_NMETAL
        READ (UNIT_, '(A2, 2X, I6, F10.3, 2I5, F10.5)')
     1        SYMBOL, dissoc_NELEMX(I), dissoc__IP(I),
     2        dissoc__IG0(I), dissoc__IG1(I), dissoc__CCLOG(I)



        ! Makes sure that elements first and second are H and HE, respectively,
        ! because SAT4() and DIE() count on this
        SELECT CASE (I)
          CASE (1)
            IF (SYMBOL .NE. 'H ')
     +       STOP 'First element must be hydrogen ("H ")!'
          CASE (2)
            IF (SYMBOL .NE. 'HE' .AND. SYMBOL .NE. 'He')
     +       STOP 'Second element must be helium ("He")!'
        END SELECT



        dissoc_ELEMS(I) = SYMBOL

        !--spill check--!
        IF (dissoc_NELEMX(I) .GT. MAX_Z) THEN
          WRITE(*,*) 'READ_DISSOC(): metal # ', I, ': NELEMXI = ',
     +     dissoc_NELEMX(I), ' over maximum allowed (',
     +     MAX_Z, ')'
          STOP ERROR_EXCEEDED
        END IF

      END DO

C rows NMETAL+2 till end-of-file
C ==============================
C   col  1     -- "name" of molecule
C   cols 2-6   -- C(J, 1-5)
C   col  7     -- MMAX(J) (number of subsequent columns)/2
C   cols 8-... -- Maximum of 8 columns here.
C                 Pairs (NELEM(M), NATOM(M)), M = 1 to MMAX(J) ISSUE NELEM(M) is atomic number, what about NATOM(M)???
      J = 0
 1010 J = J+1
C ISSUE: This 1X does not appear in my sample dissoc.dat file
C ISSUE: Atually the file that Beatriz sent me does not work under this format!!!!
C ISSUE: THere is no 1X
*      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
      READ(UNIT_, '(A3, 5X, E11.5, 4E12.5, I1, 4(I2,I1))')
     1             dissoc_MOL(J),
     2             (dissoc_C(J, K), K=1,5),
     3             dissoc_MMAX(J),
     4             (NELEMM(M), NATOMM(M), M=1,4)


      ! Check, TODO dissoc_MMAX(J) cannot be > 4

      ! TODO not tested, this
      FLAG_FOUND = .FALSE.
      DO M = 1, 4
        DO I = 1, dissoc_NMETAL
          IF (NELEMM(M) .EQ. dissoc_NELEMX(I)) THEN
            FLAG_FOUND = .TRUE.
            EXIT
          END IF
        END DO

        IF (.NOT. FLAG_FOUND) THEN
          WRITE(*,*) 'READ_DISSOC() molecule "', dissoc_MOL(J),
     +     '" atomic number ', NELEMM(M), 'not in atoms list above'
          STOP ERROR_BAD_VALUE
        END IF
      END DO


*
*        WRITE(*, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
*     1             dissoc_MOL(J),
*     2             (dissoc_C(J, K), K=1,5),
*     3             dissoc_MMAX(J),
*     4             (NELEMM(M), NATOMM(M), M=1,4)



      MMAXJ = dissoc_MMAX(J)
      IF(MMAXJ .EQ. 0) GO TO 1014  ! means end-of-file
      DO M = 1, MMAXJ
          dissoc_NELEM(M,J) = NELEMM(M)
          dissoc_NATOM(M,J) = NATOMM(M)
      END DO


*        WRITE(*, '(A3, 5X, E11.5, 4E12.5, 1X, I1, 4(I2,I1))')
*     1             dissoc_MOL(J),
*     2             (dissoc_C(J, K), K=1,5),
*     3             dissoc_MMAX(J),
*     4             (dissoc_NELEM(M, J), dissoc_NATOM(M, J), M=1,4)



      GO TO 1010

 1014 dissoc_NMOL = J-1

      CLOSE(UNIT=UNIT_)

      RETURN
      END


      END MODULE DISSOC




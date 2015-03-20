C Module "FILETOH"
C
C Routines that deal with the FILETOH files: reading and related calculations
C
C Variables
C   f_filetoh_* -- read directly from file
C   c_filetoh_* -- calculated
C ISSUE: The distinction above may be temporary, it is easier to merge prefixes than to split them
C

      MODULE FILETOH
      USE READ_FILES



C Globals filled at file reading
      ! MAX_filetoh_JJMAX = MAX_f_filetoh_JMAX*2-1
      INTEGER, PARAMETER :: filetoh_NP=7000, MAX_filetoh_JJMAX=99,
     +  MAX_filetoh_JMAX=50

      ! Read directly from file
      ! Note: PFANT doesn't use these variables, they are useful only for the unit testing  
      REAL*8 f_filetoh_LAMBDH
      CHARACTER f_filetoh_TITRE*80, f_filetoh_TTT*11
      DIMENSION      f_filetoh_TH(MAX_f_filetoh_JMAX, MAX_modeles_NTOT)
     +           f_filetoh_LAMBDH(MAX_f_filetoh_JMAX),
     
     
      ! Calculated for external use
      DIMENSION c_filetoh_TAUHI(filetoh_NP, MAX_modeles_NTOT)
      INTEGER c_filetoh_DHMI, c_filetoh_DHPI


      ! Private variables, shared between routines below
      REAL(8), PRIVATE, DIMENSION(MAX_filetoh_JJMAX) :: mi_LLAMBDH         
      REAL, PRIVATE, DIMENSION(MAX_filetoh_JJMAX, MAX_modeles_NTOT) :: 
     +  mi_TTH
      REAL, PRIVATE, DIMENSION(MAX_filetoh_JJMAX) :: 
     +  mi_ALLH, mi_TAUHN
      REAL, PRIVATE, DIMENSION(filetoh_NP) :: 
     +  mi_FTTH(filetoh_NP)
      INTEGER, PRIVATE :: JJMAX


      SAVE

C     ========
      CONTAINS
C     ========



C================================================================================================================================
C READ_FILETOH()
C
C orig LECTURE DE LA PROFONDEUR OPTIQUE DANS LA RAIE D H
C
C Original UNIT: 16

      SUBROUTINE READ_FILETOH(filename)
      INTEGER UNIT_
      PARAMETER(UNIT_=199)
      CHARACTER*256 filename

      OPEN(UNIT=UNIT_,FILE=filename,STATUS='OLD')
      READ(UNIT_,'(A80)') f_filetoh_TITRE
      READ(UNIT_,'(I4)') f_filetoh_TTT
      READ(UNIT_,'(I4)') f_filetoh_JMAX
      READ(UNIT_,'(5F14.3)') (f_filetoh_LAMBDH(J), J=1,f_filetoh_JMAX)
      READ(UNIT_,'(5E12.4)') 
     +     ((f_filetoh_TH(J,N), J=1,f_filetoh_JMAX), N=1,modeles_NTOT)

      ! TODO Check JMAX spill here

      CLOSE(UNIT_)
      END
      


C================================================================================================================================
C Find better name()
C
C Note: this is originally subroutine "LECTAUH" without the file reading part
C
C Arguments:
C   DTOT -- Number of calculation steps, I think. ISSUE: better explanation
C           Calculated as: DTOT = (LFIN-LZERO)/main_PAS + 1.0005
C   TTD -- ISSUE: get explanation
C          Calculated as: TTD(D) = ALZERO+main_PAS*(D-1)
C   ILZERO -- integer version of variable LZERO in main module


      ! Issue: Find better name for this routine.
      SUBROUTINE FILETOH_AUH(DTOT, TTD, ILZERO)
      INTEGER DTOT, ILZERO
      REAL, DIMENSION(filetoh_NP) :: TTD
      
      INTEGER D

      !--verbose--!
      LOGICAL VERBOSE
      VERBOSE = .TRUE. .AND. config_ALLOW_VERBOSE
      !-----------!

      JJMAX = 2*f_filetoh_JMAX-1
      JMA1 = f_filetoh_JMAX-1
      DO JJ = 1, f_filetoh_JMAX
        DEL = f_filetoh_LAMBDH(f_filetoh_JMAX+1-JJ)-f_filetoh_LAMBDH(1)
        mi_LLAMBDH(JJ) = f_filetoh_LAMBDH(f_filetoh_JMAX+1-JJ)-2*DEL
      END DO
      DO JJ = f_filetoh_JMAX+1, JJMAX
        mi_LLAMBDH(JJ) = f_filetoh_LAMBDH(JJ-JMA1)
      END DO
      DO N = 1, modeles_NTOT
        DO JJ = 1, f_filetoh_JMAX
          mi_TTH(JJ, N) = f_filetoh_TH(f_filetoh_JMAX+1-JJ, N)
        END DO
        DO JJ = f_filetoh_JMAX+1, JJMAX
          mi_TTH(JJ, N) = f_filetoh_TH(JJ-JMA1, N)
        END DO
      END DO
      
      !--verbose--!
      IF (VERBOSE) THEN
        WRITE(6,'(A80)') f_filetoh_TITRE
        WRITE(6,'(A11)') f_filetoh_TTT
        WRITE(6,'('' f_filetoh_JMAX='',I3)') f_filetoh_JMAX
        WRITE(6,'(2X,5F14.3)') (mi_LLAMBDH(JJ), JJ=1,JJMAX)

        WRITE(6,'(2X,5F14.3)') (mi_LLAMBDH(JJ), JJ=1,JJMAX)
        
        DO N = 1,modeles_NTOT,5
          WRITE(6,'('' N='',I3)') N
          WRITE(6,'(2X,5E12.4)') (mi_TTH(JJ,N), JJ=1,JJMAX)
        END DO
      END
      !-----------!      

      DO J = 1,JJMAX
        mi_ALLH(J) = mi_LLAMBDH(J)-ILZERO
      END DO
      
      !--verbose--!
      IF (VERBOSE) THEN
        WRITE(6, '('' mi_ALLH(1)='',F8.3,2X,''mi_ALLH(JJMAX)='',F8.3,2X)')
     +        mi_ALLH(1),mi_ALLH(JJMAX)
        WRITE(6, '('' JJMAX='',I3,2X,''NTOT='',I3,2X,''DTOT='',I5)')
              JJMAX, modeles_NTOT, DTOT
      END IF
      !-----------!
      
      DO N = 1,modeles_NTOT
            DO J = 1,JJMAX
              mi_TAUHN(J) = mi_TTH(J,N)
            END DO
            
            CALL FTLIN3H(DTOT,TTD)
            
            DO D = 1,DTOT
              c_filetoh_TAUHI(D, N) = mi_FTTH(D)
            END DO
      END DO
      
      !--verbose--!
      IF (VERBOSE) THEN
        WRITE(6,'('' TAUHI(1,1)='',E14.7,2X,''TAUHI(1,NTOT)='',E14.7)') 
     +        c_filetoh_TAUHI(1,1), c_filetoh_TAUHI(1,modeles_NTOT)
        WRITE(6,'('' TAUHI(DTOT,1)='',E14.7,2X,'
     +        //'''TAUHI(DTOT,NTOT)='',E14.7)')
     +        c_filetoh_TAUHI(DTOT,1), c_filetoh_TAUHI(DTOT,modeles_NTOT)
      END IF
      !-----------!
      
      RETURN
      END


C-------------------------------------------------------------------------------
C ISSUE: Get description for this routine, what it does
      SUBROUTINE FTLIN3H(DTOT,TTD)
      INTEGER DTOT
      REAL, DIMENSION(filetoh_NP) :: TTD
      
      J=2
      KK=1
24    DO K = KK,DTOT
        KQ=K
        T=TTD(K)

        JJ=J-1
        DO 1  J=JJ,JJMAX
          IF(T-mi_ALLH(J) ) 3,2,1
1         CONTINUE
          GO TO 10
2       FT=mi_TAUHN(J)
        IF(J .EQ. 1) J = J+1
        GO TO 4
        
3       IF (J .EQ. 1) GO TO 10
        U0 = mi_TAUHN(J)-mi_TAUHN(J-1)
        T0 =  mi_ALLH(J)- mi_ALLH(J-1)
        T1 =        T- mi_ALLH(J-1)
        
        T2= T1/T0
        DY= U0*T2
        FT= mi_TAUHN(J-1) + DY
        mi_FTTH(K) = FT
      END DO

14    CONTINUE

      DO K=1,DTOT
        IF(mi_FTTH(K).NE.0.0) GO TO 20
      END DO
      
20    c_filetoh_DHMI = K

      IF (c_filetoh_DHMI .EQ. DTOT) c_filetoh_DHMI = 1       
      KK1 = c_filetoh_DHMI+1
      DO K = KK1,DTOT
        IF (mi_FTTH(K) .EQ. 0.0) GO TO 30
      END DO
      
30    c_filetoh_DHPI = K

      ! (Paula Coelho 21/11/04) instrucao da Marie Noel
      IF (mi_FTTH(DTOT) .NE. 0.0) c_filetoh_DHPI = DTOT

      RETURN
      
10    mi_FTTH(K) = 0.
      J = J+1
      
      KK = KQ
      KK = KK+1
      IF (KQ .GT. DTOT) GO TO 14
      GO TO 24
    
      END

      END MODULE FILETOH

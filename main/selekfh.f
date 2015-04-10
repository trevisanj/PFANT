! TODO Fix Initializations
! TOdO explain parameters
! TODO verbose
! TODO discover what is input and what is output

      MODULE SELEKFH


C     ========
      CONTAINS
C     ========


C-------------------------------------------------------------------------------
C Sets the Voigt profile using Hjertings' constants.
C
C Note: convolution for molecules uses Gaussian profile.
C

! ISSUE with variable MM
      SUBROUTINE SELEKFH(KIK,    ! 0/1, passed to FLINH
     +                   DTOT,   ! ?
     +                   GFAL, 
     +                   ABOND, 
     +                   ECART, 
     +                   TAUH,
     +                   DHM,
     +                   DHP,
     +                   VT,
     +                   B, 
     +                   B1,
     +                   B2,
     +                   KCD,
     +                   POP,
     +                   DELTA,
     +                   A,
     +                   TTD,
     +                   FL,
     +                   FCONT,
     +                   ECARTM
     +                  )
      USE READ_FILES
      IMPLICIT NONE
      PARAMETER(PARAMETER_NMOL=50000,NP=7000)
      INTEGER D, DTOT, DHM,DHP
      REAL lambi
      REAL KAPPA,KA,KAP,KCD,KCI,KAM,KAPPAM,KAPPT
      REAL*8 ECART,ECAR,ECARTM,ECARM
      DIMENSION VT(50)
      DIMENSION B(0:50),B1(0:50),B2(0:50),BI(0:50)
      DIMENSION ECART(MAX_atomgrade_NBLEND),
     +          ECAR(MAX_atomgrade_NBLEND),
     +          ECARTL(MAX_atomgrade_NBLEND),
     +          GFAL(MAX_atomgrade_NBLEND),
     +          ABOND(MAX_atomgrade_NBLEND),
     +          KA(MAX_atomgrade_NBLEND),
     +          KAP(50),
     +          KAPPA(50),
     +          KCD(NP,50),
     +          KCI(50),
     +          POP(MAX_atomgrade_NBLEND,50),
     +          DELTA(MAX_atomgrade_NBLEND,50),
     +          A(MAX_atomgrade_NBLEND,50)

      DIMENSION TTD(NP),FL(NP),TAUHD(50),TAUH(NP,50)
      DIMENSION FCONT(NP)
      DIMENSION DELTAM(PARAMETER_NMOL,50),
     +          ECARTM(PARAMETER_NMOL),
     +          ECARM(PARAMETER_NMOL),
     +          ECARTLM(PARAMETER_NMOL),
     +          KAM(PARAMETER_NMOL),
     +          KAPPAM(50),
     +          KAPPT(50)

      DATA DEUXR/1.6634E+8/,RPI/1.77245385/,C/2.997929E+10/
C

      IF (atomgrade_NBLEND .NE. 0) then
        DO K = 1,atomgrade_NBLEND
          ECAR(K) = ECART(K)
        END DO
      END IF
      
      IF (km_MBLEND .ne. 0) then
        DO K=1,km_MBLEND
          ECARM(K)=ECARTM(K)
        END DO
      end if
      
      DO D=1,DTOT
        lambi = (6270+(D-1)*0.02)
        if (atomgrade_NBLEND .ne. 0) then
          DO K=1,atomgrade_NBLEND
            ECAR(K)=ECAR(K)-main_PAS
            ECARTL(K)=ECAR(K)
          END DO
        end if
        
        if(km_MBLEND.ne.0) then
          DO K=1,km_MBLEND
            ECARM(K) = ECARM(K)-main_PAS
            ECARTLM(K) = ECARM(K)
          END DO
        end if
      
        DO N = 1,modeles_NTOT
          KAPPA(N) =0.
          KAPPAM(N) =0.
          T = 5040./modeles_TETA(N)
          
          ! atomes
          if(atomgrade_NBLEND.eq.0) go to 260

          DO  K=1,atomgrade_NBLEND
            IF( ABS(ECARTL(K)) .GT. atomgrade_ZINF(K) )  THEN
              KA(K)=0.
            ELSE
              V=ABS(ECAR(K)*1.E-8/DELTA(K,N))
              CALL HJENOR(A(K,N),V,DELTA(K,N),PHI)
              KA(K) = PHI * POP(K,N) * GFAL(K) * ABOND(K)
              IF(K.eq.1)KA(K) = PHI * POP(K,N) * GFAL(K)

            END IF
            KAPPA(N) = KAPPA(N) + KA(K)
          END DO   !  fin bcle sur K

260       CONTINUE

          ! molecule
          IF(km_MBLEND.EQ.0) GO TO 250
          DO L=1,km_MBLEND
            IF( ABS(ECARTLM(L)) .GT. km_ALARGM(L) )  then
              KAM(L)=0.
            else
          
              ! ISSUE uses MM, which is read within KAPMOL and potentially has a different value for each molecule!!!!! this is very weird
              ! Note that km_MM no longer exists but it is the ancient "MM" read within ancient "KAPMOL()"
              DELTAM(L,N)=(1.E-8*km_LMBDAM(L))/C*SQRT(VT(N)**2+DEUXR*T/km_MM)
              VM=ABS(ECARM(L)*1.E-08/DELTAM(L,N))
              PHI=(EXP(-VM**2))/(RPI*DELTAM(L,N))
              KAM(L)=PHI*km_GFM(L)*km_PNVJ(L,N)
            end if
            KAPPAM(N)=KAPPAM(N)+KAM(L)
          END DO   !  fin bcle sur L
        
250       KAPPT(N)=KAPPA(N)+KAPPAM(N)
          KCI(N)=KCD(D,N)
          KAP(N)=KAPPT(N)+KCI(N)
          BI(N)=((B2(N)-B1(N))*(FLOAT(D-1)))/(FLOAT(DTOT-1)) + B1(N)
        END DO    ! fin bcle sur N
        
        BI(0)=((B2(0)-B1(0))*(FLOAT(D-1)))/(FLOAT(DTOT-1)) + B1(0)
        
        IF(D.EQ.1) WRITE(6,151) D,BI(0),BI(1),BI(modeles_NTOT)
        IF(D.EQ.1) WRITE(6,150) D, KCI(1),KCI(modeles_NTOT),KAPPA(1),KAPPA(modeles_NTOT)
        IF(D.EQ.1) WRITE(6,152) KAPPAM(1),KAPPAM(modeles_NTOT)
        
c       WRITE(6,151) D,BI(0),BI(1),BI(modeles_NTOT)
c       WRITE(6,150) D, KCI(1),KCI(modeles_NTOT),KAPPA(1),KAPPA(modeles_NTOT)
c       WRITE(6,152) KAPPAM(1),KAPPAM(modeles_NTOT)
        
        !--verbose--!
        IF (VERBOSE .AND. D .EQ. DTOT) THEN 
          WRITE(6,151) D,BI(0),BI(1),BI(modeles_NTOT)
          WRITE(6,150) D,KCI(1),KCI(modeles_NTOT),KAPPA(1),KAPPA(modeles_NTOT)
          WRITE(6,152)KAPPAM(1),KAPPAM(modeles_NTOT)
        END IF
        
        IF((D.LT.DHM).OR.(D.GE.DHP)) THEN
          CALL FLIN1(KAP,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,KIK)
          FL(D) = flin_F
          IF (flin_CAVA.GT.1) THEN
            WRITE(6,131) TTD(D),CAVA
            STOP
          END IF
          
c         FN(D) = FL(D) / FCONT(D)
        ELSE
          DO N = 1,modeles_NTOT
              TAUHD(N) = TAUH(D,N)
          END DO
          CALL FLINH(KAP,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,
     +     KIK,TAUHD)
          FL(D) = flin_F
          IF(CAVA .GT. 1) THEN
            WRITE(6,131) TTD(D),CAVA
            STOP
          END IF
        END IF
            
        ! Dez 03-P. Coelho - calculate the continuum and normalized spectra
        CALL FLIN1(KCI,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,KIK)
        FCONT(D) = flin_F
        ! TODO Not checking CAVA, really gotta make it STOP from within FLIN_
      END DO  ! fin bcle sur D
      
131   FORMAT(' ENNUI AU CALCUL DU FLUX (CF LIGNE PRECEDENTE)',
     1   ' A LAMBD=',F10.3,'     CAVA=',I3)
150   FORMAT(' D=',I5,2X,'KCI(1)=',E14.7,2X,'KCI(NTOT)=',E14.7,
     1 /,10X,'KAPPA(1)=',E14.7,2X,'KAPPA(NTOT)=',E14.7)
152   FORMAT(10X,'KAPPAM(1)=',E14.7,2X,'KAPPAM(NTOT)=',E14.7)
151   FORMAT(' D=',I5,2X,'BI(0)=',E14.7,2X,'BI(1)=',E14.7,2X,
     1 'BI(NTOT)=',E14.7)
      RETURN
      END


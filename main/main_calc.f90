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

! TODO Explain that each [subroutine] module declares the variables that it calculates
! TODO module dependence map
!

! 20150505 -- Comments by Paula Coelho in 2003
!===PC2003 BEGIN===
! ======================================================================
!     >> pfantgrade.f << NOV 2003
!
!     este codigo eh uma uniao do codigo pfant03.f e do
!     pfant01h.f feita pela Paula Coelho e Jorge Melendez. O
!     codigo pfant03.f calculava apenas 4 linhas de hidrogenio enquanto
!     que pfant01.h (MNoel) calculava 10 linhas.
!     Esta relatado a seguir as modificacoes feitas ao pfant03.f
!     para chegar ao presente codigo.
!
!     Passos realizados (nov/2003):
!     1) copiei todos os fontes que estavam em /home1/barbuy/pfant03
!
!     2) troquei nome do arquivo de atomos para 'atomgrade.dat' e o de
!     moleculas para 'moleculagrade.dat'
!
!     3) examinei os codigos pabsor.f pcalr98bht.f, pncalr98.f e
!     psatox95t.f e retirei as rotinas que eram obsoletas e nao eram
!     mais utilizadas (rotinas RETIRADAS => pcalr98bht.f: cafconvh,
!     voluteh / pncalr98.f: bkf, ediga, equiv, flin2, flin2b, inait,
!     inaitb, largq, popul2, quid, selekf, trangx, step, stepb,
!     volute, naitk3, gam, xxsol.
!
!     4) juntei os arquivos limpos conforme item anterior pabsor.f,
!     pcalr98bht.f, pfant03.f, pncalr98.f psatox95t.f em um
!     UNICO FONTE PFANTGRADE.F. Dessa forma, apenas o pkapgeralgrade.f
!     continua sendo um arquivo externo, devido a sua atualizacao ser
!     SEMPRE paralela com alteracoes no arquivo de moleculas.
!
!     Portanto, a nova forma de compilar o programa eh
!     f90 -o pfantgrade pfantgrade.f pkapgeralgrade.f
!
!     5) Aumentei o tamanho maximo do espectro total possivel de
!     ser calculado de 10000A para 20000A.
!
!     6) AQUI COMECAM AS ALTERACOES DEVIDO AO CALCULO DAS LINHAS DE H
!
!     a. linhas de codigo foram comentadas (identificadas com 'cp Nov03':
!     (comentario da Paula em Nov03).
!
!     b. dimensao e data de LLHY e dimensao de main_FILETOHY foram
!     atualizadas
!
!     c. incluidos c_filetoh_TAUHI(NP,50),TAUHY(10,NP,50) e excluido IHH(500)
!
!     d. todo o codigo que se referia ao calculo das linha de H foram
!     ocultados, e o codigo a isto referente que estava em pfant01.h
!     foi acrescentado (correspondo ao codigo na secao
!     LECTURE TAU RAIE HYDROGENE ET INTERPOLATION DE TAUH ).
!
!     e. segundo as instrucoes enviadas pela Marie Noel em 2001:
!     - na rotina FTLIN3H foi incluida a linha
!     'if (ftt(itot).ne.0.0) k2=itot'
!     - DTOT foi substituido por NP nas dimensoes das matrizes
!           BK : TTD(NP), bk_KCD(NP,5)
!        LECTAUH : TTD(NP)
!        SELEKFH : TTD(NP), bk_KCD(NP, 50), selekfh_FL(NP), TAUH(NP,50)`
!
!
!     Tambem reduzi o numero de comentario que vao para a tela
!     'write(6...)' := cpc
!
! ========================================================================
!
!     Alteracao para calcular simultaneamente o continuo selekfh_FCONT(NP) e o
!     espectro normalizado FN(NP) {Paula, dez 2003}
!
!     - acrescentei as variaveis FN, selekfh_FCONT, FILEFLUX2, FILEFLUX3
!     - abro mais dois arquivos binarios unit=19 (continuo) e 20 (normalizado)
!     - rotina SELEKFH:
!           - recebe tbem selekfh_FCONT e FN
!           - selekfh_FCONT eh calculado passando p/ a rotina FLIN1 apenas o
!           coeficiente de absorcao do continuo
!     - FN = selekfh_FL / selekfh_FCONT
!     - escrevo nos devidos arquivos
!
!     Portanto, para diferenciar os arquivos binarios criados,
!     alem do arquivo normal criado como 'spe.' + nome no main.dat
!     o pfant cria mais dois arquivos que comecam com 'cont.' e 'norm.'
!===PC2003 END=== 


!> Fantomol avec sous-programmes (MNP) -
!> Calcul possible de 100 angstrom en 100 angstrom.
!> Flux sortant est en nu: Fnu x lambda
!> Flux absolu sortant a ete multiplie par 10**5
MODULE SYNTHESIS
  USE READ_FILES


  !> @TODO ISSUE this is used a lot, I gotta find out its meaning
  !> ISSUE I think it is the maximum number of calculation steps.
  INTEGER, PARAMETER :: NP = 7000, &
                        NMOL = 50000  ! ISSUE what about this parameter, what does it mean?

  !=====
  ! Subroutine outputs
  !=====
  ! The following variables have the prefix of the subroutine that is responsible for them.

  !> Calculated by subroutine POPUL
  REAL*8, DIMENSION(3,MAX_partit_NPAR, MAX_modeles_NTOT) :: popul_P


  !> Calculated by subroutine TURBUL
  REAL*8, DIMENSION(MAX_modeles_NTOT) :: turbul_VT


  !> Calculated by subroutine POPADELH
  REAL*8, DIMENSION(MAX_atomgrade_NBLEND) :: popadelh_CORCH, popadelh_CVdW
  !> Calculated by subroutine POPADELH
  REAL*8, DIMENSION(MAX_atomgrade_NBLEND,MAX_modeles_NTOT) :: &
   popadelh_POP, popadelh_A, popadelh_DELTA


  !> Calculated by subroutine SELEKFH
  REAL*8, DIMENSION(NP) :: selekfh_FL, selekfh_FCONT


  !> Calculated by subroutine BK
  REAL*8, DIMENSION(0:MAX_modeles_NTOT) :: bk_B, bk_B1, bk_B2
  !> Calculated by subroutine BK
  REAL*8, DIMENSION(MAX_modeles_NTOT) :: bk_KC, bk_KC1, bk_KC2, bk_PHN, bk_PH2
  !> Calculated by subroutine BK
  REAL*8, DIMENSION(NP, MAX_modeles_NTOT) :: bk_KCD
  !> Calculated by subroutine BK
  REAL*8, DIMENSION(NP) :: bk_FC



  !=====
  ! Constants available to all subroutines within this module
  !=====

  REAL*8, PARAMETER, DIMENSION(10) :: &
   LLHY = (/3750.150, 3770.630, 3797.900, 3835.390, 3889.050, &
   3970.076, 4101.748, 4340.468, 4861.332, 6562.817/)
  REAL*8, PARAMETER :: &
    C = 2.997929E+10,  &
    H = 6.6252E-27,    & 
   KB = 1.38046E-16,   &
   PI = 3.141593,      &
   C1 = 4.8298E+15,    &
   C2 = 8.8525E-13,    &
   C4 = 2.1179E+8,     & 
   C6 = 3.76727E+11,   &
   DEUXR = 1.6634E+8,  &
   RPI = 1.77245385

  REAL*8, PARAMETER :: C5 = 2.*PI* (3.*PI**2/2.44)**0.4


  CONTAINS



  !======================================================================================================================      
  SUBROUTINE PFANT_CALCULATE()  ! ISSUE Find a nicer name for this routine. It is what PFANT DOES
    REAL*8 LZERO, LFIN
    CHARACTER*256 FILEFLUX, FILEFLUX2,FILEFLUX3

    INTEGER D,DTOT
    INTEGER DHM,DHP,DHMY,DHPY
    
    CHARACTER FILETOH*260
    
    REAL*8 LAMBD, LZERO, LFIN, L0, LF, LLLHY
          
    REAL*8 GFAL(MAX_atomgrade_NBLEND), ECART(MAX_atomgrade_NBLEND), &
     FI(1501),TFI(1501), ECARTM(NMOL)
!   fonctions de partition TODO figure out what this comment refers to

    REAL*8 TTD(NP), FN(NP), &
           TAUH(NP, 50), TAUHY(10,NP,50), &
           DHMY(10), DHPY(10)

    ! Units for output files
    INTEGER, PARAMETER ::
     UNIT_SPEC  = 17, &
     UNIT_CONT  = 19, &
     UNIT_NORM  = 20, &
     UNIT_LINES = 32, &
     UNIT_LOG   = 31 



    !=====
    ! Read/setup
    !=====


    ! dissoc.dat needs to be read first because READ_MAIN() depends on dissoc_NMETAL
    CALL READ_DISSOC(config_FN_DISSOC)
    CALL READ_MAIN(config_FN_MAIN)
    CALL READ_PARTIT(config_FN_PARTIT)  ! LECTURE DES FCTS DE PARTITION
    CALL READ_ABSORU2(config_FN_ABSORU2)  ! LECTURE DES DONNEES ABSORPTION CONTINUE
    CALL READ_MODELE(config_FN_MODELES)  ! LECTURE DU MODELE
    CALL READ_ABONDS(config_FN_ABONDS)
    CALL READ_ATOMGRADE(config_FN_ATOMGRADE)

    TETAEF = 5040/main_TEFF


    !-----
    ! Output files opened here and left open until the end
    !-----
    !FILEFLUX1 = TRIM(main_FILEFLUX)//'.spec'
    !FILEFLUX2 = TRIM(main_FILEFLUX)//'.cont'
    !FILEFLUX3 = TRIM(main_FILEFLUX)//'.norm'
    !OPEN(UNIT=UNIT_SPEC,FILE=FILEFLUX1,STATUS='unknown')
    !OPEN(UNIT=UNIT_CONT,FILE=FILEFLUX2,STATUS='unknown')
    !OPEN(UNIT=UNIT_NORM,FILE=FILEFLUX3,STATUS='unknown')
    OPEN(UNIT=UNIT_SPEC, FILE=TRIM(main_FILEFLUX)//'.spec', STATUS='unknown')  ! spectrum
    OPEN(UNIT=UNIT_CONT, FILE=TRIM(main_FILEFLUX)//'.cont', STATUS='unknown')  ! continuum
    OPEN(UNIT=UNIT_NORM, FILE=TRIM(main_FILEFLUX)//'.norm', STATUS='unknown')  ! normalized
    OPEN(UNIT=UNIT_LINES,FILE=config_FN_LINES, STATUS='UNKNOWN')               ! lines.pfant
    OPEN(UNIT=UNIT_LOG,  FILE=config_FN_LOG, STATUS='UNKNOWN')                 ! log.log



    !=====
    ! Calculation begins!
    !=====

    CALL TURBUL()     

    ! -- III --
    ! CALCUL DE QUANT  NE DEPENDANT QUE DU METAL ET DU MODELE
    ! POPULATION DU NIV FOND DES IONS
    CALL POPUL(modeles_TETA,modeles_PE,modeles_NTOT,partit_TINI,partit_PA,partit_JKMAX,partit_KI1,partit_KI2,partit_NPAR,partit_TABU,popul_P)

    ! -- IV --
    ! CALCUL DES QUANTITES NE DEPENDANT QUE DU
    ! MODELE ET DE LAMBDA : bk_B(N)   bk_KC(N)   bk_FC
    CALL SAT4()


    ! ISSUE Explain what it does
    XLZERO = main_LLZERO-20.
    XLFIN = XLZERO+main_AINT+20.
    IF(XLFIN .GE. (main_LLFIN+20.)) THEN
      IKEYTOT = 1
    ELSE
      ! ISSUE it seems that I could write this using a modulus operator
      DO I = 2,250
        XLFIN = XLFIN+main_AINT
        IF(XLFIN .GE. (main_LLFIN+20.)) EXIT
      END DO
      IKEYTOT = I
    END IF

    LZERO = main_LLZERO-20.
    LFIN = LZERO+main_AINT+20.
    IKEY = 1



    !=====
    ! Main loop
    !=====
    DO WHILE .T. !Main loop!
      ! ISSUE Explain DTOT
      DTOT = (LFIN-LZERO)/main_PAS + 1.0005
      
      !__logging__
      117 FORMAT(5X,'LZERO=',F10.3,10X,'LFIN=',F10.3,5X,'DTOT=',I7)
      WRITE(LLL, 117) LZERO, LFIN, DTOT
      CALL LOG_INFO(LLL)

      !--halt situation--!      
      IF(DTOT .GT. 40000) THEN
        ! ISSUE: DOCUMENT THIS; see arrays that may be blown and therefore tie this 40000 to some parameter
        CALL PFANT_HALT('DTOT > 40000!')
      END IF

      LAMBD = (LZERO+LFIN)/2
      ILZERO = (LZERO/100.)*1E2
      ALZERO = LZERO -ILZERO

      DO D = 1,DTOT
        TTD(D) = ALZERO+main_PAS*(D-1)
      END DO

      CALL BK(LAMBD,TTD,DTOT,config_KIK,LZERO,LFIN)

      !__logging__
      501 FORMAT(2X,2X,'LLZERO=',F10.3,2X,'LLFIN=',F10.3,  2X,'LZERO=',F10.3,2X,'LFIN=',F10.3,2X,'LAMBD 1/2=',F10.3)
      WRITE(LLL,501) main_LLZERO,main_LLFIN,LZERO,LFIN,LAMBD
      CALL LOG_INFO(LLL)

      ! LECTURE TAU RAIE HYDROGENE ET INTERPOLATION DE TAUH
      ! Type *,' nom des fichiers TAU raies Hydrogene'
      IM = 0
      DO IH = 1,10
        ALLHY = LLHY(IH)-LZERO
        LLLHY = LLHY(IH)
        IF (((ALLHY .GT. 0) .AND. (ALLHY .LE. (main_AINT+55.))) .OR. &
            ((ALLHY .LT. 0.) .AND. (ALLHY .GE. (-35.)))) THEN
          IM = IM+1
          IRH = 1
          IHT = IH
          FILETOH = main_FILETOHY(IHT)

          !__logging__
          712 FORMAT(1X,'IM=',I3,2X,'Lambda H=',F8.3,2X,A,2X,'IH=',I5)
          WRITE(LLL,712) IM, LLHY(IH), FILETOH, IHT
          CALL LOG_INFO(LLL)

          ! ISSUE Extract this from main loop. Not too hard: c_filetoh_* just need one extra dimension
          CALL READ_FILETOH(FILETOH)
          CALL FILETOH_AUH(DTOT,TTD, ILZERO)

          DHMY(IM) = c_filetoh_DHMI
          DHPY(IM) = c_filetoh_DHPI
          DO N = 1,modeles_NTOT
            DO D = 1,DTOT
              TAUHY(IM, D, N) = c_filetoh_TAUHI(D,N)
            END DO
          END DO
        END IF
      END DO

      IMY = IM
      IF(IMY .NE. 0) THEN
        !__logging__
        WRITE(LLL,*) (DHMY(IM), IM=1,IMY)
        CALL LOG_DEBUG(LLL)
        WRITE(LLL,*) (DHPY(IM), IM=1,IMY)
        CALL LOG_DEBUG(LLL)

        DHP = MAXI(DHPY, IMY, 1, IMY)
        DHM = MINI(DHMY, IMY, 1, IMY)
        DO N = 1,modeles_NTOT
          DO D = 1,DTOT
            TAUH(D,N) = 0.0
          END DO
        END DO

        DO N = 1,modeles_NTOT
          DO D = 1,DTOT
            DO IM = 1,IMY
              TAUH(D,N) = TAUH(D,N)+TAUHY(IM,D,N)
            END DO
          END DO
        END DO
      ELSE
        IRH=0
        DHM=0
        DHP=0
      END IF


      ! -- V --
      ! QUANTITES DEPENDANT DE LA RAIE ET DU MODELE
      CALL FILTER_ATOMGRADE(LZERO, LFIN)

      IF(atomgrade_NBLEND .GT. 0) THEN
        CALL POPADELH()

        ! -- VI --
        ! CALCUL DU COEFFICIENT D ABSORPTION SELECTIF ET CALCUL DU SPECTRE
        DO K = 1, atomgrade_NBLEND
        
          ! IssuE check these variables, they may be misnamed
          GFAL(K) = atomgrade_GF(K)*C2*(atomgrade_LAMBDA(K)*1.E-8)**2
          
          ! issuE What is ECART?
          ECART(K) = atomgrade_LAMBDA(K)-LZERO+main_PAS
        END DO
      END IF

      CALL FILTER_MOLECULAGRADE()
      CALL USE_MOLECULAGRADE()

      !--debugging--!
      704 FORMAT(1X,'MBLEND=',I10)
      WRITE(LLL, 704) km_MBLEND
      CALL LOG_DEBUG(LLL)

      DO L = 1, km_MBLEND
        ECARTM(L) = km_LMBDAM(L)-LZERO + main_PAS
      END DO

      CALL SELEKFH(DTOT, GFAL, ECART, TAUH, DHM,DHP, TTD, ECARTM)

      ! TODO check if any of these variables is written, otherwise I could move this block further down
      LI = 10./main_PAS
      I1 = LI+1
      I2 = DTOT - LI
      IF (LFIN .GE. (main_LLFIN+20.)) THEN
        I2 = (main_LLFIN+10.-LZERO)/main_PAS + 1.0005
      END IF
      ITOT = I2-I1+1
      DO D = I1,I2
        selekfh_FL(D) = selekfh_FL(D)*(10.**5)
        selekfh_FCONT(D) = selekfh_FCONT(D)*(10.**5)
        FN(D) = selekfh_FL(D) / selekfh_FCONT(D)  ! normalized spectrum
      END DO
      L0 = main_LLZERO-10.
      LF = main_LLFIN+10.




    
      !=====
      ! Writes results for current iteration into open files
      !=====
      CALL WITE_LINES_FORT91()                        ! lines.pfant and fort.91
      CALL WRITE_LOG()                                ! log.log
      CALL WRITE_SPEC_ITEM(UNIT_SPEC, selekfh_FL)     ! spectrum
      CALL WRITE_SPEC_ITEM(UNIT_CONT, selekfh_FCONT)  ! continuum
      CALL WRITE_SPEC_ITEM(UNIT_NORM, FN)             ! normalized

      !__logging__
      707 FORMAT(1X,'IKEY=',I10,2X,'LZERO=',F10.3,2X,'LFIN=',F10.3, 2X,'I1=',I7,2X,'I2=',I7)
      WRITE(LLL,707) IKEY, LZERO, LFIN, I1, I2
      CALL LOG_INFO(LLL)

      IKEY = IKEY+1
      IF (IKEY .GT. IKEYTOT) EXIT !Main loop exit door! ISSUE what does this condition mean?

      !__logging__
      708 FORMAT(1X,'IKEY=',I10,2X,'IRH=',I6)
      WRITE(LLL, 708) IKEY, IRH
      CALL LOG_INFO(LLL)
      
      LZERO = LZERO+main_AINT
      LFIN = LFIN+main_AINT
      IF(LFIN .GT. (main_LLFIN+20.)) LFIN = main_LLFIN+20.

    END DO  !--Main loop--!

    CLOSE(UNIT_SPEC)
    CLOSE(UNIT_CONT)
    CLOSE(UNIT_NORM)
    CLOSE(UNIT_LOG)
    CLOSE(UNIT_LINES)

    !__logging__
    CALL LOG_INFO('Flux sortant est en nu: Fnu x lambda')
    CALL LOG_INFO('Flux absolu sortant a ete multiplie par 10**5')

  CONTAINS  !--still PFANT_CALCULATE()
    
    ! These subroutines have total knowledge of the variable names and values that appear inside
    ! their parent subroutine PFANT_CALCULATE(). http://www.personal.psu.edu/jhm/f90/statements/contains.html
      
    !> Used to write the "spectrum", "continuum", and "normalized".
    !> Their writing pattern is very similar. THe header is the same,
    !> only the "ITEM" changes from file to file.
    SUBROUTINE WRITE_SPEC_ITEM(UNIT_, ITEM)
    !> Unit number, either UNIT_SPEC, UNIT_CONT, UNIT_NORM
      INTEGER, INTENT(IN) :: UNIT_
      !> Either selekfh_FL, selekfh_FCONT, or FN
      REAL*8, INTENT(IN) :: ITEM(:)
      REAL*8 AMG
      AMG = main_XXCOR(8)  ! ISSUE is this assuming something to do with Magnesium?
      
      1130 FORMAT(I5, 5A4, 5F15.5, 4F10.1, I10, 4F15.5)
      WRITE(UNIT_, 1130)       &
       IKEYtot,                &
       (modeles_TIT(I),I=1,5), &
       TETAEF,                 &
       main_GLOG,              &
       main_ASALOG,            &
       modeles_NHE,            &
       AMG,                    &
       L0,                     &
       LF,                     &
       LZERO,                  &
       LFIN,                   &
       ITOT,                   &
       main_PAS,               &
       main_ECHX,              &
       main_ECHY,              &          
       main_FWHM                         
     
      1132 FORMAT(40000F15.5)
      WRITE(UNIT_SPEC,1132) (ITEM(D), D=I1,I2)
    END


    !> Writes into lines.pfant and fort.91
    SUBROUTINE WRITE_LINES_FORT91()
      122 FORMAT(6X,'# LAMBDA',4X,'KIEX',5X,'L GF',3X,'L ABOND',6X,'CH',10X,'GR',10X,'GE',5X,'ZINF',4X,'CORCH')
      WRITE(UNIT_LINES, 122)
      DO K=1,atomgrade_NBLEND
        LOG_ABOND = ALOG10(atomgrade_ABONDS_ABO(K))

        125 FORMAT(A2,1X,I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1, F7.1)
        WRITE(UNIT_LINES, 125)     &
         atomgrade_ELEM(K),        &
         atomgrade_IONI(K),        &
         atomgrade_LAMBDA(K),      &
         atomgrade_KIEX(K),        &
         atomgrade_ALGF(K),        &
         LOG_ABOND-main_AFSTAR+12, &
         atomgrade_CH(K),          &
         atomgrade_GR(K),          &
         atomgrade_GE(K),          &
         atomgrade_ZINF(K),        &
         popadelh_CORCH(K)                

        ! ISSUE: Is file "fort.91" still wanted???? So similar to above!!! Why repeat???
       121 FORMAT(1X,A2,I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1,F7.1)
        WRITE(91,121)              &
         atomgrade_ELEM(K),        &
         atomgrade_IONI(K),        &
         atomgrade_LAMBDA(K),      &
         atomgrade_KIEX(K),        &
         atomgrade_ALGF(K),        &
         LOG_ABOND-main_AFSTAR+12, &
         atomgrade_CH(K),          &
         atomgrade_GR(K),          &
         atomgrade_GE(K),          &
         atomgrade_ZINF(K),        &
         popadelh_CORCH(K)
      END DO
    END



    ! TODO Fix Initializations
    ! TOdO explain parameters
    ! TODO verbose
    ! TODO discover what is input and what is output
    !======================================================================================================================
    ! Sets the Voigt profile using Hjertings' constants.
    !
    ! Note: convolution for molecules uses Gaussian profile.
    !
    
    ! ISSUE with variable MM
    SUBROUTINE SELEKFH()
        USE READ_FILES
        IMPLICIT NONE
        INTEGER D
        REAL lambi
        REAL KAPPA,KA,KAP,bk_KCD,KCI,KAM,KAPPAM,KAPPT
        REAL*8 ECAR,ECARTM,ECARM
        DIMENSION turbul_VT(50)
        DIMENSION BI(0:50)
        REAL, DIMENSION(MAX_atomgrade_NBLEND) :: ECAR, ECARTL, KA

        DIMENSION KAP(50),           &
                  KAPPA(50),         &
                  bk_KCD(NP,50), &
                  KCI(50)

        DIMENSION TAUHD(50)
        DIMENSION DELTAM(NMOL,50), &
                  ECARM(NMOL),     &
                  ECARTLM(NMOL),   &
                  KAM(NMOL),       &
                  KAPPAM(50),                &
                  KAPPT(50)


        IF (atomgrade_NBLEND .NE. 0) then
          DO K = 1,atomgrade_NBLEND
            ECAR(K) = ECART(K)
          END DO
        END IF
        
        IF (km_MBLEND .NE. 0) then
          DO K=1,km_MBLEND
            ECARM(K) = ECARTM(K)
          END DO
        end if
        
        DO D = 1, DTOT
          lambi = (6270+(D-1)*0.02)
          if (atomgrade_NBLEND .ne. 0) then
            DO K=1,atomgrade_NBLEND
              ECAR(K)=ECAR(K)-main_PAS
              ECARTL(K)=ECAR(K)
            END DO
          end if
          
          if(km_MBLEND .ne. 0) then
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
            if(atomgrade_NBLEND .EQ. 0) go to 260

            DO  K=1,atomgrade_NBLEND
              IF(ABS(ECARTL(K)) .GT. atomgrade_ZINF(K)) THEN
                KA(K) = 0.
              ELSE
                V = ABS(ECAR(K)*1.E-8/popadelh_DELTA(K,N))
                CALL HJENOR(popadelh_A(K,N), V, popadelh_DELTA(K,N), PHI)
                KA(K) = PHI * popadelh_POP(K,N) * GFAL(K) * atomgrade_ABONDS_ABO(K)
                IF(K .EQ. 1) KA(K) = PHI * popadelh_POP(K,N) * GFAL(K)

              END IF
              KAPPA(N) = KAPPA(N) + KA(K)
            END DO   !  fin bcle sur K

            260 CONTINUE

            ! molecule
            IF(km_MBLEND.EQ.0) GO TO 250
            DO L=1,km_MBLEND
              IF( ABS(ECARTLM(L)) .GT. km_ALARGM(L) )  then
                KAM(L)=0.
              else
            
                ! ISSUE uses MM, which is read within KAPMOL and potentially has a different value for each molecule!!!!! this is very weird
                ! Note that km_MM no longer exists but it is the ancient "MM" read within ancient "KAPMOL()"
                DELTAM(L,N)=(1.E-8*km_LMBDAM(L))/C*SQRT(turbul_VT(N)**2+DEUXR*T/km_MM)
                VM=ABS(ECARM(L)*1.E-08/DELTAM(L,N))
                PHI=(EXP(-VM**2))/(RPI*DELTAM(L,N))
                KAM(L)=PHI*km_GFM(L)*km_PNVJ(L,N)
              end if
              KAPPAM(N)=KAPPAM(N)+KAM(L)
            END DO   !  fin bcle sur L
          
            250 CONTINUE
            KAPPT(N) = KAPPA(N)+KAPPAM(N)
            KCI(N) = bk_KCD(D,N)
            KAP(N) = KAPPT(N)+KCI(N)
            BI(N) = ((bk_B2(N)-bk_B1(N))*(FLOAT(D-1)))/(FLOAT(DTOT-1)) + bk_B1(N)
          END DO    ! fin bcle sur N
          
          BI(0) = ((bk_B2(0)-bk_B1(0))*(FLOAT(D-1)))/(FLOAT(DTOT-1)) + bk_B1(0)
          
          !__logging__
          IF (D .EQ. 1 .OR. D .EQ. DTOT) THEN
            150 FORMAT(' D=',I5,2X,'KCI(1)=',E14.7,2X,'KCI(NTOT)=',E14.7,/,10X,'KAPPA(1)=',E14.7,2X,'KAPPA(NTOT)=',E14.7)
            152 FORMAT(10X,'KAPPAM(1)=',E14.7,2X,'KAPPAM(NTOT)=',E14.7)
            151 FORMAT(' D=',I5,2X,'BI(0)=',E14.7,2X,'BI(1)=',E14.7,2X,'BI(NTOT)=',E14.7)

            WRITE(LLL,151) D,BI(0),BI(1),BI(modeles_NTOT)
            CALL LOG_DEBUG(LLL)
            WRITE(LLL,150) D,KCI(1),KCI(modeles_NTOT),KAPPA(1),KAPPA(modeles_NTOT)
            CALL LOG_DEBUG(LLL)
            WRITE(LLL,152)KAPPAM(1),KAPPAM(modeles_NTOT)
            CALL LOG_DEBUG(LLL)
          END IF
          
          IF((D .LT. DHM) .OR. (D .GE. DHP)) THEN
            CALL FLIN1(KAP,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU, config_KIK, TTD(D))
            selekfh_FL(D) = flin_F
          ELSE
            DO N = 1,modeles_NTOT
                TAUHD(N) = TAUH(D,N)
            END DO
            CALL FLINH(KAP,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU, config_KIK,TAUHD, TTD(D))
            selekfh_FL(D) = flin_F
          END IF
              
          ! Dez 03-P. Coelho - calculate the continuum and normalized spectra
          CALL FLIN1(KCI,BI,modeles_NH,modeles_NTOT,main_PTDISK,main_MU, config_KIK, TTD(D))
          selekfh_FCONT(D) = flin_F
        END DO  ! fin bcle sur D
    END


    !======================================================================================================================
    !> Calculates the flux in the continuum.
    !
    SUBROUTINE BK(LAMBD, TTD, DTOT, LZERO, LFIN)
      USE READ_FILES
      USE PARAMETERS
      USE LOGGING
      IMPLICIT NONE
      INTEGER D, DTOT
      REAL*8 LAMBD, NU, LLZERO, LLFIN, NU1, NU2, LAMBDC, KCJ, KCN,
     + ALPH_N, ! old ALPH, which was a vector, but I realized it is used only inside loop, no need for vector
     + LOG_PE  ! Created to avoid calculating ALOG10(PE) 3x
      REAL*8 LZERO, LFIN
      REAL*8, DIMENSION(2, MAX_modeles_NTOT) :: KCJ
      REAL*8, DIMENSION(2) :: KCN, LAMBDC, TOTKAP
      CHARACTER*80 LLL
      DIMENSION TTD(NP)
      DIMENSION FTTC(NP)

      
      LLZERO = LZERO
      LLFIN  = LFIN
      NU1 = C* 1.E+8 /LZERO
      AHNU1 = H*NU1
      C31 = (2*AHNU1) * (NU1/C)**2
      
      DO N = 1,modeles_NTOT
        T = 5040./modeles_TETA(N)
        ALPH_N = EXP(-AHNU1/(KB*T))
        bk_B1(N) = C31 * (ALPH_N/(1.-ALPH_N))
        CALL ABSORU(LLZERO,modeles_TETA(N),ALOG10(modeles_PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
        bk_KC1(N) = TOTKAP(1)
      END DO
      
      NU2 = C* 1.E+8 /LFIN
      AHNU2 = H*NU2
      C32 =(2*AHNU2) * (NU2/C)**2
      DO N = 1,modeles_NTOT
        ! TODO: calculate this "T" somewhere else, this is calculated all the time! a lot of waste
        T = 5040./modeles_TETA(N)
        ALPH_N = EXP(-AHNU2/(KB*T))
        bk_B2(N) = C32 * (ALPH_N/(1.-ALPH_N))
        CALL ABSORU(LLFIN,modeles_TETA(N),ALOG10(modeles_PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
        bk_KC2(N) = TOTKAP(1)
      END DO
      
      NU = C* 1.E+8 /LAMBD
      AHNU = H*NU
      C3 =(2*AHNU) * (NU/C)**2
      DO N=1,modeles_NTOT
        T=5040./modeles_TETA(N)
        ALPH_N = EXP(-AHNU/(KB*T))
        bk_B(N) = C3 * (ALPH_N/(1.-ALPH_N))
        CALL ABSORU(LAMBD,modeles_TETA(N),ALOG10(modeles_PE(N)),1,1,1,1,2,1,KKK,TOTKAP)
        bk_PHN(N) = absoru_ZNH(absoru2_NMETA+4) *KB * T
        bk_PH2(N) = absoru_ZNH(absoru2_NMETA+2) *KB * T
        bk_KC(N) = TOTKAP(1)
      END DO
      
      TET0 = FTETA0(modeles_PG, modeles_TETA)     !on extrapole modeles_TETA pour modeles_NH=0
      T = 5040./TET0
      
      
      ALPH01 = EXP(-AHNU1/(KB*T))
      bk_B1(0) = C31 * (ALPH01/(1.-ALPH01))
      CALL FLIN1(bk_KC1,bk_B1,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,config_KIK)
      FC1 = flin_F
      
      ALPH02 = EXP(-AHNU2/(KB*T))
      bk_B2(0) = C32 * (ALPH02/(1.-ALPH02))
      CALL FLIN1(bk_KC2,bk_B2,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,config_KIK)
      FC2 = flin_F
      
      ALPH0 = EXP(-AHNU/(KB*T))
      bk_B(0) = C3 * (ALPH0/(1.-ALPH0))
      CALL FLIN1(bk_KC,bk_B,modeles_NH,modeles_NTOT,main_PTDISK,main_MU,config_KIK)
      bk_FC = flin_F

      ILZERO = LZERO/100.
      ILZERO = 1E2*ILZERO
      LAMBDC(1) = LZERO-ILZERO
      LAMBDC(2) = LFIN-ILZERO
      DO N=1,modeles_NTOT
        KCJ(1,N)=bk_KC1(N)
        KCJ(2,N)=bk_KC2(N)
      END DO
      DO N=1,modeles_NTOT
        DO J=1,2
          KCN(J)=KCJ(J,N)
        END DO
        CALL FTLIN3(2,LAMBDC,KCN,DTOT,TTD,FTTC)
        DO D=1,DTOT
          bk_KCD(D,N)=FTTC(D)
        END DO
      END DO
      
      !__logging__
      153 FORMAT(' bk_KCD(1,1)=',E14.7,2X,'bk_KCD(1,NTOT)=',E14.7)
      154 FORMAT(' bk_KCD(DTOT,1)=',E14.7,2X,'bk_KCD(DTOT,NTOT)=',E14.7)
      WRITE(LLL,153) bk_KCD(1,1),bk_KCD(1,modeles_NTOT)
      CALL LOGGING_DEBUG(LLL)
      WRITE(LLL,154) bk_KCD(DTOT,1),bk_KCD(DTOT,modeles_NTOT)
      CALL LOGGING_DEBUG(LLL)

      10 CONTINUE
    END

  END SUBROUTINE PFANT_CALCULATE



  !======================================================================================================================
  ! ISSUE WHAT

  SUBROUTINE TURBUL()
    USE CONFIG
    CHARACTER*80 LLL
    
    CALL LOG_DEBUG('ENTREE DS TURBUL')
    IF(main_IVTOT .EQ. 1)   THEN
      CALL LOG_DEBUG('VT CONSTANT')
      DO N = 1, modeles_NTOT
        turbul_VT(N) = main_VVT(1)*1E5
      END DO
    ELSE
      101 FORMAT(10F8.3)
      CALL LOG_DEBUG('VT VARIABLE AVEC LA PROFONDEUR')
      CALL LOG_DEBUG('    LOG TO')
      WRITE(LLL,101) (main_TOLV(I),I=1,IVTOT)
      CALL LOG_DEBUG(LLL)
      CALL LOG_DEBUG('    VT')
      WRITE(LLL,101) (main_VVT(I),I=1,IVTOT)
      CALL LOG_DEBUG(LLL)
      
      IF(config_INTERP .EQ. 1) THEN
        CALL FTLIN3(min_IVTOT, main_TOLV, main_VVT, modeles_NTOT,
   +     modeles_T5L, turbul_VT)
      ELSEIF (config_INTERP .EQ. 2) THEN
        ! ISSUE ask if still useful mbecause it was switched off.
        ! TODO Test this interpolation
        CALL FT2(main_IVTOT, main_TOLV, main_VVT, modeles_NTOT,
   +     modeles_T5L,turbul_VT)
      END IF
        
      
      NT2 = modeles_NTOT-2
      DO N = 1, NT2, 3
        102 FORMAT(3(I5,2F8.3,5X))
        WRITE(LLL,102) N,modeles_T5L(N),turbul_VT(N),(N+1),
   +     modeles_T5L(N+1),turbul_VT(N+1),
   +     (N+2),modeles_T5L(N+2),turbul_VT(N+2)
        CALL LOG_DEBUG(LLL)
      END DO

      DO N = 1, modeles_NTOT
        turbul_VT(N) = turbul_VT(N)*1E5
      END DO
    END IF


    IF(main_IVTOT .EQ. 1) THEN
      131 FORMAT(/' V MICRO CONSTANTE  =',F6.1,'KM/S'/)
      WRITE(LLL,131) main_VVT(1)
      CALL LOG_DEBUG(LLL)
    ELSE
      132 FORMAT(/'V MICRO VARIABLE AVEC PROFONDEUR')
      WRITE(LLL,132)
      CALL LOG_DEBUG(LLL)
    END IF

    RETURN
  END SUBROUTINE


  !======================================================================================================================
  !> Calcule la pop du niv fond de l'ion pour tous les partit_NPAR atomes de
  !> la table des fonctions de partition ,a tous les niv du modele
  !>
  !> 40 elements, 50 niveaux de modele, 3 niv d'ionisation par elem.
  !> Partit donnee pour 33 temperatures au plus ds la table.
  SUBROUTINE POPUL()
    DIMENSION U(3), ALISTU(63), UE(50), TT(51)
    
    DO N = 1, modeles_NTOT
      T = 5040./modeles_TETA(N)  ! TODO I think the program deserves a modeles_T5040 because this is calculated everywhere!!!
      UE(N) = C1*KB*T/modeles_PE(N)*T**1.5
      DO J = 1, partit_NPAR
        KMAX = partit_JKMAX(J)
        TT(1) = partit_TINI(J)
        DO  L=1,3
          DO  K=1,KMAX
            TT(K+1) = TT(K) + partit_PA(J)
            ALISTU(K) = partit_TABU(J,L,K)
          END DO

          IF (modeles_TETA(N) .LT. TT(KMAX-1) ) THEN
            ! interpolation parabolique
            UUU = FT(modeles_TETA(N),KMAX,TT,ALISTU)
          ELSE
            ! interpolation lineaire entre 2 derniers pts
            AA = (ALISTU(KMAX)-ALISTU(KMAX-1)) / partit_PA(J)
            BB = ALISTU(KMAX-1) - AA * TT(KMAX-1)
            UUU = AA*modeles_TETA(N) + BB
          END IF

          U(L) = EXP(2.302585*UUU)
        END DO

        X=U(1) / (U(2)*UE(N)) * 10.**(partit_KI1(J)*modeles_TETA(N))
        TKI2= partit_KI2(J) * modeles_TETA(N)
        IF (TKI2 .GE. 77.) THEN
          Y = 0.
          popul_P(3,J,N) = 0.
        ELSE
          Y = U(3)*UE(N)/U(2) * 10.**(-partit_KI2(J)*modeles_TETA(N))
          popul_P(3,J,N) = (1./U(3))*(Y/(1.+X+Y))
        END IF
        popul_P(2,J,N) = (1./U(2))*(1./(1.+X+Y))
        popul_P(1,J,N) =  (1./U(1))*(X/(1.+X+Y))
        END DO
      END DO
    RETURN
  END SUBROUTINE


  !======================================================================================================================
  !> Calcule la population au niveau inferieur de la transition
  !> la largeur doppler popadelh_DELTA et le coefficient d'elargissement
  !> le "popadelh_A" utilise dans le calcul de H(popadelh_A,V)
  !

  SUBROUTINE POPADELH()
    USE LOGGING
    IMPLICIT NONE
    CHARACTER*1 ISI(1), ISS(1)
    INTEGER J, K
    REAL*8 KIES,KII,NUL
    DIMENSION ALPHL(50)  ! TODO 50??
    DATA ISI/' '/, ISS/' '/

    DO K = 1, atomgrade_NBLEND
      popadelh_CORCH(k) = 0.
      popadelh_CVdW(K) = 0
      DO  J=1,partit_NPAR
        IF(partit_EL(J).EQ.atomgrade_ELEM(K)) GO TO 15
      END DO

      104 FORMAT('MANQUE LES FCTS DE PARTITION DU ', A2)
      WRITE(LLL,104) atomgrade_ELEM(K)
      CALL PFANT_HALT(LLL)
      
      15 CONTINUE
      IOO = atomgrade_IONI(K)

      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      !*************************************************************************************************
      ! ISSUE File writing routine, TAKE THIS OUT!!!!
      WRITE (77,*) atomgrade_ELEM(k),atomgrade_LAMBDA(k)
      
      IF(atomgrade_CH(K).LT.1.E-37)  THEN
        KIES=(12398.54/atomgrade_LAMBDA(K)) + atomgrade_KIEX(K)
        IF(IOO.EQ.1)   KII=partit_KI1(J)
        IF(IOO.EQ.2)   KII=partit_KI2(J)
        IF(popadelh_CORCH(K).LT.1.E-37)   THEN
          popadelh_CORCH(K)=0.67 * atomgrade_KIEX(K) +1
        END IF

        ! 125 FORMAT(3X ,' POUR',F9.3,'   ON CALCULE CH ', 'VAN DER WAALS ET ON MULTIPLIE PAR ',F7.1)
        ! WRITE(6,125)  atomgrade_LAMBDA(K), popadelh_CORCH(K)
        popadelh_CVdW(K)= CALCH(KII,IOO,atomgrade_KIEX(K),ISI,KIES,ISS)
        atomgrade_CH(K)= popadelh_CVdW(K) * popadelh_CORCH(K)
      END IF

!
      IF(atomgrade_CH(K) .LT. 1.E-20) THEN 
        IOPI=1
      ELSE
        IOPI=2
      END IF
      
      DO  N=1,modeles_NTOT
        T=5040./modeles_TETA(N)
        NUL= C* 1.E+8 /atomgrade_LAMBDA(K)
        AHNUL= H*NUL
        ALPHL(N)=EXP(-AHNUL/(KB*T))

        TAP = 1.-ALPHL(N)
        TOP = 10.**(-atomgrade_KIEX(K)*modeles_TETA(N))
        popadelh_POP(K,N) = popul_P(IOO,J,N)*TOP*TAP
        ! NOXIG: ISSUE what does it mean?
        IF(K .EQ. 1) popadelh_POP(K,N) = TOP*TAP*popul_P(IOO,J,N)*sat4_PO(N)/sat4_PPH(N)
        popadelh_DELTA(K,N) =(1.E-8*atomgrade_LAMBDA(K))/C*SQRT(turbul_VT(N)**2+DEUXR*T/partit_M(J))
        VREL = SQRT(C4*T*(1.+1./partit_M(J)))
        IF (IOPI .EQ. 1) THEN
          GH = C5*atomgrade_CH(K)**0.4*VREL**0.6
        ELSE
          GH = atomgrade_CH(K) + popadelh_CORCH(K)*T
        END IF
        GAMMA = atomgrade_GR(K)+(atomgrade_GE(K)*modeles_PE(N)+GH*(bk_PHN(N)+1.0146*bk_PH2(N)))/(KB*T)
        popadelh_A(K,N) =GAMMA*(1.E-8*atomgrade_LAMBDA(K))**2 / (C6*popadelh_DELTA(K,N))
      END DO
    END DO
  END



END MODULE





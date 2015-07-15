module hydro2_calc
! command-line option: config_zph (default=12) because it will no longer be taken from file


C                              - HYD2 -
C                   MEUDON OBSERVATORY
C           CALCUL DU PROFIL D UNE RAIE DE L HYDROGENE
C
C     CE PROGRAMME A ETE ECRIT PAR F.PRADERIE (ANN D AP 1967)
C     TALAVERA Y A INTRODUIT L ELARGISSEMENT DE SELFRESONNANCE (1970)
C     G.HERNANDEZ ET M.SPITE L ONT ADAPTE AU VAX PUIS SUR LA STATION DEC
C
C     A linker avec absor et calhy
C     En sortie:
C     - Un fichier de trace compatible avec GRAFIC (Nom demande)
C     - dans le cas ou on veut calculer la raie de D dans l'aile de H
C       un fichier todeut.dat contenant le coef d'ab d'H a lambda de D

contains
  subroutine hydro2()
      LOGICAL ECRIT,PTDISK,config_amores,d3_stark,PAPIER,ASUIVRE,DEUT
      REAL*4 LAM,KC,modeles_nh,NE,modeles_nhe,MMU
      REAL*8 LAMC,LAMB
        CHARACTER*30 Nomplot
C     CHARACTER*2 TTT(11)
      DIMENSION BPL(0:99), TAU(50,0:99), TAUC(0:99)
      DIMENSION AL(50,99),FL(99),
     1 KC(99),noname%dlam(50),C(20),TOTH(99),,
     2 ALL(100), YY(100)  !FICHIER DE TRACE

C     Commons avec les sp d'absorption continue

        !> @todo see how it is in PFANT
        COMMON/LECT1/ABMET,ABHEL
C
C       Common avec ??? (mettre en dimension?)
        ! true, there is no one else using this, which was a common
        dimension TTT(11),R(50),NE(99),ZIG(99),W(2,2),AB(50),
     &             DEL(50),modeles_t5l(99)
C
C     Commons avec RAIEHU
      real*8 d3_hyn(99)  /ABSO2/MMAX
      COMMON/D2/VT(99),d2%cmu,NZ,modeles_ntot,JMAX,d2%iqm,d2%ij(10)

C ............adios!C
C ............adios!C     Common  avec READER5N
C ............adios!        COMMON /COM6/x_teff,x_glog,x_asalog,modeles_asalalf,modeles_nhe,modeles_tiabs(5),modeles_tit(5)
C actually this comment is relevant -----> ............adios!C       (si modeles_tit va bien modeles_tiabs du modele = absoru2_titre de absoru)
C ............adios!C

      DATA CSTE/6.958258E-13/, DEUT/.FALSE./



      Gotta investigate if this JMAX has the same meaning as many 46 in absoru_data.f and absoru.f90
      gotta add parameter to absoru_data.f and absoru.f90





C     POINTS DE LA RAIE OU ON EFFECTUE LE CALCUL
      JMAX=46
      DATA noname%dlam/0.,0.01,.02,.03,.04,.05,.06,.08,.1,.125,.15,.175,
     1 .20,.25,.30,.35,.40,.50,.60,.70,.80,.90,1.,1.1,1.2,1.3,
     2 1.4,1.5,1.6,1.8,2.,2.5,3.,3.5,4.,4.5,5.,7.5,10.,12.5,
     3 15.,17.5,20.,25.,30.,35.,0.,0.,0.,0./
C     NUMEROS DES DISCONTINUITES DANS LES DL:
      d2%iqm=9
      DATA d2%ij/7,9,13,17,29,31,37,43,46,0/
C        data TTT/11*'    '/

C     FICHIER POUR LE TRACE DES RAIES
C     MASSE ET CHARGE DE L ATOME D HYDROGENE
      d2%cmu=1.
      NZ=1
      noname%ll= 2      ! OPTION DANS RAIEHU (old "IX")
      noname%j1=0       ! OPTION DANS RAIEHU (CONVOLUTION STARK-DOPPLER)
      noname%nbmin = 0  ! was being passed uninitialized to raiehu()
      noname%nbmax = 0  ! was being passed uninitialized to raiehu()
      noname%ind = 1    ! IND=0 ECRITURE DANS RAIEHU ,DEPANNAGE SEULEMENT...



      IOP=0        ! OPTION 6 pts ds calcul du flux
C     IOP=1        ! OPTION 26 pts ds calcul du flux
      PTDISK=.FALSE.
        ASUIVRE=.FALSE.  !deviendra T au second calcul
      ru%stark = .true.
C
C     write(6,*)'VOULEZ UNE SORTIE PAPIER?    (T OU F)'
C     READ(5,*)   PAPIER
      PAPIER=.TRUE.
        if(papier) open(UNIT=11,file='hydro2.prt',status='unknown')
      write(6,*)' '
      write(6,*)' '
      CALL LECTUR (absoru2_zp,config_zph)
      write(6,*)'THEORIE QUASISTATIQUE:   ENTER    1'
      write(6,*)'THEORIE DE GRIEM     :   ENTER    0'
      READ(5,*)   config_kq

      noname%kq = config_kq
C           Attention astuce...
            IF(config_kq.GT.1)   THEN
            config_kq=1
            DEUT=.TRUE.
            OPEN(UNIT=16,FILE='todeut.dat',STATUS='unknown')
            END IF
      write(6,*)' SORTIES INTERMEDIAIRES ?   (T OU F)'
      READ(5,*) ECRIT
1000          IF(ECRIT)   THEN
                write(6,*)'ENTRER UN absoru2_titre POUR LE TRAVAIL'
                READ(5,12)(TTT(I),I=1,11)
                WRITE (6,71) config_kq,(TTT(I),I=1,11)
                WRITE (6,75) d2%cmu,NZ
                WRITE (6,79) (DL(J),J=1,JMAX)
                    if (papier) then
                    WRITE (11,71) config_kq,(TTT(I),I=1,11)
                    WRITE (11,75) d2%cmu,NZ
                    WRITE (11,79) (DL(J),J=1,JMAX)
                    end if
            END IF
      KZ=0
      write(6,*)'  C1=(PI*E**2)/(M*C**2) * LAMB**2 * F*10E24 '
      write(6,*)'  F=FORCE D OSCILLATEUR TOTALE DE LA RAIE'
      write(6,*)'      VALEURS PARTICULIERES DE C1 :'
      write(6,*)' '
      write(6,*)' HA:2442.326   HB:249.628   HG:74.4776'
      write(6,*)' HD:32.8903    HE:17.72642'
      write(6,*)' '
        write(6,*)' ex H alfa  2 3 6562.817 10.15 2442.326 '
        write(6,*)'    H beta  2 4 4861.342 10.15  249.628 '
        write(6,*)'    H gamma 2 5 4340.475 10.15   74.4776'
      write(6,*)'ENTER : NIV INF, NIV SUP, LAMBDA, KIEX, C1'
      READ(5,*) NA,NB,LAMB,X,C1
      LAMC=LAMB
      LAM=LAMB    ! LAMB EN SIMPLE PRECISION
      write(6,*)' AMORTISSEMENT DE RESONNANCE ?  (T OU F)'
      READ(5,*)   config_amores
c
            IF(ECRIT)   THEN
                WRITE (6,72) NA,NB,LAMB,C1,X,J1,d2%iqm
                WRITE (6,86) (d2%ij(IQ),IQ=1,d2%iqm)
                IF(config_amores) WRITE(6,301)
                IF(d3_stark) WRITE(6,302)
                   if(papier) then
                   write(11,72) NA,NB,LAMB,C1,X,J1,d2%iqm
                   write(11,86) (d2%ij(IQ),IQ=1,d2%iqm)
                   IF(config_amores) write(11,301)
                   IF(d3_stark) WRITE(11,302)
                   end if
            END IF
C
C     LECTURE DU MODELE ET DE LA VITESSE DE MICROTURBULENCE
C
      write(6,*)' VITESSE DE MICROTURBULENCE  EN KM/S ?'
      READ(5,*)   VVT
      write(6,*)' ENTER: x_teff, LOG G, [M/H], NUM DE LIGNE'
      CALL READER5N(modeles_nh,modeles_teta,modeles_pe,modeles_pg,modeles_t5l,modeles_ntot)
      write(6,*)'   sortie de READER   modeles_pg(1 a 5)'
      write(6,*)(modeles_pg(I),I=1,5)
            DO  I=1,modeles_ntot
            pe_log(I)=ALOG10(modeles_pe(I))
            VT(I)=VVT*1.E+5
            END DO
        TETAEF=5040/x_teff
      COEFALF=10**modeles_asalalf
        ASASOL=10**x_asalog
      ABHEL=modeles_nhe
        call abonio(absoru2_zp,config_zph,ASASOL,COEFALF)
      PDS=2*NA**2
C
            IF(ECRIT)   THEN
                WRITE(6,47)ABMET,(modeles_tit(I),I=1,5)
                WRITE (6,201)
                WRITE (6,202) (I,modeles_nh(I),modeles_teta(I),pe_log(I),modeles_t5l(I),I=1,modeles_ntot)
                WRITE(6,*)' '
                   if(papier) then
                   write(11,47)ABMET,(modeles_tit(I),I=1,5)
                   write(11,201)
                   write(11,202)
     1                (I,modeles_nh(I),modeles_teta(I),pe_log(I),modeles_t5l(I),I=1,modeles_ntot)
                   write(11,*)' '
                   end if
            END IF
C
      write(6,*)'appel de ABSORU pou LAM=', LAM
            DO I=1,modeles_ntot
            CALL ABSORU(LAM,modeles_teta(I),pe_log(I),1,1,1,1,2,1,KKK,TOTKAP)
            TOTH(I)=absoru_TOC
            KC(I)=absoru_TOTKAP(1)
            d3_hyn(I)=absoru_ZNH(MMAX+4)
            NE(I)=modeles_teta(I)*modeles_pe(i)/CSTE
            END DO
C
      write(6,*)' PATIENCE VOUS ENTRER DANS RAIEHU'
      !> @todo issue nmin and nmax were initialized, I initialized them with zero
      CALL RAIEHU (IX,NA,NB,NMIN,NMAX,LAMB,C1,C,DL,AL,J1,IND,config_kq)
      write(6,*)' VOUS ETES SORTI DE RAIEHU'
C
            IF(ECRIT.and.papier)   THEN
            WRITE (11,18) (modeles_tit(L),L=1,5)
            WRITE(11,14)LAMB,PDS,X
            WRITE(11,25)
            DO I=1,modeles_ntot,5
            WRITE(11,32) I
            WRITE(11,8)(AL(J,I),J=1,JMAX)
            END DO
            END IF
C
      write(6,*)' VOUS CALCULEZ LE TAU SELECTIF'
      WRITE(6,*) 'APPEL AMERU pou LAM=',LAM
      CALL AMERU (modeles_teta,pe_log,modeles_pg,modeles_nh,LAM,PDS,
     1          X,modeles_ntot,JMAX,AL,BPL,TAU,IX)
            IF(ECRIT)   THEN
            WRITE(6,38)
                if(papier) write(11,38)
              DO  I=1,modeles_ntot,5
              WRITE(6,40)I
              WRITE (6,37) (TAU(J,I),J=1,JMAX)
                    if(papier) then
                    write(11,40)I
                    write(11,37)(TAU(J,I),J=1,JMAX)
                    end if
              END DO
C
            WRITE (6,18) (modeles_tit(L),L=1,5)
            WRITE(6,121)
                  WRITE(6,11)(modeles_t5l(I),modeles_nh(I),modeles_teta(I),pe_log(I),KC(I),VT(I),
     1       NE(I),TOTH(I),I,I=1,modeles_ntot)
                  if(papier) then
                  write(11,18) (modeles_tit(L),L=1,5)
                  write(11,121)
                  write(11,11)(modeles_t5l(I),modeles_nh(I),modeles_teta(I),pe_log(I),KC(I),VT(I),
     1         NE(I),TOTH(I),I,I=1,modeles_ntot)
              end if
            END IF
C
            IF(DEUT)   THEN
      WRITE (16,70) x_teff,x_glog,x_asalog,modeles_asalalf,modeles_nhe
        WRITE (16,73) (modeles_tit(L),L=1,5)
      WRITE(16,407) (TAU(30,I),I=1,modeles_ntot)
C     (L'INDICE 30 CORRESPOND A L'EMPLACEMENT DE LA RAIE DE D)
            END IF
C
      WRITE(6,*) '   APPEL OPTIC1  '
      CALL OPTIC1(KC,modeles_nh,modeles_ntot,TAUC)
c        write(6,*) ' tauc au centre de la raie'
c        WRITE(6,26)(TAUC(I),I=1,modeles_ntot)
c        write(6,*)' fonction de Planck'
c        WRITE(6,26)(BPL(I),I=1,modeles_ntot)
      IF(TAUC(modeles_ntot).LT.3.89) GO TO 1002
            IF(ECRIT.and.papier)   THEN
            WRITE(11,19)
            WRITE(11,26)(TAUC(I),I=1,modeles_ntot)
            WRITE(11,28)
            WRITE(11,29)(BPL(I),I=1,modeles_ntot)
            END IF
C
      CALL FLUXIS (TAU,TAUC,BPL,modeles_ntot,PTDISK,MMU,JMAX,FL,FC,IOP)
            DO J=1,JMAX
            R(J)=FL(J)/FC
            END DO
C
        write(6,*)' Parametres du modele'
      WRITE (6,70) x_teff,x_glog,x_asalog,modeles_asalalf,modeles_nhe
        WRITE (6,73) (modeles_tit(L),L=1,5)
        write(6,*)' '
      WRITE(6,6)(absoru2_titre(I),I=1,5)
        if(ecrit) write(6,5) (TTT(L),L=1,11)
      WRITE (6,4) LAMB, NA,NB,X, C1
      WRITE(6,21)FC
      IF(config_kq.EQ.1)WRITE(6,401)
      IF(config_kq.NE.1)WRITE(6,402)
      IF(config_amores) WRITE(6,403)
      IF(.NOT.config_amores) WRITE(6,404)
      IF(J1.EQ.1) WRITE(6,405)
      IF(J1.EQ.0) WRITE(6,406)VVT
      WRITE(6,120)
      WRITE(6,22)
      WRITE(6,7)(DL(J),J=1,JMAX)
      WRITE(6,23)
      WRITE(6, 8)(FL(J),J=1,JMAX)
      WRITE(6,24)
      WRITE(6,34)(R(J),J=1,JMAX)
      if (papier) then
        WRITE (11,70) x_teff,x_glog,x_asalog,modeles_asalalf,modeles_nhe
        WRITE (11,73) (modeles_tit(L),L=1,5)
        WRITE(11,6)(absoru2_titre(I),I=1,17)
        if(ecrit) write(11,5)(TTT(L),L=1,11)
        WRITE (11,4)LAMB, NA,NB,X, C1
        WRITE(11,21)FC
        IF(config_kq.EQ.1)WRITE(11,401)
        IF(config_kq.NE.1)WRITE(11,402)
        IF(config_amores) WRITE(11,403)
        IF(.NOT.config_amores) WRITE(11,404)
        IF(J1.EQ.1) WRITE(11,405)
        IF(J1.EQ.0) WRITE(11,406)VVT
        WRITE(11,120)
        WRITE(11,22)
        WRITE(11,7)(DL(J),J=1,JMAX)
        WRITE(11,23)
        WRITE(11, 8)(FL(J),J=1,JMAX)
        WRITE(11,24)
        WRITE(11,34)(R(J),J=1,JMAX)
      end if
C
C           FICHIER POUR TRACE
C
        write(6,*)' Le programme va sortir un fichier lambda, I'
        write(6,*)' representant le profil de la raie'
        write(6,*)' qui pourra etre trace avec GRAFIC'
        write(6,*)' '
        write(6,*)' Nom de ce fichier?  (entre '' '')'
        READ(5,*) Nomplot
        open(unit=17,file=Nomplot,status='unknown')
c
            JJMAX=2*JMAX - 1
            JMA1 =JMAX-1
            DO JJ=1,JMAX
            ALL(JJ)=-DL(JMAX+1-JJ)
            YY(JJ)=R(JMAX+1-JJ)*10000
            END DO
C
            DO JJ=JMAX+1,JJMAX
            ALL(JJ)=DL(JJ-JMA1)
            YY(JJ)=R(JJ-JMA1)*10000
            END DO
C
            DBL=-DL(JMAX)
            FINL=DL(JMAX)
C
            ZUT1=0.
            ZUT2=1.
            ZUT3=0.0001
                WRITE(17,100) (modeles_tit(L),L=1,5),x_teff,x_glog,x_asalog,modeles_nhe
            WRITE(17,111) modeles_ntot, ZUT1, ZUT1, ZUT2, ZUT3

CCC         DO JJ=1,JJMAX
CCC         WRITE(17,112) ALL(JJ), YY(JJ)
CCC         END DO

C PFANTGRADE READS:
C     READ(16,1550)(LAMBDH(J),J=1,JMAX)
C     READ(16,1555) ((TH(J,N),J=1,JMAX),N=1,modeles_ntot)
C 1555      FORMAT(5E12.4)
              WRITE(17,1551)JMAX
            WRITE(17,1550)((DL(JJ)+LAMB),JJ=1,JMAX)
            WRITE(17,1555)((TAU(JJ,N),JJ=1,JMAX),N=1,modeles_ntot)

 1550 FORMAT(5F14.3)
 1551 FORMAT(I4)
 1555 FORMAT(5E12.4)

      GO TO 1003
1002  WRITE(6,27)
        if(papier) write(11,27)
 1003   write(6,*)' AUTRE CALCUL ?   (T OU F)'
        read(5,*)ASUIVRE
        IF(ASUIVRE)   GO TO 1000
      write(6,*) 'TRAVAIL TERMINE'
        write(6,*)' '
c
        if (DEUT)then
        write(6,*)' The optical depth of the hydrogen line at the'
        write(6,*)' wavelength of the Deuterium line (for fandeu)'
        write(6,*)' is in the file   todeut.dat'
        write(6,*)' '
        end if
c
      STOP
C   ***************************************************************
C                 FORMAT`S
C   ***************************************************************
4     FORMAT('Lambda=',F10.3,'A','   NA=',I2,' NB=',I2,4X,
     1 'KIEX=',F7.3,4X,'C1=',E12.4)
 5      FORMAT(1X,11A4)
6     FORMAT (' Absorption calculee avec table ',5A4//)
7     FORMAT (1X,5F15.3)
8     FORMAT (1X,5E15.7)
11    FORMAT (E12.5,2X, E13.5,2F9.4,E16.5,4X,F10.0,2E13.5,I8)
12    FORMAT (11A4)
14    FORMAT ('      LAMBDA=',F12.3,'   PD=',F8.3,'   X=',F8.3,' EV'/)
18    FORMAT (/10X,20A4/)
19    FORMAT (' CALCUL DE TAU CONTINU'/)
21    FORMAT (//'      FLUX CONTINU=',E16.7,'CGS')
22    FORMAT ('      LISTE DES DELTA LAMBDA (A)'/)
23    FORMAT (/'      FLUX DANS LA RAIE'/)
24    FORMAT (/'      F/FC'/)
25    FORMAT (' COEFF. PAR PARTICULE ABSORBANTE (UNITE 10-24 CGS)'/)
26    FORMAT(5E12.5)
27    FORMAT ('      VALEUR MAX. DE TAUC INF. A 3.89')
28    FORMAT (' FONCTION DE PLANCK (UNITE CGS)'/)
29    FORMAT(10E12.5)
32    FORMAT ('   I=',I4)
34    FORMAT(1X,5F15.5)
37    FORMAT(6X, 8E12.4)
38    FORMAT (1H1,40X,'CALCUL DE TAU SELECTIF'/)
40    FORMAT(I5)
47    FORMAT(2X,'AB. METAUX=',E15.8,2X,11A4)
70    FORMAT(' x_teff=',F7.0,3X,'LOG G=',F5.2,
     &         3X,'[M/H]=',F6.2,3X,'[alfa/A]=',f6.2,'  modeles_nhe=',F6.3)
 73     FORMAT(5A4)
71    FORMAT ('      config_kq=',I4,10X,11A4)
72    FORMAT ('  NA=',I4,' NB=',I4,' LAMB=',F14.3,' C1=',E12.7,
     1  ' X=',E12.7,' J1=',I4,' d2%iqm=',I4,
     2 /10X,'SI J1=0, CONVOLUTION PAR PROFIL DOPPLER')
75    FORMAT ('  d2%cmu=',F5.2,'  NZ=',I5)
79    FORMAT ('   DL   =',16F7.3)
86    FORMAT ('  d2%ij    =',10I5)
100   FORMAT(1X,5A4,2X,F6.0,3(1X,F6.2) )
111   FORMAT(I6,2F6.1,2X,2F8.4)
120   FORMAT (20A4)
121   FORMAT (6X,'TAU',9X,'modeles_nh',11X,'modeles_teta     LOGPE     KC/NOYAU DE H
     1 VT CGS      NE       TOTH'/)
201   FORMAT (/16X,'modeles_nh',10X,'modeles_teta', 5X,'LOG PE',7X,'TAU')
202   FORMAT ( 2X,I5,3X,1P,E13.5,0P,2F10.4,5X,1P,E12.5)
301   FORMAT(////,' AMORTISSEMENT RESONANCE')
302   FORMAT(///,' ECRITURE ELARG. STARK')
401   FORMAT(30X,'PROFIL QUASI-STATIQUE')
402   FORMAT(30X,'THEORIE DE GRIEM')
403   FORMAT(30X,'CALCUL DE L AMORTISSEMENT DE RESONNANCE')
404   FORMAT(30X,'ON NEGLIGE AMORTISSEMENT DE RESONNANCE')
405   FORMAT(30X,'PAS DE CONVOLUTION AVEC NOYAU DOPPLER')
406   FORMAT(30X,'CONVOLUTION STARK-DOPPLER',10X,'VT=',
     1 F5.1,'KM/S')
407   FORMAT(5E12.4)

      END
end
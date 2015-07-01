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

!> @todo Explain that each [subroutine] module declares the variables that it calculates
!> @todo module dependence map
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
!     2) troquei nome do arquivo de atomos para 'infile:atomgrade' e o de
!     moleculas para 'infile:moleculagrade'
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
!     b. dimensao e data de filetoh_llhy e dimensao de main_FILETOHY foram
!     atualizadas
!
!     c. incluidos filetoh_c_tauhi(FILETOH_NP,50),TAUHY(10,FILETOH_NP,50) e excluido IHH(500)
!
!     d. todo o codigo que se referia ao calculo das linha de H foram
!     ocultados, e o codigo a isto referente que estava em pfant01.h
!     foi acrescentado (correspondo ao codigo na secao
!     LECTURE TAU RAIE HYDROGENE ET INTERPOLATION DE TAUH ).
!
!     e. segundo as instrucoes enviadas pela Marie Noel em 2001:
!     - na rotina FTLIN3H foi incluida a linha
!     'if (ftt(itot).ne.0.0) k2=itot'
!     - DTOT foi substituido por FILETOH_NP nas dimensoes das matrizes
!           BK : TTD(FILETOH_NP), bk_KCD(FILETOH_NP,5)
!        LECTAUH : TTD(FILETOH_NP)
!        SELEKFH : TTD(FILETOH_NP), bk_KCD(FILETOH_NP, 50), selekfh_FL(FILETOH_NP), TAUH(FILETOH_NP,50)`
!
!
!     Tambem reduzi o numero de comentario que vao para a tela
!     'write(6...)' := cpc
!
! ========================================================================
!
!     Alteracao para calcular simultaneamente o continuo selekfh_FCONT(FILETOH_NP) e o
!     espectro normalizado FN(FILETOH_NP) {Paula, dez 2003}
!
!     - acrescentei as variaveis FN, selekfh_FCONT, FILEFLUX2, FILEFLUX3
!     - abro mais dois arquivos binarios unit=UNIT_CONT (continuo) e UNIT_NORM (normalizado)
!     - rotina SELEKFH:
!           - recebe tbem selekfh_FCONT e FN
!           - selekfh_FCONT eh calculado passando p/ a rotina FLIN1 apenas o
!           coeficiente de absorcao do continuo
!     - FN = selekfh_FL / selekfh_FCONT
!     - escrevo nos devidos arquivos
!
!     Portanto, para diferenciar os arquivos binarios criados,
!     alem do arquivo normal criado como 'spe.' + nome no infile:main
!     o pfant cria mais dois arquivos que comecam com 'cont.' e 'norm.'
!===PC2003 END===

!> @ingroup gr_math
!> Fantomol avec sous-programmes (MNP) -
!> Calcul possible de 100 angstrom en 100 angstrom.
!> @note Flux sortant est en nu: lambda (x-axis) vs. F(nu) (y-axis)
!>
!> @note Unit of flux: erg*s^-1*cm^-2/(Hz*ster), however (see next note)
!>
!> @note Actually what is called "flux" would be more accurately called "Specific intensity" [Gray Stellar Photospheres 3rd Ed. Eq 5.1]
!>
!> @note Flux absolu sortant a ete multiplie par 10**5
!>
!> @note Existing files are replaced
!>
!> @todo make file replacing clear somewhere because it was not the original behaviour

!>
!> @todo If I find any of the constants being used in another module, I shall move them to a separate module called "constants"

module synthesis
  use filetoh
  use read_most_files
  use molecules
  use logging
  use config
  use flin
  use misc_math
  use absoru
  use atomgrade
  use molecules
  use dissoc
  implicit none

  private  ! This statement makes all symbols private by default

  public :: synthesis_ ! subroutine


  !=====
  ! Module configuration
  !=====
  !> Whether to store the whole Fnu in vector synthesis::synthesis_ffnu
  logical, public :: synthesis_flag_ffnu = .false.


  !=====
  ! Spectrum stored in memory to be exported for nulbad
  !=====
  !> "Fnu". Only calculated if synthesis::synthesis_flag_ffnu is true
  real*8, public, allocatable :: synthesis_ffnu(:)
  !> last valid index within synthesis::synthesis_ffnu
  integer, public :: synthesis_ktot



  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  !=====
  ! Subroutine outputs
  !=====
  ! The following variables have the prefix of the subroutine that is responsible for them.

  !> Calculated by subroutine popul
  real*8, dimension(3,MAX_PARTIT_NPAR, MAX_MODELES_NTOT) :: popul_p

  !> Calculated by subroutine turbul
  real*8, dimension(MAX_MODELES_NTOT) :: turbul_vt

  !> calculated by subroutine popadelh
  real*8, dimension(MAX_ATOMGRADE_NBLEND) :: popadelh_corch, popadelh_cvdw
  !> Calculated by subroutine popadelh
  real*8, dimension(MAX_ATOMGRADE_NBLEND,MAX_MODELES_NTOT) :: &
   popadelh_pop, popadelh_a, popadelh_delta

  !> Calculated by subroutine selekfh
  real*8, dimension(FILETOH_NP) :: selekfh_fl, selekfh_fcont

  !> Calculated by subroutine bk
  real*8, dimension(0:MAX_MODELES_NTOT) :: bk_b, bk_b1, bk_b2
  !> Calculated by subroutine bk
  real*8, dimension(MAX_MODELES_NTOT) :: bk_kc, bk_kc1, bk_kc2, bk_phn, bk_ph2
  !> Calculated by subroutine bk
  real*8, dimension(FILETOH_NP, MAX_MODELES_NTOT) :: bk_kcd
  !> Calculated by subroutine bk
  real*8, dimension(FILETOH_NP) :: bk_fc

  !=====
  ! Constants available to all subroutines within this module
  !=====

  real*8, parameter :: & !< ?doc?
    C = 2.997929E+10,  & !< ?doc?
    H = 6.6252E-27,    & !< ?doc?
   KB = 1.38046E-16,   & !< ?doc?
   PI = acos(-1.),     & !< "pi" constant
   C1 = 4.8298E+15,    & !< ?doc?
   C2 = 8.8525E-13,    & !< ?doc?
   C4 = 2.1179E+8,     & !< ?doc?
   C6 = 3.76727E+11,   & !< ?doc?
   DEUXR = 1.6634E+8,  & !< ?doc?
   RPI = 1.77245385      !< ?doc?

  real*8, parameter :: &
   C5 = 2.*PI* (3.*PI**2/2.44)**0.4   !< ?doc?

  character*192 :: lll  !__logging__

contains

  !======================================================================================================================

  subroutine synthesis_()
    ! Units for output files
    integer, parameter :: &
     UNIT_SPEC  = 17, &
     UNIT_CONT  = 19, &
     UNIT_NORM  = 20, &
     UNIT_LINES = 32, &
     UNIT_LOG   = 31
    !<  amount to stretch calculation interval (both to the left and to the right)
    real*8, parameter :: LAMBDA_STRETCH = 20.

    real*8 lzero, lfin

    integer :: &
     d,        &
     dtot,     & ! (MT): The flux will be calculated for dtot different wavelenghts
     dhmy(10), &
     dhpy(10)
    integer dhm,dhp

    !> @todo still dimensions declared with numbers (absoru.f90 too)

    real*8 gfal(MAX_ATOMGRADE_NBLEND), ecart(MAX_ATOMGRADE_NBLEND), &
     ecartm(MAX_KM_MBLEND)
!   fonctions de partition TODO figure out what this comment refers to

    REAL*8 ttd(FILETOH_NP), fn(FILETOH_NP), &
           tauh(FILETOH_NP, 50), tauhy(10,FILETOH_NP,50)


    integer i, i1, i2, ih, &
     ikey,    & ! ikey-th main_aint-large calculation interval
     ikeytot, & ! total number of main_aint-large calculation intervals
     iht, ilzero, im, imy, irh, itot, k, l, li,&
     n
    real*8 lambd, l0, lf, allhy, alzero, tetaef, xlfin, xlzero, ahnu, ahnu1, &
     ahnu2, alph0, alph01, alph02

    !=====
    ! Read/setup
    !=====


    ! dissoc.dat needs to be read first because READ_MAIN() depends on dissoc_NMETAL
    call read_dissoc(fullpath_i(config_fn_dissoc))
    call read_main(fullpath_i(config_fn_main))
    call read_partit(fullpath_i(config_fn_partit))  ! LECTURE DES FCTS DE PARTITION
    call read_absoru2(fullpath_i(config_fn_absoru2))  ! LECTURE DES DONNEES ABSORPTION CONTINUE
    call read_modele(fullpath_i(config_fn_modeles))  ! LECTURE DU MODELE
    call read_abonds(fullpath_i(config_fn_abonds))
    call read_atomgrade(fullpath_i(config_fn_atomgrade))
    call read_filetoh() ! Hydrogen lines need no file names (uses main_filetohy array)
    call read_moleculagrade(fullpath_i(config_fn_moleculagrade))


    !> @todo issue ask blb overwriting variables read from infile:absoru2
    absoru2_abhel = modeles_nhe
    absoru2_abmet = absoru2_abmet*10.**main_asalog

    tetaef = 5040/main_teff

    !-----
    ! Output files opened here and left open until the end
    !-----
    ! Note that existing files are replaced
    open(unit=UNIT_SPEC, file=fullpath_o(trim(main_fileflux)//'.spec'), status='replace')  ! spectrum
    open(unit=UNIT_CONT, file=fullpath_o(trim(main_fileflux)//'.cont'), status='replace')  ! continuum
    open(unit=UNIT_NORM, file=fullpath_o(trim(main_fileflux)//'.norm'), status='replace')  ! normalized
    open(unit=UNIT_LINES,file=fullpath_o(config_fn_lines), status='replace')               ! outfile:lines
    open(unit=UNIT_LOG,  file=fullpath_o(config_fn_log), status='replace')                 ! log.log


    !=====
    ! Calculation begins!
    !=====

    call turbul()

    ! -- III --
    ! Calcul de quant  ne dependant que du metal et du modele
    ! Population du niv fond des ions
    call popul()

    ! -- IV --
    ! Calcul des quantites ne dependant que du
    ! Modele et de lambda : bk_b(n)   bk_kc(n)   bk_fc ISSUE probably wrong comment
    call sat4()


    !> @todo ISSUE Explain what it does
    xlzero = main_llzero-LAMBDA_STRETCH
    xlfin = xlzero+main_aint+LAMBDA_STRETCH
    if(xlfin .ge. (main_llfin+LAMBDA_STRETCH)) then
      ikeytot = 1
    else
      !> @todo issue it seems that i could write this using a modulus operator
      do i = 2,250
        xlfin = xlfin+main_aint
        if(xlfin .ge. (main_llfin+LAMBDA_STRETCH)) exit
      end do
      ikeytot = i
    end if

    lzero = main_llzero-LAMBDA_STRETCH
    lfin = lzero+main_aint+LAMBDA_STRETCH
    ikey = 1

    !> @todo check if 10 is LAMBDA_STRETCH/2
    l0 = main_llzero-10.
    lf = main_llfin+10.



    !=====
    ! Main loop
    !=====
    call log_progress(0, ikeytot)
    do while (.true.)
      call log_info('/\/\/\ Calculation step '//int2str(ikey)//'/'//int2str(ikeytot)//&
        ' /\/\/\')

      ! Note: (lfin-lzero) is constant except in the last iteration where lfin may be corrected
      dtot = int((lfin-lzero)/main_pas + 1.0005)

      !__spill check__
      if(dtot .gt. FILETOH_NP) then
        call pfant_halt('dtot = '//int2str(dtot)//' exceeds maximum of FILETOH_NP='//&
         int2str(FILETOH_NP))
      end if

      ! Allocates Fnu at first iteration
      if (synthesis_flag_ffnu .and. ikey .eq. 1) then
        ! This is a slight overallocation because dtot may be smaller in the last iteration.
        allocate(synthesis_ffnu(ikeytot*dtot))
        synthesis_ktot = 0
      end if




      !__logging__
      117 format(5x,'lzero=',f10.3,10x,'lfin=',f10.3,5x,'dtot=',i7)
      write(lll, 117) lzero, lfin, dtot
      call log_info(lll)

      lambd = (lzero+lfin)/2
      ilzero = int(lzero/100.)*100
      alzero = lzero-ilzero

      do d = 1,dtot
        ttd(d) = alzero+main_pas*(d-1)
      end do

      call bk()

      !__logging__
      501 format(2x,2x,'llzero=',f10.3,2x,'llfin=',f10.3,  2x,'lzero=',f10.3,2x,'lfin=',f10.3,2x,'lambd 1/2=',f10.3)
      write(lll,501) main_llzero,main_llfin,lzero,lfin,lambd
      call log_info(lll)


      im = 0
      do ih = 1,main_filetoh_numfiles
        allhy = filetoh_llhy(ih)-lzero

        !> @todo issue ask blb why +55 & -35?

        if (((allhy .gt. 0) .and. (allhy .le. (main_aint+55.))) .or. &
            ((allhy .lt. 0.) .and. (allhy .ge. (-35.)))) then

          im = im+1
          irh = 1
          iht = ih

          !__logging__
          712 format(1x,'im=',i3,2x,'lambda h=',f8.3,2x,a,2x,'ih=',i5)
          write(lll,712) im, filetoh_llhy(ih), main_filetohy(iht), iht
          call log_info(lll)

          ! Lecture tau raie hydrogene et interpolation de tauh
          ! Type *,' nom des fichiers TAU raies Hydrogene'
          !
          ! Old "LECTAUH()". Calculates tauh, dhmi and dhpi for file identified by ih.
          call filetoh_calc_tauh(ih, dtot, ttd, ilzero)

          dhmy(im) = filetoh_c_dhmi
          dhpy(im) = filetoh_c_dhpi
          do n = 1,modeles_ntot
            do d = 1,dtot
              tauhy(im, d, n) = filetoh_c_tauhi(d,n)
            end do
          end do
        end if
      end do

      imy = im
      if(imy .ne. 0) then
        !__logging__
        write(lll,*) (dhmy(im), im=1,imy)
        call log_debug('DHMY ==> '//lll)
        write(lll,*) (dhpy(im), im=1,imy)
        call log_debug('DHPY ==> '//lll)

        dhp = maxi(dhpy, imy, 1, imy)
        dhm = mini(dhmy, imy, 1, imy)
        do n = 1,modeles_ntot
          do d = 1,dtot
            tauh(d,n) = 0.0
          end do
        end do

        do n = 1,modeles_ntot
          do d = 1,dtot
            do im = 1,imy
              tauh(d,n) = tauh(d,n)+tauhy(im,d,n)
            end do
          end do
        end do
      else
        irh=0
        dhm=0
        dhp=0
      end if


      ! -- V --
      ! Quantites dependant de la raie et du modele
      call filter_atomgrade(lzero, lfin)

      if(atomgrade_nblend .gt. 0) then
        call popadelh()

        ! -- VI --
        ! Calcul du coefficient d absorption selectif et calcul du spectre
        do k = 1, atomgrade_nblend

          !> @todo ISSUE check these variables, they may be misnamed
          gfal(k) = atomgrade_gf(k)*C2*(atomgrade_lambda(k)*1.e-8)**2

          !> @todo issue ?what? ?doc? is ECART? (MT): some sort of delta lambda
          ecart(k) = atomgrade_lambda(k)-lzero+main_pas
        end do
      end if

      call filter_moleculagrade(lzero, lfin)
      call use_moleculagrade()

      !--debugging--!
      704 format(1x,'mblend=',i10)
      write(lll, 704) km_mblend
      call log_debug(lll)

      do l = 1, km_mblend
        ecartm(l) = km_lmbdam(l)-lzero + main_pas
      end do

      call selekfh()



      li = int(10./main_pas)
      i1 = li+1
      i2 = dtot - li
      if (lfin .ge. (main_llfin+LAMBDA_STRETCH)) then
        i2 = int((main_llfin+10.-lzero)/main_pas + 1.0005)
      end if
      itot = i2-i1+1
      do d = i1,i2
        selekfh_fl(d) = selekfh_fl(d)*(10.**5)
        selekfh_fcont(d) = selekfh_fcont(d)*(10.**5)
        fn(d) = selekfh_fl(d) / selekfh_fcont(d)  ! normalized spectrum
      end do


      ! Copies normalized spectrum to Fnu vector
      if (synthesis_flag_ffnu) then
        do d = i1,i2
          synthesis_ktot = synthesis_ktot+1
          synthesis_ffnu(synthesis_ktot) = fn(d)
        end do
      end if


      !=====
      ! Writes results for current iteration into open files
      !=====
      call write_lines_fort91()                        ! outfile:lines and fort.91
      call write_log()                                ! log.log
      call write_spec_item(UNIT_SPEC, selekfh_fl)     ! spectrum
      call write_spec_item(UNIT_CONT, selekfh_fcont)  ! continuum
      call write_spec_item(UNIT_NORM, fn)             ! normalized

      !__logging__
      707 format(1x,'ikey=',i10,2x,'lzero=',f10.3,2x,'lfin=',f10.3, 2x,'i1=',i7,2x,'i2=',i7)
      write(lll,707) ikey, lzero, lfin, i1, i2
      call log_info(lll)

      call log_progress(ikey, ikeytot)

      ikey = ikey+1
      if (ikey .gt. ikeytot) exit !main loop exit door!> @todo issue ?what? ?doc? does this condition mean?

      !__logging__
      708 format(1x,'ikey=',i10,2x,'irh=',i6)
      write(lll, 708) ikey, irh
      call log_info(lll)

      lzero = lzero+main_aint
      lfin = lfin+main_aint
      if(lfin .gt. (main_llfin+LAMBDA_STRETCH)) lfin = main_llfin+LAMBDA_STRETCH

    end do  ! main loop

    close(UNIT_SPEC)
    close(UNIT_CONT)
    close(UNIT_NORM)
    close(UNIT_LOG)
    close(UNIT_LINES)

    !__logging__
    call log_info('Flux sortant est en nu: Fnu x lambda')
    call log_info('Flux absolu sortant a ete multiplie par 10**5')

  contains  !--still pfant_calculate()

    ! These subroutines have total knowledge of the variable names and values that appear inside
    ! their parent subroutine PFANT_CALCULATE(). http://www.personal.psu.edu/jhm/f90/statements/contains.html

    !> Used to write the "spectrum", "continuum", and "normalized".
    !> Their writing pattern is very similar. THe header is the same,
    !> only the "ITEM" changes from file to file.

    subroutine write_spec_item(unit_, item)
      !> unit number, either UNIT_SPEC, UNIT_CONT, UNIT_NORM
      integer, intent(in) :: unit_
      !> either selekfh_fl, selekfh_fcont, or fn
      real*8, intent(in) :: item(:)
      real*8 amg
      amg = main_xxcor(8)  !> @todo issue is this assuming something to do with magnesium? (MT) I think that they did this because the alpha-enhanced is specified nowhere.

      1130 format(i5, 5a4, 5f15.5, 4f10.1, i10, 4f15.5)
      write(unit_, 1130)       &
       ikeytot,                &  ! fixed (same value for all iterations)
       modeles_tit,            &  ! fixed
       tetaef,                 &  ! fixed
       main_glog,              &  ! fixed
       main_asalog,            &  ! fixed
       modeles_nhe,            &  ! fixed
       amg,                    &  ! fixed
       l0,                     &  ! fixed
       lf,                     &  ! fixed
       lzero,                  &  ! changes (value changes with each iteration)
       lfin,                   &  ! changes
       itot,                   &  ! changes
       main_pas,               &  ! fixed
       main_echx,              &  ! fixed
       main_echy,              &  ! fixed
       main_fwhm                  ! fixed

      write(unit_,'(40000f15.5)') (item(d), d=i1,i2)
    end

    !> Writes into file log.log
    !> @todo issue too similar to write_spec_item (MT): Either get rid of it or include it as an optional output.

    subroutine write_log()
      integer d
      real*8 amg
      amg = main_xxcor(8)  !> @todo issue is this assuming something to do with magnesium?

      1130  format(i5, 5a4, 5f15.5, 4f10.1, i10, 4f15.5)
      write(UNIT_LOG, 1130) &
       ikeytot, &
       modeles_tit, &
       tetaef, &
       main_glog, &
       main_asalog, &
       modeles_nhe, &
       amg, &
       l0, &
       lf, &
       lzero, &
       lfin, &
       itot, &
       main_pas, &
       main_echx, &
       main_echy, &
       main_fwhm

      do d = i1,i2
        write(UNIT_LOG, *) l0+(d-1)*main_pas, selekfh_fl(d)
      end do
    end


    !> Writes into outfile:lines and fort.91

    subroutine write_lines_fort91()
      real*8 log_abond
      122 FORMAT(6X,'# LAMBDA',4X,'KIEX',5X,'L GF',3X,'L ABOND',6X,'CH',10X,'GR',10X,'GE',5X,'ZINF',4X,'CORCH')
      write(UNIT_LINES, 122)
      do k=1,atomgrade_nblend
        log_abond = log10(atomgrade_abonds_abo(k))

        125 format(a2,1x,i1,1x,f08.3,1x,f6.3,f09.3,f09.3,1x,3e12.3,f5.1, f7.1)
        write(UNIT_LINES, 125)     &
         atomgrade_elem(k),        &
         atomgrade_ioni(k),        &
         atomgrade_lambda(k),      &
         atomgrade_kiex(k),        &
         atomgrade_algf(k),        &
         log_abond-main_afstar+12, &
         atomgrade_ch(k),          &
         atomgrade_gr(k),          &
         atomgrade_ge(k),          &
         atomgrade_zinf(k),        &
         popadelh_corch(k)

        !> @todo ISSUE: Is file "fort.91" still wanted???? So similar to above!!! Why repeat??? (MT): It is not necessary for me.
       121 FORMAT(1X,A2,I1,1X,F08.3,1X,F6.3,F09.3,F09.3,1X,3E12.3,F5.1,F7.1)
        write(91,121)              &
         atomgrade_elem(k),        &
         atomgrade_ioni(k),        &
         atomgrade_lambda(k),      &
         atomgrade_kiex(k),        &
         atomgrade_algf(k),        &
         log_abond-main_afstar+12, &
         atomgrade_ch(k),          &
         atomgrade_gr(k),          &
         atomgrade_ge(k),          &
         atomgrade_zinf(k),        &
         popadelh_corch(k)
      end do
    end



    !======================================================================================================================
    !> Sets the Voigt profile using Hjertings' constants.
    !>
    !> @note Convolution for molecules uses Gaussian profile.
    !>
    !> @todo MT+JT Decision on variable MM: logic suggests that there should be one MM per molecule, so we are going to make

    subroutine selekfh()
      integer d
      real*8 :: bi(0:MAX_MODELES_NTOT)
      real*8, dimension(MAX_ATOMGRADE_NBLEND) :: &
       ecar, &
       ecartl, &
       ka
      real*8, dimension(MAX_MODELES_NTOT) :: &
       kap,    &
       kappa,  &
       kci,    &
       tauhd,  &
       kappam, &
       kappt
      real*8, dimension(MAX_KM_MBLEND) :: &
       ecarm,   &
       ecartlm, &
       kam
      real*8 :: &
       deltam(max_km_mblend,MAX_MODELES_NTOT), &
       phi, t, v, vm, lambi


    call log_debug('LLLLLLLLLLLLLLLLook at tetaef '//real82str(tetaef))


      if (atomgrade_nblend .ne. 0) then
        do k = 1,atomgrade_nblend
          ecar(k) = ecart(k)
        end do
      end if

      if (km_mblend .ne. 0) then
        do k=1,km_mblend
          ecarm(k) = ecartm(k)
        end do
      end if

      do d = 1, dtot
        lambi = (6270+(d-1)*0.02)
        if (atomgrade_nblend .ne. 0) then
          do k=1,atomgrade_nblend
            ecar(k)=ecar(k)-main_pas
            ecartl(k)=ecar(k)
          end do
        end if

        if(km_mblend .ne. 0) then
          do k=1,km_mblend
            ecarm(k) = ecarm(k)-main_pas
            ecartlm(k) = ecarm(k)
          end do
        end if

        do n = 1,modeles_ntot
          kappa(n) =0.
          kappam(n) =0.
          t = 5040./modeles_teta(n)

          ! atomes
          if(atomgrade_nblend .eq. 0) go to 260

          do  k=1,atomgrade_nblend
            if(abs(ecartl(k)) .gt. atomgrade_zinf(k)) then
              ka(k) = 0.
            else
              v = abs(ecar(k)*1.e-8/popadelh_delta(k,n))
              call hjenor(popadelh_a(k,n), v, popadelh_delta(k,n), phi)
              ka(k) = phi * popadelh_pop(k,n) * gfal(k) * atomgrade_abonds_abo(k)
              if(k .eq. 1) ka(k) = phi * popadelh_pop(k,n) * gfal(k)

            end if
            kappa(n) = kappa(n) + ka(k)
          end do   !  fin bcle sur k

          260 continue

          ! molecule
          if(km_mblend.eq.0) go to 250
          do l=1,km_mblend
            if( abs(ecartlm(l)) .gt. km_alargm(l) )  then
              kam(l)=0.
            else

              !> @todo ISSUE ask BLB decided to create big vector km_mm of repeated values
              deltam(l,n) = (1.e-8*km_lmbdam(l))/C*sqrt(turbul_vt(n)**2+DEUXR*t/km_mm(l))
              vm = abs(ecarm(l)*1.e-08/deltam(l,n))
              phi = (exp(-vm**2))/(RPI*deltam(l,n))
              kam(l) = phi*km_gfm(l)*km_pnvj(l,n)
            end if
            kappam(n)=kappam(n)+kam(l)
          end do   !  fin bcle sur l

          250 continue
          kappt(n) = kappa(n)+kappam(n)
          kci(n) = bk_kcd(d,n)
          kap(n) = kappt(n)+kci(n)
          bi(n) = ((bk_b2(n)-bk_b1(n))*(float(d-1)))/(float(dtot-1)) + bk_b1(n)
        end do    ! fin bcle sur n

        bi(0) = ((bk_b2(0)-bk_b1(0))*(float(d-1)))/(float(dtot-1)) + bk_b1(0)

        !__logging__
        if (d .eq. 1 .or. d .eq. dtot) then
          150 format(' d=',i5,2x,'kci(1)=',e14.7,2x,'kci(ntot)=',e14.7,10x,'kappa(1)=',e14.7,2x,'kappa(ntot)=',e14.7)
          152 format(10x,'kappam(1)=',e14.7,2x,'kappam(ntot)=',e14.7)
          151 format(' d=',i5,2x,'bi(0)=',e14.7,2x,'bi(1)=',e14.7,2x,'bi(ntot)=',e14.7)

          write(lll,151) d,bi(0),bi(1),bi(modeles_ntot)
          call log_debug(lll)
          write(lll,150) d,kci(1),kci(modeles_ntot),kappa(1),kappa(modeles_ntot)
          call log_debug(lll)
          write(lll,152)kappam(1),kappam(modeles_ntot)
          call log_debug(lll)
        end if

        if((d .lt. dhm) .or. (d .ge. dhp)) then
          call flin1(kap,bi,modeles_nh,modeles_ntot,main_ptdisk,main_mu, config_kik)
          selekfh_fl(d) = flin_f
        else
          do n = 1,modeles_ntot
              tauhd(n) = tauh(d,n)
          end do
          call flinh(kap,bi,modeles_nh,modeles_ntot,main_ptdisk,main_mu, config_kik,tauhd)
          selekfh_fl(d) = flin_f
        end if

        ! Dez 03-P. Coelho - calculate the continuum and normalized spectra
        call flin1(kci,bi,modeles_nh,modeles_ntot,main_ptdisk,main_mu, config_kik)
        selekfh_fcont(d) = flin_f
      end do  ! fin bcle sur d
    end


    !======================================================================================================================
    !> Calculates the flux in the continuum.
    !> @todo Issue there is a lot of calculation here that is independent from lzero and lfin
    !> @todo log_pe not used yet
    !>       was telling me

    subroutine bk()

      real*8 nu, llzero, llfin, nu1, nu2, &
       alph_n, &! old ALPH, which was a vector, but I realized it is used only inside loop, no need for vector
       log_pe   ! Created to avoid calculating ALOG10(PE) 3x
      real*8, dimension(2, max_modeles_ntot) :: kcj
      real*8, dimension(2) :: kcn, lambdc
      real*8 ::  fttc(FILETOH_NP)
      real*8 c3, c31, c32, fc1, fc2, t, tet0
      integer d, j

      call log_debug(ENTERING//'bk()')

      llzero = lzero
      llfin  = lfin
      nu1 = C* 1.e+8 /lzero
      ahnu1 = h*nu1
      c31 = (2*ahnu1) * (nu1/C)**2

      do n = 1,modeles_ntot
        t = 5040./modeles_teta(n)
        alph_n = exp(-ahnu1/(KB*t))
        bk_b1(n) = c31 * (alph_n/(1.-alph_n))
        call absoru_(llzero,modeles_teta(n),log10(modeles_pe(n)),1,1,1,1,2)
        bk_kc1(n) = absoru_totkap(1)
      end do

      nu2 = C* 1.e+8 /lfin
      ahnu2 = H*nu2
      c32 =(2*ahnu2) * (nu2/C)**2
      do n = 1,modeles_ntot
        !> @todo: calculate this "T" somewhere else, this is calculated all the time! a lot of waste
        t = 5040./modeles_teta(n)
        alph_n = exp(-ahnu2/(KB*t))
        bk_b2(n) = c32 * (alph_n/(1.-alph_n))
        call absoru_(llfin,modeles_teta(n),log10(modeles_pe(n)),1,1,1,1,2)
        bk_kc2(n) = absoru_totkap(1)
      end do

      nu = C* 1.e+8 /lambd
      ahnu = H*nu
      c3 =(2*ahnu) * (nu/C)**2
      do n=1,modeles_ntot
        t=5040./modeles_teta(n)
        alph_n = exp(-ahnu/(KB*t))
        bk_b(n) = c3 * (alph_n/(1.-alph_n))
        call absoru_(lambd,modeles_teta(n),log10(modeles_pe(n)),1,1,1,1,2)
        bk_phn(n) = absoru_znh(absoru2_nmeta+4) *KB * t
        bk_ph2(n) = absoru_znh(absoru2_nmeta+2) *KB * t
        bk_kc(n) = absoru_totkap(1)
      end do

      tet0 = fteta0(modeles_pg, modeles_teta, modeles_ntot)     !on extrapole modeles_teta pour modeles_nh=0
      t = 5040./tet0


      alph01 = exp(-ahnu1/(KB*t))
      bk_b1(0) = c31 * (alph01/(1.-alph01))
      call flin1(bk_kc1,bk_b1,modeles_nh,modeles_ntot,main_ptdisk,main_mu,config_kik)
      fc1 = flin_f

      alph02 = exp(-ahnu2/(KB*t))
      bk_b2(0) = c32 * (alph02/(1.-alph02))
      call flin1(bk_kc2,bk_b2,modeles_nh,modeles_ntot,main_ptdisk,main_mu,config_kik)
      fc2 = flin_f

      alph0 = exp(-ahnu/(KB*t))
      bk_b(0) = c3 * (alph0/(1.-alph0))
      call flin1(bk_kc,bk_b,modeles_nh,modeles_ntot,main_ptdisk,main_mu,config_kik)
      bk_fc = flin_f

      ilzero = int(lzero/100.)*100
      lambdc(1) = lzero-ilzero
      lambdc(2) = lfin-ilzero
      do n=1,modeles_ntot
        kcj(1,n)=bk_kc1(n)
        kcj(2,n)=bk_kc2(n)
      end do
      do n=1,modeles_ntot
        do j=1,2
          kcn(j)=kcj(j,n)
        end do
        call ftlin3(2,lambdc,kcn,dtot,ttd,fttc)
        do d=1,dtot
          bk_kcd(d,n)=fttc(d)  !> @todo these vector copies... pointer operations could speed up considerably here
        end do
      end do

      !__logging__
      153 format(' bk_kcd(1,1)=',e14.7,2x,'bk_kcd(1,ntot)=',e14.7)
      154 format(' bk_kcd(dtot,1)=',e14.7,2x,'bk_kcd(dtot,ntot)=',e14.7)
      write(lll,153) bk_kcd(1,1),bk_kcd(1,modeles_ntot)
      call log_debug(lll)
      write(lll,154) bk_kcd(dtot,1),bk_kcd(dtot,modeles_ntot)
      call log_debug(lll)

      call log_debug(LEAVING//'bk()')
    end

  end subroutine synthesis_



  !======================================================================================================================
  !> @todo issue ?what? ?doc? (MT): Related to line broadening due to Doppler effect caused by microturbulent velocity.

  subroutine turbul()
    integer i, nt2, n

    call log_debug('entree des turbul')
    if(main_ivtot .eq. 1)   then
      call log_debug('vt constant')
      do n = 1, modeles_ntot
        turbul_vt(n) = main_vvt(1)*1e5
      end do
    else
      101 format(10f8.3)
      call log_debug('vt variable avec la profondeur')
      call log_debug('    log to')
      write(lll,101) (main_tolv(i),i=1,main_ivtot)
      call log_debug(lll)
      call log_debug('    vt')
      write(lll,101) (main_vvt(i),i=1,main_ivtot)
      call log_debug(lll)

      if(config_interp .eq. 1) then
        call ftlin3(main_ivtot, main_tolv, main_vvt, modeles_ntot, modeles_t5l, turbul_vt)
      elseif (config_interp .eq. 2) then
        !> @todo issue ask if still useful mbecause it was switched off.
        !> @todo test this interpolation
        call ft2(main_ivtot, main_tolv, main_vvt, modeles_ntot, modeles_t5l,turbul_vt)
      end if


      nt2 = modeles_ntot-2
      do n = 1, nt2, 3
        102 format(3(i5,2f8.3,5x))
        write(lll,102) n,modeles_t5l(n),turbul_vt(n),(n+1), modeles_t5l(n+1), &
         turbul_vt(n+1),(n+2),modeles_t5l(n+2),turbul_vt(n+2)
        call log_debug(lll)
      end do

      do n = 1, modeles_ntot
        turbul_vt(n) = turbul_vt(n)*1e5
      end do
    end if


    if(main_ivtot .eq. 1) then
      131 format(' v micro constante  =',f6.1,'km/s')
      write(lll,131) main_vvt(1)
      call log_debug(lll)
    else
      call log_debug('v micro variable avec profondeur')
    end if

    return
  end subroutine


  !======================================================================================================================
  !> Calcule la pop du niv fond de l'ion pour tous les partit_NPAR atomes de
  !> la table des fonctions de partition ,a tous les niv du modele
  !>
  !> 40 elements, 50 niveaux de modele, 3 niv d'ionisation par elem.
  !> Partit donnee pour 33 temperatures au plus ds la table.
  subroutine popul()
    real*8 u(3), alistu(63), ue(50), tt(51), &
     aa, bb, uuu, x, y, t, tki2
    integer j, k, kmax, l, n

    do n = 1, modeles_ntot
      t = 5040./modeles_teta(n)  !> @todo I think the program deserves a modeles_T5040 because this is calculated everywhere!!!
      ue(n) = C1*KB*t/modeles_pe(n)*t**1.5
      do j = 1, partit_npar
        kmax = partit_jkmax(j)
        tt(1) = partit_tini(j)
        do  l=1,3
          do  k=1,kmax
            tt(k+1) = tt(k) + partit_pa(j)
            alistu(k) = partit_tabu(j,l,k)
          end do

          if (modeles_teta(n) .lt. tt(kmax-1) ) then
            ! interpolation parabolique
            uuu = ft(modeles_teta(n),kmax,tt,alistu)
          else
            ! interpolation lineaire entre 2 derniers pts
            aa = (alistu(kmax)-alistu(kmax-1)) / partit_pa(j)
            bb = alistu(kmax-1) - aa * tt(kmax-1)
            uuu = aa*modeles_teta(n) + bb
          end if

          u(l) = exp(2.302585*uuu)
        end do

        x=u(1) / (u(2)*ue(n)) * 10.**(partit_ki1(j)*modeles_teta(n))
        tki2= partit_ki2(j) * modeles_teta(n)
        !> @todo issue ask blb why 77???
        if (tki2 .ge. 77.) then
          y = 0.
          popul_p(3,j,n) = 0.
        else
          y = u(3)*ue(n)/u(2) * 10.**(-partit_ki2(j)*modeles_teta(n))
          popul_p(3,j,n) = (1./u(3))*(y/(1.+x+y))
        end if
        popul_p(2,j,n) = (1./u(2))*(1./(1.+x+y))
        popul_p(1,j,n) =  (1./u(1))*(x/(1.+x+y))
        end do
      end do
    return
  end subroutine


  !======================================================================================================================
  !> Calcule la population au niveau inferieur de la transition
  !> la largeur doppler popadelh_DELTA et le coefficient d'elargissement
  !> le "popadelh_A" utilise dans le calcul de H(popadelh_A,V)
  !

  subroutine popadelh()
    implicit none
    character*1 isi, iss
    integer j, k, ioo, iopi, n
    real*8 kies,kii,nul, ahnul, alphl(MAX_MODELES_NTOT), gamma, gh, t, tap, top, vrel
    data isi/' '/, iss/' '/

    do k = 1, atomgrade_nblend
      popadelh_corch(k) = 0.
      popadelh_cvdw(k) = 0
      do  j=1,partit_npar
        if(partit_el(j).eq.atomgrade_elem(k)) go to 15
      end do

      !> @todo this should probably be checked upon file reading
      104 format('Manque les fcts de partition du ', a2)
      write(lll,104) atomgrade_elem(k)
      call pfant_halt(lll)

      15 continue
      ioo = atomgrade_ioni(k)

      !> @todo ISSUE fort.77 disabled until someone misses it.
      ! write (77,*) atomgrade_elem(k),atomgrade_lambda(k)

      if(atomgrade_ch(k).lt.1.e-37)  then
        kies=(12398.54/atomgrade_lambda(k)) + atomgrade_kiex(k)
        if(ioo.eq.1)   kii=partit_ki1(j)
        if(ioo.eq.2)   kii=partit_ki2(j)
        if(popadelh_corch(k).lt.1.e-37)   then
          popadelh_corch(k)=0.67 * atomgrade_kiex(k) +1
        end if

        ! 125 format(3x ,' pour',f9.3,'   on calcule ch ', 'van der waals et on multiplie par ',f7.1)
        ! write(6,125)  atomgrade_lambda(k), popadelh_corch(k)
        popadelh_cvdw(k)= calch(kii,ioo,atomgrade_kiex(k),isi,kies,iss)
        atomgrade_ch(k)= popadelh_cvdw(k) * popadelh_corch(k)
      end if

!
      if(atomgrade_ch(k) .lt. 1.e-20) then
        iopi=1
      else
        iopi=2
      end if

      do  n=1,modeles_ntot
        t=5040./modeles_teta(n)
        nul= C* 1.e+8 /atomgrade_lambda(k)
        ahnul= H*nul
        alphl(n)=exp(-ahnul/(KB*t))

        tap = 1.-alphl(n)
        top = 10.**(-atomgrade_kiex(k)*modeles_teta(n))


        !> @todo noxig means number of atoms of oxygen
        !>
        !> @todo atomgrade first element must be oxygen
        !>
        !> @todo ISSUE BIG CONSIDERING Oxygen 1 but may have been filtered out!
        !>
        !> @todo ISSUE ask BLB why is Oxygen treated differently
        !>
        !> @todo issue ask blb what if we want more than 1 line of Oxygen?
        !>       My suggestion here is to have a vector flag what is set to true whenever
        !>       the name of the element is "O1" and test this flag in routine popadelh()

        if(k .eq. 1) then
          popadelh_pop(k,n) = top*tap*popul_p(ioo,j,n)*sat4_po(n)/sat4_pph(n)
        else
          popadelh_pop(k,n) = popul_p(ioo,j,n)*top*tap
        end if

        popadelh_delta(k,n) =(1.e-8*atomgrade_lambda(k))/C*sqrt(turbul_vt(n)**2+DEUXR*t/partit_m(j))
        vrel = sqrt(C4*t*(1.+1./partit_m(j)))
        if (iopi .eq. 1) then
          gh = C5*atomgrade_ch(k)**0.4*vrel**0.6
        else
          gh = atomgrade_ch(k) + popadelh_corch(k)*t
        end if
        gamma = atomgrade_gr(k)+(atomgrade_ge(k)*modeles_pe(n)+gh*(bk_phn(n)+1.0146*bk_ph2(n)))/(KB*t)
        popadelh_a(k,n) =gamma*(1.e-8*atomgrade_lambda(k))**2 / (C6*popadelh_delta(k,n))
      end do
    end do
  end
end module

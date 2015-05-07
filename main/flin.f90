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

!> Subroutines FLIN1() and FLINH()
!>
!> Calcul du flux ou de l'intensite par la methode d'integration
!> a 6 pts (ou 13pts) de R.Cayrel (these).
!> nouvelle methode de calcul de to . flin_TO(1) est calcule et
!> est different de 0 (On pose flin_TO(0)=0)   -Avril 1988-
!>
!> Originally by Roger Cayrel
!>
!> Outputs ONLY in module variables: flin_TO, flin_F
!> NO LONGER modifying arguments F and CAVA (they are no longer arguments; CAVA (error code) extinct)

!> @ingroup gr_math, gr_data

module flin
  implicit none

  !=====
  !> Output variables
  !=====
  real*8, dimension(0:50) flin_to
  real*8 flin_f

  private, real*8 :: td2, td,tp, cd, cp, c1, c2, c3
  dimension td2(26),td(6),tp(7),cd(6),cp(7),c1(13),c2(12),c3(12)

  data td /0.038,0.154,0.335,0.793,1.467,3.890 /
  data cd /0.1615,0.1346,0.2973,0.1872,0.1906,0.0288/
  data tp /0.0794,0.31000,0.5156,0.8608,1.3107,2.4204,4.0/
  data cp /0.176273,0.153405,0.167016,0.135428,0.210244,0.107848, 0.049787/
  data td2 /0.,.05,.1,.15,.3,.45,.60,.80,1.,1.2,1.4,1.6,1.8,2., &
   2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.2,4.6,5.487/
  data c1 /.032517,.047456,.046138,.036113,.019493,.011037,.006425, &
   .003820,.002303,.001404,.000864,.001045,.002769/
  data c2 /.111077,.154237,.143783,.108330,.059794,.034293, &
   .020169,.012060,.007308,.004473,.002761,.002757/
  data c3 /.023823,.030806,.027061,.019274,.010946,.006390, &
   .003796,.002292,.001398,.000860,.000533,.000396/

  logical, private, parameter :: mode_flinh = .true., &
                                 mode_flin1 = .false.

  !> This variable is needed just to fill in the allocation requisites for FLIN_() in FLIN1 mode
  real*8, private, dimension(max_modeles_ntot) :: dummy_tauhd

contains
  !---------------------------------------------------------------------------------------------------
  subroutine flin1(kap, b, nh, ntot, ptdisk, mu, kik)
    integer kik
    logical ptdisk
    real nh, kap, mu
    real*8, dimension(0:max_modeles_ntot) :: b
    real*8, dimension(max_modeles_ntot) :: nh, kap
    call flin_((kap, b, nh, ntot, ptdisk, mu, kik, dummy_tauhd, mode_flin1)
  end
      

  !---------------------------------------------------------------------------------------------------
  !>
  !> Two differences from FLIN1()
  !> 1) adds TAUHD vector to flin_TO
  !> 2) ignores PTDISK ISSUE!!!
  !>
  subroutine flinh(kap, b, nh, ntot, ptdisk, mu, kik, tauhd)
    integer kik
    logical ptdisk
    real nh, kap, mu
    real*8, dimension(0:max_modeles_ntot) :: b
    real*8, dimension(max_modeles_ntot) :: nh, kap, tauhd
    call flin_((kap, b, nh, ntot, ptdisk, mu, kik, tauhd, mode_flinh)
  end


  !> Generic routine, called by flin1() and flinh()
  !>
  !> @todo ISSUE BIG using 7 points in flin1() MODE!!!!!!!!!!!!!!!
  
  subroutine flin_(kap, b, nh, ntot, ptdisk, mu, kik, tauhd, mode_)
    use read_files
    implicit none
    !> ?
    real*8, intent(in) :: kap(max_modeles_ntot)
    !> ?
    real*8, intent(in) :: b(0:max_modeles_ntot)
    !> Source is probably @ref read_files::modeles_nh
    real*8, intent(in) :: nh(max_modeles_ntot)
    !> Source is probably @ref read_files::modeles_NTOT
    integer, intent(in) :: ntot
    !> Source is probably @ref read_files::main_PTDISK
    !> @li if .TRUE. : 7 points
    !> @li .FALSE.   : 6 points
    logical, intent(in) :: ptdisk
    !> Source is probably @REF read_files::main_MU
    real*8, intent(in) :: mu
    !> (old "IOP") accepts 0 or 1.
    !> @li if 0, uses the 6/7 point formulation
    !> @li if 1, uses the 26-point formulation
    integer, intent(in) :: kik
    !> Used only in FLINH mode
    real*8, intent(in) :: tauhd(max_modeles_ntot)
    !> Internal, either @ref mode_flin1 or @ref mode_flinh
    logical, intent(in) :: mode_

    real*8, dimension(13) :: fp, cc, bb
    real*8, dimension(26) :: bbb

    ! Calcul de flin_to
    epsi=0.05
    flin_to(0)=0.
    flin_to(1)=nh(1)*(kap(1)-(kap(2)-kap(1))/(nh(2)-nh(1))*nh(1)/2.)
    call integra(nh,kap,flin_to, ntot,flin_to(1))
    if (mode_ .eq. mode_flinh) then  ! flinh() mode only!!!
      do n=1,ntot
      flin_to(n) = flin_to(n)+tauhd(n)
      end do
    end if

    ! Calcul du flux
    if (kik .eq. 0) then
      ! Formule a 6 ou 7 pts
      if(ptdisk) then
        ipoint=7
        tolim=4.0
      else
        if (mode_ .eq. mode_flin1) then
          ipoint = 7  ! issue big !!!!! i kept this behaviour until i get feedback from blb
        else
          ipoint = 6
        end if
        tolim=3.89
      end if

      ! On verifie que le modele n'est pas trop court
      if (flin_to(ntot) .lt. tolim) then
        ! TODO MAKE IT FALL HERE!!!!! (TEST THIS)

        write(lll,1504)
        call log_halt(lll)
        write(lll,1503) ntot, flin_to(ntot)
        call log_halt(lll)
        call log_halt('')
        write(lll,131) ttd_d
        call pfant_halt(lll)
      end if

      2 continue
      do l=1,ipoint
        if(ptdisk) then
          tt(l) = tp(l)*mu
          cc(l)=cp(l)
        else
          tt(l) = td(l)
          cc(l) = cd(l)
        end if
      end do

      flin_f=0.
      do  l=1,ipoint
        bb(l)=faitk30(tt(l), flin_to, b, ntot)
        fp(l)=cc(l)*bb(l)
        flin_f=flin_f+fp(l)
      end do
      return

    elseif (kik .eq. 1) then
      ! Formule a 26 pts (ne marche que pour le flux!)
      ! (13pts +pts milieu)
      
      ! TODO test this error condition, better: put this verification in somewhere at startup, but has to be after READ_MAIN()
      if(ptdisk) then
        1500 format('Le sp flin_ ne peut calculer l intensite en 1 pt ', &
                    'du disque avec la formule a 26pts (utiliser 7pts kik=0)')
        write(6,1500)
        stop
      end if
      tolim=5.487  ! Le modele doit aller au moins a une prof TOLIM

      if(flin_to(ntot) .lt. tolim) then
        ! TODO MAKE IT FALL HERE!!!!! (TEST THIS)

        write(lll,1504)
        call log_halt(lll)
        write(lll,1503) ntot, flin_to(ntot)
        call log_halt(lll)
        call log_halt('')
        write(lll,131) ttd_d
        call pfant_halt(lll)
      end if

      do l=1,26
        t=td2(l)
        bbb(l) = faitk30(td2(l),flin_to,b,ntot)
      end do

      do m=1,12
        l=2*m - 1
        bb(m) = bbb(l+1)
        fp(m) = c1(m)*bbb(l) + c2(m)*bbb(l+1) + c3(m)*bbb(l+2)
        cc(m) = c2(m)
      end do
      
      fp(13) = c1(13)*bbb(26)
      bb(13) = bbb(26)
      cc(13) = c1(13)
      ! Ces bb et cc ne servent que pour les sorties (pas au calcul)
      
      flin_f=0.
      do l=1,13
        flin_f = flin_f+fp(l)
      end do
      return
    else
      call pfant_halt('Bad kik (must be 0 or 1)')
    end if  !(fin du if kik)


    1503  format(i10,5x,3hto=,f10.4)
    1504  format(18H Modele trop court)
    131   format('Ennui au calcul du flux (cf ligne precedente)', ' a lambd=',f10.3)
  end
end module flin
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

!> Subroutines flin1() and flinh()
!>
!> Calcul du flux ou de l'intensite par la methode d'integration
!> a 6 pts (ou 13pts) de R.Cayrel (these).
!> nouvelle methode de calcul de to . flin_to(1) est calcule et
!> est different de 0 (On pose flin_TO(0)=0)   -Avril 1988-
!>
!> @note in flin_(), MT+JT ipoint was being set to 7 regardless of ptdisk. This has been
!>       changed and now works as described: kik .true.: ptdisk .true. -> ipoint = 7; else 6
!>
!> @author Roger Cayrel
!>
!> @note Outputs in module variables: flin::flin_to, flin::flin_f

!> @ingroup gr_math, gr_data
!>
!> @todo gotta check, flin_to is never used, but I am pretty sure it used to be, in the original,
!> I may have deleted sth

module flin
  use misc_math
  use misc
  use max_
  implicit none
  private

  public flinh, flin1 ! Public subroutine
  !=====
  !> Output variables
  !=====
  !> ?doc?
  real*8, public, dimension(0:50) :: flin_to
  !> ?doc?
  real*8, public :: flin_f = 0

  ! 888b. 888b. 888 Yb    dP  db   88888 8888
  ! 8  .8 8  .8  8   Yb  dP  dPYb    8   8www
  ! 8wwP' 8wwK'  8    YbdP  dPwwYb   8   8
  ! 8     8  Yb 888    YP  dP    Yb  8   8888  private symbols

  real*8 :: td2, td,tp, cd, cp, c1, c2, c3
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

  logical, parameter :: mode_flinh = .true., &
                        mode_flin1 = .false.

  !> This variable is needed just to fill in the allocation requisites for FLIN_() in FLIN1 mode
  real*8, dimension(MAX_MODELES_NTOT) :: dummy_tauhd

  private flin_ ! private subroutine

contains
  !---------------------------------------------------------------------------------------------------

  !> See routine flin_() for description of parameters
  subroutine flin1(kap, b, nh, ntot, ptdisk, mu, kik)
    real*8, intent(in), dimension(MAX_MODELES_NTOT) :: kap, nh
    real*8, dimension(0:MAX_MODELES_NTOT), intent(in) :: b
    logical, intent(in) :: ptdisk
    real*8, intent(in) :: mu
    integer, intent(in) :: ntot, kik
    call flin_(kap, b, nh, ntot, ptdisk, mu, kik, dummy_tauhd, mode_flin1)
  end


  !---------------------------------------------------------------------------------------------------
  !>
  !> Two differences from FLIN1()
  !> 1) adds tauhd vector to flin_TO
  !> 2) ignores PTDISK ISSUE!!!
  !>

  subroutine flinh(kap, b, nh, ntot, ptdisk, mu, kik, tauhd)
    real*8, intent(in), dimension(MAX_MODELES_NTOT) :: kap, nh, tauhd
    real*8, dimension(0:MAX_MODELES_NTOT), intent(in) :: b
    logical, intent(in) :: ptdisk
    real*8, intent(in) :: mu
    integer, intent(in) :: ntot, kik
    call flin_(kap, b, nh, ntot, ptdisk, mu, kik, tauhd, mode_flinh)
  end


  !> Generic routine, called by flin1() and flinh()
  !>
  !> @todo ISSUE TOP using 7 points in flin1() MODE!!!!!!!!!!!!!!!

  subroutine flin_(kap, b, nh, ntot, ptdisk, mu, kik, tauhd, mode_)
    implicit none
    !> ?doc?
    real*8, intent(in) :: kap(MAX_MODELES_NTOT)
    !> ?doc?
    real*8, intent(in) :: b(0:MAX_MODELES_NTOT)
    !> Source is probably @ref reader_modeles::modeles_nh
    real*8, intent(in) :: nh(MAX_MODELES_NTOT)
    !> Source is probably @ref reader_modeles::modeles_ntot
    integer, intent(in) :: ntot
    !> Source is probably @ref reader_main::main_pfdisk
    !> @li if .TRUE. : 7 points
    !> @li .FALSE.   : 6 points
    logical, intent(in) :: ptdisk
    !> Source is probably @REF reader_main::main_mu
    real*8, intent(in) :: mu
    !> (old "IOP") accepts 0 or 1.
    !> @li if 0, uses the 6/7 point formulation
    !> @li if 1, uses the 26-point formulation
    integer, intent(in) :: kik
    !> Used only in FLINH mode
    real*8, intent(in) :: tauhd(MAX_MODELES_NTOT)
    !> Internal, either @ref mode_flin1 or @ref mode_flinh
    logical, intent(in) :: mode_

    real*8, dimension(13) :: fp, cc, bb, tt
    real*8, dimension(26) :: bbb

    real*8 epsi, t, tolim
    integer ipoint, l, m, n

    ! Calcul de flin_to
    epsi=0.05
    flin_to(0)=0.
    flin_to(1)=nh(1)*(kap(1)-(kap(2)-kap(1))/(nh(2)-nh(1))*nh(1)/2.)
    call integra(nh,kap,flin_to, ntot,flin_to(1))
    if (mode_ .eqv. mode_flinh) then  ! flinh() mode only!!!
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
        ipoint = 6

        ! if (mode_ .eqv. mode_flin1) then
        !   ipoint = 7  !> @todo ISSUE big !!!!! i kept this behaviour until i get feedback from blb
        ! else
        !   ipoint = 6
        ! end if
        tolim=3.89
      end if

      call check_modele_trop_court(1)

      continue
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

      !> @todo test this error condition, better: put this verification in somewhere at startup, but has to be after READ_main()
      if(ptdisk) then
        1500 format('Le sp flin_ ne peut calculer l intensite en 1 pt ', &
                    'du disque avec la formule a 26pts (utiliser 7pts kik=0)')
        write(6,1500)
        stop
      end if
      tolim=5.487  ! Le modele doit aller au moins a une prof TOLIM

      call check_modele_trop_court(2)


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
  contains
    !> Error verification, called twice

    subroutine check_modele_trop_court(i_call)
      !> Indicates where it was called from, used in error message: facilitates debugging
      integer, intent(in) :: i_call
      if(flin_to(ntot) .lt. tolim) then

        call pfant_halt('Modele too short (call #'//int2str(i_call)//'): ntot=' //&
         int2str(ntot) //'; to(' //&
         int2str(ntot) // ') = ' // real82str(flin_to(ntot)) // ' (must be >= '//&
          real82str(tolim) // ')')
        !write(lll,1504)
        !call log_halt(lll)
        !write(lll,1503) ntot, flin_to(ntot)
        !call pfant_halt(lll)
      end if
    end
  end
end module flin

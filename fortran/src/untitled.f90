
  !> Calculates hy_tauh, hy_dhp, hy_dhm
  subroutine calc_tauh()

      im = 0
      do ih = 1,filetoh_num_files
        allhy = filetoh_llhy(ih)-m_lzero

        if (((allhy .gt. 0) .and. (allhy .le. (main_aint+H_LINE_WIDTH+LAMBDA_STRETCH))) .or. &
            ((allhy .lt. 0.) .and. (allhy .ge. (-H_LINE_WIDTH)))) then
          im = im+1
          irh = 1
          iht = ih

          !#logging
          712 format(1x,'im=',i3,2x,'lambda h=',f8.3,2x,'filename=',a,2x,'ih=',i5)
          write(lll,712) im, filetoh_llhy(ih), ''''//trim(filetoh_filenames(iht))//'''', iht
          call log_info(lll)

          ! Lecture tau raie hydrogene et interpolation de tauh
          ! Type *,' nom des fichiers TAU raies Hydrogene'
          !
          ! Old "LECTAUH()". Calculates ct_tauh, ct_dhmi and ct_dhpi for file identified by ih.
          call calc_tauh(ih, dtot, ttd, ilzero)

          dhmy(im) = ct_dhmi
          dhpy(im) = ct_dhpi
          do n = 1,modeles_ntot
            do d = 1,dtot
              tauhy(im, d, n) = ct_tauhi(d,n)
            end do
          end do
        end if
      end do

      imy = im
      if(imy .ne. 0) then
        write(lll,*) (dhmy(im), im=1,imy)
        call log_debug('DHMY ==> '//lll)
        write(lll,*) (dhpy(im), im=1,imy)
        call log_debug('DHPY ==> '//lll)

        dhp = maxi(dhpy, imy, 1, imy)
        dhm = mini(dhmy, imy, 1, imy)

        do n = 1,modeles_ntot
          do d = 1,dtot
            tauh(n, d) = 0.0
            do im = 1,imy
              tauh(n, d) = tauh(n, d)+tauhy(im,d,n)
            end do
          end do
        end do
      else
        irh=0
        dhm=0
        dhp=0
      end if

* This is what I get so far:
*
* main_INUM =            1
* main_TEFF =    3488.0000000000000
* main_GLOG =   0.70999999999999996
* main_ASALOG=  0.22000000000000000
* modeles_DETEF   5777.00000
* modeles_DGLOG   4.44000006
* modeles_DSALOG   0.00000000
* ABS(main_TEFF-modeles_DETEF) =    2289.00000      > 1.0
*STOP 113
* ISSUE: Here what happened is that 5777-3488 = 2289 > 1
* Just reminding that there was a "GO TO 9" instead of a STOP 113

      PROGRAM TEST_READ_MODELES
      USE READ_FILES

      CHARACTER*256 fn_modele, fn_main, fn_dissoc

      fn_modele = 'modeles.mod'
      fn_main = 'main.dat'
      fn_dissoc = 'dissoc.dat'


      dissoc_NMETAL = 18  ! READ_MAIN() depends on this
      CALL READ_DISSOC(fn_dissoc)  ! I gotta fill in a few variables at the moment:
      CALL READ_MAIN(fn_main)  ! I gotta fill in a few variables at the moment:
                               !
                               ! main_TEFF
                               ! main_GLOG
                               ! main_ASALOG


      WRITE (*,*) 'main_INUM = ', main_INUM
      WRITE(*, *) 'main_TEFF = ', main_TEFF
      WRITE(*, *) 'main_GLOG = ', main_GLOG
      WRITE(*, *) 'main_ASALOG=', main_ASALOG


      DO I = 1, 10
        ! Forces main_INUM
        main_INUM = I

        CALL READ_MODELE(fn_modele)
        CALL WRITE_HERE()
      END DO
      END


      SUBROUTINE WRITE_HERE()
      USE READ_FILES

      WRITE(6,'(''1  MODELE A LIRE'',4F8.2,4X,''LIGNE'',I3,'
     +      //''' MODELES SUR LE DISQUE'')')
     +      modeles_TETAEF,
     +      modeles_GLOG,
     +      modeles_ASALOG,
     +      modeles_NHE,
     +      modeles_INUM


      WRITE(6, '(4F10.2,E15.4,5A4)') modeles_DETEF, modeles_DGLOG,
     +      modeles_DSALOG, modeles_ASALALF, modeles_NHE, modeles_TIT
      WRITE(6, '(''MODELE CALCULE AVEC LA TABLE'',A20)') modeles_TIABS



      WRITE(6,'(1X,''NTOT='',I3)') modeles_NTOT
      DO I = 1, modeles_NTOT
        WRITE(6,'(E20.4,4F15.4)') modeles_NH(I), modeles_TETA(I),
     +         modeles_PE(I), modeles_PG(I), modeles_T5L(I)
      END DO
      WRITE(*, *) 'etc.....'

      END


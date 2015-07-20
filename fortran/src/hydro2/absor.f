c     Calcule l'abondance des elements en tenant compte
c     d'une eventuelle surabondance des elements alfa
c     On entre avec les valeurs lues par lectur, le coef de deficience
c     et la surabondance des alfa on en deduit les abondances des
c     differents elements intervenant dans l'ionisation ZP et ABMET
c     l'abondance des elements lourds pour 1at d'H
c     Les elements alfa sont reconnus par leur masse
        SUBROUTINE abonio(absoru2_zp,config_zph,asasol,coefalf)

        Dimension ALFAM(7),absoru2_zp(30)
C      COMMONS AVEC LE SP D ABSORPTION CONTINUE
       COMMON/LECT2/ZP(30),absoru2_zm(30),absoru2_wi(41,2),absoru2_numset(2),CAL
       COMMON/SAHT/ZK(11),ZKM(30,9),absoru2_nr(30)
c
       DATA ALFAM/16,20.2,24.3,27,28,32,40/
       nalf=7   ! nbre d'elements alfa reconnus par leur masse ALFAM
       write(*,*) '    Appel abonio'
c
           do J=1,absoru2_nm
           ialfa=0
             do K=1, nalf
             DDM=ABS(absoru2_zm(J)-ALFAM(K))
                if (DDM.LT.0.3) then
                ialfa=ialfa+1
                end if
             end do  !fin de la bcke sur k
c            type 101,absoru2_nomet(J), ialfa
c
             if (ialfa.eq.0) then
             ZP(J)=absoru2_zp(J)
             else
             ZP(J)=absoru2_zp(J) * coefalf
             end if
          end do   ! fin de la bcle sur j
c
        SOM1=0
           DO J=1,absoru2_nm
           SOM1=SOM1+ZP(J)
           End do
        A0=SOM1/config_zph

c        write(6,*) ' liste des 4 premiers absoru2_zp', (absoru2_zp(I),I=1,4)
c        write(6,*) ' liste des 4 premiers ZP', (ZP(I),I=1,4)
        RETURN
        END

C> Data used in module absoru
C>
C> This code was kept in the .f style to avoid restructuring the data statements that
C> split numbers in half.
C>
C> @verbatim
C> DONNEES POUR H2+ =TABLE DE BATES(1952) COMPLETEE PAR HARVARD(1964)
C> ------------------
C> au_WINV=NU/C,au_GRDM=(-NU/DNU/DR)*R*D(R),U1=-U(1S SIGMA/R),au_U2=U(+2P SIGM
C> DONNEES POUR L'HELIUM NEUTRE
C> ----------------------------
C> au_ZLHEM(K)ET au_STWTM(K)=LAMBDAS DES DISCONTINUITES  ET VALEUR DU POIDS
C> STATISTIQUE CORRESPONDANT
C> au_ZLHE(L) ET au_ZEFF4(L)=SUITE DES DISCONTINUITEES ET SECTIONS EFFICACE
C> CORRESPONDANTES
C> DONNEES POUR L'HYDROGENE GRANT=MON. NOT. VOL. 118 P. 241 1958
C> ------------------------
C> au_G3D(I,J) =FACTEUR DE GAUNT EN FONCTION DE au_RHO ET DE LOG(-ETA)
C> au_RHO(I)  =VALEUR DE RHO CORRESPONDANTES
C> au_ZLETAG(J)=VALEUR DE LOG(-ETA)
C> au_YY=ZEROS DU POLYNOME DE LAGUERRE=CHANDRASEKHAR RADIATIVE TRANSFER
C> au_AA=NBR. DE CHRISTOFFEL CORRESPONDANT
C> DONNEES POUR QUASI MOLECULE H+H (DOYLE,APJ,153,187.1968)
C> @endverbatim
C>
C> References:
C> DONNEES POUR L'HYDROGENE GRANT=MON. NOT. VOL. 118 P. 241 1958
C> DONNEES POUR QUASI MOLECULE H+H (DOYLE,APJ,153,187.1968)



      module absoru_mod_data

      real*8, parameter :: au_winv(46) = (
     +/361.9,429.9,514.4,615.3,733.7,869.7,1028.7,1226.2,1460.9
     +,1737.3,2044.4,2407.4,2851.6,3378.1,3986.8,4677.7,5477.3,6442.5,75
     +74.3,8872.9,10338.2,12064.6,14049.7,16352.9,18996.2,22056.2,25576.
     +8,29634.9,34307.2,39703.3,45943.9,53171.7,61529.1,71213.6,82433.7,
     +95441.4,110445.3,127774.4,147406.7,169671.2,194568.0,221877.7,2516
     +00.4,282968.2,312800.5,329032.7/)

      real*8, parameter :: au_grdm(46) = (
     +/1729.881 ,1591.634 ,1450.598 ,1317.928 ,1198.805 ,1091.6
     +34 ,994.4223,896.8127,805.5777,722.7092,649.4024,583.2669,517.9283
     +,457.7689,405.1793,358.9641,316.3347,275.7371,238.9641,206.6136,18
     +0.4781,153.5857,130.4781,109.6813,91.9920,76.1753,62.7092,50.9960,
     +40.9562,32.5498,25.4263,19.6693,14.9920,11.2470,8.2869,5.9960,4.23
     +11,2.8900,1.9020,1.1733,0.6781,0.3544,0.1605,0.0602,0.0171,
     +0.0000/)

      real*8, parameter :: u1(46) = (
     +/0.00263,0.00286,0.00322,0.00372,0.00435,0.00511,0.00600,0.
     +00701,0.00821,0.00958,0.01190,0.01305,0.01522,0.01772,0.02061,0.02
     +394,0.02775,0.03207,0.03699,0.04257,0.04884,0.05584,0.06367,0.0722
     +9,0.08180,0.09216,0.10340,0.11542,0.12815,0.14147,0.15511,0.16871,
     +0.18167,0.19309,0.20167,0.20525,0.20052,0.18186,0.13996,0.05794,-0
     +.09644,-0.39105,-0.99032,-2.39840,-7.14260,-85.0/)

      real*8, parameter :: au_u2(46) = (
     +/0.00114,0.00124,0.00145,0.00178,0.00221,0.00277,0.00342,0.
     +00416,0.00508,0.00615,0.00745,0.00899,0.01083,0.01302,0.01561,0.01
     +869,0.02237,0.02676,0.03195,0.03810,0.04540,0.05412,0.06445,0.0767
     +6,0.09140,0.10889,0.12977,0.15473,0.18466,0.22057,0.26382,0.31606,
     +0.37932,0.45618,0.54997,0.66493,0.80665,0.98279,1.20442,1.48940,1.
     +87040,2.41450,3.28470,4.97840,9.99460,85.0/)

      real*8, parameter :: au_zlhem(5) = (
     +/504.3,2601.0,3122.0,3422.0,3680.0/)

      real*8, parameter :: au_stwtm(5) = (
     +/1.0,3.0,1.0,9.0,3.0/)

      real*8, parameter :: au_zlhe(10) = (
     +/0.0,0.0,7932.0,14380.0,22535.0,32513.0,44313.0,57936.0
     +,73383.0,90603.0/)

      real*8, parameter :: au_zeff4(10) = (
     +/0.0,0.0,1.069373 ,1.028328 ,1.022291 ,1.01
     +8358,1.015639,1.013614,1.012019,1.011869/)

      real*8, parameter :: au_zlh(19) = (
     +/911.8,3647.0,8205.9,
     +14588.2,22794.1,32823.5,44676.4,58352.9,73852.8,91176.3,110323.4,1
     +31293.9,154088.0,178705.6,205146.7,233411.4,263499.6,295411.3,3291
     +46.5/)

      real*8, parameter :: au_zlhep(19) = (
     +/227.8,911.8,2050.6,3645.6,5696.2,8202.5,11164.5,14582.
     +3,18455.7,22784.8,27569.6,32810.1,38506.3,44658.2,51265.8,58329.0,
     +65848.0,73822.6,82253.0/)

      real*8 :: g3d1(12,9)
      data g3d1
     +/2.885,2.419,2.047,1.679,1.468,1.323,1.212,1.124,1.051,0.9
     +89,0.810,0.693,2.906,2.420,2.049,1.684,1.474,1.330,1.220,1.133,1.0
     +61,1.000,0.824,0.708,2.912,2.430,2.072,1.723,1.527,1.395,1.296,1.2
     +18,1.155,1.102,0.951,0.856,2.892,2.423,2.082,1.760,1.583,1.469,1.3
     +85,1.320,1.268,1.226,1.111,1.045,2.815,2.365,2.046,1.755,1.604,1.5
     +07,1.438,1.387,1.346,1.314,1.230,1.185,2.715,2.280,1.978,1.709,1.5
     +73,1.488,1.428,1.383,1.348,1.320,1.249,1.208,2.615,2.194,1.906,1.6
     +54,1.530,1.452,1.398,1.357,1.326,1.303,1.237,1.202,2.231,1.868,1.6
     +29,1.440,1.352,1.298,1.261,1.235,1.215,1.198,1.158,1.136,1.955,1.6
     +35,1.445,1.303,1.238,1.201,1.175,1.157,1.144,1.133,1.106,1.091/

      real*8 :: g3d2(12,9)
      data g3d2
     +/1.807,1.518,1.357,1.239,1.187,1.157,1.137,1.123,1.112,1.
     +104,1.082,1.065,1.707,1.446,1.303,1.201,1.157,1.131,1.115,1.103,1.
     +094,1.087,1.069,1.054,1.634,1.394,1.266,1.175,1.136,1.114,1.100,1.
     +089,1.081,1.075,1.056,1.046,1.579,1.357,1.239,1.157,1.121,1.102,1.
     +088,1.079,1.072,1.067,1.049,1.042,1.497,1.302,1.201,1.131,1.101,1.
     +085,1.073,1.066,1.060,1.055,1.042,1.034,1.442,1.265,1.175,1.113,1.
     +088,1.073,1.064,1.057,1.052,1.046,1.035,1.030,1.400,1.237,1.156,1.
     +101,1.078,1.065,1.057,1.051,1.045,1.042,1.032,1.026,1.367,1.217,1.
     +142,1.091,1.071,1.059,1.051,1.045,1.041,1.037,1.029,1.024,1.342,1.
     +200,1.130,1.084,1.065,1.053,1.047,1.042,1.037,1.034,1.026,1.022/

      real*8, parameter :: au_rhog(12) = (
     + /1.010,1.025,1.050,1.100,1.150,1.200,1.250,1.300,1.350,1.
     +400,1.600,1.800/)
      real*8, parameter :: au_zletag(18) = (
     + /-3.0000,-2.0000,-1.0000,-0.6021,-0.3010,-0
     +.1249,0.0000,0.3979,0.6990,0.8751,1.0000,1.0969,1.1761,1.3010,1.39
     +79,1.4771,1.5441,1.6021/)

      real*8, parameter :: au_yy(4) = (/0.3225,1.7458,4.5366,9.3951/)
      real*8, parameter :: au_aa(4) = (/0.6032,0.3574,0.0389,0.0005/)

      real*8, dimension(12,18) :: au_g3d  !> au_g3d(i,j) = facteur de gaunt en fonction de au_rho et de log(-eta)
      equivalence (au_g3d(1,1),g3d1(1,1)),(au_g3d(1,10),g3d2(1,1))

      end module absoru_data

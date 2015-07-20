I think I am complicating
- introduce config variables
- see what is left
- I can even create a structure, but lets do the other things first


  type some_struc
    !> Configuration.
    !> @verbatim
    !> LL=1,CALCUL EN UN POINT SPECIFIE PAR SA LONGUEUR D'ONDE
    !> SI DANS CE CAS,J1=2 ON SOMME LES COEFF. D'ABSORPTION DE PLUSIEURS
    !> RAIES D'UNE MEME SERIE
    !> LL=2,CALCUL EN UN OU PLUSIEURS POINTS SPECIFIES PAR DELTA LAMBDA
    !> SI DANS CE CAS,J1=1,LA CONVOLUTION FINALE EST SAUTEE.
    !> @endverbatim
    integer :: ll
    !> NIV INF
    integer :: na
    !> NIV SUP
    integer :: nb
    , nbmin, nbmax,
    !> Central lambda
    real*8 :: clam

    real*8 :: c1

    real*8 :: ,c,

    !> This is being set in with a DATA statement, but first element is changed within raiehu()
    !> Is it data, or not?
    real*8 :: dlam(50)

    ,l,
    !> Configuration. See explanation in variable ll
    integer :: j1

    !> SI IND=0,ON ECRIT LE DETAIL DES APPROX. PAR LESQUELLES L EST CALCU
    !> SI IND=1, PAS D'ECRITURE
    integer :: ind

    !> "Theory" configuration
    !> @li 0: THEORIE DE GRIEM
    !> @li 1: THEORIE QUASISTATIQUE
    integer :: kq



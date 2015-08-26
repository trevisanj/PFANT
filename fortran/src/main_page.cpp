/**
@mainpage Welcome

PFANT is spectral synthesis software for astrononic applications written in Fortran.

PFANT is a suite consisting of four executables:
@li @ref innewmarcs -- interpolation d'un modele dans les grilles de modeles de
                     NEWMARCS (2005) en fonction de Teff, log g et [Fe/H]
@li @ref hydro2 -- calcul du profil d'une raie de l'hydrogene
@li @ref pfant -- spectral synthesis
@li @ref nulbad -- convolution utility


Why "PFANT"?
------------

Name evolution since started:

@verbatim
 |
t|     FANTÔME   - French for "ghost", "spectrum"
i|     FANTOMOL  - molecular calculus added
m|    PFANT      - "P" as in Perrin, Marie-Noel
e|
 |
 V
@endverbatim

Quick links
-----------

@ref overview

@ref developerguide

*/

/*
I THink I won't need these groups no longer with Doxygen 1.8.9


@defgroup gr_config System
@brief System configuration and startup
THis group contains modules and routines dedicated to setting up the environment state
for the numerical calculations.

@defgroup gr_math Maths

@defgroup gr_io Input/Output

@brief I/O and logging routines.
This group contains
@li routines to read input files
@li logging routines

@defgroup gr_data DATA statements
@brief Places where you can find @c DATA statements.
This group gathers all modules and subroutines that contain @c DATA statements.

@defgroup gr_filter Filtering routines
@brief @c lzero - @c lfin -dependent filtering

This group gathers all routines that filter arrays depending on @c lzero - @c lfin wavelength
interval.

Filtering is analogous to SQL
@code
SELECT ... WHERE lzero <= &lambda; <= lfin
@endcode

*/

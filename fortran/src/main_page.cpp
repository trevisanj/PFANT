/**
@mainpage Welcome

PFANT is a spectral synthesis software suite written in Fortran, consisting of four
executables:

@li innewmarcs -- interpolation d'un modele dans les grilles de modeles de
                     NEWMARCS (2005) en fonction de Teff, log g et [Fe/H]
@li hydro2 -- calcul du profil d'une raie de l'hydrogene
@li pfant -- spectral synthesis
@li nulbad -- convolution utility

In addition, a Python package named "pyfant" exists to manipulate PFANT
from Python.

Also, there is a set of Python scripts to visualize and edit data files, and
execute the Fortran code in a batch/parallel fashion.


Quick links
-----------

@li @ref overview -- tutorial-like page going through many relevant topics

Helping with development:

@li @ref coding_tools -- a page about development tools and environment setup

@li @ref coding_style -- a coding style proposal




*/



// this section is disabled for now
/*


@section history History

Since its start in 1967, the code has been through several enhancements. The
timeline is summarizes as follows.


@verbatim
 |
 | 1967 - FANTÔME (French for "ghost"/"spectrum")
 |        first version developed by Spite
 | 1982 - FANTOMOL
 |        Barbuy included the computation of molecular lines and
 |        dissociative equilibrium
 | 2003 - PFANT ("P" as in Perrin, Marie-Noel)
 |        large wavelength coverage and inclusion or hydrogen lines
 | 2015 - Conversion to Fortran 90 format and addition of a Python layer
t|
 V
@endverbatim

 */

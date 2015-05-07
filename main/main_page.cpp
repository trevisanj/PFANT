/**
! This is the source for the main page of the Doxygen documentation for PFANT


@mainpage Developers' documentation

Introduction
============

PFANT is spectral synthesis software written in Fortran.
This page will get you started in the structure of the code.


Structure of PFANT code
=======================

@li @ref gr_config

*/

/**
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


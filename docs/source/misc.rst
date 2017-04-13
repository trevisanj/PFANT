5  Miscellanea how-to
====================

5.1  Converting "VALD3 extended" format atomic lines to PFANT format
-------------------------------------------------------------------

| The Vienna Atomic Line Database (VALD) is "a
| collection of atomic and molecular transition parameters of
astronomical interest"
| (http://vald.astro.uu.se/).

To convert from the "VALD3 extended" to a "PFANT atomic lines" file:

.. code:: shell

    vald3-to-atoms.py <prefix>.vald3x
    tune-zinf atoms-<prefix>-untuned.dat

| This is done in two steps. The first step, ``vald3-to-atoms.py`` does
the actual conversion
| (which is quick) and saves a file, *e.g.*, ``atoms.dat``

| The second step (which is time-consuming) is performed by
``tune-zinf.py`` and aims
| to tune an important parameter used by the ``pfant`` Fortran binary.

| It is recommended to use the tool ``cut-atoms.py`` to cut the file
converted by
| ``vald3-to-atoms.py`` to a wavelength region of interest before
running ``tune-zinf.py``.

| For more information, see help for ``vald3-to-atoms.py``,
``tune-zinf.py``,
| ``cut-atoms.py`` (call these scripts with ``--help`` option).

5.2 Continuous opacities: selecting between PFANT and MARCS coefficients
------------------------------------------------------------------------

The PFANT default is to use its internal calculation of the continuum.

**PFANT-calculated continuum** (default)

.. code:: shell

    innewmarcs --opa F
    pfant --opa F --absoru T

or

::

    run4.py
    --opa F --absoru T

**MARCS opacities**

.. code:: shell

    innewmarcs --opa T
    pfant --opa T --absoru F

or

::

    run4.py
    --opa T --absoru F

| :notes: For ``innewmarcs``, "--opa T" causes the creation of an
additional file *modeles.opa*
| besides *modeles.mod*.

Related command-line options (also accessible in ``x.py``):

::

    --opa T ...... switches on MARCS opacities
                   (may be of two types: absorption and scattering)
    --abs T ...... switches on MARCS absorption
    --sca T ...... switches on MARCS scattering
    --absoru F ... switches off PFANT internal calculation

:notes: In order to use continuum opacities calculated by MARCS code
(http://marcs.astro.uu.se/), you will need to create your own
atmospheric model grid (using ``create-grid.py``) or download file
"grid.moo" as explained below (this file is too big to be stored on
GitHub (241 MB > 100 MB)). File "grid.moo" contains a 3D grid of MARCS
(http://marcs.astro.uu.se/) atmospheric models with opacities included.

(a) go to directory ``PFANT/data/common`` and run ``get-grid.moo.sh``,
or

| (b) download it from `this
location <https://docs.google.com/uc?export=download&confirm=4o6l&id=0B8m8GNLFiaewejd6dmJ6MW1pX2c>`__
| (or `this
location <https://drive.google.com/file/d/0B8m8GNLFiaewejd6dmJ6MW1pX2c/view>`__)
| and save it as *PFANT/data/common/grid.moo*

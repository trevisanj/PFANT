Cheatsheet
==========

PFANT Fortran binaries
----------------------

* ``innewmarcs``: interpolate atmospheric model
* ``hydro2``: calculate hydrogen lines
* ``pfant``: spectral synthesis
* ``nulbad``: convolution with Gaussian

Python applications from project PyFANT
---------------------------------------

Graphical applications
~~~~~~~~~~~~~~~~~~~~~~

* ``abed.py``: Abundances file editor
* ``ated.py``: Atomic lines file editor
* ``convmol.py``: Conversion of molecular lines data to PFANT format
* ``explorer.py``: F311 Explorer -- list, visualize, and edit data files (*à la* File Manager)
* ``mained.py``: Main configuration file editor
* ``mced.py``: Editor for molecular constants file
* ``mled.py``: Molecular lines file editor
* ``moldbed.py``: Editor for molecules SQLite database
* ``tune-zinf.py``: Tunes the "zinf" parameter for each atomic line in atomic lines file
* ``x.py``: PFANT Launcher -- Graphical Interface for Spectral Synthesis

Command-line tools
~~~~~~~~~~~~~~~~~~

* ``copy-star.py``: Copies stellar data files (such as main.dat, abonds.dat, dissoc.dat) to local directory
* ``create-grid.py``: Merges several atmospheric models into a single file (*i.e.*, the "grid")
* ``cut-atoms.py``: Cuts atomic lines file to wavelength interval specified
* ``cut-molecules.py``: Cuts molecular lines file to wavelength interval specified
* ``cut-spectrum.py``: Cuts spectrum file to wavelength interval specified
* ``hitran-scraper.py``: Retrieves molecular lines from the HITRAN database
* ``link.py``: Creates symbolic links to PFANT data files as an alternative to copying these (sometimes large) files into local directory
* ``nist-scraper.py``: Retrieves molecular constants from NIST Web Book for a particular molecule
* ``nulbad.py``: Convolve spectrum with Gaussian profile (similar, but not equivalent, to Fortran ``nulbad``)
* ``plot-spectra.py``: Plots spectra on screen or creates PDF file
* ``run4.py``: Runs the four Fortran binaries in sequence: ``innewmarcs``, ``hydro2``, ``pfant``, ``nulbad``
* ``save-pdf.py``: Looks for files "*.norm" inside directories session-* and saves one figure per page in a PDF file
* ``vald3-to-atoms.py``: Converts VALD3 atomic/molecular lines file to PFANT atomic lines file.

.. hint:: All programs have a ``--help`` argument.


Command-line options for the Fortran binaries
---------------------------------------------

Options are accompained by their default values.

Below, ``<main_xxxxx>`` means that option ``xxxxx`` is, by default, read from PFANT main configuration file.

``hydro2``
~~~~~~~~~~

.. code:: shell

    hydro2 \
        --logging_console T \
        --logging_dump F \
        --logging_fn_dump <executable name>_dump.log \
        --fn_main main.dat \
        --fn_modeles modeles.mod \
        --fn_absoru2 absoru2.dat \
        --fn_hmap hmap.dat \
        --interp 1 \
        --kik 0 \
        --ptdisk <main_llzero>  \
        --llfin <main_llfin>  \
        --amores T \
        --kq 1 \
        --zph 12.00

``innewmarcs``
~~~~~~~~~~~~~~

.. code:: shell

    innewmarcs \
        --logging_console T \
        --logging_dump F \
        --logging_fn_dump <executable name>_dump.log \
        --fn_main main.dat \
        --fn_modeles modeles.mod \
        --fn_modgrid grid.mod \
        --fn_moo grid.moo \
        --allow F \
        --fn_opa modeles.opa \
        --opa T

``pfant``
~~~~~~~~~

.. code:: shell

    pfant \
        --logging_console T \
        --logging_dump F \
        --logging_fn_dump <executable name>_dump.log \
        --fn_main main.dat \
        --fn_modeles modeles.mod \
        --fn_absoru2 absoru2.dat \
        --fn_hmap hmap.dat \
        --interp 1 \
        --kik 0 \
        --ptdisk <main_llzero>  \
        --llfin <main_llfin>  \
        --fn_opa modeles.opa \
        --fn_partit partit.dat \
        --fn_abonds abonds.dat \
        --fn_atoms atoms.dat \
        --no_molecules F \
        --no_atoms F \
        --no_h F \
        --pas <main_pas>  \
        --aint <main_aint> \
        --opa T \
        --abs F \
        --opa T \
        --opa T \
        --fn_dissoc dissoc.dat \
        --fn_molecules molecules.dat \
        --flprefix <main_flprefix>

``nulbad``
~~~~~~~~~~

.. code:: shell

    nulbad \
        --logging_console T \
        --logging_dump F \
        --logging_fn_dump <executable name>_dump.log \
        --fn_main main.dat \
        --flprefix <main_flprefix>  \
        --fn_flux <main_flprefix>.norm  \
        --flam F \
        --fn_cv <flux file name>.nulbad.<fwhm> \
        --pat <main_pas>  \
        --convol T \
        --fwhm <main_fwhm>


PFANT on GitHub: http://github.com/trevisanj/PFANT

PyFANT on GitHub: http://github.com/trevisanj/pyfant
Cheatsheet
==========


* **Description of stellar parameters, running settings,
  command-line options:** run ``x.py``  and navigate through the fields in Tabs 1 and 3.
  As you navigate, a description of the current field will be displayed at the bottom of the window.

* Call a program with ``--help`` option, *e.g.*, ``pfant --help``.


* ``programs.py`` lists all Fortran programs in the PFANT pack, and Python programs in the `F311 <trevisanj.github.io/f311>`_ project.


PFANT Fortran binaries
----------------------

* ``innewmarcs``: interpolate atmospheric model
* ``hydro2``: calculate hydrogen lines
* ``pfant``: spectral synthesis
* ``nulbad``: convolution with Gaussian

Python applications from project f311
-------------------------------------

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
* ``plot-spectra.py``: Plots spectra on screen or creates PDF file
* ``run4.py``: Runs the four Fortran binaries in sequence: ``innewmarcs``, ``hydro2``, ``pfant``, ``nulbad``
* ``save-pdf.py``: Looks for files "*.norm" inside directories session-* and saves one figure per page in a PDF file
* ``vald3-to-atoms.py``: Converts VALD3 atomic/molecular lines file to PFANT atomic lines file.

PFANT at GitHub: http://github.com/trevisanj/PFANT

Project f311 at GitHub: http://github.com/trevisanj/f311
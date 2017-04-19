Cheatsheet
==========


* **Description of stellar parameters, running settings,
  command-line options:** run ``x.py``  and navigate through the fields in Tabs 1 and 3.
  As you navigate, a description of the current field will be displayed at the bottom of the window.

* Call a program with "--help" option, *e.g.*, ``pfant --help``.


* ``programs.py`` lists all Fortran/Python programs.


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
* ``create-grid.py``: Merges several atmospheric models into a single file (_i.e._, the "grid")
* ``cut-atoms.py``: Cuts atomic lines file to wavelength interval specified
* ``cut-molecules.py``: Cuts molecular lines file to wavelength interval specified
* ``cut-spectrum.py``: Cuts spectrum file to wavelength interval specified
* ``download-hitran.py``: Downloads molecular lines from HITRAN database
* ``explorer.py``: F311 Explorer --  list, visualize, and edit data files (_à la_ File Manager)
* ``print-nist.py``: Downloads and prints molecular constants from NIST Web Book for a particular molecule
* ``mained.py``: Main configuration file editor.
* ``mled.py``: Molecular lines file editor.
* ``tune-zinf.py``: Tunes the "zinf" parameter for each atomic line in atomic lines file
* ``x.py``: PFANT Launcher -- Graphical Interface for Spectral Synthesis

Command-line tools
~~~~~~~~~~~~~~~~~~

* ``copy-star.py``: Copies stellar data files (such as main.dat, abonds.dat, dissoc.dat) to local directory
* ``link.py``: Creates symbolic links to PFANT data files as an alternative to copying these (sometimes large) files into local directory
* ``plot-spectra.py``: Plots spectra on screen or creates PDF file
* ``run4.py``: Runs the four Fortran binaries in sequence: ``innewmarcs``, ``hydro2``, ``pfant``, ``nulbad``
* ``save-pdf.py``: Looks for files "*.norm" inside directories session-* and saves one figure per page in a PDF file
* ``vald3-to-atoms.py``: Converts VALD3 atomic/molecular lines file to PFANT atomic lines file.

PFANT homepage: http://github.com/trevisanj/PFANT
Project f311 homepage: http://github.com/trevisanj/f311
/**

@page overview Overview

@section overview_contents Project contents

PFANT 

@section overview_install Installation

Clone from github

@code
git clone https://github.com/trevisanj/PFANT
@endcode

This will create a "PFANT" directory.

or download <a href="https://github.com/trevisanj/PFANT/archive/master.zip">this zip file</a>.





@section tree Overview of the project directory

Here is an incomplete listing of the directory tree.

@verbatim
PFANT
├── fortran
│   ├── bin                      PFANT executable binaries
│   └── src                      Fortran source code directory
├── pyfant                       PyFANT root
│   ├── scripts                  command-line tools
│   └── pyfant                   Python package
└── data                         courtesy data
    ├── arcturus                 Arcturus data files
    └── sun                      Sun data files
@endverbatim




@section overview_compile Compiling the Fortran source code.

The source code uses Fortran 2003 language features. gfortran >= 4.6 required (4.8 tested)

The code can be compiled using the CBFortran IDE, or typing

@code
$ cd fortran
$ make_linux.sh     # Linux
$ make_windows.bat  # Windows
@endcode

The executable binaries are found in PFANT/fortran/bin


@section overview_path Setting the paths

Add the following directory to your system path:

@code
PFANT/fortran/bin      # Fortran executable binaries
PFANT/pyfant/scripts   # *.py scripts
@endcode

, where "PFANT" will actually be somehing like "/home/user/.../PFANT".

Add the following directory to your PYTHONPATH environment variable:

<code>PFANT/pyfant</endcode>


@subsection overview_path_add-paths The script add-paths.py

If you run on Linux, the script PFANT/add-paths.py may be used to attempt to set
these variables automatically.

@code
$ ./add-paths.py --tcsh  # if you use the tcsh shell
$ ./add-paths.py --bash  # if you use the bash shell
@endcode



@section test_bin Quick start

This section will take you through the steps to calculate a synthetic spectrum
from the Sun, and visualize some input and output data files.

@li Create a new directory, e.g., "/home/user/sun-synthesis"
@li Enter the sun-synthesis directory
@li Copy the contents of the PFANT/data/sun-complete into sun-synthesis

@li 
Now try these four commands:

@code
$ innewmarcs  # creates modeles.mod
$ hydro2      # creates thalpha
$ pfant       # creates flux.*
$ nulbad      # creates flux.norm.nulbad
@endcode

@subsection cmd_plot Python command-line tools

To see all available Python scripts, type

@code
$ pyfant-scripts.py
@endcode

This will print something like this:

@verbatim
ated.py .................. ated - ATomic lines file EDitor
mled.py .................. mled - Molecular Lines EDitor
plot-filetoh.py .......... Plots hydrogen lines
plot-innewmarcs-result.py  Plots interpolated atmospheric model curve
                           (hopefully) in the middle of the curves
plot-mod-record.py ....... Plots one record of a binary .mod file (e.g.,
                           modeles.mod, newnewm050.mod)
plot-mod-records.py ...... Opens several windows to show what is inside a
                           NEWMARCS grid file.
plot-spectra.py .......... Plots one or more spectra, either stacked or
                           overlapped.
pyfant-scripts.py ........ Lists scripts in PFANT/pyfant/scripts directory.
run-multi.py ............. Runs pfant for different abondances for each element,
                           then run nulbad for each pfant result for different
                           FWHMs.
run4.py .................. Runs the 4 exes
save-pdf.py .............. Looks for file "flux.norm" inside directories
                           session_* and saves one figure per page in a PDF
                           file.
vis-console.py ........... Text-based menu application to open and visualize
                           data files.
@endverbatim

Let's use some of these tools. One thing that we can do is to compare the
synthetic spectrum before and after the convolution:

@code
$ plot-spectra.py --ovl flux.norm flux.norm.nulbad
@endcode

Other things to try:

@code
$ plot-mod-record.py modeles.mod      # plots interpolated atmospheric model
$ plot-mod-records.py newnewm050.mod  # plots NEWMARCS grid
$ plot-filetoh.py thalpha             # plots hydrogen lines
@endcode

@subsection overview_getting_help Command-line help

The Fortran binaries and Python scripts all have a --help option, for example:

@code
$ pfant --help
$ innewmarcs --help
$ run4.py --help
@endcode



@section required Required software

In order to run all the features described so far, here is a list of what you should have installed:

@subsection required_applications Applications

@verbatim
What              Why?
-------------------------------------------------------------------------------------
git               clone repository at github
gfortran, make    compile the Fortran code (gfortran >= 4.6 required)
Python 2.7        use PyFANT
pip               install fortranformat Python package
@endverbatim

@subsection required_python_packages Python packages

@verbatim
What              Recommended way to install
-------------------------------------------------------------------------------------
matplotlib        Debian-based Linux: $ sudo apt-get install python-matplotlib
pyqt4             Debian-based Linux: $ sudo apt-get install python-qt4
                  Windows: download Python 2.7 installer at https://riverbankcomputing.com/software/pyqt/download
mayavi2           Debian-based Linux: $ sudo apt-get install mayavi2
fortranformat     pip install fortranformat
@endverbatim

@subsection required_windows_compiler Windows compiler

A possible way to compile in Windows is to install MinGW (http://sourceforge.net/projects/mingw/files/).
In MinGW Installation Manager, install at least the following packages:
mingw-developer-toolkit, mingw32-base, mingw32-gcc-fortran, msys-base.


@section datafiles Input/output data files

Here is a list of files used, and their default names.
@verbatim
--option          |  Default name     | Description
------------------+-----------------------------------------------------------
*** star-specific files ***
--fn_main         |      main.dat     | main configuration file
--fn_abonds       | abonds.dat        | abundances
--fn_dissoc       | dissoc.dat        | dissociation equilibrium data

*** "constant" data files ***
--fn_absoru2      | absoru2.dat       | absorbances ?doc?
--fn_atoms        | atomgrade.dat     | atomic lines
--fn_molecules    | moleculagrade.dat | molecular lines
--fn_partit       | partit.dat        | partition functions
                  | hmap.dat          | hydrogen lines info (filename, lambda, na, nb, kiex, c1)
                  |   /gridsmap.dat   | list of NEWMARCS grids
                  |  / newnewm150.mod | NEWMARCS grid listed in gridsmap.dat
                  | -  newnewm100.mod | "
                  |  \ newnewm050.mod | "
                  |  | newnewp000.mod | "
                  |   \newnewp025.mod | "

*** created by innewmarcs ***
--fn_modeles      | modeles.mod       | atmospheric model (binary file)

*** created by hydro2 ***
                  | thalpha           | hydrogen lines files
                  | thbeta            | "
                  | thgamma           | "
                  | thdelta           | "
                  | thepsilon         | "

*** created by pfant ***
flux.norm         | normalized flux
flux.spec         | un-normalized flux (multiplied by 10**5)
flux.cont         | continuum flux (multiplied by 10**5)

*** created by nulbad ***
flux.norm.nulbad  | convolved flux
@endverbatim

@section overview_workflow Fortran bin workflow

The Fortran binaries run in this order: innermarcs, hydro2, pfant, nulbad

THe workflow can be summarized like this:

@verbatim
Summary:
innewmarcs creates modeles.mod (atmospheric model)
hydro2     creates a series of th* files (hydrogen lines files)
pfant      creates flux.* (spectrum, normalized spectrum, and continuum)
nulbad     creates flux.norm.nulbad (convolved un/normalized spectrum)
@endverbatim

Or in more detail:

@verbatim
                        +---------------------------+---------------------main.dat
  gridsmap.dat          |                           |                        |
newnewm150.mod          v                           v                        |
newnewm100.mod    +----------+                  +------+                     |
newnewm050.mod+-->|innewmarcs|-->modeles.mod+-->|hydro2|<----------+         |
newnewp000.mod    +----------+         +        +------+           |         |
newnewp025.mod                         |            |              |         |
                                       |            v              |         |
                                       |         thalpha           |         |
                                       |         thbeta        absoru2.dat   |
                                       |         thgamma       hmap.dat      |
                                       |         thdelta           |         |
                                       |         thepsilon         |         |
                                       |            |              |         |
                         abonds.dat    |            v              |         |
                         dissoc.dat    +-------->+-----+           |         |
                      atomgrade.dat              |pfant|<----------+         |
                  moleculagrade.dat+------------>+-----+<--------------------+
                         partit.dat                 |                        |
                                                    v                        |
                                                flux.norm                    |
                                                flux.spec                    |
                                                flux.cont                    |
                                                    |                        |
                                                    v                        |
                                                 +------+                    |
                                                 |nulbad|<-------------------+
                                                 +------+
                                                    |
                                                    v
                                             flux.norm.nulbad
@endverbatim

-x-x-x-x-x

@verbatim
.
.
.
  to be continued ...

@endverbatim

*/


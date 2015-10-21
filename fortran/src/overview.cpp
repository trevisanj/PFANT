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
│   ├── bin                      PFANT executable binaries (add to system path!)
│   └── src                      Fortran source code directory
├── pyfant                       PyFANT root
│   ├── scripts                  command-line tools (add to system path!)
│   │   │                                                                             
│   │   ├── ...                  scripts to handle data files (visualize, edit),
│   │   ├── ...                  scripts to run the Fortran code
│   │   └── ...                  etc...
│   │
│   └── pyfant                   Python package
└── data                         courtesy data
    └── sun-complete             runnable case for the Sun
@endverbatim




@section overview_compile Compiling the Fortran source code.

The source code uses Fortran 2003 language features. gfortran >= 4.6 required (4.8 tested)

The code can be compiled in CBFortran IDE, or using one of these:

@code
PFANT/fortran/make_linux.sh    # Linux
PFANT/fortran/make_windows.bt  # Windows
@endcode

The executable binaries are found in PFANT/fortran/bin


@section overview_path Setting the system path

It is best to add a few directories to the system path

@code
PFANT/fortran/bin      # Fortran executable binaries
PFANT/pyfant/scripts   # *.py scripts
@endcode

, where "PFANT" will actually be somehing like "/home/user/.../PFANT".


Add the following to your PYTHONPATH variable:
@li PFANT/pyfant

@subsection overview_path_add_paths The script add_paths.py

@note If you run on Linux, the script PFANT/add_paths.py may be used to attempt to set
these variables automatically.

@code
$ ./add_paths.py --tcsh  # if you use the tcsh shell
$ ./add_paths.py --bash  # if you use the bash shell
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

@subsection cmd_plot Command-line plotting tools

Now let's compare que flux calculated by pfant with the convolved spectrum calculated
by nulbad (requires <a href="http://matplotlib.org/faq/installing_faq.html">matplotlib</a>.

@code
$ plot_spectra.py flux.norm flux.norm.nulbad
@endcode

Try these ones as well:

@code
$ plot_mod_record.py modeles.mod      # plots interpolated atmospheric model
$ plot_mod_records.py newnewm050.mod  # plots NEWMARCS grid
$ plot_filetoh.py thalpha             # plots hydrogen lines
@endcode


@subsection overview_getting_help Command-line help

The Fortran binaries and Python scripts all have a --help option, for example:

@code
$ pfant --help
$ innewmarcs --help
$ run4.py --help
@endcode

@todo Python script to provide a menu





@section required Required software

In order to run all the features described so far, here is a list of what you should have installed:

Applications:

@verbatim
What              Why?
-------------------------------------------------------------------------------------
git               clone repository at github
gfortran, make    compile the Fortran code
Python 2.7        use PyFANT
pip               install fortranformat Python package
@endverbatim

Python packages:

@verbatim
What              Recommended way to install
-------------------------------------------------------------------------------------
matplotlib        $ sudo apt-get install python-matplotlib  # Debian-based Linux
pyqt4             $ sudo apt-get install python-qt4  # Debian-based Linux
@li fortranformat $ sudo pip install fortranformat  # Requires pip



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

The Fortran binnries run in this order: innermarcs, hydro2, pfant, nulbad

THe workflow can be summarized like this:

@verbatim
Summary:
innewmarcs creates modeles.mod
hydro2     creates a series of th* files
pfant      creates flux.*
nulbad     creates flux.norm.nulbad   
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


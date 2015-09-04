/**

@page overview Overview

@section Installation

Clone from github

@code
git clone https://github.com/trevisanj/pfant
@endcode

or download <a href="https://github.com/trevisanj/pfant/archive/master.zip">this zip file</a>.

Henceforth, the symbol @c "%" denotes the directory created during installation.

@subsection install_path Setting the system path

Add the following directories to your system path:
@li %/fortran/bin
@li %/pypfant/scripts

Add the following to your PYTHONPATH variable:
@li %/pypfant

@note If you run on Linux, the script %/add_paths.py can be handy.

@code
$ ./add_paths.py --tcsh  # if you use the tcsh shell
$ ./add_paths.py --bash  # if you use the bash shell
@endcode

@subsection required Required software

@verbatim
What                          Why?                       How to install
----------------------------------------------------------------------------------
gfortran, make                 Compile the Fortran code

python 2.7                     run the Python scripts

matplotlib (python package)    requirement for pypfant

fortranformat (python package) requirement for pypfant   pip install fortranformat
@endverbatim


@section tree Directory

Here is an incomplete listing of the directory tree.

@verbatim
%
├── add_paths.py                 utility to update PATH and PYTHONPATH
├── data                         courtesy data
│   └── sun                      complete case for the Sun
├── fortran
│   ├── make_linux.sh            Linux script to build the executable binaries
│   ├── make_windows.bat         Windows script to build the exe binaries
│   ├── bin                      PFANT executable binaries
│   │   ├── hydro2
│   │   ├── innewmarcs
│   │   ├── nulbad
│   │   └── pfant
│   └── src                      Fortran and documentation source files
└── pypfant                      Python layer
    ├── scripts                  command-line tools
    │   ├── plot_filetoh.py      plots hydrogen lines in 3D
    │   ├── plot_mod_record.py   plots atmospheric model
    │   ├── plot_mod_records.py  plots NEWMARCS grid in 3D
    │   ├── plot_spectrum.py     plots synthetic spectra
    │   └── run_4exes.py         combo-runs innewmarcs, hydro2, pfant, nulbad
    │
    └── pypfant         package to operate PFANT using Python
@endverbatim

@section datafiles Input/output data files

Here is a list of files used, and their default names.
@verbatim
Default name      | Description
------------------+-----------------------------------------------------------
*** star-specific files ***
main.dat          | main configuration file
abonds.dat        | abundances
dissoc.dat        | dissociation equilibrium data

*** "constant" data files ***
absoru2.dat       | absorbances ?doc?
atomgrade.dat     | atomic lines
moleculagrade.dat | molecular lines
partit.dat        | partition functions
hmap.dat          | hydrogen lines info (filename, lambda, na, nb, kiex, c1)
gridsmap.dat      | list of NEWMARCS grids
newnewm150.mod    | filenames listed in gridsmap.dat
newnewm100.mod    | "
newnewm050.mod    | "
newnewp000.mod    | "
newnewp025.mod    | "

*** created by innewmarcs ***
modeles.mod       | atmospheric model (binary file created by innewmarcs)

*** created by hydro2 ***
thalpha           | hydrogen lines files
thbeta            | "
thgamma           | "
thdelta           | "
thepsilon         | "

*** created by pfant ***
flux.norm         | normalized flux
flux.spec         | un-normalized flux (multiplied by 10**5)
flux.cont         | continuum flux (multiplied by 10**5)

*** created by nulbad ***
flux.norm.nulbad  | convolved flux
@endverbatim

THe workflow can be summarized in the following diagram.

Note that the running sequence is innermarcs, hydro2, pfant, nulbad

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

@section running Running innewmarcs, hydro2, pfant, nulbad

@subsection building Building the binaries

If you have gfortran installed, go to directory %/fortran and type

@code
$ ./make_linux.sh
@endcode

or

@code
> make_windows.bat
@endcode

This should create four executables in the bin directory.

Also, individual makefiles are available for each (platform)x(executable),
for example, the following will compile pfant on Linux:

@code
$ make -f makefile_linux_pfant
@endcode

@subsection test_bin Running with shipped data

Now create a new directory somewhere *out of the "%" directory* and copy the whole
contents of the %/data/sun

@par Important
Don't run commands inside %/data/sun. This is part of the repository and should be
kept clean.

If everything is alright so far, you should be able to successfully run the following
commands:

@code
$ innewmarcs
$ hydro2 --hmap True
$ pfant --hmap True
$ nulbad
@endcode

Looks good. For a full set of command-line options, use the "--help" option with
any of the executables, for example:

@code
$ pfant --help
$ innewmarcs --help
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

@section more_python Operation using Python

The four executables can be run in batch using this python script

@code
run_4exes.py
@endcode

For a list of available options, type:

@code
run_4exes.py --help
@endcode

-x-x-x-x-x

@verbatim
.
.
.
  to be continued ...

@endverbatim

*/

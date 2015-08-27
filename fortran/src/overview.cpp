/**

@page overview Overview

@section Installation

Clone from github

@code
git clone https://github.com/trevisanj/pfant
@endcode

or download <a href="https://github.com/trevisanj/pfant/archive/master.zip">this zip file</a>.

@subsection required Required software

PFANT is cross-platform.

Please make sure the following is installed on your computer.

@verbatim
What                          Why?
-------------------------------------------------------
gfortran and make             Compile the Fortran code
python 2.7                    run the Python scripts
matplotlib (python package)   plotting with pypfant
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
    ├── bin                      command-line tools
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
abonds.dat        | abondances
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
flux.norm         | normalized flux (note: multiplied by 10**5)
flux.spec         | un-normalized flux (note: multiplied by 10**5)
flux.cont         | continuum flux (note: multiplied by 10**5)

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

@section paths Paths to executables and libraries

If you run on Linux and the bash shell, you can run the script "add_paths.py"
to update your PATH and PYTHONPATH variables. Otherwise, environment variables
should be set as follows:

@verbatim
% - directory created by "installation"

environment variable     what to add
-----------------------------------------
PATH                     %/fortran/bin
PATH                     %/pypfant/bin
PYTHONPATH               %/pypfant
@endverbatim

Here is an example of add_paths.py running

@code
$ ./add_paths.py
Adding > export PYTHONPATH="${PYTHONPATH}:/home/j/Documents/projects/pfant/working/github/master/pypfant"
Adding > export PATH="${PATH}:/home/j/Documents/projects/pfant/working/github/master/pypfant/bin"
Adding > export PATH="${PATH}:/home/j/Documents/projects/pfant/working/github/master/fortran/bin"
File '/home/j/.bashrc' now contains:
--- BEGIN ---
# added by add_paths.py (PFANT)
export PYTHONPATH="${PYTHONPATH}:/home/j/Documents/projects/pfant/working/github/master/pypfant"
# added by add_paths.py (PFANT)
export PATH="${PATH}:/home/j/Documents/projects/pfant/working/github/master/pypfant/bin"
# added by add_paths.py (PFANT)
export PATH="${PATH}:/home/j/Documents/projects/pfant/working/github/master/fortran/bin"
--- END ---
running > export PYTHONPATH="${PYTHONPATH}:/home/j/Documents/projects/pfant/working/github/master/pypfant"
running > export PATH="${PATH}:/home/j/Documents/projects/pfant/working/github/master/pypfant/bin"
running > export PATH="${PATH}:/home/j/Documents/projects/pfant/working/github/master/fortran/bin"
Running another bash
@endcode

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

+-------------------->
@endverbatim

*/


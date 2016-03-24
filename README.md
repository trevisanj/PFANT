# PFANT

In the *main configuration file* there are values.

In the `main configuration file` there are values.

In the _main configuration file_ there are values.

In the **main configuration file** there are values.

In the < main configuration file > there are values.

In the <main configuration file> there are values.

In the 'main configuration file' there are values.


## Welcome

PFANT is a spectral synthesis software written in Fortran for use in Astrophysics.

Analogue softwares include TurboSpectrum and MOOG.

Other README pages:
  - [Fortran source code overview](fortran/README.md)
  - [`pyfant` Python package overview](pyfant/README.md) 

### History

```
 |
 | 1967 -- FANTÔME -- by F Spite et al.
 | 1982 -- FANTOMOL -- B Barbuy included the computation of molecular lines,
 |         dissociatipon equilibrium.
 | 2003 -- PFANT -- M-N Perrin: large wavelength coverage,
 |         inclusion or hydrogen lines.
 | 2015 -- J Trevisan: conversion of source code to Fortran 2003, Python layer.
t|
 V
```

## Installation

PFANT is cross-platform, and all features have been tested on Windows and Linux.

To use PFANT, you will need to:

1. Install the software pre-requisites
2. Go to https://github.com/trevisanj/PFANT/releases and download the most recent release
3. Compile the Fortran source code
4. Add `PFANT/fortran/bin` and `PFANT/pyfant/scripts` to your PATH
5. Add `PFANT/pyfant` to your PYTHONPATH

### Installing required software

Depending on your OS platform, you may have some of the following softwares installed already.

The install recommendations are based on successful attempts.

#### Applications

What  | Why?
----- | ----
gfortran >= 4.6, make | compile the Fortran code
Python 2.7 | use pyfant resources
pip | install some of the required Python packages

#### Python packages

Some successful ways to install the Python packages are reported together with
package names, but the exact way to install these packages will depend on
your system. In general, they should be all easy to install.

Package name | Recommended way to install
--- | ---
matplotlib | apt-Linux `sudo apt-get install python-matplotlib`
pyqt4 | apt-Linux `sudo apt-get install python-qt4`
      | Windows: download Python 2.7 installer at https://riverbankcomputing.com/software/pyqt/download
fortranformat | All systems: `pip install fortranformat`
astropy | apt-Linux: `sudo apt-get install python-astropy`
        | All systems: `pip install astropy`
                  
                 
(not yet) mayavi2           apt-Linux: `sudo apt-get install mayavi2`

**Note:** When running `pip` on Linux, you may have to run it with `sudo`.

#### Windows compiler

If you are using Windows, a possible way to compile the Fortran source code is
to install MinGW (http://sourceforge.net/projects/mingw/files/).

After installed, MinGW has its own package manager, named
"MinGW Installation Manager". There, you will need to install at least the following packages:
`mingw-developer-toolkit`, `mingw32-base`, `mingw32-gcc-fortran`, `msys-base`.


### To clone the github repository

The following command will create a directory named "PFANT"

```shell
git clone https://github.com/trevisanj/PFANT
```

### Overview of the PFANT directory tree

Here is an incomplete listing:

```
PFANT
├── fortran
│   ├── bin                      Fortran binaries
│   └── src                      Fortran source code
├── pyfant                       
│   ├── scripts                  Python command-line tools
│   └── pyfant                   Python package
└── data                         some data
    ├── arcturus                 Arcturus: only main.dat, abonds.dat, dissoc.dat
    └── sun                      Sun: complete set to run
```

### Compiling the Fortran source code.

The source code uses Fortran 2003 language features. gfortran >= 4.6 required (4.8 tested).

The code can be compiled using the CBFortran IDE, or typing

```shell
cd fortran
make_linux.sh     # Linux
make_windows.bat  # Windows
```

The executable binaries are found in PFANT/fortran/bin

### Setting the paths

Add `PFANT/fortran/bin` and `PFANT/pyfant/scripts` to your PATH.

Add `PFANT/pyfant` to your PYTHONPATH.


#### The script `PFANT/add-paths.py`

On Linux, you may try `PFANT/add-paths.py` to automatically apply the path settings to you login script:

```shell
./add-paths.py --tcsh  # tcsh shell only
./add-paths.py --bash  # bash shell only
```

## Quick start

Here is a sequence of shell commands from mounting a new "case" to plotting a synthetic spectrum:

```shell
mkdir mytest
cd mytest
copy-star.py sun-asplund-2009  # copies star-specific files to local directory
link-to-data.py common         # creates symbolic links to other (not star-specific) data files 
innewmarcs                     # creates "modeles.mod", an interpolated atmospheric model based on NEWMARCS (2005) model grids
hydro2                         # calculates the hydrogen lines profiles (in this case, file "thalpha")
pfant                          # spectral synthesis (will create "flux.spec", "flux.cont", "flux.norm")
nulbad --fwhm 0.12             # convolves the synthetic spectrum with a Gaussian function
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.12 
```

### Graphical user interface (new!)

Now you can set up a star, run the Fortran binaries, and plot results from a
graphical user interface (GUI):

```shell
x.py
```

- [See some screenshots](pyfant/screenshots.md) 


### All command-line tools

To get a realtime list of available command-line tools: 

```shell
pyfant-scripts.py
```

## More about the PFANT pipeline

### Input/output data files

The following table summarizes the files needed for spectral synthesis.

The first column of the table shows the command-line option that can be
used to change the default name for the corresponding file

```
--option          |  Default name     | Description
------------------------------------------------------------------------------
*** star-specific files ***
--fn_main         | main.dat          | main configuration file
--fn_abonds       | abonds.dat        | abundances
--fn_dissoc       | dissoc.dat        | dissociation equilibrium data

*** "constant" data files ***
--fn_absoru2      | absoru2.dat       | absorbances ?doc?
--fn_atoms        | atoms.dat         | atomic lines
--fn_molecules    | molecules.dat     | molecular lines
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
```

### PFANT pipeline

```
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
                          atoms.dat              |pfant|<----------+         |
                      molecules.dat+------------>+-----+<--------------------+
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
```

## Other topics

### Converting VALD3 extended-format atomic lines

The Vienna Atomic Line Database (VALD) (http://vald.astro.uu.se/) is a 
collection of atomic and molecular transition parameters of astronomical interest.

The ```pyfant``` package provides a tool to build an atomic lines file from a
 VALD3 extended-format file.

This is done in two steps. ```vald3-to-atoms.py``` does the actual conversion
(which is quick) and saves a file, _e.g._, ```atoms.dat```

The second step (which is time-consuming) is performed by ```tune-zinf.py``` and aims
to tune an important parameter used by the ```pfant``` Fortran binary.

It is recommended to use the tool ```cut-atoms.py``` to cut the file converted by
```vald3-to-atoms.py``` to a wavelength region of interest before running ```tune-zinf.py```.

For more information, see help for ```vald3-to-atoms.py```, ```tune-zinf.py```,
```cut-atoms.py``` (call these scripts with ```--help``` option).


# Other README pages
  - [Fortran source code overview](fortran/README.md)
  - [`pyfant` Python package overview](pyfant/README.md) 


```
-x-x-x-x-x
```

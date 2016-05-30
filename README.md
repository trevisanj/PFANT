```
PFANT
├── art                          Graphic material made in PowerPoint, GIMP etc.
├── fortran                      Fortran source code and binaries
├── pyfant                       Python package and scripts                        
└── data                         Some data
```

# PFANT

## 1 Welcome

PFANT is a spectral synthesis software written in Fortran for Astronomy.

Analogue softwares include TurboSpectrum and MOOG.

This guide provides a quick start with instructions on how to install and run the Fortran
binaries and Python scripts.

Further down it also provides more detailed information about PFANT "pipeline", _i.e._,
the steps involved to generate a synthetic spectrum.

### 1.1 History

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

## 2 Installation

PFANT is cross-platform, and all features have been tested on Windows and Linux.

To use PFANT, you will need to:

1. Install the software pre-requisites
2. Go to https://github.com/trevisanj/PFANT/releases and download the most recent release
3. Compile the Fortran source code
4. Add `PFANT/fortran/bin` and `PFANT/pyfant/scripts` to your PATH and add `PFANT/pyfant` to your PYTHONPATH

### 2.1 Installing required software

Depending on your OS platform, you may have some of these requisites installed already.

#### 2.1.1 Applications

What  | Why
----- | ----
gfortran >= 4.6, make | compile the Fortran code
Python 2.7 | use pyfant resources
pip | install some of the required Python packages

#### 2.1.2 Python packages

Some successful ways to install the Python packages are reported together with
package names, but the exact way to install these packages will depend on
your system. In general, they should be all easy to install.

Package name | Recommended way to install
--- | ---
matplotlib | apt-Linux: `sudo apt-get install python-matplotlib`
pyqt4 | apt-Linux: `sudo apt-get install python-qt4`
      | Windows: download Python 2.7 installer at https://riverbankcomputing.com/software/pyqt/download
fortranformat | All systems: `pip install fortranformat`
astropy | apt-Linux: `sudo apt-get install python-astropy`
        | All systems: `pip install astropy`
                  
                 
(not yet) mayavi2           apt-Linux: `sudo apt-get install mayavi2`

**Note:** When running `pip` on Linux, you may have to run it with `sudo`.

#### 2.1.3 Windows compiler

If you are using Windows, a possible way to compile the Fortran source code is
to install MinGW (http://sourceforge.net/projects/mingw/files/).

After installed, MinGW has its own package manager, named
"MinGW Installation Manager". There, you will need to install at least the following packages:
`mingw-developer-toolkit`, `mingw32-base`, `mingw32-gcc-fortran`, `msys-base`.


### 2.2 Downloading PFANT

There are two main ways to download PFANT:

  a) either go to https://github.com/trevisanj/PFANT/releases and download the most recent release, or

  b) clone the github repository:
     ```shell
     git clone https://github.com/trevisanj/PFANT
     ```
   

### 2.3 Compiling the Fortran source code.

The source code uses Fortran 2003 language features. gfortran >= 4.6 required (4.8 tested).

The code can be compiled using the CBFortran IDE, or typing

```shell
cd fortran
./make-linux.sh     # Linux
make-windows.bat    # Windows
```

The executable binaries are found in PFANT/fortran/bin

### 2.4 Setting the paths

Add `PFANT/fortran/bin` and `PFANT/pyfant/scripts` to your PATH.

Add `PFANT/pyfant` to your PYTHONPATH.


#### 2.4.1 The script `PFANT/add-paths.py`

On Linux, you may try `PFANT/add-paths.py` to automatically apply the path settings to you login script:

```shell
./add-paths.py --tcsh  # tcsh shell only
./add-paths.py --bash  # bash shell only
```

## 3 Quick start

There is a graphical user interface (GUI), but before using it,
let's run a command-line test sequence. 


### 3.1 Test sequence

First let's create a new directory and put some data in it: 

```shell
mkdir mystar
cd mystar
```

For convenience, this tutorial divides the data files in two types: star-specific and
star-independent.

We will create symbolic links to the star-independent files that ship with PFANT:

```
link.py common
```

Then we will copy the Sun star-specific files into the local directory:

```
copy-star.py sun-asplund-2009
```

Now your directory listing should be the following (the Pipeline section below has 
information about these files):
```
abonds.dat  absoru2.dat  atoms.dat  dissoc.dat  grid.mod  grid.moo  hmap.dat  main.dat  molecules.dat  partit.dat  python.log
```

Now run the spectral synthesis (this script is called "run4" because it runs four
Fortran programs in sequence)

```shell
run4.py --fwhm 0.12
```

Now visualize the synthetic spectra before and after the final convolution:

```shell
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.120 
```

Here is the complete test sequence:

```shell
mkdir mystar
cd mystar
link.py common                 # creates symbolic links to star-independent data files 
copy-star.py sun-asplund-2009  # copies star-specific data files to local directory
run4.py --fwhm 0.12            # runs Fortran binaries
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.120 
```

**Note** If any of these steps fails although you have all required software installed correctly,
please contact the authors.

### 3.2 Changing star properties

Now let's say we want to change a few properties of the star, say, its temperature and
its iron abundance. This will allow us t

### 3.2 Graphical User Interface (GUI)

#### ```x.py```

```x.py``` is a GUI that allows for mouse-click-based editing of the star parameters and 
command-line options, running spectral synthesis and visualization of the results. 

```shell
mkdir mystar
cd mystar
link.py common                 # creates symbolic links to star-independent data files 
copy-star.py sun-asplund-2009  # copies star-specific data files to local directory
run4.py --fwhm 0.12            # runs Fortran binaries
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.120 
```



```explorer.py``` 

- [See some screenshots](pyfant/screenshots.md) 


### Python tools

To get a realtime list of available Python tools: 

```shell
scripts.py
```

## More about the PFANT pipeline

The core of calculations is made by four Fortran programs:
  1) *innewmarcs*: interpolates an atmospheric model given a grid of MARCS models
  2) *hydro2*: created the hydrogen lines profiles
  3) *pfant*: calculates the synthetic spectrum
  4) *nulbad*: convolves the synthetic spectrum with a Gaussian function


The following diagram depicts the PFANT pipeline with the Fortran programs
and the input/output files used/generated by these programs. 

```
                    +---------------------------+---------------------main.dat
                    |                           |                        |
                    v                           v                        |
 grid.mod     +----------+                  +------+                     |
       or +-->|innewmarcs|-->modeles.mod+-->|hydro2|<----------+         |
 grid.moo     +----------+         +        +------+           |         |
                                   |            |              |         |
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
                                         flux.norm.nulbad.<fwhm>
```

### Input/output data files

Most files can be found in `PFANT/data/common` and linked to using `link.py`
(see [the common data README](data/common/README.md) for more information).

Some star-specific files are also shipped with PFANT: use script `copy-star.py`
for convenience.

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
--fn_absoru2      | absoru2.dat       | absorption info for continuum calculation
--fn_atoms        | atoms.dat         | atomic lines
--fn_molecules    | molecules.dat     | molecular lines
--fn_partit       | partit.dat        | partition functions
                  | hmap.dat          | hydrogen lines info (filename, lambda, na, nb, kiex, c1)
                  | grid.mod or       | MARCS atmospheric model grid (models only)
                  | grid.moo          | MARCS atmospheric model grid (models with opacities)

** created by innewmarcs **
--fn_modeles      | modeles.mod       | atmospheric model (binary file)
--fn_opa          | opa.dat           | atmospheric model: opacities

** created by hydro2 **
                  | thalpha           | hydrogen lines files
                  | thbeta            | "
                  | thgamma           | "
                  | thdelta           | "
                  | thepsilon         | "

** created by pfant **
flux.norm         | normalized flux
flux.spec         | un-normalized flux (multiplied by 10**5)
flux.cont         | continuum flux (multiplied by 10**5)

** created by nulbad ** 
flux.norm.nulbad  | convolved flux
```

## Other topics


### Adding MARCS opacities to the continuum (experimental!)

PFANT can use continuum opacities from the MARCS website 
(http://marcs.astro.uu.se/). To switch this on this behavior, run the Fortran binaries
with the `--no_opa F` option:

```shell
run4.py --no_opa F
```


**Note** please download file `PFANT/data/common/grid.moo` from [this location]
(https://drive.google.com/file/d/0B8m8GNLFiaewY0J1YzRrbHBCbWs/view?usp=sharing),
as this file has more than 100 MB and therefore cannot be stored in github.
This file contains a compiled atmospheric model grid
(see [the common data README](data/common/README.md) for more information).



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


# Other pages
  - [Fortran source code overview](fortran/README.md) (for Fortran developers
    interested in changing the source code)
  - [`pyfant` Python package overview](pyfant/README.md) 


```
-x-x-x-x-x
```

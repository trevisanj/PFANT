```
This directory

PFANT
├── art                     Graphic material made in PowerPoint, GIMP etc.
├── fortran                 Fortran source code and binaries
├── pyfant                  Python package and scripts                        
└── data                    Some stellar data, spectral lines etc.
```

# Welcome

# PFANT Manual

# Table of contents

  - [Introduction](#Introduction)
  - [Installation](#Installation)
  - Quick start guide
  - Pipeline
  - Other topics

# Introduction




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

# 2 Installation

PFANT is cross-platform, and all features have been tested on Windows and Linux.

To use PFANT, you will need to:

1. Install the software pre-requisites
2. Go to https://github.com/trevisanj/PFANT/releases and download the most recent release
3. Compile the Fortran source code
4. Add `PFANT/fortran/bin` and `PFANT/pyfant/scripts` to your PATH and add `PFANT/pyfant` to your PYTHONPATH

This section will take you through these steps.

### 2.1 Installing required software

Depending on your OS platform, you may have some of these installed already.

#### 2.1.1 Standalone applications

  - gfortran >= 4.6
  - make
  - Python 2.7
  - pip

##### 2.1.1.1 Windows users: gfortran and make on Windows

MinGW (http://sourceforge.net/projects/mingw/files/) is a convenient way to install the GNU Fortran compiler on Windows.

After installed, MinGW has its own package manager, named
"MinGW Installation Manager". There, you will need to install at least the following packages:
`mingw-developer-toolkit`, `mingw32-base`, `mingw32-gcc-fortran`, `msys-base`.

#### 2.1.2 Python packages

Package name | Possible way to install
--- | ---
matplotlib | apt-Linux: `sudo apt-get install python-matplotlib`
pyqt4 | apt-Linux: `sudo apt-get install python-qt4`
      | Windows: download Python 2.7 installer at https://riverbankcomputing.com/software/pyqt/download
fortranformat | All systems: `[sudo] pip install fortranformat`
astropy | apt-Linux: `sudo apt-get install python-astropy`
        | All systems: `[sudo] pip install astropy`

**Note:** running `pip` on Linux, you may have to do it with `sudo`.



### 2.2 Downloading PFANT

There are two main ways to download PFANT:

  a) either go to https://github.com/trevisanj/PFANT/releases and download the most recent release, or

  b) clone the github repository:
     ```shell
     git clone https://github.com/trevisanj/PFANT
     ```

In either case, there should be a directory named PFANT on your drive.

### 2.3 Compiling the Fortran source code.

The source code uses Fortran 2003 language features. gfortran >= 4.6 required (4.8 tested).

#### 2.3.1 Linux users

The code can be compiled using the CBFortran IDE, or using the console:

```shell
cd PFANT
cd fortran
./make-linux.sh
```

#### 2.3.1 Windows users

The code can be compiled using the CBFortran IDE.

# TODO link to other document

### 2.4 Setting the paths

Add `PFANT/fortran/bin` and `PFANT/pyfant/scripts` to your PATH.

Add `PFANT/pyfant` to your PYTHONPATH.

#### 2.4.1 Linux users: the script `PFANT/add-paths.py`

On Linux, you may try `PFANT/add-paths.py` to automatically apply the path settings to you login script:

```shell
./add-paths.py --tcsh  # tcsh shell only
./add-paths.py --bash  # bash shell only
```

## 3 Quick start

There is a graphical user interface (GUI), but before using it,
let's run a command-line test sequence. 


### 3.1 Getting the data together 

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

Now your directory listing should be the following (for more information on these files,
refer to Section 4):
```
abonds.dat  absoru2.dat  atoms.dat  dissoc.dat  grid.mod  grid.moo  hmap.dat  main.dat  molecules.dat  partit.dat  python.log
```

### 3.2 Command-line operation (1)

Run the spectral synthesis (this script is called "run4" because it runs four
Fortran programs in sequence).

#### 3.2.1 Interpolate the stellar atmospheric model

```shell
innewmarcs
```

This will create two files: `modeles.mod` and `opa.dat`. 

# TODO change default name to abs-sca.opa or sth.

#### 3.2.2 Create hydrogen lines profiles

```shell
hydro2
```

This will create files such as : `thalpha`, `thbeta`, `thgamma` etc.


#### 3.2.3 Spectral synthesis

```shell
pfant
```

This will create files such as: `flux.norm`, `flux.spec`, `flux.cont`.


#### 3.2.4 Post-synthesis convolution with Gaussian function

```shell
nulbad --fwhm 0.12
```

This will create a file such as `flux.norm.nulbad.0.120`

#### 3.2.5 Visualize results

```shell
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.120 
```

This will open a window containing two overlapped spectra (before and after convolution).

### 3.3 Command-line operation (1)

Run the spectral synthesis (this script is called "run4" because it runs four
Fortran programs in sequence).

#### 3.3.1 Run steps 3.2.1 - 3.2.4 at once

```shell
run4.py --fwhm 0.12
```

#### 3.3.2 Visualize results

As in 3.2.5:

```shell
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.120 
```

#### 3.4 Changing stellar parameters

# TODO insert main.dat, abonds.dat and dissoc.dat pictures here

# TODO Figure numbers

# TODO mu, ptdisk diagram

# TODO Why aint (diagram will help)

#### Changing running setup

# TODO insert llzero, llfin, aint diagram

### 3.4 Graphical operation

#### 3.3.1 ```x.py```

```x.py```, the *PFANT launcher*, is a user-friendly interface where you can edit the stellar
parameters and command-line options and run spectral syntheses using multiple CPU cores.
It also has a "multi mode" which can be useful for adjusting of stellar
abundances. Here's a sequence of steps to get you started:

  1. in your ```mystar``` directory, invoke ```x.py```.
  2. Change the temperature (_teff_) to **6000**.
  3. Click on button _Submit single job_ (_Runnables Manager_ window opens).
  4. When the _Status_ column shows **nulbad finished**, double-click on the table item
     (_PFANT explorer_ window opens).
  5. Double-click on **flux.norm**. Note that it turns green.
  6. Double-click on _Plot spectrum_ (spectrum appears).
  
#### 3.3.2 ```explorer.py```

```explorer.py```, the *PFANT explorer*, is a file-explorer-like tool aimed at 
facilitating visualization and editing of data files that are relevant to the context.
It can open and visualize all the input/output data files in the PFANT pipeline
and provides some interesting graphic visualizations to these files. 

Please take some time to explore the resources in the _PFANT launcher_ and 
_PFANT explorer_ tool.

### 3.4 All Python programs

To print a list of all Python programs available (with descriptions), please type:  

```shell
scripts.py
```

**Note** For further information, all programs can be called with the `--help` option.

## 4 More about the PFANT pipeline

### 4.1 Overview

The core of calculations is made by four Fortran programs:
  1. *innewmarcs*: interpolates an atmospheric model given a grid of MARCS models
  2. *hydro2*: created the hydrogen lines profiles
  3. *pfant*: calculates the synthetic spectrum
  4. *nulbad*: convolves the synthetic spectrum with a Gaussian function

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

### 4.2 Input/output data files

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
** star-specific data files **
--fn_main         | main.dat          | main configuration file
--fn_abonds       | abonds.dat        | abundances
--fn_dissoc       | dissoc.dat        | dissociation equilibrium data

** star-independent data files **
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
flux.norm.
nulbad.<fwhm>     | convolved flux
```

## 5 Other topics

### 5.1 Adding MARCS opacities to the continuum (experimental!)

PFANT can now use continuum opacities from the MARCS website 
(http://marcs.astro.uu.se/).

To switch on this behavior, activate the `--opa` option.

To disable ```pfant``` internal calculation of absorption coefficients and use
only the coefficients from MARCS, deactivate the `--absoru` option.

```shell
run4.py --opa T --absoru F
```

If if you are using ```x.py```, look for `--opa` and `--absoru` in the _Command-line options_ tab.

**Important note** please download file `PFANT/data/common/grid.moo` from [this location]
(https://drive.google.com/file/d/0B8m8GNLFiaewY0J1YzRrbHBCbWs/view?usp=sharing),
as this file has more than 100 MB and therefore cannot be stored in github.
This file contains a compiled atmospheric model grid
(see [the common data README](data/common/README.md) for more information).

### 5.2 Converting VALD3 extended-format atomic lines

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


# 6 Other readings in this repository
  - [Fortran source code overview](fortran/README.md) (for Fortran developers
    interested in changing the source code)
  - [`pyfant` Python package overview](pyfant/README.md) 


```
-x-x-x-x-x
```

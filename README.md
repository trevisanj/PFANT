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

  1. [Introduction](#S1)
  2. [Installation](#S2)
  3. [Operation](#S3)
  4. [Pipeline](#S4)
  5. [Other topics](#S5)

## <a name=S1></a>1 Introduction

PFANT is a software stellar spectra written in Fortran.

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

# 2 <a name=S2></a>Installation

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



### 2.2 Download

There are two main ways to download PFANT:

  a) either go to https://github.com/trevisanj/PFANT/releases and download the most recent release, or

  b) clone the github repository:
     ```shell
     git clone https://github.com/trevisanj/PFANT
     ```

In either case, there should be a directory named PFANT on your drive.

There is an additional data file that needs to be downloaded from a different
location because it is too big to be stored on GitHub (this file contains a 3D grid of MARCS atmospheric models with opacities included).
Please download file `grid.moo` from [here]
(https://drive.google.com/file/d/0B8m8GNLFiaewY0J1YzRrbHBCbWs/view?usp=sharing)
and save it as `PFANT/data/common/grid.moo`.

### 2.3 Compiling the Fortran source code.

The source code has been successfully compiled using gfortran 4.8.

#### 2.3.1 Linux users

The code can be compiled using the [CBFortran IDE](fortran/README.md), or using the console:

```shell
cd PFANT
cd fortran
./make-linux.sh
```

#### 2.3.1 Windows users

The code can be compiled using the [CBFortran IDE](fortran/README.md).

### 2.4 Setting the paths

Add `PFANT/fortran/bin` and `PFANT/pyfant/scripts` to your PATH.

Add `PFANT/pyfant` to your PYTHONPATH.

#### 2.4.1 Linux users: bonus: `PFANT/add-paths.py` ...

... tries to automatically apply these path settings by modifying your `home/.bashrc` or `home/.cshrc`:

Bash shell:
```shell
./add-paths.py --bash
```

Tcsh shell:
```shell
./add-paths.py --tcsh
```

## <a name=S3></a>3 Operation

**Aims**
  - Calculate a synthetic spectrum;
  - convolve with Gaussian functions of different FWHMs;
  - visualize results.

### 3.1 Input data

Input data contists of:
  1. stellar parameters (temperature, chemical abundances etc.) and running settings (_e.g._, calculation wavelength interval);
  2. star-independent data: line lists, atmospheric model grid, partition functions etc. that are unlikely to be modified.

First let's create a new directory and put some data in it: 

```shell
mkdir mystar
cd mystar
copy-star.py sun-asplund-2009   # copies main.dat and abonds.dat from PFANT/data/sun-asplund-2009
link.py common                  # creates symbolic links to PFANT/data/common
```

#### 3.1.1 Stellar data and running settings

These are contained in two text files:

  - main.dat: main configuration. Modify using `mained.py`.
  - abonds.dat: chemical abundances. Modify using `abed.py`.
 
#### 3.2 Star-independent data

  - absoru2.dat
  - atoms.dat
  - grid.moo
  - hmap.dat
  - molecules.dat
  - partit.dat
  
To learn more about these files, please check [Section 4](#S4) or use `explorer.py`.
 
### 3.2 Command-line spectral synthesis (1)

# TODO AUTO-DISSOC IN PFANT

#### 3.2.1 Interpolate the stellar atmospheric model

```shell
innewmarcs
```

This will create two files: modeles.mod and opa.dat. 

# TODO change default name to abs-sca.opa or sth.

# TODO add graphics for modeles.mod and opa.dat

#### 3.2.2 Create hydrogen lines profiles

```shell
hydro2
```

This will create files such as : thalpha, thbeta, thgamma etc.

# TODO add thalpha 3D

#### 3.2.3 Spectral synthesis

```shell
pfant
```

Creates flux.norm, flux.spec, flux.cont.


#### 3.2.4 Post-synthesis convolution with Gaussian function

```shell
nulbad --fwhm 0.12
```

creates flux.norm.nulbad.0.120

```shell
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.120 
```

opens plot window.

Now let's convolve the spectrum with several Gaussians of different FWHMs.

```shell
nulbad --fwhm 0.06
nulbad --fwhm 0.08
nulbad --fwhm 0.10
nulbad --fwhm 0.12
nulbad --fwhm 0.14
```

creates files flux.norm.nulbad.0.060, flux.norm.nulbad.0.080 etc.

```shell
plot-spectra.py --ovl flux.norm.nulbad.0.060 flux.norm.nulbad.0.080 flux.norm.nulbad.0.100 flux.norm.nulbad.0.120 flux.norm.nulbad.0.140
```

opens plot window.

# TODO Add images of plot-spectra

### 3.2.6 Running the four calculation steps at once

```shell
run4.py --fwhm 0.12
```

#### 3.4 Changing stellar parameters

# TODO insert main.dat, abonds.dat and dissoc.dat pictures here

# TODO Figure numbers

# TODO mu, ptdisk diagram

# TODO Why aint (diagram will help)

#### Changing running setup

# TODO insert llzero, llfin, aint diagram

### 3.4 Graphical operation

#### 3.3.1 ```x.py```: PFANT launcher

`x.py` is a graphical user interface (GUI) that concentrates editing of stellar parameters
and running settings, spectral synthesis, and visualization.

  1. Starting again from scratch:

```shell
mkdir mystar
cd mystar
copy-star.py sun-asplund-2009
link.py common
```

then

```shell
x.py
```

  2. Take some time to explore Tabs 1, 2 and 3 (Alt+1, Alt+2, Alt+3). Tab 4 ("multi mode") will be explained later.

  3. Once you are done making changes, click on "Submit single job" button. A new window named "Runnables Manager" opens.

  4. When the "Status" column shows "**nulbad finished**", double-click on the table item
     ("PFANT explorer" window opens).

  5. Double-click on "flux.norm". Note that it turns green.

  6. Double-click on "Plot spectrum" (spectrum appears).


  
### 3.4 If you get lost ...

Yeah we agree that there is a lot of things to remember, but maybe not so many.

:bulb: All programs have a `-h` or `--help` option.

:bulb: Use `scripts.py` to get a list of all available Python programs.

:bulb: Use `explorer.py` to browse through/edit/visualize data files.
  
  
  
## 4 <a name=S4></a>Reference section

# TODO Add grid.moo scatterplot in section 4 actually

This is the boring section that enumerates exhaustively every category.

### 4.1 Complete pipeline

Figure TODO depicts the PFANT pipeline with the Fortran programs
and the input/output files used/generated by these programs. 

```
                    +---------------------------+---------------------main.dat
                    |                           |                        |
                    v                           v                        |
 grid.mod     +----------+                  +------+                     |
      or ---->|innewmarcs|-->modeles.mod--->|hydro2|<----------+         |
 grid.moo     +----------+         |        +------+           |         |
                                   |            |              |         |
                                   |            v              |         |
                                   |         thalpha           |         |
                                   |         thbeta        absoru2.dat   |
                                   |         thgamma       hmap.dat      |
                                   |         thdelta           |         |
                                   |         thepsilon         |         |
                                   |            |              |         |
                    abonds.dat     |            v              |         |
                    dissoc.dat     +-------->+-----+           |         |
                     atoms.dat               |pfant|<----------+         |
                 molecules.dat ------------->+-----+<--------------------+
                    partit.dat                  |                        |
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
Figure TODO - workflow showing data files (boxless) as inputs/outputs of the Fortran programs (boxed).

### 4.2 Input/output data files

All file types described in this section are supported by `explorer.py` with varying visualization/editing abilities.

#### 4.2.1 Stellar data and running settings


 Default name     | --option          | Description                    
------------------------------------------------------------------------------------------
main.dat          | --fn_main         | main configuration.
 | | Edit using any text editor, `mained.py`, `explorer.py`, or `x.py`
 | | See Figure TODO
abonds.dat        | --fn_abonds       | chemical abundances.
 | | Edit using any text editor, `abed.py`, `explorer.py`, or `x.py`
| | See Figure TODO
dissoc.dat        | --fn_dissoc       | dissociation equilibrium data.
 | | This file is optional and can be created using `abed.py` if needed.
 | | Edit using any text editor.

![](art/small-main.dat.png)

 
#### 4.2.2 Star-independent data files

 Default name     | --option          | Description                    
------------------------------------------------------------------------------------------
absoru2.dat       | --fn_absoru2      | absorption info for continuum calculation.
                  |                   | Edit using any text editor
atoms.dat         | --fn_atoms        | atomic line list
 | | Edit using any text editor, `ated.py`, or `explorer.py`
molecules.dat     | --fn_molecules    | molecular line list
 | | Edit using any text editor, `mled.py`, or `explorer.py`
hmap.dat          |                   | hydrogen line list.
 | | Edit using any text editor
partit.dat        | --fn_partit       | partition functions.
 | | Edit using any text editor
grid.mod or       |                   | MARCS atmospheric model grid (models only)
 | | Created using `create-grid.py` from a bulk of models downloaded from the MARCS website
 | | Visualize using `explorer.py`
grid.moo          |                   | MARCS atmospheric model grid (models with opacities)
 | | Created using `create-grid.py` from a bulk of models downloaded from the MARCS website
 | | Visualize using `explorer.py`

#### 4.2.3 Files created by the Fortran programs

##### 4.2.3.1 Files created by `innewmarcs`

 Default name     | --option          | Description                    
------------------------------------------------------------------------------------------
modeles.mod       | --fn_modeles      | atmospheric model (binary file)
opa.dat           | --fn_opa          | atmospheric model: opacities (MARCS ".opa" format)

##### 4.2.3.2 Files created by `hydro2`

 Default name     | --option          | Description                    
------------------------------------------------------------------------------------------
thalpha           |                   | hydrogen lines files
thbeta            |                   | "
thgamma           |                   | "
thdelta           |                   | "
thepsilon         |                   | "

##### 4.2.3.2 Files created by `pfant`

 Default name     | --option          | Description                    
------------------------------------------------------------------------------------------
flux.norm         | normalized flux
flux.spec         | un-normalized flux (multiplied by 10**5)
flux.cont         | continuum flux (multiplied by 10**5)

##### 4.2.3.2 Files created by `nulbad`

 Default name           | --option          | Description                    
------------------------------------------------------------------------------------------
flux.norm.nulbad.<fwhm> | --fn_cv           | convolved flux


## 5 <a name=S5></a> Other topics

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

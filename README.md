```
This directory

PFANT
├── fortran . Fortran source code and binaries
├── data .... Some stellar data, spectral lines etc.
└── doc ..... Graphic material made in PowerPoint, GIMP etc.
```

# Welcome

# PFANT Manual

# Table of contents

  1. [Introduction](#S1)
  2. [Installation](#S2)
  3. [Quick start](#S3)
  4. [Reference](#S4)
  5. [Other topics](#S5)
  6. [Links](#S6)
  7. [Troubleshooting](#S7)
  8. [References](#S8)

# <a name=S1></a>1 Introduction

PFANT is a stellar spectral synthesis software written in Fortran.

The development started with M Spite _et al._ in France in 1967 (Figure 1). The code formulation is described in [(Barbuy 1982)](#R_BLB1982), [(Cayrel et al. 1991)](#R_CAYREL1991), [(Barbuy et al. 2003)](#R_BLB2003), and [(Coelho et al. 2005)](#R_PC2005).

```
 |
 | 1967 -- FANTÔME -- by M Spite et al.
 | 1982 -- FANTOMOL -- B Barbuy included the computation of molecular lines,
 |           dissociatipon equilibrium
 | 2003 -- PFANT -- M-N Perrin: large wavelength coverage,
 |           inclusion or hydrogen lines
 | 2015 -- J Trevisan: conversion of source code to Fortran 2003, Python interface, 
 !           tools to convert between different file formats
t|
 V
```
Figure 1 - PFANT timeline [(Coelho et al. 2005)](#RPC_2005)

# 2 <a name=S2></a>Installation

To use PFANT, you will need to:

1. Download files
2. Compile the Fortran source code
3. Add `PFANT/fortran/bin` to your PATH
4. Install the "f311" Python package (http://github.com/trevisanj/f311) (recommended)

This section will take you through these steps.

:notes: PFANT is platform-independent (it should work no any system if you can install the GNU Fortran Compiler),
however only Debian-based Linux system is "supported" in the following instructions. Windows users will find some tips
in a specific section below.


## 2.1 Installing required software

### 2.1.1 Standalone applications

Please install the following standalone applications on your system (no pain except for gfortran and make on Windows (see below)):
  - gfortran (version 4.8 recommended; does **not** compile with version 4.4)
  - make


## 2.1 Download files

### 2.1.1 Clone the GitHub repository:

```shell
git clone https://github.com/trevisanj/PFANT
```

This will create a directory named PFANT on your disk.

## 2.2 Compiling the Fortran source code.

Enter the following on your console to compile the Fortran source code:

```shell
cd PFANT
cd fortran
./make-linux.sh
```

This should create four executable binaries inside the directory _PFANT/fortran/bin_: 
`innewmarcs`, `hydro2`, `pfant`, `nulbad`.

## 2.3 Setting the paths

Depending on which shell your system uses, try one of the following:

Bash shell:
```shell
./add-path.py --bash
```

Tcsh shell:
```shell
./add-path.py --tcsh
```

This will automatically apply the path settings to your _home/.bashrc_ or _home/.cshrc_.


:notes: If the above does not work for you, manually add _PFANT/fortran/bin_ to your system path.

## 2.4 Install _f311_ Python package

Please follow instructions at http://github.com/trevisanj/f311

## 2.5 Tips for windows users

### 2.5.1 gfortran and make on Windows

MinGW (http://sourceforge.net/projects/mingw/files/) is a convenient way to install the GNU Fortran compiler on Windows.

After installed, MinGW has its own package manager, named
"MinGW Installation Manager". There, you will need to install at least the following packages:
`mingw-developer-toolkit`, `mingw32-base`, `mingw32-gcc-fortran`, `msys-base`.


### 2.5.2 Compiling the source code on Windows

The source can be compiled using the CodeBlock Fortran IDE. For more information,
please visit [the Fortran source code README](fortran/README.md). The _PFANT/fortran_ forder contains a CodeBlocks
project named _PFANT-windows.cbp_


# <a name=S3></a>3 Quickstart

## 3.1 Command-line operation

**Aims for this tutorial:**
  - calculate a synthetic spectrum;
  - convolve with Gaussian functions of varying full-width-at-half-maximum (FWHM);
  - visualize results.

### 3.1.1 Short story

Here is the full command sequence:

```shell
mkdir mystar
cd mystar
copy-star.py
link.py
run4.py
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.120 
```

:notes: If you choose star "Mu-Leo", run `run4.py --allow true` to bypass the fact that its
metallicity if outside the atmospheric model grid provided

### 3.1.2 Long story

#### 3.1.2.1 Create a new directory

```shell
mkdir mystar
cd mystar
```

#### 3.1.2.2 Gather input data

Input data consists of:
  1. stellar parameters (temperature, chemical abundances etc.) and running settings
     (_e.g._, calculation wavelength interval);
  2. star-independent physical data: line lists, atmospheric model grid, partition
     functions etc. that are less likely to be modified.
     We refer to these as "common" data.

##### 3.1.2.2.1 Stellar data and running settings

The following displays a menu allowing you to choose among a few stars:

```shell
copy-star.py
```

After running this, the following files will be copied into the _mystar_ directory:
  - *main.dat*: main configuration (editable with `mained.py`, `x.py`)
  - *abonds.dat*: chemical abundances (editable with `abed.py`, `x.py`)
  
##### 3.1.2.2.2 Common data

For these data, we will create links instead of copying the files, as the files are big 
and/or unlikely to change:

```shell
link.py
```

The following links that should appear in your directory now:
  - *absoru2.dat*
  - *atoms.dat*
  - *grid.moo*
  - *hmap.dat*
  - *molecules.dat*
  - *partit.dat*
  
 
#### 3.1.2.3 Spectral synthesis pipeline

Spectral synthesis involves a few steps,
as shown Figure 2,
and described in the next subsections.

```
+-------------------+   +----------------+   +-----------+   +----------+
| innewmarcs        |   | hydro2         |   | pfant     |   | nulbad   |
| ==========        |   | ======         |   | =====     |   | ======   |
|   interpolate the |   |         create |   | calculate |   | convolve |
|       atmospheric |-->| hydrogen lines |-->| synthetic |-->|     with |
|             model |   |       profiles |   |  spectrum |   | Gaussian |
+-------------------+   +----------------+   +-----------+   +----------+
```
Figure 2 - PFANT spectral synthesis pipeline showing the Fortran program names
and what they do.

##### 3.1.2.3.1 Interpolate the stellar atmospheric model

This step takes a 3D grid of atmospheric models (usually a file named _grid.mod_) and interpolates a new model
given a certain point (temperature, gravity, metallicity) (specified in the main configuration file) contained within the
limits of the grid.

```shell
innewmarcs
```

will create two files: _modeles.mod_ and _modeles.opa_. 

:notes: If the combination of (temperature, gravity, metallicity) is outside the limits of the grid, `innewmarcs` will refuse to interpolate. However, it can be forced to use the nearest points in the grid with command-line option `--allow T`.

##### 3.1.2.3.2 Create hydrogen lines profiles

```shell
hydro2
```

will create files such as: _thalpha_ (Figure 8), _thbeta_, _thgamma_ etc.

##### 3.1.2.3.3 Calculate synthetic spectrum

```shell
pfant
```

creates files _flux.norm_, _flux.spec_, _flux.cont_,
respectively: normalized, un-normalized, continuum spectrum.

To visualize these files:

```shell
plot-spectra.py flux.spec flux.cont flux.norm
```

will open a plot window (Figure 3).

![](img/spec-cont-norm0.png)

Figure 3 -- plots of three files generated by `pfant`

##### 3.1.2.3.4 Convolve synthetic spectrum with Gaussian function

The following will take the normalized spectrum from the previous step and convolve it
with a Gaussian function of FWHM=0.12 :

```shell
nulbad
```

creates file _flux.norm.nulbad.0.120_

:bulb: You can change the FWHM using option `--fwhm`


##### 3.1.2.3.5 Plot spectra


```shell
plot-spectra.py --ovl flux.norm flux.norm.nulbad.0.120 
```

opens a plot window where one can see how the spectrum looks before and after the convolution (Figure 4).

![](img/normfwhm.png)

Figure 4 -- plot comparing spectra without and after convolution with Gaussian function (FWHM=0.12).


##### 3.1.2.3.6 Running the four calculation steps at once

The script `run4.py` is provided for convenience. 
The following is almost equivalent to steps 3.1.2.3.1 - 3.1.2.3.4:

```shell
run4.py --fwhm 0.12
```

#### 3.1.3 Where you can find more information

:book: **Description of stellar parameters, running settings, command-line options:** run `x.py`
and navigate through the fields in Tabs 1 and 3. As you navigate, a description for
the current field will be displayed at the bottom of the window.

:book: Call a program with "--help" option, _e.g._, `pfant --help`.

:book: **other _README.md_ files** can be found in other PFANT subdirectories.

:book: `programs.py` lists all Fortran/Python programs.
 
:book: The Fortran source code

TODO :book: Also check the pyfant tutorial(s) at http://github.com/trevisanj/pyfant


## 3.2 <a name=S4></a>Graphical interface operation

## 3.2.1 Spectral Synthesis from Scratch

Shell commands:
```shell
mkdir mystar
cd mystar
copy-star.py
link.py common
x.py
```

The last command will invoke the PFANT Launcher (Figure 5): 

  1. Change parameters in Tab 1/2/3 (Tab 4 is a different story) 
  2. Click on the "Submit single job" button: a new window named "Runnables Manager" opens
  3. When the "Status" column shows "nulbad finished", double-click on the table item: "PFANT Explorer" window opens
  4. Double-click on "flux.norm": turns green (if wasn't so)
  5. Double-click on "Plot spectrum": spectrum appears

![](img/x.py-0.png)

![](img/x.py-1.png)

![](img/x.py-2.png)

![](img/x.py-3.png)

Figure 5 -- Screenshots of the `x.py` application

## 3.2.2 Browse files with _F311 Explorer_

```shell
explorer.py
```

![](img/explorer.png)

This application allows you to navigate through your file system and visualize/edit files,
depending on their type. A list with all supported file types is available [here](...)

You can select several spectral files and plot them all at once (stacked in different sub-plots,
or overlapped in a single plot).

## 3.2.3 Edit Atomic Lines file

First make a copy of file "atoms.dat" to leave the current one untouched.


```shell
copy atoms.dat atoms2.dat
```
  
Now open the Atomic Lines Editor

```shell
ated.py atoms2.dat
```

![](img/ated.png)


## 3.2.3 Edit Molecular Lines file

First make a copy of file "molecules.dat" to leave the current one untouched.


```shell
copy molecules.dat molecules2.dat
```
  
Now open the Molecular Lines Editor

```shell
mled.py molecules2.dat
```

![](img/mled.png)

  

## 3.2.10 List all applications avaiable from the _f311_ Python project

Most of the applications from project _f311_ were created to target needs from PFANT users or 
people working with spectral synthesis. Here is a list of these applications obtained using 
```programs.py``` in 16/Feb/2017:

```shell

$ programs.py

Package 'aosss'
===============

  Graphical applications
  ----------------------

    wavelength-chart.py ......... Draws chart showing spectral lines of
                                  interest, spectrograph wavelength ranges, ESO
                                  atmospheric model, etc.

  Command-line tools
  ------------------

    create-simulation-reports.py  Creates HTML reports from WebSim-COMPASS
                                  output files
    create-spectrum-lists.py .... Create several .splist (spectrum list) files
                                  from WebSim-COMPASS output files; groups
                                  spectra that share same wavelength vector
    get-compass.py .............. Downloads WebSim-COMPASS simulations
    list-mosaic-modes.py ........ Lists MOSAIC Spectrograph modes
    organize-directory.py ....... Organizes simulation directory (creates
                                  folders, moves files, creates 'index.html')

Package 'convmol'
=================

  Graphical applications
  ----------------------

    convmol.py ........ Conversion of molecular lines data to PFANT format

  Command-line tools
  ------------------

    download-hitran.py  Downloads molecular lines from HITRAN database
    print-nist.py ..... Downloads and prints molecular constants from NIST Web
                        Book for a particular molecule

Package 'explorer'
==================

  Graphical applications
  ----------------------

    abed.py .......... Abundances file editor
    ated.py .......... Atomic lines file editor
    cubeed.py ........ Data Cube Editor, import/export WebSim-COMPASS data cubes
    explorer.py ...... F311 Explorer --  list, visualize, and edit data files
                       (_à la_ File Manager)
    mained.py ........ Main configuration file editor.
    mled.py .......... Molecular lines file editor.
    splisted.py ...... Spectrum List Editor
    tune-zinf.py ..... Tunes the "zinf" parameter for each atomic line in atomic
                       lines file

  Command-line tools
  ------------------

    create-grid.py ... Merges several atmospheric models into a single file
                       (_i.e._, the "grid")
    cut-atoms.py ..... Cuts atomic lines file to wavelength interval specified
    cut-molecules.py . Cuts molecular lines file to wavelength interval
                       specified
    cut-spectrum.py .. Cuts spectrum file to wavelength interval specified
    plot-spectra.py .. Plots spectra on screen or creates PDF file
    vald3-to-atoms.py  Converts VALD3 atomic/molecular lines file to PFANT
                       atomic lines file.

Package 'pyfant'
================

  Graphical applications
  ----------------------

    x.py ........ PFANT Launcher -- Graphical Interface for Spectral Synthesis

  Command-line tools
  ------------------

    copy-star.py  Copies stellar data files (such as main.dat, abonds.dat,
                  dissoc.dat) to local directory
    link.py ..... Creates symbolic links to PFANT data files as an alternative
                  to copying these (sometimes large) files into local directory
    run4.py ..... Runs the four Fortran binaries in sequence: `innewmarcs`,
                  `hydro2`, `pfant`, `nulbad`
    save-pdf.py . Looks for files "*.norm" inside directories session-* and
                  saves one figure per page in a PDF file

PFANT Fortran binaries
======================

    innewmarcs  found
    hydro2 .... found
    pfant ..... found
    nulbad .... found
```

# 3.3 <a name=S5>Writing Python scripts with _pyfant_ package

## 3.3.1 Running innewmarcs, hydro2, pfant, nulbad in sequence & plotting spectra

```python
import f311.pyfant as pf
import f311.explorer as ex
obj = pf.Combo()
obj.run()
obj.load_result()

# Plots continuum, spectrum, normalized in three sub-plots
ex.plot_spectra([obj.result["cont"], obj.result["spec"], obj.result["norm"]])

# Plots normalized unconvolved, normalized convolved spectra overlapped
ex.plot_spectra_overlapped([obj.result["norm"], obj.result["convolved"]])
```

![](img/pyfant-example-00.png)


  
# 4 <a name=S4></a>Reference section

This section contains a more complete description of the PFANT pipeline and the files and file types involved.

## 4.1 Spectral synthesis pipeline

```
                    +---------------------------+---------------------main.dat
                    |                           |                        |
                    v                           v  (H lines)             |
 grid.mod     +----------+                  +------+                     |
      or ---->|innewmarcs|-->modeles.mod--->|hydro2|<----------+         |
 grid.moo     +----------+         |        +------+           |         |
              (interpolate         |            |              |         |
               atm. model)         |            v              |         |
                                   |         thalpha           |         |
                                   |         thbeta        absoru2.dat   |
                                   |         thgamma       hmap.dat      |
                                   |         thdelta           |         |
                                   |         thepsilon         |         |
                                   |            |              |         |
                    abonds.dat     |            v              |         |
                    dissoc.dat     +-------->+-----+           |         |
                     atoms.dat    (synthesis)|pfant|<----------+         |
                 molecules.dat ------------->+-----+<--------------------+
                    partit.dat                  |                        |
                                                v                        |
                                            flux.norm                    |
                                            flux.spec                    |
                                            flux.cont                    |
                                                |                        |
                                                v                        |
                                             +------+                    |
                         (convolve spectrum) |nulbad|<-------------------+
                                             +------+
                                                |
                                                v
                                         flux.norm.nulbad.<fwhm>
```
Figure 5 - Spectral synthesis pipeline - Fortran programs (boxes) and their 
input/output files (see Figure 2 for extended description of modules).

## 4.2 Input/output data files

The different file types in the pipeline will be explained in the next subsections.
 
### 4.2.1 Stellar data and running settings

Table 2 -- stellar and running settings data files. The "--options" column shows the
command-line options that can be used to change the name for a particular file, _e.g._,
`run4.py --fn_main main-other.dat`.

 Default name     | --option    | Description                    
------------------|-------------|----------------------------------------------------
_main.dat_        | --fn_main   | main configuration file containing all stellar parameters except abundances (Figure 6).
_abonds.dat_      | --fn_abonds | chemical abundances
_dissoc.dat_      | --fn_dissoc | dissociation equilibrium data. This file is optional, and can be created using `abed.py` if needed.

![](img/small-main.dat.png)

Figure 6 - representaion of a file _main.dat_. The black color shows the characters
that are the actual parts of the file. Descriptions of parameters can be found in `x.py` graphical interface


### 4.2.2 Common data files

Table 3 -- Common data files.

 Default name     | --option       | Description                    
------------------|----------------|---------------------------------------------------
_absoru2.dat_     | --fn_absoru2   | absorption info for continuum calculation.
_atoms.dat_       | --fn_atoms     | atomic line list
_molecules.dat_   | --fn_molecules | molecular line list
_hmap.dat_        | --fn_hmap      | hydrogen line list
_partit.dat_      | --fn_partit    | partition functions
_grid.mod_ or     | --fn_modgrid   | MARCS atmospheric model grid (models only).
                  |                | Created using `create-grid.py --mode modbin` from files _newnew*.mod_
_grid.moo_        | --fn_moo       | MARCS atmospheric model grid (models with opacities) (Figure 8).
                  |                | Created using `create-grid.py --mode opa` from a bulk of models downloaded from the MARCS website

![](img/grid.moo_--_asalog-teff-glog_scatterplot.png)

Figure 7 -- 3D grid of atmospheric models. The scatterplot in the figure shows the (teff, glog, [Fe/H]) 
values for all existing atmospheric models in the grid (this is the file _grid.moo_ provided). The uppermost point are the Sun coordinates.

### 4.2.3 Files created by the Fortran programs

#### 4.2.3.1 Files created by `innewmarcs`

Table 4 -- Files created by `innewmarcs`

 Default name     | --option     | Description                    
------------------|--------------|---------------------------------------------------
_modeles.mod_     | --fn_modeles | atmospheric model (binary file) (Figure 8A)
_modeles.opa_     | --fn_opa     | atmospheric model: opacities (MARCS ".opa" format) (Figure 8B,8C)

![](img/modeles.png)

Figure 8 -- Atmospheric model information (Sun).
**(A)** data in file modeles.mod;
**(B)**, **(C)** data in modeles.opa  

`innewmarcs` creates two separate files (Table 4). They are created separately for
historical reasons. _modeles.opa_ follows the same structure of ".opa" files downloaded from
the MARCS website. _modeles.mod_ does **not** follow the same structure of MARCS ".mod" files.
Figure 8 illustrates the information contained in these files. 

#### 4.2.3.2 Files created by `hydro2`

`hydro2` creates a series of files named _thalpha_ (Figure 9), _thbeta_, _thgamma_, 
_thdelta_, _thepsilon_ etc (the series of hydrogen lines is given in _hmap.dat_).

![](img/thalpha.png)

Figure 9 -- Example of H-alpha line profile calculated by `hydro2`.  

#### 4.2.3.3 Files created by `pfant`

Table 5 - Files created by `pfant`

 Default name     | Description                    
------------------|--------------------------------------------------
_flux.spec_       | un-normalized flux (erg/cm**2/s/Hz multiplied by 10**5)
_flux.cont_       | continuum flux (erg/cm**2/s/Hz multiplied by 10**5)
_flux.norm_       | normalized flux (un-normalized flux)/(continuum flux)

![](img/spec-cont-norm.png)

Figure 10 - plots showing three `pfant` output files for the [4000, 7000] angstrom region: 
calculated spectrum; continuum; normalized spectrum.

The common prefix "flux" can be changed in file _main.dat_ to give a set of files
with different names.

#### 4.2.3.4 Files created by `nulbad`

`nulbad` creates a file whose name by default is the full input file name with the FWHM
added with three decimal places. For example,
 
```shell
nulbad --fwhm 1.2
```

creates a file named _flux.norm.nulbad.1.200_.
 
To change this, use option "--fn_cv", for example,

```shell
nulbad --fwhm 1.2 --fn_cv another-name
```


# 5 <a name=S7></a> Miscellanea how-to

## 5.1 <a name=S7_1></a> Converting "VALD3 extended" format atomic lines to PFANT format

The Vienna Atomic Line Database (VALD) is "a 
collection of atomic and molecular transition parameters of astronomical interest"
(http://vald.astro.uu.se/).

To convert from the "VALD3 extended" to a "PFANT atomic lines" file:

```shell
vald3-to-atoms.py <prefix>.vald3x
tune-zinf atoms-<prefix>-untuned.dat
```

This is done in two steps. The first step, `vald3-to-atoms.py` does the actual conversion
(which is quick) and saves a file, _e.g._, `atoms.dat`

The second step (which is time-consuming) is performed by `tune-zinf.py` and aims
to tune an important parameter used by the `pfant` Fortran binary.

It is recommended to use the tool `cut-atoms.py` to cut the file converted by
`vald3-to-atoms.py` to a wavelength region of interest before running `tune-zinf.py`.

For more information, see help for `vald3-to-atoms.py`, `tune-zinf.py`,
`cut-atoms.py` (call these scripts with `--help` option).


## 5.2 Continuous opacities: selecting between PFANT and MARCS coefficients

The PFANT default is to use its internal calculation of the continuum.

**PFANT-calculated continuum** (default)

```shell
innewmarcs --opa F
pfant --opa F --absoru T
```

or

```
run4.py
--opa F --absoru T
```

**MARCS opacities**

```shell
innewmarcs --opa T
pfant --opa T --absoru F
```

or

```
run4.py
--opa T --absoru F
```

:notes: For `innewmarcs`, "--opa T" causes the creation of an additional file _modeles.opa_
besides _modeles.mod_.

Related command-line options (also accessible in `x.py`):

```
--opa T ...... switches on MARCS opacities
               (may be of two types: absorption and scattering)
--abs T ...... switches on MARCS absorption
--sca T ...... switches on MARCS scattering
--absoru F ... switches off PFANT internal calculation
```


:notes: In order to use continuum opacities calculated by MARCS code (http://marcs.astro.uu.se/), you will need to create your own atmospheric model grid (using `create-grid.py`) or download file "grid.moo" as explained below (this file is too big to be stored on GitHub (241 MB > 100 MB)). File "grid.moo" contains a 3D grid of MARCS (http://marcs.astro.uu.se/) atmospheric models with opacities included.

(a) go to directory `PFANT/data/common` and run `get-grid.moo.sh`, or
  
(b) download it from [this location](https://docs.google.com/uc?export=download&confirm=4o6l&id=0B8m8GNLFiaewejd6dmJ6MW1pX2c)
(or [this location](https://drive.google.com/file/d/0B8m8GNLFiaewejd6dmJ6MW1pX2c/view))
and save it as _PFANT/data/common/grid.moo_


# <a name=S6></a>6 Links
  - [fortran/README.md](fortran/README.md) -- Fortran source code overview 
  - [_f311_ project](http://github.com/trevisanj/f311) -- Astronomy-related packages in Python


# <a name=S7></a>7 Troubleshooting

## 7.1 Tracking down why Fortran binaries fail to run

_pyfant_ creates a "session directory" named `session-xxx` every time it attempts
to run a PFANT binary. Each of these directories will always contain at least two files:
  - _commands.log_ -- this is the command lines attempted. You can copy-paste this
    line in your console to see how the Fortran binary goes.
  - _fortran.log_ -- messages printed by the Fortran binary.

Commonly, inspecting these files will be enough to figure out what happened.

Your current directory will also have a file named _python.log_, containing
debug/info/warning/error messages from Python.

:bulb: `explorer.py` has a "Collect errors" button, which
recurses directories in search for error/warning messages in all files named _fortran.log_

## 7.2 Metallicity/temperature/gravity of star is outside range in the grid of models
  
You can activate option "--allow True" to make bypass this check, but beware that the calculation
may be incorrect.

This can be done in the command line, _e.g._, `run4.py --allow T`, or check option "--alow"
in Tab 3 of `x.py`.


:bulb: `run4.py` creates a directory named _session-&lt;n&gt; where it saves temporary
files. It may be useful to check the log files in this directory if you encounter any
errors during program execution.


# 8 <a name=S8></a>References

**(Barbuy 1982)**<a name=R_BLB1982></a> Barbuy, B. 1982, PhD Thesis, Université de Paris VII

**Cayrel et al. 1991)**<a name=R_CAYREL1991></a> Cayrel, R., Perrin, M. N., Barbuy, B., & Buser, R. (1991). A grid of synthetic spectra for the determination of effective temperature, gravity and metallicity of F, G, and K stars. I-Description of the method. II-Application to 41 stellar spectra taken in the Basel field of SA 141. Astronomy and Astrophysics, 247, 108-129.

**(Barbuy et al. 2003)**<a name=R_BLB2003></a> Barbuy, B., Perrin, M.-N., Katz, D. et al. 2003, A&A,  404, 661

**(Coelho et al. 2005)**<a name=R_PC2005></a> Coelho, P., Barbuy, B., Meléndez, J., Schiavon, R. P., & Castilho, B. V. 
(2005). A library of high resolution synthetic stellar spectra from 300 nm to 
1.8${\ rm\ mu} $ m with solar and $\ alpha $-enhanced composition. Astronomy & 
Astrophysics, 443(2), 735-746.



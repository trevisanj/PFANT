# pyfant

pyfant provides a Python interface to the PFANT Fortran binaries. Some features:
  - object-oriented library for Python coding
  - command-line tools for
    - plotting, PDF generation: ```plot-spectra.py```, ```save-pdf.py```
    - cutting: ```cut-atoms.py```, ```cut-molecules.py```, ```cut-spectra.py```
    - running: ```run4.py```
    - VALD3 conversion: ```vald3-to-atoms.py```
    - etc
     
  - graphical user interfaces:
    - run Fortran binaries in parallel: ```x.py```
    - file explorer: ```explorer.py```
    - edit data files: ```ated.py```, ```mled.py```, ```mained.py```, ```abed.py```
    - etc

pyfant has support to loading a spectrum in a FITS file. 

## Command-line tools

To view the available tools, type ```pyfant-scripts.py``` in your console.

As of 22/Mar/2016, the following the avaiable tools:

Script name               | Purpose
--------------------------|----------
abed.py                   | ABundances file EDitor.
ated.py                   | ATomic lines file EDitor
cut-atoms.py              | Cuts atomic lines file to wavelength interval specified.
cut-spectrum.py           | Cuts spectrum file to wavelength interval specified. Saved in 2-column format.
explorer.py               | File Explorer - lists all files in directory and provides visualization options
link-to-data.py           | Creates symbolic links to PFANT data files to prevent copies of these data files.
mained.py                 | Main configuration file editor.
mled.py                   | Molecular Lines EDitor
plot-filetoh.py           | Plots hydrogen lines
plot-innewmarcs-result.py | Plots interpolated atmospheric model curve (hopefully) in the middle of the curves
plot-mod-record.py        | Plots one record of a binary .mod file (e.g., modeles.mod, newnewm050.mod)
plot-mod-records.py       | Opens several windows to show what is inside a NEWMARCS grid file.
plot-spectra.py           | Utility to quickly visualize spectra.
pyfant-scripts.py         | Lists scripts in PFANT/pyfant/scripts directory.
run4.py                   | Runs the four Fortran binaries in sequence: innewmarcs, hydro2, pfant, nulbad
save-pdf.py               | Looks for file "flux.norm" inside directories session-* and saves one figure per page in a PDF file.
tune-zinf.py              | Tunes the zinf parameter for each atomic line in atomic lines file.
vald3-to-atoms.py         | Converts VALD3 atomic/molecular lines file to PFANT atomic lines file.
vis-console.py            | Text-based menu application to open and visualize data files.
x.py                      | X - Graphical interface for running the PFANT Fortran executable binaries.
xm.py                     | XM - Graphical interface -- single and multi modes.

## Library

### Minimal example

```python
from pyfant import *
obj = Pfant()
obj.run()
```
This is equivalent to running ```pfant``` from the command line.

### Running innewmarcs, hydro2, pfant, nulbad in sequence

```python
from pyfant import *
obj = Combo()
obj.run()
```

### More examples

  - The ```demos``` directory contains a few (non-exhaustive) examples of how
    to use pyfant to perform different tasks.

  - The ```scripts``` directory, which contains tools, is also a good source of
    examples.
  

### Troubleshooting

#### Tracking down why Fortran binaries fail to run

pyfant creates a "session directory" named ```session-xxx``` every time it attempts
to run a PFANT binary. Each of these directories will always contain at least two files:
  - ```commands.log``` -- this is the command lines attempted. You can copy-paste this
    line in your console to see how the Fortran binary goes.
  - ```fortran.log``` -- messages printed by the Fortran binary.

Commonly, inspecting these files will be enough to figure out what happened.

Your current directory will also have a file named ```python.log```, containing
debug/info/warning/error messages from Python.

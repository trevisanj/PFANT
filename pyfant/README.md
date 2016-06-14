# pyfant

pyfant provides a Python interface to the PFANT Fortran binaries. Some features:
  - graphical user interfaces ([see screenshots](screenshots.md)):
    - run Fortran binaries in parallel: ```x.py```
    - file explorer: ```explorer.py```
    - edit data files: ```ated.py```, ```mled.py```, ```mained.py```, ```abed.py```
    - etc
  - command-line tools for
    - plotting, PDF generation: ```plot-spectra.py```, ```save-pdf.py```
    - cutting: ```cut-atoms.py```, ```cut-molecules.py```, ```cut-spectra.py```
    - running: ```run4.py```
    - VALD3 conversion: ```vald3-to-atoms.py```
    - etc
  - object-oriented library for Python coding
     

pyfant has support to loading a spectrum in a FITS file. 

## Command-line tools

To get an updated list of available tools, run `programs.py`.

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

(**new!**) Now `explorer.py` has a "Collect Fortran errors" button, which opens
all files names `fortran.log` in search of errors.
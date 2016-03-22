# pyfant

pyfant provides a Python interface to the PFANT Fortran binaries. Some features:
  - object-oriented library for Python coding
  - command-line tools
  - graphical user interfaces
  - runs various Fortran's in parallel
 
## Command-line tools

To view the available tools, type ```pyfant-scripts.py``` in your console.

As of 22/Mar/2016, the following the avaiable tools:


Script name               | Purpose
----------
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
run-multi.py              | Runs pfant for different abundances for each element, then run nulbad for each pfant result for different FWHMs.
run4.py                   | Runs the four Fortran binaries in sequence: innewmarcs, hydro2, pfant, nulbad
save-pdf.py               | Looks for file "flux.norm" inside directories session-* and saves one figure per page in a PDF file.
tune-zinf.py              | Tunes the zinf parameter for each atomic line in atomic lines file.
vald3-to-atoms.py         | Converts VALD3 atomic/molecular lines file to PFANT atomic lines file.
vis-console.py            | Text-based menu application to open and visualize data files.
x.py                      | X - Graphical interface for running the PFANT Fortran executable binaries.
xm.py                     | XM - Graphical interface -- single and multi modes.

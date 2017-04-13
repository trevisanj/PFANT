7 Troubleshooting
=================

7.1 Tracking down why Fortran binaries fail to run
--------------------------------------------------

| *pyfant* creates a "session directory" named ``session-xxx`` every
time it attempts
| to run a PFANT binary. Each of these directories will always contain
at least two files:

-  *commands.log* -- this is the command lines attempted. You can
   copy-paste this
    line in your console to see how the Fortran binary goes.
-  *fortran.log* -- messages printed by the Fortran binary.

Commonly, inspecting these files will be enough to figure out what
happened.

| Your current directory will also have a file named *python.log*,
containing
| debug/info/warning/error messages from Python.

| :bulb: ``explorer.py`` has a "Collect errors" button, which
| recurses directories in search for error/warning messages in all files
named *fortran.log*

7.2 Metallicity/temperature/gravity of star is outside range in the grid of models
----------------------------------------------------------------------------------

| You can activate option "--allow True" to make bypass this check, but
beware that the calculation
| may be incorrect.

| This can be done in the command line, *e.g.*, ``run4.py --allow T``,
or check option "--alow"
| in Tab 3 of ``x.py``.

| :bulb: ``run4.py`` creates a directory named \_session-<n> where it
saves temporary
| files. It may be useful to check the log files in this directory if
you encounter any
| errors during program execution.
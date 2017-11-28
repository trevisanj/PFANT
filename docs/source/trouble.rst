Troubleshooting
===============

Tracking down why Fortran binaries fail to run
----------------------------------------------

- If you are running the Fortran binaries directly, error messages are printed on screen.
- If you are running from ``x.py``, the "Runnables Manager" window has a "Collect errors" button.
- If you are running ``run4.py``, open ``explorer.py`` and click the "Collect errors" button.

``run4.py`` or ``x.py`` create **session directories** named ``session-xxx``
containing at least these two files:

-  *commands.log* -- this is the command lines used to invoke the Fortran binaries. This file
   may be useful if you want to specifically reproduce one of these commands for debugging reasons.
   In such case, copy-paste this line in your console to see how the Fortran binary runs.
-  *fortran.log* -- Fortran output is logged into this file.

Your current directory will also have a file named *python.log*,
containing debug/info/warning/error messages from Python.

Metallicity/temperature/gravity of star is outside range in the grid of models
------------------------------------------------------------------------------

You can activate option ``--allow True`` to make bypass this check.

This can be done in the command line, *e.g.*, ``run4.py --allow T``, or check option "--alow"
in Tab 3 of ``x.py``.

.. attention:: Beware that with ``--allow True`` the interpolation for the atmospheric model may be innapropriate.
               With that option, ``innewmarcs`` will **not** extrapolate the atmospheric model grid,
               but will instead "project" your (teff, glog, metallicity) coordinate onto the closest grid wall,
               then interpolate.

/**
@page codeblocks CodeBlocks configuration

CodeBlocks is a good IDE to compile and debug Fortran sources.

However, I was experiencing some problems while trying to compile PFANT, which have been
sorted after some online research.

Compiling modules in right order
================================

One problem was the order in which CodeBlocks wanted to compile my modules (alphabetical).
This was giving me errors of the kind:
@verbatim
Fatal Error: Can't open module file 'config.mod' for reading at (1): No such file or directory
@endverbatim

Solution
--------

This was solved following the tip at
http://stackoverflow.com/questions/17538500/gfortran-and-codeblocks-issue-with-modules-and-multiple-files,
quoted (adapted) below:

@verbatim
Menu Project/Properties...
In Build Target Files at the tab Build targets check only pfant.f90
And use the command Include 'File_Name.f90' inside the pfant.f90 code to include the other
.f90 files for compilation in the right order.
@endverbatim

So, the mentioned @c include goes inside @c pfant.f90 *before* the @c program statement.
I try to compile using @c Ctrl+F9 and it says:

@verbatim
Fatal Error: Can't open module file 'config.mod' for reading at (1): No such file or directory
@endverbatim

So I add the requirement, and now my program starts like this:

@code
include 'config.f90'
program pfant
...
@endcode

I press @c Ctrl+F9 again and now it says

@verbatim
Fatal Error: Can't open module file 'logging.mod' for reading at (1): No such file or directory
@endverbatim

because @c config.f90 uses the @c logging module.

So, now I add @c logging.f90 as well, and my @c pfant.f90 now looks like

@code
include 'logging.f90'
include 'config.f90'
program pfant
...
@endcode

I keep going in this fashion until my @c pfant.f90 now looks like

@todo to be compleeted
@code
To be continued
include 'logging.f90'
include 'config.f90'
program pfant
...
@endcode

*/

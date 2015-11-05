/**
@page coding_style Coding style

Sketch of a coding style manual

Introduction
============

This document contains:
@li a set of guidelines about how to format the code to keep consistent among all .f and .f90 files
@li some [almost] rules on how to deal with modules and variables


@todo how to create a new executable

@todo how to insert new command-line option

@todo reading either from main file or command-line option for all variables

Format
======

Now using the "modern" .f90-.f95 file format (instead of .f "punchcard").

Indentation
-----------

@li 2-space indentation for all IF, DO, SUBROUTINE etc. structures.

@li Continuation lines: suggestion: continue with a 1-space indentation from the
    beginning line, i.e.:

@code
... WRITE (*,*) 'A equals ', A, '; B equals ', B, '; C equals ', &
...  C, 'D equals ', D
     |
     1-space indentation after beginning line
@endcode

Number of columns
-----------------

Guideline applies to files of type
@li .f
@li .f90
@li .txt
@li .cpp

*90-column maximum* as a tolerance over the 80-column historical width.

Coders ofter pronounce on keeping code width to 80 columns maximum.
Common arguments are for for readability, ergonomics, and ability to split screen.
@ref Cesm0 @ref StackEx0 @ref JavaGuide

Code structure
==============

COMMON blocks
-------------
Do *not* use COMMON blocks!! COMMON blocks require variables to be declared multiple
times with different names and shapes. This is high-maintainance and makes the code harder
to understand, specially for newbies.

MODULE provides a more clear structure to share variables among subroutines and functions.
Variables are declared only once at the header section of a module.

A module template
-----------------

@code
!> Doxygen "brief" description line for module xyz.
!>
!> xyz_* -- public variables from this module
!>

module xyz
  use another_module_1
  use another_module_2
  implicit none
  private  ! makes all symbols declared below become private by default
  
  !> refers to what comes after, i.e.,
  !> explains variable num_items
  integerm public :: xyz_num_items
  real*8, public :: xyz_some_lambda !< explains xyz_some_lambda
                       !! more about xyz_some_lambda

  integer :: i, &  !< ?doc?
   j !< ?doc?
                       
  public calc_x
  
contains
  !> description line for calc_x()
  
  subroutine calc_x(par1, par2, result)
    ! i, j, xyz_* are visible here
  end    
  
  !> ?doc?

  real*8 function f(x)
    ! etc...
  end 
@endcode  

Modules can be imported around through the @c USE statement.

Variable names
--------------

* Prefixes*: in many sections of the code, a preceding "prefix_" has been added to
the original names of variables that are shared among subroutines and functions
preceded. 
@li Prefixes help to track down the meaning and origin of a certain variable.
@li Additionally, they help to ensure that variable names don't clash across different modules.

Sometimes the prefix matches the module name where the variable is declared, sometimes not.
    
Examples:
@verbatim
au_g3d           from module absoru
config_fn_main   from module config
main_ptdisk      from module reader_main
selekfh_fl       calculated by subroutine synthesis::selekfh()
MAX_PARTIT_NPAR  constant having maximum allowed value of variable partit_npar
...
@endverbatim

real numbers
------------

real*8 is used throughout, except for reading the binary .mod files.

"If you must use floating point, use double precision unless you have reason
to be concerned about memory use (your program uses large arrays) and you do
not need much precision. Modern computers do not take significantly longer to
process double precision values than they do to process reals." @ref Uwm0


Always IMPLICIT NONE
--------------------

Add the IMPLICIT NONE statement at the beginning of each MODULE or PROGRAM.
@li types of variables becomes clear from reading the code
@li we are forced to remember to declare real variables as real*8

Case convention
---------------
All symbol (variable, function, subroutine, module) names are now declared in *lowercase*,
except for PARAMETERs which are all *UPPERCASE*.
Constants not declared as PARAMETER may be also in uppercase.

@note The main reasons for this was a limitation in Doxygen, which converts all symbols
to lowercase in the HTMLs anyway (fix has been asked for but no action yet
http://doxygen.10944.n7.nabble.com/The-lowercase-problem-when-using-the-Doxygen-with-Fortran-td1243.html).
It sucks to have to change your code conventions because of the documentation tool, but
considering all you get from Doxygen, it seemed to be a good trade-off. The symbols could
have been left in uppercase in the source code and would appear in lowercase in the HTML,
but this would be kind of confusing.


Variable declarations
---------------------
@li Always use intent(in/out) for subroutine/function arguments. This will allow the reader to
    know which arguments are inputs and which arguments are outputs for the subroutine.
@li <b>Module globals and subroutine/function arguments</b>: declare *only one* variable per
    code line, and document the variable.
@li <b>Any variable</b>: document the variable if its use/purpose is not obviuos.

Function/subroutines
--------------------
Write documentation explaining what the function/subroutine does and whenever possible,
why it was created.

Logging (i.e., printing things on the screen)
---------------------------------------------
For outputting messages on the screen, use the routines in the LOGGING
module (not just WRITE or PRINT). This may give a bit more work
(may require one extra line of code if the string is formatted), but
there are good reasons for that.
There are different routines to be used, such as
log_critical(), log_error(), log_warning(), log_info(), log_debug().

Logging can be silenced and/or sent to specific logging file using
command-line config options.

For assertions, call log_halt("message", is_assertion=.true.).

@note Assertions are checks against coding errors.
Assertions serve both as documentation and error protection.


Documentation
=============

Some guidelines inspired in Agile modelling documentation guidelines
(http://www.agilemodeling.com/essays/agileDocumentation.htm):

@li Explain what the code does in the .f90 files, preferrably as close
    to the actual code as possible.
@li Keep the information in the .cpp files to a minimum, mostly to give
    directions to users who just bumped into the project.


C++ sources
-----------
The .cpp extension was adopted to write documentation pages because C++
block comments are more convenient than Fortran "!" at the beginning of each line.

.f, .f90 files
--------------

*What to put in comments*. It is recommended to document at least this:

@li Explain -- at the beginning of a module -- what the prefixes of variables declared within it stand for.
@li module, subroutine, and function: at least one description line .
@li subroutine or function arguments: at least one sentence for each argument.
@li variables declared in the header section of a module: at least one sentence.
@li when the logic becomes tricky, it is a kind gesture to explain what the code is doing


How to format comments for Doxygen in the .f, .f90 files
--------------------------------------------------------

We tell Doxygen which comments we want parsed by using these indicators after the "!", i.e.,
@code
!> explains what comes next
!< explains what came last ...
!! ... continuing
@endcode

See module template in this document for example uses.
                       
More about how Doxygen:

https://www.stack.nl/~dimitri/doxygen/manual/docblocks.html#fortranblocks

https://modelingguru.nasa.gov/docs/DOC-1811


Tags
====

These are search keywords with the following meaning:

@verbatim

todo                 ordinary to-do item.
issue                high-priority to-do item, usually an unsolved issue about the way the code works.
?doc?                documentation "gap"
?what?               high-priority documentation gap, e.g. subroutine lacks description line.
ask BLB              suggestion on who to ask about issue
ask MT               "
ask PC               "
ask EC               "

@endverbatim

Tags are case-insensitive. This list is probably not complete. 

References
==========

https://www.stack.nl/~dimitri/doxygen/manual/docblocks.html#fortranblocks

https://modelingguru.nasa.gov/docs/DOC-1811

http://www.agilemodeling.com/essays/agileDocumentation.htm
*/

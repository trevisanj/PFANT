/**
@page codingStyle Coding style

Sketch of a coding style manual

@todo steps to create a new executable

Format
======

Now using the "modern" .f90-.f95 file format (instead of .f "punchcard").

Indentation
-----------

@li 2-space indentation for all IF, DO, SUBROUTINE etc. structures.

@li Continuation lines: suggestion: continue with a 1-space indentation from the beginning line, i.e.:

@code
... WRITE (*,*) 'A equals ', A, '; B equals ', B, '; C equals ', &
...  C, 'D equals ', D
     |
     1-space indentation after beginning line
@endcode

Number of columns
-----------------

Rule for all @c .f, @c .f90, and @c .txt files: *90-column maximum*.
This is probably a good a compromise between
  predominant sense (which is 80-column, arguing about readability and ergonomics),
  accessibility (code being read by different people on different system setups) and
  horizontal space (not having to wrap formulas and long lines of code too often).
@ref Cesm0 @ref StackEx0 @ref JavaGuide





Common blocks
=============
Do NOT use common blocks! Use Fortran MODULE to declare globals!


Data types
==========
  REAL*4 vs. REAL*8
  -----------------
  Now all real variables must be REAL*8.
  The routine that reads file infile:modeles still has to declare REAL*4 to read this binary
  file.

  "If you must use floating point, use double precision unless you have reason
  to be concerned about memory use (your program uses large arrays) and you do
  not need much precision. Modern computers do not take significantly longer to
  process double precision values than they do to process reals." @ref Uwm0

  ISSUE the molecules part may break the rule, let us see when trying to run the thing.


Programming
===========

Always IMPLICIT NONE
--------------------
Add the IMPLICIT NONE statement at the beginning of each MODULE or PROGRAM.
Advantages:
@li type of variables becomes clear (not all code readers will know about Fortran i-n
    convention;
@li we are forced to declare real variables as real*8.

Case convention
---------------
All symbol (variable, function, subroutine, module) names are now declared in *lowercase*,
except for *constants* which are all *UPPERCASE*.

@note The main reasons for this was a limitation in Doxygen, which converts all symbols
to lowercase in the HTMLs anyway (fix has been asked for but no action yet
http://doxygen.10944.n7.nabble.com/The-lowercase-problem-when-using-the-Doxygen-with-Fortran-td1243.html).
It sucks to have to change your code conventions because of the documentation tool, but
considering all you get from Doxygen, it seemed to be a good trade-off. The symbols could
have been left in uppercase in the source code and would appear in lowercase in the HTML,
but this would be kind of confusing.

Name conventions
----------------

@li Routines synthesis::synthesis_ and absoru::absoru_ names have a trailing underscore in order to
    differentiate from their respective module names;

@li Prefixes are now used extensively throughout the code to group variables according
    to some meaning, for example:
    @code
    main_ptdisk     ! variable comes from file main.dat
    selekfh_fl      ! variable is calculated by subroutine synthesis::selekfh()
    MAX_PARTIT_NPAR ! constant having maximum allowed value of variable partit_npar
    ...
    @endcode
    Every module explains the meaning of its own prefixes.




Variable declarations
---------------------
@li Always use intent(in/out) for subroutine arguments. This will allow the reader to
    know which arguments are inputs and which arguments are outputs for the subroutine.
@li <b>Module globals and subroutine/function arguments</b>: declare *only one* variable per
    code line, and document the variable.
@li <b>Any variable</b>: document the variable if its use/purpose is not obviuos.

Function/subroutines
--------------------
Write documentation explaining what the function/subroutine does and whenever possible,
why it was created.

Logging (i.e., printing things on the screen)
For outputting messages on the screen, use the routines in the LOGGING
-------
module (not just WRITE or PRINT). This may give a bit more work
(may require one extra line of code if the string is formatted), but
there are good reasons. for that.
There are different routines to be used, such as
LOG_CRITICAL(), LOG_ERROR(), LOG_WARNING(), LOG_INFO(), LOG_DEBUG(), and
logging can be silenced and/or sent to specific logging file using
command-line config options.


Documenting
===========

@todo Doxygen Fortran section

  Note that
  @code !> @endcode refers to what comes after, and
  @code !< @endcode refers to what comes before.


Tags
----
Some tags used in comments throughout the source code:

@note All tags, except "@@todo" are case-insensitive.

@verbatim

@todo                  Doxygen's todo tag. Note that not all "todos" are marked in this
                       way

issue                  Something that needs to be solved in the code. Similar to @todo
                       (this tag was being used before Doxygen was adopted)

?doc?                  Item (variable/subroutine etc) lacks proper documentation

?what?                 More serious version of ?doc?: crucial item (variable/subroutine etc)
                       is undocumented. This is reserved undocumented items that seem
                       important

ask BLB
ask PC
ask MT                 Suggests the person who probably knows to solve the issue

#assertion          Marks bit of code that is an assertion. Assertions serve both as
                       documentation and error protection.

#logging            Marks bit of code whose purpose is to output log messages.
                       *deprecated*, I don't think it pays off

#spill_check        Marks bit of code destinated to verify whether we are trying to assign
                       an element beyond the size of a vector/matrix, commonly performed while
                       reading files

                       *Note*: it is recommended that the error message that accompanies this tag
                               includes the name of the constant that has been violated (one will
                               probably want to increase the size of this constant and recompile
                               the program)
                               
#consistency_check  Marks bit of code destinated to verify whether we are trying to assign
                       an element beyond the size of a vector/matrix, commonly performed while
                       reading files


__temporary__          Temporary code to keep going, but will require future action



@endverbatim



*/

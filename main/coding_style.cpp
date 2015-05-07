/**
@page codingStyle Coding style

Sketch of a coding style manual

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
  The routine that reads file modeles.mod still has to declare REAL*4 to read this binary
  file.

  "If you must use floating point, use double precision unless you have reason
  to be concerned about memory use (your program uses large arrays) and you do
  not need much precision. Modern computers do not take significantly longer to
  process double precision values than they do to process reals." @ref Uwm0
  
  ISSUE the molecules part may break the rule, let us see when trying to run the thing.


Programming
===========

  Logging (i.e., printing things on the screen)
  -------
  For outputting messages on the screen, use the routines in the LOGGING
  module (not just WRITE or PRINT). This may give a bit more work
  (may require one extra line of code if the string is formatted), but
  there are good reasons. for that.
  There are different routines to be used, such as
  LOG_CRITICAL(), LOG_ERROR(), LOG_WARNING(), LOG_INFO(), LOG_DEBUG(), and
  logging can be silenced and/or sent to specific logging file using
  command-line config options.



*/

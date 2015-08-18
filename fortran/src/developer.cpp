/**

@page developerguide Developer's guide

@subpage codingStyle

@subpage github

@subpage codeblocks

@subpage doxygen

*/


/**
@page doxygen Working with Doxygen

Troubleshooting segmentation fault
==================================

Doxygen has got Fortran code parsing bugs. I don't know the exact situation that causes
Doxygen to crash, but I have figured out things to be avoided.

Backslashes
-----------

Doxygen crashed because of the following line of code:


      if (wdir_trim(i:i) .eq. '\') wdir_trim(i:i) = '/'


Doxyfile must have

@code
ENABLE_PREPROCESSING   = NO
ndcode


*/
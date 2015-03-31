      MODULE ERRORS

      ! ERROR_EXCEEDED: Error code if any array index blow maximum number of elements
      !                 allowed while reading from file.
      ! ERROR_NOT_FOUND: When value from somewhere selects something that does not exist
      !                  (example in READ_MODELES()).
      ! ERROR_BAD_VALUE: generic error for bad value found inside file
      INTEGER, PARAMETER ::
     +  ERROR_EXCEEDED=111,
     +  ERROR_NOT_FOUND=112,
     +  ERROR_BAD_VALUE=113,
     +  ERROR_ASSERTION=987

      CONTAINS


      ! TODO subroutine RAISE_ERROR(S) to quit program after applying some standard formatting
      
      
      
      
      

      END MODULE ERRORS



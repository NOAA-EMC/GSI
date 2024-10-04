;+
;
; NAME:
;       Valid_String
;
; PURPOSE:
;       Function to check if a string is defined AND of non-zero length
;
; CALLING SEQUENCE:
;       result = Valid_String(string)
;
; INPUTS:
;       string:   String variable to check
;
; FUNCTION RESULT:
;       result:  The return value is an integer defining the logical result.
;                The result codes are defined in the
;                  Error_Handling/error_codes.pro
;                file.
;                If == TRUE  the string is defined and of non-zero length
;                   == FALSE the string is NOT defined or of ZERO length
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 16-Sep-2005
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION Valid_String, Input_String

  @error_codes

  IF ( N_ELEMENTS( Input_String ) EQ 0 ) THEN RETURN, FALSE
  IF ( STRLEN( STRTRIM( Input_String, 2 ) ) EQ 0 ) THEN RETURN, FALSE

  RETURN, TRUE

END

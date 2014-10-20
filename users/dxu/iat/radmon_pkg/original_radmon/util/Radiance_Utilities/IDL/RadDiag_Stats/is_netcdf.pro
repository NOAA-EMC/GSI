;+
;
; NAME:
;       Is_netCDF
;
; PURPOSE:
;       Function to check if a file is a netCDF format file.
;
; CALLING SEQUENCE:
;       result = Is_netCDF( file )
;
; INPUTS:
;       file:    Name of the input file to test.
;
; FUNCTION RESULT:
;       result:  The return value is an integer defining the logical result.
;                The result codes are defined in the
;                  Error_Handling/error_codes.pro
;                file.
;                If == TRUE  the file is a netCDF file
;                   == FALSE - an error occurred processing the input, or
;                            - the file could not be opened as a netCDF file
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 19-Apr-1999
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION Is_netCDF, file

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FALSE
  ENDIF

  ; Check input
  ; Must supply a file to check
  nArguments = 1
  IF ( N_PARAMS() NE nArguments) THEN $
    MESSAGE, 'Must supply an input filename', $
             /NONAME, NOPRINT
  ; File argument must be defined
  IF ( NOT Valid_String( file ) ) THEN $
    MESSAGE, 'File argument must be defined', $
             /NONAME, /NOPRINT

  ; Open the file
  ncdf_id = NCDF_OPEN( file )
  IF ( ncdf_id EQ -1 ) THEN $
    MESSAGE, 'Error opening file ' + file, $
             /NONAME, /NOPRINT

  ; Close the file
  NCDF_CLOSE, ncdf_id

  ; Done
  RETURN, TRUE

END

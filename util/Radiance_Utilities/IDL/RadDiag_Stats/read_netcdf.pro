;+
;
; NAME:
;       Read_netCDF
;
; PURPOSE:
;       Function to read variable and attribute data from netCDF 
;       format files.
;
; CALLING SEQUENCE:
;       result = Read_netCDF( ncFile                                       , $ ; Input
;                             data                                         , $ ; Output
;                             VARIABLE_LIST         = variable_list        , $ ; Input keyword
;                             COUNT                 = count                , $ ; Input keyword
;                             OFFSET                = offset               , $ ; Input keyword
;                             STRIDE                = stride               , $ ; Input keyword
;                             DIMENSIONS_ONLY       = dimensions_only      , $ ; Input keyword
;                             VARIABLE_ATTRIBUTES   = variable_attributes  , $ ; Input keyword
;                             GLOBAL_ATTRIBUTES     = global_attributes    , $ ; Input keyword
;                             NO_VAR_BYTE_TO_STRING = no_var_byte_to_string, $ ; Input keyword
;                             NO_ATT_BYTE_TO_STRING = no_att_byte_to_string, $ ; Input keyword
;                             QUIET                 = quiet                  ) ; Input keyword
;
; INPUTS:
;       ncFile:                 The name of the NetCDF file to read
;
; INPUT KEYWORD PARAMETERS:
;       variable_list:          A string array of variable name to read from
;                               the NetCDF file. If not specified, ALL the
;                               variables are read.
;       count:                  Set this keyword to a vector containing the
;                               number of points in each dimension that are
;                               required for a variable read. It is a 1-based
;                               vector and defaults to match the size of all
;                               dimensions so that all data is read.
;       offset:                 Set this keyword to a vector containing the
;                               starting index position for each dimension of
;                               the variable required. It is a 0-based
;                               vector and defaults to zero for every dimension
;                               so that all data is read.
;       stride:                 Set this keyword to a vector containing the
;                               strides, or sampling intervals, between accessed
;                               values of the required variable. It is a 1-based
;                               vector and defaults to one for every dimension
;                               so that all data is read.
;       dimensions_only:        Set this keyword to return *ONLY* the netcdf
;                               dimension data.
;       variable_attributes:    Set this keyword to return variable
;                               attribute data. Using this keyword modified the
;                               the form of the output structure. See the 
;                               OUTPUTS description below.
;       global_attributes:      Set this keyword to return global
;                               attribute data.
;       no_var_byte_to_string:  Set this keyword to prevent the
;                               conversion of BYTE variable data
;                               to STRING type. (IDL 5.2 and earlier only)
;       no_att_byte_to_string:  Set this keyword to prevent the
;                               conversion of BYTE attribute data
;                               to STRING type. (IDL 5.2 and earlier only)
;       quiet:                  Set this keyword to suppress informational
;                               output.
;
; OUTPUTS:
;       data:          The data structure containing the file data
;                      requested.
;
;                      OUTPUT DATA STRUCTURE FORM
;                      --------------------------
;                      o The file dimensions are always returned,
;
;                          data.dim1
;                              .dim2
;                              .dim3
;                            .....
;                              .dimN
;
;                      o If variable data is read in, they are present in
;                        the output structure like so:
;
;                          data.var1
;                              .var2
;                              .var3
;                            .....
;                              .varN
;  
;                      o If variable attributes are also requested, the variable
;                        portion of the output structure has the form:
;  
;                          data.var1.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;                              .var2.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;                            .....
;                              .varN.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;
;                        where the capitalised tag DATA is the actual tag name
;                        used for the variable data.
;
;                      o If global attributes are requested, they are present 
;                        in the output structure like so:
;  
;                          data.gatt1
;                              .gatt2
;                              .gatt3
;                            .....
;                              .gattN
;
;
; FUNCTION RESULT:
;       Error_Status:   The return value is an integer defining the error status.
;                       The error codes are defined in the
;                         Error_Handling/error_codes.pro
;                       file.
;                       If == SUCCESS the netCDF data read was successful.
;                          == FAILURE an unrecoverable error occurred.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 23-Sep-1999
;                       paul.vandelst@ssec.wisc.edu
;
;-

; -------------------------------------------
; Local function to check the validity of the
; COUNT, OFFSET, and STRIDE vector keywords.
;
; Adapted from Liam Gumley's NC_READ.PRO
; -------------------------------------------
FUNCTION check_vectors, ncId         , $
                        variable_info, $
                        count        , $
                        offset       , $
                        stride       , $
                        count_vector , $
                        offset_vector, $
                        stride_vector
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    


  ; Get the variable dimensions
  ;
  ; Create the variable dimension array
  variable_dimensions = LONARR( variable_info.NDIMS )

  ; Loop over the dimensions
  FOR i = 0, variable_info.NDIMS - 1 DO BEGIN
    ; Get the dimension name and size
    NCDF_DIMINQ, ncId, $                 ; Input
                 variable_info.DIM[ i ], $  ; Input
                 dimension_name, $          ; Output
                 dimension_size             ; Output
    ; Save the dimension size
    variable_dimensions[ i ] = dimension_size
  ENDFOR


  ; Check for COUNT, OFFSET, and STRIDE vectors of the wrong length
  ;
  ; Count vector
  IF ( N_ELEMENTS( count ) NE 0 AND $
       N_ELEMENTS( count ) NE variable_info.NDIMS ) THEN $
    MESSAGE, 'COUNT vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT

  ; Offset vector
  IF ( N_ELEMENTS( offset ) NE 0 AND $
       N_ELEMENTS( offset ) NE variable_info.NDIMS ) THEN $
    MESSAGE, 'OFFSET vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT

  ; Stride vector
  IF ( N_ELEMENTS( stride ) NE 0 AND $
       N_ELEMENTS( stride ) NE variable_info.NDIMS ) THEN $
    MESSAGE, 'STRIDE vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT


  ; Check for definition and range of COUNT, OFFSET, and STRIDE vectors
  ;
  ; Offset vector
  ; Is it defined?
  IF ( N_ELEMENTS( offset ) EQ 0 ) THEN $
    offset_vector = REPLICATE( 0L, variable_info.NDIMS ) $
  ELSE $
    offset_vector = LONG( offset )
  ; Is it valid?
  offset_vector = ( offset_vector < ( variable_dimensions - 1L ) ) > $
                  REPLICATE( 0L, variable_info.NDIMS )

  ; Stride vector
  ; Is it defined?
  IF ( N_ELEMENTS( stride ) EQ 0 ) THEN $
    stride_vector = REPLICATE( 1L, variable_info.NDIMS ) $
  ELSE $
    stride_vector = LONG(stride)
  ; Is it valid?
  stride_vector = ( stride_vector < ( variable_dimensions - offset_vector ) ) > $
                  REPLICATE( 1L, variable_info.NDIMS )

  ; Count vector  
  ; Is it defined?
  IF ( N_ELEMENTS( count ) EQ 0 ) THEN $
    count_vector = ( variable_dimensions - offset_vector ) / stride_vector $
  ELSE $
    count_vector = LONG( count )
  ; Is it valid?
  count_vector = ( count_vector < ( ( variable_dimensions - offset_vector ) / stride_vector ) ) > $
                 REPLICATE( 1L, variable_info.NDIMS )


  ; Done
  RETURN, SUCCESS

END ; FUNCTION check_vectors


; --------------------------------------------------------
; Local function to convert BYTE/CHAR values
; to a STRING data type.
;
; Need this because all string netCDF data
; types were returned as BYTE arrays.
;
; For pre IDL v5.3 the returned data type string is "BYTE"
; For IDL v5.3 the returned data type string is "CHAR"
; --------------------------------------------------------
FUNCTION convert_string, data_type, $
                         input_string, $
                         NO_CONVERT=no_convert

  ; Set the IDL version with the bug fix
  fixed_IDL_version = 5.3

  ; If the data type is CHAR, then we have a string. Convert and return.
  IF ( STRUPCASE( data_type ) EQ 'CHAR' ) THEN $
    RETURN, STRING( input_string )

  ; The data type is not CHAR. Maybe it needs converting, maybe not.
  IF ( FLOAT( !VERSION.RELEASE ) LT fixed_IDL_version AND $
       STRUPCASE( data_type ) EQ 'BYTE'               AND $
       ( NOT KEYWORD_SET( no_convert ) )                  ) THEN $
    RETURN, STRING( input_string )

  ; Don't do anything
  RETURN, input_string

END ; FUNCTION convert_string


;###############################################################################
;
; Main function
;
;###############################################################################
FUNCTION Read_netCDF, ncFile                                       , $  ; Input
                      data                                         , $  ; Output
                      VARIABLE_LIST         = variable_list        , $  ; Input keyword
                      COUNT                 = count                , $  ; Input keyword
                      OFFSET                = offset               , $  ; Input keyword
                      STRIDE                = stride               , $  ; Input keyword
                      DIMENSIONS_ONLY       = dimensions_only      , $  ; Input keyword
                      VARIABLE_ATTRIBUTES   = variable_attributes  , $  ; Input keyword
                      GLOBAL_ATTRIBUTES     = global_attributes    , $  ; Input keyword
                      NO_VAR_BYTE_TO_STRING = no_var_byte_to_string, $  ; Input keyword
                      NO_ATT_BYTE_TO_STRING = no_att_byte_to_string, $  ; Input keyword
                      QUIET                 = quiet                     ; Input keyword

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS( ncId ) NE 0 ) THEN NCDF_CLOSE, ncId
    RETURN, FAILURE
  ENDIF    

  ; Check input
  ;
  ; Argument list
  nArguments = 2
  IF ( N_PARAMS() LT nArguments ) THEN $
    MESSAGE, 'Invalid number of arguments', /NONAME, /NOPRINT
  ; Filename input
  IF ( NOT Valid_String(ncFile) ) THEN $
    MESSAGE, 'Input ncFile argument not defined!', /NONAME, /NOPRINT
  ; Ensure file is netCDF
  IF ( NOT Is_netCDF( ncFile ) ) THEN $
    MESSAGE, ncFile + ' is not a NetCDF format file', /NONAME, /NOPRINT
  ; Read ALL data if no variable list keyword
  read_all_variables = TRUE
  IF ( KEYWORD_SET( variable_list ) ) THEN read_all_variables = FALSE
  ; Process QUIET keyword
  noisy = TRUE
  IF ( KEYWORD_SET(quiet) ) THEN noisy = FALSE


  ; Open the netCDF file
  ncId = NCDF_OPEN( ncFile, /NOWRITE )
  IF ( ncId EQ -1 ) THEN $
    MESSAGE, 'Error opening file ' + ncFile, /NONAME, /NOPRINT


  ; Outut dimension info
  ncFile_info = NCDF_INQUIRE( ncId )
  IF ( noisy ) THEN BEGIN
    PRINT, FORMAT = '( 10x,"Number of dimensions        : ",i7 )', $
                    ncFile_info.NDIMS
    PRINT, FORMAT = '( 10x,"Number of variables         : ",i7 )', $
                    ncFile_info.NVARS
    PRINT, FORMAT = '( 10x,"Number of global attributes : ",i7 )', $
                    ncFile_info.NGATTS
    PRINT, FORMAT = '( 10x,"ID of unlimited dimension   : ",i7 )', $
                    ncFile_info.RECDIM
  ENDIF


  ; Get the dimension information 
  FOR i=0, ncFile_info.NDIMS-1 DO BEGIN
    ; Get dimension info
    NCDF_DIMINQ, ncId, i, dimName, dimSize
    ; Load into structure
    IF ( i EQ 0 ) THEN $
      data = CREATE_STRUCT( dimName, dimSize ) $
    ELSE $
      data = CREATE_STRUCT( data, dimName, dimSize )
  ENDFOR
  IF ( KEYWORD_SET( dimensions_only ) ) THEN GOTO, Done


  ; Read global attributes
  IF ( KEYWORD_SET( global_attributes ) ) THEN BEGIN
    n_global_attributes = ncFile_info.NGATTS
    IF ( n_global_attributes GT 0 ) THEN BEGIN
      FOR i = 0, n_global_attributes - 1 DO BEGIN
        ; Get global attribute name
        attribute_name = NCDF_ATTNAME( ncId   , $  ; Input
                                       i      , $  ; Input
                                       /GLOBAL  )  ; Input keyword
        ; Get global attribute value
        NCDF_ATTGET, ncId          , $  ; Input
                     attribute_name, $  ; Input
                     attribute     , $  ; Output
                     /GLOBAL            ; Input keyword
        ; Get global attribute info
        attribute_info = NCDF_ATTINQ( ncId          , $  ; Input
                                      attribute_name, $  ; Input
                                      /GLOBAL         )  ; Input keyword

        ; If necessary and required, convert BYTE/CHAR attribute to STRING
        attribute = convert_string( attribute_info.datatype           , $  ; Input
                                    attribute                         , $  ; Input
                                    NO_CONVERT = no_att_byte_to_string  )  ; Input keyword
        ; Append to structure
        data = CREATE_STRUCT( data          , $  ; Input
                              attribute_name, $  ; Input
                              attribute       )  ; Input

      ENDFOR ; i
    ENDIF ELSE BEGIN
      MESSAGE, 'No global attributes to read!', /INFORMATIONAL
    ENDELSE
  ENDIF


  ; Determine if there are any variables to read
  ;
  ; Set the number of variables to read and
  ; initialise the valid variable counter
  IF ( read_all_variables ) THEN $
    n_variables = ncFile_info.NVARS $
  ELSE $
    n_variables = N_ELEMENTS( variable_list )

  ; Are there any variables to read?
  IF ( n_variables EQ 0 ) THEN $
    MESSAGE, 'No variables to read!', /NONAME, /NOPRINT


  ; Begin reading variable data
  ;
  ; Loop over variables
  FOR i = 0, n_variables - 1 DO BEGIN

    ; Get the variable ID
    IF ( read_all_variables ) THEN BEGIN
      variable_id = i
    ENDIF ELSE BEGIN
      variable_name = STRTRIM(variable_list[i],2)
      variable_id = NCDF_VARID( ncId         , $  ; Input
                                variable_name  )  ; Input
      IF ( variable_id LT 0 ) THEN $
        MESSAGE, 'Variable ' + variable_name + ' not present in ' + ncFile, $
                 /NONAME, /NOPRINT
    ENDELSE

    ; Get the variable info
    variable_info = NCDF_VARINQ( ncId       , $  ; Input
                                 variable_id  )  ; Input

    ; Make sure we have the variable name
    variable_name = variable_info.NAME

    ; Does the current variable have dimensions?
    IF ( variable_info.NDIMS EQ 0 ) THEN BEGIN
      ; No. It is scalar. Simply read it.
      NCDF_VARGET, ncId         , $  ; Input
                   variable_id  , $  ; Input
                   variable_data     ; Output

    ENDIF ELSE BEGIN
      ; Yes. Check COUNT, OFFSET, and STRIDE vectors for this variable.
      result = check_vectors( ncId         , $  ; Input
                              variable_info, $  ; Input
                              count        , $  ; Input
                              offset       , $  ; Input
                              stride       , $  ; Input
                              count_vector , $  ; Output
                              offset_vector, $  ; Output
                              stride_vector  )  ; Output
      IF ( result NE SUCCESS ) THEN BEGIN
        MESSAGE, 'COUNT, OFFSET, and/or STRIDE vector check failed.', /NONAME, /NOPRINT
      ENDIF
;     Read the variable data
      NCDF_VARGET, ncId                  , $  ; Input
                   variable_id           , $  ; Input
                   variable_data         , $  ; Output
                   COUNT  = count_vector , $  ; Input keyword
                   OFFSET = offset_vector, $  ; Input keyword
                   STRIDE = stride_vector     ; Input keyword
    ENDELSE ; Variable dimensions IF statement

;   If necessary and required, convert BYTE/CHAR variable data to STRING
    variable_data = convert_string( variable_info.datatype            , $  ; Input
                                    variable_data                     , $  ; Input
                                    NO_CONVERT = no_var_byte_to_string  )  ; Input keyword

;   Retrieve the variable attributes if required
    n_variable_attributes = variable_info.NATTS
    IF ( KEYWORD_SET( variable_attributes ) AND $
         n_variable_attributes GT 0             ) THEN BEGIN

      ; Create the variable data structure with generic DATA tag
      variable_data = CREATE_STRUCT( 'data'       , $  ; Input
                                     variable_data  )  ; Input

      ; Loop over current variable's attribute
      FOR j = 0, n_variable_attributes - 1 DO BEGIN

        ; Get the current attribute name
        attribute_name = NCDF_ATTNAME( ncId       , $  ; Input
                                       variable_id, $  ; Input
                                       j            )  ; Input

        ; Get the current attribute value
        NCDF_ATTGET, ncId          , $  ; Input
                     variable_id   , $  ; Input
                     attribute_name, $  ; Input
                     attribute          ; Output

        ; Get the current attribute info
        attribute_info = NCDF_ATTINQ( ncId          , $  ; Input
                                      variable_id   , $  ; Input
                                      attribute_name  )  ; Input

        ; If necessary and required, convert BYTE attribute to STRING
        attribute = convert_string( attribute_info.datatype           , $  ; Input
                                    attribute                         , $  ; Input
                                    NO_CONVERT = no_att_byte_to_string  )  ; Input keyword

        ; Add current attribute to variable structure
        variable_data = CREATE_STRUCT( variable_data , $  ; Input
                                       attribute_name, $  ; Input
                                       attribute       )  ; Input

      ENDFOR ; Loop over current variable attributes
    ENDIF ; Get attributes IF statement

    ; Append data to return structure
    data = CREATE_STRUCT( data                    , $  ; Input
                          variable_name           , $  ; Input
                          TEMPORARY(variable_data)  )  ; Input

  ENDFOR  ; Loop over requested variables


  ; Close the netCDF data file
  Done:
  NCDF_CLOSE, ncId


  ; Done
  RETURN, SUCCESS

END ; FUNCTION Read_netCDF

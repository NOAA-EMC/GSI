!
! RadDiag_Stats_2D_IO
!
! Module containing routines to read and write RadDiag_Stats_2D files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 01-Jun-2009
!                       paul.vandelst@noaa.gov (1D version)
!
! 13th July 2010    2D version.         Andrew Collard.
!

MODULE RadDiag_Stats_2D_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: sp=>Single
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message
  USE File_Utility         , ONLY: File_Exists
  USE String_Utility       , ONLY: StrClean
  USE RadDiag_Stats_2D_Define , ONLY: N_VARIABLES, &
                                      VNSL       , &
                                      RadDiag_Stats_2D_type         , &
                                      RadDiag_Stats_2D_Associated   , &
                                      RadDiag_Stats_2D_Destroy      , &
                                      RadDiag_Stats_2D_Create       , &
                                      RadDiag_Stats_2D_Inspect      , &
                                      RadDiag_Stats_2D_ValidRelease , &
                                      RadDiag_Stats_2D_Info         , &
                                      RadDiag_Stats_2D_DefineVersion
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: RadDiag_Stats_2D_InquireFile
  PUBLIC :: RadDiag_Stats_2D_ReadFile
  PUBLIC :: RadDiag_Stats_2D_WriteFile


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module version
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: RadDiag_Stats_2D_netCDF_IO.f90 8661 2010-07-01 21:21:17Z andrew.collard@noaa.gov $'
  ! Default msg string length
  INTEGER, PARAMETER :: ML = 1024
  
  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment' 
  
  ! Dimension names. Case sensitive
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_DIMNAME      = 'n_Channels'
  CHARACTER(*), PRIVATE, PARAMETER :: LATITUDE_DIMNAME     = 'n_Latitudes'
  CHARACTER(*), PRIVATE, PARAMETER :: LONGITUDE_DIMNAME    = 'n_Longitudes'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_DIMNAME = 'n_Variables'
  CHARACTER(*), PRIVATE, PARAMETER :: VNSL_DIMNAME         = 'vnsl'

  ! Variable names. Case sensitive.
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_VARNAME       = 'Channel'
  CHARACTER(*), PRIVATE, PARAMETER :: AIRMASSCOEFF_VARNAME  = 'AirMassCoefficients'
  CHARACTER(*), PRIVATE, PARAMETER :: LONGITUDE_VARNAME     = 'Longitude'
  CHARACTER(*), PRIVATE, PARAMETER :: LATITUDE_VARNAME      = 'Latitude'
  CHARACTER(*), PRIVATE, PARAMETER :: MEAN_VARNAME          = 'Mean'
  CHARACTER(*), PRIVATE, PARAMETER :: STDDEV_VARNAME        = 'StdDev'
  CHARACTER(*), PRIVATE, PARAMETER :: NSAMPLES_VARNAME      = 'nSamples'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_VARNAME  = 'VariableNames'

  ! Variable description attribute.
  CHARACTER(*), PRIVATE, PARAMETER :: DESCRIPTION_ATTNAME = 'description'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_DESCRIPTION  = 'List of the names of the data variables'
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_DESCRIPTION       = 'Sensor channel number (not necessarily contiguous)'
  CHARACTER(*), PRIVATE, PARAMETER :: LATITUDE_DESCRIPTION      = 'The central latitudes for the latitude boxes'
  CHARACTER(*), PRIVATE, PARAMETER :: LONGITUDE_DESCRIPTION     = 'The central longitudes for the longitude boxes'
  CHARACTER(*), PRIVATE, PARAMETER :: MEAN_DESCRIPTION     = &
       'Mean Obs-Calc differences for each lat/lon box'
  CHARACTER(*), PRIVATE, PARAMETER :: STDDEV_DESCRIPTION   = &
       'Standard Deviation of Obs-Calc differences for each lat/lon box'
  CHARACTER(*), PRIVATE, PARAMETER :: NSAMPLES_DESCRIPTION = 'Number of valid samples for each lat/lon box'

  ! Variable long name attribute.
  CHARACTER(*), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_LONGNAME  = 'Data variable names'
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_LONGNAME       = 'Channel number'
  CHARACTER(*), PRIVATE, PARAMETER :: LATITUDE_LONGNAME      = 'Latitudes'
  CHARACTER(*), PRIVATE, PARAMETER :: LONGITUDE_LONGNAME     = 'Longitudes'
  CHARACTER(*), PRIVATE, PARAMETER :: MEAN_LONGNAME          = 'Means'
  CHARACTER(*), PRIVATE, PARAMETER :: STDDEV_LONGNAME        = 'Std Devs'
  CHARACTER(*), PRIVATE, PARAMETER :: NSAMPLES_LONGNAME      = 'Number of valid samples'

  ! Variable units attribute.
  CHARACTER(*), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_UNITS  = 'N/A'
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_UNITS       = 'N/A'
  CHARACTER(*), PRIVATE, PARAMETER :: LATITUDE_UNITS      = 'degrees north'
  CHARACTER(*), PRIVATE, PARAMETER :: LONGITUDE_UNITS     = 'degrees east'
  CHARACTER(*), PRIVATE, PARAMETER :: MEAN_UNITS          = 'Kelvin'
  CHARACTER(*), PRIVATE, PARAMETER :: STDDEV_UNITS        = 'Kelvin'
  CHARACTER(*), PRIVATE, PARAMETER :: NSAMPLES_UNITS       = 'N/A'

  ! Variable _FillValue attribute.
  CHARACTER(*), PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_FILLVALUE  = '0'
  INTEGER,      PRIVATE, PARAMETER :: CHANNEL_FILLVALUE       = -1
  REAL(sp),     PRIVATE, PARAMETER :: LATITUDE_FILLVALUE      = -999_sp
  REAL(sp),     PRIVATE, PARAMETER :: LONGITUDE_FILLVALUE     = -999_sp
  REAL(sp),     PRIVATE, PARAMETER :: MEAN_FILLVALUE          = 0.0_sp
  REAL(sp),     PRIVATE, PARAMETER :: STDDEV_FILLVALUE        = 0.0_sp
  INTEGER,      PRIVATE, PARAMETER :: NSAMPLES_FILLVALUE      = -1

  ! Variable netCDF datatypes
  INTEGER, PRIVATE, PARAMETER :: VARIABLENAME_TYPE  = NF90_CHAR
  INTEGER, PRIVATE, PARAMETER :: CHANNEL_TYPE       = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: LATITUDE_TYPE      = NF90_FLOAT
  INTEGER, PRIVATE, PARAMETER :: LONGITUDE_TYPE     = NF90_FLOAT
  INTEGER, PRIVATE, PARAMETER :: MEAN_TYPE          = NF90_FLOAT
  INTEGER, PRIVATE, PARAMETER :: STDDEV_TYPE        = NF90_FLOAT
  INTEGER, PRIVATE, PARAMETER :: NSAMPLES_TYPE      = NF90_INT


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_InquireFile
!
! PURPOSE:
!       Function to inquire a RadDiag_Stats file to obtain the dimensions
!       and global attributes.
!
! CALLING SEQUENCE:
!       err_stat = RadDiag_Stats_2D_InquireFile( &
!                    Filename                           , &
!                    n_Channels       = n_Channels      , &
!                    n_Latitudes      = n_Latitudes     , &
!                    n_Longitudes     = n_Longitudes    , &
!                    n_Variables      = n_Variables     , &
!                    Release          = Release         , &
!                    Version          = Version         , &
!                    Sensor_Id        = Sensor_Id       , &
!                    WMO_Satellite_Id = WMO_Satellite_Id, &
!                    WMO_Sensor_Id    = WMO_Sensor_Id   , &
!                    Title            = Title           , &
!                    History          = History         , &
!                    Comment          = Comment           )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           RadDiag_Stats data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Channels:         The number of spectral channels for the sensor.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Latitudes:        The number of latitude bins.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Longitudes:       The number of longitude bins.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Variables:        The number of radiance diagnostic variables for
!                           which statistics have been computed.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the RadDiag_Stats file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the RadDiag_Stats file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the RadDiag_Stats file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the RadDiag_Stats file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the RadDiag_Stats file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       err_stat:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquiry was successful
!                              == FAILURE an error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION RadDiag_Stats_2D_InquireFile( &
    Filename        , &  ! Input
    n_Channels      , &  ! Optional output
    n_Latitudes     , &  ! Optional output
    n_Longitudes    , &  ! Optional output
    n_Variables     , &  ! Optional output
    Release         , &  ! Optional output
    Version         , &  ! Optional output
    Sensor_Id       , &  ! Optional output
    WMO_Satellite_Id, &  ! Optional output
    WMO_Sensor_Id   , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Channels      
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Latitudes    
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Longitudes        
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Variables     
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release         
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Stats_2D_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: DimId
    TYPE(RadDiag_Stats_2D_type) :: rds

    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.

    ! Open the file
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Get the dimensions
    ! ...n_Channels dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,CHANNEL_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=rds%n_Channels )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Latitudes dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,LATITUDE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LATITUDE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=rds%n_Latitudes )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//LATITUDE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Longitudes dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,LONGITUDE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LONGITUDE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=rds%n_Longitudes )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//LONGITUDE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Variables dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,VARIABLENAME_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//VARIABLENAME_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=rds%n_Variables )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//VARIABLENAME_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileId  , &
                          Release          = rds%Release         , &
                          Version          = rds%Version         , &
                          Sensor_Id        = rds%Sensor_Id       , &
                          WMO_Satellite_Id = rds%WMO_Satellite_Id, &
                          WMO_Sensor_Id    = rds%WMO_Sensor_Id   , &
                          Title            = Title  , &
                          History          = History, &
                          Comment          = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Channels      ) ) n_Channels       = rds%n_Channels  
    IF ( PRESENT(n_Latitudes     ) ) n_Latitudes      = rds%n_Latitudes      
    IF ( PRESENT(n_Longitudes    ) ) n_Longitudes     = rds%n_Longitudes      
    IF ( PRESENT(n_Variables     ) ) n_Variables      = rds%n_Variables 
    IF ( PRESENT(Release         ) ) Release          = rds%Release
    IF ( PRESENT(Version         ) ) Version          = rds%Version
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = rds%Sensor_Id
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = rds%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = rds%WMO_Sensor_Id   

  CONTAINS
 
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION RadDiag_Stats_2D_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_WriteFile
!
! PURPOSE:
!       Function to write RadDiag_Stats data to file.
!
! CALLING SEQUENCE:
!       Error_Status = RadDiag_Stats_WriteFile( &
!                        Filename         , &  ! Input
!                        RadDiag_Stats    , &  ! Input
!                        Quiet   = Quiet  , &  ! Optional input
!                        Title   = Title  , &  ! Optional input
!                        History = History, &  ! Optional input
!                        Comment = Comment  )  ! Optional input
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       RadDiag_Stats data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RadDiag_Stats:  Structure containing the radiance diagnostic
!                       statistics data to write to file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Stats_2D_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the RadDiag_Stats file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the RadDiag_Stats file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the RadDiag_Stats file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION RadDiag_Stats_2D_WriteFile( &
    Filename     , &  ! Input
    RadDiag_Stats, &  ! Input
    Quiet        , &  ! Optional input
    Title        , &  ! Optional input
    History      , &  ! Optional input
    Comment      ) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN) :: Filename
    TYPE( RadDiag_Stats_2D_type ), INTENT(IN) :: RadDiag_Stats
    LOGICAL,          OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*),     OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*),     OPTIONAL, INTENT(IN) :: History
    CHARACTER(*),     OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Stats_2D_WriteFile'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: IsValid
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: VarId
    INTEGER :: n_Latitudes, n_Longitudes

    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check structure pointer association status
    IF ( .NOT. RadDiag_Stats_2D_Associated( RadDiag_Stats ) ) THEN
      msg = 'RadDiag_Stats structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IsValid = RadDiag_Stats_2D_ValidRelease( RadDiag_Stats )
    IF ( .NOT. IsValid ) THEN
      msg = 'RadDiag_Stats Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet

    n_Latitudes  = RadDiag_Stats%n_Latitudes 
    n_Longitudes = RadDiag_Stats%n_Longitudes 

    ! Create the output file
    err_stat = CreateFile_2D( &
                 Filename                  , &  ! Input
                 RadDiag_Stats%n_Channels  , &  ! Input
                 RadDiag_Stats%n_Latitudes , &  ! Input
                 RadDiag_Stats%n_Longitudes, &  ! Input
                 FileId                    , &  ! Output
                 Version          = RadDiag_Stats%Version         , &  ! Optional input
                 Sensor_Id        = RadDiag_Stats%Sensor_Id       , &  ! Optional input
                 WMO_Satellite_Id = RadDiag_Stats%WMO_Satellite_Id, &  ! Optional input
                 WMO_Sensor_Id    = RadDiag_Stats%WMO_Sensor_Id   , &  ! Optional input                           Title         = Title,         &  ! Optional input
                 Title            = Title                         , &  ! Optional input
                 History          = History                       , &  ! Optional input
                 Comment          = Comment                         )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Write the data items
    ! ...Channel variable 
    NF90_Status = NF90_INQ_VARID( FileId,CHANNEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,RadDiag_Stats%Channel )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//CHANNEL_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Latitude variable 
    NF90_Status = NF90_INQ_VARID( FileId,LATITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LATITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,RadDiag_Stats%LATITUDE(1:n_Latitudes) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LATITUDE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Longitude variable 
    NF90_Status = NF90_INQ_VARID( FileId,LONGITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LONGITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,RadDiag_Stats%LONGITUDE(1:n_Longitudes) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LONGITUDE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Mean variable 
    NF90_Status = NF90_INQ_VARID( FileId,MEAN_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MEAN_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,RadDiag_Stats%Mean )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//MEAN_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Stddev variable 
    NF90_Status = NF90_INQ_VARID( FileId,STDDEV_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//STDDEV_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,RadDiag_Stats%StdDev )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//STDDEV_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...nSamples variable 
    NF90_Status = NF90_INQ_VARID( FileId,NSAMPLES_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//NSAMPLES_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,RadDiag_Stats%nSamples )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//NSAMPLES_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...VariableNames variable 
    NF90_Status = NF90_INQ_VARID( FileId,VARIABLENAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//VARIABLENAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,RadDiag_Stats%VariableNames )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//VARIABLENAME_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL RadDiag_Stats_2D_Info( RadDiag_Stats, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      ! Close file if necessary
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION RadDiag_Stats_2D_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_ReadFile
!
! PURPOSE:
!       Function to read RadDiag_Stats data from file.
!
! CALLING SEQUENCE:
!       Error_Status = RadDiag_Stats_ReadFile( &
!                        Filename         , &  ! Input
!                        RadDiag_Stats    , &  ! Output
!                        Quiet   = Quiet  , &  ! Optional input
!                        Title   = Title  , &  ! Optional output
!                        History = History, &  ! Optional output
!                        Comment = Comment  )  ! Optional output
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       RadDiag_Stats data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       RadDiag_Stats:  Structure containing the radiance diagnostic
!                       statistics data read from file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Stats_2D_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the RadDiag_Stats file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the RadDiag_Stats file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the RadDiag_Stats file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION RadDiag_Stats_2D_ReadFile( &
    Filename     , &  ! Input
    RadDiag_Stats, &  ! Output
    Quiet        , &  ! Optional input
    Title        , &  ! Optional output
    History      , &  ! Optional output
    Comment      ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE( RadDiag_Stats_2D_type ), INTENT(OUT) :: RadDiag_Stats
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Stats_2D_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: IsValid
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: n_Channels   
    INTEGER :: n_Latitudes   
    INTEGER :: n_Longitudes   
    INTEGER :: File_n_Variables
    INTEGER :: VarId


    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet


    ! Inquire the file to get the dimensions
    err_stat = RadDiag_Stats_2D_InquireFile( Filename, &
                                            n_Channels   = n_Channels      , &
                                            n_Latitudes  = n_Latitudes     , &
                                            n_Longitudes = n_Longitudes    , &
                                            n_Variables  = File_n_Variables  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining RadDiag_Stats dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check the number of variables
    IF ( N_VARIABLES /= File_n_Variables ) THEN
      msg = 'Mismatch between variable count!'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL RadDiag_Stats_2D_Create( RadDiag_Stats, &
                                  n_Channels   , &
                                  n_Latitudes  , &
                                  n_Longitudes)
    IF ( .NOT. RadDiag_Stats_2D_Associated(RadDiag_Stats) ) THEN
      msg = 'Error allocating output RadDiag_Stats'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file for reading
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileID  , &
                          Release          = RadDiag_Stats%Release         , &
                          Version          = RadDiag_Stats%Version         , &
                          Sensor_Id        = RadDiag_Stats%Sensor_Id       , &
                          WMO_Satellite_Id = RadDiag_Stats%WMO_Satellite_Id, &
                          WMO_Sensor_Id    = RadDiag_Stats%WMO_Sensor_Id   , &
                          Title            = Title                         , &
                          History          = History                       , &
                          Comment          = Comment                         )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IsValid = RadDiag_Stats_2D_ValidRelease( RadDiag_Stats )
    IF ( .NOT. IsValid ) THEN
      msg = 'RadDiag_Stats Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Read the RadDiag_Stats data
    ! ...Channel variable 
    NF90_Status = NF90_INQ_VARID( FileId,CHANNEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,RadDiag_Stats%Channel )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//CHANNEL_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Latitude variable 
    NF90_Status = NF90_INQ_VARID( FileId,LATITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LATITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,RadDiag_Stats%Latitude )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LATITUDE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Longitude variable 
    NF90_Status = NF90_INQ_VARID( FileId,LONGITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LONGITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,RadDiag_Stats%Longitude )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LONGITUDE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Mean variable 
    NF90_Status = NF90_INQ_VARID( FileId,MEAN_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MEAN_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,RadDiag_Stats%Mean )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//MEAN_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
     ! ...Stddev variable 
    NF90_Status = NF90_INQ_VARID( FileId,STDDEV_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//STDDEV_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,RadDiag_Stats%StdDev )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//STDDEV_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...nSamples variable 
    NF90_Status = NF90_INQ_VARID( FileId,NSAMPLES_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//NSAMPLES_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,RadDiag_Stats%nSamples )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//NSAMPLES_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...VariableNames variable 
    NF90_Status = NF90_INQ_VARID( FileId,VARIABLENAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//VARIABLENAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,RadDiag_Stats%VariableNames )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//VARIABLENAME_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL RadDiag_Stats_2D_Info( RadDiag_Stats, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
 
    SUBROUTINE Read_CleanUp()
      ! Close file if necessary
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      ! Destroy the structure if necessary
      IF ( RadDiag_Stats_2D_Associated( RadDiag_Stats ) ) &
        CALL RadDiag_Stats_2D_Destroy( RadDiag_Stats )
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION RadDiag_Stats_2D_ReadFile



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a RadDiag_Stats data file.

  FUNCTION WriteGAtts( &
    Filename        , &  ! Input
    FileId          , &  ! Input
    Version         , &  ! Optional input
    Sensor_Id       , &  ! Optional input
    WMO_Satellite_Id, &  ! Optional input
    WMO_Sensor_Id   , &  ! Optional input
    Title           , &  ! Optional input
    History         , &  ! Optional input
    Comment         ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER,                INTENT(IN) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Stats_WriteGAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    INTEGER, PARAMETER :: nPutGAtts = 7
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: Ver
    INTEGER :: NF90_Status
    TYPE(RadDiag_Stats_2D_type) :: rds

    ! Set up
    err_stat = SUCCESS
    msg = ' '

    ! Mandatory global attributes
    ! ...Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),MODULE_VERSION_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),rds%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = rds%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),history )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    
 CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( FileId )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(msg), &
                            err_stat )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


  ! Function to read the global attributes from a RadDiag_Stats data file.

  FUNCTION ReadGAtts( &
    Filename        , &  ! Input
    FileId          , &  ! Input
    Release         , &  ! Optional output
    Version         , &  ! Optional output
    Sensor_Id       , &  ! Optional output
    WMO_Satellite_Id, &  ! Optional output
    WMO_Sensor_Id   , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(IN)  :: FileId
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Stats_ReadGAtts'
    ! Local variables
    CHARACTER(ML)   :: msg
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: NF90_Status
    INTEGER :: Rel
    TYPE(RadDiag_Stats_2D_type) :: rds
    
    ! Set up
    err_stat = SUCCESS

    ! The mandatory GAtts for checking
    ! ..The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),Rel )
    IF ( NF90_Status /= NF90_NOERR .OR. Rel /= rds%Release) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    IF ( PRESENT(Release) ) Release = rds%Release
    
    
    ! The optional GAtts
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME; Sensor_Id = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      Sensor_Id = GAttString(1:MIN(LEN(Sensor_Id), LEN_TRIM(GAttString)))
    END IF
    ! ...The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      GAttName = TITLE_GATTNAME; title = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      title = GAttString(1:MIN(LEN(title), LEN_TRIM(GAttString)))
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      GAttName = HISTORY_GATTNAME; history = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),history )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      history = GAttString(1:MIN(LEN(history), LEN_TRIM(GAttString)))
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      GAttName = COMMENT_GATTNAME; comment = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      comment = GAttString(1:MIN(LEN(comment), LEN_TRIM(GAttString)))
    END IF

  CONTAINS

    SUBROUTINE ReadGAtts_CleanUp()
      err_stat = FAILURE
      msg = 'Error reading '//TRIM(GAttName)//' attribute from '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ) )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts



  ! Function to create a RadDiag_Stats file for writing

  FUNCTION CreateFile_2D( &
    Filename        , &  ! Input
    n_Channels      , &  ! Input
    n_Latitudes     , &  ! Input
    n_Longitudes    , &  ! Input
    FileId          , &  ! Output
    Version         , &  ! Optional input
    Sensor_Id       , &  ! Optional input
    WMO_Satellite_Id, &  ! Optional input
    WMO_Sensor_Id   , &  ! Optional input
    Title           , &  ! Optional input
    History         , &  ! Optional input
    Comment         ) &  ! Optional input
  RESULT( err_stat )

    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(IN)  :: n_Channels   
    INTEGER,                INTENT(IN)  :: n_Latitudes  
    INTEGER,                INTENT(IN)  :: n_Longitudes 
    INTEGER,                INTENT(OUT) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Stats_2D_CreateFile'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: n_Channels_DimID
    INTEGER :: n_Latitudes_DimID
    INTEGER :: n_Longitudes_DimID
    INTEGER :: N_VARIABLES_DimID
    INTEGER :: VNSL_DimID
    INTEGER :: varID
    INTEGER :: Put_Status(4)

    ! Setup
    err_stat = SUCCESS
    Close_File = .FALSE.
    IF ( n_Channels   < 1 .OR. &
         n_Latitudes  < 1 .OR. &
         n_Longitudes < 1 ) THEN
      msg = 'Invalid dimension input.'
      CALL Create_Cleanup(); RETURN
    END IF    


    ! Create the data file
    NF90_Status = NF90_CREATE( Filename,NF90_CLOBBER,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Define the dimensions
     ! ...Number of spectral channels
    NF90_Status = NF90_DEF_DIM( FileID,CHANNEL_DIMNAME,n_Channels,n_Channels_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of Latitude boxes
    NF90_Status = NF90_DEF_DIM( FileID,LATITUDE_DIMNAME,n_Latitudes,n_Latitudes_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LATITUDE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of Longitude boxes
    NF90_Status = NF90_DEF_DIM( FileID,LONGITUDE_DIMNAME,n_Longitudes,n_Longitudes_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LONGITUDE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of variables for which statistics are accumulated
    NF90_Status = NF90_DEF_DIM( FileID,VARIABLENAME_DIMNAME,N_VARIABLES,N_VARIABLES_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//VARIABLENAME_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...String length for variable name
    NF90_Status = NF90_DEF_DIM( FileID,VNSL_DIMNAME,VNSL,VNSL_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//VNSL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts( Filename, &
                           FileId  , &
                           Version          = Version         , &
                           Sensor_Id        = Sensor_Id       , &
                           WMO_Satellite_Id = WMO_Satellite_Id, &
                           WMO_Sensor_Id    = WMO_Sensor_Id   , &
                           Title            = Title           , &
                           History          = History         , &
                           Comment          = Comment           )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attribute to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the variables
    ! ...Channel variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                CHANNEL_VARNAME, &
                                CHANNEL_TYPE, &
                                dimIDs=(/n_Channels_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,CHANNEL_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,CHANNEL_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,CHANNEL_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,CHANNEL_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//CHANNEL_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...LATITUDE variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                LATITUDE_VARNAME, &
                                LATITUDE_TYPE, &
                                dimIDs=(/n_Latitudes_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LATITUDE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,LATITUDE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,LATITUDE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,LATITUDE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,LATITUDE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//LATITUDE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...LONGITUDE variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                LONGITUDE_VARNAME, &
                                LONGITUDE_TYPE, &
                                dimIDs=(/n_Longitudes_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LONGITUDE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,LONGITUDE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,LONGITUDE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,LONGITUDE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,LONGITUDE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//LONGITUDE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Mean variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                MEAN_VARNAME, &
                                MEAN_TYPE, &
                                dimIDs=(/n_Variables_DimID,n_Channels_DimID,&
                                        n_Longitudes_DimID,n_Latitudes_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//MEAN_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,MEAN_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,MEAN_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,MEAN_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,MEAN_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//MEAN_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...StdDev variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                STDDEV_VARNAME, &
                                STDDEV_TYPE, &
                                dimIDs=(/n_Variables_DimID,n_Channels_DimID,&
                                        n_Longitudes_DimID,n_Latitudes_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//STDDEV_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,STDDEV_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,STDDEV_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,STDDEV_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,STDDEV_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//STDDEV_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...nSamples variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                NSAMPLES_VARNAME, &
                                NSAMPLES_TYPE, &
                                dimIDs=(/n_Channels_DimID,&
                                        n_Longitudes_DimID,n_Latitudes_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//NSAMPLES_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,NSAMPLES_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,NSAMPLES_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,NSAMPLES_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,NSAMPLES_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//NSAMPLES_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...VariableNames variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                VARIABLENAME_VARNAME, &
                                VARIABLENAME_TYPE, &
                                dimIDs=(/VNSL_DimID, n_Variables_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//VARIABLENAME_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,VARIABLENAME_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,VARIABLENAME_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,VARIABLENAME_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,VARIABLENAME_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//VARIABLENAME_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    NF90_Status = NF90_ENDDEF( FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error taking file '//TRIM(Filename)// &
            ' out of define mode - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
 
    SUBROUTINE Create_CleanUp()
      ! Close file
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileID )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION CreateFile_2D

END MODULE RadDiag_Stats_2D_netCDF_IO

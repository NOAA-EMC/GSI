!
! RadDiag_IO
!
! Module to read GSI radiance diagnostic files
!

MODULE RadDiag_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds,      ONLY: sp=>Single
  USE File_Utility,    ONLY: Get_Lun, File_Open
  USE Message_Handler, ONLY: SUCCESS, FAILURE, EOF, Display_Message
  USE RadDiag_Define,  ONLY: RadDiag_Hdr_Scalar_type , &
                             RadDiag_Hdr_Channel_type, &
                             RadDiag_Hdr_type        , &
                             RadDiag_Hdr_Associated  , &
                             RadDiag_Hdr_Destroy     , &
                             RadDiag_Hdr_Create      , &
                             RADDIAG_N_FPELEMENTS, &
                             RADDIAG_N_CHELEMENTS, &
                             RADDIAG_N_PRELEMENTS, &
                             RadDiag_Data_Scalar_type , &
                             RadDiag_Data_Channel_type, &
                             RadDiag_Data_type        , &
                             RadDiag_Data_Associated  , &
                             RadDiag_Data_Destroy     , &
                             RadDiag_Data_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ---------------------
  ! Explicit visibilities
  ! ---------------------
  PRIVATE
  ! Inherited derived type definitions
  PUBLIC :: RadDiag_Hdr_type
  PUBLIC :: RadDiag_Data_type
  ! Inherited module subprograms
  PUBLIC :: RadDiag_Hdr_Associated
  PUBLIC :: RadDiag_Hdr_Destroy
  PUBLIC :: RadDiag_Hdr_Create
  PUBLIC :: RadDiag_Data_Associated
  PUBLIC :: RadDiag_Data_Destroy
  PUBLIC :: RadDiag_Data_Create
  ! Module parameters
  PUBLIC :: RADDIAG_READMODE  
  PUBLIC :: RADDIAG_WRITEMODE 
  PUBLIC :: RADDIAG_APPENDMODE
  ! Module subprograms
  PUBLIC :: RadDiag_OpenFile
  PUBLIC :: RadDiag_Hdr_ReadFile
  PUBLIC :: RadDiag_Data_ReadFile
  PUBLIC :: RadDiag_Hdr_WriteFile
  PUBLIC :: RadDiag_Data_WriteFile
  PUBLIC :: RadDiag_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Defined file access modes
  INTEGER,      PARAMETER :: RADDIAG_READMODE   = 1
  INTEGER,      PARAMETER :: RADDIAG_WRITEMODE  = 2
  INTEGER,      PARAMETER :: RADDIAG_APPENDMODE = 3
  CHARACTER(*), PARAMETER :: RADDIAG_MODENAME(3) = (/'read  ','write ','append'/)
  ! Module version id
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256


CONTAINS


!------------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_OpenFile
!
! PURPOSE:
!       Function to open a GSI radiance diagnostic file for reading or writing.
!
! CALLING SEQUENCE:
!       Error_Status = RadDiag_OpenFile( Filename,               &  ! Input
!                                        FileID,                 &  ! Output
!                                        AccessMode = AccessMode )  ! Optional input
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       GSI Radiance diagnostic data file to open.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       FileID:         File logical unit number to be used for for subsequent
!                       file access. Value is set to zero if an error occurs.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       AccessMode:     Integer flag specifying the type of file access required.
!                       Valid parameter values are:
!                         RADDIAG_READMODE:   Open existing file for reading. [DEFAULT]
!                         RADDIAG_WRITEMODE:  Open new file for writing. 
!                         RADDIAG_APPENDMODE: Open existing file for writing.
!                       If not specified, RADDIAG_READMODE is the default.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file open was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------------

  FUNCTION RadDiag_OpenFile( &
    Filename   , &  ! Input
    FileID     , &  ! Output
    AccessMode ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Filename
    INTEGER,           INTENT(OUT) :: FileID    
    INTEGER, OPTIONAL, INTENT(IN)  :: AccessMode
    ! Function result
    INTEGER :: err_stat
    ! Local Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_OpenFile'
    ! Local Variables
    CHARACTER(ML) :: msg
    INTEGER :: fid
    INTEGER :: mode
    INTEGER :: io_stat
    CHARACTER(10) :: File_Status
    CHARACTER(10) :: File_Position
    CHARACTER(10) :: File_Action


    ! Set up
    err_stat = SUCCESS
    FileID = 0
    ! ...Open the file for reading by default
    Mode = RADDIAG_READMODE
    IF ( PRESENT(AccessMode) ) Mode = AccessMode


    ! Assign the OPEN specifiers
    SELECT CASE ( Mode )
      CASE (RADDIAG_READMODE)
        File_Status   = 'OLD'
        File_Position = 'ASIS'
        File_Action   = 'READ'
      CASE (RADDIAG_WRITEMODE)
        File_Status   = 'REPLACE'
        File_Position = 'ASIS'
        File_Action   = 'WRITE'
      CASE (RADDIAG_APPENDMODE)
        File_Status   = 'UNKNOWN'
        File_Position = 'APPEND'
        File_Action   = 'READWRITE'
      CASE DEFAULT
        err_stat = FAILURE
        msg = 'Invalid RadDiag file access mode.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat )
        RETURN
    END SELECT


    ! Get a free unit number
    fid = Get_Lun()
    IF ( fid < 0 ) THEN
      err_stat = FAILURE
      msg = 'Error obtaining file unit number for '//TRIM(Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! Open the file
    OPEN( fid, FILE     = Filename, &
               STATUS   = File_Status, &
               POSITION = File_Position, &
               ACTION   = File_Action, &
               ACCESS   = 'SEQUENTIAL', &
               FORM     = 'UNFORMATTED', &
               IOSTAT   = io_stat )
    IF ( io_stat /= 0 ) THEN
      err_stat = FAILURE
      WRITE( msg,'("Error opening ",a," for ",a," access. IOSTAT = ",i0)' ) &
             TRIM(Filename), TRIM(RADDIAG_MODENAME(Mode)), io_stat
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! Assign the output argument
    FileID = fid

  END FUNCTION RadDiag_OpenFile


!------------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Hdr_ReadFile
!
! PURPOSE:
!       Function to read header data from a GSI radiance diagnostic file
!
! CALLING SEQUENCE:
!       Error_Status = RadDiag_Hdr_ReadFile( FileID,     &  ! Input
!                                            RadDiag_Hdr )  ! Output
!
! INPUTS:
!       FileID:         File logical unit number of the radiance diagnostic file
!                       to read. Returned from call to RadDiag_OpenFile() function.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       RadDiag_Hdr:    RadDiag header structure read from file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Hdr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file read was successful.
!                          == FAILURE an error occurred reading the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------------

  FUNCTION RadDiag_Hdr_ReadFile( &
    FileID      , &  ! Input
    RadDiag_Hdr ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER,                INTENT(IN)  :: FileID
    TYPE(RadDiag_Hdr_type), INTENT(OUT) :: RadDiag_Hdr
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Hdr_ReadFile'
    ! Local variables
    CHARACTER(ML)  :: msg
    CHARACTER(256) :: Filename
    INTEGER :: io_stat
    INTEGER :: i
    TYPE(RadDiag_Hdr_Scalar_type) :: Scalar

    ! Set up
    err_stat = SUCCESS
    ! ...Make sure the file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'File is not open.'
      CALL Hdr_Read_CleanUp(); RETURN
    END IF
    ! ...Get the filename
    INQUIRE( UNIT=FileID, NAME=Filename )


    ! Read the fixed part of the header
    READ( FileID, IOSTAT=io_stat ) Scalar
!    READ( FileID, IOSTAT=io_stat ) Scalar%isis   , &
!                                   Scalar%id     , &
!                                   Scalar%obstype, &
!                                   Scalar%jiter  , &
!                                   Scalar%nchan  , &
!                                   Scalar%npred  , &
!                                   Scalar%idate  , &
!                                   Scalar%ireal  , &
!                                   Scalar%ipchan , &
!                                   Scalar%iextra , &
!                                   Scalar%jextra 
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading RadDiag header fixed portion from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Hdr_Read_CleanUp(); RETURN
    END IF


    ! Check the header format/dimensions
    IF( Scalar%ireal  /= RADDIAG_N_FPELEMENTS .OR. &       ! Number of floating point elements
        Scalar%ipchan /= RADDIAG_N_CHELEMENTS .OR. &       ! Number of channel elements
        Scalar%npred  /= RADDIAG_N_PRELEMENTS      ) THEN  ! Number of bias correction terms
      msg = 'Invalid RadDiag_Hdr dimension values.'
      CALL Hdr_Read_CleanUp(); RETURN
    END IF


    ! Allocate the RadDiag_Hdr structure
    CALL RadDiag_Hdr_Create( RadDiag_Hdr, Scalar%nchan )
    IF ( .NOT. RadDiag_Hdr_Associated(RadDiag_Hdr) ) THEN
      msg = 'Error allocating RadDiag_Hdr structure'
      CALL Hdr_Read_CleanUp(); RETURN
    END IF


    ! Copy the fixed portion of the header
    RadDiag_Hdr%Scalar = Scalar


    ! Read the channel portion of the header
    DO i = 1, RadDiag_Hdr%n_Channels
      READ( FileID, IOSTAT=io_stat ) RadDiag_Hdr%Channel(i)
!      READ( FileID, IOSTAT=io_stat ) RadDiag_Hdr%Channel(i)%freq    , &
!                                     RadDiag_Hdr%Channel(i)%polar   , &
!                                     RadDiag_Hdr%Channel(i)%wave    , &
!                                     RadDiag_Hdr%Channel(i)%varch   , &
!                                     RadDiag_Hdr%Channel(i)%tlapmean, &
!                                     RadDiag_Hdr%Channel(i)%iuse    , &
!                                     RadDiag_Hdr%Channel(i)%nuchan  , &
!                                     RadDiag_Hdr%Channel(i)%iochan  
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading RadDiag header channel index ",i0,&
                    &" data from ",a,". IOSTAT = ",i0)' ) &
               i, TRIM(Filename), io_stat
        CALL Hdr_Read_CleanUp(); RETURN
      END IF
    END DO

  CONTAINS
  
    SUBROUTINE Hdr_Read_CleanUp()
      IF ( File_Open(FileId) ) THEN
        CLOSE( FileId,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL RadDiag_Hdr_Destroy( RadDiag_Hdr )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Hdr_Read_CleanUp
  
  END FUNCTION RadDiag_Hdr_ReadFile


!------------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Data_ReadFile
!
! PURPOSE:
!       Function to read data from a GSI radiance diagnostic file
!
! CALLING SEQUENCE:
!       Error_Status = RadDiag_Data_ReadFile( FileID,      &  ! Input
!                                             RadDiag_Hdr, &  ! Input
!                                             RadDiag_Data )  ! Output
!
! INPUTS:
!       FileID:         File unit number of the radiance diagnostic file to read.
!                       Returned from call to Open_RadDiag() function.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RadDiag_Hdr:    RadDiag header structure for the file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Hdr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       RadDiag_Data:   RadDiag data structure read from file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Data_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file read was successful.
!                          == EOF     the end-of-file was reached
!                          == FAILURE an error occurred reading the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------------

  FUNCTION RadDiag_Data_ReadFile( &
    FileID       , &  ! Input
    RadDiag_Hdr  , &  ! Input
    RadDiag_Data ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER,                 INTENT(IN)  :: FileID
    TYPE(RadDiag_Hdr_type),  INTENT(IN)  :: RadDiag_Hdr
    TYPE(RadDiag_Data_type), INTENT(OUT) :: RadDiag_Data
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Data_ReadFile'
    ! Local variables
    CHARACTER(ML)  :: msg
    CHARACTER(256) :: Filename
    INTEGER :: io_stat
    INTEGER :: i
    REAL(sp), DIMENSION(RadDiag_Hdr%Scalar%iextra) :: Extra

    ! Set up
    err_stat = SUCCESS
    ! ...Make sure the file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'File is not open.'
      CALL Data_Read_CleanUp(); RETURN
    END IF
    ! ...Get the filename
    INQUIRE( UNIT=FileID, NAME=Filename )


    ! Allocate the RadDiag_Data structure
    CALL RadDiag_Data_Create( RadDiag_Data, RadDiag_Hdr%n_Channels )
    IF ( .NOT. RadDiag_Data_Associated(RadDiag_Data) ) THEN
      msg = 'Error allocating RadDiag_Data structure'
      CALL Data_Read_CleanUp(); RETURN
    END IF


    ! Read all the data
    READ( FileID, IOSTAT=io_stat ) &
      RadDiag_Data%Scalar, &
      (RadDiag_Data%Channel(i), i=1,RadDiag_Data%n_Channels), &
      Extra
!      RadDiag_Data%Scalar%lat       , &
!      RadDiag_Data%Scalar%lon       , &
!      RadDiag_Data%Scalar%zsges     , &
!      RadDiag_Data%Scalar%obstime   , &
!      RadDiag_Data%Scalar%senscn_pos, &
!      RadDiag_Data%Scalar%satzen_ang, &
!      RadDiag_Data%Scalar%satazm_ang, &
!      RadDiag_Data%Scalar%solzen_ang, &
!      RadDiag_Data%Scalar%solazm_ang, &
!      RadDiag_Data%Scalar%sungln_ang, &
!      RadDiag_Data%Scalar%water_frac, &
!      RadDiag_Data%Scalar%land_frac , &
!      RadDiag_Data%Scalar%ice_frac  , &
!      RadDiag_Data%Scalar%snow_frac , &
!      RadDiag_Data%Scalar%water_temp, &
!      RadDiag_Data%Scalar%land_temp , &
!      RadDiag_Data%Scalar%ice_temp  , &
!      RadDiag_Data%Scalar%snow_temp , &
!      RadDiag_Data%Scalar%soil_temp , &
!      RadDiag_Data%Scalar%soil_mois , &
!      RadDiag_Data%Scalar%land_type , &
!      RadDiag_Data%Scalar%veg_frac  , &
!      RadDiag_Data%Scalar%snow_depth, &
!      RadDiag_Data%Scalar%sfc_wndspd, &
!      RadDiag_Data%Scalar%qcdiag1   , &
!      RadDiag_Data%Scalar%qcdiag2   , ( RadDiag_Data%Channel(i)%tbobs , &
!                                        RadDiag_Data%Channel(i)%omgbc , &
!                                        RadDiag_Data%Channel(i)%omgnbc, &
!                                        RadDiag_Data%Channel(i)%errinv, & 
!                                        RadDiag_Data%Channel(i)%qcmark, & 
!                                        RadDiag_Data%Channel(i)%emiss , &  
!                                        RadDiag_Data%Channel(i)%tlap  , & 
!                                        RadDiag_Data%Channel(i)%bifix , & 
!                                        RadDiag_Data%Channel(i)%bilap , &
!                                        RadDiag_Data%Channel(i)%bilap2, &
!                                        RadDiag_Data%Channel(i)%bicons, & 
!                                        RadDiag_Data%Channel(i)%biang , &  
!                                        RadDiag_Data%Channel(i)%biclw , & 
!                                        i=1,RadDiag_Data%n_Channels      ), Extra
    ! ...Check for error
    IF ( io_stat > 0 ) THEN
      WRITE( msg,'("Error reading RadDiag Data from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Data_Read_CleanUp(); RETURN
    END IF


    ! Check for end of file
    IF ( io_stat < 0 ) THEN
      err_stat = EOF
      WRITE( msg,'("End-of-file ",a," reached.")' ) TRIM(Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END IF

  CONTAINS
  
    SUBROUTINE Data_Read_CleanUp()
      IF ( File_Open(FileId) ) THEN
        CLOSE( FileId,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL RadDiag_Data_Destroy( RadDiag_Data )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Data_Read_CleanUp
  
  END FUNCTION RadDiag_Data_ReadFile


!------------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Hdr_WriteFile
!
! PURPOSE:
!       Function to write header data to a GSI radiance diagnostic file
!
! CALLING SEQUENCE:
!       Error_Status = RadDiag_Hdr_WriteFile( FileID,     &  ! Input
!                                             RadDiag_Hdr )  ! Input
!
! INPUTS:
!       FileID:         File logical unit number of the radiance diagnostic file
!                       to write. Returned from call to RadDiag_OpenFile() function.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RadDiag_Hdr:    RadDiag header structure to write to file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Hdr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file write was successful.
!                          == FAILURE an error occurred writing the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------------

  FUNCTION RadDiag_Hdr_WriteFile( &
    FileID      , &  ! Input
    RadDiag_Hdr ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER,                INTENT(IN)  :: FileID
    TYPE(RadDiag_Hdr_type), INTENT(IN)  :: RadDiag_Hdr
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Hdr_WriteFile'
    ! Local variables
    CHARACTER(ML)  :: msg
    CHARACTER(256) :: Filename
    INTEGER :: io_stat
    INTEGER :: i

    ! Set up
    err_stat = SUCCESS
    ! ...Make sure the file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'File is not open.'
      CALL Hdr_Write_CleanUp(); RETURN
    END IF
    ! ...Get the filename
    INQUIRE( UNIT=FileID, NAME=Filename )


    ! Write the fixed part of the header
    WRITE( FileID, IOSTAT=io_stat ) RadDiag_Hdr%Scalar
!    WRITE( FileID, IOSTAT=io_stat ) RadDiag_Hdr%Scalar%isis   , &
!                                    RadDiag_Hdr%Scalar%id     , &
!                                    RadDiag_Hdr%Scalar%obstype, &
!                                    RadDiag_Hdr%Scalar%jiter  , &
!                                    RadDiag_Hdr%Scalar%nchan  , &
!                                    RadDiag_Hdr%Scalar%npred  , &
!                                    RadDiag_Hdr%Scalar%idate  , &
!                                    RadDiag_Hdr%Scalar%ireal  , &
!                                    RadDiag_Hdr%Scalar%ipchan , &
!                                    RadDiag_Hdr%Scalar%iextra , &
!                                    RadDiag_Hdr%Scalar%jextra 
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing RadDiag header fixed portion to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Hdr_Write_CleanUp(); RETURN
    END IF


    ! Write the channel portion of the header
    DO i = 1, RadDiag_Hdr%n_Channels
      WRITE( FileID, IOSTAT=io_stat ) RadDiag_Hdr%Channel(i)
!      WRITE( FileID, IOSTAT=io_stat ) RadDiag_Hdr%Channel(i)%freq    , &
!                                      RadDiag_Hdr%Channel(i)%polar   , &
!                                      RadDiag_Hdr%Channel(i)%wave    , &
!                                      RadDiag_Hdr%Channel(i)%varch   , &
!                                      RadDiag_Hdr%Channel(i)%tlapmean, &
!                                      RadDiag_Hdr%Channel(i)%iuse    , &
!                                      RadDiag_Hdr%Channel(i)%nuchan  , &
!                                      RadDiag_Hdr%Channel(i)%iochan  
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing RadDiag header channel index ",i0,&
                    &" data to ",a,". IOSTAT = ",i0)' ) &
               i, TRIM(Filename), io_stat
        CALL Hdr_Write_CleanUp(); RETURN
      END IF
    END DO

  CONTAINS
  
    SUBROUTINE Hdr_Write_CleanUp()
      IF ( File_Open(FileId) ) THEN
        CLOSE( FileId,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) msg = TRIM(msg)//'; Error closing output file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Hdr_Write_CleanUp
  
  END FUNCTION RadDiag_Hdr_WriteFile


!------------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Data_WriteFile
!
! PURPOSE:
!       Function to write data to a GSI radiance diagnostic file
!
! CALLING SEQUENCE:
!       Error_Status = RadDiag_Data_WriteFile( FileID,      &  ! Input
!                                              RadDiag_Hdr, &  ! Input
!                                              RadDiag_Data )  ! Input
!
! INPUTS:
!       FileID:         File unit number of the radiance diagnostic file to write.
!                       Returned from call to Open_RadDiag() function.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RadDiag_Hdr:    RadDiag header structure for the file (already written).
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Hdr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RadDiag_Data:   RadDiag data structure to write to file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Data_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file write was successful.
!                          == FAILURE an error occurred writing the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------------

  FUNCTION RadDiag_Data_WriteFile( &
    FileID       , &  ! Input
    RadDiag_Hdr  , &  ! Input
    RadDiag_Data ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER,                 INTENT(IN) :: FileID
    TYPE(RadDiag_Hdr_type),  INTENT(IN) :: RadDiag_Hdr
    TYPE(RadDiag_Data_type), INTENT(IN) :: RadDiag_Data
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Data_WriteFile'
    ! Local variables
    CHARACTER(ML)  :: msg
    CHARACTER(256) :: Filename
    INTEGER :: io_stat
    INTEGER :: i
    REAL(sp), DIMENSION(RadDiag_Hdr%Scalar%iextra) :: Extra

    ! Set up
    err_stat = SUCCESS
    ! ...Make sure the file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'File is not open.'
      CALL Data_Write_CleanUp(); RETURN
    END IF
    ! ...Get the filename
    INQUIRE( UNIT=FileID, NAME=Filename )


    ! Write all the data
    Extra = 0.0_sp
    WRITE( FileID, IOSTAT=io_stat ) &
      RadDiag_Data%Scalar, &
      (RadDiag_Data%Channel(i), i=1,RadDiag_Data%n_Channels), &
      Extra
!      RadDiag_Data%Scalar%lat       , &
!      RadDiag_Data%Scalar%lon       , &
!      RadDiag_Data%Scalar%zsges     , &
!      RadDiag_Data%Scalar%obstime   , &
!      RadDiag_Data%Scalar%senscn_pos, &
!      RadDiag_Data%Scalar%satzen_ang, &
!      RadDiag_Data%Scalar%satazm_ang, &
!      RadDiag_Data%Scalar%solzen_ang, &
!      RadDiag_Data%Scalar%solazm_ang, &
!      RadDiag_Data%Scalar%sungln_ang, &
!      RadDiag_Data%Scalar%water_frac, &
!      RadDiag_Data%Scalar%land_frac , &
!      RadDiag_Data%Scalar%ice_frac  , &
!      RadDiag_Data%Scalar%snow_frac , &
!      RadDiag_Data%Scalar%water_temp, &
!      RadDiag_Data%Scalar%land_temp , &
!      RadDiag_Data%Scalar%ice_temp  , &
!      RadDiag_Data%Scalar%snow_temp , &
!      RadDiag_Data%Scalar%soil_temp , &
!      RadDiag_Data%Scalar%soil_mois , &
!      RadDiag_Data%Scalar%land_type , &
!      RadDiag_Data%Scalar%veg_frac  , &
!      RadDiag_Data%Scalar%snow_depth, &
!      RadDiag_Data%Scalar%sfc_wndspd, &
!      RadDiag_Data%Scalar%qcdiag1   , &
!      RadDiag_Data%Scalar%qcdiag2   , ( RadDiag_Data%Channel(i)%tbobs , &
!                                        RadDiag_Data%Channel(i)%omgbc , &
!                                        RadDiag_Data%Channel(i)%omgnbc, &
!                                        RadDiag_Data%Channel(i)%errinv, & 
!                                        RadDiag_Data%Channel(i)%qcmark, & 
!                                        RadDiag_Data%Channel(i)%emiss , &  
!                                        RadDiag_Data%Channel(i)%tlap  , & 
!                                        RadDiag_Data%Channel(i)%bifix , & 
!                                        RadDiag_Data%Channel(i)%bilap , &
!                                        RadDiag_Data%Channel(i)%bilap2, &
!                                        RadDiag_Data%Channel(i)%bicons, & 
!                                        RadDiag_Data%Channel(i)%biang , & 
!                                        RadDiag_Data%Channel(i)%biclw , &  
!                                        i=1,RadDiag_Data%nChannels      ), Extra
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing RadDiag Data to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Data_Write_CleanUp(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Data_Write_CleanUp()
      IF ( File_Open(FileId) ) THEN
        CLOSE( FileId,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) msg = TRIM(msg)//'; Error closing output file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Data_Write_CleanUp
  
  END FUNCTION RadDiag_Data_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL RadDiag_IOVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE RadDiag_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE RadDiag_IOVersion

END MODULE RadDiag_IO

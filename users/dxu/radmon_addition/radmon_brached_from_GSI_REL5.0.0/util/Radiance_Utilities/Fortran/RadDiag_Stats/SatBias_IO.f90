!
! SatBias_IO
!
! Module containing routines to read and write GSI SatBiasAngle
! and SatBiasAirMass data files.
!
! Written by:     Paul van Delst, CIMSS/SSEC 27-Dec-2005
!                 paul.vandelst@ssec.wisc.edu
!
MODULE SatBias_IO

  ! -----------
  ! Environment
  ! -----------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE SatInfo_Define
  USE SatBias_Define
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------------
  ! Module entity visibilities
  ! --------------------------
  PRIVATE
  ! Module procedures
  PUBLIC :: n_SatBiasAngle_Entries
  PUBLIC :: n_SatBiasAirMass_Entries
  PUBLIC :: Read_SatBias
  PUBLIC :: Write_SatBias
  ! Inherited entity visibilities
  ! Parameters from SatBias_Define
  PUBLIC :: MAX_SATBIAS_FOVS
  PUBLIC :: MAX_SATBIAS_PREDICTORS
  ! Derived type definitions from SatBias_Define
  PUBLIC :: SatBiasAngle_type
  PUBLIC :: SatBiasAirMass_type
  ! Module procedures from SatBias_Define
  PUBLIC :: SatBias_Clear
  PUBLIC :: SatBias_Zero
  PUBLIC :: SatBias_CountSensors


  ! ----------------
  ! Interface blocks
  ! ----------------
  INTERFACE Read_SatBias
    MODULE PROCEDURE Read_SatBiasAngle
    MODULE PROCEDURE Read_SatBiasAirMass
  END INTERFACE Read_SatBias

  INTERFACE Write_SatBias
    MODULE PROCEDURE Write_SatBiasAngle
    MODULE PROCEDURE Write_SatBiasAirMass
  END INTERFACE Write_SatBias


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: SATBIASANGLE_HEADER_FMTSTRING = '(i5,1x,a20,1x,i5,es15.6,:,es15.6,i5)'
  CHARACTER(*), PARAMETER :: SATBIASANGLE_BIAS_FMTSTRING   = '(9(4x,10f7.3/))'
  CHARACTER(*), PARAMETER :: SATBIASAIRMASS_FMTSTRING = '(i5,1x,a20,1x,i5,10(f12.6,:))'


CONTAINS


!----------------------------------------------------------------------------------
!
! Functions to count the number of entries (i.e. channels) in GSI SatBiasAngle
! and SatBiasAirMass data files.
!
! CALLING SEQUENCE:
!
!   For SatBias Angle files:
!   ------------------------
!     nEntries = n_SatBiasAngle_Entries( Filename,               &  ! Input
!                                        RCS_Id=RCS_Id,          &  ! Revision control
!                                        Message_Log=Message_Log )  ! Error messaging
!
!
!   For SatBias AirMass files:
!   --------------------------
!     nEntries = n_SatBiasAirMass_Entries( Filename,               &  ! Input
!                                          RCS_Id=RCS_Id,          &  ! Revision control
!                                          Message_Log=Message_Log )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!   Filename:     Character string specifying the name of the SatBiasAngle
!                 ir SatBiasAirMass data file to read.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:  Character string specifying a filename in which any
!                 messages will be logged. If not specified, or if an
!                 error occurs opening the log file, the default action
!                 is to output messages to standard output.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:       Character string containing the Revision Control
!                 System Id field for the module.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!   nEntries:     The number of SatBias entries present in the file.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Aug-2006
!                       paul.vandelst@ssec.wisc.edu
!----------------------------------------------------------------------------------

  ! ---------------------------------------------------------------
  ! Function to count the number of SatBiasAngle entries in a file
  ! --------------------------------------------------------------
  FUNCTION n_SatBiasAngle_Entries( Filename,     &  ! Input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( nEntries )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: nEntries
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'n_SatBiasAngle_Entries'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: j
    INTEGER :: FileID
    TYPE( SatBiasAngle_type ) :: Dummy

    ! Set up
    nEntries = 0
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Does the file exist?
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'SatBiasAngle file '//TRIM( Filename )//' not found.'
      GOTO 2000
    END IF

    ! Open the file
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Message = 'Error obtaining unit number for SatBiasAngle file '//TRIM( Filename )
      GOTO 2000
    END IF

    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error opening SatBiasAngle file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF


    ! Read the file
    Read_Loop: DO

      ! Read the entry header
      READ( FileID, FMT    = SATBIASANGLE_HEADER_FMTSTRING, &
                    IOSTAT = IO_Status       ) Dummy%Channel_Index, &
                                               Dummy%Sensor_Id, &
                                               Dummy%Channel, &
                                               Dummy%LapseRate_Mean, &
                                               Dummy%Bias_Mean, &
                                               Dummy%nFOVs

      ! Check for EOF
      IF ( IO_Status < 0 ) EXIT Read_Loop

      ! Increment the entry counter
      nEntries = nEntries + 1

      ! Check for read error
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SatBiasAngle file ", a, " entry # ", i5, &
                          &" header. IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), nEntries, IO_Status
        GOTO 1000
      END IF

      ! Read the entry bias values
      ! ** NOTE: The number of FOVs for each channel is currently the same.
      ! **       Eventually it will be sensor/channel dependent value.
      READ( FileID, FMT    = SATBIASANGLE_BIAS_FMTSTRING, &
                    IOSTAT = IO_Status       ) &
        (Dummy%Bias(j), j=1,MAX_SATBIAS_FOVS )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SatBiasAngle file ", a, " entry # ", i5, &
                          &" bias data. IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), nEntries, IO_Status
        GOTO 1000
      END IF

    END DO Read_Loop

    CLOSE( FileID )

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )
    2000 CONTINUE
    nEntries = 0
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          FAILURE, &
                          Message_Log = Message_Log )

  END FUNCTION n_SatBiasAngle_Entries


  ! ----------------------------------------------------------------
  ! Function to count the number of SatBiasAirMass entries in a file
  ! ----------------------------------------------------------------
  FUNCTION n_SatBiasAirMass_Entries( Filename,     &  ! Input
                                     RCS_Id,       &  ! Revision control
                                     Message_Log ) &  ! Error messaging
                                   RESULT ( nEntries )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: nEntries
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'n_SatBiasAirMass_Entries'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: j
    INTEGER :: FileID
    TYPE( SatBiasAirMass_type ) :: Dummy

    ! Set up
    nEntries = 0
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Does the file exist?
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'SatBiasAirMass file '//TRIM( Filename )//' not found.'
      GOTO 2000
    END IF

    ! Open the file
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Message = 'Error obtaining unit number for SatBiasAirMass file '//TRIM( Filename )
      GOTO 2000
    END IF

    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error opening SatBiasAirMass file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF


    ! Read the file
    Read_Loop: DO

      ! Read the entry values
      READ( FileID, FMT    = SATBIASAIRMASS_FMTSTRING, &
                    IOSTAT = IO_Status  ) Dummy%Channel_Index, &
                                          Dummy%Sensor_Id, &
                                          Dummy%Channel, &
                                          (Dummy%c(j), j=1,MAX_SATBIAS_PREDICTORS )
        
      ! Check for EOF
      IF ( IO_Status < 0 ) EXIT Read_Loop

      ! Update entry counter
      nEntries = nEntries + 1

      ! Check for read error
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SatBiasAirMass file ", a, " entry # ", i5, &
                          &". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), nEntries, IO_Status
        GOTO 1000
      END IF

    END DO Read_Loop

    CLOSE( FileID )

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )
    2000 CONTINUE
    nEntries = 0
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          FAILURE, &
                          Message_Log = Message_Log )

  END FUNCTION n_SatBiasAirMass_Entries


!----------------------------------------------------------------------------------
!
! Function to read GSI SatBiasAngle and SatBiasAirMass data files.
!
! CALLING SEQUENCE:
!   Error_Status = Read_SatBias( Filename,               &  ! Input
!                                SatBias,                &  ! Output
!                                SatInfo=SatInfo,        &  ! Optional input
!                                nEntries=nEntries,      &  ! Optional output
!                                RCS_Id=RCS_Id,          &  ! Revision control
!                                Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:     Character string specifying the name of a
!                 SatBiasAngle data file to read.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!   SatInfo:      Structure array used to index the SatBias data. The
!                 SatInfo structure array is searched for matching
!                 sensor channel entries and the location of the unique
!                 match is used to sort the output SatBias structure array.
!                 ** Note: If this argument is specified, then only matched
!                          sensors and channels are retained. Any unmatched
!                          Sensor_Id entries are *NOT* stored in the returned
!                          SatBias structure array. The SatBias result contains
!                          only the matched entries in the order they were
!                          encountered in the SatInfo structure array.
!                 UNITS:      N/A
!                 TYPE:       SatInfo_type
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Message_Log:  Character string specifying a filename in which any
!                 messages will be logged. If not specified, or if an
!                 error occurs opening the log file, the default action
!                 is to output messages to standard output.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!   SatBias :     Data structure array to hold the contents of the
!                 bias data file.
!                 UNITS:      N/A
!                 TYPE:       SatBiasAngle_type
!                               OR
!                             SatBiasAirMass_type
!                 DIMENSION:  Rank-1
!                 ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!   nEntries:     The number of SatBias entries read in.
!                 - If the size of the output array is LARGER than the
!                   number of valid SatBias entries in the input file,
!                   then on output this argument holds the number of
!                   entries read.
!                 - If the size of the output array is SMALLER than the
!                   number of valid SatBias entries in the input file,
!                   then on output this argument simply holds the size
!                   of the SatBias structure array argument.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(OUT)
!
!   RCS_Id:       Character string containing the Revision Control
!                 System Id field for the module.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status: The return value is an integer defining the error status.
!                 The error codes are defined in the Message_Handler module.
!                 If == SUCCESS the file read was successful
!                    == FAILURE an unrecoverable error occurred.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Dec-2005
!                       paul.vandelst@ssec.wisc.edu
!----------------------------------------------------------------------------------

  ! ---------------------------------------------------------
  ! Function to read the SatBiasAngle data into its structure
  ! ---------------------------------------------------------
  FUNCTION Read_SatBiasAngle( Filename,     &  ! Input
                              SatBiasAngle, &  ! Output
                              SatInfo,      &  ! Optional input
                              nEntries,     &  ! Optional output
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging
                            RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                               INTENT(IN)  :: Filename
    TYPE(SatBiasAngle_type),      DIMENSION(:), INTENT(OUT) :: SatBiasAngle
    TYPE(SatInfo_type), OPTIONAL, DIMENSION(:), INTENT(IN)  :: SatInfo
    INTEGER,            OPTIONAL,               INTENT(OUT) :: nEntries
    CHARACTER(*),       OPTIONAL,               INTENT(OUT) :: RCS_Id
    CHARACTER(*),       OPTIONAL,               INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SatBias(Angle)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: i, j, maxEntries, nValidEntries, Idx
    INTEGER :: FileID
    TYPE( SatBiasAngle_type ), DIMENSION(SIZE(SatBiasAngle)) :: Dummy


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Does the file exist?
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'SatBiasAngle file '//TRIM( Filename )//' not found.'
      GOTO 2000
    END IF

    ! Get the number of channel entries to read
    maxEntries = SIZE( SatBiasAngle )
    IF ( maxEntries < 1 ) THEN
      Message = 'Output SatBiasAngle structure array has zero size'
      GOTO 2000
    END IF

    ! Open the file
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Message = 'Error obtaining unit number for SatBiasAngle file '//TRIM( Filename )
      GOTO 2000
    END IF

    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error opening SatBiasAngle file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF


    ! Read the file
    nValidEntries = 0
    Read_Loop: DO i = 1, maxEntries

      ! Read the entry header
      READ( FileID, FMT    = SATBIASANGLE_HEADER_FMTSTRING, &
                    IOSTAT = IO_Status       ) SatBiasAngle(i)%Channel_Index, &
                                               SatBiasAngle(i)%Sensor_Id, &
                                               SatBiasAngle(i)%Channel, &
                                               SatBiasAngle(i)%LapseRate_Mean, &
                                               SatBiasAngle(i)%Bias_Mean, &
                                               SatBiasAngle(i)%nFOVs

      ! Check for EOF
      IF ( IO_Status < 0 ) EXIT Read_Loop

      ! Check for read error
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SatBiasAngle file ", a, " entry # ", i5, &
                          &" header. IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), i, IO_Status
        GOTO 1000
      END IF

      ! Read the entry bias values
      ! ** NOTE: The number of FOVs for each channel is currently the same.
      ! **       Eventually it will be sensor/channel dependent value.
      READ( FileID, FMT    = SATBIASANGLE_BIAS_FMTSTRING, &
                    IOSTAT = IO_Status       ) &
        (SatBiasAngle(i)%Bias(j), j=1,MAX_SATBIAS_FOVS )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SatBiasAngle file ", a, " entry # ", i5, &
                          &" bias data. IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), i, IO_Status
        GOTO 1000
      END IF

      ! Update valid entry counter
      nValidEntries = nValidEntries + 1

    END DO Read_Loop

    CLOSE( FileID )


    ! Sort the data if required
    IF ( PRESENT( SatInfo ) ) THEN

      ! Operate on a copy
      Dummy = SatBiasAngle
      CALL SatBias_Clear(SatBiasAngle)

      j=0
      Sort_Loop: DO i = 1, maxEntries

        ! Get the sorting indices
        Idx = SatInfo_IndexChannels( Dummy(i)%Sensor_Id, &
                                     Dummy(i)%Channel, &
                                     SatInfo )
        IF ( Idx == 0 ) CYCLE Sort_Loop

        ! Only keep the matching channels
        j=j+1
        SatBiasAngle(j) = Dummy(Idx)
      END DO Sort_Loop
      nValidEntries = j
    END IF

    ! Set the optional return argument
    IF ( PRESENT( nEntries ) ) nEntries = nValidEntries

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Read_SatBiasAngle


  ! -----------------------------------------------------------
  ! Function to read the SatBiasAirMass data into its structure
  ! -----------------------------------------------------------
  FUNCTION Read_SatBiasAirMass( Filename,       &  ! Input
                                SatBiasAirMass, &  ! Output
                                SatInfo,        &  ! Optional input
                                nEntries,       &  ! Optional output
                                RCS_Id,         &  ! Revision control
                                Message_Log )   &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                                 INTENT(IN)  :: Filename
    TYPE( SatBiasAirMass_type ),    DIMENSION(:), INTENT(OUT) :: SatBiasAirMass
    TYPE( SatInfo_type ), OPTIONAL, DIMENSION(:), INTENT(IN)  :: SatInfo
    INTEGER,              OPTIONAL,               INTENT(OUT) :: nEntries
    CHARACTER(*),         OPTIONAL,               INTENT(OUT) :: RCS_Id
    CHARACTER(*),         OPTIONAL,               INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SatBias(AirMass)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: i, j, maxEntries, nValidEntries, Idx
    INTEGER :: FileID
    TYPE( SatBiasAirMass_type ), DIMENSION(SIZE(SatBiasAirMass)) :: Dummy


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Does the file exist?
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'SatBiasAirMass file '//TRIM( Filename )//' not found.'
      GOTO 2000
    END IF

    ! Get the number of channel entries to read
    maxEntries = SIZE( SatBiasAirMass )
    IF ( maxEntries < 1 ) THEN
      Message = 'Output SatBiasAirMass structure array has zero size'
      GOTO 2000
    END IF

    ! Open the file
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Message = 'Error obtaining unit number for SatBiasAirMass file '//TRIM( Filename )
      GOTO 2000
    END IF

    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error opening SatBiasAirMass file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF


    ! Read the file
    nValidEntries = 0
    Read_Loop: DO i = 1, maxEntries

      ! Read the entry values
      READ( FileID, FMT    = SATBIASAIRMASS_FMTSTRING, &
                    IOSTAT = IO_Status  ) SatBiasAirMass(i)%Channel_Index, &
                                          SatBiasAirMass(i)%Sensor_Id, &
                                          SatBiasAirMass(i)%Channel, &
                                          (SatBiasAirMass(i)%c(j), j=1,MAX_SATBIAS_PREDICTORS )
        
      ! Check for EOF
      IF ( IO_Status < 0 ) EXIT Read_Loop

      ! Check for read error
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SatBiasAirMass file ", a, " entry # ", i5, &
                          &". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), i, IO_Status
        GOTO 1000
      END IF

      ! Update valid entry counter
      nValidEntries = nValidEntries + 1

    END DO Read_Loop

    CLOSE( FileID )


    ! Sort the data if required
    IF ( PRESENT( SatInfo ) ) THEN

      ! Operate on a copy
      Dummy = SatBiasAirMass
      CALL SatBias_Clear(SatBiasAirMass)

      j=0
      Sort_Loop: DO i = 1, maxEntries

        ! Get the sorting indices
        Idx = SatInfo_IndexChannels( Dummy(i)%Sensor_Id, &
                                     Dummy(i)%Channel, &
                                     SatInfo )
        IF ( Idx == 0 ) CYCLE Sort_Loop

        ! Only keep the matching channels
        j=j+1
        SatBiasAirMass(j) = Dummy(Idx)
      END DO Sort_Loop
      nValidEntries = j
    END IF

    ! Set the optional return argument
    IF ( PRESENT( nEntries ) ) nEntries = nValidEntries

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Read_SatBiasAirMass


!----------------------------------------------------------------------------------
!
! Function to write GSI SatBiasAngle and SatBiasAirMass data files. 
!
! CALLING SEQUENCE:
!   Error_Status = Write_SatBias( Filename,               &  ! Input
!                                 SatBias,                &  ! Input
!                                 RCS_Id=RCS_Id,          &  ! Revision control
!                                 Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:     Character string specifying the name of a
!                 SatBiasAngle data file to read.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN)
!
!   SatBias:      Data structure array containing the data to
!                 write to file.
!                 UNITS:      N/A
!                 TYPE:       SatBiasAngle_type
!                               OR
!                             SatBiasAirMass_type
!                 DIMENSION:  Rank-1
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:  Character string specifying a filename in which any
!                 messages will be logged. If not specified, or if an
!                 error occurs opening the log file, the default action
!                 is to output messages to standard output.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:       Character string containing the Revision Control
!                 System Id field for the module.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!
! FUNCTION RESULT:
!   Error_Status: The return value is an integer defining the error status.
!                 The error codes are defined in the Message_Handler module.
!                 If == SUCCESS the SatBiasAngle file write was successful
!                    == FAILURE an unrecoverable error occurred.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs in this routine, the output file is deleted
!         before returning to the calling routine.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Dec-2005
!                       paul.vandelst@ssec.wisc.edu
!----------------------------------------------------------------------------------

  ! ----------------------------------------------------------
  ! Function to write the SatBiasAngle data from its structure
  ! ----------------------------------------------------------
  FUNCTION Write_SatBiasAngle( Filename,     &  ! Input
                               SatBiasAngle, &  ! Input
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                            INTENT(IN)  :: Filename
    TYPE( SatBiasAngle_type ), DIMENSION(:), INTENT(IN)  :: SatBiasAngle
    CHARACTER(*), OPTIONAL,                  INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,                  INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SatBias(Angle)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: i, j, nEntries
    INTEGER :: FileID


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Get the number of channel entries to write
    nEntries = SIZE( SatBiasAngle )
    IF ( nEntries < 1 ) THEN
      Message = 'Input SatBiasAngle structure array has zero size'
      GOTO 2000
    END IF

    ! Open the file
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Message = 'Error obtaining unit number for SatBiasAngle file '//TRIM( Filename )
      GOTO 2000
    END IF

    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'REPLACE', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'WRITE', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error opening SatBiasAngle file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF


    ! Write the file
    Write_Loop: DO i = 1, nEntries

      ! If the NCEP Sensor ID is invalid, skip this entry
      IF ( LEN_TRIM(ADJUSTL(SatBiasAngle(i)%Sensor_Id)) == 0 ) CYCLE Write_Loop

      ! Write the entry header
      WRITE( FileID, FMT    = SATBIASANGLE_HEADER_FMTSTRING, &
                     IOSTAT = IO_Status       ) &
        SatBiasAngle(i)%Channel_Index, &
        SatBiasAngle(i)%Sensor_Id, &
        SatBiasAngle(i)%Channel, &
        SatBiasAngle(i)%LapseRate_Mean, &
        SatBiasAngle(i)%Bias_Mean, &
        SatBiasAngle(i)%nFOVs
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing SatBiasAngle file ", a, " entry # ", i5, &
                          &" header. IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), i, IO_Status
        GOTO 1000
      END IF

      ! Write the entry bias values
      ! ** NOTE: The number of FOVs for each channel is currently the same.
      ! **       Eventually it will be sensor/channel dependent value.
      WRITE( FileID, FMT    = SATBIASANGLE_BIAS_FMTSTRING, &
                     IOSTAT = IO_Status       ) &
        (SatBiasAngle(i)%Bias(j), j=1,MAX_SATBIAS_FOVS )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing SatBiasAngle file ", a, " entry # ", i5, &
                          &" bias data. IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), i, IO_Status
        GOTO 1000
      END IF

    END DO Write_Loop

    CLOSE( FileID )

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR, &
                   IOSTAT = IO_Status )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_SatBiasAngle



  ! ------------------------------------------------------------
  ! Function to write the SatBiasAirMAss data from its structure
  ! ------------------------------------------------------------
  FUNCTION Write_SatBiasAirMass( Filename,       &  ! Input
                                 SatBiasAirMass, &  ! Input
                                 RCS_Id,         &  ! Revision control
                                 Message_Log )   &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                              INTENT(IN)  :: Filename
    TYPE( SatBiasAirMass_type ), DIMENSION(:), INTENT(IN)  :: SatBiasAirMass
    CHARACTER(*), OPTIONAL,                    INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,                    INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SatBias(AirMass)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: i, j, nEntries
    INTEGER :: FileID


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Get the number of channel entries to write
    nEntries = SIZE( SatBiasAirMass )
    IF ( nEntries < 1 ) THEN
      Message = 'Input SatBiasAirMass structure array has zero size'
      GOTO 2000
    END IF

    ! Open the file
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Message = 'Error obtaining unit number for SatBiasAirMass file '//TRIM( Filename )
      GOTO 2000
    END IF

    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'REPLACE', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'WRITE', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error opening SatBiasAirMass file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    
    ! Write the file
    Write_Loop: DO i = 1, nEntries

      ! If the NCEP Sensor ID is invalid, skip this entry
      IF ( LEN_TRIM(ADJUSTL(SatBiasAirMass(i)%Sensor_Id)) == 0 ) CYCLE Write_Loop

      ! Write the entry
      WRITE( FileID, FMT    = SATBIASAIRMASS_FMTSTRING, &
                     IOSTAT = IO_Status  ) &
        SatBiasAirMass(i)%Channel_Index, &
        SatBiasAirMass(i)%Sensor_Id, &
        SatBiasAirMass(i)%Channel, &
        (SatBiasAirMass(i)%c(j), j=1,MAX_SATBIAS_PREDICTORS )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing SatBiasAirMass file ", a, " entry # ", i5, &
                          &". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), i, IO_Status
        GOTO 1000
      END IF

    END DO Write_Loop

    CLOSE( FileID )

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR, &
                   IOSTAT = IO_Status )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_SatBiasAirMass

END MODULE SatBias_IO


! Program to read the GSI radiance diagnostic output files and
! compute scan and time series statistics.
!
PROGRAM Compute_RadDiag_Stats_2D

  ! Module usage
  USE Type_Kinds             , ONLY: sp=>Single
  USE File_Utility           , ONLY: Get_Lun, File_Exists, Count_Lines_in_File
  USE Message_Handler        , ONLY: SUCCESS, WARNING, FAILURE, EOF, &
                                     Program_Message, Display_Message
  USE List_File_Utility      , ONLY: Character_List_File_type, &
                                     Read_List_File,           &
                                     Get_List_Size,            &
                                     Get_List_Entry
  USE RadDiag_IO             , ONLY: RadDiag_Hdr_type,   &
                                     RadDiag_Data_type,  &
                                     RADDIAG_READMODE,   &
                                     RADDIAG_WRITEMODE,  &
                                     RADDIAG_APPENDMODE, &
                                     RadDiag_OpenFile,       &
                                     RadDiag_Hdr_ReadFile,   &
                                     RadDiag_Data_ReadFile
  USE RadDiag_Stats_2D_Define   , ONLY: RadDiag_Stats_2D_type,    &
                                     INVALID_FOV,           &
                                     N_VARIABLES,           &
                                     IBC,                   &
                                     INBC,                  &
                                     RadDiag_Stats_2D_Destroy, &
                                     RadDiag_Stats_2D_Create
  USE RadDiag_Stats_2D_netCDF_IO, ONLY: RadDiag_Stats_2D_WriteFile


  ! Disable implicit typing
  IMPLICIT NONE

  ! Parameters
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Compute_RadDiag_Stats'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id: Compute_RadDiag_Stats.f90 8759 2010-07-09 21:41:53Z andrew.collard@noaa.gov $'
  
  CHARACTER(*), PARAMETER :: RadDiag_Prefix='diag'
  REAL(sp),     PARAMETER :: ZERO = 0.0_sp                   
  REAL(sp),     PARAMETER :: minInverseVariance = 1.0e-06_sp 
  INTEGER,      PARAMETER :: NHEADER = 3                     
  INTEGER,      PARAMETER :: SET = 1
  INTEGER,      PARAMETER :: MAX_SATBIAS_PREDICTORS=6
  INTEGER,      PARAMETER :: MAX_FOVs = 120

  ! Parameters for surface filtering
  INTEGER,      PARAMETER :: All_Surfaces  = 0
  INTEGER,      PARAMETER :: Sea           = 1
  INTEGER,      PARAMETER :: Land          = 2
  INTEGER,      PARAMETER :: Ice           = 3
  INTEGER,      PARAMETER :: Snow          = 4
  INTEGER,      PARAMETER :: SnowIce       = 5
  INTEGER,      PARAMETER :: Max_Surface_Type  = 5
  REAL(sp),     PARAMETER :: Sea_Threshold     = 0.99_sp
  REAL(sp),     PARAMETER :: Land_Threshold    = 0.99_sp
  REAL(sp),     PARAMETER :: Ice_Threshold     = 0.99_sp
  REAL(sp),     PARAMETER :: Snow_Threshold    = 0.99_sp
  REAL(sp),     PARAMETER :: SnowIce_Threshold = 0.99_sp
  CHARACTER(7), PARAMETER :: Surface_Text(0:5) = (/'AllSurf','Sea    ','Land   ','Ice    ','Snow   ','SnowIce'/)

  ! Variables
  CHARACTER(3)    :: gesanal
  CHARACTER(200)  :: Message
  CHARACTER(200)  :: RadDiag_Filename
  CHARACTER(200)  :: Sensor_Id 
  CHARACTER(200)  :: Output_Filename
  CHARACTER(1000) :: Title
  CHARACTER(1000) :: Comment
  INTEGER :: Year, Month, Day, Hour
  INTEGER :: FileID
  INTEGER :: Error_Status
  INTEGER :: Read_Status
  TYPE(RadDiag_Hdr_type)  :: RadDiag_Hdr
  TYPE(RadDiag_Data_type) :: RadDiag_Data
  TYPE(RadDiag_Stats_2D_type) :: RadDiag_Stats
  INTEGER :: i, j, m, n, ilat, ilon
  INTEGER :: iTime, n_Times
  INTEGER :: nlat, nlon
  INTEGER :: Surface_Type, ans
  CHARACTER(10) :: TimeStamp, First_Timestamp, Last_Timestamp
  REAL(sp) :: rnSamples
  LOGICAL :: LeapYear


  ! Output a program header message
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read GSI radiance diagnostic (RadDiag) files and '//&
                        'compute various statistics over scan position and time.', &
                        '$Revision: 8759 $' )
  
 ! Get the RadDiag_Stats output filename
  WRITE( *,FMT='("Enter prefix for output filename: ")', ADVANCE='NO' )
  READ( *,'(a)' ) Output_Filename
  Output_Filename = ADJUSTL(Output_Filename)


  ! Get an output file comment
  WRITE( *,'("Enter comment string for output file:")' )
  READ( *,'(a)' ) Comment
  Comment = ADJUSTL(Comment)


  ! Get the required sensor ID
  WRITE( *,'("Enter sensor id (e.g., iasi_metop-a):")' )
  READ( *,'(a)' ) Sensor_Id
  Sensor_Id = ADJUSTL(Sensor_Id)

  ! Get the first date/time to process
  WRITE( *,'("Enter the first date/time to process (YYYYMMDDHH):")' )
  READ( *,'(a)' ) TimeStamp
  TimeStamp = ADJUSTL(TimeStamp)
  First_TimeStamp = TRIM(TimeStamp)
  READ(TimeStamp,FMT="(I4,I2,I2,I2,I2)") Year, Month, Day, Hour
  Error_Status = SUCCESS
  IF (Day < 1) THEN 
    Error_Status = FAILURE
  ELSE
    SELECT CASE (Month)
      CASE (1, 3, 5, 7, 8, 10, 12)
        IF (Day > 31) Error_Status = FAILURE
      CASE (4, 6, 9, 11)  
        IF (Day > 30) Error_Status = FAILURE
      CASE (2)
        IF (Day >= 28) THEN
           LeapYear = ( (Year/4)*4 == Year .AND. &
                ((Year/100)*100 /= Year .OR. (Year/400)*400 == Year))
           IF ((LeapYear .AND. Day > 29) .OR. (.NOT.(LeapYear) .AND. Day > 28)) &
                Error_Status = FAILURE
        END IF
      CASE DEFAULT
        Error_Status = FAILURE
     END SELECT
  END IF
  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message, '( "You have entered date/time as Year:",i4," Month:",'//&
         'i2.2," Day:",i2.2," Hour:",i2.2," This does not seem right.")' ) &
         Year,Month,Day,Hour
    CALL Display_Message(PROGRAM_NAME, &
                         TRIM(Message), &
                         Error_Status)
    STOP
  END IF

  ! Get the number of cycles to process
  WRITE( *,'("Enter the number of cycles to process:")' )
  READ( *,*) n_Times

  ! Get the required Surface Type
  WRITE( *,'("Enter the required surface type:")' )
  WRITE( *,'(5x,i2," = All")' ) All_Surfaces
  WRITE( *,'(5x,i2," = Sea")' ) Sea
  WRITE( *,'(5x,i2," = Land")' ) Land
  WRITE( *,'(5x,i2," = Ice")' ) Ice
  WRITE( *,'(5x,i2," = Snow")' ) Snow
  WRITE( *,'(5x,i2," = Snow and Ice")' ) SnowIce
  WRITE( *,'(5x,"Any other value defaults to all")' )
  READ( *,*) Surface_Type
  IF (Surface_Type < 1 .OR. Surface_Type > Max_Surface_Type) Surface_Type=0    


  ! Are we processing Guess or Analysis Fields?
  WRITE( *,'("Enter 1 to process analysis fields (otherwise it will be Guess fields):")' )
  READ( *,*) ans
  IF (ans == 1) THEN
    gesanal='anl'
  ELSE
    gesanal='ges'
  END IF

  ! Set up output grid
  WRITE( *,*) 'Enter number of boxes in latitude direction'
  WRITE( *,*) 'Entering zero or less defaults to 90 (2 degree) boxes'
  READ( *,*) nlat
  IF (nlat <= 0) THEN
    write(*,*) 'Resetting number of latitude boxes to 90 (2 degrees)'
    nlat=90
  END IF
  WRITE( *,*) 'Enter number of boxes in longitude direction'
  WRITE( *,*) 'Entering zero or less defaults to 180 (2 degree) boxes'
  READ( *,*) nlon
  IF (nlon <= 0) THEN
    write(*,*) 'Resetting number of longitude boxes to 180 (2 degrees)'
    nlon=180
  END IF

 ! Loop over RadDiag times
  m = 0

  File_Loop: DO iTime = 1, n_Times

    WRITE(*,*) 'Processing '//TimeStamp

    ! Create the input filenames
    RadDiag_Filename        = TRIM(RadDiag_Prefix)//'_'//TRIM(Sensor_ID)//&
         '_'//TRIM(gesanal)//'.'//TRIM(TimeStamp)

    ! Add six hours to Timestamp and Cycle file loop if file doesn't exist
    IF ( .NOT. File_Exists(RadDiag_Filename)) THEN
      CALL Display_Message(PROGRAM_NAME, 'Files for timestamp '//&
            TRIM(TimeStamp)//' not present', WARNING)

      READ(TimeStamp,FMT="(I4,I2,I2,I2,I2)") Year, Month, Day, Hour
      Hour = Hour + 6
      IF (Hour >= 24) THEN
        Hour = Hour - 24
        CALL AddOneDay(Year, Month, Day)
      END IF
      WRITE(TimeStamp,FMT="(I4,I2.2,I2.2,I2.2)") Year, Month, Day, Hour
      CYCLE File_Loop
    END IF


    ! Increment the time/file counter
    m = m + 1
    Last_TimeStamp = TRIM(TimeStamp)

    ! Open the RadDiag file
    Error_Status = RadDiag_OpenFile( RadDiag_Filename, FileID )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error opening '//&
           TRIM(RadDiag_Filename), FAILURE)
      STOP
    END IF

    ! Read the RadDiag file header
    WRITE(*, '( /5x, "Reading ", a, " header...." )' ) TRIM( RadDiag_Filename )
    Error_Status = RadDiag_Hdr_ReadFile( FileID, RadDiag_Hdr )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error reading header from '//TRIM(RadDiag_Filename), FAILURE)
      STOP
    END IF

    ! Perform the various allocations
    IF ( iTime == 1 ) THEN
      CALL RadDiag_Stats_2D_Create( RadDiag_Stats, &
                                   RadDiag_Hdr%n_Channels, &
                                   nlat, &
                                   nlon )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message(PROGRAM_NAME, &
             'Error allocating RadDiag_Stats arrays', FAILURE)
        STOP
      END IF
      ! Save the channel numbers
      RadDiag_Stats%Channel = RadDiag_Hdr%Channel(:)%nuchan
      ! Save the lats and longs
      DO ilat=1,nlat
         RadDiag_Stats%Latitude(ilat) = (90.0_sp/nlat)*(2*ilat-1) - 90.0_sp
      END DO
      DO ilon=1,nlon
         RadDiag_Stats%Longitude(ilon) = (180.0_sp/nlon)*(2*ilon-1)
      END DO
    END IF

    ! Initialise file data record counter
    n = 0

    ! Loop to read RadDiag data until end-of-file
    WRITE(*, '( 5x, "Begin ", a, " data read/summation loop...." )' ) TRIM( RadDiag_Filename )
    Read_Loop: DO

      ! Read the current entry
      Read_Status = RadDiag_Data_ReadFile( FileID, RadDiag_Hdr, RadDiag_Data )
      SELECT CASE (Read_Status)
        CASE (EOF)
          EXIT Read_Loop
        CASE (FAILURE)
          CALL Display_Message(PROGRAM_NAME, &
               'Error reading data from '//TRIM( RadDiag_Filename ), WARNING)
          EXIT Read_Loop !STOP
        CASE DEFAULT
          ! Success or warning: do nothing
      END SELECT

      ! Increment data record counter
      n = n + 1

      ! Decide whether the suface type is correct.
      IF (Surface_Type ==  Sea .AND. RadDiag_Data%Scalar%Water_Frac <  Sea_Threshold) CYCLE Read_Loop
      IF (Surface_Type == Land .AND. RadDiag_Data%Scalar%Land_Frac  < Land_Threshold) CYCLE Read_Loop
      IF (Surface_Type ==  Ice .AND. RadDiag_Data%Scalar%Ice_Frac   <  Ice_Threshold) CYCLE Read_Loop
      IF (Surface_Type == Snow .AND. RadDiag_Data%Scalar%Snow_Frac  < Snow_Threshold) CYCLE Read_Loop
      IF (Surface_Type == SnowIce .AND. &
           (RadDiag_Data%Scalar%Snow_Frac + RadDiag_Data%Scalar%Snow_Frac)  < SnowIce_Threshold) CYCLE Read_Loop

      ! Identify appropriate longitude and latitude bins
      ilat = INT(nlat*(RadDiag_Data%Scalar%Lat+90.0_sp)/180.0_sp) + 1
      ilon = INT(nlon*(RadDiag_Data%Scalar%Lon)/360.0_sp) + 1

      IF (ilat > nlat .OR. ilon > nlon) THEN
         write(*,*) 'Latitude or Longitude out of range, skipping:',&
              RadDiag_Data%Scalar%Lat,RadDiag_Data%Scalar%Lon
         CYCLE Read_Loop
      END IF

      ! Loop over channels to sum data
      Channel_Loop: DO j = 1, RadDiag_Data%n_Channels
         IF ( RadDiag_Data%Channel(j)%errinv > minInverseVariance ) THEN

          ! Summation by lat/lon only
          RadDiag_Stats%nSamples(j,ilon,ilat)    = RadDiag_Stats%nSamples(j,ilon,ilat) + 1
          RadDiag_Stats%mean(iBC,j,ilon,ilat)    = RadDiag_Stats%mean(iBC,j,ilon,ilat) + &
                                                        RadDiag_Data%Channel(j)%omgbc 
          RadDiag_Stats%mean(iNBC,j,ilon,ilat)   = RadDiag_Stats%mean(iNBC,j,ilon,ilat) + &
                                                        RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%StdDev(iBC,j,ilon,ilat)  = RadDiag_Stats%StdDev(iBC,j,ilon,ilat) + &
                                       RadDiag_Data%Channel(j)%omgbc * RadDiag_Data%Channel(j)%omgbc
          RadDiag_Stats%StdDev(iNBC,j,ilon,ilat) = RadDiag_Stats%StdDev(iNBC,j,ilon,ilat) + &
                                       RadDiag_Data%Channel(j)%omgnbc * RadDiag_Data%Channel(j)%omgnbc
       END IF
     END DO Channel_Loop
    END DO Read_Loop
    WRITE(*, '( 5x, "Number of records read: ", i7 )' ) n

    ! Increment Time Stamp by 6 hours
      READ(TimeStamp,FMT="(I4,I2,I2,I2,I2)") Year, Month, Day, Hour
      Hour = Hour + 6
      IF (Hour >= 24) THEN
        Hour = Hour - 24
        CALL AddOneDay(Year, Month, Day)
      END IF
      WRITE(TimeStamp,FMT="(I4,I2.2,I2.2,I2.2)") Year, Month, Day, Hour

  END DO File_Loop

  ! Exit here if no files found
  IF (m == 0) THEN
     CALL Display_Message(PROGRAM_NAME, 'No Suitable Files have been found', &
          FAILURE)
     STOP
  END IF

  ! Compute the channel only averages
  WRITE(*, '( /5x, "Computing lat/lon averages...." )' )
  DO ilat=1,nlat
     DO ilon=1,nlon
        Channel_Loop2: DO j = 1, RadDiag_Stats%n_Channels
           rnSamples = REAL(RadDiag_Stats%nSamples(j,ilon,ilat),sp)
           IF (rnSamples == 0) CYCLE Channel_Loop2
           DO i = 1, RadDiag_Stats%n_Variables
              RadDiag_Stats%Mean(i,j,ilon,ilat) = RadDiag_Stats%Mean(i,j,ilon,ilat)/rnSamples
              RadDiag_Stats%StdDev(i,j,ilon,ilat) = &
                   SQRT(RadDiag_Stats%StdDev(i,j,ilon,ilat)/rnSamples - &
                   RadDiag_Stats%Mean(i,j,ilon,ilat)*RadDiag_Stats%Mean(i,j,ilon,ilat))
           END DO
        END DO Channel_Loop2
     END DO
  END DO

  ! Write averages to file
   IF (TRIM(Output_Filename) /= '') Output_Filename=TRIM(Output_Filename)//'_'
   Output_Filename=TRIM(Output_Filename)//TRIM(Sensor_ID)//'_'//&
        TRIM(gesanal)//'_'//TRIM(Surface_Text(Surface_Type))//'_'//First_TimeStamp//&
        '_'//Last_TimeStamp//'_2D.nc'
 
   WRITE(Title,'("RadDiag 2D Stats for ", a, " from ", a, " to ", a )' ) &
               TRIM(Sensor_Id), &
               First_TimeStamp, &
               Last_TimeStamp
   WRITE(*, '( /5x, "Writing output file ", a, "...." )' ) TRIM(Output_Filename)

   Error_Status = RadDiag_Stats_2D_WriteFile( TRIM(Output_Filename), &
                                              RadDiag_Stats, &
                                              Title   = TRIM(Title), &
                                              History = PROGRAM_VERSION_ID, &
                                              Comment = TRIM(Comment) )
   IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message(PROGRAM_NAME, 'Error writing output file '//TRIM(Output_Filename), Error_Status)
     STOP
   END IF


  ! Cleanup
  CALL RadDiag_Stats_2D_Destroy( RadDiag_Stats )

END PROGRAM Compute_RadDiag_Stats_2D

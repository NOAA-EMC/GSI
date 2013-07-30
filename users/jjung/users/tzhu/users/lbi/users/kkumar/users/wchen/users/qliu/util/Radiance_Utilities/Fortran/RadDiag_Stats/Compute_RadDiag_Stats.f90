
! Program to read the GSI radiance diagnostic output files and
! compute scan and time series statistics.
!
PROGRAM Compute_RadDiag_Stats

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
  USE RadDiag_Stats_Define   , ONLY: RadDiag_Stats_type,    &
                                     INVALID_FOV,           &
                                     N_VARIABLES,           &
                                     IBC,                   &
                                     INBC,                  &
                                     ISCAN,                 &
                                     ICONST,                &
                                     IANGLE,                &
                                     ILPSR,                 &
                                     ILPSR2,                &
                                     ICLW,                  &
                                     RadDiag_Stats_Destroy, &
                                     RadDiag_Stats_Create
  USE RadDiag_Stats_netCDF_IO, ONLY: RadDiag_Stats_WriteFile


  ! Disable implicit typing
  IMPLICIT NONE

  ! Parameters
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Compute_RadDiag_Stats'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id$'
  
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
  TYPE(RadDiag_Stats_type) :: RadDiag_Stats
  INTEGER :: i, j, k, m, n
  INTEGER :: iTime, n_Times
  INTEGER :: Surface_Type, ans
  CHARACTER(10) :: TimeStamp, First_Timestamp, Last_Timestamp
  REAL(sp) :: rnSamples
  LOGICAL :: LeapYear


  ! Output a program header message
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read GSI radiance diagnostic (RadDiag) files and '//&
                        'compute various statistics over scan position and time.', &
                        '$Revision$' )
  
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
      CALL RadDiag_Stats_Create( RadDiag_Stats, &
                                 MAX_SATBIAS_PREDICTORS, &
                                 RadDiag_Hdr%n_Channels, &
                                 MAX_FOVs, &
                                 n_Times )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message(PROGRAM_NAME, &
             'Error allocating RadDiag_Stats arrays', FAILURE)
        STOP
      END IF
      ! Save the channel numbers
      RadDiag_Stats%Channel = RadDiag_Hdr%Channel(:)%nuchan
      ! Initialised the counter for the number of FOVs
      RadDiag_Stats%n_FOVS = 1
    END IF

    ! Save the current date/time
    RadDiag_Stats%DateTime(m) = RadDiag_Hdr%Scalar%idate

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

      ! Save the current scan position and update maximum
      k = RadDiag_Data%Scalar%senscn_pos
      RadDiag_Stats%FOV(k) = k
      RadDiag_Stats%n_FOVS = MAX(RadDiag_Stats%n_FOVS,k)
      IF (RadDiag_Stats%n_FOVS > MAX_FOVs) THEN    
         WRITE( Message, '( "You need to increase MAX_FOVs as you are asking for ",'//&
              'i5,"when the maximum is ",i5)' ) RadDiag_Stats%n_FOVS, MAX_FOVs
         CALL Display_Message(PROGRAM_NAME, Message, FAILURE)
         STOP
      END IF

      IF (Surface_Type ==  Sea .AND. RadDiag_Data%Scalar%Water_Frac <  Sea_Threshold) CYCLE Read_Loop
      IF (Surface_Type == Land .AND. RadDiag_Data%Scalar%Land_Frac  < Land_Threshold) CYCLE Read_Loop
      IF (Surface_Type ==  Ice .AND. RadDiag_Data%Scalar%Ice_Frac   <  Ice_Threshold) CYCLE Read_Loop
      IF (Surface_Type == Snow .AND. RadDiag_Data%Scalar%Snow_Frac  < Snow_Threshold) CYCLE Read_Loop
      IF (Surface_Type == SnowIce .AND. &
           (RadDiag_Data%Scalar%Snow_Frac + RadDiag_Data%Scalar%Snow_Frac)  < SnowIce_Threshold) CYCLE Read_Loop

      ! Loop over channels to sum data
      Channel_Loop: DO j = 1, RadDiag_Data%n_Channels
         IF ( RadDiag_Data%Channel(j)%errinv > minInverseVariance ) THEN

          ! Channel-Scan position summation
          RadDiag_Stats%scan_nSamples(j,k)      = RadDiag_Stats%scan_nSamples(j,k) + 1

          RadDiag_Stats%Scan_Mean(iBC,j,k)      = RadDiag_Stats%Scan_Mean(iBC,j,k)    + RadDiag_Data%Channel(j)%omgbc 
          RadDiag_Stats%Scan_Mean(iNBC,j,k)     = RadDiag_Stats%Scan_Mean(iNBC,j,k)   + RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%Scan_Mean(iScan,j,k)    = RadDiag_Stats%Scan_Mean(iScan,j,k)  + RadDiag_Data%Channel(j)%bifix
          RadDiag_Stats%Scan_Mean(iConst,j,k)   = RadDiag_Stats%Scan_Mean(iConst,j,k) + RadDiag_Data%Channel(j)%bicons
          RadDiag_Stats%Scan_Mean(iAngle,j,k)   = RadDiag_Stats%Scan_Mean(iAngle,j,k) + RadDiag_Data%Channel(j)%biang
          RadDiag_Stats%Scan_Mean(iLpsR ,j,k)   = RadDiag_Stats%Scan_Mean(iLpsR ,j,k) + RadDiag_Data%Channel(j)%bilap
          RadDiag_Stats%Scan_Mean(iLpsR2,j,k)   = RadDiag_Stats%Scan_Mean(iLpsR2,j,k) + RadDiag_Data%Channel(j)%bilap2
          RadDiag_Stats%Scan_Mean(iCLW  ,j,k)   = RadDiag_Stats%Scan_Mean(iCLW  ,j,k) + RadDiag_Data%Channel(j)%biclw

          RadDiag_Stats%Scan_StdDev(iBC,j,k)    = RadDiag_Stats%Scan_StdDev(iBC,j,k)    + &
                                                      RadDiag_Data%Channel(j)%omgbc * RadDiag_Data%Channel(j)%omgbc 
          RadDiag_Stats%Scan_StdDev(iNBC,j,k)   = RadDiag_Stats%Scan_StdDev(iNBC,j,k)   + &
                                                      RadDiag_Data%Channel(j)%omgnbc * RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%Scan_StdDev(iScan,j,k)  = RadDiag_Stats%Scan_StdDev(iScan,j,k)  + &
                                                      RadDiag_Data%Channel(j)%bifix * RadDiag_Data%Channel(j)%bifix
          RadDiag_Stats%Scan_StdDev(iConst,j,k) = RadDiag_Stats%Scan_StdDev(iConst,j,k) + &
                                                      RadDiag_Data%Channel(j)%bicons *  RadDiag_Data%Channel(j)%bicons
          RadDiag_Stats%Scan_StdDev(iAngle,j,k) = RadDiag_Stats%Scan_StdDev(iAngle,j,k) + &
                                                      RadDiag_Data%Channel(j)%biang *  RadDiag_Data%Channel(j)%biang
          RadDiag_Stats%Scan_StdDev(iLpsR ,j,k) = RadDiag_Stats%Scan_StdDev(iLpsR ,j,k) + &
                                                      RadDiag_Data%Channel(j)%bilap * RadDiag_Data%Channel(j)%bilap
          RadDiag_Stats%Scan_StdDev(iLpsR2,j,k) = RadDiag_Stats%Scan_StdDev(iLpsR2,j,k) + &
                                                      RadDiag_Data%Channel(j)%bilap2 * RadDiag_Data%Channel(j)%bilap2
          RadDiag_Stats%Scan_StdDev(iCLW  ,j,k) = RadDiag_Stats%Scan_StdDev(iCLW  ,j,k) + &
                                                      RadDiag_Data%Channel(j)%biclw * RadDiag_Data%Channel(j)%biclw

          ! Channel-Timeseries summation
          RadDiag_Stats%time_nSamples(j,m)    = RadDiag_Stats%time_nSamples(j,m) + 1

          RadDiag_Stats%time_mean(iBC,j,m)    = RadDiag_Stats%time_mean(iBC,j,m)    + RadDiag_Data%Channel(j)%omgbc 
          RadDiag_Stats%time_mean(iNBC,j,m)   = RadDiag_Stats%time_mean(iNBC,j,m)   + RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%time_mean(iScan,j,m)  = RadDiag_Stats%time_mean(iScan,j,m)  + RadDiag_Data%Channel(j)%bifix
          RadDiag_Stats%time_mean(iConst,j,m) = RadDiag_Stats%time_mean(iConst,j,m) + RadDiag_Data%Channel(j)%bicons
          RadDiag_Stats%time_mean(iAngle,j,m) = RadDiag_Stats%time_mean(iAngle,j,m) + RadDiag_Data%Channel(j)%biang
          RadDiag_Stats%time_mean(iLpsR ,j,m) = RadDiag_Stats%time_mean(iLpsR ,j,m) + RadDiag_Data%Channel(j)%bilap
          RadDiag_Stats%time_mean(iLpsR2,j,m) = RadDiag_Stats%time_mean(iLpsR2,j,m) + RadDiag_Data%Channel(j)%bilap2
          RadDiag_Stats%time_mean(iCLW  ,j,m) = RadDiag_Stats%time_mean(iCLW  ,j,m) + RadDiag_Data%Channel(j)%biclw

          RadDiag_Stats%Time_StdDev(iBC,j,m)    = RadDiag_Stats%Time_StdDev(iBC,j,m)    + &
                                                     RadDiag_Data%Channel(j)%omgbc * RadDiag_Data%Channel(j)%omgbc
          RadDiag_Stats%Time_StdDev(iNBC,j,m)   = RadDiag_Stats%Time_StdDev(iNBC,j,m)   + &
                                                     RadDiag_Data%Channel(j)%omgnbc * RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%Time_StdDev(iScan,j,m)  = RadDiag_Stats%Time_StdDev(iScan,j,m)  + &
                                                     RadDiag_Data%Channel(j)%bifix * RadDiag_Data%Channel(j)%bifix
          RadDiag_Stats%Time_StdDev(iConst,j,m) = RadDiag_Stats%Time_StdDev(iConst,j,m) + &
                                                     RadDiag_Data%Channel(j)%bicons * RadDiag_Data%Channel(j)%bicons
          RadDiag_Stats%Time_StdDev(iAngle,j,m) = RadDiag_Stats%Time_StdDev(iAngle,j,m) + &
                                                     RadDiag_Data%Channel(j)%biang * RadDiag_Data%Channel(j)%biang 
          RadDiag_Stats%Time_StdDev(iLpsR ,j,m) = RadDiag_Stats%Time_StdDev(iLpsR ,j,m) + &
                                                     RadDiag_Data%Channel(j)%bilap * RadDiag_Data%Channel(j)%bilap
          RadDiag_Stats%Time_StdDev(iLpsR2,j,m) = RadDiag_Stats%Time_StdDev(iLpsR2,j,m) + &
                                                     RadDiag_Data%Channel(j)%bilap2 * RadDiag_Data%Channel(j)%bilap2
          RadDiag_Stats%Time_StdDev(iCLW  ,j,m) = RadDiag_Stats%Time_StdDev(iCLW  ,j,m) + &
                                                     RadDiag_Data%Channel(j)%biclw * RadDiag_Data%Channel(j)%biclw

          ! Summation by channel only
          RadDiag_Stats%nSamples(j)    = RadDiag_Stats%nSamples(j) + 1

          RadDiag_Stats%mean(iBC,j)    = RadDiag_Stats%mean(iBC,j)    + RadDiag_Data%Channel(j)%omgbc 
          RadDiag_Stats%mean(iNBC,j)   = RadDiag_Stats%mean(iNBC,j)   + RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%mean(iScan,j)  = RadDiag_Stats%mean(iScan,j)  + RadDiag_Data%Channel(j)%bifix
          RadDiag_Stats%mean(iConst,j) = RadDiag_Stats%mean(iConst,j) + RadDiag_Data%Channel(j)%bicons
          RadDiag_Stats%mean(iAngle,j) = RadDiag_Stats%mean(iAngle,j) + RadDiag_Data%Channel(j)%biang
          RadDiag_Stats%mean(iLpsR ,j) = RadDiag_Stats%mean(iLpsR ,j) + RadDiag_Data%Channel(j)%bilap
          RadDiag_Stats%mean(iLpsR2,j) = RadDiag_Stats%mean(iLpsR2,j) + RadDiag_Data%Channel(j)%bilap2
          RadDiag_Stats%mean(iCLW  ,j) = RadDiag_Stats%mean(iCLW  ,j) + RadDiag_Data%Channel(j)%biclw

          RadDiag_Stats%StdDev(iBC,j)    = RadDiag_Stats%StdDev(iBC,j)    + &
                                                     RadDiag_Data%Channel(j)%omgbc * RadDiag_Data%Channel(j)%omgbc
          RadDiag_Stats%StdDev(iNBC,j)   = RadDiag_Stats%StdDev(iNBC,j)   + &
                                                     RadDiag_Data%Channel(j)%omgnbc * RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%StdDev(iScan,j)  = RadDiag_Stats%StdDev(iScan,j)  + &
                                                     RadDiag_Data%Channel(j)%bifix * RadDiag_Data%Channel(j)%bifix
          RadDiag_Stats%StdDev(iConst,j) = RadDiag_Stats%StdDev(iConst,j) + &
                                                     RadDiag_Data%Channel(j)%bicons * RadDiag_Data%Channel(j)%bicons
          RadDiag_Stats%StdDev(iAngle,j) = RadDiag_Stats%StdDev(iAngle,j) + &
                                                     RadDiag_Data%Channel(j)%biang * RadDiag_Data%Channel(j)%biang 
          RadDiag_Stats%StdDev(iLpsR ,j) = RadDiag_Stats%StdDev(iLpsR ,j) + &
                                                     RadDiag_Data%Channel(j)%bilap * RadDiag_Data%Channel(j)%bilap
          RadDiag_Stats%StdDev(iLpsR2,j) = RadDiag_Stats%StdDev(iLpsR2,j) + &
                                                     RadDiag_Data%Channel(j)%bilap2 * RadDiag_Data%Channel(j)%bilap2
          RadDiag_Stats%StdDev(iCLW  ,j) = RadDiag_Stats%StdDev(iCLW  ,j) + &
                                                     RadDiag_Data%Channel(j)%biclw * RadDiag_Data%Channel(j)%biclw
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



  ! Copy the bias correction coefficients into the AirMassCoeffients part of the structure 
  ! (for backward compatibility).
  RadDiag_Stats%AirMassCoefficients(1,:,:) = RadDiag_Stats%time_Mean(iScan,:,:)
  RadDiag_Stats%AirMassCoefficients(2,:,:) = RadDiag_Stats%time_Mean(iConst,:,:)
  RadDiag_Stats%AirMassCoefficients(3,:,:) = RadDiag_Stats%time_Mean(iAngle,:,:)
  RadDiag_Stats%AirMassCoefficients(4,:,:) = RadDiag_Stats%time_Mean(iLpsR,:,:)
  RadDiag_Stats%AirMassCoefficients(5,:,:) = RadDiag_Stats%time_Mean(iLpsR2,:,:)
  RadDiag_Stats%AirMassCoefficients(6,:,:) = RadDiag_Stats%time_Mean(iCLW,:,:)


  ! Replace the RadDiag_Stats nTimes dimension with
  ! the actual number of times/files read
  RadDiag_Stats%n_Times = m

  ! Compute the channel-scan averages
  WRITE(*, '( /5x, "Computing scan averages...." )' )

  FOV_Loop: DO k = 1, RadDiag_Stats%n_FOVS

    ! If FOV not used, go to next one
    IF ( RadDiag_Stats%FOV(k) == INVALID_FOV ) CYCLE FOV_Loop

    ! Loop over channels to compute averages
    DO j = 1, RadDiag_Stats%n_Channels
      rnSamples = REAL(RadDiag_Stats%scan_nSamples(j,k),sp)
      DO i = 1, RadDiag_Stats%n_Variables
        RadDiag_Stats%Scan_Mean(i,j,k) = RadDiag_Stats%Scan_Mean(i,j,k)/rnSamples
        RadDiag_Stats%Scan_StdDev(i,j,k) = &
             SQRT(RadDiag_Stats%Scan_StdDev(i,j,k)/rnSamples - &
             RadDiag_Stats%Scan_Mean(i,j,k)*RadDiag_Stats%Scan_Mean(i,j,k))
      END DO
    END DO
  END DO FOV_Loop


  ! Compute the channel-timeseries averages
  WRITE(*, '( /5x, "Computing timeseries averages...." )' )
  DateTime_Loop: DO m = 1, RadDiag_Stats%n_Times

    ! Loop over channels to compute averages
    DO j = 1, RadDiag_Stats%n_Channels
      rnSamples = REAL(RadDiag_Stats%time_nSamples(j,m),sp)
      DO i = 1, RadDiag_Stats%n_Variables
        RadDiag_Stats%time_Mean(i,j,m) = RadDiag_Stats%time_Mean(i,j,m)/rnSamples
        RadDiag_Stats%Time_StdDev(i,j,m) = &
             SQRT(RadDiag_Stats%Time_StdDev(i,j,m)/rnSamples - &
             RadDiag_Stats%Time_Mean(i,j,m)*RadDiag_Stats%Time_Mean(i,j,m))
     END DO
    END DO
  END DO DateTime_Loop

  ! Compute the channel only averages
  WRITE(*, '( /5x, "Computing channel averages...." )' )
  DO j = 1, RadDiag_Stats%n_Channels
     rnSamples = REAL(RadDiag_Stats%nSamples(j),sp)
     DO i = 1, RadDiag_Stats%n_Variables
        RadDiag_Stats%Mean(i,j) = RadDiag_Stats%Mean(i,j)/rnSamples
        RadDiag_Stats%StdDev(i,j) = &
             SQRT(RadDiag_Stats%StdDev(i,j)/rnSamples - &
             RadDiag_Stats%Mean(i,j)*RadDiag_Stats%Mean(i,j))
     END DO
  END DO


  ! Write averages to file
  IF (TRIM(Output_Filename) /= '') Output_Filename=TRIM(Output_Filename)//'_'
  Output_Filename=TRIM(Output_Filename)//TRIM(Sensor_ID)//'_'//&
       TRIM(gesanal)//'_'//TRIM(Surface_Text(Surface_Type))//'_'//First_TimeStamp//&
       '_'//Last_TimeStamp//'.nc'

  WRITE(Title,'("RadDiag Stats for ", a, " from ", i10, " to ", i10 )' ) &
              TRIM(Sensor_Id), &
              RadDiag_Stats%DateTime(1), &
              RadDiag_Stats%DateTime(RadDiag_Stats%n_Times)
  WRITE(*, '( /5x, "Writing output file ", a, "...." )' ) TRIM(Output_Filename)
  
  Error_Status = RadDiag_Stats_WriteFile( TRIM(Output_Filename), &
                                          RadDiag_Stats, &
                                          Title   = TRIM(Title), &
                                          History = PROGRAM_VERSION_ID, &
                                          Comment = TRIM(Comment) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error writing output file '//TRIM(Output_Filename), Error_Status)
    STOP
  END IF


  ! Cleanup
  CALL RadDiag_Stats_Destroy( RadDiag_Stats )

END PROGRAM Compute_RadDiag_Stats

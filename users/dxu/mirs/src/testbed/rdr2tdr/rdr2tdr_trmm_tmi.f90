!===============================================================================
! Name:       rdr2tdr_trmm_tmi.f90
!
!
! Type:       Main Program
!
!
! Description:
!        Program to convert raw data of TRMM_TMI into MIRS TDRs format.
!
! Modules needed:
!       - Consts
!       - misc
!       - IO_MeasurData
!       - IO_Noise
!       - IO_Misc
!       - USE ErrorHandling
!
! History:
!       07-01-2010      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!       07-21-2010      Wanchun Chen, DELL Inc @ NOAA/NESDIS/STAR
!       08-05-2011      Wanchun Chen, Modified to handele TRMM/TMI version 7
!
!===============================================================================

PROGRAM rdr2tdr_trmm_tmi

  USE Consts
  USE misc
  USE IO_MeasurData
  USE IO_Noise
  USE IO_Misc
  USE ErrorHandling

  IMPLICIT NONE
  
  !---- parameter section ----
  
  INTEGER, PARAMETER :: NFOV_LOW = 104, NCHAN_LOW = 7
  INTEGER, PARAMETER :: NFOV_HIGH = 208, NCHAN_HIGH = 2
  INTEGER, PARAMETER :: DEBUG=0
  
  !---- missing,validity,qac,geoQuality, plus 9 channels quality flags ----
  INTEGER, PARAMETER :: NQC = 13
  INTEGER, PARAMETER :: NQC_LOW = 11
  INTEGER, PARAMETER :: NQC_HIGH = 6

  real,dimension(NFOV_LOW,NCHAN_LOW)   :: tb1
  real,dimension(NFOV_HIGH,NCHAN_HIGH) :: tb2
  
  INTEGER,    PARAMETER   :: maxchar=20, maxchrfn=200
  INTEGER,    PARAMETER   :: nfovlr=104, nfovhr=2*104
  CHARACTER(LEN=maxchar ) :: vdata_name

  integer    :: i, retn, iscan, ichan, ifov, jfov
  integer(4) :: sd_id, sds_id, start(3), stride(3), edge(3)
  integer(4) :: file_id, vdata_id, n_rec, interlace
  integer(4) :: vdata_size
  integer(4) :: start2(2), stride2(2), edge2(2)
  integer(4) :: start1(1), stride1(1), edge1(1)

  
  integer              :: status, n_attrs
  integer              :: rank, data_type
  integer,dimension(3) :: dim_sizes
  character(len=16)    :: name
  
  !---- scan_time fields variable ----
  integer(4) :: vdata_id_year
  integer(4) :: vdata_id_month
  integer(4) :: vdata_id_dom
  integer(4) :: vdata_id_hour
  integer(4) :: vdata_id_minute
  integer(4) :: vdata_id_second
  integer(4) :: vdata_id_doy
  
  character(len=1) :: scantimes(9)
  
  integer(2), dimension(:), allocatable :: years
  integer(1), dimension(:), allocatable :: months
  integer(1), dimension(:), allocatable :: doms
  integer(1), dimension(:), allocatable :: hours
  integer(1), dimension(:), allocatable :: minutes
  integer(1), dimension(:), allocatable :: seconds
  integer(2), dimension(:), allocatable :: milliseconds
  integer(2), dimension(:), allocatable :: doys
  
  integer(1), dimension(:), allocatable :: qcs

  !---- corresponding utc times into mirs format ( in milli seconds ) ----
  integer(4), dimension(:), allocatable :: utcs
  
  !---- lat/lon/angle ----
  real, dimension(:,:),   allocatable :: lons,lons_low
  real, dimension(:,:),   allocatable :: lats,lats_low
  real, dimension(:,:),   allocatable :: angles,angles_low
  
  !---- not get values inside this code, just use default values ----
  real, dimension(NFOV_LOW)  :: relAziAngles_low = DEFAULT_VALUE_REAL
  real, dimension(NFOV_HIGH) :: relAziAngles_high = DEFAULT_VALUE_REAL

  !-----------------------------------------------------------------------------
  !  Output variables section
  !-----------------------------------------------------------------------------

  INTEGER :: iu_low, iu_high ! IU unit for low/high resolution output TDR files
  CHARACTER(LEN=256) :: tdrFileTmi_low, tdrFileTmi_high ! output TDR file names
  
  real, dimension(NCHAN_LOW)  :: freq_low  ! low resolution channel central frequency
  real, dimension(NCHAN_HIGH) :: freq_high ! high resolution channel central frequency
  
  !---- polarization: vertical or horizontal ( 4:V, 5:H ) ----
  integer, dimension(NCHAN_LOW)  :: polar_low 
  integer, dimension(NCHAN_HIGH) :: polar_high
  
  DATA freq_low  / 10.65, 10.65, 19.35, 19.35, 21.3, 37.0, 37.0 /
  DATA freq_high / 85.5, 85.5 /
  
  DATA polar_low  / 4, 5, 4, 5, 4, 4, 5 /
  DATA polar_high / 4, 5 /
  

  integer(2), dimension(:,:,:), allocatable :: lowresch
  integer(2), dimension(:,:,:), allocatable :: highresch

  real, dimension(:,:,:), allocatable :: tbs_low
  real, dimension(:,:,:), allocatable :: tbs_high
  
  !---- quality flags ----
  integer(1), dimension(:,:), allocatable :: qcs_low
  integer(1), dimension(:,:), allocatable :: qcs_high
  
  !--- ascending/descending passing mode ( 0: asc ; 1: des ) ----
  integer, dimension(:), allocatable :: nodes


  CHARACTER(LEN=256) :: fields
  CHARACTER(LEN=256) :: granule_name


  !---------------------------
  ! HDF Function declarations
  !---------------------------
  !integer hopen, vsfatch, vsfinq, vsfdtch, hclose
  integer vsfread, vsfsfld, vsffnd
  integer sfstart, sfselect, sfn2index, sfginfo, sfrdata, sfendacc, sfend

  integer, parameter :: DFACC_RDONLY = 1

  !---- Specify global varables ----
  INTEGER                             :: nfile,ifile
  INTEGER                             :: iu_list
  INTEGER(4)                          :: nscan

  !----Pointers and other allocatable arrays ----
  CHARACTER(LEN=256), DIMENSION(:), POINTER :: rdrFilesTmi,tdrFilesTmi

  !----Namelist data ---- 
  CHARACTER(LEN=256)                  :: rdrfileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=256)                  :: pathTDR=DEFAULT_VALUE_STR4
  INTEGER                             :: norbits2process=DEFAULT_VALUE_INT
  CHARACTER(LEN=256)                  :: logFile=DEFAULT_VALUE_STR4

  NAMELIST /ContrlRDR2TDR/rdrfileList,pathTDR,norbits2process,logFile


  !-------------------------------------------------------------------------------------
  !   Read control-data from namelist
  !-------------------------------------------------------------------------------------
  READ(*,NML=ContrlRDR2TDR)
  
  !---- open Log file ----
  CALL OpenLogFile(logFile)

  !----Read the file names of TRMM-TMI RDR data and build TDR files names, tdrFilesTmi not used here
  call ReadList(iu_list,trim(rdrfileList),rdrFilesTmi,nfile,tdrFilesTmi,pathTDR,'TDR_')
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'')
  nfile=minval((/nfile,norbits2process/))

  !---- loop the files ----
  fileLoop: DO ifile=1,nfile

    granule_name = trim(rdrFilesTmi(ifile))
    write(*,'(A)') trim(granule_name)

    !-----------------------------------------------------------
    ! Open granule for SDS access
    !-----------------------------------------------------------
    sd_id = sfstart(TRIM(granule_name), DFACC_RDONLY)
    if ( sd_id .eq. -1 ) then
       write(*,'(A)') 'Error opening HDF file:'//TRIM(granule_name)
       stop 1
    endif

    !-----------------------------------------------------------
    !    Year
    !    16-bit integer,    3019
    !    units = years
    !-----------------------------------------------------------
    !---- Select the desired SDS ----
    sds_id = sfselect(sd_id, sfn2index(sd_id,'Year'))
    !write(*,*) 'Year: sds_id=', sds_id
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the Year', sds_id
       stop 1
    endif
    
    !---- only need call once to get nscan ----
    status = sfginfo(sds_id, name, rank, dim_sizes, data_type, n_attrs)
    
    !---- debug ---
    !     write(*,*)  "name = ", name(1:16)
    !     write(*,*)  "rank = ", rank
    !     write(*,*)  "dimension sizes are : ", (dim_sizes(i), i=1, rank)
    !     write(*,*)  "data type is ", data_type
    !     write(*,*)  "number of attributes is ", n_attrs   
    
    !---- only assign once ----
    nscan = dim_sizes(1)
    
    !---- Read Year ----
    start1(1) = 0
    stride1(1) = 1
    edge1(1) = nscan
    
    allocate( years(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, years)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading Year',retn
      stop 1
    ENDIF
    
    if( DEBUG .eq. 1 ) then
      write(*,*) '******** Year ********'
      do i=nscan-10,nscan
        write(*,*) years(i)
      enddo
    endif
    
    !---- ends access to SDS ---- 
    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending Year',retn
       stop 1
    ENDIF



    !-----------------------------------------------------------
    !    Month
    !    8-bit integer,    3019
    !    units = months
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'Month'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the Month', sds_id
       stop 1
    endif

    allocate( months(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, months)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading Month',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** Month ********'
      do i=nscan-10,nscan
        write(*,*) months(i)
      enddo
    endif

    !---- ends access to Latitude ---- 
    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending Month',retn
       stop 1
    ENDIF



    !-----------------------------------------------------------
    !    DayOfMonth
    !    8-bit integer,    3019
    !    units = days
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'DayOfMonth'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the DayOfMonth', sds_id
       stop 1
    endif

    allocate( doms(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, doms)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading DayOfMonth',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** DayOfMonth ********'
      do i=nscan-10,nscan
        write(*,*) doms(i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending DayOfMonth',retn
       stop 1
    ENDIF



    !-----------------------------------------------------------
    !    Hour
    !    8-bit integer,    3019
    !    units = hours
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'Hour'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the Hour', sds_id
       stop 1
    endif

    allocate( hours(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, hours)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading Hour',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** Hour ********'
      do i=nscan-10,nscan
        write(*,*) hours(i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending Hour',retn
       stop 1
    ENDIF



    !-----------------------------------------------------------
    !    Minute
    !    8-bit integer,    3019
    !    units = minutes
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'Minute'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the Minute', sds_id
       stop 1
    endif

    allocate( minutes(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, minutes)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading Minute',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** Minute ********'
      do i=nscan-10,nscan
         write(*,*) minutes(i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending Minute',retn
       stop 1
    ENDIF



    !-----------------------------------------------------------
    !    Second
    !    8-bit integer,    3019
    !    units = seconds
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'Second'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the Second', sds_id
       stop 1
    endif

    allocate( seconds(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, seconds)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading Second',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** Second ********'
      do i=nscan-10,nscan
        write(*,*) seconds(i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending Second',retn
       stop 1
    ENDIF



    !-----------------------------------------------------------
    !    MilliSecond
    !    16-bit integer,    3019
    !    units = milliseconds
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'MilliSecond'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the MilliSecond', sds_id
       stop 1
    endif

    allocate( milliseconds(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, milliseconds)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading MilliSecond',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** MilliSecond ********'
      do i=nscan-10,nscan
        write(*,*) milliseconds(i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending MilliSecond',retn
       stop 1
    ENDIF



    !-----------------------------------------------------------
    !    DayOfYear
    !    16-bit integer,    3019
    !    units = days
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'DayOfYear'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the DayOfYear', sds_id
       stop 1
    endif

    allocate( doys(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, doys)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading DayOfYear',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** DayOfYear ********'
      do i=nscan-10,nscan
        write(*,*) doys(i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending DayOfYear',retn
       stop 1
    ENDIF





    !-----------------------------------------------------------
    !    LowResCh
    !    16-bit integer,    3019 x 104 x 7
    !    Number of attributes = 6
    !    scale_factor = 100.0
    !    scale_factor_err = 0.0
    !    add_offset = 100.0
    !    add_offset_err = 0.0
    !    calibrated_nt = 22
    !    units = K
    !-----------------------------------------------------------

    sds_id = sfselect(sd_id, sfn2index(sd_id,'lowResCh'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the lowResCh parameter',sds_id
       stop 1
    endif

    start(1) = 0
    start(2) = 0
    start(3) = 0

    stride(1) = 1
    stride(2) = 1
    stride(3) = 1

    edge(1) = NCHAN_LOW
    edge(2) = NFOV_LOW
    edge(3) = nscan

    allocate(lowresch(NCHAN_LOW,NFOV_LOW,nscan))
    allocate(tbs_low(NCHAN_LOW,NFOV_LOW,nscan))
    
    retn = sfrdata(sds_id, start, stride, edge, lowresch)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error reading lowResCh data', retn
       stop 1
    ENDIF

    tbs_low(:,:,:)  = lowresch(:,:,:)  * 0.01 + 100.0
    
    if( DEBUG .eq. 1 ) then
      !do iscan = 1, nscan
      !do ifov  = 1, NFOV_LOW
      !do ichan = 1, NCHAN_LOW
      !  tbs_low(ichan,ifov,iscan) = lowresch(ichan,ifov,iscan) * 0.01 + 100.0 
      !enddo
      !enddo
      !enddo
    
      print*,'Printing TBs from the 7 low resolution channels (SDS data):'
      do i=1,7
     	write(*,*) 'Low Res. Channel:', i,  '  Temperature(k):', tbs_low(i,1,10)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending lowresch SDS data',retn
       stop 1
    ENDIF

    deallocate(lowresch)
 


    !-----------------------------------------------------------
    !    highResCh
    !    16-bit integer,    3019 x 208 x 2
    !    Number of attributes = 6
    !    scale_factor = 100.0
    !    scale_factor_err = 0.0
    !    add_offset = 100.0
    !    add_offset_err = 0.0
    !    calibrated_nt = 22
    !    units = K
    !-----------------------------------------------------------
    	 
    sds_id = sfselect(sd_id, sfn2index(sd_id,'highResCh'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the highResCh parameter',sds_id
       stop 1
    endif

    start(1) = 0
    start(2) = 0
    start(3) = 0

    stride(1) = 1
    stride(2) = 1
    stride(3) = 1

    edge(1) = NCHAN_HIGH
    edge(2) = NFOV_HIGH
    edge(3) = nscan

    allocate(highresch(NCHAN_HIGH,NFOV_HIGH,nscan))
    allocate(tbs_high(NCHAN_HIGH,NFOV_HIGH,nscan))
    
    retn = sfrdata(sds_id, start, stride, edge, highresch)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error Reading highResCh data',retn
       stop 1
    ENDIF

    tbs_high(:,:,:) = highresch(:,:,:) * 0.01 + 100.0
    
    if( DEBUG .eq. 1 ) then
      !do iscan = 1, nscan
      !do ifov  = 1, NFOV_HIGH
      !do ichan = 1, NCHAN_HIGH
      !  tbs_high(ichan,ifov,iscan) = highresch(ichan,ifov,iscan) * 0.01 + 100.0
      !enddo
      !enddo
      !enddo

      write(*,*) 'Printing TBs from the 2 high resolution channels (SDS data):'
      do i=1,2
     	write(*,*) 'High Res. Channel:', i,  '  Temperature(k):', tbs_high(i,1,10)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending highResCh SDS data',retn
       stop 1
    ENDIF

    deallocate(highresch)



    !-----------------------------------------------------------
    !    Latitude 
    !    32-bit floating-point,    nscan x NFOV_HIGH: 3019 x 208
    !    units = degrees
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'Latitude'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the Latitude', sds_id
       stop 1
    endif

    start2(1) = 0
    start2(2) = 0

    stride2(1) = 1
    stride2(2) = 1

    edge2(1) = NFOV_HIGH
    edge2(2) = nscan
    
    allocate( lats(NFOV_HIGH,nscan) )
    retn = sfrdata(sds_id, start2, stride2, edge2, lats)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading Latitude',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** Latitude ********'
      do i=nscan-10,nscan
        write(*,*) lats(12,i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending Latitude',retn
       stop 1
    ENDIF

    allocate( lats_low(NFOV_LOW,nscan) )
    !---- low resolution lat take 1,3,5 of high resolution ---- 
    do iscan = 1, nscan
    do ifov  = 1, NFOV_LOW
      lats_low(ifov,iscan) = lats(ifov*2-1,iscan)
    enddo
    enddo



    !-----------------------------------------------------------
    !    Longitude 
    !    32-bit floating-point,    nscan x NFOV_HIGH: 3019 x 208
    !    units = degrees
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'Longitude'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the Longitude', sds_id
       stop 1
    endif

    start2(1) = 0
    start2(2) = 0

    stride2(1) = 1
    stride2(2) = 1

    edge2(1) = NFOV_HIGH
    edge2(2) = nscan
    
    allocate( lons(NFOV_HIGH,nscan) )
    retn = sfrdata(sds_id, start2, stride2, edge2, lons)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading Longitude',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** Longitude ********'
      do i=nscan-10,nscan
      	write(*,*) lons(12,i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending Longitude',retn
       stop 1
    ENDIF

    allocate( lons_low(NFOV_LOW,nscan) )
    !---- low resolution lat take 1,3,5 of high resolution ---- 
    do iscan = 1, nscan
    do ifov  = 1, NFOV_LOW
      lons_low(ifov,iscan) = lons(ifov*2-1,iscan)
    enddo
    enddo


    !-----------------------------------------------------------
    !    satLocZenAngle
    !    32-bit floating-point,    ncan x NFOV_HIGH: 3019 x 208
    !    units = degrees
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'satLocZenAngle'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the satLocZenAngle', sds_id
       stop 1
    endif

    start2(1) = 0
    start2(2) = 0

    stride2(1) = 1
    stride2(2) = 1

    edge2(1) = NFOV_HIGH
    edge2(2) = nscan
    
    allocate( angles(NFOV_HIGH,nscan) )
    retn = sfrdata(sds_id, start2, stride2, edge2, angles)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading satLocZenAngle',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** satLocZenAngle ********'
      do i=nscan-10,nscan
      	write(*,*) angles(12,i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending satLocZenAngle',retn
       stop 1
    ENDIF

    !---- low resolution satellite zenith angle take 1,3,5 of high resolution ---- 
    allocate( angles_low(NFOV_LOW,nscan) )
    do iscan = 1, nscan
    do ifov  = 1, NFOV_LOW
      angles_low(ifov,iscan) = angles(ifov*2-1,iscan)
    enddo
    enddo



    !-----------------------------------------------------------
    !    dataQuality
    !    8-bit integer,    3019
    !-----------------------------------------------------------
    sds_id = sfselect(sd_id, sfn2index(sd_id,'dataQuality'))
    if ( sds_id .eq. -1 ) then
       write(*,*) 'Error selecting the dataQuality', sds_id
       stop 1
    endif

    allocate( qcs(nscan) )
    retn = sfrdata(sds_id, start1, stride1, edge1, qcs)
    
    IF ( retn .ne. 0 ) THEN
      write(*,*) 'Error reading dataQuality',retn
      stop 1
    ENDIF

    if( DEBUG .eq. 1 ) then
      write(*,*) '******** dataQuality ********'
      do i=nscan-10,nscan
        write(*,*) qcs(i)
      enddo
    endif

    retn = sfendacc(sds_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error ending dataQuality',retn
       stop 1
    ENDIF

    allocate( qcs_low( NQC_LOW, nscan ) )
    allocate( qcs_high( NQC_HIGH, nscan ) )

    do iscan = 1, nscan
      qcs_low(1:NQC_LOW,iscan) = qcs(iscan)
      qcs_high(1:NQC_HIGH,iscan) = qcs(iscan)
    enddo



    !---- This closes the HDF to SDS access for all SDSs in the granule ----
    retn = sfend(sd_id)
    IF ( retn .ne. 0 ) THEN
       write(*,*) 'Error closing HDF to SDS access',retn
       stop 1
    ENDIF



    !-----------------------------------------------------------------------------------------------
    ! 
    ! prepare output parameters
    !
    !-----------------------------------------------------------------------------------------------
    allocate( nodes(nscan) )
    nodes(:) = 0
    do iscan = 1, nscan-1
      !---- use the center one ( 104 or 105 ) ----
      if( lats(104,iscan) .gt. lats(104,iscan+1) ) nodes(iscan) = 1
    enddo
    
    if( nodes(nscan-1) .eq. 1 ) nodes(nscan) = 1
    
   
    !-----------------------------------------------------------------------------------------------
    ! 
    ! output section to write into MIRS internal TDR format
    !
    !-----------------------------------------------------------------------------------------------

    !---- generate 2 output tdr file names ----
    call insert_path_string( rdrFilesTmi(ifile), TRIM(pathTDR), 'TDR_LR.', tdrFileTmi_low  )
    call insert_path_string( rdrFilesTmi(ifile), TRIM(pathTDR), 'TDR_HR.', tdrFileTmi_high )
    
    !---- wirte header part ----
    call WriteRadHdrScanLMode(tdrFileTmi_low, iu_low, nscan,NFOV_LOW, NQC_LOW, NCHAN_LOW, freq_low, polar_low)
    call WriteRadHdrScanLMode(tdrFileTmi_high,iu_high,nscan,NFOV_HIGH,NQC_HIGH,NCHAN_HIGH,freq_high,polar_high)

    allocate( utcs( nscan ) )
    
    do iscan = 1, nscan
      
      !---- mirs internal seconds are saved in milli-seconds of the day
      utcs = ( hours(iscan) * 3600 + minutes(iscan) * 60 + seconds(iscan) ) * 1000 + milliseconds(iscan)
      
      !---- swich NCHAN,NFOV ====> NFOV,NCHAN order ----
      do ichan = 1, NCHAN_LOW
      do ifov  = 1, NFOV_LOW
        tb1(ifov,ichan) = tbs_low(ichan,ifov,iscan)
      enddo
      enddo
      
      do ichan = 1, NCHAN_HIGH
      do ifov  = 1, NFOV_HIGH
        tb2(ifov,ichan) = tbs_high(ichan,ifov,iscan)
      enddo
      enddo
      
      !---- write scan line of low resolution data ----
      CALL WriteRadMeasScanLMode(iu_low,NQC_LOW,INT(qcs_low(1:NQC_LOW,iscan)),NCHAN_LOW,NFOV_LOW, &
             angles_low(1:NFOV_LOW,iscan),tb1(1:NFOV_LOW,1:NCHAN_LOW), &
             lats_low(1:NFOV_LOW,iscan),lons_low(1:NFOV_LOW,iscan),nodes(iscan), &
	     utcs(iscan),int(doys(iscan)),int(years(iscan)),relAziAngles_low(:),angles_low(1:NFOV_LOW,iscan) )

      !---- write scan line of high resolution data ----
      CALL WriteRadMeasScanLMode(iu_high,NQC_HIGH,INT(qcs_high(1:NQC_HIGH,iscan)),NCHAN_HIGH,NFOV_HIGH, &
             angles(1:NFOV_HIGH,iscan),tb2(1:NFOV_HIGH,1:NCHAN_HIGH), &
             lats(1:NFOV_HIGH,iscan),lons(1:NFOV_HIGH,iscan),nodes(iscan), &
	     utcs(iscan),int(doys(iscan)),int(years(iscan)),relAziAngles_high(:),angles(1:NFOV_HIGH,iscan) )

    enddo
    
    deallocate( years )
    deallocate( months )
    deallocate( doms )
    deallocate( hours )
    deallocate( minutes )
    deallocate( seconds )
    deallocate( milliseconds )
    deallocate( doys )
    
    deallocate( qcs )
    deallocate( qcs_low )
    deallocate( qcs_high )

    deallocate( lats_low )
    deallocate( lons_low )
    
    deallocate( lats )
    deallocate( lons )
    
    deallocate( tbs_low )
    deallocate( tbs_high )
    
    deallocate( angles_low )
    deallocate( angles )
    
    deallocate( nodes )
    deallocate( utcs )


    CLOSE(iu_low)
    CLOSE(iu_high)

  ENDDO fileLoop

  DEALLOCATE(rdrFilesTmi,tdrFilesTmi)
  CALL CloseLogFile()

END PROGRAM rdr2tdr_trmm_tmi

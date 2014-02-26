 !==============================================================================
 !
 ! Name:        interEdr
 !
 ! Type:        F90 Program
 !
 ! Description: P2P collocate of EDR products from 2 satellites
 !
 !
 ! Namelist Parameters: 
 !
 !    Parameter          I/O    Desc.
 !  -----------------------------------------------------------------------
 !    satId1             I      String Sensor ID 1
 !    satId2             I      String Sensor ID 2
 !    List1              I      List of filenames to collocated with List2
 !    List2              I      List of filenames to collocated with List1
 !    Files1Out          I      name of aggregated output file from List1 data
 !    Files2Out          I      name of aggregated output file from List2 data
 !    TimeCut            I      maximum allowed time difference (hours)
 !
 ! Modules needed:
 !    -IO_Misc
 !    -Consts
 !    -utils
 !    -misc
 !    -ErrorHandling
 !
 ! Subroutines Contained:
 !    - distance
 !    - fov_amsua
 !    - readEDRHeader
 !    - readEDR
 !    -
 !    -
 !
 ! History
 !
 !    -  02-01-2011        Wanchun Chen   Original Coder
 !    -  11-31-2011        Wanchun Chen   Added fmType to deal different resol.
 !                                        Dynamicly allocate memory now
 !    -  02-17-2011        Wanchun Chen   Updated to handle un-matched pair
 !==============================================================================
	
Program interEdr

  USE IO_Misc
  USE Consts
  USE utils
  USE misc
  USE ErrorHandling
  USE IO_Scene

  IMPLICIT NONE
  
  !---- Parameters ----
  INTEGER, PARAMETER                         :: NLAY_IN=100
  INTEGER, PARAMETER                         :: NLEV=101
  INTEGER, PARAMETER                         :: NPOS=30
  INTEGER, PARAMETER                         :: NABSORB=2
  INTEGER, PARAMETER                         :: NQC=4
  INTEGER, PARAMETER                         :: NLAY=11
  INTEGER                                    :: NCHAN=20    ! number of matchec channels 
  INTEGER                                    :: NCHAN_1=20  ! number of sat1 channels
  INTEGER                                    :: NCHAN_2=20  ! number of sat2 channels
 
  !---- layers of pressure ----
  REAL,DIMENSION(NLAY)                       :: PLAYERS=(/100.,200.,300.,400.,500.,600.,&
                                                          700.,800.,850.,900.,950./)
  
  !---- Outer loop EDR header variables ----
  CHARACTER(LEN=256), DIMENSION(:), POINTER  :: Files1In
  INTEGER                                    :: iu_1in, ierr1
  INTEGER            	                     :: iTyp1,AlgSN1,nprf1,nLay1,nLev1,nChan1,nPosScan1,nScanLine1
  INTEGER		                     :: nAbsorb1,nParmCLW1,nParmRain1,nParmSnow1,nParmIce1,nParmGrpl1
  INTEGER, DIMENSION(NABSORB)                :: AbsorbID1
  REAL,    DIMENSION(:), ALLOCATABLE         :: freq1
  INTEGER, DIMENSION(:), ALLOCATABLE         :: polar1
  INTEGER                                    :: nqc1
  
  !---- Outer loop EDR variables ----
  INTEGER                                    :: iprf1        !Profile Index 
  REAL,       DIMENSION(NLAY_IN)	     :: pres_lay1    !Layer pressure grid
  REAL,       DIMENSION(NLEV)	             :: pres_lev1    !Level pressure grid
  REAL,       DIMENSION(NLAY_IN)	     :: temp_lay1    !Layer temperature profile
  REAL,       DIMENSION(NLAY_IN,NABSORB)     :: absorb_lay1  !Layer aborbents profiles
  REAL,       DIMENSION(NLAY_IN)	     :: clw_lay1     !Cloud amount vector
  REAL,       DIMENSION(NLAY_IN)	     :: rain_lay1    !Rain amount vector
  REAL,       DIMENSION(NLAY_IN)	     :: graupel_lay1 !Graupel amount vector
  REAL,       DIMENSION(:), ALLOCATABLE      :: emis1        !Emissivities vector
  REAL                                       :: angle1       !Scan Angle
  REAL                                       :: windSp1      !Wind speed
  REAL                                       :: tskin1       !Skin temperature
  REAL                                       :: psfc1        !Surface pressure
  INTEGER                                    :: sfc1         !Surface type ID
  REAL                                       :: windU1       !U-direction wind speed
  REAL                                       :: windV1       !V-direction wind speed
  REAL                                       :: relAziAngle1 !Relative Azimuth Angle
  REAL                                       :: solZenAngle1 !Solar Zenith Angle
  REAL                                       :: snowdepth1   !Snow Depth
  INTEGER(2),        DIMENSION(NQC)          :: qc1          !QC vector
  REAL                                       :: lat1         !Latitude
  REAL                                       :: lon1         !Longitude
  INTEGER                                    :: node1        !=0->ASC, =1->DESC
  REAL                                       :: scanUTC1     !UTC time
  INTEGER                                    :: scanYear1    !Year
  INTEGER                                    :: scanDAY1     !Day 
  INTEGER                                    :: iscanPos1    !Scan position 
  INTEGER                                    :: iScanLine1   !Scan line Index
  INTEGER                                    :: nAttempt1    !Number of attempts performed for retrieval
  INTEGER                                    :: nIter1       !Number of iterations
  REAL                                       :: ChiSq1       !Convergence metric
  REAL,     DIMENSION(:), ALLOCATABLE        :: yfwd1        !Last forward simulated TBs in retr.
  INTEGER,  DIMENSION(:), ALLOCATABLE        :: ChanSel1     !Channels selection used in retr.
  REAL,     DIMENSION(:), ALLOCATABLE        :: ym1          !Measured TBs used for retrieval (uncorrected)
  REAL,     DIMENSION(:), ALLOCATABLE        :: ymcorr1      !Measured TBs used for retrieval (corrected)
  
  !---- Inner loop EDR header variables ----
  CHARACTER(LEN=256), DIMENSION(:), POINTER  :: Files2In
  INTEGER                                    :: iu_2in, ierr2
  INTEGER		                     :: iTyp2,AlgSN2,nprf2,nLay2,nLev2,nChan2,nPosScan2,nScanLine2
  INTEGER		                     :: nAbsorb2,nParmCLW2,nParmRain2,nParmSnow2,nParmIce2,nParmGrpl2
  INTEGER, DIMENSION(NABSORB)                :: AbsorbID2
  REAL,    DIMENSION(:), ALLOCATABLE         :: freq2
  INTEGER, DIMENSION(:), ALLOCATABLE         :: polar2
  INTEGER		                     :: nqc2

  !----Inner loop EDR variables ----
  INTEGER                                    :: iprf2        !Profile Index 
  REAL,      DIMENSION(NLAY_IN)              :: pres_lay2    !Layer pressure grid
  REAL,      DIMENSION(NLEV)                 :: pres_lev2    !Level pressure grid
  REAL,      DIMENSION(NLAY_IN)              :: temp_lay2    !Layer temperature profile
  REAL,      DIMENSION(NLAY_IN,NABSORB)      :: absorb_lay2  !Layer aborbents profiles
  REAL,      DIMENSION(NLAY_IN)              :: clw_lay2     !Cloud amount vector
  REAL,      DIMENSION(NLAY_IN)              :: rain_lay2    !Rain amount vector
  REAL,      DIMENSION(NLAY_IN)              :: graupel_lay2 !Graupel amount vector
  REAL,      DIMENSION(:), ALLOCATABLE       :: emis2        !Emissivities vector
  REAL                                       :: angle2       !Scan Angle
  REAL                                       :: windSp2      !Wind speed
  REAL                                       :: tskin2       !Skin temperature
  REAL                                       :: psfc2        !Surface pressure
  INTEGER                                    :: sfc2         !Surface type ID
  REAL                                       :: windU2       !U-direction wind speed
  REAL                                       :: windV2       !V-direction wind speed
  REAL                                       :: relAziAngle2 !Relative Azimuth Angle
  REAL                                       :: solZenAngle2 !Solar Zenith Angle
  REAL                                       :: snowdepth2   !Snow Depth
  INTEGER(2),        DIMENSION(NQC)          :: qc2          !QC vector
  REAL                                       :: lat2         !Latitude
  REAL                                       :: lon2         !Longitude
  INTEGER                                    :: node2        !=0->ASC, =1->DESC
  REAL                                       :: scanUTC2     !UTC time
  INTEGER                                    :: scanYear2    !Year
  INTEGER                                    :: scanDAY2     !Day 
  INTEGER                                    :: iscanPos2    !Scan position 
  INTEGER                                    :: iScanLine2   !Scan line Index 
  INTEGER                                    :: nAttempt2    !Number of attempts performed for retrieval
  INTEGER                                    :: nIter2       !Number of iterations
  REAL                                       :: ChiSq2       !Convergence metric
  REAL,     DIMENSION(:), ALLOCATABLE        :: yfwd2	     !Last forward simulated TBs in retr.
  INTEGER,  DIMENSION(:), ALLOCATABLE        :: ChanSel2     !Channels selection used in retr.
  REAL,     DIMENSION(:), ALLOCATABLE        :: Ym2	     !Measured TBs used for retrieval (uncorrected)
  REAL,     DIMENSION(:), ALLOCATABLE        :: ymcorr2      !Measured TBs used for retrieval (corrected)
  
  
  !---- loop variables ----
  INTEGER                                    :: iu_list1, iu_list2, nFiles1,nFiles2
  INTEGER                                    :: iFile_1,iFile_2,iProf1,iProf2
  INTEGER                                    :: ifile, nprf
  
  !---- distance variable ----
  REAL, PARAMETER                            :: R = 6371.0
  !REAL, PARAMETER                           :: PI = 3.1415926
  REAL                                       :: delta_lat, delta_lon
  REAL                                       :: a, c, distance
  
  !---- shared output variables ----  
  REAL, DIMENSION(NLAY) 		     :: PRES_LAY_OUT=(/100.,200.,300.,400.,500.,600.,&
                                                               700.,800.,850.,900.,950./)
  REAL, DIMENSION(NLAY) 		     :: Temp_lay, Wv_lay
  INTEGER, DIMENSION(NQC)                    :: qc !QC vector, we convert from 2 bytes into a 4 byte integer
  REAL                                       :: timeDiff,distDiff
  LOGICAL                                    :: meetCriteria  ! space & time meet selected criteria or not
  REAL                                       :: time1, time2
  INTEGER                                    :: iscan1,DistCut
  INTEGER                                    :: nCollocated = 0, nCollocated1 = 0


  !---- output filenames ----
  CHARACTER(LEN=256), DIMENSION(:), POINTER     :: Files1Out,Files2Out
  
  !---- P2P parameters ----
  INTEGER, PARAMETER                            :: NP2P=375000
  INTEGER, PARAMETER                            :: NHALF=187500
  INTEGER, PARAMETER                            :: NCEND=2
  INTEGER, PARAMETER                            :: NPROD_1D=10
  INTEGER, PARAMETER                            :: NPROD_NCHAN=3
  INTEGER, PARAMETER                            :: NPROD_NLAY=2
  INTEGER,DIMENSION(NCEND)                      :: IP2P=0
  INTEGER                                       :: ichan=0, ilay=0, nTot1=0, nTot2=0, nTot, iTot, iTot1, iTot2
  REAL,PARAMETER                                :: missing=-999.0
  
  !---- BIG buffer arrays to hold all parameters ----
  INTEGER,DIMENSION(:),  ALLOCATABLE            :: BIG_qc1
  REAL,DIMENSION(:,:),   ALLOCATABLE            :: BIG_prod1_1D
  REAL,DIMENSION(:,:,:), ALLOCATABLE            :: BIG_prod1_NCHAN
  REAL,DIMENSION(:,:,:), ALLOCATABLE            :: BIG_prod1_NLAY
  
  INTEGER,DIMENSION(:),  ALLOCATABLE            :: BIG_qc2
  REAL,DIMENSION(:,:),   ALLOCATABLE            :: BIG_prod2_1D
  REAL,DIMENSION(:,:,:), ALLOCATABLE            :: BIG_prod2_NCHAN
  REAL,DIMENSION(:,:,:), ALLOCATABLE            :: BIG_prod2_NLAY
  
  
  !---- P2P variables ----
  REAL,DIMENSION(:,:,:),   ALLOCATABLE          :: P2P_prod1_1D
  REAL,DIMENSION(:,:,:,:), ALLOCATABLE          :: P2P_prod1_NCHAN
  REAL,DIMENSION(:,:,:,:), ALLOCATABLE          :: P2P_prod1_NLAY
  
  REAL,DIMENSION(:,:,:),   ALLOCATABLE          :: P2P_prod2_1D
  REAL,DIMENSION(:,:,:,:), ALLOCATABLE          :: P2P_prod2_NCHAN
  REAL,DIMENSION(:,:,:,:), ALLOCATABLE          :: P2P_prod2_NLAY
  
  INTEGER,DIMENSION(:), ALLOCATABLE	        :: index1_chan
  INTEGER,DIMENSION(:), ALLOCATABLE	        :: index2_chan
  
  CHARACTER(LEN=256)                            :: p2pFile
  CHARACTER(LEN=2), DIMENSION(2)                :: cendIds=(/'as','ds'/)
  CHARACTER(LEN=8), DIMENSION(NPROD_1D)         :: prodIds_1D=(/ 'lat     ', 'lon     ', 'jday    ', 'node    ', &
                                                                 'scanpos ', 'angle   ', 'sfc     ', 'psfc    ', &
                                                                 'tskin   ', 'time    ' /)
  CHARACTER(LEN=3), DIMENSION(NPROD_NCHAN)      :: prodIds_NCHAN=(/ 'em ', 'tbu', 'tbc' /)
  CHARACTER(LEN=4), DIMENSION(NPROD_NLAY)       :: prodIds_NLAY=(/ 'temp',   'wv  ' /)

 
  !---- variables of asymmetry ----
  INTEGER, PARAMETER                            :: NSFC=5
  INTEGER                                       :: NBIN=0
  INTEGER, DIMENSION(:),ALLOCATABLE             :: BIN_BOX
  INTEGER                                       :: isfc, ibin, iprf, icend, sfc, iprod, irec=0
  REAL   	                                :: diff=-999.0
  REAL                                          :: angle=0.0
  INTEGER                                       :: scanpos=0
  LOGICAL                                       :: isAngle=.true.
 
  REAL,   DIMENSION(:,:,:,:),ALLOCATABLE        :: asym_val_1D,stdv_val_1D
  INTEGER,DIMENSION(:,:,:,:),ALLOCATABLE        :: asym_cnt_1D,stdv_cnt_1D
  REAL,   DIMENSION(:,:,:,:,:),ALLOCATABLE      :: asym_val_NCHAN,stdv_val_NCHAN
  INTEGER,DIMENSION(:,:,:,:,:),ALLOCATABLE      :: asym_cnt_NCHAN,stdv_cnt_NCHAN
  REAL,   DIMENSION(:,:,:,:,:),ALLOCATABLE      :: asym_val_NLAY,stdv_val_NLAY
  INTEGER,DIMENSION(:,:,:,:,:),ALLOCATABLE      :: asym_cnt_NLAY,stdv_cnt_NLAY

 
  !---- FOV sizes ----
  INTEGER                                       :: NFOV=30, NFOV1=30, NFOV2=30
  REAL, DIMENSION(:), ALLOCATABLE               :: fov_sizes, fov1_sizes, fov2_sizes

  !---Namelist variables ----
  CHARACTER(LEN=16)                             :: satId1,satId2
  CHARACTER(LEN=256)                            :: List1,List2
  CHARACTER(LEN=10)                             :: yyyymmdd
  CHARACTER(LEN=256)                            :: p2pDir
  REAL                                          :: TimeCut
  INTEGER                                       :: fmType=0
  
  CHARACTER(LEN=32)                             :: pair
  
  NAMELIST /CollCntrl/satId1,satId2,List1,List2,yyyymmdd,p2pDir,TimeCut,fmType

  !---- execution section begin here ----
  read(*,NML=CollCntrl)
  
  pair = TRIM(satId1)//'_'//TRIM(satId2)
  
  if( ( strcmp(satId1,'n18')    .eq. 0 .or. &
        strcmp(satId1,'n19')    .eq. 0 .or. &
        strcmp(satId1,'metopA') .eq. 0 .or. &
        strcmp(satId1,'metopB') .eq. 0 )    &
                 .and.                      &
      ( strcmp(satId2,'n18')    .eq. 0 .or. &
        strcmp(satId2,'n19')    .eq. 0 .or. &
        strcmp(satId2,'metopA') .eq. 0 .or. &
        strcmp(satId2,'metopB') .eq. 0 )    ) then
   
    NCHAN = 20
    NCHAN_1 = 20
    NCHAN_2 = 20
    
    allocate( index1_chan(NCHAN) )
    allocate( index2_chan(NCHAN) )
    do ichan = 1, NCHAN
      index1_chan(ichan) = ichan
      index2_chan(ichan) = ichan
    enddo
    
    isAngle = .true.
    NBIN = 12
    ALLOCATE( BIN_BOX( NBIN ) )
    do ibin = 1, NBIN
      BIN_BOX(ibin) = -55 + 10 * (ibin-1)
    enddo
    
    if( fmType .eq. 0 ) then
      NFOV=30
      ALLOCATE(fov1_sizes(1:NFOV))
      ALLOCATE(fov2_sizes(1:NFOV))
      call fov_amsua( fov1_sizes )
      call fov_amsua( fov2_sizes )
    else
      NFOV=90
      ALLOCATE(fov1_sizes(1:NFOV))
      ALLOCATE(fov2_sizes(1:NFOV))
      call fov_mhs( fov1_sizes )
      call fov_mhs( fov2_sizes )
    endif  
  
  else if ( strcmp(pair,'f16_f18') .eq. 0 .or. strcmp(pair,'f18_f16') .eq. 0 ) then

    NCHAN = 19
    NCHAN_1 = 24
    NCHAN_2 = 24

    allocate( index1_chan(NCHAN) )
    allocate( index2_chan(NCHAN) )
    do ichan = 1, NCHAN
      index1_chan(ichan) = ichan+5
      index2_chan(ichan) = ichan+5
    enddo

    if( fmType .eq. 0 ) then
      NFOV=30
    else if( fmType .eq. 1 ) then
      NFOV=60
    else if( fmType .eq. 2 ) then
      NFOV=90
    else if( fmType .eq. 3 ) then
      NFOV=180
    else
      STOP 'Error: Incorrect fmType for SSMIS ( F16 vs F18 )'
    endif
    ALLOCATE(fov1_sizes(1:NFOV))
    ALLOCATE(fov2_sizes(1:NFOV))
    
    if( fmType .eq. 0 ) then
      fov1_sizes(:) = 75    ! SSMI/S UAS sample interval and foot print size
      fov2_sizes(:) = 75    ! SSMI/S UAS sample interval and foot print size
    else if( fmType .eq. 1 ) then
      fov1_sizes(:) = 37.5  ! SSMI/S LAS sample interval and foot print size 
      fov2_sizes(:) = 37.5  ! SSMI/S LAS sample interval and foot print size 
    else if( fmType .eq. 2 ) then
      fov1_sizes(:) = 25    ! SSMI/S ENV sample interval and foot print size
      fov2_sizes(:) = 25    ! SSMI/S ENV sample interval and foot print size
    else if( fmType .eq. 3 ) then
      fov1_sizes(:) = 12.5  ! SSMI/S IMG sample interval and foot print size
      fov2_sizes(:) = 12.5  ! SSMI/S IMG sample interval and foot print size
    else
      fov1_sizes(:) = 75    ! default SSMI/S UAS sample interval and foot print size
      fov2_sizes(:) = 75    ! default SSMI/S UAS sample interval and foot print size
    endif

    isAngle = .false.
    NBIN = 30
    ALLOCATE( BIN_BOX( NBIN ) )
    do ibin = 1, NBIN
      BIN_BOX(ibin) = ibin
    enddo
   
  else if ( strcmp(pair,'f16_f17') .eq. 0 .or. strcmp(pair,'f17_f16') .eq. 0 ) then

    NCHAN = 19
    NCHAN_1 = 24
    NCHAN_2 = 24

    allocate( index1_chan(NCHAN) )
    allocate( index2_chan(NCHAN) )
    do ichan = 1, NCHAN
      index1_chan(ichan) = ichan+5
      index2_chan(ichan) = ichan+5
    enddo

    if( fmType .eq. 0 ) then
      NFOV=30
    else if( fmType .eq. 1 ) then
      NFOV=60
    else if( fmType .eq. 2 ) then
      NFOV=90
    else if( fmType .eq. 3 ) then
      NFOV=180
    else
      STOP 'Error: Incorrect fmType for SSMIS ( F16 vs F17 )'
    endif
    ALLOCATE(fov1_sizes(1:NFOV))
    ALLOCATE(fov2_sizes(1:NFOV))
    
    if( fmType .eq. 0 ) then
      fov1_sizes(:) = 75    ! SSMI/S UAS sample interval and foot print size
      fov2_sizes(:) = 75    ! SSMI/S UAS sample interval and foot print size
    else if( fmType .eq. 1 ) then
      fov1_sizes(:) = 37.5  ! SSMI/S LAS sample interval and foot print size 
      fov2_sizes(:) = 37.5  ! SSMI/S LAS sample interval and foot print size 
    else if( fmType .eq. 2 ) then
      fov1_sizes(:) = 25    ! SSMI/S ENV sample interval and foot print size
      fov2_sizes(:) = 25    ! SSMI/S ENV sample interval and foot print size
    else if( fmType .eq. 3 ) then
      fov1_sizes(:) = 12.5  ! SSMI/S IMG sample interval and foot print size
      fov2_sizes(:) = 12.5  ! SSMI/S IMG sample interval and foot print size
    else
      fov1_sizes(:) = 75    ! default SSMI/S UAS sample interval and foot print size
      fov2_sizes(:) = 75    ! default SSMI/S UAS sample interval and foot print size
    endif

    isAngle = .false.
    NBIN = 30
    ALLOCATE( BIN_BOX( NBIN ) )
    do ibin = 1, NBIN
      BIN_BOX(ibin) = ibin
    enddo
   
  else if ( strcmp(pair,'f17_f18') .eq. 0 .or. strcmp(pair,'f18_f17') .eq. 0 ) then

    NCHAN = 19
    NCHAN_1 = 24
    NCHAN_2 = 24

    allocate( index1_chan(NCHAN) )
    allocate( index2_chan(NCHAN) )
    do ichan = 1, NCHAN
      index1_chan(ichan) = ichan+5
      index2_chan(ichan) = ichan+5
    enddo

    if( fmType .eq. 0 ) then
      NFOV=30
    else if( fmType .eq. 1 ) then
      NFOV=60
    else if( fmType .eq. 2 ) then
      NFOV=90
    else if( fmType .eq. 3 ) then
      NFOV=180
    else
      STOP 'Error: Incorrect fmType for SSMIS ( F17 vs F18 )'
    endif
    ALLOCATE(fov1_sizes(1:NFOV))
    ALLOCATE(fov2_sizes(1:NFOV))
    
    if( fmType .eq. 0 ) then
      fov1_sizes(:) = 75    ! SSMI/S UAS sample interval and foot print size
      fov2_sizes(:) = 75    ! SSMI/S UAS sample interval and foot print size
    else if( fmType .eq. 1 ) then
      fov1_sizes(:) = 37.5  ! SSMI/S LAS sample interval and foot print size 
      fov2_sizes(:) = 37.5  ! SSMI/S LAS sample interval and foot print size 
    else if( fmType .eq. 2 ) then
      fov1_sizes(:) = 25    ! SSMI/S ENV sample interval and foot print size
      fov2_sizes(:) = 25    ! SSMI/S ENV sample interval and foot print size
    else if( fmType .eq. 3 ) then
      fov1_sizes(:) = 12.5  ! SSMI/S IMG sample interval and foot print size
      fov2_sizes(:) = 12.5  ! SSMI/S IMG sample interval and foot print size
    else
      fov1_sizes(:) = 75    ! default SSMI/S UAS sample interval and foot print size
      fov2_sizes(:) = 75    ! default SSMI/S UAS sample interval and foot print size
    endif

    isAngle = .false.
    NBIN = 30
    ALLOCATE( BIN_BOX( NBIN ) )
    do ibin = 1, NBIN
      BIN_BOX(ibin) = ibin
    enddo
   
   ! 1st poes, 2nd npp atms
   else if ( strcmp(pair,'n18_npp')    .eq. 0 .or. &
             strcmp(pair,'n19_npp')    .eq. 0 .or. &
             strcmp(pair,'metopA_npp') .eq. 0 .or. &
             strcmp(pair,'metopB_npp') .eq. 0 ) then
    
    NCHAN = 13
    NCHAN_1 = 20
    NCHAN_2 = 22
  
    allocate( index1_chan(NCHAN) )
    allocate( index2_chan(NCHAN) )
    
    ! poes:
    !chanIds=['23v','31v','53h','54h','55h','57h1','57h2','57h3','57h4','57h5','57h6','184h','186h']
    !          1     2     5     6     8     9      10     11     12     13     14     18     19
    
    ! npp/atms:
    !IDL: ind2s = [0, 1, 5, 6, 8,  9, 10, 11, 12, 13, 14, 21, 19]
    !For: ind2s = [1, 2, 6, 7, 9, 10, 11, 12, 13, 14, 15, 22, 20]
    
    index1_chan(1) = 1
    index1_chan(2) = 2
    index1_chan(3) = 5
    index1_chan(4) = 6
    index1_chan(5) = 8
    index1_chan(6) = 9
    index1_chan(7) = 10
    index1_chan(8) = 11
    index1_chan(9) = 12
    index1_chan(10) = 13
    index1_chan(11) = 14
    index1_chan(12) = 18
    index1_chan(13) = 19
    
    index2_chan(1) = 1
    index2_chan(2) = 2
    index2_chan(3) = 6
    index2_chan(4) = 7
    index2_chan(5) = 9
    index2_chan(6) = 10
    index2_chan(7) = 11
    index2_chan(8) = 12
    index2_chan(9) = 13
    index2_chan(10) = 14
    index2_chan(11) = 15
    index2_chan(12) = 22
    index2_chan(13) = 20

    isAngle = .true.
    NBIN = 12
    ALLOCATE( BIN_BOX( NBIN ) )
    do ibin = 1, NBIN
      BIN_BOX(ibin) = -55 + 10 * (ibin-1)
    enddo
    
    if( fmType .eq. 0 ) then
      NFOV1=30
      NFOV2=32
      allocate( fov1_sizes(1:NFOV1))
      allocate( fov2_sizes(1:NFOV2))
      call fov_amsua( fov1_sizes )
      call fov_npp_lr( fov2_sizes )
    else
      NFOV1=90
      NFOV2=96
      allocate( fov1_sizes(1:NFOV1))
      allocate( fov2_sizes(1:NFOV2))
      call fov_mhs( fov1_sizes )
      call fov_npp_hr( fov2_sizes )
    endif  
  
   ! 1st npp/atms, 2nd poes
   else if ( strcmp(pair,'npp_n18')    .eq. 0 .or. &
             strcmp(pair,'npp_n19')    .eq. 0 .or. &
             strcmp(pair,'npp_metopA') .eq. 0 .or. &
             strcmp(pair,'npp_metopB') .eq. 0 ) then
    
    NCHAN = 13
    NCHAN_1 = 22
    NCHAN_2 = 20
  
    ! npp/atms:
    !IDL: ind2s = [0, 1, 5, 6, 8,  9, 10, 11, 12, 13, 14, 21, 19]
    !For: ind2s = [1, 2, 6, 7, 9, 10, 11, 12, 13, 14, 15, 22, 20]
    
    ! poes:
    !chanIds=['23v','31v','53h','54h','55h','57h1','57h2','57h3','57h4','57h5','57h6','184h','186h']
    !          1     2     5     6     8     9      10     11     12     13     14     18     19
    
    index1_chan(1) = 1
    index1_chan(2) = 2
    index1_chan(3) = 6
    index1_chan(4) = 7
    index1_chan(5) = 9
    index1_chan(6) = 10
    index1_chan(7) = 11
    index1_chan(8) = 12
    index1_chan(9) = 13
    index1_chan(10) = 14
    index1_chan(11) = 15
    index1_chan(12) = 22
    index1_chan(13) = 20

    index2_chan(1) = 1
    index2_chan(2) = 2
    index2_chan(3) = 5
    index2_chan(4) = 6
    index2_chan(5) = 8
    index2_chan(6) = 9
    index2_chan(7) = 10
    index2_chan(8) = 11
    index2_chan(9) = 12
    index2_chan(10) = 13
    index2_chan(11) = 14
    index2_chan(12) = 18
    index2_chan(13) = 19
    
    isAngle = .true.
    NBIN = 12
    ALLOCATE( BIN_BOX( NBIN ) )
    do ibin = 1, NBIN
      BIN_BOX(ibin) = -55 + 10 * (ibin-1)
    enddo
    
    if( fmType .eq. 0 ) then
      NFOV1=32
      NFOV2=30
      allocate( fov1_sizes(1:NFOV1))
      allocate( fov2_sizes(1:NFOV2))
      call fov_npp_lr( fov1_sizes )
      call fov_amsua( fov2_sizes )
    else
      NFOV1=96
      NFOV2=90
      allocate( fov1_sizes(1:NFOV1))
      allocate( fov2_sizes(1:NFOV2))
      call fov_npp_hr( fov1_sizes )
      call fov_mhs( fov2_sizes )
    endif  
  
  else
    STOP 'Unsupported pair'
  endif


  !---Open index file and read ----
  CALL ReadList(iu_list1,List1,Files1In,nFiles1,Files1Out,'./','FILES1')
  CALL ReadList(iu_list2,List2,Files2In,nFiles2,Files2Out,'./','FILES2')

  !---- to get the total number of profiles by looping all files
  nTot1 = 0 
  DO ifile = 1, nFiles1
    CALL getSceneNprf( Files1In(ifile), nprf )
    nTot1 = nTot1 + nprf
  ENDDO
  write(*,*) 'nTot1 = ', nTot1
  
  nTot2 = 0 
  DO ifile = 1, nFiles2
    CALL getSceneNprf( Files2In(ifile ), nprf )
    nTot2 = nTot2 + nprf
  ENDDO
  write(*,*) 'nTot2 = ', nTot2
  
  !---- pick min one lah
  if( nTot1 .lt. nTot2 ) then
    nTot = nTot1
  else
    nTot = nTot2
  endif

  !---- Allocate arrays which depending on NCHAN ----
  ALLOCATE( freq1(NCHAN_1) )
  ALLOCATE( polar1(NCHAN_1) )
  ALLOCATE( emis1(NCHAN_1) )
  ALLOCATE( yfwd1(NCHAN_1) )
  ALLOCATE( ChanSel1(NCHAN_1) )
  ALLOCATE( ym1(NCHAN_1) )
  ALLOCATE( ymCorr1(NCHAN_1) )

  ALLOCATE( freq2(NCHAN_2) )
  ALLOCATE( polar2(NCHAN_2) )
  ALLOCATE( emis2(NCHAN_2) )
  ALLOCATE( yfwd2(NCHAN_2) )
  ALLOCATE( ChanSel2(NCHAN_2) )
  ALLOCATE( ym2(NCHAN_2) )
  ALLOCATE( ymCorr2(NCHAN_2) )

  ALLOCATE( BIG_qc1(nTot1) )
  ALLOCATE( BIG_qc2(nTot2) )

  ALLOCATE( BIG_prod1_1D(nTot1,NPROD_1D) )
  ALLOCATE( BIG_prod2_1D(nTot2,NPROD_1D) )

  ALLOCATE( BIG_prod1_NCHAN(nTot1,NCHAN_1,NPROD_NCHAN) )
  ALLOCATE( BIG_prod2_NCHAN(nTot2,NCHAN_2,NPROD_NCHAN) )

  ALLOCATE( BIG_prod1_NLAY(nTot1,NLAY,NPROD_NLAY) )
  ALLOCATE( BIG_prod2_NLAY(nTot2,NLAY,NPROD_NLAY) )


  ALLOCATE( P2P_prod1_1D(nTot,NPROD_1D,NCEND) )
  ALLOCATE( P2P_prod2_1D(nTot,NPROD_1D,NCEND) )

  ALLOCATE( P2P_prod1_NCHAN(nTot,NCHAN_1,NPROD_NCHAN,NCEND) )
  ALLOCATE( P2P_prod2_NCHAN(nTot,NCHAN_2,NPROD_NCHAN,NCEND) )

  ALLOCATE( P2P_prod1_NLAY(nTot,NLAY,NPROD_NLAY,NCEND) )
  ALLOCATE( P2P_prod2_NLAY(nTot,NLAY,NPROD_NLAY,NCEND) )


  ALLOCATE( asym_val_1D(NBIN,NSFC,NPROD_1D,NCEND) )
  ALLOCATE( stdv_val_1D(NBIN,NSFC,NPROD_1D,NCEND) )

  ALLOCATE( asym_cnt_1D(NBIN,NSFC,NPROD_1D,NCEND) )
  ALLOCATE( stdv_cnt_1D(NBIN,NSFC,NPROD_1D,NCEND) )

  ALLOCATE( asym_val_NCHAN(NBIN,NSFC,NCHAN,NPROD_NCHAN,NCEND) )
  ALLOCATE( stdv_val_NCHAN(NBIN,NSFC,NCHAN,NPROD_NCHAN,NCEND) )

  ALLOCATE( asym_cnt_NCHAN(NBIN,NSFC,NCHAN,NPROD_NCHAN,NCEND) )
  ALLOCATE( stdv_cnt_NCHAN(NBIN,NSFC,NCHAN,NPROD_NCHAN,NCEND) )

  ALLOCATE( asym_val_NLAY(NBIN,NSFC,NLAY,NPROD_NLAY,NCEND) )
  ALLOCATE( stdv_val_NLAY(NBIN,NSFC,NLAY,NPROD_NLAY,NCEND) )

  ALLOCATE( asym_cnt_NLAY(NBIN,NSFC,NLAY,NPROD_NLAY,NCEND) )
  ALLOCATE( stdv_cnt_NLAY(NBIN,NSFC,NLAY,NPROD_NLAY,NCEND) )


  !---- initialize them ----
  BIG_qc1 = 999
  BIG_qc2 = 999

  BIG_prod1_1D = missing
  BIG_prod2_1D = missing

  BIG_prod1_NCHAN = missing
  BIG_prod2_NCHAN = missing

  BIG_prod1_NLAY = missing
  BIG_prod2_NLAY = missing

  P2P_prod1_1D = missing
  P2P_prod2_1D = missing

  P2P_prod1_NCHAN = missing
  P2P_prod2_NCHAN = missing

  P2P_prod1_NLAY = missing
  P2P_prod2_NLAY = missing

  asym_val_1D = 0.0
  stdv_val_1D = 0.0

  asym_cnt_1D = 0
  stdv_cnt_1D = 0

  asym_val_NCHAN = 0.0
  stdv_val_NCHAN = 0.0
  
  asym_cnt_NCHAN = 0
  stdv_cnt_NCHAN = 0

  asym_val_NLAY = 0.0
  stdv_val_NLAY = 0.0

  asym_cnt_NLAY = 0
  stdv_cnt_NLAY = 0



  !--- Read all EDRs of data set 1 and save them in BIG arrays ----
  
  iTot1 = 0
  Scene1Loop: DO iFile_1=1,nFiles1

    CALL ReadEDRHeader(iu_1in,Files1In(iFile_1),iTyp1,AlgSN1,nprf1,nLay1,nLev1,&
         nChan1, nPosScan1, nScanLine1, nAbsorb1, &
         nParmCLW1, nParmRain1, nParmSnow1, nParmIce1, nParmGrpl1,&
         AbsorbID1, freq1, polar1, nqc1)
    !write(*,'(A, I)') Files1In(iFile_1), nprf1
    
    ProfLoop1: DO iProf1=1, nprf1
      
      CALL ReadEDR(iu_1in,ierr1,iprf1,pres_lay1,pres_lev1,&
           temp_lay1,absorb_lay1,clw_lay1,rain_lay1,graupel_lay1,&
           emis1,angle1,WindSp1,Tskin1,psfc1,sfc1,&
           WindU1,WindV1,RelAziAngle1,SolZenAngle1,SnowDepth1,qc1,&
           lat1,lon1,node1,scanUTC1,scanYear1,scanDay1,iscanPos1,iscanLine1,&
	   nAttempt1,nIter1,ChiSq1,yfwd1,ChanSel1,ym1,ymcorr1)
      
      IF ( ierr1 .ne. 0) EXIT ProfLoop1
      
      iTot1 = iTot1 + 1
      
      BIG_qc1(iTot1)               = qc1(1)
      BIG_prod1_1D(iTot1,1)        = lat1
      BIG_prod1_1D(iTot1,2)        = lon1
      BIG_prod1_1D(iTot1,3)        = scanDay1 + scanUTC1/86400.0
      BIG_prod1_1D(iTot1,4)        = node1
      BIG_prod1_1D(iTot1,5)        = iscanPos1
      BIG_prod1_1D(iTot1,6)        = angle1
      BIG_prod1_1D(iTot1,7)        = sfc1
      BIG_prod1_1D(iTot1,8)        = psfc1
      BIG_prod1_1D(iTot1,9)        = tskin1
      BIG_prod1_1D(iTot1,10)       = (scanDay1-1)*24 + scanUTC1/3600.0
      
      BIG_prod1_NCHAN(iTot1, :, 1) = emis1(:)
      BIG_prod1_NCHAN(iTot1, :, 2) = ym1(:)
      BIG_prod1_NCHAN(iTot1, :, 3) = ymcorr1(:)
 
      BIG_prod1_NLAY(iTot1, 1,  1) = temp_lay1(44)
      BIG_prod1_NLAY(iTot1, 2,  1) = ( temp_lay1(55) + temp_lay1(56) ) * 0.5
      BIG_prod1_NLAY(iTot1, 3,  1) = ( temp_lay1(63) + temp_lay1(64) ) * 0.5
      BIG_prod1_NLAY(iTot1, 4,  1) = temp_lay1(70)
      BIG_prod1_NLAY(iTot1, 5,  1) = temp_lay1(76)
      BIG_prod1_NLAY(iTot1, 6,  1) = temp_lay1(81)
      BIG_prod1_NLAY(iTot1, 7,  1) = temp_lay1(85)
      BIG_prod1_NLAY(iTot1, 8,  1) = ( temp_lay1(89) + temp_lay1(90) ) * 0.5
      BIG_prod1_NLAY(iTot1, 9,  1) = ( temp_lay1(91) + temp_lay1(92) ) * 0.5
      BIG_prod1_NLAY(iTot1, 10, 1) = ( temp_lay1(93) + temp_lay1(94) ) * 0.5
      BIG_prod1_NLAY(iTot1, 11, 1) = temp_lay1(95)
      
      BIG_prod1_NLAY(iTot1, 1,  2) = absorb_lay1(44,1)
      BIG_prod1_NLAY(iTot1, 2,  2) = ( absorb_lay1(55,1) + absorb_lay1(56,1) ) * 0.5
      BIG_prod1_NLAY(iTot1, 3,  2) = ( absorb_lay1(63,1) + absorb_lay1(64,1) ) * 0.5
      BIG_prod1_NLAY(iTot1, 4,  2) = absorb_lay1(70,1)
      BIG_prod1_NLAY(iTot1, 5,  2) = absorb_lay1(76,1)
      BIG_prod1_NLAY(iTot1, 6,  2) = absorb_lay1(81,1)
      BIG_prod1_NLAY(iTot1, 7,  2) = absorb_lay1(85,1)
      BIG_prod1_NLAY(iTot1, 8,  2) = ( absorb_lay1(89,1) + absorb_lay1(90,1) ) * 0.5
      BIG_prod1_NLAY(iTot1, 9,  2) = ( absorb_lay1(91,1) + absorb_lay1(92,1) ) * 0.5
      BIG_prod1_NLAY(iTot1, 10, 2) = ( absorb_lay1(93,1) + absorb_lay1(94,1) ) * 0.5
      BIG_prod1_NLAY(iTot1, 11, 2) = absorb_lay1(95,1)

    ENDDO ProfLoop1
    CLOSE(iu_1in)
  
  ENDDO Scene1Loop
  write(*,*) 'iTot1=', iTot1


 !--- Read all EDRs of data set 2 and save them in BIG arrays
  
  iTot2 = 0
  Scene2Loop: DO iFile_2=1,nFiles2
    
    CALL ReadEDRHeader(iu_2in,Files2In(iFile_2),&
         iTyp2,AlgSN2,nprf2,nLay2,nLev2,&
         nChan2,nPosScan2,nScanLine2,nAbsorb2,& 
         nParmCLW2,nParmRain2,nParmSnow2,nParmIce2,nParmGrpl2,&
         AbsorbID2,freq2,polar2,nqc2)
    
    ProfLoop2: DO iProf2=1,nprf2
      
      CALL ReadEDR(iu_2in,ierr2,iprf2,pres_lay2,pres_lev2,&
           temp_lay2,absorb_lay2,clw_lay2,rain_lay2,graupel_lay2,&
  	   emis2,angle2,WindSp2,Tskin2,psfc2,sfc2,&
           WindU2,WindV2,RelAziAngle2,SolZenAngle2,SnowDepth2,qc2,&
  	   lat2,lon2,node2,scanUTC2,scanYear2,scanDay2,iscanPos2,iscanLine2,&
    	   nAttempt2,nIter2,ChiSq2,yfwd2,ChanSel2,Ym2,ymcorr2)
    	 
      IF( ierr2 .ne. 0 ) EXIT ProfLoop2

      iTot2 = iTot2 + 1
      
      BIG_qc2(iTot2)               = qc2(1)
      BIG_prod2_1D(iTot2,1)        = lat2
      BIG_prod2_1D(iTot2,2)        = lon2
      BIG_prod2_1D(iTot2,3)        = scanDay2 + scanUTC2/86400.0
      BIG_prod2_1D(iTot2,4)        = node2
      BIG_prod2_1D(iTot2,5)        = iscanPos2
      BIG_prod2_1D(iTot2,6)        = angle2
      BIG_prod2_1D(iTot2,7)        = sfc2
      BIG_prod2_1D(iTot2,8)        = psfc2
      BIG_prod2_1D(iTot2,9)        = tskin2
      BIG_prod2_1D(iTot2,10)       = (scanDay2-1)*24 + scanUTC2/3600.0
      
      BIG_prod2_NCHAN(iTot2, :, 1) = emis2(:)
      BIG_prod2_NCHAN(iTot2, :, 2) = ym2(:)
      BIG_prod2_NCHAN(iTot2, :, 3) = ymcorr2(:)
 
      BIG_prod2_NLAY(iTot2, 1,  1) = temp_lay2(44)
      BIG_prod2_NLAY(iTot2, 2,  1) = ( temp_lay2(55) + temp_lay2(56) ) * 0.5
      BIG_prod2_NLAY(iTot2, 3,  1) = ( temp_lay2(63) + temp_lay2(64) ) * 0.5
      BIG_prod2_NLAY(iTot2, 4,  1) = temp_lay2(70)
      BIG_prod2_NLAY(iTot2, 5,  1) = temp_lay2(76)
      BIG_prod2_NLAY(iTot2, 6,  1) = temp_lay2(81)
      BIG_prod2_NLAY(iTot2, 7,  1) = temp_lay2(85)
      BIG_prod2_NLAY(iTot2, 8,  1) = ( temp_lay2(89) + temp_lay2(90) ) * 0.5
      BIG_prod2_NLAY(iTot2, 9,  1) = ( temp_lay2(91) + temp_lay2(92) ) * 0.5
      BIG_prod2_NLAY(iTot2, 10, 1) = ( temp_lay2(93) + temp_lay2(94) ) * 0.5
      BIG_prod2_NLAY(iTot2, 11, 1) = temp_lay2(95)
      
      BIG_prod2_NLAY(iTot2, 1,  2) = absorb_lay2(44,1)
      BIG_prod2_NLAY(iTot2, 2,  2) = ( absorb_lay2(55,1) + absorb_lay2(56,1) ) * 0.5
      BIG_prod2_NLAY(iTot2, 3,  2) = ( absorb_lay2(63,1) + absorb_lay2(64,1) ) * 0.5
      BIG_prod2_NLAY(iTot2, 4,  2) = absorb_lay2(70,1)
      BIG_prod2_NLAY(iTot2, 5,  2) = absorb_lay2(76,1)
      BIG_prod2_NLAY(iTot2, 6,  2) = absorb_lay2(81,1)
      BIG_prod2_NLAY(iTot2, 7,  2) = absorb_lay2(85,1)
      BIG_prod2_NLAY(iTot2, 8,  2) = ( absorb_lay2(89,1) + absorb_lay2(90,1) ) * 0.5
      BIG_prod2_NLAY(iTot2, 9,  2) = ( absorb_lay2(91,1) + absorb_lay2(92,1) ) * 0.5
      BIG_prod2_NLAY(iTot2, 10, 2) = ( absorb_lay2(93,1) + absorb_lay2(94,1) ) * 0.5
      BIG_prod2_NLAY(iTot2, 11, 2) = absorb_lay2(95,1)
      
    ENDDO ProfLoop2
    CLOSE(iu_2in)
  ENDDO Scene2Loop
  write(*,*) 'iTot2=', iTot2
   
  !-------------------------------------------------------------
  !  do collocation: 
  ! (1) timeDiff <= TimeCut
  ! (2) distDiff < DistCut ( FOV size of each scan position )
  ! (3) qc < 2
  ! (4) temp/wv, layered prod pressure < psfc
  !-------------------------------------------------------------
  
  nCollocated = 0
  ProfLoopOuter: DO iprf1 = 1, nTot1
      
      lat1  = BIG_prod1_1D(iprf1,1)
      lon1  = BIG_prod1_1D(iprf1,2)
      node1 = BIG_prod1_1D(iprf1,4)
      sfc1  = BIG_prod1_1D(iprf1,7)
      psfc1 = BIG_prod1_1D(iprf1,8)
      time1 = BIG_prod1_1D(iprf1,10)

      !---- distance threshhold value changes with scan position
      iscan1 = BIG_prod1_1D(iprf1,5)
      !---- pick bigger fov to use
      !DistCut = 0.5 * MAX( fov1_sizes(iscan1), fov2_sizes(iscan1)
      DistCut = MAX( fov1_sizes(iscan1), fov2_sizes(iscan1) )
      
      ProfLoopInner: DO iprf2 = 1, nTot2
     
          lat2  = BIG_prod2_1D(iprf2,1)
          lon2  = BIG_prod2_1D(iprf2,2)
          node2 = BIG_prod2_1D(iprf2,4)
	  sfc2  = BIG_prod2_1D(iprf2,7)
	  psfc2 = BIG_prod2_1D(iprf2,8)
          time2 = BIG_prod2_1D(iprf2,10)
         
          !---- node constraints
          if( node2 .ne. node1 ) CYCLE ProfLoopInner 

          !---- time constraints
	  timeDiff = ABS( time1 - time2 )
	  if( timeDiff .gt. TimeCut ) CYCLE ProfLoopInner
      
	  delta_lat = ( lat2 - lat1 ) * 0.0087266
          delta_lon = ( lon2 - lon1 ) * 0.0087266
          a = sin(delta_lat) * sin(delta_lat) + cos(lat1*0.0174532) * &
              cos(lat2*0.0174532) * sin(delta_lon) * sin(delta_lon)
          c = 2 * atan2( sqrt(a), sqrt(1-a) )
          distDiff = R * c
	  
          meetCriteria = ( timeDiff .le. TimeCut .and. distDiff .lt. DistCut .and. &
                           BIG_qc1(iprf1) .lt. 2 .and. BIG_qc2(iprf2) .lt. 2 )

	  if( meetCriteria ) then
            
	      nCollocated = nCollocated + 1
	      
              !---- a sanity check 
              if( nCollocated .gt. nTot ) then
                write(*,*) 'Error: nCollocated > nTot ', nCollocated, nTot 
	        stop
              endif
	      
              if( node1 .eq. 0 .or. node1 .eq. 1 ) then
                  
		  icend = node1 + 1
                  IP2P(icend) = IP2P(icend) + 1
                  iprf = IP2P(icend)
		 
                  !---- data set 1 values -----------------------------------------------------
		  do iprod = 1, NPROD_1D
		    P2P_prod1_1D(iprf,iprod,icend) = BIG_prod1_1D(iprf1,iprod)
		  enddo
		  
		  do iprod = 1, NPROD_NCHAN
		  do ichan = 1, NCHAN
		    !P2P_prod1_NCHAN(iprf,ichan,iprod,icend) = BIG_prod1_NCHAN(iprf1,ichan,iprod)
		    P2P_prod1_NCHAN(iprf,ichan,iprod,icend) = BIG_prod1_NCHAN(iprf1,index1_chan(ichan),iprod)
		  enddo
		  enddo
		  
		  do iprod = 1, NPROD_NLAY
		  do ilay  = 1, NLAY
		    if( PLAYERS(ilay) .le. psfc1 ) &
		      P2P_prod1_NLAY(iprf,ilay,iprod,icend) = BIG_prod1_NLAY(iprf1,ilay,iprod)
		  enddo
		  enddo
		  
                  !---- data set 2 values -----------------------------------------------------
		  do iprod = 1, NPROD_1D
		    P2P_prod2_1D(iprf,iprod,icend) = BIG_prod2_1D(iprf2,iprod)
		  enddo
		  
		  do iprod = 1, NPROD_NCHAN
		  do ichan = 1, NCHAN
		    !P2P_prod2_NCHAN(iprf,ichan,iprod,icend) = BIG_prod2_NCHAN(iprf2,ichan,iprod)
		    P2P_prod2_NCHAN(iprf,ichan,iprod,icend) = BIG_prod2_NCHAN(iprf2,index2_chan(ichan),iprod)
		  enddo
		  enddo
		  
		  do iprod = 1, NPROD_NLAY
		  do ilay  = 1, NLAY
		    if( PLAYERS(ilay) .le. psfc2 ) &
		      P2P_prod2_NLAY(iprf,ilay,iprod,icend) = BIG_prod2_NLAY(iprf2,ilay,iprod)
		  enddo
		  enddo

              endif
            
	      EXIT ProfLoopInner
	    
	  endif
	  
      ENDDO ProfLoopInner
      
  ENDDO ProfLoopOuter
  
  write(*,*) 'nCollocated=', nCollocated
  write(*,*) 'IP2P=', IP2P(1:NCEND)
  

  !-----------------------------------------------------
  !    to compute bias _1D 
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD_1D
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
  do iprf  = 1, IP2P(icend)
    if( P2P_prod1_1D(iprf,iprod,icend) .gt. missing .and. &
        P2P_prod2_1D(iprf,iprod,icend) .gt. missing ) then	
      diff  = P2P_prod1_1D(iprf,iprod,icend) - P2P_prod2_1D(iprf,iprod,icend)
      angle = P2P_prod1_1D(iprf,6,icend)
      scanpos = P2P_prod1_1D(iprf,5,icend)
      sfc1  = INT(P2P_prod1_1D(iprf,7,icend))
      sfc2  = INT(P2P_prod2_1D(iprf,7,icend))

      if( isfc .lt. NSFC ) then
        if( isAngle) then
            meetCriteria = ( ABS( angle - BIN_BOX(ibin) ) .le. 5 .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) )
        else
            meetCriteria = ( scanpos .eq. ibin .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) )
        endif
      else
        if( isAngle) then
            meetCriteria = ( ABS( angle - BIN_BOX(ibin) ) .le. 5 .and. sfc1 .eq. sfc2 )
        else
            meetCriteria = ( scanpos .eq. ibin .and. sfc1 .eq. sfc2 )
        endif
      endif

      if( meetCriteria ) then
  	asym_val_1D(ibin,isfc,iprod,icend) = asym_val_1D(ibin,isfc,iprod,icend) + diff
  	asym_cnt_1D(ibin,isfc,iprod,icend) = asym_cnt_1D(ibin,isfc,iprod,icend) + 1
      endif
    endif    
  enddo
  enddo
  enddo
  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD_1D
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
    if( asym_cnt_1D(ibin,isfc,iprod,icend) .gt. 1 ) then
      asym_val_1D(ibin,isfc,iprod,icend) = asym_val_1D(ibin,isfc,iprod,icend)/&
                                           asym_cnt_1D(ibin,isfc,iprod,icend)
    else
      asym_val_1D(ibin,isfc,iprod,icend) = missing
    endif
  enddo
  enddo
  enddo
  enddo

  !-----------------------------------------------------
  !    to compute stdv _1D
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD_1D
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
  do iprf  = 1, IP2P(icend)
    if( P2P_prod1_1D(iprf,iprod,icend) .gt. missing .and. &
  	P2P_prod2_1D(iprf,iprod,icend) .gt. missing ) then
      diff  = P2P_prod1_1D(iprf,iprod,icend) - P2P_prod2_1D(iprf,iprod,icend)
      angle = P2P_prod1_1D(iprf,6,icend)
      scanpos = P2P_prod1_1D(iprf,5,icend)
      sfc1  = INT(P2P_prod1_1D(iprf,7,icend))
      sfc2  = INT(P2P_prod2_1D(iprf,7,icend))

      if( isfc .lt. NSFC ) then
        if( isAngle) then
          meetCriteria = ( ABS(angle-BIN_BOX(ibin)) .le. 5 .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) .and. & 
        		   asym_val_1D(ibin,isfc,iprod,icend) .gt. missing )
        else
          meetCriteria = ( scanpos .eq. ibin .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) .and. & 
        		   asym_val_1D(ibin,isfc,iprod,icend) .gt. missing )
        endif
      else
        if( isAngle ) then
          meetCriteria = ( ABS(angle-BIN_BOX(ibin)) .le. 5 .and. sfc1 .eq. sfc2 .and. &
        		   asym_val_1D(ibin,isfc,iprod,icend) .gt. missing )
        else
          meetCriteria = ( scanpos .eq. ibin .and. sfc1 .eq. sfc2 .and. &
        		   asym_val_1D(ibin,isfc,iprod,icend) .gt. missing )
        endif
      endif

      if( meetCriteria ) then
  	stdv_val_1D(ibin,isfc,iprod,icend) = stdv_val_1D(ibin,isfc,iprod,icend) + &
  	(diff-asym_val_1D(ibin,isfc,iprod,icend))*(diff-asym_val_1D(ibin,isfc,iprod,icend)) 
  	stdv_cnt_1D(ibin,isfc,iprod,icend) = stdv_cnt_1D(ibin,isfc,iprod,icend) + 1
      endif
    endif    
  enddo
  enddo
  enddo
  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD_1D
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
    if( stdv_cnt_1D(ibin,isfc,iprod,icend) .gt. 1 ) then
      stdv_val_1D(ibin,isfc,iprod,icend) = &
      SQRT( stdv_val_1D(ibin,isfc,iprod,icend)/(stdv_cnt_1D(ibin,isfc,iprod,icend)-1) )
    else
      stdv_val_1D(ibin,isfc,iprod,icend) = missing
    endif
  enddo
  enddo
  enddo
  enddo


  !-----------------------------------------------------
  !    to compute bias _NCHAN
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD_NCHAN
  do ichan = 1, NCHAN
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
  do iprf  = 1, IP2P(icend)
    if( P2P_prod1_NCHAN(iprf,ichan,iprod,icend) .gt. missing .and. &
  	P2P_prod2_NCHAN(iprf,ichan,iprod,icend) .gt. missing ) then
      diff  = P2P_prod1_NCHAN(iprf,ichan,iprod,icend) - P2P_prod2_NCHAN(iprf,ichan,iprod,icend)
      angle = P2P_prod1_1D(iprf,6,icend)
      scanpos = P2P_prod1_1D(iprf,5,icend)
      sfc1  = INT(P2P_prod1_1D(iprf,7,icend))
      sfc2  = INT(P2P_prod2_1D(iprf,7,icend))

      if( isfc .lt. NSFC ) then
        if( isAngle) then
            meetCriteria = ( ABS( angle - BIN_BOX(ibin) ) .le. 5 .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) )
        else
            meetCriteria = ( scanpos .eq. ibin .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) )
        endif
      else
        if( isAngle) then
            meetCriteria = ( ABS( angle - BIN_BOX(ibin) ) .le. 5 .and. sfc1 .eq. sfc2 )
        else
            meetCriteria = ( scanpos .eq. ibin .and. sfc1 .eq. sfc2 )
        endif
      endif

      if( meetCriteria ) then
  	asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) = asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) + diff
  	asym_cnt_NCHAN(ibin,isfc,ichan,iprod,icend) = asym_cnt_NCHAN(ibin,isfc,ichan,iprod,icend) + 1
      endif
    endif    
  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD_NCHAN
  do ichan = 1, NCHAN
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
    if( asym_cnt_NCHAN(ibin,isfc,ichan,iprod,icend) .gt. 1 ) then
      asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) = &
      asym_val_NCHAN(ibin,isfc,ichan,iprod,icend)/asym_cnt_NCHAN(ibin,isfc,ichan,iprod,icend)
    else
      asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) = missing
    endif
  enddo
  enddo
  enddo
  enddo
  enddo

  !-----------------------------------------------------
  !    to compute stdv _NCHAN
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD_NCHAN
  do ichan = 1, NCHAN
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
  do iprf  = 1, IP2P(icend)
    if( P2P_prod1_NCHAN(iprf,ichan,iprod,icend) .gt. missing .and. &
  	P2P_prod2_NCHAN(iprf,ichan,iprod,icend) .gt. missing ) then
      diff  = P2P_prod1_NCHAN(iprf,ichan,iprod,icend) - P2P_prod2_NCHAN(iprf,ichan,iprod,icend)
      angle = P2P_prod1_1D(iprf,6,icend)
      scanpos = P2P_prod1_1D(iprf,5,icend)
      sfc1  = INT(P2P_prod1_1D(iprf,7,icend))
      sfc2  = INT(P2P_prod2_1D(iprf,7,icend))

      if( isfc .lt. NSFC ) then
        if( isAngle) then
          meetCriteria = ( ABS(angle-BIN_BOX(ibin)) .le. 5 .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) .and. & 
        		   asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) .gt. missing )
        else
          meetCriteria = ( scanpos .eq. ibin .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) .and. & 
        		   asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) .gt. missing )
        endif
      else
        if( isAngle ) then
          meetCriteria = ( ABS(angle-BIN_BOX(ibin)) .le. 5 .and. sfc1 .eq. sfc2 .and. &
        		   asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) .gt. missing )
        else
          meetCriteria = ( scanpos .eq. ibin .and. sfc1 .eq. sfc2 .and. &
        		   asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) .gt. missing )
        endif
      endif

      if( meetCriteria ) then
  	stdv_val_NCHAN(ibin,isfc,ichan,iprod,icend) = &
        stdv_val_NCHAN(ibin,isfc,ichan,iprod,icend) + &
  	( diff - asym_val_NCHAN(ibin,isfc,ichan,iprod,icend) ) * &
        ( diff - asym_val_NCHAN(ibin,isfc,ichan,iprod,icend)) 
  	stdv_cnt_NCHAN(ibin,isfc,ichan,iprod,icend) = &
        stdv_cnt_NCHAN(ibin,isfc,ichan,iprod,icend) + 1
      endif
    endif    
  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD_NCHAN
  do ichan = 1, NCHAN
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
    if( stdv_cnt_NCHAN(ibin,isfc,ichan,iprod,icend) .gt. 1 ) then
      stdv_val_NCHAN(ibin,isfc,ichan,iprod,icend) = &
      SQRT( stdv_val_NCHAN(ibin,isfc,ichan,iprod,icend)/&
           (stdv_cnt_NCHAN(ibin,isfc,ichan,iprod,icend)-1) )
    else
      stdv_val_NCHAN(ibin,isfc,ichan,iprod,icend) = missing
    endif
  enddo
  enddo
  enddo
  enddo
  enddo


  !-----------------------------------------------------
  !    to compute bias _NLAY
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD_NLAY
  do ilay  = 1, NLAY
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
  do iprf  = 1, IP2P(icend)
    if( P2P_prod1_NLAY(iprf,ilay,iprod,icend) .gt. missing .and. &
        P2P_prod2_NLAY(iprf,ilay,iprod,icend) .gt. missing ) then
      diff = P2P_prod1_NLAY(iprf,ilay,iprod,icend) - P2P_prod2_NLAY(iprf,ilay,iprod,icend)
      angle = P2P_prod1_1D(iprf,6,icend)
      scanpos = P2P_prod1_1D(iprf,5,icend)
      sfc1  = INT(P2P_prod1_1D(iprf,7,icend))
      sfc2  = INT(P2P_prod2_1D(iprf,7,icend))

      if( isfc .lt. NSFC ) then
        if( isAngle) then
            meetCriteria = ( ABS( angle - BIN_BOX(ibin) ) .le. 5 .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) )
        else
            meetCriteria = ( scanpos .eq. ibin .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) )
        endif
      else
        if( isAngle) then
            meetCriteria = ( ABS( angle - BIN_BOX(ibin) ) .le. 5 .and. sfc1 .eq. sfc2 )
        else
            meetCriteria = ( scanpos .eq. ibin .and. sfc1 .eq. sfc2 )
        endif
      endif

      if( meetCriteria ) then
        asym_val_NLAY(ibin,isfc,ilay,iprod,icend) = asym_val_NLAY(ibin,isfc,ilay,iprod,icend) + diff
        asym_cnt_NLAY(ibin,isfc,ilay,iprod,icend) = asym_cnt_NLAY(ibin,isfc,ilay,iprod,icend) + 1
      endif
    endif    
  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD_NLAY
  do ilay  = 1, NLAY
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
    if( asym_cnt_NLAY(ibin,isfc,ilay,iprod,icend) .gt. 1 ) then
      asym_val_NLAY(ibin,isfc,ilay,iprod,icend) = &
      asym_val_NLAY(ibin,isfc,ilay,iprod,icend)/asym_cnt_NLAY(ibin,isfc,ilay,iprod,icend)
    else
      asym_val_NLAY(ibin,isfc,ilay,iprod,icend) = missing
    endif
  enddo
  enddo
  enddo
  enddo
  enddo

  !-----------------------------------------------------
  !    to compute stdv _NLAY
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD_NLAY
  do ilay  = 1, NLAY
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
  do iprf  = 1, IP2P(icend)
    if( P2P_prod1_NLAY(iprf,ilay,iprod,icend) .gt. missing .and. &
  	P2P_prod2_NLAY(iprf,ilay,iprod,icend) .gt. missing ) then
      diff  = P2P_prod1_NLAY(iprf,ilay,iprod,icend) - P2P_prod2_NLAY(iprf,ilay,iprod,icend)
      angle = P2P_prod1_1D(iprf,6,icend)
      scanpos = P2P_prod1_1D(iprf,5,icend)
      sfc1  = INT(P2P_prod1_1D(iprf,7,icend))
      sfc2  = INT(P2P_prod2_1D(iprf,7,icend))

      if( isfc .lt. NSFC ) then
        if( isAngle) then
          meetCriteria = ( ABS(angle-BIN_BOX(ibin)) .le. 5 .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) .and. & 
        		   asym_val_NLAY(ibin,isfc,ilay,iprod,icend) .gt. missing )
        else
          meetCriteria = ( scanpos .eq. ibin .and. &
        		   sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) .and. & 
        		   asym_val_NLAY(ibin,isfc,ilay,iprod,icend) .gt. missing )
        endif
      else
        if( isAngle ) then
          meetCriteria = ( ABS(angle-BIN_BOX(ibin)) .le. 5 .and. sfc1 .eq. sfc2 .and. &
        		   asym_val_NLAY(ibin,isfc,ilay,iprod,icend) .gt. missing )
        else
          meetCriteria = ( scanpos .eq. ibin .and. sfc1 .eq. sfc2 .and. &
        		   asym_val_NLAY(ibin,isfc,ilay,iprod,icend) .gt. missing )
        endif
      endif

      if( meetCriteria ) then
  	stdv_val_NLAY(ibin,isfc,ilay,iprod,icend) = &
        stdv_val_NLAY(ibin,isfc,ilay,iprod,icend) + &
  	( diff - asym_val_NLAY(ibin,isfc,ilay,iprod,icend) ) * &
        ( diff - asym_val_NLAY(ibin,isfc,ilay,iprod,icend) ) 
  	stdv_cnt_NLAY(ibin,isfc,ilay,iprod,icend) = stdv_cnt_NLAY(ibin,isfc,ilay,iprod,icend) + 1
      endif
    endif    
  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD_NLAY
  do ilay  = 1, NLAY
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
    if( stdv_cnt_NLAY(ibin,isfc,ilay,iprod,icend) .gt. 1 ) then
      stdv_val_NLAY(ibin,isfc,ilay,iprod,icend) = &
      SQRT( stdv_val_NLAY(ibin,isfc,ilay,iprod,icend)/&
           (stdv_cnt_NLAY(ibin,isfc,ilay,iprod,icend)-1) )
    else
      stdv_val_NLAY(ibin,isfc,ilay,iprod,icend) = missing
    endif
  enddo
  enddo
  enddo
  enddo
  enddo


  !-----------------------------------------------------
  !     writeout P2P stuff
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD_1D
    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(yyyymmdd)//&
            '_'//trim(prodIds_1D(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*IP2P(icend))
    write(25,rec=1) P2P_prod1_1D(1:IP2P(icend),iprod,icend)
    close(25)

    p2pFile='p2p_collocate_'//trim(satId2)//'_'//trim(yyyymmdd)//&
            '_'//trim(prodIds_1D(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*IP2P(icend))
    write(25,rec=1) P2P_prod2_1D(1:IP2P(icend),iprod,icend)
    close(25)
  enddo
  enddo

  do icend = 1, NCEND
  do iprod = 1, NPROD_NCHAN
    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(yyyymmdd)//&
            '_'//trim(prodIds_NCHAN(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*IP2P(icend)*NCHAN)
    write(25,rec=1) P2P_prod1_NCHAN(1:IP2P(icend),1:NCHAN,iprod,icend)
    close(25)

    p2pFile='p2p_collocate_'//trim(satId2)//'_'//trim(yyyymmdd)//&
            '_'//trim(prodIds_NCHAN(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*IP2P(icend)*NCHAN)
    write(25,rec=1) P2P_prod2_NCHAN(1:IP2P(icend),1:NCHAN,iprod,icend)
    close(25)
  enddo
  enddo

  do icend = 1, NCEND
  do iprod = 1, NPROD_NLAY
    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(yyyymmdd)//&
            '_'//trim(prodIds_NLAY(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*IP2P(icend)*NLAY)
    write(25,rec=1) P2P_prod1_NLAY(1:IP2P(icend),1:NLAY,iprod,icend)
    close(25)

    p2pFile='p2p_collocate_'//trim(satId2)//'_'//trim(yyyymmdd)//&
            '_'//trim(prodIds_NLAY(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*IP2P(icend)*NLAY)
    write(25,rec=1) P2P_prod2_NLAY(1:IP2P(icend),1:NLAY,iprod,icend)
    close(25)
  enddo
  enddo

  !-----------------------------------------------------
  !     writeout P2P stuff - asymmetry and stdv
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD_1D

    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(satId2)//'_glb_asym_'//&
            trim(yyyymmdd)//'_'//trim(prodIds_1D(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*NBIN*NSFC)
    write(25,rec=1) asym_val_1D(1:NBIN,1:NSFC,iprod,icend)
    close(25)

    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(satId2)//'_glb_stdv_'//&
            trim(yyyymmdd)//'_'//trim(prodIds_1D(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*NBIN*NSFC)
    write(25,rec=1) stdv_val_1D(1:NBIN,1:NSFC,iprod,icend)
    close(25)

  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD_NCHAN

    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(satId2)//'_glb_asym_'//&
            trim(yyyymmdd)//'_'//trim(prodIds_NCHAN(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*NBIN*NSFC)
    irec=0
    do ichan = 1, NCHAN
	irec=irec+1
        write(25,rec=irec) asym_val_NCHAN(1:NBIN,1:NSFC,ichan,iprod,icend)
    enddo
   
    close(25)
    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(satId2)//'_glb_stdv_'//&
            trim(yyyymmdd)//'_'//trim(prodIds_NCHAN(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*NBIN*NSFC)
    irec=0
    do ichan = 1, NCHAN
	irec=irec+1
        write(25,rec=irec) stdv_val_NCHAN(1:NBIN,1:NSFC,ichan,iprod,icend)
    enddo
    close(25)

  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD_NLAY

    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(satId2)//'_glb_asym_'//&
            trim(yyyymmdd)//'_'//trim(prodIds_NLAY(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*NBIN*NSFC)
    irec=0
    do ilay = 1, NLAY
	irec=irec+1
        write(25,rec=irec) asym_val_NLAY(1:NBIN,1:NSFC,ilay,iprod,icend)
    enddo
    close(25)

    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(satId2)//'_glb_stdv_'//&
            trim(yyyymmdd)//'_'//trim(prodIds_NLAY(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*NBIN*NSFC)
    irec=0
    do ilay = 1, NLAY
	irec=irec+1
        write(25,rec=irec) stdv_val_NLAY(1:NBIN,1:NSFC,ilay,iprod,icend)
    enddo
    close(25)

  enddo
  enddo

  
  !---- free up resources ----
  DEALLOCATE( freq1 )
  DEALLOCATE( polar1 )
  DEALLOCATE( emis1 )
  DEALLOCATE( yfwd1 )
  DEALLOCATE( ChanSel1 )
  DEALLOCATE( ym1 )
  DEALLOCATE( ymCorr1 )

  DEALLOCATE( freq2 )
  DEALLOCATE( polar2 )
  DEALLOCATE( emis2 )
  DEALLOCATE( yfwd2 )
  DEALLOCATE( ChanSel2 )
  DEALLOCATE( ym2 )
  DEALLOCATE( ymCorr2 )

  DEALLOCATE( BIG_qc1 )
  DEALLOCATE( BIG_qc2 )

  DEALLOCATE( BIG_prod1_1D )
  DEALLOCATE( BIG_prod2_1D )

  DEALLOCATE( BIG_prod1_NCHAN )
  DEALLOCATE( BIG_prod2_NCHAN )

  DEALLOCATE( BIG_prod1_NLAY )
  DEALLOCATE( BIG_prod2_NLAY )

  DEALLOCATE( P2P_prod1_1D )
  DEALLOCATE( P2P_prod2_1D )

  DEALLOCATE( P2P_prod1_NCHAN )
  DEALLOCATE( P2P_prod2_NCHAN )
 
  DEALLOCATE( P2P_prod1_NLAY )
  DEALLOCATE( P2P_prod2_NLAY )


  DEALLOCATE( asym_val_1D )
  DEALLOCATE( stdv_val_1D )

  DEALLOCATE( asym_cnt_1D )
  DEALLOCATE( stdv_cnt_1D )

  DEALLOCATE( asym_val_NCHAN )
  DEALLOCATE( stdv_val_NCHAN )

  DEALLOCATE( asym_cnt_NCHAN )
  DEALLOCATE( stdv_cnt_NCHAN )

  DEALLOCATE( asym_val_NLAY )
  DEALLOCATE( stdv_val_NLAY )

  DEALLOCATE( asym_cnt_NLAY )
  DEALLOCATE( stdv_cnt_NLAY )


  CONTAINS

  
  !---- to compute FOV size according to different scan position ----
  subroutine fov_amsua( fov_a )
    IMPLICIT NONE
    INTEGER, PARAMETER          :: NUMSPOT_A=30     ! samples per scan line
    REAL,DIMENSION(1:NUMSPOT_A) :: fov_a
    REAL                        :: PI=3.1415926
    REAL                        :: SCAN_ANG_A=3.3
    REAL                        :: REARTH=6371.0
    REAL                        :: RSAT=833.0       ! height of satellite above earth
    INTEGER                     :: i
    REAL                        :: angle=0.0
    REAL                        :: angle1=0.0, angle2=0.0

    do i=1, NUMSPOT_A
      angle = PI * SCAN_ANG_A * (i - NUMSPOT_A/2.0 -1 ) / 180.0 
      angle1 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

      angle = PI * SCAN_ANG_A * (i - NUMSPOT_A/2.0 ) / 180.0	  
      angle2 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

      fov_a(i) = REARTH * ( ( angle1 - angle2 ) - PI * SCAN_ANG_A / 180.0 )
      !write(*,*)fov_a(i)

    enddo
    return
    
  end subroutine fov_amsua
  
  !---- to compute FOV size according to different scan position
  subroutine fov_mhs( fov_m )
    IMPLICIT NONE
    INTEGER, PARAMETER          :: NUMSPOT_M=90     ! samples per scan line
    REAL,DIMENSION(1:NUMSPOT_M) :: fov_m
    REAL                        :: PI=3.1415926
    REAL                        :: SCAN_ANG_M=1.1
    REAL                        :: REARTH=6371.0
    REAL                        :: RSAT=833.0       ! height of satellite above earth
    INTEGER                     :: i
    REAL                        :: angle=0.0
    REAL                        :: angle1=0.0, angle2=0.0

    do i=1, NUMSPOT_M
      angle = PI * SCAN_ANG_M * (i - NUMSPOT_M/2.0 -1 ) / 180.0 
      angle1 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

      angle = PI * SCAN_ANG_M * (i - NUMSPOT_M/2.0 ) / 180.0	  
      angle2 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

      fov_m(i) = REARTH * ( ( angle1 - angle2 ) - PI * SCAN_ANG_M / 180.0 )
      !write(*,*)fov_m(i)

    enddo
    return
    
  end subroutine fov_mhs

  !---- compute fov size of npp/atms low resolution
  subroutine fov_npp_lr( fovs )
    
    INTEGER, PARAMETER          :: NFOV=32
    REAL, PARAMETER             :: SCAN_ANG=3.3
    REAL,DIMENSION(1:NFOV)      :: fovs
    REAL                        :: PI=3.1415926
    REAL                        :: REARTH=6371.0
    REAL                        :: RSAT=824.0
    INTEGER                     :: i
    REAL                        :: angle=0.0
    REAL                        :: angle1=0.0, angle2=0.0

    do i=1, NFOV
      angle = PI * SCAN_ANG * (i - NFOV/2.0 -1 ) / 180.0 
      angle1 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

      angle = PI * SCAN_ANG * (i - NFOV/2.0 ) / 180.0	  
      angle2 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

      fovs(i) = REARTH * ( ( angle1 - angle2 ) - PI * SCAN_ANG / 180.0 )
      !write(*,*)fovs(i)

    enddo
  
  end subroutine fov_npp_lr
 
  !---- compute fov size of npp/atms high resolution
  subroutine fov_npp_hr( fovs )
    
    INTEGER, PARAMETER          :: NFOV=96
    REAL, PARAMETER             :: SCAN_ANG=1.11
    REAL,DIMENSION(1:NFOV)      :: fovs
    REAL                        :: PI=3.1415926
    REAL                        :: REARTH=6371.0
    REAL                        :: RSAT=824.0
    INTEGER                     :: i
    REAL                        :: angle=0.0
    REAL                        :: angle1=0.0, angle2=0.0

    do i=1, NFOV
      angle = PI * SCAN_ANG * (i - NFOV/2.0 -1 ) / 180.0 
      angle1 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

      angle = PI * SCAN_ANG * (i - NFOV/2.0 ) / 180.0	  
      angle2 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

      fovs(i) = REARTH * ( ( angle1 - angle2 ) - PI * SCAN_ANG / 180.0 )
      !write(*,*)fovs(i)

    enddo
  
  end subroutine fov_npp_hr
 

  !---- read EDR file header ----
  SUBROUTINE ReadEDRHeader(iu,InputFile,iTyp,AlgSN,nprf,nLay,nLev,&
             nChan, nPosScan, nScanLine, nAbsorb,& 
             nParmCLW, nParmRain, nParmSnow, nParmIce, nParmGrpl,&
             AbsorbID, freq, polar, nqc)
    
    IMPLICIT NONE
    CHARACTER(LEN=*)       :: InputFile
    INTEGER                :: iu,iTyp, AlgSN, nprf, nLay, nLev, nChan, nPosScan, nScanLine
    INTEGER                :: nAbsorb, nParmCLW, nParmRain, nParmSnow, nParmIce, nParmGrpl
    INTEGER, DIMENSION(:)  :: AbsorbID
    REAL,    DIMENSION(:)  :: freq
    INTEGER, DIMENSION(:)  :: polar
    INTEGER                :: nqc
    
    iu=get_lun()
    OPEN(iu,file=InputFile,form='unformatted')
    READ(iu) iTyp,AlgSN
    READ(iu) nprf
    READ(iu) nLay
    READ(iu) nLev
    READ(iu) nChan
    READ(iu) nPosScan
    READ(iu) nScanLine
    READ(iu) nAbsorb
    READ(iu) nParmCLW
    READ(iu) nParmRain
    READ(iu) nParmSnow
    READ(iu) nParmIce
    READ(iu) nParmGrpl
    READ(iu) AbsorbID
    READ(iu) freq
    READ(iu) polar
    READ(iu) nqc
    RETURN
  END SUBROUTINE ReadEDRHeader


  !---- read EDR content ----
  SUBROUTINE ReadEDR( iu,ierr,iprf,pres_lay,pres_lev,temp_lay,absorb_lay,&
             clw_lay,rain_lay,graupel_lay,emis,angle,WindSp,Tskin,psfc,sfc,&
             WindU,WindV,RelAziAngle,SolZenAngle,SnowDepth,qc,&
             lat,lon,node,scanUTC,scanYear,scanDay,iscanPos,iscanLine,&
	     nAttempt,nIter,ChiSq,yfwd,ChanSel,Ym,ymcorr )
	     
    IMPLICIT NONE

    INTEGER                                    :: iu,ierr
    INTEGER                                    :: iprf        !Profile Index 
    REAL,              DIMENSION(:)            :: pres_lay    !Layer pressure grid
    REAL,              DIMENSION(:)            :: pres_lev    !Level pressure grid
    REAL,              DIMENSION(:)            :: temp_lay    !Layer temperature profile
    REAL,              DIMENSION(:,:)          :: absorb_lay  !Layer aborbents profiles
    REAL,              DIMENSION(:)            :: clw_lay     !Cloud amount vector
    REAL,              DIMENSION(:)            :: rain_lay    !Rain amount vector
    REAL,              DIMENSION(:)            :: graupel_lay !Graupel amount vector
    REAL,              DIMENSION(:)            :: emis        !Emissivities vector
    REAL                                       :: angle       !Scan Angle
    REAL                                       :: windSp      !Wind speed
    REAL                                       :: tskin       !Skin temperature
    REAL                                       :: psfc        !Surface pressure
    INTEGER                                    :: sfc         !Surface type ID
    REAL                                       :: windU       !U-direction wind speed
    REAL                                       :: windV       !V-direction wind speed
    REAL                                       :: relAziAngle !Relative Azimuth Angle
    REAL                                       :: solZenAngle !Solar Zenith Angle
    REAL                                       :: snowdepth   !Snow Depth
    INTEGER(2),        DIMENSION(:)            :: qc          !QC vector
    REAL                                       :: lat         !Latitude
    REAL                                       :: lon         !Longitude
    INTEGER                                    :: node        !=0->ASC, =1->DESC
    REAL                                       :: scanUTC     !UTC time
    INTEGER                                    :: scanYear    !Year
    INTEGER                                    :: scanDAY     !Day 
    INTEGER                                    :: iscanPos    !Scan position 
    INTEGER                                    :: iScanLine   !Scan line Index 
    INTEGER                                    :: nAttempt    !Number of attempts performed for retrieval
    INTEGER                                    :: nIter       !Number of iterations
    REAL                                       :: ChiSq       !Convergence metric
    REAL,              DIMENSION(:)            :: yfwd        !Last forward simulated TBs in retr.
    INTEGER,           DIMENSION(:)            :: ChanSel     !Channels selection used in retr.
    REAL,              DIMENSION(:)            :: Ym          !Measured TBs used for retrieval (uncorrected)
    REAL,              DIMENSION(:)            :: ymcorr      !Measured TBs used for retrieval (corrected)
      
    ierr=0
    READ(iu,iostat=ierr,end=10) iprf
    IF (ierr.ne.0) THEN
       RETURN
    ENDIF
    !---Atmospheric constituents
    READ(iu,err=20) pres_lay
    READ(iu,err=20) pres_lev
    READ(iu,err=20) temp_lay
    READ(iu,err=20) absorb_lay
    !---hydrometeors
    READ(iu,err=20) clw_lay
    READ(iu,err=20) rain_lay
    READ(iu,err=20) graupel_lay
    !---Emissivity/reflectivity vectors
    READ(iu,err=20) emis
    !---Surface-level paremeters
    READ(iu,err=20) angle,windSp,tskin,psfc,sfc,windU,windV,relAziAngle,solZenAngle,snowDepth
    !---QC variables 
    READ(iu,err=20) qc
    !---Positioning variables
    READ(iu,err=20) lat,lon,node,scanUTC,scanYear,scanDay,iscanPos,iscanLine 
    READ(iu,err=20) nAttempt,nIter,ChiSq
    READ(iu,err=20) yfwd
    READ(iu,err=20) ChanSel
    READ(iu,err=20) Ym
    READ(iu,err=20) ymcorr
    RETURN
10  CONTINUE
    ierr=Warn_EndOfFile
    CALL ErrHandl(WarningType,Warn_EndOfFile,'Edr') 
    RETURN
20  ierr=Warn_readInvalid
    CALL ErrHandl(WarningType,Warn_readInvalid,'(ReadEdr)')
    RETURN
  END SUBROUTINE ReadEDR


End Program interEdr

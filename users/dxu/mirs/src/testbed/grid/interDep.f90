 !==============================================================================
 !
 ! Name:        interDep
 !
 ! Type:        F90 Program
 !
 ! Description: P2P Collocates DEP files from 2 satellites and output asymmetry 
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
 !    -IO_Dep
 !    -Consts
 !    -utils
 !    -misc
 !    -ErrorHandling
 !
 ! Subroutines Contained:
 !    - fov_amsua
 !    - fov_mhs
 !
 ! History
 !
 !    -  01-31-2011        Wanchun Chen   Original Coder
 !    -  11-31-2011        Wanchun Chen   Added fmType to deal different resol.
 !                                        Dynamicly allocate memory now
 !
 !==============================================================================

Program interDep
  
  USE Consts
  USE misc
  USE utils
  USE IO_DEP
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE
  
  !---- Outer loop DEP variables
  CHARACTER(LEN=256), DIMENSION(:), POINTER  :: Files1In
  INTEGER                                    :: iu_1in, ierr1, nprf1, iprf1
  TYPE(DEP_type)                             :: Dep1
  
  !---- Inner loop EDR header variables
  CHARACTER(LEN=256), DIMENSION(:), POINTER  :: Files2In
  INTEGER                                    :: iu_2in, ierr2, nprf2, iprf2
  TYPE(DEP_type)                             :: Dep2
  
  !---- loop variables
  INTEGER                                    :: iu_list1, iu_list2, nFiles1,nFiles2
  INTEGER                                    :: iFile_1,iFile_2,iProf1,iProf2
  INTEGER                                    :: ifile, nprf
  
  
  !---- variable related to compute distance
  REAL, PARAMETER                            :: R = 6371.0
  !REAL, PARAMETER                           :: PI = 3.1415926
  REAL                                       :: delta_lat, delta_lon
  REAL                                       :: a, c, distance
  
  !---- shared output variables  
  REAL                                       :: timeDiff,distDiff
  LOGICAL                                    :: meetCriteria
  REAL                                       :: time1, time2
  INTEGER                                    :: iscan1,DistCut
  INTEGER                                    :: nCollocated = 0
  
  REAL                                       :: lat1         !Latitude
  REAL                                       :: lon1         !Longitude
  INTEGER                                    :: node1        !=0->ASC, =1->DESC
  REAL                                       :: lat2         !Latitude
  REAL                                       :: lon2         !Longitude
  INTEGER                                    :: node2        !=0->ASC, =1->DESC

  !---- output variables
  CHARACTER(LEN=256), DIMENSION(:), POINTER  :: Files1Out,Files2Out
  INTEGER                                    :: iu_1out, iu_2out


  !---- P2P identifiers
  INTEGER, PARAMETER                         :: NP2P=3750000, NPROD=13, NCEND=2  
  INTEGER,DIMENSION(NCEND)                   :: IP2P=0
  INTEGER                                    :: nTot1, nTot2, iTot1, iTot2, iTot, nTot
  REAL,PARAMETER                             :: missing=-999.0

  !---- BIG buffer arrays to hold all parameters
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_qc1
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_lat1
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_lon1
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_time1
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_iscan1
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_node1
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_angle1
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_sfc1
  REAL,DIMENSION(:,:), ALLOCATABLE           :: BIG_prod1
  
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_qc2
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_lat2
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_lon2
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_time2
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_iscan2
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_node2
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_angle2
  REAL,DIMENSION(:), ALLOCATABLE             :: BIG_sfc2
  REAL,DIMENSION(:,:), ALLOCATABLE           :: BIG_prod2
 
  !---- P2P variables
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_lat1
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_lon1
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_time1
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_iscan1
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_node1
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_angle1
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_sfc1
  REAL,DIMENSION(:,:,:), ALLOCATABLE         :: P2P_prod1

  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_lat2
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_lon2
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_time2
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_iscan2
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_node2
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_angle2
  REAL,DIMENSION(:,:), ALLOCATABLE  	     :: P2P_sfc2
  REAL,DIMENSION(:,:,:), ALLOCATABLE         :: P2P_prod2

  CHARACTER(LEN=256)                         :: p2pFile
  CHARACTER(LEN=2), DIMENSION(2)             :: cendIds=(/'as','ds'/)
  CHARACTER(LEN=5), DIMENSION(NPROD)         :: prodIds=(/ 'sfc2 ', 'tpw  ', 'clw  ', 'rr   ', 'gs   ', 'iwp  ', &
                                                'rwp  ', 'lwp  ','swe  ', 'sic  ', 'sicfy', 'sicmy', 'snow ' /)

  !---- variables of asymmetry
  INTEGER, PARAMETER                         :: NSFC=5
  INTEGER                                    :: NBIN=0
  INTEGER, DIMENSION(:),ALLOCATABLE          :: BIN_BOX
  INTEGER                                    :: isfc, ibin, iprf, icend, sfc, iprod, sfc1, sfc2
  REAL,   DIMENSION(:,:,:,:),ALLOCATABLE     :: asym_val, stdv_val
  INTEGER,DIMENSION(:,:,:,:),ALLOCATABLE     :: asym_cnt, stdv_cnt
  REAL   	                             :: diff=-999.0
  REAL                                       :: angle=0.0
  INTEGER                                    :: scanpos=-999
  LOGICAL                                    :: isAngle=.true.
  
  !---- FOV size
  INTEGER                                    :: NFOV1=30,NFOV2=30
  REAL, DIMENSION(:), ALLOCATABLE            :: fov1_sizes,fov2_sizes
  
  !---Namelist variables
  CHARACTER(LEN=16)                          :: satId1,satId2
  CHARACTER(LEN=256)                         :: List1,List2
  CHARACTER(LEN=10)                          :: yyyymmdd
  CHARACTER(LEN=256)                         :: p2pDir
  REAL                                       :: TimeCut
  INTEGER                                    :: fmType=0
  
  CHARACTER(LEN=32)                          :: pair
 
  NAMELIST /CollCntrl/satId1,satId2,List1,List2,yyyymmdd,p2pDir,TimeCut,fmType
  
  read(*,NML=CollCntrl)
  
  pair = TRIM(satId1)//'_'//TRIM(satId2)
  
  if( ( strcmp(satId1,'n18')    .eq. 0 .or. &
        strcmp(satId1,'n19')    .eq. 0 .or. &
        strcmp(satId1,'metopA') .eq. 0 .or. &
        strcmp(satId1,'metopB') .eq. 0 )    &
                  .and.                     &
      ( strcmp(satId2,'n18')    .eq. 0 .or. &
        strcmp(satId2,'n19')    .eq. 0 .or. &
        strcmp(satId2,'metopA') .eq. 0 .or. &
        strcmp(satId2,'metopB') .eq. 0 )    ) then
   
    isAngle = .true.
    NBIN = 12
    ALLOCATE( BIN_BOX( NBIN ) )
    do ibin = 1, NBIN
      BIN_BOX(ibin) = -55 + 10 * (ibin-1)
    enddo
    
    if( fmType .eq. 0 ) then
      NFOV1=30
      NFOV2=30
      ALLOCATE(fov1_sizes(1:NFOV1))
      ALLOCATE(fov2_sizes(1:NFOV2))
      call fov_amsua( fov1_sizes )
      call fov_amsua( fov2_sizes )
    else
      NFOV1=90
      NFOV2=90
      ALLOCATE(fov1_sizes(1:NFOV1))
      ALLOCATE(fov2_sizes(1:NFOV2))
      call fov_mhs( fov1_sizes )
      call fov_mhs( fov2_sizes )
    endif  
  
  else if ( strcmp(pair,'f16_f18') .eq. 0 .or. strcmp(pair,'f18_f16') .eq. 0 ) then

    if( fmType .eq. 0 ) then
      NFOV1=30
      NFOV2=30
    else if( fmType .eq. 1 ) then
      NFOV1=60
      NFOV2=60
    else if( fmType .eq. 2 ) then
      NFOV1=90
      NFOV2=90
    else if( fmType .eq. 3 ) then
      NFOV1=180
      NFOV2=180
    else
      STOP 'Error: Incorrect fmType for SSMIS ( F16 vs F18 )'
    endif
    ALLOCATE(fov1_sizes(1:NFOV1))
    ALLOCATE(fov2_sizes(1:NFOV2))
    
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

    if( fmType .eq. 0 ) then
      NFOV1=30
      NFOV2=30
    else if( fmType .eq. 1 ) then
      NFOV1=60
      NFOV2=60
    else if( fmType .eq. 2 ) then
      NFOV1=90
      NFOV2=90
    else if( fmType .eq. 3 ) then
      NFOV1=180
      NFOV2=180
    else
      STOP 'Error: Incorrect fmType for SSMIS ( F16 vs F17 )'
    endif
    ALLOCATE(fov1_sizes(1:NFOV1))
    ALLOCATE(fov2_sizes(1:NFOV2))
    
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

    if( fmType .eq. 0 ) then
      NFOV1=30
      NFOV2=30
    else if( fmType .eq. 1 ) then
      NFOV1=60
      NFOV2=60
    else if( fmType .eq. 2 ) then
      NFOV1=90
      NFOV2=90
    else if( fmType .eq. 3 ) then
      NFOV1=180
      NFOV2=180
    else
      STOP 'Error: Incorrect fmType for SSMIS ( F17 vs F18 )'
    endif
    ALLOCATE(fov1_sizes(1:NFOV1))
    ALLOCATE(fov2_sizes(1:NFOV2))
    
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
   
   ! 1 - poes, 2 - npp/atms
   else if ( strcmp(pair,'n18_npp')    .eq. 0 .or. &
             strcmp(pair,'n19_npp')    .eq. 0 .or. &
             strcmp(pair,'metopA_npp') .eq. 0 .or. &
             strcmp(pair,'metopB_npp') .eq. 0 ) then
    
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
      call fov_npp_lr( fov2_sizes )
    endif  
  
   ! 1 - npp/atms, 2 - poes
   else if ( strcmp(pair,'npp_n18')    .eq. 0 .or. &
             strcmp(pair,'npp_n19')    .eq. 0 .or. &
             strcmp(pair,'npp_metopA') .eq. 0 .or. &
             strcmp(pair,'npp_metopB') .eq. 0 ) then
    
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
      call fov_npp_lr( fov1_sizes )
      call fov_mhs( fov2_sizes )
    endif  
  
  else
    STOP 'Unsupported pair'
  endif


  ALLOCATE( asym_val(NBIN,NSFC,NPROD,NCEND) )
  ALLOCATE( stdv_val(NBIN,NSFC,NPROD,NCEND) )

  ALLOCATE( asym_cnt(NBIN,NSFC,NPROD,NCEND) )
  ALLOCATE( stdv_cnt(NBIN,NSFC,NPROD,NCEND) )

  asym_val=0.0
  stdv_val=0.0

  asym_cnt=0
  stdv_cnt=0
  
  !---Open index file and read
  CALL ReadList(iu_list1,List1,Files1In,nFiles1,Files1Out,'./','FILES1')
  CALL ReadList(iu_list2,List2,Files2In,nFiles2,Files2Out,'./','FILES2')

  !---- to get the total number of profiles by looping all files
  nTot1 = 0 
  DO ifile = 1, nFiles1
    CALL getDepNprf( Files1In(ifile), nprf )
    nTot1 = nTot1 + nprf
  ENDDO
  write(*,*) 'nTot1 = ', nTot1
  
  nTot2 = 0 
  DO ifile = 1, nFiles2
    CALL getDepNprf( Files2In(ifile ), nprf )
    nTot2 = nTot2 + nprf
  ENDDO
  write(*,*) 'nTot2 = ', nTot2
  
  !---- pick min one lah
  if( nTot1 .lt. nTot2 ) then
    nTot = nTot1
  else
    nTot = nTot2
  endif
  
  !---- allocate memory
  
  ALLOCATE( BIG_qc1( nTot1 ) )
  ALLOCATE( BIG_lat1( nTot1 ) )
  ALLOCATE( BIG_lon1( nTot1 ) )
  ALLOCATE( BIG_time1( nTot1 ) )
  ALLOCATE( BIG_iscan1( nTot1 ) )
  ALLOCATE( BIG_node1( nTot1 ) )
  ALLOCATE( BIG_angle1( nTot1 ) )
  ALLOCATE( BIG_sfc1( nTot1 ) )
  ALLOCATE( BIG_prod1( nTot1,NPROD ) )
  
  ALLOCATE( BIG_qc2( nTot2 ) )
  ALLOCATE( BIG_lat2( nTot2 ) )
  ALLOCATE( BIG_lon2( nTot2 ) )
  ALLOCATE( BIG_time2( nTot2 ) )
  ALLOCATE( BIG_iscan2( nTot2 ) )
  ALLOCATE( BIG_node2( nTot2 ) )
  ALLOCATE( BIG_angle2( nTot2 ) )
  ALLOCATE( BIG_sfc2( nTot2 ) )
  ALLOCATE( BIG_prod2( nTot2,NPROD ) )
  
  ALLOCATE( P2P_lat1(nTot,NCEND) )
  ALLOCATE( P2P_lon1(nTot,NCEND) )
  ALLOCATE( P2P_time1(nTot,NCEND) )
  ALLOCATE( P2P_iscan1(nTot,NCEND) )
  ALLOCATE( P2P_node1(nTot,NCEND) )
  ALLOCATE( P2P_angle1(nTot,NCEND) )
  ALLOCATE( P2P_sfc1(nTot,NCEND) )
  ALLOCATE( P2P_prod1(nTot,NPROD,NCEND) )

  ALLOCATE( P2P_lat2(nTot,NCEND) )
  ALLOCATE( P2P_lon2(nTot,NCEND) )
  ALLOCATE( P2P_time2(nTot,NCEND) )
  ALLOCATE( P2P_iscan2(nTot,NCEND) )
  ALLOCATE( P2P_node2(nTot,NCEND) )
  ALLOCATE( P2P_angle2(nTot,NCEND) )
  ALLOCATE( P2P_sfc2(nTot,NCEND) )
  ALLOCATE( P2P_prod2(nTot,NPROD,NCEND) )

  !---- initialize those arrays

  BIG_qc1 = missing
  BIG_lat1 = missing
  BIG_lon1 = missing
  BIG_time1 = missing
  BIG_iscan1 = missing
  BIG_node1 = missing
  BIG_angle1 = missing
  BIG_sfc1 = missing
  BIG_prod1 = missing

  BIG_qc2 = missing
  BIG_lat2 = missing
  BIG_lon2 = missing
  BIG_time2 = missing
  BIG_iscan2 = missing
  BIG_node2 = missing
  BIG_angle2 = missing
  BIG_sfc2 = missing
  BIG_prod2 = missing

  P2P_lat1 = missing
  P2P_lon1 = missing
  P2P_time1 = missing
  P2P_iscan1 = missing
  P2P_node1 = missing
  P2P_angle1 = missing
  P2P_sfc1 = missing
  P2P_prod1 = missing

  P2P_lat2 = missing
  P2P_lon2 = missing
  P2P_time2 = missing
  P2P_iscan2 = missing
  P2P_node2 = missing
  P2P_angle2 = missing
  P2P_sfc2 = missing
  P2P_prod2 = missing
  
  !---- Read all DEPs of data set 1 and save them in BIG array set 1
  iTot1 = 0
  Dep1Loop: DO iFile_1=1,nFiles1

    CALL ReadHdrDEP(iu_1in, Files1In(iFile_1), Dep1, nprf1)
    !write(*,'(A,I)') Files1In(iFile_1), nprf1
    
    ProfLoop1: DO iprf1=1, nprf1
      
      CALL ReadDEP(iu_1in,Dep1,ierr1)
      
      IF ( ierr1 .eq. Warn_EndOfFile   ) EXIT  ProfLoop1
      IF ( ierr1 .eq. Warn_readInvalid ) CYCLE ProfLoop1
     
      iTot1 = iTot1 + 1
      if( iTot1 .gt. nTot1 ) then
        write(*,*)'Error: iTot1 > nTot1 ', iTot1,nTot1
	stop 
      endif
      BIG_qc1(iTot1)       = Dep1%qc(1)
      BIG_lat1(iTot1)      = Dep1%lat
      BIG_lon1(iTot1)      = Dep1%lon
      BIG_time1(iTot1)     = (Dep1%scanDay-1)*24.0 + Dep1%scanUTC/3600.0
      BIG_node1(iTot1)     = Dep1%node
      BIG_iscan1(iTot1)    = Dep1%iscanPos
      BIG_angle1(iTot1)    = Dep1%angle
      BIG_sfc1(iTot1)      = Dep1%iTypSfc
      
      BIG_prod1(iTot1,1)   = Dep1%iTypSfc
      BIG_prod1(iTot1,2)   = Dep1%tpw
      BIG_prod1(iTot1,3)   = Dep1%clw
      BIG_prod1(iTot1,4)   = Dep1%rr
      BIG_prod1(iTot1,5)   = Dep1%SnowGS
      BIG_prod1(iTot1,6)   = Dep1%GWP
      BIG_prod1(iTot1,7)   = Dep1%RWP
      BIG_prod1(iTot1,8)   = Dep1%LWP
      BIG_prod1(iTot1,9)   = Dep1%SWE
      BIG_prod1(iTot1,10)  = Dep1%SIC
      BIG_prod1(iTot1,11)  = Dep1%SIC_FY
      BIG_prod1(iTot1,12)  = Dep1%SIC_MY
      BIG_prod1(iTot1,13)  = Dep1%SnowCover

    ENDDO ProfLoop1
    
    CLOSE(iu_1in)
    
  ENDDO Dep1Loop

  write(*,*) 'Reading List1 done'  
    
  !---- Read all DEPs of data set 2 and save them in BIG array set 2
  iTot2 = 0
  Dep2Loop: DO iFile_2=1,nFiles2

    CALL ReadHdrDEP(iu_2in, Files2In(iFile_2), Dep2, nprf2)
    !write(*,'(A, I)') Files2In(iFile_2), nprf2
    
    ProfLoop2: DO iprf2=1, nprf2
      
      CALL ReadDEP(iu_2in,Dep2,ierr2)
      
      IF ( ierr2 .eq. Warn_EndOfFile   ) EXIT  ProfLoop2
      IF ( ierr2 .eq. Warn_readInvalid ) CYCLE ProfLoop2
     
      iTot2 = iTot2 + 1
      if( iTot2 .gt. nTot2 ) then
        write(*,*)'Error: iTot2 > nTot2 ',iTot2, nTot2
	stop 
      endif
      
      BIG_qc2(iTot2)       = Dep2%qc(1)
      BIG_lat2(iTot2)      = Dep2%lat
      BIG_lon2(iTot2)      = Dep2%lon
      BIG_time2(iTot2)     = (Dep2%scanDay-1)*24.0 + Dep2%scanUTC/3600.0
      BIG_node2(iTot2)     = Dep2%node
      BIG_iscan2(iTot2)    = Dep2%iscanPos
      BIG_angle2(iTot2)    = Dep2%angle
      BIG_sfc2(iTot2)      = Dep2%iTypSfc
      
      BIG_prod2(iTot2,1)   = Dep2%iTypSfc
      BIG_prod2(iTot2,2)   = Dep2%tpw
      BIG_prod2(iTot2,3)   = Dep2%clw
      BIG_prod2(iTot2,4)   = Dep2%rr
      BIG_prod2(iTot2,5)   = Dep2%SnowGS
      BIG_prod2(iTot2,6)   = Dep2%GWP
      BIG_prod2(iTot2,7)   = Dep2%RWP
      BIG_prod2(iTot2,8)   = Dep2%LWP
      BIG_prod2(iTot2,9)   = Dep2%SWE
      BIG_prod2(iTot2,10)  = Dep2%SIC
      BIG_prod2(iTot2,11)  = Dep2%SIC_FY
      BIG_prod2(iTot2,12)  = Dep2%SIC_MY
      BIG_prod2(iTot2,13)  = Dep2%SnowCover
    
    ENDDO ProfLoop2
    
    CLOSE(iu_1in)
    
  ENDDO Dep2Loop

  write(*,*) 'Reading List2 done'  
    
  write(*,*) 'iTot1=', iTot1 
  write(*,*) 'iTot2=', iTot2
  write(*,*) 'Collocation processing...'
  
  !---- do collocation ----
    
  IP2P(:)=0
  
  nCollocated = 0
  ProfLoop3: DO iprf1 = 1, nTot1
      
      lat1  = BIG_lat1(iprf1) 
      lon1  = BIG_lon1(iprf1) 
      time1 = BIG_time1(iprf1)
      node1 = BIG_node1(iprf1) 
      
      !---- distance threshhold value changes with scan position, <= diameter considered the same box
      iscan1 = BIG_iscan1(iprf1)
      !---- pick bigger fov size 
      !DistCut = 0.5 * MAX( fov1_sizes(iscan1), fov2_sizes(iscan1) )
      DistCut = MAX( fov1_sizes(iscan1), fov2_sizes(iscan1) )
      
      ProfLoop4: DO iprf2 = 1, nTot2
     
          lat2  = BIG_lat2(iprf2) 
          lon2  = BIG_lon2(iprf2) 
          time2 = BIG_time2(iprf2) 
          node2 = BIG_node2(iprf2) 
         
          !---- node constraints
          if( node2 .ne. node1 ) CYCLE ProfLoop4

          !---- time constraints
	  timeDiff = ABS( time1 - time2 )
	  if( timeDiff .gt. TimeCut ) CYCLE ProfLoop4
      
	  delta_lat = (lat2 - lat1)*0.0087266
          delta_lon = (lon2 - lon1)*0.0087266
          a = sin(delta_lat) * sin(delta_lat) + cos(lat1*0.0174532) * &
              cos(lat2*0.0174532) * sin(delta_lon) * sin(delta_lon)
          c = 2 * atan2(sqrt(a), sqrt(1-a))
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
	      
	      if ( node1 .eq. 0 .or. node1 .eq. 1 ) then
	      
	          icend = node1 + 1
	          IP2P(icend) = IP2P(icend) + 1
	          iprf = IP2P(icend)
                  
		  !---- data set 1 values ----
                  P2P_lat1(iprf,icend)   = lat1
                  P2P_lon1(iprf,icend)   = lon1
		  P2P_time1(iprf,icend)  = time1
		  P2P_node1(iprf,icend)  = node1
		  P2P_iscan1(iprf,icend) = iscan1
		  P2P_angle1(iprf,icend) = BIG_angle1(iprf1)
		  P2P_sfc1(iprf,icend)   = BIG_sfc1(iprf1)
		  do iprod = 1, NPROD
                     P2P_prod1(iprf,iprod,icend) = BIG_prod1(iprf1,iprod)
		  enddo   
                  
 		  !---- data set 2 values ----
                  P2P_lat2(iprf,icend)   = lat2
                  P2P_lon2(iprf,icend)   = lon2
		  P2P_time2(iprf,icend)  = time2
		  P2P_node2(iprf,icend)  = node2
		  P2P_iscan2(iprf,icend) = BIG_iscan2(iprf2)
		  P2P_angle2(iprf,icend) = BIG_angle2(iprf2)
		  P2P_sfc2(iprf,icend)   = BIG_sfc2(iprf2)
		  do iprod = 1, NPROD
                     P2P_prod2(iprf,iprod,icend) = BIG_prod2(iprf2,iprod)
		  enddo   
                  
              endif
            
	      EXIT ProfLoop4
	    
	  endif
	  
      ENDDO ProfLoop4
      
  ENDDO ProfLoop3
  
 
  write(*,*) 'IP2P=', IP2P(1:2)
  write(*,*) 'nCollocated=', nCollocated
  
  !---- to compute bias ----
  do icend = 1, NCEND
  do iprod = 1, NPROD

    do isfc = 1, NSFC
    do ibin = 1, NBIN
        do iprf = 1, IP2P(icend)
	  if( P2P_prod1(iprf,iprod,icend) .gt. missing .and. &
              P2P_prod2(iprf,iprod,icend) .gt. missing ) then    
	    diff = P2P_prod1(iprf,iprod,icend) - P2P_prod2(iprf,iprod,icend)
	    angle = P2P_angle1(iprf,icend)
            scanpos = P2P_iscan1(iprf,icend)
	    sfc1  = INT(P2P_sfc1(iprf,icend))
	    sfc2  = INT(P2P_sfc2(iprf,icend))

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
	      asym_val(ibin,isfc,iprod,icend) = asym_val(ibin,isfc,iprod,icend) + diff
	      asym_cnt(ibin,isfc,iprod,icend) = asym_cnt(ibin,isfc,iprod,icend) + 1
	    endif
	  endif    
        enddo
    enddo
    enddo

  enddo
  enddo
  
  do icend = 1, NCEND
  do iprod = 1, NPROD
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
    if( asym_cnt(ibin,isfc,iprod,icend) .ge. 1 ) then
      asym_val(ibin,isfc,iprod,icend) = asym_val(ibin,isfc,iprod,icend)/&
                                        asym_cnt(ibin,isfc,iprod,icend)
    else
      asym_val(ibin,isfc,iprod,icend) = missing
    endif
  enddo
  enddo
  enddo
  enddo


  !---- to compute stdv ----
  do icend = 1, NCEND
  do iprod = 1, NPROD

    do isfc = 1, NSFC
    do ibin = 1, NBIN
        do iprf = 1, IP2P(icend)
	  if( P2P_prod1(iprf,iprod,icend) .gt. missing .and. &
              P2P_prod2(iprf,iprod,icend) .gt. missing ) then
	    diff  = P2P_prod1(iprf,iprod,icend) - P2P_prod2(iprf,iprod,icend)
	    angle = P2P_angle1(iprf,icend)
            scanpos = P2P_iscan1(iprf,icend)
	    sfc1  = INT(P2P_sfc1(iprf,icend))
	    sfc2  = INT(P2P_sfc2(iprf,icend))

            if( isfc .lt. NSFC ) then
              if( isAngle) then
                meetCriteria = ( ABS(angle-BIN_BOX(ibin)) .le. 5 .and. &
                                 sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) .and. & 
	                         asym_val(ibin,isfc,iprod,icend) .gt. missing )
              else
                meetCriteria = ( scanpos .eq. ibin .and. &
                                 sfc1 .eq. sfc2 .and. isfc .eq. (sfc1+1) .and. & 
                                 asym_val(ibin,isfc,iprod,icend) .gt. missing )
              endif
            else
              if( isAngle ) then
                meetCriteria = ( ABS(angle-BIN_BOX(ibin)) .le. 5 .and. sfc1 .eq. sfc2 .and. &
                                 asym_val(ibin,isfc,iprod,icend) .gt. missing )
              else
                meetCriteria = ( scanpos .eq. ibin .and. sfc1 .eq. sfc2 .and. &
                                 asym_val(ibin,isfc,iprod,icend) .gt. missing )
              endif
            endif

	    if( meetCriteria ) then
	      stdv_val(ibin,isfc,iprod,icend) = stdv_val(ibin,isfc,iprod,icend) + &
	        ( diff - asym_val(ibin,isfc,iprod,icend) ) * ( diff - asym_val(ibin,isfc,iprod,icend) )
	      stdv_cnt(ibin,isfc,iprod,icend) = stdv_cnt(ibin,isfc,iprod,icend) + 1
	    endif
	  endif    
        enddo
    enddo
    enddo

  enddo
  enddo
  

  do icend = 1, NCEND
  do iprod = 1, NPROD
  do isfc  = 1, NSFC
  do ibin  = 1, NBIN
    if( stdv_cnt(ibin,isfc,iprod,icend) .gt. 1 ) then
      stdv_val(ibin,isfc,iprod,icend) = SQRT( &
      stdv_val(ibin,isfc,iprod,icend)/(stdv_cnt(ibin,isfc,iprod,icend)-1) )
    else
      stdv_val(ibin,isfc,iprod,icend) = missing
    endif
  enddo
  enddo
  enddo
  enddo



  !-----------------------------------------------------
  !     writeout P2P stuff
  !-----------------------------------------------------
  do icend = 1, NCEND
  do iprod = 1, NPROD
 
    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(yyyymmdd)//'_'//trim(prodIds(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*IP2P(icend))
    write(25,rec=1) P2P_prod1(1:IP2P(icend),iprod,icend)
    close(25)

    p2pFile='p2p_collocate_'//trim(satId2)//'_'//trim(yyyymmdd)//'_'//trim(prodIds(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*IP2P(icend))
    write(25,rec=1) P2P_prod2(1:IP2P(icend),iprod,icend)
    close(25)

  enddo
  enddo


  !-----------------------------------------------------
  !     writeout P2P stuff - asymmetry and stdv
  !-----------------------------------------------------
  
  do icend = 1, NCEND
  do iprod = 1, NPROD
 
    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(satId2)//'_glb_asym_'//&
            trim(yyyymmdd)//'_'//trim(prodIds(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*NBIN*NSFC)
    write(25,rec=1) asym_val(1:NBIN,1:NSFC,iprod,icend)
    close(25)

    p2pFile='p2p_collocate_'//trim(satId1)//'_'//trim(satId2)//'_glb_stdv_'//trim(yyyymmdd)//'_'//&
             trim(prodIds(iprod))//'_'//cendIds(icend)//'.dat'
    open(25,file=trim(p2pDir)//p2pFile,form='unformatted', access='direct', recl=4*NBIN*NSFC)
    write(25,rec=1) stdv_val(1:NBIN,1:NSFC,iprod,icend)
    close(25)

  enddo
  enddo


  !-----------------------------------------------------
  !      free up memory
  !-----------------------------------------------------

  DEALLOCATE( BIG_qc1 )
  DEALLOCATE( BIG_lat1 )
  DEALLOCATE( BIG_lon1 )
  DEALLOCATE( BIG_time1 )
  DEALLOCATE( BIG_iscan1 )
  DEALLOCATE( BIG_node1 )
  DEALLOCATE( BIG_angle1 )
  DEALLOCATE( BIG_sfc1 )
  DEALLOCATE( BIG_prod1 )
  
  DEALLOCATE( BIG_qc2 )
  DEALLOCATE( BIG_lat2 )
  DEALLOCATE( BIG_lon2 )
  DEALLOCATE( BIG_time2 )
  DEALLOCATE( BIG_iscan2 )
  DEALLOCATE( BIG_node2 )
  DEALLOCATE( BIG_angle2 )
  DEALLOCATE( BIG_sfc2 )
  DEALLOCATE( BIG_prod2 )
  
  DEALLOCATE( P2P_lat1 )
  DEALLOCATE( P2P_lon1 )
  DEALLOCATE( P2P_time1 )
  DEALLOCATE( P2P_iscan1 )
  DEALLOCATE( P2P_node1 )
  DEALLOCATE( P2P_angle1 )
  DEALLOCATE( P2P_sfc1 )
  DEALLOCATE( P2P_prod1 )

  DEALLOCATE( P2P_lat2 )
  DEALLOCATE( P2P_lon2 )
  DEALLOCATE( P2P_time2 )
  DEALLOCATE( P2P_iscan2 )
  DEALLOCATE( P2P_node2 )
  DEALLOCATE( P2P_angle2 )
  DEALLOCATE( P2P_sfc2 )
  DEALLOCATE( P2P_prod2 )

  DEALLOCATE( asym_val )
  DEALLOCATE( stdv_val )
  DEALLOCATE( asym_cnt )
  DEALLOCATE( stdv_cnt )
  DEALLOCATE( BIN_BOX  )
  DEALLOCATE( fov1_sizes )
    
 CONTAINS   
 
    
  !---- to compute FOV size according to different scan position
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
 

End Program interDep

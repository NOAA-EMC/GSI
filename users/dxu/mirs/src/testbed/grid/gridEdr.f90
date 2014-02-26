!*******************************************************************************
!
! To Grid MIRS EDR or Collocated NWP for different resolution.
! To overcome memory problem, for a product id list(separated by comma),
! we grid product one by one and cend by cend.
!
!  Wanchun Chen      10/20/2010         Original Coder
!  Wanchun Chen      01/20/2011         Modified one by one, cend by cend
!
!*******************************************************************************

Program gridEdr

  USE Consts
  USE misc
  USE utils
  USE IO_Scene
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ABS,COS,INT,MOD,TRIM,BTEST,ALLOCATED
  
  INTEGER            :: iu_list=20,iuEDR,ierr,iprof,nprf
  INTEGER, PARAMETER :: LENF=256
  INTEGER            :: nqc=4
  INTEGER            :: ifile, nfiles
  CHARACTER(LEN=LENF), DIMENSION(:), POINTER :: inputFiles, dumFiles 
  
  INTEGER            :: qc, cend
  REAL               :: tpw, clw, rr, iwp, rwp, lwp, psfc
  
  !----Structures
  TYPE(Scene_type)   :: Scene
  
  INTEGER, PARAMETER :: NLAY=100
  INTEGER, PARAMETER :: NPMAX=32
  INTEGER, PARAMETER :: NCEND=2
  INTEGER            :: NCHAN=20
  INTEGER            :: INT_SATID=SENSOR_ID_N18
  INTEGER            :: NCOL, NROW, ilay, ichan
  LOGICAL            :: nadirLogic = .FALSE.
  CHARACTER(LEN=8)   :: nwpString=DEFAULT_VALUE_STR4
  
  !---- product id stuffs
  CHARACTER(LEN=16),DIMENSION(NPMAX) :: prodIds
  CHARACTER(LEN=16)                  :: prodId
  INTEGER                            :: iprod, NPROD=0, icend=0
  INTEGER                            :: pos1 = 1, pos2, n = 0, i
  CHARACTER(LEN=2),DIMENSION(2)      :: cends = (/ 'as', 'ds' /)
  
  !----namelist data
  CHARACTER(LEN=LENF) :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=8)    :: satId=DEFAULT_VALUE_STR4  ! string type satellite id: n18, m2, f16, f18
  CHARACTER(LEN=64)   :: yyyymmdd=DEFAULT_VALUE_STR4
  REAL                :: gridfactor=DEFAULT_VALUE_INT
  CHARACTER(LEN=LENF) :: gridPath=DEFAULT_VALUE_STR4
  REAL                :: latmin=DEFAULT_VALUE_REAL
  REAL                :: latmax=DEFAULT_VALUE_REAL
  REAL                :: lonmin=DEFAULT_VALUE_REAL
  REAL                :: lonmax=DEFAULT_VALUE_REAL
  INTEGER             :: isMirs=DEFAULT_VALUE_INT   ! 0(MIRS EDR), 1(gdas), 2(ecmwf), 3(gfs)
  INTEGER             :: processMode=DEFAULT_VALUE_INT   ! 1 - daily   0 - orbit
  INTEGER             :: fmType=0
  CHARACTER(LEN=256)  :: prodStr ! comma separated product id list (Ex: 'tpw,clw,rr,temp' )
  
  NAMELIST /gridEdrNameList/filesList,satId,yyyymmdd,gridfactor,gridPath,&
            latmin,latmax,lonmin,lonmax,isMirs,processMode,fmType,prodStr
  
  !-------------------------------------------------------------
  !     Output variables identifiers definiton section
  !-------------------------------------------------------------
  REAL                                :: var1D=DEFAULT_VALUE_REAL
  REAL, DIMENSION(:,:),   ALLOCATABLE :: var1D_grid
  
  REAL, DIMENSION(:),     ALLOCATABLE :: varChan
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: varChan_grid

  REAL, DIMENSION(:),     ALLOCATABLE :: varLay
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: varLay_grid

  INTEGER :: is1D = 0, isChan = 0, isLay = 0  ! 1D, channel or layer product
  INTEGER :: isQc = 0  ! QC failing impact or not
  
  REAL                                  :: qcf=DEFAULT_VALUE_REAL
  INTEGER(2), DIMENSION(:), ALLOCATABLE :: qcg

  CHARACTER(LEN=LENF) :: gridFile
  
  !-------------------------------------------------------------
  !     grid and filling variables
  !-------------------------------------------------------------
  INTEGER, DIMENSION(:,:), ALLOCATABLE  :: nadir
  INTEGER     :: NFOV=30
  INTEGER     :: LATLIM_A=12
  REAL        :: FIX=0.5
  REAL        :: loncorr
  INTEGER     :: ifov
  REAL        :: lonleft, lonright
  INTEGER     :: gridlon_left, gridlon_right
  INTEGER     :: gridlat_bot, gridlat_top
  INTEGER     :: lonbox, latbox, near_nadir
  REAL, DIMENSION(:), ALLOCATABLE :: fov_size
  REAL        :: SCAN_ANG=3.3
  REAL        :: RSAT=833.0

  !-------------------------------------------------------------
  !     Execute section begins here
  !-------------------------------------------------------------
  READ(*,NML=gridEdrNameList)

  ! string type satellite id --> integer type satellite id
  if      ( strcmp(satId, SATID_F16)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F16
      NCHAN=24
      if( fmType .eq. 0 ) then
         NFOV=30
      else if( fmType .eq. 1 ) then
         NFOV=60
      else if( fmType .eq. 2 ) then
         NFOV=90
      else if( fmType .eq. 3 ) then
         NFOV=180
      else
         STOP 'Error: Incorrect fmType for F16'
      endif
      
   else if( strcmp(satId, SATID_F17)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F17
      NCHAN=24
      if( fmType .eq. 0 ) then
         NFOV=30
      else if( fmType .eq. 1 ) then
         NFOV=60
      else if( fmType .eq. 2 ) then
         NFOV=90
      else if( fmType .eq. 3 ) then
         NFOV=180
      else
         STOP 'Error: Incorrect fmType for F17'
      endif
      
   else if( strcmp(satId, SATID_F18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F18
      NCHAN=24
      if( fmType .eq. 0 ) then
         NFOV=30
      else if( fmType .eq. 1 ) then
         NFOV=60
      else if( fmType .eq. 2 ) then
         NFOV=90
      else if( fmType .eq. 3 ) then
         NFOV=180
      else
         STOP 'Error: Incorrect fmType for F18'
      endif
      
   else if( strcmp(satId, SATID_N18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N18
      NCHAN=20
      if( fmType .eq. 0 ) then
         NFOV=30
         SCAN_ANG=3.3
      else if( fmType .eq. 1 ) then
         NFOV=90
         SCAN_ANG=1.1
      else
         STOP 'Error: Incorrect fmType for N18'
      endif
      
   else if( strcmp(satId, SATID_N19)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N19
      NCHAN=20
      if( fmType .eq. 0 ) then
         NFOV=30
         SCAN_ANG=3.3
      else if( fmType .eq. 1 ) then
         NFOV=90
         SCAN_ANG=1.1
      else
         STOP 'Error: Incorrect fmType for N19'
      endif
      
   else if( strcmp(satId, SATID_METOPA) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPA
      NCHAN=20
      if( fmType .eq. 0 ) then
         NFOV=30
         SCAN_ANG=3.3
      else if( fmType .eq. 1 ) then
         NFOV=90
         SCAN_ANG=1.1
      else
         STOP 'Error: Incorrect fmType for metopA'
      endif
      
   else if( strcmp(satId, SATID_METOPB) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPB
      NCHAN=20
      if( fmType .eq. 0 ) then
         NFOV=30
         SCAN_ANG=3.3
      else if( fmType .eq. 1 ) then
         NFOV=90
         SCAN_ANG=1.1
      else
         STOP 'Error: Incorrect fmType for metopB'
      endif
      
   else if( strcmp(satId, SATID_TRMM)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_TRMM
      NCHAN=9
      if( fmType .eq. -1 ) then
         NFOV=26
      else if( fmType .eq. 0 ) then
         NFOV=104
      else if( fmType .eq. 1 ) then
         NFOV=208
      else
         STOP 'Error: Incorrect fmType for TRMM TMI'
      endif

   else if( strcmp(satId, SATID_TRMM2A12)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_TRMM2A12
      NCHAN=9
      if( fmType .eq. -1 ) then
         NFOV=26
      else if( fmType .eq. 0 ) then
         NFOV=104
      else if( fmType .eq. 1 ) then
         NFOV=208
      else
         STOP 'Error: Incorrect fmType for TRMM_2A12'
      endif
      
   else if( strcmp(satId, SATID_NPP)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_NPP
      NCHAN=22
      RSAT=824.0
      if( fmType .eq. 0 ) then
         NFOV=32
         SCAN_ANG=3.3
      else if( fmType .eq. 1 ) then
         NFOV=96
         SCAN_ANG=1.1
      else
         STOP 'Error: Incorrect fmType for NPP ATMS'
      endif
      
   else if( strcmp(satId, SATID_AMSRE)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_AMSRE
      NCHAN=12
      NFOV=191
      
   else if( strcmp(satId, SATID_GCOMW1)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_GCOMW1
      NCHAN=14
      if( fmType .eq. -1 ) then
         NFOV=27
      else if( fmType .eq. 0 ) then
         NFOV=243
      else if( fmType .eq. 1 ) then
         NFOV=486
      else
         STOP 'Error: Incorrect fmType for GCOMW1 AMSR2'
      endif
      
   else if( strcmp(satId, SATID_FY3RI)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_FY3RI
      NCHAN=10
      NFOV=120
      
! For now, MT MADRAS proxy data uses same params as TRMM_TMI
! This will need to be changed when real data are processed
   else if( strcmp(satId, SATID_MTMA)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_MTMA
      NCHAN=9
!      if( fmType .eq. -1 ) then
!        NFOV=60
!      else if( fmType .eq. 0 ) then
!        NFOV=240
!      else if( fmType .eq. 1 ) then
!        NFOV=480
!      else
!        STOP 'Error: Incorrect fmType for MT MADRAS'
!      endif
      if( fmType .eq. -1 ) then
         NFOV=27
      else if( fmType .eq. 0 ) then
         NFOV=107
      else if( fmType .eq. 1 ) then
         NFOV=214
      else
         STOP 'Error: Incorrect fmType for MT MADRAS'
      endif

! For now, MT SAPHIR proxy data uses same params as N18_AMSUA_MHS
! This will need to be changed when real data are processed
   else if( strcmp(satId, SATID_MTSA)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_MTSA
      NCHAN=6
!      if( fmType .eq. -1 ) then
!        NFOV=45
!        SCAN_ANG=3.3
!      elseif( fmType .eq. 0 ) then
!        NFOV=91
!        SCAN_ANG=2.2
!      else if( fmType .eq. 1 ) then
!        NFOV=182
!        SCAN_ANG=1.1
!      else
!        STOP 'Error: Incorrect fmType for MT SAPHIR'
!      endif
      if( fmType .eq. -1 ) then
         NFOV=26
         SCAN_ANG=3.3
      elseif( fmType .eq. 0 ) then
         NFOV=65
         SCAN_ANG=2.2
      else if( fmType .eq. 1 ) then
         NFOV=130
         SCAN_ANG=1.1
      else
         STOP 'Error: Incorrect fmType for MT SAPHIR'
      endif
      
   else
      write(*,*)'satId='//satId
      STOP 'Error: unsupported satId'
      
   endif
  
  !---- for colocation data set ----
   if( isMirs .eq. 1 ) then
      nwpString='gdas'
   else if( isMirs .eq. 2 ) then
      nwpString='ecmwf'
   else if( isMirs .eq. 3 ) then
      nwpString='gfs'
   endif
  
  
  !---- extract prodStr and put them into prodIds
   DO
      pos2 = INDEX(prodStr(pos1:), ",")
      if(pos2 == 0) THEN
         n = n + 1
         prodIds(n) = prodStr(pos1:)
         EXIT
      END IF
      n = n + 1
      prodIds(n) = prodStr(pos1:pos1+pos2-2)
      pos1 = pos2+pos1
   END DO
  
   NPROD=n
   
  !DO i = 1, n
  !  WRITE(*, '(A)') TRIM(prodIds(i))
  !END DO
  
  
   if( isMirs .ne. 0 ) nqc = 1  ! NWP files, nqc = 1
  

   call ReadList(iu_list, trim(filesList), inputFiles, nfiles, dumFiles, '', '')
   if(nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 
   
  !--------------------------------------------------------------------------------
  ! Allocate output array and initialize them, always output onto global grid mesh
  !--------------------------------------------------------------------------------
   NCOL=int(360*gridfactor)
   NROW=int(180*gridfactor)
   
  
  !---------------------------------------------------------------------------------
  !     Start loop over cend/product list 
  !---------------------------------------------------------------------------------
   prodLoop: DO iprod = 1, NPROD
   cendLoop: DO icend = 1, NCEND
         
      prodId = TRIM(prodIds(iprod))
      write(*,*)
      write(*,'(A)') 'grid '//TRIM(prodId)//' '//cends(icend)//' ......'
    
      ALLOCATE (nadir ( 0:NCOL-1, 0:NROW-1 ) )  
      nadir = NFOV
      
      is1D = 0
      isChan = 0
      isLay = 0
      isQc = 0
      
      if( strcmp(prodId,'angle')    .eq. 0 .or. strcmp(prodId,'chisq')   .eq. 0 .or. &
           strcmp(prodId,'nattempt') .eq. 0 .or. strcmp(prodId,'niter')   .eq. 0 .or. & 
           strcmp(prodId,'sfc')      .eq. 0 .or. strcmp(prodId,'scanday') .eq. 0 .or. &
           strcmp(prodId,'scanpos')  .eq. 0  ) then
         
         is1D = 1
         isChan = 0
         isLay = 0
         isQc = 0
         
         ALLOCATE (var1D_grid ( 0:NCOL-1, 0:NROW-1 ) )
         var1D_grid = -999.0
         
      else if( &
           strcmp(prodId,'clw')      .eq. 0 .or. strcmp(prodId,'iwp')     .eq. 0 .or. & 
           strcmp(prodId,'psfc')     .eq. 0 .or. strcmp(prodId,'rr')      .eq. 0 .or. &
           strcmp(prodId,'rwp')      .eq. 0 .or. strcmp(prodId,'lwp')     .eq. 0 .or. &
           strcmp(prodId,'swe')      .eq. 0 .or. strcmp(prodId,'tskin')   .eq. 0 .or. &
           strcmp(prodId,'wspd')     .eq. 0 .or. &
           strcmp(prodId,'tpw')      .eq. 0 ) then
         
         is1D = 1
         isChan = 0
         isLay = 0
         isQc = 1
         
         ALLOCATE (var1D_grid ( 0:NCOL-1, 0:NROW-1 ) )
         var1D_grid = -999.0
         
      else if( strcmp(prodId,'qc') .eq. 0 ) then
         is1D = 1
         isChan = 0
         isLay = 0
         isQc = 0
         
         ALLOCATE (qcg(nqc))
         qcg = -999
         
         ALLOCATE (var1D_grid ( 0:NCOL-1, 0:NROW-1 ) )
         var1D_grid = -999.0
         
      else if( strcmp(prodId,'tbc') .eq. 0 .or. strcmp(prodId,'tbf') .eq. 0 .or. &
           strcmp(prodId,'tbu') .eq. 0 ) then
         
         is1D = 0
         isChan = 1
         isLay = 0
         isQc = 0
         
         ALLOCATE (varChan(NCHAN))
         varChan(:) = -999.0     
         ALLOCATE ( varChan_grid ( 0:NCOL-1, 0:NROW-1, 1:NCHAN ) )     
         varChan_grid = -999.0
         
      else if( strcmp(prodId,'em') .eq. 0 .or. strcmp(prodId,'tbl') .eq. 0 ) then
         
         is1D = 0
         isChan = 1
         isLay = 0
         isQc = 1
         
         ALLOCATE (varChan(NCHAN))
         varChan(:) = -999.0     
         ALLOCATE ( varChan_grid ( 0:NCOL-1, 0:NROW-1, 1:NCHAN ) )
         varChan_grid = -999.0
         
      else if( strcmp(prodId,'temp')      .eq. 0 .or. strcmp(prodId,'wv')       .eq. 0 .or. &
           strcmp(prodId,'clwp')      .eq. 0 .or. strcmp(prodId,'rainp')    .eq. 0 .or. &
           strcmp(prodId,'graupelp')  .eq. 0 .or. strcmp(prodId,'pressure') .eq. 0) then
         
         is1D = 0
         isChan = 0
         isLay = 1
         isQc = 1
         
         ALLOCATE (varLay(NLAY))
         varLay(:) = -999.0     
         ALLOCATE ( varLay_grid ( 0:NCOL-1, 0:NROW-1, 1:NLAY ) )
         varLay_grid = -999.0
         
      endif
    
    !---- get fov size ----
      if(.NOT.ALLOCATED(fov_size)) ALLOCATE(fov_size(0:NFOV-1))
      if( INT_SATID .eq. SENSOR_ID_N18 .or. INT_SATID .eq. SENSOR_ID_METOPA .or. &
           INT_SATID .eq. SENSOR_ID_N19 .or. INT_SATID .eq. SENSOR_ID_METOPB .or. &
           INT_SATID .eq. SENSOR_ID_NPP ) then
         call get_fov_size(fov_size,NFOV,SCAN_ANG,RSAT)
    !--- For now do same filling for MT SAPHIR as for N18, but may need to change with real data
      else if( INT_SATID .eq. SENSOR_ID_MTSA ) then
         call get_fov_size(fov_size,NFOV,SCAN_ANG,RSAT)
      else if(INT_SATID .eq. SENSOR_ID_F16 .or. INT_SATID .eq. SENSOR_ID_F18 .or. &
                INT_SATID .eq. SENSOR_ID_F17 ) then
         fov_size(:) = 0.6745
      else
         fov_size(:) = 1.0
      endif
    
    !---- loop over the files ----  
      FilesLoop: DO ifile=1,nfiles

      !write(*,'(A)') trim(inputFiles(ifile))

      !---Read header of EDR/Collocated NWP file
         CALL ReadHdrScene(iuEDR,inputFiles(ifile),Scene,nprf)

      !---Loop over the profiles within the file
         ProfilesLoop: DO iprof=1,nPrf
            CALL ReadScene(iuEDR,Scene,ierr)

            if(ierr.eq.Warn_EndOfFile)   EXIT  ProfilesLoop
            if(ierr.eq.Warn_readInvalid) CYCLE ProfilesLoop
            if(ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. EDR/NWP file.')
            
            cend = Scene%node + 1 
            IF( cend .ne. icend ) CYCLE ProfilesLoop
            
            loncorr=abs(1/cos(PI*Scene%lat/180.0))
            if( loncorr .gt. 200 ) loncorr=200
            ifov = MOD( iprof, NFOV )
            
            if( INT_SATID .eq. SENSOR_ID_N18 .or. INT_SATID .eq. SENSOR_ID_METOPA .or. &
                 INT_SATID .eq. SENSOR_ID_N19 .or. INT_SATID .eq. SENSOR_ID_METOPB .or. &
                 INT_SATID .eq. SENSOR_ID_NPP ) THEN
               
               lonleft  = Scene%lon - 0.5 * fov_size(ifov) * loncorr
               lonright = Scene%lon + 0.5 * fov_size(ifov) * loncorr
               gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
               gridlon_right = INT((lonright + 180.0) * gridfactor + fix)
               
               if( fmType .eq. 0 ) then
                  if( abs(ifov-(NFOV-1.)/2.) .lt. (LATLIM_A - 0.4 )) then
                     gridlat_bot = INT((Scene%lat+90) * gridfactor)
                     gridlat_top = gridlat_bot + 1 
                  else 
                     gridlat_bot = INT((Scene%lat+90.0) * gridfactor - 1 + fix)
                     gridlat_top = INT((Scene%lat+90.0) * gridfactor + 1 + fix)
                  endif
               else
                  gridlat_bot = INT((Scene%lat+90) * gridfactor) 
                  gridlat_top = INT((Scene%lat+90) * gridfactor) 
               endif
               
            ELSE if( INT_SATID .eq. SENSOR_ID_F16 .or. INT_SATID .eq. SENSOR_ID_F18 .or. &
	              INT_SATID .eq. SENSOR_ID_F17 ) THEN
               
               lonleft  = Scene%lon - 0.5 * fov_size(ifov) * loncorr
               lonright = Scene%lon + 0.5 * fov_size(ifov) * loncorr
               gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
               gridlon_right = INT((lonright + 180.0) * gridfactor + fix)
               
               gridlat_bot = INT((Scene%lat+90.0) * gridfactor - 1 + fix)
               gridlat_top = INT((Scene%lat+90.0) * gridfactor + 1 + fix)
               
            ELSE IF ( INT_SATID .eq. SENSOR_ID_AMSRE .or. INT_SATID .eq. SENSOR_ID_GCOMW1 ) THEN
               
               lonleft  = Scene%lon - 0.5 * fov_size(ifov) * loncorr
               lonright = Scene%lon + 0.5 * fov_size(ifov) * loncorr
               gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
               gridlon_right = INT((lonright + 180.0) * gridfactor + fix)
               
               gridlat_bot = INT((Scene%lat+90.0) * gridfactor - 1 + fix)
               gridlat_top = INT((Scene%lat+90.0) * gridfactor + 1 + fix)
               !--- For now do same filling for MT SAPHIR as for N18, but may need to change with real data
            ELSE if( INT_SATID .eq. SENSOR_ID_MTSA ) THEN
               
               lonleft  = Scene%lon - 0.5 * fov_size(ifov) * loncorr
               lonright = Scene%lon + 0.5 * fov_size(ifov) * loncorr
               gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
               gridlon_right = INT((lonright + 180.0) * gridfactor + fix)
               
               if( fmType .eq. 0 ) then
                  if( abs(ifov-(NFOV-1.)/2.) .lt. (LATLIM_A - 0.4 )) then
                     gridlat_bot = INT((Scene%lat+90) * gridfactor)
                     gridlat_top = gridlat_bot + 1 
                  else 
                     gridlat_bot = INT((Scene%lat+90.0) * gridfactor - 1 + fix)
                     gridlat_top = INT((Scene%lat+90.0) * gridfactor + 1 + fix)
                  endif
               else
                  gridlat_bot = INT((Scene%lat+90) * gridfactor) 
                  gridlat_top = INT((Scene%lat+90) * gridfactor) 
               endif
            
            ELSE  ! NO FILLING
               
               gridlon_left  = INT((Scene%lon + 180.0) * gridfactor + fix)
               gridlon_right = INT((Scene%lon + 180.0) * gridfactor + fix)
               gridlat_bot   = INT((Scene%lat + 90.0 ) * gridfactor + fix)
               gridlat_top   = INT((Scene%lat + 90.0 ) * gridfactor + fix)
            
            ENDIF


            if( gridlon_left  .lt. 0    ) gridlon_left=0
            if( gridlon_left  .ge. NCOL ) gridlon_left=NCOL-1
            if( gridlon_right .lt. 0    ) gridlon_right=0
            if( gridlon_right .ge. NCOL ) gridlon_right=NCOL-1
            
            if( gridlat_bot   .lt. 0    ) gridlat_bot=0
            if( gridlat_top   .lt. 0    ) gridlat_top=0
            if( gridlat_top   .ge. NROW ) gridlat_top=NROW-1
            if( gridlat_bot   .ge. NROW ) gridlat_bot=NROW-1
            
            qc =  Scene%qc(1)
            psfc = Scene%SfcPress

        !---- extract variables from Scece structure ( 16 1D variables) ----
            if( strcmp(prodId,'angle') .eq. 0 ) then
               var1D = Scene%Angle
            else if( strcmp(prodId,'chisq') .eq. 0 ) then
               var1D = Scene%Chisq
            else if( strcmp(prodId,'nattempt') .eq. 0 ) then
               var1D = Scene%nAttempt
            else if( strcmp(prodId,'niter') .eq. 0 ) then
               var1D = Scene%nIter
            else if( strcmp(prodId,'psfc') .eq. 0 ) then
               var1D = Scene%SfcPress
            else if( strcmp(prodId,'sfc') .eq. 0 ) then
               var1D = Scene%iTypSfc
            else if( strcmp(prodId,'scanday') .eq. 0 ) then
               var1D = Scene%scanDay + Scene%scanUTC/86400.0
            else if( strcmp(prodId,'scanpos') .eq. 0 ) then
               var1D = Scene%iscanPos
            else if( strcmp(prodId,'swe') .eq. 0 ) then
               var1D = Scene%SnowDepth
            else if( strcmp(prodId,'wspd') .eq. 0 ) then
               var1D = Scene%WindSp
            else if( strcmp(prodId,'tskin') .eq. 0 ) then
               var1D = Scene%Tskin
               !---- 6 derived variables ( supposed to be in DEP for MIRS )----
            else if( strcmp(prodId,'clw') .eq. 0 ) then
               var1D = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                    Scene%SfcPress,Scene%Clw(1:Scene%nLay))
            else if( strcmp(prodId,'tpw') .eq. 0 ) then
               call ComputeTPW(Scene%Pres_lev(1:Scene%nLev),Scene%SfcPress, &
                    Scene%Absorb_lay(1:Scene%nLay,Scene%iH2o),tpw)
               var1D = tpw
            else if( strcmp(prodId,'iwp') .eq. 0 ) then
               var1D = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                    Scene%SfcPress,Scene%Graupel(1:Scene%nLay))
            else if( strcmp( prodId, 'rwp' ) .eq. 0 ) then
               var1D = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                    Scene%SfcPress,Scene%Rain(1:Scene%nLay))
            else if( strcmp( prodId, 'lwp' ) .eq. 0 ) then
               clw = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                    Scene%SfcPress,Scene%Clw(1:Scene%nLay))
               rwp = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                    Scene%SfcPress,Scene%Rain(1:Scene%nLay))
               !lwp = clw + rwp          
               var1D = clw + rwp
            else if( strcmp(prodId,'rr') .eq. 0 ) then
               var1D = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                    Scene%SfcPress,Scene%Rain(1:Scene%nLay))


        !---- 1 1-D qc variables ----
            else if( strcmp(prodId,'qc') .eq. 0 ) then
               qcg(:) = Scene%qc(:)
               if( qcg(1) .eq. 0 ) qcf = 0.0
               if( qcg(1) .eq. 2 ) qcf = 1.0
               
               if( isMirs .eq. 0 .and. qcg(1) .NE. 2 .and. qcg(1) .ge. 0 ) then
                  
                  if(BTEST(qcg(2),3) .EQV. .TRUE.) qcf=2.0
                  if(BTEST(qcg(2),4) .EQV. .TRUE.) qcf=3.0
                  if(BTEST(qcg(2),5) .EQV. .TRUE.) qcf=4.0
                  if(BTEST(qcg(3),1) .EQV. .TRUE.) qcf=5.0
                  if(BTEST(qcg(3),4) .EQV. .TRUE.) qcf=6.0
                  if((BTEST(qcg(3),1) .EQV. .TRUE.) .AND. (BTEST(qcg(3),4) .EQV. .TRUE.)) qcf=7.0
                  if((BTEST(qcg(2),2) .EQV. .TRUE.) .OR. (BTEST(qcg(3),5) .EQV. .TRUE.)) THEN
                     IF((BTEST(qcg(3),2) .EQV. .TRUE.)) qcf=8.0
                  ENDIF
                  if((BTEST(qcg(2),2) .EQV. .FALSE.) .AND. (BTEST(qcg(3),5) .EQV. .FALSE.)) THEN
                     IF((BTEST(qcg(3),2) .EQV. .TRUE.)) qcf=9.0
                  ENDIF
                  
               endif
               
               var1D = qcf

        !---- 5 channel variables ----
            else if( strcmp(prodId,'em') .eq. 0 ) then
               varChan(:) = Scene%Emiss(:)
            else if( strcmp(prodId,'tbu') .eq. 0 ) then
               varChan(:) = Scene%Ym(:)
            else if( strcmp(prodId,'tbc') .eq. 0 ) then
               varChan(:) = Scene%YmCorr(:)
            else if( strcmp(prodId,'tbf') .eq. 0 ) then
               varChan(:) = Scene%YFwd(:)
            else if( strcmp(prodId,'tbl') .eq. 0 ) then
               clw = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                    Scene%SfcPress,Scene%Clw(1:Scene%nLay))
               if( clw .lt. MAX_CLD4CLR ) varChan(:) = Scene%YmCorr(:)
               if( clw .ge. MAX_CLD4CLR ) varChan(:) = -999.0

        !---- 6 layer variables ----
            else if( strcmp(prodId,'temp') .eq. 0 ) then
               varLay(:) = Scene%Temp_lay(:)
            else if( strcmp(prodId,'wv') .eq. 0 ) then
               varLay(:) = Scene%Absorb_lay(:,1)
            else if( strcmp(prodId,'clwp') .eq. 0 ) then
               varLay(:) = Scene%clw(:)
            else if( strcmp(prodId,'rainp') .eq. 0 ) then
               varLay(:) = Scene%rain(:)
            else if( strcmp(prodId,'graupelp') .eq. 0 ) then
               varLay(:) = Scene%graupel(:)
            else if( strcmp(prodId,'pressure') .eq. 0 ) then
               varLay(:) = Scene%Pres_lay(:)
            endif
        !---- end extracting variables from Scece structure ------------------


            near_nadir = INT(abs(ifov - (NFOV-1.)/2.) + 0.6)

        !---- start putting them into grid mesh ----
            do latbox=gridlat_bot,  gridlat_top
               do lonbox=gridlon_left, gridlon_right
                  
                  nadirLogic = near_nadir .le. nadir(lonbox,latbox)
                  if( INT_SATID .eq. SENSOR_ID_AMSRE .or. INT_SATID .eq. SENSOR_ID_FY3RI .or. &
                       INT_SATID .eq. SENSOR_ID_TRMM  .or. INT_SATID .eq. SENSOR_ID_GPM   .or. &
                       INT_SATID .eq. SENSOR_ID_TRMM2A12 ) nadirLogic = .TRUE.
                  
                  if( cend .ge. 1 .and. nadirLogic ) then
                     nadir(lonbox,latbox) = near_nadir
                     
                     if( isQc .eq. 0 ) then  ! NO QC failing impacted variables
                        
                        if( is1D .eq. 1 ) then
                           var1D_grid(lonbox,latbox) = var1D
                        else if( isChan .eq. 1 ) then
                           varChan_grid(lonbox,latbox,1:NCHAN) = varChan(1:NCHAN)
                        else if( isLay .eq. 1 ) then
                           do ilay = 1, NLAY
                              if( Scene%Pres_lay(ilay) .lt. psfc ) varLay_grid(lonbox,latbox,ilay) = varLay(ilay)
                           enddo
                        endif
                        
                     else  ! QC failing impacted variables 
                        
                        if( qc .lt. 2 ) then
                           if( is1D .eq. 1 ) then
                              var1D_grid(lonbox,latbox) = var1D
                           else if( isChan .eq. 1 ) then
                              varChan_grid(lonbox,latbox,1:NCHAN) = varChan(1:NCHAN)
                           else if( isLay .eq. 1 ) then
                              do ilay = 1, NLAY
                                 if( Scene%Pres_lay(ilay) .lt. psfc ) varLay_grid(lonbox,latbox,ilay) = varLay(ilay)
                              enddo
                           endif
                        else if( qc .eq. 2 ) then 
                           if( is1D .eq. 1 ) then
                              var1D_grid(lonbox,latbox) = -99.0
                           else if( isChan .eq. 1 ) then
                              varChan_grid(lonbox,latbox,1:NCHAN) = -99.0
                           else if( isLay .eq. 1 ) then
                              do ilay = 1, NLAY
                                 if( Scene%Pres_lay(ilay) .lt. psfc ) varLay_grid(lonbox,latbox,ilay) = -99.0
                              enddo
                           endif
                        endif
                        
                     endif  ! QC failing branch end
                     
                  endif
                  
               enddo
            enddo
            !---- end putting them into grid mesh ------
            
         ENDDO ProfilesLoop
         
         !---Close the EDR file
         CLOSE (iuEDR)
         !---Release memory allocated to Scene
         CALL DestroyScene(Scene)
         
      ENDDO FilesLoop
    
    !---- start write out --------------------------------------------------------------------------
      gridFile='GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prodId)//'_'//cends(icend)//'.dat'
      if( isMirs .ge. 1 ) gridFile='GRID_'//trim(satId)//'_'//trim(nwpString)//'_'//&
           trim(yyyymmdd)//'_'//trim(prodId)//'_'//cends(icend)//'.dat'
      write(*,'(A)') 'grid file='//trim(gridPath)//trim(gridFile)
      open(25,file=trim(gridPath)//trim(gridFile),form='unformatted',access='direct',recl=4*NCOL*NROW)
      
      if( is1D .eq. 1 ) then
         write(25,rec=1) var1D_grid( 0:NCOL-1, 0:NROW-1 )
      else if( isChan .eq. 1 ) then
         do ichan=1,NCHAN
            write(25,rec=ichan) varChan_grid( 0:NCOL-1, 0:NROW-1, ichan )
         enddo
      else if( isLay .eq. 1 ) then
         do ilay=1,NLAY
            write(25,rec=ilay) varLay_grid( 0:NCOL-1, 0:NROW-1, ilay )
         enddo
      endif
      
      close(25)
    !---- end write out ----------------------------------------------------------------------------
    
      IF( ALLOCATED(nadir)        ) DEALLOCATE( nadir )
      IF( ALLOCATED(varChan)      ) DEALLOCATE( varChan )
      IF( ALLOCATED(varLay)       ) DEALLOCATE( varLay )
      IF( ALLOCATED(var1D_grid)   ) DEALLOCATE( var1D_grid )
      IF( ALLOCATED(varChan_grid) ) DEALLOCATE( varChan_grid )
      IF( ALLOCATED(varLay_grid)  ) DEALLOCATE( varLay_grid )
      IF( ALLOCATED(qcg)          ) DEALLOCATE( qcg )
      IF( ALLOCATED(fov_size)     ) DEALLOCATE( fov_size )
      
   ENDDO cendLoop
   ENDDO prodLoop
  !---------------------------------------------------------------------------------
  !     End loop over cend/product list 
  !---------------------------------------------------------------------------------
  
  !---- Release memory
   DEALLOCATE( inputFiles )
   DEALLOCATE( dumFiles )
   
  
end program gridEdr

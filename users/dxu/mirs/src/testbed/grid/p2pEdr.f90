!***************************************************************************************************
! 
! To ouput P2P data parameter by parameter / cend by cend to overcome limited memory problem.
! 
! Wanchun Chen        11/18/2008        Original Coder
!
!***************************************************************************************************

Program p2pEdr

  USE Consts
  USE misc
  USE utils
  USE IO_Scene
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE
  
  !---INTRINSIC FUNCTIONS USED
  INTRINSIC :: TRIM,MIN
  
  INTEGER, PARAMETER :: nlen = 256
  INTEGER, PARAMETER :: NPMAX = 32
  INTEGER, PARAMETER :: NCEND = 2
  INTEGER, PARAMETER :: CHISQ_THRESH = 5
  
  INTEGER            :: iu_list = 20, iuEDR, ierr, iprf, nprf
  INTEGER            :: ifile, nfiles, inode
  REAL               :: tpw, clw, rwp, iwp, lwp, lat, lon, psfc, chisq
  INTEGER            :: qc, icend, cend, ok = 0
  
  !---- EDR Structures
  TYPE(Scene_type)   :: Scene
  
  CHARACTER(LEN = nlen), DIMENSION(:), POINTER :: inputFiles, dumFiles 
  
  INTEGER              :: ilay, ichan
  CHARACTER(LEN = 8)   :: nwpString = DEFAULT_VALUE_STR4
 
  !---- product id stuffs
  CHARACTER(LEN = 16),DIMENSION(NPMAX) :: prodIds
  CHARACTER(LEN = 16)                  :: prodId
  INTEGER                              :: iprod, NPROD = 0
  INTEGER                              :: pos1 = 1, pos2, n = 0, i
  CHARACTER(LEN = 2),DIMENSION(2)      :: cends = (/ 'as', 'ds' /)
  
  !-------------------------------------------------------------------------------
  !     Output variables identifiers definiton section
  !-------------------------------------------------------------------------------
  REAL                                :: var1D = DEFAULT_VALUE_REAL
  REAL, DIMENSION(:,:),   ALLOCATABLE :: var1D_p2p
  
  REAL, DIMENSION(:),     ALLOCATABLE :: varChan
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: varChan_p2p

  REAL, DIMENSION(:),     ALLOCATABLE :: varLay
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: varLay_p2p

  INTEGER :: is1D = 0, isChan = 0, isLay = 0
  
  !-----------------------------------------------------
  !     P2P identifiers
  !-----------------------------------------------------
  CHARACTER( LEN = nlen )  :: p2pFile,metaFile
  INTEGER                  :: MAX_FILE = 28
  INTEGER                  :: MAX_PROFILE = 30000, NLAY = 100, NCHAN = 20
  LOGICAL                  :: metaExist=.FALSE.
  
  !---- namelist data
  CHARACTER( LEN = nlen )  :: filesList = DEFAULT_VALUE_STR4
  CHARACTER( LEN = 8 )     :: satId = DEFAULT_VALUE_STR4  ! n18, m2, f16
  CHARACTER( LEN = 64 )    :: yyyymmdd = DEFAULT_VALUE_STR4
  CHARACTER( LEN = nlen )  :: p2pPath = DEFAULT_VALUE_STR4
  REAL                     :: latmin = DEFAULT_VALUE_REAL
  REAL                     :: latmax = DEFAULT_VALUE_REAL
  REAL                     :: lonmin = DEFAULT_VALUE_REAL
  REAL                     :: lonmax = DEFAULT_VALUE_REAL
  INTEGER                  :: isMirs = DEFAULT_VALUE_INT  ! 0(EDR),1(gdas),2(ecmwf),3(gfs)
  INTEGER                  :: processMode = DEFAULT_VALUE_INT   ! 1 - daily   0 - orbit
  INTEGER                  :: fmType=0
  CHARACTER( LEN = nlen )  :: prodStr ! comma separated product id list (Ex: 'chisq,scanpos,sfc,temp,wv' )
  
  NAMELIST /p2pEdrNameList/filesList,satId,yyyymmdd,p2pPath,latmin,latmax,lonmin,lonmax,&
            isMirs,processMode,fmType,prodStr

  !-----------------------------------------------------
  !     Execute section begins here
  !-----------------------------------------------------
  
  READ(*, NML = p2pEdrNameList)

  if( strcmp(  satId, SATID_F16 )         .eq. 0 ) then
    NCHAN = 24

  else if( strcmp(  satId, SATID_F17 )    .eq. 0 ) then
    NCHAN = 24

  else if( strcmp(  satId, SATID_F18 )    .eq. 0 ) then
    NCHAN = 24

  else if( strcmp(  satId, SATID_N18 )    .eq. 0 ) then
    NCHAN = 20

  else if( strcmp(  satId, SATID_N19 )    .eq. 0 ) then
    NCHAN = 20

  else if( strcmp(  satId, SATID_METOPA ) .eq. 0 ) then
    NCHAN = 20

  else if( strcmp(  satId, SATID_METOPB ) .eq. 0 ) then
    NCHAN = 20

  else if( strcmp(  satId, SATID_NPP )    .eq. 0 ) then
    NCHAN = 22

  else if( strcmp(  satId, SATID_AMSRE )  .eq. 0 ) then
    NCHAN = 12

  else if( strcmp(  satId, SATID_GCOMW1 ) .eq. 0 ) then
    NCHAN = 14

  else if( strcmp(  satId, SATID_FY3RI )  .eq. 0 ) then
    NCHAN = 10

  else if( strcmp(  satId, SATID_TRMM )   .eq. 0 ) then
    NCHAN = 9

  else if( strcmp(  satId, SATID_MTMA )   .eq. 0 ) then
    NCHAN = 9

  else if( strcmp(  satId, SATID_MTSA )   .eq. 0 ) then
    NCHAN = 6
  
  else
    write(*,*) 'satId='//TRIM(satId)
    STOP 'Un-supported satId'
    
  endif
  
  !---- nwp string
  if( isMirs .eq. 1 ) then
    nwpString = 'gdas'
  else if( isMirs .eq. 2 ) then
    nwpString = 'ecmwf'
  else if( isMirs .eq. 3 ) then
    nwpString = 'gfs'
  endif
    

  !---- extract prodStr and put them into prodIds
  DO
    pos2 = INDEX(prodStr(pos1:), ",")
    IF (pos2 == 0) THEN
       n = n + 1
       prodIds(n) = prodStr(pos1:)
       EXIT
    END IF
    n = n + 1
    prodIds(n) = prodStr(pos1:pos1+pos2-2)
    pos1 = pos2 + pos1
  END DO
  
  NPROD = n
  
  !DO i = 1, n
  !  WRITE(*, '(A)' ) TRIM(prodIds(i))
  !END DO

  call ReadList( iu_list, trim(filesList), inputFiles, nfiles, dumFiles, '', '' )
  IF( nfiles .lt. 1 ) CALL ErrHandl( ErrorType, Err_NoFilesFound, '' ) 
  
  MAX_FILE = nfiles
  MAX_PROFILE = 0
  !---- get MAX_FILE and MAX_PROFILE by looping file
  DO ifile = 1, nfiles
    CALL getSceneNprf( inputFiles( ifile ), nprf )
    if( nprf .gt. MAX_PROFILE ) MAX_PROFILE = nprf
  ENDDO
  write(*,*) 'p2pEdr    MAX_FILE = ', MAX_FILE
  write(*,*) 'p2pEdr MAX_PROFILE = ', MAX_PROFILE
  
  !------------------------------------------------------------------------------------
  !     Start loop over cend/product list 
  !------------------------------------------------------------------------------------
  prodLoop: DO iprod = 1, NPROD
  cendLoop: DO icend = 1, NCEND
    
    prodId = TRIM(prodIds(iprod))
    write(*,*)
    write(*,'(A)') 'p2pEdr '//TRIM(prodId)//' '//cends(icend)//' ...'
    
    is1D = 0
    isChan = 0
    isLay = 0
   
    if( strcmp(prodId,'angle')	  .eq. 0 .or. strcmp(prodId,'chisq')   .eq. 0 .or. &
    	strcmp(prodId,'clw')	  .eq. 0 .or. strcmp(prodId,'iwp')     .eq. 0 .or. &		 
        strcmp(prodId,'nattempt') .eq. 0 .or. strcmp(prodId,'niter')   .eq. 0 .or. &		 
    	strcmp(prodId,'psfc')	  .eq. 0 .or. strcmp(prodId,'rr')      .eq. 0 .or. &
        strcmp(prodId,'rwp')	  .eq. 0 .or. strcmp(prodId,'lwp')     .eq. 0 .or. &
        strcmp(prodId,'sfc')      .eq. 0 .or. strcmp(prodId,'scanday') .eq. 0 .or. &
        strcmp(prodId,'scanpos')  .eq. 0 .or. strcmp(prodId,'swe')     .eq. 0 .or. &
        strcmp(prodId,'wspd')     .eq. 0 .or. &
        strcmp(prodId,'tskin')    .eq. 0 .or. strcmp(prodId,'tpw')     .eq. 0 ) then
	
      is1D = 1
      isChan = 0
      isLay = 0
   
      ALLOCATE (var1D_p2p ( MAX_PROFILE, MAX_FILE ) )
      var1D_p2p = -999.0
    
    else if( strcmp( prodId, 'em'  ) .eq. 0 .or. strcmp( prodId, 'tbc' ) .eq. 0 .or. &
             strcmp( prodId, 'tbf' ) .eq. 0 .or. strcmp( prodId, 'tbl' ) .eq. 0 .or. &
             strcmp( prodId, 'tbu' ) .eq. 0 ) then
      
      is1D = 0
      isChan = 1
      isLay = 0
    
      ALLOCATE (varChan(NCHAN))
      varChan(:) = -999.0	     
      ALLOCATE (varChan_p2p( MAX_PROFILE, MAX_FILE, NCHAN ) )	     
      varChan_p2p = -999.0
      
    else if( strcmp( prodId, 'temp'     ) .eq. 0 .or. strcmp( prodId, 'wv'       ) .eq. 0 .or. &
             strcmp( prodId, 'clwp'     ) .eq. 0 .or. strcmp( prodId, 'rainp'    ) .eq. 0 .or. &
             strcmp( prodId, 'graupelp' ) .eq. 0 .or. strcmp( prodId, 'pressure' ) .eq. 0) then

      is1D = 0
      isChan = 0
      isLay = 1
           
      ALLOCATE (varLay(NLAY))
      varLay(:) = -999.0	     
      ALLOCATE (varLay_p2p ( MAX_PROFILE, MAX_FILE, NLAY ) )	     
      varLay_p2p = -999.0

    endif
 
 
    !-----------------------------------------------------
    !     Loop over the EDR/NWP files
    !-----------------------------------------------------
    FilesLoop: DO ifile = 1, MIN(nfiles,MAX_FILE)

      !write( *, '(A)' ) trim( inputFiles( ifile ) )

      !---Read header of EDR file
      CALL ReadHdrScene( iuEDR,inputFiles( ifile ), Scene, nprf )

      !---Loop over the profiles within the file
      ProfilesLoop: DO iprf = 1, nPrf
          CALL ReadScene( iuEDR,Scene, ierr )

          IF( ierr.eq.Warn_EndOfFile )   EXIT  ProfilesLoop
          IF( ierr.eq.Warn_readInvalid ) CYCLE ProfilesLoop
          IF( ierr.ne.0 ) CALL ErrHandl( ErrorType, Err_ReadingFile, '. EDR/NWP file.' )
          
	  cend = Scene%node + 1 
	  IF( cend .ne. icend ) CYCLE ProfilesLoop
	  
	  qc = Scene%qc(1)
	  chisq = Scene%ChiSq
	  psfc = Scene%SfcPress
	  lat = Scene%lat
	  lon = Scene%lon

	  !---- start extracting variables from Scece structure ----------------
	  !---- 16 1-D variables ----
	  if( strcmp( prodId, 'angle' ) .eq. 0 ) then
	    var1D = Scene%Angle
	  else if( strcmp( prodId, 'chisq' ) .eq. 0 ) then
	    var1D = Scene%Chisq
	  else if( strcmp( prodId, 'nattempt' ) .eq. 0 ) then
	    var1D = Scene%nAttempt
	  else if( strcmp( prodId, 'niter' ) .eq. 0 ) then
	    var1D = Scene%nIter
	  else if( strcmp( prodId, 'psfc' ) .eq. 0 ) then
	    var1D = Scene%SfcPress
	  else if( strcmp( prodId, 'sfc' ) .eq. 0 ) then
	    var1D = Scene%iTypSfc
	  else if( strcmp( prodId, 'scanday' ) .eq. 0 ) then
	    var1D = Scene%scanDay + Scene%scanUTC/86400.0
	  else if( strcmp( prodId, 'scanpos' ) .eq. 0 ) then
	    var1D = Scene%iscanPos
	  else if( strcmp( prodId, 'swe' ) .eq. 0 ) then
	    var1D = Scene%SnowDepth
	  else if( strcmp( prodId, 'wspd' ) .eq. 0 ) then
	    var1D = Scene%WindSp
	  else if( strcmp( prodId, 'tskin' ) .eq. 0 ) then
	    var1D = Scene%Tskin

	  !---- 6 derived variables ( supposed to be in DEP for MIRS )----
	  else if( strcmp( prodId, 'clw' ) .eq. 0 ) then
	    var1D = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                                Scene%SfcPress,Scene%Clw(1:Scene%nLay))
	  else if( strcmp( prodId, 'tpw' ) .eq. 0 ) then
	    call ComputeTPW(Scene%Pres_lev(1:Scene%nLev),Scene%SfcPress, &
                            Scene%Absorb_lay(1:Scene%nLay,Scene%iH2o),tpw)
	    var1D = tpw
	  else if( strcmp( prodId, 'iwp' ) .eq. 0 ) then
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
	    var1D = clw + rwp
	  else if( strcmp( prodId, 'rr' ) .eq. 0 ) then
	    var1D = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                                Scene%SfcPress,Scene%Rain(1:Scene%nLay))


	  !---- 5 channel variables ----
	  else if( strcmp( prodId, 'em' ) .eq. 0 ) then
	    varChan(:) = Scene%Emiss(:)
	  else if( strcmp( prodId, 'tbu' ) .eq. 0 ) then
	    varChan(:) = Scene%Ym(:)
	  else if( strcmp( prodId, 'tbc' ) .eq. 0 ) then
	    varChan(:) = Scene%YmCorr(:)
	  else if( strcmp( prodId, 'tbf' ) .eq. 0 ) then
	    varChan(:) = Scene%YFwd(:)
	  else if( strcmp( prodId, 'tbl' ) .eq. 0 ) then
	    clw = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                              Scene%SfcPress,Scene%Clw(1:Scene%nLay))
	    if( clw .lt. MAX_CLD4CLR ) varChan(:) = Scene%YmCorr(:)

	  !---- 6 layer variables ----
	  else if( strcmp( prodId, 'temp' ) .eq. 0 ) then
	    varLay(:) = Scene%Temp_lay(:)
	  else if( strcmp( prodId, 'wv' ) .eq. 0 ) then
	    varLay(:) = Scene%Absorb_lay(:,1)
	  else if( strcmp( prodId, 'clwp' ) .eq. 0 ) then
	    varLay(:) = Scene%clw(:)
	  else if( strcmp( prodId, 'rainp' ) .eq. 0 ) then
	    varLay(:) = Scene%rain(:)
	  else if( strcmp( prodId, 'graupelp' ) .eq. 0 ) then
	    varLay(:) = Scene%graupel(:)
	  else if( strcmp( prodId, 'pressure' ) .eq. 0 ) then
	    varLay(:) = Scene%Pres_lay(:)
	  endif
	  !---- end extracting variables from Scece structure ------------------


	  ok = 0

	  !---- EDR ( isMirs == 0 )  collocated NWP ( isMirs != 0 )
	  if( cend .le. icend  .and. &
	      lon  .ge. lonmin .and. lon .le. lonmax .and. &
	      lat  .ge. latmin .and. lat .le. latmax ) then

	    if( strcmp( prodId, 'sfc'      ) .eq. 0 .or. &
		strcmp( prodId, 'scanpos'  ) .eq. 0 .or. &
		strcmp( prodId, 'tbu'      ) .eq. 0 ) then

		ok = 1

	    else if( ( ( isMirs .eq. 0 .and. qc .lt. 2 ) .or. ( isMirs .ne. 0 .and. qc .eq. 0 ) ) &
		  .and. chisq .le. CHISQ_THRESH ) then

		ok = 1

	    endif

	  endif

          if( ok .eq. 1 ) then

	    if( is1D .eq. 1 ) then
	      var1D_p2p( iprf, ifile ) = var1D
            else if( isChan .eq. 1 ) then
	      varChan_p2p( iprf, ifile, 1:NCHAN ) = varChan( 1:NCHAN )	  
            else if( isLay .eq. 1 ) then
              do ilay = 1, NLAY
        	if( Scene%Pres_lay( ilay ) .lt. psfc ) varLay_p2p( iprf, ifile, ilay ) = varLay( ilay )
              enddo
	    endif

	  endif


      ENDDO ProfilesLoop
      !---Close the EDR file
      CLOSE( iuEDR )
      !---Release memory
      CALL DestroyScene( Scene )

    ENDDO FilesLoop

    !---- start writing out P2P stuff --------------------------------------------------------------
    p2pFile = 'P2P_'//TRIM(satId)//'_'//TRIM(yyyymmdd)//'_'//TRIM(prodId)//'_'//cends(icend)//'.dat'
    if ( isMirs .ge. 1 ) p2pFile='P2P_'//TRIM(satId)//'_'//TRIM(nwpString)//'_'//&
                	 TRIM(yyyymmdd)//'_'//TRIM(prodId)//'_'//cends(icend)//'.dat'
    write(*,'(A)') 'p2p file='//TRIM(p2pPath)//TRIM(p2pFile)
    open( 25,file = TRIM(p2pPath)//TRIM(p2pFile), form = 'unformatted', &
          access = 'direct', recl = 4*MAX_FILE*MAX_PROFILE )

      if( is1D .eq. 1 ) then
	write( 25,rec = 1 ) var1D_p2p( 1:MAX_PROFILE, 1:MAX_FILE )
      else if( isChan .eq. 1 ) then
	do ichan = 1, NCHAN
          write( 25,rec = ichan ) varChan_p2p( 1:MAX_PROFILE, 1:MAX_FILE, ichan )
	enddo
      else if( isLay .eq. 1 ) then
	do ilay = 1, NLAY
          write( 25,rec = ilay ) varLay_p2p( 1:MAX_PROFILE, 1:MAX_FILE, ilay )
	enddo
      endif

    close(25)
    !---- end write out  P2P stuff -----------------------------------------------------------------

    !---- deallocate
    IF( ALLOCATED( varChan )     ) DEALLOCATE( varChan )
    IF( ALLOCATED( varLay )      ) DEALLOCATE( varLay )
    IF( ALLOCATED( var1D_p2p )   ) DEALLOCATE( var1D_p2p )
    IF( ALLOCATED( varChan_p2p ) ) DEALLOCATE( varChan_p2p )
    IF( ALLOCATED( varLay_p2p )  ) DEALLOCATE( varLay_p2p )

  ENDDO cendLoop
  ENDDO prodLoop
  !------------------------------------------------------------------------------------
  !     End loop over cend/product list 
  !------------------------------------------------------------------------------------
  
  !---- output a P2P meta data file if not exist yet ( to be used by p2p_mirs_nwp.pro ) ----
  metaFile = TRIM(p2pPath)//'P2P_'//TRIM(satId)//'_'//TRIM(yyyymmdd)//'_meta.txt'
  !inquire(file=metaFile,exist=metaExist)
  !if( .NOT. metaExist ) then
    open( 25,file = metaFile)
    write(25,*)MAX_FILE
    write(25,*)MAX_PROFILE
    close(25)
  !endif
    
  
  DEALLOCATE( inputFiles )
  DEALLOCATE( dumFiles )

end program p2pEdr

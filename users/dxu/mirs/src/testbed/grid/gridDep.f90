!***************************************************************************************************
!
! To Grid DEP into gridded products.
!
!       Note: gridded IWP is really DEP%GWP
!
!       Wanchun Chen    11/20/2007        Original Coder
!
!***************************************************************************************************

Program gridDep

  USE Consts
  USE misc
  USE IO_DEP
  USE IO_Misc
  USE ErrorHandling
 
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ABS,COS,INT,MOD,TRIM,ALLOCATED
  
  INTEGER            :: iu_list=20,iuDEP,ierr,iprof,nprf
  INTEGER, PARAMETER :: LENF=256
  INTEGER            :: ifile, nfiles
  CHARACTER(LEN=LENF), DIMENSION(:), POINTER :: inputFiles, dumFiles 
  
  INTEGER            :: qc, cend
  
  !---- Structures ----
  TYPE(DEP_type)     :: Dep
  
  INTEGER, PARAMETER :: NPMAX=16
  INTEGER, PARAMETER :: NCEND=2
  INTEGER            :: NCOL, NROW
  INTEGER            :: INT_SATID=SENSOR_ID_N18
  LOGICAL            :: nadirLogic=.FALSE.
  
  !---- product id stuffs
  CHARACTER(LEN=16),DIMENSION(NPMAX) :: prodIds
  CHARACTER(LEN=16)                  :: prodId
  INTEGER                            :: iprod, NPROD=0, icend=0
  INTEGER                            :: pos1 = 1, pos2, n = 0, i
  CHARACTER(LEN=2),DIMENSION(2)      :: cends = (/ 'as', 'ds' /)

  !---- namelist data ----
  CHARACTER(LEN=LENF) :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=16)   :: satId=DEFAULT_VALUE_STR4  ! n18, m2, f16, n19, npp
  CHARACTER(LEN=64)   :: yyyymmdd=DEFAULT_VALUE_STR4
  REAL                :: gridfactor=DEFAULT_VALUE_INT
  CHARACTER(LEN=LENF) :: gridPath=DEFAULT_VALUE_STR4
  REAL                :: latmin=DEFAULT_VALUE_REAL
  REAL                :: latmax=DEFAULT_VALUE_REAL
  REAL                :: lonmin=DEFAULT_VALUE_REAL
  REAL                :: lonmax=DEFAULT_VALUE_REAL
  INTEGER             :: processMode=DEFAULT_VALUE_INT   ! 1(daily), 0(orbit)
  INTEGER             :: fmType=0
  CHARACTER(LEN=256)  :: prodStr ! comma separated product id list (Ex: 'clw,rr,rwp' )
  
  NAMELIST /gridDepNameList/filesList,satId,yyyymmdd,gridfactor,&
            gridPath,latmin,latmax,lonmin,lonmax,processMode,fmType,prodStr
  
  !-------------------------------------------------------------------------------
  !     Output variables identifiers definiton section, commented part from DEP
  !-------------------------------------------------------------------------------
  REAL                                 :: var1D=DEFAULT_VALUE_REAL
  REAL, DIMENSION(:,:),   ALLOCATABLE  :: var1D_grid
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: nadir
  CHARACTER(LEN=LENF)                  :: gridFile
  
  !-----------------------------------------------------
  !     grid and fill in stuff goes here
  !-----------------------------------------------------
  REAL        :: RSAT=833.0
  INTEGER     :: NFOV=30
  REAL        :: SCAN_ANG=3.3
  INTEGER     :: LATLIM_A=12
  REAL        :: FIX=0.5
  REAL        :: loncorr
  INTEGER     :: ifov
  REAL        :: lonleft, lonright
  INTEGER     :: gridlon_left, gridlon_right
  INTEGER     :: gridlat_bot, gridlat_top
  INTEGER     :: lonbox, latbox, near_nadir
  REAL,   DIMENSION(:), ALLOCATABLE :: fov_size

  !-----------------------------------------------------
  !     Execute section begins here
  !-----------------------------------------------------
  
  READ(*,NML=gridDepNameList)

  ! string satId --> integer sat id, fmType --> NFOV
  if      ( strcmp(satId, SATID_F16)    .eq. 0 ) then
     INT_SATID = SENSOR_ID_F16
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
     
  else if ( strcmp(satId, SATID_F17)    .eq. 0 ) then
     INT_SATID = SENSOR_ID_F17
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
     
  else if ( strcmp(satId, SATID_F18)    .eq. 0 ) then
     INT_SATID = SENSOR_ID_F18
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
     
  else if ( strcmp(satId, SATID_N18)    .eq. 0 ) then
     INT_SATID = SENSOR_ID_N18
     if( fmType .eq. 0 ) then
        NFOV=30
        SCAN_ANG=3.3
     else if( fmType .eq. 1 ) then
        NFOV=90
        SCAN_ANG=1.1
     else
        STOP 'Error: Incorrect fmType for N18'
     endif
      
  else if ( strcmp(satId, SATID_N19)    .eq. 0 ) then
     INT_SATID = SENSOR_ID_N19
     if( fmType .eq. 0 ) then
        NFOV=30
        SCAN_ANG=3.3
     else if( fmType .eq. 1 ) then
        NFOV=90
        SCAN_ANG=1.1
     else
        STOP 'Error: Incorrect fmType for N19'
     endif
     
  else if ( strcmp(satId, SATID_METOPA) .eq. 0 ) then
     INT_SATID = SENSOR_ID_METOPA
     if( fmType .eq. 0 ) then
        NFOV=30
        SCAN_ANG=3.3
     else if( fmType .eq. 1 ) then
        NFOV=90
        SCAN_ANG=1.1
     else
        STOP 'Error: Incorrect fmType for metopA'
     endif
     
  else if ( strcmp(satId, SATID_METOPB) .eq. 0 ) then
     INT_SATID = SENSOR_ID_METOPB
     if( fmType .eq. 0 ) then
        NFOV=30
        SCAN_ANG=3.3
     else if( fmType .eq. 1 ) then
        NFOV=90
        SCAN_ANG=1.1
     else
        STOP 'Error: Incorrect fmType for metopB'
     endif
     
  else if ( strcmp(satId, SATID_TRMM)  .eq. 0 ) then
     INT_SATID = SENSOR_ID_TRMM
     if( fmType .eq. -1 ) then
        NFOV=26
     else if( fmType .eq. 0 ) then
        NFOV=104
     else if( fmType .eq. 1 ) then
        NFOV=208
     else
        STOP 'Error: Incorrect fmType for TRMM TMI'
     endif

  else if ( strcmp(satId, SATID_TRMM2A12)  .eq. 0 ) then
     INT_SATID = SENSOR_ID_TRMM2A12
     if( fmType .eq. -1 ) then
        NFOV=26
     else if( fmType .eq. 0 ) then
        NFOV=104
     else if( fmType .eq. 1 ) then
        NFOV=208
     else
        STOP 'Error: Incorrect fmType for TRMM_2A12'
     endif
     
  else if ( strcmp(satId, SATID_NPP)    .eq. 0 ) then
     INT_SATID = SENSOR_ID_NPP
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
     
  else if ( strcmp(satId, SATID_AMSRE)  .eq. 0 ) then
     INT_SATID = SENSOR_ID_AMSRE
     NFOV=191
     
  else if( strcmp(satId, SATID_GCOMW1)  .eq. 0 ) then
     INT_SATID = SENSOR_ID_GCOMW1
     if( fmType .eq. -1 ) then
        NFOV=27
     else if( fmType .eq. 0 ) then
        NFOV=243
     else if( fmType .eq. 1 ) then
        NFOV=486
     else
        STOP 'Error: Incorrect fmType for GCOMW1 AMSR2'
     endif
     
  else if ( strcmp(satId, SATID_FY3RI)  .eq. 0 ) then
     INT_SATID = SENSOR_ID_FY3RI
     NFOV=120

! For now, MT MADRAS proxy data uses same params as TRMM_TMI
! This will need to be changed when real data are processed
  else if( strcmp(satId, SATID_MTMA)  .eq. 0 ) then
     INT_SATID = SENSOR_ID_MTMA
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
     write(*,'(A)')'satId='//satId
     STOP 'Error: unsupported satId'
     
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


  call ReadList(iu_list, trim(filesList), inputFiles, nfiles, dumFiles, '', '')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 
  

  !-----------------------------------------------------
  !     Allocate output array and initialize them
  !-----------------------------------------------------
  NCOL=int(360*gridfactor)
  NROW=int(180*gridfactor)

  !------------------------------------------------------------------------------------
  !     Start loop over product and cend 
  !------------------------------------------------------------------------------------
  prodLoop: DO iprod = 1, NPROD
  cendLoop: DO icend = 1, NCEND
    
     prodId = TRIM(prodIds(iprod))
     write(*,*)
     write(*,'(A)') 'grid '//TRIM(prodId)//' '//cends(icend)//' ......'
     
     ALLOCATE (nadir ( 0:NCOL-1, 0:NROW-1 ) )  
     nadir = NFOV
     
     ALLOCATE (var1D_grid ( 0:NCOL-1, 0:NROW-1 ) )
     var1D_grid = -999.0
     
     !---- get fov size ----
     if(.NOT.ALLOCATED(fov_size)) ALLOCATE(fov_size(0:NFOV-1))
     
     if( INT_SATID .eq. SENSOR_ID_N18 .or. INT_SATID .eq. SENSOR_ID_METOPA .or. &
          INT_SATID .eq. SENSOR_ID_N19 .or. INT_SATID .eq. SENSOR_ID_METOPB .or. &
          INT_SATID .eq. SENSOR_ID_NPP ) THEN
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
     
  
    !-----------------------------------------------------
    !     Loop over the DEP files
    !-----------------------------------------------------
     FilesLoop: DO ifile=1,nfiles

      !write(*,'(A)') trim(inputFiles(ifile))

      !---- Read header of DEP file ----
        CALL ReadHdrDEP(iuDEP,inputFiles(ifile),Dep,nprf)

      !---Loop over the profiles within the file
        ProfilesLoop: DO iprof=1,nPrf

           CALL ReadDep(iuDEP,Dep,ierr)
           
           IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfilesLoop
           IF (ierr.eq.Warn_readInvalid) CYCLE ProfilesLoop
           
           cend = Dep%node + 1 
           IF( cend .ne. icend ) CYCLE ProfilesLoop
           
           loncorr=abs(1/cos(PI*Dep%lat/180.0))
           IF ( loncorr .gt. 200 ) loncorr=200
           ifov = MOD( iprof, NFOV )
           
           IF( INT_SATID .eq. SENSOR_ID_N18 .or. INT_SATID .eq. SENSOR_ID_METOPA .or. &
                INT_SATID .eq. SENSOR_ID_N19 .or. INT_SATID .eq. SENSOR_ID_METOPB .or. &
                INT_SATID .eq. SENSOR_ID_NPP ) THEN
              
              lonleft  = Dep%lon - 0.5 * fov_size(ifov) * loncorr
              lonright = Dep%lon + 0.5 * fov_size(ifov) * loncorr
              gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
              gridlon_right = INT((lonright + 180.0) * gridfactor + fix)
              
              if( fmType .eq. 0 ) then
                 if ( abs(ifov-(NFOV-1.)/2.) .lt. (LATLIM_A - 0.4 )) then
                    gridlat_bot = INT((Dep%lat+90) * gridfactor)
                    gridlat_top = gridlat_bot + 1 
                 else 
                    gridlat_bot = INT((Dep%lat+90.0) * gridfactor - 1 + fix)
                    gridlat_top = INT((Dep%lat+90.0) * gridfactor + 1 + fix)
                 endif
              else
                 gridlat_bot = INT((Dep%lat+90) * gridfactor) 
                 gridlat_top = INT((Dep%lat+90) * gridfactor) 
              endif
              
           ELSE IF ( INT_SATID .eq. SENSOR_ID_F16 .or. INT_SATID .eq. SENSOR_ID_F18 .or. &
	              INT_SATID .eq. SENSOR_ID_F17 ) THEN
            
              lonleft  = Dep%lon - 0.5 * fov_size(ifov) * loncorr
              lonright = Dep%lon + 0.5 * fov_size(ifov) * loncorr
              gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
              gridlon_right = INT((lonright + 180.0) * gridfactor + fix)
              
              gridlat_bot = INT((Dep%lat+90.0) * gridfactor - 1 + fix)
              gridlat_top = INT((Dep%lat+90.0) * gridfactor + 1 + fix)
           ELSE IF ( INT_SATID .eq. SENSOR_ID_AMSRE .or. INT_SATID .eq. SENSOR_ID_GCOMW1 ) THEN
              
              lonleft  = Dep%lon - 0.5 * fov_size(ifov) * loncorr
              lonright = Dep%lon + 0.5 * fov_size(ifov) * loncorr
              gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
              gridlon_right = INT((lonright + 180.0) * gridfactor + fix)
              
              gridlat_bot = INT((Dep%lat+90.0) * gridfactor - 1 + fix)
              gridlat_top = INT((Dep%lat+90.0) * gridfactor + 1 + fix)
              !--- For now do same filling for MT SAPHIR as for N18, but may need to change with real data
           ELSE if( INT_SATID .eq. SENSOR_ID_MTSA ) THEN
              
              lonleft  = Dep%lon - 0.5 * fov_size(ifov) * loncorr
              lonright = Dep%lon + 0.5 * fov_size(ifov) * loncorr
              gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
              gridlon_right = INT((lonright + 180.0) * gridfactor + fix)
              
              if( fmType .eq. 0 ) then
                 if( abs(ifov-(NFOV-1.)/2.) .lt. (LATLIM_A - 0.4 )) then
                    gridlat_bot = INT((Dep%lat+90) * gridfactor)
                    gridlat_top = gridlat_bot + 1 
                 else 
                    gridlat_bot = INT((Dep%lat+90.0) * gridfactor - 1 + fix)
                    gridlat_top = INT((Dep%lat+90.0) * gridfactor + 1 + fix)
                 endif
              else
                 gridlat_bot = INT((Dep%lat+90) * gridfactor) 
                 gridlat_top = INT((Dep%lat+90) * gridfactor) 
              endif
              
           ELSE  ! NO FILLING

              gridlon_left  = INT((Dep%lon + 180.0) * gridfactor + fix)
              gridlon_right = INT((Dep%lon + 180.0) * gridfactor + fix)
              gridlat_bot   = INT((Dep%lat + 90.0 ) * gridfactor + fix)
              gridlat_top   = INT((Dep%lat + 90.0 ) * gridfactor + fix)
              
           ENDIF
           
           if ( gridlon_left  .lt. 0    ) gridlon_left=0
           if ( gridlon_left  .ge. NCOL ) gridlon_left=NCOL-1
           if ( gridlon_right .lt. 0    ) gridlon_right=0
           if ( gridlon_right .ge. NCOL ) gridlon_right=NCOL-1
           
           if ( gridlat_bot   .lt. 0    ) gridlat_bot=0
           if ( gridlat_top   .lt. 0    ) gridlat_top=0
           if ( gridlat_top   .ge. NROW ) gridlat_top=NROW-1
           if ( gridlat_bot   .ge. NROW ) gridlat_bot=NROW-1
           
           qc = Dep%qc(1)

	  !---- extract variables from Dep structure ----
           if( strcmp(prodId,'sfc2') .eq. 0 ) then
              var1D = Dep%iTypSfc
           else if( strcmp(prodId,'tpw') .eq. 0 ) then
              var1D = Dep%tpw
           else if( strcmp(prodId,'clw') .eq. 0 ) then
              var1D = Dep%clw
           else if( strcmp(prodId,'sice') .eq. 0 ) then
              var1D = Dep%sic
           else if( strcmp(prodId,'sicefy') .eq. 0 ) then
              var1D = Dep%sic_fy
           else if( strcmp(prodId,'sicemy') .eq. 0 ) then
              var1D = Dep%sic_my
           else if( strcmp(prodId,'swe') .eq. 0 ) then
              var1D = Dep%swe
           else if( strcmp(prodId,'snow') .eq. 0 ) then
              var1D = Dep%SnowCover
           else if( strcmp(prodId,'gs') .eq. 0 ) then
              var1D = Dep%SnowGS
           else if( strcmp(prodId,'iwp') .eq. 0 ) then
              var1D = Dep%GWP
           else if( strcmp(prodId,'rwp') .eq. 0 ) then
              var1D = Dep%RWP
           else if( strcmp(prodId,'lwp') .eq. 0 ) then
              var1D = Dep%LWP
           else if( strcmp(prodId,'rr') .eq. 0 ) then
              var1D = Dep%RR
           else if( strcmp(prodId,'wspd') .eq. 0 ) then
              var1D = Dep%WindSp
           endif
           
           near_nadir = INT(abs(ifov - (NFOV-1.)/2.) + 0.6)
           
           do latbox=gridlat_bot,  gridlat_top
              do lonbox=gridlon_left, gridlon_right
                 
                 nadirLogic = near_nadir .le. nadir(lonbox,latbox)
                 if( INT_SATID .eq. SENSOR_ID_AMSRE .or. INT_SATID .eq. SENSOR_ID_FY3RI .or. & 
                      INT_SATID .eq. SENSOR_ID_TRMM  .or. INT_SATID .eq. SENSOR_ID_GPM   .or. &
                      INT_SATID .eq. SENSOR_ID_TRMM2A12 ) nadirLogic = .TRUE.
                 
                 if ( cend .ge. 1 .and. nadirLogic .and. qc .le. 2 ) then
                    nadir(lonbox,latbox)      = near_nadir
                    var1D_grid(lonbox,latbox) = var1D
		  
		  !---- snow or ice impacts some parameters, we put -888.0 there
                    if ( ( strcmp(prodId,'iwp') .eq. 0 .or. strcmp(prodId,'rwp') .eq. 0   .or.  &
                         strcmp(prodId,'lwp') .eq. 0 .or. strcmp(prodId,'rr' ) .eq. 0 ) .and. &
                         ( Dep%iTypSfc .eq. 1  .or. Dep%iTypSfc .eq. 3 ) ) then
                       var1D_grid(lonbox,latbox)  = -888.0                   		      
                    endif
                    
                 endif
                 
                 if ( cend .ge. 1 .and. nadirLogic .and. qc .eq. 2 ) then
                    var1D_grid(lonbox,latbox)  = -99.0
                 endif
                 
              enddo
           enddo
           
        ENDDO ProfilesLoop

      !---Close the DEP file
        CLOSE (iuDEP)
        
     ENDDO FilesLoop

    !---- write out the p2p data
     gridFile='GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prodId)//'_'//cends(icend)//'.dat'
     write(*,'(A)') 'grid file='//trim(gridPath)//trim(gridFile)
     open(25,file=trim(gridPath)//trim(gridFile),form='unformatted',access='direct',recl=4*NCOL*NROW)
     write(25,rec=1)var1D_grid(0:NCOL-1, 0:NROW-1)
     close(25)
 
    !---- Release memory ----
     IF( ALLOCATED(nadir)      ) DEALLOCATE( nadir )
     IF( ALLOCATED(var1D_grid) ) DEALLOCATE( var1D_grid )
     IF( ALLOCATED(fov_size)   ) DEALLOCATE( fov_size )
     
  ENDDO cendLoop
  ENDDO prodLoop
  !------------------------------------------------------------------------------------
  !     End loop over product and cend
  !------------------------------------------------------------------------------------
  
  !---- Release memory ----
  DEALLOCATE(inputFiles)
  DEALLOCATE(dumFiles)
  

end program gridDep

!***************************************************************************************************
!  To Grid Profiles into gridded data set. ( NCOL, NROW, NLAY )
!
!  For AMSR-E/FY3 MWRI/TRMM2A12, NO filling needed. 
!  The reason to grid each product is because of memory concern.  
!  
!  Limitation:  Only for low resolution at this moment( fov_A )
!
!  Wanchun Chen      03/25/2009          Original code
!  Wanchun Chen      09/30/2009          Add TRMM_2A12 branch
!  Wanchun Chen      11/20/2009          Add TRMM_2A12 scanday gridding.
!
!***************************************************************************************************

Program gridVertical

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
  INTEGER, PARAMETER :: len=256,nqc=4
  INTEGER            :: ifile, nfiles
  CHARACTER(LEN=len), DIMENSION(:), POINTER :: inputFiles, dumFiles 
  
  !----Structures
  TYPE(Scene_type)   :: Scene
  
  INTEGER, PARAMETER :: NLAY=100, NPROD=4, NCEND=2
  INTEGER            :: INT_SATID=SENSOR_ID_N18
  INTEGER            :: NCOL, NROW, ilay, icend, iprod  !, ichan
  LOGICAL            :: nadirLogic=.FALSE.
  INTEGER            :: NLAY_PROD=100
  
  !---- product id's used
  character(LEN=8),dimension(NPROD) :: prods = (/ 'clwp    ', 'rainp   ', 'graupelp', 'pressure' /)
  character(LEN=2),dimension(NCEND) :: cends = (/ 'as', 'ds'/)
  real, dimension(NLAY)             :: values = -999.0 ! to save time, we use static array here to avoid dynamic memory
  character(LEN=8)                  :: prod
  character(LEN=2)                  :: cend
  
  !-------------------------------------------------------------------------------
  !     Output variables identifiers definiton section, commented part from DEP
  !-------------------------------------------------------------------------------
  REAL, DIMENSION(:,:,:),   ALLOCATABLE  :: psfc_grid
  REAL, DIMENSION(:,:,:),   ALLOCATABLE  :: jday_grid
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE  :: prod_grid
  
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: nadirs
  CHARACTER(LEN=len)                     :: gridFile
  
  INTEGER     :: FILLING=1  ! default is to fill in, but TRMM_2a12 no filling.
  !-----------------------------------------------------
  !     grid and filling variables
  !-----------------------------------------------------
  INTEGER     :: NUMSPOT_A=30
  INTEGER     :: LATLIM_A=12
  REAL        :: FIX=0.5
  REAL        :: loncorr
  INTEGER     :: ifov
  REAL        :: lonleft, lonright
  INTEGER     :: gridlon_left, gridlon_right
  INTEGER     :: gridlat_bot, gridlat_top
  INTEGER     :: lonbox, latbox, near_nadir
  REAL, DIMENSION(:), ALLOCATABLE :: fov_size_A


  !----namelist data
  CHARACTER(LEN=len) :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=16)  :: satId=DEFAULT_VALUE_STR4  ! string type satellite id: n18, m2, f16, f18, trmm_2a12 etc
  CHARACTER(LEN=64)  :: yyyymmdd=DEFAULT_VALUE_STR4
  REAL               :: gridfactor=DEFAULT_VALUE_INT
  CHARACTER(LEN=len) :: gridPath=DEFAULT_VALUE_STR4
  REAL               :: latmin=DEFAULT_VALUE_REAL
  REAL               :: latmax=DEFAULT_VALUE_REAL
  REAL               :: lonmin=DEFAULT_VALUE_REAL
  REAL               :: lonmax=DEFAULT_VALUE_REAL
  INTEGER            :: isMirs=DEFAULT_VALUE_INT      ! 0(MIRS EDR), 1(gdas), 2(ecmwf), 3(gfs), 4(trmm_2a12)
  INTEGER            :: processMode=DEFAULT_VALUE_INT ! 1 - daily   0 - orbit
  
  NAMELIST /GridNameList/filesList,satId,yyyymmdd,gridfactor,gridPath,latmin,latmax,lonmin,lonmax,isMirs,processMode
  
  !-----------------------------------------------------
  !     Execute section begins here
  !-----------------------------------------------------
  READ(*,NML=GridNameList)
  
  ! string type satellite id --> integer type satellite id
  if      ( strcmp(satId, SATID_F16)   .eq. 0 ) then
      INT_SATID = SENSOR_ID_F16
  else if( strcmp(satId, SATID_F17)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F17
  else if( strcmp(satId, SATID_F18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F18
  else if( strcmp(satId, SATID_N18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N18
  else if( strcmp(satId, SATID_N19)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N19
  else if( strcmp(satId, SATID_NPP)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_NPP
      NUMSPOT_A = 32
  else if( strcmp(satId, SATID_METOPA) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPA
  else if( strcmp(satId, SATID_METOPB) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPB
  else if( strcmp(satId, SATID_AMSRE)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_AMSRE
      NUMSPOT_A = 191
  else if( strcmp(satId, SATID_GCOMW1) .eq. 0 ) then
      INT_SATID = SENSOR_ID_GCOMW1
      NUMSPOT_A = 243
  else if( strcmp(satId, SATID_FY3RI)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_FY3RI
      NUMSPOT_A = 120
  else if( strcmp(satId, SATID_TRMM)   .eq. 0 ) then
      INT_SATID = SENSOR_ID_TRMM
      NUMSPOT_A = 26
  else
      INT_SATID = -999 
  endif
 
  if(.NOT.ALLOCATED(fov_size_A)) ALLOCATE(fov_size_A(0:NUMSPOT_A-1))

  call ReadList(iu_list, trim(filesList), inputFiles, nfiles, dumFiles, '', '')
  if(nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 
  
  !-----------------------------------------------------
  !     Allocate output array and initialize them
  !-----------------------------------------------------
  NCOL=int(360*gridfactor)
  NROW=int(180*gridfactor)

  !-----------------------------------------------------
  !     Compute FOV_A size for filling purpose
  !-----------------------------------------------------
  call FOV_A(fov_size_A,NUMSPOT_A,INT_SATID)
  
  ! to handle F16/F17/F18 SSMIS differently ( DMSP is fixed ) 
  if( INT_SATID .eq. SENSOR_ID_F16 .or. INT_SATID .eq. SENSOR_ID_F18 ) fov_size_A = 0.6783 
  if( INT_SATID .eq. SENSOR_ID_F17 ) fov_size_A = 0.6783 
  ! this TRMM need further computation
  if(INT_SATID .eq. SENSOR_ID_TRMM ) fov_size_A(:) = 0.6745
  
  ALLOCATE (nadirs ( 0:NCOL-1, 0:NROW-1, NCEND ) )  

  !---- skip gdas(1) and gfs(3), only do mirs(0),ecmwf(2) and trmm_2a12(4)   
  IF( isMirs .eq. 0 .or. isMirs .eq. 2 .or. isMirs .eq. 4 ) THEN  !---- IF block over isMirs start

    ALLOCATE (psfc_grid( 0:NCOL-1, 0:NROW-1, NCEND ) )
    ALLOCATE (jday_grid( 0:NCOL-1, 0:NROW-1, NCEND ) )  

    psfc_grid = -999.0
    jday_grid = -999.0

    DO iprod = 1, NPROD !---- loop over products start

      prod = prods(iprod)

      ALLOCATE (prod_grid( 0:NCOL-1, 0:NROW-1, 1:NLAY, NCEND) )
      prod_grid = -999.0

      nadirs = NUMSPOT_A

      !-----------------------------------------------------
      !     Loop over the EDR files
      !-----------------------------------------------------
      FilesLoop: DO ifile=1,nfiles

        write(*,'(A)') trim(inputFiles(ifile))

        !---Read header of EDR file
        CALL ReadHdrScene(iuEDR,inputFiles(ifile),Scene,nprf)

        !---Loop over the profiles within the file
        ProFilesLoop: DO iprof=1,nPrf
            CALL ReadScene(iuEDR,Scene,ierr)

            if(ierr.eq.Warn_EndOfFile)   EXIT  ProFilesLoop
            if(ierr.eq.Warn_readInvalid) CYCLE ProFilesLoop
            if(ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. EDR/NWP file.')

            loncorr=abs(1/cos(PI*Scene%lat/180.0))
            if( loncorr .gt. 200 ) loncorr=200
            ifov = MOD( iprof, NUMSPOT_A )

            IF( INT_SATID .eq. SENSOR_ID_N18 .or. INT_SATID .eq. SENSOR_ID_METOPA .or. & 
                INT_SATID .eq. SENSOR_ID_N19 .or. INT_SATID .eq. SENSOR_ID_METOPB .or. &
                INT_SATID .eq. SENSOR_ID_NPP ) THEN

              lonleft  = Scene%lon - 0.5 * fov_size_A(ifov) * loncorr
              lonright = Scene%lon + 0.5 * fov_size_A(ifov) * loncorr
              gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
              gridlon_right = INT((lonright + 180.0) * gridfactor + fix)

              if( abs(ifov-(NUMSPOT_A-1.)/2.) .lt. (LATLIM_A - 0.4 )) then
                  gridlat_bot = INT((Scene%lat+90) * gridfactor)
                  gridlat_top = gridlat_bot + 1 
              else 
                  gridlat_bot = INT((Scene%lat+90.0) * gridfactor - 1 + fix)
                  gridlat_top = INT((Scene%lat+90.0) * gridfactor + 1 + fix)
              endif

              FILLING = 1

            ELSE IF( INT_SATID .eq. SENSOR_ID_F16 .or. INT_SATID .eq. SENSOR_ID_F18 .or. &
	              INT_SATID .eq. SENSOR_ID_F17 ) THEN

              lonleft  = Scene%lon - 0.5 * fov_size_A(ifov) * loncorr
              lonright = Scene%lon + 0.5 * fov_size_A(ifov) * loncorr
              gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
              gridlon_right = INT((lonright + 180.0) * gridfactor + fix)

              gridlat_bot = INT((Scene%lat+90.0) * gridfactor - 1 + fix)
              gridlat_top = INT((Scene%lat+90.0) * gridfactor + 1 + fix)

              FILLING = 1

            ELSE  ! NO FILLING ( TRMM_2A12 NO FILLING )

              gridlon_left  = INT((Scene%lon + 180.0) * gridfactor + fix)
              gridlon_right = INT((Scene%lon + 180.0) * gridfactor + fix)
              gridlat_bot   = INT((Scene%lat +  90.0) * gridfactor)
              gridlat_top   = INT((Scene%lat +  90.0) * gridfactor)

              FILLING = 0

            ENDIF

            if( gridlon_left  .lt. 0    ) gridlon_left=0
            if( gridlon_left  .ge. NCOL ) gridlon_left=NCOL-1
            if( gridlon_right .lt. 0    ) gridlon_right=0
            if( gridlon_right .ge. NCOL ) gridlon_right=NCOL-1

            if( gridlat_bot   .lt. 0    ) gridlat_bot=0
            if( gridlat_top   .lt. 0    ) gridlat_top=0
            if( gridlat_top   .ge. NROW ) gridlat_top=NROW-1
            if( gridlat_bot   .ge. NROW ) gridlat_bot=NROW-1

            if(      iprod .eq. 1 ) then
              NLAY_PROD = Scene%nParmCLW
              values(:) = Scene%clw(:)
            else if( iprod .eq. 2 ) then
              NLAY_PROD = Scene%nParmRain
              values(:) = Scene%rain(:)
            else if( iprod .eq. 3 ) then
              NLAY_PROD = Scene%nParmGrpl
              values(:) = Scene%graupel(:)
            else if( iprod .eq. 4 ) then
              NLAY_PROD = Scene%nParmGrpl
              values(:) = Scene%Pres_lay(:)
            endif

            IF( FILLING .EQ. 1 ) THEN

              near_nadir = INT(abs(ifov - (NUMSPOT_A-1.)/2.) + 0.6)

              do latbox=gridlat_bot,  gridlat_top
              do lonbox=gridlon_left, gridlon_right

                  icend = Scene%node+1
                  if( icend .eq. 1 .or. icend .eq. 2 ) then

                    nadirLogic = near_nadir .le. nadirs(lonbox,latbox,icend)
                    if( INT_SATID .eq. SENSOR_ID_AMSRE  .or. &
                        INT_SATID .eq. SENSOR_ID_GCOMW1 .or. &
                        INT_SATID .eq. SENSOR_ID_FY3RI ) nadirLogic = .TRUE.

                    if( nadirLogic .and. Scene%qc(1) .le. 2 ) then
                      nadirs(lonbox,latbox,icend) = near_nadir
                      do ilay=1,NLAY_PROD
                        if( Scene%Pres_lay(ilay) .lt. Scene%SfcPress ) then
                            prod_grid(lonbox,latbox,ilay,icend) = values(ilay)
                        endif
                      enddo
                    endif

                    !---- MIRS sensors qc=2 means fail, we put -99.0
                    if( nadirLogic .and. Scene%qc(1) .eq. 2 .and. isMirs .eq. 0 ) then
                      do ilay=1,NLAY_PROD
                        if( Scene%Pres_lay(ilay) .lt. Scene%SfcPress ) then
                            prod_grid(lonbox,latbox,ilay,icend) = -99.0 
                        endif
                      enddo
                    endif

                    !---- non-MIRS sensors qc!=0 means fail, we put -999.0
                    if( nadirLogic .and. Scene%qc(1) .ne. 0 .and. isMirs .ne. 0 ) then
                      do ilay=1,NLAY_PROD
                        if( Scene%Pres_lay(ilay) .lt. Scene%SfcPress ) then
                            prod_grid(lonbox,latbox,ilay,icend) = -990.0 
                        endif
                      enddo
                    endif

                  endif

              enddo
              enddo 

            ELSE  ! NO FILLING ( TRMM_2A12 has no filling, so we only add psfc and jday in this NO filling block )

                  icend = Scene%node+1
                  if( icend .eq. 1 .or. icend .eq. 2 ) then

                    if( Scene%qc(1) .le. 2 ) then
                      if( iprod .eq. 1 ) then !---- psfc and jday only need to once
                        psfc_grid(gridlon_left,gridlat_bot,icend) = Scene%SfcPress
                        jday_grid(gridlon_left,gridlat_bot,icend) = Scene%scanDay + Scene%scanUTC/86400.0
                      endif
                      do ilay=1,NLAY_PROD
                        if( Scene%Pres_lay(ilay) .lt. Scene%SfcPress ) then
                            prod_grid(gridlon_left,gridlat_bot,ilay,icend) = values(ilay)
                        endif
                      enddo
                    endif

                    !---- MIRS sensors qc=2 means failing, we put -99.0
                    if( Scene%qc(1) .eq. 2 .and. isMirs .eq. 0 ) then
                      psfc_grid(gridlon_left,gridlat_bot,icend) = -99.0
                      jday_grid(gridlon_left,gridlat_bot,icend) = -99.0
                      do ilay=1,NLAY_PROD
                        if( Scene%Pres_lay(ilay) .lt. Scene%SfcPress ) then
                            prod_grid(gridlon_left,gridlat_bot,ilay,icend) = -99.0 
                        endif
                      enddo
                    endif

                    !---- non-MIRS sensors qc != 0 means failing, we put -999.0 (missing)
                    if( Scene%qc(1) .ne. 0 .and. isMirs .ne. 0 ) then
                      psfc_grid(gridlon_left,gridlat_bot,icend) = -999.0
                      jday_grid(gridlon_left,gridlat_bot,icend) = -999.0
                      do ilay=1,NLAY_PROD
                        if( Scene%Pres_lay(ilay) .lt. Scene%SfcPress ) then
                            prod_grid(gridlon_left,gridlat_bot,ilay,icend) = -999.0 
                        endif
                      enddo
                    endif

                  endif

            ENDIF  ! END IF OF FILING BRANCH

        ENDDO ProFilesLoop

        !---Close the EDR file
        CLOSE (iuEDR)
        !---Release memory allocated to Scene
        CALL DestroyScene(Scene)
      ENDDO FilesLoop

      !---- write out ----
      do icend = 1, NCEND
        cend=cends(icend)
        gridFile='GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prod)//'_'//cend//'.dat'
        if( isMirs .eq. 1 ) gridFile='GRID_'//trim(satId)//'_gdas_'//trim(yyyymmdd)//'_'//trim(prod)//'_'//cend//'.dat'
        if( isMirs .eq. 2 ) gridFile='GRID_'//trim(satId)//'_ecmwf_'//trim(yyyymmdd)//'_'//trim(prod)//'_'//cend//'.dat'
        if( isMirs .eq. 3 ) gridFile='GRID_'//trim(satId)//'_gfs_'//trim(yyyymmdd)//'_'//trim(prod)//'_'//cend//'.dat'
        open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
        do ilay=1,NLAY
          write(25,rec=ilay)prod_grid(0:NCOL-1, 0:NROW-1, ilay, icend)
        enddo
        close(25)
      enddo

      DEALLOCATE(prod_grid)

    ENDDO !---- loop over products end

    !---- TRMM_2A12, we output scanday and psfc
    IF( isMirs .eq. 4 ) THEN 

      do icend = 1, NCEND
        cend=cends(icend)
        gridFile='GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_psfc_'//cend//'.dat'
        open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
        write(25,rec=1)psfc_grid(0:NCOL-1, 0:NROW-1,icend)
        close(25)
      enddo

      do icend = 1, NCEND
        cend=cends(icend)
        gridFile='GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_scanday_'//cend//'.dat'
        open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
        write(25,rec=1)jday_grid(0:NCOL-1, 0:NROW-1,icend)
        close(25)
      enddo

    ENDIF

    DEALLOCATE(psfc_grid)
    DEALLOCATE(jday_grid)

  ENDIF  !---- IF block over isMirs end

  !--- Release memory
  DEALLOCATE(inputFiles)
  DEALLOCATE(dumFiles)
  DEALLOCATE(fov_size_A)
  DEALLOCATE(nadirs)  
  
end program gridVertical

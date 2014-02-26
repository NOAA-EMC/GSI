!***************************************************************************************************
!
! To Generate DEP composite products(pentad/weekly/monthly)
!
!       Note: gridded IWP is really DEP%GWP
!
!       Wanchun Chen
!       11/24/2008
!
!***************************************************************************************************

Program GridDepClimate

  USE Consts
  USE misc
  USE IO_DEP
  USE IO_Misc
  USE ErrorHandling
 
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ABS,INT,TRIM
  
  INTEGER            :: iu_list=20,iuDEP,ierr,iprf,nprf
  INTEGER, PARAMETER :: len=250
  CHARACTER(LEN=len) :: file
  CHARACTER(LEN=len) :: gridFile
  
  !---Structures
  TYPE(DEP_type)     :: Dep
  
  INTEGER            :: NCOL, NROW
  INTEGER            :: INT_SATID=SENSOR_ID_N18
  REAL               :: fix=0.5
  INTEGER            :: icol,irow
  
  CHARACTER(LEN=21)  :: timeSuffixStr=''
  CHARACTER(LEN=7)   :: gridFactorStr=''
  
  !----namelist data
  CHARACTER(LEN=len) :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=8)   :: satId=DEFAULT_VALUE_STR4  ! n18, m2, f16
  CHARACTER(LEN=10)  :: yyyymmdd_start='yyyy-mm-dd'
  CHARACTER(LEN=10)  :: yyyymmdd_end='yyyy-mm-dd'
  REAL               :: gridfactor=DEFAULT_VALUE_REAL
  CHARACTER(LEN=len) :: gridPath=DEFAULT_VALUE_STR4
  INTEGER            :: climateType=DEFAULT_VALUE_INT  ! 0(pentad), 1(weekly), 2(monthly)
  
  NAMELIST /GridNameList/filesList,satId,yyyymmdd_start,yyyymmdd_end,gridfactor,gridPath,climateType
  
  !-------------------------------------------------------------------------------
  !     Output variables identifiers definiton section
  !-------------------------------------------------------------------------------
  REAL, DIMENSION(:,:), ALLOCATABLE    :: clw_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: tpw_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: sice_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: swe_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: gs_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: sicemy_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: sicefy_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: iwp_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: rwp_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: lwp_grid
  REAL, DIMENSION(:,:), ALLOCATABLE    :: rr_grid

  REAL, DIMENSION(:,:), ALLOCATABLE    :: sfc_grid

  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: clw_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: tpw_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: sice_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: swe_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: gs_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: sicemy_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: sicefy_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: iwp_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: rwp_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: lwp_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: rr_grid_num

  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: sfc_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: sfc0_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: sfc1_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: sfc2_grid_num
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: sfc3_grid_num
    


  !-----------------------------------------------------
  !     Execute section begins here
  !-----------------------------------------------------
  
  READ(*,NML=GridNameList)
  
  ! string satId --> integer sat id
  if      ( strcmp(satId, SATID_F16)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F16
  else if ( strcmp(satId, SATID_F17)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F17
  else if ( strcmp(satId, SATID_F18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F18
  else if ( strcmp(satId, SATID_N18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N18
  else if ( strcmp(satId, SATID_N19)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N19
  else if ( strcmp(satId, SATID_NPP)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_NPP
  else if ( strcmp(satId, SATID_METOPA) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPA
  else if ( strcmp(satId, SATID_METOPB) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPB
  else if ( strcmp(satId, SATID_AMSRE)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_AMSRE
  else if ( strcmp(satId, SATID_GCOMW1) .eq. 0 ) then
      INT_SATID = SENSOR_ID_GCOMW1
  else if ( strcmp(satId, SATID_TRMM)   .eq. 0 ) then
      INT_SATID = SENSOR_ID_TRMM
  else if ( strcmp(satId, SATID_MTMA)   .eq. 0 ) then
      INT_SATID = SENSOR_ID_MTMA
  else if ( strcmp(satId, SATID_MTSA)   .eq. 0 ) then
      INT_SATID = SENSOR_ID_MTSA
  endif
  
  !-----------------------------------------------------
  !     Allocate output array and initialize them
  !-----------------------------------------------------
  NCOL=int(360*gridfactor)
  NROW=int(180*gridfactor)

  ALLOCATE (clw_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (tpw_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (sice_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (swe_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (gs_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (sicemy_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (sicefy_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (iwp_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (rwp_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (lwp_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (rr_grid( 1:NCOL, 1:NROW ) )
  ALLOCATE (sfc_grid( 1:NCOL, 1:NROW ) )

  clw_grid = 0.0
  tpw_grid = 0.0
  sice_grid = 0.0
  swe_grid = 0.0
  gs_grid = 0.0
  sicemy_grid = 0.0
  sicefy_grid = 0.0
  iwp_grid = 0.0
  rwp_grid = 0.0
  lwp_grid = 0.0
  rr_grid = 0.0
  sfc_grid = -999.0
  
  ALLOCATE (clw_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (tpw_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (sice_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (swe_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (gs_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (sicemy_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (sicefy_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (iwp_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (rwp_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (lwp_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (rr_grid_num( 1:NCOL, 1:NROW ) )
  
  ALLOCATE (sfc_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (sfc0_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (sfc1_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (sfc2_grid_num( 1:NCOL, 1:NROW ) )
  ALLOCATE (sfc3_grid_num( 1:NCOL, 1:NROW ) )

  clw_grid_num = 0
  tpw_grid_num = 0
  sice_grid_num = 0      
  swe_grid_num = 0      
  gs_grid_num = 0      
  sicemy_grid_num = 0      
  sicefy_grid_num = 0      
  iwp_grid_num = 0
  rwp_grid_num = 0      
  lwp_grid_num = 0      
  rr_grid_num = 0
  
  sfc_grid_num = 0

  sfc0_grid_num = 0
  sfc1_grid_num = 0
  sfc2_grid_num = 0
  sfc3_grid_num = 0


  open(66,file=trim(filesList) )
  !-----------------------------------------------------
  !     Loop over the EDR files
  !-----------------------------------------------------
  FilesLoop: DO while( 1 .eq. 1 )
  
    read(66, '(A)', end=99 ) file
    !write(*,'(A)') trim(file) 

    !---Read header of DEP file
    CALL ReadHdrDEP(iuDEP,trim(file),Dep,nprf)

    !---Loop over the profiles within the file
    ProfilesLoop: DO iprf=1,nPrf

        CALL ReadDep(iuDEP,Dep,ierr)

        IF ( ierr.eq.Warn_EndOfFile   ) EXIT  ProfilesLoop
        IF ( ierr.eq.Warn_readInvalid ) CYCLE ProfilesLoop
        IF ( ierr .ne. 0              ) EXIT  ProfilesLoop

        icol = INT((Dep%lon + 180.0) * gridfactor + fix)
        irow = INT((Dep%lat + 90.0 ) * gridfactor + fix)

        if ( icol  .lt. 1    ) icol = 1
        if ( icol  .gt. NCOL ) icol = 1

        if ( irow  .lt. 1    ) irow = 1
        if ( irow  .gt. NROW ) irow = NROW

        if ( Dep%qc(1) .lt. 2 ) then

            if( Dep%tpw .ge. 0.0 ) then
                tpw_grid(icol,irow)     = tpw_grid(icol,irow) + 0.01*Dep%tpw
                tpw_grid_num(icol,irow) = tpw_grid_num(icol,irow) + 1 
            endif

            if( Dep%clw .ge. 0.0 ) then
                clw_grid(icol,irow)     = clw_grid(icol,irow)+ Dep%clw
                clw_grid_num(icol,irow) = clw_grid_num(icol,irow)+ 1 
            endif

            if( Dep%sic .ge. 0.0 ) then
                sice_grid(icol,irow)     = sice_grid(icol,irow) + 0.01*Dep%sic
                sice_grid_num(icol,irow) = sice_grid_num(icol,irow)  + 1 
            endif

            if( Dep%swe .ge. 0.0 ) then
                swe_grid(icol,irow)     = swe_grid(icol,irow) + 0.01*Dep%swe
                swe_grid_num(icol,irow) = swe_grid_num(icol,irow)   + 1
            endif

            if( Dep%SnowGS .ge. 0.0 ) then
                gs_grid(icol,irow)     = gs_grid(icol,irow)  + Dep%SnowGS
                gs_grid_num(icol,irow) = gs_grid_num(icol,irow) + 1 
            endif

            if( Dep%SIC_MY .ge. 0.0 ) then
                sicemy_grid(icol,irow)     = sicemy_grid(icol,irow) + 0.01*Dep%SIC_MY
                sicemy_grid_num(icol,irow) = sicemy_grid_num(icol,irow) + 1 
            endif

            if( Dep%SIC_FY .ge. 0.0 ) then
                sicefy_grid(icol,irow)     = sicefy_grid(icol,irow) + 0.01*Dep%SIC_FY
                sicefy_grid_num(icol,irow) = sicefy_grid_num(icol,irow) + 1 
            endif

            if( Dep%GWP .ge. 0.0 ) then
                iwp_grid(icol,irow)     = iwp_grid(icol,irow) + Dep%GWP
                iwp_grid_num(icol,irow) = iwp_grid_num(icol,irow) + 1 
            endif

            if( Dep%RWP .ge. 0.0 ) then
                rwp_grid(icol,irow)     = rwp_grid(icol,irow) + Dep%RWP
                rwp_grid_num(icol,irow) = rwp_grid_num(icol,irow) + 1
            endif

            if( Dep%LWP .ge. 0.0 ) then
                lwp_grid(icol,irow)     = lwp_grid(icol,irow) + Dep%LWP
                lwp_grid_num(icol,irow) = lwp_grid_num(icol,irow) + 1 
            endif

            if( Dep%RR  .ge. 0.0 ) then
                rr_grid(icol,irow)     = rr_grid(icol,irow) + 0.01*Dep%RR
                rr_grid_num(icol,irow) = rr_grid_num(icol,irow) + 1 
            endif
            
            !---- surface type
            if( Dep%iTypSfc .eq. 0 ) then
                sfc0_grid_num(icol,irow) = sfc0_grid_num(icol,irow) + 1 
            else if( Dep%iTypSfc .eq. 1 ) then
                sfc1_grid_num(icol,irow) = sfc1_grid_num(icol,irow) + 1 
            else if( Dep%iTypSfc .eq. 2 ) then
                sfc2_grid_num(icol,irow) = sfc2_grid_num(icol,irow) + 1 
            else if( Dep%iTypSfc .eq. 3 ) then
                sfc3_grid_num(icol,irow) = sfc3_grid_num(icol,irow) + 1 
            endif

        endif


    ENDDO ProfilesLoop
   
    !---Close the DEP file
    CLOSE (iuDEP)

  ENDDO FilesLoop

99 continue
   close(66)  


  !-----------------------------------------------------------------------------
  !     average
  !-----------------------------------------------------------------------------
  do irow = 1, NROW
  do icol = 1, NCOL
  
    if( tpw_grid_num(icol,irow) .gt. 0 ) then
      tpw_grid(icol,irow) = tpw_grid(icol,irow)/tpw_grid_num(icol,irow)*100.0
    else
      tpw_grid(icol,irow) = -999.0
    endif

    if( clw_grid_num(icol,irow) .gt. 0 ) then
      clw_grid(icol,irow) = clw_grid(icol,irow)/clw_grid_num(icol,irow)
    else
      clw_grid(icol,irow) = -999.0
    endif

    if( sice_grid_num(icol,irow) .gt. 0 ) then
      sice_grid(icol,irow) = sice_grid(icol,irow)/sice_grid_num(icol,irow)*100.0
    else
      sice_grid(icol,irow) = -999.0
    endif

    if( swe_grid_num(icol,irow) .gt. 0 ) then
      swe_grid(icol,irow) = swe_grid(icol,irow)/swe_grid_num(icol,irow)*100.0
    else
      swe_grid(icol,irow) = -999.0
    endif

    if( gs_grid_num(icol,irow) .gt. 0 ) then
      gs_grid(icol,irow) = gs_grid(icol,irow)/gs_grid_num(icol,irow)
    else
      gs_grid(icol,irow) = -999.0
    endif

    if( sicemy_grid_num(icol,irow) .gt. 0 ) then
      sicemy_grid(icol,irow) = sicemy_grid(icol,irow)/sicemy_grid_num(icol,irow)*100.0
    else
      sicemy_grid(icol,irow) = -999.0
    endif

    if( sicefy_grid_num(icol,irow) .gt. 0 ) then
      sicefy_grid(icol,irow) = sicefy_grid(icol,irow)/sicefy_grid_num(icol,irow)*100.0
    else
      sicefy_grid(icol,irow) = -999.0
    endif

    if( iwp_grid_num(icol,irow) .gt. 0 ) then
      iwp_grid(icol,irow) = iwp_grid(icol,irow)/iwp_grid_num(icol,irow)
    else
      iwp_grid(icol,irow) = -999.0
    endif

    if( rwp_grid_num(icol,irow) .gt. 0 ) then
      rwp_grid(icol,irow) = rwp_grid(icol,irow)/rwp_grid_num(icol,irow)
    else
      rwp_grid(icol,irow) = -999.0
    endif

    if( lwp_grid_num(icol,irow) .gt. 0 ) then
      lwp_grid(icol,irow) = lwp_grid(icol,irow)/lwp_grid_num(icol,irow)
    else
      lwp_grid(icol,irow) = -999.0
    endif
    
    if( rr_grid_num(icol,irow) .gt. 0 ) then
      rr_grid(icol,irow) = rr_grid(icol,irow)/rr_grid_num(icol,irow)*100.0
    else
      rr_grid(icol,irow) = -999.0
    endif
    
    !---- surface loop: to get predominant one ---- 
    if( sfc0_grid_num(icol,irow) .gt. sfc_grid_num(icol,irow) ) then
        sfc_grid_num(icol,irow) = sfc0_grid_num(icol,irow) 
        sfc_grid(icol,irow) = 0.0
    endif
    
    if( sfc1_grid_num(icol,irow) .gt. sfc_grid_num(icol,irow) ) then
        sfc_grid_num(icol,irow) = sfc1_grid_num(icol,irow) 
        sfc_grid(icol,irow) = 1.0
    endif
    
    if( sfc2_grid_num(icol,irow) .gt. sfc_grid_num(icol,irow) ) then
        sfc_grid_num(icol,irow) = sfc2_grid_num(icol,irow) 
        sfc_grid(icol,irow) = 2.0
    endif
    
    if( sfc3_grid_num(icol,irow) .gt. sfc_grid_num(icol,irow) ) then
        sfc_grid_num(icol,irow) = sfc3_grid_num(icol,irow) 
        sfc_grid(icol,irow) = 3.0
    endif
    
    
  enddo
  enddo


  !-----------------------------------------------------
  !     writeout the average
  !-----------------------------------------------------
  !tpw_avrg_2.5deg_2008-10-07_2008-10-11
  !tpw_avrg_1.0deg_2008-10-07_2008-10-11
  
  timeSuffixStr = yyyymmdd_start//'_'//yyyymmdd_end
  
  if( ABS(gridFactor-1.0) .lt. 0.0001 ) then
    gridFactorStr = '1.0deg_'
  else if ( ABS(gridFactor-0.4) .lt. 0.0001 ) then
    gridFactorStr = '2.5deg_'
  endif

  gridFile=trim(satId)//'_clw_'//gridFactorStr//timeSuffixStr 
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)clw_grid(1:NCOL, 1:NROW)
  close(25)

  gridFile=trim(satId)//'_tpw_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)tpw_grid(1:NCOL, 1:NROW)
  close(25)
  
  gridFile=trim(satId)//'_sice_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)sice_grid(1:NCOL, 1:NROW)
  close(25)
 
  gridFile=trim(satId)//'_swe_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)swe_grid(1:NCOL, 1:NROW)
  close(25)
 
  gridFile=trim(satId)//'_gs_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)gs_grid(1:NCOL, 1:NROW)
  close(25)

  gridFile=trim(satId)//'_sicemy_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)sicemy_grid(1:NCOL, 1:NROW)
  close(25)

  gridFile=trim(satId)//'_sicefy_'//gridFactorStr//timeSuffixStr 
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)sicefy_grid(1:NCOL, 1:NROW)
  close(25)

  gridFile=trim(satId)//'_iwp_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)iwp_grid(1:NCOL, 1:NROW)
  close(25)

  gridFile=trim(satId)//'_rwp_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)rwp_grid(1:NCOL, 1:NROW)
  close(25)

  gridFile=trim(satId)//'_lwp_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)lwp_grid(1:NCOL, 1:NROW)
  close(25)

  gridFile=trim(satId)//'_rr_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)rr_grid(1:NCOL, 1:NROW)
  close(25)
  
  gridFile=trim(satId)//'_sfc_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)sfc_grid(1:NCOL, 1:NROW)
  close(25)
  
  gridFile=trim(satId)//'_sfc_num_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)sfc_grid_num(1:NCOL, 1:NROW)
  close(25)
  
  
  !---Release memory
  
  DEALLOCATE(clw_grid)  
  DEALLOCATE(tpw_grid)
  DEALLOCATE(sice_grid)
  DEALLOCATE(swe_grid)
  DEALLOCATE(gs_grid)
  DEALLOCATE(sicemy_grid)
  DEALLOCATE(sicefy_grid)
  DEALLOCATE(iwp_grid)
  DEALLOCATE(rwp_grid)
  DEALLOCATE(lwp_grid)
  DEALLOCATE(rr_grid)
  DEALLOCATE(sfc_grid)

  DEALLOCATE(clw_grid_num)
  DEALLOCATE(tpw_grid_num)   
  DEALLOCATE(sice_grid_num)   
  DEALLOCATE(swe_grid_num)   
  DEALLOCATE(gs_grid_num)   
  DEALLOCATE(sicemy_grid_num)   
  DEALLOCATE(sicefy_grid_num)   
  DEALLOCATE(iwp_grid_num)   
  DEALLOCATE(rwp_grid_num)   
  DEALLOCATE(lwp_grid_num)
  DEALLOCATE(rr_grid_num)
  
  DEALLOCATE(sfc_grid_num)
  DEALLOCATE(sfc0_grid_num)
  DEALLOCATE(sfc1_grid_num)
  DEALLOCATE(sfc2_grid_num)
  DEALLOCATE(sfc3_grid_num)

end program gridDepClimate

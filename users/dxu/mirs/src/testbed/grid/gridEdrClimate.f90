!***************************************************************************************************
! To Grid MIRS EDR into climate data(pentad, weekly and monthly)
!
!  Wanchun Chen      02/10/2009
!
!***************************************************************************************************

Program GridEdrClimate

  USE Consts
  USE misc
  USE utils
  USE IO_Scene
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE

  !---INTRINSIC functions used
  INTRINSIC :: ABS,INT,TRIM
  
  INTEGER            :: iu_list=20,iuEDR,ierr,iprf,nprf
  INTEGER, PARAMETER :: len=250,nqc=4
  CHARACTER(LEN=len) :: file

  !----Structures
  TYPE(Scene_type)   :: Scene
  
  INTEGER, PARAMETER :: NLAY=100, NLAY_PLOT=11
  INTEGER            :: NCHAN=20
  INTEGER            :: NCOL, NROW
  INTEGER            :: icol, irow, ilay, ichan
  INTEGER            :: INT_SATID=SENSOR_ID_N18

  INTEGER, DIMENSION(NLAY_PLOT) :: layers

  !layers=[100,200,300,400,500,600,700,800,850,900,950]
  !index=[44 55 63 70 76 81 85 89 91 93 95]

  CHARACTER(LEN=21)  :: timeSuffixStr=''  
  CHARACTER(LEN=7)   :: gridFactorStr=''
  
  !----namelist data
  CHARACTER(LEN=len) :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=8)   :: satId=DEFAULT_VALUE_STR4  ! string type satellite id: n18, m2, f16, f18, etc
  CHARACTER(LEN=10)  :: yyyymmdd_start='yyyy-mm-dd'
  CHARACTER(LEN=10)  :: yyyymmdd_end='yyyy-mm-dd'
  REAL               :: gridfactor=DEFAULT_VALUE_REAL
  CHARACTER(LEN=len) :: gridPath=DEFAULT_VALUE_STR4
  INTEGER            :: climateType=DEFAULT_VALUE_INT  ! 0(pentad), 1(weekly), 2(monthly)
  
  NAMELIST /GridNameList/filesList,satId,yyyymmdd_start,yyyymmdd_end,gridfactor,gridPath,climateType
  
  
  !-------------------------------------------------------------------------------
  !     Output variables identifiers definiton section, commented part from DEP
  !-------------------------------------------------------------------------------
  REAL, DIMENSION(:,:),   ALLOCATABLE :: chisq_grid
  REAL, DIMENSION(:,:),   ALLOCATABLE :: psfc_grid
  REAL, DIMENSION(:,:),   ALLOCATABLE :: tskin_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: em_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: yfwd_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ym_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ymCorr_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: temp_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: wv_grid

  INTEGER, DIMENSION(:,:),   ALLOCATABLE :: chisq_grid_num
  INTEGER, DIMENSION(:,:),   ALLOCATABLE :: psfc_grid_num
  INTEGER, DIMENSION(:,:),   ALLOCATABLE :: tskin_grid_num
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: em_grid_num
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: yfwd_grid_num
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ym_grid_num
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ymCorr_grid_num
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: temp_grid_num
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: wv_grid_num

 
  CHARACTER(LEN=len) :: gridFile
  
  !-----------------------------------------------------
  !     grid variables
  !-----------------------------------------------------
  REAL        :: fix = 0.5

  !-----------------------------------------------------
  !     Execute section begins here
  !-----------------------------------------------------
  
  layers(1)  = 44
  layers(2)  = 55
  layers(3)  = 63
  layers(4)  = 70
  layers(5)  = 76  
  layers(6)  = 81  
  layers(7)  = 85  
  layers(8)  = 89  
  layers(9)  = 91  
  layers(10) = 93  
  layers(11) = 95  

  
  READ(*,NML=GridNameList)
  
  ! string type satellite id --> integer type satellite id
  if      ( strcmp(satId, SATID_F16)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F16
      NCHAN=24
  else if ( strcmp(satId, SATID_F17)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F17
      NCHAN=24
  else if ( strcmp(satId, SATID_F18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F18
      NCHAN=24
  else if ( strcmp(satId, SATID_N18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N18
      NCHAN=20
  else if ( strcmp(satId, SATID_N19)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N19
      NCHAN=20
  else if ( strcmp(satId, SATID_NPP)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_NPP
      NCHAN=22
  else if ( strcmp(satId, SATID_METOPA) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPA
      NCHAN=20
  else if ( strcmp(satId, SATID_METOPB) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPB
      NCHAN=20
  else if ( strcmp(satId, SATID_AMSRE)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_AMSRE
      NCHAN=12
  else if ( strcmp(satId, SATID_GCOMW1) .eq. 0 ) then
      INT_SATID = SENSOR_ID_GCOMW1
      NCHAN=14
  else if ( strcmp(satId, SATID_TRMM)   .eq. 0 ) then
      INT_SATID = SENSOR_ID_TRMM
      NCHAN=9
  endif
 

  !-----------------------------------------------------
  !     Allocate output array and initialize them
  !-----------------------------------------------------
  NCOL=int(360*gridfactor)
  NROW=int(180*gridfactor)

  ALLOCATE (chisq_grid ( 1:NCOL, 1:NROW ) )  
  ALLOCATE (psfc_grid ( 1:NCOL, 1:NROW ) )  
  ALLOCATE (tskin_grid ( 1:NCOL, 1:NROW ) )
  ALLOCATE (em_grid ( 1:NCOL, 1:NROW, 1:NCHAN) )  
  ALLOCATE (yfwd_grid ( 1:NCOL, 1:NROW, 1:NCHAN) )  
  ALLOCATE (ym_grid ( 1:NCOL, 1:NROW, 1:NCHAN) ) 
  ALLOCATE (ymCorr_grid ( 1:NCOL, 1:NROW, 1:NCHAN) )
  ALLOCATE (temp_grid ( 1:NCOL, 1:NROW, 1:NLAY_PLOT) )  
  ALLOCATE (wv_grid ( 1:NCOL, 1:NROW, 1:NLAY_PLOT) )

  ALLOCATE (chisq_grid_num ( 1:NCOL, 1:NROW ) )  
  ALLOCATE (psfc_grid_num ( 1:NCOL, 1:NROW ) )  
  ALLOCATE (tskin_grid_num ( 1:NCOL, 1:NROW ) )
  ALLOCATE (em_grid_num ( 1:NCOL, 1:NROW, 1:NCHAN) )  
  ALLOCATE (yfwd_grid_num ( 1:NCOL, 1:NROW, 1:NCHAN) )  
  ALLOCATE (ym_grid_num ( 1:NCOL, 1:NROW, 1:NCHAN) ) 
  ALLOCATE (ymCorr_grid_num ( 1:NCOL, 1:NROW, 1:NCHAN) )
  ALLOCATE (temp_grid_num ( 1:NCOL, 1:NROW, 1:NLAY_PLOT) )  
  ALLOCATE (wv_grid_num ( 1:NCOL, 1:NROW, 1:NLAY_PLOT) )

  
  chisq_grid = 0.0  
  psfc_grid = 0.0
  tskin_grid = 0.0
  em_grid = 0.0
  yfwd_grid = 0.0
  ym_grid = 0.0  
  ymCorr_grid = 0.0
  temp_grid = 0.0
  wv_grid = 0.0

  chisq_grid_num = 0  
  psfc_grid_num = 0
  tskin_grid_num = 0
  em_grid_num = 0
  yfwd_grid_num = 0
  ym_grid_num = 0  
  ymCorr_grid_num = 0
  temp_grid_num = 0
  wv_grid_num = 0
  
  open(66,file=trim(filesList) )
  !-----------------------------------------------------------------------------
  !     Loop over the EDR files
  !-----------------------------------------------------------------------------
  FilesLoop: do while( 1 .eq. 1 )
  !FilesLoop: DO ifile=1,nfiles
  
    read(66, '(A)', end=99 ) file
    !write(*,'(A)') trim(file) 

    !---Read header of EDR file
    CALL ReadHdrScene(iuEDR,trim(file),Scene,nprf)

    !---Loop over the profiles within the file
    ProfilesLoop: DO iprf=1,nprf
        CALL ReadScene(iuEDR,Scene,ierr)
 
        IF ( ierr.eq.Warn_EndOfFile   ) EXIT  ProfilesLoop
        IF ( ierr.eq.Warn_readInvalid ) CYCLE ProfilesLoop
        IF ( ierr .ne. 0              ) EXIT ProfilesLoop

        icol = INT((Scene%lon + 180.0) * gridfactor + fix)
        irow = INT((Scene%lat + 90.0 ) * gridfactor + fix)

        if ( icol  .lt. 1    ) icol=1
        if ( icol  .gt. NCOL ) icol=1

        if ( irow  .lt. 1    ) irow=1
        if ( irow  .gt. NROW ) irow=NROW

        if ( Scene%qc(1) .LT. 2 ) then

          chisq_grid(icol,irow) = chisq_grid(icol,irow) +  Scene%ChiSq
          psfc_grid(icol,irow)  = psfc_grid(icol,irow) + 0.000001 * Scene%SfcPress

          if( Scene%Tskin .gt. 0 ) then
            tskin_grid(icol,irow) = tskin_grid(icol,irow) + 0.0001 * Scene%Tskin
            tskin_grid_num(icol,irow) = tskin_grid_num(icol,irow) + 1
          endif
 
          chisq_grid_num(icol,irow) = chisq_grid_num(icol,irow) + 1        
          psfc_grid_num(icol,irow)  = psfc_grid_num(icol,irow) + 1         

  
          do ilay=1,NLAY_PLOT
            if ( Scene%Pres_lay(layers(ilay)) .lt. Scene%SfcPress ) then
              temp_grid(icol,irow,ilay)     = temp_grid(icol,irow,ilay) + 0.0001 * Scene%Temp_lay(layers(ilay))
              wv_grid(icol,irow,ilay)       = wv_grid(icol,irow,ilay) + 0.01 * Scene%Absorb_lay(layers(ilay),1)
              temp_grid_num(icol,irow,ilay) = temp_grid_num(icol,irow,ilay) + 1
              wv_grid_num(icol,irow,ilay)   = wv_grid_num(icol,irow,ilay) + 1
            endif
          enddo

          do ichan=1,Scene%Nchan
            em_grid(icol,irow,ichan)     = em_grid(icol,irow,ichan) + 0.1 * Scene%Emiss(ichan)
            yfwd_grid(icol,irow,ichan)   = yfwd_grid(icol,irow,ichan) + 0.0001 * Scene%YFwd(ichan)
            ym_grid(icol,irow,ichan)     = ym_grid(icol,irow,ichan) + 0.0001 * Scene%Ym(ichan)
            ymCorr_grid(icol,irow,ichan) = ymCorr_grid(icol,irow,ichan) + 0.0001 * Scene%YmCorr(ichan)
    
            em_grid_num(icol,irow,ichan)     = em_grid_num(icol,irow,ichan) + 1        
            yfwd_grid_num(icol,irow,ichan)   = yfwd_grid_num(icol,irow,ichan) + 1 
            ym_grid_num(icol,irow,ichan)     = ym_grid_num(icol,irow,ichan) + 1        
            ymCorr_grid_num(icol,irow,ichan) = ymCorr_grid_num(icol,irow,ichan) + 1
          enddo
      
        endif


    ENDDO ProfilesLoop
   
    !---Close the EDR file
    CLOSE (iuEDR)

    !---Release memory allocated to Scene
    CALL DestroyScene(Scene)

  ENDDO FilesLoop

99 continue
   close(66)  

 
  !-----------------------------------------------------------------------------
  !     average
  !-----------------------------------------------------------------------------
  do irow = 1, NROW
  do icol = 1, NCOL
  
    if( tskin_grid_num(icol,irow) .gt. 0 ) then
      tskin_grid(icol,irow) = tskin_grid(icol,irow)/tskin_grid_num(icol,irow) * 10000.0 
    else
      tskin_grid(icol,irow) = -999.0
    endif
    
  enddo
  enddo


  do ilay = 1, NLAY_PLOT
  do irow = 1, NROW
  do icol = 1, NCOL
  
    if( temp_grid_num(icol,irow,ilay) .gt. 0 ) then
      temp_grid(icol,irow,ilay) = temp_grid(icol,irow,ilay)/temp_grid_num(icol,irow,ilay) * 10000.0
    else
      temp_grid(icol,irow,ilay) = -999.0
    endif

    if( wv_grid_num(icol,irow,ilay) .gt. 0 ) then
      wv_grid(icol,irow,ilay) = wv_grid(icol,irow,ilay)/wv_grid_num(icol,irow,ilay) * 100.0
    else
      wv_grid(icol,irow,ilay) = -999.0
    endif

  enddo
  enddo
  enddo


  do ichan = 1, NCHAN
  do irow  = 1, NROW
  do icol  = 1, NCOL
  
    if( em_grid_num(icol,irow,ichan) .gt. 0 ) then
      em_grid(icol,irow,ichan) = em_grid(icol,irow,ichan)/em_grid_num(icol,irow,ichan) * 10.0
    else
      em_grid(icol,irow,ichan) = -999.0
    endif

    if( yfwd_grid_num(icol,irow,ichan) .gt. 0 ) then
      yfwd_grid(icol,irow,ichan) = yfwd_grid(icol,irow,ichan)/yfwd_grid_num(icol,irow,ichan) * 10000.0  
    else
      yfwd_grid(icol,irow,ichan) = -999.0
    endif

    if( ym_grid_num(icol,irow,ichan) .gt. 0 ) then
      ym_grid(icol,irow,ichan) = ym_grid(icol,irow,ichan)/ym_grid_num(icol,irow,ichan) * 10000.0
    else
      ym_grid(icol,irow,ichan) = -999.0
    endif

    if( ymCorr_grid_num(icol,irow,ichan) .gt. 0 ) then
      ymCorr_grid(icol,irow,ichan) = ymCorr_grid(icol,irow,ichan)/ymCorr_grid_num(icol,irow,ichan) * 10000.0
    else
      ymCorr_grid(icol,irow,ichan) = -999.0
    endif

  enddo
  enddo
  enddo


  !-----------------------------------------------------
  !     writeout
  !-----------------------------------------------------
  !ts_avrg_2.5deg_2008-10-07_2008-10-11
  !ts_avrg_1.0deg_2008-10-07_2008-10-11
  
  timeSuffixStr = yyyymmdd_start//'_'//yyyymmdd_end
  
  if( ABS(gridFactor-1.0) .lt. 0.0001 ) then
    gridFactorStr = '1.0deg_'
  else if ( ABS(gridFactor-0.4) .lt. 0.0001 ) then
    gridFactorStr = '2.5deg_'
  endif
    
  gridFile=trim(satId)//'_tskin_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)tskin_grid(1:NCOL, 1:NROW)
  close(25)
 
  gridFile=trim(satId)//'_em_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  do ichan=1,NCHAN
    write(25,rec=ichan)em_grid(1:NCOL, 1:NROW, ichan)
  enddo
  close(25)
 
  gridFile=trim(satId)//'_yfwd_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  do ichan=1,NCHAN
    write(25,rec=ichan)yfwd_grid(1:NCOL, 1:NROW, ichan)
  enddo
  close(25)
 
  gridFile=trim(satId)//'_ym_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  do ichan=1,NCHAN
    write(25,rec=ichan)ym_grid(1:NCOL, 1:NROW, ichan)
  enddo
  close(25)
 
  gridFile=trim(satId)//'_ymCorr_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  do ichan=1,NCHAN
    write(25,rec=ichan)ymCorr_grid(1:NCOL, 1:NROW, ichan)
  enddo
  close(25)
 

  gridFile=trim(satId)//'_temp_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  do ilay=1,NLAY_PLOT
    write(25,rec=ilay)temp_grid(1:NCOL, 1:NROW, ilay)
  enddo
  close(25)
 
  gridFile=trim(satId)//'_wv_'//gridFactorStr//timeSuffixStr
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  do ilay=1,NLAY_PLOT
    write(25,rec=ilay)wv_grid(1:NCOL, 1:NROW, ilay)
  enddo
  close(25) 
 
  
  !--- Release memory
  
  DEALLOCATE(chisq_grid)
  DEALLOCATE(psfc_grid)
  DEALLOCATE(tskin_grid)
  DEALLOCATE(em_grid)
  DEALLOCATE(yfwd_grid)
  DEALLOCATE(ym_grid)
  DEALLOCATE(ymCorr_grid)
  DEALLOCATE(temp_grid)
  DEALLOCATE(wv_grid)

  DEALLOCATE(chisq_grid_num)
  DEALLOCATE(psfc_grid_num)
  DEALLOCATE(tskin_grid_num)
  DEALLOCATE(em_grid_num)
  DEALLOCATE(yfwd_grid_num)
  DEALLOCATE(ym_grid_num)
  DEALLOCATE(ymCorr_grid_num)
  DEALLOCATE(temp_grid_num)
  DEALLOCATE(wv_grid_num)
  
end program gridEdrClimate

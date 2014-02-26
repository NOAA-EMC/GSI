!***************************************************************************************************
! To Generate Dynamic background
!
!  Wanchun Chen      02/10/2009
!
!***************************************************************************************************

Program DynamicBackground

  USE Consts
  USE misc
  USE utils
  USE IO_Scene
  !USE IO_Dep
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE

  !---INTRINSIC functions used
  INTRINSIC :: ABS,INT,TRIM
  
  INTEGER            :: iu_list=20,iuEDR,ierr,iprf,nprf !,iuDEP
  INTEGER, PARAMETER :: len=250,nqc=4
  CHARACTER(LEN=len) :: file

  !----Structures
  TYPE(Scene_type)   :: Scene
  !TYPE(DEP_type)     :: Dep
  
  INTEGER, PARAMETER :: NLAY=100
  INTEGER            :: NCHAN=20
  INTEGER            :: NCOL, NROW
  INTEGER            :: icol, irow, ilay, ichan
  INTEGER            :: INT_SATID=SENSOR_ID_N18

  !----namelist data
  CHARACTER(LEN=len) :: edrList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=8)   :: satId=DEFAULT_VALUE_STR4
  REAL               :: gridstep=DEFAULT_VALUE_REAL ! 1.0 deg or 2.5 deg
  CHARACTER(LEN=len) :: gridPath=DEFAULT_VALUE_STR4
  
  NAMELIST /GridNameList/edrList,satId,gridstep,gridPath
  
  
  !-------------------------------------------------------------------------------
  !     Output variables identifiers definiton section, commented part from DEP
  !-------------------------------------------------------------------------------
  REAL, DIMENSION(:,:),   ALLOCATABLE :: tskin_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: clw_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: em_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: temp_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: wv_grid

  INTEGER, DIMENSION(:,:),   ALLOCATABLE :: tskin_cont
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: clw_cont
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: em_cont
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: temp_cont
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: wv_cont


  CHARACTER(LEN=len) :: gridFile

  !-----------------------------------------------------
  !     grid variables
  !-----------------------------------------------------
  REAL        :: fix = 0.5

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
  else if ( strcmp(satId, SATID_GCOMW1)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_GCOMW1
      NCHAN=14
  endif
 

  !-----------------------------------------------------
  !     Allocate output array and initialize them
  !-----------------------------------------------------
  NCOL=int(360/gridstep)
  NROW=int(180/gridstep)

  ALLOCATE (tskin_grid ( 1:NCOL, 1:NROW ) )
  ALLOCATE (clw_grid ( 1:NCOL, 1:NROW, 1:NLAY) )
  ALLOCATE (em_grid ( 1:NCOL, 1:NROW, 1:NCHAN) )  
  ALLOCATE (temp_grid ( 1:NCOL, 1:NROW, 1:NLAY) )  
  ALLOCATE (wv_grid ( 1:NCOL, 1:NROW, 1:NLAY) )

  ALLOCATE (tskin_cont ( 1:NCOL, 1:NROW ) )
  ALLOCATE (clw_cont ( 1:NCOL, 1:NROW, 1:NLAY ) )
  ALLOCATE (em_cont ( 1:NCOL, 1:NROW, 1:NCHAN) )  
  ALLOCATE (temp_cont ( 1:NCOL, 1:NROW, 1:NLAY) )  
  ALLOCATE (wv_cont ( 1:NCOL, 1:NROW, 1:NLAY) )

  tskin_grid = 0.0
  clw_grid = 0.0
  em_grid = 0.0
  temp_grid = 0.0
  wv_grid = 0.0

  tskin_cont = 0
  clw_cont = 0
  em_cont = 0
  temp_cont = 0
  wv_cont = 0

  open(66,file=trim(edrList) )
  !-----------------------------------------------------------------------------
  !     Loop over the EDR files
  !-----------------------------------------------------------------------------
  FilesLoop: do while( 1 .eq. 1 )

    read(66, '(A)', end=77 ) file
    write(*,'(A)') trim(file) 

    !---Read header of EDR file
    CALL ReadHdrScene(iuEDR,trim(file),Scene,nprf)

    !---Loop over the profiles within the file
    ProfilesLoop: DO iprf=1,nprf
        CALL ReadScene(iuEDR,Scene,ierr)

        IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfilesLoop
        IF (ierr.eq.Warn_readInvalid) CYCLE ProfilesLoop
        IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. EDR file.')
        
        icol = INT((Scene%lon + 180.0) / gridstep + fix)
        irow = INT((Scene%lat + 90.0 ) / gridstep + fix)

        if ( icol  .lt. 1    ) icol=1
        if ( icol  .gt. NCOL ) icol=1

        if ( irow  .lt. 1    ) irow=1
        if ( irow  .gt. NROW ) irow=NROW

        if ( Scene%qc(1) .EQ. 0 ) then

          if( Scene%Tskin .gt. 0 ) then
            tskin_grid(icol,irow) = tskin_grid(icol,irow) + 0.0001 * Scene%Tskin
            tskin_cont(icol,irow) = tskin_cont(icol,irow) + 1
          endif

          do ilay=1,NLAY
            if( Scene%Pres_lay(ilay) .lt. Scene%SfcPress ) then
              if( Scene%Temp_lay(ilay) .gt. 0. .and. Scene%Temp_lay(ilay) .lt. 400 ) then
                temp_grid(icol,irow,ilay) = temp_grid(icol,irow,ilay) + 0.0001 * Scene%Temp_lay(ilay)
                temp_cont(icol,irow,ilay) = temp_cont(icol,irow,ilay) + 1
              endif
              if( Scene%Absorb_lay(ilay,1) .gt. 0. .and. Scene%Absorb_lay(ilay,1) .lt. 50 ) then
                wv_grid(icol,irow,ilay) = wv_grid(icol,irow,ilay) + 0.01 * Scene%Absorb_lay(ilay,1)
                wv_cont(icol,irow,ilay) = wv_cont(icol,irow,ilay) + 1
              endif
              if( Scene%CLW(ilay) .ge. 0. ) then
                 clw_grid(icol,irow,ilay) = clw_grid(icol,irow,ilay) + Scene%CLW(ilay)
                 clw_cont(icol,irow,ilay) = clw_cont(icol,irow,ilay) + 1
              endif
            endif  
          enddo

          do ichan=1,Scene%Nchan
              if( Scene%Emiss(ichan) .gt. 0 .and. Scene%Emiss(ichan) .lt. 1 ) then
                  em_grid(icol,irow,ichan) = em_grid(icol,irow,ichan) + 0.1 * Scene%Emiss(ichan)
                  em_cont(icol,irow,ichan) = em_cont(icol,irow,ichan) + 1
              endif        
          enddo

        endif


    ENDDO ProfilesLoop
   
    !---Close the EDR file
    CLOSE (iuEDR)

    !---Release memory allocated to Scene
    CALL DestroyScene(Scene)

  ENDDO FilesLoop

77 continue
   close(66)  

 
  !-----------------------------------------------------------------------------
  !     average
  !-----------------------------------------------------------------------------
  do irow = 1, NROW
  do icol = 1, NCOL

    if( tskin_cont(icol,irow) .gt. 0 ) then
      tskin_grid(icol,irow) = tskin_grid(icol,irow)/tskin_cont(icol,irow) * 10000.0 
    else
      tskin_grid(icol,irow) = -999.0
    endif

  enddo
  enddo


  do ilay = 1, NLAY
  do irow = 1, NROW
  do icol = 1, NCOL

    if( temp_cont(icol,irow,ilay) .gt. 0 ) then
      temp_grid(icol,irow,ilay) = temp_grid(icol,irow,ilay)/temp_cont(icol,irow,ilay) * 10000.0
    else
      temp_grid(icol,irow,ilay) = -999.0
    endif

    if( wv_cont(icol,irow,ilay) .gt. 0 ) then
      wv_grid(icol,irow,ilay) = wv_grid(icol,irow,ilay)/wv_cont(icol,irow,ilay) * 100.0
    else
      wv_grid(icol,irow,ilay) = -999.0
    endif

    if( clw_cont(icol,irow,ilay) .ge. 0 ) then
      clw_grid(icol,irow,ilay) = clw_grid(icol,irow,ilay)/clw_cont(icol,irow,ilay)
    else
      clw_grid(icol,irow,ilay)=-999.0
    endif

  enddo
  enddo
  enddo


  do ichan = 1, NCHAN
  do irow  = 1, NROW
  do icol  = 1, NCOL

    if( em_cont(icol,irow,ichan) .gt. 0 ) then
      em_grid(icol,irow,ichan) = em_grid(icol,irow,ichan)/em_cont(icol,irow,ichan) * 10.0
    else
      em_grid(icol,irow,ichan) = -999.0
    endif

  enddo
  enddo
  enddo

  !-----------------------------------------------------
  !     writeout
  !-----------------------------------------------------

  gridFile='background_'//trim(satId)//'.dat'
  open(25,file=trim(gridPath)//gridFile,form='unformatted')
 
  do ilay=1,NLAY
    write(25)temp_grid(1:NCOL, 1:NROW, ilay)
  enddo
 
  do ilay=1,NLAY
    write(25)wv_grid(1:NCOL, 1:NROW, ilay)
  enddo

  write(25)tskin_grid(1:NCOL, 1:NROW)

  do ilay=1,NLAY
    write(25)clw_grid(1:NCOL, 1:NROW, ilay)
  enddo

  do ichan=1,NCHAN
    write(25)em_grid(1:NCOL, 1:NROW, ichan)
  enddo

 
  close(25) 


  !--- Release memory

  DEALLOCATE(tskin_grid)
  DEALLOCATE(clw_grid)
  DEALLOCATE(em_grid)
  DEALLOCATE(temp_grid)
  DEALLOCATE(wv_grid)

  DEALLOCATE(tskin_cont)
  DEALLOCATE(clw_cont)
  DEALLOCATE(em_cont)
  DEALLOCATE(temp_cont)
  DEALLOCATE(wv_cont)

end program DynamicBackground

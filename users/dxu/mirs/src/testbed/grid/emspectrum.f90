!*******************************************************************************
!
! To compute emissivity spectrum
!
! Wanchun Chen      11/20/2011         Original Coder
!
!*******************************************************************************

Program emspectrum

  USE Consts
  USE misc
  USE utils
  USE IO_Scene
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ABS,TRIM
  
  INTEGER            :: iu_list=20,iuEDR,ierr,iprf,nprf
  INTEGER, PARAMETER :: LENF=256
  INTEGER            :: ifile, nfile
  CHARACTER(LEN=LENF), DIMENSION(:), POINTER :: inputFiles, dumFiles 
  
  !----Structures
  TYPE(Scene_type)   :: Scene
  
  INTEGER            :: NCHAN=20
  INTEGER, PARAMETER :: NSFC=4
  INTEGER, PARAMETER :: NCEND=2
  INTEGER, PARAMETER :: NANGLE=3
  INTEGER            :: INT_SATID=SENSOR_ID_N18
  INTEGER            :: isfc,icend,ichan,iangle
  
  CHARACTER(LEN=2),DIMENSION(2) :: cends = (/ 'as', 'ds' /)
  
  REAL               :: angle=0.0
  
  !----namelist data
  CHARACTER(LEN=LENF) :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=8)    :: satId=DEFAULT_VALUE_STR4  ! string type satellite id: n18, m2, f16, f18
  CHARACTER(LEN=64)   :: yyyymmdd=DEFAULT_VALUE_STR4
  CHARACTER(LEN=LENF) :: gridPath=DEFAULT_VALUE_STR4
  
  NAMELIST /emspectrumNameList/filesList,satId,yyyymmdd,gridPath
	    
  
  !-------------------------------------------------------------
  !     Output variables identifiers definiton section
  !-------------------------------------------------------------
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: avg_em,cnt_em
  CHARACTER(LEN=LENF) :: gridFile
  

  !-------------------------------------------------------------
  !     Execute section begins here
  !-------------------------------------------------------------
  READ(*,NML=emspectrumNameList)
  
  ! string type satellite id --> integer type satellite id
  if      ( strcmp(satId, SATID_F16)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F16
      NCHAN=24

  else if( strcmp(satId, SATID_F17)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F17
      NCHAN=24

  else if( strcmp(satId, SATID_F18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_F18
      NCHAN=24
      
  else if( strcmp(satId, SATID_N18)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N18
      NCHAN=20
      
  else if( strcmp(satId, SATID_N19)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_N19
      NCHAN=20
 
  else if( strcmp(satId, SATID_METOPA) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPA
      NCHAN=20
      
  else if( strcmp(satId, SATID_METOPB) .eq. 0 ) then
      INT_SATID = SENSOR_ID_METOPB
      NCHAN=20
      
  else if( strcmp(satId, SATID_TRMM)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_TRMM
      NCHAN=9

  else if( strcmp(satId, SATID_TRMM2A12)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_TRMM2A12
      NCHAN=9

  else if( strcmp(satId, SATID_NPP)    .eq. 0 ) then
      INT_SATID = SENSOR_ID_NPP
      NCHAN=22
      
  else if( strcmp(satId, SATID_AMSRE)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_AMSRE
      NCHAN=12

  else if( strcmp(satId, SATID_GCOMW1)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_GCOMW1
      NCHAN=14

  else if( strcmp(satId, SATID_FY3RI)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_FY3RI
      NCHAN=10

  else if( strcmp(satId, SATID_MTMA)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_MTMA
      NCHAN=9

  else if( strcmp(satId, SATID_MTSA)  .eq. 0 ) then
      INT_SATID = SENSOR_ID_MTSA
      NCHAN=6

  else
      write(*,*)'satId='//satId
      STOP 'Error: unsupported satId'

  endif
  

  call ReadList(iu_list, trim(filesList), inputFiles, nfile, dumFiles, '', '')
  if( nfile .lt. 1 ) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 
  
  !--------------------------------------------------------------------------------
  ! Allocate output array and initialize them, always output onto global grid mesh
  !--------------------------------------------------------------------------------
  ALLOCATE ( avg_em ( 1:NCHAN, 1:NSFC, 1:NANGLE, 1:NCEND ) )
  ALLOCATE ( cnt_em ( 1:NCHAN, 1:NSFC, 1:NANGLE, 1:NCEND ) )
  
  avg_em = 0.0
  cnt_em = 0.0

  !---- loop over the files ----  
  FilesLoop: DO ifile=1,nfile

    !write(*,'(A)') trim(inputFiles(ifile))

    !---Read header of EDR/Collocated NWP file
    CALL ReadHdrScene(iuEDR,inputFiles(ifile),Scene,nprf)

    !---Loop over the profiles within the file
    ProfilesLoop: DO iprf=1,nprf
      CALL ReadScene(iuEDR,Scene,ierr)

      if(ierr.eq.Warn_EndOfFile)   EXIT  ProfilesLoop
      if(ierr.eq.Warn_readInvalid) CYCLE ProfilesLoop
      if(ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. EDR file.')

      icend = Scene%node + 1
      isfc = Scene%iTypSfc + 1
      
      iangle = -1
      angle = Scene%Angle
      if( ABS( angle ) .le. 20.0 ) then
        iangle = 1
      else if( ABS( angle ) .gt. 20 .and. ABS( angle ) .le. 40.0 ) then
        iangle = 2
      else if( ABS( angle ) .gt. 40 .and. ABS( angle ) .le. 60.0 ) then
        iangle = 3
      endif
      
      if( INT_SATID .eq. SENSOR_ID_F16 .or. INT_SATID .eq. SENSOR_ID_F18 ) iangle = 1
      if( INT_SATID .eq. SENSOR_ID_F17 ) iangle = 1
      if( INT_SATID .eq. SENSOR_ID_MTMA ) iangle = 1
      
      if( Scene%qc(1) .eq. 0 .and. isfc .ge. 1 .and. isfc .le. 4 .and. iangle .ge. 1 .and. &
          icend .ge. 1 .and. icend .le. 2 ) then 
        do ichan = 1, NCHAN
          if( Scene%Emiss(ichan) .ge. 0 .and. Scene%Emiss(ichan) .lt. 1 ) then
            avg_em(ichan,isfc,iangle,icend) = avg_em(ichan,isfc,iangle,icend) + Scene%Emiss(ichan)  
            cnt_em(ichan,isfc,iangle,icend) = cnt_em(ichan,isfc,iangle,icend) + 1.0 
          endif
        enddo
      endif
      
    ENDDO ProfilesLoop

    !---Close the EDR file
    CLOSE (iuEDR)
    !---Release memory allocated to Scene
    CALL DestroyScene(Scene)
  
  ENDDO FilesLoop
  
  !write(*,*) 'averaging...'
  !---- average ----
  do icend  = 1, NCEND
  do iangle = 1, NANGLE
  do isfc   = 1, NSFC  
  do ichan  = 1, NCHAN 

    if( cnt_em(ichan,isfc,iangle,icend) .ge. 1. ) then
      avg_em(ichan,isfc,iangle,icend) = avg_em(ichan,isfc,iangle,icend)/cnt_em(ichan,isfc,iangle,icend)
    else
      avg_em(ichan,isfc,iangle,icend) = -999.0
    endif

  enddo
  enddo
  enddo
  enddo

  
  !write(*,*) 'output...'
  !---- output ----
  do icend = 1, NCEND
    gridFile='AVG_'//trim(satId)//'_'//trim(yyyymmdd)//'_emspectrum_'//cends(icend)//'.dat'
    write(*,'(A)') trim(gridPath)//trim(gridFile)
    open(25,file=trim(gridPath)//trim(gridFile),form='unformatted',access='direct',recl=4*NCHAN*NSFC)
    do iangle = 1, NANGLE
      write(25,rec=iangle) avg_em(1:NCHAN,1:NSFC,iangle,icend)
    enddo
    close(25)
  enddo  

  !---- Release memory
  IF( ALLOCATED(avg_em) ) DEALLOCATE ( avg_em )
  IF( ALLOCATED(cnt_em) ) DEALLOCATE ( cnt_em )
  
  DEALLOCATE( inputFiles )
  DEALLOCATE( dumFiles )
  
  !write(*,*) 'Finished emspectrum.f90'
  
end program emspectrum

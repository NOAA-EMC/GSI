!***************************************************************************************************
!
! Point by Point Output of Radiance (FWD) data
!
! 01/06/2007        Wanchun Chen          Original Coder
! 
!***************************************************************************************************

Program p2pRAD

  USE Consts
  USE misc
  USE utils
  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE
  !---INTRINSIC FUNCTIONS USED
  INTRINSIC :: TRIM,SUM,MIN
  
  INTEGER            :: iu_list=20,iuRAD,ierr,iprof,nprf
  INTEGER, PARAMETER :: lenf=256
  INTEGER            :: ifile, nfiles !, inode
  CHARACTER(LEN=lenf), DIMENSION(:), POINTER :: inputFiles, dumFiles 
  
  !---Structures
  TYPE(MeasurData_type) :: Rad

  INTEGER               :: ichan
  
  !----namelist data
  CHARACTER(LEN=lenf) :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=8)    :: satId=DEFAULT_VALUE_STR4  ! n18, metopA, f16, etc
  CHARACTER(LEN=64)   :: yyyymmdd=DEFAULT_VALUE_STR4
  CHARACTER(LEN=lenf) :: p2pPath=DEFAULT_VALUE_STR4
  REAL                :: latmin=DEFAULT_VALUE_REAL
  REAL                :: latmax=DEFAULT_VALUE_REAL
  REAL                :: lonmin=DEFAULT_VALUE_REAL
  REAL                :: lonmax=DEFAULT_VALUE_REAL
  INTEGER             :: isMirs=DEFAULT_VALUE_INT  ! 0(MIRS EDR), 1(gdas), 2(ecmwf), 3(gfs)
  
  NAMELIST /p2pRadNameList/filesList,satId,yyyymmdd,p2pPath,latmin,latmax,lonmin,lonmax,isMirs
  
  !-----------------------------------------------------
  !     P2P identifiers
  !-----------------------------------------------------
  CHARACTER(LEN=lenf)                   :: p2pFile,metaFile
  LOGICAL                               :: metaExist=.FALSE.
  INTEGER                               :: MAX_FILE=28
  INTEGER                               :: MAX_PROFILE=30000, NLAY=100, NCHAN=20
  REAL, DIMENSION(:,:,:), ALLOCATABLE   :: PPTB_AS, PPTB_DS
  
  !-----------------------------------------------------
  !     Execute section begins here
  !-----------------------------------------------------
  
  READ(*,NML=p2pRadNameList)
  
  if      ( strcmp(satId, SATID_F16 )    .eq. 0 ) then
    NCHAN=24

  else if ( strcmp(satId, SATID_F17 )    .eq. 0 ) then
    NCHAN=24

  else if ( strcmp(satId, SATID_F18 )    .eq. 0 ) then
    NCHAN=24

  else if ( strcmp(satId, SATID_N18 )    .eq. 0 ) then
    NCHAN=20

  else if ( strcmp(satId, SATID_N19 )    .eq. 0 ) then
    NCHAN=20

  else if ( strcmp(satId, SATID_NPP )    .eq. 0 ) then
    NCHAN=22

  else if ( strcmp(satId, SATID_METOPA ) .eq. 0 ) then
    NCHAN=20

  else if ( strcmp(satId, SATID_METOPB ) .eq. 0 ) then
    NCHAN=20

  else if ( strcmp(satId, SATID_AMSRE )  .eq. 0 ) then
    NCHAN=12

  else if ( strcmp(satId, SATID_GCOMW1 ) .eq. 0 ) then
    NCHAN=14

  else if ( strcmp(satId, SATID_FY3RI )  .eq. 0 ) then
    NCHAN=10

  else if ( strcmp(satId, SATID_TRMM )   .eq. 0 ) then
    NCHAN=9

  else if ( strcmp(satId, SATID_GPM )    .eq. 0 ) then
    NCHAN=13

  else if( strcmp(satId, SATID_MTMA )    .eq. 0 ) then
    NCHAN = 9

  else if( strcmp(satId, SATID_MTSA )    .eq. 0 ) then
    NCHAN = 6

  else
    write(*,*) 'satId='//TRIM(satId)
    STOP 'Un-supported satId'

  endif

  call ReadList(iu_list, trim(filesList), inputFiles, nfiles, dumFiles, '', '')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 
  
  MAX_FILE = nfiles
  MAX_PROFILE = 0
  
  !---- get MAX_FILE and MAX_PROFILE by looping all files
  DO ifile = 1, nfiles
    CALL getRadNprf( inputFiles( ifile ), nprf )
    if( nprf .gt. MAX_PROFILE ) MAX_PROFILE = nprf
  ENDDO
  write(*,*) 'p2pRad MAX_FILE    = ', MAX_FILE
  write(*,*) 'p2pRad MAX_PROFILE = ', MAX_PROFILE

  ALLOCATE ( PPTB_AS(1:MAX_PROFILE,1:MAX_FILE,1:NCHAN ) )
  ALLOCATE ( PPTB_DS(1:MAX_PROFILE,1:MAX_FILE,1:NCHAN ) )
  
  !-----------------------------------------------------
  !     Allocate output array and initialize them
  !-----------------------------------------------------
  PPTB_AS    = -999.0
  PPTB_DS    = -999.0
  
  write(*,*) 'p2p tb ( NWP simulated tb from FWD files, Rad%tb-> p2p tb )'
  write(*,*)
  
  !-----------------------------------------------------
  !     Loop over the FWD or FMSDR files
  !-----------------------------------------------------
  FilesLoop: DO ifile=1,nfiles
    
    !write(*,'(A)') trim(inputFiles(ifile))
    
    !---Read header of Measurement file
    CALL ReadHdrMeasurmts(inputFiles(ifile),iuRAD,nprf,Rad)
    
    !---Loop over the profiles within the file
    profLoop: DO iprof=1,nPrf
        CALL ReadMeasurmts(iuRAD,Rad,ierr)
 
        IF (ierr.eq.Warn_EndOfFile)   EXIT  profLoop
        IF (ierr.eq.Warn_readInvalid) CYCLE profLoop
        IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. Rad file.')

        !-- ascending 
        if (  Rad%node .eq. 0 .and. SUM(Rad%qc)  .eq. 0  &
        .and. Rad%lon  .ge. lonmin .and. Rad%lon .le. lonmax &
        .and. Rad%lat  .ge. latmin .and. Rad%lat .le. latmax) then   
           do ichan=1,Rad%Nchan
              PPTB_AS(iprof,ifile,ichan) = Rad%tb(ichan)
           enddo
        endif

        !-- descending
        if (  Rad%node .eq. 1 .and. SUM(Rad%qc)  .eq. 0  &
        .and. Rad%lon  .ge. lonmin .and. Rad%lon .le. lonmax &
        .and. Rad%lat  .ge. latmin .and. Rad%lat .le. latmax) then    
           do ichan=1,Rad%Nchan
              PPTB_DS(iprof,ifile,ichan) = Rad%tb(ichan)
           enddo
        endif

    ENDDO profLoop
    !---Close the Rad file
    CLOSE (iuRAD)
  ENDDO FilesLoop

  
  !---- output a P2P meta data file if not exist yet ( to be used by p2p_mirs_nwp.pro )
  metaFile = TRIM(p2pPath)//'P2P_'//TRIM(satId)//'_'//TRIM(yyyymmdd)//'_meta.txt'
  inquire(file=metaFile,exist=metaExist)
  if( .NOT. metaExist ) then
    open( 25,file = metaFile)
    write(25,*)MAX_FILE
    write(25,*)MAX_PROFILE
    close(25)
  endif
    
  !-----------------------------------------------------
  !     writeout P2P stuff
  !-----------------------------------------------------
  p2pFile='P2P_'//trim(satId)//'_'//trim(yyyymmdd)//'_tb_as.dat'
  if ( isMirs .eq. 1 ) p2pFile='P2P_'//trim(satId)//'_gdas_'//trim(yyyymmdd)//'_tb_as.dat'
  if ( isMirs .eq. 2 ) p2pFile='P2P_'//trim(satId)//'_ecmwf_'//trim(yyyymmdd)//'_tb_as.dat'
  if ( isMirs .eq. 3 ) p2pFile='P2P_'//trim(satId)//'_gfs_'//trim(yyyymmdd)//'_tb_as.dat'
  write(*,'(A)')'p2p file='//trim(p2pPath)//trim(p2pFile)
  open(25,file=trim(p2pPath)//trim(p2pFile),form='unformatted', access='direct', recl=4*MAX_FILE*MAX_PROFILE)
  do ichan=1,NCHAN
    write(25,rec=ichan)PPTB_AS(1:MAX_PROFILE, 1:MAX_FILE, ichan)
  enddo
  close(25)
  
  p2pFile='P2P_'//trim(satId)//'_'//trim(yyyymmdd)//'_tb_ds.dat'
  if ( isMirs .eq. 1 ) p2pFile='P2P_'//trim(satId)//'_gdas_'//trim(yyyymmdd)//'_tb_ds.dat'
  if ( isMirs .eq. 2 ) p2pFile='P2P_'//trim(satId)//'_ecmwf_'//trim(yyyymmdd)//'_tb_ds.dat'
  if ( isMirs .eq. 3 ) p2pFile='P2P_'//trim(satId)//'_gfs_'//trim(yyyymmdd)//'_tb_ds.dat'
  write(*,'(A)')'p2p file='//trim(p2pPath)//trim(p2pFile)
  open(25,file=trim(p2pPath)//trim(p2pFile),form='unformatted', access='direct', recl=4*MAX_FILE*MAX_PROFILE)
  do ichan=1,NCHAN
    write(25,rec=ichan)PPTB_DS(1:MAX_PROFILE, 1:MAX_FILE, ichan)
  enddo
  close(25)

  !-----------------------------------------------------
  !     deallocate
  !-----------------------------------------------------
  DEALLOCATE(PPTB_AS)
  DEALLOCATE(PPTB_DS)
  DEALLOCATE(inputFiles)
  DEALLOCATE(dumFiles)

end program p2pRAD

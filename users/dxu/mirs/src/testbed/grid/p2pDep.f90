!***************************************************************************************************
! To p2p Dep parameters.
! 
! 01/13/2010        Wanchun Chen        Original Coder
!
!***************************************************************************************************

Program p2pDep

  USE Consts
  USE misc
  USE utils
  USE IO_DEP
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE
  !---INTRINSIC FUNCTIONS USED
  INTRINSIC          :: TRIM,INT,BTEST,MIN
  
  INTEGER            :: iu_list=20,iuDEP,ierr,iprof,nprf
  INTEGER, PARAMETER :: nlen = 256
  INTEGER, PARAMETER :: NPMAX = 16
  INTEGER, PARAMETER :: NCEND = 2
  INTEGER, PARAMETER :: CHISQ_THRESH = 5
  INTEGER            :: ifile, nfiles !, inode
  CHARACTER(LEN=nlen), DIMENSION(:), POINTER :: inputFiles, dumFiles  

  !----Structures
  TYPE(DEP_type)     :: DEP
  
  INTEGER            :: qc, icend, cend, ok = 0
  REAL               :: lat, lon, chisq
  
  !---- product id stuffs
  CHARACTER(LEN = 16),DIMENSION(NPMAX) :: prodIds
  CHARACTER(LEN = 16)                  :: prodId
  INTEGER                              :: iprod, NPROD = 0
  INTEGER                              :: pos1 = 1, pos2, n = 0, i
  CHARACTER(LEN = 2),DIMENSION(2)      :: cends = (/ 'as', 'ds' /)
  
  !----namelist data
  CHARACTER(LEN=nlen)      :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=8)         :: satId=DEFAULT_VALUE_STR4  ! n18/metopA/n19/f16/f18/trmm/npp/gpm/fy3ri/amsre
  CHARACTER(LEN=64)        :: yyyymmdd=DEFAULT_VALUE_STR4
  CHARACTER(LEN=nlen)      :: p2pPath=DEFAULT_VALUE_STR4
  REAL                     :: latmin=DEFAULT_VALUE_REAL
  REAL                     :: latmax=DEFAULT_VALUE_REAL
  REAL                     :: lonmin=DEFAULT_VALUE_REAL
  REAL                     :: lonmax=DEFAULT_VALUE_REAL
  INTEGER                  :: processMode=DEFAULT_VALUE_INT   ! 0:orbit, 1:daily
  INTEGER                  :: fmType=0
  CHARACTER( LEN = nlen )  :: prodStr ! comma separated product id list (Ex: 'tpw,clw,rr' )
  
  NAMELIST /p2pDepNameList/filesList,satId,yyyymmdd,p2pPath,&
           latmin,latmax,lonmin,lonmax,processMode,fmType,prodStr
  
  !-----------------------------------------------------
  !     P2P identifiers
  !-----------------------------------------------------
  CHARACTER(LEN=nlen)                 :: p2pFile,metaFile
  LOGICAL                             :: metaExist=.FALSE.
  INTEGER                             :: MAX_FILE=28
  INTEGER                             :: MAX_PROFILE=30000
  REAL                                :: var1D = DEFAULT_VALUE_REAL
  REAL, DIMENSION(:,:),   ALLOCATABLE :: var1D_p2p
  


  !-----------------------------------------------------
  !     Execute section begins here
  !-----------------------------------------------------
  
  READ(*,NML=p2pDepNameList)
    
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
 
  call ReadList(iu_list, trim(filesList), inputFiles, nfiles, dumFiles, '', '')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 

  MAX_FILE = nfiles
  MAX_PROFILE = 0
  
  !---- get MAX_FILE and MAX_PROFILE by looping all files
  DO ifile = 1, nfiles
    CALL getDepNprf( inputFiles( ifile ), nprf )
    if( nprf .gt. MAX_PROFILE ) MAX_PROFILE = nprf
  ENDDO
  write(*,*) 'p2pDep MAX_FILE    = ', MAX_FILE
  write(*,*) 'p2pDep MAX_PROFILE = ', MAX_PROFILE

  !------------------------------------------------------------------------------------
  !     Start loop over product list 
  !------------------------------------------------------------------------------------
  prodLoop: DO iprod = 1, NPROD
  cendLoop: DO icend = 1, NCEND

    prodId = TRIM(prodIds(iprod))
    write(*,*)
    write(*,'(A)') 'p2pDep '//TRIM(prodId)//' '//cends(icend)//' ...'
    
    ALLOCATE (var1D_p2p ( MAX_PROFILE, MAX_FILE ) )
    var1D_p2p = -999.0

    !-----------------------------------------------------
    !     Loop over the DEP files
    !-----------------------------------------------------
    FilesLoop: DO ifile=1,nfiles

      !write(*,'(A)') trim(inputFiles(ifile))
    
      !---Read header of EDR file
      CALL ReadHdrDEP(iuDEP,inputFiles(ifile),DEP,nprf)
      !---Loop over the profiles within the file
      ProfilesLoop: DO iprof=1,nPrf
        CALL ReadDEP(iuDEP,DEP,ierr)
 
        IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfilesLoop
        IF (ierr.eq.Warn_readInvalid) CYCLE ProfilesLoop
        IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. DEP file.')
	 
	cend = Dep%node + 1 
	IF( cend .ne. icend ) CYCLE ProfilesLoop
        
	qc = DEP%qc(1)
	chisq = DEP%ChiSq
	lat = DEP%lat
	lon = DEP%lon
	
	if( strcmp(prodId,'lat') .eq. 0 ) then
	  var1D = Dep%lat
	else if( strcmp(prodId,'lon') .eq. 0 ) then
	  var1D = Dep%lon
	else if( strcmp(prodId,'angle') .eq. 0 ) then
	  var1D = Dep%Angle
	else if( strcmp(prodId,'sfc2') .eq. 0 ) then
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
	
        !---- assign
	ok = 0
	if( strcmp(prodId,'lat') .eq. 0 ) then
	  ok = 1
	else if(  qc   .lt. 2      .and. cend .ge. 1 .and. cend .le. 2 &
	    .and. lon  .ge. lonmin .and. lon  .le. lonmax  &
	    .and. lat  .ge. latmin .and. lat  .le. latmax ) then
          ok = 1
	endif
	
	if( ok .eq. 1 ) var1D_p2p ( iprof, ifile ) = var1D
	
      ENDDO ProfilesLoop
    
      !---Close the DEP file
      CLOSE (iuDEP)

    ENDDO FilesLoop

    !---- write out P2P stuff
    p2pFile='P2P_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prodId)//'_'//cends(icend)//'.dat'
    write(*,'(A)')'p2p file='//trim(p2pPath)//trim(p2pFile)
    open(25,file=trim(p2pPath)//trim(p2pFile),form='unformatted',access='direct',recl=4*MAX_FILE*MAX_PROFILE)
    write(25,rec=1)var1D_p2p
    close(25)
  
    !---- deallocate
    IF( ALLOCATED( var1D_p2p ) ) DEALLOCATE( var1D_p2p )

  ENDDO cendLoop
  ENDDO prodLoop
  !------------------------------------------------------------------------------------
  !     End loop over product list 
  !------------------------------------------------------------------------------------

  !---- output a P2P meta data file if not exist yet ( to be used by p2p_mirs_nwp.pro )
  metaFile = TRIM(p2pPath)//'P2P_'//TRIM(satId)//'_'//TRIM(yyyymmdd)//'_meta.txt'
  !inquire(file=metaFile,exist=metaExist)
  !if( .NOT. metaExist ) then
    open( 25,file = metaFile)
    write(25,*)MAX_FILE
    write(25,*)MAX_PROFILE
    close(25)
  !endif
    

  DEALLOCATE(inputFiles)
  DEALLOCATE(dumFiles)

end program p2pDep

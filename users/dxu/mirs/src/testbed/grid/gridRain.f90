!***************************************************************************************************
!
! To Grid Rain from Dep files
!
!       Wanchun Chen       10/20/2008
!       Flavio             10/24/2008
!
!***************************************************************************************************

Program GridRain

  USE Consts
  USE misc
  USE IO_DEP
  USE IO_Misc
  USE ErrorHandling
 
  IMPLICIT NONE
  
  !---INTRINSIC functions
  INTRINSIC :: ABS,COS,INDEX,INT,MOD,TRIM,ALLOCATED
  
  INTEGER            :: iu_list=20,iuDEP,ierr,iprof,nprf
  INTEGER, PARAMETER :: LEN_FILE=250
  INTEGER            :: ifile, nfiles
  INTEGER            :: icol, jrow
  INTEGER            :: indx_doy,Year,Month,Day,JDay,DOY_file
  CHARACTER(LEN=LEN_FILE), DIMENSION(:),       POINTER     :: inputFiles, dumFiles 
  
  !---Structures
  TYPE(DEP_type)     :: Dep
  
  INTEGER, PARAMETER :: NSAT=3
  INTEGER            :: isat
  INTEGER            :: INT_SATID=SENSOR_ID_N18
  INTEGER            :: NCOL,NROW,NROWQG
  INTEGER            :: NTIME=8
  
  !----namelist data
  CHARACTER(LEN=LEN_FILE),DIMENSION(NSAT) :: filesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=64)                       :: yyyymmdd=DEFAULT_VALUE_STR4
  CHARACTER(LEN=20)                       :: satId=DEFAULT_VALUE_STR4  
  REAL                                    :: gridfactor=DEFAULT_VALUE_INT
  CHARACTER(LEN=LEN_FILE)                 :: gridPath=DEFAULT_VALUE_STR4
  REAL                                    :: latmin=DEFAULT_VALUE_REAL
  REAL                                    :: latmax=DEFAULT_VALUE_REAL
  REAL                                    :: lonmin=DEFAULT_VALUE_REAL
  REAL                                    :: lonmax=DEFAULT_VALUE_REAL
  INTEGER                                 :: filling=0
  
  NAMELIST /GridRainNameList/filesList,satId,yyyymmdd,gridfactor,gridPath,latmin,latmax,lonmin,lonmax,filling

  !-------------------------------------------------------------------------------
  !     Output variables identifiers definiton section, commented part from DEP
  !-------------------------------------------------------------------------------
  INTEGER, DIMENSION(:,:), ALLOCATABLE   :: num_grid
  INTEGER, DIMENSION(:,:), ALLOCATABLE   :: num_grid24z
  REAL, DIMENSION(:,:), ALLOCATABLE      :: intrr_grid,avgrr_grid,tmprr_grid
  REAL, DIMENSION(:,:), ALLOCATABLE      :: intrr_grid24z,tmprr_grid24z
  REAL, DIMENSION(:,:,:), ALLOCATABLE    :: rr_grid,time_grid
  REAL, DIMENSION(:,:,:), ALLOCATABLE    :: rr_grid24z,time_grid24z

  CHARACTER(LEN=LEN_FILE) :: gridFile
  !-----------------------------------------------------
  !     grid and fill in stuff goes here
  !-----------------------------------------------------
  INTEGER     :: NUMSPOT_A=30
  INTEGER     :: LATLIM_A=12
  REAL        :: FIX=0.5
  
  REAL        :: loncorr
  INTEGER     :: ifov
  REAL        :: lonleft, lonright
  INTEGER     :: gridlon_left, gridlon_right
  INTEGER     :: gridlat_bot, gridlat_top
  
  INTEGER :: lonbox, latbox, itime, itime24z
  INTEGER :: itime_max = 0
  
  REAL, DIMENSION(:), ALLOCATABLE :: fov_size_A

  !-----------------------------------------------------
  !     Execute section begins here
  !-----------------------------------------------------
  
  READ(*,NML=GridRainNameList)
  
  IF(filling .eq. 1 ) NTIME=100
  !-----------------------------------------------------
  !     Allocate output array and initialize them
  !-----------------------------------------------------
  NCOL=int(360*gridfactor)
  NROW=int(180*gridfactor)
  NROWQG=int(120*gridfactor)
  
  ALLOCATE (num_grid( 0:NCOL-1, 0:NROW-1 ) )
  ALLOCATE (num_grid24z( 0:NCOL-1, 0:NROW-1 ) )
  ALLOCATE (time_grid( 0:NCOL-1, 0:NROW-1, NTIME ) )
  ALLOCATE (time_grid24z( 0:NCOL-1, 0:NROW-1, NTIME ) )
  ALLOCATE (rr_grid( 0:NCOL-1, 0:NROW-1, NTIME ) )
  ALLOCATE (rr_grid24z( 0:NCOL-1, 0:NROW-1, NTIME ) )
  ALLOCATE (intrr_grid( 0:NCOL-1, 0:NROW-1 ) )
  ALLOCATE (intrr_grid24z( 0:NCOL-1, 0:NROW-1 ) )
  ALLOCATE (avgrr_grid( 0:NCOL-1, 0:NROW-1 ) )
  ALLOCATE (tmprr_grid( 0:NCOL-1, 0:NROW-1 ) )
  ALLOCATE (tmprr_grid24z( 0:NCOL-1, 0:NROW-1 ) )

  num_grid    = 0
  num_grid24z = 0
  time_grid    = -999.0
  time_grid24z = -999.0
  rr_grid    = -999.0
  rr_grid24z = -999.0
  intrr_grid    = 0.0
  intrr_grid24z = 0.0
  avgrr_grid = 0.0
  tmprr_grid    = 0.0
  tmprr_grid24z = 0.0
  itime    = 0
  itime24z = 0
  
  !---Convert day to Jday
!  indx_doy=index(yyyymmdd,'201',back=.true.)
  indx_doy=1
  read(yyyymmdd(indx_doy:indx_doy+3),*) Year
  read(yyyymmdd(indx_doy+4:indx_doy+5),*) Month
  read(yyyymmdd(indx_doy+6:indx_doy+7),*) Day
  print*,'Year:',Year,' Month:',Month,' Day:',Day
  CALL compJulDay(Year,Month,Day,JDay)
  
  !-----------------------------------------------------
  !     Loop over the satellites
  !-----------------------------------------------------
  !--- isat_max=NSAT-1 removes F16 and isat_max=NSAT considers F16
  SatellitesLoop: DO isat=1, NSAT
     IF ( isat .EQ. 1 ) INT_SATID = SENSOR_ID_N18
     IF ( isat .EQ. 2 ) INT_SATID = SENSOR_ID_METOPA
     IF ( isat .EQ. 3 ) INT_SATID = SENSOR_ID_F16
     !---- Compute FOV_A size for filling purpose
     IF(INT_SATID .EQ. SENSOR_ID_NPP) THEN
        NUMSPOT_A = 32
        ALLOCATE(fov_size_A(0:31))
     ENDIF
     IF (.NOT.ALLOCATED(fov_size_A)) ALLOCATE(fov_size_A(0:29))

     call FOV_A(fov_size_A,NUMSPOT_A,INT_SATID)

     !---- to handle F16/F17/F18 SSMIS differently
     if ( isat .eq. SENSOR_ID_F16 .or. isat .eq. SENSOR_ID_F18 ) fov_size_A = 0.6745 
     if ( isat .eq. SENSOR_ID_F17 ) fov_size_A = 0.6745 
    
     call ReadList(iu_list, trim(filesList(isat)), inputFiles, nfiles, dumFiles, '', '')
     IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 
     print*,''
     !-----------------------------------------------------
     !     Loop over the DEP files
     !-----------------------------------------------------
     FilesLoop: DO ifile=1,nfiles
        print *, 'isat:',isat, 'ifile',ifile, trim(inputFiles(ifile))
        indx_doy=index(inputFiles(ifile),'DEP_',back=.true.)
        read(inputFiles(ifile)(indx_doy+13:indx_doy+15),*) DOY_file

        !---Read header of DEP file
        CALL ReadHdrDEP(iuDEP,inputFiles(ifile),Dep,nprf)

        !---Loop over the profiles within the file
        ProfilesLoop: DO iprof=1,nPrf

           CALL ReadDep(iuDEP,Dep,ierr)

           IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfilesLoop
           IF (ierr.eq.Warn_readInvalid) CYCLE ProfilesLoop

           IF( Dep%lat .GE. latmin .AND. Dep%lat .LE. latmax .AND. Dep%lon .GE. lonmin .AND. Dep%lon .LE. lonmax ) THEN
          
              IF( FILLING .EQ. 1 ) THEN  
                 loncorr=abs(1/cos(PI*Dep%lat/180.0))
                 if ( loncorr .gt. 200 ) loncorr=200

                 ifov = MOD( iprof, NUMSPOT_A )

                 lonleft  = Dep%lon - 0.5 * fov_size_A(ifov) * loncorr
                 lonright = Dep%lon + 0.5 * fov_size_A(ifov) * loncorr
                 gridlon_left  = INT((lonleft  + 180.0) * gridfactor + fix)
                 gridlon_right = INT((lonright + 180.0) * gridfactor + fix)

                 if ( gridlon_left   .lt. 0     )  gridlon_left=0
                 if ( gridlon_left   .ge. NCOL  )  gridlon_left=NCOL-1
                 if ( gridlon_right  .lt. 0     )  gridlon_right=0
                 if ( gridlon_right  .ge. NCOL  )  gridlon_right=NCOL-1
               
                 if ( abs(ifov-(NUMSPOT_A-1.)/2.) .lt. (LATLIM_A - 0.4 )) then
                    gridlat_bot = INT((Dep%lat+90) * gridfactor)
                    gridlat_top = gridlat_bot + 1 
                 else 
                    gridlat_bot = INT((Dep%lat+90.0) * gridfactor - 1 + fix)
                    gridlat_top = INT((Dep%lat+90.0) * gridfactor + 1 + fix)
                 endif

                 if ( isat .eq. SENSOR_ID_F16 .or. isat .eq. SENSOR_ID_F17 .or. isat .eq. SENSOR_ID_F18 ) then
                    gridlat_bot = INT((Dep%lat+90.0) * gridfactor - 1 + fix)
                    gridlat_top = INT((Dep%lat+90.0) * gridfactor + 1 + fix)
                 endif

                 if ( gridlat_bot .lt. 0 ) gridlat_bot=0
                 if ( gridlat_top .lt. 0 ) gridlat_top=0

                 if ( gridlat_top .ge. nrow ) gridlat_top=NROW-1
                 if ( gridlat_bot .ge. nrow ) gridlat_bot=NROW-1
                 !---For a time-period of 00:00-12:00 UTC (current day). .
                 IF ( (Dep%qc(1) .GE. 0) .AND. (Dep%qc(1) .lt. 2) .AND.(DOY_file .EQ. JDay) &
                      .AND. (DEP%scanUTC .LT. (12*3600)) ) THEN
                    do latbox=gridlat_bot,  gridlat_top
                       do lonbox=gridlon_left, gridlon_right
                             num_grid(lonbox,latbox) = num_grid(lonbox,latbox) + 1
                             itime = num_grid(lonbox,latbox)
                             IF(itime .GT. NTIME) THEN
                                write(*,*)'itime is too large=', itime
                                stop
                             ENDIF
                             time_grid(lonbox,latbox,itime) = DEP%scanUTC + 12*3600
                             rr_grid(lonbox,latbox,itime)   = DEP%RR
                             IF(rr_grid(lonbox,latbox,itime) .LT. 0.0) rr_grid(lonbox,latbox,itime) = 0.0
                             CALL SortRR(rr_grid(lonbox,latbox,:),time_grid(lonbox,latbox,:),num_grid(lonbox,latbox))
                             CALL IntegrateRR(rr_grid(lonbox,latbox,:),time_grid(lonbox,latbox,:),num_grid(lonbox,latbox), &
                                  intrr_grid(lonbox,latbox))
                             CALL AverageRR(rr_grid(lonbox,latbox,:),time_grid(lonbox,latbox,:),num_grid(lonbox,latbox), &
                                  avgrr_grid(lonbox,latbox))
                       enddo
                    enddo
                 ENDIF
                 !---For a time-period of 12:00-00:00 UTC (previous day)
                 IF ( (Dep%qc(1) .GE. 0) .AND. (Dep%qc(1) .lt. 2) .AND.(DOY_file .EQ. JDay-1) &
                      .AND. (DEP%scanUTC .GE. (12*3600)) ) THEN
                    do latbox=gridlat_bot,  gridlat_top
                       do lonbox=gridlon_left, gridlon_right
                             num_grid(lonbox,latbox) = num_grid(lonbox,latbox) + 1
                             itime = num_grid(lonbox,latbox)
                             IF(itime .GT. NTIME) THEN
                                write(*,*)'itime is too large=', itime
                                stop
                             ENDIF
                             time_grid(lonbox,latbox,itime) = DEP%scanUTC - 12*3600
                             rr_grid(lonbox,latbox,itime)   = DEP%RR
                             IF(rr_grid(lonbox,latbox,itime) .LT. 0.0) rr_grid(lonbox,latbox,itime) = 0.0
                             CALL SortRR(rr_grid(lonbox,latbox,:),time_grid(lonbox,latbox,:),num_grid(lonbox,latbox))
                             CALL IntegrateRR(rr_grid(lonbox,latbox,:),time_grid(lonbox,latbox,:),num_grid(lonbox,latbox), &
                                  intrr_grid(lonbox,latbox))
                             CALL AverageRR(rr_grid(lonbox,latbox,:),time_grid(lonbox,latbox,:),num_grid(lonbox,latbox), &
                                  avgrr_grid(lonbox,latbox))
                       enddo
                    enddo
                 ENDIF
                 !---For a time-period of 00:00-24:00 UTC (current day)
                 IF ( (Dep%qc(1) .GE. 0) .AND. (Dep%qc(1) .lt. 2) .AND.(DOY_file .EQ. JDay) ) THEN
                    do latbox=gridlat_bot,  gridlat_top
                       do lonbox=gridlon_left, gridlon_right
                             num_grid24z(lonbox,latbox) = num_grid24z(lonbox,latbox) + 1
                             itime24z = num_grid24z(lonbox,latbox)
                             IF(itime24z .GT. NTIME) THEN
                                write(*,*)'itime_24z is too large=', itime24z
                                stop
                             ENDIF
                             time_grid24z(lonbox,latbox,itime24z) = DEP%scanUTC
                             rr_grid24z(lonbox,latbox,itime24z)   = DEP%RR
                             IF(rr_grid24z(lonbox,latbox,itime24z) .LT. 0.0) rr_grid24z(lonbox,latbox,itime24z) = 0.0
                             CALL SortRR(rr_grid24z(lonbox,latbox,:),time_grid24z(lonbox,latbox,:),num_grid24z(lonbox,latbox))
                             CALL IntegrateRR(rr_grid24z(lonbox,latbox,:),time_grid24z(lonbox,latbox,:),&
                                  num_grid24z(lonbox,latbox), intrr_grid24z(lonbox,latbox))
                       enddo
                    enddo
                 ENDIF

              ELSE   ! NO FILLING
                 lonbox = INT( (Dep%lon+180.0) * gridfactor )
                 latbox = INT( (Dep%lat+90.0)  * gridfactor )

                 if( lonbox .lt. 0    ) lonbox=0
                 if( lonbox .ge. NCOL ) lonbox=NCOL-1
                 if( latbox .lt. 0    ) latbox=0
                 if( latbox .ge. NROW ) latbox=NROW-1

                 if( Dep%qc(1) .ge. 0 .and. Dep%qc(1) .lt. 2 ) then
                    num_grid(lonbox,latbox) = num_grid(lonbox,latbox) + 1
                    itime = num_grid(lonbox,latbox)
                    if(itime .gt. itime_max) itime_max = itime
                    IF(itime .GT. NTIME) THEN
                       write(*,*)'itime is too large=', itime
                       stop
                    ENDIF
                    time_grid(lonbox,latbox,itime) = DEP%scanUTC
                    rr_grid(lonbox,latbox,itime)   = DEP%RR

                    CALL SortRR(rr_grid(lonbox,latbox,:),time_grid(lonbox,latbox,:),num_grid(lonbox,latbox))
                    CALL IntegrateRR(rr_grid(lonbox,latbox,:),time_grid(lonbox,latbox,:),num_grid(lonbox,latbox), &
                         intrr_grid(lonbox,latbox))
                 endif
              ENDIF

           ENDIF

        ENDDO ProfilesLoop
        !---Close the DEP file
        CLOSE (iuDEP)
      
     ENDDO FilesLoop

     !---Release memory
     DEALLOCATE(inputFiles)
     DEALLOCATE(dumFiles)
     DEALLOCATE(fov_size_A)

  ENDDO SatellitesLoop

  !--Generation of Global files (-180<=Lon<=180 and -90<=Lat<=90)
  !---For time-period of 12:00-12:00 UTC
!  gridFile='GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rrday_intg_glb_ad.dat'
!  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
!  write(25,rec=1)intrr_grid(0:NCOL-1, 0:NROW-1)
!  close(25)

!  gridFile='GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rrday_avg_glb_ad.dat'
!  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
!  write(25,rec=1)avgrr_grid(0:NCOL-1, 0:NROW-1)
!  close(25)

  gridFile='GRID_1212Z_0025D_'//trim(yyyymmdd)//'_rrday_intg_glb_ad.dat'
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)intrr_grid(0:NCOL-1, 0:NROW-1)
  close(25)

  !---For time-period of 00:00-24:00 UTC
  gridFile='GRID_0024Z_0025D_'//trim(yyyymmdd)//'_rrday_intg_glb_ad.dat'
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROW)
  write(25,rec=1)intrr_grid24z(0:NCOL-1, 0:NROW-1)
  close(25)

  !-Swapping the longitude from (-180,180) to (0,360)
  tmprr_grid=intrr_grid
  tmprr_grid24z=intrr_grid24z
  DO jrow=0,NROW-1
     DO icol=0,NCOL-1
        !---For time-period of 12:00-12:00 UTC
        IF(icol .LT. 720 ) intrr_grid(icol,jrow)=tmprr_grid(icol+720,jrow)
        IF(icol .GE. 720 ) intrr_grid(icol,jrow)=tmprr_grid(icol-720,jrow)
        !---For time-period of 00:00-24:00 UTC
        IF(icol .LT. 720 ) intrr_grid24z(icol,jrow)=tmprr_grid24z(icol+720,jrow)
        IF(icol .GE. 720 ) intrr_grid24z(icol,jrow)=tmprr_grid24z(icol-720,jrow)
     ENDDO
  ENDDO
  tmprr_grid=avgrr_grid
  DO jrow=0,NROW-1
     DO icol=0,NCOL-1
        IF(icol .LT. 720 ) avgrr_grid(icol,jrow)=tmprr_grid(icol+720,jrow)
        IF(icol .GE. 720 ) avgrr_grid(icol,jrow)=tmprr_grid(icol-720,jrow)
     ENDDO
  ENDDO
  !--Generation of SemiGlobal files (0<=Lon<=360 and -60<=Lat<=60)
  !---For time-period of 12:00-12:00 UTC
!  gridFile='MIRS_IPWG_'//trim(satId)//'_'//trim(yyyymmdd)//'_rrday_intg_ad.dat'
!  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROWQG)
!  write(25,rec=1)intrr_grid(0:NCOL-1,120:599)
!  close(25)

  gridFile='MIRS_1212Z_0025D_'//trim(yyyymmdd)//'_rrday_intg_ad.dat'
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROWQG)
  write(25,rec=1)intrr_grid(0:NCOL-1,120:599)
  close(25)

!  gridFile='MIRS_IPWG_'//trim(satId)//'_'//trim(yyyymmdd)//'_rrday_avg_ad.dat'
!  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROWQG)
!  write(25,rec=1)avgrr_grid(0:NCOL-1,120:599)
!  close(25)

  !---For time-period of 00:00-24:00 UTC
  gridFile='MIRS_0024Z_0025D_'//trim(yyyymmdd)//'_rrday_intg_ad.dat'
  open(25,file=trim(gridPath)//gridFile,form='unformatted', access='direct', recl=4*NCOL*NROWQG)
  write(25,rec=1)intrr_grid24z(0:NCOL-1,120:599)
  close(25) 


  
  DEALLOCATE(num_grid)
  DEALLOCATE(num_grid24z)
  DEALLOCATE(time_grid)
  DEALLOCATE(time_grid24z)
  DEALLOCATE(rr_grid)
  DEALLOCATE(rr_grid24z)
  DEALLOCATE(intrr_grid)
  DEALLOCATE(intrr_grid24z)
  DEALLOCATE(avgrr_grid)
  DEALLOCATE(tmprr_grid)
  DEALLOCATE(tmprr_grid24z)
 
  write(*,*) 'itime_max=', itime_max

CONTAINS

!===============================================================
! Name:         SortRR
!
! Type:         Subroutine
!
!
! Description:  Performs sorting of rain rate by time
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - rr                 I              Rain rate array
!       - time               I              Time array
!       - num                I              Number of valid array elements
!
! Modules needed:
!       - None
!
!
! History:
!       10-10-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE SortRR(rr,time,num)
    !---Input/Output variables
    INTEGER              :: num
    REAL, DIMENSION(:)   :: rr,time
    !---Local variables
    INTEGER              :: itime,jtime,iptr
    REAL                 :: temp_time,temp_rr

    DO itime = 1,num-1
       iptr = itime
       DO jtime = itime+1,num
          IF(time(jtime) .LT. time(iptr)) iptr = jtime
       ENDDO
       IF (itime .NE. iptr) THEN
          temp_time=time(itime)
          temp_rr=rr(itime)
          time(itime)=time(iptr)
          rr(itime)=rr(iptr)
          time(iptr)=temp_time
          rr(iptr)=temp_rr
       ENDIF
    ENDDO
  END SUBROUTINE SortRR

!===============================================================
! Name:         IntegrateRR
!
! Type:         Subroutine
!
!
! Description:  Performs computation of Rain Rate in mm/day
!               using integration of Rain Rain samples at an
!               specific grid point.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - rr                 I              Rain rate array
!       - time               I              Time array
!       - num                I              Number of valid array elements
!       - intrr              O              Integrated Rain Rate (mm/day)
!
! Modules needed:
!       - None
!
!
! History:
!       10-10-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE IntegrateRR(rr,time,num,intrr)
    !---Input/Output variables
    INTEGER, INTENT(IN)             :: num
    REAL, DIMENSION(:), INTENT(IN)  :: rr,time
    REAL                            :: intrr
    !---Local variables
    INTEGER, PARAMETER              :: Tmax=24,Tsh=3600
    INTEGER                         :: ntim
    INTEGER                         :: itime
    REAL, DIMENSION(:), ALLOCATABLE :: rrs,times
    REAL, DIMENSION(:), ALLOCATABLE :: Dtime,Ts

    ntim=num
    IF (num .GT. 1 ) THEN
       DO itime=1,num-1
          IF( ABS(time(itime) - time(itime+1)) .lt. epsilon ) ntim=ntim-1
       ENDDO
       ALLOCATE(rrs(ntim),times(ntim),Dtime(ntim),Ts(ntim+1))       
       Dtime = 0.0
       Ts = 0.0
       rrs=0.0
       times=0.0
       CALL FormatRR(rr,time,num,rrs,times)
       Ts(1)=0
       Ts(ntim+1)=Tmax
       DO itime = 2,ntim
          Ts(itime) = (times(itime)+times(itime-1))/(2*Tsh)
       ENDDO
       intrr=0
       DO itime=1,ntim
          Dtime(itime)= Ts(itime+1)-Ts(itime)
          intrr=rrs(itime)*Dtime(itime)+intrr
       ENDDO
       DEALLOCATE(rrs,times)
       DEALLOCATE(Dtime,Ts)
    ENDIF
    IF(num .EQ. 1) THEN
       intrr=rr(1)*Tmax
    ENDIF
    IF( intrr .LT. 0) intrr=0

  END SUBROUTINE IntegrateRR

!===============================================================
! Name:         AverageRR
!
! Type:         Subroutine
!
!
! Description:  Performs computation of Rain Rate in mm/day
!               using the average of Rain Rain samples at an
!               specific grid point.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - rr                 I              Rain rate array
!       - time               I              Time array
!       - num                I              Number of valid array elements
!       - avgrr              O              Average Rain rate (mm/day)
!
! Modules needed:
!       - None
!
!
! History:
!       10-10-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE AverageRR(rr,time,num,avgrr)
    !---Input/Output variables
    INTEGER, INTENT(IN)             :: num
    REAL, DIMENSION(:), INTENT(IN)  :: rr,time
    REAL                            :: avgrr
    !---Local variables
    INTEGER, PARAMETER     :: Tmax=24
    INTEGER                :: itime

    avgrr=0
    DO itime=1,num
       avgrr=rr(itime)+avgrr
    ENDDO
    avgrr=(avgrr/num)*Tmax
    IF( avgrr .LT. 0) avgrr=0
  END SUBROUTINE AverageRR

!===============================================================
! Name:         FormatRR
!
! Type:         Subroutine
!
!
! Description:  Sort Rain rate arrays based on the time array.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - rr                 I              Rain rate array (mm/hr)
!       - time               I              Time array (sec)
!       - num                I              Number of valid array elements
!       - rrs                O              Formated Rain rate array (mm/hr)
!       - times              O              Formated Time rate array (sec)
!
! Modules needed:
!       - None
!
!
! History:
!       10-16-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE FormatRR(rr,time,num,rrs,times)
    !---Input/Output variables
    INTEGER, INTENT(IN)             :: num
    REAL, DIMENSION(:), INTENT(IN)  :: rr,time
    REAL, DIMENSION(:)              :: rrs,times   
    !---Local variables
    INTEGER                :: irr,icnt,cnt
    INTEGER                :: itime
       cnt=0
       irr=1
       FormatLoop: DO itime=1,num-1
          IF( ABS(time(itime) - time(itime+1)) .lt. epsilon ) THEN
             cnt=cnt+1

             IF(itime+1 .EQ. num) THEN
                IF(cnt .GT. 0) THEN
                   DO icnt=0,cnt
                      rrs(irr)=rr(itime-cnt+1+icnt)+rrs(irr)
                   ENDDO
                   rrs(irr)=rrs(irr)/(cnt+1)
                   times(irr)=time(itime)
                   cnt=0
                ENDIF
             ENDIF
          ELSE
             IF(cnt .EQ. 0 .AND. itime+1 .NE. num) THEN
                rrs(irr)=rr(itime)
                times(irr)=time(itime)
                irr=irr+1
             ENDIF
             IF(cnt .EQ. 0 .AND. itime+1 .EQ. num) THEN
                rrs(irr)=rr(itime)
                rrs(irr+1)=rr(itime+1)
                times(irr)=time(itime)
                times(irr+1)=time(itime+1)
             ENDIF
             IF(cnt .GT. 0 .AND. itime+1 .NE. num) THEN
                DO icnt=0,cnt
                   rrs(irr)=rr(itime-cnt+icnt)+rrs(irr)
                ENDDO
                rrs(irr)=rrs(irr)/(cnt+1)
                times(irr)=time(itime)
                irr=irr+1
                cnt=0
             ENDIF
             IF(cnt .GT. 0 .AND. itime+1 .EQ. num) THEN
                DO icnt=0,cnt
                   rrs(irr)=rr(itime-cnt+icnt)+rrs(irr)
                ENDDO
                rrs(irr)=rrs(irr)/(cnt+1)
                times(irr)=time(itime)
                rrs(irr+1)=rr(itime+1)
                times(irr+1)=time(itime+1)
                cnt=0
             ENDIF
          ENDIF
       ENDDO FormatLoop

  END SUBROUTINE FormatRR


end program gridRain

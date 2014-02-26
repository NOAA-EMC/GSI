!$Id: misc.f90 3276 2013-03-20 21:44:46Z chrisg $
!-----------------------------------------------------------------------------------------------
! Name:         misc
! 
! Type:       F90 module
!
! Description:
!       Module that contains several routines of different functions
!       and several applications.
!
! Modules needed:
!       - Consts
!
! Subroutines contained:
!       - Get_lun
!       - ShrkMatrx
!       - ShrkVect
!       - printSummary
!       - deg2rad
!       - rad2deg
!       - TransfSigma2Press
!       - ReadList 
!       - plank 
!       - cnode
!       - Linear
!       - compJulDay
!       - Translate2I1
!       - Translate2I2
!       - Translate2I4
!       - Stdev
!       - Mean
!       - read_topography
!       - day_month
!       - REMOVESIGN
!       - DetermGeogrLimits
!       - CountClassTypes
!       - EstimatSfcPress
!       - SatZenAng2ViewAng
!
! Data type included:
!       - none
! 
! History:
!        2006   S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE misc
  USE Consts
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Get_lun,ShrkMatrx,ShrkVect,printSummary,deg2rad,rad2deg
  PUBLIC :: TransfSigma2Press,plank,cnode,Linear,compJulDay
  PUBLIC :: Translate2I1,Translate2I2,Translate2I4,Stdev,Mean
  PUBLIC :: read_topography,day_month,REMOVESIGN,DetermGeogrLimits
  PUBLIC :: CountClassTypes,EstimatSfcPress,SatZenAng2ViewAng
  PUBLIC :: fov_a, strcmp, insert_path_string, replace_path_string, replace_path_string_mt, get_fov_size

  INTRINSIC :: EXP,LEN_TRIM,ABS,ATAN,SIN,ASIN,SQRT,REAL,INT,LOG,IACHAR,SIZE,SUM,DBLE,ICHAR,SNGL,MOD
  
CONTAINS


!===============================================================
! Name:         EstimatSfcPress
!
!
! Type:         Subroutine
!
!
! Description:  Estimation of the surface pressure from the 
!               altitude info (only over non-ocean)
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - xalt               I             Altitude
!       - SfcClass           I             Surface class/type
!       - SfcPress           I/O           Surface pressure
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE EstimatSfcPress(xalt,SfcClass,SfcPress)
    REAL      :: xalt,SfcPress,SfcP0=1013.,Z0=0.,T0=280.
    INTEGER   :: SfcClass
    IF (SfcClass .eq. LD_TYP .or. SfcClass .eq. SNOW_TYP) THEN
       SfcPress=SfcP0*exp(-(STANDARD_GRAVITY*(xalt-Z0))/(DryAirConst*T0))
    ENDIF
    RETURN
  END SUBROUTINE EstimatSfcPress


!===============================================================
! Name:         CountClassTypes
!
!
! Type:         Subroutine
!
!
! Description:  Counts the incremented number of class types within
!               a file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SfcClass            I            Surface class
!       - nProfsOverOcean     I/O          #profiles over ocean
!       - nProfsOverSeaIce    I/O          #profiles over sea ice
!       - nProfsOverLand      I/O          #profiles over land
!       - nProfsOverSnow      I/O          #profiles over snow
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE CountClassTypes(SfcClass,nProfsOverOcean,nProfsOverSeaIce,nProfsOverLand,nProfsOverSnow)
    INTEGER :: SfcClass,nProfsOverOcean,nProfsOverSeaIce,nProfsOverLand,nProfsOverSnow
    IF (SfcClass .eq. OC_TYP)     nProfsOverOcean  = nProfsOverOcean+1
    IF (SfcClass .eq. SEAICE_TYP) nProfsOverSeaIce = nProfsOverSeaIce+1
    IF (SfcClass .eq. LD_TYP)     nProfsOverLand   = nProfsOverLand+1
    IF (SfcClass .eq. SNOW_TYP)   nProfsOverSnow   = nProfsOverSnow+1
    RETURN
  END SUBROUTINE CountClassTypes



!===============================================================
! Name:         DetermGeogrLimits
!
!
! Type:         Subroutine
!
!
! Description:  Determines a logical signal that says if the 
!               particular profile (with its lat, lon, pass, type)
!               pass the test.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Ok                 O            Logical signal (within geogr limits or not)
!       - GeogrLimits        I            Flag of requested geogr limits (all, 
!                                         ocean-only, land-only, within lat/lon box)
!       - stypeSfc           I            Surface type of partic. profile
!       - lat                I            Latitude of partic. profile
!       - lon                I            Longitude of partic. profile
!       - minLat             I            Minimum latitude of lat/lon box
!       - maxLat             I            Maximum latitude of lat/lon box
!       - minLon             I            Minimum Longitude of lat/lon box
!       - maxLon             I            Maximum Longitude of lat/lon box
!       - pass               I            Requested orbit mode to select
!       - node               I            Actual orbit mode of partic. profile
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE  DetermGeogrLimits(Ok,GeogrLimits,stypeSfc,lat,lon,minLat,maxLat,minLon,maxLon,pass,node)
    LOGICAL :: OK
    INTEGER :: GeogrLimits,stypeSfc,pass,node
    REAL    :: lat,lon,minLat,maxLat,minLon,maxLon
    Ok =.TRUE.
    !---Process data only in [lat-lon] box
    IF (GeogrLimits .eq. 1 .and. stypeSfc .ge. 0) THEN 
       IF( (lat .lt. minLat) .or. (lat .gt. maxLat) .or. (lon .lt. minLon) .or. (lon .gt. maxLon) ) Ok=.FALSE.
    !---Process only data over ocean
    ELSE IF (GeogrLimits .eq. 2 .and. stypeSfc .ge. 0) THEN 
       IF (stypeSfc .ne. OC_TYP) Ok=.FALSE.
    !---Process only data over land
    ELSE IF (GeogrLimits .eq. 3 .and. stypeSfc .ge. 0) THEN
       IF (stypeSfc .ne. LD_TYP) Ok=.FALSE.
    ENDIF
    !---Process according to node/pass
    IF (pass.ne.2 .and. pass.ne.node) Ok=.FALSE.
    RETURN
  END SUBROUTINE DetermGeogrLimits


!===============================================================
! Name:         day_month
!
!
! Type:         Subroutine
!
!
! Description:  Converts the year and julian day information into
!               a regular calendar day and month information
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - year               I               year
!       - month              O               month
!       - day                O               calendar day within a month
!       - jul_day            I               Julian day
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!       11-04-2008      Wanchun Chen  fixed bug for leap year
!                                     removed go to statement
!
!===============================================================

    subroutine day_month(year,month,day,jul_day)
    !---Input/Output variables
    INTEGER year,month,day,jul_day,sum1,i
    !---Local variables
    INTEGER, SAVE, DIMENSION(12) :: day1=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    INTEGER, SAVE, DIMENSION(12) :: day2=(/31,29,31,30,31,30,31,31,30,31,30,31/)
    sum1=0
    month=0
    monthLoop: DO i=1,12
       if( ( MOD(year,4).eq.0 .and. MOD(year,100).ne.0 ) .or. MOD(year,400).eq.0 ) then
          sum1=sum1+day2(i)
       else
          sum1=sum1+day1(i)
       endif
       if(jul_day.le.sum1) then
         month = i
         exit monthLoop
       endif
    ENDDO monthLoop

    if(( MOD(year,4).eq.0 .and. MOD(year,100).ne.0 ) .or. MOD(year,400).eq.0) then
       day=jul_day-(sum1-day2(i))
    else
       day=jul_day-(sum1-day1(i))
    endif
    
    return
  END subroutine day_month


!===============================================================
! Name:         compJulDay
!
!
! Type:         Subroutine
!
!
! Description:  Computes the Julian day from the month, day, year
!               information
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - year               I              Year
!       - month              I              Month
!       - day                I              Calendar day
!       - jul_day            O              Julian day
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!       11-04-2008      Wanchun Chen  fixed bug for leap year
!
!===============================================================

  subroutine compJulDay(year,month,day,jul_day)
    !---Input/Output variables
    INTEGER year,month,day,jul_day
    !---Local variables
    INTEGER                      :: sum1,i
    INTEGER, SAVE, DIMENSION(12) :: day1=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    INTEGER, SAVE, DIMENSION(12) :: day2=(/31,29,31,30,31,30,31,31,30,31,30,31/)
    sum1=0
    DO i=1,month-1
       if( ( MOD(year,4).eq.0 .and. MOD(year,100).ne.0 ) .or. MOD(year,400).eq.0 ) then
          sum1=sum1+day2(i)
       else
          sum1=sum1+day1(i)
       endif
    ENDDO
    jul_day=sum1+day
    return
  END subroutine compJulDay


!===============================================================
! Name:         read_topography
!
!
! Type:         Subroutine
!
!
! Description:  Reads the topography info (once) and gives 
!               local information (altitude, type, etc) for every
!               lat/lon point. There is a mapping vector to give
!               the surface type from the CRTM-like surface type.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - filetopo           I            Name of topography file
!       - xlat               I            Latitude
!       - xlon               I            Longitude
!       - xalt               O            Altitude
!       - stypeSfc           O            Surface type
!       - xcover             O            Surface cover
!       - stypeSfcCRTM       O            CRTM-compatible surface type
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE read_topography(filetopo,xlat,xlon,xalt,stypeSfc,xcover,stypeSfcCRTM)
    !---Input variables
    REAL             :: xlat,xlon
    CHARACTER(LEN=*) :: filetopo
    !---Output variables
    REAL             :: xalt,xcover
    INTEGER          :: stypeSfc,stypeSfcCRTM
    !---Local variables
    REAL(KIND=8)     :: alat,alon,alon1,alt,scover
    INTEGER          :: i,index_lat,index_lon
    CHARACTER(LEN=1) :: I1,I3
    INTEGER(KIND=2)  :: I2
    INTEGER, SAVE    :: init_topo=0,iu

    !---If Lat/Lon information undefined, return undefined stype
    IF ((xlat .le. -999.) .or. (xlon .le. -999.)) THEN
       !stypeSfc =DEFAULT_VALUE_INT !Undefined lat/lon->undefined stype
       return
    ENDIF
    alat = DBLE(xlat)
    alon = DBLE(xlon)
    if(init_topo.eq.0) then
       iu=Get_Lun()
       open(iu,file=fileTopo,form='unformatted',access='direct',RECL=4)
       init_topo=1
    endif
    if( alon .gt. DBLE(180.0) ) then
       alon1=alon-DBLE(360.0)
    else
       alon1=DBLE(alon)
    endif
    index_lat=INT((DBLE(90.0)-alat)*6+1)
    index_lon=INT((DBLE(180)+alon1)*6+1)
    if(index_lat.gt.1080) index_lat=1080
    if(index_lon.gt.2160) index_lon=2160
    i=(index_lat-1)*2160+index_lon
    read(iu,rec=i) I1,I3,I2
    alt          = I2*1.0
    scover       = ICHAR(I3)*0.01
    xalt         = REAL(alt) 
    xcover       = REAL(scover)
    stypeSfcCRTM = ICHAR(I1)
    !---Go from the topography database convention to the MIRS convention
    stypeSfc=MAP_CRTM_2_TYP(stypeSfcCRTM)
    return
  END SUBROUTINE read_topography


!===============================================================
! Name:          Stdev
!
!
! Type:         Function
!
!
! Description:  Computes standard deviation of a vector X
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - x                  I              Vector
!       - Stdev              O              Standard deviation
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION Stdev(x)
    REAL, DIMENSION(:) :: x
    REAL               :: Stdev, Mean
    INTEGER            :: N,I
    N    = SIZE(x)
    Mean = SUM(x(1:N))/N
    Stdev=0.
    DO i=1,N
       Stdev=Stdev+(X(i)-Mean)**2.
    ENDDO
    Stdev=SQRT(Stdev/N)
    RETURN
  END FUNCTION Stdev

!===============================================================
! Name:          Mean
!
!
! Type:         Function
!
!
! Description:  Computes the mean value of a vector x
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - x                  I              Vector
!       - Mean               O              Mean value
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION Mean(x)
    REAL, DIMENSION(:) :: x
    REAL               :: Mean
    INTEGER            :: N
    N    = SIZE(x)
    Mean = SUM(x(1:N))/N
    RETURN
  END FUNCTION Mean

!===============================================================
! Name:         Translate2I4
!
!
! Type:         Function
!
!
! Description:  Converts four one-byte-long characters into an 
!               four-bytes-long Integer
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - numChar           I             Vector of 4 one-byte-long CHAR 
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION Translate2I4(numChar)
    !---Big-Endian to Little-Endian Translation (4 bytes -> integer*4)
    INTEGER(4),       PARAMETER    :: POW=256   
    INTEGER(4)                     :: Translate2I4
    CHARACTER(LEN=1), DIMENSION(4) :: numChar
    INTEGER(4)                     :: I1,I2,I3,I4
    !---------------
    I1=INT(REMOVESIGN(IACHAR(numChar(1))),KIND=4)
    I2=INT(REMOVESIGN(IACHAR(numChar(2))),KIND=4)
    I3=INT(REMOVESIGN(IACHAR(numChar(3))),KIND=4)
    I4=INT(REMOVESIGN(IACHAR(numChar(4))),KIND=4)
    Translate2I4=I1*POW**3+I2*POW**2+I3*POW+I4
    !----------
    !Translate2I4=IACHAR(numChar(1))*256**3+IACHAR(numChar(2))*256**2+&
    !     IACHAR(numChar(3))*256+IACHAR(numChar(4))
    !-----------
    RETURN
  END FUNCTION Translate2I4

!===============================================================
! Name:          Translate2I2
!
!
! Type:         Function
!
!
! Description:  Converts two one-byte-long characters into an 
!               two-bytes-long Integer
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - numChar            I           Vector of 2 one-byte-long CHAR
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION Translate2I2(numChar)
    !---Big-Endian to Little-Endian Translation (2 bytes -> integer)
    INTEGER(2),       PARAMETER    :: POW=256   
    INTEGER(2)                     :: Translate2I2
    CHARACTER(LEN=1), DIMENSION(2) :: numChar
    INTEGER(2)                     :: I1,I2
    !-------------
    I1=INT(REMOVESIGN(IACHAR(numChar(1))),KIND=2)
    I2=INT(REMOVESIGN(IACHAR(numChar(2))),KIND=2)
    Translate2I2=I1*POW+I2
    !--------
    !Translate2I2=IACHAR(numChar(1))*256+IACHAR(numChar(2))
    !--------
    RETURN
  END FUNCTION Translate2I2

!===============================================================
! Name:          Translate2I1
!
!
! Type:         Function
!
!
! Description:  Converts 1 one-byte-long characters into an 
!               one-byte-long Integer
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - numChar           I            Vector of 1 one-byte-long CHAR
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION Translate2I1(numChar)
    !---Big-Endian to Little-Endian Translation (1 bytes -> integer)
    INTEGER                        :: Translate2I1
    CHARACTER(LEN=1), DIMENSION(1) :: numChar
    INTEGER(1)                     :: I1
    !---------------
    I1=INT(REMOVESIGN(IACHAR(numChar(1))),KIND=1)
    Translate2I1=I1
    !----------
    !Translate2I1=IACHAR(numChar(1))
    !----------
    RETURN
  END FUNCTION Translate2I1

!===============================================================
! Name:          REMOVESIGN
!
!
! Type:         Function
!
!
! Description:  Removes the negative sign from an integer
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - I                  I              Integer whose sign to be removed
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  INTEGER FUNCTION REMOVESIGN(I)
    INTEGER :: I
    REMOVESIGN=INT(I)
    IF (I.LT.0) REMOVESIGN=256+INT(I)
  END FUNCTION REMOVESIGN



  REAL FUNCTION plank(r, wvnm)
    !============================================================================================
    !    Purpose:
    !      Returns brightness temperature
    !    
    !    Input variables
    !        wvnm : wavenumber (cm^-1)
    !        r    : radiance (w m^-2 st^-1 cm)
    !      
    !    Output variables
    !        radiance_to_tb : brightness temperature (k)
    !
    !    Internal variables
    !        c1      : w m**-2 cm ster^-1 cm**3
    !        c2      : k cm
    ! 
    !    Record of revisions:
    !      Date           Programmer                 Description of change
    !    =======       ================         ==============================
    !    1998.8         Fuzhong Weng             Original code
    !
    !============================================================================================
    IMPLICIT NONE
    REAL(4), INTENT(IN) :: wvnm, r
    REAL(4) :: c1=1.1909E-8, c2=1.4388
    IF ( wvnm <= 0.0 .OR. r <= 0.0 ) THEN 
       PRINT*, "Wavenumber or radiance is less than or equals zero"
       plank = -999
    ELSE
       plank = c2*wvnm/LOG(c1*wvnm**3./r + 1.0)
    END IF
    RETURN 
  END FUNCTION plank
  
  
  
  SUBROUTINE linear(alat, alon, ix, jy, maxcol, maxrow, resl)
    !============================================================================================
    !    Purpose:
    !      Inteperate obs points into grid points
    !
    !    Input variables
    !      alat: obs Latitude        alon: obs Longitude
    !      maxcol: max column        maxrow: max row 
    !      resl: grid point resolution
    !
    !    Output variables
    !      ix: grid point longitude   jy: grid point latitude
    !
    !    Record of revisions:
    !      Date           Programmer                 Description of change
    !    =======       ================         ==============================
    !    1998.8         Fuzhong Weng             Original code
    !
    !============================================================================================
    IMPLICIT NONE
    REAL(4), INTENT(IN) :: alat, alon, resl
    INTEGER(2), INTENT(IN) :: maxcol, maxrow
    INTEGER(2), INTENT(OUT) :: ix, jy
    REAL(4) :: blat, blon

    blat = (90.0 - alat)
    jy = INT( blat/resl + 1, kind = 2 )
    IF (jy > maxrow) jy = maxrow
    IF (jy < 1) jy = 1
    IF (alon <= 0.0) THEN
       blon = 360 + alon
    ELSE 
       blon = alon
    END IF
    ix = INT( blon/resl + 1, kind = 2 )
    IF (ix < 1) ix = 1
    IF (ix > maxcol) ix = 1       ! put to the other side of grid data
  END SUBROUTINE linear
  
  
  
  SUBROUTINE cnode(alat1, alat2, node)
    !============================================================================================
    !    Purpose:
    !
    !    Input variables
    !      alat2:  current Latitude        
    !      alat1:  previous Latitude
    !
    !    Output variables
    !      node:
    !
    !    Record of revisions:
    !      Date           Programmer                 Description of change
    !    =======       ================         ==============================
    !    1998.8         Fuzhong Weng             Original code
    !
    !============================================================================================
    IMPLICIT NONE
    REAL(4), INTENT(IN) :: alat2, alat1
    INTEGER(2), INTENT(OUT) :: node
    REAL(4) :: diff    
    diff = alat2 - alat1
    node = 99
    IF (diff < 0.0) THEN 
       node = 0  ! Descending (Southbound 7:30 am) 
    ELSE
       node = 1  ! Ascending (Northbound 7:30 pm) 
    END IF
    IF (ABS(diff) > 5.0) node = 99  
  END SUBROUTINE cnode


!===============================================================
! Name:         TransfSigma2Press
!
!
! Type:         Subroutine
!
!
! Description:  Transforms a sigma vertical grid into a pressure 
!               grid.
!
!
! Arguments:
!
!      Name                  Type          Description
!      ---------------------------------------------------
!       - nx                  I            Number of X-direction points
!       - ny                  I            Number of Y-direction points
!       - nz                  I            Number of vertical grid points
!       - ptop                I            Top-Atmosphere pressure assumed
!       - SfcPress            I            Surface pressure
!       - hfsigma             I            Sigma grid
!       - press               O            Pressure grid field (3D)
!       - deltP               I            Delta pressure 3D field
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE TransfSigma2Press(nx,ny,nz,ptop,SfcPress,hfsigma,press,deltP)
    INTEGER                  :: nx,ny,nz
    REAL                     :: ptop
    REAL,   DIMENSION(:)     :: hfsigma
    REAL,   DIMENSION(:,:)   :: SfcPress
    REAL,   DIMENSION(:,:,:) :: press,deltP
    !---Local variables
    INTEGER                  :: i,j,k
    do i=1,nx
       do j=1,ny
          do k=1,nz
             press(i,j,k)= ptop+hfsigma(k)*(Sfcpress(i,j)-ptop)+deltP(i,j,k)
          enddo
       enddo
    enddo
    RETURN
  END SUBROUTINE TransfSigma2Press

!===============================================================
! Name:         deg2rad
!
!
! Type:         Function
!
!
! Description:  Converts degrees to radians
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - angle              I              Angle in degrees
!       - deg2rad            O              Angle in radians
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  REAL FUNCTION deg2rad(angle)
    REAL :: angle
    deg2rad=angle*3.14159/180.
  END FUNCTION deg2rad


!===============================================================
! Name:         rad2deg
!
!
! Type:         Function
!
!
! Description:  Converts radians to degrees
!
!
! Arguments:
!
!      Name                Type             Description
!      ---------------------------------------------------
!       - rad               I               Angle in radians
!       - rad2deg           O               Angle in degrees
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  REAL FUNCTION rad2deg(rad)
    REAL :: rad
    rad2deg=rad*180./3.14159
  END FUNCTION rad2deg


!===============================================================
! Name:          Get_Lun
!
!
! Type:          Function
!
!
! Description:  Gets automatically a unit number by making sure 
!               the unit number is not taken already. Eliminates
!               the risk of having unit numbers confused in many
!               places and avoids having to declare the unit numbers 
!               in a hard coded manner. Inherited from CRTM.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!      - get_lun             O              Unit number
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION Get_Lun() RESULT( Lun )
    INTEGER :: Lun
    Lun = 9
    Lun_Search: DO
       Lun = Lun + 1
       IF ( .NOT. File_Unit_Exists( Lun ) ) THEN
          Lun = -1
          EXIT Lun_Search
       END IF
       IF ( .NOT. File_Open_by_Unit( Lun ) ) EXIT Lun_Search
    END DO Lun_Search
  END FUNCTION Get_Lun


!===============================================================
! Name:         File_Unit_Exists
!
!
! Type:         Function
!
!
! Description:  Tests if file unit exists already. Inherited from CRTM
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - FileID             I              Unit number
!       - File_Unit_Exists   O              Logical 'unit number exists'
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION File_Unit_Exists( FileID ) RESULT ( Existence )
    INTEGER, INTENT( IN ) :: FileID
    LOGICAL :: Existence
    INQUIRE( UNIT = FileID, EXIST = Existence )
  END FUNCTION File_Unit_Exists
  
!===============================================================
! Name:         File_Open_by_Unit
!
!
! Type:        Function
!
!
! Description:  Tests if file is open or not (by checking unit number).
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - FileID             I              Unit number
!       - File_Open_by_Unit  O              Logical 'File is open'
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION File_Open_by_Unit( FileID ) RESULT ( Is_Open )
    INTEGER, INTENT( IN ) :: FileID
    LOGICAL :: Is_Open
    INQUIRE( UNIT = FileID, OPENED = Is_Open )
  END FUNCTION File_Open_by_Unit

  
!===============================================================
! Name:         ShrkMatrx
!
!
! Type:         Subroutine
!
!
! Description:  Shrinks matrix using a selection index vector
!               Worth noting that shrinking could be done in 
!               rows, columns or both.
!
! Arguments:
!
!      Name                 Type         Description
!      ---------------------------------------------------
!       - nch                I           Number of selected channels
!       - ShrkRadVec         I           Index-Vector of selected channels
!       - C                  I           Matrix to be shrinked
!       - Cstar              O           Shrinked matrix
!       - Idim               I           Flag onhow to shrink matrix 
!                                        (rows, columns, both)
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ShrkMatrx(nch,ShrkRadVec,C,Cstar,Idim)
    REAL,    DIMENSION(:,:) :: C,Cstar
    INTEGER, DIMENSION(:)   :: ShrkRadVec
    INTEGER                 :: nch,Idim
    !-------Shrink both dimensions (1 & 2)
    IF (idim.eq.0)  Cstar(1:nch,1:nch) = C(ShrkRadVec(1:nch),ShrkRadVec(1:nch))
    !-------Shrink dimension along row axis
    IF (idim.eq.1) Cstar(1:nch,:) = C(ShrkRadVec(1:nch),:)
    !-------Shrink dimension along column axis
    IF (idim.eq.2) Cstar(:,1:nch) = C(:,ShrkRadVec(1:nch))
    RETURN
  END SUBROUTINE ShrkMatrx
  

!===============================================================
! Name:         ShrkVect
!
!
! Type:         Subroutine
!
!
! Description:  Shrinks a vector
!
!
! Arguments:
!
!      Name                 Type         Description
!      ---------------------------------------------------
!       - nch                I           Number of selected channels
!       - ShrkRadVec         I           Index-Vector of selected channels
!       - V                  I           Vector to be shrinked
!       - Vstar              O           Shrinked vector
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ShrkVect(nch,ShrkRadVec,V,Vstar)
    REAL,    DIMENSION(:)  :: V,Vstar
    INTEGER, DIMENSION(:)  :: ShrkRadVec
    INTEGER                :: nch
    Vstar(1:nch) = V(ShrkRadVec(1:nch))
    RETURN
  END SUBROUTINE ShrkVect



!===============================================================
! Name:         printSummary
!
!
! Type:       Subroutine
!
!
! Description:  Prints a summary of the performances after the 
!               completion of the retrieval process.
!
!
! Arguments:
!
!          Name                    Type          Description
!      ---------------------------------------------------
!       - nprofiles                  I           Total # profiles submitted
!       - nIterTot                   I           Total # iterations
!       - nConvProfs                 I           # convergent profiles
!       - nprofsEff                  I           # effectively processed profiles
!       - nProfsOverOcean            I           # profiles detected over ocean
!       - nProfsOverSeaIce           I           # profiles detected over  sea ice
!       - nProfsOverLand             I           # profiles detected over land
!       - nProfsOverSnow             I           # profiles detected over snow
!       - Tim                        I           Vector containing all 
!                                                timing for different 
!                                                sections of the retrieval
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE printSummary(nprofiles,nIterTot,Tim,nConvProfs,nprofsEff,&
       nProfsOverOcean,nProfsOverSeaIce,nProfsOverLand,nProfsOverSnow,filein,fileout)
    INTEGER               :: nprofiles,nConvProfs,iprof,nprofsEff
    INTEGER               :: nProfsOverOcean,nProfsOverSeaIce,nProfsOverLand,nProfsOverSnow
    INTEGER, DIMENSION(:) :: nIterTot
    REAL,    DIMENSION(:) :: Tim
    REAL                  :: SumIters,avgIter=0.,avgRate=0.
    CHARACTER(LEN=*)      :: filein, fileout
    Print *, ''
    Print *, ''
    Print *, ''
    Print *, '_____________________________________'
    Print *, ''
    Print *, 'Process Summary:'
    Print *, '_____________________________________'
    SumIters  =0.
    DO iprof=1,nprofiles
       IF (nIterTot(iprof) .ge. 0) THEN
          SumIters=SumIters+nIterTot(iprof)
       ENDIF
    ENDDO
    IF (nprofsEff .ne. 0) THEN
       avgIter=SumIters/nprofsEff
       avgRate=(real(nConvProfs)/real(nprofsEff))*100.
    ENDIF
    Write(*,'(a)'      )      'Input file='//TRIM(filein)
    Write(*,'(a)'      )      'Output file='//TRIM(fileout)
    Write(*,'(a45,i15)')      'Total Number of Profiles submitted:',nprofiles
    Write(*,'(a45,i15)')      'Total Number of Profiles processed:',nprofsEff
    Write(*,'(a45,i15)')      'Total Number of Profiles detected over ocean:',nProfsOverOcean
    Write(*,'(a45,i15)')      'Total Number of Profiles detected over ice:',nProfsOverSeaIce
    Write(*,'(a45,i15)')      'Total Number of Profiles detected over land:',nProfsOverLand
    Write(*,'(a45,i15)')      'Total Number of Profiles detected over snow:',nProfsOverSnow
    Write(*,'(a45,i15)')      'Number of Convergent Profiles:',nConvProfs
    Write(*,'(a45,i15)')      'Total Number of Iterations:',int(SumIters)
    Write(*,'(a45,f15.5)')    'Avg # Iterations/Profile:',avgIter
    Write(*,'(a45,f15.5,a1)') 'Convergence Rate:',avgRate,'%'
    Print *, '_____________________________________'
    Print *, ''
    Print *, 'Timing:'
    Print *, '_____________________________________'
    Write(*,'(a45,2f15.5,a1)') 'Uploading/Initialization:',Tim(2),(Tim(2)/Tim(1))*100.,'%'
    Write(*,'(a45,2f15.5,a1)') 'Reading radiance/extr data:',Tim(3),(Tim(3)/Tim(1))*100.,'%'
    Write(*,'(a45,2f15.5,a1)') 'Preparing matrices(Sa,Xb,Se,Fe,U,X1st):',Tim(4),(Tim(4)/Tim(1))*100.,'%'
    Write(*,'(a45,2f15.5,a1)') 'Setting up scene/Merging cov, EOF proj:',Tim(5),(Tim(5)/Tim(1))*100.,'%'
    Write(*,'(a45,2f15.5,a1)') 'Forward Operator:',Tim(6),(Tim(6)/Tim(1))*100.,'%'
    Write(*,'(a45,2f15.5,a1)') 'Inversion & Pre-Post Processing:',Tim(7),(Tim(7)/Tim(1))*100.,'%'
    Write(*,'(a45,2f15.5,a1)') 'Error Analysis & characterization:',Tim(8),(Tim(8)/Tim(1))*100.,'%'
    Write(*,'(a45,2f15.5,a1)') 'Output (Scene & Monitoring):',Tim(9),(Tim(9)/Tim(1))*100.,'%'
    Write(*,'(a45,2f15.5,a1)') 'Total:',Tim(1),(Tim(1)/Tim(1))*100.,'%'
    print *

    RETURN
  END SUBROUTINE printSummary
  
  
!===============================================================
! Name:         fov_a
!
!
! Type:        Subroutine
!
!
! Description: Compute Field Of View of Amsua
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------------
!       - fov_size_a                 I           fov size array
!
!
! Modules needed:
!       - None
!
!
! History:
!       02-14-2008      Wanchun Chen                    PSGS
!
!===============================================================
  
  SUBROUTINE fov_a(FOV_SIZE_A,NUMSPOT_A,satId)

    INTEGER                 :: satId
    INTEGER                 :: NUMSPOT_A
    REAL                    :: PI=3.141593
    REAL                    :: SCAN_ANG_A=3.3
    REAL                    :: REARTH=6371.0
    REAL                    :: RSAT=833.0
    REAL,   DIMENSION(0:NUMSPOT_A-1) :: fov_size_A 
    REAL,   DIMENSION(0:NUMSPOT_A) :: ang
    !REAL,   DIMENSION(0:SIZE(fov_size_A)+1) :: ang

    INTEGER :: i=0,j
    REAL    :: angle=0.0
    REAL    :: angle1=0.0

    if (satId .eq. SENSOR_ID_NPP) RSAT=817.0

    do i=0, NUMSPOT_A
      angle = PI * SCAN_ANG_A * (i - NUMSPOT_A/2.0) / 180.0
      angle1 = (REARTH + RSAT) * sin(angle) / REARTH
      angle1 = atan(angle1 / sqrt(1 - angle1 * angle1))
      ang(i) = (angle1 * 180.0 / PI) - SCAN_ANG_A * (i - NUMSPOT_A/2.0)
    enddo

    do i=0, NUMSPOT_A-1
       fov_size_A(i) = abs(ang(i+1) - ang(i))
    enddo

    return
  
  END SUBROUTINE fov_a


!===============================================================
! Name:         get_fov_size
!
!
! Type:        Subroutine
!
!
! Description: generic form of Computing Field Of View
!
!
! Arguments:
!
!           Name                    Type          Description
!      ---------------------------------------------------------
!       - fov_size                   O            fov size array
!       - NFOV                       I            NUMBER of Points
!       - SCAN_ANG                   I            scan angle
!       - RSAT                       I            height of satellite
!
!
! Modules needed:
!       - None
!
!
! History:
!       10-07-2010      Wanchun Chen
!
!===============================================================

  SUBROUTINE get_fov_size(fov_size,NFOV,SCAN_ANG,RSAT)

    INTEGER                   :: NFOV
    REAL, DIMENSION(0:NFOV-1) :: fov_size
    REAL                      :: SCAN_ANG
    REAL                      :: RSAT
    
    REAL                      :: PI=3.141593
    REAL                      :: REARTH=6371.0
    REAL, DIMENSION(0:NFOV)   :: ang
    INTEGER                   :: i=0,j
    REAL                      :: angle=0.0
    REAL                      :: angle1=0.0

    do i=0, NFOV
      angle = PI * SCAN_ANG * (i - NFOV/2.0) / 180.0
      angle1 = (REARTH + RSAT) * sin(angle) / REARTH
      angle1 = atan(angle1 / sqrt(1 - angle1 * angle1))
      ang(i) = (angle1 * 180.0 / PI) - SCAN_ANG * (i - NFOV/2.0)
    enddo

    do i=0, NFOV-1
       fov_size(i) = abs(ang(i+1) - ang(i))
    enddo

    return
  
  END SUBROUTINE get_fov_size


!===============================================================
! Name:         strcmp
!
!
! Type:        Function
!
!
! Description: Comparison 2 strings, return 0 if equal; 1 not equal
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       -  S1                       char string           
!       -  S2                       char string           
!
!
! Modules needed:
!       - None
!
!
! History:
!       07-28-2008      Wanchun Chen              QSS/PSGS/DELL
!
!===============================================================
function strcmp(s1,s2)
  integer          :: strcmp
  character(len=*) :: s1, s2
  integer          :: L1, L2, i
  
  L1=LEN_TRIM(s1)
  L2=LEN_TRIM(s2)
  
  if ( L1 .ne. L2 ) then
    strcmp = 1
    return
  else
    strcmp = 0
    do i=1,L1
      if ( s1(i:i) .ne. s2(i:i) ) then
        strcmp = 1
        return
      endif
    enddo
  endif
  return
end function strcmp


!-----------------------------------------------------------------------------
! Purpose:
!
!   To insert a string after the last slash character if there is a slash,
!   otherwise, insert into at beginning of strin
!   It's mainly proper at rdr2tdr step to generate TDR file names.
!
! Input Arguments:
!     - strin : INPUT String
!     - str_replace : replace part
!     - str_insert : the string to be inserted into string
!
! Ouput Arguments:
!     - : strout : the string after being inserted
!
! Histroy:
!   Wanchun Chen   07-22-2010	    Original coder
!
!-----------------------------------------------------------------------------
  
SUBROUTINE insert_path_string(strin, str_replace, str_insert, strout)

  !---- input variables ---- 
  character(len=*) :: strin
  character(len=*) :: str_replace
  character(len=*) :: str_insert

  !---- output variable ----
  character(len=*) :: strout

  !---- local variables ----
  integer :: indx = 0
  integer :: len_effective=0

  indx = INDEX (strin, '/', BACK = .TRUE.)
  len_effective = LEN_TRIM(strin)

  !---- if no slash inside input strin ---
  if( indx .lt. 1 ) then
    strout = TRIM(str_replace)//TRIM(str_insert)//TRIM(strin)
  else
    strout = TRIM(str_replace)//TRIM(str_insert)//strin(indx+1:len_effective)
  endif

END SUBROUTINE insert_path_string


!-----------------------------------------------------------------------------
! Purpose:
!
!   To insert a string after the last slash character if there is a slash,
!   otherwise, insert into at beginning of strin
!   It's mainly proper at FM step to generate FMSDR file names.
!  
! Input Arguments:
!     - strin : INPUT String
!     - str_append : append part
!     - str_old: gone part
!     - str_new: new part
!
! Ouput Arguments:
!     - : strout : the string after being inserted
!
! Histroy:
!   Wanchun Chen   07-28-2010	    Original coder
!
!-----------------------------------------------------------------------------
  
SUBROUTINE replace_path_string(strin, str_append, str_old, str_new, strout)

  !---- input variables ---- 
  character(len=*) :: strin
  character(len=*) :: str_append
  character(len=*) :: str_old
  character(len=*) :: str_new

  !---- output variable ----
  character(len=*) :: strout

  !---- local variables ----
  integer :: indx = 0, indx_dot = 0
  integer :: len_effective = 0
  character(len=128) :: str_tmp

  indx = INDEX (strin, '/', BACK = .TRUE.)
  len_effective = LEN_TRIM(strin)

  !---- if no slash inside input strin ---
  if( indx .lt. 1 ) then
    indx_dot = INDEX (strin, '.')
    strout = TRIM(str_append)//TRIM(str_new)//TRIM(strin(indx_dot:len_effective))
  else
    str_tmp = strin(indx+1:len_effective)
    indx_dot = INDEX (str_tmp, '.')
    len_effective = LEN_TRIM(str_tmp)
    strout = TRIM(str_append)//TRIM(str_new)//str_tmp(indx_dot:len_effective)
  endif

END SUBROUTINE replace_path_string

!-----------------------------------------------------------------------------
! Purpose:
!
!   To insert a string after the last slash character if there is a slash,
!   otherwise, insert into at beginning of strin
!   It's mainly proper at FM step to generate FMSDR file names.
!   Specific for Megha-Tropiques L1A filename convention
!  
! Input Arguments:
!     - strin : INPUT String
!     - str_append : append part
!     - str_old: gone part
!     - str_new: new part
!
! Ouput Arguments:
!     - : strout : the string after being inserted
!
! Histroy:
!   Wanchun Chen   07-28-2010	    Original coder
!
!-----------------------------------------------------------------------------
  
SUBROUTINE replace_path_string_mt(strin, str_append, str_mark, str_old, str_new, strout)

  !---- input variables ---- 
  character(len=*) :: strin
  character(len=1) :: str_mark
  character(len=*) :: str_append
  character(len=*) :: str_old
  character(len=*) :: str_new

  !---- output variable ----
  character(len=*) :: strout

  !---- local variables ----
  integer :: indx = 0, indx_dot = 0
  integer :: len_effective = 0
  character(len=128) :: str_tmp

  indx = INDEX (strin, '/', BACK = .TRUE.)
  len_effective = LEN_TRIM(strin)

  !---- if no slash inside input strin ---
  if( indx .lt. 1 ) then
!    indx_dot = INDEX (strin, '.')
    indx_dot = INDEX (strin, trim(str_mark))
    strout = TRIM(str_append)//TRIM(str_new)//TRIM(strin(indx_dot:len_effective))
  else
    str_tmp = strin(indx+1:len_effective)
!    indx_dot = INDEX (str_tmp, '.')
    indx_dot = INDEX (str_tmp, trim(str_mark))
    print *,'indx_dot=',indx_dot
    len_effective = LEN_TRIM(str_tmp)
    strout = TRIM(str_append)//TRIM(str_new)//str_tmp(indx_dot:len_effective)
  endif

END SUBROUTINE replace_path_string_mt

!-----------------------------------------------------------------------------
! Purpose:
!
!   Convert a satellite zenith angle to viewing angle
!  
! Input Arguments:
!     - sensor_id        : the sensor ID from Consts module
!     - sat_zenith_angle : specified sat zenith angle
!
! Ouput Arguments:
!     - : view_angle     : the calculated viewing angle
!
! Histroy:
!   Kevin Garrett   12-21-2011	    Original coder
!
!-----------------------------------------------------------------------------

SUBROUTINE SatZenAng2ViewAng(sensor_id,sat_zenith_angle,view_angle)
  !---input variables
  INTEGER      :: sensor_id
  REAL         :: sat_zenith_angle

  !---output variables
  REAL         :: view_angle

  !---local variables
  REAL            :: sat_altitude,view_angle_rad,sat_zen_rad
  REAL, PARAMETER :: earth_radius=6378.1

  view_angle=-999
  sat_zen_rad = deg2rad(sat_zenith_angle)

  IF (sensor_id .eq. SENSOR_ID_NPP) THEN
     sat_altitude = 824.0
     view_angle_rad=(sin(sat_zen_rad)*earth_radius)/(sat_altitude+earth_radius)
     view_angle_rad=asin(view_angle_rad)
     view_angle=rad2deg(view_angle_rad)
  ENDIF

END SUBROUTINE SatZenAng2ViewAng

END MODULE misc

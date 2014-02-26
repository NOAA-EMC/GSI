
!====================================================================
!
! Name: IO_Dropsonde
!
! Type: F90 Module
!
! Description: Subroutines needed to read DropSonde files in the 
!              FRD format
!
! Datatypes Defined:
!       DropSonde_type
!
! Subroutines Contained:
!       ReadHdrDSscene
!       ReadDSScene
!       intrp2levelGrid
!
! Modules Needed:
!       Misc
!       utils
!       ErrorHandling
!
! Original Code:
!       2008-03-01  Kevin Garrett, IMSG @ NOAA/NESDIS/STAR
!
!
!
!
!
!=====================================================================


MODULE IO_DropSonde

  USE Misc
  USE utils
  USE ErrorHandling

  IMPLICIT NONE
  PRIVATE
  !---Public Subroutines and data types
  PUBLIC :: ReadHdrDSscene,ReadDSScene,intrp2levelGrid
  PUBLIC :: DropSonde_type
  INTRINSIC :: INT,COUNT,PACK,SIZE,ABS,INDEX

  TYPE :: DropSonde_type
     !---Header
     INTEGER                          :: nDS
     INTEGER                          :: mxLevs
     INTEGER                          :: SN
     INTEGER                          :: date
     REAL                             :: time
     CHARACTER                        :: aircft
     REAL                             :: lat
     REAL                             :: lon
     INTEGER                          :: hours
     INTEGER                          :: mins
     INTEGER                          :: secs
     INTEGER                          :: month
     INTEGER                          :: day
     INTEGER                          :: year
     INTEGER                          :: jday
     !---Body
     INTEGER                          :: nLevs
     REAL,      DIMENSION(:), POINTER :: tsecProf
     REAL,      DIMENSION(:), POINTER :: Press
     REAL,      DIMENSION(:), POINTER :: Temp
     REAL,      DIMENSION(:), POINTER :: RelHum
     REAL,      DIMENSION(:), POINTER :: qProf
     REAL,      DIMENSION(:), POINTER :: Z
     REAL,      DIMENSION(:), POINTER :: WSProf
     REAL,      DIMENSION(:), POINTER :: FP
     REAL,      DIMENSION(:), POINTER :: FT
     REAL,      DIMENSION(:), POINTER :: FH
     REAL,      DIMENSION(:), POINTER :: FW
     REAL,      DIMENSION(:), POINTER :: LatProf
     REAL,      DIMENSION(:), POINTER :: LonProf
     REAL                             :: WSatSurf
     REAL                             :: PSurf
     REAL                             :: TPW
  END TYPE DropSonde_type

CONTAINS


!====================================================================
!
! Name: ReadHdrDSscene
!
! Type: F90 Program
!
! Description:  Reads dropsonde frd formatted data header
!
!
! Original Code:
!       2008-03-01  Kevin Garrett, IMSG @ NOAA/NESDIS/STAR
!
!
!
!=====================================================================

  SUBROUTINE ReadHdrDSscene(iu,DS,ierr)

    INTEGER                         :: iu,ierr
    TYPE(DropSonde_type)            :: DS
    !---Local variables
    CHARACTER(LEN=200)              :: ligne
    CHARACTER(LEN=1)                :: Dlat,Dlon
    INTEGER                         :: i
    INTEGER                         :: jdayPope
    INTEGER                         :: jday0

    !---Read Header Data
    ierr=0
    READ(iu,err=10,fmt='(a)') ligne
    READ(iu,err=10,fmt='(66x,I20)') DS%SN
    READ(iu,err=10,fmt='(a)') ligne
    READ(iu,err=10,fmt='(10x,a15,25x,I7,8x,I3,I2,I2)') DS%aircft,DS%date,DS%hours,DS%mins,DS%secs
    !---Calculate some header variables
    DS%time = (DS%hours*3600)+(DS%mins*60)+DS%secs    
    DS%year  = int(DS%date/10000.)
    DS%month = int((DS%date-DS%year*10000.)/100.)
    DS%day   = int(DS%date-DS%year*10000.-DS%month*100.)
    DS%year  = 2000+DS%year
    CALL compJulDay(DS%Year,DS%Month,DS%Day,jdaypope)
    CALL compJulDay(DS%Year,1,1,jday0)
    DS%jday  = int(jdayPope-jday0+1)
    !---Read non-used variables
    DO i=1,11 
       read(iu,err=10,fmt='(a)') ligne
    ENDDO
    !---Read lat/lon
    read(iu,err=10,fmt='(20x,F7.2,1x,a1)') DS%lat,DLat
    read(iu,err=10,fmt='(20x,F7.2,1x,a1)') DS%lon,DLon
    IF (Dlat .eq. 'S') DS%lat=-DS%lat
    IF (Dlon .eq. 'W') DS%lon=-DS%lon
    !---Read non-used variables
    DO i=1,4
       read(iu,err=10,fmt='(a)') ligne
    ENDDO
    RETURN
10  ierr=Warn_readInvalid
    CALL ErrHandl(WarningType,Warn_readInvalid,'(ReadDropSonde)')
    RETURN
  END SUBROUTINE ReadHdrDSscene


!====================================================================
!
! Name: ReadDSscene
!
! Type: F90 Program
!
! Description:  Reads dropsonde data and returns DS structure
!
!
! Original Code:
!       2008-03-01  Kevin Garrett, IMSG @ NOAA/NESDIS/STAR
!
!
!
!
!
!=====================================================================

  SUBROUTINE ReadDSscene(iu,mxLevs,DS,ierr)

    INTEGER                         :: iu,mxLevs,ierr
    TYPE(DropSonde_type)            :: DS
    !---Local Variables
    CHARACTER(LEN=4)                :: ix
    INTEGER                         :: nLevs,indx0,indx1,indx2
    INTEGER                         :: fp,ft,fh,fw,z,wd,zw
    REAL                            :: tsec,p,t,rh,ws,u,v
    REAL                            :: ns,wz,lat,lon

    !---Find number of levels in dropsonde and fill structure
    nLevs=0
    LevelsLoop: DO While(.true.)
       read(iu,'(a4,3x,f5.1,2x,f6.1,3x,f7.2,3x,f6.1,4x,2(I4,2x),2(f7.2,3x),f7.2,2x,I4,2x,'//&
            'f6.1,4x, I4,5x,I1,3(3x,I1),4x,f7.4,2x,f9.4,40x)',err=100,end=100) &
            ix,tsec,p,t,rh,z,wd,ws,u,v,ns,wz,zw,fp,ft,fh,fw,lat,lon

       indx0=INDEX(ix,'0',.false.)
       indx1=INDEX(ix,'1',.false.)
       indx2=INDEX(ix,'2',.false.)
       IF (indx0 .eq. 1 .or. indx1 .eq. 1 .or. indx2 .eq. 1) THEN
          nLevs=nLevs+1
          IF (nlevs .ge. mxLevs) THEN 
             print *,'Error:MxLevs must be increased:'
             STOP
          ENDIF
          DS%tsecProf(nLevs) = tsec
          DS%Press(nLevs)    = p
          IF (t .gt. -99) THEN
             DS%Temp(nLevs) = t+273.15
          ELSE
             DS%Temp(nLevs) = -999.
          ENDIF
          DS%RelHum(nLevs)   = rh
          DS%Z(nLevs)        = z
          DS%WSProf(nLevs)   = ws
          DS%FP(nLevs)       = fp
          DS%FT(nLevs)       = ft
          DS%FH(nLevs)       = fh
          DS%FW(nLevs)       = fw
          DS%LatProf(nLevs)  = lat
          DS%LonProf(nLevs)  = lon
          !---Convert RH to Mixing Ratio
          IF (rh .gt. 0 .and. t .gt. -99) THEN
             DS%qProf(nLevs) = RelHum_to_Mixingratio(rh/100.,t+273.15,P)
          ELSE
             DS%qProf(nLevs) = -999.
          ENDIF
          IF (abs(z-10) .le. 5) THEN
             DS%WSatSurf = ws
             DS%Psurf    = p
          ENDIF
       ENDIF
       ! not acceptable in g95 to compare integer with character
       !IF (indx0 .eq. '' .and. indx1 .eq. '' .and. indx2 .eq. '') THEN
       !   print *,'No string'
       !   STOP
       !ENDIF
       IF (indx0 .eq. 0 .and. indx1 .eq. 0 .and. indx2 .eq. 0) THEN
          print *,'No Profiles'
          ierr=1
          RETURN
       ENDIF
    ENDDO LevelsLoop
100 CONTINUE
    DS%nLevs = nLevs
    RETURN
  END SUBROUTINE ReadDSscene

!================================================================================
!
! Name:         intrp2levelGrid
!
! Type:         F90 Subroutine
!
! Description:  Interpolates an atmospheric state vector to any
!               provided pressure level grid
!
! Arguments:  
!
!      Argument     I/O    Desc.
!    ----------------------------------------------------------------
!    XProfIn         I     Profile to be interpolated         
!    PresLevsIn      I     Pressure grid of input profile
!    nLevOut         O     Number of levels to interpolate to
!    PresLevsOut     O     Output profile pressure level grid
!    PresBot         O     Pressure at lowest level not extrapolated
!    PresTop         O     Pressure at highest level not extrapolated
!    XProfOut        O     Output profile
!    qc              O     qc flag based on number of original valid
!                          levels and profile data gaps
!
! Modules Required:
!    - utils
!
! History
!
!    02/12/2008 - Original Code    Kevin Garrett,  IMSG @ NOAA/NESDIS/STAR
!
!================================================================================

  SUBROUTINE intrp2levelGrid(XProfIn,PresLevsIn,nLevOut,PresLevsOut,PresBot,PresTop,XProfOut,qc)

    IMPLICIT NONE
    !---I/O variables   
    INTEGER,                 INTENT(IN)   :: nLevOut
    REAL,    DIMENSION(:),   INTENT(IN)   :: XProfIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PresLevsIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PresLevsOut
    REAL,                    INTENT(OUT)  :: PresBot,PresTop
    REAL,    DIMENSION(:),   INTENT(OUT)  :: XProfOut
    INTEGER,                 INTENT(OUT)  :: qc
    !---Local variables
    INTEGER                               :: nVal_Levs
    INTEGER                               :: i,iLev
    INTEGER, DIMENSION(:),   ALLOCATABLE  :: idx_inLevs
    REAL                                  :: PresDiff
    REAL,    DIMENSION(:),   ALLOCATABLE  :: XProfTrun,PresProfTrun
    
    !---Determine number of levels with good data 
    nVal_Levs = COUNT(XProfIn(:) .gt. 0.)
    IF (nVal_Levs .lt. 10) THEN
       XProfOut(:) = -9999.
       qc = 1
       RETURN
    ENDIF
    IF (nVal_Levs .ge. 10) THEN
       !---Create index of array elements with good data, these are the elements to interpolate
       ALLOCATE (idx_inLevs(nVal_Levs),XProfTrun(nVal_Levs),PresProfTrun(nVal_Levs))
       idx_inLevs = PACK( (/(i,i=1,SIZE(XProfIn(:)))/), (XProfIn(:) .gt. 0))
       PresTop = PresLevsIn(idx_inLevs(1))
       PresBot = PresLevsIn(idx_inLevs(nVal_Levs))
       XProfTrun(:)    = XProfIn(idx_inLevs)
       PresProfTrun(:) = PresLevsIn(idx_inLevs)
       !---Check for gaps in profile and depth of profile
       DO iLev=1,nVal_levs-1
          PresDiff = PresProfTrun(iLev+1)-PresProfTrun(iLev)
          IF (PresDiff .gt. 100) qc = 1 
       ENDDO
       !---Call linear interpolation subroutine

       CALL LINT(PresProfTrun,XProfTrun,1,nVal_Levs,nLevOut,PresLevsOut,XProfOut)
       DO iLev=1,nLevOut
          IF (PresLevsOut(iLev) .lt. PresTop .or. PresLevsOut(iLev) .gt. PresBot) THEN
             XProfOut(iLev) = -999.
          ENDIF
       ENDDO
       DEALLOCATE(idx_inLevs,XProfTrun,PresProfTrun)        
    ENDIF
    RETURN
  END SUBROUTINE intrp2levelGrid
  
End Module IO_DropSonde



!$Id: IO_MeasurData.f90 2644 2011-06-30 20:17:14Z wchen $
!-----------------------------------------------------------------------------------------------
! Name:         IO_MeasurData
! 
! Type:         F90 module
!
! Description:
!       Module that contains the necessary routines and corresponding data
!       related to reading measurements data (radiances, brightness temps).
!
!       Updated in June 06 to account for as many angles as channels (to
!       accomodate Windsat). Sid Ahmed Boukabara
!
!       Updated in Feb 08 to account relative azimuth and solar zenith angles. 
!       Sid Ahmed Boukabara
!
! Modules needed:
!       - misc
!       - Consts
!
! Subroutines contained:
!       - ReadHdrMeasurmts,,,
!       - ReadMeasurmts
!       - WriteHdrMeasurmts
!       - WriteMeasurmts
!       - WriteRadMeasScanLMode
!       - WriteRadHdrScanLMode
!       - ReadRadHdrScanLMode
!       - ReadRadMeasScanLMode
!       - WriteBias
!       - ReadBias
!       - ReadRadHdrScanLMode_ascii
!       - ReadRadMeasScanLMode_ascii
!
! Data type included:
!       - MeasurData_type
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!       2008    W. Chen PSGS Inc. @ NOAA/NESDIS/STAR
!
!-----------------------------------------------------------------------------------------------

MODULE IO_MeasurData
  USE Consts
  USE misc
  USE ErrorHandling
  IMPLICIT NONE
  PRIVATE
  !----Publicly available subroutine(s)
  PUBLIC :: ReadHdrMeasurmts,ReadMeasurmts,WriteHdrMeasurmts,WriteMeasurmts,getRadNprf
  PUBLIC :: WriteRadMeasScanLMode,WriteRadHdrScanLMode
  PUBLIC :: ReadRadHdrScanLMode,ReadRadMeasScanLMode,WriteBias,ReadBias
  PUBLIC :: ReadRadHdrScanLMode_ascii,ReadRadMeasScanLMode_ascii
  PUBLIC :: WriteRadHdrScanLMode_ascii,WriteRadMeasScanLMode_ascii
  !----Publicly available data/types
  PUBLIC :: MeasurData_type
  !----Global section of the module
  TYPE :: MeasurData_type
    INTEGER                         :: nchan        !# of channels
    INTEGER                         :: nPosScan     !# of scan positions within scanline
    INTEGER                         :: nScanLines   !# of scanlines within file (some might be missing)
    INTEGER                         :: nqc          !# QC elements
    INTEGER                         :: iscanPos     !Scan position
    INTEGER                         :: iScanLine    !Scan line index
    REAL                            :: lat          !Latitude
    REAL                            :: lon          !Longitude
    INTEGER                         :: Node         !Orbit mode (Asc/Des)
    INTEGER                         :: Year         !Year of measuremrent
    INTEGER                         :: Month        !Month of measurement
    INTEGER                         :: Day          !Day
    INTEGER                         :: julDay       !Julian day
    REAL                            :: secs         !Seconds within the day
    REAL                            :: RelAziAngle  !Relative Azimuth Angle
    REAL                            :: SolZenAngle  !Solar Zenith Angle
    REAL,    DIMENSION(:), POINTER  :: CentrFreq    !Central frequencies
    REAL,    DIMENSION(:), POINTER  :: Rad          !Radiances
    REAL,    DIMENSION(:), POINTER  :: tb           !Brightness temperatures
    REAL,    DIMENSION(:), POINTER  :: angle        !Viewing Angle
    REAL,    DIMENSION(:), POINTER  :: secant_view  !Secant of viewing angle
    INTEGER, DIMENSION(:), POINTER  :: qc           !QC vector
    INTEGER, DIMENSION(:), POINTER  :: polar        !Polarizations
  END TYPE MeasurData_type
  
  !---INTRINSIC functions used in this module
  INTRINSIC :: ASSOCIATED,COS
  
CONTAINS

  !----------------------------------------------------------------
  !
  ! Bias related I/O
  !
  !----------------------------------------------------------------


!===============================================================
! Name:         WriteBias
!
!
! Type:         Subroutine
!
!
! Description:  Writes the biases in an output file (bias is a 
!               generic term here: it encompasses biases, slopes,
!               intercepts).
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - BiasFile           I             Name of the output file
!       - nchan              I             Number of channels
!       - npos               I             Number of scan positions within scanline
!       - cfreq              I             Central frequencies
!       - Bias               I             Biases array (npos,nchan)
!       - Slope              I             Slopes array (npos,nchan)
!       - Intercept          I             Intercepts array (npos,nchan)
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

  SUBROUTINE WriteBias(BiasFile,nchan,npos,cfreq,Bias,Slope,Intercept)
    CHARACTER(LEN=*)                   :: BiasFile
    INTEGER                            :: nchan,npos,iu,ichan
    REAL,            DIMENSION(:)      :: Cfreq
    REAL,            DIMENSION(:,:)    :: Bias,Slope,Intercept
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=BiasFile,form='formatted',status='unknown')
    write(iu,'(2i4)') nchan,npos
    !----Bias
    DO ichan=1,nchan
       write(iu,'(i4,f8.3,30f7.2)') ichan,cfreq(ichan),bias(1:npos,ichan)
    ENDDO
    !----Slope
    DO ichan=1,nchan
       write(iu,'(i4,f8.3,30f7.2)') ichan,cfreq(ichan),slope(1:npos,ichan)
    ENDDO
    !----Intercept
    DO ichan=1,nchan
       write(iu,'(i4,f8.3,30f7.2)') ichan,cfreq(ichan),intercept(1:npos,ichan)
    ENDDO
    CLOSE(iu)
    RETURN
  END SUBROUTINE WriteBias

!===============================================================
! Name:         ReadBias
!
!
! Type:         Subroutine
!
!
! Description:  Reads the biases, slopes, intercepts from the
!               bias file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - BiasFile           I            Name of the bias file
!       - nchan              O            Number of channels
!       - npos               O            Number of scan positions within scanline
!       - cfreq              O            Central frequencies
!       - Bias               O            Biases array (npos,nchan)
!       - Slope              O            Slopes array (npos,nchan)
!       - Intercept          O            Intercepts array (npos,nchan)
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ReadBias(BiasFile,nchan,npos,cfreq,Bias,Slope,Intercept,err)
    CHARACTER(LEN=*)                   :: BiasFile
    INTEGER                            :: nchan,npos,iu,ichan,ichan0,err
    REAL,   DIMENSION(:),   POINTER    :: Cfreq
    REAL,   DIMENSION(:,:), POINTER    :: Bias,Slope,Intercept
    CHARACTER(LEN=17)                  :: fmt
    !---Open file containing radiance measurements
    err = 0
    iu=get_lun()
    OPEN(iu,file=BiasFile,form='formatted',status='unknown',err=10)
    read(iu,'(2i4)',err=10) nchan,npos
    ALLOCATE(bias(npos,nchan),slope(npos,nchan),intercept(npos,nchan),cfreq(nchan))
    
    fmt(1:9)='(i4,f8.3,'
    !--- decide the rest format    
    if(npos .lt. 10 ) then
      write(fmt(10:10),'(I1)') npos
      fmt(11:15)='f7.2)'
    else if ( npos .ge. 10  .and. npos .lt. 100 ) then
      write(fmt(10:11),'(I2)') npos
      fmt(12:16)='f7.2)'
    else if ( npos .gt. 100 ) then
      write(fmt(10:12),'(I3)') npos
      fmt(13:17)='f7.2)'
    endif
    
    !----Bias
    DO ichan=1,nchan
       !read(iu,'(i4,f8.3,90f7.2)',err=10) ichan0,cfreq(ichan),bias(1:npos,ichan)
       !read(iu,'(i4,f8.3,191f7.2)',err=10) ichan0,cfreq(ichan),bias(1:npos,ichan)
       read(iu,fmt,err=10) ichan0,cfreq(ichan),bias(1:npos,ichan)
    ENDDO
    !----Slope
    DO ichan=1,nchan
       !read(iu,'(i4,f8.3,90f7.2)',err=10) ichan0,cfreq(ichan),slope(1:npos,ichan)
       !read(iu,'(i4,f8.3,191f7.2)',err=10) ichan0,cfreq(ichan),slope(1:npos,ichan)
       read(iu,fmt,err=10) ichan0,cfreq(ichan),slope(1:npos,ichan)
    ENDDO
    !----Intercept
    DO ichan=1,nchan
       !read(iu,'(i4,f8.3,90f7.2)',err=10) ichan0,cfreq(ichan),intercept(1:npos,ichan)
       !read(iu,'(i4,f8.3,191f7.2)',err=10) ichan0,cfreq(ichan),intercept(1:npos,ichan)
       read(iu,fmt,err=10) ichan0,cfreq(ichan),intercept(1:npos,ichan)
    ENDDO
    CLOSE(iu)
    RETURN
10  err=1
    RETURN
  END SUBROUTINE ReadBias



  !----------------------------------------------------------------
  !
  !     Point-By-Point mode of storage (typically for FM data)
  !
  !----------------------------------------------------------------


!===============================================================
! Name:         ReadHdrMeasurmts
!
!
! Type:         Subroutine
!
!
! Description:  Reads header of radiances measurements file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - MeasurFile         I             Name of the measurements file
!       - iu                 O             Unit number
!       - nMeasurData        O             Number of measurements within file
!       - MeasurData         I/O           Structure containing the measurements
!                                          (see definition in top section of module)
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

  SUBROUTINE ReadHdrMeasurmts(MeasurFile,iu,nMeasurData,MeasurData)
    CHARACTER(LEN=*)        :: MeasurFile
    TYPE(MeasurData_type)   :: MeasurData
    INTEGER                 :: iu,nMeasurData
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=MeasurFile,form='unformatted',status='old')
    !---Read header/Place holder
    READ(iu) nMeasurData   
    READ(iu) MeasurData%nchan
    READ(iu) MeasurData%nPosScan,MeasurData%nScanLines
    READ(iu) MeasurData%nqc
    ALLOCATE(MeasurData%CentrFreq(MeasurData%nchan),MeasurData%Rad(MeasurData%nchan), &
         MeasurData%qc(MeasurData%nqc),MeasurData%Tb(MeasurData%nchan),               &
         MeasurData%polar(MeasurData%nchan),MeasurData%angle(MeasurData%nchan),       &
         MeasurData%secant_view(MeasurData%nchan))
    READ(iu) MeasurData%CentrFreq(1:MeasurData%nchan)
    READ(iu) MeasurData%polar(1:MeasurData%nchan)
    RETURN
  END SUBROUTINE ReadHdrMeasurmts


!===============================================================
! Name:         getRadNprf
!
!
! Type:         Subroutine
!
!
! Description:  get radiances measurements file number of profiles
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - MeasurFile         I             Name of the measurements file
!       - nprf               O             Number of measurements within file
!
!
! Modules needed:
!       - get_lun
!
!
! History:
!       06-29-2011      Wanchun Chen       DELL Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE getRadNprf(MeasurFile,nprf)
    CHARACTER(LEN=*)        :: MeasurFile
    INTEGER                 :: iu,nprf
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=MeasurFile,form='unformatted',status='old')
    !---Read header/Place holder
    READ(iu) nprf
    CLOSE(iu)
    RETURN
  END SUBROUTINE getRadNprf


!===============================================================
! Name:         WriteHdrMeasurmts
!
!
! Type:         Subroutine
!
!
! Description:  Writes out the header of the measurements file.
!
!
! Arguments:
!
!        Name                      Type          Description
!      ---------------------------------------------------
!       - MeasurFile                I            Name of the measurements file
!       - iu                        O            Unit number
!       - nMeasurData               I            Number of measurements
!       - nqc                       I            Number of QC elements
!       - nchan                     I            Number of channels
!       - nPosScan                  I            Number of scan positions
!       - CentrFreq                 I            Central frequencies
!       - polar                     I            Polarizations
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

  SUBROUTINE WriteHdrMeasurmts(MeasurFile,iu,nMeasurData,nqc,nchan,&
       nPosScan,CentrFreq,polar,nScanLines)
    CHARACTER(LEN=*)        :: MeasurFile
    INTEGER                 :: iu,nMeasurData,nchan,nqc,nPosScan,nScanLines
    REAL,    DIMENSION(:)   :: CentrFreq
    INTEGER, DIMENSION(:)   :: polar

    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=MeasurFile,form='unformatted',status='unknown')
    !---Read header/Place holder
    WRITE(iu) nMeasurData   
    WRITE(iu) nchan
    WRITE(iu) nPosScan,nScanLines
    WRITE(iu) nqc
    WRITE(iu) CentrFreq(1:nchan)
    WRITE(iu) polar(1:nchan)
    RETURN
  END SUBROUTINE WriteHdrMeasurmts


!===============================================================
! Name:         ReadMeasurmts
!
!
! Type:         Subroutine
!
!
! Description:  Reads the measurements from the measurements file
!               and puts them into a structure (see top module for 
!               definition)
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 I              Unit number
!       - Y                  I/O            Structure containing the 
!                                           radiances and other elements
!       - ierr               O              Error flag (0:fine, <>0:pb) 
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

  SUBROUTINE ReadMeasurmts(iu,Y,ierr)
    INTEGER               :: iu
    TYPE(MeasurData_type) :: Y
    INTEGER               :: ierr,ichan
    ierr=0
    READ(iu,iostat=ierr,end=10)     Y%lat,Y%lon,Y%RelAziAngle,Y%SolZenAngle
    IF (ierr.ne.0) THEN
       ierr=Warn_readInvalid
       CALL ErrHandl(WarningType,Warn_readInvalid,'Radiances invalid.(ReadMeasurements)')
       RETURN
    ENDIF
    READ(iu,err=20)                 Y%Node,Y%iscanPos,Y%iscanLine,Y%Year,Y%julDAY,Y%secs
    READ(iu,err=20)                 Y%angle(1:Y%nchan)
    READ(iu,err=20)                 Y%tb(1:Y%nchan)
    IF (Y%nqc.gt.0) READ(iu,err=20) Y%qc(1:Y%nqc)
    DO ichan=1,Y%nchan
       Y%secant_view(ichan)    = 1./cos(deg2rad(Y%angle(ichan)))
    ENDDO
    RETURN
10  ierr=Warn_EndOfFile
    CALL ErrHandl(WarningType,Warn_EndOfFile,'(ReadMeasurements)') 
    RETURN
20  ierr=Warn_readInvalid
    CALL ErrHandl(WarningType,Warn_readInvalid,'(ReadMeasurements)')
    RETURN
  END SUBROUTINE ReadMeasurmts


!===============================================================
! Name:         WriteMeasurmts
!
!
! Type:         Subroutine
!
!
! Description:  Writes out the radiances and other elements into
!               mesurements file.
!
!
! Arguments:
!
!        Name                       Type           Description
!      ---------------------------------------------------
!       - iu                         I             Unit number
!       - nqc                        I             # QC elements
!       - qc                         I             QC vector
!       - nchan                      I             Number of channels
!       - angle                      I             Viewing Angle
!       - RelAziangle                I             Relative Azimuth Angle
!       - SolZenangle                I             Solar Zenith Angle
!       - tb                         I             Brightness Temp.
!       - lat                        I             Latitude
!       - lon                        I             Longitude
!       - node                       I             Orbit mode (Asc/Des)
!       - scanUTC                    I             UTC Time
!       - scanDAY                    I             Day
!       - scanYear                   I             Year
!       - iscanPos                   I             Scan Position
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

  SUBROUTINE WriteMeasurmts(iu,nqc,qc,nchan,angle,tb,lat,lon,node,&
       scanUTC,scanDAY,scanYear,iscanPos,iscanLine,RelAziAngle,SolZenAngle)
    INTEGER               :: iu,nchan,nqc,iscanPos,iscanLine
    REAL,    DIMENSION(:) :: tb,angle
    INTEGER, DIMENSION(:) :: qc
    REAL                  :: lat,lon,RelAziAngle,SolZenAngle
    INTEGER               :: node,scanDAY,scanYear
    REAL                  :: scanUTC
    Write(iu) lat,lon,RelAziAngle,SolZenAngle
    Write(iu) Node,iscanPos,iscanLine,scanYear,scanDAY,scanUTC
    Write(iu) angle(1:nchan)
    Write(iu) tb(1:nchan)
    IF (nqc .gt.0)  Write(iu) qc(1:nqc)
    RETURN
  END SUBROUTINE WriteMeasurmts




  !----------------------------------------------------------------
  !
  !  ScanLine-By-ScanLine mode of storage (typically TDR/SDR data)
  !
  !----------------------------------------------------------------


!===============================================================
! Name:         WriteRadHdrScanLMode
!
!
! Type:         Subroutine
!
!
! Description:  Writes out the header of the measurements file 
!               (in scanLine mode, as opposed to point-by-point mode)
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - MeasurFile         I            Name of scanline mode file
!       - iu                 O            Unit number
!       - nScanL             I            Number of scanlines
!       - nFovs              I            Number of FOVs per scanline
!       - nqc                I            Number of QC Elements
!       - nchan              I            Number of channels
!       - CFreq              I            Central frequencies
!       - pol                I            Polarizations
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

  SUBROUTINE WriteRadHdrScanLMode(MeasurFile,iu,nScanL,nFovs,nqc,nchan,CFreq,pol)
    CHARACTER(LEN=*)        :: MeasurFile
    INTEGER                 :: iu,nScanL,nFovs,nqc,nchan
    REAL,    DIMENSION(:)   :: CFreq
    INTEGER, DIMENSION(:)   :: pol

    !---Open file containing radiance measurements (scanline by scanline mode)
    iu=get_lun()
    OPEN(iu,file=MeasurFile,form='unformatted',status='unknown')
    !---Read header/Place holder
    WRITE(iu) nScanL,nFovs,nqc,nchan
    WRITE(iu) CFreq
    WRITE(iu) pol
    RETURN
  END SUBROUTINE WriteRadHdrScanLMode


!===============================================================
! Name:         ReadRadHdrScanLMode
!
!
! Type:         Subroutine
!
!
! Description:  Reads the header of a measurements file in a
!               scanline mode (as opposed to point-by-point mode).
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - MeasurFile         I             Scanline Measurements file
!       - iu                 O             Unit number
!       - nScanL             O             Number of scanlines within file
!       - nFovs              O             Number of FOVs per scanline
!       - nqc                O             Number of QC elements
!       - nchan              O             Number of channels
!       - CFreq              O             Central Frequencies
!       - pol                O             Polarizations
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

  SUBROUTINE ReadRadHdrScanLMode(MeasurFile,iu,nScanL,nFovs,nqc,nchan,CFreq,pol)
    CHARACTER(LEN=*)        :: MeasurFile
    INTEGER                 :: iu,nScanL,nFovs,nqc,nchan
    REAL,    DIMENSION(:),  POINTER  :: Cfreq
    INTEGER, DIMENSION(:),  POINTER  :: pol

    !---Open file containing radiance measurements (scanline by scanline mode)
    iu=get_lun()
    OPEN(iu,file=MeasurFile,form='unformatted',status='unknown')
    !---Read header/Place holder
    READ(iu) nScanL,nFovs,nqc,nchan
    IF (ASSOCIATED(CFreq)) DEALLOCATE(Cfreq)
    IF (ASSOCIATED(pol))   DEALLOCATE(pol)
    ALLOCATE(Cfreq(nchan),pol(nchan))
    READ(iu) CFreq
    READ(iu) pol
    RETURN
  END SUBROUTINE ReadRadHdrScanLMode


!===============================================================
! Name:         WriteRadMeasScanLMode
!
!
! Type:         Subroutine
!
!
! Description:  Writes the measurements and other elements into 
!               the measurements file (scanline mode).
!
!
! Arguments:
!
!        Name                Type          Description
!      ---------------------------------------------------
!       - iu                  I            Unit number           
!       - nqc                 I            Number of QC elements
!       - qc                  I            QC vector
!       - nchan               I            Number of channels
!       - nFovs               I            Number of FOVs
!       - angle               I            Satellite Zenith Angle
!       - RelAziAngle         I            Relative Azimuth Angle
!       - SolZenAngle         I            Solar Zenith Angle
!       - tb                  I            Brightness Temperatures vector
!       - lat                 I            Latitude
!       - lon                 I            Longitude
!       - node                I            Orbit mode (Asc/Des)
!       - scanUTC             I            UTC Time
!       - scanDAY             I            Day
!       - scanYear            I            Year
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

  SUBROUTINE WriteRadMeasScanLMode(iu,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,&
       scanUTC,scanDAY,scanYear,RelAziAngle,SolZenAngle)
    INTEGER                 :: iu,nchan,nqc,nFovs
    INTEGER, DIMENSION(:)   :: qc
    REAL,    DIMENSION(:,:) :: tb
    REAL,    DIMENSION(:)   :: angle,lat,lon,RelAziAngle,SolZenAngle
    INTEGER                 :: node,scanDAY,scanYear,scanUTC

    Write(iu) node
    Write(iu) scanDAY,scanYear
    Write(iu) scanUTC
    write(iu) lat(1:nFovs)
    write(iu) lon(1:nFovs)
    write(iu) angle(1:nFovs)
    write(iu) RelAziAngle(1:nFovs)
    write(iu) SolZenAngle(1:nFovs)
    write(iu) tb(1:nFovs,1:nchan)
    IF (nqc .gt.0) Write(iu) qc(1:nqc)
    RETURN
  END SUBROUTINE WriteRadMeasScanLMode

!===============================================================
! Name:         ReadRadMeasScanLMode
!
!
! Type:         Subroutine
!
!
! Description:  Reads the measurements and other elements from 
!               the measurements file (scanline mode).
!
!
! Arguments:
!
!        Name                Type          Description
!      ---------------------------------------------------
!       - iu                  I            Unit number           
!       - nqc                 O            Number of QC elements
!       - qc                  O            QC vector
!       - nchan               O            Number of channels
!       - nFovs               O            Number of FOVs
!       - angle               O            Satellite Zenith Angle
!       - RelAziAngle         0            Relative Azimuth Angle
!       - SolZenAngle         0            Solar Zenith Angle
!       - tb                  O            Brightness Temperatures vector
!       - lat                 O            Latitude
!       - lon                 O            Longitude
!       - node                O            Orbit mode (Asc/Des)
!       - scanUTC             O            UTC Time
!       - scanDAY             O            Day
!       - scanYear            O            Year

!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ReadRadMeasScanLMode(iu,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,&
       scanUTC,scanDAY,scanYear,RelAziAngle,SolZenAngle)
    INTEGER                 :: iu,nchan,nqc,nFovs
    INTEGER, DIMENSION(:)   :: qc
    REAL,    DIMENSION(:,:) :: tb
    REAL,    DIMENSION(:)   :: angle,lat,lon,RelAziAngle,SolZenAngle
    INTEGER                 :: node,scanDAY,scanYear,scanUTC
    Read(iu) node
    Read(iu) scanDAY,scanYear
    Read(iu) scanUTC
    Read(iu) lat(1:nFovs)
    Read(iu) lon(1:nFovs)
    Read(iu) angle(1:nFovs)
    Read(iu) RelAziAngle(1:nFovs)
    Read(iu) SolZenAngle(1:nFovs)
    Read(iu) tb(1:nFovs,1:nchan)
    IF (nqc .gt.0) Read(iu) qc(1:nqc)
    RETURN
  END SUBROUTINE ReadRadMeasScanLMode

  

  !----------------------------------------------------------------
  !  -ascii formats-
  !----------------------------------------------------------------



!===============================================================
! Name:         ReadRadHdrScanLMode_ascii
!
!
! Type:         Subroutine
!
!
! Description:  Reads the header of an ASCII-formatted scanline 
!               mode-based measurements file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - MeasurFile         I            Name of the ASCII measurements file
!       - iu                 O            Unit number
!       - nScanL             O            Number of scanlines within file
!       - nFovs              O            Number of FOVs per scanline
!       - nqc                O            Number of QC elements
!       - nchan              O            Number of channels
!       - CFreq              O            Central Frequencies
!       - pol                O            Polarizations
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

  SUBROUTINE ReadRadHdrScanLMode_ascii(MeasurFile,iu,nScanL,nFovs,nqc,nchan,CFreq,pol)
    CHARACTER(LEN=*)        :: MeasurFile
    INTEGER                 :: iu,nScanL,nFovs,nqc,nchan
    REAL,    DIMENSION(:),  POINTER  :: Cfreq
    INTEGER, DIMENSION(:),  POINTER  :: pol

    !---Open file containing radiance measurements (scanline by scanline mode)
    iu=get_lun()
    OPEN(iu,file=MeasurFile,form='formatted',status='unknown')
    !---Read header/Place holder
    READ(iu,'(4i4)') nScanL,nFovs,nqc,nchan
    ALLOCATE(Cfreq(nchan),pol(nchan))
    READ(iu,'(10f10.5)') CFreq
    READ(iu,'(20i3)') pol
    RETURN
  END SUBROUTINE ReadRadHdrScanLMode_ascii

!===============================================================
! Name:         ReadRadMeasScanLMode_ascii
!
!
! Type:         Subroutine
!
!
! Description:  Reads the content of an ASCII-formatted scanline 
!               mode-based measurements file.
!
!
! Arguments:
!
!           Name            Type            Description
!      ---------------------------------------------------
!       - iu                 I            Unit number        
!       - nqc                I            Number of QC elements
!       - qc                 I            QC vector
!       - nchan              I            Number of channels
!       - nFovs              I            Number of FOVs
!       - angle              O            Satellite Zenith Angle
!       - RelAziAngle        O            Relative Azimuth Angle
!       - SolZenAngle        O            Solar Zenith Angle
!       - tb                 O            Brightn. Temperatures vector
!       - lat                O            Latitude
!       - lon                O            Longitude
!       - node               O            Orbit mode (Asc/Des)
!       - scanUTC            O            UTC time of measurement
!       - scanDAY            O            Day of measurement
!       - scanYear           O            Year of measurement
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

  SUBROUTINE ReadRadMeasScanLMode_ascii(iu,nqc,qc,nchan,nFovs,angle,tb,&
       lat,lon,node,scanUTC,scanDAY,scanYear,RelAziAngle,SolZenAngle)
    INTEGER                 :: iu,nchan,nqc,nFovs
    INTEGER, DIMENSION(:)   :: qc
    REAL,    DIMENSION(:,:) :: tb
    REAL,    DIMENSION(:)   :: angle,lat,lon,RelAziAngle,SolZenAngle
    INTEGER                 :: node
    INTEGER                 :: scanDAY,scanYear
    INTEGER                 :: scanUTC

    Read(iu,'(i4,3i10)') node,scanDAY,scanYear,scanUTC
    Read(iu,'(10f8.2)') lat(1:nFovs)
    Read(iu,'(10f8.2)') lon(1:nFovs)
    Read(iu,'(10f8.2)') angle(1:nFovs)
    Read(iu,'(10f8.2)') RelAziAngle(1:nFovs)
    Read(iu,'(10f8.2)') SolZenAngle(1:nFovs)
    Read(iu,'(10f8.2)') tb(1:nFovs,1:nchan)
    IF (nqc .gt.0) Read(iu,'(10i4)') qc(1:nqc)
    RETURN
  END SUBROUTINE ReadRadMeasScanLMode_ascii


!===============================================================
!  added for comparison with IDL version
!  -Wanchun Chen
!===============================================================
  SUBROUTINE WriteRadHdrScanLMode_ascii(MeasurFile,iu,nScanL,nFovs,nqc,nchan,CFreq,pol)
    CHARACTER(LEN=*)        :: MeasurFile
    INTEGER                 :: iu,nScanL,nFovs,nqc,nchan
    REAL,    DIMENSION(:)   :: Cfreq
    INTEGER, DIMENSION(:)   :: pol

    !---Open file
    iu=get_lun()
    OPEN(iu,file=MeasurFile,form='formatted',status='unknown')
    !---WRITE header/Place holder
    WRITE(iu,'(4i4)') nScanL,nFovs,nqc,nchan
    WRITE(iu,'(10f10.5)') CFreq
    WRITE(iu,'(20i3)') pol
    RETURN
  END SUBROUTINE WRITERadHdrScanLMode_ascii


!===============================================================
!  added for comparison with IDL version
!  -Wanchun Chen
!===============================================================

  SUBROUTINE WriteRadMeasScanLMode_ascii(iu,nqc,qc,nchan,nFovs,angle,tb,&
       lat,lon,node,scanUTC,scanDAY,scanYear,RelAziAngle,SolZenAngle)
    INTEGER                 :: iu,nchan,nqc,nFovs
    INTEGER, DIMENSION(:)   :: qc
    REAL,    DIMENSION(:,:) :: tb
    REAL,    DIMENSION(:)   :: angle,lat,lon,RelAziAngle,SolZenAngle
    INTEGER                 :: node
    INTEGER                 :: scanDAY,scanYear
    INTEGER                 :: scanUTC

    WRITE(iu,'(i4,3i10)') node,scanDAY,scanYear,scanUTC
    WRITE(iu,'(10f8.2)') lat(1:nFovs)
    WRITE(iu,'(10f8.2)') lon(1:nFovs)
    WRITE(iu,'(10f8.2)') angle(1:nFovs)
    WRITE(iu,'(10f8.2)') RelAziAngle(1:nFovs)
    WRITE(iu,'(10f8.2)') SolZenAngle(1:nFovs)
    WRITE(iu,'(10f8.2)') tb(1:nFovs,1:nchan)
    IF (nqc .gt.0) WRITE(iu,'(10i4)') qc(1:nqc)
    RETURN
  END SUBROUTINE WRITERadMeasScanLMode_ascii

END MODULE IO_MeasurData

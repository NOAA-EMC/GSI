!$Id: IO_Colocate.f90 1594 2008-10-24 21:25:43Z wchen $
!-----------------------------------------------------------------------------------------------
! Name:         IO_Colocate
! 
! Type:         F90 module
!
! Description:
!       This module is dedicated to the I/O of the collocated datasets.
!       It could be used to create but also to process colocated sets.
!       An example of use would be for instance to read a collocated 
!       set, process it with MIRS and then generate a new colocated 
!       set that includes the added MIRS retrieval.
!
!       S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 2005-2006.
!
!
!          OVERVIEW OF THE FORMAT
!         -------------------------------------------------------
!       The idea of having a single format for colocation 
!       file is driven by the fact that multi-validation
!       tasks are undertaken for MIRS: using multiple 
!       sources (raobs, ARM, dropsondes, etc) and multiple
!       sensors (NOAA-18 AMSU, MHS, SSMIS, etc). Having 
!       separate or inconsistent formats for each 
!       combination would be cumbersome to maintain and 
!       process.
!       The basic assumption in this format is that we have
!       'nColoc' reference points, corresponding to the #
!       of raobs or dropsondes, etc. Surrounding these ref.
!       points are 'nSens' sensors (like AMSU and MHS) as 
!       as well as 'nGeoph' geophysical data sources. The
!       latter could for instance be GDAS analyses or 
!       retrieved data. The header is written once. The 
!       body of the structure is written 'nColoc' times.
!
! Modules needed:
!       - misc
!       - Consts
!       - utils
!
! Subroutines contained:
!       - WriteHdrColoc
!       - WriteColoc
!       - ReadHdrColoc
!       - ReadColoc 
!
! Data type included:
!       - Colocate_type
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE IO_Colocate
  USE misc
  USE Consts
  USE utils
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: WriteHdrColoc,WriteColoc,ReadHdrColoc,ReadColoc
  !---Publicly available data/type definitions
  PUBLIC :: Colocate_type
  !---Declaration sections
  TYPE   :: Colocate_type
    !-------------------------------------------------------
    !    HEADER
    !-------------------------------------------------------
    INTEGER                           :: nColoc          !# colocated ref. points (raobs, dropsondes, etc)
    REAL                              :: DeltaTimeColoc  !Time criterion used for colocation (hours)
    REAL                              :: DeltaLatColoc   !Latitude criterion used for colocation (degs)
    REAL                              :: DeltaLonColoc   !Latitude criterion used for colocation (degs)
    INTEGER                           :: MxPoints        !Maximum # pts allowed around a reference point
    INTEGER                           :: MxLays          !Maximum # layers allowed for vertical profiles
    INTEGER                           :: MxChan          !Maximum # channels allowed for sensors
    INTEGER                           :: MxEDRs          !Maximum # EDRs allowed for geoph sources
    INTEGER                           :: MxParams        !Maximum # params allowed for geoph sources / EDRs
    !---Sensor-related data 
    INTEGER                                  :: nSens    !# sensors collocated around ref. points (max #)
    CHARACTER(LEN=20),DIMENSION(:),  POINTER :: SensID   !Sensors IDs used for colocation (AMSU, MWR, etc)
    INTEGER,          DIMENSION(:),  POINTER :: nChans   !Number of channels for each sensor used
    INTEGER,          DIMENSION(:,:),POINTER :: Polar    !Polarization corresponding to each sensor/channel
    REAL,             DIMENSION(:,:),POINTER :: Freq     !Frequency corresponding to each sensor/channel
    !---Geophysical data source-related data 
    INTEGER                                  :: nGeoph   !# geoph. data sources around ref. points (max #)
    CHARACTER(LEN=20),DIMENSION(:),  POINTER :: GeoSrcID !IDs of Geophysical sources (GDAS, MWRP Retr, etc)
    INTEGER,          DIMENSION(:),  POINTER :: nEDRs    !Number of EDRs (varies by geophysical source) 
    CHARACTER(LEN=20),DIMENSION(:,:),POINTER :: EDRLabels!Labels of EDRs corresponding to each GeoSrc/EDR 
    INTEGER,          DIMENSION(:,:),POINTER :: nParams  !# of Params corresponding to each GeoSrc/EDR 
    !-------------------------------------------------------
    !    BODY
    !-------------------------------------------------------
    INTEGER                               :: irec
    CHARACTER(LEN=20)                     :: colocID     !ID of the colocation reference point
    CHARACTER(LEN=20)                     :: statID      !Station ID of particular colocated ref. point
    !---Time and Space Location Data
    INTEGER                               :: YearRef     !Year of reference point
    INTEGER                               :: MonthRef    !Month of reference point
    INTEGER                               :: DayRef      !Day of reference point
    INTEGER                               :: HourRef     !Hour of reference point
    INTEGER                               :: MinRef      !Minute of reference point
    INTEGER                               :: SfcTypRef   !Surface Type of ref. point
    REAL                                  :: LatRef      !Latitude of reference point               [Degrees]
    REAL                                  :: LonRef      !Longitude of reference point              [Degrees]
    !---Surface-related Data
    REAL                                  :: PsfcRef     !Surface pressure of reference point       [mb]
    REAL                                  :: LandFrRef   !Land Fraction of reference point          [0..1]
    REAL                                  :: TskinRef    !Skin tmperature of reference point        [Kelvin]
    REAL                                  :: TsfcRef     !Surface Air tmperature of reference point [Kelvin]
    REAL                                  :: WindSfcRef  !Surface Wind Speed of reference point     [m/s]
    REAL                                  :: AltRef      !Altitude of Ref. point                    [m]
    !---Atmospheric-Profile-related data
    INTEGER                               :: nLayRef     !Number of layers of ref.-point profile
    REAL,     DIMENSION(:),       POINTER :: PressLayRef !Layer Pressure grid                       [mb]
    REAL,     DIMENSION(:),       POINTER :: TempLayRef  !Layer temperature grid                    [Kelvin]
    REAL,     DIMENSION(:),       POINTER :: WVLayRef    !Layer water vapor mixing ratio grid       [g/Kg]
    REAL                                  :: PtopRef_temp!Top pressure of reference point T profile [mb]
    REAL                                  :: PtopRef_wvap!Top pressure of reference point Q profile [mb]
    !---Sensor-related data
    INTEGER                               :: nSens0      !# sensors around this particular ref. point
    INTEGER,  DIMENSION(:),       POINTER :: SensIndx    !Sensor index vector for this colocation (points 2 SensID)
    INTEGER,  DIMENSION(:),       POINTER :: nPtsRad     !# of rad/tb measurmts around ref point 4 each sensor 
    REAL,     DIMENSION(:,:),     POINTER :: AnglRad     !Angles corresponding 2 different Rad/Tb pts around ref pt.
    REAL,     DIMENSION(:,:,:),   POINTER :: Tb          !Tbs corresponding 2 pts around ref pt. (4 diff. sensors)
    REAL,     DIMENSION(:,:),     POINTER :: LatRad      !Latitudes corresponding 2 different Rad/Tb pts around ref pt.
    REAL,     DIMENSION(:,:),     POINTER :: LonRad      !Longitudes corresponding 2 different Rad/Tb pts around ref pt.
    REAL,     DIMENSION(:,:),     POINTER :: DiffHourRad !Time difference in fractional hours of Rad/Tb pts wrt. ref pt.
    INTEGER,  DIMENSION(:,:),     POINTER :: ScanPos     !Scan Position of rad points
    !---Geophysical data source-related data
    INTEGER                               :: nGeoph0     !# sources for geoph data around this particular ref. point
    INTEGER,  DIMENSION(:),       POINTER :: GeoSrcIndx  !geop Src index vector for this colocation (points 2 GeoSrc)
    INTEGER,  DIMENSION(:),       POINTER :: nPtsGeo     !# of geoph data around ref point 4 each source 
    REAL,     DIMENSION(:,:),     POINTER :: LatGeo      !Latitudes corresponding 2 different geoph measur pts around ref pt.
    REAL,     DIMENSION(:,:),     POINTER :: LonGeo      !Longitudes corresponding 2 different geoph measur pts around ref pt.
    REAL,     DIMENSION(:,:),     POINTER :: DiffHourGeo !Time difference in fractional hours of geoph measur pts wrt. ref pt.
    REAL,     DIMENSION(:,:,:,:), POINTER :: DataGeo     !Geoph data measurements around ref. pt
  END TYPE Colocate_type
  
  !---INTRINSIC functions used in this module
  INTRINSIC :: MAXVAL  
  
  
CONTAINS

 
  SUBROUTINE ReadHdrColoc(iu,ColocFile,ColocData)
    !---Input/Output variables
    TYPE(Colocate_type)      :: ColocData
    CHARACTER(LEN=*)         :: ColocFile
    INTEGER                  :: iu
    !---Local variables
    INTEGER                  :: iSens,iChan,iGeoph,iEDR
    INTEGER                  :: iSens0,iChan0,iGeoph0,iEDR0
    CHARACTER(LEN=40)        :: Ligne
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=ColocFile,form='formatted',status='old')
    !---Read general information
    READ(iu,'(a)')     Ligne
    READ(iu,'(6i8)')   ColocData%nColoc,ColocData%nSens,ColocData%nGeoph,ColocData%MxLays,&
                       ColocData%MxPoints,ColocData%MxParams
    READ(iu,'(3f9.3)') ColocData%DeltaTimeColoc,ColocData%DeltaLatColoc,ColocData%DeltaLonColoc
    !---Read sensor-related information
    IF (ColocData%nSens .gt. 0) THEN
       Allocate(ColocData%sensID(ColocData%nSens),ColocData%nchans(ColocData%nSens),     &
                ColocData%SensIndx(ColocData%nSens),ColocData%nPtsRad(ColocData%nSens),  &
                ColocData%AnglRad(ColocData%nSens,ColocData%MxPoints),                   &
                ColocData%LatRad(ColocData%nSens,ColocData%MxPoints),                    &
                ColocData%LonRad(ColocData%nSens,ColocData%MxPoints),                    &
                ColocData%DiffHourRad(ColocData%nSens,ColocData%MxPoints),               &
                ColocData%ScanPos(ColocData%nSens,ColocData%MxPoints))
       READ(iu,'(4a10)')  ColocData%sensID(1:ColocData%nSens)
       READ(iu,'(4i3)')   ColocData%nchans(1:ColocData%nSens)
       ColocData%MxChan  = maxval(ColocData%nchans(1:ColocData%nSens))
       Allocate(ColocData%polar(ColocData%nSens,ColocData%MxChan),                       &
                ColocData%freq(ColocData%nSens,ColocData%MxChan),                        &
                ColocData%tb(ColocData%nSens,ColocData%MxPoints,ColocData%MxChan))
       DO iSens=1,ColocData%nSens
          DO iChan=1,ColocData%nchans(iSens)
             READ(iu,'(3i3,f10.4)') iSens0,iChan0,ColocData%polar(iSens,iChan),ColocData%freq(iSens,iChan)
          ENDDO
       ENDDO
    ENDIF
    !---Read data related to Geoph data sources
    IF (ColocData%nGeoph .gt. 0) THEN
       Allocate(ColocData%GeoSrcID(ColocData%nGeoph),ColocData%nEDRs(ColocData%nGeoph),     &
                ColocData%GeoSrcIndx(ColocData%nGeoph),ColocData%nPtsGeo(ColocData%nGeoph), &
                ColocData%LatGeo(ColocData%nGeoph,ColocData%MxPoints),                      &
                ColocData%LonGeo(ColocData%nGeoph,ColocData%MxPoints),                      &
                ColocData%DiffHourGeo(ColocData%nGeoph,ColocData%MxPoints))
       READ(iu,'(4a10)')  ColocData%GeoSrcID(1:ColocData%nGeoph)
       READ(iu,'(4i3)')   ColocData%nEDRs(1:ColocData%nGeoph)
       ColocData%MxEDRs  = maxval(ColocData%nEDRs(1:ColocData%nGeoph))
       Allocate(ColocData%nParams(ColocData%nGeoph,ColocData%MxEDRs),                       &
            ColocData%EDRLabels(ColocData%nGeoph,ColocData%MxEDRs),                         &
            ColocData%DataGeo(ColocData%nGeoph,ColocData%MxPoints,ColocData%MxEDRs,ColocData%MxParams))
       DO iGeoph=1,ColocData%nGeoph
          DO iEDR=1,ColocData%nEDRs(iGeoph)
             READ(iu,'(3i4,3x,a20)') iGeoph0,iEDR0,ColocData%nParams(iGeoph,iEDR),ColocData%EDRLabels(iGeoph,iEDR)
          ENDDO
       ENDDO
    ENDIF
    READ(iu,'(a)') Ligne
    !---Allocate other arrays for when we read the body of the colocation file
    ALLOCATE(ColocData%PressLayRef(ColocData%MxLays),ColocData%TempLayRef(ColocData%MxLays),&
             ColocData%WVLayRef(ColocData%MxLays))
    RETURN
  END SUBROUTINE ReadHdrColoc



  SUBROUTINE WriteHdrColoc(iu,ColocFile,nColoc,DeltaTimeColoc,DeltaLatColoc,DeltaLonColoc,&
                           nSens,nGeoph,sensID,nchans,freq,polar,GeoSrcID,nEDRs,EDRLabels,&
                           nParams,MxLays,MxPoints,MxParams)
    !---Input/Output variables
    CHARACTER(LEN=*)                  :: ColocFile
    INTEGER                           :: iu,MxLays,MxPoints,MxParams
    INTEGER                           :: nColoc,nSens,nGeoph
    REAL                              :: DeltaTimeColoc,DeltaLatColoc,DeltaLonColoc
    CHARACTER(LEN=*), DIMENSION(:)    :: sensID,GeoSrcID
    INTEGER,          DIMENSION(:)    :: nchans,nEDRs
    REAL,             DIMENSION(:,:)  :: freq
    INTEGER,          DIMENSION(:,:)  :: polar,nParams
    CHARACTER(LEN=*), DIMENSION(:,:)  :: EDRLabels
    !---Local variables
    INTEGER                           :: iSens,iChan,iGeoph,iEDR
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=ColocFile,form='formatted',status='unknown')
    !---Write general information
    WRITE(iu,'(a15)')   LabelColocFile
    WRITE(iu,'(6i8)')   nColoc,nSens,nGeoph,MxLays,MxPoints,MxParams
    WRITE(iu,'(3f9.3)') DeltaTimeColoc,DeltaLatColoc,DeltaLonColoc
    !---Write sensor-related information
    IF (nSens.gt.0) THEN 
       WRITE(iu,'(4a10)')  sensID(1:nSens)
       WRITE(iu,'(4i3)')   nchans(1:nSens)
       DO iSens=1,nSens
          DO iChan=1,nchans(iSens)
             WRITE(iu,'(3i3,f10.4)') iSens,iChan,polar(iSens,iChan),freq(iSens,iChan)
          ENDDO
       ENDDO
    ENDIF
    !---Write data related to Geoph data sources
    IF (nGeoph.gt.0) THEN 
       WRITE(iu,'(4a10)')  GeoSrcID(1:nGeoph)
       WRITE(iu,'(4i3)')   nEDRs(1:nGeoph)
       DO iGeoph=1,nGeoph
          DO iEDR=1,nEDRs(iGeoph)
             WRITE(iu,'(3i4,3x,a20)') iGeoph,iEDR,nParams(iGeoph,iEDR),EDRLabels(iGeoph,iEDR)
          ENDDO
       ENDDO
    ENDIF
    WRITE(iu,'(a)') '----------------END OF HEADER------------------'
    RETURN
  END SUBROUTINE WriteHdrColoc




  SUBROUTINE WriteColoc(iu,irec,colocID,statID,YearRef,MonthRef,DayRef,HourRef,MinRef,SfcTypRef,&
                        LatRef,LonRef,PsfcRef,LandFrRef,TskinRef,TsfcRef,WindSfcRef,AltRef,     &
                        nLayRef,PressLayRef,TempLayRef,WVLayRef,nSens0,SensIndx,nPtsRad,        &
                        AnglRad,LatRad,LonRad,DiffHourRad,Tb,nchans,nGeoph0,GeoSrcIndx,         &
                        nPtsGeo,nEDRs,nParams,LatGeo,LonGeo,DiffHourGeo,DataGeo,ptopRef_temp,   &
                        ptopRef_wvap,ScanPos)

    !---Input/Output variables
    CHARACTER(LEN=*)            :: colocID,statID
    INTEGER                     :: iu,YearRef,MonthRef,DayRef,HourRef,MinRef,SfcTypRef
    INTEGER                     :: nLayRef,nSens0,nGeoph0,irec
    REAL                        :: LatRef,LonRef,PsfcRef,LandFrRef,TskinRef,TsfcRef
    REAL                        :: WindSfcRef,AltRef,ptopRef_temp,ptopRef_wvap
    REAL,    DIMENSION(:)       :: PressLayRef,TempLayRef,WVLayRef
    INTEGER, DIMENSION(:)       :: SensIndx,GeoSrcIndx,nPtsRad,nchans,nPtsGeo,nEDRs
    INTEGER, DIMENSION(:,:)     :: nParams,ScanPos
    REAL,    DIMENSION(:,:)     :: AnglRad,LatRad,LonRad,DiffHourRad,LatGeo,LonGeo,DiffHourGeo
    REAL,    DIMENSION(:,:,:)   :: Tb
    REAL,    DIMENSION(:,:,:,:) :: DataGeo
    !---Local variables
    INTEGER                     :: iLay,iSens,iPt,iGeoph,iEDR
    !---Write location data and surface-related info
    WRITE(iu,'(i8,2x,2a10,i5,5i4,i10,7f10.3,3f9.2,2i4)') irec,colocID,statID,YearRef,MonthRef,   &
         DayRef,HourRef,MinRef,nLayRef,SfcTypRef,LatRef,LonRef,PsfcRef,ptopRef_temp,ptopRef_wvap,&
         TskinRef,TsfcRef,LandFrRef,WindSfcRef,AltRef,nSens0,nGeoph0
    !---Write geophysical data of reference point
    DO ilay=1,nLayRef
       WRITE(iu,'(i3,3f9.3)') iLay,PressLayRef(iLay),TempLayRef(iLay),WVLayRef(iLay)
    ENDDO
    !---Write sensor-related data (brightness temperatures)
    DO iSens=1,nSens0
       WRITE(iu,'(2i5)') SensIndx(iSens),nPtsRad(iSens)
       DO ipt=1,nPtsRad(iSens)
          WRITE(iu,'(i5,4f8.2,i4)') iPt,AnglRad(iSens,iPt),LatRad(iSens,iPt),LonRad(iSens,iPt),&
               DiffHourRad(iSens,iPt),ScanPos(iSens,iPt)
          WRITE(iu,'(10f7.2)') Tb(iSens,iPt,1:nchans(iSens))
       ENDDO
    ENDDO
    !---Write data related to other geophysical sources (other than Ref, like GDAS, etc)
    DO iGeoph=1,nGeoph0
       WRITE(iu,'(2i5)') GeoSrcIndx(iGeoph),nPtsGeo(iGeoph)
       DO ipt=1,nPtsGeo(iGeoph)
          WRITE(iu,'(i5,4f8.2)') iPt,LatGeo(iGeoph,iPt),LonGeo(iGeoph,iPt),DiffHourGeo(iGeoph,iPt)
          DO iEDR=1,nEDRs(iGeoph)
             Write(iu,'(10f10.3)') DataGeo(iGeoph,ipt,iEDR,1:nParams(iGeoph,iEDR))
          ENDDO
       ENDDO
    ENDDO
    RETURN
  END SUBROUTINE WriteColoc






  SUBROUTINE ReadColoc(iu,ColocData,ierr)
    !---Input/Output variables
    TYPE(Colocate_type)      :: ColocData
    INTEGER                  :: iu,ierr
    !---Local variables
    INTEGER                  :: iLay,iSens,iPt,iGeoph,iEDR
    INTEGER                  :: iLay0,iPt0
    !---Read location data and surface-related info
    ierr=0
    READ(iu,'(i8,2x,2a10,i5,5i4,i10,7f10.3,3f9.2,2i4)',iostat=ierr) ColocData%irec,ColocData%colocID,      &
         ColocData%statID,ColocData%YearRef,ColocData%MonthRef,ColocData%DayRef,ColocData%HourRef,         &
         ColocData%MinRef,ColocData%nLayRef,ColocData%SfcTypRef,ColocData%LatRef,ColocData%LonRef,         &
         ColocData%PsfcRef,ColocData%ptopRef_temp,ColocData%ptopRef_wvap,ColocData%TskinRef,               &
         ColocData%TsfcRef,ColocData%LandFrRef,ColocData%WindSfcRef,ColocData%AltRef,ColocData%nSens0,     &
         ColocData%nGeoph0
    IF (ierr .ne. 0) RETURN
    !---Read geophysical data of reference point
    DO ilay=1,ColocData%nLayRef
       READ(iu,'(i3,3f9.3)') iLay0,ColocData%PressLayRef(iLay),ColocData%TempLayRef(iLay),ColocData%WVLayRef(iLay)
    ENDDO
    !---Read sensor-related data (brightness temperatures)
    DO iSens=1,ColocData%nSens0
       READ(iu,'(2i5)') ColocData%SensIndx(iSens),ColocData%nPtsRad(iSens)
       DO ipt=1,ColocData%nPtsRad(iSens)
          READ(iu,'(i5,4f8.2,i4)') iPt0,ColocData%AnglRad(iSens,iPt),ColocData%LatRad(iSens,iPt),   &
               ColocData%LonRad(iSens,iPt),ColocData%DiffHourRad(iSens,iPt),ColocData%ScanPos(iSens,iPt)
          READ(iu,'(10f7.2)') ColocData%Tb(iSens,iPt,1:ColocData%nchans(iSens))
       ENDDO
    ENDDO
    !---Read data related to other geophysical sources (other than Ref, like GDAS, etc)
    DO iGeoph=1,ColocData%nGeoph0
       READ(iu,'(2i5)') ColocData%GeoSrcIndx(iGeoph),ColocData%nPtsGeo(iGeoph)
       DO ipt=1,ColocData%nPtsGeo(iGeoph)
          READ(iu,'(i5,4f8.2)') iPt0,ColocData%LatGeo(iGeoph,iPt),ColocData%LonGeo(iGeoph,iPt),  &
               ColocData%DiffHourGeo(iGeoph,iPt)
          DO iEDR=1,ColocData%nEDRs(iGeoph)
             READ(iu,'(10f10.3)') ColocData%DataGeo(iGeoph,ipt,iEDR,1:ColocData%nParams(iGeoph,iEDR))
          ENDDO
       ENDDO
    ENDDO
    RETURN
  END SUBROUTINE ReadColoc


END MODULE IO_Colocate

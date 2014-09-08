!
!        dataformats.inc
!
!*************************************************
!           a, b and f deck data records
!*************************************************/

!
!  I added the SEQUENCE statements back in because the 
!  NCEP IBM mainframe would not compile dataio.f without them.
!  03/2004   A. Schrader
!



      integer  AidTauMax
      PARAMETER  (AidTauMax = 36)
      integer  AidRadMax
      PARAMETER  (AidRadMax = 4)
      integer  ObjectiveAidsTaus
      PARAMETER  (ObjectiveAidsTaus = 200)

!       Fix Format type definitions
! 
        integer DVTStype
        integer DVTOtype
        integer MICRtype
        integer SCATtype
        integer RDRtype
        integer AIRCtype
        integer DROPtype
        integer ANALtype
        parameter(DVTStype=10)
        parameter(DVTOtype=20)
        parameter(MICRtype=30)
        parameter(SCATtype=31)
        parameter(RDRtype=40)
        parameter(AIRCtype=50)
        parameter(DROPtype=60)
        parameter(ANALtype=70)


      type ATCF_RECORD
         SEQUENCE
         character      basin*2
         character      cyNum*2
         character      DTG*10
         character      technum*2
         character      tech*4
         character      tau*3
         character      latns*4
         character      lonew*5
         character      vmax*3
         character      mslp*4
         character      ty*2
         character      rad*3
         character      windcode*3
         character*4    radii(4)
         character      radp*4
         character      rrp*4
         character      mrd*3
         character      gusts*3
         character      eye*3
         character      subregion*3
         character      maxseas*3
         character      initials*3
         character      dir*3
         character      speed*3
         character      stormname*10
         character      depth*1
         character      seas*2
         character      seascode*3
         character*3    seasrad(4)
      end type ATCF_RECORD

!
!   Data structure to hold best track records (b-deck)
!
      type B_RECORD
         SEQUENCE
         character      basin*2
         integer        cyNum
         character      DTG*10
         character      tech*4
         integer        tau
         real           lat
         character      NS*1
         real           lon
         character      EW*1
         integer        vmax
         integer        mslp
         character      ty*2
         character      stormname*10
      end type B_RECORD

!
!   Data structure to hold objective aids records (a-deck)
!
      type A_RECORD
         SEQUENCE
         character      basin*2
         integer        cyNum
         character      DTG*10
         integer        technum
         character      tech*4
         integer        tau
         real           lat
         character      NS*1
         real           lon
         character      EW*1
         integer        vmax
         integer        mslp
         character      ty*2
         integer        rad
         character      windcode*3
         integer        radii(4)
         integer        radp
         integer        rrp
         integer        mrd
         integer        gusts
         integer        eye
         character      subregion*3
         integer        maxseas
         character      initials*3
         integer        dir
         integer        speed
         character      stormname*10
         character      depth*1
         integer        seas
         character      seascode*3
         integer        seasrad(4)
      end type A_RECORD

!
!   Data structure to hold all objective aid info for a single technique
!   and a single DTG
!
      type AID_DATA
         SEQUENCE
         integer        numrcrds
         type (A_RECORD)    aRecord(AidTauMax*AidRadMax)
         type (ATCF_RECORD) atcfRcd(AidTauMax*AidRadMax)
      end type AID_DATA

!
!   Data structure to hold all objective aid info for a single DTG
!
      type BIG_AID_DATA
         SEQUENCE
         integer        numrcrds
         type (A_RECORD)    aRecord(100*AidTauMax*AidRadMax)
         type (ATCF_RECORD) atcfRcd(100*AidTauMax*AidRadMax)
      end type BIG_AID_DATA

!******************************************
!   OBJECTIVE AIDS STRUCTURES
!******************************************
!
!   Information for one tau of objective aid data.
!

      type PNT
         SEQUENCE
         integer*2      v
         integer*2      h
      end type PNT

      type OBJECTIVE_AIDS_DATAPOINT
         SEQUENCE
         logical*2      IsValid
         logical*2      IsEmpty
         integer        tau;
         real           Latitude
         logical*2      North
         real           Longitude
         logical*2      East
         type (PNT)     pos
         integer*2      Wind
         character*3    windcode(4)
         integer        radii(4, 4)
         integer        dir
         integer        speed
      end type OBJECTIVE_AIDS_DATAPOINT

!
!   A collection of taus where 0=12hrs, 1=24hrs, 2=36hrs, 3=48hrs, 4=72hrs...
!
      type OBJECTIVE_AIDS_DATAPOINTS
         SEQUENCE
         type (OBJECTIVE_AIDS_DATAPOINT) Point(ObjectiveAidsTaus)
      end type OBJECTIVE_AIDS_DATAPOINTS

!
!   structure to contain all the data for one DTG of one aid technique
!   NOTE! make sure to zero the entire contents before ANY use.
!
      type OBJECTIVE_AIDS_RCRD
         SEQUENCE
         character      StormID*32
         character      Technique*16
         character      DTG*10
         type (OBJECTIVE_AIDS_DATAPOINTS) Data
      end type OBJECTIVE_AIDS_RCRD

!
!   Info for an objective aid data record.
!
      type OBJAID_RECORD
         SEQUENCE
         character      techname*5
         character      dtg*10
         integer        lat(ObjectiveAidsTaus)
         integer        lon(ObjectiveAidsTaus)
         integer        vmax(ObjectiveAidsTaus)
         integer        mslp(ObjectiveAidsTaus)
         character      stormID*8
         character      initials*3
         integer        dir
         integer        speed
      end type OBJAID_RECORD

!******************************************
!   FIX STRUCTURES
!******************************************

      type FIX_RECORD
         SEQUENCE
         character      basin*2
         character      cyNum*2
         character      DTG*12
         character      fixformat*3
         character      fixtype*4
         character      cicode*10
         character      flagged         
         character      latns*5
         character      lonew*6
         character      height*5
         character      positConf
         character      v*3
         character      vConf
         character      pressure*4
         character      presConf
         character      presDeriv*4
         character      rad*3
         character      windcode*3
         character*4    radii(4)
         character      radMod(4)
         character      radConf
         character      mrd*3
         character      eye*3
         character      subregion
         character      fixsite*5
         character      initials*3
         character      remainder*200
      end type FIX_RECORD


!******************************************
!     Individual Fix Type Record Structures
!******************************************/

!
!  dvorak code
!
      type DVORAK
         SEQUENCE
         integer       tnum       !  t number, tenths, 0.0-8.0 or blank
         integer       cinum      !  ci number, tenths, 0.0-8.0 or blank
         character     intChg     !  forecast intensity chg, + - or " "
         character     pastChg    !  D-developed, S-steady, W-weakened
         integer       tnumChg    !  amount of tnum change, tenths, 0.0-8.0 or blank
         integer       lastEvalHrsAgo  !  hours since previous eval, 0-96 hrs
      end type DVORAK


!
!  Subjective dvorak technique
!
      type DVTS_RECORD
         SEQUENCE
         character     sensor*4      !  sensor type - vis, ir, microwave ...
         integer       pcn           !  deprecated
         type (DVORAK) longTerm
         type (DVORAK) shortTerm
         integer       ci24hr        !  ci 24 hour forecast
         character     satType*6     !  GMS, DMSP, DMSP45, TRMM, NOAA...
         character     centertype*4  !  CSC, LLCC, ULCC ...
         character     tropical      !  S-subtropical, E-extratropical, T-tropical
         character     comments*52
      end type DVTS_RECORD



!
!  Objective dvorak technique
!
      type DVTO_RECORD
         SEQUENCE
         character     sensor*4       !  sensor type - V, I, M, VI, IM, VM, VIM
         integer       cinum
         integer       ciConf
         integer       tnumAvg        !  t num average
         integer       tnumAvgTime    !  t num averaging time period (hrs)
         character     tnumAvgDeriv   !  L, T ...
         integer       tnumRaw        !  t num raw
         integer       eyeTemp        !  temperature (eye), celsius
         integer       cloudTemp      !  temperature (cloud surrounding eye)
         character     sceneType*4    !  CDO, EEYE ...
         character     algorithm*2    !  R9, RP ...
         character     satType*6      !  GMS, DMSP, DMSP45, TRMM, NOAA...
         character     tropical       !  S-subtropical, E-extratropical, T-tropical
         character     comments*52
      end type DVTO_RECORD


!
!  Microwave
!
      type MICROWAVE_RECORD
         SEQUENCE
         logical*2   rain             !  rain flag
         integer     rainrate         !  rain rate - mm/hour
         character   algorithm*6      !  FNMOC, NESDIS, RSS ...
         integer     wave             !  wave height (active micro), 0-99 ft
         integer     temp             !  (passive mircowave)
         integer     slpraw           !  SLP raw, amsu only
         integer     slpretr          !  SLP retrieved, amsu only
         integer     seas             !  max seas, altimeter, 0-999 ft
         character   satType*6        !  GMS, DMSP, DMSP45, TRMM, NOAA...
         integer     rad
         character   windcode*4
         integer     radii(8)
         logical*2   edge(8)          !  edge of pass
         logical*2   cut(8)           !  cut off by land
         integer     radConf
         character   comments*52
      end type MICROWAVE_RECORD


!
!  Radar
!
      type RADAR_RECORD
         SEQUENCE
         character   rdrtype         !  L-land,S-ship,A-Aircraft,T-Satellite
         logical*2   radobcode
         logical*2   plainlanguage
         logical*2   doppler
         character   radob*10        !  radob code - ASWartddff
         character   eyeShape*2      !  CI-Circ.; EL-Elliptic; CO-concentric
         integer     eyeWallOb       !  percent of eye wall observed, 99=100%
         integer     spiralOv        !  spiral overlay - degrees
         real        lat             !  radar site position
         character   NS
         real        lon
         character   EW
         integer     vmaxin          !  inbound max wind 0-300 kts
         integer     azimuthin       !  degrees, 1-360
         integer     rangein         !  nm
         integer     elevin          !  feet
         integer     vmaxout         !  outbound max wind 0-300 kts
         integer     azimuthout      !  degrees, 1-360
         integer     rangeout        !  nm
         integer     elevout         !  feet
         integer     cloudHeight     !  max cloud height, 0-70000 ft
         real        rainAccum       !  rain accumulation, hundreths of inches
         integer     rainAcTimeInt   !  rain accum. time interval, hours
         real        rainlat         !  rain accumulation position
         character   rainNS
         real        rainlon
         character   rainEW
         character   comments*52
      end type RADAR_RECORD


!
!  Aircraft
!
      type AIRCRAFT_RECORD
         SEQUENCE
         integer*2 FlightLevelFt         !  The flight level in 100s feet
         integer*2 FlightLevelMB         !  The flight level in millibars
         integer*2 MinHeight             !  In meters

         integer*2 MaxSurfaceWind        !  0-300 Knots
         integer*2 MaxSurfaceWindBearing !  0-360 degrees
         integer*2 MaxSurfaceWindRange   !  0-999 nautical miles

         integer*2 MaxFlightLevelWindDir !  0-360 degrees
         integer*2 MaxFlightLevelWindInt !  0-999 knots
         integer*2 MaxFlightLevelWindBearing !  0-360 degrees
         integer*2 MaxFlightLevelWindRange   !  0-999 nautical miles

         integer*2 MinSeaLevelPressure   !  0-9999 mb

         integer*2 EyeTempOutside        !  0-99 degrees      C
         integer*2 EyeTempInside         !  0-99 degrees      C

         integer*2 DewPoint              !  0-99 degrees      C
         integer*2 SeaSurface            !  0-99 degrees

         integer*2 WallCloudThickness    !  0-99 nautical miles

         integer*2 EyeShape    !  0=none, 1=CI (circ), 2=EL (elip), 3=CO (concentric)
         integer*2 EyeOrientation        !  0-360 degrees
         integer*2 DiameterLongAxis      !  0-99 nm (ONLY IF EYE SHAPE IS ELIPTICAL)
         integer*2 DiameterShortAxis     !  0-99 nm (ONLY IF EYE SHAPE IS ELIPTICAL)

         real NavigationalAccuracy       !  0-99.9 tenths of nautical miles
         real NavigationalMeteorological !  0-99.9 tenths of nautical miles

         integer*2 MissionNumber         !  0-99

         character      comments*52
      end type AIRCRAFT_RECORD


!
!  Dropsonde
!
      type DROPSONDE_RECORD
         SEQUENCE
         character      sondeEnv*10      !  Sonde environment - EYEWALL, EYE,
                                         !  RAINBAND, MXWNDBND, SYNOPTIC
         integer        height150        !  midpoint 150 m height
         integer        vspd150          !  speed of mean wind over lowest 150 m
         integer        vspd500          !  speed of mean wind from 0-500 m
         character      comments*52
      end type DROPSONDE_RECORD


!
!  Analysis
!
      type ANALYSIS_RECORD
         SEQUENCE
         character      initials*3             !  analyst initials
         character      startDTG*12            !  YYYYMMDDHHMM
         character      endDTG*12              !  YYYYMMDDHHMM
         integer        DistanceToNearestData  !  0-999 nautical miles
         integer        SST                    !  celsius
         character      obSources*24           !  observation sources
         character      comments*52
      end type ANALYSIS_RECORD



      type F_RECORD
         SEQUENCE
         character      basin*2
         integer        cyNum
         character      DTG*12           !  YYYYMMDDHHMM
         integer        fixformat        !  10 - subjective dvorak
                                         !  20 - objective dvorak
                                         !  30 - microwave
                                         !  40 - radar
                                         !  50 - aircraft
                                         !  60 - dropsonde
                                         !  70 - analysis
         character      fixtype*4
         logical*2      centerFix
         logical*2      intensityFix
         logical*2      radiiFix
         logical*2      pressureFix
         character      flagged          !  F-flagged fix
         real           lat
         character      NS
         real           lon
         character      EW
         integer        height
         integer        positConf
         integer        v
         integer        vConf
         integer        pressure
         integer        presConf
         character      presDeriv*4
         integer        rad
         character      windcode*4
         integer        radii(4)
         logical*2      edge(4)
         logical*2      cut(4)
         integer        radConf
         integer        mrd              !  radius of max winds
         integer        eye              !  eye diameter
         character      subregion
         character      fixsite*5
         character      initials*3
         type (DVTS_RECORD) dvts
         type (DVTO_RECORD) dvto
         type (MICROWAVE_RECORD) micro
         type (RADAR_RECORD) radar
         type (AIRCRAFT_RECORD) air
         type (DROPSONDE_RECORD) drop
         type (ANALYSIS_RECORD) anal
      end type F_RECORD

!
!   Data structure to hold all of the fix info for a single fix,
!   including all the records required for the 34,50,64 & 100 kt wind radii.
!
      type FIX_DATA
         SEQUENCE
         character       stormID*8
         integer         file_pos      ! compatibile w/dataformats.h
         integer         numrcrds
         type (F_RECORD) fRecord(AidRadMax)
      end type FIX_DATA

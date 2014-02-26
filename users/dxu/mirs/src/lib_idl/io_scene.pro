;$Id: io_scene.pro 1369 2008-06-26 16:05:35Z sidb $
;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; different scene files (geophysical data).
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------



;-------------------------------------------------------------------------------------
;
;
; Geophysical I/O subroutines
;
;
;-------------------------------------------------------------------------------------

;===============================================================
; Name:		WriteSceneFileHdr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes the header of the scene file with all
;               info coming from inputs
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- iu                 O           Unit number       
;	- file               I           Name of file to write
;	- iTyp               I           Type of file (truth:0,retr:1)
;       - algSN              I           Algorithm Serial number(svn)
;	- nPrf               I           Number of profiles
;	- nLay               I           Number of layers
;	- nLev               I           Number of levels
;	- nChan              I           Number of channels
;	- nAbsorb            I           Number of absorbents
;	- nParmCLW           I           # params 2 define non-prec cld
;	- nParmRain          I           # params 2 define rain
;	- nParmSnow          I           # params 2 define snow
;	- nParmIce           I           # params 2 define ice
;	- nParmGrpl          I           # params 2 define graupel
;	- AbsorbID           I           vector of absorbents IDs
;	- cfreq              I           Central frequencies
;	- polar              I           Polarizations
;	- nqc                I           # elements of Atmospheric QC
;	- nPosScan           I           # scan positions within scanline
;;	- nScanLines         I           # scan lines within orbit 

;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO WriteSceneFileHdr,iu,file,iTyp,nPrf,nLay,nLev,nChan,nAbsorb,nParmCLW,nParmRain,nParmSnow,nParmIce,nParmGrpl,$
                   AbsorbID,cfreq,polar,nqc,nPosScan,nScanLines,AlgSN
    ;----------------------------------------------------------------------------------
    ; Subroutine to write the Scene file (either truth file or MIRS-retrieved file).
    ;
    ;    Sid Ahmed Boukabara. IMSG Inc. @ NOAA/NESDIS/ORA. Jan. 2006
    ;----------------------------------------------------------------------------------
    OPENW,iu,file,/get_lun,/f77_unformatted;,/swap_if_little_endian
    writeu,iu,iTyp,AlgSN
    writeu,iu,nPrf
    writeu,iu,nLay
    writeu,iu,nLev
    writeu,iu,nChan
    writeu,iu,nPosScan
    writeu,iu,nScanLines
    writeu,iu,nAbsorb
    writeu,iu,nParmCLW
    writeu,iu,nParmRain
    writeu,iu,nParmSnow
    writeu,iu,nParmIce
    writeu,iu,nParmGrpl
    writeu,iu,AbsorbID
    writeu,iu,cfreq
    writeu,iu,polar
    writeu,iu,nqc
END

;===============================================================
; Name:		WriteSceneFile
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes the scene content to a file (assumed 
;               already open and header-filled).
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- iu                 I           Unit number    
;	- iTyp               I           Type of file (truth:0,retr:1)
;	- nLay               I           Number of layers
;	- nLev               I           Number of levels
;	- nChan              I           Number of channels
;	- nAbsorb            I           Number of absorbents
;	- nParmCLW           I           # params 2 define non-prec cld
;	- nParmRain          I           # params 2 define rain
;	- nParmSnow          I           # params 2 define snow
;	- nParmIce           I           # params 2 define ice
;	- nParmGrpl          I           # params 2 define graupel
;	- AbsorbID           I           vector of absorbents IDs
;	- nqc                I           # elements of Atmospheric QC
;	- ProfIndx           I           Index of the profile
;	- PresLay            I           Layer-based pressure grid 
;	- PresLev            I           Level-based pressure grid
;	- TempLay            I           Layer-based temp. profile
;	- AbsorbLayVec       I           Absorbents array (nLay x nAbsorb)
;	- ClwLay             I           Layer cloud profile
;	- RainLay            I           Layer rain profile
;	- SnowLay            I           Layer snow profile
;	- IceLay             I           Layer ice profile
;	- GrplLay            I           Layer graupel amount profile
;	- Angl               I           Viewing angle
;	- RelAziAngl         I           Relative Azimuth angle
;	- SolZenAngl         I           Solar Zenith angle
;	- Emiss              I           Emissivity vector
;	- Refl               I           Reflectivity vector
;	- WindSp             I           Wind speed [m/s]
;	- Tskin              I           Skin temperature [K]
;	- SnowDepth          I           Snow Depth [mm]
;	- SfcPress           I           Surface pressure [mb]
;	- SfcTyp             I           Surface type [see consts]
;	- qc                 I           Vector of Atmospheric QC elemts
;	- lat                I           Latitude
;	- lon                I           Longitude
;	- node               I           Orbit mode
;	- scanUTC            I           UTC time
;	- scanYear           I           Year
;	- scanDay            I           Julian day
;	- nAtt               I           Number of retrieval attempts
;	- nIter              I           Number iterations (last attempt)
;	- Chisq              I           Convergence metric
;	- windU              I           U-direction wind
;	- windV              I           V-direction wind
;	- YFwd               I           Latest forward simulations
;	- Ym                 I           TB measurements (uncorrected)
; 	- YmCorr             I           TB measurements used for the retrieval

;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO WriteSceneFile,iu,iTyp,nLay,nLev,nChan,nAbsorb,nParmCLW,nParmRain,nParmSnow,nParmIce,nParmGrpl,$
                   nqc,ProfIndx,PresLay,PresLev,TempLay,AbsorbLayVec,ClwLay,RainLay,SnowLay,$
                   IceLay,GrplLay,Angl,Emiss,Refl,WindSp,Tskin,SfcPress,SfcTyp,qc,lat,lon,node,$
                   scanUTC,scanYear,scanDay,nAtt,nIter,Chisq,windU,windV,YFwd,ChanSel,Ym,RelAziAngl,SolZenAngl,YmCorr,$
                   snowDepth
    writeu,iu, ProfIndx
    ;---Atmospheric constituents
    writeu,iu, PresLay(0:nLay-1) 
    writeu,iu, PresLev(0:nLev-1) 
    writeu,iu, TempLay(0:nLay-1) 
    writeu,iu, AbsorbLayVec(0:nLay-1,0:nAbsorb-1) 
    ;---hydrometeors
    writeu,iu, ClwLay(0:nParmCLW-1) 
    writeu,iu, RainLay(0:nParmRain-1) 
    writeu,iu, GrplLay(0:nParmGrpl-1) 
    ;---emiss/reflec
    writeu,iu, Emiss(0:nchan-1)
    ;---Surface-level paremeters
    writeu,iu, Angl,WindSp,Tskin,SfcPress,SfcTyp,windU,windV,RelAziAngl,SolZenAngl,snowDepth
    ;---QC variables 
    writeu,iu, qc(0:nqc-1)
    ;---Positioning variables
    writeu,iu, lat,lon,node,scanUTC,scanYear,scanDay
    IF (iTyp eq 1) THEN BEGIN
        writeu,iu,nAtt,nIter,Chisq
        writeu,iu,YFwd(0:nchan-1)
        writeu,iu,ChanSel(0:nchan-1)
        writeu,iu,Ym(0:nchan-1)
        writeu,iu,YmCorr(0:nchan-1)
    ENDIF
END


;===============================================================
; Name:		ReadScene
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the content of a file (will be opened and then read).
;
;
; Arguments:
;
;	    Name             Type	    Description
;      ---------------------------------------------------
;	- file_in             I            Name of file to read from
;	- nprofiles           O            Number of profiles read
;	- nlay                O            Number of layers
;	- nLev                O            Number of levels
;	- nChan               O            Number of channels
;	- nAbsorb             O            Number of absorbents
;	- nParmCLW            O            # params 2 define non-prec cld
;	- nParmRain           O            # params 2 define rain
;	- nParmSnow           O            # params 2 define snow
;	- nParmIce            O            # params 2 define ice
;	- nParmGrpl           O            # params 2 define graupel
;	- AbsorbID            O            vector of absorbents IDs
;	- CentrFrq            O            Central frequencies vector
;	- Polarty             O            Polarity vector
;	- iH2o                O            Water vapor index within ABSORBID
;	- iO3                 O            Ozone index within ABSORBID
;	- ProfIndx            O            Index of the profile (nprof)
;	- PresLay             O            Layer-based pressure grid (nprof x nLay)
;	- PresLev             O            Level-based pressure grid (nprof x nLay)
;	- TempLay             O            Layer-based temp. profile (nprof x nLay)
;	- Abso_lay            O            Absorbents array (nprof x nLay x nAbsorb)
;	- Clw                 O            Layer cloud array (nprof x nLay)
;	- Rain                O            Layer rain array (nprof x nLay)
;	- Snow                O            Layer snow array (nprof x nLay)
;	- Ice                 O            Layer ice array (nprof x nLay)
;	- Grpl                O            Layer graupel amount array (nprof x nLay)
;	- Angle               O            Viewing angle vector (nprof)
;	- RelAziAngle         O            Relative Azimuth angle vector (nprof)
;	- SolZenAngle         O            Solar zenith angle vector (nprof)
;	- Emiss               O            Emissivity array (nprof,nchan)
;	- Refl                O            Reflectivity array (nprof,nchan)
;	- WindSp              O            Wind speed vector (nprof)
;	- Tskin               O            Skin temperature vector (nprof)
;	- SnowDepth           O            snowdepth (nprof)
;	- SfcPress            O            Surface pressure vector (nprof)
;	- SfcTyp              O            Surface type vector (nprof)
;	- deltaT              0            Delta-Temperature vector (nprof)
;	- Scene               O            Structure of geophysical
;                                          data (see definition in top)  
;	- Lat                 O            Latitude vector (nprof)
;	- Lon                 O            Longitude vector (nprof)
;	- windU               O            U-direction wind vector (nprof)
;	- windV               O            U-direction wind vector (nprof)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ReadScene,file_in, nprofiles,nlay,nlev,nchan,nAbsorb,nParmCLW,nParmRain,nParmSnow, $
              nParmIce,nParmGrpl,AbsorbID,CentrFrq,Polarty,iH2o,iO3,ProfIndx,Pres_lay, $
              Pres_lev,Temp_lay,Abso_lay,clw,rain,snow,ice,graupel,Angle,Emiss,Refl,   $
              WindSp,Tskin,SfcPress,SfcTyp,deltaT,Scene,Lat,Lon,windU,windV,RelAziAngle,SolZenAngle,snowdepth

    LoadSceneFile,file_in,topId,Scene,1000000000L
    nprofiles   = Scene.nProfsProcessed
    nlay        = Scene.nLay
    nlev        = Scene.nLev
    nchan       = Scene.nChan
    nAbsorb     = Scene.nAbsorb
    nParmCLW    = Scene.nParmCLW
    nParmRain   = Scene.nParmRain
    nParmSnow   = Scene.nParmSnow
    nParmIce    = Scene.nParmIce
    nParmGrpl   = Scene.nParmGrpl
    AbsorbID    = Scene.AbsorbID
    CentrFrq    = Scene.Cfreq
    Polarty     = Scene.Polarity

    ProfIndx    = Scene.ProfIndxVec(0:nprofiles-1)
    Pres_lay    = Scene.PresLayVec(0:nprofiles-1,*)
    Pres_lev    = Scene.PresLevVec(0:nprofiles-1,*)
    Temp_lay    = Scene.TempLayVec(0:nprofiles-1,*)
    Abso_lay    = Scene.AbsorbLayVec(0:nprofiles-1,*,*)
    clw         = Scene.ClwLayVec(0:nprofiles-1,*)
    rain        = Scene.RainLayVec(0:nprofiles-1,*)
    snow        = Scene.SnowLayVec(0:nprofiles-1,*)
    ice         = Scene.IceLayVec(0:nprofiles-1,*)
    graupel     = Scene.GrplLayVec(0:nprofiles-1,*)
    Angle       = Scene.AnglVec(0:nprofiles-1)
    RelAziAngle = Scene.RelAziAnglVec(0:nprofiles-1)
    SolZenAngle = Scene.SolZenAnglVec(0:nprofiles-1)
    Emiss       = Scene.EmissVec(0:nprofiles-1,*)
    refl        = Scene.ReflVec(0:nprofiles-1,*)
    WindSp      = Scene.WindspVec(0:nprofiles-1)
    WindU       = Scene.WinduVec(0:nprofiles-1)
    WindV       = Scene.WindvVec(0:nprofiles-1)
    Tskin       = Scene.TskinVec(0:nprofiles-1)
    SnowDepth   = Scene.snowdepthVec(0:nprofiles-1)
    SfcPress    = Scene.SfcPressVec(0:nprofiles-1)
    SfcTyp      = Scene.SfcTypVec(0:nprofiles-1)
    deltaT      = Scene.TskinVec(0:nprofiles-1)
    Lat         = Scene.Lat(0:nprofiles-1)
    Lon         = Scene.Lon(0:nprofiles-1)
    FOR j=0L,nprofiles-1 DO BEGIN
        nLayEff = 0
        IF (TOTAL(Scene.qc(j,0:Scene.nqc-1)) eq 0) THEN BEGIN
            FOR i=0,nLay-1 DO BEGIN
                IF (pres_lay(j,i) le SfcPress(j)) THEN BEGIN
                    nLayEff = nLayEff + 1
                ENDIF
            ENDFOR
            deltaT(j)    = Scene.TskinVec(j)-Scene.TempLayVec(j,nlayEff-1)
        ENDIF
    ENDFOR

    H2OID = 1
    O3ID  = 3
    ;---Find the indexes for H2O and O3 gases
    iH2O = where(AbsorbID eq H2OID,ncount)
    IF (ncount ne 1) THEN BEGIN
        print,'Error: Pb finding the H2O ID in the list of absorbers'
        STOP
    ENDIF
    iO3  = where(AbsorbID eq O3ID,ncount)
    IF (ncount ne 1) THEN BEGIN
        print,'Error: Pb finding the O3 ID in the list of absorbers'
        STOP
    ENDIF
END


;===============================================================
; Name:		LoadHdrScene
;
;
; Type:		IDL Subroutine
;
;
; Description:  Loads the header of a Scene file
;
;
; Arguments:
;
;	    Name		    Type	    Description
;      ---------------------------------------------------
;	- fileTruth                  I             Name of the file to read
;	- topId                      I             Not used. Dummy variable.
;	- nProfs2read                I             Number of profiles 2 read
;	- iu                         O             Unit number
;	- iTyp                       O             Type of file (truth:0,retr:1)
;	- nPrf                       O             # profiles actually read
;	- nLay                       O             Number of layers
;	- nLev                       O             Number of levels
;	- nChan                      O             Number of channels
;	- nAbsorb                    O             Number of absorbents
;	- nParmCLW                   O             # params 2 define non-prec cld
;	- nParmRain                  O             # params 2 define rain
;	- nParmSnow                  O             # params 2 define snow
;	- nParmIce                   O             # params 2 define ice
;	- nParmGrpl                  O             # params 2 define graupel
;	- AbsorbID                   O             vector of absorbents IDs
;	- cfreq                      O             Vector of central frequencies
;	- polar                      O             Vector of polarizations
;	- nqc                        O             # elements of Atmospheric QC
;	- Scene                      O             Structure of geophy. data
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO LoadHdrScene,fileTruth,topId,nProfs2read,iu,iTyp,nPrf,nLay,nLev,nChan ,nAbsorb,$
                 nParmCLW,nParmRain,nParmSnow,nParmIce,nParmGrpl,AbsorbID,cfreq,$
                 polar,nqc,Scene         
    OPENR,iu,fileTruth,/get_lun,error=err,/f77_unformatted,/swap_if_little_endian
    IF (err ne 0) THEN BEGIN
        print ,!ERROR_STATE.MSG
        RETURN
    ENDIF
    ligne=''
    ;************************************************
    ;  HEADER
    ;************************************************
    iTyp     =0L & nPrf      =0L & nLay       =0L & nLev       =0L & nChan     =0L & nAbsorb =0L
    nParmCLW =0L & nParmRain =0L & nParmSnow  =0L & nParmIce   =0L & nParmGrpl =0L & nqc     =0L
    nPosScan  =0L & nScanLines =0L & AlgSN =0L
    readu,iu,iTyp,AlgSN
    readu,iu,nPrf
    nPrf=min([nPrf,nProfs2read])
    readu,iu,nLay
    readu,iu,nLev
    readu,iu,nChan
    readu,iu,nPosScan
    readu,iu,nScanLines
    readu,iu,nAbsorb
    readu,iu,nParmCLW
    readu,iu,nParmRain
    readu,iu,nParmSnow
    readu,iu,nParmIce
    readu,iu,nParmGrpl
    AbsorbID=lonarr(nAbsorb)
    AbsorbID2use4declaration=lonarr(2) & AbsorbID2use4declaration=[1,3]
    readu,iu,AbsorbID
    cfreq=fltarr(nChan)
    readu,iu,cfreq
    polar=lonarr(nChan)
    readu,iu,polar
    readu,iu,nqc
    ;----Define the Scene structure
    typeScene=size(scene,/type)
    SceneIsNotDefined = (typeScene ne 8)
    NprfBiggerDeclaredSize=0
    IF (not(SceneIsNotDefined)) THEN NprfBiggerDeclaredSize=(nprf gt Scene.DeclarNprf)
    IF (SceneIsNotDefined or NprfBiggerDeclaredSize) THEN BEGIN
        Scene={                                       $
          ;----Header
          AlgSN:AlgSN,                                $
          iTyp:iTyp,                                  $
          nProf:nPrf,                                 $
          nProfsProcessed:0L,                         $
          nLay:nLay,                                  $ 
          nLev:nLev,                                  $ 
          nChan:nchan,                                $ 
          nScanPos:nPosScan,                          $ 
          nScanLines:nScanLines,                      $ 
          nAbsorb:nAbsorb,                            $ 
          nParmCLW:nParmCLW,                          $ 
          nParmRain:nParmRain,                        $ 
          nParmSnow:nParmSnow,                        $ 
          nParmIce:nParmIce,                          $ 
          nParmGrpl:nParmGrpl,                        $ 
          AbsorbID:AbsorbID2use4declaration,          $
          Cfreq:cfreq,                                $
          Polarity:polar,                             $
          nqc:nqc,                                    $ 
          DeclarNprf:nPrf,                            $
          ;----Body of Scene file
          ProfIndxVec:  intarr(nprf),                 $
          PresLayVec:   fltarr(nprf,nLay),            $
          PresLevVec:   fltarr(nprf,nLev),            $
          TempLayVec:   fltarr(nprf,nLay),            $
          AbsorbLayVec: fltarr(nprf,nLay,2),    $
          TPWvec:       fltarr(nPrf),                 $
          RHvec:        fltarr(nPrf,nLay),            $
          ClwLayVec:    fltarr(nprf,nParmCLW),        $
          CLWvec:       fltarr(nPrf),                 $
          RWPvec:       fltarr(nPrf),                 $
          IWPvec:       fltarr(nPrf),                 $
          GWPvec:       fltarr(nPrf),                 $
          SWPvec:       fltarr(nPrf),                 $
          RainLayVec:   fltarr(nprf,nParmRain),       $
          SnowLayVec:   fltarr(nprf,nParmSnow),       $
          IceLayVec:    fltarr(nprf,nParmIce),        $
          GrplLayVec:   fltarr(nprf,nParmGrpl),       $
          AnglVec:      fltarr(nprf),                 $
          RelAziAnglVec:fltarr(nprf),                 $
          SolZenAnglVec:fltarr(nprf),                 $
          EmissVec:     fltarr(nprf,nChan),           $
          ReflVec:      fltarr(nprf,nChan),           $
          WindspVec:    fltarr(nPrf),                 $
          WinduVec:     fltarr(nPrf),                 $
          WindvVec:     fltarr(nPrf),                 $
          TskinVec:     fltarr(nPrf),                 $
          SnowDepthVec: fltarr(nPrf),                 $
          SfcPressVec:  fltarr(nPrf),                 $
          SfcTypVec:    intarr(nPrf),                 $
          qc:           intarr(nPrf,nqc),             $
          lat:          fltarr(nPrf),                 $
          lon:          fltarr(nPrf),                 $
          Direc:        intarr(nPrf),                 $
          time:         fltarr(nPrf),                 $
          Year:         intarr(nPrf),                 $
          Day:          intarr(nPrf),                 $
          Hours:        fltarr(nPrf),                 $
          Mins:         fltarr(nPrf),                 $
          Secs:         fltarr(nPrf),                 $
          nAttempt:     intarr(nPrf),                 $
          nIter:        intarr(nPrf),                 $
          ChiSq:        fltarr(nPrf),                 $
          YFwd:         fltarr(nPrf,nChan),           $
          ChanSel:      lonarr(nPrf,nChan),           $
          Ym:           fltarr(nPrf,nChan),           $
          YmCorr:       fltarr(nPrf,nChan),           $
          ScanPos:      lonarr(nPrf),                 $
          ScanLine:     lonarr(nPrf)                  $
            }
    ENDIF
    Scene.AlgSN                 = AlgSN
    Scene.nqc                   = nqc
    Scene.nchan                 = nchan
    Scene.iTyp                  = iTyp
    Scene.nProf                 = min([nPrf,Scene.DeclarNprf])
    Scene.nLay                  = nLay
    Scene.nlev                  = nLev
    Scene.nAbsorb               = nAbsorb
    Scene.nParmCLW              = nParmCLW
    Scene.nParmRain             = nParmRain
    Scene.nParmSnow             = nParmSnow
    Scene.nParmIce              = nParmIce
    Scene.nParmGrpl             = nParmGrpl
    Scene.AbsorbID(0:nAbsorb-1) = AbsorbID(0:nAbsorb-1)
    Scene.Cfreq(0:nchan-1)      = cfreq(0:nchan-1)
    Scene.Polarity(0:nchan-1)   = polar(0:nchan-1)
END

;===============================================================
; Name:		LoadCoreScene
;
;
; Type:		IDL Subroutine
;
;
; Description:  Loads the content of an individual profile 
;               in a scene file. This will be used in a loop
;               over the number of profiles to be read.
;               Note: the arrays must be initialized outside the 
;               subroutine with the right dimension. That's why 
;               they are defined as I/O.
;               The vertical integration of the atmospheric profiles is also done
;               in this subroutine. Thi sincludes the integration to obtain TPW,
;               rain water path, ice water path, snow water path as well as total
;               cloud amount. The conversion into relative humidity is also
;               part of this subroutine.
;
; Arguments:
;
;	    Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                         I            Unit number
;	- iprof                      I            Profile loop index
;	- Scene                      I            Geoph data structure
;	- ProfIndx                   I/O          Profile index from file
;	- pressLay                   I/O          Layer Pressure grid
;	- pressLev                   I/O          Level pressure grid
;	- TempLay                    I/O          Layer-based temp. profile (nLay)
;	- Absorbents                 I/O          Absorbents array (nLay x nAbsorb)
;	- xclw                       I/O          Layer cloud profile (nLay)
;	- xrain                      I/O          Layer rain profile (nLay)
;	- xsnow                      I/O          Layer snow profile (nLay)
;	- xice                       I/O          Layer ice profile (nLay)
;	- xGrpl                      I/O          Layer graupel profile (nLay)
;	- emiss                      I/O          Emissivity vector (nchan)
;	- Angl                       O            Viewing angle 
;	- RelAziAngl                 O            Relative Azimuth angle 
;	- SolZenAngl                 O            Solar Zenith angle 
;	- Windsp                     O            Wind speed 
;	- Tskin                      O            Skin temperature
;	- SnowDepth                  O            Snow Depth
;	- SfcPress                   O            Surface pressure
;	- SfcTyp                     I/O          Surface type
;	- qc                         I/O          Atmosph QC vector
;	- lat                        O            Latitude
;	- lon                        O            Longitude
;	- node                       O            Orbit direction
;	- scanUTC                    O            UTC time
;	- scanYear                   O            Year
;	- scanDay                    O            Day
;	- nAtt                       O            Number retrieval attempts
;	- nIter                      O            Number iterations
;	- Chisq                      O            Convergence metric
;	- water                      O            Total precipitable water
;	- clw                        O            Integrated cld amount
;	- rwp                        O            Integrated rain amount
;	- swp                        O            Integrated snow amount
;	- iwp                        O            Integrated ice amount
;	- gwp                        O            Integrated graupel amount
;	- rh                         NA           Not used anymore
;	- ioErr                      O            Reading error indicator
;	- YFwd                       O            Last fwd simulated TBs
;       - ChanSel                    O            Channels seleted for retr
;	- Ym                         O            TBs measur. (uncorrected)
;	- YmCorr                     O            TBs measur. used for retr.
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO LoadCoreScene,iu,iprof,Scene,ProfIndx,pressLay,pressLev,tempLay,Absorbents,xclw, $
                  xrain,xsnow,xice,xGrpl,emiss,Angl,Windsp,Tskin,SfcPress,SfcTyp,   $
                  qc,lat,lon,node,scanUTC,scanYear,scanDay,nAtt,$
                  nIter,Chisq,water,clw,rwp,swp,iwp,gwp,rh,ioErr,YFwd,ChanSel,Ym,   $
                  iscanPos,iscanLine,RelAziAngl,SolZenAngl,YmCorr,snowdepth
   ioErr=0
   ON_IOerror,endRead
   readu,iu, ProfIndx
   Scene.ProfIndxVec(iprof) = ProfIndx
   ;---Atmospheric constituents
   readu,iu, pressLay
   Scene.PresLayVec(iprof,0:Scene.nLay-1) = pressLay(0:Scene.nLay-1)
   readu,iu, pressLev
   Scene.PresLevVec(iprof,0:Scene.nLev-1) = pressLev(0:Scene.nLev-1)
   readu,iu, tempLay
   Scene.TempLayVec(iprof,0:Scene.nLay-1) = tempLay(0:Scene.nLay-1)
   readu,iu,Absorbents
   Scene.AbsorbLayVec(iprof,0:Scene.nLay-1,0:Scene.nAbsorb-1) = Absorbents(0:Scene.nLay-1,0:Scene.nAbsorb-1)
   ;---Hydrometeors
   readu,iu, xCLW
   readu,iu, xRain
   readu,iu, xGrpl
   Scene.ClwLayVec(iprof,0:Scene.nParmCLW-1)   = xCLW(0:Scene.nParmCLW-1)
   Scene.RainLayVec(iprof,0:Scene.nParmRain-1) = xRain(0:Scene.nParmRain-1)
   Scene.GrplLayVec(iprof,0:Scene.nParmGrpl-1) = xGrpl(0:Scene.nParmGrpl-1)
   ;---These parameters are not stored in file. So we set them to zero. 
   Scene.SnowLayVec(iprof,0:Scene.nParmGrpl-1) = 0.
   Scene.IceLayVec(iprof,0:Scene.nParmGrpl-1)  = 0.
   Scene.AbsorbLayVec(iprof,0:Scene.nLay-1,1)  = 0.  ;ozone
   ;---Emissivity/reflectivity vectors
   readu,iu, emiss
   Scene.EmissVec(iprof,0:Scene.nchan-1) = emiss(0:Scene.nchan-1)
   ;---Surface-level paremeters
   readu,iu, Angl,Windsp,Tskin,SfcPress,SfcTyp,windU,windV,RelAziAngl,SolZenAngl,snowdepth
   Scene.AnglVec(iprof)      = Angl
   Scene.RelAziAnglVec(iprof)= RelAziAngl
   Scene.SolZenAnglVec(iprof)= SolZenAngl
   Scene.WindSpvec(iprof)    = WindSp
   Scene.WinduVec(iprof)     = WindU
   Scene.WindvVec(iprof)     = WindV
   Scene.TskinVec(iprof)     = Tskin
   Scene.SnowDepthVec(iprof) = snowdepth
   Scene.SfcPressVec(iprof)  = SfcPress
   Scene.SfcTypVec(iprof)    = SfcTyp
   ;---QC variables 
   readu,iu, qc
   Scene.qc(iprof,0:Scene.nqc-1)  = qc(0:Scene.nqc-1)
   ;---Positioning variables
   readu,iu,lat,lon,node,scanUTC,scanYear,scanDay,iscanPos,iscanLine
   IF (lon gt 180.) then lon = lon -360.
   Scene.lat(iprof)          = lat
   Scene.lon(iprof)          = lon
   Scene.direc(iprof)        = node
   Scene.time(iprof)         = scanUTC
   Scene.Hours(iprof)        = fix(scanUTC/3600.)
   Scene.Mins(iProf)         = fix((Scene.time(iprof)-fix(Scene.Hours(iProf))*3600.)/60.)
   Scene.Secs(iProf)         = (Scene.Time(iProf)-(Scene.Hours(iProf))*3600.-(Scene.Mins(iProf)*60.))
   Scene.Year(iprof)         = scanYear
   Scene.Day(iprof)          = scanDay
   Scene.ScanPos(iprof)      = iscanPos
   Scene.ScanLine(iprof)     = iscanLine
   IF (Scene.iTyp eq 1) THEN BEGIN
       readu,iu,nAtt,nIter,Chisq
       Scene.nAttempt(iprof) = nAtt
       Scene.nIter(iprof)    = nIter
       Scene.Chisq(iprof)    = ChiSq
       readu,iu,YFwd
       readu,iu,ChanSel
       readu,iu,Ym
       readu,iu,YmCorr
       Scene.YFwd(iprof,0:Scene.nchan-1)=YFwd(0:Scene.nchan-1)
       Scene.ChanSel(iprof,0:Scene.nchan-1)=ChanSel(0:Scene.nchan-1)
       Scene.Ym(iprof,0:Scene.nchan-1)=Ym(0:Scene.nchan-1)
       Scene.YmCorr(iprof,0:Scene.nchan-1)=YmCorr(0:Scene.nchan-1)
   ENDIF
   ;---compute TPW
   ind=where(Scene.AbsorbLayVec(iprof,0:Scene.nLay-1,0) ge 0.,ncount)
   IF (ncount gt 0) THEN BEGIN 
   PresLevFiltered=fltarr(ncount+1)
   PresLevFiltered(0:ncount-1)=Scene.PresLevVec(iprof,ind) & PresLevFiltered(ncount)=Scene.PresLevVec(iprof,ind(ncount-1)+1)
   ColumIntegr_LayW,ncount,PresLevFiltered,SfcPress,Scene.AbsorbLayVec(iprof,ind,0),water 
   Scene.TPWVec(iprof)  = water
   ENDIF ELSE BEGIN
   Scene.TPWVec(iprof)  = 0
   ENDELSE
   ;---compute ozon amt
   IF (where(scene.absorbid(0:scene.nabsorb-1) eq 3) ge 0) THEN BEGIN  
       ColumIntegr_LayW,Scene.nLay,Scene.PresLevVec(iprof,0:Scene.nLev-1),SfcPress,$
         Scene.AbsorbLayVec(iprof,0:Scene.nLay-1,1),ozon 
   ENDIF
   ;---compute hydrometeors integrated amounts
   ColumIntegr,Scene.nParmCLW,Scene.PresLevVec(iprof,0:Scene.nLev-1),SfcPress,$
     Scene.ClwLayVec(iprof,0:Scene.nParmCLW-1),clw
   Scene.CLWvec(iprof) = clw
   ColumIntegr,Scene.nParmRain,Scene.PresLevVec(iprof,0:Scene.nLev-1),SfcPress,$
     Scene.RainLayVec(iprof,0:Scene.nParmRain-1),rwp
   Scene.RWPvec(iprof) = rwp
   ColumIntegr,Scene.nParmGrpl,Scene.PresLevVec(iprof,0:Scene.nLev-1),SfcPress,$
     Scene.GrplLayVec(iprof,0:Scene.nParmGrpl-1),gwp
   Scene.GWPvec(iprof) = gwp
   swp=0.
   iwp=0.
   ;---compute RH
   FOR ilay=0L,Scene.nLay-1 DO BEGIN
       IF (Scene.AbsorbLayVec(iprof,iLay,0) gt 0) THEN BEGIN
           T=Scene.TempLayVec(iprof,iLay)  
           P=Scene.PresLayVec(iprof,iLay) 
           Q=Scene.AbsorbLayVec(iprof,iLay,0)/1000.
           Scene.RHvec(iprof,iLay) = Mixingratio_to_RelHum(Q,T,P)*100.
       ENDIF ELSE BEGIN
           Scene.RHvec(iprof,iLay) = -999.
       ENDELSE
   ENDFOR

   RETURN
   endRead: ioErr=1
END

;===============================================================
; Name:		LoadSceneFile
;
;
; Type:		IDL Subroutine
;
;
; Description:  Subroutine to load the Scene file (either truth file or MIRS-retrieved file).
;               The output is the Scene structure. It contains self-explanatory variables
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- fileTruth          I              File to be read
;	- topId              NA             Dummy. Not used anymore
;	- Scene              O              Structure of geoph data
;	- nProfs2read        I              Number of profiles to read
;
;
; Subroutines needed:
;       - LoadHdrScene
;       - LoadCoreScene 
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO LoadSceneFile,fileTruth,topId,Scene,nProfs2read
    LoadHdrScene,fileTruth,topId,nProfs2read,iu,iTyp,nPrf,nLay,nLev,nChan ,nAbsorb,$
                 nParmCLW,nParmRain,nParmSnow,nParmIce,nParmGrpl,AbsorbID,cfreq,$
                 polar,nqc,Scene
    ;**************************************************
    ; BODY OF THE SCENE FILE
    ;**************************************************
    pressLay     = fltarr(Scene.Nlay)
    pressLev     = fltarr(Scene.nLev)
    tempLay      = fltarr(Scene.Nlay)
    Absorbents   = fltarr(Scene.nLay,Scene.nAbsorb)
    xCLW         = fltarr(Scene.nParmCLW)
    xRain        = fltarr(Scene.nParmRain)
    xSnow        = fltarr(Scene.nParmSnow)
    xIce         = fltarr(Scene.nParmIce)
    xGrpl        = fltarr(Scene.nParmGrpl)
    emiss        = fltarr(Scene.nchan)
    YFwd         = fltarr(Scene.nchan)
    ChanSel      = lonarr(Scene.nchan)
    Ym           = fltarr(Scene.nchan)
    YmCorr       = fltarr(Scene.nchan)
    qc           = intarr(Scene.nqc)
    scanyear     = 0L
    scanDay      = 0L
    nAtt         = 0L
    nIter        = 0L
    node         = 0L
    SfcTyp       = 0L
    nprofsEff    = 0L
    ProfIndx     = 0L
    iscanPos     = 0L
    iscanLine     = 0L
    FOR iprof=0L,Scene.nProf-1 DO BEGIN
        LoadCoreScene,iu,iprof,Scene,ProfIndx,pressLay,pressLev,tempLay,Absorbents,xclw, $
          xrain,xsnow,xice,xGrpl,emiss,Angl,Windsp,Tskin,SfcPress,SfcTyp,   $
          qc,lat,lon,node,scanUTC,scanYear,scanDay,nAtt,$
          nIter,Chisq,water,clw,rwp,swp,iwp,gwp,rh,ioErr,YFwd,ChanSel,Ym,iscanPos,iscanLine,$
          RelAziAngl,SolZenAngl,YmCorr,SnowDepth
        IF (ioErr eq 0) then nProfsEff =nprofsEff+1
    ENDFOR
    print,'Effective number of profiles read:',nprofsEff,Scene.nprof
    Scene.nProfsProcessed=nprofsEff
    close,iu
    free_lun,iu
END



;===============================================================
; Name:		readProf_ncdf
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads a ncdf type of file. Used for internal 
;               geophysical files only.
;
;
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO readProf_ncdf,file_in,Prof,nlevel,nprofiles,Gmatrx
    print, read_ncdf(file_in, prof)
    nprofiles     = n_elements(prof.LATITUDE)
    nlevel        = n_elements(prof.pressure_levels)
    Lat      = prof.surface_latitude[0:nprofiles-1]
    Lon      = prof.surface_longitude[0:nprofiles-1]
    sfcPres  = prof.surface_pressure[0:nprofiles-1]
    sfcTemp  = prof.surface_temperature[0:nprofiles-1]
    sfcAirT  = prof.temperature[0,0:nprofiles-1]
    sfcAirWV = prof.water_vapor_1[0,0:nprofiles-1]
    sfcAirOz = prof.ozone_1[0,0:nprofiles-1]
    PresProf = prof.pressure_levels[0:nlevel-1]
    TempProf = prof.temperature[0:nlevel-1, 0:nprofiles-1]
    WVapProf = prof.water_vapor_1[0:nlevel-1, 0:nprofiles-1]
    OzonProf = prof.ozone_1[0:nlevel-1, 0:nprofiles-1]
    close, out
    ;----construct a unique matrix with all parameters in it. 
    nTemp     = nLev    &   iTemp     = 0
    nWVap     = nLev    &   iWVap     = iTemp     + nTemp
    nOzon     = nLev    &   iOzon     = iWVap     + nWVap
    nSfcP     = 1       &   iSfcP     = iOzon     + nOzon
    nSfcT     = 1       &   iSfcT     = iSfcP     + nSfcP
    nSfcAirT  = 1       &   iSfcAirT  = iSfcT     + nSfcT
    nSfcAirWV = 1       &   iSfcAirWV = iSfcAirT  + nSfcAirT
    nSfcAirOz = 1       &   iSfcAirOz = iSfcAirWV + nSfcAirWV
    nParams   = nTemp + nWVap + nOzon + nSfcP + nSfcT + nSfcAirT + nSfcAirWV + nSfcAirOz
    Gmatrx    = fltarr(nprofiles,nParams)
    FOR iprof=0L,nprofiles-1 DO begin
        Gmatrx(iprof, iTemp: iTemp+ nTemp-1) = TempProf(0:nlevel-1, iprof)
        Gmatrx(iprof, iWVap: iWVap+ nWVap-1) = WVapProf(0:nlevel-1, iprof)
        Gmatrx(iprof, iOzon: iOzon+ nOzon-1) = OzonProf(0:nlevel-1, iprof)
        Gmatrx(iprof, iSfcP: iSfcP+ nSfcP-1) = sfcPres(iprof)
        Gmatrx(iprof, iSfcT: iSfcT+ nSfcT-1) = sfcTemp(iprof)
        Gmatrx(iprof, iSfcAirT:  iSfcAirT  + nSfcAirT-1)  = sfcAirT(iprof)
        Gmatrx(iprof, iSfcAirWV: iSfcAirWV + nSfcAirWV-1) = sfcAirWV(iprof)
        Gmatrx(iprof, iSfcAirOz: iSfcAirOz + nSfcAirOz-1) = sfcAirOz(iprof)
    ENDFOR
END


;===============================================================
; Name:		readProf_asci
;
;
; Type:		IDL Subroutine
;
;
; Description: Reads an ASCII type of file. Used for internal 
;              geophysical files only.
;
;
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO readProf_asci,iformat,file_in,nlev,nlay,nprofiles,Gmatrx,nParams,pres_lay,$
    Tair,q_vap,sfcPres,sfcTemp,iTemp,iWVap,iSfcP,iSfcT,nTemp,nWVap,nSfcP,nSfcT,DeltaT

   IF (iformat eq 0) THEN BEGIN ;---->NOAA88 format
       readProf_asci_noaa,file_in,nlev,nlay,nprofiles,pres_lay,$
         Tair,q_vap,sfcPres,sfcTemp,DeltaT
   ENDIF 
   IF (iformat eq 1) THEN BEGIN ;---->ECMWF format
       readProf_asci_ecmwf,file_in,nlev,nlay,nprofiles,pres_lay,$
         Tair,q_vap,sfcPres,sfcTemp,DeltaT
   ENDIF 
   
   ;----construct a unique matrix with all parameters in it. 
   nTemp     = nLay    &   iTemp     = 0
   nWVap     = nLay    &   iWVap     = iTemp     + nTemp
   nSfcP     = 1       &   iSfcP     = iWVap     + nWVap
   nSfcT     = 1       &   iSfcT     = iSfcP     + nSfcP
   nParams   = nTemp + nWVap + nSfcP + nSfcT 
   Gmatrx    = fltarr(nprofiles,nParams)
   FOR iprof=0L,nprofiles-1 DO begin
       Gmatrx(iprof,iTemp: iTemp+ nTemp-1) = Tair(iprof,0:nlay-1)
       Gmatrx(iprof,iWVap: iWVap+ nWVap-1) = q_vap(iprof,0:nlay-1)
       Gmatrx(iprof,iSfcP: iSfcP+ nSfcP-1) = sfcPres(iprof)
       Gmatrx(iprof,iSfcT: iSfcT+ nSfcT-1) = sfcTemp(iprof)
   ENDFOR
   return
END

;===============================================================
; Name:		readProf_asci_ecmwf
;
;
; Type:		IDL Subroutine
;
;
; Description: Reads an ASCII type of file. Used for ECMWF 
;              internally formmated geophysical files only.
;
;
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO readProf_asci_ecmwf,file_in,nlev,nlay,nprofiles,pres_lay,$
    Tair,q_vap,sfcPres,sfcTemp,DeltaT
    nlev           = 101
    nlay           = nlev-1
    nprofiles      = 52
    pres_lev       = fltarr(nlev)
    DeltaT         = fltarr(nprofiles)
    SfcPres        = fltarr(nprofiles)
    SfcTemp        = fltarr(nprofiles)
    temp_lev       = fltarr(nprofiles,nlev)
    qwv_lev        = fltarr(nprofiles,nlev)
    qo3_lev        = fltarr(nprofiles,nlev)
    pres_lay       = fltarr(nlay)
    tair           = fltarr(nprofiles,nlay)
    q_vap          = fltarr(nprofiles,nlay)
    ;----Open file and read header
    iou=10
    openr, iou, file_in
    dummystr=''
    readf, iou, form='(a10 )', dummystr
    readf, iou, form='(a10 )', dummystr
    readf, iou, form='(a10 )', dummystr
    ;---Read first profile
    iprof=0
    readf, iou, form='(1x,i3,a10,2x,f,2x,f,2x,f,2x,f)', iprof0,dummystr,lat,lon,ps,pland
    readf, iou, form='(a10 )', dummystr
    SfcPres(iprof) = ps
    FOR ilev=0,nlev-1 DO BEGIN
        readf,iou, p,t,q,o3
        pres_lev[ilev]       = p
        temp_lev[iprof,ilev] = t
        qwv_lev[iprof,ilev]  = q*1000.  ;->kg/kg -> g/kg
        qo3_lev[iprof,ilev]  = o3*1000. ;->kg/kg -> g/kg
    ENDFOR
    ;----read rest of profiles
    FOR iprof=1,nprofiles-1 DO BEGIN
        readf, iou, form='(1x,i3,a10,2x,f,2x,f,2x,f,2x,f)', iprof0,dummystr,lat,lon,ps,pland
        SfcPres(iprof) = ps
        FOR ilev=0,nlev-1 DO BEGIN
            readf,iou,p,t,q,o3
            pres_lev[ilev]       = p
            temp_lev[iprof,ilev] = t
            qwv_lev[iprof,ilev]  = q*1000.  ;->kg/kg -> g/kg
            qo3_lev[iprof,ilev]  = o3*1000. ;->kg/kg -> g/kg
            IF (p ge ps) THEN BEGIN
                sfcTemp(iprof) = t
                deltaT(iprof)  = t - sfcTemp(iprof-1)
            ENDIF
        ENDFOR
    ENDFOR
    ;---Layer-averaging
    FOR ilay=0,nlay-1 DO BEGIN
        pres_lay(ilay) = (pres_lev(ilay)+pres_lev(ilay+1))/2.
        FOR iprof=0L,nprofiles-1 DO BEGIN
            tair(iprof,ilay)  = (temp_lev(iprof,ilay)+temp_lev(iprof,ilay+1))/2.
            q_vap(iprof,ilay) = (qwv_lev(iprof,ilay)+qwv_lev(iprof,ilay+1))/2.
        ENDFOR
    ENDFOR
    close,iou
    RETURN
END

;===============================================================
; Name:		readProf_asci_noaa
;
;
; Type:		IDL Subroutine
;
;
; Description: Reads an ASCII type of file. Used for NOAA 
;              internally formmated geophysical files only.
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO readProf_asci_noaa,file_in,nlev,nlay,nprofiles,pres_lay,$
    Tair,q_vap,sfcPres,sfcTemp,DeltaT
    ;----Open file and read header
    iou=10
    openr, iou, file_in
    dummystr=''
    readf, iou, form='(a10, i5, i10, i10, i10, i10 )', dummystr, nprofiles, nemis, ncemis, nlev0, dummy 
    nLev               = nlev0+1
    pres_lev0          = fltarr(nlev0)
    pres_lev           = fltarr(nlev)
    nlay               = nlev-1
    readf, iou, pres_lev0
    pres_lev[0]        = 0.00001
    pres_lev[1:nlev-1] = pres_lev0
    pres_lay           = (pres_lev[1:nlev-1]-pres_lev[0:nlev-2])/alog(pres_lev[1:nlev-1]/pres_lev[0:nlev-2])
    for i=1,10 do begin
        readf, iou, dummystr
    endfor
    mxclds     = 5   ; Maximum number of cloud layers allowed in one profile
    mxEmis     = 10  ; Maximum number of emissivities allowed in one profile
    Lat        = fltarr(nprofiles)
    Lon        = fltarr(nprofiles)
    NcldyLay   = fltarr(nprofiles)
    sfcPres    = fltarr(nprofiles)
    sfcTemp    = fltarr(nprofiles)
    DeltaT     = fltarr(nprofiles)
    pcldtop    = fltarr(mxclds,nprofiles)
    cldfrc     = fltarr(mxclds,nprofiles)
    tair       = fltarr(nprofiles,nlay)
    h2ocd      = fltarr(nprofiles,nlay)
    ozocd      = fltarr(nprofiles,nlay)
    wlcd       = fltarr(nprofiles,nlay)
    ciw        = fltarr(nprofiles,nlay)
    q_vap      = fltarr(nprofiles,nlay)
    q_o3       = fltarr(nprofiles,nlay)
    ;----temporary data to contain:
    data2      = fltarr(2) ;cld top and fraction
    data3      = fltarr(3) ;emiss: /reflectances
    data5      = fltarr(5) ;atmospheric data
    ;----Load the physical constants
    consts, MW_H2O,MW_O3,MW_DRYAIR,STANDARD_GRAVITY,AVOGADRO_CONSTANT,mr2ppmv_o3
    ;-----Read the body of the file
    idprof=''
    for iprof=0L, nprofiles-1 do begin
        readf, iou, form='(a8, 2f9.3, i3, 4i2, i4 )', idprof, $
          alat, alon, nyear, nmonth, nday, nhour, nminute, nsec
        readf, iou, pland, psurf, tsurf, emismw, topog, ncld
        Lat(iprof)      = alat
        Lon(iprof)      = alon
        sfcPres(iprof)  = psurf
        sfcTemp(iprof)  = tsurf
        NcldyLay(iprof) = ncld
        ;----read cloud info if ncld > 0
        if (ncld gt 0)then begin
            for l=0, ncld-1 do begin
                readf, iou, data2
                pcldtop(l,iprof) = data2[0]
                cldfrc(l,iprof)  = data2[1]
            endfor
        endif
        ;----read the atmospheric info (temp,wv,ozo,cld,iwp, etc) -layer-
        for l=0, nlev0-1 do begin
            readf, iou, data5
            tair(iprof,l)  = data5[0]
            h2ocd(iprof,l) = data5[1]
            ozocd(iprof,l) = data5[2]
            wlcd(iprof,l)  = data5[3]
            ciw(iprof,l)   = data5[4]
        endfor
        DeltaT(iprof)  = sfcTemp(iprof)-Tair(iprof,nlay-1)

        ;----read the emissivity
        for i=0, nemis-1 do begin
            readf, iou, data3
        endfor
        ;----read the cld emittances/reflectances
        for j=0, 0 do begin
            for i = 0, ncld-1 do begin
                readf, iou, data3
            endfor
        endfor
        readf, iou, nspi, nspr
        if (nspi gt 0) then begin
            for j = 0, nspi-1 do begin
                readf, iou, x ;-spare
            endfor
        endif
        if (nspr gt 0)then begin 
            readf, iou,data2
        endif
        ;---conversion mol/cm2->g/kg
        q_vap(iprof,*) = 100.*MW_H2O*(h2ocd(iprof,0:nlay-1)/AVOGADRO_CONSTANT)* $
          STANDARD_GRAVITY / (pres_lev(1:nlev-1) - pres_lev(0:nlev-2)) ; g/kg
        q_o3(iprof,*)  = 100.*MW_O3*(ozocd(iprof,0:nlay-1)/AVOGADRO_CONSTANT)* $
          STANDARD_GRAVITY / (pres_lev(1:nlev-1) - pres_lev(0:nlev-2)) ; g/kg
        ;---the o3 values at level 4 and 5 are strange.  Here is a fix
        y1             = alog(q_o3[iprof,3])     & y2 = alog(q_o3[iprof,6])
        x1             = alog(pres_lay[3])       & x2 = alog(pres_lay[6])
        r              = (y2-y1)/(x2-x1)
        x              = alog(pres_lay[4])
        q_o3[iprof,4]  = exp(y1 + r * (x - x1))
        x              = alog(pres_lay[5])
        q_o3[iprof,5]  = exp(y1 + r * (x - x1))
    endfor
    close,iou

END


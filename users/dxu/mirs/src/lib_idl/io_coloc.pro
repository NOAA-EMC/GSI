;$Id: io_coloc.pro 2922 2012-02-07 21:13:34Z kgarrett $
;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; colocation files (geophysical and/or radiometric data).
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------

;===============================================================
; Name:		LoadColocFile
;
;
; Type:		IDL Subroutine
;
;
; Description:  Subroutine to load the Coloc file. The output is the Coloc structure. 
;               The vertical integration of the atmospheric profiles is also done
;               in this subroutine. This includes the integration to obtain TPW,
;               rain water path, ice water path, snow water path as well as total
;               cloud amount. The conversion into relative humidity is also
;               part of this subroutine. Please see the definition of
;               the elements of the colocation structure below (in LoadHdrColoc).
; 
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- fileColoc          I             Colocation file to read
;	- ColocData          O             Structure containing all data
;
;
; Subroutines needed:
;       - LoadHdrColoc
;       - LoadCoreColoc
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO LoadColocFile,fileColoc,ColocData
    LoadHdrColoc,fileColoc,iu,ColocData
    nprofsEff    = 0L
    FOR iprof=0L,ColocData.nColoc-1 DO BEGIN
        LoadCoreColoc,iu,ColocData,ierr
        IF (iErr eq 0) then nProfsEff =nprofsEff+1
    ENDFOR
    print,'Effective number of profiles read:',nprofsEff,ColocData.nColoc
    ColocData.nColoc=nprofsEff
    close,iu
    free_lun,iu
RETURN
END

;===============================================================
; Name:		LoadCoreColoc
;
;
; Type:		IDL Subroutine
;
;
; Description:  Loads the core of the colocation file assuming the header
;               has been read already and the structure already defined.
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                 I             Unit number (open)
;	- ColocData          I/O           Colocation structure
;	- ierr               O             I/O error indicator
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

PRO LoadCoreColoc,iu,ColocData,ierr
    irec         = 0L
    colocID      = ''
    statID       = ''
    YearRef      = 0L
    MonthRef     = 0L
    DayRef       = 0L
    HourRef      = 0L
    MinRef       = 0L
    nLayRef      = 0L
    SfcTypRef    = 0L
    LatRef       = 0.
    LonRef       = 0.
    PsfcRef      = 0.
    PtopRef      = 0.
    TskinRef     = 0.
    TsfcRef      = 0.
    LandFrRef    = 0.
    WindSfcRef   = 0.
    AltRef       = 0.
    nSens0       = 0L
    nGeoph0      = 0L
    ierr         = 0
    scanPos      = 0L
    ON_IOerror,endRead
    READF,iu,format='(i8,2x,2a10,i5,5i4,i10,7f10.3,3f9.2,2i4)',irec,colocID,statID,YearRef,MonthRef,DayRef,HourRef,$
      MinRef,nLayRef,SfcTypRef,LatRef,LonRef,PsfcRef,ptopRef_temp,ptopref_wvap,TskinRef,TsfcRef,LandFrRef,         $
      WindSfcRef,AltRef,nSens0,nGeoph0
    ColocData.irec        = irec
    ColocData.colocID     = colocID
    ColocData.statID      = statID
    ColocData.YearRef     = YearRef
    ColocData.MonthRef    = MonthRef
    ColocData.DayRef      = DayRef
    ColocData.HourRef     = HourRef
    ColocData.MinRef      = MinRef
    ColocData.nLayRef     = nLayRef
    ColocData.SfcTypRef   = SfcTypRef
    ColocData.LatRef      = LatRef
    ColocData.LonRef      = LonRef
    ColocData.PsfcRef     = PsfcRef
    ColocData.PtopRef_temp= PtopRef_temp
    ColocData.PtopRef_wvap= PtopRef_wvap
    ColocData.TskinRef    = TskinRef
    ColocData.TsfcRef     = TsfcRef
    ColocData.LandFrRef   = LandFrRef
    ColocData.WindSfcRef  = WindSfcRef
    ColocData.AltRef      = AltRef
    ColocData.nSens0      = nSens0
    ColocData.nGeoph0     = nGeoph0
    ;---Read geophysical data of reference point
    FOR ilay=0,nLayRef-1 DO BEGIN
        READF,iu,format='(i3,3f9.3)', iLay0,Press,temp,wv
        ColocData.PressLayRef(iLay) = press
        ColocData.TempLayRef(iLay)  = temp
        ColocData.WVLayRef(iLay)    = wv
    ENDFOR
    ;---Read sensor-related data (brightness temperatures)
    FOR iSens=0,nSens0-1 DO BEGIN
        READF,iu,format='(2i5)', sensindx0,nptsrad0
        ColocData.SensIndx(iSens) = sensindx0
        ColocData.nPtsRad(iSens)  = nptsrad0
        nchan0                    = ColocData.nchans(sensindx0-1)
        tb0                       = fltarr(nchan0)
        FOR ipt=0,nptsrad0-1 DO BEGIN
            READF,iu,format='(i5,4f8.2,i4)', iPt0,angl,lat,lon,diff,scanPos
            READF,iu,format='(10f7.2)', tb0
            ColocData.AnglRad(iSens,iPt)        = angl
            ColocData.LatRad(iSens,iPt)         = lat
            ColocData.LonRad(iSens,iPt)         = lon
            ColocData.DiffHourRad(iSens,iPt)    = diff
            ColocData.ScanPos(iSens,iPt)        = scanPos
            ColocData.Tb(iSens,iPt,0:nchan0-1)  = tb0(0:nchan0-1)
        ENDFOR
    ENDFOR
    ;---Read data related to other geophysical sources (other than Ref, like GDAS, etc)
    FOR iGeoph=0,nGeoph0-1 DO BEGIN
        READF,iu,format='(2i5)', GeoSrcIndx0,nPtsGeo0
        ColocData.GeoSrcIndx(iGeoph)  = GeoSrcIndx0
        ColocData.nPtsGeo(iGeoph)     = nPtsGeo0
        nEDRs0                        = ColocData.nEDRs(GeoSrcIndx0-1)
        FOR ipt=0,nPtsGeo0-1 DO BEGIN
            READF,iu,format='(i5,4f8.2)', iPt0,Lat,Lon,Diff
            ColocData.LatGeo(iGeoph,iPt)       = Lat
            ColocData.LonGeo(iGeoph,iPt)       = Lon
            ColocData.DiffHourGeo(iGeoph,iPt)  = Diff
            FOR iEDR=0,nEDRs0-1 DO BEGIN
                nParam0 = ColocData.nParams(iGeoph,iEDR)
                data    = fltarr(nParam0)
                READF,iu,format='(10f10.3)', data
                ColocData.DataGeo(iGeoph,ipt,iEDR,0:nParam0-1) = data(0:nParam0-1)
            ENDFOR
        ENDFOR
    ENDFOR
    RETURN
    endRead: ierr = 1
    RETURN
END





;===============================================================
; Name:		LoadHdrColoc
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the header of a colocation file and then 
;               initializes the colocation structure. See below 
;               for the definition of the structure elements.
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- fileColoc         I              Name of the coloc file to open
;	- iu                O              Unit number
;	- ColocData         O              Colocation data structure
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

PRO LoadHdrColoc,fileColoc,iu,ColocData
    OPENR,iu,fileColoc,/get_lun,error=err
    IF (err ne 0) THEN BEGIN
        print ,!ERROR_STATE.MSG
        RETURN
    ENDIF
    ;************************************************
    ;  HEADER
    ;************************************************
    ligne          = ''
    nColoc         = 0L
    nSens          = 0L
    nGeoph         = 0L
    MxLays         = 0L
    MxPoints       = 0L
    MxParams       = 0L
    DeltaTimeColoc = 0.
    DeltaLatColoc  = 0.
    DeltaLonColoc  = 0.
    pol            = 0L
    fr             = 0.
    nparam0        = 0L
    label          = ''
    READF,iu,format='(a)',     Ligne
    READF,iu,format='(6i8)',   nColoc,nSens,nGeoph,MxLays,MxPoints,MxParams
    READF,iu,format='(3f9.3)', DeltaTimeColoc,DeltaLatColoc,DeltaLonColoc
    ;---Read sensor-related information
    MxChan = 100
    sensID = strarr(max([1,nSens]))
    nchans = lonarr(max([1,nSens]))
    polar  = lonarr(max([1,nSens]),MxChan)
    Freq   = fltarr(max([1,nSens]),MxChan)
    IF (nSens gt 0) THEN BEGIN
        READF,iu,format='(4a10)',  sensID
        READF,iu,format='(4i3)',   nchans
        MxChan0  = max(nchans(0:nSens-1))
        IF (MxChan0 gt MxChan) THEN BEGIN
            print,'Error: MxChan not big enough'
            STOP
        ENDIF
        FOR iSens=0,nSens-1 DO BEGIN
            FOR ichan=0,nchans(iSens)-1 DO BEGIN
                READF,iu,format='(3i3,f10.4)', iSens0,iChan0,pol,fr
                polar(iSens,iChan) = pol
                freq(iSens,iChan)  = fr
            ENDFOR
        ENDFOR
    ENDIF
    ;---Read data related to Geoph data sources
    MxEDRs    = MxLays
    GeoSrcID  = strarr(max([1,nGeoph]))
    nEDRs     = lonarr(max([1,nGeoph]))
    nParams   = lonarr(max([1,nGeoph]),MxEDRs)
    EDRLabels = strarr(max([1,nGeoph]),MxEDRs)
    IF (nGeoph gt 0) THEN BEGIN
        READF,iu,format='(4a10)',  GeoSrcID
        READF,iu,format='(4i3)',   nEDRs
        MxEDRs0    = max(nEDRs(0:nGeoph-1))
        IF (MxEDRs0 gt MxEDRs) THEN BEGIN
            print,'Error: MxEDRs not big enough'
            STOP
        ENDIF
        FOR iGeoph=0,nGeoph-1 DO BEGIN
            FOR iEDR=0,nEDRs(iGeoph)-1 DO BEGIN
                READF,iu,format='(3i4,3x,a20)', iGeoph0,iEDR0,nparam0,label
                nParams(iGeoph,iEDR)   = nparam0
                EDRLabels(iGeoph,iEDR) = label
          ENDFOR
       ENDFOR
    ENDIF
    READF,iu,format='(a)', Ligne
    ;************************************************
    ;  DEFINE THE COLOC STRUCTURE
    ;************************************************
    ColocData={                                              $
     ;-------------------------------------------------------
     ;    HEADER
     ;-------------------------------------------------------
     nColoc:nColoc,                                          $ ;# colocated ref. points (raobs, dropsondes, etc)
     DeltaTimeColoc:DeltaTimeColoc,                          $ ;Time criterion used for colocation (hours)
     DeltaLatColoc:DeltaLatColoc,                            $ ;Latitude criterion used for colocation (degs)
     DeltaLonColoc:DeltaLonColoc,                            $ ;Latitude criterion used for colocation (degs)
     MxPoints:MxPoints,                                      $ ;Maximum # pts allowed around a reference point
     MxLays:MxLays,                                          $ ;Maximum # layers allowed for vertical profiles
     MxChan:MxChan,                                          $ ;Maximum # channels allowed for sensors
     MxEDRs:MxEDRs,                                          $ ;Maximum # EDRs allowed for geoph sources
     MxParams:MxParams,                                      $ ;Maximum # params allowed for geoph sources / EDRs
     ;---Sensor-related data 
     nSens:nSens,                                            $ ;# sensors collocated around ref. points (max #)
     SensID: sensID(0:nSens-1),                              $ ;Sensors IDs used for colocation (AMSU, MWR, etc)
     nChans:nchans(0:nSens-1),                               $ ;Number of channels for each sensor used
     Polar:Polar(0:nSens-1,0:MxChan-1),                      $ ;Polarization corresponding to each sensor/channel
     Freq:Freq(0:nSens-1,0:MxChan-1),                        $ ;Frequency corresponding to each sensor/channel
     ;---Geophysical data source-related data 
     nGeoph:nGeoph,                                          $ ;# geoph. data sources around ref. points (max #)
     ;GeoSrcID:GeoSrcID(0:nGeoph-1),                          $ ;IDs of Geophysical sources (GDAS, MWRP Retr, etc)
     ;nEDRs:nEDRs(0:nGeoph-1),                                $ ;Number of EDRs (varies by geophysical source) 
     ;EDRLabels:EDRLabels(0:nGeoph-1,0:MxEDRs-1),             $ ;Labels of EDRs corresponding to each GeoSrc/EDR 
     ;nParams:nParams(0:nGeoph-1,0:MxEDRs-1),                 $ ;# of Params corresponding to each GeoSrc/EDR 
     GeoSrcID:GeoSrcID,                                      $ ;IDs of Geophysical sources (GDAS, MWRP Retr, etc)
     nEDRs:nEDRs,                                            $ ;Number of EDRs (varies by geophysical source) 
     EDRLabels:EDRLabels,                                    $ ;Labels of EDRs corresponding to each GeoSrc/EDR 
     nParams:nParams,                                        $ ;# of Params corresponding to each GeoSrc/EDR 
     ;-------------------------------------------------------
     ;    BODY
     ;-------------------------------------------------------
     irec:0L,                                        $ ;Record number
     colocID:'',                                     $ ;ID of the colocation reference point
     statID:'',                                      $ ;Station ID of particular colocated ref. point
     ;---Time and Space Location Data
     YearRef:0L,                                     $ ;Year of reference point
     MonthRef:0L,                                    $ ;Month of reference point
     DayRef:0L,                                      $ ;Day of reference point
     HourRef:0L,                                     $ ;Hour of reference point
     MinRef:0L,                                      $ ;Minute of reference point
     SfcTypRef:0L,                                   $ ;Surface Type of ref. point
     LatRef:0.,                                      $ ;Latitude of reference point               [Degrees]
     LonRef:0.,                                      $ ;Longitude of reference point              [Degrees]
     ;---Surface-related Data
     PsfcRef:0.,                                     $ ;Surface pressure of reference point       [mb]
     LandFrRef:0.,                                   $ ;Land Fraction of reference point          [0..1]
     TskinRef:0.,                                    $ ;Skin tmperature of reference point        [Kelvin]
     TsfcRef:0.,                                     $ ;Surface Air tmperature of reference point [Kelvin]
     WindSfcRef:0.,                                  $ ;Surface Wind Speed of reference point     [m/s]
     AltRef:0.,                                      $ ;Altitude of Ref. point                    [m]
     ;---Atmospheric-Profile-related data
     nLayRef:0L,                                      $ ;Number of layers of ref.-point profile
     PressLayRef:fltarr(MxLays),                      $ ;Layer Pressure grid                       [mb]
     TempLayRef:fltarr(MxLays),                       $ ;Layer temperature grid                    [Kelvin]
     WVLayRef:fltarr(MxLays),                         $ ;Layer water vapor mixing ratio grid       [g/Kg]
     PtopRef_temp:0.,                                 $ ;Top pressure of reference point T prof    [mb]
     PtopRef_wvap:0.,                                 $ ;Top pressure of reference point Q prof    [mb]
     ;---Sensor-related data
     nSens0:0L,                                       $ ;# sensors around this particular ref. point
     SensIndx:lonarr(nSens),                          $ ;Sensor index vector for this colocation (points 2 SensID)
     nPtsRad:lonarr(nSens),                           $ ;# of rad/tb measurmts around ref point 4 each sensor 
     AnglRad:fltarr(nSens,MxPoints),                  $ ;Angles corresponding 2 different Rad/Tb pts around ref pt.
     Tb:fltarr(nSens,MxPoints,MxChan),                $ ;Tbs corresponding 2 pts around ref pt. (4 diff. sensors)
     LatRad:fltarr(nSens,MxPoints),                   $ ;Latitudes corresponding 2 different Rad/Tb pts around ref pt.
     LonRad:fltarr(nSens,MxPoints),                   $ ;Longitudes corresponding 2 different Rad/Tb pts around ref pt.
     DiffHourRad:fltarr(nSens,MxPoints),              $ ;Time difference in fractional hours of Rad/Tb pts wrt. ref pt.
     ScanPos:lonarr(nSens,MxPoints),                  $ ;Scan Position of the radiance points
     ;---Geophysical data source-related data
     nGeoph0:0L,                                      $ ;# sources for geoph data around this particular ref. point
     GeoSrcIndx:lonarr(nGeoph),                       $ ;geop Src index vector for this colocation (points 2 GeoSrc)
     nPtsGeo:lonarr(nGeoph),                          $ ;# of geoph data around ref point 4 each source 
     LatGeo:fltarr(nGeoph,MxPoints),                  $ ;Latitudes corresponding 2 different geoph measur pts around ref pt.
     LonGeo:fltarr(nGeoph,MxPoints),                  $ ;Longitudes corresponding 2 different geoph measur pts around ref pt.
     DiffHourGeo:fltarr(nGeoph,MxPoints),             $ ;Time difference in fractional hours of geoph measur pts wrt. ref pt.
     DataGeo:fltarr(nGeoph,MxPoints,MxEDRs,MxParams)  $ ;Geoph data measurements around ref. pt
     }     

     print, 'DeltaTimeColoc used in colocation:',ColocData.DeltaTimeColoc
     print, 'DeltaLatColoc used in colocation:',ColocData.DeltaLatColoc
     print, 'DeltaLonColoc used in colocation:',ColocData.DeltaLonColoc
     print, '# Sensors used in colocation:',ColocData.nSens
     For iSens=0,ColocData.Nsens-1 DO BEGIN
         print,'---------------------------------------------------'
         print,'       Sensor:',iSens+1,':',ColocData.sensID(iSens)
         print,'       Nchans:',ColocData.nchans(iSens)
         FOR ichan=0,ColocData.nchans(iSens)-1 DO BEGIN
             print,'          Chan#',ichan+1,' Freq:',ColocData.Freq(iSens,ichan),' Pol:',ColocData.polar(iSens,ichan)
         ENDFOR
     ENDFOR
     print, '# Geoph sources used in colocation:',ColocData.nGeoph
     FOR iGeoph=0,ColocData.nGeoph-1 DO BEGIN
         print,'---------------------------------------------------'
         print,'       Geoph Source:',iGeoph+1,':',ColocData.GeoSrcID(iGeoph)
         print,'       nEDRs:',ColocData.nEDRs(iGeoph)
         FOR iEDR=0,ColocData.nEDRs(iGeoph)-1 DO BEGIN
             print,'          EDR#',iEDR+1,' EDR:',ColocData.EDRLabels(iGeoph,iEDR),' #Params:',ColocData.nParams(iGeoph,iEDR)
         ENDFOR         
     ENDFOR
     RETURN
END



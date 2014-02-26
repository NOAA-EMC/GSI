;$Id: Calib_generic_rad.pro 3345 2013-10-09 17:31:28Z amims $
@../../../setup/paths_idl.pro

;------------------------------------------------------------------
;
; Program used to assess the calibration of microwave radiances
; by comparing them to simulations based on analyses (GDAS).
;
; Sid-Ahmed Boukabara. September, 2005. 
;
; Updated in June 06 to make it instrument-independent. SAB
;
; Extended to perform the monitoring of:
;  - bias residuals
;
; Known Limitations:
;
;  This program assumes at least two orbits. If only one 
;  orbit is provided, dimension of the 3D array becomes 2D which
;  creates all sorts of problems. SAB
;
;------------------------------------------------------------------

PRO Calib_generic_rad,NamelistInputs

;------------------------------------------------------------------------------
; Initiate constants from the constants module
;------------------------------------------------------------------------------
Consts,Consts

Use_as_main_program =0  ;=0->as a subroutine, 1->as a main program

IF (Use_as_main_program eq 1) THEN BEGIN
    device='x'
    device,true=24,retain=2,decompose=0
    device,pseudo_color=8,retain=2
    device,get_visual_name=thisvisualClass
    if (thisvisualClass ne 'pseudoColor') then device,decomposed=0
    close,/all
    ;---fy3a
    NamelistInputs='../../../data/ControlData/fy3ri_Inputs4BiasComputation_2008-09-14.in_gdas'
    sensor_id=8
    ;---windsat
    ;NamelistInputs='../../../data/ControlData/windsat_Inputs4BiasComputation.in'
    ;sensor_id=8
    ;---n18
    ;NamelistInputs='../../../data/ControlData/n18_Inputs4BiasComputation_2006-02-01.in'
    ;sensor_id=1
    ;---ssmis
    NamelistInputs='../../../data/ControlData/f16_Inputs4BiasComputation_2006-02-01.in'
    sensor_id=3
    ;---npp
    NamelistInputs='../../../data/ControlData/npp_Inputs4BiasComputation_2008-03-15.in_ecmw'
    sensor_id=6
    ;-----
    device='x'
    set_plot,'x'
    PRINT, 'Do you want to read again the data (1/0)?'
    read,answer
    IF (answer eq 0) then GOTO, sels
ENDIF



;------------------------------------------------------------------------------
; Reading namelist inputs
;------------------------------------------------------------------------------
ListAnalysName='/disk1/pub/wchen/mirs_gfortran_linux_x64/data/InputsData/n19_NWPanalysFiles_4Bias_2011-07-20.list_gdas'
listRadSimulName='/disk1/pub/wchen/mirs_gfortran_linux_x64/data/InputsData/n19_FWDanalysSimulFiles_4Bias_2011-07-20.list_gdas'
listRadMeasName='/disk1/pub/wchen/mirs_gfortran_linux_x64/data/InputsData/n19_fmsdrFiles_4Bias_2011-07-20.list_2011-07-20.list'
iBiasComputMethod=1
biasfile='/disk1/pub/wchen/mirs_gfortran_linux_x64/data/SemiStaticData/biasCorrec/biasCorrec_n19_2011_07_20.dat_gdas'
ErrFile='/disk1/pub/wchen/mirs_gfortran_linux_x64/data/SemiStaticData/biasCorrec/ModelErrFile_n19_2011_07_20.dat_gdas'
psFilename='/disk1/pub/wchen/mirs_gfortran_linux_x64/data/TestbedData/PerfsMonitoring/n19_amsua_mhs/2011-07-20/biasGen_gdas.ps'
norbits2process=1000000
sensor_id=4
fmType=1
minLat = -10.0
maxLat = 60.
minlon = -160.
maxlon = 100.

readFromList=1
if readFromList eq 1 then begin
  openr,iu,NamelistInputs,/get_lun
  readf,iu,format='(a)',ListAnalysName
  readf,iu,format='(a)',listRadSimulName
  readf,iu,format='(a)',listRadMeasName
  readf,iu,format='(i)',iBiasComputMethod
  readf,iu,format='(a)',biasfile
  readf,iu,format='(a)',ErrFile
  readf,iu,format='(a)',psFilename
  readf,iu,format='(i)',norbits2process
  readf,iu,format='(i)',sensor_id
  readf,iu,format='(i)',fmType
  readf,iu,format='(f)',minLat
  readf,iu,format='(f)',maxLat
  readf,iu,format='(f)',minLon
  readf,iu,format='(f)',maxLon
  close,iu
  free_lun,iu,/force
endif

plot_output=0 ;---set to 1 for .ps output generation
IF (Use_as_main_program eq 0) THEN BEGIN
    set_plot,'ps'
    device,filename=psFilename,/color,xsize=23,ysize=16,yoffset=2,/landscape
ENDIF

;---Scaling factors used to adjust the stdev (considered the modeling
;error). These factors are a bit subjective but do reflect uncertainty
;in the GDAS variables for which  a particular channel is most
;sensitive. For example, if a channel is sensitive to cloud and to surface
;(more unknown than temperature for instance) than the the scaling factor
;should be low, close to 0. If the channel is dependent on mostly the temperature profile
;(which is relatively well modeled by GDAS) then the scaling factor should
;be almost 1. These factors also reflect errors in the collocation.

mxChan = 30L ; predefined a relative value to be overwritten by real case below

;-------------------------------------------------------------------------------
;     Set sensor-specific items
;-------------------------------------------------------------------------------
IF (sensor_id eq Consts.INT_SATID_N18) THEN BEGIN   ;N18 AMSUA/MHS
    mxChan = 20L
    if fmType eq 0 then npos = 30 ; 30, or 1 or whatever number of scan positions
    if fmType eq 1 then npos = 90 ; 90, or 1 or whatever number of scan positions
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    scalFact       = [0.3,0.3,0.3,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.3,0.3,0.3,0.5,0.5,0.5]
    ChannelsNot2Correct  = [-1]  ;--actual channels numbers (not IDL indexes)
    ;Channels2plotFig  = [0,1,2,3,7,8,9,10,11,14,16,18]
    Channels2plotFig  = [0]
    ScanPos2plot      = [0,2,5,10,15,20,25,29]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_METOPA) THEN BEGIN   ;METOP-A AMSUA/MHS
    mxChan = 20L
    if fmType eq 0 then npos = 30 ; 30, or 1 or whatever number of scan positions
    if fmType eq 1 then npos = 90 ; 90, or 1 or whatever number of scan positions
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    ;                 23  31  50.3 52  53  54  54  55  57  57  57  57  57  57  89  89  157 183 183 183
    scalFact       = [0.3,0.3,0.3,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.3,0.3,0.3,0.5,0.5,0.5]
    ChannelsNot2Correct  = [-1] ;--actual channels numbers (not IDL indexes)
    ;Channels2plotFig  = [0,1,2,3,7,8,9,10,11,14,16,18]
    Channels2plotFig  = [0]
    ScanPos2plot      = [0,2,5,10,15,20,25,29]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_METOPB) THEN BEGIN   ;METOP-B AMSUA/MHS
    mxChan = 20L
    if fmType eq 0 then npos = 30 ; 30, or 1 or whatever number of scan positions
    if fmType eq 1 then npos = 90 ; 90, or 1 or whatever number of scan positions
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    ;                 23  31  50.3 52  53  54  54  55  57  57  57  57  57  57  89  89  157 183 183 183
    scalFact       = [0.3,0.3,0.3,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.3,0.3,0.3,0.5,0.5,0.5]
    ChannelsNot2Correct  = [-1] ;--actual channels numbers (not IDL indexes)
    ;Channels2plotFig  = [0,1,2,3,7,8,9,10,11,14,16,18]
    Channels2plotFig  = [0]
    ScanPos2plot      = [0,2,5,10,15,20,25,29]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_N19) THEN BEGIN   ;N19 AMSUA/MHS
    mxChan = 20L
    if fmType eq 0 then npos = 30 ; 30, or 1 or whatever number of scan positions
    if fmType eq 1 then npos = 90 ; 90, or 1 or whatever number of scan positions
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    scalFact       = [0.3,0.3,0.3,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.3,0.3,0.3,0.5,0.5,0.5]
    ChannelsNot2Correct  = [-1]  ;--actual channels numbers (not IDL indexes)
    ;Channels2plotFig  = [0,1,2,3,7,8,9,10,11,14,16,18]
    Channels2plotFig  = [0]
    ScanPos2plot      = [0,2,5,10,15,20,25,29]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_F16) THEN BEGIN   ;F16 SSMIS
    mxChan = 24L
    if fmType eq 0 then npos = 30 
    if fmType eq 1 then npos = 60 
    if fmType eq 2 then npos = 90 
    if fmType eq 3 then npos = 180 
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
    ;                 50. 52  53   54  55  57  59 150 183 183 183  19  19  22  37  37 91  91  63  60  60  60  60  60
    scalFact       = [0.3,0.9,0.9,0.9,0.9,0.9,0.9,0.5,0.5,0.5,0.5,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.5,0.5,0.5,0.5,0.5,0.5]
    ChannelsNot2Correct  = [19,20,21,22,23,24]  ;--actual channels numbers (not IDL indexes)
    ;Channels2plotFig  = [0,1,2,3,7,8,9,10,11,14,16,18]
    Channels2plotFig  = [0]
    ScanPos2plot      = [0]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_F17) THEN BEGIN   ;F17 SSMIS
    mxChan = 24L
    if fmType eq 0 then npos = 30 
    if fmType eq 1 then npos = 60 
    if fmType eq 2 then npos = 90 
    if fmType eq 3 then npos = 180 
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
    ;                 50. 52  53   54  55  57  59 150 183 183 183  19  19  22  37  37 91  91  63  60  60  60  60  60
    scalFact       = [0.3,0.9,0.9,0.9,0.9,0.9,0.9,0.5,0.5,0.5,0.5,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.5,0.5,0.5,0.5,0.5,0.5]
    ChannelsNot2Correct  = [19,20,21,22,23,24]  ;--actual channels numbers (not IDL indexes)
    ;Channels2plotFig  = [0,1,2,3,7,8,9,10,11,14,16,18]
    Channels2plotFig  = [0]
    ScanPos2plot      = [0]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_F18) THEN BEGIN   ;F18 SSMIS
    mxChan = 24L
    if fmType eq 0 then npos = 30 
    if fmType eq 1 then npos = 60 
    if fmType eq 2 then npos = 90 
    if fmType eq 3 then npos = 180 
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
    ;                 50. 52  53   54  55  57  59 150 183 183 183  19  19  22  37  37 91  91  63  60  60  60  60  60
    scalFact       = [0.3,0.9,0.9,0.9,0.9,0.9,0.9,0.5,0.5,0.5,0.5,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.5,0.5,0.5,0.5,0.5,0.5]
    ChannelsNot2Correct  = [19,20,21,22,23,24]  ;--actual channels numbers (not IDL indexes)
    ;Channels2plotFig  = [0,1,2,3,7,8,9,10,11,14,16,18]
    Channels2plotFig  = [0]
    ScanPos2plot      = [0]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_TRMM) THEN BEGIN   ;TRMM TMI
    mxChan = 9L
    if fmType eq -1 then npos = 26
    if fmType eq 0  then npos = 104
    if fmType eq 1  then npos = 208
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8]
    scalFact          = [0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3]
    ChannelsNot2Correct  = [-1]  ;--actual channels numbers (not IDL indexes)
    Channels2plotFig  = [0]
    ScanPos2plot      = [0]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_GPM) THEN BEGIN   ;GPM TMI
    mxChan = 13L
    if fmType eq -1 then npos = 26
    if fmType eq 0  then npos = 104
    if fmType eq 1  then npos = 208
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12]
    scalFact          = [0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3]
    ChannelsNot2Correct  = [-1]  ;--actual channels numbers (not IDL indexes)
    Channels2plotFig  = [0]
    ScanPos2plot      = [0]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_NPP) THEN BEGIN   ;NPP ATMS
    mxChan = 22L
    npos = 32L ;  default to low resolution
    if fmType eq 0  then npos = 32
    if fmType eq 1  then npos = 96
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]
    scalFact       = [0.3,0.3,0.3,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.3,0.3,0.5,0.5,0.5,0.5,0.5]
    ChannelsNot2Correct  = [-1]  ;--actual channels numbers (not IDL indexes)
    Channels2plotFig  = [0,3,5,10,15,17,20]
    ScanPos2plot      = [0,2,5,10,15,20,25,29]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_AQUA) THEN BEGIN   ;AQUA AMSRE
    npos = 191
    mxChan = 12L
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11]
    scalFact          = [0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3]
    ChannelsNot2Correct  = [-1]  ;--actual channels numbers (not IDL indexes)
    Channels2plotFig  = [0]
    ScanPos2plot      = [0]
    iEmiss2Plot       = 0
ENDIF

IF (sensor_id eq Consts.INT_SATID_FY3RI) THEN BEGIN   ;FY3 MWRI
    npos = 120
    mxChan = 10L
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8,9]
    scalFact       = [0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3]
    ChannelsNot2Correct=[-1] ;--actual channels numbers (not IDL indexes)
    Channels2plotFig  = [0,1,2,3,4,5,6,7,8,9]
    ScanPos2plot      = [60]
    iEmiss2Plot       = 1
ENDIF

; MTMA proxy data based on CR TMI
IF (sensor_id eq Consts.INT_SATID_MTMA) THEN BEGIN   ;MT MADRAS
    mxChan = 9L
;    if fmType eq -1 then npos = 26
;    if fmType eq 0  then npos = 104
;    if fmType eq 1  then npos = 208
    if fmType eq -1 then npos = 60
    if fmType eq 0  then npos = 240
    if fmType eq 1  then npos = 480
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5,6,7,8]
    scalFact          = [0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3]
    ChannelsNot2Correct  = [-1]  ;--actual channels numbers (not IDL indexes)
    Channels2plotFig  = [0]
    ScanPos2plot      = [0]
    iEmiss2Plot       = 0
ENDIF

; MTSA proxy data based on LR N18
IF (sensor_id eq Consts.INT_SATID_MTSA) THEN BEGIN   ;MT SAPHIR
    mxChan = 6L
;    if fmType eq 0 then npos = 30 ; 30, or 1 or whatever number of scan positions
;    if fmType eq 1 then npos = 90 ; 90, or 1 or whatever number of scan positions
    if fmType eq -1 then npos = 45 ; 30, or 1 or whatever number of scan positions
    if fmType eq 0  then npos = 91 ; 30, or 1 or whatever number of scan positions
    if fmType eq 1  then npos = 182 ; 90, or 1 or whatever number of scan positions
    ;---channels to process for bias computation
    Channels2process  = [0,1,2,3,4,5]
    scalFact          = [0.3,0.3,0.3,0.3,0.3,0.3]
    ChannelsNot2Correct  = [-1]  ;--actual channels numbers (not IDL indexes)
    Channels2plotFig  = [0]
    ScanPos2plot      = [0]
    iEmiss2Plot       = 0
ENDIF

nChannelsNot2Correct = n_elements(ChannelsNot2Correct)


loadct,39

;------------------------------------------------------------------------------
;  Reading radiances and scene data (analyses)
;------------------------------------------------------------------------------
readlist,listRadMeasName,listRadMeas,nfilesRadMeas
nListRadMeas = n_elements(listRadMeas)
readlist,listRadSimulName,listRadSimul,nfilesRadSimul
nListRadSimul = n_elements(listRadSimul)
readlist,listAnalysName,listAnalys,nfilesAnalys
nListAnalys = n_elements(listAnalys)
nList = min([nListRadSimul,nListRadMeas,nListAnalys,norbits2process])

;---Determine the date to process from the name of 1 file
x=listRadMeas[0]
pos1=strpos(x,'/',/reverse_search)
pos2=strpos(x,'/',pos1-1,/reverse_search)
date2process=strmid(x,pos2,(pos1-pos2))

;---consistency checks
IF (nListRadMeas ne nListRadSimul) THEN BEGIN
    print,'Warning: # files in simulation/measurements lists not identical',nListRadSimul,nListRadMeas
    STOP
ENDIF
IF (nListRadMeas ne nListAnalys) THEN BEGIN
    print,'Warning: #files in analyses/Rad-measurements lists not identical',nListAnalys,nListRadMeas
    STOP
ENDIF

ListRad              = strarr(nList,2)
ListRad(0:nList-1,0) = listRadMeas(0:nList-1)
ListRad(0:nList-1,1) = listRadSimul(0:nList-1)
;------------------------------------------------------------------------------
;  Allocation of memory for variables
;------------------------------------------------------------------------------

;--- Radiance variables maxFovs and mxChan should be derived from those files to avoid memory problem
mxFovs = 0L
nprf   = 0L
for ifile = 0L, nList - 1L do begin
  file = ListRad(ifile,0)
  openr,iu,file,/get_lun,/f77_unformatted,/swap_if_little_endian
  readu,iu,nprf
  if nprf gt mxFovs then mxFovs = nprf
  free_lun,iu 
endfor
print, 'mxFovs=', mxFovs
print, 'mxChan=', mxChan


nFovsRad                = lonarr(nList)
LatTabRad               = fltarr(mxFovs,nList,2)
LonTabRad               = fltarr(mxFovs,nList,2)
DirTabRad               = fltarr(mxFovs,nList,2)
PosTabRad               = fltarr(mxFovs,nList,2)
AngTabRad               = fltarr(mxFovs,nList,2)
tbTabRad                = fltarr(mxFovs,nList,mxChan,2)

;---NWP-Analyses variables
nFovsAnalys             = lonarr(nList)
LatAnalys               = fltarr(mxFovs,nList)
LonAnalys               = fltarr(mxFovs,nList)
DirAnalys               = fltarr(mxFovs,nList)
TPWAnalys               = make_array(mxFovs,nList,/float,value=-99.)
TSKAnalys               = make_array(mxFovs,nList,/float,value=-99.)
SfcTypAnalys            = make_array(mxFovs,nList,/integer,value=-99)
WindSpAnalys            = make_array(mxFovs,nList,/float,value=-99.)
EmissAnalys             = make_array(mxFovs,mxChan,nList,/float,value=-99.)


;------------------------------------------------------------------------------
;  Read radiances (measurements & simulations)
;------------------------------------------------------------------------------
FOR ifile=0L,nList-1L DO BEGIN
    print,'RAD-Processing:',ifile,ListRad(ifile,0:1)
    LoadRadFile,2,ListRad(ifile,0:1),Rad,0
    print, Rad.nprof,Rad.nchan
    nFovsRad(ifile) = Rad.nprof
    FOR iprof=0L,Rad.nprof-1 DO BEGIN
        LatTabRad(iprof,ifile,0:1) = Rad.Lat(0:1,iprof)
        LonTabRad(iprof,ifile,0:1) = Rad.Lon(0:1,iprof)
        DirTabRad(iprof,ifile,0:1) = Rad.Direc(0:1,iprof)
        PosTabRad(iprof,ifile,0:1) = Rad.ScanPos(0:1,iprof)
        AngTabRad(iprof,ifile,0) = mean(Rad.Angle(0,iprof,0:Rad.nchan-1))
        AngTabRad(iprof,ifile,1) = mean(Rad.Angle(1,iprof,0:Rad.nchan-1))
        tbTabRad(iprof,ifile,0:Rad.nchan-1,0:1) = transpose(Rad.tb(0:1,iprof,0:Rad.nchan-1))
    ENDFOR
ENDFOR


;------------------------------------------------------------------------------
;  Read scene data (analyses)
;------------------------------------------------------------------------------
FOR ifile=0L,nList-1L DO BEGIN
    print,'Analys-Processing:',ifile,listAnalys[ifile]
    LoadSceneFile,listAnalys[ifile],0,Scene,nFovsRad(ifile)
    nFovsAnalys(ifile) = Scene.nprofsprocessed
    IF (Scene.nprofsprocessed ge 1) THEN BEGIN
        LatAnalys(0:Scene.nprofsprocessed-1,ifile)    = Scene.lat(0:Scene.nprofsprocessed-1)
        LonAnalys(0:Scene.nprofsprocessed-1,ifile)    = Scene.lon(0:Scene.nprofsprocessed-1)
        DirAnalys(0:Scene.nprofsprocessed-1,ifile)    = Scene.direc(0:Scene.nprofsprocessed-1)
        TPWAnalys(0:Scene.nprofsprocessed-1,ifile)    = Scene.TPWvec(0:Scene.nprofsprocessed-1)
        TSKAnalys(0:Scene.nprofsprocessed-1,ifile)    = Scene.Tskinvec(0:Scene.nprofsprocessed-1)
        SfcTypAnalys(0:Scene.nprofsprocessed-1,ifile) = Scene.SfcTypVec(0:Scene.nprofsprocessed-1)
        WindSpAnalys(0:Scene.nprofsprocessed-1,ifile) = Scene.WindspVec(0:Scene.nprofsprocessed-1)
        EmissAnalys(0:Scene.nprofsprocessed-1,0:scene.nchan-1,ifile) = Scene.Emissvec(0:Scene.nprofsprocessed-1,0:scene.nchan-1)
    ENDIF
ENDFOR



sels:

    impr = 0
 

    ;-----Certain user-driven choices

    node2plot      = 0                  ;0/1/2 for asc/des/both
    polarities     = ['VH','V+H','H+V','V','H']
    fill_contin    = 1
    coasts         = 1

    ;---ranges of stratification 
    ;maxtpwVec      = [30.,100.]    ; maximum value of tpw allowed in computing biases
    ;mintpwVec      = [0., 30.]     ; minimum value of tpw allowed in computing biases
    ;maxwindVec     = [7.,100.]     ; maximum value of wind allowed in computing biases
    ;minwindVec     = [0.,7.  ]     ; minimum value of wind allowed in computing biases
    maxtpwVec      = [100.]     ; maximum value of tpw allowed in computing biases
    mintpwVec      = [0.]       ; minimum value of tpw allowed in computing biases
    maxwindVec     = [100.]     ; maximum value of wind allowed in computing biases
    minwindVec     = [0.]       ; minimum value of wind allowed in computing biases

    linesty =[0,0,1,1]
    psm     =[0,1,0,1]
    col     =[0,0,0,0,0,0,0,0,0]

    ;---derived variables and quality control
    ntpwranges=n_elements(maxtpwVec)
    if (n_elements(mintpwVec) ne ntpwranges) then begin
        stop,'Error in number of elements in TPW ranges vector'
    endif
    nWindranges=n_elements(maxwindVec)
    if (n_elements(minwindVec) ne nWindranges) then begin
        stop,'Error in number of elements in Wind ranges vector'
    endif

    ;---Reform arrays
    LatRad            = reform(LatTabRad(*,*,0),nList*mxFovs)
    LonRad            = reform(LonTabRad(*,*,0),nList*mxFovs)
    NodeRad           = reform(DirTabRad(*,*,0),nList*mxFovs)
    PosRad            = reform(PosTabRad(*,*,0),nList*mxFovs)
    SfcTyp            = reform(SfcTypAnalys(*,*),nList*mxFovs)
    emAn              = reform(EmissAnalys(*,iEmiss2Plot,*),mxFovs*nList)


    ;---Transformation into scanlines format
    nchan2plot   = n_elements(Channels2process)
    TransfIntoScan,reform(LatTabRad(*,*,0),mxFovs,nList),latScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(LonTabRad(*,*,0),mxFovs,nList),lonScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(PosTabRad(*,*,0),mxFovs,nList),PosRadScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(TPWAnalys(*,*), mxFovs,nList),tpwAnlScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(WindSpAnalys(*,*), mxFovs,nList),WindAnlScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(TSKAnalys(*,*), mxFovs,nList),TskinAnlScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(SfcTypAnalys(*,*),mxFovs,nList),sfcTypScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(EmissAnalys(*,iEmiss2Plot,*),mxFovs,nList),emAnlScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(AngTabRad(*,*,0),mxFovs,nList),angScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList


    ;----Declare stratified variables needed (biases, std )
    meanBiasReg           = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    meanBiasByHist        = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    meanTbsimu            = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    meanTbmeas            = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    stdv                  = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    nPoints               = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    Intercept             = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    Slope                 = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    AnlysTPW              = fltarr(npos,ntpwranges,nwindranges)
    AnlysTskin            = fltarr(npos,ntpwranges,nwindranges)
    AnlysEm               = fltarr(npos,ntpwranges,nwindranges)
    meanTbsimu2Output     = fltarr(npos,Rad.nchan)
    meanTbmeas2Output     = fltarr(npos,Rad.nchan)
    meanBiasReg2Output    = fltarr(npos,Rad.nchan)
    meanBiasByHist2Output = fltarr(npos,Rad.nchan)
    Slope2Output          = fltarr(npos,Rad.nchan)
    Intercept2Output      = fltarr(npos,Rad.nchan)
    stdv2Output           = fltarr(Rad.nchan)

    maxTbDiffallowed = 20

    ;----Compute scan-dependent and stratified mean biases/stadev/Number of Pts,etc
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2process[ichan]
        tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
        tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
        TransfIntoScan,reform(tbTabRad(*,*,ichan2plot,0),mxFovs,nList),tbmeasScan,npos,nscanl,scanPosTot,$
          reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
        TransfIntoScan,reform(tbTabRad(*,*,ichan2plot,1),mxFovs,nList),tbsimuScan,npos,nscanl,scanPosTot,$
          reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
        tbdiffScan = tbmeasScan-tbsimuScan
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            mintpw=mintpwVec(itpwrange)
            maxtpw=maxtpwVec(itpwrange)
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                minwind=minWindVec(iwindrange)
                maxwind=maxWindVec(iwindrange)
                for ipos=0,npos-1 do begin
                    ;----Filtering based on non-retrieved parameters
                    ind=where(sfcTypScan(*,ipos) eq Consts.OC_TYP  and latScan(*,ipos) ge minLat and $
                              latScan(*,ipos) le maxLat and $
                              tpwAnlScan(*,ipos) le maxtpw and tpwAnlScan(*,ipos) ge mintpw and $
                              WindAnlScan(*,ipos) le maxwind and WindAnlScan(*,ipos) ge minwind and $
                              tbsimuScan(*,ipos) gt 0 and tbmeasScan(*,ipos) gt 0 ,ncount)
                    ncount2 = 0
		    if ( ncount gt 1 ) then $
		    	ind2=where(abs(tbdiffScan(ind,ipos)-mean(tbdiffScan(ind,ipos))) le maxTbDiffallowed,ncount2)
                    IF (ncount2 gt 2) then begin
                        ;---radiometric stratified statistics
                        meanTbsimu(ipos,ichan2plot,itpwrange,iwindrange)  = mean(tbsimuScan(ind(ind2),ipos))                                         
                        meanTbmeas(ipos,ichan2plot,itpwrange,iwindrange)  = mean(tbmeasScan(ind(ind2),ipos))                                         
                        meanBiasReg(ipos,ichan2plot,itpwrange,iwindrange) = mean(tbdiffScan(ind(ind2),ipos))
                        stdv(ipos,ichan2plot,itpwrange,iwindrange)        = stdev(tbdiffScan(ind(ind2),ipos))
                        nPoints(ipos,ichan2plot,itpwrange,iwindrange)     = ncount2
                        
                        poly  = poly_fit(tbmeasScan(ind(ind2),ipos),tbsimuScan(ind(ind2),ipos),1,/double,yfit=yfit,Yerror=yErr)
                        Intercept(ipos,ichan2plot,itpwrange,iwindrange)   = poly(0)
                        Slope(ipos,ichan2plot,itpwrange,iwindrange)       = poly(1)
                        ;---Bias computation by histogram adjustment
                        res=histogram(tbdiffScan(ind(ind2),ipos),nbins=40,locations=loc)
                        res=res/float(max(res))*100.
                        indHist=where(res ge 80.)
                        ;indHist=where(res eq 100.)
                        meanBiasByHist(ipos,ichan2plot,itpwrange,iwindrange)   = mean(loc(indHist))
                        ;---Geophysical stratified statistics
                        anlysTPW(ipos,itpwrange,iwindrange)               = mean(tpwAnlScan(ind(ind2),ipos))
                        anlysTskin(ipos,itpwrange,iwindrange)             = mean(tskinAnlScan(ind(ind2),ipos))
                        anlysEm(ipos,itpwrange,iwindrange)                = mean(emAnlScan(ind(ind2),ipos))
                    ENDIF
                endfor
            ENDFOR
        ENDFOR
    ENDFOR

;--- For AMSRE, ChannelsNot2Correct is empty, in other words, we should correct all channels
IF (ChannelsNot2Correct[0] ge 0) THEN BEGIN
    ;---Cancel biases of non-trusted comparisons
    FOR ichan=0,nChannelsNot2Correct-1 DO BEGIN
        ichan2Cancel=ChannelsNot2Correct(ichan)-1
        meanBiasReg(*,ichan2Cancel,*,*)      = 0.
        meanBiasByHist(*,ichan2Cancel,*,*)   = 0.
        stdv(*,ichan2Cancel,*,*)             = 10.
        Intercept(*,ichan2Cancel,*,*)        = 0.
        Slope(*,ichan2Cancel,*,*)            = 1.
    ENDFOR
ENDIF

    ;---Fill-in Bias/Std file(s)
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2process[ichan]
        for ipos=0,npos-1 do begin
            meanTbsimu2Output(ipos,ichan2plot)     = mean(meanTbsimu(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            meanTbmeas2Output(ipos,ichan2plot)     = mean(meanTbmeas(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            meanBiasReg2Output(ipos,ichan2plot)    = mean(meanBiasReg(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            meanBiasByHist2Output(ipos,ichan2plot) = mean(meanBiasByHist(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            Slope2Output(ipos,ichan2plot)          = mean(Slope(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            Intercept2Output(ipos,ichan2plot)      = mean(Intercept(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
        endfor
        stdv2Output(ichan2plot) = mean(stdv(0:npos-1,ichan2plot,0:ntpwranges-1,0:nwindranges-1))*scalFact(ichan2plot)
    ENDFOR


    ;---Choose between regular bias and histogram-based bias
    IF (iBiasComputMethod eq 0) THEN BEGIN
        meanBias        = meanBiasReg
        meanBias2Output = meanBiasReg2Output
    ENDIF
    IF (iBiasComputMethod eq 1) THEN BEGIN
        meanBias        = meanBiasByHist
        meanBias2Output = meanBiasByHist2Output
    ENDIF
    


    WriteBias,22,biasFile,nchan2plot,npos,Rad.cfreq(Channels2process(0:nchan2plot-1)), $
      meanBias2Output(0:npos-1,Channels2process(0:nchan2plot-1)),                      $
      Slope2Output(0:npos-1,Channels2process(0:nchan2plot-1)),                         $
      Intercept2Output(0:npos-1,Channels2process(0:nchan2plot-1)),                     $
      meanTbsimu(0:npos-1,Channels2process(0:nchan2plot-1)),                           $
      meanTbmeas(0:npos-1,Channels2process(0:nchan2plot-1))
    writeErr,ErrFile,Rad.cfreq(Channels2process(0:nchan2plot-1)),stdv2Output(Channels2process(0:nchan2plot-1)),nchan2plot

    ;---Apply bias correction
    if ( nList gt 1 ) then tbTabRadMeasCal = reform(tbTabRad(*,*,*,0))
    if ( nList eq 1 ) then tbTabRadMeasCal = tbTabRad(*,*,*,0) 
    FOR ifile=0,nList-1 DO BEGIN
        FOR iprof=0L,nFovsRad(ifile)-1 DO BEGIN
            iPos=PosTabRad(iprof,ifile,0)-1
            FOR ichan=0,nchan2plot-1 DO BEGIN
                ichan2Cal = Channels2process[ichan]
                bias=meanBias2Output(ipos,ichan2Cal)
                tbTabRadMeasCal(iprof,ifile,ichan2cal) = tbTabRadMeasCal(iprof,ifile,ichan2cal) - bias
            ENDFOR
        ENDFOR
    ENDFOR



imp:

    print,'End of IDL processing...(radiom biases determination)'
    
    ;---plotting of results of biases if flag is set
    if (plot_output eq 0) then begin
        print, 'No plots generated...exiting IDL'
        exit
    endif
    !p.font=1
    !p.charsize   = 1.6
    nchan2plotFig     = n_elements(Channels2plotFig)


    plot_bias_hist        = 1
    plot_map_tb           = 1
    plot_map_biasItself   = 0
    plot_map_bias_aftCal  = 0
    plot_map_bias_befCal  = 0
    plot_map_bias_ScanPos = 1
    plot_map_bias_SfcTyp  = 1
    plot_map_bias_Emiss   = 1

    FOR ichan=0,nchan2plotFig-1 DO BEGIN
        ichan2plot = Channels2plotFig[ichan]
        tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
        tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
        tbmeasCal  = reform(tbTabRadMeasCal(*,*,ichan2plot),nList*mxFovs)
        TransfIntoScan,reform(tbTabRad(*,*,ichan2plot,0),mxFovs,nList),tbmeasScan,npos,nscanl,scanPosTot,$
          reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
        TransfIntoScan,reform(tbTabRad(*,*,ichan2plot,1),mxFovs,nList),tbsimuScan,npos,nscanl,scanPosTot,$
          reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
        tbdiffScan = tbmeasScan-tbsimuScan
        erase
        !p.multi=[16,4,4,0,0]
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            mintpw=mintpwVec(itpwrange)
            maxtpw=maxtpwVec(itpwrange)
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                minwind=minWindVec(iwindrange)
                maxwind=maxWindVec(iwindrange)
                for ipos0=0,n_elements(ScanPos2plot)-1 do begin
                    ipos=ScanPos2plot[iPos0]
                    ;----Filtering based on non-retrieved parameters
                    ind=where(sfcTypScan(*,ipos) eq Consts.OC_TYP  and latScan(*,ipos) ge minLat and $
                              latScan(*,ipos) le maxLat and $
                              tpwAnlScan(*,ipos) le maxtpw and tpwAnlScan(*,ipos) ge mintpw and $
                              WindAnlScan(*,ipos) le maxwind and WindAnlScan(*,ipos) ge minwind and $
                              tbsimuScan(*,ipos) gt 0 and tbmeasScan(*,ipos) gt 0,ncount)
                    ncount2 = 0
		    if ( ncount gt 1 ) then $
		    	ind2=where(abs(tbdiffScan(ind,ipos)-mean(tbdiffScan(ind,ipos))) le maxTbDiffallowed,ncount2)
                    ;for i=0,ncount2-1 do begin
                    ;    print,ipos,PosRadScan(ind(ind2(i)),ipos)
                    ;endfor
                    IF (ncount gt 1 and ncount2 gt 1 and plot_bias_hist eq 1) then begin
                        ;---original measurements
                        X0=tbmeasScan(ind(ind2),ipos)
                        ;---Bias-removed measurements
                        X=tbmeasScan(ind(ind2),ipos)-meanBias(ipos,ichan2plot,itpwrange,iwindrange)
                        ;---Bias-and-Slope-removed measurements
                        ;X=tbmeasScan(ind(ind2),ipos)*Slope(ipos,ichan2plot,itpwrange,iwindrange)+Intercept(ipos,ichan2plot,itpwrange,iwindrange)
                        ;---Simulated TBs
                        Y=tbsimuScan(ind(ind2),ipos)
                        tit='Scan Pos='+string(ipos+1,'(i2)')+' Chan#'+string(ichan2plot+1,'(i2)')+' Frq:'+string(Rad.cfreq(ichan2plot),'(f6.2)')+'GHz'
                        titSuppl=  'Mean Bias Applied:'+string(meanBias(ipos,ichan2plot,itpwrange,iwindrange),'(f7.2)')
                        ChoppVec,min([min(X),min(Y)]),max([max(X),max(Y)]),min([min(X),min(Y)]),max([max(X),max(Y)]),X,Y,20,20,'Measured TB [K]',$
                          'Simulated TB [K]',tit+titSuppl,0.6,1,stats
                        ;---Histogram(s)
                        plotHist,x-y,40,'Histogram bef/after calib.'+tit,'TB Departures [K]','Normalized PDF',mean(x-y),0,-10,10,res,0,0
                        plotHist,x0-y,40,'','','Normalized PDF',mean(x0-y),1,-10,10,res,0,0
                        plots,[0,0],[0,100],thick=4
                    ENDIF
                endfor
            ENDFOR
        ENDFOR
        ;---Map measurement itself
        IF (plot_map_tb eq 1) THEN BEGIN
            erase
            !p.multi=1
            x2plot=tbmeas
            tit='TB Measurement'+' Chan#'+string(ichan2plot+1,'(i2)')+' Frq:'+string(Rad.cfreq(ichan2plot),'(f6.2)')+$
              ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
            IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
            IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)
            ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0 and $
                      nodeTest,cnt)
	    if( cnt lt 1 ) then ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0)
	    
	    xmin=min(x2plot(ind))
            xmax=max(x2plot(ind))
            dx=xmax-xmin
            ecart=min([xmax-mean(x2plot(ind)),mean(x2plot(ind))-xmin])
            xmin=mean(x2plot(ind))-ecart
            xmax=mean(x2plot(ind))+ecart
            mapPlot,minlat,maxlat,minlon,maxlon,latRad,lonRad,ind,tit,xmin,xmax,x2plot,'[K]',0.8,8,0.5,0,'(f7.2)'
            map_continents,/continents,/noborder,/hires,fill_continents=0,color=18,coasts=coasts
        
	ENDIF
        ;---Map bias (bef calib)
        IF (plot_map_bias_befCal eq 1) THEN BEGIN
            erase
            !p.multi=1
            x2plot=tbmeas-tbsimu
            tit='Bias Bef Correction.'+' Chan#'+string(ichan2plot+1,'(i2)')+' Frq:'+string(Rad.cfreq(ichan2plot),'(f6.2)')
            IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
            IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)
            ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0 and $
                      sfcTypAnalys eq Consts.OC_TYP and nodeTest,cnt)
	    if( cnt lt 1 ) then ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and $
	     		                  tbmeas gt 0 and tbsimu gt 0 and  sfcTypAnalys eq Consts.OC_TYP)
            mapPlot,minlat,maxlat,minlon,maxlon,latRad,lonRad,ind,tit,-5,5,x2plot,'[K]',0.8,8,0.5,0,'(f7.2)'
            map_continents,/continents,/noborder,/hires,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
        ;---Map bias (aft calib)
        IF (plot_map_bias_aftCal eq 1) THEN BEGIN
            erase
            !p.multi=1
            x2plot=tbmeasCal-tbsimu
            tit='Bias Aft Correction.'+' Chan#'+string(ichan2plot+1,'(i2)')+' Frq:'+string(Rad.cfreq(ichan2plot),'(f6.2)')
            IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
            IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)
            ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0 and $
                      sfcTypAnalys eq Consts.OC_TYP and nodeTest,cnt)
	    if( cnt lt 1 ) then ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge $
	    		                  minLon and tbmeas gt 0 and tbsimu gt 0 and sfcTypAnalys eq Consts.OC_TYP)     
            mapPlot,minlat,maxlat,minlon,maxlon,latRad,lonRad,ind,tit,-5,5,x2plot,'[K]',0.8,8,0.5,0,'(f7.2)'
            map_continents,/continents,/noborder,/hires,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
        ;---Map (calib correction itself)
        IF (plot_map_biasItself eq 1) THEN BEGIN
            erase
            !p.multi=1
            x2plot=tbmeasCal-tbmeas
            tit='Calib Corr. [Cal-Orig]'+' Chan#'+string(ichan2plot+1,'(i2)')+' Frq:'+string(Rad.cfreq(ichan2plot),'(f6.2)')
            IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
            IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)
            ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0 and $
                      sfcTypAnalys eq Consts.OC_TYP and nodeTest,cnt)
	    if( cnt lt 1 ) then ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and $
		                          LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0 and sfcTypAnalys eq Consts.OC_TYP)    
            mapPlot,minlat,maxlat,minlon,maxlon,latRad,lonRad,ind,tit,-5,5,x2plot,'[K]',0.8,8,0.5,0,'(f7.2)'
            map_continents,/continents,/noborder,/hires,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDFOR
    ;---Map scan position
    IF (plot_map_bias_ScanPos eq 1) THEN BEGIN
        erase
        !p.multi=1
        x2plot=PosRad
        tit='Scan Position'
        IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
        IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)
        ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0 and nodeTest,cnt)
	if( cnt lt 1 ) then ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0)
        mapPlot,minlat,maxlat,minlon,maxlon,latRad,lonRad,ind,tit,0,npos,x2plot,'[Index]',0.8,8,0.5,0,'(f7.2)'
        map_continents,/continents,/noborder,/hires,fill_continents=fill_contin,color=18,coasts=coasts
    ENDIF
    ;---Map surface Type
    IF (plot_map_bias_SfcTyp eq 1) THEN BEGIN
        erase
        !p.multi=1
        x2plot=SfcTyp
        tit='Surface Type'
        IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
        IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)
        ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0 and nodeTest,cnt)
	if( cnt lt 1 ) then ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0)
        mapPlot,minlat,maxlat,minlon,maxlon,latRad,lonRad,ind,tit,0,3,x2plot,'[Index]',0.8,8,0.5,0,'(f7.2)'
        map_continents,/continents,/noborder,/hires,fill_continents=0,color=18,coasts=coasts
    ENDIF
    ;---Map emissivity 
    IF (plot_map_bias_Emiss eq 1) THEN BEGIN
        erase
        !p.multi=1
        x2plot=emAn
        tit='Surface Emissivity @ '+string(Rad.cfreq(iEmiss2Plot),'(f6.2)')+'GHz'+$
              ' Pol:'+polarities(Rad.polarity(iEmiss2Plot)-1)
        IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
        IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)
        ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0 and nodeTest,cnt)
	if( cnt lt 1 ) then ind=where(LatRad le maxlat and LatRad ge minLat and LonRad le maxlon and LonRad ge minLon and tbmeas gt 0 and tbsimu gt 0)
        mapPlot,minlat,maxlat,minlon,maxlon,latRad,lonRad,ind,tit,0.3,0.7,x2plot,'[%]',0.8,8,0.5,0,'(f7.2)'
        map_continents,/continents,/noborder,/hires,fill_continents=0,color=18,coasts=coasts
    ENDIF



    IF (Use_as_main_program eq 1) THEN BEGIN
        ;---Use this if used as main program
        IF (impr eq 1) THEN begin
            device,/close
            device ='x'
            GOTO, bo
        ENDIF
        Print,'________________________________________________'
        boucle:print,'PS/Exit(1/0)'
        read,impr
        IF (impr gt 3) THEN GOTO, boucle
        IF (impr eq 0) THEN GOTO, bo
        ip: IF (impr eq 1) THEN BEGIN
            ficres=psFilename
            print,' PostScript File created:',ficres
            set_plot,'ps'
            device,filename=ficres,/color,xsize=23,ysize=16,yoffset=2,/landscape
                                ;device,filename=ficres,/color,/landscape
            goto,imp
        ENDIF
        bo: print,'________________________________________________'
        impr=0
        devicd = 'x'
        set_plot,'x'
        fin:close,/all
        print,'End of procesing...'
    ENDIF
    IF (Use_as_main_program eq 0) THEN device,/close


END

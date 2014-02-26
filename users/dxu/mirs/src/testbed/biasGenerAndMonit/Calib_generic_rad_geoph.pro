;$Id: Calib_generic_rad_geoph.pro 2922 2012-02-07 21:13:34Z kgarrett $
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
;  - geophysical biases (wrt analyses, etc)
;
;------------------------------------------------------------------

;----Uncomment the following if used as subroutine
PRO Calib_generic_rad_geoph,Namelist=NamelistInputs,sensor_id=sensor_id
;------------------------------


Use_as_main_program =0  ;=0->as a subroutine, 1->as a main program

IF (Use_as_main_program eq 1) THEN BEGIN
    device='x'
    device,true=24,retain=2,decompose=0
    device,pseudo_color=8,retain=2
    device,get_visual_name=thisvisualClass
    loadct,39
    if (thisvisualClass ne 'pseudoColor') then device,decomposed=0
    close,/all
    ;----Windsat
    ;NamelistInputs='../../../data/ControlData/windsat_Inputs4BiasVerification.in'
    ;sensor_id=8
    ;----NOAA-18
    NamelistInputs='../../../data/ControlData/n18_Inputs4BiasVerification.in'
    sensor_id=1
    ;------------
    device='x'
    set_plot,'x'
    PRINT, 'Do you want to read again the data (1/0)?'
    read,answer
    IF (answer eq 0) then GOTO, sels
ENDIF
;------------------------------------------------

;--------------------------------------
;     Set sensor-specific items
;--------------------------------------
IF (sensor_id eq 1 or sensor_id eq 2) THEN BEGIN   ;METOPA & N18 AMSUA/MHS
    ;--------------------------------------
    ;       AMSU/MHS
    ;--------------------------------------
    npos = 30 ; 30, or 1 or whatever number of scan positions
    ;---channels to process for bias computation
    ;Channels2plot  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    ;Channels2plotFig  = [0,1,2,3,7,8,9,10,11,14,16,18]
    Channels2plot  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    Channels2plotFig  = [0,1]
    iEmiss2Plot       = 1
    ;Layers2process          = [20,44,55,63,70]
    Layers2process          = [20,70]
    nLayers2process         = n_elements(Layers2process)
    ;--------------------------------------
    ;--------------------------------------------------------------------
    ;---Plots, Contour, Map of TB simul vs TB measured or their diff
    ;--------------------------------------------------------------------
    plot_tb_scatterplt                            = 1
    plot_tb_map_simul                             = 0
    plot_tb_map_meas                              = 0
    plot_tbdiff_map                               = 1
    plot_tbdiff_vs_ScanPos                        = 0
    ;--------------------------------------------------------------------
    ;---Metrics (bias, std, #pnts, etc at #geophys situations(by scan pos)
    ;--------------------------------------------------------------------
    plot_tbdiffMeanStd_vs_ScanPos_All_bias        = 1
    plot_tbdiffMeanStd_vs_ScanPos_All_stdv        = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_Npts        = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_Intc        = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_Slop        = 0
    ;--------------------------------------------------------------------
    ;---Geophysical plots, maps, diff, etc
    ;--------------------------------------------------------------------
    ;---CLW
    plot_tbdiffMeanStd_vs_ScanPos_All_clw         = 0
    ;---TPW
    plot_tbdiffMeanStd_vs_ScanPos_All_tpwdiff     = 0
    plot_tpwDiff_analys_mirs                      = 0
    plot_tpwDiff_analys_mirs_map                  = 1
    plot_tpw_mirs_vs_analys                       = 0
    ;---ChiSq
    plot_ChiSq_mirs                               = 0
    plot_ChiSq_mirs_map                           = 1 
    ;---Tskin
    plot_tskin_mirs_vs_analys                     = 0
    plot_tskinDiff_analys_mirs_map                = 1
    plot_tskinDiff_analys_mirs_vsScanPos          = 0
    plot_tskin_analys_map                         = 0
    plot_tskin_mirs_map                           = 0
    ;---Emissivity/SfcType
    plot_em_mirs_vs_analys                        = 1
    plot_emDiff_analys_mirs_map                   = 1
    plot_emDiff_analys_mirs_vsScanPos             = 0
    plot_em_analys_map                            = 0
    plot_em_mirs_map                              = 1
    plot_sfc_type_mirs_map                        = 1
    plot_sfc_type_analys_map                      = 1
    ;---Temperature profile
    plot_tempP_analy_map                          = 0
    plot_tempP_mirs_map                           = 0
    plot_tempPdiff_mirs_analys_map                = 1
    plot_tempPstats                               = 1
    plot_tempP_histograms                         = 1
    ;---Humidity profile
    plot_qP_analy_map                             = 0
    plot_qP_mirs_map                              = 0
    plot_qPdiff_mirs_analys_map                   = 1
    plot_qPstats                                  = 1
    ;---Cloud 
    plot_clw_mirs_map                             = 1
    ;---Layers used
    plot_layers_used                              = 0
    ;---Surface pressure
    plot_sfcp_mirs_vs_analys                      = 1
ENDIF



IF (sensor_id eq 8) THEN BEGIN   ;WINDSAT
    ;--------------------------------------
    ;       Windsat
    ;--------------------------------------
    npos = 1 ; 30, or 1 or whatever number of scan positions
    ;---channels to process for bias computation
    Channels2plot  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
    Channels2plotFig  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
    iEmiss2Plot       = 7
    Layers2process          = [0]
    nLayers2process         = 1
    ;--------------------------------------
    ;--------------------------------------------------------------------
    ;---Plots, Contour, Map of TB simul vs TB measured or their diff
    ;--------------------------------------------------------------------
    plot_tb_scatterplt                            = 0
    plot_tb_map_simul                             = 0
    plot_tb_map_meas                              = 0
    plot_tbdiff_map                               = 0
    plot_tbdiff_vs_ScanPos                        = 0
    ;--------------------------------------------------------------------
    ;---Metrics (bias, std, #pnts, etc at #geophys situations(by scan pos)
    ;--------------------------------------------------------------------
    plot_tbdiffMeanStd_vs_ScanPos_All_bias        = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_stdv        = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_Npts        = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_Intc        = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_Slop        = 0
    ;--------------------------------------------------------------------
    ;---Geophysical plots, maps, diff, etc
    ;--------------------------------------------------------------------
    ;---CLW
    plot_tbdiffMeanStd_vs_ScanPos_All_clw         = 0
    ;---TPW
    plot_tbdiffMeanStd_vs_ScanPos_All_tpwdiff     = 0
    plot_tpwDiff_analys_mirs                      = 0
    plot_tpwDiff_analys_mirs_map                  = 0
    plot_tpw_mirs_vs_analys                       = 0
    ;---ChiSq
    plot_ChiSq_mirs                               = 0
    plot_ChiSq_mirs_map                           = 0 
    ;---Tskin
    plot_tskin_mirs_vs_analys                     = 0
    plot_tskinDiff_analys_mirs_map                = 0
    plot_tskinDiff_analys_mirs_vsScanPos          = 0
    plot_tskin_analys_map                         = 0
    plot_tskin_mirs_map                           = 0
    ;---Emissivity/SfcType
    plot_em_mirs_vs_analys                        = 0
    plot_emDiff_analys_mirs_map                   = 0
    plot_emDiff_analys_mirs_vsScanPos             = 0
    plot_em_analys_map                            = 0
    plot_em_mirs_map                              = 0
    plot_sfc_type_mirs_map                        = 1
    plot_sfc_type_analys_map                      = 1
    ;---Temperature profile
    plot_tempP_analy_map                          = 0
    plot_tempP_mirs_map                           = 0
    plot_tempPdiff_mirs_analys_map                = 0
    plot_tempPstats                               = 0
    plot_tempP_histograms                         = 0
    ;---Humidity profile
    plot_qP_analy_map                             = 0
    plot_qP_mirs_map                              = 0
    plot_qPdiff_mirs_analys_map                   = 0
    plot_qPstats                                  = 0
    plot_clw_mirs_map                             = 0
    ;---Layers used
    plot_layers_used                              = 0
    ;---Surface pressure
    plot_sfcp_mirs_vs_analys                      = 0
ENDIF


;------------------------------------------------------------------------------
; Reading namelist inputs
;------------------------------------------------------------------------------
  ListAnalysName='' & listRadSimulName='' & listRadMeasName='' & listEDRName='' & biasfile='' & psFilename=''
  openr,iu,NamelistInputs,/get_lun
  readf,iu,format='(a)',ListAnalysName
  readf,iu,format='(a)',listRadSimulName
  readf,iu,format='(a)',listRadMeasName
  readf,iu,format='(a)',listEDRName
  readf,iu,format='(a)',biasfile
  readf,iu,format='(a)',psFilename
  readf,iu,format='(i)',norbits2process
  close,iu
  free_lun,iu,/force

IF (Use_as_main_program eq 0) THEN BEGIN
    set_plot,'ps'
    device,filename=psFilename,/color,xsize=23,ysize=16,yoffset=2,/landscape
ENDIF


loadct,39

;------------------------------------------------------------------------------
; Initiate constants from the constants module
;------------------------------------------------------------------------------
Consts,Consts
;------------------------------------------------------------------------------
;  Reading radiances and scene data (analyses and retrievals)
;------------------------------------------------------------------------------
readlist,listRadMeasName,listRadMeas,nfilesRadMeas
nListRadMeas   = n_elements(listRadMeas)
readlist,listRadSimulName,listRadSimul,nfilesRadSimul
nListRadSimul  = n_elements(listRadSimul)
readlist,listAnalysName,listAnalys,nfilesAnalys
nListAnalys    = n_elements(listAnalys)
readlist,listEDRName,listEDR,nfilesEDR
nListEDR       = n_elements(listEDR)
nList          = min([nListRadSimul,nListRadMeas,nListAnalys,nListEDR,norbits2process])


;---Determine the date to process from the name of 1 file
x=listRadMeas[0]
pos1=strpos(x,'/',/reverse_search)
pos2=strpos(x,'/',pos1-1,/reverse_search)
date2process=strmid(x,pos2,(pos1-pos2))

;---consistency checks
IF (nListRadMeas ne nListRadSimul) THEN BEGIN
    print,'Warning: # files in simulation/measurements lists not identical',nListRadSimul,nListRadMeas
    ;STOP
ENDIF
IF (nListRadMeas ne nListAnalys) THEN BEGIN
    print,'Warning: #files in analyses/Rad-measurements lists not identical',nListAnalys,nListRadMeas
    ;STOP
ENDIF

ListRad              = strarr(nList,2)
ListRad(0:nList-1,0) = listRadMeas(0:nList-1)
ListRad(0:nList-1,1) = listRadSimul(0:nList-1)
;------------------------------------------------------------------------------
;  Allocation of memory for variables
;------------------------------------------------------------------------------

;---Radiance variables
mxFovs                  = 50000L
mxChan                  = 30
nFovsRad                = lonarr(nList)
LatTabRad               = make_array(mxFovs,nList,2,/float,value=-99.)
LonTabRad               = make_array(mxFovs,nList,2,/float,value=-99.)
DirTabRad               = make_array(mxFovs,nList,2,/float,value=-99.)
PosTabRad               = make_array(mxFovs,nList,2,/float,value=-99.)
AngTabRad               = make_array(mxFovs,nList,2,/float,value=-99.)
tbTabRad                = fltarr(mxFovs,nList,mxChan,2)

;---NWP-Analyses variables
nFovsAnalys             = lonarr(nList)
LatAnalys               = make_array(mxFovs,nList,/float,value=-99.)
LonAnalys               = make_array(mxFovs,nList,/float,value=-99.)
DirAnalys               = make_array(mxFovs,nList,/float,value=-99.)
TPWAnalys               = make_array(mxFovs,nList,/float,value=-99.)
TSKAnalys               = make_array(mxFovs,nList,/float,value=-99.)
SfcTypAnalys            = make_array(mxFovs,nList,/integer,value=-99)
SfcPressAnalys          = make_array(mxFovs,nList,/integer,value=-99)
WindSpAnalys            = make_array(mxFovs,nList,/float,value=-99.)
EmissAnalys             = make_array(mxFovs,mxChan,nList,/float,value=-99.)
TprofAnalys             = make_array(mxFovs,nLayers2process,nList,/float,value=-99.)
QprofAnalys             = make_array(mxFovs,nLayers2process,nList,/float,value=-99.)

;---MIRS-based EDR variables
nFovsEDR             = lonarr(nList)
LatEDR               = make_array(mxFovs,nList,/float,value=-99.)
LonEDR               = make_array(mxFovs,nList,/float,value=-99.)
DirEDR               = make_array(mxFovs,nList,/float,value=-99.)
ChiSqEDR             = make_array(mxFovs,nList,/float,value=-99.)
CLWEDR               = make_array(mxFovs,nList,/float,value=-99.)
RWPEDR               = make_array(mxFovs,nList,/float,value=-99.)
GWPEDR               = make_array(mxFovs,nList,/float,value=-99.)
TPWEDR               = make_array(mxFovs,nList,/float,value=-99.)
TSKEDR               = make_array(mxFovs,nList,/float,value=-99.)
SfcTypEDR            = make_array(mxFovs,nList,/integer,value=-99)
WindSpEDR            = make_array(mxFovs,nList,/float,value=-99.)
EmissEDR             = make_array(mxFovs,mxChan,nList,/float,value=-99.)
SfcPressEDR          = make_array(mxFovs,nList,/integer,value=-99)
qcAtmEDR             = make_array(mxFovs,nList,/integer,value=-99)
qcSfcEDR             = make_array(mxFovs,nList,/integer,value=-99)
TprofEDR             = make_array(mxFovs,nLayers2process,nList,/float,value=-99.)
QprofEDR             = make_array(mxFovs,nLayers2process,nList,/float,value=-99.)

;------------------------------------------------------------------------------
;  Read radiances (measurements & simulations)
;------------------------------------------------------------------------------
FOR ifile=0,nList-1 DO BEGIN
    print,'RAD-Processing:',ifile,ListRad(ifile,0:1)
    LoadRadFile,2,ListRad(ifile,0:1),Rad,0
    print, Rad.nprof,Rad.nchan
    nFovsRad(ifile) = Rad.nprof
    FOR iprof=0L,Rad.nprof-1 DO BEGIN
        LatTabRad(iprof,ifile,0:1) = Rad.Lat(0:1,iprof)
        LonTabRad(iprof,ifile,0:1) = Rad.Lon(0:1,iprof)
        DirTabRad(iprof,ifile,0:1) = Rad.Direc(0:1,iprof)
        PosTabRad(iprof,ifile,0:1) = Rad.ScanPos(0:1,iprof)
        AngTabRad(iprof,ifile,0)   = mean(Rad.Angle(0,iprof,0:Rad.nchan-1))
        AngTabRad(iprof,ifile,1)   = mean(Rad.Angle(1,iprof,0:Rad.nchan-1))
        tbTabRad(iprof,ifile,0:Rad.nchan-1,0:1) = transpose(Rad.tb(0:1,iprof,0:Rad.nchan-1))
    ENDFOR
ENDFOR

;------------------------------------------------------------------------------
;  Read scene data (retrievals)
;------------------------------------------------------------------------------
FOR ifile=0L,nList-1 DO BEGIN
    print,'Retrieval-Processing:',ifile,listEDR[ifile]
    LoadSceneFile,listEDR[ifile],0,Scene,nFovsRad(ifile)
    nFovsEDR(ifile) = Scene.nprofsprocessed
    IF (Scene.nprofsprocessed ge 1) THEN BEGIN
        LatEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.lat(0:Scene.nprofsprocessed-1)
        LonEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.lon(0:Scene.nprofsprocessed-1)
        DirEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.direc(0:Scene.nprofsprocessed-1)
        ChiSqEDR(0:Scene.nprofsprocessed-1,ifile) = Scene.ChiSq(0:Scene.nprofsprocessed-1)
        TPWEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.TPWvec(0:Scene.nprofsprocessed-1)
        CLWEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.CLWvec(0:Scene.nprofsprocessed-1)
        RWPEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.RWPvec(0:Scene.nprofsprocessed-1)
        GWPEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.GWPvec(0:Scene.nprofsprocessed-1)
        TSKEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.Tskinvec(0:Scene.nprofsprocessed-1)
        SfcTypEDR(0:Scene.nprofsprocessed-1,ifile)= Scene.SfcTypVec(0:Scene.nprofsprocessed-1)
        SfcPressEDR(0:Scene.nprofsprocessed-1,ifile)= Scene.SfcPressVec(0:Scene.nprofsprocessed-1)
        WindSpEDR(0:Scene.nprofsprocessed-1,ifile)= Scene.WindspVec(0:Scene.nprofsprocessed-1)
        EmissEDR(0:Scene.nprofsprocessed-1,0:scene.nchan-1,ifile)   = Scene.Emissvec(0:Scene.nprofsprocessed-1,0:scene.nchan-1)
        qcAtmEDR(0:Scene.nprofsprocessed-1,ifile) = Scene.qcAtm(0:Scene.nprofsprocessed-1,0)
        qcSfcEDR(0:Scene.nprofsprocessed-1,ifile) = Scene.qcSfc(0:Scene.nprofsprocessed-1,0)
        FOR ilay=0,nLayers2process-1 DO BEGIN
            TprofEDR(0:Scene.nprofsprocessed-1,ilay,ifile) = Scene.TempLayVec(0:Scene.nprofsprocessed-1,Layers2process(iLay))
            QprofEDR(0:Scene.nprofsprocessed-1,ilay,ifile) = Scene.AbsorbLayVec(0:Scene.nprofsprocessed-1,Layers2process(iLay),0)
        ENDFOR
    ENDIF
ENDFOR
PressLayEDR = Scene.PresLayVec(0,0:Scene.nLay-1)
;------------------------------------------------------------------------------
;  Read scene data (analyses)
;------------------------------------------------------------------------------
FOR ifile=0L,nList-1 DO BEGIN
    print,'Analys-Processing:',ifile,listAnalys[ifile]
    LoadSceneFile,listAnalys[ifile],0,Scene,nFovsRad(ifile)
    nFovsAnalys(ifile) = Scene.nprofsprocessed
    IF (Scene.nprofsprocessed ge 1) THEN BEGIN
        LatAnalys(0:Scene.nprofsprocessed-1,ifile)   = Scene.lat(0:Scene.nprofsprocessed-1)
        LonAnalys(0:Scene.nprofsprocessed-1,ifile)   = Scene.lon(0:Scene.nprofsprocessed-1)
        DirAnalys(0:Scene.nprofsprocessed-1,ifile)   = Scene.direc(0:Scene.nprofsprocessed-1)
        TPWAnalys(0:Scene.nprofsprocessed-1,ifile)   = Scene.TPWvec(0:Scene.nprofsprocessed-1)
        TSKAnalys(0:Scene.nprofsprocessed-1,ifile)   = Scene.Tskinvec(0:Scene.nprofsprocessed-1)
        SfcTypAnalys(0:Scene.nprofsprocessed-1,ifile)= Scene.SfcTypVec(0:Scene.nprofsprocessed-1)
        SfcPressAnalys(0:Scene.nprofsprocessed-1,ifile)= Scene.SfcPressVec(0:Scene.nprofsprocessed-1)
        WindSpAnalys(0:Scene.nprofsprocessed-1,ifile)= Scene.WindspVec(0:Scene.nprofsprocessed-1)
        EmissAnalys(0:Scene.nprofsprocessed-1,0:scene.nchan-1,ifile)   = Scene.Emissvec(0:Scene.nprofsprocessed-1,0:scene.nchan-1)
        FOR ilay=0,nLayers2process-1 DO BEGIN
            TprofAnalys(0:Scene.nprofsprocessed-1,ilay,ifile) = Scene.TempLayVec(0:Scene.nprofsprocessed-1,Layers2process(iLay))
            QprofAnalys(0:Scene.nprofsprocessed-1,ilay,ifile) = Scene.AbsorbLayVec(0:Scene.nprofsprocessed-1,Layers2process(iLay),0)
        ENDFOR
    ENDIF
ENDFOR
PressLayAnalys = Scene.PresLayVec(0,0:Scene.nLay-1)



sels:

    impr = 0
 
    ;-----Certain user-driven choices
    ;minLat  = 13
    ;maxLat  = 27
    ;minlon  = -94
    ;maxlon  = -66

    ;minLat  = 17
    ;maxLat  = 37
    ;minlon  = -115
    ;maxlon  = -75
    ;
    ;minLat  = -50
    ;maxLat  = 50
    ;minlon  = -180
    ;maxlon  = 180

    minLat  = -90
    maxLat  = 90
    minlon  = -180
    maxlon  = 180

    node2plot      = 2                  ;0/1/2 for asc/des/both
    polarities     = ['VH','V+H','H+V','V','H']
    fill_contin    = 0
    coasts         = 1
    ChiSqThresh    = 5.

    ;---ranges of stratification 
    maxtpwVec      = [20.,100.]    ; maximum value of tpw allowed in computing biases
    mintpwVec      = [0., 20.]     ; minimum value of tpw allowed in computing biases
    maxwindVec     = [5.,100.]     ; maximum value of wind allowed in computing biases
    minwindVec     = [0.,5.  ]       ; minimum value of wind allowed in computing biases
    ;maxtpwVec      = [100.]    ; maximum value of tpw allowed in computing biases
    ;mintpwVec      = [0.]     ; minimum value of tpw allowed in computing biases
    ;maxwindVec     = [100.]     ; maximum value of wind allowed in computing biases
    ;minwindVec     = [0.]       ; minimum value of wind allowed in computing biases

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

    ;---Transformation into scanlines format
    scanPos      = findgen(npos)+1
    nchan2plot   = n_elements(Channels2plot)
    NodeRad      = reform(DirTabRad(*,*,0),nList*mxFovs)
    TransfIntoScan,reform(LatTabRad(*,*,0),mxFovs,nList),latScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(LonTabRad(*,*,0),mxFovs,nList),lonScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(PosTabRad(*,*,0),mxFovs,nList),PosRadScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(TPWAnalys(*,*), mxFovs,nList),tpwAnlScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(WindSpAnalys(*,*), mxFovs,nList),WindAnlScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(TSKAnalys(*,*), mxFovs,nList),TskinAnlScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(SfcTypAnalys(*,*),mxFovs,nList),sfcTypScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(SfcTypEDR(*,*),mxFovs,nList),sfcTypEDRScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(EmissAnalys(*,iEmiss2Plot,*),mxFovs,nList),emAnlScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(AngTabRad(*,*,0),mxFovs,nList),angScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(ChiSqEDR(*,*), mxFovs,nList),ChiSqMirsScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(CLWEDR(*,*), mxFovs,nList),clwMirsScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(TPWEDR(*,*), mxFovs,nList),tpwMirsScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(TSKEDR(*,*), mxFovs,nList),TskinMirsScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
    TransfIntoScan,reform(EmissEDR(*,iEmiss2Plot,*),mxFovs,nList),emMirsScan,npos,nscanl,scanPosTot,reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList

    ;----Declare stratified variables needed (biases, std )
    meanBias         = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    meanBias2Output  = fltarr(npos,Rad.nchan)
    Slope2Output     = fltarr(npos,Rad.nchan)
    Intercept2Output = fltarr(npos,Rad.nchan)
    stdv             = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    nPoints          = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    Intercept        = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    Slope            = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    mirsChiSq        = fltarr(npos,ntpwranges,nwindranges)
    mirsCLW          = fltarr(npos,ntpwranges,nwindranges)
    mirsTPW          = fltarr(npos,ntpwranges,nwindranges)
    mirsTskin        = fltarr(npos,ntpwranges,nwindranges)
    mirsEm           = fltarr(npos,ntpwranges,nwindranges)
    AnlysTPW         = fltarr(npos,ntpwranges,nwindranges)
    AnlysTskin       = fltarr(npos,ntpwranges,nwindranges)
    AnlysEm          = fltarr(npos,ntpwranges,nwindranges)


    ;----Compute scan-dependent and stratified mean biases/stadev/Number of Pts,etc
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2plot[ichan]
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
                    ind=where(sfcTypEDRScan(*,ipos) eq Consts.OC_TYP  and latScan(*,ipos) ge minLat and $
                              latScan(*,ipos) le maxLat and $
                              tpwAnlScan(*,ipos) le maxtpw and tpwAnlScan(*,ipos) ge mintpw and $
                              WindAnlScan(*,ipos) le maxwind and WindAnlScan(*,ipos) ge minwind and $
                              tbsimuScan(*,ipos) gt -999 and tbmeasScan(*,ipos) gt -999 ,ncount)
                    IF (ncount gt 1) then begin
                        ;---radiometric stratified statistics
                        meanBias(ipos,ichan2plot,itpwrange,iwindrange)   = mean(tbdiffScan(ind,ipos))
                        stdv(ipos,ichan2plot,itpwrange,iwindrange)       = stdev(tbdiffScan(ind,ipos))
                        nPoints(ipos,ichan2plot,itpwrange,iwindrange)    = ncount
                        poly  = poly_fit(tbmeasScan(ind,ipos),tbsimuScan(ind,ipos),1,/double,yfit=yfit,Yerror=yErr)
                        Intercept(ipos,ichan2plot,itpwrange,iwindrange)  = poly(0)
                        Slope(ipos,ichan2plot,itpwrange,iwindrange)      = poly(1)
                        ;---Geophysical stratified statistics
                        mirsChiSq(ipos,itpwrange,iwindrange)             = mean(ChiSqMirsScan(ind,ipos))
                        mirsCLW(ipos,itpwrange,iwindrange)               = mean(clwMirsScan(ind,ipos))
                        mirsTPW(ipos,itpwrange,iwindrange)               = mean(tpwMirsScan(ind,ipos))
                        mirsTskin(ipos,itpwrange,iwindrange)             = mean(tskinMirsScan(ind,ipos))
                        mirsEm(ipos,itpwrange,iwindrange)                = mean(emMirsScan(ind,ipos))
                        anlysTPW(ipos,itpwrange,iwindrange)              = mean(tpwAnlScan(ind,ipos))
                        anlysTskin(ipos,itpwrange,iwindrange)            = mean(tskinAnlScan(ind,ipos))
                        anlysEm(ipos,itpwrange,iwindrange)               = mean(emAnlScan(ind,ipos))
                    ENDIF
                endfor
            ENDFOR
        ENDFOR
    ENDFOR


    ;---Fill-in Bias file
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2plot[ichan]
        for ipos=0,npos-1 do begin
            meanBias2Output(ipos,ichan2plot)  = mean(meanBias(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            Slope2Output(ipos,ichan2plot)     = mean(Slope(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            Intercept2Output(ipos,ichan2plot) = mean(Intercept(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
        endfor
    ENDFOR
    WriteBias,22,biasFile,nchan2plot,npos,Rad.cfreq(Channels2plot(0:nchan2plot-1)),$
      meanBias2Output(0:npos-1,Channels2plot(0:nchan2plot-1)),                      $
      Slope2Output(0:npos-1,Channels2plot(0:nchan2plot-1)),                         $
      Intercept2Output(0:npos-1,Channels2plot(0:nchan2plot-1))


imp:

    print,'End of IDL processing...(radiom/geophys biases determination)...starting generation of figures'


    !p.font=1
    !p.charsize   = 1.6
    csize         = 16
    a             = findgen(csize+1) *(!pi*2./float(csize))
    usersym,cos(a)/2,sin(a)/2,/fill
    
    nchan2plotfig     = n_elements(Channels2plotFig)
    symsize        = 0.7
    ;----common variables
    comm    =strarr(ntpwranges*nwindranges)
    ipl=-1
    for itpwrange=0,ntpwranges-1 DO BEGIN
        FOR iWindrange=0,nwindranges-1 DO BEGIN
            ipl=ipl+1
            comm[ipl] = string(mintpwVec[itpwrange],'(f5.1)')+'<TPW<'+string(maxtpwVec[itpwrange],'(f5.1)')+' /  '+  $
                        string(minwindVec[iWindrange],'(f5.1)')+'<WIND<'+string(maxwindVec[iWindRange],'(f5.1)')
        ENDFOR
    ENDFOR
    IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
    IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)

    IF (plot_sfcp_mirs_vs_analys eq 1) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        sfcp       = reform(SfcPressAnalys(*,*),nList*mxFovs)
        sfcpEDR    = reform(SfcPressEDR(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = sfcp
        y          = sfcpEDR
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
        ind_ld     = where(sfctyp ne Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_ld) 
        ind_all    = where(lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_all) 
        xmin=850
        ymin=850
        xmax=1050
        ymax=1050
        IF (ncount_oc ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            ChoppVec,xmin,xmax,ymin,ymax,X(ind_oc), Y(ind_oc), 50,50,'GDAS Sfc Press [mb]','MIRS Sfc Press [mb]','ocean',0.8,1,stats
            ERASE
            !p.multi   = 1
            ChoppVec,xmin,xmax,-10,10,X(ind_oc),Y(ind_oc)-X(ind_oc),50,50,'GDAS Sfc Press [mb]','MIRS-GDAS SfcP Diff.[mb]','ocean',0.8,0,stats
            plots,[xmin,xmax],[0,0]
        ENDIF
        IF (ncount_all ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            ChoppVec,xmin,xmax,ymin,ymax,X(ind_all),Y(ind_all),50,50,'GDAS Sfc Press [mb]','MIRS Sfc Press [mb]','ocean+land',0.8,1,stats
        ENDIF
    ENDIF


    ;----Layers used
    IF (plot_layers_used eq 1) THEN BEGIN
        nlayEDR=n_elements(presslayEDR)
        erase
        !p.multi=[3,3,1,0,0]
        FOR ilay=0,nlayEDR-1 DO BEGIN
            if (ilay eq 0) THEN plot,[0,1],[presslayEDR(ilay),presslayEDR(ilay)],xtickname=[''],xticklayout=1,$
              yrange=[1100,0.01],xrange=[0,1],/ylog,xstyle=4,ystyle=1,xticks=1,ytitle='Pressure (mb)',yticks=6,thick=2
            if (ilay ne 0) then oplot,[0,1],[presslayEDR(ilay),presslayEDR(ilay)],thick=2
        ENDFOR
        FOR ilay=0,nlayEDR-1 DO BEGIN
            if (ilay eq 0) THEN plot,[0,1],[presslayEDR(ilay),presslayEDR(ilay)],xtickname=[''],xticklayout=1,$
              yrange=[1100,0.01],xrange=[0,1],xstyle=4,ystyle=1,xticks=1,ytitle='Pressure (mb)',yticks=6,thick=2
            if (ilay ne 0) then oplot,[0,1],[presslayEDR(ilay),presslayEDR(ilay)],thick=2
        ENDFOR
        dPressLayEDR=pressLayEDR
        FOR ilay=0,nlayEDR-1 DO BEGIN
            if (ilay eq 0) THEN dPressLayEDR(ilay)=PressLayEDR(ilay)
            if (ilay ne 0) then dPressLayEDR(ilay)=presslayEDR(ilay)-presslayEDR(ilay-1)
        ENDFOR
        plot,dPressLayEDR,presslayEDR,yrange=[1100,0.01],xrange=[0,30],ystyle=1,ytitle='Pressure Layer Thickness (mb)',yticks=6,thick=2
    ENDIF

    ;---Cloud profile
    IF (plot_clw_mirs_map) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        FOR iLay2plot=0,nLayers2process-1 DO BEGIN 
            ERASE
            !p.multi   = 1
            x          = reform(QprofEDR(*,iLay2plot,*), nList*mxFovs)
            y          = reform(QprofAnalys(*,iLay2plot,*), nList*mxFovs)
            ind        = where(lat ge minLat and lat le maxLat and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount) 
            IF (ncount ne 0) THEN BEGIN
                ymin       = min([min(x(ind)),min(y(ind))])
                ymax       = max([max(x(ind)),max(y(ind))])
                tit        = Date2process+'/ Map of MIRS WV Field @ Layer#'+string(Layers2process(iLay2plot)+1,'(i3)')+$
                  ' P:'+string(pressLayEDR(Layers2process(iLay2plot)),'(f6.1)')+'mb'
                mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
                  ymin,ymax,x,'[g/Kg]',0.6,8,1,0,'(f7.2)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            ENDIF
        ENDFOR
    ENDIF


;--------Temperature profile
    IF (plot_qPstats) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        stats      = fltarr(nLayers2process,3,4)
        FOR iLay=0,nLayers2process-1 DO BEGIN 
            x          = reform(QprofEDR(*,iLay,*), nList*mxFovs)
            y          = reform(QprofAnalys(*,iLay,*), nList*mxFovs)
            ind_oc     = where(sfctyp eq Consts.OC_TYP and chisq0 le chisqThresh and lat ge minLat and lat le maxLat and $
                               chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc)
            ind_ld     = where(sfctyp ne Consts.OC_TYP and chisq0 le chisqThresh and lat ge minLat and lat le maxLat and $
                               chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_ld)
            IF (ncount_oc ne 0) THEN BEGIN 
                ;---ocean
                stats(ilay,0,0) = (mean(x(ind_oc)-y(ind_oc)))/(mean(y(ind_oc)))*100.
                stats(ilay,1,0) = (stdev(x(ind_oc)-y(ind_oc)))/(mean(y(ind_oc)))*100.
            ENDIF
            IF (ncount_ld ne 0) THEN BEGIN 
                ;---land
                stats(ilay,0,1) = (mean(x(ind_ld)-y(ind_ld)))/(mean(y(ind_ld)))*100.
                stats(ilay,1,1) = (stdev(x(ind_ld)-y(ind_ld)))/(mean(y(ind_ld)))*100.
            ENDIF
        ENDFOR
        erase
        !p.multi=1
        plot,stats(*,0,0),pressLayEDR(Layers2process),yrange=[1000,300],xrange=[-40,40],title='',$
          xtitle='Water Vapor M. Ratio Bias [%]',ytitle='Pressure (mb)',psym=-8,/ylog,thick=4,xthick=4,ythick=4
        plots,[0,0],[1000,300],color=160,thick=4
        oplot,stats(*,0,1),pressLayEDR(Layers2process),linestyle=2,color=240,thick=4
        plot_legend,0,2,[0,2],[8,0],[1,240],['Ocean','Land'],0.9
        erase
        !p.multi=1
        plot,stats(*,1,0),pressLayEDR(Layers2process),yrange=[1000,300],xrange=[0,100],title='',$
          xtitle='WV M. Ratio Std Deviation [%]',ytitle='Pressure (mb)',psym=-8,/ylog,thick=4,xthick=4,ythick=4
        oplot,stats(*,1,1),pressLayEDR(Layers2process),linestyle=2,color=240,thick=4
        plot_legend,0,2,[0,2],[8,0],[1,240],['Ocean','Land'],0.9
    ENDIF

    IF (plot_qPdiff_mirs_analys_map) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        FOR iLay2plot=0,nLayers2process-1 DO BEGIN 
            ERASE
            !p.multi   = 1
            x          = reform(QprofEDR(*,iLay2plot,*), nList*mxFovs)
            y          = reform(QprofAnalys(*,iLay2plot,*), nList*mxFovs)
            x          = x-y
            ind        = where(lat ge minLat and lat le maxLat and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount)
            IF (ncount ne 0) THEN BEGIN
                ymin       = min(x(ind))
                ymax       = max(x(ind))
                                ;ymin       = -4.
                                ;ymax       = 4.
                tit        = Date2process+'/ Map of MIRS-GDAS WV Diff. @ Layer#'+string(Layers2process(iLay2plot)+1,'(i3)')+$
                  ' P:'+string(pressLayEDR(Layers2process(iLay2plot)),'(f6.1)')+'mb'
                mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
                  ymin,ymax,x,'[g/Kg]',0.6,8,1,0,'(f7.2)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_qP_mirs_map) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        FOR iLay2plot=0,nLayers2process-1 DO BEGIN 
            ERASE
            !p.multi   = 1
            x          = reform(QprofEDR(*,iLay2plot,*), nList*mxFovs)
            y          = reform(QprofAnalys(*,iLay2plot,*), nList*mxFovs)
            ind        = where(lat ge minLat and lat le maxLat and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount)
            IF (ncount ne 0) THEN BEGIN
                ymin       = min([min(x(ind)),min(y(ind))])
                ymax       = max([max(x(ind)),max(y(ind))])
                tit        = Date2process+'/ Map of MIRS WV Field @ Layer#'+string(Layers2process(iLay2plot)+1,'(i3)')+$
                  ' P:'+string(pressLayEDR(Layers2process(iLay2plot)),'(f6.1)')+'mb'
                mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
                  ymin,ymax,x,'[g/Kg]',0.6,8,1,0,'(f7.2)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_qP_analy_map) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        FOR iLay2plot=0,nLayers2process-1 DO BEGIN 
            ERASE
            !p.multi   = 1
            x          = reform(QprofAnalys(*,iLay2plot,*), nList*mxFovs)
            y          = reform(QprofEDR(*,iLay2plot,*), nList*mxFovs)
            ind        = where(lat ge minLat and lat le maxLat and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount) 
            IF (ncount ne 0) THEN BEGIN
                ymin       = min([min(x(ind)),min(y(ind))])
                ymax       = max([max(x(ind)),max(y(ind))])
                tit        = Date2process+'/ Map of GDAS Field WV @ Layer#'+string(Layers2process(iLay2plot)+1,'(i3)')+$
                  ' P:'+string(pressLayEDR(Layers2process(iLay2plot)),'(f6.1)')+'mb'
                mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
                  ymin,ymax,x,'[g/Kg]',0.6,8,1,0,'(f7.2)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            ENDIF
        ENDFOR

    ENDIF
    

;--------Temperature profile
    IF (plot_tempPstats) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        stats      = fltarr(nLayers2process,3,4)
        FOR iLay=0,nLayers2process-1 DO BEGIN 
            x          = reform(TprofEDR(*,iLay,*), nList*mxFovs)
            y          = reform(TprofAnalys(*,iLay,*), nList*mxFovs)
            ind_oc     = where(sfctyp eq Consts.OC_TYP and chisq0 le chisqThresh and lat ge minLat and lat le maxLat and $
                               chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc)
            ind_ld     = where(sfctyp ne Consts.OC_TYP and chisq0 le chisqThresh and lat ge minLat and lat le maxLat and $
                               chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_ld)
            ;---ocean
            IF (ncount_oc ne 0) THEN BEGIN
                stats(ilay,0,0) = mean(x(ind_oc)-y(ind_oc))
                stats(ilay,1,0) = stdev(x(ind_oc)-y(ind_oc))
                stats(ilay,2,0) = sqrt(mean((x(ind_oc)-y(ind_oc))^2))
            ENDIF
            ;---land
            IF (ncount_ld ne 0) THEN BEGIN
                stats(ilay,0,1) = mean(x(ind_ld)-y(ind_ld))
                stats(ilay,1,1) = stdev(x(ind_ld)-y(ind_ld))
                stats(ilay,2,1) = sqrt(mean((x(ind_ld)-y(ind_ld))^2))
            ENDIF
        ENDFOR
        erase
        !p.multi=1
        plot,stats(*,0,0),pressLayEDR(Layers2process),yrange=[1000,10],xrange=[-2,2],title='',$
          xtitle='Temperature Bias [K]',ytitle='Pressure (mb)',psym=-8,/ylog,thick=4,xthick=4,ythick=4
        plots,[0,0],[1000,10],color=160,thick=4
        oplot,stats(*,0,1),pressLayEDR(Layers2process),linestyle=2,color=240,thick=4
        plot_legend,0,2,[0,2],[8,0],[1,240],['Ocean','Land'],0.9

        erase
        !p.multi=1
        plot,stats(*,1,0),pressLayEDR(Layers2process),yrange=[1000,10],xrange=[0,4],title='',$
          xtitle='Temperature Std Deviation [K]',ytitle='Pressure (mb)',psym=-8,/ylog,thick=4,xthick=4,ythick=4
        oplot,stats(*,1,1),pressLayEDR(Layers2process),linestyle=2,color=240,thick=4
        plot_legend,0,2,[0,2],[8,0],[1,240],['Ocean','Land'],0.9
    ENDIF

    IF (plot_tempP_histograms) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        ERASE
        !p.multi   = [12,3,4,0,0]
        FOR iLay2plot=0,nLayers2process-1 DO BEGIN 
            x          = reform(TprofEDR(*,iLay2plot,*), nList*mxFovs)
            y          = reform(TprofAnalys(*,iLay2plot,*), nList*mxFovs)
            x          = x-y
            ind_oc     = where(sfctyp eq Consts.OC_TYP and chisq0 le chisqThresh and lat ge minLat and lat le maxLat and $
                               chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest, ncount_oc) 
            ind_ld     = where(sfctyp ne Consts.OC_TYP and chisq0 le chisqThresh and lat ge minLat and lat le maxLat and $
                               chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest, ncount_ld) 
            tit        = Date2process+'/ Hist of Temp. Error @ Lay#'+string(Layers2process(iLay2plot)+1,'(i3)')+$
              ' P:'+string(pressLayEDR(Layers2process(iLay2plot)),'(f6.1)')+'mb'
            IF (ncount_oc ne 0) THEN BEGIN
                plotHist,x(ind_oc),60,tit,'MIRS-GDAS Departures [K]','Normalized PDF',mean(x(ind_oc)),0,-10,10,res,0,0
                plots,[0,0],[min(res),max(res)],color=120
            ENDIF
            IF (ncount_ld ne 0) THEN BEGIN
                plotHist,x(ind_ld),60,tit,'MIRS-GDAS Departures [K]','Normalized PDF',mean(x(ind_ld)),1,-10,10,res,0,0
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_tempPdiff_mirs_analys_map) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        FOR iLay2plot=0,nLayers2process-1 DO BEGIN 
            ERASE
            !p.multi   = 1
            x          = reform(TprofEDR(*,iLay2plot,*), nList*mxFovs)
            y          = reform(TprofAnalys(*,iLay2plot,*), nList*mxFovs)
            x          = x-y
            ind        = where(lat ge minLat and lat le maxLat and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount)
            IF (ncount ne 0) THEN BEGIN
                ymin       = min(x(ind))
                ymax       = max(x(ind))
                ymin       = -10.
                ymax       = 10.
                tit        = Date2process+'/ Map of MIRS-GDAS Temperature Diff. @ Layer#'+string(Layers2process(iLay2plot)+1,'(i3)')+$
                  ' P:'+string(pressLayEDR(Layers2process(iLay2plot)),'(f6.1)')+'mb'
                mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
                  ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.1)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_tempP_mirs_map) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        FOR iLay2plot=0,nLayers2process-1 DO BEGIN 
            ERASE
            !p.multi   = 1
            x          = reform(TprofEDR(*,iLay2plot,*), nList*mxFovs)
            y          = reform(TprofAnalys(*,iLay2plot,*), nList*mxFovs)
            ind        = where(lat ge minLat and lat le maxLat and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount)
            IF (ncount ne 0) THEN BEGIN
                ymin       = min([min(x(ind)),min(y(ind))])
                ymax       = max([max(x(ind)),max(y(ind))])
                tit        = Date2process+'/ Map of MIRS Temperature Field @ Layer#'+string(Layers2process(iLay2plot)+1,'(i3)')+$
                  ' P:'+string(pressLayEDR(Layers2process(iLay2plot)),'(f6.1)')+'mb'
                mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
                  ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.1)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_tempP_analy_map) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        FOR iLay2plot=0,nLayers2process-1 DO BEGIN 
            ERASE
            !p.multi   = 1
            x          = reform(TprofAnalys(*,iLay2plot,*), nList*mxFovs)
            y          = reform(TprofEDR(*,iLay2plot,*), nList*mxFovs)
            ind        = where(lat ge minLat and lat le maxLat and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount)
            IF (ncount ne 0) THEN BEGIN
                ymin       = min([min(x(ind)),min(y(ind))])
                ymax       = max([max(x(ind)),max(y(ind))])
                tit        = Date2process+'/ Map of GDAS Field Temperature @ Layer#'+string(Layers2process(iLay2plot)+1,'(i3)')+$
                  ' P:'+string(pressLayEDR(Layers2process(iLay2plot)),'(f6.1)')+'mb'
                mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
                  ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.1)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            ENDIF
        ENDFOR
    ENDIF

    ;-----------EMISSIVITY/SFCTYP PLOTS
    IF (plot_sfc_type_analys_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = SfcTypAnalys
        x          = reform(x(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind        = where(lat ge minLat and lat le maxLat and nodeTest,ncount) 
        IF (ncount ne 0) THEN BEGIN
            ymin       = min(x(ind))
            ymax       = max(x(ind))
            tit        = Date2process+'/ Map distribution of GDAS Surface Type '
            mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
              ymin,ymax,x,'[Index]',0.6,8,1,0,'(f3.1)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF

    IF (plot_sfc_type_mirs_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = sfctypEDR
        x          = reform(x(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        IF (node2plot ne 2) THEN nodeTest=(nodeRad eq node2plot)
        IF (node2plot eq 2) THEN nodeTest=(nodeRad eq nodeRad)
        ind        = where(lat ge minLat and lat le maxLat and nodeTest,ncount) 
        IF (ncount ne 0) THEN BEGIN
            ymin       = min(x(ind))
            ymax       = max(x(ind))
            tit        = Date2process+'/ Map distribution of MIRS Surface Type '
            mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
              ymin,ymax,x,'[Index]',0.6,8,1,0,'(f3.1)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF


    IF (plot_emDiff_analys_mirs_vsScanPos eq 1) THEN BEGIN
        erase
        !p.multi=1
        extFreq='F:' +string(Rad.cfreq(iEmiss2Plot),'(f5.1)') + ' Pol:'+polarities(Rad.polarity(iEmiss2Plot)-1)
        tit=Date2process+' MIRS-ANALYS Emissivity Diff @'+extFreq
        x=mirsEm-AnlysEm
        plot,scanPos,x(*,0,0),xtitle='Scan Position',ytitle=extFreq+' Emiss MIRS-ANALYS Diff',$
          title=tit,yrange=[min(x(*,*,*)),max(x(*,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,x(*,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6        
    ENDIF

    IF (plot_em_mirs_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = EmissEDR
        x          = reform(x(*,1,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
        ind_ld     = where(sfctyp ne Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_ld) 
        ind_all    = where(lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_all)
        IF (ncount_oc ne 0) THEN BEGIN
            ymin       = min(x(ind_oc))
            ymax       = max(x(ind_oc))
            ;ymin       = 0.42
            ;ymax       = 0.52
            extFreq    = 'F:' +string(Rad.cfreq(iEmiss2Plot),'(f5.1)') + ' Pol:'+polarities(Rad.polarity(iEmiss2Plot)-1)
            tit        = Date2process+' Map distribution of MIRS Emissivity @'+extFreq
            ERASE
            !p.multi   = 1
            mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
              ymin,ymax,x,'[]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
        ENDIF
        IF (ncount_ld ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            ymin       = 0.65
            ymax       = 0.95
            mapPlot,min(lat(ind_ld)),max(lat(ind_ld)),min(lon(ind_ld)),max(lon(ind_ld)),lat,lon,ind_ld,tit,$
              ymin,ymax,x,'[]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
        ENDIF
        IF (ncount_all ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            ymin       = 0.4
            ymax       = 0.95
            mapPlot,min(lat(ind_all)),max(lat(ind_all)),min(lon(ind_all)),max(lon(ind_all)),lat,lon,ind_all,tit,$
              ymin,ymax,x,'[]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF

    IF (plot_em_analys_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = EmissAnalys
        x          = reform(x(*,1,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
        IF (ncount_oc ne 0) THEN BEGIN
            ymin       = min(x(ind_oc))
            ymax       = max(x(ind_oc))
            ;ymin       = 0.42
            ;ymax       = 0.52
            extFreq    = 'F:' +string(Rad.cfreq(iEmiss2Plot),'(f5.1)') + ' Pol:'+polarities(Rad.polarity(iEmiss2Plot)-1)
            tit        = Date2process+' Map distribution of ANALYS Emissivity @'+extFreq
            mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
              ymin,ymax,x,'[]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF

    IF (plot_emDiff_analys_mirs_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = EmissEDR-EmissAnalys
        x          = reform(x(*,1,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
        IF (ncount_oc ne 0) THEN BEGIN
            ymin       = -0.1
            ymax       = 0.1
            extFreq    = 'F:' +string(Rad.cfreq(iEmiss2Plot),'(f5.1)') + ' Pol:'+polarities(Rad.polarity(iEmiss2Plot)-1)
            tit        = Date2process+' Map distribution of MIRS-ANALYS EMISS.Diff '+extFreq
            mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
              ymin,ymax,x,'[]',0.8,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF


    IF (plot_em_mirs_vs_analys eq 1) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = reform(EmissAnalys(*,1,*), nList*mxFovs)
        y          = reform(EmissEDR(*,1,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
        ind_ld     = where(sfctyp ne Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_ld) 
        ind_all    = where(lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_all) 
        IF (ncount_oc ne 0) THEN BEGIN
            xmin=min([min(x(ind_oc)),min(y(ind_oc))])
            ymin=xmin
            xmax=max([max(x(ind_oc)),max(y(ind_oc))])
            ymax=xmax
            ERASE
            !p.multi   = 1
            ;!p.multi   = [3,1,3,0,0]
            extFreq    = 'F:' +string(Rad.cfreq(iEmiss2Plot),'(f5.1)') + ' Pol:'+polarities(Rad.polarity(iEmiss2Plot)-1)
            ChoppVec,min([min(x(ind_oc)),min(y(ind_oc))]),max([max(x(ind_oc)),max(y(ind_oc))]),$
              min([min(x(ind_oc)),min(y(ind_oc))]),max([max(x(ind_oc)),max(y(ind_oc))]),X(ind_oc),$
              Y(ind_oc),50,50,'GDAS Analysis Emissivity'+extFreq,'MIRS Emissivity '+extFreq,'Ocean',1.2,1,stats
        ENDIF
    ENDIF


    IF (plot_tpw_mirs_vs_analys eq 1) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        sfcp       = reform(SfcPressAnalys(*,*),nList*mxFovs)
        sfcpEDR    = reform(SfcPressEDR(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = reform(TPWAnalys(*,*), nList*mxFovs)
        y          = reform(TPWEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
        ind_ld     = where(sfctyp ne Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_ld) 
        ind_all    = where(lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_all) 
        xmin=0
        ymin=0
        xmax=80
        ymax=80
        IF (ncount_oc ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            ChoppVec,xmin,xmax,ymin,ymax,X(ind_oc), Y(ind_oc), 50,50,'GDAS Anlysis TPW [mm]','MIRS TPW [mm]','ocean',0.8,1,stats
            ERASE
            !p.multi   = 1
            ChoppVec,xmin,xmax,-10,10,X(ind_oc),Y(ind_oc)-X(ind_oc),50,50,'GDAS Anlysis TPW [mm]','MIRS-GDAS TPW Diff.[mm]','ocean',0.8,0,stats
            plots,[xmin,xmax],[0,0]
            ERASE
            !p.multi   = [3,1,3,0,0]
            plot,sfcP(ind_oc),Y(ind_oc)-X(ind_oc),yrange=[-10,10],xrange=[850,1030],$
              xtitle='GDAS Sfc Pressure [mb]',ytitle='MIRS-GDAS TPW Diff.[mm]',title='ocean',charsize=0.8,psym=1,symsize=0.5
            plots,[min(sfcP(ind_oc)),max(sfcP(ind_oc))],[0,0]
            plot,sfcpEDR(ind_oc),Y(ind_oc)-X(ind_oc),yrange=[-10,10],xrange=[850,1030],$
              xtitle='MIRS Sfc Pressure [mb]',ytitle='MIRS-GDAS TPW Diff.[mm]',title='ocean',charsize=0.8,psym=1,symsize=0.5
            plots,[min(sfcpEDR(ind_oc)),max(sfcpEDR(ind_oc))],[0,0]
            plot,sfcP(ind_oc)-sfcpEDR(ind_oc),Y(ind_oc)-X(ind_oc),yrange=[-10,10],$
              xtitle='GDAS-MIRS Sfc Pressure Diff [mb]',ytitle='MIRS-GDAS TPW Diff.[mm]',title='ocean',charsize=0.8,psym=1,symsize=0.5
            plots,[min(sfcP(ind_oc)-sfcpEDR(ind_oc)),max(sfcP(ind_oc)-sfcpEDR(ind_oc))],[0,0]
        ENDIF
        IF (ncount_ld ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            ChoppVec,xmin,xmax,ymin,ymax,X(ind_ld), Y(ind_ld), 50,50,'GDAS Anlysis TPW [mm]','MIRS TPW [mm]','land',0.8,1,stats
        ENDIF
        IF (ncount_all ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            ChoppVec,xmin,xmax,ymin,ymax,X(ind_all),Y(ind_all),50,50,'GDAS Anlysis TPW [mm]','MIRS TPW [mm]','ocean+land',0.8,1,stats
        ENDIF
    ENDIF


    IF (plot_tpwDiff_analys_mirs_map eq 1) THEN BEGIN
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TPWEDR-TPWAnalys
        x          = reform(x(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
        ind_ld     = where(sfctyp ne Consts.OC_TYP and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_ld) 
        ind_all    = where(lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_all) 
        ymin       = -10.
        ymax       = 10.
        tit        = Date2process+' Map distribution of MIRS-ANALYS TPW Diff [mm]. '
        IF (ncount_oc ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
              ymin,ymax,x,'[mm]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
        IF (ncount_ld ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            mapPlot,min(lat(ind_ld)),max(lat(ind_ld)),min(lon(ind_ld)),max(lon(ind_ld)),lat,lon,ind_ld,tit,$
              ymin,ymax,x,'[mm]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
        IF (ncount_all ne 0) THEN BEGIN
            ERASE
            !p.multi   = 1
            mapPlot,min(lat(ind_all)),max(lat(ind_all)),min(lon(ind_all)),max(lon(ind_all)),lat,lon,ind_all,tit,$
              ymin,ymax,x,'[mm]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF

    IF (plot_tpwDiff_analys_mirs eq 1) THEN BEGIN
        erase
        !p.multi=1
        tit=Date2process+' TPW DIFFERENCE (MIRS-ANALYSIS) -over ocean-'
        x=MirsTPW-AnlysTPW
        plot,scanPos,x(*,0,0),xtitle='Scan Position',ytitle='TPW Diff MIRS-ANAL [mm]',$
          title=tit,yrange=[min(x(*,*,*)),max(x(*,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,x(*,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6        
    ENDIF


;--------TSKIN
    IF (plot_tskinDiff_analys_mirs_vsScanPos eq 1) THEN BEGIN
        erase
        !p.multi=1
        tit=Date2process+' MIRS-ANALYS TSKIN DIFF [K]'
        x=mirsTskin-AnlysTskin
        plot,scanPos,x(*,0,0),xtitle='Scan Position',ytitle='Tskin MIRS-ANALYS Diff [K]',$
          title=tit,yrange=[min(x(*,*,*)),max(x(*,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,x(*,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6        
    ENDIF

    IF (plot_tskin_mirs_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TSKEDR
        x          = reform(x(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind        = where(lat ge minLat and lat le maxLat and chisq0 le chisqThresh and chisq0 ge 0. and $
                               qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount)
        IF (ncount ne 0) THEN BEGIN
            ymin       = min(x(ind))
            ymax       = max(x(ind))
            ymin       = 240
            ymax       = 310
            tit        = Date2process+' Map distribution of MIRS TSKIN [K]. '
            mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
              ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF


    IF (plot_tskin_analys_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TSKAnalys
        x          = reform(x(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind        = where(lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount) 
        IF (ncount ne 0) THEN BEGIN
            ymin       = min(x(ind))
            ymax       = max(x(ind))
            ymin       = 240
            ymax       = 310
            tit        = Date2process+' Map distribution of ANALYS TSKIN [K]. '
            mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
              ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF

    IF (plot_tskinDiff_analys_mirs_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TSKEDR-TSKAnalys
        x          = reform(x(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind        = where(lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount) 
        IF (ncount ne 0) THEN BEGIN
            ymin       = -20.
            ymax       = 20.
            tit        = Date2process+' Map distribution of MIRS-ANALYS TSKIN Diff [K]. '
            mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
              ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF

    IF (plot_tskin_mirs_vs_analys eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        x          = reform(TskAnalys(*,*), nList*mxFovs)
        y          = reform(TSKEDR(*,*), nList*mxFovs)
        ind        = where(lat ge minLat and lat le maxLat and chisq0 le chisqThresh $
                           and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount) 
        IF (ncount ne 0) THEN BEGIN
            xmin=240
            ymin=240
            xmax=310
            ymax=310
            PlotScatt,X(ind),Y(ind),'GDAS Anlysis Tskin [K]','MIRS Tskin [K]','',1,$
              xmin,xmax,ymin,ymax,1.,1.
        ENDIF
    ENDIF



;------------------ChiSquare
    IF (plot_ChiSq_mirs_map eq 1) THEN BEGIN
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_all    = where(lat ge minLat and lat le maxLat $
                               and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_all) 
        IF (ncount_all ne 0) THEN BEGIN
            ymin       = 0.
            ymax       = 5.
            tit        = Date2process+'/ Geographical distribution of MIRS ChiSq '
            mapPlot,min(lat(ind_all)),max(lat(ind_all)),min(lon(ind_all)),max(lon(ind_all)),lat,lon,ind_all,tit,$
              ymin,ymax,chisq0,'[]',0.6,8,1,0,'(f7.3)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
        ENDIF
    ENDIF

    IF (plot_ChiSq_mirs eq 1) THEN BEGIN
        erase
        !p.multi=1
        tit=Date2process+'MIRS Convergence Metric (ChiSq)'
        x=MirsChiSq
        plot,scanPos,x(*,0,0),xtitle='Scan Position',ytitle='ChiSq',$
          title=tit,yrange=[min(x(*,*,*)),max(x(*,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,x(*,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6        
    ENDIF


;-----TB comparisons (slope, bias, intc. etc)
    IF (plot_tbdiffMeanStd_vs_ScanPos_All_bias eq 1) THEN BEGIN
        ;---All means together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Bias
        FOR ichan=0,nchan2plotFig-1 DO BEGIN
            ichan2plot = Channels2plotFig[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+$
              ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
            plot,scanPos,meanBias(*,ichan2plot,0,0),xtitle='Scan Position',ytitle='Bias [meas-simu] [K]',$
              title=tit,yrange=[min(meanBias(*,ichan2plot,*,*)),max(meanBias(*,ichan2plot,*,*))]
            ipl=-1
            FOR itpwrange=0,ntpwranges-1 DO BEGIN
                FOR iWindrange=0,nwindranges-1 DO BEGIN
                    ipl=ipl+1
                    oplot,scanPos,meanBias(*,ichan2plot,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
                ENDFOR
            ENDFOR
            plots,[0,30],[0,0],color=240
            plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
        ENDFOR
    ENDIF


    IF (plot_tbdiffMeanStd_vs_ScanPos_All_stdv eq 1) THEN BEGIN
        ;---All stdv together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Stdev
        FOR ichan=0,nchan2plotFig-1 DO BEGIN
            ichan2plot = Channels2plotFig[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+$
              ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
            plot,scanPos,Stdv(*,ichan2plot,0,0),xtitle='Scan Position',ytitle='Std Dev [K]',$
              title=tit,yrange=[min(stdv(*,ichan2plot,*,*)),max(stdv(*,ichan2plot,*,*))]
            ipl=-1
            FOR itpwrange=0,ntpwranges-1 DO BEGIN
                FOR iWindrange=0,nwindranges-1 DO BEGIN
                    ipl=ipl+1
                    oplot,scanPos,stdv(*,ichan2plot,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
                ENDFOR
            ENDFOR
            plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
        ENDFOR
    ENDIF


    IF (plot_tbdiffMeanStd_vs_ScanPos_All_Npts eq 1) THEN BEGIN
        ;---All Npts together
        erase
        !p.multi=1
        tit=Date2process
        plot,scanPos,Npoints(*,0,0,0),xtitle='Scan Position',ytitle='# of Points',$
          title=tit,yrange=[min(Npoints(*,0,*,*)),max(Npoints(*,0,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,Npoints(*,0,itpwrange,iwindrange),linestyle=linesty[ipl],$
                  psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
    ENDIF


    IF (plot_tbdiffMeanStd_vs_ScanPos_All_Intc eq 1) THEN BEGIN
        ;---All intercepts together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Stdev
        FOR ichan=0,nchan2plotFig-1 DO BEGIN
            ichan2plot = Channels2plotFig[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+$
              ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
            plot,scanPos,Intercept(*,ichan2plot,0,0),xtitle='Scan Position',ytitle='Intercept [K]',$
              title=tit,yrange=[min(Intercept(*,ichan2plot,*,*)),max(Intercept(*,ichan2plot,*,*))]
            ipl=-1
            FOR itpwrange=0,ntpwranges-1 DO BEGIN
                FOR iWindrange=0,nwindranges-1 DO BEGIN
                    ipl=ipl+1
                    oplot,scanPos,Intercept(*,ichan2plot,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
                ENDFOR
            ENDFOR
            plots,[0,30],[0,0],color=240
            plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
        ENDFOR
    ENDIF

    IF (plot_tbdiffMeanStd_vs_ScanPos_All_Slop eq 1) THEN BEGIN
        ;---All Slopes together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Stdev
        FOR ichan=0,nchan2plotFig-1 DO BEGIN
            ichan2plot = Channels2plotFig[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+$
              ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
            plot,scanPos,slope(*,ichan2plot,0,0),xtitle='Scan Position',ytitle='Slope ',$
              title=tit,yrange=[min(Slope(*,ichan2plot,*,*)),max(Slope(*,ichan2plot,*,*))]
            ipl=-1
            FOR itpwrange=0,ntpwranges-1 DO BEGIN
                FOR iWindrange=0,nwindranges-1 DO BEGIN
                    ipl=ipl+1
                    oplot,scanPos,Slope(*,ichan2plot,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
                ENDFOR
            ENDFOR
            plots,[0,30],[1,1],color=240
            plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
        ENDFOR
    ENDIF


    IF (plot_tbdiff_vs_ScanPos eq 1) THEN BEGIN
        ERASE
        !p.multi=[20,4,5,0,0]
        scanPos=findgen(npos)+1
        FOR ichan2plot=0,Rad.nchan-1 DO BEGIN
            tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
            sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
            ind_oc     = where(sfctyp eq Consts.OC_TYP and tbmeas gt -999 and tbsimu gt -999 and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
            IF (ncount_oc ne 0) THEN BEGIN
                ;ymin       = min(tbmeas(ind_oc)-tbsimu(ind_oc))
                ;ymax       = max(tbmeas(ind_oc)-tbsimu(ind_oc))
                ymin       = -10.
                ymax       = 10.
                TransfIntoScan,reform(tbTabRad(*,*,ichan2plot,0),mxFovs,nList),tbmeasScan,npos,nscanl,scanPosTot,$
                  reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
                TransfIntoScan,reform(tbTabRad(*,*,ichan2plot,1),mxFovs,nList),tbsimuScan,npos,nscanl,scanPosTot,$
                  reform(PosTabRad(*,*,0),mxFovs,nList),nFovsRad,nList
                tit=Date2process+' Channel#'+string(ichan2plot+1,'(i2)')+' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+$
                  ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
                iplotted=0
                FOR iscan=0L,nscanl-1 DO BEGIN
                    ind=where(sfcTypEDRScan(iscan,*) eq Consts.OC_TYP and latScan(iscan,*) ge minLat and latScan(iscan,*) le maxLat,ncount)
                    IF (ncount ne 0 and iplotted eq 0) THEN BEGIN 
                        plot,scanPos(ind),tbmeasScan(iscan,ind)-tbsimuScan(iscan,ind),xtitle='Scan position',$
                          ytitle='TB diff [meas-simul] [K]',title=tit,psym=1,symsize=0.6,yrange=[ymin,ymax]
                        iplotted=iplotted+1
                    ENDIF
                    IF (ncount ne 0 and iplotted ne 0) THEN BEGIN 
                        oplot,scanPos(ind),tbmeasScan(iscan,ind)-tbsimuScan(iscan,ind),psym=1,symsize=0.6
                        iplotted=iplotted+1
                    ENDIF
                ENDFOR
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_tb_scatterplt eq 1) THEN BEGIN
        ERASE
        !p.multi=[20,4,5,0,0]
        FOR ichan2plot=0,Rad.nchan-1 DO BEGIN
            tbmeas=reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu=reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
            sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
            ind_oc     = where(sfctyp eq Consts.OC_TYP and tbmeas gt -999 and tbsimu gt -999 and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
            IF (ncount_oc ne 0) THEN BEGIN
                ymin=min([min(tbmeas(ind_oc)),min(tbsimu(ind_oc))])
                ymax=max([max(tbmeas(ind_oc)),max(tbsimu(ind_oc))])
                tit=Date2process+' Channel#'+string(ichan2plot+1,'(i2)')+' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+$
                  ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
                ChoppVec,ymin,ymax,ymin,ymax,tbmeas(ind_oc),tbsimu(ind_oc),20,20,'TB Measurements [K]',$
                  'TB Simulations [K]',tit,0.6,1,stats
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_tb_map_simul eq 1) THEN BEGIN
        FOR ichan=0,n_elements(channels2plotFig)-1 DO BEGIN
            ERASE
            !p.multi   = 1
            ichan2plot = channels2plotFig[ichan]
            tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
            sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
            ind_oc     = where(sfctyp eq Consts.OC_TYP and tbmeas gt -999 and tbsimu gt -999 and lat ge minLat and lat le maxLat $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
            IF (ncount_oc ne 0) THEN BEGIN
                ymin       = min([min(tbmeas(ind_oc)),min(tbsimu(ind_oc))])
                ymax       = max([max(tbmeas(ind_oc)),max(tbsimu(ind_oc))])
                tit        = Date2process+' TB simulations @ Channel#'+string(ichan2plot+1,'(i2)')+' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+'GHz'+$
                  ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
                x2plot=tbsimu
                dy=ymax-ymin
                ecart=min([ymax-mean(x2plot(ind)),mean(x2plot(ind))-ymin])
                ymin=mean(x2plot(ind))-ecart
                ymax=mean(x2plot(ind))+ecart
                mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
                  ymin,ymax,tbsimu,'[K]',0.6,8,1,0,'(f6.1)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_tb_map_meas eq 1) THEN BEGIN
        FOR ichan=0,n_elements(channels2plotFig)-1 DO BEGIN
            ERASE
            !p.multi   = 1
            ichan2plot = channels2plotFig[ichan]
            tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
            ind        = where(tbmeas gt -999 and tbsimu gt -999 and lat ge minLat and lat le maxLat $
                               and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount) 
            IF (ncount ne 0) THEN BEGIN
                ymin       = min([min(tbmeas(ind)),min(tbsimu(ind))])
                ymax       = max([max(tbmeas(ind)),max(tbsimu(ind))])
                tit        = Date2process+' TB measurements @ Channel#'+string(ichan2plot+1,'(i2)')+' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+'GHz '+$
                  ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
                x2plot=tbmeas
                dy=ymax-ymin
                ecart=min([ymax-mean(x2plot(ind)),mean(x2plot(ind))-ymin])
                ymin=mean(x2plot(ind))-ecart
                ymax=mean(x2plot(ind))+ecart
                mapPlot,min(lat(ind)),max(lat(ind)),min(lon(ind)),max(lon(ind)),lat,lon,ind,tit,$
                  ymin,ymax,tbmeas,'[K]',0.6,8,1,0,'(f6.1)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
            ENDIF
        ENDFOR
    ENDIF

    IF (plot_tbdiff_map eq 1) THEN BEGIN
        FOR ichan=0,n_elements(channels2plotFig)-1 DO BEGIN
            ERASE
            !p.multi   = 1
            ichan2plot = channels2plotFig[ichan]
            tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            ;sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
            sfctyp     = reform(SfcTypEDR(*,*),nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
            ind_oc     = where(sfctyp eq Consts.OC_TYP and tbmeas gt -999 and tbsimu gt -999 and lat ge minLat and lat le maxLat $
                               and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0 and nodeTest,ncount_oc) 
            IF (ncount_oc ne 0) THEN BEGIN
                ymin       = min(tbmeas(ind_oc)-tbsimu(ind_oc))
                ymax       = max(tbmeas(ind_oc)-tbsimu(ind_oc))
                ymin       = -5
                ymax       = 5.
                tit        = Date2process+' TB Differences [Meas-Simul] @ Channel#'+string(ichan2plot+1,'(i2)')+$
                  ' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+'GHz '+$
                  ' Pol:'+polarities(Rad.polarity(ichan2plot)-1)
                mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
                  ymin,ymax,tbmeas-tbsimu,'[K]',0.6,8,1,0,'(f6.1)'
                map_continents,/continents,/noborder,/hires,/usa,fill_continents=fill_contin,color=18,coasts=coasts
            ENDIF
        ENDFOR
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

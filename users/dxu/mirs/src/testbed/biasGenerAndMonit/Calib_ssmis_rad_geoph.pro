@../../../../setup/paths_idl.pro
PRO Calib_ssmis_rad_geoph,ListAnalys=ListAnalysName,listRadSimul=listRadSimulName,listRadMeas=listRadMeasName,$
              listEDRName=listEDRName,biasfile=biasFile

;ListAnalysName='NWPanalys_n18.list'
;listRadSimulName='FWDanalys_n18.list'
;listRadMeasName='RadFiles.dat'
;listEDRName='edrFiles_amsuam.list'
;biasfile='biasCorrec_n18.dat'

;------------------------------------------------------------------
;
; Program used to assess the calibration of microwave radiances
; by comparing them to simulations based on analyses (GDAS).
;
; Sid-Ahmed Boukabara. September, 2005.
;
; Extended to perform the monitoring of:
;  - bias residuals
;  - geophysical biases (wrt analyses, MSPPS, etc)
;
;------------------------------------------------------------------
set_plot,'ps'
loadct,39


;------------------------------------------------------------------------------
;  Reading radiances and scene data (analyses and retrievals)
;------------------------------------------------------------------------------
readlist,listRadMeasName,listRadMeas,nfilesRadMeas
nListRadMeas = n_elements(listRadMeas)
readlist,listRadSimulName,listRadSimul,nfilesRadSimul
nListRadSimul = n_elements(listRadSimul)
readlist,listAnalysName,listAnalys,nfilesAnalys
nListAnalys = n_elements(listAnalys)
readlist,listEDRName,listEDR,nfilesEDR
nListEDR = n_elements(listEDR)
nList=min([nListRadSimul,nListRadMeas,nListAnalys,nListEDR])

;---Determine the date to process from the name of 1 file
x=listRadMeas[0]
pos1=strpos(x,'/',/reverse_search)
pos2=strpos(x,'/',pos1-1,/reverse_search)
date2process=strmid(x,pos2,(pos1-pos2))


ListRad              = strarr(nList,2)
ListRad(0:nList-1,0) = listRadMeas(0:nList-1)
ListRad(0:nList-1,1) = listRadSimul(0:nList-1)
;------------------------------------------------------------------------------
;  Allocation of memory for variables
;------------------------------------------------------------------------------

;---Radiance variables
mxFovs                  = 99999L
mxChan                  = 100
nFovsRad                = lonarr(nList)
LatTabRad               = fltarr(mxFovs,nList,2)
LonTabRad               = fltarr(mxFovs,nList,2)
DirTabRad               = fltarr(mxFovs,nList,2)
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

;---MIRS-based EDR variables
nFovsEDR             = lonarr(nList)
LatEDR               = fltarr(mxFovs,nList)
LonEDR               = fltarr(mxFovs,nList)
DirEDR               = fltarr(mxFovs,nList)
ChiSqEDR             = make_array(mxFovs,nList,/float,value=-99.)
CLWEDR               = make_array(mxFovs,nList,/float,value=-99.)
TPWEDR               = make_array(mxFovs,nList,/float,value=-99.)
TSKEDR               = make_array(mxFovs,nList,/float,value=-99.)
SfcTypEDR            = make_array(mxFovs,nList,/integer,value=-99)
WindSpEDR            = make_array(mxFovs,nList,/float,value=-99.)
EmissEDR             = make_array(mxFovs,mxChan,nList,/float,value=-99.)
qcAtmEDR             = make_array(mxFovs,nList,/integer,value=-99)
qcSfcEDR             = make_array(mxFovs,nList,/integer,value=-99)


;----MSPPS-based EDR variables
CLW_mspps            = fltarr(mxFovs,nList)
TPW_mspps            = fltarr(mxFovs,nList)

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
        TSKEDR(0:Scene.nprofsprocessed-1,ifile)   = Scene.Tskinvec(0:Scene.nprofsprocessed-1)
        SfcTypEDR(0:Scene.nprofsprocessed-1,ifile)= Scene.SfcTypVec(0:Scene.nprofsprocessed-1)
        WindSpEDR(0:Scene.nprofsprocessed-1,ifile)= Scene.WindspVec(0:Scene.nprofsprocessed-1)
        EmissEDR(0:Scene.nprofsprocessed-1,0:scene.nchan-1,ifile)   = Scene.Emissvec(0:Scene.nprofsprocessed-1,0:scene.nchan-1)
        qcAtmEDR(0:Scene.nprofsprocessed-1,ifile) = Scene.qcAtm(0:Scene.nprofsprocessed-1,0)
        qcSfcEDR(0:Scene.nprofsprocessed-1,ifile) = Scene.qcSfc(0:Scene.nprofsprocessed-1,0)
    ENDIF
ENDFOR

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
        WindSpAnalys(0:Scene.nprofsprocessed-1,ifile)= Scene.WindspVec(0:Scene.nprofsprocessed-1)
        EmissAnalys(0:Scene.nprofsprocessed-1,0:scene.nchan-1,ifile)   = Scene.Emissvec(0:Scene.nprofsprocessed-1,0:scene.nchan-1)
    ENDIF
ENDFOR



sels:

    impr = 0
 

    ;----Compute CLW 4 for cloud removal (using NOAA-18 algorithms)
    FOR ifile=0L,nList-1 DO BEGIN
        nprof=min([nFovsAnalys(ifile),nFovsRad(ifile)])
        TL=make_array(nprof,value=280.,/float)
        computeFromAMSU_TPW_CLW_EMIS,tbTabRad(0:nprof-1,ifile,0,0),tbTabRad(0:nprof-1,ifile,1,0),$
          TSKAnalys(0:nprof-1,ifile),TL(0:nprof-1),EmissAnalys(0:nprof-1,0,ifile),$
          EmissAnalys(0:nprof-1,1,ifile),AngTabRad(0:nprof-1,ifile,0),nprof,V,L
          CLW_mspps(0:nprof-1,ifile) = L(0:nprof-1)
          TPW_mspps(0:nprof-1,ifile) = V(0:nprof-1)
    ENDFOR



    ;-----Certain user-driven choices
    minLat  = 22
    maxLat  = 32
    minlon  = -100
    maxlon  = -80
    ;
    minLat  = 17
    maxLat  = 37
    minlon  = -115
    maxlon  = -75
    ;
    minLat  = -50
    maxLat  = 50
    minlon  = -180
    maxlon  = 180

    minLat  = -80
    maxLat  = 80
    minlon  = -180
    maxlon  = 180

    node2plot      = 2                  ;0/1/2 for asc/des/both
    maxclw         = 0.005             ; maximum value of clw allowed in computing biases
    ChiSqThresh    = 10.

    ;---ranges of stratification 
    maxtpwVec      = [20.,100.]    ; maximum value of tpw allowed in computing biases
    mintpwVec      = [0., 20.]     ; minimum value of tpw allowed in computing biases
    maxwindVec     = [5.,100.]     ; maximum value of wind allowed in computing biases
    minwindVec     = [0.,5.  ]       ; minimum value of wind allowed in computing biases
    ;maxtpwVec      = [100.]    ; maximum value of tpw allowed in computing biases
    ;mintpwVec      = [0.]     ; minimum value of tpw allowed in computing biases
    ;maxwindVec     = [100.]     ; maximum value of wind allowed in computing biases
    ;minwindVec     = [0.]       ; minimum value of wind allowed in computing biases

    ;---channels to process for bias computation
    Channels2plot  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
    ;Channels2plot  = [1,8]

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
    npos         = 30
    scanPos      = findgen(npos)+1
    nchan2plot   = n_elements(Channels2plot)
    TransfIntoScan,reform(LatTabRad(*,*,0),nList*mxFovs),latScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(LonTabRad(*,*,0),nList*mxFovs),lonScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(CLW_mspps(*,*), nList*mxFovs),clw0Scan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(TPW_mspps(*,*), nList*mxFovs),tpw0Scan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(TPWAnalys(*,*), nList*mxFovs),tpwAnlScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(WindSpAnalys(*,*), nList*mxFovs),WindAnlScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(TSKAnalys(*,*), nList*mxFovs),TskinAnlScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(SfcTypAnalys(*,*),nList*mxFovs),sfcTypScan,npos,nscanl,scanPosTot
    tb89d      = reform(tbTabRad(*,*,14,0),nList*mxFovs)-reform(tbTabRad(*,*,15,0),nList*mxFovs)
    TransfIntoScan,tb89d,tb89dScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(tbTabRad(*,*,0,0),nList*mxFovs),tb23mScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(tbTabRad(*,*,1,0),nList*mxFovs),tb31mScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(EmissAnalys(*,0,*),nList*mxFovs),em23AnlScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(EmissAnalys(*,1,*),nList*mxFovs),em31AnlScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(AngTabRad(*,*,0),nList*mxFovs),angScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(ChiSqEDR(*,*), nList*mxFovs),ChiSqMirsScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(CLWEDR(*,*), nList*mxFovs),clwMirsScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(TPWEDR(*,*), nList*mxFovs),tpwMirsScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(TSKEDR(*,*), nList*mxFovs),TskinMirsScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(EmissEDR(*,0,*),nList*mxFovs),em23MirsScan,npos,nscanl,scanPosTot
    TransfIntoScan,reform(EmissEDR(*,1,*),nList*mxFovs),em31MirsScan,npos,nscanl,scanPosTot

    ;----Declare stratified variables needed (biases, std )
    meanBias        = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    meanBias2Output = fltarr(npos,Rad.nchan)
    meanBias_cal    = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    stdv            = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    nPoints         = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    Intercept       = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    Slope           = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    msppsCLW        = fltarr(npos,ntpwranges,nwindranges)
    msppsTPW        = fltarr(npos,ntpwranges,nwindranges)
    mirsChiSq       = fltarr(npos,ntpwranges,nwindranges)
    mirsCLW         = fltarr(npos,ntpwranges,nwindranges)
    mirsTPW         = fltarr(npos,ntpwranges,nwindranges)
    mirsTskin       = fltarr(npos,ntpwranges,nwindranges)
    mirsEm23        = fltarr(npos,ntpwranges,nwindranges)
    mirsEm31        = fltarr(npos,ntpwranges,nwindranges)
    AnlysTPW        = fltarr(npos,ntpwranges,nwindranges)
    AnlysTskin      = fltarr(npos,ntpwranges,nwindranges)
    AnlysEm23       = fltarr(npos,ntpwranges,nwindranges)
    AnlysEm31       = fltarr(npos,ntpwranges,nwindranges)
    tpwdiff         = fltarr(npos,ntpwranges,nwindranges)
    tpwdiff_cal     = fltarr(npos,ntpwranges,nwindranges)
    tb89diff        = fltarr(npos,ntpwranges,nwindranges)


    ;----Compute scan-dependent and stratified mean biases/stadev/Number of Pts,etc
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2plot[ichan]
        tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
        tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
        TransfIntoScan,tbmeas,tbmeasScan,npos,nscanl,scanPosTot
        TransfIntoScan,tbsimu,tbsimuScan,npos,nscanl,scanPosTot
        tbdiffScan = tbmeasScan-tbsimuScan
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            mintpw=mintpwVec(itpwrange)
            maxtpw=maxtpwVec(itpwrange)
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                minwind=minWindVec(iwindrange)
                maxwind=maxWindVec(iwindrange)
                for ipos=0,npos-1 do begin
                    ;----Filtering based also on retrieved-parameters
                    ;ind=where(sfcTypScan(*,ipos) eq 1  and latScan(*,ipos) ge minLat and $
                    ;          latScan(*,ipos) le maxLat and clw0Scan(*,ipos) le maxclw and $
                    ;          tpwAnlScan(*,ipos) le maxtpw and tpwAnlScan(*,ipos) ge mintpw and $
                    ;          WindAnlScan(*,ipos) le maxwind and WindAnlScan(*,ipos) ge minwind and $
                    ;          tbmeasScan(*,ipos) gt 0 and tbsimuScan(*,ipos) gt 0 and $
                    ;          ChiSqMirsScan(*,ipos) le ChiSqThresh,ncount)
                    ;----Filtering based on non-retrieved parameters
                    ind=where(sfcTypScan(*,ipos) eq 1  and latScan(*,ipos) ge minLat and $
                              latScan(*,ipos) le maxLat and $
                              tpwAnlScan(*,ipos) le maxtpw and tpwAnlScan(*,ipos) ge mintpw and $
                              WindAnlScan(*,ipos) le maxwind and WindAnlScan(*,ipos) ge minwind and $
                              tbsimuScan(*,ipos) gt 0 ,ncount)
                    IF (ncount gt 1) then begin
                        ;---radiometric stratified statistics
                        print,ipos,ichan,mean(tbmeasScan(ind,ipos)),mean(tbsimuScan(ind,ipos)),mean(tbdiffScan(ind,ipos))
                        meanBias(ipos,ichan2plot,itpwrange,iwindrange)   = mean(tbdiffScan(ind,ipos))
                        stdv(ipos,ichan2plot,itpwrange,iwindrange)       = stdev(tbdiffScan(ind,ipos))
                        nPoints(ipos,ichan2plot,itpwrange,iwindrange)    = ncount
                        poly  = poly_fit(tbmeasScan(ind,ipos),tbsimuScan(ind,ipos),1,/double,yfit=yfit,Yerror=yErr)
                        Intercept(ipos,ichan2plot,itpwrange,iwindrange)  = poly(0)
                        Slope(ipos,ichan2plot,itpwrange,iwindrange)      = poly(1)
                        ;---Geophysical stratified statistics
                        msppsCLW(ipos,itpwrange,iwindrange)              = mean(clw0Scan(ind,ipos))
                        msppsTPW(ipos,itpwrange,iwindrange)              = mean(tpw0Scan(ind,ipos))
                        mirsChiSq(ipos,itpwrange,iwindrange)             = mean(ChiSqMirsScan(ind,ipos))
                        mirsCLW(ipos,itpwrange,iwindrange)               = mean(clwMirsScan(ind,ipos))
                        mirsTPW(ipos,itpwrange,iwindrange)               = mean(tpwMirsScan(ind,ipos))
                        mirsTskin(ipos,itpwrange,iwindrange)             = mean(tskinMirsScan(ind,ipos))
                        mirsEm23(ipos,itpwrange,iwindrange)              = mean(em23MirsScan(ind,ipos))
                        mirsEm31(ipos,itpwrange,iwindrange)              = mean(em31MirsScan(ind,ipos))
                        anlysTPW(ipos,itpwrange,iwindrange)              = mean(tpwAnlScan(ind,ipos))
                        anlysTskin(ipos,itpwrange,iwindrange)            = mean(tskinAnlScan(ind,ipos))
                        anlysEm23(ipos,itpwrange,iwindrange)             = mean(em23AnlScan(ind,ipos))
                        anlysEm31(ipos,itpwrange,iwindrange)             = mean(em31AnlScan(ind,ipos))
                        tpwdiff(ipos,itpwrange,iwindrange)               = mean(tpw0Scan(ind,ipos)-tpwAnlScan(ind,ipos))
                        tb89diff(ipos,itpwrange,iwindrange)              = mean(tb89dScan(ind,ipos))
                    ENDIF
                endfor
            ENDFOR
        ENDFOR
    ENDFOR

    ;---Cancel biases of non-trusted comparisons (channels 12,13 and 14)
    ChannelsNot2Correct=[12,13,14]
    FOR ichan=0,n_elements(ChannelsNot2Correct)-1 DO BEGIN
        ichan2Cancel=ChannelsNot2Correct(ichan)-1
        meanBias(*,ichan2Cancel,*,*)   = 0.
        Intercept(*,ichan2Cancel,*,*)  = 0.
        Slope(*,ichan2Cancel,*,*)      = 1.
    ENDFOR

    ;---Fill-in Bias file
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2plot[ichan]
        for ipos=0,npos-1 do begin
            meanBias2Output(ipos,ichan2plot)=mean(meanBias(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
        endfor
    ENDFOR
    WriteBias,22,biasFile,nchan2plot,npos,Rad.cfreq(Channels2plot(0:nchan2plot-1)),$
      meanBias2Output(0:npos-1,Channels2plot(0:nchan2plot-1))


    ;----Compute new MSPPS retrievals using bias-calibrated TBs
    tpw0Scan_cal               = fltarr(mxFovs*nList,npos)
    clw0Scan_cal               = fltarr(mxFovs*nList,npos)
    for ipos=0,npos-1 do begin
        ind=where(sfcTypScan(*,ipos) eq 1  and latScan(*,ipos) ge minLat and $
                  latScan(*,ipos) le maxLat and tb23mScan(*,ipos) gt 0,nprof)
        IF (nprof ge 1) THEN BEGIN
            TL=make_array(nprof,value=280.,/float)
            b23=meanBias(ipos,0,0,0)
            b31=meanBias(ipos,1,0,0)
            computeFromAMSU_TPW_CLW_EMIS,tb23mScan(ind(0:nprof-1),ipos)-b23,tb31mScan(ind(0:nprof-1),ipos)-b31,$
              TskinAnlScan(ind(0:nprof-1),ipos),TL(0:nprof-1),em23AnlScan(ind(0:nprof-1),ipos),$
              em31AnlScan(ind(0:nprof-1),ipos),AngScan(ind(0:nprof-1),ipos),nprof,V,L
            clw0Scan_cal(ind(0:nprof-1),ipos) = L(0:nprof-1)
            tpw0Scan_cal(ind(0:nprof-1),ipos) = V(0:nprof-1)
        ENDIF
    ENDFOR
    ;---Recompute TPW diffs with MSPPS-retrieved using calibrated TBs
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2plot[ichan]
        tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
        tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
        TransfIntoScan,tbmeas,tbmeasScan,npos,nscanl,scanPosTot
        TransfIntoScan,tbsimu,tbsimuScan,npos,nscanl,scanPosTot
        tbdiffScan = tbmeasScan-tbsimuScan
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            mintpw=mintpwVec(itpwrange)
            maxtpw=maxtpwVec(itpwrange)
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                minwind=minWindVec(iwindrange)
                maxwind=maxWindVec(iwindrange)
                for ipos=0,npos-1 do begin
                    b=meanBias(ipos,ichan2plot,0,0)
                    ind=where(sfcTypScan(*,ipos) eq 1  and latScan(*,ipos) ge minLat and $
                              latScan(*,ipos) le maxLat and clw0Scan(*,ipos) le maxclw and $
                              tpwAnlScan(*,ipos) le maxtpw and tpwAnlScan(*,ipos) ge mintpw and $
                              WindAnlScan(*,ipos) le maxwind and WindAnlScan(*,ipos) ge minwind and $
                              tbmeasScan(*,ipos) gt 0 and tbsimuScan(*,ipos) gt 0,ncount)
                    IF (ncount gt 1) then begin
                        tpwdiff_cal(ipos,itpwrange,iwindrange)    = mean(tpw0Scan_cal(ind,ipos)-tpwAnlScan(ind,ipos))
                        meanBias_cal(ipos,ichan2plot,itpwrange,iwindrange)   = mean(tbdiffScan(ind,ipos)-b)
                    ENDIF
                endfor
            ENDFOR
        ENDFOR
    ENDFOR


imp:
    !p.font=1
    !p.charsize   = 1.6
    csize         = 16
    a             = findgen(csize+1) *(!pi*2./float(csize))
    usersym,cos(a)/2,sin(a)/2,/fill
    
    Channels2plot  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]
    nchan2plot     = n_elements(Channels2plot)
    symsize        = 0.9
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
    plot_tbdiffMeanStd_vs_ScanPos_All_bias        = 1
    plot_tbdiffMeanStd_vs_ScanPos_All_bias_aftCal = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_stdv        = 0
    plot_tbdiffMeanStd_vs_ScanPos_All_Npts        = 1
    plot_tbdiffMeanStd_vs_ScanPos_All_Intc        = 1
    plot_tbdiffMeanStd_vs_ScanPos_All_Slop        = 1
    plot_tbdiffMeanStd_vs_ScanPos_All_tb89d       = 1
    ;--------------------------------------------------------------------
    ;---Geophysical plots, maps, diff, etc
    ;--------------------------------------------------------------------
    ;---CLW
    plot_tbdiffMeanStd_vs_ScanPos_All_clw         = 0
    plot_MSPPSclw_map                             = 0 
    plot_clw_mirs_vs_mspps                        = 1
    ;---TPW
    plot_tbdiffMeanStd_vs_ScanPos_All_tpwdiff     = 1
    plot_tpwDiff_analys_mspps_map                 = 1
    plot_tpw_mspps_vs_analys                      = 1
    plot_tbdiffMeanStd_vs_ScanPos_All_tpwdiff_cal = 0
    plot_tpwDiff_analys_mirs                      = 1
    plot_tpwDiff_analys_mirs_map                  = 1
    plot_tpw_mirs_vs_analys                       = 1
    ;---ChiSq
    plot_ChiSq_mirs                               = 1
    plot_ChiSq_mirs_map                           = 1 
    ;---Tskin
    plot_tskin_mirs_vs_analys                     = 1
    plot_tskinDiff_analys_mirs_map                = 1
    plot_tskinDiff_analys_mirs_vsScanPos          = 1
    plot_tskin_analys_map                         = 1
    plot_tskin_mirs_map                           = 1
    ;---Emissivity@31GHz
    plot_em31_mirs_vs_analys                      = 1
    plot_em31Diff_analys_mirs_map                 = 1
    plot_em31Diff_analys_mirs_vsScanPos           = 1
    plot_em31_analys_map                          = 1
    plot_em31_mirs_map                            = 1
    


    IF (plot_em31Diff_analys_mirs_vsScanPos eq 1) THEN BEGIN
        ficres='plot_em31Diff_analys_mirs_vsScanPos.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        erase
        !p.multi=1
        tit=Date2process+' MIRS-ANALYS 31 GHz Emissivity Diff'
        x=mirsEm31-AnlysEm31
        plot,scanPos,x(*,0,0),xtitle='Scan Position',ytitle='31 GHz Emiss MIRS-ANALYS Diff',$
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

    IF (plot_em31_mirs_map eq 1) THEN BEGIN
        ficres='plot_em31_mirs_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = EmissEDR
        x          = reform(x(*,1,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 



        ymin       = min(x(ind_oc))
        ymax       = max(x(ind_oc))
        ymin       = 0.42
        ymax       = 0.52
        tit        = Date2process+' Map distribution of 31 GHz MIRS Emissivity '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,x,'[]',0.6,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF

    IF (plot_em31_analys_map eq 1) THEN BEGIN
        ficres='plot_em31_analys_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = EmissAnalys
        x          = reform(x(*,1,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 

        ymin       = min(x(ind_oc))
        ymax       = max(x(ind_oc))
        ymin       = 0.42
        ymax       = 0.52
        tit        = Date2process+' Map distribution of ANALYS 31 GHz Emissivity '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,x,'[]',0.6,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF

    IF (plot_em31Diff_analys_mirs_map eq 1) THEN BEGIN
        ficres='plot_em31Diff_analys_mirs_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = EmissEDR-EmissAnalys
        x          = reform(x(*,1,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 

        ;ymin       = min(x(ind_oc))
        ;ymax       = max(x(ind_oc))
        ymin       = -0.1
        ymax       = 0.1
        dy         = ymax-ymin
        tit        = Date2process+' Map distribution of MIRS-ANALYS 31 GHz EMISS.Diff '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,x,'[]',0.8,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF


    IF (plot_em31_mirs_vs_analys eq 1) THEN BEGIN
        ficres='plot_em31_mirs_vs_analys.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = reform(EmissAnalys(*,1,*), nList*mxFovs)
        y          = reform(EmissEDR(*,1,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 

        xmin=min([min(x(ind_oc)),min(y(ind_oc))])
        ymin=xmin
        xmax=max([max(x(ind_oc)),max(y(ind_oc))])
        ymax=xmax
        ;PlotScatt,X(ind_oc),Y(ind_oc),'GDAS Analysis 31GHz Emissivity','MIRS 31GHz Emissivity','',1,$
        ;  xmin,xmax,ymin,ymax,1.,1.
        ChoppVec,xmin,xmax,ymin,ymax,X(ind_oc),Y(ind_oc),50,50,'GDAS Analysis 31GHz Emissivity','MIRS 31GHz Emissivity ','',1.2,1,stats
    ENDIF


    IF (plot_tpw_mspps_vs_analys eq 1) THEN BEGIN
        ficres='plot_tpw_mspps_vs_analys.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = reform(TPWAnalys(*,*), nList*mxFovs)
        y          = reform(TPW_MSPPS(*,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 



        xmin=0
        ymin=0
        xmax=80
        ymax=80
        ;PlotScatt,X(ind_oc),Y(ind_oc),'GDAS Anlysis TPW [mm]','MSPPS TPW [mm]','',1,$
        ;  xmin,xmax,ymin,ymax,1.6,1.
        ChoppVec,xmin,xmax,ymin,ymax,X(ind_oc),Y(ind_oc),50,50,'GDAS Analysis TPW [mm]','MSPPS TPW [mm]','',0.8,1,stats
    ENDIF

    IF (plot_tpw_mirs_vs_analys eq 1) THEN BEGIN
        ficres='plot_tpw_mirs_vs_analys.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = reform(TPWAnalys(*,*), nList*mxFovs)
        y          = reform(TPWEDR(*,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 



        xmin=0
        ymin=0
        xmax=80
        ymax=80
        ;PlotScatt,X(ind_oc),Y(ind_oc),'GDAS Anlysis TPW [mm]','MIRS TPW [mm]','',1,$
        ;  xmin,xmax,ymin,ymax,1.6,1.
        ChoppVec,xmin,xmax,ymin,ymax,X(ind_oc),Y(ind_oc),50,50,'GDAS Anlysis TPW [mm]','MIRS TPW [mm]','',0.8,1,stats
    ENDIF

    IF (plot_tskinDiff_analys_mirs_vsScanPos eq 1) THEN BEGIN
        ficres='plot_tskinDiff_analys_mirs_vsScanPos.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
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
        ficres='plot_tskin_mirs_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TSKEDR
        x          = reform(x(*,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 



        ymin       = min(x(ind_oc))
        ymax       = max(x(ind_oc))
        ;ymin       = -20.
        ;ymax       = 20.
        tit        = Date2process+' Map distribution of MIRS TSKIN [K]. '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF


    IF (plot_tskin_analys_map eq 1) THEN BEGIN
        ficres='plot_tskin_analys_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TSKAnalys
        x          = reform(x(*,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 



        ymin       = min(x(ind_oc))
        ymax       = max(x(ind_oc))
        ;ymin       = -20.
        ;ymax       = 20.
        tit        = Date2process+' Map distribution of ANALYS TSKIN [K]. '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF

    IF (plot_tskinDiff_analys_mirs_map eq 1) THEN BEGIN
        ficres='plot_tskinDiff_analys_mirs_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TSKEDR-TSKAnalys
        x          = reform(x(*,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 

        ;ymin       = min(x(ind_oc))
        ;ymax       = max(x(ind_oc))

        ymin       = -20.
        ymax       = 20.
        dy         = ymax-ymin
        tit        = Date2process+' Map distribution of MIRS-ANALYS TSKIN Diff [K]. '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,x,'[Kelvin]',0.6,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF


    IF (plot_clw_mirs_vs_mspps eq 1) THEN BEGIN
        ficres='plot_clw_mirs_vs_mspps.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = reform(CLW_MSPPS(*,*), nList*mxFovs)
        y          = reform(CLWEDR(*,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 


        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 


        xmin=min(x(ind_oc))
        ymin=min(y(ind_oc))
        xmax=max(x(ind_oc))
        ymax=max(y(ind_oc))
        PlotScatt,X(ind_oc),Y(ind_oc),'MSPPS CLW [mm]','MIRS CLW [mm]','',1,$
          xmin,xmax,ymin,ymax,1.,1.
    ENDIF

    IF (plot_tpwDiff_analys_mspps_map eq 1) THEN BEGIN
        ficres='plot_tpwDiff_analys_mspps_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TPW_MSPPS-TPWAnalys
        x          = reform(x(*,*), nList*mxFovs)
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 


        ;ymin       = min(x(ind_oc))
        ;ymax       = max(x(ind_oc))
        ymin       = -10.
        ymax       = 10.
        dy         = ymax-ymin
        tit        = Date2process+' Map distribution of MSPPS-ANALYS TPW Diff [mm]. '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,x,'[mm]',0.6,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF

    IF (plot_tpwDiff_analys_mirs_map eq 1) THEN BEGIN
        ficres='plot_tpwDiff_analys_mirs_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        x          = TPWEDR-TPWAnalys
        x          = reform(x(*,*), nList*mxFovs)

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 


        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh ,ncount_oc) 
        ;ymin       = min(x(ind_oc))
        ;ymax       = max(x(ind_oc))
        ymin       = -10.
        ymax       = 10.
        dy         = ymax-ymin
        tit        = Date2process+' Map distribution of MIRS-ANALYS TPW Diff [mm]. '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,x,'[mm]',0.6,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF


    IF (plot_ChiSq_mirs_map eq 1) THEN BEGIN
        ficres='plot_ChiSq_mirs_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)

        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 

        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat ,ncount_oc) 
        ymin       = min(chisq0(ind_oc))
        ;ymax       = max(chisq0(ind_oc))
        ymax       = 5.
        dy         = ymax-ymin
        tit        = Date2process+' Map distribution of MIRS-ChiSq []. '
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,chisq0,'[]',0.6,8,1,0,'(f7.3)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF



    IF (plot_ChiSq_mirs eq 1) THEN BEGIN
        ficres='plot_ChiSq_mirs.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
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

    IF (plot_tpwDiff_analys_mirs eq 1) THEN BEGIN
        ficres='plot_tpwDiff_analys_mirs.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        erase
        !p.multi=1
        tit=Date2process+' TPW DIFFERENCE (MIRS-ANALYSIS)'
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

    IF (plot_tbdiffMeanStd_vs_ScanPos_All_bias eq 1) THEN BEGIN
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_bias.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ;---All means together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Bias
        FOR ichan=0,nchan2plot-1 DO BEGIN
            ichan2plot = Channels2plot[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')
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


    IF (plot_tbdiffMeanStd_vs_ScanPos_All_bias_aftCal eq 1) THEN BEGIN
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_bias_aftCal.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ;---All means together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Bias
        FOR ichan=0,nchan2plot-1 DO BEGIN
            ichan2plot = Channels2plot[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')
            plot,scanPos,meanBias_cal(*,ichan2plot,0,0),xtitle='Scan Position',ytitle='Bias [meas-simu] [K]',$
              title=tit,yrange=[min(meanBias_cal(*,ichan2plot,*,*)),max(meanBias_cal(*,ichan2plot,*,*))]
            ipl=-1
            FOR itpwrange=0,ntpwranges-1 DO BEGIN
                FOR iWindrange=0,nwindranges-1 DO BEGIN
                    ipl=ipl+1
                    oplot,scanPos,meanBias_cal(*,ichan2plot,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
                ENDFOR
            ENDFOR
            plots,[0,30],[0,0],color=240
            plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
        ENDFOR
    ENDIF

    IF (plot_tbdiffMeanStd_vs_ScanPos_All_stdv eq 1) THEN BEGIN
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_stdv.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ;---All stdv together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Stdev
        FOR ichan=0,nchan2plot-1 DO BEGIN
            ichan2plot = Channels2plot[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')
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
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_Npts.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
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
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_Intc.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ;---All intercepts together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Stdev
        FOR ichan=0,nchan2plot-1 DO BEGIN
            ichan2plot = Channels2plot[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')
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
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_Slop.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ;---All Slopes together
        erase
        !p.multi=[20,4,5,0,0]
        ;---Stdev
        FOR ichan=0,nchan2plot-1 DO BEGIN
            ichan2plot = Channels2plot[ichan]
            tit=Date2process+' Ch#'+string(ichan2plot+1,'(i2)')+' Fr:'+string(Rad.cfreq(ichan2plot),'(f5.1)')
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

    IF (plot_tbdiffMeanStd_vs_ScanPos_All_clw eq 1) THEN BEGIN
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_clw.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ;---All clw together
        erase
        !p.multi=1
        ;---Stdev
        tit=Date2process+' MSPPS-based CLW [mm]'
        plot,scanPos,msppsclw(*,0,0),xtitle='Scan Position',ytitle='MSPPS-based CLW [mm]',$
          title=tit,yrange=[min(msppsclw(*,*,*)),max(msppsclw(*,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,msppsclw(*,itpwrange,iwindrange),linestyle=linesty[ipl],$
                  psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
    ENDIF

    IF (plot_tbdiffMeanStd_vs_ScanPos_All_tpwdiff eq 1) THEN BEGIN
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_tpwdiff.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        erase
        !p.multi=1
        ;---Stdev
        tit=Date2process+' TPW DIFFERENCE (MSPPS-ANALYSIS)'
        plot,scanPos,tpwdiff(*,0,0),xtitle='Scan Position',ytitle='TPW Diff MSPPS-ANAL [mm]',$
          title=tit,yrange=[min(tpwdiff(*,*,*)),max(tpwdiff(*,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,tpwdiff(*,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
    ENDIF

    IF (plot_tbdiffMeanStd_vs_ScanPos_All_tpwdiff_cal eq 1) THEN BEGIN
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_tpwdiff_cal.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ;---All clw together
        erase
        !p.multi=1
        ;---Stdev
        tit=Date2process+' TPW DIFFERENCE (MSPPS(w CAL-TB) -ANALYSIS)'
        plot,scanPos,tpwdiff_cal(*,0,0),xtitle='Scan Position',ytitle='TPW Diff CAL/MSPPS-ANAL [mm]',$
          title=tit,yrange=[min(tpwdiff_cal(*,*,*)),max(tpwdiff_cal(*,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,tpwdiff_cal(*,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
    ENDIF

    IF (plot_tbdiffMeanStd_vs_ScanPos_All_tb89d eq 1) THEN BEGIN
        ficres='plot_tbdiffMeanStd_vs_ScanPos_All_tb89d.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        erase
        !p.multi=1
        tit=Date2process+' TB Difference @ 89 GHz (AMSUA-MHS)'
        plot,scanPos,tb89diff(*,0,0,0),xtitle='Scan Position',ytitle='TB Difference [K]',$
          title=tit,yrange=[min(tb89diff(*,*,*)),max(tb89diff(*,*,*))]
        ipl=-1
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                ipl=ipl+1
                oplot,scanPos,tb89diff(*,itpwrange,iwindrange),linestyle=linesty[ipl],$
                      psym=-psm[ipl],symsize=0.5
            ENDFOR
        ENDFOR
        plots,[0,30],[0,0],color=240
        plot_legend,2,ntpwranges*nwindranges,linesty,psm,col,comm,0.6
    ENDIF

    IF (plot_tbdiff_vs_ScanPos eq 1) THEN BEGIN
        ficres='plot_tbdiff_vs_ScanPos.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi=[20,4,5,0,0]
        npos=30
        scanPos=findgen(npos)+1
        FOR ichan2plot=0,Rad.nchan-1 DO BEGIN
            tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)

            clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

            ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat,ncount_oc) 

            ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 


            ind_ld     = where(sfctyp ne 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat,ncount_ld)
            ymin       = min(tbmeas(ind_oc)-tbsimu(ind_oc))
            ymax       = max(tbmeas(ind_oc)-tbsimu(ind_oc))
            TransfIntoScan,tbmeas,tbmeasScan,npos,nscanl,scanPosTot
            TransfIntoScan,tbsimu,tbsimuScan,npos,nscanl,scanPosTot
            TransfIntoScan,lat,latScan,npos,nscanl,scanPosTot
            TransfIntoScan,lon,lonScan,npos,nscanl,scanPosTot
            TransfIntoScan,sfcTyp,sfcTypScan,npos,nscanl,scanPosTot
            tit=Date2process+' Channel#'+string(ichan2plot+1,'(i2)')+' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')
            iplotted=0
            FOR iscan=0,nscanl-1 DO BEGIN
                ind=where(sfcTypScan(iscan,*) eq 1 and latScan(iscan,*) ge minLat and latScan(iscan,*) le maxLat,ncount)
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
        ENDFOR
    ENDIF

    IF (plot_tb_scatterplt eq 1) THEN BEGIN
        ficres='plot_tb_scatterplt.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi=[20,4,5,0,0]
        FOR ichan2plot=0,Rad.nchan-1 DO BEGIN
            tbmeas=reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu=reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
            clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

            ;ind_oc=where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat,ncount_oc) 
            ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 
            ind_ld=where(sfctyp ne 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat,ncount_ld)
            ymin=min([min(tbmeas(ind_oc)),min(tbsimu(ind_oc))])
            ymax=max([max(tbmeas(ind_oc)),max(tbsimu(ind_oc))])
            dy=ymax-ymin
            tit=Date2process+' Channel#'+string(ichan2plot+1,'(i2)')+' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')

            ChoppVec,ymin,ymax,ymin,ymax,tbmeas(ind_oc),tbsimu(ind_oc),20,20,'TB Measurements [K]',$
              'TB Simulations [K]',tit,0.6,1,stats

            ;plot,tbmeas(ind_oc),tbsimu(ind_oc),psym=1,xtitle='TB Measurements [K]',$
            ;  ytitle='TB Simulations [K]',title=tit,xrange=[ymin,ymax],yrange=[ymin,ymax],$
            ;  xstyle=1,ystyle=1
            ;;oplot,tbmeas(ind_ld),tbsimu(ind_ld),psym=2,color=220
            ;plots,[ymin,ymax],[ymin,ymax],color=240
            ;diffmean = mean(tbmeas(ind_oc)-tbsimu(ind_oc))
            ;diffstdv = stdev(tbmeas(ind_oc)-tbsimu(ind_oc))
            ;diffrms  = sqrt(mean((tbmeas(ind_oc)-tbsimu(ind_oc))^2))
            ;xyouts, ymin+dy/10.,ymax-1*dy/10.,'Bias:'+string(diffmean,'(f6.2)'),charsize=symsize*0.6
            ;xyouts, ymin+dy/10.,ymax-2*dy/10.,'Stdv:'+string(diffstdv,'(f6.2)'),charsize=symsize*0.6
            ;xyouts, ymin+dy/10.,ymax-3*dy/10.,'RMS:' +string(diffrms,'(f6.2)'),charsize=symsize*0.6
            ;xyouts, ymin+dy/10.,ymax-4*dy/10.,'Npts:'+string(ncount_oc,'(i6)'),charsize=symsize*0.6
        ENDFOR
    ENDIF

    IF (plot_tb_map_simul eq 1) THEN BEGIN
        ichannels2plot=[0,1,2,3,4,5,14,15,16,17,18,19]
        FOR ichan=0,n_elements(ichannels2plot)-1 DO BEGIN
            ficres='plot_tb_map_simul'+string(ichannels2plot[ichan]+1,'(i2.2)')+'.ps'
            print,' PostScript File created:',ficres
            device,filename=ficres,/color,/landscape
            ERASE
            !p.multi   = 1
            ichan2plot = ichannels2plot[ichan]
            tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
            clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
            ;ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat $
            ;                   and clw0 le maxclw ,ncount_oc) 
            ind_ld     = where(sfctyp ne 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat,ncount_ld)
            ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 
            ymin       = min([min(tbmeas(ind_oc)),min(tbsimu(ind_oc))])
            ymax       = max([max(tbmeas(ind_oc)),max(tbsimu(ind_oc))])
            dy         = ymax-ymin
            tit        = Date2process+' TB-simulations @ Channel#'+string(ichan2plot+1,'(i2)')+' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+'GHz'
            mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
              ymin,ymax,tbsimu,'[K]',1.,8,1,0,'(f6.1)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
        ENDFOR
    ENDIF

    IF (plot_tb_map_meas eq 1) THEN BEGIN
        ichannels2plot=[0,1,2,3,4,5,14,15,16,17,18,19]
        FOR ichan=0,n_elements(ichannels2plot)-1 DO BEGIN
            ficres='plot_tb_map_meas'+string(ichannels2plot[ichan]+1,'(i2.2)')+'.ps'
            print,' PostScript File created:',ficres
            device,filename=ficres,/color,/landscape
            ERASE
            !p.multi   = 1
            ichan2plot = ichannels2plot[ichan]
            tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
            clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
            ;ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat $
            ;                   and clw0 le maxclw,ncount_oc) 
            ind_ld     = where(sfctyp ne 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat,ncount_ld)
            ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 
            ymin       = min([min(tbmeas(ind_oc)),min(tbsimu(ind_oc))])
            ymax       = max([max(tbmeas(ind_oc)),max(tbsimu(ind_oc))])
            dy         = ymax-ymin
            tit        = Date2process+' TB-measurements @ Channel#'+string(ichan2plot+1,'(i2)')+' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+'GHz'
            mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
              ymin,ymax,tbmeas,'[K]',1.,8,1,0,'(f6.1)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
        ENDFOR
    ENDIF

    IF (plot_tbdiff_map eq 1) THEN BEGIN
        ichannels2plot=[0,1,2,3,4,5,14,15,16,17,18,19]
        FOR ichan=0,n_elements(ichannels2plot)-1 DO BEGIN
            ficres='plot_tbdiff_map'+string(ichannels2plot[ichan]+1,'(i2.2)')+'.ps'
            print,' PostScript File created:',ficres
            device,filename=ficres,/color,/landscape
            ERASE
            !p.multi   = 1
            ichan2plot = ichannels2plot[ichan]
            tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
            tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
            lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
            lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
            sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
            clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
            chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
            qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
            qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)

            ;ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat $
            ;                   and clw0 le maxclw,ncount_oc) 
            ind_ld     = where(sfctyp ne 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat,ncount_ld)
            ind_oc     = where(sfctyp eq 1 and tbmeas gt 0 and tbsimu gt 0 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                               and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 
            ymin       = min(tbmeas(ind_oc)-tbsimu(ind_oc))
            ymax       = max(tbmeas(ind_oc)-tbsimu(ind_oc))
            ymin       = -10
            ymax       = 10.
            dy         = ymax-ymin
            tit        = Date2process+' TB Differences [Meas-Simul] @ Channel#'+string(ichan2plot+1,'(i2)')+$
                         ' Freq:'+string(Rad.cfreq(ichan2plot),'(f5.1)')+'GHz'
            mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
              ymin,ymax,tbmeas-tbsimu,'[K]',1.,8,1,0,'(f6.1)'
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
        ENDFOR
    ENDIF

    IF (plot_MSPPSclw_map eq 1) THEN BEGIN
        ficres='plot_MSPPSclw_map.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        clw0       = reform(CLW_mspps(*,*), nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw $
                           and chisq0 le chisqThresh and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 
        ;ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and clw0 le maxclw,ncount_oc) 
        ymin       = min(clw0(ind_oc))
        ymax       = max(clw0(ind_oc))
        dy         = ymax-ymin
        tit        = Date2process+' Map distribution of MSPPS-CLW [mm]. Threshold Used:'+string(maxclw,'(f7.4)')
        mapPlot,min(lat(ind_oc)),max(lat(ind_oc)),min(lon(ind_oc)),max(lon(ind_oc)),lat,lon,ind_oc,tit,$
          ymin,ymax,clw0,'[mm]',0.6,8,1,0,'(f7.4)'
        map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
    ENDIF

    IF (plot_tskin_mirs_vs_analys eq 1) THEN BEGIN
        ficres='plot_tskin_mirs_vs_analys.ps'
        print,' PostScript File created:',ficres
        device,filename=ficres,/color,/landscape
        ERASE
        !p.multi   = 1
        lat        = reform(LatTabRad(*,*,0),nList*mxFovs)
        lon        = reform(LonTabRad(*,*,0),nList*mxFovs)
        sfctyp     = reform(SfcTypAnalys(*,*),nList*mxFovs)
        chisq0     = reform(ChiSqEDR(*,*), nList*mxFovs)
        qcatm      = reform(qcAtmEDR(*,*), nList*mxFovs)
        qcsfc      = reform(qcSfcEDR(*,*), nList*mxFovs)
        x          = reform(TskAnalys(*,*), nList*mxFovs)
        y          = reform(TSKEDR(*,*), nList*mxFovs)
        ind_oc     = where(sfctyp eq 1 and lat ge minLat and lat le maxLat and chisq0 le chisqThresh $
                           and chisq0 ge 0. and qcatm eq 0 and qcsfc eq 0,ncount_oc) 
        xmin=270
        ymin=270
        xmax=310
        ymax=310
        PlotScatt,X(ind_oc),Y(ind_oc),'GDAS Anlysis Tskin [K]','MIRS Tskin [K]','',1,$
          xmin,xmax,ymin,ymax,1.,1.
    ENDIF

    ;IF (impr eq 1) THEN begin
    ;    device,/close
    ;    device ='x'
    ;    GOTO, bo
    ;ENDIF
    ;Print,'________________________________________________'
    ;boucle:print,'PS/Exit(1/0)'
    ;read,impr
    ;IF (impr gt 3) THEN GOTO, boucle
    ;IF (impr eq 0) THEN GOTO, bo
ip: ;IF (impr eq 1) THEN BEGIN
    ;    ficres='visu.ps'
    ;    print,' PostScript File created:',ficres
    ;    set_plot,'ps'
    ;    ;device,filename=ficres,/color,ysize=23,xsize=16,yoffset=2,/portrait
    ;    device,filename=ficres,/color,/landscape
    ;    goto,imp
    ;ENDIF
    ;bo: print,'________________________________________________'
    ;impr=0
    ;devicd = 'x'
    ;set_plot,'x'

    device,/close
    fin:close,/all
    print,'End of IDL processing...(radiom/geoph biases determination, monitoring)'

END

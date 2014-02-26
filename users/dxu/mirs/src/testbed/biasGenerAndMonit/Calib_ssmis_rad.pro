@../../../../setup/paths_idl.pro

;------------------------------------------------------------------
;
; Program used to assess the calibration of microwave radiances
; by comparing them to simulations based on analyses (GDAS).
;
; Sid-Ahmed Boukabara. September, 2005.
;
; Extended to perform the monitoring of:
;  - bias residuals
;
;------------------------------------------------------------------

;----Uncomment the following is used as subroutine
PRO Calib_ssmis_rad,ListAnalys=ListAnalysName,listRadSimul=listRadSimulName,listRadMeas=listRadMeasName,$
              biasfile=biasFile,ErrFile=ErrFile
set_plot,'ps'
;------------------------------


;----UNcomment the following if used as main program
;device='x'
;device,true=24,retain=2,decompose=0
;device,pseudo_color=8,retain=2
;device,get_visual_name=thisvisualClass
;loadct,39
;if (thisvisualClass ne 'pseudoColor') then device,decomposed=0
;close,/all
;ListAnalysName='NWPanalys_n18.list'
;listRadSimulName='FWDanalys_n18.list'
;listRadMeasName='RadFiles.dat'
;biasfile='biasCorrec_n18.dat'
;ErrFile='ErrModelingErr_n18.dat'
;device='x'
;set_plot,'x'
;PRINT, 'Do you want to read again the data (1/0)?'
;read,answer
;IF (answer eq 0) then GOTO, sels
;------------------------------------------------


loadct,39


;------------------------------------------------------------------------------
;  Reading radiances and scene data (analyses)
;------------------------------------------------------------------------------
readlist,listRadMeasName,listRadMeas,nfilesRadMeas
nListRadMeas = n_elements(listRadMeas)

;---Determine the date to process from the name of 1 file
x=listRadMeas[0]
pos1=strpos(x,'/',/reverse_search)
pos2=strpos(x,'/',pos1-1,/reverse_search)
date2process=strmid(x,pos2,(pos1-pos2))

readlist,listRadSimulName,listRadSimul,nfilesRadSimul
nListRadSimul = n_elements(listRadSimul)
IF (nListRadMeas ne nListRadSimul) THEN BEGIN
    print,'Warning: # files in simulation/measurements lists not identical',nListRadSimul,nListRadMeas
    ;STOP
    minnList=min([nListRadSimul,nListRadMeas])
    nListRadSimul=minnList
    nListRadMeas=minnList
ENDIF
;----Set input filename (for analyses scenes)
readlist,listAnalysName,listAnalys,nfilesAnalys
nListAnalys = n_elements(listAnalys)
IF (nListRadMeas ne nListAnalys) THEN BEGIN
    print,'Warning: #files in analyses/Rad-measurements lists not identical',nListAnalys,nListRadMeas
    ;STOP
    minnList=min([nListAnalys,nListRadMeas])
    nListAnalys=minnList
    nListRadMeas=minnList
ENDIF

;---test
nList                   = nListRadMeas
;nList                   = 1
;-------

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
    minLat  = -60
    maxLat  = 60
    minlon  = -180
    maxlon  = 180

    node2plot      = 2                  ;0/1/2 for asc/des/both
    maxclw         = 0.005             ; maximum value of clw allowed in computing biases
    ChiSqThresh    = 10.

    ;---ranges of stratification 
    ;maxtpwVec      = [20.,100.]    ; maximum value of tpw allowed in computing biases
    ;mintpwVec      = [0., 20.]     ; minimum value of tpw allowed in computing biases
    ;maxwindVec     = [5.,100.]     ; maximum value of wind allowed in computing biases
    ;minwindVec     = [0.,5.  ]       ; minimum value of wind allowed in computing biases
    maxtpwVec      = [100.]    ; maximum value of tpw allowed in computing biases
    mintpwVec      = [0.]     ; minimum value of tpw allowed in computing biases
    maxwindVec     = [100.]     ; maximum value of wind allowed in computing biases
    minwindVec     = [0.]       ; minimum value of wind allowed in computing biases

    ;---channels to process for bias computation
    Channels2plot  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
    ;---Scaling factors used to adjust the stdev (considered the modeling
    ;error). These factors are a bit subjective but do reflect uncertainty
    ;in the GDAS variables for which  a particular channel is most
    ;sensitive. For example, if a channel is sensitive to cloud and to surface
    ;(more unknown than temperature for instance) than the the scaling factor
    ;should be low, close to 0. If the channel is dependent on mostly the temperature profile
    ;(which is relatively well modeled by GDAS) then the scaling factor should
    ;be almost 1. These factors also reflect errors in the collocation.
    scalFact       = [0.5,0.5,0.5,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.5,0.5,0.5,0.5,0.5,0.5]

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

    ;----Declare stratified variables needed (biases, std )
    meanBias         = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    meanBias_cal     = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    stdv             = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    nPoints          = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    Intercept        = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    Slope            = fltarr(npos,Rad.nchan,ntpwranges,nwindranges)
    msppsCLW         = fltarr(npos,ntpwranges,nwindranges)
    msppsTPW         = fltarr(npos,ntpwranges,nwindranges)
    AnlysTPW         = fltarr(npos,ntpwranges,nwindranges)
    AnlysTskin       = fltarr(npos,ntpwranges,nwindranges)
    AnlysEm23        = fltarr(npos,ntpwranges,nwindranges)
    AnlysEm31        = fltarr(npos,ntpwranges,nwindranges)
    tpwdiff          = fltarr(npos,ntpwranges,nwindranges)
    tpwdiff_cal      = fltarr(npos,ntpwranges,nwindranges)
    tb89diff         = fltarr(npos,ntpwranges,nwindranges)
    meanBias2Output  = fltarr(npos,Rad.nchan)
    Slope2Output     = fltarr(npos,Rad.nchan)
    Intercept2Output = fltarr(npos,Rad.nchan)
    stdv2Output      = fltarr(Rad.nchan)

    maxTbDiffallowed = 10

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
                    ;----Filtering based on non-retrieved parameters
                    ind=where(sfcTypScan(*,ipos) eq 1  and latScan(*,ipos) ge minLat and $
                              latScan(*,ipos) le maxLat and $
                              tpwAnlScan(*,ipos) le maxtpw and tpwAnlScan(*,ipos) ge mintpw and $
                              WindAnlScan(*,ipos) le maxwind and WindAnlScan(*,ipos) ge minwind and $
                              tbsimuScan(*,ipos) gt 0 and abs(tbdiffScan(*,ipos)-mean(tbdiffScan(*,ipos))) le maxTbDiffallowed,ncount)
                    IF (ncount gt 1) then begin
                        ;---radiometric stratified statistics
                        meanBias(ipos,ichan2plot,itpwrange,iwindrange)   = mean(tbdiffScan(ind,ipos))
                        stdv(ipos,ichan2plot,itpwrange,iwindrange)       = stdev(tbdiffScan(ind,ipos))
                        nPoints(ipos,ichan2plot,itpwrange,iwindrange)    = ncount
                        poly  = poly_fit(tbmeasScan(ind,ipos),tbsimuScan(ind,ipos),1,/double,yfit=yfit,Yerror=yErr)
                        Intercept(ipos,ichan2plot,itpwrange,iwindrange)  = poly(0)
                        Slope(ipos,ichan2plot,itpwrange,iwindrange)      = poly(1)
                        ;---Geophysical stratified statistics
                        msppsCLW(ipos,itpwrange,iwindrange)              = mean(clw0Scan(ind,ipos))
                        msppsTPW(ipos,itpwrange,iwindrange)              = mean(tpw0Scan(ind,ipos))
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
        stdv(*,ichan2Cancel,*,*)       = 0.
        Intercept(*,ichan2Cancel,*,*)  = 0.
        Slope(*,ichan2Cancel,*,*)      = 1.
    ENDFOR

    ;---Fill-in Bias/Std file(s)
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2plot[ichan]
        for ipos=0,npos-1 do begin
            meanBias2Output(ipos,ichan2plot)  = mean(meanBias(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            Slope2Output(ipos,ichan2plot)     = mean(Slope(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
            Intercept2Output(ipos,ichan2plot) = mean(Intercept(ipos,ichan2plot,0:ntpwranges-1,0:nwindranges-1))
        endfor
        stdv2Output(ichan2plot)     = mean(stdv(0:npos-1,ichan2plot,0:ntpwranges-1,0:nwindranges-1))*scalFact(ichan2plot)
    ENDFOR
    WriteBias,22,biasFile,nchan2plot,npos,Rad.cfreq(Channels2plot(0:nchan2plot-1)), $
      meanBias2Output(0:npos-1,Channels2plot(0:nchan2plot-1)),                      $
      Slope2Output(0:npos-1,Channels2plot(0:nchan2plot-1)),                         $
      Intercept2Output(0:npos-1,Channels2plot(0:nchan2plot-1))
    writeErr,ErrFile,Rad.cfreq(Channels2plot(0:nchan2plot-1)),stdv2Output(Channels2plot(0:nchan2plot-1)),nchan2plot

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
                              tbmeasScan(*,ipos) gt 0 and tbsimuScan(*,ipos) gt 0 ,ncount)
                    IF (ncount gt 1) then begin
                        tpwdiff_cal(ipos,itpwrange,iwindrange)    = mean(tpw0Scan_cal(ind,ipos)-tpwAnlScan(ind,ipos))
                        meanBias_cal(ipos,ichan2plot,itpwrange,iwindrange)   = mean(tbdiffScan(ind,ipos)-b)
                    ENDIF
                endfor
            ENDFOR
        ENDFOR
    ENDFOR

imp:

    ;---use this if used as subroutine
    ;device,/close
    ;fin:close,/all
    print,'End of IDL processing...(radiom biases determination)'

;---plotting of results of biases
    !p.font=1
    Channels2plot  = [0,1,2,3,5,14,16,18]
    ScanPos2plot   = [0,5,10,15,20,25,29]
    nchan2plot     = n_elements(Channels2plot)
    FOR ichan=0,nchan2plot-1 DO BEGIN
        ichan2plot = Channels2plot[ichan]
        tbmeas     = reform(tbTabRad(*,*,ichan2plot,0),nList*mxFovs)
        tbsimu     = reform(tbTabRad(*,*,ichan2plot,1),nList*mxFovs)
        TransfIntoScan,tbmeas,tbmeasScan,npos,nscanl,scanPosTot
        TransfIntoScan,tbsimu,tbsimuScan,npos,nscanl,scanPosTot
        tbdiffScan = tbmeasScan-tbsimuScan
        erase
        !p.multi=[8,2,4,0,0]
        FOR itpwrange=0,ntpwranges-1 DO BEGIN
            mintpw=mintpwVec(itpwrange)
            maxtpw=maxtpwVec(itpwrange)
            FOR iWindrange=0,nwindranges-1 DO BEGIN
                minwind=minWindVec(iwindrange)
                maxwind=maxWindVec(iwindrange)
                for ipos0=0,n_elements(ScanPos2plot)-1 do begin
                    ipos=ScanPos2plot[iPos0]
                    ;----Filtering based on non-retrieved parameters
                    ind=where(sfcTypScan(*,ipos) eq 1  and latScan(*,ipos) ge minLat and $
                              latScan(*,ipos) le maxLat and $
                              tpwAnlScan(*,ipos) le maxtpw and tpwAnlScan(*,ipos) ge mintpw and $
                              WindAnlScan(*,ipos) le maxwind and WindAnlScan(*,ipos) ge minwind and $
                              tbsimuScan(*,ipos) gt 0 and abs(tbdiffScan(*,ipos)-mean(tbdiffScan(*,ipos))) le maxTbDiffallowed ,ncount)
                    IF (ncount gt 1) then begin
                        ;---original measurements
                        ;X=tbmeasScan(ind,ipos)
                        ;---Bias-removed measurements
                        ;X=tbmeasScan(ind,ipos)-meanBias(ipos,ichan2plot,itpwrange,iwindrange)
                        ;---Bias-and-Slope-removed measurements
                        X=tbmeasScan(ind,ipos)*Slope(ipos,ichan2plot,itpwrange,iwindrange)+Intercept(ipos,ichan2plot,itpwrange,iwindrange)

                        Y=tbsimuScan(ind,ipos)
                        tit='Scan Pos='+string(ipos+1,'(i2)')+' Chan#'+string(ichan2plot+1,'(i2)')+' Frq:'+string(Rad.cfreq(ichan2plot),'(f6.2)')+$
                          'GHz  Mean Bias:'+string(meanBias(ipos,ichan2plot,itpwrange,iwindrange),'(f7.2)')
                        ChoppVec,min([min(X),min(Y)]),max([max(X),max(Y)]),min([min(X),min(Y)]),max([max(X),max(Y)]),X,Y,20,20,'Measured TB [K]',$
                          'Simulated TB [K]',tit,0.6,1,stats
                    ENDIF
                endfor
            ENDFOR
        ENDFOR
    ENDFOR


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
        ficres='visu.ps'
        print,' PostScript File created:',ficres
        set_plot,'ps'
        device,filename=ficres,/color,ysize=23,xsize=16,yoffset=2,/portrait
        ;device,filename=ficres,/color,/landscape
        goto,imp
    ENDIF
    bo: print,'________________________________________________'
    impr=0
    devicd = 'x'
    set_plot,'x'
    fin:close,/all
    print,'End of procesing...'



END

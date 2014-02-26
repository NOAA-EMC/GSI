@../../../setup/paths_idl.pro
;*******************************************************************************
;
; Name:  nongridMirs_edr
;
; Type:  IDL Program
;
;
; Description:
;   Program used to read in the contents of edr (or other MiRS scene
;   files) profile-by-profile, to store in arrays and plot various
;   preselected maps.
;
; Requirements:
;   MIRS IDL library
;
; History:
;   Kevin Garrett 2008-10-07  original coder
;   Wanchun Chen  2010-04-08  adjust fov in cross & along track direction
;
;*******************************************************************************

PRO NONGRIDMIRS_EDR, nameList=namelist
Consts,Consts

;---- Set Input ----
satId='trmm'
figsDir='./'
date='2011-06-26'
version='2653'

latmin=17
latmax=32
lonmin=-98
lonmax=-68
region=3

if satId eq 'metopA' then begin
  latmin=20
  latmax=50
  lonmin=-160
  lonmax=-115
  region=4
endif

if satId eq 'metopB' then begin
  latmin=20
  latmax=50
  lonmin=-160
  lonmax=-115
  region=4
endif

if satId eq 'f16' then begin
  latmin=5
  latmax=35
  lonmin=110
  lonmax=130
  region=5
endif

if satId eq 'f17' then begin
  latmin=5
  latmax=35
  lonmin=110
  lonmax=130
  region=5
endif

if satId eq 'f18' then begin
  latmin=5
  latmax=35
  lonmin=110
  lonmax=130
  region=5
endif

if satId eq 'aqua' then begin
  latmin=20
  latmax=55
  lonmin=-130
  lonmax=-60
  region=6
endif

if satId eq 'trmm' or satId eq 'gpm' or satId eq 'trmm2a12' then begin
  latmin=20
  latmax=40
  lonmin=-110
  lonmax=-70
  region=7
endif

if satId eq 'npp' then begin
  latmin=-30
  latmax=30
  lonmin=-90
  lonmax=-30
  region=0
endif


fileList='/disk1/pub/mirs_hr/data/InputsData/trmm_edrFiles_2011-06-26.list'
resolution=1 ;  0 - denote low resolution,  1 - denote high resolution

readFromList=1
if ( readFromList eq 1 ) then begin
    openr,iu,namelist,/get_lun 
    readf,iu,format='(a)', satId 		;Satellite ID
    readf,iu,format='(a)', figsDir 		;path where to put image files
    readf,iu,format='(a)', date 		;file extension of image
    readf,iu,format='(a)', version 		;version number
    readf,iu,format='(f)', latmin 		;min lat
    readf,iu,format='(f)', latmax 		;max lat
    readf,iu,format='(f)', lonmin 		;min lon
    readf,iu,format='(f)', lonmax 		;max lon
    readf,iu,format='(a)', fileList 		;file with list of files to read
    readf,iu,format='(I)', region 		;region flag for plotting
    readf,iu,format='(f)', resolution    	;resolution flag of data
    close,iu
    free_lun,iu,/force
endif

;---- get yyyymmdd from yyyy-mm-dd ----
yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

;---- check if orbit mode or not ----
pos = STRPOS(date,'.D')
if pos ge 0 then yyyymmdd=date

;---- resolution identifier ----
resoStr=''
if(resolution eq 0 ) then resoStr=''
if(resolution ge 1 ) then resoStr='hr_'


;---- region identifier ----
regionStr='roi_'
;---- Region Of Interest ----
if (region eq -1) then regionStr='roi_'

if (region eq 0) then regionStr='glb_'
;---United States
if (region eq 1) then regionStr='us_'
;----Europe
if (region eq 2) then regionStr='eu_'
;----Gulf of Mexico
if (region eq 3) then regionStr='gulf_'
;----West Coast
if (region eq 4) then regionStr='westcoast_'
;----East China
if (region eq 5) then regionStr='china_'
;----Conus
if (region eq 6) then regionStr='conus_'
;----Tropical
if (region eq 7) then regionStr='trop_'

;---- image name prefix ----
prefix=''
if satId eq 'n18'      then prefix='mirs_adv_poes_'+satId+'_amsuamhs_'+regionStr
if satId eq 'metopA'   then prefix='mirs_adv_poes_'+satId+'_amsuamhs_'+regionStr
if satId eq 'metopB'   then prefix='mirs_adv_poes_'+satId+'_amsuamhs_'+regionStr
if satId eq 'n19'      then prefix='mirs_adv_poes_'+satId+'_amsuamhs_'+regionStr
if satId eq 'f16'      then prefix='mirs_adv_dmsp_'+satId+'_ssmis_'   +regionStr
if satId eq 'f17'      then prefix='mirs_adv_dmsp_'+satId+'_ssmis_'   +regionStr
if satId eq 'f18'      then prefix='mirs_adv_dmsp_'+satId+'_ssmis_'   +regionStr
if satId eq 'aqua'     then prefix='mirs_adv_eos_' +satId+'_amsre_'   +regionStr
if satId eq 'gcomw1'   then prefix='mirs_adv_eos_' +satId+'_amsr2_'   +regionStr
if satId eq 'fy3ri'    then prefix='mirs_adv_eos_' +satId+'_mwri_'    +regionStr
if satId eq 'trmm'     then prefix='mirs_adv_eos_' +satId+'_tmi_'     +regionStr
if satId eq 'gpm'      then prefix='mirs_adv_eos_' +satId+'_gmi_'     +regionStr
if satId eq 'npp'      then prefix='mirs_adv_npoess_'+satId+'_atms_'  +regionStr
if satId eq 'trmm2a12' then prefix='mirs_adv_eos_trmm_2A12_'          +regionStr


;---- Read list of scene files ----
readlist,fileList,list,nfile
if (nfile eq 0 ) then begin
    print, 'Error: No files found in :',fileList
    stop
endif

nList = n_elements(list)
nFovsTot = lonarr(nList)
;---- Open file headers to count number of profiles for max allocation ----
mxFovs=0L
FOR ifile=0L,nList-1 DO BEGIN
  OPENR,iu,list(ifile),/get_lun,error=err,/f77_unformatted,/swap_if_little_endian
  IF (err ne 0) THEN BEGIN
      print ,!ERROR_STATE.MSG
      RETURN
  ENDIF
  iTyp = 0L & nPrf = 0L & AlgSN = 0L & mxLay = 0L & mxLev = 0L & mxChan = 0L
  readu,iu,iTyp,AlgSN
  readu,iu,nPrf
  readu,iu,mxLay
  readu,iu,mxLev
  readu,iu,mxChan
  mxFovs=mxFovs+nPrf
  nFovsTot(ifile) = nPrf
  print, nPrf
  close,iu
  free_lun,iu
ENDFOR


;------------------------------------------------------------------------------
;  arrays to hold variables
;------------------------------------------------------------------------------
Lays2process = [43,55,63,69,75,80,84,88,90,92,94]
nLay         = n_elements(Lays2process)
nChan        = mxChan
lats         = make_array(mxFovs,/float,value=-999.) 
lons         = make_array(mxFovs,/float,value=-999.) 
sfcs         = make_array(mxFovs,/integer,value=-999)
nodes        = make_array(mxFovs,/integer,value=-999)
scans        = make_array(mxFovs,/long,value=-999)
atmpts       = make_array(mxFovs,/integer,value=-999.)
iters        = make_array(mxFovs,/integer,value=-999.)
chisqs       = make_array(mxFovs,/float,value=-999.)
qcs          = make_array(mxFovs,/integer,value=-999)
tsks         = make_array(mxFovs,/float,value=-999.)
psfcs        = make_array(mxFovs,/float,value=-999.)
hours        = make_array(mxFovs,/float,value=-999.)
ems          = make_array(mxFovs,mxChan,/float,value=-999.)
tbs          = make_array(mxFovs,mxChan,/float,value=-999.)
utbs         = make_array(mxFovs,mxChan,/float,value=-999.)
stbs         = make_array(mxFovs,mxChan,/float,value=-999.)
tpws         = make_array(mxFovs,/float,value=-999.)
temps        = make_array(mxFovs,nLay,/float,value=-999.)
wvs          = make_array(mxFovs,nLay,/float,value=-999.)

qcfs         = make_array(mxFovs,/integer,value=-999)
qc_tmp       = intarr(4) & qc_tmp(*) = -999

;---- Loop over files and profiles and store wanted data ----
iTotProf=0L
FOR ifile=0L,nList-1 DO BEGIN
    print,'EDR file=',list[ifile]
    ;---- Set nprofs2read to 1 and read header to intialize scene structure to 1 profile ----
    nProfs2read=1
    LoadHdrScene,list(ifile),topId,nProfs2read,iu,iTyp,nPrf,mxLay,mxLev,mxChan,nAbsorb,$
      nParmCLW,nParmRain,nParmSnow,nParmIce,nParmGrpl,AbsorbID,cfreq,polar,nqc,Scene

    pressLay     = fltarr(mxLay)
    pressLev     = fltarr(mxLay+1)
    tempLay      = fltarr(mxLay)
    Absorbents   = fltarr(mxLay,2)
    xCLW         = fltarr(Scene.nParmCLW)
    xRain        = fltarr(Scene.nParmRain)
    xSnow        = fltarr(Scene.nParmSnow)
    xIce         = fltarr(Scene.nParmIce)
    xGrpl        = fltarr(Scene.nParmGrpl)
    emiss        = fltarr(nchan)
    YFwd         = fltarr(nchan)
    ChanSel      = lonarr(nchan)
    Ym           = fltarr(nchan)
    YmCorr       = fltarr(nchan)
    qc           = intarr(nqc)
    scanyear     = 0L
    scanDay      = 0L
    nAtt         = 0L
    nIter        = 0L
    node         = 0L
    SfcTyp       = 0L
    ProfIndx     = 0L
    iscanPos     = 0L
    iscanLine    = 0L

    FOR iprof=0L,nFovsTot(ifile)-1 DO BEGIN
        ;---Get 1 profile at a time (zeroeth profile)
        get1prof=0
        LoadCoreScene,iu,get1prof,Scene,ProfIndx,pressLay,pressLev,tempLay,Absorbents,xclw, $
          xrain,xsnow,xice,xGrpl,emiss,Angl,Windsp,Tskin,SfcPress,SfcTyp,   $
          qc,lat,lon,node,scanUTC,scanYear,scanDay,nAtt,$
          nIter,Chisq,water,clw,rwp,swp,iwp,gwp,rh,ioErr,YFwd,ChanSel,Ym,iscanPos,iscanLine,$
          RelAziAngl,SolZenAngl,YmCorr,SnowDepth

        lats(iTotProf)                    = Scene.lat
        lons(iTotProf)                    = Scene.lon
        nodes(iTotProf)                   = Scene.direc
        temps(iTotProf,0:nLay-1)          = Scene.TempLayVec(0,Lays2process)
        wvs(iTotProf,0:nlay-1)            = Scene.AbsorbLayVec(0,Lays2process,0)
        tpws(iTotProf)                    = Scene.TPWvec
        chisqs(iTotProf)                  = Scene.Chisq
        atmpts(iTotProf)                  = Scene.nAttempt
        iters(iTotProf)                   = Scene.nIter
        tsks(iTotProf)                    = Scene.Tskinvec
	hours(iTotProf)                   = Scene.Time / 3600.0
        ems(iTotProf,0:nchan-1)           = Scene.Emissvec(0,0:scene.nchan-1)
        tbs(iTotProf,0:nchan-1)           = Scene.YmCorr(0,0:scene.nchan-1)
        utbs(iTotProf,0:nchan-1)          = Scene.Ym(0,0:scene.nchan-1)
        stbs(iTotProf,0:nchan-1)          = Scene.YFwd(0,0:scene.nchan-1)
        sfcs(iTotProf)                    = Scene.SfcTypVec
        psfcs(iTotProf)                   = Scene.SfcPressVec
        scans(iTotProf)                   = Scene.ScanPos
        qcs(iTotProf)                     = Scene.qc(0,0)
	
	;---- different handling of QC flags, give a new modified QC to plot ----
	qc_tmp(*) =  Scene.qc(0,*)
	if qc_tmp(0) eq 0 then  qcfs(iTotProf) = 0
	if qc_tmp(0) eq 2 then  qcfs(iTotProf) = 1
	if qc_tmp(0) ne 2 and qc_tmp(0) ge 0 then begin
	  if BTEST( qc_tmp(1), 3 ) eq 1 then qcfs(iTotProf) = 2 
	  if BTEST( qc_tmp(1), 4 ) eq 1 then qcfs(iTotProf) = 3
	  if BTEST( qc_tmp(1), 5 ) eq 1 then qcfs(iTotProf) = 4 
	  if BTEST( qc_tmp(2), 1 ) eq 1 then qcfs(iTotProf) = 5 
	  if BTEST( qc_tmp(2), 4 ) eq 1 then qcfs(iTotProf) = 6 
	  if BTEST( qc_tmp(2), 1 ) eq 1 and BTEST( qc_tmp(2), 4 ) eq 1 then qcfs(iTotProf) = 7
	  if ( BTEST( qc_tmp(1), 2 ) eq 1 or  BTEST( qc_tmp(2), 5 ) eq 1 ) $
	   and BTEST( qc_tmp(2), 2 ) eq 1 then qcfs(iTotProf) = 8
	  if ( BTEST( qc_tmp(1), 2 ) eq 0 and BTEST( qc_tmp(2), 5 ) eq 0 ) $
	   and BTEST( qc_tmp(2), 2 ) eq 1 then qcfs(iTotProf) = 9
	endif
	
        iTotProf=iTotProf+1
    ENDFOR
    close,iu
    free_lun,iu
ENDFOR


;------------------------------------------------------------------------------
;  Set Up Plot
;------------------------------------------------------------------------------

PLOT_CHISQ	 = 1
PLOT_NITER	 = 1
PLOT_NATTEMPT	 = 1
PLOT_QC 	 = 1
PLOT_PSFC	 = 1
PLOT_SFC	 = 1
PLOT_TSKIN	 = 1
PLOT_HOUR	 = 1

PLOT_EM 	 = 1
PLOT_TBC 	 = 1
PLOT_TBU 	 = 1
PLOT_TEMP	 = 1
PLOT_WV 	 = 1
PLOT_SFC_POLAR	 = 1

PLOT_SCANPOS	 = 0

nodeIds	          = ['as','ds']
nodeTitles	  = ['Asc','Des']
nodes2plot	  = [0,1] ;0/1 for asc/des
nNode	  	  = n_elements(nodes2plot)

sfcIds	          = ['all','sea','lnd']
sfcs2plot	  = [-1,0,2] ;-1/0/1/2 for all/ocean/seaice/land/snow/etc
nsfc	  	  = n_elements(sfcs2plot)
chisqThresh	  = 5

csize = 10
thck = 1
fill_continents = 0
ioverplot = 0

xscale = 0.085
yscale = 0.075

;---- FOV size, used to plot different size of symbols in different scan position ----
NUMSPOT_M=90

fov_cross = fltarr(NUMSPOT_M)
FOV_MHS_cross_track, fov_cross
fov_scale_cross = fltarr(NUMSPOT_M)
fov_scale_cross(*) = fov_cross(*) / 17.0138

fov_along = fltarr(NUMSPOT_M)
FOV_MHS_along_track, fov_along
fov_scale_along = fltarr(NUMSPOT_M)
fov_scale_along(*) = fov_along(*) / 16.1554

nscanpos = NUMSPOT_M

;---- the angle between track direction and x direction ----
rotate_angles = [30.0, -30.0]

;---- default map map_dim for POES ----
map_dim = [650,500]

;---- longitude span too wide ----
;if satId eq 'metopA' or satId eq 'n19' then begin
;  latavg = (latmax+latmin)/2.0
;  lonspan = lonmax-lonmin
;  latspan = latmax-latmin
;  map_dim = [ LONG(lonspan*!DTOR*6371.0*cos(latavg*!DTOR)), LONG(latspan*!DTOR*6371.0) ]
;endif

;---- IMG FOV 13.2 X 15.5 km ----
if satId eq 'f16' or satId eq 'f17' or satId eq 'f18' then begin
  nscanpos = 180L
  fov_scale_cross = make_array(nscanpos,/float,value=1.0)
  fov_scale_along = make_array(nscanpos,/float,value=1.0)
  xscale = 0.0528
  yscale = xscale * 155.0/132.0
  ;latavg = (latmax+latmin)/2.0
  ;lonspan = lonmax-lonmin
  ;latspan = latmax-latmin
  ;map_dim = [ LONG(lonspan*!DTOR*6371.0*cos(latavg*!DTOR)), LONG(latspan*!DTOR*6371.0) ]
endif

;----  IFOV 14 X 8 km ----
if satId eq 'aqua' then begin
  nscanpos = 191L  ;---- should be 243, but we did data thinning ---
  fov_scale_cross = make_array(nscanpos,/float,value=1.0)
  fov_scale_along = make_array(nscanpos,/float,value=1.0)
  yscale = 0.056
  xscale = yscale * 8.0 / 14.0
  ;latavg = (latmax+latmin)/2.0
  ;lonspan = lonmax-lonmin
  ;latspan = latmax-latmin
  ;map_dim = [ LONG(lonspan*!DTOR*6371.0*cos(latavg*!DTOR)), LONG(latspan*!DTOR*6371.0) ]
endif

;----  IFOV 14 X 8 km ----
if satId eq 'gcomw1' then begin
  nscanpos = 243L
  fov_scale_cross = make_array(nscanpos,/float,value=1.0)
  fov_scale_along = make_array(nscanpos,/float,value=1.0)
  yscale = 0.056
  xscale = yscale * 8.0 / 14.0
  ;latavg = (latmax+latmin)/2.0
  ;lonspan = lonmax-lonmin
  ;latspan = latmax-latmin
  ;map_dim = [ LONG(lonspan*!DTOR*6371.0*cos(latavg*!DTOR)), LONG(latspan*!DTOR*6371.0) ]
endif

;----  IFOV 9.1 km^2(lr) or 4.6 km^2 (hr) ----
if satId eq 'trmm' or satId eq 'gpm' or satId eq 'trmm2a12' then begin
  nscanpos = 208L  ; default to high resolution
  if resolution eq -1 then nscanpos = 26    ; CR
  if resolution eq 0  then nscanpos = 104   ; LR
  if resolution eq 1  then nscanpos = 208   ; HR
  fov_scale_cross = make_array(nscanpos,/float,value=1.0)
  fov_scale_along = make_array(nscanpos,/float,value=1.0)
  xscale = 0.05
  yscale = 0.05
  ;latavg = (latmax+latmin)/2.0
  ;lonspan = lonmax-lonmin
  ;latspan = latmax-latmin
  ;map_dim = [ 650,500 ]
endif

;----  NPP ATMS high resolution stats ----
; near nadir 15.8 km x 15.8 km.  At edge of scan it is 68.4 km x 30.0 km 
; (dY x dX, Y is along track, X is cross track).

if satId eq 'npp' then begin
  
  Sat_Height = 824.0
  
  if resolution eq 0 then begin  ; LR
     NUMSPOT_M = 32L
     scan_angle_deg = 3.333
  endif
  
  if resolution eq 1  then begin ; HR
    NUMSPOT_M = 96L
    scan_angle_deg = 1.111
  endif

  nscanpos =  NUMSPOT_M
  
  fov_cross = fltarr(NUMSPOT_M)
  fov_cross_track, NUMSPOT_M, scan_angle_deg, Sat_Height, fov_cross
  fov_scale_cross = fltarr(NUMSPOT_M)
  fov_scale_cross(*) = fov_cross(*) / 15.9809

  fov_along = fltarr(NUMSPOT_M)
  fov_along_track, NUMSPOT_M, scan_angle_deg, Sat_Height, fov_along
  fov_scale_along = fltarr(NUMSPOT_M)
  fov_scale_along(*) = fov_along(*) / 15.9786

  xscale = 0.085
  yscale = 0.075
  
  ;latavg = (latmax+latmin)/2.0
  ;lonspan = lonmax-lonmin
  ;latspan = latmax-latmin
  ;xscale = 0.045
  ;yscale = 0.045
  ;map_dim = [ LONG(lonspan*!DTOR*6371.0*cos(latavg*!DTOR)), LONG(latspan*!DTOR*6371.0) ] 
endif


;------------------------------------------------------------------------------
;  Plot Section
;------------------------------------------------------------------------------
IF( satId eq Consts.STR_SATID_AQUA ) THEN satId = 'amsre'

vtxt = ' (r'+version+')'
SAT = STRUPCASE(satId)
if satId eq 'trmm'     then SAT = 'TRMM/TMI'
if satId eq 'gpm'      then SAT = 'GPM/GMI'
if satId eq 'npp'      then SAT = 'NPP/ATMS'
if satId eq 'trmm2a12' then SAT = 'TRMM_2A12'

titSat='MIRS '+SAT
if satId eq 'trmm2a12' then titSat = SAT

;---- plot a border over symbol or not. 1-yes, 0-no ----
border = 0

FOR iNode=0,nNode-1 DO BEGIN  ;---- loop start for iNode
  
  node = nodes2plot(iNode)
  nodeId = nodeIds(iNode)
  nodetxt = nodeTitles(iNode)
  rotate_angle = rotate_angles(iNode)
  
  FOR isfc=0,nsfc-1 DO BEGIN  ;---- loop start for isfc

    sfc = sfcs2plot(isfc)
    sfcId = sfcIds(isfc)
    
    ;---- Scan Time in Hour ----
    IF (PLOT_HOUR eq 1) THEN BEGIN
      minY=0
      maxY=24
      div  = 8
      fmt  = '(I2)'
      unit = '(Hour)'
      isQc = 0
      tit = titSat+' Scan Time '+unit+' '+date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where(hours ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where(hours ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
	img_name = figsDir+prefix+yyyymmdd+'_hour_'+sfcId+'_' +nodeId+'.png'
	map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,hours,$
	       ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,xscale,yscale,$
	       tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Number of attmempts ----
    IF (PLOT_NATTEMPT eq 1) THEN BEGIN
      minY = 0
      maxY = 2
      div  = 2
      fmt  = '(I2)'
      unit = ''
      isQc = 0
      tit = titSat+' Number of Retrieval Attempts '+date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and atmpts ge 0 and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and atmpts ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
	img_name = figsDir+prefix+yyyymmdd+'_nattempt_'+sfcId+'_' +nodeId+'.png'
	map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,atmpts,$
	        ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,xscale,yscale,$
	        tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF
    
    ;---- Number of Iterations ----
    IF (PLOT_NITER eq 1) THEN BEGIN
      minY = 0
      maxY = 7
      div  = 7
      fmt  = '(I2)'
      unit = ''
      isQc = 0
      tit = titSat+' Number of Iterations '+date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and iters ge 0 and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and iters ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
	img_name = figsDir+prefix+yyyymmdd+'_niter_'+sfcId+'_' +nodeId+'.png'
	map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,iters,$
	        ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,xscale,yscale,$
	        tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- scan position ----
    IF (PLOT_SCANPOS eq 1) THEN BEGIN
      minY = 0
      maxY = 30
      div  = 6
      fmt  = '(I2)'
      unit = ''
      isQc = 0
      tit = titSat+' Scan Position '+ date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and scans ge 0 and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and scans ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
	img_name = figsDir+prefix+yyyymmdd+'_npos_'+sfcId+'_' +nodeId+'.png'
	map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,scans,$
	        ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,xscale,yscale,$
	        tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF
    
    ;---- Chi-Square----
    IF (PLOT_CHISQ eq 1) THEN BEGIN
      tit = titSat+' Convergence Metric (ChiSq) '+date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where(chisqs ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where(chisqs ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs eq sfc, cnt)
      minY = 0.
      maxY = 10
      div  = maxY - minY 
      fmt  = '(I2)'
      unit = ''
      isQc = 0
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_chisq_'+sfcId+'_' +nodeId+'.png'
	map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,chisqs,$
	        ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,xscale,yscale,$
	        tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF
    
    ;---- QC Flags ----
    IF (PLOT_QC eq 1) THEN BEGIN
      minY = 0
      maxY = 9
      div = maxY - minY
      fmt  = '(I2)'
      unit = ''
      tit = titSat+' Geophysical Events '+date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and qcfs ge 0 and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and qcfs ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
	img_name = figsDir+prefix+yyyymmdd+'_qc_'+sfcId+'_' +nodeId+'.png'
	map_hr_qcf,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,qcfs,$
	           ind,nscanpos,fov_scale_cross,fov_scale_along,xscale,yscale,$
	           tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF
    
    
    ;---- Surface Type ----
    IF (PLOT_SFC eq 1) THEN BEGIN
      minY = 0
      maxY = 3
      div  = 3
      fmt  = '(I2)'
      unit = ''
      isQc = 0
      tit = titSat+' Pre-Classified Sfc Type '+date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where(lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where(lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_sfc_cyl_'+sfcId+'_' +nodeId+'.png'
	map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,sfcs,$
	       ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,xscale,yscale,$
	       tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Surface Pressure ----
    IF (PLOT_PSFC eq 1) THEN BEGIN
      minY = 700
      maxY = 1050
      div  = 7
      fmt  = '(I4)'
      unit = '(mb)'
      isQc = 1
      tit = titSat+' Surface Pressure '+unit+ ' '+date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where(psfcs ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where(psfcs ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
	img_name = figsDir+prefix+yyyymmdd+'_psfc_'+sfcId+'_' +nodeId+'.png'
	map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,psfcs,$
	       ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,xscale,yscale,$
	       tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Skin Temperature ----
    IF (PLOT_TSKIN eq 1) THEN BEGIN
      minY=275
      maxY=315
      if region eq 3 then begin
        minY = 275
        maxY = 315
      endif
      div  = 8
      fmt  = '(I3)'
      unit = '(K)'
      isQc = 1
      tit = titSat+' Skin Temperature '+unit+' '+date+' '+nodetxt+vtxt
      if sfc lt 0 THEN $
        ind = where(tsks ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs ge 0, cnt) $
      else $
        ind = where(tsks ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
	img_name = figsDir+prefix+yyyymmdd+'_tskin_'+sfcId+'_' +nodeId+'.png'
	map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,tsks,$
	       ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,xscale,yscale,$
	       tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Measured corrected TBs ----
    IF (PLOT_TBC eq 1) THEN BEGIN
      Channels2plot = indgen(Scene.nChan)
      nchans = n_elements(Channels2plot)
      titles    = ['23v','31v','50v','52v','53h',  '54h','54v','55h','57h1','57h2',  '57h3','57h4','57h5','57h6','89v1',  '89v2','157h','184h','186h','190v']
      titlesStr = ['23v','31v','50v','52v','53h',  '54h','54v','55h','57h1','57h2',  '57h3','57h4','57h5','57h6','89v1',  '89v2','157h','184h','186h','190v']
      chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20']

      minvalues = [160,160,220,250,240, 225,215,205,200,210, 220,232,240,245,190, 190,250,220,220,200]
      maxvalues = [300,300,280,275,260, 240,225,220,215,218, 228,239,250,255,290, 290,300,265,280,290]
      divs      = [  7,  7,  6,  5,  5,   5,  5,  5,  5,  8,   8, 7,  5,  5,  5,   5,  8,  9,  6,  9]

      if sfc eq 0 then begin
	minvalues = [160,160,220,250,240, 225,210,205,205,210, 220,232,240,245,190, 190,250,220,220,200]
        maxvalues = [280,280,280,265,260, 240,225,220,215,218, 228,239,250,255,270, 270,300,265,280,290]
        divs      = [  6,  6,  6,  5,  5,   5,  5,  5,  5,  8,   8, 7,  5,  5,  8,   8,  8,  9,  6,  9]
      endif

      if sfc eq 2 then begin
	minvalues = [250,250,250,260,240, 230,220,210,205,210, 220,232,240,255,260, 260,270,220,250,250]
        maxvalues = [300,300,300,290,280, 255,235,220,215,218, 228,239,255,270,300, 300,300,270,280,290]
        divs      = [  5,  5,  5,  6,  8,   5,  5,  5,  5,  8,   8, 7,  5,  5,  8,   8,  6,  5,  6,  8]
      endif

      if ( satId eq 'npp' ) then begin
          titles    = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
          titlesStr = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
	  minvalues = [140,140,170,170,170,200, 200,210,200,200,180, 180,180,180,200,150,150, 220,220,220,200,200]
	  maxvalues = [300,300,290,290,290,270, 250,230,230,230,230, 250,250,280,290,300,300, 300,300,300,300,300]
	  divs      = [8,8,6,6,7, 5,5,6,6,5, 7,10,7,9,5, 5,5,9,6,5,6,6]
	  chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22']
      endif

      if ( satId eq 'f16' ) then begin
          titles    = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          titlesStr = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          minvalues = [ 240,  240,  240,  220,  200,  200,   210,   150,   200,   200,   225,   100,  140,  140,  100,  200,  200,  180,  205,   200,    230,    225,    220,    215]
          maxvalues = [ 290,  290,  290,  270,  220,  225,   225,   300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,   230,    260,    260,    250,    230]
	  divs      = [  5,    5,    5,    5,    4,    5,    5,      5,     9,     5,     5,    10,    8,    8,    10,   10,   10,   6,    7,     6,      6,      7,      6,      5]  
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
      endif
      if ( satId eq 'f17' ) then begin
          titles    = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          titlesStr = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          minvalues = [ 200,  210,  215,  210,  200,  200,  210,  150,   200,   200,   225,   100,  140,  140,  100,  140,  140,  100,  205,   200,    230,    225,    220,    215]
          maxvalues = [ 270,  270,  250,  230,  225,  225,  225,  300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,   230,    260,    260,    250,    230]
	  divs      = [  7,    6,    7,    4,    5,    5,    5,    5,     9,     5,     5,    10,    8,    8,    10,   8,    8,    10,    7,     6,      6,      7,      6,      5]  
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
      endif
      if ( satId eq 'f18' ) then begin
          titles    = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          titlesStr = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          minvalues = [ 200,  210,  215,  210,  200,  200,  210,  150,   200,   200,   225,   100,  140,  140,  100,  140,  140,  100,  205,   200,    230,    225,    220,    215]
          maxvalues = [ 270,  270,  250,  230,  225,  225,  225,  300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,   230,    260,    260,    250,    230]
	  divs      = [  7,    6,    7,    4,    5,    5,    5,    5,     9,     5,     5,    10,    8,    8,    10,   8,    8,    10,    7,     6,      6,      7,      6,      5]  
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
      endif
      if ( satId eq 'amsre' ) then begin
          titles    = ['6.9v','6.9h','10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h' ]       
          titlesStr = ['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ]       
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12']
	  minvalues = [  75,    50,    125,     50,    150,    75,     175,     75,    150,    100,   175,  125  ]
          maxvalues = [ 315,   300,    300,    300,    315,   315,     315,    300,    315,    315,   315,  300  ]
	  divs      = [  6,     5,      7,      5,      11,    6,       7,      9,      11,     5,     7,    7   ]
	  ;---- - ocean ranges ---
	  if sfcs2plot(isfc) eq 0 then begin
	    minvalues = [ 150,    75,    150,     75,    175,   100,     175,    150,    200,    125,   225,  125  ]
            maxvalues = [ 175,   125,    200,    125,    225,   200,     250,    250,    250,    250,   300,  300  ]
	    divs      = [  5,     10,     10,     10,     10,    10,      5,     10,     10,     5,     5,    7   ]
	  endif
	  ;---- - land ranges ---
	  if sfcs2plot(isfc) eq 2 then begin
	    minvalues = [ 200,   200,    200,   200, 200, 200, 200, 200, 200, 200, 200, 200  ]
            maxvalues = [ 300,   300,    300,   300, 300, 300, 300, 300, 300, 300, 300, 300  ]
	    divs      = [  5,     5,      5,     5,  5,   5,   5,    5,   5,  5,   5,   5    ]
	  endif
      endif
      if ( satId eq 'gcomw1' ) then begin
          titles    = ['6.9v','6.9h','7.3v','7.3h','10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h' ]       
          titlesStr = ['6v','6h','7v','7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h' ]       
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14']
	  minvalues = [  75,    50,  75,    50,  125,     50,    150,    75,     175,     75,    150,    100,   175,  125  ]
          maxvalues = [ 315,   300,  315,   300,  300,    300,   315,   315,     315,    300,    315,    315,   315,  300  ]
	  divs      = [  6,     5,    6,     5,   7,      5,      11,    6,       7,      9,      11,     5,     7,    7   ]
	  ;---- - ocean ranges ---
	  if sfcs2plot(isfc) eq 0 then begin
	    minvalues = [ 150,   75,  150,    75,    150,     75,    175,   100,   175,    150,    200,    125,   225,  125  ]
            maxvalues = [ 175,  125,  175,   125,    200,    125,    225,   200,   250,    250,    250,    250,   300,  300  ]
	    divs      = [  5,    10,   5,     10,    10,      10,     10,    10,    5,     10,      10,     5,     5,    7   ]
	  endif
	  ;---- - land ranges ---
	  if sfcs2plot(isfc) eq 2 then begin
	    minvalues = [ 200,   200,  200,   200,   200,   200, 200, 200, 200, 200, 200, 200, 200, 200  ]
            maxvalues = [ 300,   300,  300,   300,   300,   300, 300, 300, 300, 300, 300, 300, 300, 300  ]
	    divs      = [  5,     5,    5,     5,     5,     5,  5,   5,   5,    5,   5,  5,   5,   5    ]
	  endif
      endif
      if ( satId eq 'trmm' ) then begin
          titles    = ['10.65v','10.65h','19.35v','19.35h','21.3v','37.0v','37.0h','85.5v','85.5h' ]  	
          titlesStr = ['11v','11h','19v','19h','21v','37v','37h','85v','85h' ]       
          chan_str  = ['1','2','3','4','5','6','7','8','9']
	  minvalues = [  125,	  50,	 150,	 75,	 175,  150,    100,   175,  125  ]
          maxvalues = [  300,	 300,	 315,	315,	 315,  315,    315,   315,  300  ]
	  divs      = [   7,	  5,	  11,	 6,	  7,    11,	5,     7,    7   ]
	  ;---- ocean ranges ---
	  if sfcs2plot(isfc) eq 0 then begin
	    minvalues = [ 150,     75,    175,   100,	  175,     200,    125,   225,  125  ]
            maxvalues = [ 200,    125,    225,   200,	  250,     250,    250,   300,  300  ]
	    divs      = [  10,     10,     10,    10,	   5,	   10,     5,	  5,	7    ]
	  endif
	  ;---- land ranges ---
	  if sfcs2plot(isfc) eq 2 then begin
	    minvalues = [  200,   200, 200, 200, 200, 200, 200, 200, 200  ]
            maxvalues = [  300,   300, 300, 300, 300, 300, 300, 300, 300  ]
	    divs      = [   5,     5,  5,   5,   5,    5,  5,	5,   5    ]
	  endif
      endif
      if ( satId eq 'gpm' ) then begin
          titles    = ['10.65v','10.65h','18.70v','18.70h','23.8v','36.5v','36.5h','89.0v','89.0h', '165.5v','165.5h','183.31v1','183.31v2']  	
          titlesStr = ['11v','11h','19v','19h','24v','37v','37h','89v','89h','166v','166h','183v1','183v2' ]       
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13']
	  minvalues = [  125,	  50,	 150,	 75,	 175,  150,    100,   175,  125, 250, 265, 240, 240 ]
          maxvalues = [  300,	 300,	 315,	315,	 315,  315,    315,   315,  300, 300, 295, 280, 280 ]
	  divs      = [   7,	  5,	  11,	 6,	  7,    11,	5,     7,    7,    5,   6,  8,   8  ]
	  ;---- ocean ranges ---
	  if sfcs2plot(isfc) eq 0 then begin
            minvalues = [ 150,  75, 175, 100, 175, 200, 125, 225, 125,  250, 265, 240, 240 ]
            maxvalues = [ 200, 125, 225, 200, 250, 250, 250, 300, 300,  300, 295, 280, 280 ]
            divs      = [  10,  10,  10,  10,   5,  10,   5,   5,   7,   5,   6,   8,   8  ]
	  endif
	  ;---- land ranges ---
	  if sfcs2plot(isfc) eq 2 then begin
	    minvalues = [  200, 200, 200, 200, 200, 200, 200, 200, 200, 250, 265, 240, 240  ]
            maxvalues = [  300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 295, 280, 280  ]
	    divs      = [   5,   5,  5,   5,	5,   5,   5,   5,   5,   5,   6,   8,	8   ]
	  endif
      endif
      if ( satId eq 'fy3ri' ) then begin
          titles    = ['10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h' ]  	
          titlesStr = ['11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ]       
          minvalues = [    125,     50,    150,    75,     175,     75,    150,    100,   175,  125  ]
          maxvalues = [    300,    300,    315,   315,     315,    300,    315,    315,   315,  300  ]
	  divs      = [      7,      5,     11,    6,       7,      9,      11,     5,     7,    7   ]  
          chan_str  = ['1','2','3','4','5','6','7','8','9','10']
      endif
	
      minYs = minvalues
      maxYs = maxvalues
      unit = '(K)'
      fmt  = '(I3)'
      isQc = 0
      FOR ichan=0,nchans-1 DO BEGIN
	minY = minYs(ichan)
	maxY = maxYs(ichan)
	div  = divs(ichan)
        tit  = titSat+' Corr. TB '+unit+' @ Ch'+string(ichan+1,'(i2)')+' ('+titles(ichan)+') '+date +' '+nodetxt+vtxt
	if sfc lt 0 THEN $
          ind = where(tbs(*,ichan) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                and nodes eq node and sfcs ge 0, cnt) $
	else $
          ind = where(tbs(*,ichan) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                and nodes eq node and sfcs eq sfc, cnt)
	
	img_name = figsDir+prefix+yyyymmdd+'_tbc_'+titlesStr(ichan)+'_'+sfcId+'_'+nodeId+'.png'
	IF cnt gt 0 THEN BEGIN
	  map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,$
	         tbs(*,ichan),ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,$
	         xscale,yscale,tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
	ENDIF ELSE BEGIN
	  map_hr_blank,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,$
	         tbs(*,ichan),ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,$
	         xscale,yscale,tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
	ENDELSE
      ENDFOR
    ENDIF

    ;---- Measured Un-corrected TBs ----
    IF (PLOT_TBU eq 1) THEN BEGIN
      Channels2plot = indgen(Scene.nChan)
      nchans = n_elements(Channels2plot)
      titles    = ['23v','31v','50v','52v','53h',  '54h','54v','55h','57h1','57h2',  '57h3','57h4','57h5','57h6','89v1',  '89v2','157h','184h','186h','190v']
      titlesStr = ['23v','31v','50v','52v','53h',  '54h','54v','55h','57h1','57h2',  '57h3','57h4','57h5','57h6','89v1',  '89v2','157h','184h','186h','190v']
      chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20']

      minvalues = [160,160,220,250,240, 225,215,205,200,210, 220,232,240,245,190, 190,150,220,220,200]
      maxvalues = [300,300,280,275,260, 240,225,220,215,218, 228,239,250,255,290, 290,300,265,280,290]
      divs      = [  7,  7,  6,  5,  5,   5,  5,  5,  5,  8,   8, 7,  5,  5,  5,   5,  5,  9,  6,  9]

      if sfc eq 0 then begin
	minvalues = [160,160,220,250,240, 225,210,205,205,210, 220,232,240,245,190, 190,150,220,220,200]
        maxvalues = [280,280,280,265,260, 240,225,220,215,218, 228,239,250,255,270, 270,300,265,280,290]
        divs      = [  6,  6,  6,  5,  5,   5,  5,  5,  5,  8,   8, 7,  5,  5,  8,   8,  5,  9,  6,  9]
      endif

      if sfc eq 2 then begin
	minvalues = [250,250,250,260,240, 230,220,210,205,210, 220,232,240,255,260, 260,270,220,250,250]
        maxvalues = [300,300,300,290,280, 255,235,220,215,218, 228,239,255,270,300, 300,300,270,280,290]
        divs      = [  5,  5,  5,  6,  8,   5,  5,  5,  5,  8,   8, 7,  5,  5,  8,   8,  6,  5,  6,  8]
      endif
      
      if ( satId eq 'npp' ) then begin
          titles    = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
          titlesStr = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
	  minvalues = [140,140,170,170,170,200, 200,210,200,200,180, 180,180,180,200,150,150, 220,220,220,200,200]
	  maxvalues = [300,300,290,290,290,270, 250,230,230,230,230, 250,250,280,290,300,300, 300,300,300,300,300]
	  divs      = [8,8,6,6,7, 5,5,6,6,5, 7,10,7,9,5, 5,5,9,6,5,6,6]
	  chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22']
      endif
      if ( satId eq 'f16' ) then begin
          titles    = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          titlesStr = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          minvalues = [ 240,  240,  240,  220,  200,  200,   210,   150,   200,   200,   225,   100,  140,  140,  100,  200,  200,  180,  205,   200,    230,    225,    220,    215]
          maxvalues = [ 290,  290,  290,  270,  220,  225,   225,   300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,   230,    260,    260,    250,    230]
	  divs      = [  5,    5,    5,    5,    4,    5,    5,      5,     9,     5,     5,    10,    8,    8,    10,   10,   10,   6,    7,     6,      6,      7,      6,      5]  
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
      endif
      if ( satId eq 'f17' ) then begin
          titles    = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          titlesStr = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          minvalues = [ 200,  210,  215,  210,  200,  200,  210,  150,   200,   200,   225,   100,  140,  140,  100,  140,  140,  100,  205,   200,    230,    225,    220,    215]
          maxvalues = [ 270,  270,  250,  230,  225,  225,  225,  300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,   230,    260,    260,    250,    230]
	  divs      = [  7,    6,    7,    4,    5,    5,    5,    5,     9,     5,     5,    10,    8,    8,    10,   8,    8,    10,    7,     6,      6,      7,      6,      5]  
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
      endif
      if ( satId eq 'f18' ) then begin
          titles    = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          titlesStr = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
          minvalues = [ 200,  210,  215,  210,  200,  200,  210,  150,   200,   200,   225,   100,  140,  140,  100,  140,  140,  100,  205,   200,    230,    225,    220,    215]
          maxvalues = [ 270,  270,  250,  230,  225,  225,  225,  300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,   230,    260,    260,    250,    230]
	  divs      = [  7,    6,    7,    4,    5,    5,    5,    5,     9,     5,     5,    10,    8,    8,    10,   8,    8,    10,    7,     6,      6,      7,      6,      5]  
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
      endif
      if ( satId eq 'amsre' ) then begin
          titles    = ['6.9v','6.9h','10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h' ]  	
          titlesStr = ['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ]       
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12']
          minvalues = [ 150,    75,    150,      75,    175,    100,     175,    150,    150,    125,   225,  125  ]
          maxvalues = [ 300,   300,    300,     300,    300,    300,     300,    300,    300,    300,   300,  300  ]
	  divs      = [  6,     9,      6,       9,      5,      8,       5,      6,      6,      7,     5,    7   ]  
	  ;---- - ocean ranges ---
	  if sfcs2plot(isfc) eq 0 then begin
	    minvalues = [ 150,    75,    150,     75,    175,   100,     175,    150,    200,    125,   225,  125  ]
            maxvalues = [ 175,   125,    200,    125,    225,   200,     250,    250,    250,    250,   300,  300  ]
	    divs      = [  5,     10,     10,     10,     10,    10,      5,     10,     10,     5,     5,    7    ]
	  endif
	  ;---- - land ranges ---
	  if sfcs2plot(isfc) eq 2 then begin
	    minvalues = [ 200,   200,    200,   200, 200, 200, 200, 200, 200, 200, 200, 200  ]
            maxvalues = [ 300,   300,    300,   300, 300, 300, 300, 300, 300, 300, 300, 300  ]
	    divs      = [  5,     5,      5,     5,  5,   5,   5,    5,   5,  5,   5,   5    ]
	  endif
      endif
      if ( satId eq 'gcomw1' ) then begin
          titles    = ['6.9v','6.9h','7.3v','7.3h','10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h' ]       
          titlesStr = ['6v','6h','7v','7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h' ]       
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14']
	  minvalues = [  75,    50,  75,    50,  125,     50,    150,    75,     175,     75,    150,    100,   175,  125  ]
          maxvalues = [ 315,   300,  315,   300,  300,    300,   315,   315,     315,    300,    315,    315,   315,  300  ]
	  divs      = [  6,     5,    6,     5,   7,      5,      11,    6,       7,      9,      11,     5,     7,    7   ]
	  ;---- - ocean ranges ---
	  if sfcs2plot(isfc) eq 0 then begin
	    minvalues = [ 150,   75,  150,    75,    150,     75,    175,   100,   175,    150,    200,    125,   225,  125  ]
            maxvalues = [ 175,  125,  175,   125,    200,    125,    225,   200,   250,    250,    250,    250,   300,  300  ]
	    divs      = [  5,    10,   5,     10,    10,      10,     10,    10,    5,     10,      10,     5,     5,    7   ]
	  endif
	  ;---- - land ranges ---
	  if sfcs2plot(isfc) eq 2 then begin
	    minvalues = [ 200,   200,  200,   200,   200,   200, 200, 200, 200, 200, 200, 200, 200, 200  ]
            maxvalues = [ 300,   300,  300,   300,   300,   300, 300, 300, 300, 300, 300, 300, 300, 300  ]
	    divs      = [  5,     5,    5,     5,     5,     5,  5,   5,   5,    5,   5,  5,   5,   5    ]
	  endif
      endif
      if ( satId eq 'trmm' ) then begin
          titles    = ['10.65v','10.65h','19.35v','19.35h','21.3v','37.0v','37.0h','85.5v','85.5h' ]  	
          titlesStr = ['11v','11h','19v','19h','21v','37v','37h','85v','85h' ]       
          chan_str  = ['1','2','3','4','5','6','7','8','9']
	  minvalues = [  125,	  50,	 150,	 75,	 175,  150,    100,   175,  125  ]
          maxvalues = [  300,	 300,	 315,	315,	 315,  315,    315,   315,  300  ]
	  divs      = [   7,	  5,	  11,	 6,	  7,    11,	5,     7,    7   ]
	  ;---- ocean ranges ---
	  if sfcs2plot(isfc) eq 0 then begin
	    minvalues = [ 150,     75,    175,   100,	  175,     200,    125,   225,  125  ]
            maxvalues = [ 200,    125,    225,   200,	  250,     250,    250,   300,  300  ]
	    divs      = [  10,     10,     10,    10,	   5,	   10,     5,	  5,	7    ]
	  endif
	  ;---- land ranges ---
	  if sfcs2plot(isfc) eq 2 then begin
	    minvalues = [  200,   200, 200, 200, 200, 200, 200, 200, 200  ]
            maxvalues = [  300,   300, 300, 300, 300, 300, 300, 300, 300  ]
	    divs      = [   5,     5,  5,   5,   5,    5,  5,	5,   5    ]
	  endif
      endif
      if ( satId eq 'gpm' ) then begin
          titles    = ['10.65v','10.65h','18.70v','18.70h','23.8v','36.5v','36.5h','89.0v','89.0h', '165.5v','165.5h','183.31v1','183.31v2']  	
          titlesStr = ['11v','11h','19v','19h','24v','37v','37h','89v','89h','166v','166h','183v1','183v2' ]       
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13']
	  minvalues = [  125,	  50,	 150,	 75,	 175,  150,    100,   175,  125, 250, 265, 240, 240 ]
          maxvalues = [  300,	 300,	 315,	315,	 315,  315,    315,   315,  300, 300, 295, 280, 280 ]
	  divs      = [   7,	  5,	  11,	 6,	  7,    11,	5,     7,    7,    5,   6,  8,   8  ]
	  ;---- ocean ranges ---
	  if sfcs2plot(isfc) eq 0 then begin
	    minvalues = [ 150,     75,    175,   100,	  175,     200,    125,   225,  125, 250, 265, 240, 240 ]
            maxvalues = [ 200,    125,    225,   200,	  250,     250,    250,   300,  300, 300, 295, 280, 280 ]
	    divs      = [  10,     10,     10,    10,	   5,	   10,      5,	   5,	 7,   5,   6,   8,   8  ]
	  endif
	  ;---- land ranges ---
	  if sfcs2plot(isfc) eq 2 then begin
	    minvalues = [  200, 200, 200, 200, 200, 200, 200, 200, 200, 250, 265, 240, 240  ]
            maxvalues = [  300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 295, 280, 280  ]
	    divs      = [   5,   5,  5,   5,	5,   5,   5,   5,   5,   5,   6,   8,	8   ]
	  endif
      endif
      if ( satId eq 'fy3ri' ) then begin
          titles    = ['10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h' ]  	
          titlesStr = ['11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ]       
          minvalues = [ 125,	 50,	150,	75,	175,	 75,	150,	100,   175,  125  ]
          maxvalues = [ 300,	300,	315,   315,	315,	300,	315,	315,   315,  300  ]
	  divs      = [   7,	  5,	 11,	6,	 7,	 9,	 11,	 5,	7,    7   ]  
          chan_str  = ['1','2','3','4','5','6','7','8','9','10']
      endif

      minYs = minvalues
      maxYs = maxvalues
      unit = '(K)'
      fmt  = '(I3)'
      isQc = 0
      FOR ichan=0,nchans-1 DO BEGIN
	minY = minYs(ichan)
	maxY = maxYs(ichan)
	div  = divs(ichan)
        tit  = titSat+' UnCorr. TB '+unit+' @ Ch'+string(ichan+1,'(i2)')+' ('+titles(ichan)+') '+date +' '+nodetxt+vtxt
	if sfc lt 0 THEN $
          ind = where(utbs(*,ichan) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                and nodes eq node and sfcs ge 0, cnt) $
	else $
          ind = where(utbs(*,ichan) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                and nodes eq node and sfcs eq sfc, cnt)
	img_name = figsDir+prefix+yyyymmdd+'_tbu_'+titlesStr(ichan)+'_'+sfcId+'_'+nodeId+'.png'
        IF cnt gt 0 THEN BEGIN
	  map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,$
	         utbs(*,ichan),ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,$
	         xscale,yscale,tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
	ENDIF ELSE BEGIN
	  map_hr_blank,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,$
	         utbs(*,ichan),ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,$
	         xscale,yscale,tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
        ENDELSE
      ENDFOR
    ENDIF

    ;---- Emissivity ----
    IF (PLOT_EM eq 1) THEN BEGIN
      Channels2plot=indgen(Scene.nChan)

      chan_str = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20']
      titles   =['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2','57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
      titlesStr=['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2','57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
      divs=replicate(10,20)
      
      if ( satId eq 'npp' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22']
          titles    = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
          titlesStr = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
	  divs=replicate(10,22)
      endif
      if ( satId eq 'f16' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
          titles    = ['50v', '52v', '53v', '54v', '55v', '57rc', '59rc', '150h', '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
          titlesStr = ['50v', '52v', '53v', '54v', '55v', '57rc', '59rc', '150h', '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
	  divs=replicate(10,24)
      endif
      if ( satId eq 'f17' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
          titles    = ['50h', '52h', '53h', '54h', '55h', '57rc', '59rc', '150h', '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
          titlesStr = ['50h', '52h', '53h', '54h', '55h', '57rc', '59rc', '150h', '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
	  divs=replicate(10,24)
      endif
      if ( satId eq 'f18' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24']
          titles    = ['50h', '52h', '53h', '54h', '55h', '57rc', '59rc', '150h', '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
          titlesStr = ['50h', '52h', '53h', '54h', '55h', '57rc', '59rc', '150h', '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
	  divs=replicate(10,24)
      endif
      if ( satId eq 'amsre' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12']
          titles    = ['6.9v','6.9h','10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h']
          titlesStr = ['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ]
	  divs=replicate(10,12)
      endif
      if ( satId eq 'gcomw1' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14']
          titles    = ['6.9v','6.9h','7.3v','7.3h','10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h']
          titlesStr = ['6v','6h','7v','7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h' ]
	  divs=replicate(10,14)
      endif
      if ( satId eq 'trmm' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9']
          titles    = ['10.65v','10.65h','19.35v','19.35h','21.3v','37.0v','37.0h','85.5v','85.5h' ]  	
          titlesStr = ['11v','11h','19v','19h','21v','37v','37h','85v','85h' ]       
	  divs=replicate(10,9)
      endif
      if ( satId eq 'gpm' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9','10','11','12','13']
          titles    = ['10.65v','10.65h','18.70v','18.70h','23.8v','36.5v','36.5h','89.0v','89.0h', '165.5v','165.5h','183.31v1','183.31v2']
          titlesStr = ['11v','11h','19v','19h','24v','37v','37h','89v','89h','166v','166h','183v1','183v2'] 
	  divs=replicate(10,13)
      endif
      if ( satId eq 'fy3ri' ) then  begin
          chan_str  = ['1','2','3','4','5','6','7','8','9','10']
          titles    = ['10.7v','10.7h','18.7v','18.7h','23.8v','23.8h','36.5v','36.5h','89v','89h']
          titlesStr = ['11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ]
	  divs=replicate(10,10)
      endif

      IF sfc eq -1 THEN BEGIN ; all
          minvalues=[0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.60,0.65,0.65,0.65]
          maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]                
          if (satId eq 'f16') then begin
              minvalues=[0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.50,0.50,0.50, 0.50, 0.50, 0.50]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,0.70,0.70,0.70,0.70,0.70, 0.70, 0.70, 0.70]
          endif
          if (satId eq 'f17') then begin
              minvalues=[0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.50,0.50,0.50, 0.50, 0.50, 0.50]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,0.70,0.70,0.70,0.70,0.70, 0.70, 0.70, 0.70]
          endif
          if (satId eq 'f18') then begin
              minvalues=[0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.50,0.50,0.50, 0.50, 0.50, 0.50]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,0.70,0.70,0.70,0.70,0.70, 0.70, 0.70, 0.70]
          endif
          if (satId eq 'amsre') then begin
              minvalues=[0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65,0.65]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'gcomw1') then begin
              minvalues=[0.50,0.50,0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65,0.65]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'trmm') then begin
              minvalues=[0.50, 0.20, 0.55, 0.25, 0.50, 0.60, 0.25, 0.70, 0.40]
              maxvalues=[1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
          endif
          if (satId eq 'gpm') then begin
              minvalues=[0.50, 0.20, 0.55, 0.25, 0.50, 0.60, 0.25, 0.70, 0.40,  0.70,0.40,0.70,0.70]
              maxvalues=[1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,  1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'npp') then begin
              minvalues=[0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
          endif
      ENDIF
      IF sfc eq 0 THEN BEGIN ; sea
          minvalues=[0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.66]
          maxvalues=[0.55,0.60,0.70,0.70,0.65,0.65,0.70,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.75,0.75,0.80,0.75,0.80,0.84]
          if (satId eq 'f16') then begin
              minvalues=[0.3,0.3,0.3,0.60,0.50,  0.65,0.7,0.40,0.40,0.55,  0.35,0.3,0.3,0.3,0.3,  0.3,0.70,0.4,0.55,0.55,0.55,0.55,0.55,0.55]
              maxvalues=[0.6,0.6,0.6,0.80,0.75,  0.75,0.8,0.65,0.65,0.75,  0.65,0.6,0.6,0.6,0.6,  0.6,0.80,0.6,0.70,0.70,0.70,0.70,0.70,0.70]
          endif
          if (satId eq 'f17') then begin
              minvalues=[0.3,0.3,0.3,0.60,0.50,  0.65,0.7,0.40,0.40,0.55,  0.35,0.3,0.3,0.3,0.3,  0.3,0.70,0.4,0.55,0.55,0.55,0.55,0.55,0.55]
              maxvalues=[0.6,0.6,0.6,0.80,0.75,  0.75,0.8,0.65,0.65,0.75,  0.65,0.6,0.6,0.6,0.6,  0.6,0.80,0.6,0.70,0.70,0.70,0.70,0.70,0.70]
          endif
          if (satId eq 'f18') then begin
              minvalues=[0.3,0.3,0.3,0.60,0.50,  0.65,0.7,0.40,0.40,0.55,  0.35,0.3,0.3,0.3,0.3,  0.3,0.70,0.4,0.55,0.55,0.55,0.55,0.55,0.55]
              maxvalues=[0.6,0.6,0.6,0.80,0.75,  0.75,0.8,0.65,0.65,0.75,  0.65,0.6,0.6,0.6,0.6,  0.6,0.80,0.6,0.70,0.70,0.70,0.70,0.70,0.70]
          endif
          if (satId eq 'amsre') then begin
              minvalues=[0.56,0.24,0.55,0.25,0.58,0.25,  0.60,0.30,0.65,0.25,0.75,0.45]
              maxvalues=[0.60,0.32,0.63,0.35,0.68,0.45,  0.70,0.45,0.75,0.50,0.90,0.75]
          endif
          if (satId eq 'gcomw1') then begin
              minvalues=[0.56,0.24,0.56,0.24,0.55,0.25,0.58,0.25,  0.60,0.30,0.65,0.25,0.75,0.45]
              maxvalues=[0.60,0.32,0.60,0.32,0.63,0.35,0.68,0.45,  0.70,0.45,0.75,0.50,0.90,0.75]
          endif
          if (satId eq 'trmm') then begin
              minvalues=[0.50, 0.20, 0.55, 0.25, 0.55, 0.60, 0.25, 0.70, 0.40]
              maxvalues=[0.60, 0.40, 0.60, 0.45, 0.60, 0.65, 0.45, 0.80, 0.50]
          endif
          if (satId eq 'gpm') then begin
              minvalues=[0.50, 0.20, 0.55, 0.25, 0.55, 0.60, 0.25, 0.70, 0.40,  0.70,0.40,0.70,0.70]
              maxvalues=[0.60, 0.40, 0.60, 0.45, 0.60, 0.65, 0.45, 0.80, 0.50,  0.95,0.50,0.95,0.95]
          endif
          if (satId eq 'npp') then begin
              minvalues=[0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
              maxvalues=[0.55,0.60,0.70,0.70,0.65,0.65,0.65,0.70,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.75,0.80,0.75,0.80,0.85,0.85,0.85]
          endif
      ENDIF
      IF sfc eq 2 THEN BEGIN ; land
          minvalues=[0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80]
          maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
          if (satId eq 'f16') then begin
              minvalues=[0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.90,0.90,0.70,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'f17') then begin
              minvalues=[0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.90,0.90,0.70,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'f18') then begin
              minvalues=[0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.90,0.90,0.70,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'amsre') then begin
              minvalues=[0.70,0.65,0.70,0.65,0.70,0.65, 0.80,0.90,0.90,0.70,0.75,0.75]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00, 1.00,1.00,1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'gcomw1') then begin
              minvalues=[0.70,0.65,0.70,0.65,0.70,0.65,0.70,0.65, 0.80,0.90,0.90,0.70,0.75,0.75]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00, 1.00,1.00,1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'trmm') then begin
              minvalues=[0.80, 0.65, 0.80, 0.65, 0.80, 0.90, 0.70, 0.80, 0.75]
              maxvalues=[1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
          endif
          if (satId eq 'gpm') then begin
              minvalues=[0.80, 0.65, 0.80, 0.65, 0.80, 0.90, 0.70, 0.80, 0.75,  0.75,0.75,0.75,0.75]
              maxvalues=[1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,  1.00,1.00,1.00,1.00]
          endif
          if (satId eq 'npp') then begin
              minvalues=[0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
              maxvalues=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
          endif
      ENDIF
      
      minYs = minvalues
      maxYs = maxvalues
      unit = ''
      fmt  = '(f5.2)'
      isQc = 1
      FOR ichan=0,n_elements(Channels2plot)-1 DO BEGIN
	 minY = minYs(ichan)
	 maxY = maxYs(ichan)
	 div  = divs(ichan)
         tit  = titSat+' Emissivity @ Ch'+string(ichan+1,'(i2)')+' ('+titles(ichan)+') '+date+' '+nodetxt+vtxt
	 if sfc lt 0 THEN $
           ind = where(ems(*,ichan) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and nodes eq node and sfcs ge 0, cnt) $
	 else $
           ind = where(ems(*,ichan) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and nodes eq node and sfcs eq sfc, cnt)
	 IF cnt gt 0 THEN BEGIN
           img_name = figsDir+prefix+yyyymmdd+'_em_'+titlesStr(ichan)+'_'+sfcId+'_' +nodeId+'.png' 
	   map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,$
	          ems(*,ichan),ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,$
	          xscale,yscale,tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
         ENDIF
      ENDFOR
    ENDIF
    
    ;---- Temperature Layers ----
    IF (PLOT_TEMP eq 1) THEN BEGIN
      minYs=[180,200,210,220,230,240,250,270,270,270,275]
      maxYs=[215,230,260,270,280,285,290,300,300,300,305]
      unit = '(K)'
      fmt  = '(I3)'
      isQc = 1
      divs = [7,6,10,10,10,9,8,6,6,6,6]
      layers = [100,200,300,400,500,600,700,800,850,900,950]
      titles = ['100mb','200mb','300mb','400mb','500mb','600mb', '700mb','800mb','850mb','900mb','950mb']
      FOR ilay=0,n_elements(Lays2process)-1 DO BEGIN
        minY = minYs(ilay)
        maxY = maxYs(ilay)
	div  = divs(ilay)
        tit  = titSat+' Temperature '+unit+' @ '+titles(ilay)+' '+date+' '+nodetxt+vtxt
	if sfc lt 0 THEN $
          ind = where(temps(*,ilay) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                and nodes eq node and sfcs ge 0, cnt) $
	else $
          ind = where(temps(*,ilay) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                and nodes eq node and sfcs eq sfc, cnt)
	minY = minYs(ilay)
        maxY = maxYs(ilay)
        IF cnt gt 0 THEN BEGIN
	  img_name = figsDir+prefix+yyyymmdd+'_temp_'+titles(ilay)+'_'+sfcId+'_' +nodeId+'.png' 
	  map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,$
	         temps(*,ilay),ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,$
	         xscale,yscale,tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
        ENDIF
      ENDFOR
    ENDIF

    ;---- Water vapor layers ----
    IF (PLOT_WV eq 1) THEN BEGIN
      minYs = [0.0,   0.0, 0.0, 0.0, 0.0, 0.0,  0.0,  0.0,  0.0,  0.0,  0.0]
      maxYs = [0.01, 0.15, 1.0, 2.0, 3.5, 6.0, 12.0, 12.0, 14.0, 14.0, 16.0]
      unit = '(g/kg)'
      fmt  = '(f6.3)'
      isQc = 1
      divs = [10,10,10,10,7,6,6,6,7,7,8]
      layers = [100,200,300,400,500,600,700,800,850,900,950]
      titles = ['100mb','200mb','300mb','400mb','500mb','600mb','700mb','800mb','850mb','900mb','950mb']
      FOR ilay=0,n_elements(Lays2process)-1 DO BEGIN
        minY = minYs(ilay)
        maxY = maxYs(ilay)
	div  = divs(ilay)
        tit  = titSat+' Water Vapor '+unit+' @ '+titles(ilay)+' '+date+' '+nodetxt+vtxt
	if sfc lt 0 THEN $
          ind = where(wvs(*,ilay) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                and nodes eq node and sfcs ge 0, cnt) $
	else $
          ind = where(wvs(*,ilay) ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                and nodes eq node and sfcs eq sfc, cnt)
	IF cnt gt 0 THEN BEGIN
	  img_name = figsDir+prefix+yyyymmdd+'_wv_'+titles(ilay)+'_'+sfcId+'_' +nodeId+'.png' 
	  map_hr,latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,$
	         wvs(*,ilay),ind,nscanpos,qcs,isQc,fov_scale_cross,fov_scale_along,$
	         xscale,yscale,tit,unit,thck,ioverplot,fmt,div,img_name,map_dim,border,rotate_angle
        ENDIF
      ENDFOR
    ENDIF
    
    ;---- Polar Surface Type ----
    IF (PLOT_SFC_POLAR eq 1) THEN BEGIN
      ind = where(sfcs ge 0 and lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
            and nodes eq nodes2plot(iNode),cnt) 
      
      minY = 0
      maxY = 3
      div  = 3
      format='(I2)'
      sfcPick=2
      IF (cnt gt 0 and latmin gt 0) THEN BEGIN
   	centrlat = 90.0
  	orientlon = -80.0
  	latmin_ps = 0.0
  	latmax_ps = 90.0
	tit = titSat+' Northern Hemisphere Pre-Classified Sfc Type '+ date+' '+nodetxt+vtxt
	img_name = figsDir+prefix+yyyymmdd+'_sfc_pn_'+sfcId+'_' +nodeId+'.png'
  	plot_polar,sfcs(ind),sfcs(ind),img_name,minY,maxY,latmin_ps,latmax_ps,-180,180,centrlat,$
		   orientlon,lats(ind),lons(ind),tit,sfcPick,div,format
      ENDIF

      IF (cnt gt 0 and latmax lt 0) THEN BEGIN
   	centrlat = -90.0
  	orientlon = 0.0
  	latmin_ps = -90.0
  	latmax_ps = 0.0
	tit = titSat+' Southern Hemisphere Pre-Classified Sfc Type '+ date+' '+nodetxt+vtxt
	img_name = figsDir+prefix+yyyymmdd+'_sfc_ps_'+sfcId+'_' +nodeId+'.png'
  	plot_polar,sfcs(ind),sfcs(ind),img_name,minY,maxY,latmin_ps,latmax_ps,-180,180,centrlat,$
		   orientlon,lats(ind),lons(ind),tit,sfcPick,div,format
      ENDIF
    ENDIF

  ENDFOR  ;---- loop end for isfc
ENDFOR  ;---- loop end for iNode


END

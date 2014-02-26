@../../../setup/paths_idl.pro
;*******************************************************************************
;
; Name:  nongridMirs_dep
;
; Type:  IDL Program
;
;
; Description:
;   Program used to read in the contents of dep files
;   profile-by-profile, to store in arrays and plot various
;   preselected maps.
;
; Requirements:
;   MiRS IDL library
;
; History:
;   Kevin Garrett 2008-10-07  original coder
;   Wanchun Chen  2010-04-08  adjust fov in cross & along track direction
;*******************************************************************************

PRO NONGRIDMIRS_DEP, nameList=nameList
Consts,Consts

;---- Set Input ----
satId='npp'
date='2010-09-06'
figsDir='./img_dep/'
version='2349'

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


fileList='npp_dep_list'
resolution=1	; -1: coarse resolution, 0:low resolution, 1:high resolution

;---- read from namelist or not ---- 0: no, use above set; 1: yes, from namelist file
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

;---- Global ---- 
if (region eq 0) then regionStr='glb_'
;---- United States
if (region eq 1) then regionStr='us_'
;---- Europe ----
if (region eq 2) then regionStr='eu_'
;---- Gulf of Mexico
if (region eq 3) then regionStr='gulf_'
;---- West Coast ----
if (region eq 4) then regionStr='westcoast_'
;---- East China ----
if (region eq 5) then regionStr='china_'
;---- Conus ----
if (region eq 6) then regionStr='conus_'
;---- Tropical ----
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


;---- Read each DEP file header to get max profile number needed ----
readlist,fileList,listDEPs,nfile
if (nfile eq 0 ) then begin
    print, 'Error: No files found in :', fileList
    stop
endif

nList = nfile
nFovs = lonarr(nList)
mxFovs = 0L
FOR ifile=0,nList-1 DO BEGIN
  iTyp       = 0L
  AlgSN      = 0L
  nPrf       = 0L
  iu	     = 0L
  OPENR,iu,listDEPs(ifile),/get_lun,error=err,/f77_unformatted,/swap_if_little_endian
  IF (err ne 0) THEN BEGIN
      print ,!ERROR_STATE.MSG
      RETURN
  ENDIF
  readu,iu,iTyp,AlgSN
  readu,iu,nPrf
  close,iu
  free_lun,iu
  nFovs(ifile)=nPrf
  mxFovs = mxFovs+nPrf
ENDFOR

;---- arrays to hold variables ----
lats     = make_array(mxFovs,/float,value=-999.)
lons     = make_array(mxFovs,/float,value=-999.)
nodes    = make_array(mxFovs,/integer,value=-999)
scans    = make_array(mxFovs,/integer,value=-999)
qcs      = make_array(mxFovs,/integer,value=-999)
sfcs     = make_array(mxFovs,/integer,value=-999)
chisqs   = make_array(mxFovs,/float,value=-999.)
clws     = make_array(mxFovs,/float,value=-999.)
gss      = make_array(mxFovs,/float,value=-999.)
gwps     = make_array(mxFovs,/float,value=-999.)
lwps     = make_array(mxFovs,/float,value=-999.)
rwps     = make_array(mxFovs,/float,value=-999.)
sics     = make_array(mxFovs,/float,value=-999.)
sicfys   = make_array(mxFovs,/float,value=-999.)
sicmys   = make_array(mxFovs,/float,value=-999.)
snows    = make_array(mxFovs,/float,value=-999.)
swes     = make_array(mxFovs,/float,value=-999.)
rrs      = make_array(mxFovs,/float,value=-999.)
tpws     = make_array(mxFovs,/float,value=-999.)


;------------------------------------------------------------------------------
;  Read data (DEP) and store in arrays
;------------------------------------------------------------------------------
cnt=0L
FOR ifile=0L,nList-1 DO BEGIN
    print,''
    print,'ifile=',ifile
    print,'DEP file=',listDEPs[ifile]
    
    readDEP,listDEPs[ifile],nFovs(ifile),Dep
    IF (DEP.nprofsprocessed ge 1) THEN BEGIN
	step = DEP.nprofsprocessed - 1
	lats(cnt:cnt+step)     = Dep.lat(0:step)
        lons(cnt:cnt+step)     = Dep.lon(0:step)
        nodes(cnt:cnt+step)    = Dep.node(0:step)
        scans(cnt:cnt+step)    = Dep.iScanPos(0:step)
        qcs(cnt:cnt+step)      = Dep.qc(0,0:step)
        chisqs(cnt:cnt+step)   = Dep.ChiSq(0:step)
	clws(cnt:cnt+step)     = Dep.CLW(0:step)
	gss(cnt:cnt+step)      = Dep.SnowGS(0:step)
        gwps(cnt:cnt+step)     = Dep.GWP(0:step)
        lwps(cnt:cnt+step)     = Dep.LWP(0:step)
        rwps(cnt:cnt+step)     = Dep.RWP(0:step)
        sfcs(cnt:cnt+step)     = Dep.iTypSfc(0:step)
	sics(cnt:cnt+step)     = Dep.SIC(0:step)
	sicfys(cnt:cnt+step)   = Dep.SIC_FY(0:step)
	sicmys(cnt:cnt+step)   = Dep.SIC_MY(0:step)
	snows(cnt:cnt+step)    = Dep.SnowCover(0:step)
	swes(cnt:cnt+step)     = Dep.Swe(0:step)
        rrs(cnt:cnt+step)      = Dep.RR(0:step)
	tpws(cnt:cnt+step)     = Dep.TPW(0:step)
        cnt=cnt+step+1L
    ENDIF
ENDFOR


;------------------------------------------------------------------------------
;  Plotting Setup
;------------------------------------------------------------------------------
PLOT_CLW	= 1
PLOT_GS 	= 1
PLOT_GWP	= 1
PLOT_LWP	= 1
PLOT_RWP	= 1
PLOT_SFC2	= 1
PLOT_SIC	= 1
PLOT_SICFY	= 1
PLOT_SICMY	= 1
PLOT_SNOW	= 1
PLOT_SWE	= 1
PLOT_RR 	= 1
PLOT_TPW	= 1

PLOT_SFC2_POLAR = 1
PLOT_SIC_POLAR  = 1
PLOT_SWE_POLAR  = 1


if( satId eq Consts.STR_SATID_AQUA or satId eq 'trmm' or satId eq 'gpm' or satId eq 'trmm2a12' ) then begin
  PLOT_CLW     = 1
  PLOT_GS      = 0
  PLOT_GWP     = 1
  PLOT_LWP     = 1
  PLOT_RWP     = 1
  PLOT_SFC2    = 0
  PLOT_SIC     = 0
  PLOT_SICFY   = 0
  PLOT_SICMY   = 0
  PLOT_SNOW    = 0
  PLOT_SWE     = 0
  PLOT_RR      = 1
  PLOT_TPW     = 1
  PLOT_SFC2_POLAR = 0
  PLOT_SIC_POLAR  = 0
  PLOT_SWE_POLAR  = 0
endif 

if satId eq 'trmm2a12' then begin
  PLOT_TPW     = 0
endif

nodeIds	          = ['as','ds']
nodeTitles	  = ['Asc','Des']
nodes2plot	  = [0,1] ;0/1/2 for asc/des/both
nNode	          = n_elements(nodes2plot)

sfcIds	          = ['all','sea','lnd']
sfcs2plot	  = [-1,0,2] ;-1/0/1/2/3 for all/ocean/seaice/land/snow/etc
nSfc              = n_elements(sfcs2plot)

chisqThresh	  = 5

ioverplot=0
xscale = 0.085
yscale = 0.075

;---- FOV size, used to plot different size of symbols in different scan position ----
NUMSPOT_M=90L

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

;---- default map pixels ----
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


;----  IFOV 9.1 x 9.1 km (lr) or 4.6 x 4.6 km (hr) ----
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
  ;xscale = 0.045
  ;yscale = 0.045
  ;map_dim = [ LONG(lonspan*!DTOR*6371.0*cos(latavg*!DTOR)), LONG(latspan*!DTOR*6371.0) ] 
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
;  Begin Plotting
;------------------------------------------------------------------------------

IF( satId eq Consts.STR_SATID_AQUA ) THEN satId = 'amsre'

vtxt = ' (r'+version+')'
SAT = STRUPCASE(satId)
if satId eq 'trmm' then SAT = 'TRMM/TMI'
if satId eq 'gpm'  then SAT = 'GPM/GMI'
if satId eq 'npp'  then SAT = 'NPP/ATMS'
if satId eq 'trmm2a12' then SAT = 'TRMM_2A12'

titSat='MIRS '+SAT
if satId eq 'trmm2a12' then titSat = SAT

;---- plot a border over symbol or not. 1-yes, 0-no ----
border = 0
isQc = 1

FOR iNode=0,nNode-1 DO BEGIN
 
  node = nodes2plot(iNode)
  nodeId = nodeIds(iNode)
  nodetxt = nodeTitles(iNode)
  rotate_angle = rotate_angles(iNode)
  
  FOR iSfc=0,nSfc-1 DO BEGIN
 
    sfc = sfcs2plot(iSfc)
    sfcId = sfcIds(iSfc)
    
    ;---- TPW ----
    IF (PLOT_TPW eq 1) THEN BEGIN            
      minY = 0
      maxY = 70
      div  = 14
      fmt  = '(I2)'
      unit = '(mm)'
      tit  = titSat+' TPW '+unit+' '+date+' '+nodetxt+vtxt
      color_table_index = 41
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and tpws ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and tpws ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_tpw_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,tpws,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle,color_table_index=color_table_index
      ENDIF
    ENDIF

    ;---- Post-Process Surface Type ----
    IF (PLOT_SFC2 eq 1) THEN BEGIN
      minY = 0
      maxY = 3
      div  = 3
      fmt  = '(I2)'
      unit = ''
      tit  = titSat+' Post-Processed Sfc Type '+date+' '+nodetxt+vtxt
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and sfcs ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and sfcs ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_sfc2_cyl_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,sfcs,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Cloud Liquid Water ----
    IF (PLOT_CLW eq 1) THEN BEGIN
      minY = 0.0
      maxY = 0.7
      div  = 7
      fmt  = '(F5.2)'
      unit = '(mm)'
      color_table_index = 41
      tit = titSat+' Cloud Liquid Water '+unit+' '+date+' '+nodetxt+vtxt
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and clws ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and clws ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name=figsDir+prefix+yyyymmdd+'_clw_'+sfcId+'_'+nodeId+ '.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,clws,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle,color_table_index=color_table_index
      ENDIF
    ENDIF

    ;---- GS ----
    IF (PLOT_GS eq 1) THEN BEGIN
      minY = 0.3
      maxY = 0.7
      div  = 4
      fmt  = '(F4.1)'
      unit = '(mm)'
      tit  = titSat+' Snow Grain Size Radius '+unit+' '+date+' '+nodetxt+vtxt
      color_table_index = 41
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and gss ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and gss ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_gs_cyl_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,gss,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- GWP ----
    IF (PLOT_GWP eq 1) THEN BEGIN
      minY = 0.0
      maxY = 0.70
      div  = 7
      fmt  = '(F5.2)'
      unit = '(mm)'
      tit  = titSat+' Graupel Water Path '+unit+' '+date+' '+nodetxt+vtxt
      color_table_index = 41
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and gwps ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and gwps ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_iwp_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,gwps,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle,color_table_index=color_table_index
      ENDIF
    ENDIF

    ;---- Liquid Water Path ----
    IF (PLOT_LWP eq 1) THEN BEGIN
      minY = 0.0
      maxY = 0.70
      div  = 7
      fmt  = '(F5.2)'
      unit = '(mm)'
      tit  = titSat+' Liquid Water Path '+unit+' '+date+' '+nodetxt+vtxt
      color_table_index = 41
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and lwps ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and lwps ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
 	img_name = figsDir+prefix+yyyymmdd+'_lwp_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,lwps,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle,color_table_index=color_table_index
      ENDIF
    ENDIF

    ;---- Rain Water Path ----
    IF (PLOT_RWP eq 1) THEN BEGIN
      minY = 0.0
      maxY = 0.50
      div  = 5
      fmt  = '(F5.2)'
      unit = '(mm)'
      tit  = titSat+' Rain Water Path '+unit+' '+date+' '+nodetxt+vtxt
      color_table_index = 41
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and rwps ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and rwps ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_rwp_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,rwps,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle,color_table_index=color_table_index
      ENDIF
    ENDIF

    ;---- Sea Ice Concentration ----
    IF (PLOT_SIC eq 1) THEN BEGIN
      minY = 0
      maxY = 100
      div  = 10
      fmt  = '(I3)'
      unit = '(%)'
      tit  = titSat+' Sea Ice Concentration '+unit+' '+date+' '+nodetxt+vtxt
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and sics ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and sics ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_sice_cyl_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,sics,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Sea Ice First Year ----
    IF (PLOT_SICFY eq 1) THEN BEGIN
      minY = 0
      maxY = 100
      div  = 10
      fmt  = '(I3)'
      unit = '(%)'
      tit  = titSat+' First Year Sea Ice Concentration '+unit+' '+date+' '+nodetxt+vtxt
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and sicfys ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and sicfys ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_sicefy_cyl_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,sicfys,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Sea Ice Multiple Years ----
    IF (PLOT_SICMY eq 1) THEN BEGIN
      minY = 0
      maxY = 100
      div  = 10
      fmt  = '(I3)'
      unit = '(%)'
      tit = titSat+' Multiple Years Sea Ice Concentration '+unit+' '+date+' '+nodetxt+vtxt
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and sicmys ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and sicmys ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_sicemy_cyl_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,sicmys,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Snow Cover ----
    IF (PLOT_SNOW eq 1) THEN BEGIN
      minY = 0
      maxY = 1
      div  = 1
      fmt  = '(I1)'
      unit = ''
      tit = titSat+' Snow Cover '+date+' '+nodetxt+vtxt
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and snows ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and snows ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_snow_cyl_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,snows,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Snow Water Equivalent ----
    IF (PLOT_SWE eq 1) THEN BEGIN
      minY = 0
      maxY = 8
      div  = 8
      fmt  = '(I2)'
      unit = '(cm)'
      tit  = titSat+' Snow Water Equivalent '+unit+' '+date+' '+nodetxt+vtxt
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and swes ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and swes ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_swe_cyl_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,swes,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle
      ENDIF
    ENDIF

    ;---- Rain Rate ----
    IF (PLOT_RR eq 1) THEN BEGIN
      minY = 0.0
      maxY = 10.0
      div  = 5
      fmt  = '(F4.1)'
      unit = '(mm/hr)'
      tit  = titSat+' Rain Rate '+unit+' '+date+' '+nodetxt+vtxt
      color_table_index = 41
      if sfc lt 0 then $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and rrs ge 0 and nodes eq node, cnt) $
      else $
        ind = where( lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
                 and rrs ge 0 and nodes eq node and sfcs eq sfc, cnt)
      IF cnt gt 0 THEN BEGIN
        img_name = figsDir+prefix+yyyymmdd+'_rr_'+sfcId+'_' +nodeId+'.png'
        map_hr, latmin,latmax,lonmin,lonmax,minY,maxY,lats,lons,scans,rrs,ind,nscanpos,qcs,isQc,$
	        fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thck,ioverplot,fmt,div,$
		img_name,map_dim,border,rotate_angle,color_table_index=color_table_index
      ENDIF
    ENDIF

    ;---- Polar Post-process Surface Type ----
    IF (PLOT_SFC2_POLAR eq 1) THEN BEGIN
      ind = where(lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and sfcs ge 0 and nodes eq node,cnt) 
      minY = 0
      maxY = 3
      div  = 3
      fmt  = '(I2)'
      sfcPick = 2
      IF (cnt gt 0 and latmin gt 0) THEN BEGIN
        centrlat  = 90.0
        orientlon = -80.0
        latmin_ps = 0.0
        latmax_ps = 90.0
        tit = titSat+' Northern Hemisphere Post-Processed Sfc Type '+date+' '+nodetxt+vtxt
        img_name = figsDir+prefix+yyyymmdd+'_sfc2_pn_'+sfcId+'_' +nodeId+'.png'
        plot_polar,sfcs(ind),sfcs(ind),img_name,minY,maxY,latmin_ps,latmax_ps,-180,180,centrlat,$
                   orientlon,lats(ind),lons(ind),tit,sfcPick,div,fmt
      ENDIF
      IF (cnt gt 0 and latmax lt 0) THEN BEGIN
        centrlat = -90.0
        orientlon = 0.0
        latmin_ps = -90.0
        latmax_ps = 0.0
        tit = titSat+' Southern Hemisphere Post-Processed Sfc Type '+date+' '+nodetxt+vtxt
        img_name = figsDir+prefix+yyyymmdd+'_sfc2_ps_'+sfcId+'_' +nodeId+'.png'
        plot_polar,sfcs(ind),sfcs(ind),img_name,minY,maxY,latmin_ps,latmax_ps,-180,180,centrlat,$
                   orientlon,lats(ind),lons(ind),tit,sfcPick,div,fmt
      ENDIF
    ENDIF

    ;---- Polar Sea Ice ----
    IF( PLOT_SIC_POLAR eq 1) THEN BEGIN
      indSfc = where(sfcs eq 0)
      ind = where(lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and sics ge 0 and nodes eq node,cnt) 
      minY = 0
      maxY = 100
      div  = 10
      fmt  = '(I3)'
      sfcPick = 0
      IF (cnt gt 0 and latmin gt 0) THEN BEGIN
   	centrlat = 90.0
  	orientlon = -80.0
  	latmin_ps = 0.0
  	latmax_ps = 90.0
	tit = titSat+' Northern Hemisphere Sea Ice Concentration '+date+' '+nodetxt+vtxt
	img_name = figsDir+prefix+yyyymmdd+'_sice_pn_sea_' +nodeId+'.png'
  	plot_polar,sics(ind),sics(ind),img_name,minY,maxY,latmin_ps,latmax_ps,-180,180,centrlat,$
	           orientlon,lats(ind),lons(ind),tit,sfcPick,div,fmt
      ENDIF
      IF (cnt gt 0 and latmax lt 0) THEN BEGIN
   	centrlat = -90.0
  	orientlon = 0.0
  	latmin_ps = -90.0
  	latmax_ps = 0.0
	tit = titSat+' Southern Hemisphere Sea Ice Concentration '+date+' '+nodetxt+vtxt
	img_name = figsDir+prefix+yyyymmdd+'_sice_ps_sea_' +nodeId+'.png'
  	plot_polar,sics(ind),sics(ind),img_name,minY,maxY,latmin_ps,latmax_ps,-180,180,centrlat,$
	           orientlon,lats(ind),lons(ind),tit,sfcPick,div,fmt
      ENDIF
    ENDIF

    ;---- Polar SWE Cover ----
    IF (PLOT_SWE_POLAR eq 1) THEN BEGIN
      indSfc = where(sfcs eq 2)
      ind = where(lats ge latmin and lats le latmax and lons ge lonmin and lons le lonmax $
              and swes ge 0 and nodes eq node,cnt) 
      minY = 0
      maxY = 8
      div  = 8
      fmt  = '(I2)'
      sfcPick = 1
      IF (cnt gt 0 and latmin gt 0) THEN BEGIN
   	centrlat = 90.0
  	orientlon = -80.0
  	latmin_ps = 0.0
  	latmax_ps = 90.0
	tit = titSat+' Northern Hemisphere Snow Water Equivalent(cm) '+date+' '+nodetxt+vtxt
	img_name = figsDir+prefix+yyyymmdd+'_swe_pn_lnd_' +nodeId+'.png'
  	plot_polar,swes(ind),swes(ind),img_name,minY,maxY,latmin_ps,latmax_ps,-180,180,centrlat,$
	           orientlon,lats(ind),lons(ind),tit,sfcPick,div,fmt
      ENDIF
      IF (cnt gt 0 and latmax lt 0) THEN BEGIN
   	centrlat = -90.0
  	orientlon = 0.0
  	latmin_ps = -90.0
  	latmax_ps = 0.0
	tit = titSat+' Southern Hemisphere Snow Water Equivalent(cm) '+date+' '+nodetxt+vtxt
	img_name = figsDir+prefix+yyyymmdd+'_swe_ps_lnd_' +nodeId+'.png'
  	plot_polar,swes(ind),swes(ind),img_name,minY,maxY,latmin_ps,latmax_ps,-180,180,centrlat,$
	           orientlon,lats(ind),lons(ind),tit,sfcPick,div,fmt
      ENDIF
    ENDIF

  ENDFOR
ENDFOR

END

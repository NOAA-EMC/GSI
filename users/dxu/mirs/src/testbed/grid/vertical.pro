@../../../setup/paths_idl.pro
;***************************************************************************************************
;
;  Purpose:
;    To Plot gridded vertical section data set into png images.
;
;  Dependence:
;    vertical_cross.pro
;    plot_vert.pro
;
;  Usg: Need satId and isMirs to be combined to decide which data set to plot.
;
;  if satId is POES/DMSP/NPP, then isMirs should be 0/1/2/3(sensor itself or collocated)
;  isMirs = 0  MIRS itslef, isMirs = 1/2/3 --> nwp collocated data set(gdas/ecmwf/gfs)
;  isMirs = 0, gridded data filename like: GRID_n18_20091118_wv_as.dat
;  isMirs = 1, gridded data filename like: GRID_n18_nwp_gdas_20091118_wv_as.dat
;
; if satId is trmm_2a12, then isMirs must be set as 4. ( trmm_2a12 not collocated )
;
;  Record of revisions:
;       Date            Programmer     	Description of change
;    ------------    --------------    -------------------------------------
;     09/01/07         Wanchun Chen     Original Code
;     03/25/09         Wanchun Chen     Added 3 more hydrological parameters
;     04/02/09         Wanchun Chen     Plot rain and graupel in one page
;     04/08/09         Wanchun Chen     Moved singe product plot into sub vertical_cross
;     09/30/09         Wanchun Chen     Added TRMM_2A12
;     11/19/09         Wanchun Chen     Incorporate verticalNwp into this code.
;***************************************************************************************************

Pro Vertical, namelist=namelist
Consts, Consts

;***************************************************************
;    some constants definitions
;***************************************************************
satId='n19'
gridfactor=4
gridDir='/disk1/pub/archive/grid/n19_amsua_mhs/2013-01-10/'
figsDir='./img/'
date='2013-01-10'
processMode=1
version='3249'
latmin=-90.0
latmax=90.0
lonmin=-180.0
lonmax=180
isMirs=0

readFromList=1
if readFromList eq 1 then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId		       ;Satellite ID
  readf,iu,format='(i)', gridfactor	       ;gridfactor
  readf,iu,format='(a)', gridDir	       ;gridded data dir
  readf,iu,format='(a)', figsDir  	       ;path where to put image files
  readf,iu,format='(a)', date		       ;file extension of image
  readf,iu,format='(i)', processMode           ;process Mode ( 0-orbit, 1-daily )
  readf,iu,format='(a)', version	       ;version
  readf,iu,format='(f)', latmin                ;min lat
  readf,iu,format='(f)', latmax                ;max lat
  readf,iu,format='(f)', lonmin                ;min lon
  readf,iu,format='(f)', lonmax                ;max lon
  readf,iu,format='(i)', isMirs                ;0-mirs,1-gdas,2-ecmwf,3-gfs,4-trmm_2a12
  free_lun,iu
endif

print, 'vertical.pro ... ' + satId

;---- default to 4 ----
step_glb=40
step_us=4
if gridfactor eq 2 then begin
  step_glb=20
  step_us=2
endif

;---- data source 
if isMirs eq 0 then nwpId=''
if isMirs eq 1 then nwpId='_gdas'
if isMirs eq 2 then nwpId='_ecmwf'
if isMirs eq 3 then nwpId='_gfs'
if isMirs eq 4 then nwpId=''

if isMirs eq 0 then nwpTitle=''
if isMirs eq 1 then nwpTitle='GDAS Collo. '
if isMirs eq 2 then nwpTitle='ECMWF Collo. '
if isMirs eq 3 then nwpTitle='GFS Collo. '
if isMirs eq 4 then nwpTitle=''


prefix='mirs_adv_poes_'+satId+nwpId+'_vert_glb'
prefix_us='mirs_adv_poes_'+satId+nwpId+'_vert_us'

if ( satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F17 or satId eq Consts.STR_SATID_F18 ) then begin
  prefix='mirs_adv_dmsp_'+satId+nwpId+'_vert_glb'
  prefix_us='mirs_adv_dmsp_'+satId+nwpId+'_vert_us'
endif

if ( satId eq Consts.STR_SATID_AQUA  or $
     satId eq Consts.STR_SATID_FY3RI or $
     satId eq Consts.STR_SATID_GCOMW1 ) then begin
  prefix='mirs_adv_eos_'+satId+nwpId+'_vert_glb'
  prefix_us='mirs_adv_eos_'+satId+nwpId+'_vert_us'
endif

if ( satId eq Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_GPM ) then begin
  prefix='mirs_adv_eos_'+satId+nwpId+'_vert_glb'
  prefix_us='mirs_adv_eos_'+satId+nwpId+'_vert_us'
endif

if ( satId eq Consts.STR_SATID_NPP ) then begin
  prefix='mirs_adv_npoess_'+satId+nwpId+'_vert_glb'
  prefix_us='mirs_adv_npoess_'+satId+nwpId+'_vert_us'
endif

if ( satId eq 'trmm_2a12' ) then begin
  prefix='mirs_'+satId+nwpId+'_vert_glb'
  prefix_us='mirs_'+satId+nwpId+'_vert_us'
endif

; NB: these subregions are not applicable for MT which are not covered by
; low-inclination orbit (plotting flags will be disabled below)
if ( satId eq Consts.STR_SATID_MTMA or satId eq Consts.STR_SATID_MTSA ) then begin
  prefix='mirs_adv_mt_'+satId+nwpId+'_vert_glb'
  prefix_us='mirs_adv_mt_'+satId+nwpId+'_vert_us'
endif

if ( processMode eq 0 ) then yyyymmdd=date
if ( processMode eq 1 ) then yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)


satTxt='MIRS '+strupcase(satId)
if satId eq Consts.STR_SATID_AQUA   then satTxt='MIRS '+'AMSR-E'
if satId eq Consts.STR_SATID_GCOMW1 then satTxt='MIRS '+'GCOMW1/AMSR2'
if satId eq Consts.STR_SATID_FY3RI  then satTxt='MIRS '+'FY3/MWRI'
if satId eq Consts.STR_SATID_TRMM   then satTxt='MIRS '+'TRMM/TMI'
if satId eq Consts.STR_SATID_GPM    then satTxt='MIRS '+'GPM/GMI'
if satId eq Consts.STR_SATID_MTMA   then satTxt='MIRS '+'MT/MADRAS'
if satId eq Consts.STR_SATID_MTSA   then satTxt='MIRS '+'MT/SAPHIR'

NCOL=360*gridfactor
NROW=180*gridfactor
NLAY=100
NCEND=2

RAIN_CUT = 0.001
ICE_CUT = 0.001

DEGREE0 = 273.15
DEGREE1 = 270.0
DEGREE2 = 276.0

cends=['as','ds']
cendtxts=[' Asc ',' Des ']


;***************************************************************
;    flgs to turn on/off plot
;***************************************************************
PLOT_TEMP=1
PLOT_WV=1
PLOT_CLWP=1
PLOT_RAINP=1
PLOT_GRAUPELP=1
PLOT_RAIN_GRAUPEL=1

;---- TRMM_2A12 has no temp/wv profile
if ( satId eq 'trmm2a12' ) then begin
  PLOT_TEMP=0
  PLOT_WV=0
endif

;---- GDAS and GFS has no hydrological parameters
if isMirs eq 1 or isMirs eq 3 then begin
  PLOT_CLWP=0
  PLOT_RAINP=0
  PLOT_GRAUPELP=0
  PLOT_RAIN_GRAUPEL=0
endif

if ( satId eq Consts.STR_SATID_AQUA ) then begin
  PLOT_TEMP=0
  PLOT_WV=0
  PLOT_CLWP=0
  PLOT_RAINP=0
  PLOT_GRAUPELP=1
  PLOT_RAIN_GRAUPEL=0
endif

; For now, do not plot for MT
;---- MT MADRAS 
if ( satId eq 'mtma' ) then begin
  PLOT_TEMP=0
  PLOT_WV=0
  PLOT_CLWP=0
  PLOT_RAINP=0
  PLOT_GRAUPELP=0
  PLOT_RAIN_GRAUPEL=0
endif

;---- MT SAPHIR 
if ( satId eq 'mtsa' ) then begin
  PLOT_TEMP=0
  PLOT_WV=0
  PLOT_CLWP=0
  PLOT_RAINP=0
  PLOT_GRAUPELP=0
  PLOT_RAIN_GRAUPEL=0
endif


;***************************************************************
;   read in surface pressure
;***************************************************************
psfcs=fltarr(NCOL,NROW,NCEND)
psfc=fltarr(NCOL,NROW)
for icend = 0, 1 do begin
  openr, lun, gridDir+'GRID_'+satId+'_'+yyyymmdd+'_psfc_'+cends(icend)+'.dat',/get_lun,/Swap_Endian
  readu, lun, psfc
  psfcs(*,*,icend) = psfc(*,*)
  free_lun,lun
endfor

;---- fix gridVertical error including qc fail into non-mirs products
if isMirs ne 0 then begin
  ss = where( FIX(psfcs) eq -99, cnt)
  if cnt ge 1 then psfcs(ss) = -999.0
endif


;***************************************************************
;   temp
;***************************************************************
IF( PLOT_TEMP EQ 1 ) THEN BEGIN
  
  prod_name = 'temp'
  div = 6
  fmt='(I3)'
  minval=150
  maxval=300

  for icend = 0, 1 do begin
    psfc=psfcs(*,*,icend)
    vertical_cross, NCOL,NROW,NLAY, gridfactor, gridDir, figsDir, satId, yyyymmdd, date, $
                    prod_name, cends(icend), psfc, div, fmt, minval, maxval, isMirs
  endfor

ENDIF


;***************************************************************
;   wv 
;***************************************************************
IF( PLOT_WV EQ 1 ) THEN BEGIN

  prod_name = 'wv'
  div = 8
  fmt='(I2)'
  minval=0
  maxval=16

  for icend = 0, 1 do begin
    psfc=psfcs(*,*,icend)
    vertical_cross, NCOL,NROW,NLAY, gridfactor, gridDir, figsDir, satId, yyyymmdd, date, $
                    prod_name, cends(icend), psfc, div, fmt, minval, maxval, isMirs
  endfor

ENDIF


;***************************************************************
;   clwp
;***************************************************************
IF PLOT_CLWP EQ 1 THEN BEGIN

  prod_name = 'clwp'
  div=4
  fmt='(F5.2)'
  minval=0.0
  maxval=0.2

  for icend = 0, 1 do begin
    psfc=psfcs(*,*,icend)
    vertical_cross, NCOL,NROW,NLAY, gridfactor, gridDir, figsDir, satId, yyyymmdd, date, $
                    prod_name, cends(icend), psfc, div, fmt, minval, maxval, isMirs
  endfor

ENDIF


;***************************************************************
;   rainp
;***************************************************************
IF PLOT_RAINP EQ 1 THEN BEGIN

  prod_name = 'rainp'
  div=5
  fmt='(F5.2)'
  minval=0.0
  maxval=0.1

  for icend = 0, 1 do begin
    psfc=psfcs(*,*,icend)
    vertical_cross, NCOL,NROW,NLAY, gridfactor, gridDir, figsDir, satId, yyyymmdd, date, $
                    prod_name, cends(icend), psfc, div, fmt, minval, maxval, isMirs
  endfor

ENDIF


;***************************************************************
;   graupelp
;***************************************************************
IF PLOT_GRAUPELP EQ 1 THEN BEGIN

  prod_name = 'graupelp'
  div=5
  fmt='(F5.2)'
  minval=0.0
  maxval=0.1

  for icend = 0, 1 do begin
    psfc=psfcs(*,*,icend)
    vertical_cross, NCOL,NROW,NLAY, gridfactor, gridDir, figsDir, satId, yyyymmdd, date, $
                    prod_name, cends(icend), psfc, div, fmt, minval, maxval, isMirs
  endfor

ENDIF



;*************************************************************************************************
;
;    Plot rain and graupel into the same page
;
;*************************************************************************************************
color_table = 41

IF( PLOT_RAIN_GRAUPEL EQ 1 ) THEN BEGIN
for icend = 0, 1 do begin
  
  cend = cends(icend)   
  cendtxt = cendtxts(icend)
  psfc = psfcs(*,*,icend)
  NLAY_START= 43 ; starting layer where we have valid hydrometeo data, about 100mb

  ;*****************************************************************
  ; read in temperature and pressure to get freezing level if it's MIRS
  ;*****************************************************************
  freezing_levels=fltarr(NCOL,NROW)
  freezing_levels(*,*) = -999.0

  tmp = fltarr(NCOL,NROW)

  if isMirs eq 0 then begin
  
    temps = fltarr(NCOL,NROW,NLAY)
    openr, lun, gridDir + 'GRID_'+satId+'_'+yyyymmdd+'_temp_'+cends(icend)+'.dat', /get_lun, /Swap_Endian
    for ilay=0, NLAY-1 do begin
      readu, lun, tmp
      temps(*,*,ilay) = tmp(*,*)
    endfor
    free_lun,lun

    pressures = fltarr(NCOL,NROW,NLAY,NCEND)
    openr, lun, gridDir + 'GRID_'+satId+'_'+yyyymmdd+'_pressure_'+cends(icend)+'.dat', /get_lun, /Swap_Endian
    for ilay=0, NLAY-1 do begin
      readu, lun, tmp
      pressures(*,*,ilay) = tmp(*,*)
    endfor
    free_lun,lun

    for irow = 0, NROW-1 do begin
    for icol = 0, NCOL-1 do begin

      sum_pressure=0.0
      cnt_pressure=0.0

      ;---- we only have valid values of rain and graupuel from 100mb -- index 43 ( 0.01mb = index 1 )
      for ilay = NLAY_START, NLAY-1 do begin
	if temps(icol,irow,ilay) GE DEGREE1 and temps(icol,irow,ilay) LE DEGREE2 then begin
          sum_pressure = sum_pressure + pressures(icol,irow,ilay)
	  cnt_pressure = cnt_pressure + 1.0
	endif 
      endfor

      if( cnt_pressure ge 1.0 ) then freezing_levels(icol,irow) = sum_pressure/cnt_pressure

    endfor
    endfor

    ;---- release resources to free up memory
    UNDEFINE, temps
    UNDEFINE, pressures
 
  endif
  
  ;***************************************************************
  ;   read in rainp   
  ;***************************************************************
  rainps = fltarr(NCOL,NROW,NLAY)
  openr, lun, gridDir+'GRID_'+satId+'_'+yyyymmdd+'_rainp_'+cend+'.dat',/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    rainps(*,*,ilay) = tmp(*,*)
  endfor
  free_lun,lun

  ;---- rain top level
  raintop_levels=intarr(NCOL,NROW)
  raintop_levels(*,*) = -99
  
  for irow = 0, NROW-1 do begin
  for icol = 0, NCOL-1 do begin
    for ilay = NLAY-1, NLAY_START, -1 do begin
      if rainps(icol,irow,ilay) ge RAIN_CUT then raintop_levels(icol,irow) = ilay
    endfor
  endfor
  endfor
  
  ;***************************************************************
  ;   read in graupelp  
  ;***************************************************************
  graupelps = fltarr(NCOL,NROW,NLAY)
  openr, lun, gridDir+'GRID_'+satId+'_'+yyyymmdd+'_graupelp_'+cend+'.dat',/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    graupelps(*,*,ilay) = tmp(*,*)
  endfor
  free_lun,lun
  
  ;---- fix gridVertical error to include qc fail into non-mirs products
  ;---- only mirs( isMirs == 0 ) has qc failed values(-99.0)
  if isMirs ne 0 then begin
    ss = where( FIX(rainps) eq -99, cnt)
    if cnt ge 1 then rainps(ss) = -999.0
    ss = where( FIX(graupelps) eq -99, cnt)
    if cnt ge 1 then graupelps(ss) = -999.0
  endif

  ;---- pick out graupel bottom lvele, and overlay graupel onto rain profile
  ;---- so rainps will be contain both rain and ice 
  icebottom_levels=intarr(NCOL,NROW)
  icebottom_levels(*,*) = -99
  
  for irow = 0, NROW-1 do begin
  for icol = 0, NCOL-1 do begin
    for ilay = NLAY_START, NLAY-1 do begin
      if graupelps(icol,irow,ilay) ge ICE_CUT then icebottom_levels(icol,irow) = ilay

      if graupelps(icol,irow,ilay) ge ICE_CUT then begin
        if rainps(icol,irow,ilay) lt RAIN_CUT then begin
	  rainps(icol,irow,ilay) = graupelps(icol,irow,ilay)
        endif else begin
	  rainps(icol,irow,ilay) = rainps(icol,irow,ilay) + graupelps(icol,irow,ilay)
	endelse
      endif
      
    endfor
  endfor
  endfor

  ;***************************************************************
  ;    mark rain top and ice bottom level
  ;***************************************************************
  for irow = 0, NROW-1 do begin
  for icol = 0, NCOL-1 do begin
    if raintop_levels(icol,irow)   ge 0 then rainps(icol,irow,raintop_levels(icol,irow))   = -66.0
    if icebottom_levels(icol,irow) ge 0 then rainps(icol,irow,icebottom_levels(icol,irow)) = -77.0
  endfor
  endfor
 
 
  ;***************************************************************
  ;   reverse vertical-direction
  ;***************************************************************
  dummy1=rainps
  for ilay=0, NLAY-1 do begin
    rainps(*,*,ilay) = dummy1(*,*,NLAY-1-ilay)  
  endfor
  UNDEFINE, dummy1
  
  yval=[1085,100]
  yrange=[1085,100]
  ytickname=['1085','890','718','565','430','320','230','150','100']
  yticks=N_elements(ytickname)-1
  ytitle='Pressure (MB)'
 
  div=4
  fmt='(F5.2)'
  minval=0.0
  maxval=0.1
  deg=String(37B)
  
  ;---- only need those layers below 100mb(including 100mb)
  index_100 = 43
  NLAY_RAINP = NLAY - index_100

  ;---- Globe --------------------------------------------------------------------------------------
  step=step_glb
  
  ;---- lat fixed ----
  xval=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
  xrange=[-180,180]
  xtitle='Longitude'
  xticks=N_elements(xval)-1
  lons = -180.0 + findgen(NCOL)/gridfactor

  for ilat=0, NROW-1, step do begin
    latval = ( ilat * 1.0 / gridfactor ) - 90.0
    latstr = strtrim(string(fix(latval)),2)
    title = nwpTitle+satTxt+' Rain & Graupel (mm) on latitude '+latstr +'!9'+deg+'!X'+cendtxt+date
    map_name = figsDir + prefix + '_rain_graupel_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
    sb=fltarr(360*gridfactor,NLAY_RAINP)
    for ilay = 0, NLAY_RAINP - 1 do begin
      sb(*,ilay) = rainps(*,ilat,ilay)
    endfor
    plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
       		lons,psfc(*,ilat),freezing_levels(*,ilat),raintop_levels(*,ilat),icebottom_levels(*,ilat),color_table,isMirs
  endfor

  ilat=NROW
  latval = ( ilat * 1.0 / gridfactor ) - 90.0
  latstr = strtrim(string(fix(latval)),2)
  title = nwpTitle+satTxt+' Rain & Graupel (mm) on latitude '+latstr +'!9'+deg+'!X'+cendtxt+date
  map_name = figsDir + prefix + '_rain_graupel_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
  sb=fltarr(360*gridfactor,NLAY_RAINP)
  for ilay = 0, NLAY_RAINP - 1 do begin
    sb(*,ilay) = rainps(*,ilat-1,ilay)
  endfor
  plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
   	      lons,psfc(*,ilat-1),freezing_levels(*,ilat-1),raintop_levels(*,ilat-1),icebottom_levels(*,ilat-1),color_table,isMirs

  ;---- lon fixed ----
  xval=[-90,-60,-30,0,30,60,90]
  xrange=[-90,90]
  xtitle='Latitude'
  xticks=N_elements(xval)-1
  lats = -90 + findgen(NROW)/gridfactor

  for ilon=0, NCOL-1, step  do begin
    lonval = ( ilon * 1.0 / gridfactor ) - 180.0
    lonstr = strtrim(string(fix(lonval)),2)
    title = nwpTitle+satTxt+' Rain & Graupel (mm) on longitude '+lonstr +'!9'+deg+'!X'+cendtxt+date
    map_name = figsDir + prefix + '_rain_graupel_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
    sb=fltarr(180*gridfactor,NLAY_RAINP)
    for ilay = 0, NLAY_RAINP - 1 do begin
      sb(*,ilay) = rainps(ilon,*,ilay)
    endfor
    plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
      		lats,psfc(ilon,*),freezing_levels(ilon,*),raintop_levels(ilon,*),icebottom_levels(ilon,*),color_table,isMirs
  endfor

  ilon=NCOL
  lonval = ( ilon * 1.0 / gridfactor ) - 180.0
  lonstr = strtrim(string(fix(lonval)),2)
  title = nwpTitle+satTxt+' Rain & Graupel (mm) on longitude '+lonstr +'!9'+deg+'!X'+cendtxt+date
  map_name = figsDir + prefix + '_rain_graupel_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
  sb=fltarr(180*gridfactor,NLAY_RAINP)
  for ilay = 0, NLAY_RAINP - 1 do begin
    sb(*,ilay) = rainps(ilon-1,*,ilay)
  endfor
  plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
    	      lats,psfc(ilon-1,*),freezing_levels(ilon-1,*),raintop_levels(ilon-1,*),icebottom_levels(ilon-1,*),color_table,isMirs


  ;---- USA ----------------------------------------------------------------------------------------
  step=step_us

  lat_min=20.0
  lat_max=55.0
  lon_min=-130.0
  lon_max=-60.0

  ilat_bot=(lat_min+90)*gridfactor
  ilat_top=(lat_max+90)*gridfactor

  ilon_left=(lon_min+180)*gridfactor
  ilon_right=(lon_max+180)*gridfactor

  ;---- lat fixed ----
  xval = fix(lon_min) + indgen(8)*10
  xrange=[lon_min,lon_max]
  xtitle='Longitude'
  xticks=N_elements(xval)-1
  lons = lon_min + findgen(ilon_right-ilon_left)/gridfactor

  for ilat=ilat_bot,  ilat_top, step do begin
    latval = ( ilat * 1.0 / gridfactor ) - 90.0
    latstr = strtrim(string(fix(latval)),2)
    title = nwpTitle+satTxt+' Rain & Graupel (mm) on latitude '+latstr +'!9'+deg+'!X'+cendtxt+date
    map_name = figsDir + prefix_us + '_rain_graupel_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
    sb=fltarr((ilon_right-ilon_left),NLAY_RAINP)
    for ilay=0, NLAY_RAINP - 1 do begin
    for ilon=0, ilon_right-ilon_left-1 do begin
      sb(ilon,ilay) = rainps(ilon+ilon_left,ilat,ilay)
    endfor
    endfor
    plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
		lons,psfc(ilon_left:ilon_right-1,ilat),freezing_levels(ilon_left:ilon_right-1,ilat),$
		raintop_levels(ilon_left:ilon_right-1,ilat),icebottom_levels(ilon_left:ilon_right-1,ilat),color_table,isMirs
  endfor

  ;---- lon fixed ----------
  xval=fix(lat_min) + indgen(8)*5
  xrange=[lat_min,lat_max]
  xtitle='Latitude'
  xticks=N_elements(xval)-1
  lats = lat_min + findgen(ilat_top-ilat_bot)/gridfactor

  for ilon=ilon_left, ilon_right, step  do begin
    lonval = ( ilon * 1.0 / gridfactor ) - 180.0
    lonstr = strtrim(string(fix(lonval)),2)
    title = nwpTitle+satTxt+' Rain & Graupel (mm) on longitude '+lonstr +'!9'+deg+'!X'+cendtxt+date
    map_name = figsDir + prefix_us + '_rain_graupel_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
    sb=fltarr((ilat_top-ilat_bot),NLAY_RAINP)
    for ilay=0, NLAY_RAINP - 1 do begin
    for ilat=0, ilat_top-ilat_bot-1 do begin
      sb(ilat,ilay) = rainps(ilon,ilat+ilat_bot,ilay)
    endfor
    endfor
    plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
		lats,psfc(ilon,ilat_bot:ilat_top-1),freezing_levels(ilon,ilat_bot:ilat_top-1),$
		raintop_levels(ilon,ilat_bot:ilat_top-1),icebottom_levels(ilon,ilat_bot:ilat_top-1),color_table,isMirs
  endfor
      

  UNDEFINE, rainps
  UNDEFINE, graupelps
  
endfor 
ENDIF


END


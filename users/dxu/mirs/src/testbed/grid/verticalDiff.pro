@../../../setup/paths_idl.pro

;===================================================================================================
;
;  Purpose:
;    To plot difference of two gridded vertical section data sets into png images.
;
;  Dependence:
;    vertical_cross_diff.pro
;    plot_vert_diff.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ============    ==============    =======================
;     11/19/2009       Wanchun Chen     Original Code
;
;===================================================================================================
Pro VerticalDiff, namelist=namelist
Consts, Consts

;***************************************************************
;    some constants definitions
;***************************************************************
satId1='metopA'
satId2='trmm2a12'
gridfactor=4
gridDir1='/disk1/pub/archive/grid/metopA_amsua_mhs/2013-01-10/'
gridDir2='/disk1/pub/archive/grid/trmm2a12/2013-01-10/'
figsDir='./img/'
date='2013-01-10'
version='3249'
latmin=-90
latmax=90
lonmin=-180
lonmax=180

readFromList=1
if readFromList eq 1 then begin
  openr,iu,namelist,/get_lun
  readf,iu,format='(a)', satId1        ;Satellite ID 1
  readf,iu,format='(a)', satId2        ;Satellite ID 2
  readf,iu,format='(i)', gridfactor    ;gridfactor
  readf,iu,format='(a)', gridDir1      ;gridded data dir 1
  readf,iu,format='(a)', gridDir2      ;gridded data dir 2
  readf,iu,format='(a)', figsDir       ;path where to put image files
  readf,iu,format='(a)', date          ;file extension of image
  readf,iu,format='(a)', version       ;Svn version control number
  readf,iu,format='(f)', latmin        ;min lat
  readf,iu,format='(f)', latmax        ;max lat
  readf,iu,format='(f)', lonmin        ;min lon
  readf,iu,format='(f)', lonmax        ;max lon
  free_lun,iu
endif

print, 'verticalDiff.pro ... ' + satId1 + ' vs ' + satId2 + ' ' + date

refId=satId2
refTitle=STRUPCASE(satId2)

if satId2 eq 'gdas' or satId2 eq 'ecmwf' or satId2 eq 'gfs' then begin
  refId=satId1+'_'+satId2
  refTitle='Collo. '+STRUPCASE(satId2)
endif

prefix='mirs_adv_poes_'+satId1+'_'+satId2+'_diff_vert_glb'
prefix_us='mirs_adv_poes_'+satId1+'_'+satId2+'_diff_vert_us'

if ( satId1 eq Consts.STR_SATID_F16 or satId1 eq Consts.STR_SATID_F17 or satId1 eq Consts.STR_SATID_F18 ) then begin
  prefix='mirs_adv_dmsp_'+satId1+'_'+satId2+'_diff_vert_glb'
  prefix_us='mirs_adv_dmsp_'+satId1+'_'+satId2+'_diff_vert_us'
endif

if ( satId1 eq Consts.STR_SATID_AQUA or satId1 eq Consts.STR_SATID_FY3RI or satId1 eq Consts.STR_SATID_GCOMW1 ) then begin
  prefix='mirs_adv_eos_'+satId1+'_'+satId2+'_diff_vert_glb'
  prefix_us='mirs_adv_eos_'+satId1+'_'+satId2+'_diff_vert_us'
endif

if ( satId1 eq Consts.STR_SATID_TRMM or satId1 eq Consts.STR_SATID_GPM ) then begin
  prefix='mirs_adv_eos_'+satId1+'_'+satId2+'_diff_vert_glb'
  prefix_us='mirs_adv_eos_'+satId1+'_'+satId2+'_diff_vert_us'
endif

if ( satId1 eq Consts.STR_SATID_NPP ) then begin
  prefix='mirs_adv_npoess_'+satId1+'_'+satId2+'_diff_vert_glb'
  prefix_us='mirs_adv_npoess_'+satId1+'_'+satId2+'_diff_vert_us'
endif


yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

NLAY=100
NCEND=2

step_glb=40
step_us=4

if gridfactor eq 2 then begin
  step_glb=20
  step_us=2
endif


titleSatId=strupcase(satId1)
if ( satId1 eq Consts.STR_SATID_AQUA   ) then titleSatId='AMSR-E'
if ( satId1 eq Consts.STR_SATID_GCOMW1 ) then titleSatId='GCOMW1/AMSR2'
if ( satId1 eq Consts.STR_SATID_FY3RI  ) then titleSatId='FY3/MWRI'
if ( satId1 eq Consts.STR_SATID_NPP    ) then titleSatId='NPP/ATMS'

NCOL=360*gridfactor
NROW=180*gridfactor

RAIN_CUT=0.001
ICE_CUT=0.001

DEGREE0=273.15
DEGREE1=270.0
DEGREE2=276.0

cends=['as','ds']
cendtxts=[' Asc ',' Des ']

psfcs=fltarr(NCOL,NROW,NCEND)


;***************************************************************
;    some flgs to turn on/off to plot or not
;***************************************************************
PLOT_TEMP=1
PLOT_WV=1
PLOT_CLW=1
PLOT_RAIN=1
PLOT_ICE=1
PLOT_RAIN_ICE=1

;---- GDAS only has temp and wv ----
if satId2 eq 'gdas' then begin
  PLOT_CLW=0
  PLOT_RAIN=0
  PLOT_ICE=0
  PLOT_RAIN_ICE=0
endif

;---- TRMM_2A12 has no temp and wv
if satId2 eq 'trmm2a12' then begin
  PLOT_TEMP=0
  PLOT_WV=0
endif


;***************************************************************
;    read in surface pressure - we use data set 1
;***************************************************************
psfc=fltarr(NCOL,NROW)
for icend=0, 1 do begin
  openr, lun, gridDir1+'GRID_'+satId1+'_'+yyyymmdd+'_psfc_'+cends(icend)+'.dat',/get_lun,/Swap_Endian
  readu, lun, psfc
  psfcs(*,*,icend)=psfc(*,*)
  free_lun,lun
endfor


;***************************************************************
;    temp
;***************************************************************
IF PLOT_TEMP EQ 1 THEN BEGIN

  prod_id='temp'
  div=10
  fmt='(I3)'
  minval=-10
  maxval=10

  for icend=0, 1 do begin
    psfc=psfcs(*,*,icend)
    cend=cends(icend)
    vertical_cross_diff, NCOL,NROW,NLAY,gridfactor,satId1,satId2,gridDir1,gridDir2,figsDir,$
                         yyyymmdd,date,prod_id,cend,psfc,div,fmt,minval,maxval
  endfor

ENDIF


;***************************************************************
;    wv
;***************************************************************
IF PLOT_WV EQ 1 THEN BEGIN

  prod_id='wv'
  div=10
  fmt='(I2)'
  minval=-5
  maxval=5

  for icend=0, 1 do begin
    psfc=psfcs(*,*,icend)
    cend=cends(icend)
    vertical_cross_diff, NCOL,NROW,NLAY,gridfactor,satId1,satId2,gridDir1,gridDir2,figsDir,$
                         yyyymmdd,date,prod_id,cend,psfc,div,fmt,minval,maxval
  endfor

ENDIF


;***************************************************************
;    clwp
;***************************************************************
IF PLOT_CLW EQ 1 THEN BEGIN

  prod_id='clwp'
  div=5
  fmt='(F5.2)'
  minval=-0.1
  maxval=0.1

  for icend=0, 1 do begin
    psfc=psfcs(*,*,icend)
    cend=cends(icend)
    vertical_cross_diff, NCOL,NROW,NLAY,gridfactor,satId1,satId2,gridDir1,gridDir2,figsDir,$
                         yyyymmdd,date,prod_id,cend,psfc,div,fmt,minval,maxval
  endfor

ENDIF


;***************************************************************
;    rainp
;***************************************************************
IF PLOT_RAIN EQ 1 THEN BEGIN

  prod_id='rainp'
  div=5
  fmt='(F5.2)'
  minval=-0.1
  maxval=0.1

  for icend=0, 1 do begin
    psfc=psfcs(*,*,icend)
    cend=cends(icend)
    vertical_cross_diff, NCOL,NROW,NLAY,gridfactor,satId1,satId2,gridDir1,gridDir2,figsDir,$
                         yyyymmdd,date,prod_id,cend,psfc,div,fmt,minval,maxval
  endfor

ENDIF


;***************************************************************
;    graupelp
;***************************************************************
IF PLOT_ICE EQ 1 THEN BEGIN

  prod_id='graupelp'
  div=5
  fmt='(F5.2)'
  minval=-0.1
  maxval=0.1

  for icend=0, 1 do begin
    psfc=psfcs(*,*,icend)
    cend=cends(icend)
    vertical_cross_diff, NCOL,NROW,NLAY,gridfactor,satId1,satId2,gridDir1,gridDir2,figsDir,$
                         yyyymmdd,date,prod_id,cend,psfc,div,fmt,minval,maxval
  endfor

ENDIF



;*************************************************************************************************
;
;    Plot rain and graupel into the same page
;
;*************************************************************************************************
color_table=45
isMirs=4 ;---- difference not include any qc failed points

IF( PLOT_RAIN_ICE EQ 1 ) THEN BEGIN
for icend=0, 1 do begin
  
  cend=cends(icend)   
  cendtxt=cendtxts(icend)
  psfc=psfcs(*,*,icend)
  NLAY_START= 43 ; starting layer where we have valid hydrometeo data, about 100mb

  ;***********************************************************************
  ; read in temperature and pressure to get freezing level from data 1 set
  ;***********************************************************************
  freezing_levels=fltarr(NCOL,NROW)
  freezing_levels(*,*)=-999.0
  
  file_temp = gridDir1+'GRID_'+satId1+'_'+yyyymmdd+'_temp_'+cends(icend)+'.dat'
  if FILE_TEST(file_temp) eq 0 then begin
    print, 'Not exist:'+file_temp
    goto,HERE
  endif
  
  tmp=fltarr(NCOL,NROW)
  
  temps=fltarr(NCOL,NROW,NLAY)
  openr, lun, file_temp, /get_lun, /Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    temps(*,*,ilay)=tmp(*,*)
  endfor
  free_lun,lun
  
  file_pressure = gridDir1+'GRID_'+satId1+'_'+yyyymmdd+'_pressure_'+cends(icend)+'.dat'
  if FILE_TEST(file_pressure) eq 0 then begin
    print, 'Not exist:'+file_pressure
    goto,HERE
  endif
  
  pressures=fltarr(NCOL,NROW,NLAY,NCEND)
  openr, lun, file_pressure, /get_lun, /Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    pressures(*,*,ilay)=tmp(*,*)
  endfor
  free_lun,lun

  for irow=0, NROW-1 do begin
  for icol=0, NCOL-1 do begin

    sum_pressure=0.0
    cnt_pressure=0.0

    ;---- we only have valid values of rain and graupuel from 100mb -- index 43 ( 0.01mb=index 1 )
    for ilay=NLAY_START, NLAY-1 do begin
      if temps(icol,irow,ilay) GE DEGREE1 and temps(icol,irow,ilay) LE DEGREE2 then begin
  	sum_pressure=sum_pressure+pressures(icol,irow,ilay)
        cnt_pressure=cnt_pressure+1.0
      endif 
    endfor

    if( cnt_pressure ge 1.0 ) then freezing_levels(icol,irow)=sum_pressure/cnt_pressure

  endfor
  endfor

  ;---- release resources to free up memory
  UNDEFINE, temps
  UNDEFINE, pressures
  
  ;***************************************************************
  ;   read in rainp 1   
  ;***************************************************************
  file_rainp1 = gridDir1+'GRID_'+satId1+'_'+yyyymmdd+'_rainp_'+cend+'.dat'
  if FILE_TEST(file_rainp1) eq 0 then begin
    print, 'Not exist:'+file_rainp1
    goto,HERE
  endif
  
  rainps1=fltarr(NCOL,NROW,NLAY)
  openr, lun, file_rainp1,/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    rainps1(*,*,ilay)=tmp(*,*)
  endfor
  free_lun,lun

  ;---- rain top level
  raintop_levels=intarr(NCOL,NROW)
  raintop_levels(*,*)=-999
  
  for ilay=NLAY-1, NLAY_START, -1 do begin
  for irow=0, NROW-1 do begin
  for icol=0, NCOL-1 do begin
      if rainps1(icol,irow,ilay) ge RAIN_CUT then raintop_levels(icol,irow)=ilay
  endfor
  endfor
  endfor
  
  ;***************************************************************
  ;   read in graupelp 1  
  ;***************************************************************
  file_graupelp1 = gridDir1+'GRID_'+satId1+'_'+yyyymmdd+'_graupelp_'+cend+'.dat'
  if FILE_TEST(file_graupelp1) eq 0 then begin
    print, 'Not exist:'+file_graupelp1
    goto,HERE
  endif
  
  graupelps1=fltarr(NCOL,NROW,NLAY)
  openr, lun, file_graupelp1,/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    graupelps1(*,*,ilay)=tmp(*,*)
  endfor
  free_lun,lun
  
  ;---- fix gridVertical error to include qc fail into non-mirs products
  ss=where( FIX(rainps1) eq -99, cnt)
  if cnt ge 1 then rainps1(ss)=-999.0
  ss=where( FIX(graupelps1) eq -99, cnt)
  if cnt ge 1 then graupelps1(ss)=-999.0

  ;---- pick out graupel bottom lvele, and overlay graupel onto rain profile
  ;---- so rainps will be contain both rain and ice 
  icebottom_levels=intarr(NCOL,NROW)
  icebottom_levels(*,*)=-999
  
  for ilay=NLAY_START, NLAY-1 do begin
  for irow=0, NROW-1 do begin
  for icol=0, NCOL-1 do begin
      if graupelps1(icol,irow,ilay) ge ICE_CUT then icebottom_levels(icol,irow)=ilay
  endfor
  endfor
  endfor

  ;***************************************************************
  ;    mark rain top and ice bottom level
  ;***************************************************************
  ;for irow=0, NROW-1 do begin
  ;for icol=0, NCOL-1 do begin
  ;  if raintop_levels(icol,irow)   ge 0 then rainps1(icol,irow,raintop_levels(icol,irow))   = -66.0
  ;  if icebottom_levels(icol,irow) ge 0 then rainps1(icol,irow,icebottom_levels(icol,irow)) = -77.0
  ;endfor
  ;endfor
 
  ;***************************************************************
  ;    combine rain & ice from data set 1
  ;***************************************************************
  for ilay=NLAY_START, NLAY-1 do begin
  for irow=0, NROW-1 do begin
  for icol=0, NCOL-1 do begin
    if graupelps1(icol,irow,ilay) ge ICE_CUT then begin
      if rainps1(icol,irow,ilay) lt RAIN_CUT then begin
        rainps1(icol,irow,ilay)=graupelps1(icol,irow,ilay)
      endif else begin
        rainps1(icol,irow,ilay)=rainps1(icol,irow,ilay)+graupelps1(icol,irow,ilay)
      endelse
    endif
  endfor
  endfor
  endfor

      
  ;***************************************************************
  ;   read in rainp 2   
  ;***************************************************************
  file_rainp2 = gridDir2+'GRID_'+refId+'_'+yyyymmdd+'_rainp_'+cend+'.dat'
  if FILE_TEST(file_rainp2) eq 0 then begin
    print, 'Not exist:'+file_rainp2
    goto, HERE
  endif
  
  rainps2=fltarr(NCOL,NROW,NLAY)
  openr, lun, file_rainp2,/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    rainps2(*,*,ilay)=tmp(*,*)
  endfor
  free_lun,lun

  ;***************************************************************
  ;   read in graupelp 2  
  ;***************************************************************
  file_graupelp2 = gridDir2+'GRID_'+refId+'_'+yyyymmdd+'_graupelp_'+cend+'.dat'
  if FILE_TEST(file_graupelp2) eq 0 then begin
    print, 'Not exist:'+file_graupelp2
    goto, HERE
  endif
  
  graupelps2=fltarr(NCOL,NROW,NLAY)
  openr, lun, file_graupelp2,/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    graupelps2(*,*,ilay)=tmp(*,*)
  endfor
  free_lun,lun
  
  ;---- fix gridVertical error to include qc fail into non-mirs products
  ss=where( FIX(rainps2) eq -99, cnt)
  if cnt ge 1 then rainps2(ss)=-999.0
  ss=where( FIX(graupelps2) eq -99, cnt)
  if cnt ge 1 then graupelps2(ss)=-999.0

  ;***************************************************************
  ;    combine rain & ice from data set 2
  ;***************************************************************
  for ilay=NLAY_START, NLAY-1 do begin
  for irow=0, NROW-1 do begin
  for icol=0, NCOL-1 do begin
    if graupelps2(icol,irow,ilay) ge ICE_CUT then begin
      if rainps2(icol,irow,ilay) lt RAIN_CUT then begin
        rainps2(icol,irow,ilay)=graupelps2(icol,irow,ilay)
      endif else begin
        rainps2(icol,irow,ilay)=rainps2(icol,irow,ilay)+graupelps2(icol,irow,ilay)
      endelse
    endif
  endfor
  endfor
  endfor

  ;***************************************************************
  ;    take difference of two data sets
  ;***************************************************************
  rainps=fltarr(NCOL,NROW,NLAY) & rainps(*,*,*)=-999.0
  for ilay=NLAY_START, NLAY-1 do begin
  for irow=0, NROW-1 do begin
  for icol=0, NCOL-1 do begin
    if rainps1(icol,irow,ilay) gt -99 and rainps2(icol,irow,ilay) gt -99 then $
    rainps(icol,irow,ilay)=rainps1(icol,irow,ilay) - rainps2(icol,irow,ilay)
  endfor
  endfor
  endfor


  ;***************************************************************
  ;    mark rain top and ice bottom level
  ;***************************************************************
  for irow=0, NROW-1 do begin
  for icol=0, NCOL-1 do begin
    if raintop_levels(icol,irow)   ge 0 then rainps(icol,irow,raintop_levels(icol,irow))   = -66.0
    if icebottom_levels(icol,irow) ge 0 then rainps(icol,irow,icebottom_levels(icol,irow)) = -77.0
  endfor
  endfor
 

  UNDEFINE, rainps1
  UNDEFINE, rainps2
  UNDEFINE, graupelps1
  UNDEFINE, graupelps2

  ;***************************************************************
  ;   reverse vertical-direction
  ;***************************************************************
  dummy1=rainps
  for ilay=0, NLAY-1 do begin
    rainps(*,*,ilay)=dummy1(*,*,NLAY-1-ilay)  
  endfor
  UNDEFINE, dummy1
  
  yval=[1085,100]
  yrange=[1085,100]
  ytickname=['1085','890','718','565','430','320','230','150','100']
  yticks=N_elements(ytickname)-1
  ytitle='Pressure (MB)'
 
  div=5
  fmt='(F5.2)'
  minval=-0.1
  maxval=0.1
  deg=String(37B)
  
  ;---- only need those layers below 100mb(including 100mb)
  index_100=43
  NLAY_RAINP=NLAY - index_100

  ;---- Globe --------------------------------------------------------------------------------------
  step=step_glb

  ;---- lat fixed ----
  xval=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
  xrange=[-180,180]
  xtitle='Longitude'
  xticks=N_elements(xval)-1
  lons=-180.0+findgen(NCOL)/gridfactor

  for ilat=0, NROW-1, step do begin
    latval=( ilat * 1.0 / gridfactor ) - 90.0
    latstr=strtrim(string(fix(latval)),2)
    title='MIRS '+titleSatId+' - '+refTitle+' Rain & Graupel (mm) on latitude '+latstr +'!9'+deg+'!X'+cendtxt+date
    map_name=figsDir+prefix+'_rain_graupel_'+yyyymmdd+'_lat_'+latstr+'_'+cend+'.png'
    sb=fltarr(360*gridfactor,NLAY_RAINP)
    for ilay=0, NLAY_RAINP - 1 do begin
      sb(*,ilay)=rainps(*,ilat,ilay)
    endfor
    plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
       		lons,psfc(*,ilat),freezing_levels(*,ilat),raintop_levels(*,ilat),icebottom_levels(*,ilat),color_table,isMirs
  endfor

  ilat=NROW
  latval=( ilat * 1.0 / gridfactor ) - 90.0
  ;latstr=strtrim(string(fix(latval)),2)
  latstr=strtrim(string(round(latval)),2)
  title='MIRS '+titleSatId+' - '+refTitle+' Rain & Graupel (mm) on latitude '+latstr +'!9'+deg+'!X'+cendtxt+date
  map_name=figsDir+prefix+'_rain_graupel_'+yyyymmdd+'_lat_'+latstr+'_'+cend+'.png'
  sb=fltarr(360*gridfactor,NLAY_RAINP)
  for ilay=0, NLAY_RAINP - 1 do begin
    sb(*,ilay)=rainps(*,ilat-1,ilay)
  endfor
  plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
   	      lons,psfc(*,ilat-1),freezing_levels(*,ilat-1),raintop_levels(*,ilat-1),icebottom_levels(*,ilat-1),color_table,isMirs

  ;---- lon fixed ----
  xval=[-90,-60,-30,0,30,60,90]
  xrange=[-90,90]
  xtitle='Latitude'
  xticks=N_elements(xval)-1
  lats=-90+findgen(NROW)/gridfactor

  for ilon=0, NCOL-1, step  do begin
    lonval=( ilon * 1.0 / gridfactor ) - 180.0
    lonstr=strtrim(string(fix(lonval)),2)
    title='MIRS '+titleSatId+' - '+refTitle+' Rain & Graupel (mm) on longitude '+lonstr +'!9'+deg+'!X'+cendtxt+date
    map_name=figsDir+prefix+'_rain_graupel_'+yyyymmdd+'_lon_'+lonstr+'_'+cend+'.png'
    sb=fltarr(180*gridfactor,NLAY_RAINP)
    for ilay=0, NLAY_RAINP - 1 do begin
      sb(*,ilay)=rainps(ilon,*,ilay)
    endfor
    plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
      		lats,psfc(ilon,*),freezing_levels(ilon,*),raintop_levels(ilon,*),icebottom_levels(ilon,*),color_table,isMirs
  endfor

  ilon=NCOL
  lonval=( ilon * 1.0 / gridfactor ) - 180.0
  ;lonstr=strtrim(string(fix(lonval)),2)
  lonstr=strtrim(string(round(lonval)),2)
  title='MIRS '+titleSatId+' - '+refTitle+' Rain & Graupel (mm) on longitude '+lonstr +'!9'+deg+'!X'+cendtxt+date
  map_name=figsDir+prefix+'_rain_graupel_'+yyyymmdd+'_lon_'+lonstr+'_'+cend+'.png'
  sb=fltarr(180*gridfactor,NLAY_RAINP)
  for ilay=0, NLAY_RAINP - 1 do begin
    sb(*,ilay)=rainps(ilon-1,*,ilay)
  endfor
  plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
    	      lats,psfc(ilon-1,*),freezing_levels(ilon-1,*),raintop_levels(ilon-1,*),icebottom_levels(ilon-1,*),color_table,isMirs


  ;---- USA -----------------------------------------------------------------------------
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
  lons=lon_min+findgen(ilon_right-ilon_left)/gridfactor

  for ilat=ilat_bot,  ilat_top-1+step, step do begin
    latval=( ilat * 1.0 / gridfactor ) - 90.0
    latstr=strtrim(string(fix(latval)),2)
    title='MIRS '+titleSatId+' - '+refTitle+' Rain & Graupel (mm) on latitude '+latstr +'!9'+deg+'!X'+cendtxt+date
    map_name=figsDir+prefix_us+'_rain_graupel_'+yyyymmdd+'_lat_'+latstr+'_'+cend+'.png'
    sb=fltarr((ilon_right-ilon_left),NLAY_RAINP)
    for ilay=0, NLAY_RAINP - 1 do begin
    for ilon=0, ilon_right-ilon_left-1 do begin
      sb(ilon,ilay)=rainps(ilon+ilon_left,ilat,ilay)
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
  lats=lat_min+findgen(ilat_top-ilat_bot)/gridfactor

  for ilon=ilon_left, ilon_right-1+step, step  do begin
    lonval=( ilon * 1.0 / gridfactor ) - 180.0
    lonstr=strtrim(string(fix(lonval)),2)
    title='MIRS '+titleSatId+' - '+refTitle+' Rain & Graupel (mm) on longitude '+lonstr +'!9'+deg+'!X'+cendtxt+date
    map_name=figsDir+prefix_us+'_rain_graupel_'+yyyymmdd+'_lon_'+lonstr+'_'+cend+'.png'
    sb=fltarr((ilat_top-ilat_bot),NLAY_RAINP)
    for ilay=0, NLAY_RAINP - 1 do begin
    for ilat=0, ilat_top-ilat_bot-1 do begin
      sb(ilat,ilay)=rainps(ilon,ilat+ilat_bot,ilay)
    endfor
    endfor
    plot_vert2, sb,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minval,maxval,xticks,div,fmt,yticks,ytickname,$
		lats,psfc(ilon,ilat_bot:ilat_top-1),freezing_levels(ilon,ilat_bot:ilat_top-1),$
		raintop_levels(ilon,ilat_bot:ilat_top-1),icebottom_levels(ilon,ilat_bot:ilat_top-1),color_table,isMirs
  endfor

      
  UNDEFINE, rainps
  
endfor 
ENDIF

HERE:

End


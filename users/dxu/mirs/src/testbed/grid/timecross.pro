;***********************************************************************************************
; To plot time cross section of MIRS-TRMM_2A12
;
; 02/08/2011         Wanchun Chen       Original Coder
;
;***********************************************************************************************

@../../../setup/paths_idl.pro

Pro timecross, namelist=namelist

satId1='n18'
satId2='trmm2a12'
gridDir1='/net/orbit082l/disk3/pub/mirs/grid/n18_amsua_mhs/2011-02-11/'
gridDir2='/disk1/pub/wchen/mirs_ifort_linux_x64/data/TestbedData/Outputs/grid/trmm2a12/2011-02-11/'
figsDir='./img/'
gridfactor=4
date='2011-02-11'
version='2504'

readFromList=1
if readFromList eq 1 then begin
  openr, lun, namelist, /get_lun
  readf,lun,format='(a)', satId1		;Satellite ID 1
  readf,lun,format='(a)', satId2		;Satellite ID 2
  readf,lun,format='(a)', gridDir1	       	;gridded data dir 1
  readf,lun,format='(a)', gridDir2	       	;gridded data dir 2
  readf,lun,format='(a)', figsDir  	       	;path where to put image files
  readf,lun,format='(i)', gridfactor	       	;gridfactor
  readf,lun,format='(a)', date		       	;file extension of image
  readf,lun,format='(a)', version	       	;version
  free_lun,lun
endif


NCOL=360 * gridfactor 
NROW=180 * gridfactor  

;---- default to 4 ----
step_glb=40
step_gulf=4
step_china=4
if gridfactor eq 2 then begin
  step_glb=20
  step_gulf=2
  step_china=2
endif

yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)
yyyy=strmid(date,0,4)
mm=strmid(date,5,2)
dd=strmid(date,8,2)

year=FIX(yyyy)
month=FIX(mm)
day=FIX(dd)

cal2jday, year, month, day, jday
print, year, month, day, jday 

cends=['as','ds']
cendtxts=[' Asc ',' Des ']
satname=STRUPCASE(satId1)


;---- parameters related to data set 1 ----
prefix1='mirs_adv_poes_'+satId1+'_vert_glb'
prefix_gulf1='mirs_adv_poes_'+satId1+'_vert_gulf'
prefix_china1='mirs_adv_poes_'+satId1+'_vert_china'

if ( satId1 eq 'f16' or satId1 eq 'f17' or satId1 eq 'f18' ) then begin
  prefix1='mirs_adv_dmsp_'+satId1+'_vert_glb'
  prefix_gulf1='mirs_adv_dmsp_'+satId1+'_vert_gulf'
  prefix_china1='mirs_adv_dmsp_'+satId1+'_vert_china'
endif

if ( satId1 eq 'npp' ) then begin
  prefix1='mirs_adv_npoess_'+satId1+'_vert_glb'
  prefix_gulf1='mirs_adv_npoess_'+satId1+'_vert_gulf'
  prefix_china1='mirs_adv_npoess_'+satId1+'_vert_china'
endif

if ( satId1 eq 'trmm2a12' ) then begin
  prefix1='mirs_'+satId1+'_vert_glb'
  prefix_gulf1='mirs_'+satId1+'_vert_gulf'
  prefix_china1='mirs_'+satId1+'_vert_china'
endif


;---- parameters related to data set 2 ----
prefix2='mirs_adv_poes_'+satId2+'_vert_glb'
prefix_gulf2='mirs_adv_poes_'+satId2+'_vert_gulf'
prefix_china2='mirs_adv_poes_'+satId2+'_vert_china'

if ( satId2 eq 'f16' or satId2 eq 'f17' or satId2 eq 'f18' ) then begin
  prefix2='mirs_adv_dmsp_'+satId2+'_vert_glb'
  prefix_gulf2='mirs_adv_dmsp_'+satId2+'_vert_gulf'
  prefix_china2='mirs_adv_dmsp_'+satId2+'_vert_china'
endif

if ( satId2 eq 'npp' ) then begin
  prefix2='mirs_adv_npoess_'+satId2+'_vert_glb'
  prefix_gulf2='mirs_adv_npoess_'+satId2+'_vert_gulf'
  prefix_china2='mirs_adv_npoess_'+satId2+'_vert_china'
endif

if ( satId2 eq 'trmm2a12' ) then begin
  prefix2='mirs_'+satId2+'_vert_glb'
  prefix_gulf2='mirs_'+satId2+'_vert_gulf'
  prefix_china2='mirs_'+satId2+'_vert_china'
endif



;---- parameters shared ----
prefix='mirs_adv_poes_'+satId1+'_'+satId2+'_diff_vert_glb'
prefix_gulf='mirs_adv_poes_'+satId1+'_'+satId2+'_diff_vert_gulf'
prefix_china='mirs_adv_poes_'+satId1+'_'+satId2+'_diff_vert_china'

if ( satId1 eq 'f16' or satId1 eq 'f17' or satId1 eq 'f18' ) then begin
  prefix='mirs_adv_dmsp_'+satId1+'_'+satId2+'_diff_vert_glb'
  prefix_gulf='mirs_adv_dmsp_'+satId1+'_'+satId2+'_diff_vert_gulf'
  prefix_china='mirs_adv_dmsp_'+satId1+'_'+satId2+'_diff_vert_china'
endif

if ( satId1 eq 'npp' ) then begin
  prefix='mirs_adv_npoess_'+satId1+'_'+satId2+'_diff_vert_glb'
  prefix_gulf='mirs_adv_npoess_'+satId1+'_'+satId2+'_diff_vert_gulf'
  prefix_china='mirs_adv_npoess_'+satId1+'_'+satId2+'_diff_vert_china'
endif
 
 
titlesatId1=strupcase(satId1)
if satId1 eq 'npp' then titlesatId1='NPP/ATMS'
titlesatId2=strupcase(satId2)
if satId2 eq 'npp' then titlesatId2='NPP/ATMS'

prod_id = 'scanday'
prod_title = ' Scan Time (hour)'


degree=String(37B)
minval=-6
maxval=30
yrange=[minval,maxval]
yticks=6
 

FOR icend = 0, 0 DO BEGIN

cend = cends(icend)
cendtxt = cendtxts(icend)

;---- read in data 1 ----
prod1=fltarr(NCOL,NROW)
openr,lun,gridDir1+'GRID_'+satId1+'_'+yyyymmdd+'_'+prod_id+'_'+cend+'.dat',/get_lun,/Swap_Endian
readu, lun, prod1
free_lun,lun

;---- read in data 2 ----
;---- trmm2a12 only gridded on 0.25 degree step ----
prod2=fltarr(NCOL,NROW)
if gridfactor eq 2 and satId2 eq 'trmm2a12' then begin
  tmp2=fltarr(1440,720)
  openr,lun,gridDir2+'GRID_'+satId2+'_'+yyyymmdd+'_'+prod_id+'_'+cend+'.dat',/get_lun,/Swap_Endian
  readu, lun, tmp2
  free_lun,lun
  for irow=0,NROW-1 do begin
  for icol=0,NCOL-1 do begin
    prod2(icol,irow) = tmp2(2*icol+1,2*irow+1)
  endfor
  endfor  
endif else begin
  openr,lun,gridDir2+'GRID_'+satId2+'_'+yyyymmdd+'_'+prod_id+'_'+cend+'.dat',/get_lun,/Swap_Endian
  readu, lun, prod2
  free_lun,lun
endelse


;---- convert into hours
ss = where(prod1 gt 0, cnt) & if cnt gt 0 then prod1(ss) = ( prod1(ss) - jday ) * 24.0
ss = where(prod2 gt 0, cnt) & if cnt gt 0 then prod2(ss) = ( prod2(ss) - jday ) * 24.0

;---- difference ----
prod=fltarr(NCOL,NROW) & prod(*,*) = -999.0
ss = where(prod1 gt 0 and prod2 gt 0, cnt)
if cnt ge 1 then prod(ss) = ABS( prod1(ss) - prod2(ss) )
print, 'cnt=', cnt



;---- plot -------------------------------------------------------------------------------------
ytitle='hour'
 
;---- Globe ----
step=step_glb

;---- lat fixed ----
xtickname=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
xticks=N_ELEMENTS(xtickname)-1
xrange=[-180,180]
xtitle='Longitude'
xval = -180.0+findgen(NCOL)/gridfactor
 
for ilat=0, NROW-1, step do begin
  latval = ( ilat * 1.0 / gridfactor ) - 90.0
  latstr = strtrim(string(fix(latval)),2)
  title = prod_title+' on latitude '+latstr+'!9'+degree+'!X'+cendtxt+date
  map_name = figsDir+prefix +'_'+prod_id+'_'+yyyymmdd+'_lat_'+latstr+'_'+cend+'.png'
  sb1=prod1(*,ilat) & sb2=prod2(*,ilat) & sb=prod(*,ilat) 
  plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
endfor

for ilat=NROW-1, NROW-1 do begin
  latval = ( ilat * 1.0 / gridfactor ) - 90.0
  ;latstr = strtrim(string(fix(latval)),2)
  latstr = strtrim(string(round(latval)),2)
  title = prod_title+' on latitude '+latstr+'!9'+degree+'!X'+cendtxt+date
  map_name = figsDir+prefix +'_'+prod_id+'_'+yyyymmdd+'_lat_'+latstr+'_'+cend+'.png'
  sb1=prod1(*,ilat) & sb2=prod2(*,ilat) & sb=prod(*,ilat) 
  plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
endfor


;---- lon fixed ----
xtickname=[-90,-75,-60,-45,-30,-15,0,15,30,45,60,75,90]
xticks=N_ELEMENTS(xtickname)-1
xrange=[-90,90]
xtitle='Latitude'
xval = -90.0+findgen(NROW)/gridfactor
 
for ilon=0, NCOL-1, step  do begin
  lonval = ( ilon * 1.0 / gridfactor ) - 180.0
  lonstr = strtrim(string(fix(lonval)),2)
  title=prod_title+' on longitude '+lonstr+'!9'+degree+'!X'+cendtxt+date
  map_name = figsDir+prefix+'_'+prod_id+'_'+yyyymmdd+'_lon_'+lonstr+'_'+cend+'.png'
  sb1=prod1(ilon,*) & sb2=prod2(ilon,*) & sb=prod(ilon,*)
  plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
endfor

for ilon=NCOL-1, NCOL-1 do begin
  lonval = ( ilon * 1.0 / gridfactor ) - 180.0
  ;lonstr = strtrim(string(fix(lonval)),2)
  lonstr = strtrim(string(round(lonval)),2)
  title=prod_title+' on longitude '+lonstr+'!9'+degree+'!X'+cendtxt+date
  map_name = figsDir+prefix+'_'+prod_id+'_'+yyyymmdd+'_lon_'+lonstr+'_'+cend+'.png'
  sb1=prod1(ilon,*) & sb2=prod2(ilon,*) & sb=prod(ilon,*)
  plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
endfor


;---- Gulf of Mexico -----
step=step_gulf

lat_min=17.0
lat_max=32.0
lon_min=-98.0
lon_max=-68.0

ilat_bot=(lat_min+90)*gridfactor
ilat_top=(lat_max+90)*gridfactor
ilon_left=(lon_min+180)*gridfactor
ilon_right=(lon_max+180)*gridfactor

;---- lat fixed ----
;xtickname=[-98,-88,-78,-68]
xtickname=fix(lon_min) + indgen(16)*2
xticks=N_elements(xtickname)-1
xrange=[lon_min,lon_max]
xtitle='Longitude'
xval = lon_min+findgen(ilon_right-ilon_left)/gridfactor

for ilat=ilat_bot,  ilat_top, step do begin
  latval = ( ilat * 1.0 / gridfactor ) - 90.0
  latstr = strtrim(string(fix(latval)),2)
  title = prod_title+' on latitude '+latstr+'!9'+degree+'!X'+cendtxt+date
  map_name = figsDir+prefix_gulf +'_'+prod_id+'_'+yyyymmdd+'_lat_'+latstr+'_'+cend+'.png'
  sb1=prod1(ilon_left:ilon_right-1,ilat)
  sb2=prod2(ilon_left:ilon_right-1,ilat)
  sb=prod(ilon_left:ilon_right-1,ilat)
  plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
endfor

;---- lon fixed ----------
;xtickname=[17,20,23,26,29,32]
xtickname=fix(lat_min) + indgen(16)
xticks=N_elements(xtickname)-1
xrange=[lat_min,lat_max]
xtitle='Latitude'
lats = lat_min+findgen(ilat_top-ilat_bot)/gridfactor

for ilon=ilon_left, ilon_right, step  do begin
  lonval = ( ilon * 1.0 / gridfactor ) - 180.0
  lonstr = strtrim(string(fix(lonval)),2)
  title=prod_title+' on longitude '+lonstr+'!9'+degree+'!X'+cendtxt+date
  map_name = figsDir+prefix_gulf+'_'+prod_id+'_'+yyyymmdd+'_lon_'+lonstr+'_'+cend+'.png'
  sb1=prod1(ilon,ilat_bot:ilat_top-1)
  sb2=prod2(ilon,ilat_bot:ilat_top-1)
  sb=prod(ilon,ilat_bot:ilat_top-1)
  plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
endfor

       
;---- China South East Sea ----
step=step_china

lat_min=5
lat_max=35
lon_min=110
lon_max=130

ilat_bot=(lat_min+90)*gridfactor
ilat_top=(lat_max+90)*gridfactor
ilon_left=(lon_min+180)*gridfactor
ilon_right=(lon_max+180)*gridfactor

;---- lat fixed ----
xtickname=[110,115,120,125,130]
xticks=N_elements(xtickname)-1
xrange=[lon_min,lon_max]
xtitle='Longitude'
xval = lon_min+findgen(ilon_right-ilon_left)/gridfactor

for ilat=ilat_bot,  ilat_top, step do begin
  latval = ( ilat * 1.0 / gridfactor ) - 90.0
  latstr = strtrim(string(fix(latval)),2)
  title = prod_title+' on latitude '+latstr+'!9'+degree+'!X'+cendtxt+date
  map_name = figsDir+prefix_china +'_'+prod_id+'_'+yyyymmdd+'_lat_'+latstr+'_'+cend+'.png'
  sb1=prod1(ilon_left:ilon_right-1,ilat)
  sb2=prod2(ilon_left:ilon_right-1,ilat)
  sb=prod(ilon_left:ilon_right-1,ilat)
  plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
endfor

;---- lon fixed ----------
xtickname=[5,10,15,20,25,30,35]
xticks=N_elements(xtickname)-1
xrange=[lat_min,lat_max]
xtitle='Latitude'
lats = lat_min+findgen(ilat_top-ilat_bot)/gridfactor

for ilon=ilon_left, ilon_right, step  do begin
  lonval = ( ilon * 1.0 / gridfactor ) - 180.0
  lonstr = strtrim(string(fix(lonval)),2)
  title=prod_title+' on longitude '+lonstr+'!9'+degree+'!X'+cendtxt+date
  map_name = figsDir+prefix_china+'_'+prod_id+'_'+yyyymmdd+'_lon_'+lonstr+'_'+cend+'.png'
  sb1=prod1(ilon,ilat_bot:ilat_top-1)
  sb2=prod2(ilon,ilat_bot:ilat_top-1)
  sb=prod(ilon,ilat_bot:ilat_top-1)
  plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
endfor


ENDFOR


End

  

;===================================================================================================
; Name:		PLOT_TIME_CROSS
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots 1 line
;
; Arguments:
;
;      Name		    	Type	    	Description
;      ---------------------------------------------------
;	- xval			I		x-direction values
;	- yval	               	I   		y-direction values         
;	- title			I		title
;	- xtitle		I		x title
;	- ytitle		I		y title
;	- xrange		I		x range
;	- yrange		I		y range
;	- map_name		I		map name
;
; Subroutines needed:
;       - None
;
;
; History:
;      ------------------------------------------------------
;       11-22-2009      Wanchun Chen		Original code
;
;
;===================================================================================================
PRO plot_timecross,xval,sb1,sb2,sb,xrange,yrange,xtitle,ytitle,title,map_name,xticks,yticks,satname
		     
   set_plot,'Z'
   device,set_resolution=[550, 150]
   r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
   r(*)=255 & g(*)=255 & b(*)=255
   r(1)=0 & g(1)=0 & b(1)=0
   r(2)=255 & g(2)=127 & b(1)=0
  
   r(11)=255 & g(11)=0   & b(11)=0
   r(22)=0   & g(22)=0   & b(22)=255
   r(33)=0   & g(33)=255 & b(33)=0
   
   TVLCT, r, g, b
   
   color1=11 & color2=22 &color3=33
   
   ;position = [0.10, 0.10, 0.95, 0.95]
   symsize = 0.75
   
   ss = where(FIX(sb1) eq -999, count)  &  if( count GT 0 ) then sb1(ss) = !VALUES.F_NAN
   ss = where(FIX(sb1) eq -99,  count)  &  if( count GT 0 ) then sb1(ss) = !VALUES.F_NAN
   
   ss = where(FIX(sb2) eq -999, count)  &  if( count GT 0 ) then sb2(ss) = !VALUES.F_NAN
   ss = where(FIX(sb2) eq -99,  count)  &  if( count GT 0 ) then sb2(ss) = !VALUES.F_NAN
   
   ss = where(FIX(sb) eq -999, count)  &  if( count GT 0 ) then sb(ss) = !VALUES.F_NAN
   ss = where(FIX(sb) eq -99,  count)  &  if( count GT 0 ) then sb(ss) = !VALUES.F_NAN
   
   PLOT, xval,sb1,XTITLE=xtitle,YTITLE=ytitle,TITLE=title,XRANGE=xrange,YRANGE=YRange,$
    	 COLOR=1,BACKGROUND=255,CHARSIZE=0.7,/NODATA,XSTYLE=1,YSTYLE=1,$
	 position=position,xticks=xticks,yticks=yticks
   
   OPLOT, xval, sb1, COLOR=color1
   OPLOT, xval, sb2, COLOR=color2
   OPLOT, xval, sb,  COLOR=color3
   
   legend1='___'+satname
   legend2='___TRMM'
   legend3='___('+satname+'-TRMM)'
   
   xyouts, 0.15, 0.02, legend1, color=color1,charsize=0.7,/normal
   xyouts, 0.40, 0.02, legend2, color=color2,charsize=0.7,/normal
   xyouts, 0.70, 0.02, legend3, color=color3,charsize=0.7,/normal
   
   write_png, map_name, TVRD(), r, g, b
   device,/close
END


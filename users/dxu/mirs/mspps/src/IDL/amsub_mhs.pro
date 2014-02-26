Pro amsub_mhs, namelist=namelist

;****************************************
;    some variables initialization
;****************************************
sat='n18'
date='20090922'
filelist='list'
figsDir='./'

fromNameList=1
if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', sat                  ;Satellite ID
  readf,iu,format='(a)', date                 ;file extension of image
  readf,iu,format='(a)', filelist             ;file list
  readf,iu,format='(a)', figsDir              ;path where to put image files
  close,iu
  free_lun,iu,/force
endif



;*************************
; identifiers declaration
;*************************
gridfactor=4.0
ncol=360*gridfactor & nrow=180*gridfactor

; initial values are different, depending on average or near nadir
rr_grid_as=fltarr(ncol,nrow)
rr_grid_as(*,*)=-999.0

rr_grid_ds=fltarr(ncol,nrow)
rr_grid_ds(*,*)=-999.0

snow_grid_as=fltarr(ncol,nrow)
snow_grid_as(*,*)=-999.0

snow_grid_ds=fltarr(ncol,nrow)
snow_grid_ds(*,*)=-999.0

iwp_grid_as=fltarr(ncol,nrow)
iwp_grid_as(*,*)=-999.0

iwp_grid_ds=fltarr(ncol,nrow)
iwp_grid_ds(*,*)= -999.0

swe_grid_as=fltarr(ncol,nrow)
swe_grid_as(*,*)=-999.0

swe_grid_ds=fltarr(ncol,nrow)
swe_grid_ds(*,*)=-999.0



;**************************
; fill in stuff
;**************************

NUMSPOT_B=90

Anadir=fltarr(ncol,nrow)
Anadir(*,*)=NUMSPOT_B

Dnadir=fltarr(ncol,nrow)
Dnadir(*,*)=NUMSPOT_B


fov_size_B = fltarr(NUMSPOT_B)
FOV_B, fov_size_B

PI=3.141593



filein=''
openr, luf, filelist,  /get_lun


grid_name='AMSUB_Swath'
if( sat eq 'n15' or sat eq 'n16' or sat eq 'n17' ) then grid_name='AMSUB_Swath'
if( sat eq 'n18' or sat eq 'n19' or sat eq 'm2'  ) then grid_name='MHS_Swath'


WHILE ( NOT EOF(luf) ) DO BEGIN
   
  readf, luf, filein, format='(a)'
  print, filein 
  
  
  fileID=EOS_SW_OPEN(filein,/read)
  gridID=EOS_SW_ATTACH(fileid,grid_name)
  
  success=EOS_SW_READFIELD(gridID,'Latitude', lat)
  success=EOS_SW_READFIELD(gridID,'Longitude',lon)
  success=EOS_SW_READFIELD(gridID,'Orbit_mode',mode)
  ;success=EOS_SW_READFIELD(gridID,'Chan1_AT',at1)
  success=EOS_SW_READFIELD(gridID,'Sfc_type',sfc)
  
  success=EOS_SW_READFIELD(gridID,'RR',rr)
  success=EOS_SW_READFIELD(gridID,'Snow',snow)
  success=EOS_SW_READFIELD(gridID,'IWP',iwp)
  success=EOS_SW_READFIELD(gridID,'SWE',swe)
  
  success=EOS_SW_DETACH(gridID)
  success=EOS_SW_CLOSE(fileID)
  
  dim=size(lat)
  NX=dim(1) & NY=dim(2)
  

  for j=0, NY-1 do begin
    cend = mode(j)
    
  for i=0, NX-1 do begin
    
    loncorr=abs(1/cos(PI*lat(i,j)/180))
    if loncorr gt 200 then loncorr=200
  
    lonleft  = lon(i,j) - 0.5 * fov_size_B[i] * loncorr
    lonright = lon(i,j) + 0.5 * fov_size_B[i] * loncorr
    gridlon_left  = fix ( (lonleft  + 180.0) * gridfactor + 0.5)
    gridlon_right = fix ( (lonright + 180.0) * gridfactor + 0.5)

    if gridlon_left   lt 0     then  gridlon_left=0
    if gridlon_left   ge ncol  then  gridlon_left=ncol-1
    if gridlon_right  lt 0     then  gridlon_right=0
    if gridlon_right  ge ncol  then  gridlon_right=ncol-1
  
    
    gridlat = (lat(i,j)+90.0) * gridfactor 
    if gridlat lt 0     then gridlat=0
    if gridlat ge nrow  then gridlat=nrow -1
    
    near_nadir = abs(i - (NUMSPOT_B-1.)/2.) + 0.6
    
    if( cend eq 1 ) then begin
    
      for lonbox=gridlon_left, gridlon_right do begin
	if near_nadir le Anadir(lonbox,gridlat) then begin
          Anadir(lonbox,gridlat)=near_nadir
	  if( rr(i,j)   ge 0 ) then rr_grid_as(lonbox,gridlat)=rr(i,j)*0.1    
	  if( iwp(i,j)  ge 0 ) then iwp_grid_as(lonbox,gridlat)=iwp(i,j)*0.01 
	  if( swe(i,j)  ge 0 ) then swe_grid_as(lonbox,gridlat)=swe(i,j)*0.01 
	  
	  if( snow(i,j) gt 0 ) then begin
	  
	    snow_grid_as(lonbox,gridlat)=snow(i,j)
	  
	  endif else begin
	    if sfc(i,j) eq 0 then snow_grid_as(lonbox,gridlat)=5
	    
	    if sfc(i,j) eq 1 then begin
	       snow_grid_as(lonbox,gridlat)=12
	       if snow(i,j) eq -6 or snow(i,j) eq -10 or snow(i,j) eq -11 then snow_grid_as(lonbox,gridlat)=5
	    endif
	    
	    if sfc(i,j) eq 2 then snow_grid_as(lonbox,gridlat)=5 
	  
	  endelse 

	endif
      endfor
    
    endif
    
    
    if( cend eq 2 ) then begin
    
      for lonbox=gridlon_left, gridlon_right do begin
	if near_nadir le Dnadir(lonbox,gridlat) then begin
          Dnadir(lonbox,gridlat)=near_nadir
	  if( rr(i,j)   ge 0 ) then rr_grid_ds(lonbox,gridlat)=rr(i,j)*0.1    
	  if( iwp(i,j)  ge 0 ) then iwp_grid_ds(lonbox,gridlat)=iwp(i,j)*0.01 
	  if( swe(i,j)  ge 0 ) then swe_grid_ds(lonbox,gridlat)=swe(i,j)*0.01 
	  
	  if( snow(i,j) gt 0 ) then begin
	  
	    snow_grid_ds(lonbox,gridlat)=snow(i,j)
	  
	  endif else begin
	    if sfc(i,j) eq 0 then snow_grid_ds(lonbox,gridlat)=5
	    
	    if sfc(i,j) eq 1 then begin
	       snow_grid_ds(lonbox,gridlat)=12
	       if snow(i,j) eq -6 or snow(i,j) eq -10 or snow(i,j) eq -11 then snow_grid_ds(lonbox,gridlat)=5
	    endif
	    
	    if sfc(i,j) eq 2 then snow_grid_ds(lonbox,gridlat)=5 
	  
	  endelse 
	  
	endif
      endfor
    
    endif
 

 
  endfor
  endfor


    

ENDWHILE
 
CLOSE, LUF
FREE_LUN, LUF





;**********************************************************
; plot subroutine
;**********************************************************
plot_rr,  rr_grid_as, sat, 'as', date,  -0.5,  10.0, figsDir
plot_rr,  rr_grid_ds, sat, 'ds', date,  -0.5,  10.0, figsDir

plot_iwp,  iwp_grid_as, sat,   'as', date,  -0.05,  1.0, figsDir
plot_iwp,  iwp_grid_ds, sat,   'ds', date,  -0.05,  1.0, figsDir

plot_swe,  swe_grid_as, sat,   'as', date,  -2,  16, figsDir
plot_swe,  swe_grid_ds, sat,   'ds', date,  -2,  16, figsDir

plot_snow,  snow_grid_as, sat,   'as', date,  0,  100, figsDir
plot_snow,  snow_grid_ds, sat,   'ds', date,  0,  100, figsDir


END







;**********************************************************
; all subs used
;**********************************************************



PRO FOV_B, fov_size_B
  
  NUMSPOT_B=90
  PI=3.141593
  SCAN_BNG_B=1.11
  REARTH=6371.0
  RSAT=833.0
  
  i=0
  angle=0.0
  angle1=0.0
  ang=fltarr(NUMSPOT_B+1)
  
  for i=0, NUMSPOT_B do begin
  
    angle = PI * SCAN_BNG_B * (i - NUMSPOT_B/2.0) / 180.0
    angle1 = (REARTH + RSAT) * sin(angle) / REARTH
    angle1 = atan(angle1 / sqrt(1 - angle1 * angle1))
    ang(i) = (angle1 * 180.0 / PI) - SCAN_BNG_B * (i - NUMSPOT_B/2.0)
  endfor
  
  for i=0, NUMSPOT_B-1 do begin
     fov_size_B(i) = abs(ang(i+1) - ang(i)) 
  endfor
  
  ;print, fov_size_B

END



PRO PLOT_RR, tmp, sat, cend, DATE, MINVALUE, MAXVALUE, dir

image=bytscl(tmp, min=minvalue, max=maxvalue, top=254)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; color table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET_PLOT,'Z'
;DEVICE, SET_RESOLUTION=[860,630]
position=[0.1, 0.15, 0.9, 0.9]

;restore, 'colors_rr.sav'
;r = Congrid(r, !D.N_Colors)
;g = Congrid(g, !D.N_Colors)
;b = Congrid(b, !D.N_Colors)
loadct_rr, r, g, b 
TVLCT, r, g, b

r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0
blackColor = 255

datelabel=strmid(date,0,4)+'-'+strmid(date,4,2)+'-'+strmid(date,6,2)
title="MSPPS " + strupcase(sat) + " Rain Rate (mm/hr) " + datelabel

MAP_SET, /CYL, LIMIT=[-90,-180, 90, 180],POSITION=position,  $
	 TITLE=title, COLOR=blackColor, CHARSIZE=0.8,/NOBORDER  

warp=MAP_IMAGE(image,xx,yy,xs,ys,/BILIN,COMPRESS=1)
TV,warp,xx,yy,xsize=xs,ysize=ys

MAP_GRID, COLOR=blackColor
MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=blackColor

;**********************************************************************
; longitude and latitude
;**********************************************************************
longitude=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
latitude =[-90,-60,-30,0,30,60,90]

PLOT, longitude,latitude, /NoData, XStyle=1, YStyle=1, Xticks=12, Yticks=6, $
      POSITION=position, /NoErase, xtitle='Longitude', ytitle='Latitude',   $
      Color=blackColor,Charsize=0.8,xrange=[-180,180],yrange=[-90,90]

;**********************************************************************
; Draw a color bar
;**********************************************************************
position_bar=fltarr(4)
position_bar[0]=position[0]+0.1
position_bar[1]=position[1]-0.1
position_bar[2]=position[2]-0.1
position_bar[3]=position_bar[1]+0.025

ColorBar,NColors=!D.N_Colors-2, DIVISION=5, FORMAT='(F4.1)',  $
          MIN=MINVALUE, MAX=maxvalue, POSITION=position_bar,  $
          CHARSIZE=0.8, COLOR=blackColor

map_name = dir + 'mspps_' + sat +'_rr_'+date+'_'+cend+'.png'
write_png, map_name, TVRD(), r, g, b
 
End




PRO PLOT_IWP, tmp, sat, cend, DATE, MINVALUE, MAXVALUE, dir

image=bytscl(tmp, min=minvalue, max=maxvalue, top=254)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; color table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET_PLOT,'Z'
;DEVICE, SET_RESOLUTION=[860,630]
position=[0.1, 0.15, 0.9, 0.9]

;restore, 'colors_rr.sav'
;r = Congrid(r, !D.N_Colors)
;g = Congrid(g, !D.N_Colors)
;b = Congrid(b, !D.N_Colors)
loadct_rr, r, g, b 
TVLCT, r, g, b

r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0
blackColor = 255

datelabel=strmid(date,0,4)+'-'+strmid(date,4,2)+'-'+strmid(date,6,2)
title="MSPPS " + strupcase(sat) + " Ice Water Path (kg/m^2) " + datelabel
MAP_SET, /CYL, LIMIT=[-90,-180, 90, 180],POSITION=position,  $
	 TITLE=title, COLOR=blackColor, CHARSIZE=0.8,/NOBORDER  

warp=MAP_IMAGE(image,xx,yy,xs,ys,/BILIN,COMPRESS=1)
TV,warp,xx,yy,xsize=xs,ysize=ys

MAP_GRID, COLOR=blackColor
MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=blackColor

;**********************************************************************
; longitude and latitude
;**********************************************************************
longitude=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
latitude =[-90,-60,-30,0,30,60,90]

PLOT, longitude,latitude, /NoData, XStyle=1, YStyle=1, Xticks=12, Yticks=6, $
      POSITION=position, /NoErase, xtitle='Longitude', ytitle='Latitude',   $
      Color=blackColor,Charsize=0.8,xrange=[-180,180],yrange=[-90,90]

;**********************************************************************
; Draw a color bar
;**********************************************************************
position_bar=fltarr(4)
position_bar[0]=position[0]+0.1
position_bar[1]=position[1]-0.1
position_bar[2]=position[2]-0.1
position_bar[3]=position_bar[1]+0.025

ColorBar, NColors=!D.N_Colors-2, DIVISION=6, FORMAT='(F5.2)',  $
          MIN=MINVALUE, MAX=maxvalue, POSITION=position_bar,   $
          CHARSIZE=0.8, COLOR=blackColor

map_name = dir + 'mspps_' + sat +'_iwp_'+date+'_'+cend+'.png'
write_png, map_name, TVRD(), r, g, b
 
end





PRO PLOT_SWE, tmp, sat, cend, DATE, MINVALUE, MAXVALUE, dir

image=bytscl(tmp, min=minvalue, max=maxvalue, top=254)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; color table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET_PLOT,'Z'
;DEVICE, SET_RESOLUTION=[860,630]
position=[0.1, 0.15, 0.9, 0.9]

;restore, 'colors_swe.sav'
;r = Congrid(r, !D.N_Colors)
;g = Congrid(g, !D.N_Colors)
;b = Congrid(b, !D.N_Colors)
loadct_swe, r, g, b 
TVLCT, r, g, b

r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0
blackColor = 255

datelabel=strmid(date,0,4)+'-'+strmid(date,4,2)+'-'+strmid(date,6,2)
title="MSPPS " + strupcase(sat) + " Snow Water Equivalent (cm) " + datelabel

MAP_SET, /CYL, LIMIT=[-90,-180, 90, 180],POSITION=position,  $
	 TITLE=title, COLOR=blackColor, CHARSIZE=0.8,/NOBORDER  

warp=MAP_IMAGE(image,xx,yy,xs,ys,/BILIN,COMPRESS=1)
TV,warp,xx,yy,xsize=xs,ysize=ys

MAP_GRID, COLOR=blackColor
MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=blackColor

;XYOUTS, 0.825, 0.027, '(mm/hr)', COLOR=blackColor, /normal

;**********************************************************************
; longitude and latitude
;**********************************************************************
longitude=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
latitude =[-90,-60,-30,0,30,60,90]

PLOT, longitude,latitude, /NoData, XStyle=1, YStyle=1, Xticks=12, Yticks=6, $
      POSITION=position, /NoErase, xtitle='Longitude', ytitle='Latitude',   $
      Color=blackColor,Charsize=0.8,xrange=[-180,180],yrange=[-90,90]

;**********************************************************************
; Draw a color bar
;**********************************************************************
position_bar=fltarr(4)
position_bar[0]=position[0]+0.1
position_bar[1]=position[1]-0.1
position_bar[2]=position[2]-0.1
position_bar[3]=position_bar[1]+0.025

ColorBar, NColors=!D.N_Colors-2, DIVISION=9, FORMAT='(I3)',  $
          MIN=MINVALUE, MAX=maxvalue, POSITION=position_bar,    	$
          CHARSIZE=0.8, COLOR=blackColor

map_name = dir + 'mspps_' + sat +'_swe_'+date+'_'+cend+'.png'
write_png, map_name, TVRD(), r, g, b
 
end





PRO PLOT_Snow, tmp, sat, cend, DATE, MINVALUE, MAXVALUE, dir

image=bytscl(tmp, min=minvalue, max=maxvalue, top=!D.N_Colors-5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; color table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET_PLOT,'Z'
;DEVICE, SET_RESOLUTION=[860,630]
position=[0.1, 0.15, 0.9, 0.9]

;restore, 'colors_snow.sav'
;r = Congrid(r, !D.N_Colors)
;g = Congrid(g, !D.N_Colors)
;b = Congrid(b, !D.N_Colors)
loadct_snow, r, g, b 
TVLCT, r, g, b

r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0
blackColor = 255

r(254)=196 & g(254)=0 & b(254)=162


datelabel=strmid(date,0,4)+'-'+strmid(date,4,2)+'-'+strmid(date,6,2)
title="MSPPS " + strupcase(sat) + " Snow Cover " + datelabel

MAP_SET, /CYL, LIMIT=[-90,-180, 90, 180],POSITION=position,  $
	 TITLE=title, COLOR=blackColor, CHARSIZE=0.8,/NOBORDER  

warp=MAP_IMAGE(image,xx,yy,xs,ys,/BILIN,COMPRESS=1)
TV,warp,xx,yy,xsize=xs,ysize=ys

;MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=blackColor
MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=254
MAP_GRID, COLOR=blackColor

;**********************************************************************
; longitude and latitude
;**********************************************************************
longitude=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
latitude =[-90,-60,-30,0,30,60,90]

PLOT, longitude,latitude, /NoData, XStyle=1, YStyle=1, Xticks=12, Yticks=6, $
      POSITION=position, /NoErase, xtitle='Longitude', ytitle='Latitude',   $
      Color=blackColor,Charsize=0.8,xrange=[-180,180],yrange=[-90,90]

;**********************************************************************
; Draw a color bar
;**********************************************************************
position_bar=fltarr(4)
position_bar[0]=position[0]+0.1
position_bar[1]=position[1]-0.1
position_bar[2]=position[2]-0.1
position_bar[3]=position_bar[1]+0.025

;ColorBar,NColors=!D.N_Colors-5, DIVISION=10, FORMAT='(I3)',  	$
;          MIN=MINVALUE, MAX=maxvalue, POSITION=position_bar,   $
;          CHARSIZE=0.8, COLOR=blackColor


bar_position=fltarr(4)
bar_position[1]=position[1]-0.1
bar_position[3]=position_bar[1]+0.025


;---- Plot Missing lengend (white) - 0
bar_position[0]=0.10
bar_position[2]=0.3
ColorBar2,Bottom=0,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
         Minor=1,TickLen=0.0001,TickNames=[' ','no data',' '],CharSize=0.8

;---- Plot Ocean lengend (light blue) - 8
bar_position[0]=0.3
bar_position[2]=0.5
ColorBar2,Bottom=8,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','undetermined',' '],CharSize=0.8

;---- Plot land lengend (yellow) - 252
bar_position[0]=0.5
bar_position[2]=0.7
ColorBar2,Bottom=30,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','land without snow',' '],CharSize=0.8


;---- Plot snow (blue) - 251
bar_position[0]=0.7
bar_position[2]=0.9
ColorBar2,Bottom=251,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
	 Minor=1,TickLen=0.0001,TickNames=[' ','snow',' '],CharSize=0.8



map_name = dir + 'mspps_' + sat +'_snow_'+date+'_'+cend+'.png'
write_png, map_name, TVRD(), r, g, b
 
end






;+
; NAME:
;   COLORBAR
;
; PURPOSE:
;       The purpose of this routine is to add a color bar to the current
;       graphics window.
;
; CATEGORY:
;       Graphics, Widgets.
;
; CALLING SEQUENCE:
;       COLORBAR
;
; INPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;
;       BOTTOM: The lowest color index of the colors to be loaded in
;                 the bar.
;
;       CHARSIZE: The character size of the color bar annotations. Default is 1.0.
;
;       COLOR:    The color index of the bar outline and characters. Default
;                 is ncolors - 1 + bottom.
;
;       DIVISIONS: The number of divisions to divide the bar into. There will
;                 be (divisions + 1) annotations. The default is 2.
;
;       FORMAT:   The format of the bar annotations. Default is '(F6.2)'.
;
;       MAX:      The maximum data value for the bar annotation. Default is
;                 NCOLORS-1.
;
;       MIN:      The minimum data value for the bar annotation. Default is 0.
;
;       NCOLORS:  This is the number of colors in the color bar.
;
;       POSITION: A four-element array of normalized coordinates in the same
;                 form as the POSITION keyword on a plot. Default is
;                 [0.88, 0.15, 0.95, 0.95] for a vertical bar and
;                 [0.15, 0.88, 0.95, 0.95] for a horizontal bar.
;
;       PSCOLOR:  This keyword is only applied if the output is being sent to
;                 a PostScript file. It indicates that the PostScript device
;                 is configured for color output. If this keyword is set, then
;                 the annotation is drawn in the color specified by the COLOR
;                 keyword. If the keyword is not set, the annotation is drawn
;                 in the color specified by the !P.COLOR system variable
;                 (usually this will be the color black). In general, this
;                 gives better looking output on non-color or gray-scale
;                 printers. If you are not specifically setting the annotation
;                 color (with the COLOR keyword), it will probably
;                 be better NOT to set this keyword either, even if you
;                 are outputting to a color PostScript printer.
;
;       RIGHT:    This puts the labels on the right-hand side of a vertical
;                 color bar. It applies only to vertical color bars.
;
;       TITLE:    This is title for the color bar. The default is to have
;                 no title.
;
;       TOP:      This puts the labels on top of the bar rather than under it.
;                 The keyword only applies if a horizontal color bar is rendered.
;
;       VERTICAL: Setting this keyword give a vertical color bar. The default
;                 is a horizontal color bar.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       Color bar is drawn in the current graphics window.
;
; RESTRICTIONS:
;       The number of colors available on the display device (not the
;       PostScript device) is used unless the NCOLORS keyword is used.
;
; EXAMPLE:
;       To display a horizontal color bar above a contour plot, type:
;
;       LOADCT, 5, NCOLORS=100
;       CONTOUR, DIST(31,41), POSITION=[0.15, 0.15, 0.95, 0.75], $
;          C_COLORS=INDGEN(25)*4, NLEVELS=25
;       COLORBAR, NCOLORS=100
;
; MODIFICATION HISTORY:
;       Written by: David Fanning, 10 JUNE 96.
;       10/27/96: Added the ability to send output to PostScript. DWF
;       11/4/96: Substantially rewritten to go to screen or PostScript
;           file without having to know much about the PostScript device
;           or even what the current graphics device is. DWF
;       1/27/97: Added the RIGHT and TOP keywords. Also modified the
;            way the TITLE keyword works. DWF
;       7/15/97: Fixed a problem some machines have with plots that have
;            no valid data range in them. DWF
;-

PRO COLORBAR, BOTTOM=bottom, CHARSIZE=charsize, COLOR=color, DIVISIONS=divisions, $
   FORMAT=format, POSITION=position, MAX=max, MIN=min, NCOLORS=ncolors, $
   PSCOLOR=pscolor, TITLE=title, VERTICAL=vertical, TOP=top, RIGHT=right

   ; Is the PostScript device selected?

postScriptDevice = (!D.NAME EQ 'PS')

  ; Check and define keywords.

IF N_ELEMENTS(ncolors) EQ 0 THEN BEGIN

   ; Most display devices to not use the 256 colors available to
   ; the PostScript device. This presents a problem when writing
   ; general-purpose programs that can be output to the display or
   ; to the PostScript device. This problem is especially bothersome
   ; if you don't specify the number of colors you are using in the
   ; program. One way to work around this problem is to make the
   ; default number of colors the same for the display device and for
   ; the PostScript device. Then, the colors you see in PostScript are
   ; identical to the colors you see on your display. Here is one way to
   ; do it.

   IF postScriptDevice THEN BEGIN
      oldDevice = !D.NAME

         ; What kind of computer are we using? SET_PLOT to appropriate
         ; display device.

      thisOS = !VERSION.OS_FAMILY
      thisOS = STRMID(thisOS, 0, 3)
      thisOS = STRUPCASE(thisOS)
      CASE thisOS of
         'MAC': SET_PLOT, thisOS
         'WIN': SET_PLOT, thisOS
         ELSE: SET_PLOT, 'X'
      ENDCASE

         ; Open a window (to make sure !D.N_COLORS is accurate).

      WINDOW, /FREE, /PIXMAP, XSIZE=10, YSIZE=10
      WDELETE, !D.WINDOW

         ; Here is how many colors we should use.

      ncolors = !D.N_COLORS
      SET_PLOT, oldDevice
    ENDIF ELSE ncolors = !D.N_COLORS
ENDIF
IF N_ELEMENTS(bottom) EQ 0 THEN bottom = 0B
IF N_ELEMENTS(charsize) EQ 0 THEN charsize = 1.0
IF N_ELEMENTS(format) EQ 0 THEN format = '(F6.2)'
IF N_ELEMENTS(color) EQ 0 THEN color = ncolors - 1 + bottom
IF N_ELEMENTS(min) EQ 0 THEN min = 0.0
IF N_ELEMENTS(max) EQ 0 THEN max = FLOAT(ncolors) - 1
IF N_ELEMENTS(divisions) EQ 0 THEN divisions = 2
IF N_ELEMENTS(title) EQ 0 THEN title = ''
pscolor = KEYWORD_SET(pscolor)

IF KEYWORD_SET(vertical) THEN BEGIN
   bar = REPLICATE(1B,10) # BINDGEN(256)
   IF N_ELEMENTS(position) EQ 0 THEN position = [0.88, 0.15, 0.95, 0.95]
ENDIF ELSE BEGIN
   bar = BINDGEN(256) # REPLICATE(1B, 10)
   IF N_ELEMENTS(position) EQ 0 THEN position = [0.15, 0.88, 0.95, 0.95]
ENDELSE
;print, BINDGEN(256)
;print, REPLICATE(1B,10)
;print,bar

   ; Scale the color bar.

 bar = BYTSCL(bar, TOP=ncolors-1) + bottom

   ; Get starting locations in DEVICE coordinates.

xstart = position(0) * !D.X_VSIZE
ystart = position(1) * !D.Y_VSIZE

   ; Get the size of the bar in DEVICE coordinates.

xsize = (position(2) - position(0)) * !D.X_VSIZE
ysize = (position(3) - position(1)) * !D.Y_VSIZE

   ; For PostScript output only, draw the annotation in !P.COLOR
   ; unless "pscolor" is set. This makes better output on grayscale
   ; printers.

IF postScriptDevice AND (pscolor NE 1) THEN BEGIN
   oldcolor = color
   color = !P.COLOR
ENDIF

   ; Display the color bar in the window. Sizing is
   ; different for PostScript and regular display.

IF postScriptDevice THEN BEGIN

   TV, bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize

ENDIF ELSE BEGIN

   bar = CONGRID(bar, CEIL(xsize), CEIL(ysize), /INTERP)
   TV, bar, xstart, ystart

ENDELSE

   ; Annotate the color bar.

IF KEYWORD_SET(vertical) THEN BEGIN

   IF KEYWORD_SET(right) THEN BEGIN

      PLOT, [min,max], [min,max], /NODATA, XTICKS=1, $
         YTICKS=divisions, XSTYLE=1, YSTYLE=9, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', YTICKLEN=0.1 , $
         YRANGE=[min, max], YTITLE=title

      AXIS, YAXIS=1, YRANGE=[min, max], YTICKFORMAT=format, YTICKS=divisions, $
         YTICKLEN=0.1, YSTYLE=1, COLOR=color, CHARSIZE=charsize

   ENDIF ELSE BEGIN

      PLOT, [min,max], [min,max], /NODATA, XTICKS=1, $
         YTICKS=divisions, XSTYLE=1, YSTYLE=9, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT=format, XTICKFORMAT='(A1)', YTICKLEN=0.1 , $
         YRANGE=[min, max]

      AXIS, YAXIS=1, YRANGE=[min, max], YTICKFORMAT='(A1)', YTICKS=divisions, $
         YTICKLEN=0.1, YTITLE=title, YSTYLE=1, COLOR=color, CHARSIZE=charsize

   ENDELSE

ENDIF ELSE BEGIN

   IF KEYWORD_SET(top) THEN BEGIN

      PLOT, [min,max], [min,max], /NODATA, XTICKS=divisions, $
         YTICKS=1, XSTYLE=9, YSTYLE=1, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', XTICKLEN=0.1, $
         XRANGE=[min, max], XTITLE=title

      AXIS, XTICKS=divisions, XSTYLE=1, COLOR=color, CHARSIZE=charsize, $
         XTICKFORMAT=format, XTICKLEN=0.1, XRANGE=[min, max], XAXIS=1

   ENDIF ELSE BEGIN

      PLOT, [min,max], [min,max], /NODATA, XTICKS=divisions, $
         YTICKS=1, XSTYLE=1, YSTYLE=1, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT=format, XTICKLEN=0.1, $
         XRANGE=[min, max], TITLE=title

    ENDELSE

ENDELSE

   ; Restore color variable if changed for PostScript.

IF postScriptDevice AND (pscolor NE 1) THEN color = oldcolor

END







;----------------------------------------------------------------------------------
;+
; NAME:
;   COLORBAR
;
; PURPOSE:
;
;       The purpose of this routine is to add a color bar to the current
;       graphics window.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;       Graphics, Widgets.
;
; CALLING SEQUENCE:
;
;       COLORBAR
;
; INPUTS:
;
;       None.
;
; KEYWORD PARAMETERS:
;
;       BOTTOM:       The lowest color index of the colors to be loaded in
;                     the bar.
;
;       CHARSIZE:     The character size of the color bar annotations. Default is 1.0.
;
;       COLOR:        The color index of the bar outline and characters. Default
;                     is !P.Color..
;
;       DIVISIONS:     The number of divisions to divide the bar into. There will
;                     be (divisions + 1) annotations. The default is 6.
;
;       FONT:         Sets the font of the annotation. Hershey: -1, Hardware:0, True-Type: 1.
;
;       FORMAT:       The format of the bar annotations. Default is '(I5)'.
;
;       INVERTCOLORS: Setting this keyword inverts the colors in the color bar.
;
;       MAXRANGE:     The maximum data value for the bar annotation. Default is
;                     NCOLORS.
;
;       MINRANGE:     The minimum data value for the bar annotation. Default is 0.
;
;       MINOR:        The number of minor tick divisions. Default is 2.
;
;       NCOLORS:      This is the number of colors in the color bar.
;
;       POSITION:     A four-element array of normalized coordinates in the same
;                     form as the POSITION keyword on a plot. Default is
;                     [0.88, 0.10, 0.95, 0.90] for a vertical bar and
;                     [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;;
;       RANGE:        A two-element vector of the form [min, max]. Provides an
;                     alternative way of setting the MINRANGE and MAXRANGE keywords.
;
;       RIGHT:        This puts the labels on the right-hand side of a vertical
;                     color bar. It applies only to vertical color bars.
;
;       TICKNAMES:    A string array of names or values for the tick marks.
;
;       TITLE:        This is title for the color bar. The default is to have
;                     no title.
;
;       TOP:          This puts the labels on top of the bar rather than under it.
;                     The keyword only applies if a horizontal color bar is rendered.
;
;       VERTICAL:     Setting this keyword give a vertical color bar. The default
;                     is a horizontal color bar.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       Color bar is drawn in the current graphics window.
;
; RESTRICTIONS:
;
;       The number of colors available on the display device (not the
;       PostScript device) is used unless the NCOLORS keyword is used.
;
; EXAMPLE:
;
;       To display a horizontal color bar above a contour plot, type:
;
;       LOADCT, 5, NCOLORS=100
;       CONTOUR, DIST(31,41), POSITION=[0.15, 0.15, 0.95, 0.75], $
;          C_COLORS=INDGEN(25)*4, NLEVELS=25
;       COLORBAR, NCOLORS=100, POSITION=[0.15, 0.85, 0.95, 0.90]
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 10 JUNE 96.
;       10/27/96: Added the ability to send output to PostScript. DWF
;       11/4/96: Substantially rewritten to go to screen or PostScript
;           file without having to know much about the PostScript device
;           or even what the current graphics device is. DWF
;       1/27/97: Added the RIGHT and TOP keywords. Also modified the
;            way the TITLE keyword works. DWF
;       7/15/97: Fixed a problem some machines have with plots that have
;            no valid data range in them. DWF
;       12/5/98: Fixed a problem in how the colorbar image is created that
;            seemed to tickle a bug in some versions of IDL. DWF.
;       1/12/99: Fixed a problem caused by RSI fixing a bug in IDL 5.2. Sigh... DWF.
;       3/30/99: Modified a few of the defaults. DWF.
;       3/30/99: Used NORMAL rather than DEVICE coords for positioning bar. DWF.
;       3/30/99: Added the RANGE keyword. DWF.
;       3/30/99: Added FONT keyword. DWF
;       5/6/99: Many modifications to defaults. DWF.
;       5/6/99: Removed PSCOLOR keyword. DWF.
;       5/6/99: Improved error handling on position coordinates. DWF.
;       5/6/99. Added MINOR keyword. DWF.
;       5/6/99: Set Device, Decomposed=0 if necessary. DWF.
;       2/9/99: Fixed a problem caused by setting BOTTOM keyword, but not NCOLORS. DWF.
;       8/17/99. Fixed a problem with ambiguous MIN and MINOR keywords. DWF
;       8/25/99. I think I *finally* got the BOTTOM/NCOLORS thing sorted out. :-( DWF.
;       10/10/99. Modified the program so that current plot and map coordinates are
;            saved and restored after the colorbar is drawn. DWF.
;       3/18/00. Moved a block of code to prevent a problem with color decomposition. DWF.
;       4/28/00. Made !P.Font default value for FONT keyword. DWF.
;       9/26/00. Made the code more general for scalable pixel devices. DWF.
;       1/16/01. Added INVERTCOLORS keyword. DWF.
;       5/11/04. Added TICKNAME keyword. DWF.
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000-2004 Fanning Software Consulting.
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################


PRO COLORBAR2, BOTTOM=bottom, CHARSIZE=charsize, COLOR=color, DIVISIONS=divisions, $
   FORMAT=format, POSITION=position, MAXRANGE=maxrange, MINRANGE=minrange, NCOLORS=ncolors, $
   TITLE=title, VERTICAL=vertical, TOP=top, RIGHT=right, MINOR=minor, $
   RANGE=range, FONT=font, TICKLEN=ticklen, _EXTRA=extra, INVERTCOLORS=invertcolors, $
   TICKNAMES=ticknames

   ; Return to caller on error.

On_Error, 2

   ; Save the current plot state.

bang_p = !P
bang_x = !X
bang_Y = !Y
bang_Z = !Z
bang_Map = !Map

   ; Are scalable pixels available on the device?

IF (!D.Flags AND 1) NE 0 THEN scalablePixels = 1 ELSE scalablePixels = 0

   ; Which release of IDL is this?

thisRelease = Float(!Version.Release)

    ; Check and define keywords.

IF N_ELEMENTS(ncolors) EQ 0 THEN BEGIN

   ; Most display devices to not use the 256 colors available to
   ; the PostScript device. This presents a problem when writing
   ; general-purpose programs that can be output to the display or
   ; to the PostScript device. This problem is especially bothersome
   ; if you don't specify the number of colors you are using in the
   ; program. One way to work around this problem is to make the
   ; default number of colors the same for the display device and for
   ; the PostScript device. Then, the colors you see in PostScript are
   ; identical to the colors you see on your display. Here is one way to
   ; do it.

   IF scalablePixels THEN BEGIN
      oldDevice = !D.NAME

         ; What kind of computer are we using? SET_PLOT to appropriate
         ; display device.

      thisOS = !VERSION.OS_FAMILY
      thisOS = STRMID(thisOS, 0, 3)
      thisOS = STRUPCASE(thisOS)
      CASE thisOS of
         'MAC': SET_PLOT, thisOS
         'WIN': SET_PLOT, thisOS
         ELSE: SET_PLOT, 'X'
      ENDCASE

         ; Here is how many colors we should use.

      ncolors = !D.TABLE_SIZE
      SET_PLOT, oldDevice
    ENDIF ELSE ncolors = !D.TABLE_SIZE
ENDIF
IF N_ELEMENTS(bottom) EQ 0 THEN bottom = 0B
IF N_ELEMENTS(charsize) EQ 0 THEN charsize = 1.0
IF N_ELEMENTS(format) EQ 0 THEN format = '(I5)'
IF N_ELEMENTS(color) EQ 0 THEN color = !P.Color
IF N_ELEMENTS(minrange) EQ 0 THEN minrange = 0
IF N_ELEMENTS(maxrange) EQ 0 THEN maxrange = ncolors
IF N_ELEMENTS(ticklen) EQ 0 THEN ticklen = 0.2
IF N_ELEMENTS(minor) EQ 0 THEN minor = 2
IF N_ELEMENTS(range) NE 0 THEN BEGIN
   minrange = range[0]
   maxrange = range[1]
ENDIF
IF N_ELEMENTS(divisions) EQ 0 THEN divisions = 6
IF N_ELEMENTS(font) EQ 0 THEN font = !P.Font
IF N_ELEMENTS(title) EQ 0 THEN title = ''

; You can't have a format set *and* use ticknames.
IF N_ELEMENTS(ticknames) NE 0 THEN format = ""

IF KEYWORD_SET(vertical) THEN BEGIN
   bar = REPLICATE(1B,20) # BINDGEN(ncolors)
   IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 2)
   IF N_ELEMENTS(position) EQ 0 THEN BEGIN
      position = [0.88, 0.1, 0.95, 0.9]
   ENDIF ELSE BEGIN
      IF position[2]-position[0] GT position[3]-position[1] THEN BEGIN
         position = [position[1], position[0], position[3], position[2]]
      ENDIF
      IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
      IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
   ENDELSE
ENDIF ELSE BEGIN
   bar = BINDGEN(ncolors) # REPLICATE(1B, 20)
   IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 1)
   IF N_ELEMENTS(position) EQ 0 THEN BEGIN
      position = [0.1, 0.88, 0.9, 0.95]
   ENDIF ELSE BEGIN
      IF position[3]-position[1] GT position[2]-position[0] THEN BEGIN
         position = [position[1], position[0], position[3], position[2]]
      ENDIF
      IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
      IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
   ENDELSE
ENDELSE

   ; Scale the color bar.

 bar = BYTSCL(bar, TOP=(ncolors-1 < (255-bottom))) + bottom

   ; Get starting locations in NORMAL coordinates.

xstart = position(0)
ystart = position(1)

   ; Get the size of the bar in NORMAL coordinates.

xsize = (position(2) - position(0))
ysize = (position(3) - position(1))

   ; Display the color bar in the window. Sizing is
   ; different for PostScript and regular display.

IF scalablePixels THEN BEGIN

   TV, bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize, /Normal

ENDIF ELSE BEGIN

   bar = CONGRID(bar, CEIL(xsize*!D.X_VSize), CEIL(ysize*!D.Y_VSize), /INTERP)

        ; Decomposed color off if device supports it.

   CASE  StrUpCase(!D.NAME) OF
        'X': BEGIN
            IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
            Device, Decomposed=0
            ENDCASE
        'WIN': BEGIN
            IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
            Device, Decomposed=0
            ENDCASE
        'MAC': BEGIN
            IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
            Device, Decomposed=0
            ENDCASE
        ELSE:
   ENDCASE

   TV, bar, xstart, ystart, /Normal

      ; Restore Decomposed state if necessary.

   CASE StrUpCase(!D.NAME) OF
      'X': BEGIN
         IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
         ENDCASE
      'WIN': BEGIN
         IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
         ENDCASE
      'MAC': BEGIN
         IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
         ENDCASE
      ELSE:
   ENDCASE

ENDELSE

   ; Annotate the color bar.

IF KEYWORD_SET(vertical) THEN BEGIN

   IF KEYWORD_SET(right) THEN BEGIN

      PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1, $
         YTICKS=divisions, XSTYLE=1, YSTYLE=9, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', YTICKLEN=ticklen , $
         YRANGE=[minrange, maxrange], FONT=font, _EXTRA=extra, YMINOR=minor

      AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT=format, YTICKS=divisions, $
         YTICKLEN=ticklen, YSTYLE=1, COLOR=color, CHARSIZE=charsize, $
         FONT=font, YTITLE=title, _EXTRA=extra, YMINOR=minor, YTICKNAME=ticknames

   ENDIF ELSE BEGIN

      PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1, $
         YTICKS=divisions, XSTYLE=1, YSTYLE=9, YMINOR=minor, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT=format, XTICKFORMAT='(A1)', YTICKLEN=ticklen , $
         YRANGE=[minrange, maxrange], FONT=font, YTITLE=title, _EXTRA=extra, $
         YTICKNAME=ticknames

      AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT='(A1)', YTICKS=divisions, $
         YTICKLEN=ticklen, YSTYLE=1, COLOR=color, CHARSIZE=charsize, $
         FONT=font, _EXTRA=extra, YMINOR=minor

   ENDELSE

ENDIF ELSE BEGIN

   IF KEYWORD_SET(top) THEN BEGIN

      PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
         YTICKS=1, XSTYLE=9, YSTYLE=1, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', XTICKLEN=ticklen, $
         XRANGE=[minrange, maxrange], FONT=font, _EXTRA=extra, XMINOR=minor

      AXIS, XTICKS=divisions, XSTYLE=1, COLOR=color, CHARSIZE=charsize, $
         XTICKFORMAT=format, XTICKLEN=ticklen, XRANGE=[minrange, maxrange], XAXIS=1, $
         FONT=font, XTITLE=title, _EXTRA=extra, XCHARSIZE=charsize, XMINOR=minor, $
         XTICKNAME=ticknames

   ENDIF ELSE BEGIN

      PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
         YTICKS=1, XSTYLE=1, YSTYLE=1, TITLE=title, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT=format, XTICKLEN=ticklen, $
         XRANGE=[minrange, maxrange], FONT=font, XMinor=minor, _EXTRA=extra, $
         XTICKNAME=ticknames

    ENDELSE

ENDELSE

   ; Restore the previous plot and map system variables.

!P = bang_p
!X = bang_x
!Y = bang_y
!Z = bang_z
!Map = bang_map

END








Pro loadct_rr, r, g, b

r = [ $ 
255B,254B,253B,252B,252B,251B,250B,250B,249B,248B,247B,247B,246B,245B,245B,244B,$
243B,243B,242B,241B,240B,240B,232B,221B,210B,198B,187B,176B,165B,153B,142B,131B,$
120B,108B, 97B, 86B, 75B, 63B, 52B, 41B, 30B, 18B,  7B,  0B,  0B,  0B,  0B,  0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,7B,19B,31B,43B,55B,66B,78B,90B,102B,114B,$
126B,137B,149B,161B,173B,185B,196B,208B,220B,232B,244B,251B,251B,251B,251B,250B,$
250B,250B,250B,250B,249B,249B,249B,249B,248B,248B,248B,248B,247B,247B,247B,247B,$
247B,241B,235B,230B,224B,219B,213B,207B,202B,196B,191B,185B,180B,174B,168B,163B,$
157B,152B,146B,141B,135B,129B,131B,137B,143B,149B,155B,161B,167B,172B,178B,184B,$
190B,196B,202B,208B,213B,219B,225B,231B,237B,243B,249B,252B,251B,250B,249B,248B,$
247B,246B,245B,244B,243B,242B,241B,240B,239B,238B,237B,236B,235B,234B,233B,232B,$
231B,231B,231B,232B,232B,233B,233B,234B,234B,235B,235B,236B,236B,237B,237B,238B,$
238B,238B,239B,239B,240B,240B,241B,241B,242B,242B,242B,243B,243B,244B,244B,245B,$
245B,245B,246B,246B,247B,247B,248B,248B,248B,249B,249B,250B,250B,250B,250B,250B,$
250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B ]

g = [ $
255B,253B,251B,250B,248B,246B,245B,243B,241B,240B,238B,236B,235B,233B,232B,230B,$
228B,227B,225B,223B,222B,220B,217B,214B,211B,208B,205B,202B,199B,196B,193B,190B,$
187B,184B,181B,178B,175B,172B,169B,166B,163B,160B,157B,155B,155B,155B,155B,156B,$
156B,156B,156B,156B,157B,157B,157B,157B,158B,158B,158B,158B,159B,159B,159B,159B,$
160B,164B,168B,172B,177B,181B,185B,190B,194B,198B,203B,207B,211B,216B,220B,224B,$
229B,233B,237B,241B,246B,250B,251B,251B,250B,249B,249B,248B,248B,247B,247B,246B,$
246B,245B,244B,244B,243B,243B,242B,242B,241B,240B,240B,240B,240B,240B,240B,240B,$
240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,234B,228B,223B,217B,212B,206B,201B,195B,190B,184B,179B,173B,168B,162B,157B,$
151B,145B,140B,134B,129B,123B,122B,123B,124B,125B,126B,127B,128B,129B,130B,131B,$
132B,132B,133B,134B,135B,136B,137B,138B,139B,140B,141B,140B,137B,133B,129B,126B,$
122B,118B,115B,111B,107B,104B,100B,96B,93B,89B,85B,82B,78B,74B,71B,67B,$
64B,61B,58B,56B,53B,51B,48B,46B,43B,41B,38B,36B,33B,31B,28B,26B,$
23B,20B,18B,15B,13B,10B,10B,10B,10B,10B,11B,11B,11B,11B,12B,12B,$
12B,12B,12B,13B,13B,13B,13B,14B,14B,14B,14B,15B,15B,15B,15B,15B,$
15B,15B,15B,15B,15B,15B,15B,15B,15B,15B,15B,15B,15B,15B,15B,15B ]

b = [ $
255B,252B,250B,248B,246B,244B,242B,240B,238B,236B,233B,231B,229B,227B,225B,223B,$
221B,219B,217B,214B,212B,210B,203B,193B,183B,173B,164B,154B,144B,134B,124B,114B,$
105B,95B,85B,75B,65B,55B,45B,36B,26B,16B,6B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,1B,1B,1B,$
1B,1B,1B,1B,2B,2B,2B,2B,2B,2B,2B,2B,2B,2B,2B,2B,$
2B,2B,1B,1B,1B,1B,1B,1B,1B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,1B,1B,1B,1B,1B,2B,2B,2B,2B,2B,$
3B,3B,3B,3B,3B,3B,3B,3B,3B,3B,3B,2B,2B,2B,2B,2B,$
2B,1B,1B,1B,1B,1B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,1B,1B,1B,1B,1B,1B,1B,1B,1B,1B,$
2B,2B,2B,2B,2B,2B,2B,2B,2B,2B,2B,3B,3B,3B,3B,3B,$
3B,3B,3B,3B,3B,3B,3B,3B,3B,3B,3B,2B,2B,2B,2B,2B,$
2B,1B,1B,1B,1B,1B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B ]

end



Pro loadct_swe, r, g, b

r = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,$
136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,$
136B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,137B,137B,137B,137B,137B,137B,137B,$
137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,$
137B,137B,137B,137B,137B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,$
160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,$
160B,160B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B ]


g = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,228B,228B,228B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,$
202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,$
202B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,$
200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,150B,150B,150B,150B,150B,150B,150B,$
150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,$
150B,150B,150B,150B,150B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,$
90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,$
90B,90B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,255B ]


b = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,200B,200B,200B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,26B,26B,26B,26B,26B,26B,26B,$
26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,$
26B,26B,26B,26B,26B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,$
8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,$
8B,8B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,255B ]
	
end




Pro loadct_snow, r, g, b

r=bytarr(256)
g=bytarr(256)
b=bytarr(256)

r = [ $
255,255,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,179,222,221,220,219,218,217,216,215,214,213,212,$
211,210,209,208,207,207,206,205,204,203,202,201,200,199,198,197,196,195,194,193,193,192,191,190,189,188,187,186,185,184,183,182,181,180,179,179,178,$
177,176,175,174,173,172,171,170,169,168,167,166,165,165,164,163,162,161,160,159,158,157,156,155,154,153,152,151,151,150,149,148,147,146,145,144,143,$
142,141,140,139,138,137,136,136,135,134,133,132,131,130,129,128,127,126,125,124,123,122,122,121,120,119,118,117,116,115,114,113,112,111,110,109,108,$
108,107,106,105,104,103,102,101,100,99,98,97,96,95,94,94,93,92,91,90,89,88,87,86,85,84,83,82,81,80,80,79,78,77,76,75,74,$
73,72,71,70,69,68,67,66,65,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,51,50,49,48,47,46,45,44,43,42,41,40,39,$
38,37,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,9,255]


g = [ $
255,255,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,225,$
225,179,178,177,176,175,175,174,173,172,171,171,170,169,168,168,167,166,165,164,164,163,162,161,160,$
160,159,158,157,157,156,155,154,153,153,152,151,150,149,149,148,147,146,146,145,144,143,142,142,141,$
140,139,138,138,137,136,135,135,134,133,132,131,131,130,129,128,127,127,126,125,124,124,123,122,121,$
120,120,119,118,117,116,116,115,114,113,113,112,111,110,109,109,108,107,106,105,105,104,103,102,102,$
101,100,99,98,98,97,96,95,94,94,93,92,91,91,90,89,88,87,87,86,85,84,84,83,82,$
81,80,80,79,78,77,76,76,75,74,73,73,72,71,70,69,69,68,67,66,65,65,64,63,62,$
62,61,60,59,58,58,57,56,55,54,54,53,52,51,51,50,49,48,47,47,46,45,44,43,43,$
42,41,40,40,39,38,37,36,36,35,34,33,32,32,31,30,29,29,28,27,26,25,25,24,23,$
22,21,21,20,19,18,18,17,16,15,14,14,13,12,11,10,10,9,8,7,7,6,5,4,3,3,2,1,0,0,255]


b = [$
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
0,1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,$
31,32,33,34,35,36,38,39,40,41,42,43,44,45,46,48,49,50,51,52,53,54,55,57,58,59,60,$
61,62,63,64,65,67,68,69,70,71,72,73,74,76,77,78,79,80,81,82,83,85,86,87,88,89,90,$
91,92,93,95,96,97,98,99,100,101,102,104,105,106,107,108,109,110,111,112,114,115,116,117,118,119,120,$
121,123,124,125,126,127,128,129,130,131,133,134,135,136,137,138,139,140,142,143,144,145,146,147,148,149,150,$
152,153,154,155,156,157,158,159,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,180,181,$
182,183,184,185,186,187,189,190,191,192,193,194,195,196,197,199,200,201,202,203,204,205,206,208,209,210,211,$
212,213,214,215,216,218,219,220,221,222,223,224,225,227,228,229,230,231,232,233,234,235,237,238,239,240,241,$
242,243,244,246,247,248,249,250,251,252,253,255,255]

	
End



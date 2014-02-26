;$Id: io_Mapping.pro 2833 2011-12-14 22:13:03Z chrisg $
;---------------------------------------------------------------------------------
; Summary of all subroutines related to mapping subroutines.
;
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------
;===============================================================
; Name:		mapPlot
;
;
; Type:		IDL Subroutine
;
;
; Description:  maps out the content of an EDR vector. This is done 
;               on a point by point basis: no averaging
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- minlat             I              Min lat of region to be mapped
;	- maxlat             I              Max lat of region to be mapped
;	- minlon             I              Min lon of region to be mapped
;	- maxlon             I              Max lon of region to be mapped
;	- xlat               I              Vector of latitudes to be mapped
;	- xlon               I              Vector of longitudes to be mapped
;	- ind                I              Filter vector to be used
;	- tit                I              Map title to be used
;	- minY               I              Min range of xtab to be mapped
;	- maxY               I              Max range of xtab to be mapped
;	- xtab               I              Vector to be mapped
;	- unit               I              Unit label to add to color bar
;	- scal               I              scaling factor for symbol sizes
;	- symb               I              Symbol index to use 
;	- thk                I              Thickness factor to use
;       - overlap            I              Flag,first map or an overlay
;	- fmt                I              Format used to print unit
;	- charsz             I              character size scal factor
;	- labels             I              Labels of stickers
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtab,unit,scal,symb,thk,overlap,fmt,charsz,labels,nrec
  charsz0=1.
  typCharsz=size(charsz,/type)
  IF (typCharsz ne 0) THEN charsz0=charsz
  IF (size(nrec,/type) eq 0) THEN nrec=n_elements(ind)
  nc=!D.table_size
  nc=nc-2
  csize=16
  a = findgen(csize+1) * (!PI*2.5/float(csize))
  usersym,scal*cos(a)/2,scal*sin(a)/2,/fill
  IF (overlap eq 0) then begin
      center_lon=0
      latdel=max([fix((maxlat-minlat)/100),1])*30
      londel=max([fix((maxlon-minlon)/100),1])*20
      map_set,0,center_lon,/noerase,xmargin=[8,8],ymargin=[38,38],$
        limit=[minlat,minlon,maxlat,maxlon],/hires,title=tit,$
        ;position=[0.1,0.2,0.9,0.8],latdel=latdel,londel=londel,/grid,/sinusoidal
        position=[0.1,0.2,0.9,0.8],latdel=latdel,londel=londel,/grid,$
        charsize=charsz0*1.3
  endif
  FOR iprof=0L,nrec-1 DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc
      indcol=long(xind) > 1L < nc
      oplot,[xlon[ind(iprof)]],[xlat[ind(iprof)]],color=indcol-1,psym=symb,$
        symsize=scal,thick=thk
      IF (size(labels,/type) ne 0) THEN xyouts,xlon[ind(iprof)],xlat[ind(iprof)],labels[ind(iprof)],charsize=charsz0*0.8
  ENDFOR
  ;map_continents,/continents,/noborder,/hires,/usa
  IF (overlap eq 0) then begin
      map_grid,latdel=latdel,londel=londel,/label,lonlab=minlat,latlab=minlon,lonalign=0.,latalign=0.,$
        charsize=charsz0*1.3
      ;map_continents,/continents,/noborder,/hires,/usa
      IF ((maxY-minY) gt 0.) THEN BEGIN
          colorbar,ncolors=nc,/horizontal,range=[minY,maxY],title=unit,$
            format=fmt,charsize=charsz0*1.3,font=1,position=[0.10,0.05,0.9,0.1]
      ENDIF
      IF ((maxY-minY) le 0.) THEN BEGIN
          print, 'Warning: in mapPlot: No color bar plotted: (maxY-minY) <= 0.'
      ENDIF
  ENDIF
END


;===============================================================
; PNG version of mapPlot
;
; 10/16/2008	Wanchun Chen
;
; color_table_index is optional
;
;===============================================================
PRO mapPlot_png,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtab,unit,scal,symb,thk,overlap,fmt,divs,img_name,$
    color_table_index = color_table_index

  ;---- set Z buffer
  set_plot,'Z'
  erase
  device,set_resolution=[650, 500]
  position=[0.05, 0.15, 0.95, 0.9]
  
  ;---- load color table 33 if no color table given
  IF N_ELEMENTS(color_table_index) EQ 0 THEN BEGIN
    loadct, 33, /SILENT
    TVLCT, r, g, b, /get
  ENDIF
  
  ;---- if have color_table_index, then use it
  IF N_ELEMENTS(color_table_index) EQ 1 THEN BEGIN

    IF color_table_index GE 0 and color_table_index LE 40 THEN BEGIN
      loadct, color_table_index, /SILENT
      TVLCT, r, g, b, /get
    ENDIF

    IF color_table_index EQ 41 THEN BEGIN
      ; load MSPPS TPW colors
      loadct_tpw, r, g, b
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 42 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_sice, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 43 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_swe, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 44 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_climate, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    ;---- more options go here

  ENDIF
  
  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
  
  charsz0=1.
  ;typCharsz=size(charsz,/type)
  ;IF (typCharsz ne 0) THEN charsz0=charsz
  ;IF (size(nrec,/type) eq 0) THEN nrec=n_elements(ind)
  nrec=n_elements(ind)

  nc=239    ; to be consistent with our plot_grid subroutine
  csize=16
  a = findgen(csize+1) * (!PI*2.5/float(csize))
  usersym,scal*cos(a)/2,scal*sin(a)/2,/fill
  
  IF (overlap eq 0) THEN BEGIN
      center_lon=0
      
      latdiff=maxlat-minlat
      if( latdiff le 45                    ) then latdel=5
      if( latdiff gt 45 and latdiff le 90  ) then latdel=10
      if( latdiff gt 90                    ) then latdel=15
      
      londiff=maxlon-minlon
      if( londiff le 45                    ) then londel=5
      if( londiff gt 45 and londiff le 90  ) then londel=10
      if( londiff gt 90 and londiff le 120 ) then londel=15
      if( londiff gt 120                   ) then londel=30
      
      map_set,0,center_lon,/noerase,xmargin=[8,8],ymargin=[38,38],$
              limit=[minlat,minlon,maxlat,maxlon],/hires,title=tit,$
              position=position,latdel=latdel,londel=londel,/grid,charsize=charsz0*0.8
  ENDIF
  
  FOR iprof=0L,nrec-1 DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc + 1
      indcol=long(xind) > 1L < nc
      oplot,[xlon[ind(iprof)]],[xlat[ind(iprof)]],color=indcol,psym=symb,symsize=scal,thick=thk
      ;IF (size(labels,/type) ne 0) THEN xyouts,xlon[ind(iprof)],xlat[ind(iprof)],labels[ind(iprof)],charsize=charsz0*0.8
  ENDFOR
  
  map_continents,/continents,/noborder,/hires,/usa
  IF (overlap eq 0) THEN BEGIN
      map_grid,latdel=latdel,londel=londel,/label,lonlab=minlat,latlab=minlon,lonalign=0.,latalign=0.,charsize=charsz0*0.8
      IF ((maxY-minY) gt 0.) THEN BEGIN
	position_bar=fltarr(4)
	position_bar[0]=position[0]+0.3
	position_bar[1]=position[1]-0.1
	position_bar[2]=position[2]
	position_bar[3]=position_bar[1]+0.03
        colorbar,bottom=1,ncolors=nc+1,/horizontal,range=[minY,maxY],title=unit,divisions=divs, $
            	 format=fmt,position=position_bar,Minor=1,TickLen=0.0001,CharSize=0.8
      ENDIF
      IF ((maxY-minY) le 0.) THEN BEGIN
          print, 'Error: No color bar plotted: minY and maxY inconsistent.'
      ENDIF
  ENDIF
  
  ;---- write out image file
  write_png, img_name, TVRD(), r, g, b
  device,/close
END



Pro FOV_AMSUA, fov_a

  NUMSPOT_A=30
  PI=3.141593
  SCAN_ANG_A=3.333
  REARTH=6371.0
  RSAT=849.1
  i=0
  angle=0.0
  angle1=0.0
  angle2=0.0

  for i=0, NUMSPOT_A-1 do begin
    angle = PI * SCAN_ANG_A * (i - NUMSPOT_A/2.0 ) / 180.0 
    angle1 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

    angle = PI * SCAN_ANG_A * (i - NUMSPOT_A/2.0 + 1) / 180.0	
    angle2 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

    fov_a(i) = REARTH * ( ( angle1 - angle2 ) - PI * SCAN_ANG_A / 180.0 )
  endfor

End



Pro FOV_MHS, fov_m

  NUMSPOT_M=90
  PI=3.141593
  SCAN_ANG_M=1.111
  REARTH=6371.0
  RSAT=870.0
  i=0
  angle=0.0
  angle1=0.0
  angle2=0.0

  for i=0, NUMSPOT_M-1 do begin
    angle = PI * SCAN_ANG_M * (i - NUMSPOT_M/2.0 ) / 180.0 
    angle1 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

    angle = PI * SCAN_ANG_M * (i - NUMSPOT_M/2.0 + 1) / 180.0	
    angle2 = PI - ASIN( (REARTH + RSAT) * sin(angle) / REARTH )

    fov_m(i) = REARTH * ( ( angle1 - angle2 ) - PI * SCAN_ANG_M / 180.0 )
  endfor

End



;=======================================================================
; 
; This is to compute each scan position's FOV size of AMSUA cross track
; 
; 04/07/2010           Wanchun Chen 
;  
;======================================================================

Pro fov_amsua_cross_track, fov_cross_track

  N=30
  PI=3.14159
  R=6371.0
  H=849.1  ; the lowest altitude
  H=877.3  ; the highest altitude
  scan_angle = 3.333
  D2R = PI/180.0
  delta = scan_angle * D2R

  for i = 0, N-1 do begin

    theta = (i-N/2) * scan_angle * D2R
    alpha = PI - asin( (1+H/R) * sin(theta) )  

    theta1 = (i-N/2+1) * scan_angle * D2R
    beta = PI - asin( (1+H/R) * sin(theta1) )

    rad = alpha - beta - delta  
    fov_cross_track(i) =  R * rad

  endfor
  
End


;=======================================================================
; 
; This is to compute each scan position's FOV size of AMSUA along track
; 
; 04/07/2010           Wanchun Chen 
;  
;======================================================================

Pro fov_amsua_along_track, fov_along_track
  N=30
  PI=3.14159
  D2R = PI/180.0
  R=6371.0
  H=849.1
  H=877.3
  scan_angle_deg = 3.333
  scan_angle_rad = scan_angle_deg * D2R

  half = fltarr(N/2)

  for i = 1, N/2 do begin
    theta = i *  scan_angle_rad
    half(i-1) = H / cos(theta) * scan_angle_rad  
  endfor

  fov_along_track(0:N/2-1) = reverse(half)
  fov_along_track(N/2:N-1) = half(*)

End


;=======================================================================
; 
; This is to compute each scan position's FOV size of MHS cross track
; 
; 04/07/2010           Wanchun Chen 
;  
;======================================================================

Pro fov_mhs_cross_track, fov_cross_track
  N=90
  PI=3.1415926
  R=6371.0
  H=849.1
  H=877.3
  scan_angle = 1.111
  D2R = PI/180.0
  delta = scan_angle * D2R

  for i = 0, N-1 do begin

    theta = (i-N/2) * scan_angle * D2R
    alpha = PI - asin( (1+H/R) * sin(theta) )  

    theta1 = (i-N/2+1) * scan_angle * D2R
    beta = PI - asin( (1+H/R) * sin(theta1) )

    rad = alpha - beta - delta  
    fov_cross_track(i) = R * rad

  endfor

End


;=======================================================================
; 
; This is to compute each scan position's FOV size of MHS along track
; 
; 04/07/2010           Wanchun Chen 
;  
;======================================================================

Pro fov_mhs_along_track, fov_along_track
  N=90
  PI=3.14159
  D2R = PI/180.0
  R=6371.0
  H=849.1
  H=877.3
  scan_angle_deg = 1.111
  scan_angle_rad = scan_angle_deg * D2R
  half = fltarr(N/2)

  for i = 1, N/2 do begin
    theta = i *  scan_angle_rad
    half(i-1) = H / cos(theta) * scan_angle_rad  
  endfor

  fov_along_track(0:N/2-1) = reverse(half)
  fov_along_track(N/2:N-1) = half(*)

End



;=======================================================================
; 
;  This is more generic version of FOV sizes of cross track
;
;  input parameters:
;
;    N               - number of fov 
;    scan_angle_deg  - scan angle in degree
;    H               - satellite height
;
;  output parameter:
;
;   fovs_cross_track - array to hold fov sizes 
;
;  04/12/2011           Wanchun Chen 
;  
;======================================================================

Pro fov_cross_track, N, scan_angle_deg, H, fovs_cross_track
  
  PI=3.1415926
  R=6371.0

  D2R = PI/180.0
  delta = scan_angle_deg * D2R

  for i = 0, N-1 do begin

    theta = (i-N/2) * scan_angle_deg * D2R
    alpha = PI - asin( (1+H/R) * sin(theta) )  

    theta1 = (i-N/2+1) * scan_angle_deg * D2R
    beta = PI - asin( (1+H/R) * sin(theta1) )

    rad = alpha - beta - delta  
    fovs_cross_track(i) = R * rad

  endfor

End


;=======================================================================
; 
;  This is more generic version of FOV sizes of along track
;  
;  input parameters:
;
;    N               - number of fov 
;    scan_angle_deg  - scan angle in degree
;    H               - satellite height
;
;  output parameter:
;
;   fovs_along_track - array to hold fov sizes 
;
;  04/12/2011           Wanchun Chen 
;  
;======================================================================

Pro fov_along_track, N, scan_angle_deg, H, fovs_along_track
  PI=3.14159
  D2R = PI/180.0
  R=6371.0

  scan_angle_rad = scan_angle_deg * D2R
  half = fltarr(N/2)

  for i = 1, N/2 do begin
    theta = i *  scan_angle_rad
    half(i-1) = H / cos(theta) * scan_angle_rad  
  endfor

  fovs_along_track(0:N/2-1) = reverse(half)
  fovs_along_track(N/2:N-1) = half(*)

End



;===============================================================
; 
; Make an ellipse ( is a circle when xradius == yradius )
; angle -- counter-clockwise rotate angle in degree
; 
;===============================================================

FUNCTION MAKE_ELLIPSE, xcenter, ycenter, xradius, yradius, angle=angle
   ;npoints = 1024L
   npoints = 64L
   points = (2 * !PI / float(npoints-1.0)) * FINDGEN(npoints)
   
   IF N_ELEMENTS(angle) EQ 0 THEN BEGIN
     x = xcenter + xradius * COS( points )
     y = ycenter + yradius * SIN( points )
   ENDIF
  
   IF N_ELEMENTS(angle) EQ 1 THEN BEGIN
     x = xcenter + xradius * COS(points) * COS(angle * !dtor) - yradius * SIN(points) * SIN(angle * !dtor)
     y = ycenter + yradius * SIN(points) * COS(angle * !dtor) + xradius * COS(points) * SIN(angle * !dtor)
   ENDIF

   RETURN, TRANSPOSE([[x],[y]])
END


;===============================================================================================
; 
; high resolution plot
; fov_scale_cross - fov scale cross track
; fov_scale_along - fov scale along track
; nscanpos        - number of scan position
; border          - add border or not ( 0-no, 1-yes )
;
; color_table_index is optional
;
; Wanchun Chen    04/08/2010
; 
;===============================================================================================

PRO map_hr_ellipse, minlat,maxlat,minlon,maxlon,minY,maxY,xlat,xlon,xscan,xtab,ind,nscanpos,$
            fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thk,overlap,fmt,divs,$
	    img_name,map_dim,border,rotate_angle,color_table_index=color_table_index

  ;---- set Z buffer
  set_plot,'Z'
  device,set_resolution=map_dim

  xdim = map_dim[0]
  ydim = map_dim[1]

  position=[0.05, 0.125, 0.95, 0.95]
  if xdim gt 650 then position=[0.025, 0.025, 0.975, 0.975]
  
  ;---- load color table 33 if no color table given ----
  IF N_ELEMENTS(color_table_index) EQ 0 THEN BEGIN
    loadct, 33, /SILENT
    TVLCT, r, g, b, /get
  ENDIF
  
  ;---- if have color_table_index, then use it ----
  IF N_ELEMENTS(color_table_index) EQ 1 THEN BEGIN

    IF color_table_index GE 0 and color_table_index LE 40 THEN BEGIN
      loadct, color_table_index, /SILENT
      TVLCT, r, g, b, /get
    ENDIF

    IF color_table_index EQ 41 THEN BEGIN
      ; load MSPPS TPW colors
      loadct_tpw, r, g, b
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 42 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_sice, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 43 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_swe, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 44 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_climate, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    ;---- more color table options go here ----

  ENDIF
  
  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
  r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255

  drawColor = 255
  borderColor = 255
  charsz=0.75

  nrec=n_elements(ind)
  
  ;---- to be consistent with our plot_grid subroutine ----
  nc=239
  
  ;P0lat = (minlat+maxlat)/2
  ;P0lon = (minlon+maxlon)/2
  ;Rot   = 0

  latdiff=maxlat-minlat
  londiff=maxlon-minlon
  
  latdel=1
  londel=2
  
  if xdim gt 650 then begin
    latdel=1  
    londel=1  
    if londiff gt 45 then londel=2
  endif
  
  ;---- due to IDL having a limit on number of ticks allowed ----
  if latdiff ge  90 then latdel=5
  if londiff ge 180 then londel=10
  
  if xdim gt 650 then $
      map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	       title='', color=drawColor, charsize=charsz, /NOBORDER  
  
  if xdim le 650 then $
      map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	      title=tit, color=drawColor, charsize=charsz, /NOBORDER  

  ;---- plot fov symbol one by one ----
  FOR iprof=0L,nrec-1L DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc + 1
      indcol=long(xind) > 1L < nc
      iscanpos = xscan[ind(iprof)] - 1
      if iscanpos lt 0 or iscanpos ge nscanpos then iscanpos = 0
      xradius = fov_scale_cross( iscanpos ) * xscale
      yradius = fov_scale_along( iscanpos ) * yscale
      ellipse = MAKE_ELLIPSE( xlon[ind(iprof)], xlat[ind(iprof)], xradius, yradius, angle=rotate_angle)
      POLYFILL, ellipse, color=indcol
      if border eq 1 then PlotS, ellipse, color=borderColor
  ENDFOR
  
  map_continents,/continents,/noborder,/hires,/usa
  map_grid,latdel=latdel,londel=londel,color=drawColor

  ticks_lon=FIX((maxlon-minlon)/londel)
  ticks_lat=FIX((maxlat-minlat)/latdel)
  
  longitude = minlon + findgen(ticks_lon+1)*londel
  latitude  = minlat + findgen(ticks_lat+1)*latdel

  ;---- plot X-Y axis ----
  Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
     	POSITION=position, xrange=[minlon,maxlon], yrange=[minlat,maxlat], XTICKLEN=0.001, YTICKLEN=0.001, $
     	Color=drawColor, charsize=charsz, /NoData, /NoErase, xtitle='Longitude', ytitle='Latitude'
  
  ;---- plot tick values on top and bottom side of plot ----
  if xdim gt 650 then begin
    AXIS, XAXIS=1, XRANGE=[minlon,maxlon],XStyle=1,charsize=charsz,Xticks=ticks_lon,XTICKLEN=0.001,YTICKLEN=0.001,/SAVE
    AXIS, YAXIS=1, YRANGE=[minlat,maxlat],YStyle=1,charsize=charsz,Yticks=ticks_lat,XTICKLEN=0.001,YTICKLEN=0.001,/SAVE
  endif
  
  ;---- write out title for large one ----
  if xdim gt 650 then xyouts, 0.44, position[3] + 0.01, tit, /normal, color=drawColor
  
  ;---- plot a color bar ----
  position_bar=fltarr(4)
  position_bar[0]=position[0]+0.1
  position_bar[1]=position[1]*0.3
  position_bar[2]=position[2]-0.1
  position_bar[3]=position_bar[1]+0.01
  if xdim gt 650 then position_bar[3]=position_bar[1]+0.005
  colorbar,bottom=1,ncolors=nc+1,/horizontal,range=[minY,maxY],divisions=divs, $
           format=fmt,position=position_bar,Minor=1,TickLen=0.0001,charsize=charsz

  ;---- write out image file ----
  write_png, img_name, TVRD(), r, g, b
  device,/close
END



;===============================================================
; To plot high map_dim of QC Flags in MIRS
;
; 04/08/2010	Wanchun Chen
;
;===============================================================

PRO map_hr_qcf_ellipse, minlat,maxlat,minlon,maxlon,minY,maxY,xlat,xlon,xscan,xtab,ind,$
                nscanpos,fov_scale_cross,fov_scale_along,xscale,yscale,$
		tit,unit,thk,overlap,fmt,divs,img_name,map_dim,border,rotate_angle

  ;---- set Z buffer ----
  set_plot,'Z'
  device,set_resolution=map_dim
  
  xdim = map_dim[0]
  ydim = map_dim[1]
  
  position=[0.05, 0.15, 0.95, 0.95]
  if xdim gt 650 then position=[0.025, 0.025, 0.975, 0.975]
  
  ;---- load color table ----
  loadct, 39, /SILENT	
  TVLCT, r, g, b, /get
  ;0
  for i=1,23 do begin
     r(i)=245 & g(i)=245 & b(i)=220	;beige 
  endfor
  ;1:
  for i=24,46 do begin
    r(i)=255 & g(i)=0 & b(i)=0		;red [14,16] 
  endfor
  ;2:
  for i=47,69  do begin
    r(i)=51 & g(i)=102 & b(i)=255	;light blue
  endfor
  ;3:
  for i=70,92 do begin
    r(i)=0 & g(i)=255 & b(i)=255	;aqua [0, 2]
  endfor
  ;4:
  for i=93,115 do begin
    r(i)=0 & g(i)=0 & b(i)=255		;blue [2,4]
  endfor
  ;5:
  for i=116,138 do begin
    r(i)=50 & g(i)=205 & b(i)=50	;light green [6,8]
  endfor
  ;6:
  for i=139,161 do begin
     r(i)=0 & g(i)=255 & b(i)=0		;bright green 
  endfor
  ;7:
  for i=162,184 do begin
     r(i)=0 & g(i)=102 & b(i)=0		;dark green
  endfor
  ;8:
  for i=185,207 do begin
    r(i)=255 & g(i)=205 & b(i)=0	;dark yellow
  endfor
  ;9:
  for i=208,230 do begin
    r(i)=255 & g(i)=90 & b(i)=0		;orange
  endfor

  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
  r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
  r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)

  drawColor = 255
  borderColor = 255
  charsz=0.75
  nrec=n_elements(ind)
  nc=239    ; to be consistent with our plot_grid subroutine
  
  latdiff=maxlat-minlat
  londiff=maxlon-minlon
      
  latdel=1
  londel=2
  
  if xdim gt 650 then begin
    latdel=1  
    londel=1  
    if londiff gt 45 then londel=2
  endif
  
  if xdim le 650 then $
      map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	       title=tit, color=drawColor, charsize=charsz, /NOBORDER
  
  if xdim gt 650 then $
      map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	       title='', color=drawColor, charsize=charsz, /NOBORDER

  ;---- plot fov symbol one by one ----
  FOR iprof=0L,nrec-1 DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc + 1
      indcol=long(xind) > 1L < nc
      iscanpos = xscan[ind(iprof)] - 1
      if iscanpos lt 0 or iscanpos gt nscanpos-1 then iscanpos = 0
      xradius = fov_scale_cross( iscanpos ) * xscale
      yradius = fov_scale_along( iscanpos ) * yscale
      ellipse = MAKE_ELLIPSE( xlon[ind(iprof)], xlat[ind(iprof)], xradius, yradius, angle=rotate_angle )
      POLYFILL, ellipse, color=indcol
      if border eq 1 then PlotS, ellipse, color=drawColor
  ENDFOR
  
  map_continents,/continents,/noborder,/hires,/usa
  map_grid,latdel=latdel,londel=londel,color=drawColor
  
  ticks_lon=FIX((maxlon-minlon)/londel)
  ticks_lat=FIX((maxlat-minlat)/latdel)

  longitude = minlon + findgen(ticks_lon+1)*londel
  latitude  = minlat + findgen(ticks_lat+1)*latdel

  ;---- plot X-Y axis ----
  Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
     	POSITION=position, xrange=[minlon,maxlon], yrange=[minlat,maxlat], $
     	Color=drawColor, charsize=charsz, /NoData, /NoErase, xtitle='Longitude', ytitle='Latitude'
  
  ;---- plot tick values on top and bottom side of plot ----
  if xdim gt 650 then begin
    AXIS, XAXIS=1, XRANGE=[minlon,maxlon],XStyle=1,charsize=charsz,Xticks=ticks_lon,/SAVE
    AXIS, YAXIS=1, YRANGE=[minlat,maxlat],YStyle=1,charsize=charsz,Yticks=ticks_lat,/SAVE
  endif
  
  ;---- write out title for large one ----
  if xdim gt 650 then xyouts, 0.44, position[3] + 0.01, tit, /normal, color=drawColor
  
  if xdim le 650 then begin

    ;---- plot a color bar ----
    position_bar=fltarr(4)
    position_bar[0]=position[0]
    position_bar[1]=position[1]*0.25
    position_bar[2]=position[2]
    position_bar[3]=position_bar[1]+0.01
    colorbar,bottom=1,ncolors=230,/horizontal,range=[minY,maxY],divisions=divs, $
             format=fmt,position=position_bar,Minor=1,TickLen=0.0001,charsize=charsz

    legen1='0:GOOD'
    legen2='1:BAD'
    legen3='PRECIP:'
    legen4='2(LGHT)'
    legen5='3(MED)'
    legen6='4(HVY)'
    legen7='INVERS:'
    legen8='5(TMP)'
    legen9='6(HUM)'
    legen10='7(TMP+HUM)'
    legen11='SATUR:'
    legen12='8(CLD/PRECIP)'
    legen13='9(noCLD/noPRECIP)'
    
    ypos = position_bar[3]+0.01
    colors = [ 1,24,47,70,93,116,139,162,185,208 ]
    
    XYOUTS, 0.000, ypos, legen1, charsize=charsz,charthick=1.0, /normal
    XYOUTS, 0.060, ypos, legen2, charsize=charsz,charthick=1.0,color=24, /normal

    XYOUTS, 0.115, ypos, legen3, charsize=charsz,charthick=1.0, /normal
    XYOUTS, 0.180, ypos, legen4, charsize=charsz,charthick=1.0,color=47, /normal
    XYOUTS, 0.250, ypos, legen5, charsize=charsz,charthick=1.0,color=70, /normal
    XYOUTS, 0.310, ypos, legen6, charsize=charsz,charthick=1.0,color=93, /normal

    XYOUTS, 0.370, ypos, legen7, charsize=charsz,charthick=1.0, /normal
    XYOUTS, 0.430, ypos, legen8, charsize=charsz,charthick=1.0,color=116, /normal
    XYOUTS, 0.490, ypos, legen9, charsize=charsz,charthick=1.0,color=139, /normal
    XYOUTS, 0.550, ypos, legen10,charsize=charsz,charthick=1.0,color=162, /normal

    XYOUTS, 0.660, ypos, legen11,charsize=charsz,charthick=1.0, /normal
    XYOUTS, 0.715, ypos, legen12,charsize=charsz,charthick=1.0,color=185, /normal
    XYOUTS, 0.840, ypos, legen13,charsize=charsz,charthick=1.0,color=208, /normal
 
  endif else begin
    
    ;---- plot a color bar ----
    position_bar=fltarr(4)
    position_bar[0]=position[0]+0.075
    position_bar[1]=position[1]*0.25
    position_bar[2]=position[2]-0.075
    position_bar[3]=position_bar[1]+0.005
    colorbar,bottom=1,ncolors=230,/horizontal,range=[minY,maxY],divisions=divs, $
             format=fmt,position=position_bar,Minor=1,TickLen=0.0001,charsize=charsz

    legends = ['GOOD','BAD','LIGHT PRECIP','MEDIUM PRECIP','HEAVY PRECIP',$
               'INVERS(TMP)','INVERS(HUM)','INVERS(TMP+HUM)','SATUR(CLD/PRECIP)','SATUR(noCLD/noPRECIP)']
    
    xpos = 0.1 + findgen(10) * 0.08
    ypos = position_bar[3] + 0.001
    colors = [ 24,24,47,70,93,116,139,162,185,208 ]
    
    for i = 0, 9 do begin
      XYOUTS, xpos[i], ypos, legends[i], charsize=charsz, color=colors[i], /normal
    endfor

  endelse

  ;---- write out image file ----
  write_png, img_name, TVRD(), r, g, b
  device,/close

END




;===============================================================
; P2P version of Plot
;
; 10/26/2009	Wanchun Chen
; xscan     --- scan position
; symbsize  --- base size(near nadir symbol size to be used)
; fova_a    --- amsua foot print size scale
;
; color_table_index is optional
;
;===============================================================
PRO p2p_plot,minlat,maxlat,minlon,maxlon,minY,maxY, xlat,xlon,xscan,xtab,ind,fov_scale, tit,unit,$
    symbol, symbsize, thk, overlap, fmt, divs, img_name, color_table_index=color_table_index

  ;---- set Z buffer
  set_plot,'Z'
  device,set_resolution=[650, 500]
  position=[0.05, 0.15, 0.95, 0.9]
  
  ;---- load color table 33 if no color table given
  IF N_ELEMENTS(color_table_index) EQ 0 THEN BEGIN
    loadct, 33, /SILENT
    TVLCT, r, g, b, /get
  ENDIF
  
  ;---- if have color_table_index, then use it
  IF N_ELEMENTS(color_table_index) EQ 1 THEN BEGIN

    IF color_table_index GE 0 and color_table_index LE 40 THEN BEGIN
      loadct, color_table_index, /SILENT
      TVLCT, r, g, b, /get
    ENDIF

    IF color_table_index EQ 41 THEN BEGIN
      ; load MSPPS TPW colors
      loadct_tpw, r, g, b
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 42 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_sice, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 43 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_swe, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 44 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_climate, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    ;---- more options go here

  ENDIF
  
  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
  drawColor = 255
  charsz0=1.

  nrec=n_elements(ind)

  nc=239    ; to be consistent with our plot_grid subroutine
  csize=16
  a = findgen(csize+1) * (!PI*2.5/float(csize))
  usersym,symbsize*cos(a)/2,symbsize*sin(a)/2,/fill
  
  IF (overlap eq 0) THEN BEGIN
      center_lon=0
      
      latdiff=maxlat-minlat
      if( latdiff le 45                    ) then latdel=5
      if( latdiff gt 45 and latdiff le 90  ) then latdel=10
      if( latdiff gt 90                    ) then latdel=15
      
      londiff=maxlon-minlon
      if( londiff le 45                    ) then londel=5
      if( londiff gt 45 and londiff le 90  ) then londel=10
      if( londiff gt 90 and londiff le 120 ) then londel=15
      if( londiff gt 120                   ) then londel=30
      
      map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	       title=tit, color=drawColor, charsize=0.75, /NOBORDER  
  ENDIF
  
  FOR iprof=0L,nrec-1 DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc + 1
      indcol=long(xind) > 1L < nc
      iscanpos = xscan[ind(iprof)] - 1
      if iscanpos lt 0 or iscanpos gt 29 then iscanpos = 0
      scale = symbsize * fov_scale( iscanpos )
      oplot,[xlon[ind(iprof)]],[xlat[ind(iprof)]],color=indcol,psym=symbol, symsize=scale, thick=thk
  ENDFOR
  
  map_continents,/continents,/noborder,/hires,/usa
  IF (overlap eq 0) THEN BEGIN
      map_grid,latdel=latdel,londel=londel,color=drawColor
      
     ticks_lon=FIX((maxlon-minlon)/londel)
     ticks_lat=FIX((maxlat-minlat)/latdel)

     longitude = minlon + findgen(ticks_lon+1)*londel
     latitude  = minlat + findgen(ticks_lat+1)*latdel

     ;---- plot X-Y axis
     Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
     	   POSITION=position, xrange=[minlon,maxlon], yrange=[minlat,maxlat], $
     	   Color=drawColor, Charsize=0.75, /NoData, /NoErase
      
      IF ((maxY-minY) gt 0.) THEN BEGIN
	position_bar=fltarr(4)
	position_bar[0]=position[0]+0.1
	position_bar[1]=position[1]-0.1
	position_bar[2]=position[2]-0.1
	position_bar[3]=position_bar[1]+0.03
        colorbar,bottom=1,ncolors=nc+1,/horizontal,range=[minY,maxY],title=unit,divisions=divs, $
            	 format=fmt,position=position_bar,Minor=1,TickLen=0.0001,CharSize=0.75
      ENDIF
      IF ((maxY-minY) le 0.) THEN BEGIN
          print, 'Error: No color bar plotted: minY and maxY inconsistent.'
      ENDIF
  ENDIF
  
  ;---- write out image file
  write_png, img_name, TVRD(), r, g, b
  device,/close
END


;===============================================================
; PNG version of mapPlot of QCF
;
; 10/16/2008	Wanchun Chen
;
; color_table_file is optional here
;
;===============================================================
PRO mapPlot_qcf,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtab,unit,scal,symb,thk,overlap,fmt,divs,img_name

  ;---- set Z buffer
  set_plot,'Z'
  erase
  device,set_resolution=[650, 500]
  position=[0.05, 0.15, 0.95, 0.9]
  
  ;---- load color table
  loadct, 39, /SILENT
  TVLCT, r, g, b, /get
  ;0
  for i=1,11 do begin
     r(i)=245 & g(i)=245 & b(i)=220	;beige 
  endfor
  ;1:
  for i=12,38 do begin
    r(i)=255 & g(i)=0 & b(i)=0		;red [14,16] 
  endfor
  ;2:
  for i=39,65  do begin
    r(i)=51 & g(i)=102 & b(i)=255	;light blue
  endfor
  ;3:
  for i=66,92 do begin
    r(i)=0 & g(i)=255 & b(i)=255	;aqua [0, 2]
  endfor
  ;4:
  for i=93,119 do begin
    r(i)=0 & g(i)=0 & b(i)=255		;blue [2,4]
  endfor
  ;5:
  for i=120,146 do begin
    r(i)=50 & g(i)=205 & b(i)=50	;light green [6,8]
  endfor
  ;6:
  for i=147,173 do begin
     r(i)=0 & g(i)=255 & b(i)=0		;bright green 
  endfor
  ;7:
  for i=174,200 do begin
     r(i)=0 & g(i)=102 & b(i)=0		;dark green
  endfor
  ;8:
  for i=201,227 do begin
    r(i)=255 & g(i)=205 & b(i)=0	;dark yellow
  endfor
  ;9:
  for i=228,239 do begin
    r(i)=255 & g(i)=90 & b(i)=0		;orange
  endfor

  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
  r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
  r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
  drawColor = 255
  
  charsz0=1.
  nrec=n_elements(ind)

  nc=239    ; to be consistent with our plot_grid subroutine
  csize=16
  a = findgen(csize+1) * (!PI*2.5/float(csize))
  usersym,scal*cos(a)/2,scal*sin(a)/2,/fill
  
  IF (overlap eq 0) THEN BEGIN
      center_lon=0
      
      latdiff=maxlat-minlat
      if( latdiff le 45 ) then latdel=5
      if( latdiff gt 45 and latdiff le 90 ) then latdel=10
      if( latdiff gt 90 ) then latdel=15
      
      londiff=maxlon-minlon
      if( londiff le 45 ) then londel=5
      if( londiff gt 45 and londiff le 90 ) then londel=10
      if( londiff gt 90 and londiff le 120 ) then londel=15
      if( londiff gt 120 ) then londel=30
      
      map_set,0,center_lon,/noerase,xmargin=[8,8],ymargin=[38,38],$
              limit=[minlat,minlon,maxlat,maxlon],/hires,title=tit,$
              position=position,latdel=latdel,londel=londel,/grid,charsize=charsz0*0.8
  ENDIF
  
  FOR iprof=0L,nrec-1 DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc + 1
      indcol=long(xind) > 1L < nc
      oplot,[xlon[ind(iprof)]],[xlat[ind(iprof)]],color=indcol,psym=symb,symsize=scal,thick=thk
  ENDFOR
  
  map_continents,/continents,/noborder,/hires,/usa

  ;---Labels
  legen1='0:GOOD'
  legen2='1:BAD'
  legen3='PRECIP:'
  legen4='2(LGHT)'
  legen5='3(MED)'
  legen6='4(HVY)'
  legen7='INVERS:'
  legen8='5(TMP)'
  legen9='6(HUM)'
  legen10='7(TMP+HUM)'
  legen11='SATUR:'
  legen12='8(CLD/PRECIP)'
  legen13='9(noCLD/noPRECIP)'

  XYOUTS, 0.000, 0.0175, legen1, charsize=0.75,charthick=1.0, /normal
  XYOUTS, 0.060, 0.0175, legen2, charsize=0.75,charthick=1.0,color=12, /normal
  
  XYOUTS, 0.115, 0.0175, legen3, charsize=0.75,charthick=1.0, /normal
  XYOUTS, 0.180, 0.0175, legen4, charsize=0.75,charthick=1.0,color=39, /normal
  XYOUTS, 0.250, 0.0175, legen5, charsize=0.75,charthick=1.0,color=66, /normal
  XYOUTS, 0.310, 0.0175, legen6, charsize=0.75,charthick=1.0,color=93, /normal
  
  XYOUTS, 0.370, 0.0175, legen7, charsize=0.75,charthick=1.0, /normal
  XYOUTS, 0.430, 0.0175, legen8, charsize=0.75,charthick=1.0,color=120, /normal
  XYOUTS, 0.490, 0.0175, legen9, charsize=0.75,charthick=1.0,color=147, /normal
  XYOUTS, 0.550, 0.0175, legen10,charsize=0.75,charthick=1.0,color=174, /normal
  
  XYOUTS, 0.660, 0.0175, legen11,charsize=0.75,charthick=1.0, /normal
  XYOUTS, 0.715, 0.0175, legen12,charsize=0.75,charthick=1.0,color=201, /normal
  XYOUTS, 0.840, 0.0175, legen13,charsize=0.75,charthick=1.0,color=228, /normal

  IF (overlap eq 0) THEN BEGIN
      map_grid,latdel=latdel,londel=londel,/label,lonlab=minlat,latlab=minlon,lonalign=0.,latalign=0.,charsize=charsz0*0.8
      IF ((maxY-minY) gt 0.) THEN BEGIN
	position_bar=fltarr(4)
	position_bar[0]=position[0]+0.3
	position_bar[1]=position[1]-0.08
	position_bar[2]=position[2]
	position_bar[3]=position_bar[1]+0.03
        colorbar,bottom=1,ncolors=nc+1,Color=drawColor,range=[minY,maxY],divisions=divs, $
            	 format=fmt,position=position_bar,Minor=1,TickLen=0.0001,CharSize=0.8
      ENDIF
      IF ((maxY-minY) le 0.) THEN BEGIN
          print, 'Error: No color bar plotted: minY and maxY inconsistent.'
      ENDIF
  ENDIF
  ;---- write out image file
  write_png, img_name, TVRD(), r, g, b
END


;===============================================================
; Name:		PLOTmap
;
;
; Type:		IDL Subroutine
;
;
; Description:  This subroutine uses the low level subroutine mapPlot
;               It loops over a list of channels, and maps out 
;               the brightness temperatures of measurements that
;               satisfy the criteria of latitude,longitude,pass, etc.
;               Note that in this case, the TB, lat, etc are organized
;               by scanline and by file.
;
;
; Arguments:
;
;	    Name		    Type	    Description
;      ---------------------------------------------------
;	- minlat             I              Min lat of region to be mapped
;	- maxlat             I              Max lat of region to be mapped
;	- minlon             I              Min lon of region to be mapped
;	- maxlon             I              Max lon of region to be mapped
;	- LatTab             I              Vector of latitudes to be mapped
;	- LonTab             I              Vector of longitudes to be mapped
;	- tit                I              Map title to be used
;	- minY0              I              Min range of xtab to be mapped
;	- maxY0              I              Max range of xtab to be mapped
;	- TBsTab             I              TB Array to be mapped
;	- symsize            I              Symbol size factor to use 
;	- ichan2plot         I              Which channel to plot
;	- mDay               I              Day to plot
;	- Node2plot          I              Which orbit mode to plot
;	- xnod               O              Not used for now
;	- nList              I              Number of files to process
;	- nScanLTot          I              Vector of # scanlines per file
;	- nFovs              I              # FOVs per scanline
;       - DayTab             I              Day array corresponding to measurmts
;	- NodeTab            I              Array repr orbit modes
;	- suff               I              Not used for now
;
;
; Subroutines needed:
;       - mapPlot
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO PLOTmap,symsize,ichan2plot,mDay,Node2plot,xnod,nList,nScanLTot,nFovs,LatTab,LonTab,$
            TBsTab,DayTab,NodeTab,minLat,maxLat,minLon,maxLon,minY0,maxY0,suff,tit
  iplot=0
  x=TBsTab(*,ichan2plot,*,*)
  ind=where(lattab ge minLat and lattab le maxLat and lontab ge minLon and lontab le maxLon and x gt 0.,ncount)
  minY=min(x(ind))
  maxY=max(x(ind))
  dY=maxY-minY
  ;maxY=maxY-dy/10.
  ;minY=minY+dy/10.
  maxY=min([maxY,maxY0])
  minY=max([minY,minY0])
  FOR ifile=0,nList-1 DO BEGIN
      FOR iscan=0,nScanLTot(ifile)-1 DO BEGIN
          xlat = LatTab(0:nFovs-1,iscan,ifile)
          xlon = LonTab(0:nFovs-1,iscan,ifile)
          xtb  = TBsTab(0:nFovs-1,ichan2plot,iscan,ifile)
          xday = DayTab(iscan,ifile)
          xnod = NodeTab(iscan,ifile)
          IF (Node2plot ne 2) THEN testOndirec=(xnod eq Node2plot)
          IF (Node2plot eq 2) THEN testOndirec=(xnod eq xnod)
          ind  = where(xlat ge minLat and xlat le maxLat and xlon ge minLon and xlon le maxLon and xtb gt 0.,ncount)
          IF (testOndirec and fix(xday) eq fix(mDay) and ncount gt 0) THEN BEGIN
              IF (iplot eq 0) THEN BEGIN
                  mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtb,'[K]',symsize,8,1,0,'(f7.2)'
                  iplot=1
              ENDIF
              IF (iplot ne 0) THEN BEGIN
                  mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtb,'[K]',symsize,8,1,1,'(f7.2)'
              ENDIF
          ENDIF
      ENDFOR
  ENDFOR
END

;===============================================================
; Name:		PLOTmapdiff
;
;
; Type:		IDL Subroutine
;
;
; Description: This subrotines performs similarly to PLOTmap execpt
;              it deals with differences between two sets of 
;              measurements. All arguments are similar. We list only
;              those that are not redundant with the above.
;
;
; Arguments:
;
;	    Name		    Type	    Description
;      ---------------------------------------------------
;	- minlat             I              Min lat of region to be mapped
;	- maxlat             I              Max lat of region to be mapped
;	- minlon             I              Min lon of region to be mapped
;	- maxlon             I              Max lon of region to be mapped
;	- LatTab             I              Vector of latitudes to be mapped
;	- LonTab             I              Vector of longitudes to be mapped
;	- tit                I              Map title to be used
;	- minY0              I              Min range of xtab to be mapped
;	- maxY0              I              Max range of xtab to be mapped
;	- TBsTab             I              TB Array to be mapped
;	- symsize            I              Symbol size factor to use 
;	- ichan2plot1        I              1st channel to use 2 compute diff 
;	- ichan2plot2        I              2nd channel to use 2 compute diff 
;	- mDay               I              Day to plot
;	- Node2plot          I              Which orbit mode to plot
;	- xnod               O              Not used for now
;	- nList              I              Number of files to process
;	- nScanLTot          I              Vector of # scanlines per file
;	- nFovs              I              # FOVs per scanline
;       - DayTab             I              Day array corresponding to measurmts
;	- NodeTab            I              Array repr orbit modes
;	- suff               I              Not used for now
;
;
; Subroutines needed:
;       - mapPlot
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO PLOTmapdiff,symsize,ichan2plot1,ichan2plot2,mDay,Node2plot,xnod,nList,nScanLTot,nFovs,LatTab,LonTab,$
            TBsTab,DayTab,NodeTab,minLat,maxLat,minLon,maxLon,minY0,maxY0,suff,tit
  iplot=0
  x1=TBsTab(*,ichan2plot1,*,*)
  x2=TBsTab(*,ichan2plot2,*,*)
  x=x1-x2
  ind=where(lattab ge minLat and lattab le maxLat and lontab ge minLon and lontab le maxLon and x1 gt 0.,ncount)
  minY=min(x(ind))
  maxY=max(x(ind))
  dY=maxY-minY
  ;maxY=maxY-dy/10.
  minY=minY-dy/20.
  maxY=min([maxY,maxY0])
  minY=max([minY,minY0])
  FOR ifile=0,nList-1 DO BEGIN
      FOR iscan=0,nScanLTot(ifile)-1 DO BEGIN
          xlat = LatTab(0:nFovs-1,iscan,ifile)
          xlon = LonTab(0:nFovs-1,iscan,ifile)
          xtb1 = TBsTab(0:nFovs-1,ichan2plot1,iscan,ifile)
          xtb2 = TBsTab(0:nFovs-1,ichan2plot2,iscan,ifile)
          xtb  = xtb1 - xtb2
          xday = DayTab(iscan,ifile)
          xnod = NodeTab(iscan,ifile)
          IF (Node2plot ne 2) THEN testOndirec=(xnod eq Node2plot)
          IF (Node2plot eq 2) THEN testOndirec=(xnod eq xnod)
          ind  = where(xlat ge minLat and xlat le maxLat and xlon ge minLon and xlon le maxLon and xtb1 gt 0.,ncount)
          IF (testOndirec and fix(xday) eq fix(mDay) and ncount gt 0) THEN BEGIN
              IF (iplot eq 0) THEN BEGIN
                  mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtb,'[K]',symsize,8,1,0,'(f7.2)'
                  iplot=1
              ENDIF
              IF (iplot ne 0) THEN BEGIN
                  mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtb,'[K]',symsize,8,1,1,'(f7.2)'
              ENDIF
          ENDIF
      ENDFOR
  ENDFOR
END


;===============================================================
; To plot high resolution map, point by point plots.
; isQc = 0, not draw qc fail points;
; isQc = 1, draw qc failed points;
;
; 10/27/2010	Wanchun Chen
;
;===============================================================

PRO map_hr, minlat,maxlat,minlon,maxlon,minY,maxY,xlat,xlon,xscan,xtab,ind,nscanpos,qcs,isQc,$
            fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thk,overlap,fmt,divs,$
	    img_name,map_dim,border,rotate_angle,color_table_index=color_table_index

  ;---- set Z buffer ----
  set_plot,'Z'
  device,set_resolution=map_dim

  xdim = map_dim[0]
  ydim = map_dim[1]

  position=[0.05, 0.125, 0.95, 0.95]
  
  ;---- load color table 33 if no color table given ----
  IF N_ELEMENTS(color_table_index) EQ 0 THEN BEGIN
    loadct, 33, /SILENT
    TVLCT, r, g, b, /get
  ENDIF
  
  ;---- if have color_table_index, then use it ----
  IF N_ELEMENTS(color_table_index) EQ 1 THEN BEGIN

    IF color_table_index GE 0 and color_table_index LE 40 THEN BEGIN
      loadct, color_table_index, /SILENT
      TVLCT, r, g, b, /get
    ENDIF

    IF color_table_index EQ 41 THEN BEGIN
      ; load MSPPS TPW colors
      loadct_tpw, r, g, b
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 42 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_sice, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 43 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_swe, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 44 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_climate, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    ;---- more color table options go here ----

  ENDIF
  
  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
  r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255

  drawColor = 255
  borderColor = 255
  charsz = 0.8
  qcFailColor = 252
  
  nrec=n_elements(ind)
  
  ;---- to be consistent with our plot_grid subroutine ----
  nc=239

  latdiff=maxlat-minlat
  londiff=maxlon-minlon
  
  latdel=1
  londel=2
  
  if latdiff le 15 then latdel = 1
  if latdiff gt 15 and latdiff le 30 then latdel = 2
  if latdiff gt 30 and latdiff le 60 then latdel = 5
  if latdiff gt 60 then latdel = 15
 
  if londiff le 30 then londel = 2
  if londiff gt 30 and londiff le 60 then londel = 5
  if londiff gt 60 and londiff le 120 then londel = 10
  if londiff gt 120 then londel = 30
  
  map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	    title=tit, color=drawColor, charsize=charsz, /NOBORDER  

  ;---- plot fov symbol one by one ----
  FOR iprof=0L,nrec-1L DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc + 1
      indcol=long(xind) > 1L < nc
      
      if qcs(ind(iprof)) eq 2 and isQc eq 1 then indcol = qcFailColor  ; ---- add qc fail  
      
      iscanpos = xscan[ind(iprof)] - 1
      if iscanpos lt 0 or iscanpos ge nscanpos then iscanpos = 0
      xradius = fov_scale_cross( iscanpos ) * xscale
      yradius = fov_scale_along( iscanpos ) * yscale
      xcenter = xlon[ind(iprof)]
      ycenter = xlat[ind(iprof)]
      xxxx = [ xcenter-xradius, xcenter+xradius, xcenter+xradius, xcenter-xradius ]
      yyyy = [ ycenter-yradius, ycenter-yradius, ycenter+yradius, ycenter+yradius ]
      if( xcenter-xradius ge minlon and xcenter+xradius le maxlon and ycenter-yradius ge minlat and ycenter+yradius le maxlat ) then begin
        POLYFILL, xxxx, yyyy, color=indcol
      endif	
  ENDFOR
  
  map_continents,/continents,/noborder,/hires,/usa
  ;map_grid,latdel=latdel,londel=londel,color=drawColor

  ticks_lon=FIX((maxlon-minlon)/londel)
  ticks_lat=FIX((maxlat-minlat)/latdel)
  
  longitude = minlon + findgen(ticks_lon+1)*londel
  latitude  = minlat + findgen(ticks_lat+1)*latdel

  ;---- plot X-Y axis ----
  Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
     	POSITION=position, xrange=[minlon,maxlon], yrange=[minlat,maxlat], xticklen=0.01, yticklen=0.01, $
     	Color=drawColor, charsize=charsz, /NoData, /NoErase;, xtitle='Longitude', ytitle='Latitude'
  
  ;---- plot a color bar ----
  position_bar=fltarr(4)
  position_bar[0]=position[0]+0.2
  position_bar[1]=position[1]-0.085
  position_bar[2]=position[2]-0.1
  position_bar[3]=position_bar[1]+0.03
  colorbar,bottom=1,ncolors=nc+1,/horizontal,range=[minY,maxY],divisions=divs, $
           format=fmt,position=position_bar,Minor=1,TickLen=0.0001,charsize=charsz
  
  ;---- do we add qc failed legend or not ----
  if isQc eq 1 then begin
    ;---- Plot QC fail lengend (gray)
    qc_position=fltarr(4)
    qc_position[0]=position[0]+0.075
    qc_position[1]=position[1]-0.085
    qc_position[2]=position[0]+0.125
    qc_position[3]=qc_position[1]+0.03
    ColorBar,Bottom=252,NColors=1, Position=qc_position, Divisions=2, COLOR=drawColor,$
	     Minor=1,TickLen=0.0001,TickNames=[' ','QC fail',' '],CharSize=0.8
  endif
  
  ;---- write out image file ----
  write_png, img_name, TVRD(), r, g, b
  device,/close
END

;===============================================================
; To plot high resolution map, point by point plots.
; isQc = 0, not draw qc fail points;
; isQc = 1, draw qc failed points;
;
; 10/27/2010	Wanchun Chen
; adapted for plotting over land
;
;===============================================================

PRO map_hr_lnd, minlat,maxlat,minlon,maxlon,minY,maxY,xlat,xlon,xscan,xtab,sfc,ind,nscanpos,qcs,isQc,$
            fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thk,overlap,fmt,divs,$
	    img_name,map_dim,border,rotate_angle,color_table_index=color_table_index

  ;---- set Z buffer ----
  set_plot,'Z'
  device,set_resolution=map_dim

  xdim = map_dim[0]
  ydim = map_dim[1]

  position=[0.05, 0.125, 0.95, 0.95]
  
  ;---- load color table 33 if no color table given ----
  IF N_ELEMENTS(color_table_index) EQ 0 THEN BEGIN
    loadct, 33, /SILENT
    TVLCT, r, g, b, /get
  ENDIF
  
  ;---- if have color_table_index, then use it ----
  IF N_ELEMENTS(color_table_index) EQ 1 THEN BEGIN

    IF color_table_index GE 0 and color_table_index LE 40 THEN BEGIN
      loadct, color_table_index, /SILENT
      TVLCT, r, g, b, /get
    ENDIF

    IF color_table_index EQ 41 THEN BEGIN
      ; load MSPPS TPW colors
      loadct_tpw, r, g, b
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 42 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_sice, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 43 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_swe, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    IF color_table_index EQ 44 THEN BEGIN
      ; load MSPPS Climate colors
      loadct_climate, r, g, b 
      TVLCT, r, g, b
    ENDIF  

    ;---- more color table options go here ----

  ENDIF
  
  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
  r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255

  drawColor = 255
  borderColor = 255
  charsz = 0.8
  qcFailColor = 252
  lndColor = 253
  seaColor = 253
  
  nrec=n_elements(ind)
  
  ;---- to be consistent with our plot_grid subroutine ----
  nc=239

  latdiff=maxlat-minlat
  londiff=maxlon-minlon
  
  latdel=1
  londel=2
  
  if latdiff le 15 then latdel = 1
  if latdiff gt 15 and latdiff le 30 then latdel = 2
  if latdiff gt 30 and latdiff le 60 then latdel = 5
  if latdiff gt 60 then latdel = 15
 
  if londiff le 30 then londel = 2
  if londiff gt 30 and londiff le 60 then londel = 5
  if londiff gt 60 and londiff le 120 then londel = 10
  if londiff gt 120 then londel = 30
  
  map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	    title=tit, color=drawColor, charsize=charsz, /NOBORDER  

  ;---- plot fov symbol one by one ----
  FOR iprof=0L,nrec-1L DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc + 1
      indcol=long(xind) > 1L < nc
      
;      if qcs(ind(iprof)) eq 2 and isQc eq 1 then indcol = qcFailColor  ; ---- add qc fail  
      if qcs(ind(iprof)) ge 2 and isQc eq 1 then indcol = qcFailColor  ; ---- add qc fail  
      if xtab(ind(iprof)) eq -999.0 then indcol = 0  ; ---- add missing/no data
;      if sfc(ind(iprof)) ge 2 then indcol = lndColor  ; ---- add land 
      if sfc(ind(iprof)) le 1 then indcol = seaColor  ; ---- add ocean
      
      iscanpos = xscan[ind(iprof)] - 1
      if iscanpos lt 0 or iscanpos ge nscanpos then iscanpos = 0
      xradius = fov_scale_cross( iscanpos ) * xscale
      yradius = fov_scale_along( iscanpos ) * yscale
      xcenter = xlon[ind(iprof)]
      ycenter = xlat[ind(iprof)]
      xxxx = [ xcenter-xradius, xcenter+xradius, xcenter+xradius, xcenter-xradius ]
      yyyy = [ ycenter-yradius, ycenter-yradius, ycenter+yradius, ycenter+yradius ]
      if( xcenter-xradius ge minlon and xcenter+xradius le maxlon and ycenter-yradius ge minlat and ycenter+yradius le maxlat ) then begin
        POLYFILL, xxxx, yyyy, color=indcol
      endif	
  ENDFOR
  
  map_continents,/continents,/noborder,/hires,/usa
  ;map_grid,latdel=latdel,londel=londel,color=drawColor

  ticks_lon=FIX((maxlon-minlon)/londel)
  ticks_lat=FIX((maxlat-minlat)/latdel)
  
  longitude = minlon + findgen(ticks_lon+1)*londel
  latitude  = minlat + findgen(ticks_lat+1)*latdel

  ;---- plot X-Y axis ----
  Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
     	POSITION=position, xrange=[minlon,maxlon], yrange=[minlat,maxlat], xticklen=0.01, yticklen=0.01, $
     	Color=drawColor, charsize=charsz, /NoData, /NoErase;, xtitle='Longitude', ytitle='Latitude'
  
  ;---- plot a color bar ----
  position_bar=fltarr(4)
  position_bar[0]=position[0]+0.2
  position_bar[1]=position[1]-0.085
  position_bar[2]=position[2]-0.1
  position_bar[3]=position_bar[1]+0.03
  colorbar,bottom=1,ncolors=nc+1,/horizontal,range=[minY,maxY],divisions=divs, $
           format=fmt,position=position_bar,Minor=1,TickLen=0.0001,charsize=charsz
  
  ;---- do we add qc failed legend or not ----
  if isQc eq 1 then begin
    ;---- Plot QC fail lengend (gray)
    qc_position=fltarr(4)
    qc_position[0]=position[0]+0.075
    qc_position[1]=position[1]-0.085
    qc_position[2]=position[0]+0.125
    qc_position[3]=qc_position[1]+0.03
    ColorBar,Bottom=252,NColors=1, Position=qc_position, Divisions=2, COLOR=drawColor,$
	     Minor=1,TickLen=0.0001,TickNames=[' ','QC fail',' '],CharSize=0.8
  endif
  
  ;---- write out image file ----
  write_png, img_name, TVRD(), r, g, b
  device,/close
END

;===============================================================
; To plot high map_dim of QC Flags in MIRS
;
; 04/08/2010	Wanchun Chen
;
;===============================================================

PRO map_hr_qcf, minlat,maxlat,minlon,maxlon,minY,maxY,xlat,xlon,xscan,xtab,ind,$
                nscanpos,fov_scale_cross,fov_scale_along,xscale,yscale,$
		tit,unit,thk,overlap,fmt,divs,img_name,map_dim,border,rotate_angle

  ;---- set Z buffer ----
  set_plot,'Z'
  device,set_resolution=map_dim
  
  xdim = map_dim[0]
  ydim = map_dim[1]
  
  position=[0.05, 0.125, 0.95, 0.95]
  
  ;---- load color table ----
  loadct, 39, /SILENT
  TVLCT, r, g, b, /get
  
  ;---- define different colors index for different geophysical events ----
  ;0
  for i=1,23 do begin
     r(i)=245 & g(i)=245 & b(i)=220	;beige 
  endfor
  ;1:
  for i=24,46 do begin
    r(i)=255 & g(i)=0 & b(i)=0		;red [14,16] 
  endfor
  ;2:
  for i=47,69  do begin
    r(i)=51 & g(i)=102 & b(i)=255	;light blue
  endfor
  ;3:
  for i=70,92 do begin
    r(i)=0 & g(i)=255 & b(i)=255	;aqua [0, 2]
  endfor
  ;4:
  for i=93,115 do begin
    r(i)=0 & g(i)=0 & b(i)=255		;blue [2,4]
  endfor
  ;5:
  for i=116,138 do begin
    r(i)=50 & g(i)=205 & b(i)=50	;light green [6,8]
  endfor
  ;6:
  for i=139,161 do begin
     r(i)=0 & g(i)=255 & b(i)=0		;bright green 
  endfor
  ;7:
  for i=162,184 do begin
     r(i)=0 & g(i)=102 & b(i)=0		;dark green
  endfor
  ;8:
  for i=185,207 do begin
    r(i)=255 & g(i)=205 & b(i)=0	;dark yellow
  endfor
  ;9:
  for i=208,230 do begin
    r(i)=255 & g(i)=90 & b(i)=0		;orange
  endfor

  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
  r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
  r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)

  drawColor = 255
  borderColor = 255
  charsz = 0.75
  nrec=n_elements(ind)
  nc=239    ; to be consistent with our plot_grid subroutine
  
  latdiff=maxlat-minlat
  londiff=maxlon-minlon
  
  latdel=1
  londel=2
  
  if latdiff le 15 then latdel = 1
  if latdiff gt 15 and latdiff le 30 then latdel = 2
  if latdiff gt 30 and latdiff le 60 then latdel = 5
  if latdiff gt 60 then latdel = 15
 
  if londiff le 30 then londel = 2
  if londiff gt 30 and londiff le 60 then londel = 5
  if londiff gt 60 and londiff le 120 then londel = 10
  if londiff gt 120 then londel = 30
  
  map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	   title=tit, color=drawColor, charsize=charsz, /NOBORDER

  ;---- plot fov symbol one by one ----
  FOR iprof=0L,nrec-1 DO BEGIN
      y=xtab[ind(iprof)]
      indcol=0L
      xind=(float(y-minY)/float(maxY-minY))*nc + 1
      indcol=long(xind) > 1L < nc
      iscanpos = xscan[ind(iprof)] - 1
      if iscanpos lt 0 or iscanpos gt nscanpos-1 then iscanpos = 0
      xradius = fov_scale_cross( iscanpos ) * xscale
      yradius = fov_scale_along( iscanpos ) * yscale
      xcenter = xlon[ind(iprof)]
      ycenter = xlat[ind(iprof)]
      xxxx = [ xcenter-xradius, xcenter+xradius, xcenter+xradius, xcenter-xradius ]
      yyyy = [ ycenter-yradius, ycenter-yradius, ycenter+yradius, ycenter+yradius ]
      if( xcenter-xradius ge minlon and xcenter+xradius le maxlon and ycenter-yradius ge minlat and ycenter+yradius le maxlat ) then begin
        POLYFILL, xxxx, yyyy, color=indcol
      endif
  ENDFOR
  
  map_continents,/continents,/noborder,/hires,/usa
  ;map_grid,latdel=latdel,londel=londel,color=drawColor
  
  ticks_lon=FIX((maxlon-minlon)/londel)
  ticks_lat=FIX((maxlat-minlat)/latdel)

  longitude = minlon + findgen(ticks_lon+1)*londel
  latitude  = minlat + findgen(ticks_lat+1)*latdel

  ;---- plot X-Y axis ----
  Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
     	POSITION=position, xrange=[minlon,maxlon], yrange=[minlat,maxlat], xticklen=0.01, yticklen=0.01,$
     	Color=drawColor, charsize=charsz, /NoData, /NoErase;, xtitle='Longitude', ytitle='Latitude'
  
  ;---- plot a color bar ----
  position_bar=fltarr(4)
  position_bar[0]=position[0]
  position_bar[1]=position[1]-0.1
  position_bar[2]=position[2]
  position_bar[3]=position_bar[1]+0.03
  
  colorbar,bottom=1,ncolors=230,/horizontal,range=[minY,maxY],divisions=divs, $
           format=fmt,position=position_bar,Minor=1,TickLen=0.0001,charsize=charsz
  
  ;---- extra legend ----
  legen1='0:GOOD'
  legen2='1:BAD'
  legen3='PRECIP:'
  legen4='2(LGHT)'
  legen5='3(MED)'
  legen6='4(HVY)'
  legen7='INVERS:'
  legen8='5(TMP)'
  legen9='6(HUM)'
  legen10='7(TMP+HUM)'
  legen11='SATUR:'
  legen12='8(CLD/PRECIP)'
  legen13='9(noCLD/noPRECIP)'

  ypos = position_bar[3]+0.01
  colors = [ 1,24,47,70,93,116,139,162,185,208 ]

  XYOUTS, 0.000, ypos, legen1, charsize=charsz,charthick=1.0, /normal
  XYOUTS, 0.060, ypos, legen2, charsize=charsz,charthick=1.0,color=24, /normal

  XYOUTS, 0.115, ypos, legen3, charsize=charsz,charthick=1.0, /normal
  XYOUTS, 0.180, ypos, legen4, charsize=charsz,charthick=1.0,color=47, /normal
  XYOUTS, 0.250, ypos, legen5, charsize=charsz,charthick=1.0,color=70, /normal
  XYOUTS, 0.310, ypos, legen6, charsize=charsz,charthick=1.0,color=93, /normal

  XYOUTS, 0.370, ypos, legen7, charsize=charsz,charthick=1.0, /normal
  XYOUTS, 0.430, ypos, legen8, charsize=charsz,charthick=1.0,color=116, /normal
  XYOUTS, 0.490, ypos, legen9, charsize=charsz,charthick=1.0,color=139, /normal
  XYOUTS, 0.550, ypos, legen10,charsize=charsz,charthick=1.0,color=162, /normal

  XYOUTS, 0.660, ypos, legen11,charsize=charsz,charthick=1.0, /normal
  XYOUTS, 0.715, ypos, legen12,charsize=charsz,charthick=1.0,color=185, /normal
  XYOUTS, 0.840, ypos, legen13,charsize=charsz,charthick=1.0,color=208, /normal

  ;---- write out image file ----
  write_png, img_name, TVRD(), r, g, b
  device,/close

END


;===============================================================
; To plot a dummy blank image when no data 
;
; 11/28/2010	Wanchun Chen
;
;===============================================================
PRO map_hr_blank, minlat,maxlat,minlon,maxlon,minY,maxY,xlat,xlon,xscan,xtab,ind,nscanpos,qcs,isQc,$
	fov_scale_cross,fov_scale_along,xscale,yscale,tit,unit,thk,overlap,fmt,divs,$
	img_name,map_dim,border,rotate_angle

  ;---- set Z buffer ----
  set_plot,'Z'
  device,set_resolution=map_dim
  
  xdim = map_dim[0]
  ydim = map_dim[1]
  
  position=[0.05, 0.125, 0.95, 0.95]
  
  ;---- load color table ----
  loadct, 39, /SILENT
  TVLCT, r, g, b, /get
  
  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
  r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
  r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)

  drawColor = 255
  borderColor = 255
  charsz = 0.75
  nrec=n_elements(ind)
  nc=239    ; to be consistent with our plot_grid subroutine
  
  latdiff=maxlat-minlat
  londiff=maxlon-minlon
  
  latdel=1
  londel=2
  
  if latdiff le 15 then latdel = 1
  if latdiff gt 15 and latdiff le 30 then latdel = 2
  if latdiff gt 30 and latdiff le 60 then latdel = 5
  if latdiff gt 60 then latdel = 15
 
  if londiff le 30 then londel = 2
  if londiff gt 30 and londiff le 60 then londel = 5
  if londiff gt 60 and londiff le 120 then londel = 10
  if londiff gt 120 then londel = 30
  
  map_set, /cyl, limit=[minlat,minlon,maxlat,maxlon], position=position,$
	   title=tit, color=drawColor, charsize=charsz, /NOBORDER

  map_continents,/continents,/noborder,/hires,/usa
  
  ticks_lon=FIX((maxlon-minlon)/londel)
  ticks_lat=FIX((maxlat-minlat)/latdel)

  longitude = minlon + findgen(ticks_lon+1)*londel
  latitude  = minlat + findgen(ticks_lat+1)*latdel

  ;---- plot X-Y axis ----
  Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
     	POSITION=position, xrange=[minlon,maxlon], yrange=[minlat,maxlat], xticklen=0.01, yticklen=0.01,$
     	Color=drawColor, charsize=charsz, /NoData, /NoErase;, xtitle='Longitude', ytitle='Latitude'
  
  XYOUTS, 0.35, 0.5, 'NO DATA', charsize=3.0, charthick=2.0,color=255, /normal

  ;---- write out image file ----
  write_png, img_name, TVRD(), r, g, b
  device,/close

END


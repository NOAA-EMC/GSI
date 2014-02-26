;=====================================================================================================
; Name:		EXPORT_IMG
;
; Type:		IDL Subroutine
;  
; Description:
;    To export grided data into png files using z-buffer
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;      ---------------------------------------------------
;       img   		i 		img array     
;       img_title	i		img title
;	img_fname	i		img file name
;	zmax		i		max value
;	zmin		i		min value
;	unit		i		unit
;	ct_r		i		color table red value
;	ct_g		i		color table green value
;	ct_b		i		color table blue value
;	lndsea_tag 	i		land/sea Tag    
;     	Land 		i 		Plotting is over land(1) or ocean(0) or all(-1)
;	DIV 		i		Color bar digits are integer(10) or real(1)
;	HERITAGE 	i		Heritage plot (1), 1DVAR plot (0)
;	LatMin 		i		Minimum lat of output image (-90,89)
;	LatMax 		i		Maximum lat of output image (-89,90)
;	LonMin 		i		Minimum lon of output image (-180, 179)
;	LonMax 		i		Maximum lon of output image (-179, 180)
;     	FIG_SIZE 	i		Output image resolution
;     
;
; Subroutines needed:
;	- PLOT_MAP
;
;
; Record of revisions:
;        Date          Programmer                        Description of change
;    ============    ==============    =============================================================
;     08/01/2006       Ninghai Sun      Original code
;     09/11/2006       Ninghai Sun      Add land sea tag for different color, lndsea_tag and Land=land
;     01/11/2007       Sid Boukabara    Modified the use of img_buff (to avoid overwriting)
;     01/13/2007       Sid Boukabara    correct the label in case sea+lnd are plotted
;     
;=====================================================================================================

PRO EXPORT_IMG, img, NonConvPoints, img_title, img_fname, zmax, zmin, unit, ct_r, ct_g, ct_b, lndsea_tag, $
                Land=land, DIV=div, HERITAGE=heritage, LatMin=latmin, LatMax=latmax, LonMin=lonmin, LonMax=lonmax, $
                FIG_SIZE=fig_size
;=====================================================================================================
;
;  Purpose:
;    To export grided data into png files using z-buffer
;
;  Record of revisions:
;        Date          Programmer                        Description of change
;    ============    ==============    =============================================================
;     08/01/2006       Ninghai Sun      Original code
;     09/11/2006       Ninghai Sun      Add land sea tag for different color, lndsea_tag and Land=land
;     01/11/2007       Sid Boukabara    Modified the use of img_buff (to avoid overwriting)
;     01/13/2007       Sid Boukabara    correct the label in case sea+lnd are plotted
;
;=====================================================================================================
  img_buff  = img
  IF (N_Elements(Land) EQ 0) THEN land = -1         ; Plotting is over land(1) or ocean(0) or all(-1)
  IF (N_Elements(DIV) EQ 0) THEN div = 10           ; Color bar digits are integer(10) or real(1)
  IF (N_Elements(HERITAGE) EQ 0) THEN heritage = 0  ; Heritage plot (1), 1DVAR plot (0)
  IF (N_Elements(LatMin) EQ 0) THEN latmin = -90.0  ; Minimum lat of output image (-90,89)
  IF (N_Elements(LatMax) EQ 0) THEN latmax =  90.0  ; Maximum lat of output image (-89,90)
  IF (N_Elements(LonMin) EQ 0) THEN lonmin = -180.0 ; Minimum lon of output image (-180, 179)
  IF (N_Elements(LonMax) EQ 0) THEN lonmax =  180.0 ; Maximum lon of output image (-179, 180)
  IF (N_Elements(FIG_SIZE) EQ 0) THEN fig_size = [650, 500] ; Output image resolution
  ;IF (N_Elements(FIG_SIZE) EQ 0) THEN fig_size = [700, 550] ; Output image resolution

  ; Find the number of NonConvPoints
  count = N_Elements(NonConvPoints)
  
  ; Find the size of image data
  data_size = SIZE(img_buff)
  title_size = SIZE(img_title)
  fname_size = SIZE(img_fname)
  zmax_size = SIZE(zmax)
  zmin_size = SIZE(zmin)

  ; For 2-d data ( single field )
  IF ( data_size[0] EQ 2 ) THEN BEGIN
    ; Set non converg points to be -9999.0 different from missing gap (-999.0)
    IF ( count GT 1 ) THEN img_buff[NonConvPoints]=-9999.0
    
    ; Call subroutine to plot map into png file
    Print, 'Plotting ... ' + img_title
    PLOT_MAP, img_buff, img_title, img_fname, zmax, zmin, unit, ct_r, ct_g, ct_b, lndsea_tag, land, div, $
              heritage, latmin, latmax, lonmin, lonmax, fig_size
    
  ENDIF

  ; For 3-d data ( profile array ) 
  IF ( data_size[0] EQ 3 ) THEN BEGIN
    ; iterate through every element
    FOR iElement=0, data_size[3]-1 DO BEGIN
      tmp_img = 0
      tmp_img = img_buff[*,*,iElement]
      
      ; Set non converg points to be -999.0
      IF ( count GT 1 ) THEN tmp_img[NonConvPoints]=-9999.0
      
      ; Call subroutine to plot map into png file
      Print, 'Plotting ... ' + img_title[iElement]
      PLOT_MAP, tmp_img, img_title[iElement], img_fname[iElement], zmax[iElement], zmin[iElement], unit, $
                ct_r, ct_g, ct_b, lndsea_tag, land, div, heritage, latmin, latmax, lonmin, lonmax, fig_size
    
    ENDFOR
  ENDIF

END


;=====================================================================================================
; Name:		PLOT_MAP
;
; Type:		IDL Subroutine
;  
; Description:
;    To export grided data into png files using z-buffer
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;      ---------------------------------------------------
;       databuff   	i 		img array     
;       img_title	i		img title
;	img_fname	i		img file name
;	zmax		i		max value
;	zmin		i		min value
;	unit		i		unit
;	ct_r		i		color table red value
;	ct_g		i		color table green value
;	ct_b		i		color table blue value
;	lndsea_tag 	i		land/sea Tag    
;     	Land 		i 		Plotting is over land(1) or ocean(0) or all(-1)
;	DIV 		i		Color bar digits are integer(10) or real(1)
;	HERITAGE 	i		Heritage plot (1), 1DVAR plot (0)
;	LatMin 		i		Minimum lat of output image (-90,89)
;	LatMax 		i		Maximum lat of output image (-89,90)
;	LonMin 		i		Minimum lon of output image (-180, 179)
;	LonMax 		i		Maximum lon of output image (-179, 180)
;     	FIG_SIZE 	i		Output image resolution
;     
;
; Subroutines needed:
;	- none
;
;
; Record of revisions:
;        Date          Programmer                        Description of change
;    ============    ==============    =============================================================
;     06/27/2006       Ninghai Sun      Original code
;     09/11/2006       Ninghai Sun      Add land sea tag for different colors, lndsea_tag and 
;     
;=====================================================================================================

PRO PLOT_MAP, databuff, img_title, img_fname, zmax, zmin, unit, ct_r, ct_g, ct_b, lndsea_tag, land, div, $
              heritage, latmin, latmax, lonmin, lonmax, fig_size

  ; Set device to be 'Z' buffer
  thisDevice=!D.Name
  Set_Plot, 'Z'
  Device, Set_Resolution=fig_size, Set_Color=256
;  !P.FONT=1  ; True Type Fonts
;  DEVICE, SET_FONT='Time',/TT_FONT;,SET_CHARACTER_SIZE=[9,12]
  !P.FONT=-1  ; Vector Fonts
  !P.CharThick=0.5
;  DEVICE, SET_CHARACTER_SIZE=[8,15] ; Font set to 'Triplex Roman' using embeded command '!17' 
;  !P.FONT=0  ; Device Fonts

  TVLCT, ct_r, ct_g, ct_b   ; Load Blue-Red color table
  TVLCT,255,255,255,0       ; Load White Color for background
  TVLCT,0,0,0,255           ; Load Black Color
  TVLCT,255,255,255,254     ; Load White Color for Missing Data
  TVLCT,215,215,215,253     ; Load Grey Color (20%) for Land/Ocean Coverage
  TVLCT,98,98,98,252        ; Load Grey Color (70%) for non-convergent points
  Erase,color=0

  Start_color = 1            ; Line 1 in color_table. IDL array is from line 0 (here used for background white).
  nclrs=10*24 ;10*19                ; Totoal 10*19 color divided into 10 sections
 
  color_top = nclrs-1

  ; Create byte data from 0 to 159.
  buffer = BytScl(databuff,Min=zmin,Max=zmax,Top=color_top) + start_color

  ; Replace gap and ocean with special value
  img_size = Size(databuff)
  FOR i=0,img_size[1]-1 DO BEGIN
    FOR j=0,img_size[2]-1 DO BEGIN
      ;missing data is using index=254(white)
      IF (databuff[i,j] EQ -999.0) THEN buffer[i,j] = Byte(254) 
      ; Non convergent points will be filled using grey color
      IF (databuff[i,j] EQ -9999.0) THEN buffer[i,j] = Byte(252)
      ; Ocean is covered by grey color (253) if plot over land only
      IF ( land EQ 1 AND lndsea_tag[i,j] EQ 0) THEN buffer[i,j] = Byte(253)  
      ; Land is covered by grey color (253) if plot over ocean only
      IF ( land EQ 0 AND lndsea_tag[i,j] EQ 1) THEN buffer[i,j] = Byte(253) 
    ENDFOR
  ENDFOR

  ; Setup map position in display
  map_position = [0.05,0.15,0.95,0.9]     ; Map position
  
  ;;??? some logic problem here, color bar sometimes get screwed up
  IF ( heritage EQ 0 ) THEN BEGIN
    bar_position = [0.35,0.05,0.95,0.08]    ; Color bar position
  ENDIF
  IF ( heritage EQ 1 ) THEN BEGIN
    bar_position = [0.25,0.05,0.95,0.08]    ; Color bar position, heritage color bar is longer
  ENDIF
  IF ( land EQ -1 ) THEN BEGIN
    bar_position = [0.15,0.05,0.95,0.08]    ; Color bar position, cover land/sea bar for plotting all
  ENDIF
  
  bar_position = [0.35,0.05,0.95,0.08] 
  
  mis_position = [0.05,0.05,0.10,0.08]    ; Missing point color bar position
  bar1_position = [0.15,0.05,0.20,0.08]   ; Land/Ocean color bar position
  bar2_position = [0.25,0.05,0.30,0.08]   ; Non-convergent color bar position for 1DVAR output
  charsize = 1.0
  bar_charsize = 0.8

  ; Setup map projection to be cylindrical
  rlat1=latmin
  rlon1=lonmin
  rlat2=latmax
  rlon2=lonmax

  Map_Set,0,0,/Cyl,Limit=[rlat1,rlon1,rlat2,rlon2],/NoBorder,/Advance,$
          Position=map_position,Color=255

  ; Create image buffer using map_set parameters and plot using TV
  ; Grid Data is from lat=[-90,90] and lon=[0,360]
  warp = Map_Image(buffer,StartX,StartY,XSize,YSize,Missing=254,Compress=1,LatMin=-90,LatMax=90,$
                   LonMin=0,LonMax=360)
  TV,warp,StartX,StartY,XSize=XSize,YSize=Ysize,/Order

  ; Plot continent, grid line and title using the same map_set parameters
  Map_Set,0,0,Title=img_title,/Cyl,Limit=[rlat1,rlon1,rlat2,rlon2],/NoErase,/NoBorder,$
          Position=map_position,Color=255,CharSize=charsize
  Map_Grid,LatDel=15,LonDel=30,Color=255
  Map_Continents,/Coasts,Color=255

  ; Plot lat/lon label
  xlon = IndGen(rlon2-rlon1+1)+Fix(rlon1)
  ylat = IndGen(rlat2-rlat1+1)+Fix(rlat1)
  ticks = Fix(rlon2-rlon1)/30
  Plot,xlon,ylat,XStyle=1,YStyle=1,XTicks=ticks,YTicks=ticks,TickLen=0.0001,  $
       XMinor=1,YMinor=1,/NoErase,/NoData,Position=map_position,Color=255,CharSize=bar_charsize

  ; Plot color bar with integer number
  IF (div EQ 10) THEN BEGIN
    ColorBar,Bottom=start_color,Color=255,Position=bar_position,Format='(I4)',Title=unit, $
             NColors=nclrs,Divisions=10,Minor=1,TickLen=0.0001,Range=[zmin,zmax],CharSize=bar_charsize
  ENDIF
  
  ; Plot color bar with float number
  IF (div EQ 1) THEN BEGIN
    ColorBar,Bottom=start_color,Color=255,Position=bar_position,Format='(F6.2)',Title=unit, $
             NColors=nclrs,Divisions=10,Minor=1,TickLen=0.0001,Range=[zmin,zmax],CharSize=bar_charsize
  ENDIF
  
  ; Plot Missing lengend (white)
  ColorBar,Bottom=0,Color=255,Position=mis_position,NColors=1,Divisions=2, $
           Minor=1,TickLen=0.0001,TickNames=[' ','NoData',' '],CharSize=bar_charsize

  ; Plot Land/Ocean lengend (Grey-20%)
  ticksname=[' ',' ',' ']
  IF ( land EQ 1 ) THEN BEGIN
      ticksname=[' ','Ocean',' ']
      ColorBar,Bottom=253,Color=255,Position=bar1_position,NColors=1,Divisions=2, $
        Minor=1,TickLen=0.0001,TickNames=ticksname,CharSize=bar_charsize
  ENDIF
  IF ( land EQ 0 ) THEN BEGIN
      ticksname=[' ','Land',' ']
      ColorBar,Bottom=253,Color=255,Position=bar1_position,NColors=1,Divisions=2, $
        Minor=1,TickLen=0.0001,TickNames=ticksname,CharSize=bar_charsize
  ENDIF
  
  ; Plot Non-convergent lengend (Grey-70%)
  IF ( heritage EQ 0 ) THEN BEGIN
    ColorBar,Bottom=252,Color=255,Position=bar2_position,NColors=1,Divisions=2, $
             Minor=1,TickLen=0.0001,TickNames=[' ','QC fail',' '],CharSize=bar_charsize
  ENDIF
  
  ; Save figures from z-buffer to PNG file
  snapshot = TVRD()
  TVLCT, r, g, b, /GET
  Write_PNG, img_fname, TVRD(), r, g, b
;  s = SIZE(snapshot, /Dimensions)
;  img3D = BytArr(3, s[0], s[1])
;  img3D[0,*,*] = r[snapshot]
;  img3D[1,*,*] = g[snapshot]
;  img3D[2,*,*] = b[snapshot]
;  Write_JPEG, img_fname, img3D, True=1, Quality=100
;  Write_TIFF, img_fname, Reverse(Temporary(img3D),3)

  ; Set back to default display device
  Set_Plot, thisDevice

END


;=====================================================================================================
; Name:		FILL_POINTS
;
; Type:		IDL Subroutine
;  
; Description:
;    To fill in missing points
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;      ---------------------------------------------------
;     	Grid_Array	i/o		Grid array of input
;  	Angle_Array     i		cross-track scan scene zenith angle array
;
; Subroutines needed:
;	- none
;
;
; Record of revisions:
;        Date          Programmer                        Description of change
;    ============    ==============    =============================================================
;     06/27/2006       Ninghai Sun      		Original code
;     09/11/2006       Ninghai Sun      		Add scan angle dependent fill
;     
;=====================================================================================================

PRO FILL_POINTS, Grid_Array, Angle_Array=Angle_Array

  ; If no angle_array is presented, there is no angle dependent filling.
  IF (N_Elements(Angle_Array) EQ 0) THEN angle_dependence = 0 ELSE angle_dependence = 1
 
  ; Estimate input array
  size_arr = SIZE(Grid_Array)
  nColumn = size_arr[1]
  nLine = size_arr[2]
  IF ( size_arr[0] EQ 3 ) THEN nElement = size_arr[3]
  IF ( size_arr[0] NE 2 AND size_arr[0] NE 3) THEN BEGIN
    Print, 'Error!! Only fill missing points for 2D or 3D array. EXIT !'
    RETURN
  ENDIF

  ; For angle dependent sensors, determine the angle threshold for 5X3 filling
  IF ( angle_dependence EQ 1 ) THEN BEGIN
    angle_threshold = 0.35 * (MAX(Angle_Array[WHERE(Angle_Array NE -999)]))
  ENDIF ELSE BEGIN
    Angle_Array = Replicate(-999.0, nColumn, nLine)
    angle_threshold = -999.0
  ENDELSE

  ; For 2D input array
  IF ( size_arr[0] EQ 2 ) THEN BEGIN
    Fill2D, Grid_Array, nLine, nColumn, angle_dependence, angle_threshold, Angle_Array
  ENDIF    ; 2D array

  ; 3D array
  IF ( size_arr[0] EQ 3 ) THEN BEGIN
    FOR iElement=0, nElement-1 DO BEGIN
      ; Fill missing points one field by one field
      array_2d = 0
      array_2d = Grid_Array[*,*,iElement]
      
      ; Fill missing points
      Fill2D, array_2d, nLine, nColumn, angle_dependence, angle_threshold, Angle_Array
      
      ; Write back output array
      Grid_Array[*,*,iElement] = array_2d[*,*]
    ENDFOR    ; iElement
  ENDIF    ; 3D array

END


;=====================================================================================================
; Name:		Fill2D
;
; Type:		IDL Subroutine
;  
; Description:
;    To fill in missing points of 2D array
;
; 
; Arguments:    
;     
;	Name	    		Type	    	Description
;      ---------------------------------------------------
;     	Field_Array		I/O		2-D field array		
;  	nLine			I		Number of row			
;	nColumn			I		Number of column				
;	Angle_Dependence	I		angle dependency
;	Angle_Threshold		I		angle threshold
;	Angle_Array		I		angle array
;
;
; Subroutines needed:
;	- none
;
;
; Record of revisions:
;        Date          Programmer                        Description of change
;    ============    ==============    =============================================================
;     06/27/2006       Ninghai Sun      		Original code
;     
;=====================================================================================================

PRO Fill2D, Field_Array, nLine, nColumn, Angle_Dependence, Angle_Threshold, Angle_Array
  ; Assign temperary array for filling
  tmp_array=0
  tmp_array = Field_Array

  ; Loop over lines
  FOR jLine=1, nLine-2 DO BEGIN
    ; Loop over columns 
    FOR iColumn=2, nColumn-3 DO BEGIN
      ; Only work in missing points (-999)
      IF ( tmp_array[iColumn, jLine] EQ -999.0 ) THEN BEGIN
        sum_angle = 0.0
        avg_angle = -999.0

        ; For angle dependent sensors, more horizontal points are needed for missing point filling over edge.
        IF ( Angle_Dependence EQ 1 ) THEN BEGIN
          ; Create a 5X3 array for all scan dependent sensors
          Angle_Box = Angle_Array[iColumn-2:iColumn+2, jLine-1:jLine+1]
          nMissing = N_ELEMENTS(WHERE(Angle_Box EQ -999.0))
            
          ; if all 15 points are missing, go to next point
          IF ( nMissing EQ 15 ) THEN GOTO, NEXT_POINT

          ; Calculate average scan angle for all 15 points
          sum_angle = TOTAL(Angle_Box[WHERE(Angle_Box NE -999.0)],/DOUBLE)
          avg_angle = ABS(FLOAT(sum_angle/(15.0 - nMissing)))
        ENDIF
          
        IF ( avg_angle GT Angle_Threshold ) THEN BEGIN
          ; Use 15 points for interpolation if angle is larger than half of maximum scan angle
          Grid_Box = tmp_array[iColumn-2:iColumn+2, jLine-1:jLine+1]
        
          ; Estimate if all 5X3 points are missing
          nMissing = N_ELEMENTS(WHERE(Grid_Box EQ -999.0))

          ; Goto next point if all are missing
          IF ( nMissing EQ 15 ) THEN GOTO, NEXT_POINT
          
          ; Calculate mean value of box and fill it in the missing point
          sum_box = TOTAL(Grid_Box[WHERE(Grid_Box NE -999.0)],/DOUBLE)
          Field_Array[iColumn,jLine] = FLOAT(sum_box/(15.0 - nMissing))
        
        ENDIF ELSE BEGIN
          ; For other situation, only get a 3X3 subarray arround missing point
          Grid_Box = tmp_array[iColumn-1:iColumn+1, jLine-1:jLine+1]
          
          ; Estimate if all 3X3 points are missing
          nMissing = N_ELEMENTS(WHERE(Grid_Box EQ -999.0))

          ; Goto next point if all are missing
          IF ( nMissing EQ 9 ) THEN GOTO, NEXT_POINT
          
          ; Calculate mean value of box and fill it in the missing point
          sum_box = TOTAL(Grid_Box[WHERE(Grid_Box NE -999.0)],/DOUBLE)
          Field_Array[iColumn,jLine] = FLOAT(sum_box/(9.0 - nMissing))
        ENDELSE
      ENDIF    ; missing point
        
      NEXT_POINT:
      
    ENDFOR    ; iColumn
  ENDFOR    ; jLine

END


;=====================================================================================================
; Name:		INTERP2GRID
;
; Type:		IDL Subroutine
;  
; Description:
;    To interpolate data into grid points based on the latitude/longitude
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;      ---------------------------------------------------
;    	Lat        	I     		latitude
;    	Lon        	I   	       	longitude
;    	jLat       	O  	       	Grid Point in Line
;    	iLon       	O  	       	Grid Point in Column
;    	resl       	I   	       	resolution of grid point ( 360/nColumn )
;    	dist2grid  	O  	       	Distance from [Lon, Lat] to grid point [iLon, jLat]
;
;
; Subroutines needed:
;	- none
;
;
; Record of revisions:
;        Date          Programmer                        Description of change
;    ============    ==============    =============================================================
;     06/27/2006       Ninghai Sun      Original code
;     
;=====================================================================================================

PRO INTERP2GRID, Lat, Lon, jLat, iLon, resl, dist2grid

  alat = 90.0 - lat
  IF ( lon GE 0.0 ) THEN alon=lon ELSE alon=lon+360.0

  jLat = Round(alat/resl)-1
  iLon = Round(alon/resl)-1

  mxLat = Round(180.0/resl)-1
  mxLon = Round(360.0/resl)-1
  
  IF ( jLat LT 0 ) THEN jLat = 0
  IF ( iLon LT 0 ) THEN iLon = 0

  IF ( jLat GT mxLat ) THEN jLat = mxLat
  IF ( iLon GT mxLon ) THEN iLon = mxLon
  

  dLat = ( (jLat+1.0)*resl - alat )^2
  dLon = ( ( (iLon+1.0)*resl - alon ) * COS(lat*!DPI/180.0) )^2

  dist2grid = dLat + dLon

END


;=====================================================================================================
; Name:		PLOT_ASYMMETRY
;
; Type:		IDL Subroutine
;  
; Description:
;   	To Plot LZA asymmetry 
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;      ---------------------------------------------------
;    	x        	I     		latitude
;    	y       	I   	       	longitude
;    	title       	O  	       	Grid Point in Line
;    	xtitle      	O  	       	Grid Point in Column
;    	ytitle       	I   	       	resolution of grid point ( 360/nColumn )
;    	xrange		I		X range
;    	yrange		I		Y range
;	figname		I		fig name
;    	layer		I		Number of layer in each plot 
;	scatter		I		plot scattered image or not
;	FIG_SIZE	I		Output image resolution
;
; Subroutines needed:
;	- none
;
;
; Record of revisions:
;        Date          Programmer                        Description of change
;    ============    ==============    =============================================================
;     06/27/2006       Ninghai Sun      Original code
;     
;=====================================================================================================

PRO PLOT_ASYMMETRY, x, y, title, xtitle, ytitle, xrange, yrange, figname, Layer=layer, SCATTER=scatter, FIG_SIZE=fig_size

  IF (N_Elements(Layer) EQ 0) THEN layer = 1 ; Number of layer in each plot 
  IF (N_Elements(SCATTER) EQ 0) THEN scatter = 0 ; Plot scatter 
  IF (N_Elements(FIG_SIZE) EQ 0) THEN fig_size = [650, 500] ; Output image resolution

  ; Set device to be 'Z' buffer
  thisDevice=!D.Name
  Set_Plot, 'Z'
  Device, Set_Resolution=fig_size, Set_Color=256
  !P.FONT=-1  ; Vector Fonts

  TVLCT,255,255,255,0       ; Load White Color for background
  TVLCT,0,0,0,255           ; Load Black Color
  Erase, Color=0

  position = [0.10,0.10,0.95,0.9]

  ; For LZA asymmetry single line plot
  IF ( scatter EQ 0 AND layer EQ 1 ) THEN BEGIN
    Plot,x,y,Title=title,XTitle=xtitle,YTitle=ytitle,XRange=xrange,YRange=yrange,XStyle=1,YStyle=1,$
         PSym=-4, Position=position
  ENDIF

  ; For LZA asymmetry multiple line plot
  IF ( scatter EQ 0 AND layer EQ 4 ) THEN BEGIN
    TVLCT,0,0,0,1            ; Load black color
    TVLCT,255,0,0,2          ; Load red color
    TVLCT,0,255,0,3          ; Load green color
    TVLCT,0,0,255,4          ; Load blue color

    Plot,x,y[*,0],color=1,Title=title,Xtitle=xtitle,YTitle=ytitle,XRange=xrange,YRange=yrange,XStyle=1, $
         YStyle=1, PSym=-4, Position=position
    Plots,x,y[*,1],color=2,PSym=-4
    Plots,x,y[*,2],color=3,PSym=-4
    Plots,x,y[*,3],color=4,PSym=-4
   
    hight=(yrange[1]-yrange[0])/20.0
    Plots,-55, yrange[1]-hight,color=1,PSym=4
    XYOutS, -50, yrange[1]-hight, color=1, '200 mb'
    Plots,-35, yrange[1]-hight,color=2,PSym=4
    XYOutS, -30, yrange[1]-hight, color=2, '500 mb'
    Plots,-15, yrange[1]-hight,color=3,PSym=4
    XYOutS, -10, yrange[1]-hight, color=3, '700 mb'
    Plots,5, yrange[1]-hight,color=4,PSym=4
    XYOutS, 10, yrange[1]-hight, color=4, '850 mb'
  ENDIF

  ; For scatter plot
  IF ( scatter EQ 1 ) THEN BEGIN
    Plot,x,y,Title=title,XTitle=xtitle,YTitle=ytitle,XRange=xrange,YRange=yrange,XStyle=1,YStyle=1,$
         PSym=3, Position=position
  ENDIF
  
  ; Save figures from z-buffer to PNG file
  TVLCT, r, g, b, /GET
  Write_PNG, figname, TVRD(), r, g, b

  ; Set back to default display device
  Set_Plot, thisDevice

END




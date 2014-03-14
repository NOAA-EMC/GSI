;---------------------------------------------------------------------------------
; Name:  plotRad.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to plot both observed radiance and simulated radiance.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO plotRad, chPlotArray, chanNumArray, chanInfoArray, prefix,       $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,  $
    refRadData, date

   ; Set XSIZE and YSIZE for PS.
   xSizeVal=18
   ySizeVal=30

   ; Save graphics in PS
   SET_PLOT, 'PS'

   ; Get num of channels to plot
   numOfChans = N_ELEMENTS(chPlotArray)

   ; Total number of graphics files
   IF ((numOfChans MOD 4) eq 0 ) THEN BEGIN 
      nFiles = numOfChans / 4
   ENDIF ELSE BEGIN 
      nFiles = numOfChans / 4 + 1
   ENDELSE
   ; Create string array with an extra element, so we 
   ; can refer to it as 1-base string array.
   fileNumArray = SINDGEN(nFiles+1)
   ; Graphics file number tracking: 1-base
   fileIndex = 1
   
   ; Loop thru. channels to plot
   FOR i=0, numOfChans - 1 DO BEGIN
      ; Start a new page every 4 channels.
      rowPostion = i MOD 4
      IF ( rowPostion eq 0 ) THEN BEGIN
         imageName = STRCOMPRESS(prefix + fileNumArray(fileIndex) + '.ps',/remove_all)
         ; Get the file number for the next graphics file
         fileIndex = fileIndex + 1 
         ERASE
         LOADCT, 39
         !P.FONT=-1
         DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
                 XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=2,  $
                 /PORTRAIT, FONT_SIZE=7, /BOLD, /COURIER
      ENDIF

      ;------------------------------------------------
      ; step 1:
      ;   Plot observed radiances for chosen channels.
      ;------------------------------------------------
      channel = chanInfoArray[chPlotArray(i)] + ' GHz '
      title = 'SSMIS observed TB ' + channel + date
      radType = 'obs'

      ; Select out profiles
      ;    radiance: ref_Tb1 > 0.
      ;    Orbit mode flag: ref_ModeFlag1 = 0
      filter1 = WHERE(refRadData.ref_Lat1 ge MIN_LAT       $
		and refRadData.ref_Lat1 le MAX_LAT         $
		and refRadData.ref_Tb1(*,chPlotArray(i)) gt 0 $
		and refRadData.ref_ModeFlag1 eq 0)

      radPloting, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,   $
	       refRadData.ref_Lat1,refRadData.ref_Lon1,                  $
	       filter1,   $
	       title,     $
	       minBT_Values(chPlotArray(i)),   $
	       maxBT_Values(chPlotArray(i)),   $
	       refRadData.ref_Tb1(*,chPlotArray(i)),         $
	       'K', $   ;unit
	       0.8, $   ;scale
	       8,   $   ;symb
	       1,   $   ;thick
	       '(f5.1)', $ ;fmt
               rowPostion, radType

      ;-------------------------------------------------
      ; step 2:
      ;   Plot simulated radiances for chosen channels.
      ;-------------------------------------------------
      channel = chanInfoArray[chPlotArray(i)] + ' GHz '
      title = 'SSMIS simulated TB ' + channel + date
      radType = 'sim'

      ; Select out profiles
      ;    radiance: ref_Tb2 > 0.
      ;    Orbit mode flag: ref_ModeFlag2 = 0
      filter2 = WHERE(refRadData.ref_Lat2 ge MIN_LAT          $
		and refRadData.ref_Lat2 le MAX_LAT            $
		and refRadData.ref_Tb2(*,chPlotArray(i)) gt 0 $
		and refRadData.ref_ModeFlag2 eq 0)

      radPloting, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,    $
	       refRadData.ref_Lat2,refRadData.ref_Lon2,                  $
	       filter2,   $
	       title,     $
	       minBT_Values(chPlotArray(i)),      $
	       maxBT_Values(chPlotArray(i)),      $
	       refRadData.ref_Tb2(*,chPlotArray(i)),         $
	       'K', $   ;unit
	       0.8, $   ;scale
	       8,   $   ;symb
	       1,   $   ;thick
	       '(f5.1)', $ ;fmt
               rowPostion, radType
   ENDFOR

END

;======================================================

PRO radPloting,MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,   $
    ref_Lat1,ref_Lon1,filter,title,              $
    minBT_Values,maxBT_Values,ref_Tb1,unit,scal,  $ 
    symb,thickVal,fmt, rowPostion, radType

   ; set the default charsz
   charSize=1.
   nRec=n_elements(filter)
   
   ; num of colos 
   nColor=!D.table_size
   nColor=nColor-2

   csize=16
   ; Divide 2.5PI by 16.
   tmpVal = findgen(csize+1) * (!PI*2.5/float(csize))
   usersym,scal*cos(tmpVal)/2,scal*sin(tmpVal)/2,/fill

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   center_lon=0
   latdelVal=max([fix((MAX_LAT-MIN_LAT)/100),1])*30
   londelVal=max([fix((MAX_LON-MIN_LON)/100),1])*20

   ; Define where plots start
   yOrgin=0.07
   xOrgin=0.05
   xGap=0.5

   ; plot width
   plotWidth = 0.42
   ; plot height
   plotHeight = 0.16
   ; color bar height
   barHeight = 0.025

   innerSpace=0.01    ; space between plot and bar vertically.
   outerY_Space=0.03  ; space between bar and next plot vertically.
   outerX_Space=0.10  ; space between two plots horizontally.

   plotY_Pos = findgen(4) ; array holding box position
   barY_Pos = findgen(4) ; array holding bar position

   ; Define y positions of boxes and bars
   FOR i=0, 3 DO BEGIN
      ; box's y positions
      plotY_Pos(i) = yOrgin + barHeight + innerSpace    $
	      + ( 3 - i ) * (plotHeight + innerSpace + barHeight + outerY_Space )
      ; bar's y positions
      barY_Pos(i) = yOrgin + ( 3 - i )     $
              * (plotHeight + innerSpace + barHeight + outerY_Space )
   ENDFOR

   ; Define box's position
   IF ( radType eq 'obs' ) THEN BEGIN 
      x0 = xOrgin
      y0 = plotY_Pos ( rowPostion )
      x1 = xOrgin + plotWidth
      y1 = plotY_Pos ( rowPostion ) + plotHeight
   ENDIF ELSE BEGIN 
      x0 = xOrgin + xGap
      y0 = plotY_Pos ( rowPostion )
      x1 = xOrgin + plotWidth + xGap
      y1 = plotY_Pos ( rowPostion ) + plotHeight
   ENDELSE 

   MAP_SET,0,CENTER_LON,XMARGIN=[8,8],YMARGIN=[8,8], /NOERASE,  $
      LIMIT=[MIN_LAT,MIN_LON,MAX_LAT,MAX_LON],/HIRES,TITLE=title,$
      POSITION=[x0,y0,x1,y1],LATDEL=latdelVal,LONDEL=londelVal,/grid,$

   charsize=charSize*1.3

   MAP_CONTINENTS,/CONTINENTS,/NOBORDER,/HIRES,/USA,FILL_CONTINENTS=0,COLOR=18

   MAP_GRID,LATDEL=latdelVal,LONDEL=londelVal,/LABEL,          $
      LONLAB=MIN_LAT,LATLAB=MIN_LON,LONALIGN=0.,LATALIGN=0.,   $
      CHARSIZE=charSize*1.3

   FOR iprof=0L,nRec-1 DO BEGIN
      ; index value
      index = filter(iprof)
      ; BT value
      btVal=ref_Tb1[index]
      colorVal=0L
      colorNum=(float(btVal-minBT_Values)/float(maxBT_Values-minBT_Values))*nColor
      ;dxu colorVal=long(colorNum) > 1L < nColor
      ;colorVal=long(colorNum) > 1L < nColor
      colorVal=long(colorNum)

      OPLOT,[ref_Lon1[index]],[ref_Lat1[index]],color=colorVal-1,psym=symb,$
         symsize=scal,thick=thickVal

   ENDFOR

   IF ((maxBT_Values-minBT_Values) gt 0.) THEN BEGIN
      ; Define the position of color bar
      IF ( radType eq 'obs' ) THEN BEGIN 
	 x0 = xOrgin
	 y0 = barY_Pos ( rowPostion )
	 x1 = xOrgin + plotWidth
	 y1 = barY_Pos ( rowPostion ) + barHeight
      ENDIF ELSE BEGIN
	 x0 = xOrgin + xGap
	 y0 = barY_Pos ( rowPostion )
	 x1 = xOrgin + plotWidth + xGap
	 y1 = barY_Pos ( rowPostion ) + barHeight
      END

      COLORBAR,NCOLORS=nColor,/HORIZONTAL,RANGE=[minBT_Values,maxBT_Values],TITLE=unit,$
	  FORMAT=fmt,CHARSIZE=charSize*1.3,FONT=1,POSITION=[x0, y0, x1, y1]
   ENDIF ELSE BEGIN
       PRINT, 'Warning: in radPloting: No color bar plotted: (maxBT_Values-minBT_Values) <= 0.'
   ENDELSE
END

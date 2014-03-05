PRO plotRad, chPlotArray, chanNumArray, chanInfoArrray, prefix1, prefix2, $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,       $
    ref_scanPos1, ref_scanLine1, ref_Lat1, ref_Lon1,      $
    ref_ModeFlag1, ref_Angle1, ref_QC1, ref_Tb1,          $
    ref_scanPos2, ref_scanLine2, ref_Lat2, ref_Lon2,      $
    ref_ModeFlag2, ref_Angle2, ref_QC2, ref_Tb2   

   ; Set XSIZE and YSIZE for PS.
   xz=8
   yz=7

   ; Get num of channels to plot
   numOfChans = N_ELEMENTS(chPlotArray)

   ;------------------------------------------------
   ; step 1:
   ;   Plot observed radiances for chosen channels.
   ;------------------------------------------------
   ;
   ; Loop thru. channels to plot
   FOR i=0, numOfChans - 1 DO BEGIN
      ; Remove all the whitespaces to make a new string
      image_name=strcompress(prefix1 + chanNumArray[chPlotArray(i)] + '.ps',/remove_all)

      ERASE
      !P.MULTI=1
      !P.FONT=-1

      LOADCT, 39
      SET_PLOT, 'PS'
      DEVICE, filename=image_name, /color, bits_per_pixel=8, $
	      xsize=xz, ysize=yz, xoffset=2, yoffset=2,      $
	      /portrait, font_size=7, /bold, /courier

      channel = chanInfoArrray[chPlotArray(i)] + ' GHz'
      title = 'SSMIS observed TB ' + channel + ' 2013-01-20'

      ; Select out profiles
      ;    radiance: ref_Tb1 > 0.
      ;    Orbit mode flag: ref_ModeFlag1 = 0
      filter1 = WHERE(ref_Lat1 ge MIN_LAT       $
		and ref_Lat1 le MAX_LAT         $
		and ref_Tb1(*,chPlotArray(i)) gt 0 $
		and ref_ModeFlag1 eq 0)

      mapPlot, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,    $
	       ref_Lat1,ref_Lon1,                  $
	       filter1,   $
	       title,     $
	       minBT_Values(chPlotArray(i)),   $
	       maxBT_Values(chPlotArray(i)),   $
	       ref_Tb1(*,chPlotArray(i)),         $
	       'K', $   ;unit
	       0.8, $   ;scale
	       8,   $   ;symb
	       1,   $   ;thick
	       0,   $   ;overlap
	       '(f5.1)' ;fmt

      ; Draws continental boundaries, etc, over an existing map projection
      ; established by MAP_SET.
      ;
      MAP_CONTINENTS,/continents,/noborder,/hires,/usa,fill_continents=0,color=18

   ENDFOR


   ;-------------------------------------------------
   ; step 2:
   ;   Plot simulated radiances for chosen channels.
   ;-------------------------------------------------
   ;
   ; Loop thru. channels to plot
   FOR i=0, numOfChans - 1 DO BEGIN
      ; Remove all the whitespaces to make a new string
      image_name=strcompress(prefix2 + chanNumArray[chPlotArray(i)] + '.ps',/remove_all)

      ERASE
      !P.MULTI=1
      !P.FONT=-1

      LOADCT, 39
      SET_PLOT, 'PS'
      DEVICE, filename=image_name, /color, bits_per_pixel=8, $
	      xsize=xz, ysize=yz, xoffset=2, yoffset=2,      $
	      /portrait, font_size=7, /bold, /courier

      channel = chanInfoArrray[chPlotArray(i)] + ' GHz'
      ;title = 'SSMIS TB ' + channel + ' 2012-04-15'
      title = 'SSMIS simulated TB ' + channel + ' 2013-01-20'

      ; Select out profiles
      ;    radiance: ref_Tb2 > 0.
      ;    Orbit mode flag: ref_ModeFlag2 = 0
      filter2 = WHERE(ref_Lat2 ge MIN_LAT       $
		and ref_Lat2 le MAX_LAT         $
		and ref_Tb2(*,chPlotArray(i)) gt 0 $
		and ref_ModeFlag2 eq 0)

      mapPlot, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,    $
	       ref_Lat2,ref_Lon2,                  $
	       filter2,   $
	       title,     $
	       minBT_Values(chPlotArray(i)),   $
	       maxBT_Values(chPlotArray(i)),   $
	       ref_Tb2(*,chPlotArray(i)),         $
	       'K', $   ;unit
	       0.8, $   ;scale
	       8,   $   ;symb
	       1,   $   ;thick
	       0,   $   ;overlap
	       '(f5.1)' ;fmt

      ; Draws continental boundaries, etc, over an existing map projection
      ; established by MAP_SET.
      ;
      MAP_CONTINENTS,/continents,/noborder,/hires,/usa,fill_continents=0,color=18

   ENDFOR




   END

PRO reformArray
   ;-----------------
   ; step 1:
   ;   constants
   ;-----------------
   ; rad1 (observed)
   ;
   nChan = rad1.nChan      ; number of channels

   ;-----------------
   ; step 2:
   ;   2-d arrays
   ;-----------------
   ; Convert 2-D (Fov X File) into 1-D array ( Fov * File )
   ; Result:
   ;     file 1        file 2             file n
   ;   [ Fov * File ][ Fov * File ] ... [ Fov * File ]
   ;
   ; for observed data, ref_ stands for reform
   ref_scanPos1  = reform(scanPosRad1(*, *), nList * MAX_FOV)
   ref_scanLine1 = reform(scanLineRad1(*, *), nList * MAX_FOV)
   ref_Lat1      = reform(latRad1(*, *), nList * MAX_FOV)
   ref_Lon1      = reform(lonRad1(*, *), nList * MAX_FOV)
   ref_ModeFlag1 = reform(dirRad1(*, *), nList * MAX_FOV)
   ref_Angle1    = reform(angleRad1(*, *), nList * MAX_FOV)
   ref_QC1       = reform(QC_Rad1(*, *), nList * MAX_FOV)
   ref_Tb1       = fltarr(nList * MAX_FOV, rad1.nChan)
   ; for simulated data
   ref_scanPos2  = reform(scanPosRad2(*, *), nList * MAX_FOV)
   ref_scanLine2 = reform(scanLineRad2(*, *), nList * MAX_FOV)
   ref_Lat2      = reform(latRad2(*, *), nList * MAX_FOV)
   ref_Lon2      = reform(lonRad2(*, *), nList * MAX_FOV)
   ref_ModeFlag2 = reform(dirRad2(*, *), nList * MAX_FOV)
   ref_Angle2    = reform(angleRad2(*, *), nList * MAX_FOV)
   ref_QC2       = reform(QC_Rad2(*, *), nList * MAX_FOV)
   ref_Tb2       = fltarr(nList * MAX_FOV, rad2.nChan)

   ;-----------------
   ; step 3:
   ;   3-d arrays
   ;-----------------
   ; Convert 3-D (Fov X File X Channel )
   ; into 2-D array ( (Fov * File) X Channel )
   ;   file 1        file 2             file n
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan 1
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan 2
   ;   ...
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan n
   ;
   FOR iChan = 0L, nChan - 1 DO BEGIN
      ref_Tb1(*, iChan) = reform(tbRad1(*, *, iChan), nList * MAX_FOV)
      ref_Tb2(*, iChan) = reform(tbRad2(*, *, iChan), nList * MAX_FOV)
   ENDFOR

END

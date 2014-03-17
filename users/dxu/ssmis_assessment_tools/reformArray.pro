;---------------------------------------------------------------------------------
; Name:  reformArray.pro
;
; Type:  IDL Program
;
; Description:
;   A locally-defined procedure used by the assessment tool main-level code
;   to combine the data of all the orbits into one big array. 
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO reformArray, MAX_FOV, nList,  $
   radData, refRadData

   ; Define struct to hold reformed data.
   ;-------------------------------------------------------
   ; Convert 2-D (Fov X File) into 1-D array ( Fov * File )
   ; Result:
   ;     file 1        file 2             file n
   ;   [ Fov * File ][ Fov * File ] ... [ Fov * File ]
   ;-------------------------------------------------------
   refRadData={  $
      ref_scanPos1  : reform(radData.scanPosRad1(*, *), nList * MAX_FOV), $
      ref_scanPos2  : reform(radData.scanPosRad2(*, *), nList * MAX_FOV), $
      ref_scanLine1 : reform(radData.scanLineRad1(*, *), nList * MAX_FOV),$
      ref_scanLine2 : reform(radData.scanLineRad2(*, *), nList * MAX_FOV),$
      ref_Lat1      : reform(radData.latRad1(*, *), nList * MAX_FOV),$
      ref_Lat2      : reform(radData.latRad2(*, *), nList * MAX_FOV),$
      ref_Lon1      : reform(radData.lonRad1(*, *), nList * MAX_FOV),$
      ref_Lon2      : reform(radData.lonRad2(*, *), nList * MAX_FOV),$
      ref_ModeFlag1 : reform(radData.dirRad1(*, *), nList * MAX_FOV),$
      ref_ModeFlag2 : reform(radData.dirRad2(*, *), nList * MAX_FOV),$
      ref_Angle1    : reform(radData.angleRad1(*, *), nList * MAX_FOV),$
      ref_Angle2    : reform(radData.angleRad2(*, *), nList * MAX_FOV),$
      ref_QC1       : reform(radData.QC_Rad1(*, *), nList * MAX_FOV),  $
      ref_QC2       : reform(radData.QC_Rad2(*, *), nList * MAX_FOV),  $
      ref_Tb1       : fltarr(nList * MAX_FOV, radData.nChan),$
      ref_Tb2       : fltarr(nList * MAX_FOV, radData.nChan),$
      ref_TbDiff    : fltarr(nList * MAX_FOV, radData.nChan) }

   ;--------------------------
   ; 3-d arrays conversion
   ;--------------------------
   ; Convert 3-D (Fov X File X Channel )
   ; into 2-D array ( (Fov * File) X Channel )
   ;   file 1        file 2             file n
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan 1
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan 2
   ;   ...
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan n
   ;
   FOR iChan = 0L, radData.nChan - 1 DO BEGIN
      refRadData.ref_Tb1(*, iChan) = reform(radData.tbRad1(*, *, iChan), nList * MAX_FOV)
      refRadData.ref_Tb2(*, iChan) = reform(radData.tbRad2(*, *, iChan), nList * MAX_FOV)
   ENDFOR

END

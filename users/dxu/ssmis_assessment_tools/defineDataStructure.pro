;---------------------------------------------------------------------------------
; Name:  defineDataStructure.pro
;
; Type:  IDL Program
;
; Description:
;   To define data structures used to hold various data.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 26, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO defineRadDataType, MAX_FOV, nOrbits, MAX_CHAN, radData

   radData = { RadDataType, $
      nFOV_Rad1    : lonarr(nOrbits),$         ; total number of FOVs in a file
      scanPosRad1  : intarr(MAX_FOV,nOrbits),$  ; pos per file
      scanLineRad1 : intarr(MAX_FOV,nOrbits),$  ; line per file
      latRad1      : fltarr(MAX_FOV,nOrbits),$  ; lat per file
      lonRad1   : fltarr(MAX_FOV,nOrbits),$  ; lon per file
      dirRad1   : fltarr(MAX_FOV,nOrbits),$  ; dir per file
      angleRad1 : fltarr(MAX_FOV,nOrbits),$  ; ang per file
      QC_Rad1   : lonarr(MAX_FOV,nOrbits),$  ; QC  per file
      tbRad1    : fltarr(MAX_FOV,nOrbits,MAX_CHAN),$ ; tb per file per channel
      nFOV_Rad2 : lonarr(nOrbits),$          ; total number of FOVs in a file
      scanPosRad2  : intarr(MAX_FOV,nOrbits),$  ; pos per file
      scanLineRad2 : intarr(MAX_FOV,nOrbits),$  ; line per file
      latRad2      : fltarr(MAX_FOV,nOrbits),$  ; lat per file
      lonRad2      : fltarr(MAX_FOV,nOrbits),$  ; lon per file
      dirRad2      : fltarr(MAX_FOV,nOrbits),$  ; dir per file
      angleRad2    : fltarr(MAX_FOV,nOrbits),$  ; ang per file
      QC_Rad2      : lonarr(MAX_FOV,nOrbits),$  ; QC  per file
      tbRad2       : fltarr(MAX_FOV,nOrbits,MAX_CHAN), $ ; tb per file per channel
      nChan        : 0L}

END

PRO defineSceneDataType, MAX_FOV, nOrbits,  sceneData

   sceneData = { SceneDataType, $
      tpwVec    : fltarr(MAX_FOV, nOrbits),  $
      clwVec    : fltarr(MAX_FOV, nOrbits),  $
      rwpVec    : fltarr(MAX_FOV, nOrbits),  $
      gwpVec    : fltarr(MAX_FOV, nOrbits),  $
      tSkinVec  : fltarr(MAX_FOV, nOrbits),  $
      sfcTypVec : lonarr(MAX_FOV, nOrbits) }

END

PRO defineRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, refRadData

   refRadData = { RefRadDataType,  $
      ref_scanPos1  : intarr(MAX_FOV,nOrbits),$
      ref_scanPos2  : intarr(MAX_FOV,nOrbits),$
      ref_scanLine1 : intarr(MAX_FOV,nOrbits),$
      ref_scanLine2 : intarr(MAX_FOV,nOrbits),$
      ref_Lat1      : fltarr(MAX_FOV,nOrbits),$
      ref_Lat2      : fltarr(MAX_FOV,nOrbits),$
      ref_Lon1      : fltarr(MAX_FOV,nOrbits),$
      ref_Lon2      : fltarr(MAX_FOV,nOrbits),$
      ref_ModeFlag1 : fltarr(MAX_FOV,nOrbits),$
      ref_ModeFlag2 : fltarr(MAX_FOV,nOrbits),$
      ref_Angle1    : fltarr(MAX_FOV,nOrbits),$
      ref_Angle2    : fltarr(MAX_FOV,nOrbits),$
      ref_QC1       : lonarr(MAX_FOV,nOrbits),$
      ref_QC2       : lonarr(MAX_FOV,nOrbits),$
      ref_Tb1       : fltarr(MAX_FOV,nOrbits,MAX_CHAN), $
      ref_Tb2       : fltarr(MAX_FOV,nOrbits,MAX_CHAN), $
      ref_TbDiff    : fltarr(MAX_FOV,nOrbits,MAX_CHAN)  }

END

PRO defineRefSceneDataType, MAX_FOV, nOrbits,  refSceneData

   refSceneData = { RefSceneDataType,  $
      ref_TPW_Vec: fltarr(MAX_FOV, nOrbits),   $
      ref_CLW_Vec: fltarr(MAX_FOV, nOrbits),   $
      ref_RWP_Vec: fltarr(MAX_FOV, nOrbits),   $
      ref_GWP_Vec: fltarr(MAX_FOV, nOrbits),   $
      ref_tSkin_Vec: fltarr(MAX_FOV, nOrbits), $
      ref_SfcType_Vec: lonarr(MAX_FOV, nOrbits)}

END


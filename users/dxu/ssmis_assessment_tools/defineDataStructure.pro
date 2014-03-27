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

   radData={ RadDataType, $
      nFOV : 0L, $  
      scanPos  : intarr(MAX_FOV,nOrbits), $  ; pos per file
      scanLine : intarr(MAX_FOV,nOrbits), $  ; line per file
      lat   : fltarr(MAX_FOV,nOrbits),$  ; lat per file
      lon   : fltarr(MAX_FOV,nOrbits),$  ; lon per file
      dir   : fltarr(MAX_FOV,nOrbits),$  ; dir per file
      angle : fltarr(MAX_FOV,nOrbits),$  ; ang per file
      QC    : lonarr(MAX_FOV,nOrbits),$  ; QC  per file
      tb    : fltarr(MAX_FOV,nOrbits,MAX_CHAN),$ ; tb per file per channel
      nChan : 0L}

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
      scanPos  : intarr(MAX_FOV * nOrbits),$
      scanLine : intarr(MAX_FOV * nOrbits),$
      lat      : fltarr(MAX_FOV * nOrbits),$
      lon      : fltarr(MAX_FOV * nOrbits),$
      modeFlag : fltarr(MAX_FOV * nOrbits),$
      angle    : fltarr(MAX_FOV * nOrbits),$
      QC       : lonarr(MAX_FOV * nOrbits),$
      tb       : fltarr(MAX_FOV * nOrbits,MAX_CHAN), $
      tbDiff    : fltarr(MAX_FOV * nOrbits,MAX_CHAN)  }

END

PRO defineRefSceneDataType, MAX_FOV, nOrbits,  refSceneData

   refSceneData = { RefSceneDataType,  $
      tpwVec: fltarr(MAX_FOV * nOrbits),   $
      clwVec: fltarr(MAX_FOV * nOrbits),   $
      rwpVec: fltarr(MAX_FOV * nOrbits),   $
      gwpVec: fltarr(MAX_FOV * nOrbits),   $
      tSkinVec: fltarr(MAX_FOV * nOrbits), $
      SfcTypeVec: lonarr(MAX_FOV * nOrbits)}

END


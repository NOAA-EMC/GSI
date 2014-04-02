;---------------------------------------------------------------------------------
; Name:  generateConditionalData.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to generate data in various conditions such as "clear sky", "cloudy sky", etc. 
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 31, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO generateConditionalData, filter, refRadObs, refRadSim, refSceneData, $
   refRadObsFilted, refRadSimFilted, refSceneDataFilted

   print, "size of filter ", N_ELEMENTS(filter)
   FOR i = 0, N_ELEMENTS(filter) - 1 DO BEGIN
      ; Get original index 
      j= filter(i)
      refRadObsFilted.scanPos(j)   = refRadObs.scanPos(j)
      refRadObsFilted.scanLine(j)  = refRadObs.scanLine(j)
      refRadObsFilted.lat(j)       = refRadObs.lat(j)
      refRadObsFilted.lon(j)       = refRadObs.lon(j)
      refRadObsFilted.modeFlag(j)  = refRadObs.modeFlag(j)
      refRadObsFilted.angle(j)     = refRadObs.angle(j)
      refRadObsFilted.QC(j)        = refRadObs.QC(j)
      refRadObsFilted.tb(j, *)     = refRadObs.tb(j, *)
      refRadObsFilted.tbDiff(j, *) = refRadObs.tbDiff(j, *)

      refRadSimFilted.scanPos(j)   = refRadSim.scanPos(j)
      refRadSimFilted.scanLine(j)  = refRadSim.scanLine(j)
      refRadSimFilted.lat(j)       = refRadSim.lat(j)
      refRadSimFilted.lon(j)       = refRadSim.lon(j)
      refRadSimFilted.modeFlag(j)  = refRadSim.modeFlag(j)
      refRadSimFilted.angle(j)     = refRadSim.angle(j)
      refRadSimFilted.QC(j)        = refRadSim.QC(j)
      refRadSimFilted.tb(j, *)     = refRadSim.tb(j, *)
      refRadSimFilted.tbDiff(j, *) = refRadSim.tbDiff(j, *)

      refSceneDataFilted.tpwVec(j)     = refSceneData.tpwVec(j)
      refSceneDataFilted.clwVec(j)     = refSceneData.clwVec(j)
      refSceneDataFilted.rwpVec(j)     = refSceneData.rwpVec(j)
      refSceneDataFilted.gwpVec(j)     = refSceneData.gwpVec(j)
      refSceneDataFilted.tSkinVec(j)   = refSceneData.tSkinVec(j)
      refSceneDataFilted.sfcTypeVec(j) = refSceneData.sfcTypeVec(j)

   ENDFOR

END

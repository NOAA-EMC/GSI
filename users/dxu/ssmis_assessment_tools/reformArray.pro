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
PRO reformArray, MAX_FOV, nOrbits,  $
   radObs, radSim, refRadObs, refRadSim, $
   sceneData, refSceneData

   ; Define struct to hold reformed data.
   ;-------------------------------------------------------
   ; Convert 2-D (Fov X File) into 1-D array ( Fov * File )
   ; Result:
   ;     file 1        file 2             file n
   ;   [ Fov * File ][ Fov * File ] ... [ Fov * File ]
   ;-------------------------------------------------------
   refRadObs.scanPos  = reform(radObs.scanPos(*, *), nOrbits * MAX_FOV)
   refRadObs.scanLine = reform(radObs.scanLine(*, *), nOrbits * MAX_FOV)
   refRadObs.lat      = reform(radObs.lat(*, *), nOrbits * MAX_FOV)
   refRadObs.lon      = reform(radObs.lon(*, *), nOrbits * MAX_FOV)
   refRadObs.modeFlag = reform(radObs.dir(*, *), nOrbits * MAX_FOV)
   refRadObs.angle    = reform(radObs.angle(*, *), nOrbits * MAX_FOV)
   refRadObs.QC       = reform(radObs.QC(*, *), nOrbits * MAX_FOV)

   refRadSim.scanPos  = reform(radSim.scanPos(*, *), nOrbits * MAX_FOV)
   refRadSim.scanLine = reform(radSim.scanLine(*, *), nOrbits * MAX_FOV)
   refRadSim.lat      = reform(radSim.lat(*, *), nOrbits * MAX_FOV)
   refRadSim.lon      = reform(radSim.lon(*, *), nOrbits * MAX_FOV)
   refRadSim.modeFlag = reform(radSim.dir(*, *), nOrbits * MAX_FOV)
   refRadSim.angle    = reform(radSim.angle(*, *), nOrbits * MAX_FOV)
   refRadSim.QC       = reform(radSim.QC(*, *), nOrbits * MAX_FOV)

   refSceneData.tpwVec = reform(sceneData.tpwVec(*, *), nOrbits * MAX_FOV)
   refSceneData.clwVec = reform(sceneData.clwVec(*, *), nOrbits * MAX_FOV)
   refSceneData.rwpVec = reform(sceneData.rwpVec(*, *), nOrbits * MAX_FOV)
   refSceneData.gwpVec = reform(sceneData.gwpVec(*, *), nOrbits * MAX_FOV)
   refSceneData.tSkinVec = reform(sceneData.tSkinVec(*, *), nOrbits * MAX_FOV)
   refSceneData.sfcTypeVec = reform(sceneData.sfcTypVec(*, *), nOrbits * MAX_FOV)

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
   FOR iChan = 0L, radObs.nChan - 1 DO BEGIN
      refRadObs.tb(*, iChan) = reform(radObs.tb(*, *, iChan), nOrbits * MAX_FOV)
      refRadSim.tb(*, iChan) = reform(radSim.tb(*, *, iChan), nOrbits * MAX_FOV)
      ; O - B, and save the same tbDiff into refRadObs and refRadSim
      refRadObs.tbDiff(*, iChan) = refRadObs.tb(*, iChan) - refRadSim.tb(*, iChan)
      refRadSim.tbDiff(*, iChan) = refRadObs.tbDiff(*, iChan)
   ENDFOR

END

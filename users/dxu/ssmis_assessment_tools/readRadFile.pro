;---------------------------------------------------------------------------------
; Name:  readRadFile.pro
;
; Type:  IDL Program
;
; Description:
;   To read radiance files using IDL code in MIRS trunk.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO readRadFile, nOrbits, MAX_FOV, MAX_CHAN,   $
    radFileList1, radFileList2, sceneFileList, $ 
    radObs, radSim, sceneData

   ;---------------------------------------
   ; step 1:
   ;   Read FMSRC file (observed radiance)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   FOR iFile = 0L, nOrbits - 1 DO BEGIN
      PRINT,'------------------------------------------------'
      PRINT,'Orbit ',iFile, " is being processed : "
      PRINT,'   observed radiance orbit file :',radFileList1(iFile)

      ;
      ; Read radiance from a pair of files to rad1 and rad2
      ;
      ; LoadRadFile I, I , O , X
      PRINT, "Start to open file to read ............" 
      LoadRadFile,1,radFileList1(iFile),rad1,0
      PRINT, "End of reading  file to read *****************"

      PRINT, "Number of files                   : ", rad1.nFilesRad
      PRINT, "Number of profiles in file        : ", rad1.nProf
      PRINT, "Number of channels in file        : ", rad1.nChan
      PRINT, "Number of scan positions per line : ", rad1.nPosScan
      PRINT, "Number of scan lines              : ", rad1.nScanLines
      PRINT,'------------------------------------------------'
      ; Save number of channels 
      IF (iFile eq 0L ) THEN BEGIN
         radObs.nChan = rad1.nChan
      ENDIF

      ; Save total number of FOVs in a file
      radObs.nFOV(iFile) = rad1.nprof

      ; Loop thru. FOVs within orbit
      FOR iProf = 0L, rad1.nprof - 1 DO BEGIN
	 radObs.scanPos(iProf, iFile) = rad1.scanPos(0, iProf)
	 radObs.scanLine(iProf, iFile) = rad1.scanLine(0, iProf)
	 radObs.lat(iProf, iFile)  = rad1.lat(0, iProf)
	 radObs.lon(iProf, iFile)  = rad1.lon(0, iProf)
	 radObs.dir(iProf, iFile)  = rad1.direc(0, iProf)
	 ; Average angles over all channels to compute mean angle
	 radObs.angle(iProf, iFile)  = mean(rad1.angle(0, iProf, 0 : rad1.nChan - 1))
	 ; Get the 1st QC in the 1st orbit
	 radObs.QC(iProf, iFile)  = rad1.QC(0,iProf,0)
	 ; Get tb for each channel
	 radObs.tb(iProf, iFile, 0 : rad1.nChan - 1)   $
	     = rad1.tb(0, iProf, 0 : rad1.nChan - 1)
      ENDFOR
   ENDFOR

   ;---------------------------------------
   ; step 2:
   ;   Read FWD file   (simulated radiance)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   FOR iFile = 0L, nOrbits - 1 DO BEGIN
      PRINT,'------------------------------------------------'
      PRINT,'Orbit ',iFile, " is being processed : "
      PRINT,'   simulated radiance orbit file :',radFileList2(iFile)

      ;
      ; Read radiance from rad2
      ;
      ; LoadRadFile I, I , O , X
      LoadRadFile,1,radFileList2(iFile),rad2,0

      PRINT, "Number of files                   : ", rad2.nFilesRad
      PRINT, "Number of profiles in file        : ", rad2.nProf
      PRINT, "Number of channels in file        : ", rad2.nChan
      PRINT, "Number of scan positions per line : ", rad2.nPosScan
      PRINT, "Number of scan lines              : ", rad2.nScanLines
      PRINT,'------------------------------------------------'
      ; Save number of channels 
      IF (iFile eq 0L ) THEN BEGIN
         radSim.nChan = rad2.nChan
      ENDIF

      ; Save total number of FOVs in a file
      radSim.nFOV(iFile) = rad2.nprof

      ; Loop thru. FOVs within orbit
      FOR iProf = 0L, rad2.nprof - 1 DO BEGIN
	 radSim.scanPos(iProf, iFile) = rad2.scanPos(0, iProf)
	 radSim.scanLine(iProf, iFile) = rad2.scanLine(0, iProf)
	 radSim.lat(iProf, iFile)  = rad2.lat(0, iProf)
	 radSim.lon(iProf, iFile)  = rad2.lon(0, iProf)
	 radSim.dir(iProf, iFile)  = rad2.direc(0, iProf)
	 ; Average angles over all channels to compute mean angle
	 radSim.angle(iProf, iFile)  = mean(rad2.angle(0, iProf, 0 : rad2.nChan - 1))
	 ; Get the 1st QC in the 1st orbit
	 radSim.QC(iProf, iFile)  = rad2.QC(0,iProf,0)
	 ; Get tb for each channel
	 radSim.tb(iProf, iFile, 0 : rad2.nChan - 1)   $
	     = rad2.tb(0, iProf, 0 : rad2.nChan - 1)
      ENDFOR
   ENDFOR

   ;---------------------------------------
   ; step 3:
   ;   Read scene data (GFS 6-hr forecast)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   FOR iFile = 0L, nOrbits - 1 DO BEGIN
      PRINT,'------------------------------------------------'
      PRINT,'Orbit ',iFile, " is being processed : "
      PRINT,'   scene file :', sceneFileList(iFile)

      ;
      ; Read scene data
      ;
      LoadSceneFile, sceneFileList(iFile), topID, scene, 100000000L

      PRINT, "Number of profiles in file        : ", scene.nProfsProcessed
      PRINT, "Number of channels in file        : ", scene.nChan
      PRINT, "Number of scan positions per line : ", scene.nScanPos 
      PRINT, "Number of scan lines              : ", scene.nScanLines 
      PRINT,'------------------------------------------------'

      ; Save the total number of profiles 
      total_num = scene.nProfsProcessed

      ; Loop thru. FOVs within orbit
      FOR iProf=0L, total_num - 1 DO BEGIN
         sceneData.tpwVec(iProf, iFile) = scene.tpwVec(iProf) 
         sceneData.clwVec(iProf, iFile) = scene.clwVec(iProf) 
         sceneData.rwpVec(iProf, iFile) = scene.rwpVec(iProf) 
         sceneData.gwpVec(iProf, iFile) = scene.gwpVec(iProf) 
         sceneData.tSkinVec(iProf, iFile) = scene.tSkinVec(iProf) 
         sceneData.sfcTypVec(iProf, iFile) = scene.sfcTypVec(iProf) 
      ENDFOR
   ENDFOR

END

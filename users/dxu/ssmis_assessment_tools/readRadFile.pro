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
    radData, sceneData

   ;---------------------------------------
   ; step 1:
   ;   Define a struct to hold data
   ;---------------------------------------
   ; Radiance data structure 
   radData={ $
   nFOV_Rad1    : lonarr(nOrbits),$         ; total number of FOVs in a file
   scanPosRad1  : intarr(MAX_FOV,nOrbits),$  ; pos per file
   scanLineRad1 : intarr(MAX_FOV,nOrbits),$  ; line per file
   latRad1      : fltarr(MAX_FOV,nOrbits),$  ; lat per file
   lonRad1   : fltarr(MAX_FOV,nOrbits),$  ; lon per file
   dirRad1   : fltarr(MAX_FOV,nOrbits),$  ; dir per file
   angleRad1 : fltarr(MAX_FOV,nOrbits),$  ; ang per file
   QC_Rad1   : fltarr(MAX_FOV,nOrbits),$  ; QC  per file
   tbRad1    : fltarr(MAX_FOV,nOrbits,MAX_CHAN),$ ; tb per file per channel
   nFOV_Rad2 : lonarr(nOrbits),$          ; total number of FOVs in a file
   scanPosRad2  : intarr(MAX_FOV,nOrbits),$  ; pos per file
   scanLineRad2 : intarr(MAX_FOV,nOrbits),$  ; line per file
   latRad2      : fltarr(MAX_FOV,nOrbits),$  ; lat per file
   lonRad2      : fltarr(MAX_FOV,nOrbits),$  ; lon per file
   dirRad2      : fltarr(MAX_FOV,nOrbits),$  ; dir per file
   angleRad2    : fltarr(MAX_FOV,nOrbits),$  ; ang per file
   QC_Rad2      : fltarr(MAX_FOV,nOrbits),$  ; QC  per file
   tbRad2       : fltarr(MAX_FOV,nOrbits,MAX_CHAN), $ ; tb per file per channel
   nChan        : 0L} 

   ; Scene data structure 
   sceneData={ $
       tpwVec: fltarr(MAX_FOV, nOrbits),  $
       clwVec: fltarr(MAX_FOV, nOrbits),  $
       rwpVec: fltarr(MAX_FOV, nOrbits),  $
       gwpVec: fltarr(MAX_FOV, nOrbits),  $
       tSkinVec: fltarr(MAX_FOV, nOrbits) }

   ;---------------------------------------
   ; step 2:
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
         radData.nChan = rad1.nChan
      ENDIF

      ; Save the number of channels
      nChan = rad1.nChan

      ; Save total number of FOVs in a file
      radData.nFOV_Rad1(iFile) = rad1.nprof

      ; Loop thru. FOVs within orbit
      FOR iProf = 0L, rad1.nprof - 1 DO BEGIN
	 radData.scanPosRad1(iProf, iFile) = rad1.ScanPos(0, iProf)
	 radData.scanLineRad1(iProf, iFile) = rad1.ScanLine(0, iProf)
	 radData.latRad1(iProf, iFile)  = rad1.Lat(0, iProf)
	 radData.lonRad1(iProf, iFile)  = rad1.Lon(0, iProf)
	 radData.dirRad1(iProf, iFile)  = rad1.Direc(0, iProf)
	 ; Average angles over all channels to compute mean angle
	 radData.angleRad1(iProf, iFile)  = mean(rad1.Angle(0, iProf, 0 : rad1.nChan - 1))
	 ; Get the 1st QC in the 1st orbit
	 radData.QC_Rad1(iProf, iFile)  = rad1.QC(0,iProf,0)
	 ; Get tb for each channel
	 radData.tbRad1(iProf, iFile, 0 : rad1.nChan - 1)   $
	     = rad1.tb(0, iProf, 0 : rad1.nChan - 1)
      ENDFOR
   ENDFOR

   ;---------------------------------------
   ; step 3:
   ;   Read FWD file   (simulated radiance)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   FOR iFile = 0L, nOrbits - 1 DO BEGIN
      PRINT,'------------------------------------------------'
      PRINT,'Orbit ',iFile, " is being processed : "
      PRINT,'   simulated orbit file :',radFileList2(iFile)

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

      ; Save total number of FOVs in a file
      radData.nFOV_Rad2(iFile) = rad2.nprof

      ; Loop thru. FOVs within orbit
      FOR iProf = 0L, rad2.nprof - 1 DO BEGIN
	 radData.scanPosRad2(iProf, iFile) = rad2.ScanPos(0, iProf)
	 radData.scanLineRad2(iProf, iFile) = rad2.ScanLine(0, iProf)
	 radData.latRad2(iProf, iFile)  = rad2.Lat(0, iProf)
	 radData.lonRad2(iProf, iFile)  = rad2.Lon(0, iProf)
	 radData.dirRad2(iProf, iFile)  = rad2.Direc(0, iProf)
	 ; Average angles over all channels to compute mean angle
	 radData.angleRad2(iProf, iFile)  = mean(rad2.Angle(0, iProf, 0 : rad2.nChan - 1))
	 ; Get the 1st QC in the 1st orbit
	 radData.QC_Rad2(iProf, iFile)  = rad2.QC(0,iProf,0)
	 ; Get tb for each channel
	 radData.tbRad2(iProf, iFile, 0 : rad2.nChan - 1)   $
	     = rad2.tb(0, iProf, 0 : rad2.nChan - 1)
      ENDFOR
   ENDFOR

   ;---------------------------------------
   ; step 4:
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
      ENDFOR
   ENDFOR

END

PRO readRadFile, nList
   ;-------------------------------
   ; step 1:
   ;   Declare variables for rad1
   ;-------------------------------
   ;
   ; 1-D array (1) : File
   nFOV_Rad1 = lonarr(nList)          ; total number of FOVs in a file
   ; 2-D array (6) : Fov x File
   scanPosRad1  = intarr(MAX_FOV,nList)  ; pos per file
   scanLineRad1 = intarr(MAX_FOV,nList)  ; line per file
   latRad1      = fltarr(MAX_FOV,nList)  ; lat per file
   lonRad1   = fltarr(MAX_FOV,nList)  ; lon per file
   dirRad1   = fltarr(MAX_FOV,nList)  ; dir per file
   angleRad1   = fltarr(MAX_FOV,nList)  ; ang per file
   QC_Rad1   = fltarr(MAX_FOV,nList)  ; QC  per file
   ; 3-D array (1) : Fov X File X Channel
   tbRad1    = fltarr(MAX_FOV,nList,MAX_CHAN) ; tb per file per channel
   ;
   ; Declare variables for rad2
   ;
   ; 1-D array (1) : File
   nFOV_Rad2 = lonarr(nList)          ; total number of FOVs in a file
   ; 2-D array (6) : Fov x File
   scanPosRad2  = intarr(MAX_FOV,nList)  ; pos per file
   scanLineRad2 = intarr(MAX_FOV,nList)  ; line per file
   latRad2      = fltarr(MAX_FOV,nList)  ; lat per file
   lonRad2      = fltarr(MAX_FOV,nList)  ; lon per file
   dirRad2      = fltarr(MAX_FOV,nList)  ; dir per file
   angleRad2    = fltarr(MAX_FOV,nList)  ; ang per file
   QC_Rad2       = fltarr(MAX_FOV,nList)  ; QC  per file
   ; 3-D array (1) : Fov X File X Channel
   tbRad2    = fltarr(MAX_FOV,nList,MAX_CHAN) ; tb per file per channel


   ;---------------------------------------
   ; step 2:
   ;   Read FMSRC file (observed radiance)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   FOR iFile = 0L, nList - 1 DO BEGIN
      PRINT,'------------------------------------------------'
      PRINT,'Orbit ',iFile, " is being processed : "
      PRINT,'   observed radiance orbit file :',radFileList1(iFile)

      ;
      ; Read radiance from a pair of files to rad1 and rad2
      ;
      ; LoadRadFile I, I , O , X
      LoadRadFile,1,radFileList1(iFile),rad1,0

      PRINT, "Number of files                   : ", rad1.nFilesRad
      PRINT, "Number of profiles in file        : ", rad1.nProf
      PRINT, "Number of channels in file        : ", rad1.nChan
      PRINT, "Number of scan positions per line : ", rad1.nPosScan
      PRINT, "Number of scan lines              : ", rad1.nScanLines
      PRINT,'------------------------------------------------'

      ; Save total number of FOVs in a file
      nFOV_Rad1(iFile) = rad1.nprof

      ; Loop thru. FOVs within orbit
      FOR iProf=0L,rad1.nprof-1 DO BEGIN
	 scanPosRad1(iProf, iFile) = rad1.ScanPos(0, iProf)
	 scanLineRad1(iProf, iFile) = rad1.ScanLine(0, iProf)
	 latRad1(iProf, iFile)  = rad1.Lat(0, iProf)
	 lonRad1(iProf, iFile)  = rad1.Lon(0, iProf)
	 dirRad1(iProf, iFile)  = rad1.Direc(0, iProf)
	 ; Average angles over all channels to compute mean angle
	 angleRad1(iProf, iFile)  = mean(rad1.Angle(0, iProf, 0 : rad1.nChan - 1))
	 ; Get the 1st QC in the 1st orbit
	 QC_Rad1(iProf, iFile)  = rad1.QC(0,iProf,0)
	 ; Get tb for each channel
	 tbRad1(iProf, iFile, 0 : rad1.nChan - 1)   $
	     = rad1.tb(0, iProf, 0 : rad1.nChan - 1)
      ENDFOR
   ENDFOR


   ;---------------------------------------
   ; step 3:
   ;   Read FWD file   (simulated radiance)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   FOR iFile=0L,nRadFiles1-1 DO BEGIN
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

      ; Loop thru. FOVs within orbit
      FOR iProf=0L,rad2.nprof-1 DO BEGIN
	 scanPosRad2(iProf, iFile) = rad2.ScanPos(0, iProf)
	 scanLineRad2(iProf, iFile) = rad2.ScanLine(0, iProf)
	 latRad2(iProf, iFile)  = rad2.Lat(0, iProf)
	 lonRad2(iProf, iFile)  = rad2.Lon(0, iProf)
	 dirRad2(iProf, iFile)  = rad2.Direc(0, iProf)
	 ; Average angles over all channels to compute mean angle
	 angleRad2(iProf, iFile)  = mean(rad2.Angle(0, iProf, 0 : rad2.nChan - 1))
	 ; Get the 1st QC in the 1st orbit
	 QC_Rad2(iProf, iFile)  = rad2.QC(0,iProf,0)
	 ; Get tb for each channel
	 tbRad2(iProf, iFile, 0 : rad2.nChan - 1)   $
	     = rad2.tb(0, iProf, 0 : rad2.nChan - 1)
      ENDFOR
   ENDFOR
END

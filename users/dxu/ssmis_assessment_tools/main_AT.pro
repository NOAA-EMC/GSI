;---------------------------------------------------------------------------------
; Name: main_AT.pro
;
; Type:  IDL Program
;
; Description: 
;   To read observed radiance data and simulated radiance data,
;   which is generated from GFS 6-h forecast, then do following steps:
;   to access SSMIS data quality. 
;     1. Plot observed radiance
;     2. Plot simulated radiance
;     3. Plot Bias ( observed radiance - simulated radiance )
;
; Author: Deyong Xu (RTI) @ JCSDA, 
;         Deyong.Xu@noaa.gov
; Version: Feb 27, 2014, DXu, Initial coding
;         
;
;---------------------------------------------------------------------------------
; Specify IDL plotting and util code to use.
@paths_idl.pro
; Specify locally-defined util code.
@AT_Util.pro

;###########################
 mark_chooseSensor:
;###########################
PRINT, 'Choose instrument: '
PRINT,' 1 : NOAA-18/AMSUA&MHS'
PRINT,' 2 : NOAA-19/AMSUA&MHS'
PRINT,' 3 : MetOp-A/AMSUA&MHS'
PRINT,' 4 : MetOp-B/AMSUA/MHS'
PRINT,' 5 : F16/SSMIS'
PRINT,' 6 : F17/SSMIS'
PRINT,' 7 : F18/SSMIS'
PRINT,' 8 : NPP/ATMS'
PRINT,' 9 : AQUA/AMSRE'
PRINT,'10 : GCOMW1/AMSR2'
PRINT,'11 : FY3/MWRI'
PRINT,'12 : FY3/MWHS/MWTS'
PRINT,'13 : TRMM/TMI'
PRINT,'14 : GPM/GMI'
PRINT,'15 : MT/MADRAS'
PRINT,'16 : MT/SAPHIR'
PRINT,'17 : WindSat'

sensorOption = 0S
READ, sensorOption

; Set flag to fill value: 999
optionFlag = 999
; Check to see if a right option is chosen.
FOR i = 1, 17 DO BEGIN 
   IF sensorOption eq i THEN BEGIN
      optionFlag = sensorOption 
      BREAK
   ENDIF
ENDFOR

; None of options is chosen
IF ( optionFlag eq 999 ) THEN BEGIN
   PRINT, "Wrong option, choose again !!!" 
   PRINT, ""
   GOTO, mark_chooseSensor
ENDIF
                        
;-------------------------------------------
; Read config params for the sensor chosen
;-------------------------------------------
configSensorParam, sensorOption, paramStruct 

; Save config parameters to shorten name
MIN_LAT = paramStruct.MIN_LAT
MAX_LAT = paramStruct.MAX_LAT
MIN_LON = paramStruct.MIN_LON
MAX_LON = paramStruct.MAX_LON
radListFile1  = paramStruct.radListFile1
radListFile2  = paramStruct.radListFile2
sceneListFile = paramStruct.sceneListFile
MAX_FOV       = paramStruct.MAX_FOV
MAX_CHAN      = paramStruct.MAX_CHAN
sensorName    = paramStruct.sensorName
INT_FILL_Val = paramStruct.INT_FILL
FLOAT_FILL_Val = paramStruct.FLOAT_FILL
STRING_FILL_Val = paramStruct.STRING_FILL
; Filter out FILL values
indices_1 = WHERE(paramStruct.chanNumArray ne INT_FILL_Val)
chanNumArray  = paramStruct.chanNumArray(indices_1)
indices_2 = WHERE(paramStruct.chanInfoArray ne STRING_FILL_Val)
chanInfoArray = paramStruct.chanInfoArray(indices_2)
indices_3 = WHERE(paramStruct.minBT_Values ne FLOAT_FILL_Val)
minBT_Values  = paramStruct.minBT_Values(indices_3)
indices_4 = WHERE(paramStruct.maxBT_Values ne FLOAT_FILL_Val)
maxBT_Values  = paramStruct.maxBT_Values(indices_4)
indices_5= WHERE(paramStruct.chPlotArray ne INT_FILL_Val)
chPlotArray   = paramStruct.chPlotArray(indices_5)
CLW_THRESHOLD_MIN = paramStruct.CLW_THRESHOLD_MIN
CLW_THRESHOLD_MAX = paramStruct.CLW_THRESHOLD_MAX
RWP_THRESHOLD = paramStruct.RWP_THRESHOLD
GWP_THRESHOLD = paramStruct.GWP_THRESHOLD
date          = paramStruct.date

;###########################
 mark_readAgain:
;###########################
PRINT, 'Read data again?'
PRINT, '1 - YES'
PRINT, '2 - NO, to reform data'
PRINT, '3 - NO, to plot unfiltered data'
PRINT, '4 - NO, to plot clear sky '
PRINT, '5 - NO, to plot cloudy sky'
PRINT, '6 - NO, to plot precipitation'

READ, readAgain
CASE readAgain OF
   1: GOTO, mark_read_data
   2: GOTO, mark_reform_data
   3: GOTO, mark_plotting_Unfiltered
   4: GOTO, mark_plotting_ClearSky 
   5: GOTO, mark_plotting_CloudySky 
   6: GOTO, mark_plotting_Precip 
   ELSE: BEGIN & PRINT, 'Wrong option!!! Chose again...' & GOTO, mark_readAgain & END
ENDCASE

; None of options is chosen
IF ( optionFlag eq 999 ) THEN BEGIN
   PRINT, "Wrong option, choose again !!!" 
   PRINT, ""
   GOTO, mark_readAgain
ENDIF


;###########################
 mark_read_data:
;###########################
;------------------------------------
; step 1: 
;   Read two lists of radiance files
;   and one list of  scene files
;------------------------------------
readlist, radListFile1, radFileList1, nFilesRad1
readlist, radListFile2, radFileList2, nFilesRad2
readlist, sceneListFile, sceneFileList, nFileScene

; Get number of radiance files in each list/array.
nRadFiles1 = n_elements(radFileList1)
nRadFiles2 = n_elements(radFileList2)
nSceneFiles = n_elements(sceneFileList)

; Make sure that numbers of radiance files and 
; scenef files are equal and not 0.
IF ( ( nRadFiles1 NE nRadFiles2 )  || $ 
     ( nRadFiles1 NE nSceneFiles ) || $ 
     ( nRadFiles1 EQ 0 ) ) THEN BEGIN
   PRINT,'Error: Number of Rad files in two list files NOT match'
   STOP
ENDIF

; Save number of rad files (orbits)
nOrbits=nRadFiles1

;---------------------------------------------
; step 2:
;   Define data structures and initalize data 
;---------------------------------------------
initializeAll, MAX_FOV, nOrbits, MAX_CHAN, $
   radObs, radSim, sceneData, $
   refRadObs, refRadSim, refSceneData

PRINT, "Begin readRadFile  =========="
;-------------------------------------------
; step 3:
;   Read radiances (measurements) from List1
;   Read radiances (simulated) from List2
;   Read scene file (GFS 6-hr forecast)
;-------------------------------------------
print, "****************    ",  sceneFileList
readRadFile, nOrbits, MAX_FOV, MAX_CHAN,      $
   radFileList1, radFileList2, sceneFileList, $
   radObs, radSim, sceneData

PRINT, "done with readRadFile  =========="

;-------------------------------------------
; step 4:
;   Reform data
;-------------------------------------------
;###########################
 mark_reform_data:
;###########################
reformArray, MAX_FOV, nOrbits,  $
   radObs, radSim, refRadObs, refRadSim, $
   sceneData, refSceneData    

;-----------------------------------------
; step 5: 
;   Plot radiances (observed + simulated)
;-----------------------------------------
;###########################
 mark_plotting_Unfiltered:
;###########################
; Plot radiances and radiance difference
fileNamePrefix = sensorName + '_Rad_plotting_' 
plotRad, chPlotArray, chanNumArray, chanInfoArray, fileNamePrefix,   $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,$
    refRadObs, refRadSim, date

; Plot various scattering plots
fileNamePrefix = sensorName + '_Scatter_plotting_' 
plotScattering, chPlotArray, chanNumArray, chanInfoArray, fileNamePrefix,  $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
    refRadObs, refRadSim, refSceneData, date

;###########################
 mark_plotting_ClearSky:
;###########################
; Declare variables for Clear Sky
refRadObs_ClearSky = CREATE_STRUCT(NAME = 'RefRadDataType')
refRadSim_ClearSky = CREATE_STRUCT(NAME = 'RefRadDataType')
refSceneData_ClearSky = CREATE_STRUCT(NAME = 'RefSceneDataType')

initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, refRadObs_ClearSky
initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, refRadSim_ClearSky
initializeRefSceneDataType, MAX_FOV, nOrbits, refSceneData_ClearSky 

; Define filter to get clear sky points via scene data 
filterClearSky = WHERE(refSceneData.clwVec LT CLW_THRESHOLD_MIN $
                  OR refSceneData.rwpVec LT RWP_THRESHOLD      $
                  OR refSceneData.rwpVec LT RWP_THRESHOLD )

; Generated filtered data
generateConditionalData, filterClearSky, refRadObs, refRadSim, refSceneData, $
   refRadObs_ClearSky, refRadSim_ClearSky, refSceneData_ClearSky

fileNamePrefix = sensorName + '_Rad_plotting_ClearSky_' 
plotRad, chPlotArray, chanNumArray, chanInfoArray, fileNamePrefix,   $
   MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,$
   refRadObs_ClearSky, refRadSim_ClearSky, date

; Plot various scattering plots
fileNamePrefix = sensorName + '_Scatter_plotting_ClearSky_' 
plotScattering, chPlotArray, chanNumArray, chanInfoArray, fileNamePrefix,  $
   MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
   refRadObs_ClearSky, refRadSim_ClearSky, refSceneData_ClearSky, date

;###########################
 mark_plotting_CloudySky:
;###########################
; Declare variables for Cloudy Sky
refRadObs_CloudySky = CREATE_STRUCT(NAME = 'RefRadDataType')
refRadSim_CloudySky = CREATE_STRUCT(NAME = 'RefRadDataType')
refSceneData_CloudySky = CREATE_STRUCT(NAME = 'RefSceneDataType')

initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, refRadObs_CloudySky
initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, refRadSim_CloudySky
initializeRefSceneDataType, MAX_FOV, nOrbits, refSceneData_CloudySky

; Define filter to get clear sky points via scene data 
filterCloudySky = WHERE(refSceneData.clwVec GT CLW_THRESHOLD_MIN $
                  AND refSceneData.clwVec LT CLW_THRESHOLD_MAX $
                  AND refSceneData.rwpVec LT RWP_THRESHOLD      $
                  AND refSceneData.rwpVec LT RWP_THRESHOLD )

; Generated filtered data
generateConditionalData, filterCloudySky, refRadObs, refRadSim, refSceneData, $
   refRadObs_CloudySky, refRadSim_CloudySky, refSceneData_CloudySky

fileNamePrefix = sensorName + '_Rad_plotting_CloudySky_' 
plotRad, chPlotArray, chanNumArray, chanInfoArray, fileNamePrefix,   $
   MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,$
   refRadObs_CloudySky, refRadSim_CloudySky, date

; Plot various scattering plots
fileNamePrefix = sensorName + '_Scatter_plotting_CloudySky_' 
plotScattering, chPlotArray, chanNumArray, chanInfoArray, fileNamePrefix, $
   MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
   refRadObs_CloudySky, refRadSim_CloudySky, refSceneData_CloudySky, date

;###########################
 mark_plotting_Precip:
;###########################
; Declare variables for Precipitation
refRadObs_Precipitation = CREATE_STRUCT(NAME = 'RefRadDataType')
refRadSim_Precipitation = CREATE_STRUCT(NAME = 'RefRadDataType')
refSceneData_Precipitation = CREATE_STRUCT(NAME = 'RefSceneDataType')

initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, refRadObs_Precipitation
initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, refRadSim_Precipitation
initializeRefSceneDataType, MAX_FOV, nOrbits, refSceneData_Precipitation

; Define filter to get clear sky points via scene data 
filterPrecipitation = WHERE(refSceneData.clwVec GT CLW_THRESHOLD_MAX $
                  OR ( refSceneData.rwpVec GT RWP_THRESHOLD      $
                  OR refSceneData.rwpVec GT RWP_THRESHOLD ) )

; Generated filtered data
generateConditionalData, filterPrecipitation, refRadObs, refRadSim, refSceneData, $
   refRadObs_Precipitation, refRadSim_Precipitation, refSceneData_Precipitation

fileNamePrefix = sensorName + '_Rad_plotting_Precipitation_' 
plotRad, chPlotArray, chanNumArray, chanInfoArray, fileNamePrefix,   $
   MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,$
   refRadObs_Precipitation, refRadSim_Precipitation, date

; Plot various scattering plots
fileNamePrefix = sensorName + '_Scatter_plotting_Preciptation_' 
plotScattering, chPlotArray, chanNumArray, chanInfoArray, fileNamePrefix,  $
   MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
   refRadObs_Precipitation, refRadSim_Precipitation, refSceneData_Precipitation, date


PRINT,'End of processing...'
END

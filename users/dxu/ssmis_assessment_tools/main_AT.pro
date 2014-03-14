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

chooseSensor:
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
FOR i = 0, 16 DO BEGIN 
   IF sensorOption eq (i + 1) THEN BEGIN
      optionFlag = sensorOption 
      BREAK
   ENDIF
ENDFOR

; None of options is chosen
IF ( optionFlag eq 999 ) THEN BEGIN
   PRINT, "Wrong option, choose again !!!" 
   PRINT, ""
   GOTO, chooseSensor
ENDIF
                        
;-------------------------------------------
; Read config params for the sensor chosen
;-------------------------------------------
configSensorParam, sensorOption, paramStruct 

; Save config parameters 
radListFile1  = paramStruct.radListFile1
radListFile2  = paramStruct.radListFile2
MAX_FOV       = paramStruct.MAX_FOV
MAX_CHAN      = paramStruct.MAX_CHAN
sceneListFile = paramStruct.sceneListFile
chanNumArray  = paramStruct.chanNumArray
chanInfoArray = paramStruct.chanInfoArray
minBT_Values  = paramStruct.minBT_Values
maxBT_Values  = paramStruct.maxBT_Values
prefixArray   = paramStruct.prefixArray

PRINT, 'Read data again?'
PRINT, '1 - YES'
PRINT, '2 - NO, to reform'
PRINT, '3 - NO, to plot radiance'
PRINT, '4 - NO, to plot radiance diff'

READ, readAgain
IF (readAgain eq 1) THEN GOTO, mark_readMeas
IF (readAgain eq 2) THEN GOTO, mark_reform
IF (readAgain eq 3) THEN GOTO, mark_plotting
IF (readAgain eq 4) THEN GOTO, mark_plotting_bias

mark_readMeas:
;------------------------------------
; step 1: 
;   Read two lists of radiance files
;------------------------------------
readlist, radListFile1, radFileList1, nfilesRad1
readlist, radListFile2, radFileList2, nfilesRad2

; Get number of radiance files in each list/array.
nRadFiles1 = n_elements(radFileList1)
nRadFiles2 = n_elements(radFileList2)

; Make sure that number of radiance files are equal and not 0.
IF ( (nRadFiles1 ne nRadFiles2) || nRadFiles1 eq 0 ) THEN BEGIN
   PRINT,'Error: Number of Rad files in two list files NOT match'
   stop
ENDIF

; Save number of rad files (orbits)
nList=nRadFiles1

print, "Begin readRadFile  =========="
;-------------------------------------------
; step 2:
;   Read radiances (measurements) from List1
;   Read radiances (simulated) from List2
;-------------------------------------------
readRadFile, nList, MAX_FOV, MAX_CHAN,    $
   radFileList1, radFileList2,            $
   radData

print, "done with readRadFile  =========="

;-------------------------------------------
; step 3:
;   Reform data
;-------------------------------------------
mark_reform:
reformArray, MAX_FOV, nList, nChan,  $
   radData, refRadData

;-----------------------------------------
; step 4: 
;   Plot radiances (observed + simulated)
;-----------------------------------------
mark_plotting:

;###########################################
; Configuration to customize needs.
;###########################################
; Specify the channels to plot
;chPlotArray = INDGEN(24)
;chPlotArray = INDGEN(7)
chPlotArray = [1]
date = '2013-01-20'

plotRad, chPlotArray, chanNumArray, chanInfoArray, prefixArr[0],   $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,$
    refRadData, date

mark_plotting_bias:

plotRadDiff, chPlotArray, chanNumArray, chanInfoArray, prefixArr[1], $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON,      $
    refRadData, nChan, date

PRINT,'End of processing...'
END

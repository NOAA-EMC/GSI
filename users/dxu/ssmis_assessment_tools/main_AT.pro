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

mark_chooseSensor:
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

; Save config parameters 
MIN_LAT = paramStruct.MIN_LAT
MAX_LAT = paramStruct.MAX_LAT
MIN_LON = paramStruct.MIN_LON
MAX_LON = paramStruct.MAX_LON
radListFile1  = paramStruct.radListFile1
radListFile2  = paramStruct.radListFile2
MAX_FOV       = paramStruct.MAX_FOV
MAX_CHAN      = paramStruct.MAX_CHAN
sceneListFile = paramStruct.sceneListFile
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
indices_5= WHERE(paramStruct.prefixArray ne STRING_FILL_Val)
prefixArray   = paramStruct.prefixArray(indices_5)
indices_6= WHERE(paramStruct.chPlotArray ne INT_FILL_Val)
chPlotArray   = paramStruct.chPlotArray(indices_6)
date          = paramStruct.date

mark_readAgain:
PRINT, 'Read data again?'
PRINT, '1 - YES'
PRINT, '2 - NO, to reform'
PRINT, '3 - NO, to plot radiance'
PRINT, '4 - NO, to plot clear sky '
PRINT, '5 - NO, to plot cloudy sky'
PRINT, '6 - NO, to plot precipitation'

READ, readAgain
CASE readAgain OF
   1: GOTO, mark_readMeas
   2: GOTO, mark_reform
   3: GOTO, mark_plotting 
   4: GOTO, mark_plotting_ClearSky 
   5: GOTO, mark_plotting_CloudySky 
   6: GOTO, mark_plotting_Precip 
   ELSE: BEGIN & print, 'Wrong option!!! Chose again...' & GOTO, mark_readAgain & END
ENDCASE

; None of options is chosen
IF ( optionFlag eq 999 ) THEN BEGIN
   PRINT, "Wrong option, choose again !!!" 
   PRINT, ""
   GOTO, mark_readAgain
ENDIF


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

PRINT, "Begin readRadFile  =========="
;-------------------------------------------
; step 2:
;   Read radiances (measurements) from List1
;   Read radiances (simulated) from List2
;-------------------------------------------
readRadFile, nList, MAX_FOV, MAX_CHAN,    $
   radFileList1, radFileList2,            $
   radData

PRINT, "done with readRadFile  =========="

;-------------------------------------------
; step 3:
;   Reform data
;-------------------------------------------
mark_reform:
reformArray, MAX_FOV, nList,  $
   radData, refRadData

;-----------------------------------------
; step 4: 
;   Plot radiances (observed + simulated)
;-----------------------------------------
mark_plotting:

; Plot radiances and radiance difference
plotRad, chPlotArray, chanNumArray, chanInfoArray, prefixArray[0],   $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,$
    refRadData, date

mark_plotting_ClearSky:
mark_plotting_CloudySky:
mark_plotting_Precip:

PRINT,'End of processing...'
END

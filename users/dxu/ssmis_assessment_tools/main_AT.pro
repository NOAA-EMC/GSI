;---------------------------------------------------------------------------------
; Name: plotTwoRads.pro 
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
configSensorParam, sensorOption
;configSensorParam, sensorOption, chanNumArray, chanInfoArrray, $
;    minBT_Values, maxBT_Values

PRINT, 'Read data again?'
PRINT, '0 - NO'
PRINT, '1 - YES'

READ, readAgain
IF (readAgain eq 0) THEN GOTO, mark_plotting
IF (readAgain eq 1) THEN GOTO, mark_readMeas

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

;-------------------------------------------
; step 2:
;   Read radiances (measurements) from List1
;   Read radiances (simulated) from List2
;-------------------------------------------
readRadFile, nList

;-------------------------------------------
; step 3:
;   Reform data
;-------------------------------------------
reformArray

;-----------------------------------------
; step 4: 
;   Plot radiances (observed + simulated)
;-----------------------------------------
mark_plotting:

; Specify the channels to plot
;chPlotArray = INDGEN(24)
chPlotArray = [1]

plotRad, chPlotArray, chanNumArray, chanInfoArrray, prefix1, prefix2, $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, minBT_Values, maxBT_Values,   $
    ref_scanPos1, ref_scanLine1, ref_Lat1, ref_Lon1,      $
    ref_ModeFlag1, ref_Angle1, ref_QC1, ref_Tb1,          $
    ref_scanPos2, ref_scanLine2, ref_Lat2, ref_Lon2,      $
    ref_ModeFlag2, ref_Angle2, ref_QC2, ref_Tb2


PRINT,'End of processing...'
END

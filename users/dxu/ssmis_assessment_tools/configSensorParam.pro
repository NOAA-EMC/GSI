;---------------------------------------------------------------------------------
; Name:  configSensorParam.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for various sensors. 
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
@importConfigParam.pro

PRO configSensorParam, sensorOption, paramStruct
   ;----------------------------------------------------
   ; Create a struct to hold all the config parameters
   ; Values need to overriden for each specific sensor. 
   ;----------------------------------------------------
   MAX_CHAN_NUM = 100L    ; Max channel number
   INT_FILL_Val    = 999
   FLOAT_FILL_Val  = -999.99
   STRING_FILL_Val = ''
   paramStruct={          $
      INT_FILL    : INT_FILL_Val    ,$
      FLOAT_FILL  : FLOAT_FILL_Val  ,$
      STRING_FILL : STRING_FILL_Val ,$
      MIN_LAT : -90,      $
      MAX_LAT : 90,       $
      MIN_LON : -180,     $
      MAX_LON : 180,      $
      radListFile1 : '',  $
      radListFile2 : '',  $
      sceneListFile: '',  $
      biasListFile : '',  $
      stddevListFile : '',$
      MAX_FOV : 30000L,   $
      MAX_CHAN : MAX_CHAN_NUM , $ 
      sensorName: STRING_FILL_Val, $ 
      chanNumArray  : MAKE_ARRAY(MAX_CHAN_NUM,/INT,     $
                                 VALUE=INT_FILL_Val),   $
      chanInfoArray : MAKE_ARRAY(MAX_CHAN_NUM,/STRING,  $
                                 VALUE=STRING_FILL_Val),$
      minBT_Values  : MAKE_ARRAY(MAX_CHAN_NUM,/FLOAT,   $
                                 VALUE=FLOAT_FILL_Val), $
      maxBT_Values  : MAKE_ARRAY(MAX_CHAN_NUM,/FLOAT,   $
                                 VALUE=FLOAT_FILL_Val), $
      chPlotArray   : MAKE_ARRAY(MAX_CHAN_NUM,/INT,     $
                                 VALUE=INT_FILL_Val),   $
      CLW_THRESHOLD_MIN : 0.05,  $
      CLW_THRESHOLD_MAX : 0.3,   $
      RWP_THRESHOLD     : 0.05,  $
      GWP_THRESHOLD     : 0.05,  $
      date : '2013-01-20'}

   ;-----------------------------------------------
   ; Call specific procedure to set config params
   ;-----------------------------------------------
   CASE sensorOption OF
       ; NOAA-18/AMSUA&MHS
       1: BEGIN
             configParam_NOAA_18_AMSUA_MHS, paramStruct   
	  END
       ; NOAA-19/AMSUA&MHS
       2: BEGIN
             configParam_NOAA_19_AMSUA_MHS, paramStruct 
	  END
       ; MetOp-A/AMSUA&MHS
       3: BEGIN
             configParam_MetOp_A_AMSUA_MHS, paramStruct      
	  END
       ; MetOp-B/AMSUA/MHS 
       4: BEGIN
             configParam_MetOp_B_AMSUA_MHS , paramStruct
	  END
       ; F16/SSMIS
       5: BEGIN
             configParam_F16_SSMIS, paramStruct
	  END
       ; F17/SSMIS
       6: BEGIN
             configParam_F17_SSMIS, paramStruct
	  END
       ; F18/SSMIS
       7: BEGIN
             configParam_F18_SSMIS, paramStruct
	  END
       ; NPP/ATMS
       8: BEGIN
             configParam_NPP_ATMS, paramStruct
	  END
       ; AQUA/AMSRE
       9: BEGIN
             configParam_AQUA_AMSRE, paramStruct
	  END
      ; GCOMW1/AMSR2
      10: BEGIN
             configParam_GCOMW1_AMSR2, paramStruct
	  END
      ; FY3/MWRI
      11: BEGIN
             configParam_FY3_MWRI, paramStruct
	  END
      ; FY3/MWHS/MWTS
      12: BEGIN
             configParam_FY3_MWHS_MWTS, paramStruct
	  END
      ; TRMM/TMI
      13: BEGIN
             configParam_TRMM_TMI, paramStruct
	  END
      ; GPM/GMI
      14: BEGIN
             configParam_GPM_GMI, paramStruct
	  END
      ; MT/MADRAS
      15: BEGIN
             configParam_MT_MADRAS, paramStruct
	  END
      ; MT/SAPHIR
      16: BEGIN
             configParam_MT_SAPHIR, paramStruct
	  END
    ; WindSat
    ELSE: BEGIN
             configParam_WindSat, paramStruct
	  END
   ENDCASE
END

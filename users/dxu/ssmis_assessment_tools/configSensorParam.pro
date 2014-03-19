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
   MIN_LAT_Val = -90
   MAX_LAT_Val = 90
   MIN_LON_Val = -180
   MAX_LON_Val = 180
   MAX_FOV_NUM = 30000L  ; Max profile number
   MAX_CHAN_NUM = 40L    ; Max channel number
   paramStruct={          $
      MIN_LAT : MIN_LAT_Val, $
      MAX_LAT : MAX_LAT_Val, $
      MIN_LON : MIN_LON_Val, $
      MAX_LON : MAX_LON_Val, $
      radListFile1 : '',   $
      radListFile2 : '',   $
      MAX_FOV : MAX_FOV_NUM,    $
      MAX_CHAN : MAX_CHAN_NUM , $ 
      sceneListFile: '',   $
      chanNumArray  : MAKE_ARRAY(MAX_CHAN_NUM,/INT, VALUE=0),     $
      chanInfoArray : MAKE_ARRAY(MAX_CHAN_NUM,/STRING, VALUE=''), $
      minBT_Values  : MAKE_ARRAY(MAX_CHAN_NUM,/FLOAT, VALUE=0.0),  $
      maxBT_Values  : MAKE_ARRAY(MAX_CHAN_NUM,/FLOAT, VALUE=0.0),  $
      prefixArray: MAKE_ARRAY(MAX_CHAN_NUM,/STRING, VALUE='') }

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

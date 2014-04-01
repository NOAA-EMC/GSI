;---------------------------------------------------------------------------------
; Name:  configParam_GCOMW1_AMSR2.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for GCOMW1_AMSR2
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_GCOMW1_AMSR2, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 70000L
   paramStruct.MAX_CHAN = 14L 

   ; Radiance files (obs + sim)
   paramStruct.radListFile1 = '/data/ejones/tools/obs_assessment/obs_assessment/meas.list'
   paramStruct.radListFile2 = '/data/ejones/tools/obs_assessment/obs_assessment/fwd_fix_em.list'
   ; Scene file
   paramStruct.sceneListFile =  $
	'/data/ejones/tools/obs_assessment/obs_assessment/scene.list'
   paramStruct.chanNumArray =   $
       ['1','2','3','4','5','6','7','8','9','10',   $
       '11','12','13','14']
   paramStruct.chanInfoArray  =  $
       ['6.925','6.925','7.30','7.30','10.65','10.65','18.7','18.7','23.8','23.8','36.5','36.5','89.0','89.0']
   paramStruct.minBT_Values =  $
       [ 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40 ]
   paramStruct.maxBT_Values =  $
       [ 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350 ]
   paramStruct.sensorName = 'GCOMW1_AMSR2'
   paramStruct.chPlotArray = INDGEN(14)
   ; paramStruct.chPlotArray = [1,2]
   paramStruct.date = '2012-10-27'

END

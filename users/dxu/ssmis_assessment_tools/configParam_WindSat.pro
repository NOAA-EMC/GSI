;---------------------------------------------------------------------------------
; Name:  configParam_WindSat.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for WindSat
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_WindSat, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 30000L
   paramStruct.MAX_CHAN = 40L 

   ; Radiance files (obs + sim)
   paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
   paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
   ; Scene file
   paramStruct.sceneListFile =  $
	'/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
   paramStruct.chanNumArray =   $
       ['1','2','3','4','5','6','7','8','9','10',   $
       '11','12','13','14','15','16','17','18','19','20',      $
       '21','22', '23', '24']
   paramStruct.chanInfoArray  =  $
       ['50.300', '52.800', '53.596', '54.400', '55.500', '57.290',$
       '59.400', '150.000', '183.310', '183.310', '183.310', '19.350', $
       '19.350', '22.235', '37.000', '37.000', '91.655', '91.655',     $
       '63.283', '60.793', '60.793', '60.793', '60.793', '60.793' ]
   paramStruct.minBT_Values =  $
       [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
       190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
       170, 170, 170, 170 ]
   paramStruct.maxBT_Values =  $
       [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
       250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
       300, 280, 280, 280 ]
   paramStruct.sensorName = 'WindSat'
   paramStruct.chPlotArray = INDGEN(24)
   paramStruct.date = '2013-01-20'

END

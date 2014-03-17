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
PRO configSensorParam, sensorOption, paramStruct
   ;-----------------------------------
   ; 1. Shared config params
   ;-----------------------------------
   MIN_LAT_Val = -90
   MAX_LAT_Val = 90
   MIN_LON_Val = -180
   MAX_LON_Val = 180

   ; Create a struct to hold all the config parameters
   ; Values need to overriden for each specific sensor. 
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

   ;-----------------------------------
   ; 2. Sensor specifc config params
   ;-----------------------------------
   CASE sensorOption OF
       ; NOAA-18/AMSUA&MHS
       1: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
       ; NOAA-19/AMSUA&MHS
       2: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
       ; MetOp-A/AMSUA&MHS
       3: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
       ; MetOp-B/AMSUA/MHS 
       4: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
       ; F16/SSMIS
       5: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
       ; F17/SSMIS
       6: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
       ; F18/SSMIS
       7: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
       ; NPP/ATMS
       8: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
       ; AQUA/AMSRE
       9: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
      ; GCOMW1/AMSR2
      10: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
      ; FY3/MWRI
      11: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
      ; FY3/MWHS/MWTS
      12: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
      ; TRMM/TMI
      13: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
      ; GPM/GMI
      14: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
      ; MT/MADRAS
      15: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
      ; MT/SAPHIR
      16: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
    ; WindSat
    ELSE: BEGIN
	    ; Radiance files (obs + sim)
	    paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
	    paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    paramStruct.MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    paramStruct.MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
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
            paramStruct.prefixArray = [ 'SSMIS_Rad_plotting_', 'SSMIS_RadDiff_plotting_']
	  END
   ENDCASE
END

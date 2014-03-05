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
PRO configSensorParam, sensorOption
   ;-----------------------------------
   ; 1. Shared config params
   ;-----------------------------------
   MIN_LAT = -90
   MAX_LAT = 90
   MIN_LON = -180
   MAX_LON = 180

   ;-----------------------------------
   ; 2. Sensor specifc config params
   ;-----------------------------------
   CASE sensorOption OF
       1: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
       2: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
       3: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
       4: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
       5: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
       6: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
       7: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
       8: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
       9: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
      10: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
      11: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
      12: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
      13: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
      14: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
      15: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
      16: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
    ELSE: BEGIN
	    ; Radiance files (obs + sim)
	    radListFile1 = '/data/home001/dxu/graphic/meas.list'
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
	    ;  Allocation of memory for variables
	    MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
	    MAX_CHAN = 24L     ; max number of channels
	    ; Scene files for surface type
	    sceneListFile =  $
		 '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'
            chanNumArray = ['1','2','3','4','5','6','7','8','9','10',   $
                '11','12','13','14','15','16','17','18','19','20',      $
                '21','22', '23', '24']
            chanInfoArrray  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29',     $
                  '59.4', '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242', '60.792668', $ 
                  '60.792668', '60.792668', '60.792668', '60.792668' ]
            minBT_Values = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
                    170, 170, 170, 170 ]
            maxBT_Values = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
                    300, 280, 280, 280 ]
            prefix1 = 'SSMIS_Obs_plotting_'
            prefix2 = 'SSMIS_Sim_plotting_'
	  END
   ENDCASE



END

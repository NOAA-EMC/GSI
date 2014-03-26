;---------------------------------------------------------------------------------
; Name:  initializeData.pro
;
; Type:  IDL Program
;
; Description:
;   To initialize data structure with FILL values
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 26, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO initializeRadDataType, MAX_FOV, nOrbits, MAX_CHAN, radData
   INT_FILL_VALUE = -999
   LONG_FILL_VALUE = -999L
   FLOAT_FILL_VALUE = -999.99

   radData.nFOV_Rad1 = MAKE_ARRAY(nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   radData.scanPosRad1 = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   radData.scanLineRad1 = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   radData.latRad1      = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.lonRad1   = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.dirRad1   = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.angleRad1 = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.QC_Rad1   = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   radData.tbRad1    = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_CHAN, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.nFOV_Rad2 = MAKE_ARRAY(nOrbits, /LONG , VALUE = LONG_FILL_VALUE)
   radData.scanPosRad2  = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   radData.scanLineRad2 = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   radData.latRad2      = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.lonRad2      = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.dirRad2      = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.angleRad2    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.QC_Rad2      = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   radData.tbRad2       = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_CHAN, $
                             /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.nChan        = LONG_FILL_VALUE
END

PRO initializeSceneDataType, MAX_FOV, nOrbits,  sceneData
   LONG_FILL_VALUE = -999L
   FLOAT_FILL_VALUE = -999.99

   sceneData.tpwVec    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.clwVec    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE) 
   sceneData.rwpVec    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.gwpVec    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.tSkinVec  = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.sfcTypVec = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
END

PRO initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, refRadData
   INT_FILL_VALUE = -999
   LONG_FILL_VALUE = -999L
   FLOAT_FILL_VALUE = -999.99

   refRadData.ref_scanPos1  = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   refRadData.ref_scanPos2  = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   refRadData.ref_scanLine1 = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   refRadData.ref_scanLine2 = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   refRadData.ref_Lat1      = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.ref_Lat2      = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.ref_Lon1      = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.ref_Lon2      = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.ref_ModeFlag1 = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   refRadData.ref_ModeFlag2 = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   refRadData.ref_Angle1    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.ref_Angle2    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.ref_QC1       = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   refRadData.ref_QC2       = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   refRadData.ref_Tb1       = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_CHAN, $
                             /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.ref_Tb2       = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_CHAN, $
                             /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.ref_TbDiff    = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_CHAN, $
                             /FLOAT, VALUE = FLOAT_FILL_VALUE)

END


PRO initializeRefSceneDataType, MAX_FOV, nOrbits,  refSceneData
   LONG_FILL_VALUE = -999L
   FLOAT_FILL_VALUE = -999.99

   refSceneData.ref_TPW_Vec = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refSceneData.ref_CLW_Vec = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE) 
   refSceneData.ref_RWP_Vec = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refSceneData.ref_GWP_Vec = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refSceneData.ref_tSkin_Vec = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refSceneData.ref_SfcType_Vec = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
END


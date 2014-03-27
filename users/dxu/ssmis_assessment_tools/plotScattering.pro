;---------------------------------------------------------------------------------
; Name:  plotScatter.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to plot scatter among parameters 
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 21, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO plotScattering, chPlotArray, chanNumArray, chanInfoArray, prefix,       $
    MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
    refRadObs, refRadSim, refSceneData, date

   ; Save graphics in PS
   SET_PLOT, 'PS'

   ; Get num of channels to plot
   numOfChans = N_ELEMENTS(chPlotArray)

   ; Loop thru. channels to plot
   FOR iChan=0, numOfChans - 1 DO BEGIN
      filter3 = WHERE(refRadObs.lat ge MIN_LAT               $
                and refRadObs.lat le MAX_LAT                $
                and refRadObs.tb(*,chPlotArray(iChan)) gt 0 $
                and refRadSim.lat ge MIN_LAT                $
                and refRadSim.lat le MAX_LAT                $
                and refRadSim.tb(*,chPlotArray(iChan)) gt 0 )


      ; chan_str
      chanNo = STRING(chanNumArray(iChan))

      imageNameTPW = STRCOMPRESS(prefix + 'TPW_' + chanNo + '.ps',/remove_all)
      titleTPW = STRCOMPRESS('SSMIS TB diff vs TPW (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameTPW, titleTPW, $
         refSceneData.tpwVec,  $
         refRadObs.tbDiff(filter3, iChan)

      imageNameCLW = STRCOMPRESS(prefix + 'CLW_' + chanNo + '.ps',/remove_all)
      titleCLW = STRCOMPRESS('SSMIS TB diff vs CLW (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameCLW, titleCLW, $
         refSceneData.clwVec,  $
         refRadObs.tbDiff(filter3, iChan)

      imageNameRWP = STRCOMPRESS(prefix + 'RWP_' + chanNo + '.ps',/remove_all)
      titleRWP = STRCOMPRESS('SSMIS TB diff vs RWP (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameRWP, titleRWP, $
         refSceneData.rwpVec,  $
         refRadObs.tbDiff(filter3, iChan)

      imageNameGWP = STRCOMPRESS(prefix + 'GWP_' + chanNo + '.ps',/remove_all)
      titleGWP = STRCOMPRESS('SSMIS TB diff vs GWP (chan ' + chanNo + ': '  $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameGWP, titleGWP, $
         refSceneData.gwpVec,  $
         refRadObs.tbDiff(filter3, iChan)

      imageNameSkinT = STRCOMPRESS(prefix + 'SkinT_' + chanNo + '.ps',/remove_all)
      titleSkinT = STRCOMPRESS('SSMIS TB diff vs SkinT (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameSkinT, titleSkinT,   $
         refSceneData.tSkinVec,      $
         refRadObs.tbDiff(filter3, iChan)

      imageNameSfcType = STRCOMPRESS(prefix + 'SfcType_' + chanNo + '.ps',/remove_all)
      titleSfcType = STRCOMPRESS('SSMIS TB diff vs SfcType (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameSfcType, titleSfcType,   $
         refSceneData.sfcTypeVec,      $
         refRadObs.tbDiff(filter3, iChan)

   ENDFOR

END


PRO doScatterPlotting, imageName, titleName, dataX, dataY
   LOADCT, 39
   ERASE
   !P.FONT =0
   !P.MULTI = [0, 1, 1]
   aspect_ratio = 1.5 ;rectangle
   xSizeVal = 20
   ySizeVal = xSizeVal / aspect_ratio
   DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
	   XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=2,  $
	   /PORTRAIT, FONT_SIZE=11, /BOLD, /COURIER

   minValue = min(dataY)
   maxValue = max(dataY)
   PLOT, dataX, dataY, PSYM=1, TITLE=titleName, MIN_VALUE = minValue, MAX_VALUE = maxValue
   DEVICE, /CLOSE
END

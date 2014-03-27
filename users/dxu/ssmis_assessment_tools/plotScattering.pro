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
      ; Sfc type 0
      f1= WHERE(refRadObs.lat ge MIN_LAT               $
                and refRadObs.lat le MAX_LAT                $
                and refRadObs.tb(*,chPlotArray(iChan)) gt 0 $
                and refRadSim.lat ge MIN_LAT                $
                and refRadSim.lat le MAX_LAT                $
                and refRadSim.tb(*,chPlotArray(iChan)) gt 0 $
                and refSceneData.SfcTypeVec eq 0 )
      ; Sfc type 1
      f2= WHERE(refRadObs.lat ge MIN_LAT               $
                and refRadObs.lat le MAX_LAT                $
                and refRadObs.tb(*,chPlotArray(iChan)) gt 0 $
                and refRadSim.lat ge MIN_LAT                $
                and refRadSim.lat le MAX_LAT                $
                and refRadSim.tb(*,chPlotArray(iChan)) gt 0 $
                and refSceneData.SfcTypeVec eq 1 )
      ; Sfc type 2
      f3= WHERE(refRadObs.lat ge MIN_LAT               $
                and refRadObs.lat le MAX_LAT                $
                and refRadObs.tb(*,chPlotArray(iChan)) gt 0 $
                and refRadSim.lat ge MIN_LAT                $
                and refRadSim.lat le MAX_LAT                $
                and refRadSim.tb(*,chPlotArray(iChan)) gt 0 $
                and refSceneData.SfcTypeVec eq 2 )
      ; Sfc type 3
      f4= WHERE(refRadObs.lat ge MIN_LAT               $
                and refRadObs.lat le MAX_LAT                $
                and refRadObs.tb(*,chPlotArray(iChan)) gt 0 $
                and refRadSim.lat ge MIN_LAT                $
                and refRadSim.lat le MAX_LAT                $
                and refRadSim.tb(*,chPlotArray(iChan)) gt 0 $
                and refSceneData.SfcTypeVec eq 3 )


      ; chan_str
      chanNo = STRING(chanNumArray(iChan))
     
      ; tbDiff vs TPW
      imageNameTPW = STRCOMPRESS(prefix + 'TPW_' + chanNo + '.ps',/remove_all)
      titleTPW = STRCOMPRESS('SSMIS TB diff vs TPW (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameTPW, titleTPW, $
         'TPW (mm)', 'TB Diff (K)', $
         refSceneData.tpwVec,  $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan

      ; tbDiff vs CLW
      imageNameCLW = STRCOMPRESS(prefix + 'CLW_' + chanNo + '.ps',/remove_all)
      titleCLW = STRCOMPRESS('SSMIS TB diff vs CLW (chan ' + chanNo + ': ' $
                              + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameCLW, titleCLW, $
         'CLW (mm)', 'TB Diff (K)', $
         refSceneData.clwVec,  $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan
 
      ; tbDiff vs RWP
      imageNameRWP = STRCOMPRESS(prefix + 'RWP_' + chanNo + '.ps',/remove_all)
      titleRWP = STRCOMPRESS('SSMIS TB diff vs RWP (chan ' + chanNo + ': ' $
                              + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameRWP, titleRWP, $
         'RWP (mm)', 'TB Diff (K)', $
         refSceneData.rwpVec,  $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan
 
      ; tbDiff vs GWP
      imageNameGWP = STRCOMPRESS(prefix + 'GWP_' + chanNo + '.ps',/remove_all)
      titleGWP = STRCOMPRESS('SSMIS TB diff vs GWP (chan ' + chanNo + ': '  $
                              + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameGWP, titleGWP, $
         'GWP (mm)', 'TB Diff (K)', $
         refSceneData.gwpVec,  $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan
 
      ; tbDiff vs SkinT
      imageNameSkinT = STRCOMPRESS(prefix + 'SkinT_' + chanNo + '.ps',/remove_all)
      titleSkinT = STRCOMPRESS('SSMIS TB diff vs SkinT (chan ' + chanNo + ': ' $
                              + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameSkinT, titleSkinT,   $
         'SkinT (mm)', 'TB Diff (K)', $
         refSceneData.tSkinVec,      $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan
 
      ; tbDiff vs SfcType
      imageNameSfcType = STRCOMPRESS(prefix + 'SfcType_' + chanNo + '.ps',/remove_all)
      titleSfcType = STRCOMPRESS('SSMIS TB diff vs SfcType (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameSfcType, titleSfcType,  $
         'SfcType (mm)', 'TB Diff (K)', $
         refSceneData.sfcTypeVec,      $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan
      
      ; tbDiff vs scanPos
      imageNameScanPos = STRCOMPRESS(prefix + 'ScanPos_' + chanNo + '.ps',/remove_all)
      titleScanPos = STRCOMPRESS('SSMIS TB diff vs scanPos (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameScanPos, titleScanPos,  $
         'scanPos ', 'TB Diff (K)', $
         refRadObs.scanPos,   $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan
      
      ; tbDiff vs angle
      imageNameAngle = STRCOMPRESS(prefix + 'Angle_' + chanNo + '.ps',/remove_all)
      titleScanPos = STRCOMPRESS('SSMIS TB diff vs angle (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameAngle, titleAngle,  $
         'angle (deg) ', 'TB Diff (K)', $
         refRadObs.angle,   $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan
      
      ; tbDiff vs lat
      imageNameLat = STRCOMPRESS(prefix + 'Lat_' + chanNo + '.ps',/remove_all)
      titleLat = STRCOMPRESS('SSMIS TB diff vs Lat (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameLat, titleLat,  $
         'Lat (deg)', 'TB Diff (K)', $
         refRadObs.lat,   $
         refRadObs.tbDiff, f1, f2, f3, f4, iChan
      
      ; radObs vs radSim
      imageNameRad = STRCOMPRESS(prefix + 'Rad_' + chanNo + '.ps',/remove_all)
      titleRad = STRCOMPRESS('SSMIS radObs vs radSim (chan ' + chanNo + ': ' $
                             + chanInfoArray(iChan) + ") " + date )
      doScatterPlotting, imageNameRad, titleRad,  $
         'TB Sim (K)', 'TB Obs (K)', $
         refRadSim.tb,   $
         refRadObs.tb, f1, f2, f3, f4, iChan
      

   ENDFOR

END

PRO doScatterPlotting, imageName, titleName, titleX, titleY, $
   dataX, dataY, f1, f2, f3, f4, iChan

   LOADCT, 39
   ERASE
   !P.FONT =0
   !P.MULTI = [0, 2, 2]
   aspect_ratio = 0.8 ;rectangle
   xSizeVal = 35
   ySizeVal = xSizeVal * aspect_ratio
   xPosRatio = 0.8  ; 20% 
   yPosRatio = 0.05  ; 10% 
   DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
	   XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=2,  $
	   /PORTRAIT, FONT_SIZE=11, /BOLD, /COURIER

   minX = min(dataX(f1))
   maxX = MAX(dataX(f1))
   minY = MIN(dataY(f1,iChan))
   maxY = MAX(dataY(f1,iChan))
   posX = maxX - xPosRatio * ( maxX - minX )
   posY = maxY - yPosRatio * ( maxY - minY )
   PLOT, dataX(f1), dataY(f1,iChan), PSYM=1, TITLE=titleName, $
      XTITLE = titleX,  YTITLE = titleY, $ 
      MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
   XYOUTS, posX, posY, ' Sfc Type 0'

   minX = min(dataX(f2))
   maxX = MAX(dataX(f2))
   minY = MIN(dataY(f2,iChan))
   maxY = MAX(dataY(f2,iChan))
   posX = maxX - xPosRatio * ( maxX - minX )
   posY = maxY - yPosRatio * ( maxY - minY )
   PLOT, dataX(f2), dataY(f2,iChan), PSYM=1, TITLE=titleName, $
      XTITLE = titleX,  YTITLE = titleY, $ 
      MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
   XYOUTS, posX, posY, ' Sfc Type 1'

   minX = min(dataX(f3))
   maxX = MAX(dataX(f3))
   minY = MIN(dataY(f3,iChan))
   maxY = MAX(dataY(f3,iChan))
   posX = maxX - xPosRatio * ( maxX - minX )
   posY = maxY - yPosRatio * ( maxY - minY )
   PLOT, dataX(f3), dataY(f3,iChan), PSYM=1, TITLE=titleName, $
      XTITLE = titleX,  YTITLE = titleY, $ 
      MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
   XYOUTS, posX, posY, ' Sfc Type 2'

   minX = min(dataX(f4))
   maxX = MAX(dataX(f4))
   minY = MIN(dataY(f4,iChan))
   maxY = MAX(dataY(f4,iChan))
   posX = maxX - xPosRatio * ( maxX - minX )
   posY = maxY - yPosRatio * ( maxY - minY )
   PLOT, dataX(f4), dataY(f4,iChan), PSYM=1, TITLE=titleName, $
      XTITLE = titleX,  YTITLE = titleY, $ 
      MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
   XYOUTS, posX, posY, ' Sfc Type 3'

   DEVICE, /CLOSE
END


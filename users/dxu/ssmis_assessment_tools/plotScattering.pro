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
    refRadData, refSceneData, date

   ; Save graphics in PS
   SET_PLOT, 'PS'

   ; Get num of channels to plot
   numOfChans = N_ELEMENTS(chPlotArray)

   ; Loop thru. channels to plot
   FOR iChan=0, numOfChans - 1 DO BEGIN
      ; chan_str
      chanNo = STRING(chanNumArray(iChan))

      imageNameTPW = STRCOMPRESS(prefix + 'TPW_' + chanNo+ '.ps',/remove_all)
      titleTPW = STRCOMPRESS('SSMIS TB diff vs TPW: chan ' + chanNo + '  ' + date )
      ;doScatterPlotting, imageNameTPW, titleTPW, $
      ;   refSceneData.ref_TPW_Vec,  $
      ;   refRadData.ref_TbDiff(*, iChan)

      imageNameCLW = STRCOMPRESS(prefix + 'CLW_' + chanNo+ '.ps',/remove_all)
      titleCLW = STRCOMPRESS('SSMIS TB diff vs CLW: chan ' + chanNo + '  ' + date )
      ;doScatterPlotting, imageNameCLW, titleCLW, $
      ;   refSceneData.ref_CLW_Vec,  $
      ;   refRadData.ref_TbDiff(*, iChan)

      imageNameRWP = STRCOMPRESS(prefix + 'RWP_' + chanNo+ '.ps',/remove_all)
      titleRWP = STRCOMPRESS('SSMIS TB diff vs RWP: chan ' + chanNo + '  ' + date )
      ;doScatterPlotting, imageNameRWP, titleRWP, $
      ;   refSceneData.ref_RWP_Vec,  $
      ;   refRadData.ref_TbDiff(*, iChan)

      imageNameGWP = STRCOMPRESS(prefix + 'GWP_' + chanNo+ '.ps',/remove_all)
      titleGWP = STRCOMPRESS('SSMIS TB diff vs GWP: chan ' + chanNo + '  ' + date )
      ;doScatterPlotting, imageNameGWP, titleGWP, $
      ;   refSceneData.ref_GWP_Vec,  $
      ;   refRadData.ref_TbDiff(*, iChan)

      imageNameSkinT = STRCOMPRESS(prefix + 'SkinT_' + chanNo+ '.ps',/remove_all)
      titleSkinT = STRCOMPRESS('SSMIS TB diff vs SkinT: chan ' + chanNo + '  ' + date )
      ;doScatterPlotting, imageNameSkinT, titleSkinT,   $
      ;   refSceneData.ref_tSkin_Vec,      $
      ;   refRadData.ref_TbDiff(*, iChan)

      imageNameSfcType = STRCOMPRESS(prefix + 'SfcType_' + chanNo+ '.ps',/remove_all)
      titleSfcType = STRCOMPRESS('SSMIS TB diff vs SfcType: chan ' + chanNo + '  ' + date )
      doScatterPlotting, imageNameSfcType, titleSfcType,   $
         refSceneData.ref_SfcType_Vec,      $
         refRadData.ref_TbDiff(*, iChan)

   ENDFOR

END


PRO doScatterPlotting, imageName, titleName, dataX, dataY
   LOADCT, 39
   ERASE
   !P.FONT=-1
   xSizeVal = 20
   ySizeVal = 20
   DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
	   XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=2,  $
	   /PORTRAIT, FONT_SIZE=7, /BOLD, /COURIER

   PLOT, dataX, dataY, PSYM=1, TITLE=titleName
   DEVICE, /CLOSE
END

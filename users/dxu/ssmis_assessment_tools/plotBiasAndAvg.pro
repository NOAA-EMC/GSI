;---------------------------------------------------------------------------------
; Name:  plotBiasAndAvg.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to plot radiance bias and standard deviation. 
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Apr 3, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
@paths_idl.pro
;plotBiasAndAvg.pro


sensorName = 'SSMIS'

; Float fill value 
FLOAT_FILL_Value = -999.99

;
biasListFile = 'bias.list'
stddevListFile = 'stddev.list'

;---Read bias filenames and read the biases
readlist, biasListFile, biasFileList, nFiles
readlist, stddevListFile, stddevFileList, nFiles


; Loop thru. all the bias files 
FOR iFile=0,nFiles-1 DO BEGIN
   ; Read bias file and get info 
   Readbias, biasFileList(iFile), nChan, nPos, cFreq,  $
      outMeanbias, outSlope, outIntercept, outTB_Sim, outTB_Meas

   ; Allocate space for all the bias files
   IF (iFile EQ 0) THEN BEGIN
      bias      = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
      slope     = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
      intercept = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
      TB_Sim    = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
      TB_Meas   = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
      scanPos = INDGEN(nPos) + 1
   ENDIF

   ; Save the current data when iFile = 0
   bias(*,*,iFile)      = outMeanbias
   slope(*,*,iFile)     = outSlope
   intercept(*,*,iFile) = outIntercept
   TB_Sim(*,*,iFile)    = outTB_Sim
   TB_Meas(*,*,iFile)   = outTB_Meas

   ;---compute averages : last element (nFiles) in last dimension. 
   ; Initialize sum to 0
   bias(*,*,nFiles)      = 0
   slope(*,*,nFiles)     = 0
   intercept(*,*,nFiles) = 0
   TB_Sim(*,*,nFiles)     = 0
   TB_Meas(*,*,nFiles)    = 0

   ; Sum up 
   bias(*,*,nFiles)      = bias(*,*,nFiles) + bias(*,*,iFile) / nFiles
   slope(*,*,nFiles)     = slope(*,*,nFiles) + slope(*,*,iFile) / nFiles
   intercept(*,*,nFiles) = intercept(*,*,nFiles) + intercept(*,*,iFile) / nFiles
   TB_Sim(*,*,nFiles)    = TB_Sim(*,*,nFiles) + TB_Sim(*,*,iFile) / nFiles
   TB_Meas(*,*,nFiles)   = TB_Meas(*,*,nFiles) + TB_Meas(*,*,iFile) / nFiles

ENDFOR 

CLOSE, /ALL

; Read bias standard deviation 
readStddev_SSMIS, nChan, nFiles, stddevFileList, stddevArray

;---------------
plotting:
;---------------

; Set XSIZE and YSIZE for PS.
xSizeVal=20
ySizeVal=15

; Save graphics in PS
SET_PLOT, 'PS'

LOADCT, 39
!P.FONT = 2

prefix = sensorName + "_bias_"
;----plot biases
FOR iChan = 0, nChan - 1 DO BEGIN
   ERASE
   imageName = prefix + strtrim(string(iChan + 1), 2) + '.ps'
   DEVICE, FILENAME = imageName, /COLOR, BITS_PER_PIXEL=8,          $
       XSIZE = xSizeVal, YSIZE = ySizeVal, XOFFSET = 2, YOFFSET = 2,  $
       /PORTRAIT, FONT_SIZE = 11, /BOLD, /COURIER
   titleName = STRCOMPRESS(sensorName + 'Mean Bias : Ch ' + STRING(iChan + 1) + ' (' $
               + STRING(cFreq(iChan)) + ' GHz)')
   PLOT, indgen(nPos) + 1, bias(*, iChan, nFiles), PSYM = -1, $
      TITLE = titleName, $
      YTITLE = 'Mean bias', XTITLE='Scan Position',  $
      YRANGE = [min(bias(*,iChan,nFiles)), max(bias(*,iChan,nFiles))],   $
      XRANGE = [0, nPos + 1]
   DEVICE, /CLOSE
ENDFOR




help,  stddevArray
print,  stddevArray

imageName = sensorName + '_stddev.ps'

;----plot bias stddev
ERASE
DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
   XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=2,  $
   /PORTRAIT, FONT_SIZE=9, /BOLD, /COURIER

titleName = sensorName + ' bias standard deviation'
PLOT, indgen(nChan) + 1, stddevArray, PSYM=-1, $
     TITLE = titleName, $
     YTITLE = 'Std Dev', XTITLE=' Channel number ',  $
     YRANGE = [min(stddevArray), max(stddevArray)] , $
     XRANGE = [0, nChan + 1]

DEVICE, /CLOSE

print,'End of processing...'

END

; TODO, reading statements are based on SSMIS std deviaiton file format.
; We need to have separate routines to read stddev files for different sensors. 
PRO readStddev_SSMIS, nChan, nFiles, stddevFileList, stddevArray
   ; Define variables
   tmpLine=''
   tmpData = FINDGEN(nChan)
   stddevArray = MAKE_ARRAY(nChan, /FLOAT, VALUE=0.0)

   ; This reading format is based on SSMIS bias file format
   ; Loop thru. all the stddev files to compute average stddev
   FOR iFile = 0, nFiles-1 DO BEGIN
      ; Read the  stddev file
      OPENR, iu, stddevFileList[iFile], /GET_LUN
      READF, iu, tmpLine
      READF, iu, tmpLine
      READF, iu, FORMAT='(10f10.3/10f10.3/4f10.3)', tmpData
      READF, iu, tmpLine
      ; Read stddev values for all channels  
      READF, iu, FORMAT='(10f10.3/10f10.3/4f10.3)', tmpData
      print, tmpData
      
      ; Compute avarage stddev for all the channels 
      stddevArray(*) = stddevArray(*) + tmpData(*) / nFiles
      print, stddevArray
   ENDFOR 

   CLOSE, /ALL
END

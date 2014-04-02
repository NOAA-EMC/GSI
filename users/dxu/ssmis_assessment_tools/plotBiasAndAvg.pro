@paths_idl.pro

   CLOSE, /ALL
   ;SET_PLOT, 'X'
   ;DEVICE = 'X'
   loadct, 39
   DEVICE, DECOMPOSE=0
   ;DEVICE, TRUE=24, RETAIN=2, DECOMPOSE=0
   ;DEVICE, PSEUDO_COLOR=8, RETAIN=2
   ;DEVICE, GET_VISUAL_NAME = thisvisualClass

   ;IF (thisvisualClass NE 'pseudoColor') THEN DEVICE,DECOMPOSED=0
   CLOSE, /ALL
   ;SET_PLOT,'PS'
   SET_PLOT, 'X'
  
   ; Float fill value 
   FLOAT_FILL_Value = -999.99

   ;
   biasFileList = 'bias.list'
   stddevFileList = 'stddev.list'

   ;---Read bias filenames and read the biases
   readlist, biasFileList, biasFiles, nFiles
   print, 'nFiles   is   ', nFiles
   readlist, stddevFileList, stddevFiles, nFiles
   print, 'nFiles   is   ', nFiles

   ; Loop thru. all the bias files 
   FOR iFile=0,nFiles-1 DO BEGIN
      ; Read bias file and get info 
      Readbias, biasFiles(iFile), nChan, nPos, cFreq,  $
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


   ;---Output average bias/slope/intercept/mean tbs, etc 
;---------------
 plotting:
;---------------
   chan_lab = [' 11v',' 11h',' 19v',' 19h',' 22v',' 37v',' 37h',' 85v',' 85h']
   chan_lab = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
               '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
   linecolor=[50,250]
   nPos = 30
   !p.multi=[0,1, 2 ]

   ;----plot biases
   FOR iChan = 0, 0 DO BEGIN
      ERASE
      ; !p.multi=1
      iPos=0
      PLOT, indgen(nPos) + 1, bias(*, iChan, 0), PSYM=-1, $
	TITLE = STRCOMPRESS('CH: '+string(iChan + 1) + STRING(cFreq(iChan)) + ' GHz'), $
	YTITLE = 'Mean bias', XTITLE='',  $
	YRANGE = [min(bias(*,iChan,0)), max(bias(*,iChan,0))],   $
	XRANGE = [0, nPos + 1]

      PLOTS,[1, nPos], [bias(*,iChan,0), bias(*,iChan,0)]

   ENDFOR

   OPENR,iu,stddevFiles[0], /get_lun
   nPos=30L
   x0         = fltarr(nPos)

   READF, iu, FORMAT='(2I4)', nchan, nPos

   FMT='(i4,f8.3,' + STRTRIM(STRING(FIX(nPos)),2) + 'f7.2)'

   READF, iu, format = fmt, ichan0, cFreq0, X0

   fin:close,/all
   print,'End of processing...'
END

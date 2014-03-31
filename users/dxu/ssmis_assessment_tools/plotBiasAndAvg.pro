@paths_idl.pro

   close,/all
   ;set_PLOT,'x'
   ;device='x'
   loadct,39
   device,true=24,retain=2,decompose=0
   device,pseudo_color=8,retain=2
   device,get_visual_name=thisvisualClass
   if (thisvisualClass ne 'pseudoColor') then device,decomposed=0
   close,/all
   ;set_PLOT,'ps'
   set_PLOT,'x'
   impr=0
  
   ; Float fill value 
   FLOAT_FILL_Value = -999.99

   biasFileList = 'bias.list'
   stddevFileList = 'stddev.list'

   plot_TimeSeries = 1
   plot_byScanPos  = 1

   ;---Read bias filenames and read the biases
   readlist,biasFileList,biasFiles,nFiles
   print, 'nFiles   is   ', nFiles
   readlist,stddevFileList,stddevFiles,nFiles
   print, 'nFiles   is   ', nFiles

   FOR iFile=0,nFiles-1 DO BEGIN
      ReadBias,biasFiles(iFile),nChan,npos,cfreq,  $
	 OutMeanBias,OutSlope,OutIntercept,OutTBsim,OutTBmeas

      ; Allocate space 
      IF (iFile eq 0) THEN BEGIN
	 Bias      = MAKE_ARRAY(nPos,nChan,nFiles+1,VALUE=FLOAT_FILL_Value,/FLOAT)
	 Slope     = MAKE_ARRAY(nPos,nChan,nFiles+1,VALUE=FLOAT_FILL_Value,/FLOAT)
	 Intercept = MAKE_ARRAY(nPos,nChan,nFiles+1,VALUE=FLOAT_FILL_Value,/FLOAT)
	 TBsim     = MAKE_ARRAY(nPos,nChan,nFiles+1,VALUE=FLOAT_FILL_Value,/FLOAT)
	 TBmeas    = MAKE_ARRAY(nPos,nChan,nFiles+1,VALUE=FLOAT_FILL_Value,/FLOAT)
	 scanpos=indgen(npos)+1
     ENDIF

     ; Allocate space 
     IF (iFile eq 0) THEN BEGIN
	 Bias(*,*,iFile)      = OutMeanBias
	 Slope(*,*,iFile)     = OutSlope
	 Intercept(*,*,iFile) = OutIntercept
	 TBsim(*,*,iFile)     = OutTBsim
	 TBmeas(*,*,iFile)    = OutTBmeas
     ENDIF

     IF (iFile gt 0) THEN BEGIN
	 FOR iChan=0,nChan-1 DO BEGIN
	     FOR i=0,nPos-1 DO BEGIN
		 idx=(i*8)+4
		 Bias(i,iChan,iFile)      = OutMeanBias(i,iChan)
		 Slope(i,iChan,iFile)     = OutSlope(i,iChan)
		 Intercept(i,iChan,iFile) = OutIntercept(i,iChan)
		 TBsim(i,iChan,iFile)     = OutTBsim(i,iChan)
		 TBmeas(i,iChan,iFile)    = OutTBmeas(i,iChan)
	     ENDFOR
	 ENDFOR
      ENDIF
      ;---compute averages
      Bias(*,*,nFiles)      = 0
      Slope(*,*,nFiles)     = 0
      Intercept(*,*,nFiles) = 0
      TBsim(*,*,nFiles)     = 0
      TBmeas(*,*,nFiles)    = 0

      Bias(*,*,nFiles)      = Bias(*,*,nFiles)+Bias(*,*,iFile)/nFiles
      Slope(*,*,nFiles)     = Slope(*,*,nFiles)+Slope(*,*,iFile)/nFiles
      Intercept(*,*,nFiles) = Intercept(*,*,nFiles)+Intercept(*,*,iFile)/nFiles
      TBsim(*,*,nFiles)     = TBsim(*,*,nFiles)+TBsim(*,*,iFile)/nFiles
      TBmeas(*,*,nFiles)    = TBmeas(*,*,nFiles)+TBmeas(*,*,iFile)/nFiles
   ENDFOR


   print, Bias(*,0,0)
   print, '---------------------'
   print, Bias(*,1,0)
   print, '---------------------'
   print, Bias(*,2,0)
   print, '---------------------'

   ;---Output average bias/slope/intercept/mean tbs, etc 
plotting:
   chan_lab = [' 11v',' 11h',' 19v',' 19h',' 22v',' 37v',' 37h',' 85v',' 85h']
   chan_lab = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
               '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
   linecolor=[50,250]
   nPos = 30
   !p.multi=[0,1, 2 ]
   IF (plot_TimeSeries eq 1) THEN BEGIN
       ;----plot biases

       FOR iChan=0,0 DO BEGIN
           erase
           ;  !p.multi=1
           iPos=0
           PLOT,indgen(nPos)+1,Bias(*,iChan,0),psym=-1, $
             title=strcompress('CH: '+string(iChan+1)+string(cfreq(iChan))+' GHz'), $
             ytitle='Mean Bias',xtitle='',  $
             yrange=[min(Bias(*,iChan,0)),max(Bias(*,iChan,0))],   $
             xrange=[0,nPos + 1]

           PLOTS,[1,nPos],  $
              [Bias(*,iChan,0), $
              Bias(*,iChan,0)]

       ENDFOR
       
   ENDIF


    openr,iu,stddevFiles[0],/get_lun
    npos=30L
    x0         = fltarr(npos)

    readf,iu,format='(2i4)',nchan,npos

    fmt='(i4,f8.3,' + STRTRIM(STRING(FIX(npos)),2) + 'f7.2)'

    readf,iu,format=fmt,ichan0,cfreq0,X0



writeps:    
   ;---Use this if used as main program
   devicd = 'x'
   ;set_PLOT,'x'
   fin:close,/all
   print,'End of processing...'
END

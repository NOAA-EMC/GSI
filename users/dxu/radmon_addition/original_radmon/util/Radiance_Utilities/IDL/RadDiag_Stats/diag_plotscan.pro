;+
PRO Diag_PlotScan, RadDiag_Stats_File                     , $  ; Input file 1
                   RADDIAG_STATS2_FILE=RadDiag_Stats2_File, $  ; Input file 2 for comparison
                   CHANNEL            =Channel            , $  ; Channel to display
                   ID_TAG             =ID_Tag             , $  ; Description of file 1 data
                   ID2_TAG            =ID2_Tag            , $  ; Description of file 2 data
                   CHARSIZE           =CharSize           , $  ; Base character size
                   XMARGIN            =Xmargin            , $  ; For moving plots if required
                   YMARGIN            =Ymargin            , $  ; For moving plots if required
                   FONT               =Font               , $  ; Character font (1 for PS)
                   _EXTRA             =Extra                   ; For future use 
;-

  ; Save some stuff
  pSysVar=!P
  ySysVar=!Y

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    !P = pSysVar
    !Y = ySysVar
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF    

  ; Include parameters
  @diag_parameters


  ; Check keywords  
  IF ( N_ELEMENTS( RadDiag_Stats2_File ) EQ 0 ) THEN d2_Present=0 ELSE d2_Present=1
  IF ( N_ELEMENTS( Channel ) EQ 0 ) THEN Channel = 1L
  IF ( N_ELEMENTS( ID_Tag ) EQ 0 ) THEN ID_Tag = 'Control'
  IF ( N_ELEMENTS( ID2_Tag ) EQ 0 ) THEN ID2_Tag = 'Test'
  IF ( N_ELEMENTS( Charsize ) NE 1 ) THEN CharSize = !P.CHARSIZE
  IF ( CharSize EQ 0.0 ) THEN CharSize = 1.0
  IF ( N_ELEMENTS( Xmargin ) NE 2 ) THEN Xmargin = DEFAULT_XMARGIN
  IF ( N_ELEMENTS( Ymargin ) NE 2 ) THEN Ymargin = DEFAULT_YMARGIN

  ; Read the data
  result = Read_NetCDF(RadDiag_Stats_File, RadDiag_Stats, /GLOBAL_ATTRIBUTES, /QUIET )
  IF ( d2_Present EQ 1 ) THEN $
    result = Read_NetCDF(RadDiag_Stats2_File, RadDiag_Stats2, /GLOBAL_ATTRIBUTES, /QUIET )

  ; Set up for plotting
  nPlots = RadDiag_Stats.nVariables + 1
  nXplots = 2
  nYplots = nPlots/nXplots + ( nPlots MOD nXplots )
  IF ( nXplots*nYplots GT 4 ) THEN $
    cSize = 2.0*CharSize $
  ELSE $
    cSize = CharSize
  !P.MULTI=[0,nXplots,nYplots]
  !Y.OMARGIN = [0,7]


  ; ------------------
  ; Plot variable data
  ; ------------------

  ; Begin loop over variables
  FOR n = 0, RadDiag_Stats.nVariables - 1 DO BEGIN

    ; Get limits
    minY = MIN(RadDiag_Stats.scan_Mean[n,Channel-1,*])
    maxY = MAX(RadDiag_Stats.scan_Mean[n,Channel-1,*])
    IF ( d2_Present EQ 1 ) THEN BEGIN
      minY = minY < MIN(RadDiag_Stats2.scan_Mean[n,Channel-1,*])
      maxY = maxY > MAX(RadDiag_Stats2.scan_Mean[n,Channel-1,*])
    ENDIF

    ; Plot the data
    PLOT, RadDiag_Stats.FOV, RadDiag_Stats.scan_Mean[n,Channel-1,*], $
          YRANGE=[minY,maxY], $
          TITLE=STRTRIM(RadDiag_Stats.VariableNames[n],2), $
          XTITLE='Scan Position', $
          YTITLE='(K)', $
          XTICKLEN=1.0, $
          XGRIDSTYLE=1, $
          XMARGIN=xMargin, $
          YTICKLEN=1.0, $
          YGRIDSTYLE=1, $
          YMARGIN=yMargin, $
          PSYM = dSymbol, $
          CHARSIZE=cSize, $
          FONT=Font, $
          /NODATA

    OPLOT, RadDiag_Stats.FOV, RadDiag_Stats.Scan_Mean[n,Channel-1,*], COLOR=DCOLOR

    IF ( d2_Present EQ 1 ) THEN BEGIN
      OPLOT, RadDiag_Stats2.FOV, RadDiag_Stats2.Scan_Mean[n,Channel-1,*], $
             COLOR=D2COLOR,psym=-4
    ENDIF

    OPLOT, !X.CRANGE,[0,0]

  ENDFOR ; nVars


  ; --------------------
  ; Plot the sample data
  ; --------------------

  ; Get limits
  minY = MIN(RadDiag_Stats.scan_nSamples[Channel-1,*])
  maxY = MAX(RadDiag_Stats.scan_nSamples[Channel-1,*])
  IF ( d2_Present EQ 1 ) THEN BEGIN
    minY = minY < MIN(RadDiag_Stats2.scan_nSamples[Channel-1,*])
    maxY = maxY > MAX(RadDiag_Stats2.scan_nSamples[Channel-1,*])
  ENDIF

    ; Plot the data
  PLOT, RadDiag_Stats.FOV, RadDiag_Stats.scan_nSamples[Channel-1,*], $
        YRANGE=[minY,maxY], $
        TITLE='Number of samples', $
        XTITLE='Scan Position', $
        YTITLE='Nobs', $
        XTICKLEN=1.0, $
        XGRIDSTYLE=1, $
        XMARGIN=xMargin, $
        YTICKLEN=1.0, $
        YGRIDSTYLE=1, $
        YMARGIN=yMargin, $
        CHARSIZE=cSize, $
        FONT=Font, $
        /NODATA

  OPLOT, RadDiag_Stats.FOV, RadDiag_Stats.scan_nSamples[Channel-1,*], COLOR=DCOLOR

  IF ( d2_Present EQ 1 ) THEN BEGIN
    OPLOT, RadDiag_Stats2.FOV, RadDiag_Stats2.scan_nSamples[Channel-1,*], $
           COLOR=D2COLOR
  ENDIF

  ; Plot the difference if possible
  IF ( d2_Present EQ 1 ) THEN BEGIN
    dSamples=RadDiag_Stats.scan_nSamples[Channel-1,*] - RadDiag_Stats2.scan_nSamples[Channel-1,*]
    minY = MIN(dSamples,MAX=maxY)

    PLOT, RadDiag_Stats.FOV, dSamples, $
          YRANGE=[minY,maxY], $
          TITLE=STRTRIM(ID_Tag,2)+'-'+STRTRIM(ID2_Tag,2)+' sample difference', $
          XTITLE='Scan Position', $
          YTITLE='d(Nobs)', $
          XTICKLEN=1.0, $
          XGRIDSTYLE=1, $
          XMARGIN=xMargin, $
          YTICKLEN=1.0, $
          YGRIDSTYLE=1, $
          YMARGIN=yMargin, $
          CHARSIZE=cSize, $
          FONT=Font, $
          /NODATA
    OPLOT, RadDiag_Stats.FOV, dSamples, COLOR=ECOLOR
    OPLOT, !X.CRANGE,[0,0]
  ENDIF


  ; ------------
  ; Plot a title
  ; ------------

  yLegPos = 0.98

  XYOUTS, 0.5, yLegPos, $
          RadDiag_Stats.Platform_Name + ' ' + RadDiag_Stats.Sensor_Name + $
          ' channel ' + STRTRIM(Channel,2) + $
          ' RadStat scan angle averages!C' + $
          STRTRIM( MIN(RadDiag_Stats.DateTime), 2 ) + ' to ' + $
          STRTRIM( MAX(RadDiag_Stats.DateTime), 2 ), $
          /NORM, $
          ALIGNMENT=0.5, $
          FONT = Font, $
          CHARSIZE = 1.5

  IF ( d2_Present EQ 0 ) THEN BEGIN
    xLegPos = 0.5
    Align   = 0.5
  ENDIF ELSE BEGIN
    xLegPos = 0.48
    Align   = 1.0
  ENDELSE

  XYOUTS, xLegPos, yLegPos, $
          '!C!C'+STRTRIM(ID_Tag,2), $
          /NORM, $
          ALIGNMENT=Align, $
          FONT=Font, $
          CHARSIZE=1.5, $
          COLOR=DCOLOR

  IF ( d2_Present EQ 1 ) THEN $
    XYOUTS, 0.52, yLegPos, $
            '!C!C'+STRTRIM(ID2_Tag,2), $
            /NORM, $
            ALIGNMENT=0.0, $
            FONT=Font, $
            CHARSIZE=1.5, $
            COLOR=D2COLOR


  ; ----
  ; Done
  ; ----

  !P = pSysVar
  !Y = ySysVar
  CATCH, /CANCEL

END ; PRO Diag_PlotScan
                 

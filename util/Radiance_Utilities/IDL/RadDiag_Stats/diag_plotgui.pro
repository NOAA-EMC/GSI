; ------------------------------
; Routine to free the Info state
; ------------------------------
PRO Diag_Plot_FreeState, ID
  ; Get pointer
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN RETURN
  ; Debug output
  IF ( (*InfoPtr).Debug EQ 1 ) THEN BEGIN
    PRINT, 'Diag_Plot_FreeState'
    HELP, *InfoPtr, /STRUCTURE
  ENDIF
  ; Free state information pointer
  PTR_FREE, InfoPtr
END ; PRO Diag_Plot_FreeState


; -----------------------------
; Routine to get the Info state
; -----------------------------
PRO Diag_Plot_GetState, ID, Info, No_Copy = No_Copy
  ; Get pointer
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  ; Check pointer
  IF ( PTR_VALID( InfoPtr )   EQ 0 ) THEN MESSAGE, 'State Information pointer is invalid'
  IF ( N_ELEMENTS( *InfoPtr ) EQ 0 ) THEN MESSAGE, 'State information structure is undefined'
  ; Get state information structure
  IF ( KEYWORD_SET( No_Copy ) ) THEN BEGIN
    Info = TEMPORARY( *InfoPtr )
  ENDIF ELSE BEGIN
    Info = *InfoPtr
  ENDELSE
  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN BEGIN
    PRINT, 'Diag_Plot_GetState'
    HELP, Info, /STRUCTURE
  ENDIF
END ; PRO Diag_Plot_GetState


; -----------------------------
; Routine to set the Info state
; -----------------------------
PRO Diag_Plot_SetState, ID, Info, No_Copy = No_Copy
  ; Get pointer
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  ; Check pointer and input
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN MESSAGE, 'State information pointer is invalid'
  IF ( N_ELEMENTS( Info )   EQ 0 ) THEN MESSAGE, 'State information structure is undefined'
  ; Set state information structure
  IF ( KEYWORD_SET( No_Copy ) ) THEN BEGIN
    *InfoPtr = TEMPORARY( Info )
  ENDIF ELSE BEGIN
    *InfoPtr = Info
  ENDELSE
  ; Debug output
  IF ( (*InfoPtr).Debug EQ 1 ) THEN BEGIN
    PRINT, 'Diag_Plot_SetState'
    HELP, *InfoPtr, /STRUCTURE
  ENDIF
END ; PRO Diag_Plot_SetState


; -------------------
; Routine to clean up
; -------------------
PRO Diag_Plot_GUICleanup, ID
  ; Get top level base info state
  Diag_Plot_GetState, ID, Info
  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN BEGIN
    PRINT, 'Diag_Plot_GUICleanup'
    HELP, Info, /STRUCTURE
    IF ( WIDGET_INFO(Info.ptBaseId, /VALID_ID) EQ 1 ) THEN $
      PRINT, '  BGroup base id still valid' $
    ELSE $
      PRINT, '  BGroup base id NOT valid'
  ENDIF    
  ; Free the top level base info state
  Diag_Plot_FreeState, ID
END ; PRO Diag_Plot_GUICleanup


; ----------------------------------------
; Routine to process file menu open events
; ----------------------------------------
PRO Diag_Plot_OpenEvent, Event
  ; Get top level base info state
  Diag_Plot_GetState, Event.Top, Info
  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'Diag_Plot_OpenEvent'
  ; Get a Diag_Plot filename
  Diag_Plot_Filename = DIALOG_PICKFILE( TITLE = 'Select a RadDiag_Stats file', $
                                        FILTER = '*RadDiag_Stats.nc', $
                                        /MUST_EXIST )
  IF ( NOT Valid_String( Diag_Plot_Filename ) ) THEN RETURN
  ; Load and display the data
  Diag_Plot_LoadFile, Diag_Plot_Filename, Event.Top
END ; PRO Diag_Plot_OpenEvent


; -----------------------------------------
; Routine to process file menu print events
; -----------------------------------------
PRO Diag_Plot_PrintEvent, Event
  ; Get top level base info state
  Diag_Plot_GetState, Event.Top, Info
  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'Diag_Plot_PrintEvent'
  ; Output to PS file
  pson, FILENAME='Diag_Plot.ps'
  DEVICE, SCALE_FACTOR=2.0
  Diag_Plot_Display, Event.Top, FONT=1, CHARSIZE=1.0
  psoff
END ; PRO Diag_Plot_PrintEvent


; ------------------------------
; Routine to process exit events
; ------------------------------
PRO Diag_Plot_ExitEvent, Event
  ; Get top level base info state
  Diag_Plot_GetState, Event.Top, Info
  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN BEGIN
    PRINT, 'Diag_Plot_ExitEvent'
    IF ( WIDGET_INFO(Info.ptBaseId, /VALID_ID) EQ 1 ) THEN $
      PRINT, '  BGroup base id still valid' $
    ELSE $
      PRINT, '  BGroup base id NOT valid'
  ENDIF    
  ; Restore the plotting parameters
  !P.MULTI = Info.PMulti
  ; Destroy the widget heirarchy
  WIDGET_CONTROL, Event.Top, /DESTROY
END ; PRO Diag_Plot_ExitEvent


; -----------------------------------
; Routine to process plot type events
; -----------------------------------
FUNCTION Diag_Plot_TypeEvent, Event
  ; Get top level base info state
  Diag_Plot_GetState, Event.Top, Info
  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'Diag_Plot_TypeEvent'
  ; Set the selected variable number index
  Info.ptIndex = Event.Value
  Diag_Plot_SetState, Event.Top, Info
  ; Display the result
  Diag_Plot_Display, Event.Top
  RETURN, 0
END ; PRO Diag_Plot_TypeEvent


; ---------------------------------
; Routine to process channel events
; ---------------------------------
PRO Diag_Plot_ChannelEvent, Event
  ; Get top level base info state
  Diag_Plot_GetState, Event.Top, Info
  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'Diag_Plot_ChannelEvent'
  ; Put the new channel value in the info structure
  WIDGET_CONTROL, Event.Id, GET_VALUE = chIndex
  Info.csIndex = chIndex
  Diag_Plot_SetState, Event.Top, Info
  ; Display the result
  Diag_Plot_Display, Event.Top
END ; PRO Diag_Plot_ChannelEvent


; -------------------------------------
; Routine to display RadDiag_Stats data
; -------------------------------------
PRO Diag_Plot_Display, ID, FONT=Font, CHARSIZE=Charsize
  ; Get top level base info state
  Diag_Plot_GetState, ID, Info
  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'Diag_Plot_Display'
  ; Plot the data
  CASE Info.ptIndex OF
    0: Diag_PlotScan, Info.cFile                      , $
                      RADDIAG_STATS2_FILE=Info.tFile  , $
                      CHANNEL            =Info.csIndex, $
                      ID_TAG             =Info.cTag   , $
                      ID2_TAG            =Info.tTag   , $
                      FONT               =Font        , $
                      CHARSIZE           =Charsize      
    1: Diag_PlotTimeseries, Info.cFile                      , $
                            RADDIAG_STATS2_FILE=Info.tFile  , $
                            CHANNEL            =Info.csIndex, $
                            ID_TAG             =Info.cTag   , $
                            ID2_TAG            =Info.tTag   , $
                            FONT               =Font        , $
                            CHARSIZE           =Charsize 
    2: Diag_PlotCoefficients, Info.cFile                      , $
                              RADDIAG_STATS2_FILE=Info.tFile  , $
                              CHANNEL            =Info.csIndex, $
                              ID_TAG             =Info.cTag   , $
                              ID2_TAG            =Info.tTag   , $
                              FONT               =Font        , $
                              CHARSIZE           =Charsize 
  ENDCASE
END ; PRO Diag_Plot_Displa


; -----------------------------------------
; Routine to load a RadDiag_Stats data file
; -----------------------------------------
PRO Diag_Plot_LoadFile, File, ID
  ; Get top level base info state
  Diag_Plot_GetState, ID, Info
  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'Diag_Plot_LoadFile'

  ; STUB
       
END ; PRO Diag_Plot_LoadFile



; ============================================
; The main routine to create the Plot_Diag GUI
; =========================================
PRO Diag_PlotGUI, ControlFile, $
                  TestFile, $
                  ControlTag=ControlTag, $
                  TestTag   =TestTag, $
                  Debug     =Debug
  @error_codes
  
  ; Check arguments
  IF ( NOT Valid_String(ControlFile) ) THEN BEGIN
    MESSAGE, 'Must specify a control file', /INFORMATIONAL
    RETURN
  END
  cFile=ControlFile
  
  IF ( NOT Valid_String(TestFile) ) THEN BEGIN
    MESSAGE, 'Must specify a test file', /INFORMATIONAL
    RETURN
  END
  tFile=TestFile

  IF ( Valid_String(ControlTag) ) THEN $
    cTag=ControlTag $
  ELSE $
    cTag='Control'

  IF ( Valid_String(TestTag) ) THEN $
    tTag=TestTag $
  ELSE $
    tTag='Test'


  IF ( NOT KEYWORD_SET( Debug ) ) THEN BEGIN
    Debug = 0 
  ENDIF ELSE BEGIN
    Debug = 1
    PRINT, 'Diag_PlotGUI'
  ENDELSE


  ; ------------------------------------------------
  ; Get the number of channels from the control file
  ; ------------------------------------------------
  result = Read_netCDF(cFile,radDiag,VARIABLE_LIST=['Channel'],/GLOBAL_ATTRIBUTES)
  IF ( result NE SUCCESS ) THEN BEGIN
    MESSAGE, 'Error inquiring control file for channel information', /INFORMATIONAL
    RETURN
  END


  ; -------------------------
  ; Create the widget display
  ; -------------------------
  NoMap = 0
  Map   = 1
  Insensitive = 0
  Sensitive   = 1

  ; Create the top level base
  tlBaseId = WIDGET_BASE( MAP  =Map, $
                          MBAR =mBarId, $
                          ROW  =1, $
                          TITLE='Diag_PlotGUI' )
  WIDGET_CONTROL, tlBaseId, UPDATE = 0

  ; Create the menu bar contents
  ;
  ; The file menu
  fMenuId  = WIDGET_BUTTON( mBarId, $
                            VALUE='File', $
                            /MENU )
  fOpenId  = WIDGET_BUTTON( fMenuId, $
                            VALUE    ='Open', $
                            EVENT_PRO='Diag_Plot_OpenEvent', $
                            UVALUE   ='Open' )
  fPrintId = WIDGET_BUTTON( fMenuId, $
                            VALUE    ='Print', $
                            EVENT_PRO='Diag_Plot_PrintEvent', $
                            UVALUE   ='Print' )
  fExitId  = WIDGET_BUTTON( fMenuId, $
                            VALUE    ='Exit', $
                            EVENT_PRO='Diag_Plot_ExitEvent', $
                            UVALUE   ='Exit', $
                            /SEPARATOR )

  ; Create the control area
  ;
  ; Create the left base container
  lBaseId = WIDGET_BASE( tlBaseId, $
                         GROUP_LEADER=tlBaseId, $
                         COLUMN      =1, $
                         FRAME       =2, $
                         MAP         =Map )
  ; Create the plot type buttons
  ptBaseId = Create_BGroup( 'Diag_Plot_TypeEvent', $
                            ['Scan','Timeseries','Coefficients'], $
                            PARENT      =lBaseId, $
                            GROUP_LEADER=tlBaseId, $
                            SENSITIVE   =Sensitive, $
                            VALUE       ='Plot type', $
                            DEBUG       =Debug )
  ; Create the channel slider
  sBaseId = WIDGET_BASE( lBaseId, $
                         GROUP_LEADER=tlBaseId, $
                         COLUMN      =1, $
                         FRAME       =2, $
                         MAP         =Map )
  csId = WIDGET_SLIDER( sBaseId, $
                        GROUP_LEADER=tlBaseId, $
                        DRAG        =1, $
                        EVENT_PRO   ='Diag_Plot_ChannelEvent', $
                        MINIMUM     =1, $
                        MAXIMUM     =radDiag.nChannels, $
                        SCROLL      =1, $
                        SENSITIVE   =Sensitive, $
                        TITLE       ='Channel index', $
                        UVALUE      ='Channel' )
 
  ; Create the display area
  ;
  ; Create the right base for drawing
  rBaseId = WIDGET_BASE( tlBaseId, $
                         GROUP_LEADER=tlBaseId, $
                         ROW         =1, $
                         MAP         =Map )
  ; Create the draw widget
  xSize =  800
  ySize = 1000
  dwId = WIDGET_DRAW( rBaseId, $
                      GROUP_LEADER=tlBaseId, $
                      XSIZE       =xSize, $
                      YSIZE       =ySIZE )

  ; Map, update and realise the widget heirarchy
  WIDGET_CONTROL, tlBaseId, MAP    =Map, $
                            REALIZE=1, $
                            UPDATE =1

  ; Obtain the draw widget window ID and set it
  WIDGET_CONTROL, dwId, GET_VALUE=winId
  WSET, winId

  ; Create and load the information structure
  Info = { Debug        : Debug                , $
           PMulti       : !P.MULTI             , $
           cFile        : cFile                , $
           tFile        : tFile                , $
           cTag         : cTag                 , $
           tTag         : tTag                 , $
           sensorName   : radDiag.Sensor_Name  , $
           platformName : radDiag.Platform_Name, $
           dwId         : dwId                 , $  ; Draw widget Id
           winId        : winId                , $  ; Draw window Ids
           ptBaseId     : ptBaseId             , $  ; Plot type base id
           ptIndex      : 0L                   , $  ; Plot type selection index
           csId         : csId                 , $  ; Channel slider id
           csIndex      : 1L                     }  ; Channel slider index
  InfoPtr = PTR_NEW( Info )
  WIDGET_CONTROL, tlBaseId, SET_UVALUE = InfoPtr

  ; Debug output
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    PRINT, 'Diag_PlotGUI'
    HELP, Info, /STRUCTURE
    Create_BGroup_GetState, Info.ptBaseId, BGroup_Info
    HELP, BGroup_Info, /STRUCTURE
  ENDIF

  ; Initial display
;  IF ( Valid_String( File ) ) THEN BEGIN
    Diag_Plot_Display, tlBaseId
;  ENDIF ELSE BEGIN
;    XYOUTS, 0.5, 0.5, $
;            'Use File->Open to select a file', $
;            /NORM, ALIGNMENT = 0.5, $
;            CHARSIZE = 2.0
;  ENDELSE


  ; Start the XManager
  XMANAGER, 'Diag_PlotGUI', tlBaseId, $
            CLEANUP = 'Diag_Plot_GUICleanup', $
            GROUP_LEADER = tlBaseId

END ; PRO Diag_PlotGUI

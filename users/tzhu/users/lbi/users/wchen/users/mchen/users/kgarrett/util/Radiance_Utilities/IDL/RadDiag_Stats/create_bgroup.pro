; ------------------------------
; Routine to free the Info state
; ------------------------------
PRO Create_BGroup_FreeState, ID
  ; Get pointer
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN RETURN
  ; Debug output
  IF ( (*InfoPtr).Debug EQ 1 ) THEN BEGIN
    PRINT, 'Create_BGroup_FreeState'
    HELP, *InfoPtr, /STRUCTURE
  ENDIF
  ; Free state information pointer
  PTR_FREE, InfoPtr
END ; PRO Create_BGroup_FreeState


; -----------------------------
; Routine to get the Info state
; -----------------------------
PRO Create_BGroup_GetState, ID, Info, No_Copy = No_Copy
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
    PRINT, 'Create_BGroup_GetState'
    HELP, Info, /STRUCTURE
  ENDIF
END ; PRO Create_BGroup_GetState


; -----------------------------
; Routine to set the Info state
; -----------------------------
PRO Create_BGroup_SetState, ID, Info, No_Copy = No_Copy
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
    PRINT, 'Create_BGroup_SetState'
    HELP, *InfoPtr, /STRUCTURE
  ENDIF
END ; PRO Create_BGroup_SetState


; -------------------
; Routine to clean up
; -------------------
PRO Create_BGroup_Cleanup, ID
  ; Get top level base info state
  Create_BGroup_GetState, ID, Info
  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN BEGIN
    PRINT, 'Create_BGroup_Cleanup'
    HELP, Info, /STRUCTURE
  ENDIF    
  ; Free the top level base info state
  Create_BGroup_FreeState, ID
END ; PRO Create_BGroup_Cleanup


; --------------------------------------
; Main function to create a button group
; --------------------------------------
FUNCTION Create_BGroup, Event_Func, $
                        Button_Name, $
                        PARENT       =Parent, $
                        GROUP_LEADER =Group_Leader, $
                        VARIABLE_NAME=Variable_Name, $
                        SENSITIVE    =Sensitive, $
                        VALUE        =Value, $
                        TITLE        =Title, $
                        DEBUG        =Debug

  ; Check input
  IF ( N_ELEMENTS( Group_Leader ) EQ 0 ) THEN Group_Leader = 0
  IF ( N_ELEMENTS( Value        ) EQ 0 ) THEN Value = 'Select'
  IF ( N_ELEMENTS( Title        ) EQ 0 ) THEN Title = 'BGroup'
  IF ( N_ELEMENTS( Debug        ) EQ 0 ) THEN Debug = 0


  ; Create a new base and label
  Column = 1
  IF ( N_ELEMENTS( Parent ) EQ 0 ) THEN BEGIN
    Base_Id = WIDGET_BASE( GROUP_LEADER  =Group_Leader, $
                           COLUMN        =Column, $
                           KILL_NOTIFY   ='Create_BGroup_Cleanup', $
                           TITLE         =Title, $
                           TLB_FRAME_ATTR=1+8 )
  ENDIF ELSE BEGIN
    Base_Id = WIDGET_BASE( Parent, $
                           GROUP_LEADER=Group_Leader, $
                           KILL_NOTIFY ='Create_BGroup_Cleanup', $
                           COLUMN      =Column, $
                           FRAME       =2 )
  ENDELSE
  Label_Id = WIDGET_LABEL( Base_Id, $
                           GROUP_LEADER=Base_Id, $, $
                           VALUE       =Value )

  ; Create the button group
  BGroup_ID = CW_BGROUP( Base_ID, $
                         Button_Name, $
                         COLUMN      =1, $
                         EVENT_FUNC  =Event_Func, $
                         EXCLUSIVE   =1, $
                         IDS         =Button_ID, $
                         MAP         =Map, $
                         NO_RELEASE  =1, $
                         RETURN_INDEX=1 )


  ; Create and store the info structure
 Info = { Debug       : Debug, $
          TLB_ID      : Group_Leader, $
          Base_ID     : Base_ID, $
          BGroup_ID   : BGroup_ID, $
          Button_Name : Button_Name, $
          Button_ID   : Button_ID }
  WIDGET_CONTROL, Base_ID, SET_UVALUE = PTR_NEW(Info)

  ; Debug output
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    PRINT, 'Create_BGroup'
    HELP, Info, /STRUCTURE
  ENDIF

  ; Realize the widget heirarchy
  WIDGET_CONTROL, Base_ID, /REALIZE, SENSITIVE = Sensitive

  ; Done
  RETURN, Base_ID

END ; FUNCTION Create_BGroup

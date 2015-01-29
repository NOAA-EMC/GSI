       SUBROUTINE PLTBARB1(IWINDOW,JIWRD,ITXWRD,LCKPRNTQ,
     1                     IMAGE,MAXIWORD,MAXJSLINE,IRET_PLB)
C      ...                                             24-OCT-1996/DSS
C      ... TO PLOT ONE WIND-BARB ITEM FROM THE LABEL-ARRAY FORMATTED
C      ...   WIND-BARB DATA.  DO NOT CALL ME FOR ANY OTHER TYPE.
C
C      ... CALLED FROM PRTITLE TO PLOT ONE GIVEN WIND-BARB LABEL-ARRAY
C      ...   ITEM; AT THE SAME PROGRAM LEVEL AS CALL TO PLTLAB1()
C      ...   WHICH PLOTS THE NOT-WIND-BARB LABEL-ARRAY ITEMS.
C
C      ... CALLS ON BARB2RAS IN ORDER TO DECODE THE WIND-BARB DEFINITION
C      ...   INTO A BIT-MAPPED SYMBOL ARRAY; 
C      ...   (WHICH CALLS ON FUNCTION IRVR());

C      ... CALLS ON SYM2IMGE() IN ORDER TO PUT THE SYMBOL INTO IMAGE;
C
C      ----------------------------------------------------------------
C      USAGE:  CALL PLTBARB1(IWINDOW,JIWRD,ITXWRD,LCKPRNTQ,
C ...        1               IMAGE,MAXIWORD,MAXJSLINE,IRET_PLB)
       INTEGER    IWINDOW(30)            !... ADJUSTMENTS
       INTEGER    JIWRD
       INTEGER    ITXWRD
       LOGICAL    LCKPRNTQ
       INTEGER    IMAGE(MAXIWORD,MAXJSLINE)
       INTEGER    IRET_PLB
C      ----------------------------------------------------------------       
C      CALL SEQ FOR CALL TO BARB2RAS:
C      ... CALL BARB2RAS(LCKPRNTQ,JIWRD,ITXWRD,JWINDRAS,IDOTDEL,JDOTDEL,
C         1              IDOT_WID,JDOT_HGT,IRET_B2R)

       INTEGER    JWINDRAS(30)
C      ...WHERE  30 WORDS PER WIND VECTOR...
       INTEGER    IDOTDEL,JDOTDEL
       INTEGER    IDOT_WID,JDOT_HGT
       INTEGER    IRET_B2R
C      ----------------------------------------------------------------
C      CALL SEQ FOR CALL TO SYM2IMGE:
C ...  ...    call SYM2IMGE(IPL,JPL,JWINDRAS,JDOT_HGT,IDOT_WID,LERASE,
C ...        1              IMAGE,MAXIWORD,MAXJSLINE,iret_s2i)
       INTEGER    IPL,JPL
       LOGICAL    LERASE
       integer    iret_s2i
 
C      ----------------------------------------------------------------
       INTEGER    MSKIII
       DATA       MSKIII     / X'00001FFF' /
       INTEGER    MSKJJJ
       DATA       MSKJJJ     / X'00007FFF' /
       INTEGER    MSKPRIOR   
       DATA       MSKPRIOR   / X'00000007' /
       integer    KVECPRI
       DATA       KVECPRI    /  3 /
    
       INTEGER    MSK16B
       DATA       MSK16B         / X'000000000000FFFF' /
       INTEGER    KSIGNBIT   
       DATA       KSIGNBIT       / X'0000000000008000' /
       INTEGER    KSIGNEXTEND   
       DATA       KSIGNEXTEND    / X'FFFFFFFFFFFF0000' /
       INTEGER    NVECS
       LOGICAL    LSRNHEMI

       INTEGER    IDOT,JDOT
       INTEGER    IVALTHIS,JVALTHIS


       SAVE
              
       IRET_PLB = 0
       LERASE = .FALSE.

       IPRIOR = IAND(MSKPRIOR,(ISHFT(JIWRD,-13)))
       IF(IPRIOR .NE. KVECPRI) THEN  		!... ERROR: NOT WNDBARB
         IRET_PLB = 1
         GO TO 999
       ENDIF
       CALL BARB2RAS(LCKPRNTQ,JIWRD,ITXWRD,JWINDRAS,IDOTDEL,JDOTDEL,
     1               IDOT_WID,JDOT_HGT,IRET_B2R)

       IF(IRET_B2R .NE. 0) THEN
          IF(LCKPRNTQ) THEN
            WRITE(6,235)IRET_B2R
  235       FORMAT(1H ,'PLTBARB1::BARB2RAS RETURN CODE=',I3)
          ENDIF
          IRET_PLB = IRET_B2R
          GO TO 999

       ELSE
C         ...( IRET_B2R .EQ. 0) ... SO,
C         ... I HAVE A GOOD BIT-MAPPED SYMBOL DEFINITION,

C         ... I ADDED A SRN HEMI IPL CORRECTION, BECAUSE
C         ... VISUAL DIFFERENCE IN PLOTTED SRN-HEMI FROM WNDBRK DATA
C
          NVECS = IAND(ITXWRD,MSK16B)
          IF((IAND(ITXWRD,KSIGNBIT)) .NE. 0) THEN
            NVECS = IOR(NVECS,KSIGNEXTEND)
          ENDIF
          IF(NVECS .LT. 0) THEN
            LSRNHEMI = .TRUE.
          ELSE
            LSRNHEMI = .FALSE.
          ENDIF

          IVALTHIS = IAND(JIWRD,MSKIII)
          IVALTHIS = IVALTHIS + IDOT_WID
          IDOT = iwindow(10) - IVALTHIS

          JVALTHIS = IAND(ISHFT(JIWRD,-17),MSKJJJ)
          JDOT = iwindow(11) + JVALTHIS

          IPL = IDOT + 1
          IF(LSRNHEMI) THEN
            IPL = IPL - 1
          ENDIF
          JPL = JDOT
      
C ...          IF(JDOTDEL .GT. 0) THEN
C ...            JPL = JDOT - JDOTDEL + 1
C ...          ELSE
C ...            JPL = JDOT + 1
C ...          ENDIF

C ...          IF(IDOTDEL .GT. O) THEN
C ...            IPL = IDOT - IDOTDEL
C ...          ELSE
C ...            IPL = IDOT
C ...          ENDIF

C         ... the following plots appear in the wrong i/j ...

          write(6,325)IPL,JPL,JIWRD
  325     format(1h ,'pltbarb1: will call sym2imge w/IPL,JPL=',
     1               I6,',',I6,
     2          /1H ,7X,'JIWRD=HEX ',Z8.8)

          call SYM2IMGE(IPL,JPL,JWINDRAS,JDOT_HGT,IDOT_WID,LERASE,
     1                  IMAGE,MAXIWORD,MAXJSLINE,IRET_S2I)
          IF(IRET_S2I .NE. 0) THEN
            WRITE(6,335)IRET_S2I
  335       FORMAT(1H ,'PLTBARB1::SYM2IMGE: RETURN CODE=',I3)
          ENDIF
       ENDIF

  999  CONTINUE
       RETURN
       END

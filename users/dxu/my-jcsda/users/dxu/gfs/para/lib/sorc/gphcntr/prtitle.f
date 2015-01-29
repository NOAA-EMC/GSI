       subroutine prtitle(luninp,LOOPRIOR,iwindow,LEBCDIC,
     1                    IMAGE,MAXIWORD,MAXJSLINE, iret_plt)
C                                                  24-OCT-1996/DSS
C      ...  Added call to PLTBARB1() to process wind-barbs
C      ...    when LOOPRIOR = 3;
C
C                                                  13-JUN-1996/DSS
C      ...  Adding strip-title mods ...
C                                                  19-Mar-1996/dss
C      ...  Renamed pltsorfe() to prtitle() since this module is
C      ...  called from CNTR() to process the text and symbols
C      ...  after the LABEL-data has been sorted.
C
C      ...  Added the logic for in-core LABEL-array case -- for which
C      ...    see additional input args in common /ALT_LBL/
C
C      ...  The LABEL-data input to this subroutine is I*8 LABEL(1024)
C      ...    i.e., the 2 I*4 data have been compressed into one I*8
C      ...    longword prior to calling me.
C
C      ...    
C                                                  31-Jan-1996/dss
C      ...  Adapting chksorfe logic for reading the sorted LABEL file 
C      ...     to add the  call pltlab1() to plot the LABEL item.
C
C                                                  18-Dec-1995/dss
C      ... to look at a LABEL-array-format file ON CRAY
C      ... after it has been SORT/MERGEd onto fort.60, or one
C      ... of the other temporary data sets;
C      ... to verify that it is in sorted order;
C      ... the sorted file contains no specially flagged records
C      ... and it should be terminated with a zero-word;
C      ... or, a physical E-O-F ????
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... like input Args is:
C      ... IWINDOW(21) -- = 0;  FLAGS THE USUAL PRTITLE() FUNCTION;
C      ...                = NON-ZERO; FLAG FOR DOING STRIP-TITLES ONLY
C                                      IN THIS CALL TO PRTITLE()
C
C      ... NRECSTART_STR IN /STITLPLT/ -- PTR TO STARTING STRIP TITLE
C                                 ITEM IN LABEL-FILE
C      ... like ouput Args is: also in /STITLPLT/:
C      ... NITMPLTED_STR -- COUNT STR_LABEL-ITEMS PLOTTED 
C      ... MXJVAL_STR    -- THE HIGHEST JVALUE OF PLOTTED STR_LABEL
C      ... MXJLABITM_STR --    AND THAT MXJ'S LABEL ITEM
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       INTEGER    MAXPRIOR
       PARAMETER (MAXPRIOR=8)

       INTEGER    LMAX
       PARAMETER (LMAX=1024)
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... The alternate source of LABEL-data is from in-core LABEL-
C      ...     array which has been moved by CNTR into this /ALT_LBL/
C      ...     common area.      

       COMMON  /ALT_LBL/ LBL_INCOREQ,LBL_EMPTYQ,LABEL_PKD(LMAX)
       LOGICAL   LBL_INCOREQ    	!... = .T. if in-core LABEL
C                  			!... = .F. if on external file

       LOGICAL   LBL_EMPTYQ   		!... = .T. if empty LABEL-data
C                  			!...         from either source

       INTEGER   LABEL_PKD    		!... LABEL-data, sorted & packed
C                                       !...     from in-core LABEL case

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C      ...   FOR STATUS OF PLOTTED STRIP-TITLES IN IMAGE_STR(I,J)  ...

       COMMON   /STITLPLT/NRECSTART_STR, NITMPLTED_STR, 
     1                    MXJVAL_STR, MXJLABITM_STR
       INTEGER           NRECSTART_STR          !... PTR IN LABEL FILE
       INTEGER           NITMPLTED_STR  	!... COUNT STRITM PLTED
       INTEGER           MXJVAL_STR		!... MAXJ STR PLTED
       INTEGER           MXJLABITM_STR   	!... AND THAT LABL ITEM

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       COMMON    / PLTSTATS / NITEMPLTED,NSYMPLT
       INTEGER                NITEMPLTED
       INTEGER                NSYMPLT(4,63)    	!... (IROTAT,ICHSET)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       INTEGER    LUNINP    		!... input file dsrn
       INTEGER    LOOPRIOR   		!... only plot this priority
       INTEGER    iwindow(30) 		!... map constants
       LOGICAL    LEBCDIC   		!... = .T. if given EBCDIC data
       INTEGER    IMAGE(MAXIWORD,MAXJSLINE)
       INTEGER    iret_plt
       
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


       INTEGER    LABEL(LMAX)   	!... a local LABEL-array
C                                       !...   for input buffer


       INTEGER    NPRIOR(MAXPRIOR)

       INTEGER    LENDWRD1_AS
       DATA       LENDWRD1_AS  / X'FFFFFFF94C454E44' /

       INTEGER    LENDWRD1_EB
       DATA       LENDWRD1_EB  / X'FFFFFFF9D3C5D5C4' /

       INTEGER    MSKRHS
       DATA       MSKRHS     / X'FFFFFFFF' /
       INTEGER    MSKIII
       DATA       MSKIII     / X'00001FFF' /
       INTEGER    MSKJJJ
       DATA       MSKJJJ     / X'00007FFF' /

       INTEGER    MSK6BITS   		!... TO MASK THE FONT INDEX
       DATA       MSK6BITS     / X'3F' /

       INTEGER    MSK3BITS   		!... TO MASK THE PRIORITY
       DATA       MSK3BITS     / X'07' /

       INTEGER     KVECPRI    		!... PRIORITY OF WIND-VECTORS
       DATA        KVECPRI      / 3 /  
C      -------------------------------------------------------------
C      ... THE INITIALIZE-DEFAULT-FONT COMMAND IS A "?" ...
C      ... LOCATED IN THE HI-ORDER BYTE OF THE 4-BYTE TEXT "WORD"
C      ... WHICH IS THE 5TH BYTE OF A LONELABEL ITEM
C 
       INTEGER    INITFONTCMD_AS
       DATA       INITFONTCMD_AS  / X'3F' /  	!... =ASCII  "?"

       INTEGER    INITFONTCMD_EB
       DATA       INITFONTCMD_EB  / X'6F' /  	!... =EBCDIC "?"

       INTEGER    LSHF_ASINIFON
       DATA       LSHF_ASINIFON   / X'3F000000' / 	!... LSHIFT '?'

       INTEGER    LSHF_EBINIFON
       DATA       LSHF_EBINIFON   / X'6F000000' /       !... LSHIFT '?'

       INTEGER    MSK5THBYT
       DATA       MSK5THBYT       / X'FF000000' /


       INTEGER    IBYTE1
       INTEGER    IBYTE5

C      -------------------------------------------------------------
       INTEGER    LONELABEL

       INTEGER    LASTJVALPLTED
       INTEGER    LASTITEMPLTED

       INTEGER    IVAL,JVAL
       INTEGER    ISAVITEM,JSAVITEM
       integer    trash
       integer    isorkey
       integer    isork_prev

       INTEGER    ITEMCOUNT
       INTEGER    NUMRECSKIP

       logical    LSTRIP_TITLEQQ

C      . . . . . . . . . . .  CALL SEQ FOR PLTLAB1   . . . . . . . .
C ...       INTEGER    iwindow    ... ALREADY DEFINED ABOVE
       INTEGER    IXDEFAULT
       INTEGER    JIWRD
       INTEGER    ITXWRD
C ...       LOGICAL    LEBCDIC   ... already defined above
       LOGICAL    LCKPRNTQ
       INTEGER    IRET_PLT1

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C ...              CALL PLTBARB1(IWINDOW,JIWRD,ITXWRD,LCKPRNTQ,
C ...     1                  IMAGE,MAXIWORD,MAXJSLINE,IRET_PLB1)
       INTEGER    IRET_PLB1
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

       INTEGER    JMAXLBLVAL
       INTEGER    NREC
C
       SAVE
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... INITIALIZE ...
       iret_plt = 0
       NREC = 0
       isorkey = 0
       isork_prev = 0
       ISAVITEM = 0
       JSAVITEM = 0

       IXDEFAULT = 0
       ITEMCOUNT = 0
       LCKPRNTQ = .FALSE.

       LSTRIP_TITLEQQ = .FALSE.
       NUMRECSKIP = 0
       IF(IWINDOW(21) .NE. 0) THEN
         LSTRIP_TITLEQQ = .TRUE.
         NUMRECSKIP = NRECSTART_STR - 1
         IF(NUMRECSKIP .LT. 0) THEN
           NUMRECSKIP = 0
         ENDIF
       ENDIF

       NITEMPLTED = 0
       LASTJVALPLTED = 0
       LASTITEMPLTED = 0

       jmaxlblval = maxjsline
       if(iwindow(11) .LT. 0) then
         jmaxlblval = jmaxlblval + iabs(iwindow(11))
       endif
C      ... DO I NEED A FURTHER TEST AGAINST MAXMAXVALUE OF STRIP TITLE?

       DO  J = 1,63    			!... MAX NO. OF CHAR SETS
         DO I = 1,4     		!... MAX NO. OF ROTATIONS
           NSYMPLT(I,J) = 0
         ENDDO
       ENDDO

       if(LBL_EMPTYQ) THEN   		!... Should not have been called
         GO TO 999
       ENDIF

       if(LBL_INCOREQ) THEN
         WRITE(6,FMT='(1H ,''PRTITLE: STARTING THE IN-CORE LABEL-'',
     1                     ''ARRAY VERSION OF 28-OCT-1996  . . . .'')')
         DO  I = 1,LMAX
           LABEL(I) = LABEL_PKD(I)
         ENDDO
         NREC = NREC + 1
         GO TO 202
C        ... HOW DO I FORCE A STOP AFTER ONE REC FOR THIS IN-CORE CASE?
C        ...    see patch after 377 ...
       ELSE
         WRITE(6,FMT='(1H ,''PRTITLE: STARTING THE EXTERNAL LABEL-'',
     1                     ''FILE VERSION OF 28-OCT-1996 . . . . . .'',
     2                /1H ,7X,''WITH SORTED DATA ON UNIT='',I4)')
     A           LUNINP

         REWIND  LUNINP

         IF(LSTRIP_TITLEQQ) THEN
C          ... TRYING TO SAVE TIME BY SKIPPING OVER THE RECORDS
C          ...   PRECEDING THE STRIP TITLES ...
C          ... but before discarding a record, be sure to search for
C          ...   the default-font setter if it has not been found yet

           IF(NUMRECSKIP .GT. 0) THEN
             DO 188  IR = 1,NUMRECSKIP
               READ(LUNINP,ERR=910,END=180) LABEL
               NREC = NREC + 1
C              ... TEST FIRST WORD FOR ANY SPECIAL RECORD HEADER ...
               IF((LABEL(1) .EQ. LENDWRD1_AS) .OR.
     1            (LABEL(1) .EQ. LENDWRD1_EB)) THEN
C                ... FOUND THE LOGICAL END-OF-FILE ...
                 WRITE(6,175)NREC,NUMRECSKIP
  175            FORMAT(1H ,'prtitle: FOUND UNEXPECTED LOGICAL END AT', 
     1                      ' RECORD NO. =',I6,
     2                 /1H ,7X,'WHILE TRYING TO SKIP',I6,' RECORDS ',
     3                      ' BEFORE STRIP TITLES')
C                ... TRY WITH NO SKIPS ...
                 REWIND LUNINP
                 NREC = 0
                 GO TO 200

               ENDIF
C              ... otherwise, this record is not the logical end-of-lab
C              ... so this is a LABEL-data record ...
C              ... If the default char-set initializer has not yet 
C              ...   been found, then I must scan each item to look for
               IF(IXDEFAULT .EQ. 0) THEN
                 do  iwd = 1,LMAX
                   LONELABEL = LABEL(IWD)
                   IBYTE5 = IAND(LONELABEL,MSK5THBYT)
                   IF((IBYTE5 .EQ. LSHF_ASINIFON) .OR.
     1                (IBYTE5 .EQ. LSHF_EBINIFON)) THEN
C                    ... I have found the default font setter cmd ...
                     IXDEFAULT = IAND(ISHFT(LONELABEL,-16),MSK6BITS)
                     GO TO 188
                   ENDIF
                 enddo
               ENDIF

               GO TO 188

  180          CONTINUE
C              ... COMES HERE ON E-O-F ...
               WRITE(6,182)NUMRECSKIP,NREC
  182          FORMAT(1H ,'PRTITLE: HIT UNEXPECTED E-O-F ON SORTED ',
     1                    'LABEL FILE WHILE SKIPPING RECORDS',
     2               /1H ,7X,'BEFORE THE STRIP TITLES ...',
     3               /1H ,7X,'NUMBER OF RECORDS REQUESTED TO SKIP=',I7,
     4               /1H ,7X,'NUMBER OF RECORDS SKIPPED BEFORE E-O-F=',
     5                    I7,
     6               /1H ,7X,'TRYING AGAIN WITH NO SKIP ...')
               REWIND LUNINP
               NREC = 0
               GO TO 200
               
  188        CONTINUE         	!... ENDDO ON RECORDS

             WRITE(6,189)NREC
  189        FORMAT(1H ,'PRTITLE: BEFORE STRIP-TITLE PASS, I HAVE ',
     1                  'SKIPPED',I5,' LABEL RECORDS')
           ENDIF
         ENDIF

         GO TO 200

       ENDIF
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... PARA 200 IS TOP OF LOOP READING THE SORTED LABEL FILE REC

  200  CONTINUE
C      ... READ ONE LABEL-ARRAY RECORD INTO INPUT-BUFFER ...
       READ(LUNINP,ERR=910,END=800)(LABEL(I),I=1,LMAX)
       NREC = NREC + 1
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  202  CONTINUE
C      ... TEST FIRST WORD FOR ANY SPECIAL RECORD HEADER ...
       IF((LABEL(1) .EQ. LENDWRD1_AS) .OR.
     1    (LABEL(1) .EQ. LENDWRD1_EB)) THEN
C        ... FOUND THE LOGICAL END-OF-FILE ...
         WRITE(6,315)NREC
  315    FORMAT(1H ,'prtitle: FOUND LOGICAL END AT RECORD NO. =',I6)
         GO TO 844 		!... TO PRINT STATS AT NORMAL END
       ENDIF
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      ... SCAN THIS DATA RECORD ...
       DO 377 IWD = 1,LMAX
         LCKPRNTQ = .FALSE.
         LONELABEL = LABEL(IWD)
         if(LONELABEL .eq. 0) then
C          ... found the zero-terminator word ...
           write(6,225)iwd,NREC
  225      format(1h ,'prtitle: found zero-terminator at word(',I5,
     1                '); in rec no.',I5)
           go to 844
         endif

C        ... otherwise, this is label-data word,
         ITEMCOUNT = ITEMCOUNT + 1

         ITXWRD = IAND(LONELABEL,MSKRHS)
         JIWRD  = IAND(ISHFT(LONELABEL,-32),MSKRHS)

         isorkey = jiwrd
         if(isork_prev .GT. isorkey) then
C           ... error. not in sorted order ...
            write(6,235)iwd,nrec
  235       format(1h ,'prtitle: ERROR. Item is not in sorted order',
     1            /1h ,7X,'at word(',I5,'); in rec no.',I5)
            iret_plt = 3
            go to 844
         else
C           ... isork_prev .LE. isorkey; so O.K.
            isork_prev = isorkey
         endif

C        ... if default-font setter has not been found yet, look for it
         IF(IXDEFAULT .EQ. 0) THEN
            IBYTE1 = ISHFT(ITXWRD,-24)
            IF((IBYTE1 .EQ. INITFONTCMD_AS) .OR.
     1         (IBYTE1 .EQ. INITFONTCMD_EB)) THEN
C              ... I have found the set command !!! ...
               IXDEFAULT = IAND(ISHFT(ITXWRD,-16),MSK6BITS)
               GO TO 377 		!... do not try to plot this cmd
            ENDIF
         ENDIF
C
         IVAL = IAND(JIWRD,MSKIII)
         JVAL = IAND(ISHFT(JIWRD,-17),MSKJJJ)
         IF(JVAL .GT. jmaxlblval) GO TO 700    	!... EXIT TOP OF WINDOW

         IF(LSTRIP_TITLEQQ) THEN
           IF(JVAL .LT. 7400) THEN
             GO TO 377   	!... SKIP THIS ITEM (NOT STRIP TITLE)
           ENDIF
         ENDIF

         IPRI = IAND(ISHFT(JIWRD,-13),MSK3BITS)
         NPRIOR(IPRI+1) = NPRIOR(IPRI+1) + 1


         if(ipri .eq. LOOPRIOR) THEN
C          . . . . . . . . . . . . . . . . . . . . . . . . .
           nitemplted = nitemplted + 1

           IF(nitemplted .LE. 5) THEN
             LCKPRNTQ = .TRUE.
           ELSE
             LCKPRNTQ = .FALSE.
           ENDIF
           IF(LOOPRIOR .EQ. KVECPRI) THEN
              CALL PLTBARB1(IWINDOW,JIWRD,ITXWRD,LCKPRNTQ,
     1                  IMAGE,MAXIWORD,MAXJSLINE,IRET_PLB1)
           ELSE

              CALL PLTLAB1(iwindow,IXDEFAULT,
     1                     JIWRD,ITXWRD,LEBCDIC,LCKPRNTQ,
     2                     IMAGE,MAXIWORD,MAXJSLINE,IRET_PLT1)
           ENDIF
           LASTJVALPLTED = JVAL
           LASTITEMPLTED = LONELABEL
C          . . . . . . . . . . . . . . . . . . . . . . . . .         
         ENDIF

  377  CONTINUE
       IF(LBL_INCOREQ) THEN
C        ... IN-CORE LABEL-DATA IS LIMITED TO ONE RECORD ONLY; FINISHED.
         GO TO 844
       ENDIF
C      ... OTHERWISE, LABEL-DATA IS OUT ON EXTERNAL SORTED FILE

       GO TO 200   		!... LOOP BACK TO RDS NEXT RECORD

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... TO CLEAN UP AT END,
  700  CONTINUE
C      ... COMES HERE IF REACHED TOP OF WINDOW AT J=MAXJSLINE ...
       iret_plt = 0     	!... 
       WRITE(6,705)jmaxlblval,NREC
  705  FORMAT(1H ,'prtitle:EXIT SCAN ON TOP-OF-WINDOW AT LIMITING ',
     1            'LABEL JVAL=',I6,
     2       /1h ,'       AFTER SCANNING NREC=',I6,' LABEL-RECORDS')
       GO TO 844

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... TO CLEAN UP AT END,
  800  CONTINUE
C      ... COMES HERE IF HIT PHYSICAL END-OF-FILE ...
       iret_plt = 2     	!... ERR:HIT E-O-F; WHICH IT SHOULD NOT
       WRITE(6,805)NREC
  805  FORMAT(1H ,'prtitle:HIT E-O-F AFTER READING NREC =',I8,
     1            ' LABEL-RECORDS')
       GO TO 844

  844  CONTINUE
C      ... COMES HERE TO PRINT STATISTICS AT END ...
C      WRITE(6,846)NREC
C 846  FORMAT(1H ,'prtitle: * * *   S U M M A R Y   * * * * * * * * *',
C    1       /1H ,'          TOTAL RECORD COUNT = ',I8)

C      WRITE(6,8491)LOOPRIOR,IXDEFAULT
C8491  FORMAT(1H ,'LOOP_PRIORITY=',I3,';  DEFAULT FONT =',I4,
C    1       /1H ,'              ROT(0)  ROT(1)  ROT(2)  ROT(3)')
       
       DO  JC = 1,63
         IORSUMP = 0
         DO  I = 1,4
           IORSUMP = IOR(IORSUMP,NSYMPLT(I,JC))
         ENDDO
C        IF(IORSUMP .NE. 0) THEN
C          WRITE(6,8495)JC,(NSYMPLT(I,JC),I=1,4)
C8495      FORMAT(1H ,'FONT=',I4,'...',4I8)
C        ENDIF
       ENDDO
       WRITE(6,8497)NITEMPLTED
 8497  FORMAT(1H ,'COUNT OF LABEL-ITEMS PASSED TO PLTLAB1() =',I8)
       
       IF(LSTRIP_TITLEQQ) THEN
         NITMPLTED_STR = NITMPLTED_STR + NITEMPLTED
         IF(MXJVAL_STR .LT. LASTJVALPLTED) THEN
            MXJVAL_STR    = LASTJVALPLTED
            MXJLABITM_STR = LASTITEMPLTED
         ENDIF
       ENDIF         

       WRITE(6,852)
  852  FORMAT(1H ,'  * * * * * * * * * * * * * * * * * * * * * * * * *')
       GO TO 999
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  910  CONTINUE
C      ... COMES HERE ON READ-PARITY ERROR ...
       iret_plt = 1
       GO TO 999
C
  999  CONTINUE
       return
       END

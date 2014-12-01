       subroutine look_lab(lunlab, lchksortqq, iret_look)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    LOOK_LAB    LOOK AT LABEL-FILE; GET STATISTICS
C   PRGMMR: KRISHNA KUMAR        ORG: W/NP12   DATE: 1999-07-01
C
C ABSTRACT: TO LOOK AT THE GIVEN LABEL-ARRAY FORMATTED FILE
C   (OR ALTERNATIVE SOURCE IS IN-CORE LABEL ARRAY)
C   AND GATHER STATISTICS, SUCH AS THE TOTAL COUNT OF ITEMS;
C   COUNT OF STRIP-TITLE ITEMS BY PRIORITY; ETC.
C
C PROGRAM HISTORY LOG:
C   96-06-11  ORIGINAL AUTHOR'S NAME: DAVID SHIMOMURA
C 1999-07-01  KRISHNA KUMAR  CONVERTED THIS CODE FROM CRAY  
C                            TO IBM RS/6000 
C
C USAGE:    CALL look_lab(lunlab, lchksortqq, iret_look)
C   INPUT ARGUMENT LIST:
C     (1.) LUNLAB     - INPUT UNIT DSRN
C     (2.) LCHKSORTQQ - WHETHER A CHECK FOR SORTED ORDER SHOULD BE 
C                           PERFORMED, OR NOT;
C     COMMON /ALT_LBL/ -- OTHER INPUT ARGS ARE FOUND IN HERE 
C
C   OUTPUT ARGUMENT LIST:    
C     (3.) IRET_LOOK - RETURN CODE
C               = 0;  NORMAL RETURN
C               = 1;  PARITY ERROR ON READING LUNLAB
C               = 2;  HIT END-OF-FILE ON LUNLAB (WHICH IT SHOULD NOT)
C               = 3;  ERROR.  DATA NOT IN SORTED ORDER 
C                       (IF TESTING IN RESPONSE TO LCHKSORTQQ)
C               = 4;  ERROR.  BAD VALUE IN LUNLAB
C
C     COMMON /STAT_LAB/  ... (STATISTICS ARE RETURNED IN HERE) ...
C       INTEGER      NITEM_TOT     !... TOTAL COUNT OF LABEL-ARRAY ITEMS  
C       INTEGER      NITEM_TITLE   !... COUNT OF STRIP-TITLE ITEMS
C       INTEGER      NPRIOR_LAB(LMTPRIOR,2)   !... BY PRIORITIES
C       integer      MAXIJ_LAB(LMTMXMN,2)     !... MAX I&J
C       integer      MINIJ_LAB(LMTMXMN,2)     !... MIN I&J
C                       where LMTMXMN = 8
C                             LMTPRIOR = 8
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C     fort.LUNLAB  -  INPUT FILE IN LABEL-ARRAY FORMAT
C                     FIXED REC SIZE = 1024 I*8 WORDS 
C                     PACKED WITH IJWORD/TXTWORD
C                                 / 32  /  32  /
C
C   OUTPUT FILES:
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: PURPOSE OF GATHERING THESE COUNTS IS TO AVOID A CALL TO
C   SUBR PRTITLE() TO DO THE STRIP TITLES IF THERE ARE NO STRIP TITLE
C   DATA ITEMS IN THE LABEL ARRAY.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY4
C
C$$$
C      ... made subr look_lab() from Program look_t55; so that
C      ...   I could call it from CNTR on a sorted LABEL-file
C      ...   as well as calling it from Program look_t55 to look at
C      ...   a Tape55.
C
C      ... to add PRIOR ...                       10-Jun-1996/dss
C                                                  4-Dec-1995/dss
C      ... to look at a LABEL-array-format Tape-55 file ON CRAY
C
C          +----------------------------------------------------+
C          |   $ assign -O -a avmerv.t55 -s unblocked fort.55   |
C          +----------------------------------------------------+
C
C      ... Luke Lin gave me a file generated on the IBM (HDS-9000)system
C      ... for me to check the format.
C      ... Especially, he wanted to know if there exists a too-large
C      ...   value for J;
C      ... So, I shall start with such a limited format-checker:
C      ... to scan the file for the max values for both i- and j-values.
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C                                                  11-Jan-1996/dss
C      ... Expand the logic to count the number of fonts requested
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C
C      ... I*4 LABEL(2,1024)    	!... = 2048 I*4 WORDS
C                                            = 1024 I*8 WORDS --CRAY
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      USAGE:  CALL look_lab(lunlab, lchksortqq, iret_look)
       INTEGER    LUNLAB
       LOGICAL    LCHKSORTQQ
       INTEGER    IRET_LOOK
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       INTEGER    MAXFONTIX     	!... ONE MORE THAN 63
       PARAMETER (MAXFONTIX=64)

       INTEGER    NWRDRECSIZ
       PARAMETER (NWRDRECSIZ=1024)

       INTEGER    LABEL(NWRDRECSIZ)

       INTEGER    NFONT(MAXFONTIX)

C      ================================================================
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
C      ================================================================

       


C      ================================================================
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       integer      lmtmxmn
       parameter   (lmtmxmn=8)

       INTEGER      LMTPRIOR
       PARAMETER   (LMTPRIOR=8)


       COMMON      /STAT_LAB/NITEM_TOT,NITEM_TITLE,NPRIOR_LAB,
     1                       MAXIJ_LAB,MINIJ_LAB

       INTEGER      NITEM_TOT     !... TOTAL COUNT OF LABEL-ARRAY ITEMS  
       INTEGER      NITEM_TITLE
       INTEGER      NPRIOR_LAB(LMTPRIOR,2)
       integer      MAXIJ_LAB(LMTMXMN,2)
       integer      MINIJ_LAB(LMTMXMN,2)
       INTEGER      IMAXVAL
       EQUIVALENCE (MAXIJ_LAB(1,1),IMAXVAL)
       INTEGER      IMAXLOC(2)
       EQUIVALENCE (MAXIJ_LAB(2,1),IMAXLOC(1))
       INTEGER      IMAXSAVITEM
       EQUIVALENCE (MAXIJ_LAB(4,1),IMAXSAVITEM)
C
       INTEGER      JMAXVAL
       EQUIVALENCE (MAXIJ_LAB(5,1),JMAXVAL)
       INTEGER      JMAXLOC(2)
       EQUIVALENCE (MAXIJ_LAB(6,1),JMAXLOC(1))
       INTEGER      JMAXSAVITEM
       EQUIVALENCE (MAXIJ_LAB(8,1),JMAXSAVITEM)
C
       INTEGER      IMAXSTRIPVAL
       EQUIVALENCE (MAXIJ_LAB(1,2),IMAXSTRIPVAL)
       INTEGER      IMAXSTRIPLOC(2)
       EQUIVALENCE (MAXIJ_LAB(2,2),IMAXSTRIPLOC(1))
       INTEGER      IMAXSTRIPITEM
       EQUIVALENCE (MAXIJ_LAB(4,2),IMAXSTRIPITEM)

       INTEGER      JMAXSTRIPVAL
       EQUIVALENCE (MAXIJ_LAB(5,2),JMAXSTRIPVAL)
       INTEGER      JMAXSTRIPLOC(2)
       EQUIVALENCE (MAXIJ_LAB(6,2),JMAXSTRIPLOC(1))
       INTEGER      JMAXSTRIPITEM
       EQUIVALENCE (MAXIJ_LAB(8,2),JMAXSTRIPITEM)
C
C      -----------------------------------------------

       INTEGER      IMINVAL
       EQUIVALENCE (MINIJ_LAB(1,1),IMINVAL)
       INTEGER      IMINLOC(2)
       EQUIVALENCE (MINIJ_LAB(2,1),IMINLOC(1))
       INTEGER      IMINSAVITEM
       EQUIVALENCE (MINIJ_LAB(4,1),IMINSAVITEM)
C
       INTEGER      JMINVAL
       EQUIVALENCE (MINIJ_LAB(5,1),JMINVAL)
       INTEGER      JMINLOC(2)
       EQUIVALENCE (MINIJ_LAB(6,1),JMINLOC(1))
       INTEGER      JMINSAVITEM
       EQUIVALENCE (MINIJ_LAB(8,1),JMINSAVITEM)
C
       INTEGER      IMINSTRIPVAL
       EQUIVALENCE (MINIJ_LAB(1,2),IMINSTRIPVAL)
       INTEGER      IMINSTRIPLOC(2)
       EQUIVALENCE (MINIJ_LAB(2,2),IMINSTRIPLOC(1))
       INTEGER      IMINSTRIPITEM
       EQUIVALENCE (MINIJ_LAB(4,2),IMINSTRIPITEM)

       INTEGER      JMINSTRIPVAL
       EQUIVALENCE (MINIJ_LAB(5,2),JMINSTRIPVAL)
       INTEGER      JMINSTRIPLOC(2)
       EQUIVALENCE (MINIJ_LAB(6,2),JMINSTRIPLOC(1))
       INTEGER      JMINSTRIPITEM
       EQUIVALENCE (MINIJ_LAB(8,2),JMINSTRIPITEM)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ================================================================

       integer    jmain_strip  		!... =1 main; =2 strip-title

       integer    nwndbar

       INTEGER    LENDWRD1_EB
       DATA       LENDWRD1_EB  / X'FFFFFFF9D3C5D5C4' /
       INTEGER    LENDWRD1_AS
       DATA       LENDWRD1_AS  / X'FFFFFFF94C454E44' /

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


       INTEGER    INITFONTCMD_EB
       DATA       INITFONTCMD_EB  / X'6F' /  	!... =EBCDIC "?"
       INTEGER    INITFONTCMD_AS
       DATA       INITFONTCMD_AS  / X'3F' /  	!... =ASCII  "?"

       INTEGER    IBYTE1
       INTEGER    IXDEFAULT

       INTEGER    IVAL
       INTEGER    JVAL



C      -----------------------------------------------
       INTEGER    IACC
       INTEGER    ITXWRD
       INTEGER    JIWRD
       INTEGER    NREC
       INTEGER    ISORKEY
       INTEGER    ISORK_PREV

       SAVE

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... INITIALIZE ...
       IRET_LOOK = 0
       NITEM_TOT = 0
       NITEM_TITLE = 0
       NREC = 0
       ISORKEY = 0
       ISORK_PREV = 0

       DO  J = 1,2
         DO  I = 1,LMTMXMN
           MAXIJ_LAB(I,J) = 0
         ENDDO
       ENDDO

       DO  J = 1,2
         DO  I = 1,LMTMXMN
           MINIJ_LAB(I,J) = 0
         ENDDO
       ENDDO

       IMINVAL = 9999
       JMINVAL = 9999
       IMINSTRIPVAL = 9999
       JMINSTRIPVAL = 9999

       IXDEFAULT = 0
       DO  I = 1,MAXFONTIX
         NFONT(I) = 0
       ENDDO

       do  j = 1,2
         DO  I = 1,LMTPRIOR
           NPRIOR_LAB(I,j) = 0
         ENDDO
       enddo

       nwndbar = 0

       if(LBL_EMPTYQ) THEN   		
         GO TO 999   		!... JUMP TO EXIT ON NO LABEL-DATA
       ENDIF

       if(LBL_INCOREQ) THEN
         WRITE(6,FMT='(1H ,''look_lab: STARTING THE IN-CORE LABEL-'',
     1                     ''ARRAY VERSION. . . . . . . . . . . .'')')
         DO  I = 1,LMAX
           LABEL(I) = LABEL_PKD(I)
         ENDDO
         NREC = NREC + 1
         GO TO 202
C        ... HOW DO I FORCE A STOP AFTER ONE REC FOR THIS IN-CORE CASE?
       ELSE
         IF((LUNLAB .LE. 0) .OR.
     1      (LUNLAB .GT. 99)) THEN
           WRITE(6,125)LUNLAB
  125      FORMAT(1H ,'LOOK_LAB: FAILED ON BAD VALUE FOR INPUT UNIT =',
     1                I7)
           IRET_LOOK = 4
           GO TO 999
         ENDIF
         WRITE(6,FMT='(1H ,''LOOK_LAB: STARTING THE EXTERNAL LABEL-'',
     1                     ''FILE VERSION. . . . . . . . . . .'',
     2                /1H ,7X,''WITH LABEL-DATA ON UNIT='',I4)')
     A           LUNLAB

         REWIND  LUNLAB
         GO TO 200

       ENDIF
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... PARA 200 IS TOP OF LOOP READING THE SORTED LABEL FILE REC
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... READ ONE LABEL-ARRAY RECORD INTO INPUT-BUFFER ...
  200  CONTINUE

       if(LBL_INCOREQ) THEN
C        ... IN THE CASE OF IN-CORE LABEL-ARRAY, IF IT COMES BACK
C        ... HERE IT HAS PROCESSED THE FIRST BIN WHICH IS ALL THERE IS
         GO TO 844   		!... JUMP TO SUMMARIZE
       ENDIF

       READ(LUNLAB,ERR=910,END=800)(LABEL(I),I=1,NWRDRECSIZ)
       NREC = NREC + 1
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... TEST FIRST WORD FOR ANY SPECIAL RECORD HEADER ...
  202  CONTINUE
       IF((LABEL(1) .EQ. LENDWRD1_AS) .OR.
     1    (LABEL(1) .EQ. LENDWRD1_EB)) THEN
C        ... FOUND THE LOGICAL END-OF-FILE ...
C        WRITE(6,315)NREC
C 315    FORMAT(1H ,'LOOK_LAB: FOUND LOGICAL END AT RECORD NO. =',I6)
         GO TO 844 		!... TO PRINT STATS AT NORMAL END
       ENDIF
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      ... SCAN THIS DATA RECORD ...
       DO 377 IWD = 1,NWRDRECSIZ
         IACC = LABEL(IWD)
         IF(IACC .EQ. 0) THEN
C          WRITE(6,317)IWD,NREC
C 317      FORMAT(1H ,'LOOK_LAB:FOUND ZERO TERMINATOR WORD AT WORD(',
C    1                I6,')',
C    1           /1H ,'           IN RECORD NO. =',I6)
           GO TO 844
         ENDIF

C        ... OTHERWISE, THIS IS A NON-ZERO LABEL-DATA ITEM ...
         NITEM_TOT = NITEM_TOT + 1        
         ITXWRD = IAND(IACC,MSKRHS)
         JIWRD  = IAND(ISHFT(IACC,-32),MSKRHS)
         ISORKEY = JIWRD
         IF(LCHKSORTQQ) THEN
           IF(ISORK_PREV .GT. ISORKEY) THEN
C            ... ERROR.  NOT IN SORTED ORDER ...
             WRITE(6,235) IWD,NREC
  235        FORMAT(1H ,'LOOK_LAB:ERROR. ITEM NOT IN SORTED ORDER',
     1             /1H ,7X,'AT WORD(',I5,'); IN RECORD NO.',I5)
             IRET_LOOK = 3
             GO TO 844
           ELSE
C            ... ISORK_PREV IS .LE. ISORKEY;  SO O.K.  ...
             ISORK_PREV = ISORKEY
           ENDIF
         ENDIF
         
         IVAL = IAND(JIWRD,MSKIII)
         JVAL = IAND(ISHFT(JIWRD,-17),MSKJJJ)
         jmain_strip = 1  		!... default in main window

         IF(JVAL .LT. 7400) THEN
           jmain_strip = 1
           if(jval .GT. jmaxval) then
             JMAXVAL = JVAL
             JMAXLOC(1) = IWD
             JMAXLOC(2) = NREC
             JMAXSAVITEM = IACC
           endif
           if(jval .LT. jMINval) then
             JMINVAL = JVAL
             JMINLOC(1) = IWD
             JMINLOC(2) = NREC
             JMINSAVITEM = IACC
           endif

           IF(IVAL .GT. IMAXVAL) THEN
             IMAXVAL = IVAL
             IMAXLOC(1) = IWD
             IMAXLOC(2) = NREC
             IMAXSAVITEM = IACC
           ENDIF
           IF(IVAL .LT. IMINVAL) THEN
             IMINVAL = IVAL
             IMINLOC(1) = IWD
             IMINLOC(2) = NREC
             IMINSAVITEM = IACC
           ENDIF
         ELSE IF(JVAL .LT. 8200) THEN
           jmain_strip = 2   		!... in strip-title region
           NITEM_TITLE = NITEM_TITLE + 1
           if(jval .GT. jmaxstripval) then
             JMAXSTRIPVAL = JVAL
             JMAXSTRIPLOC(1) = IWD
             JMAXSTRIPLOC(2) = NREC
             JMAXSTRIPITEM = IACC
           endif
           if(jval .LT. jMINSTRIPval) then
             JMINSTRIPVAL = JVAL
             JMINSTRIPLOC(1) = IWD
             JMINSTRIPLOC(2) = NREC
             JMINSTRIPITEM = IACC
           endif

           IF(IVAL .GT. IMAXSTRIPVAL) THEN
             IMAXSTRIPVAL = IVAL
             IMAXSTRIPLOC(1) = IWD
             IMAXSTRIPLOC(2) = NREC
             IMAXSTRIPITEM = IACC
           ENDIF
           IF(IVAL .LT. IMINSTRIPVAL) THEN
             IMINSTRIPVAL = IVAL
             IMINSTRIPLOC(1) = IWD
             IMINSTRIPLOC(2) = NREC
             IMINSTRIPITEM = IACC
           ENDIF

         ENDIF

         IPRI = IAND(ISHFT(JIWRD,-13),MSK3BITS)
         NPRIOR_LAB(IPRI+1,jmain_strip) = 
     1                              NPRIOR_LAB(IPRI+1,jmain_strip) + 1
         if(ipri .EQ. 3) then
C           ... this is wind-staff and wind-barb coded ...
            nwndbar = nwndbar + 1
            go to 333
         endif

         IBYTE1 = ISHFT(ITXWRD,-24)
         IF(IXDEFAULT .EQ. 0) THEN
            IF((IBYTE1 .EQ. INITFONTCMD_AS) .OR.
     1         (IBYTE1 .EQ. INITFONTCMD_EB)) THEN
               IXDEFAULT = IAND(ISHFT(ITXWRD,-16),MSK6BITS)
               GO TO 333
            endif
         ENDIF

         IF(BTEST(JIWRD,16)) THEN
C          ... THE "ARROW-UP" BIT EXISTS, SO LOOK FOR FONT SELECT
C          ... INTERPRET BYTE-1 AS FONT INDEX ...
           IXFON = IAND(IBYTE1,MSK6BITS)
           IF(IXFON .NE. 0) THEN
               NFONT(IXFON) = NFONT(IXFON) + 1
           ENDIF
         ELSE
C          ... USING DEFAULT CHAR SET ...
           NFONT(MAXFONTIX) = NFONT(MAXFONTIX) + 1
         ENDIF

  333    CONTINUE

  377  CONTINUE
       GO TO 200   		!... LOOP BACK TO RDS NEXT RECORD
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... TO CLEAN UP AT END,
  800  CONTINUE
C      ... COMES HERE IF HIT PHYSICAL END-OF-FILE ...
       IRET_LOOK = 2     		!... ERR:HIT E-O-F; WHICH IT SHOULD NOT
       WRITE(6,805)NREC
  805  FORMAT(1H ,'LOOK_T55:HIT E-O-F AFTER READING NREC =',I6,
     1            ' LABEL-RECORDS')
       GO TO 844

  844  CONTINUE
C      ... COMES HERE TO PRINT STATISTICS AT END ...
       WRITE(6,846)NREC
  846  FORMAT(1H ,'LOOK_LAB: * * *   S U M M A R Y   * * * * * * * * *',
     1       /1H ,'          TOTAL RECORD COUNT = ',I6)

       WRITE(6,8462)IMINVAL,IMINLOC(1),IMINLOC(2),IMINSAVITEM
 8462  FORMAT(1H ,'MIN I-VAL =',I7,'; AT WORD =',I6,'; REC NO.',I6,
     1       /1H ,'       LABEL-ARRAY ITEM = HEX ',Z16.16)

       WRITE(6,8464)IMAXVAL,IMAXLOC(1),IMAXLOC(2),IMAXSAVITEM
 8464  FORMAT(1H ,'MAX I-VAL =',I7,'; AT WORD =',I6,'; REC NO.',I6,
     1       /1H ,'       LABEL-ARRAY ITEM = HEX ',Z16.16)

       WRITE(6,8466)JMINVAL,JMINLOC(1),JMINLOC(2),JMINSAVITEM
 8466  FORMAT(1H ,'MIN J-VAL =',I7,'; AT WORD =',I6,'; REC NO.',I6,
     1       /1H ,'       LABEL-ARRAY ITEM = HEX ',Z16.16)

       WRITE(6,8472)JMAXVAL,JMAXLOC(1),JMAXLOC(2),JMAXSAVITEM
 8472  FORMAT(1H ,'MAX J-VAL =',I7,'; AT WORD =',I6,'; REC NO.',I6,
     1       /1H ,'       LABEL-ARRAY ITEM = HEX ',Z16.16)

C
       IF(NITEM_TITLE .GT. 0) THEN

         WRITE(6,8474)NITEM_TITLE
 8474    FORMAT(1H ,/1h ,' . . .   following is for the STRIP TITLE ',
     1                   'AREA   . . . . . .  ',
     2         /1H ,7X,'STRIP-TITLE LABEL-ARRAY-ITEM COUNT TOTAL=',I6)

         WRITE(6,8476)IMINSTRIPVAL,IMINSTRIPLOC(1),IMINSTRIPLOC(2),
     1                IMINSTRIPITEM
 8476    FORMAT(1h ,'MIN I-VAL =',I7,'; AT WORD =',I6,'; REC NO.',I6,
     2         /1H ,'       LABEL-ARRAY ITEM = HEX ',Z16.16)
         WRITE(6,8478)IMAXSTRIPVAL,IMAXSTRIPLOC(1),IMAXSTRIPLOC(2),
     1                IMAXSTRIPITEM
 8478    FORMAT(1H ,'MAX I-VAL =',I7,'; AT WORD =',I6,'; REC NO.',I6,
     2         /1H ,'       LABEL-ARRAY ITEM = HEX ',Z16.16)

         WRITE(6,8482)JMINSTRIPVAL,JMINSTRIPLOC(1),JMINSTRIPLOC(2),
     1                JMINSTRIPITEM
 8482    FORMAT(1h ,'MIN J-VAL =',I7,'; AT WORD =',I6,'; REC NO.',I6,
     2         /1H ,'       LABEL-ARRAY ITEM = HEX ',Z16.16)
         WRITE(6,8484)JMAXSTRIPVAL,JMAXSTRIPLOC(1),JMAXSTRIPLOC(2),
     1                JMAXSTRIPITEM
 8484    FORMAT(1H ,'MAX J-VAL =',I7,'; AT WORD =',I6,'; REC NO.',I6,
     2         /1H ,'       LABEL-ARRAY ITEM = HEX ',Z16.16)
       ENDIF

       WRITE(6,8489)
 8489  FORMAT(1H ,'. . . . . . . . . . . . . . . . . . . . . . . . .',
     1            ' . . . . . . . .',/1H  )

C      WRITE(6,8491)IXDEFAULT,NFONT(MAXFONTIX)
C8491  FORMAT(1H ,'DEFAULT FONT =',I4,';  COUNT OF DEFAULTS =',I8)
C      M2 = MAXFONTIX - 1
C      DO  INX = 1,M2
C        IF(NFONT(INX) .NE. 0) THEN
C          WRITE(6,8493)INX,NFONT(INX)
C8493      FORMAT(1H ,'        FONT =',I4,';              COUNT =',I8)
C        ENDIF
C      ENDDO

C      WRITE(6,8494)nwndbar
C8494  FORMAT(1H ,'              ',4X,'   COUNT OF wndbarbs =',I8)

C      do jw = 1,2
C        if(jw .EQ. 1) then
C          WRITE(6,8495)
C8495      FORMAT(1H ,/1H ,'. . . . .   COUNT OF PRIORITIES -- ',
C    1                     'MAIN PORTION . . .')
C        else
C          WRITE(6,8496)
C8496      FORMAT(1H ,/1H ,'. . . . .   COUNT OF PRIORITIES -- ',
C    1                     'STRIP-TITLES . . .')
C        endif
C        DO  IPR = 1,LMTPRIOR
C          IF(NPRIOR_LAB(IPR,jw) .NE. 0) THEN
C            IPRM1 = IPR - 1
C            WRITE(6,8497)IPRM1,NPRIOR_LAB(IPR,jw)
C8497      FORMAT(1H ,'     PRIORITY =',I3,';              COUNT =',I8)
C          ENDIF
C        ENDDO
C      enddo

C      WRITE(6,852)
C 852  FORMAT(1H ,'LOOK_LAB  * * * * * * * * * * * * * * * * * * * *')
       GO TO 999
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  910  CONTINUE
C      ... COMES HERE ON READ-PARITY ERROR ...
       IRET_LOOK = 1
       GO TO 999
C
  999  CONTINUE
       RETURN
       END

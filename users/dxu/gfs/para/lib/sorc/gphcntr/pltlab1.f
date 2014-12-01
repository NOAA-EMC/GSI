       subroutine PLTLAB1(IWINDOW,IXDEFAULT,
     1                    JIWRD,ITXWRD,LEBCDIC,LCKPRNTQ,
     2                    IMAGE,MAXIWORD,MAXJSLINE,IERROR)
C                                                 28-Mar-1996/dss
C      ... correcting error in default charset plotting logic
C
C                                                 19-Mar-1996/dss
C      ... I removed LOOPRIOR because I was already testing
C      ... for priority in higher-level PLTSORFE()
C
C                                                29-Jan-1996/dss
C      ... copied program teschix in order to adapt it to plotting
C      ... one label-array item; called from plot55e.f
C
C                                                23-Jan-1996/dss
C      ... to test the table in  BLOCK DATA  CHINDX
C      ... which is used to get a pointer to the character desired
       CHARACTER*1  C1EB2AS
       EXTERNAL     C1EB2AS    		!... C*1 FUNCTION             

       EXTERNAL  CHINDX   		!... block data for /CHINDEX/

       EXTERNAL  LOOKTLB   		!... block data for /LKTLBS/

       COMMON   /LKTLBS/ LMTSETNUM,LOOKT
       INTEGER   LMTSETNUM
       INTEGER   LOOKT(9,63)

C ...       EXTERNAL  CHINDX
       COMMON /CHINDEX/LMTNFONTS,NCDEFSPFONT,KIXALL
       INTEGER        LMTNFONTS
       INTEGER        NCDEFSPFONT(63)
       INTEGER        KIXALL(8,63)
       CHARACTER*64  CHIXALL(63)
       EQUIVALENCE   (KIXALL(1,1),CHIXALL(1))

C      . . . . .   call  sequence    . . . . . . . . . . . . . . . . .
       INTEGER  IWINDOW(30)             !... to convert I from-Fax-edge
       INTEGER  IXDEFAULT   		!... default FONT-index
       INTEGER  JIWRD    		!... right-justified 32-bits
       INTEGER  ITXWRD   		!... right-justified 32-bits
       LOGICAL  LEBCDIC   		!... =.T. if EBCDIC LABEL-item
       LOGICAL  LCKPRNTQ		!... =.T. if chk prnt desired
       INTEGER  IMAGE(MAXIWORD,MAXJSLINE)
       INTEGER  IERROR    		!... return code
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


       INTEGER      ixfound
       INTEGER      ichset

       integer      nbytsymb
       integer      lenlinpxl
       integer      lenlinbyt
       integer      kheight
       integer      iquad
       integer      newpxlwid
       integer      newpxlhgt
       integer      iret_rot

       INTEGER    MSKIII
       DATA       MSKIII     / X'00001FFF' /
       INTEGER    MSKJJJ
       DATA       MSKJJJ     / X'00007FFF' /

       INTEGER    MSK6BITS   		!... TO MASK THE FONT INDEX
       DATA       MSK6BITS     / X'3F' /

       INTEGER    MSK3BITS   		!... TO MASK THE PRIORITY
       DATA       MSK3BITS     / X'07' /

       INTEGER   KWNDFLAGSRN 		!... PRIOR=4 IS WNDFLAG SRN HEMI
       DATA      KWNDFLAGSRN     / 4 /	

       INTEGER    INITFONTCMD
       DATA       INITFONTCMD  / X'6F' /  	!... =EBCDIC "?"

       LOGICAL      LSRNHEMI
       LOGICAL      LSIDEWAYS
       INTEGER      IPRI
       INTEGER      IBYTE1
       INTEGER      IROTSPEC

       INTEGER      INTCCCC
       CHARACTER*1  CCCC(8)   		!... CRAY 8-BYTE WORD
       EQUIVALENCE (INTCCCC,CCCC(1))


       CHARACTER*1  LONECHAR
       CHARACTER*1  NULL

         SAVE

C        . . . . . .   S T A R T   . . . . . . . . . . . . . . . . .
C
         IERROR = 0
         NULL = CHAR(0)

         LSIDEWAYS = .FALSE.

         IVALSTART = IAND(JIWRD,MSKIII)
         JVALSTART = IAND(ISHFT(JIWRD,-17),MSKJJJ)

         IPRI = IAND(ISHFT(JIWRD,-13),MSK3BITS)
C        ... TEST FOR VALID RANGE OF PRIORITY ...
         IF(IPRI .EQ. KWNDFLAGSRN) THEN
           LSRNHEMI = .TRUE.
         ELSE
           LSRNHEMI = .FALSE.
         ENDIF
C
         INTCCCC = ITXWRD
         IBYTE1 = mova2i(CCCC(5))

         IF(IBYTE1 .EQ. INITFONTCMD) THEN
C           ... WHAT TO DO WITH UNEXPECTED DEFAULT-FONT INITIALIZER ?
            GO TO 333
         ENDIF

         IF(BTEST(JIWRD,16)) THEN
C          ... THE "ARROW-UP" BIT EXISTS, SO LOOK FOR FONT-ROTATION SPEC
           IROTSPEC = ISHFT(IBYTE1,-6)   	!... 2 hi-order bits 

C          ... THE "ARROW-UP" BIT EXISTS, SO LOOK FOR FONT SELECT
C          ... INTERPRET BYTE-1 AS FONT INDEX ...
           ICHSET = IAND(IBYTE1,MSK6BITS)

           IF(ICHSET .NE. 0) THEN
              IF(ICHSET .GT. LMTSETNUM) THEN
                 IERROR = 1
                 GO TO 999
              ENDIF
C             ... PLOT C2,C3,C4 FROM FONT=ICHSET; IROTSPEC...

             IHGTPXL = LOOKT(3,ICHSET)
             IF(IHGTPXL .LT. 0) THEN
               LSIDEWAYS = .TRUE.
               IHGTPXL = IABS(IHGTPXL)
             ELSE
               LSIDEWAYS = .FALSE.
             ENDIF
             IWIDPXL = LOOKT(8,ICHSET)
             JVALTHIS = JVALSTART

             IVALTHIS = IVALSTART
             IF((IROTSPEC .EQ. 0) .OR.
     1          (IROTSPEC .EQ. 2)) THEN
               IVALTHIS = IVALTHIS + IWIDPXL
               IF(LSIDEWAYS) THEN
C                ... ADVANCING VERTICALLY ...
                 IDEL2NXTC = 0
                 JDEL2NXTC = IHGTPXL
               ELSE
C                ... UPRIGHT; ADVANCING HORIZONTALLY ...
                 IDEL2NXTC = IWIDPXL
                 JDEL2NXTC = 0
               ENDIF

             ELSE
C              ... IROTSPEC == 1 .OR. 3; SO 90-, OR 270-DEGREES; 
               IVALTHIS = IVALTHIS + IHGTPXL
               IF(LSIDEWAYS) THEN
C                ... ORIGINALLY SIDEWAYS & ADVANCING VERTICALLY; 
C                ...    BUT WILL BE ROTATED & ADVANCE HORIZONTALLY ...
                 IDEL2NXTC = IHGTPXL
                 JDEL2NXTC = 0
               ELSE
C                ... ORIGINALLY UPRIGHT & ADVANCING HORIZONTALLY;
C                ...   BUT WILL BE ROTATED & ADVANCE VERTICALLY
                 IDEL2NXTC = 0
                 JDEL2NXTC = IWIDPXL
               ENDIF
             ENDIF


             DO 311 IC = 2,4
               IPL = iwindow(10) - IVALTHIS
               JPL = iwindow(11) + JVALTHIS

               LONECHAR = CCCC(IC+4)

               IF(LEBCDIC) THEN
                 LONECHAR = C1EB2AS(LONECHAR)
               ENDIF

               IF(LONECHAR .EQ. NULL) THEN
                 GO TO 333        	 	!... TERMINATE
               ELSE IF(LONECHAR .EQ. '$') THEN
                 GO TO 333        	 	!... TERMINATE

               ELSE IF(LONECHAR .EQ. '#') THEN
                 GO TO 310    		!... SKIP CHAR, BUT HOLD SPACE
               ELSE IF(LONECHAR .EQ. '*') THEN
                 GO TO 311    		!... IGNORE THIS ONE CHAR
               ENDIF
C              . . . . . . . . . . . . . . . . . . . . .
C              ... OTHERWISE, PLOT LONECHAR AT IPL,JPL
               CALL pltasym(IPL,JPL,LONECHAR,ICHSET,IROTSPEC,LCKPRNTQ,
     1                      LSRNHEMI,IMAGE,MAXIWORD,MAXJSLINE)
C              . . . . . . . . . . . . . . . . . . . . .
  310          CONTINUE
               IVALTHIS = IVALTHIS + IDEL2NXTC
               JVALTHIS = JVALTHIS + JDEL2NXTC
  311        CONTINUE
            
           ENDIF
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C          . . .   END OF EXPLICIT CHAR SET                      . . .
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

         ELSE
C          ... NO "ARROW-UP" BIT; SO USING DEFAULT CHAR SET ...
           IROTSPEC = 0

           ICHSET = IXDEFAULT
           IF(ICHSET .NE. 0) THEN
              IF(ICHSET .GT. LMTSETNUM) THEN
                 IERROR = 2
                 GO TO 999
              ENDIF

C             ... PLOT C1,C2,C3,C4 FROM FONT=ICHSET=IXDEFAULT

             IHGTPXL = LOOKT(3,ICHSET)
             IF(IHGTPXL .LT. 0) THEN
               LSIDEWAYS = .TRUE.
               IHGTPXL = IABS(IHGTPXL)
             ELSE
               LSIDEWAYS = .FALSE.
             ENDIF

             IWIDPXL = LOOKT(8,ICHSET)

             JVALTHIS = JVALSTART
             IVALTHIS = IVALSTART

C            ... IN THIS CASE IROTSPEC == 0; ALWAYS
               IVALTHIS = IVALTHIS + IWIDPXL

               IF(LSIDEWAYS) THEN
C                ... ADVANCING VERTICALLY ...
                 IDEL2NXTC = 0
                 JDEL2NXTC = IHGTPXL
               ELSE
C                ... UPRIGHT; ADVANCING HORIZONTALLY ...
                 IDEL2NXTC = IWIDPXL
                 JDEL2NXTC = 0
               ENDIF


             DO 322 IC = 1,4
               IPL = iwindow(10) - IVALTHIS
               JPL = iwindow(11) + JVALTHIS

               LONECHAR = CCCC(IC+4)
               IF(LEBCDIC) THEN
                 LONECHAR = C1EB2AS(LONECHAR)
               ENDIF

               IF(LONECHAR .EQ. NULL) THEN
                 GO TO 333        	 	!... TERMINATE LBL ITEM
               ELSE IF(LONECHAR .EQ. '$') THEN
                 GO TO 333        	 	!... TERMINATE LBL ITEM

               ELSE IF(LONECHAR .EQ. '#') THEN
                 GO TO 320    	!... SKIP CHAR, BUT MOVE TO NXT POSIT
               ELSE IF(LONECHAR .EQ. '*') THEN
                 GO TO 322    		!... IGNORE THIS ONE CHAR
               ENDIF
C              . . . . . . . . . . . . . . . . . . . . .
C              ... OTHERWISE, PLOT LONECHAR AT IPL,JPL
               CALL pltasym(IPL,JPL,LONECHAR,ICHSET,IROTSPEC,LCKPRNTQ,
     1                      LSRNHEMI,IMAGE,MAXIWORD,MAXJSLINE)
C              . . . . . . . . . . . . . . . . . . . . .
  320          CONTINUE
               IVALTHIS = IVALTHIS + IDEL2NXTC
               JVALTHIS = JVALTHIS + JDEL2NXTC
  322        CONTINUE
            
           ENDIF      			!... ON ICHSET == 0;
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C          . . .   END OF DEFAULT CHAR SET                       . . .
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

         ENDIF  		!... ON EXPLICIT OR DEFAULT CHAR SET

  333    CONTINUE
         GO TO 999

  999  CONTINUE
       RETURN
       END

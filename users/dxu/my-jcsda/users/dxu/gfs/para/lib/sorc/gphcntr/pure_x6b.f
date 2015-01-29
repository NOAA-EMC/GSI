       subroutine pure_x6b(inrast,kdimras,CLINEWRK,iby)
C                                                     22-APR-1996/DSS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PURE_X6B    ENCODE PURE RASTER INTO EXTND 6-BIT RLE
C   PRGMMR: KRISHNA KUMAR     ORG: W/NP12    DATE: 1999-07-01
C
C ABSTRACT: TO ENCODE ONE GIVEN SCANLINE (IN ITS PURE UNPACKED (RAW)
C   STATE) INTO ONE RUN-LENGTH ENCODED (RLE) SCANLINE, WHERE THE RLE
C   CODE USED HERE IS THE NMC 6-BIT RLE CODE, EXTENDED TO FIT ONE BYTE
C   PER ITEM.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR: DAVID SHIMOMURA
C   94-04-19  SHIMOMURA     --  CONVERTING FROM INTERGRAPH VAX FORTRAN 
C                               TO INTEGRAPH UNIX FORTRAN 
C   96-04-02  SHIMOMURA     --  CONVERTING FROM INTERGRAPH UNIX FORTRAN
C                               TO CRAY FORTRAN
C 1999-07-01  KRISHNA KUMAR --  CONVERTED FROM CRAY TO IBM RS/6000
C
C USAGE:    CALL pure_x6b(inrast,kdimras,CLINEWRK,iby)
C   INPUT ARGUMENT LIST:
C     INRAST   - INT*8 INRAST(KDIMRAS)
C                 ... INRAST ARRAY CONTAINS ONE PURE, UNPACKED SCANLINE 
C     KDIMRAS  - COUNT OF INTEGER*8 WORDS OF DATA IN INRAST.
C      ... I WILL EXAMINE (INRAST(I),I=1,KDIMRAS)
C      ...    UNLESS KDIMRAS EXCEEDS 66 I*8 WORDS, 
C      ...       IN WHICH CASE, I WILL LIMIT TO 66 LONGWORDS.
C      ... THE VALUE CONTAINED IN KDIMRAS WILL BE ABOUT HALF OF WHAT
C      ...    IT USED TO BE IN AN I*4 WORD LENGTH.
C
C   OUTPUT ARGUMENT LIST:
C     CLINEWRK - C*1 CLINEWRK(*)
C     IBY      - COUNT OF RESULTING BYTES IN CLINEWRK()
C
C
C REMARKS:
C     ... THIS SUBR PURE_X6B() PERFORMS NO I/O AT ALL.
C
C     ...   look_puras
C     ...      otr_pkras2()
C     ...         mid_pkras2()
C     ...            pure_x6b()       ... HERE AM I; WAY DOWN HERE
C
C     CAUTION:  THE TYPE OF LINEWRK ARRAY USED TO BE "BYTE" IN THE
C               INTERGRAPH; BUT FOR THE CRAY THIS WAS CHANGED TO 
C               "CHARACTER*1" ARRAY.
C     CAUTION:  THE GIVEN WORD COUNT: "KDIMRAS" USED TO BE THE COUNT
C               OF 32-BIT INTEGER WORDS; BUT FOR THE CRAY, THIS WAS
C               CHANGED TO THE COUNT OF 64-BIT INTEGER WORDS. 
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$

C      ... Reprogramming for UNIX                   19-Apr-1994/dss

C                                                    6-Feb-1992/dss
C      ... CALLED FROM THAT VERSION OF PAKRAST()
C      ...   WHICH IS FOUND IN MID_PAKRAST.FOR
C
C      ... copied [6,300]pakrast.for in order to separate the
C      ...   actual encoder of one scanline, 
C      ...   from pure uncompressed raster into NMC extended 6-bit RLE
C      ...   to make it more amenable to future optimizing
C
C
       INTEGER       MAXINR8
       PARAMETER    (MAXINR8=66)
C      ... WHERE 66 CRAY I*8 INTEGERS = 132 I*4 INTEGERS = 4224 BITS
C      ...   WHICH IS ONE MORE I*4 WORD THAN WAS ALLOWED IN THE
C      ...   INPUT ARRAY BEFORE; BUT TRY TO KEEP THE OUTPUT LIMIT
C      ...   THE SAME AS IT WAS.
C 
       INTEGER       MAXINR 
       parameter    (maxinr = 66)    		!... CRAY 64-bit words
C ...  parameter    (maxinr = 131) ...
C      ... WHERE 131 32-BIT INTEGERS = 4192 BITS.
C      ...    which is Bedient's limit

       INTEGER       maxdibs
       parameter    (maxdibs = 16*maxinr - 8) 	!... CRAY
C ...  parameter    (maxdibs = 8*maxinr) ...
C      ... where 8dibytes per word * 131words = 1048 dibytes = MAXDIBS
C      ... where (16dibytes per I*8word * 66 words) - 8 = 1048 dibytes
       integer       MXRUNC   
       PARAMETER    (MXRUNC = MAXDIBS)
C      ... WHERE MXRUNC (IN DIBYTES) = 1048 DIBYTES = 4192 PIXELS
C
C      . . . .  A R G S   . . . . . 
C
       INTEGER       INRAST(KDIMRAS)	! => PURE UNCOMPRESSED SCANLINE
C
       CHARACTER*1   CLINEWRK(maxdibs)	!<= RESULTING X6B CODED SCANLINE
C      ... MAX SCANLINE IN BEDIENT'S FAX SYSTEM IS 4192 BITS
C      ...   =1048 DIBYTES (WHERE A 'DIBYTE' = 4 BITS).
C      ... IF WE HAD 1048 "AS-IS" DIBYTES ENCODED IN EXTENDED
C      ...   6-BIT FAX CODE, THEN WE NEED 1048 BYTES.

       INTEGER     iby    		!<= POINTER INTO CLINEWRK
C      . . . . . . . . . . . . . . .
C
       INTEGER      MQ
C
C
       INTEGER      KEOS
       DATA         KEOS    / X'30' /
C      ... WHERE    KEOS = (0011 0000)BINARY IS END-OF-SCANLINE
C                             TT AAAA
C
C
       INTEGER     KURO 
       DATA        KURO       / X'FFFFFFFFFFFFFFFF' /
       INTEGER     MSKLO4   
       DATA        MSKLO4     / X'0000000F' /

       integer     KDIBLAK 
       DATA        KDIBLAK    / X'000F' /
       integer     KASIS
       DATA        KASIS      / X'0020' /
       integer     KBLKRUN
       DATA        KBLKRUN    / X'0010' /
       integer     K2LOW4
       DATA        K2LOW4     / X'000F' /
C
       integer     IDIBYT
       integer     IBORW
       integer     ICX
       integer     NDIBS
       integer     NDIGM2
C
       INTEGER     I
       INTEGER     IDIB
       INTEGER     IHEX
       INTEGER     IREV
       INTEGER     IRUNCT
       INTEGER     ITT
       INTEGER     ITTPRE
       INTEGER     IWD
       INTEGER     NWDM2
       INTEGER     NWDS
       

       SAVE
C
C      ... INITIALIZE FOR A NEW SCAN LINE
C
       IBY = 0
C      ... WHERE IBY IS INDEX TO CLINEWRK ARRAY,
C      ...   SO IT SHUD BE INITIALIZED EVERY SCAN LINE
       NWDS = KDIMRAS
       IF(NWDS .LE. 0) THEN
C        ... TREAT THIS CONDITION LIKE ALL BLANK SCANLINE,
         GO TO 300
       ELSE IF(NWDS .GT. MAXINR8) THEN
         NWDS = MAXINR8
       ENDIF
C      ... HOW MANY TRAILING ZERO WORDS AT RIGHT END OF LINE?
       NWDM2 = NWDS
       DO  I = 1,NWDS
         IREV = NWDS + 1 - I
         IF(INRAST(IREV) .NE. 0) GO TO 320
C        ... OTHERWISE, FOUND A TRAILING ZERO WORD
         NWDM2 = NWDM2 - 1
       ENDDO
C
C      ... IF IT FALLS THRU BOTTOM OF THIS DO_LOOP,
C      ...   THEN THIS SCAN LINE IS ALL ZERO ...
  300  CONTINUE
       CLINEWRK(1) = CHAR(KEOS)
       IBY = 1
       GO TO 600
C
  320  CONTINUE
C      ... COMES HERE WITH NWDM2= LENGTH OF SCANLINE (IN I*8 WDS)
C      ...   TO ENCODE, IGNORING TRAILING ZERO WORDS.
C
       IRUNCT = 0
       ITT = 3
C      ... TYPE OF DIBYTE INITIALIZED TO "COMMAND"
C ...       DO  499  IWD = 1,NWDM2   	!... UNIX OBJECTS TO JUMP OUT OF
       IWD = 0
  333  CONTINUE
       IWD = IWD + 1
       IF(IWD .GT. NWDM2) GO TO 500 	!... DO 499 LOOP SATISFIED
         ITTPRE = ITT
         MQ = INRAST(IWD)
         IF(MQ .NE. 0) GO TO 350
C        ... OTHERWISE, WE HAVE A RUN OF ZEROS (16 DIBYTES LONG).
         ITT = 00
         IF(ITTPRE .NE. 1) GO TO 340
C        ... PREVIOUS TYPE=01, SO STOP RUNNING BLACKS BEFORE
C        ...   STARTING RUN OF WHITE.
         ASSIGN 340 TO KRETRUN
         GO TO 700
C
  340    CONTINUE
C        ... START NEW RUN OF WHITES
         IRUNCT = IRUNCT + 16   	!... USED TO BE +8 FOR 8 DIBYTES 
         GO TO 499
C
  350    CONTINUE
         IF (MQ .NE. KURO) GO TO 400
C        ... OTHERWISE, THIS IS A RUN OF BLACKS (16 DIBYTES LONG)
         ITT = 01
         IF(ITTPRE .NE. 0) GO TO 370
C
C        ... PREVIOUS TYPE=00, SO SHUT OFF RUNNING WHITES BEFORE
C        ...   STARTING NEW RUN OF BLACKS
         ASSIGN 370 TO KRETRUN
         GO TO 700
C
  370    CONTINUE
C        ... START NEW RUN OF BLACKS
         IRUNCT = IRUNCT + 16   	!... USED TO BE +8 FOR 8 DIBYTES
         GO TO 499
C
  400    CONTINUE
C        ... COMES HERE IF WE HAVE TO EXAMINE EACH DIBYTE OF
C        ...   64-BIT WORD; where 16 dibytes per CRAY I*8 word;

C ...         DO  488 IDIB = 1,16    	!... UNIX OBJECTED TO JUMP OUTS
         IDIB = 0
  411    CONTINUE
         IDIB = IDIB + 1
         IF(IDIB .GT. 16) GO TO 489

           ITTPRE = ITT
           MQ = ISHFTC(MQ,4,64)  
           IDIBYT = IAND(MQ,MSKLO4)

           IF(IDIBYT .NE. 0) GO TO 440
C          ... OTHERWISE, THIS IS A RUN OF 1-DIBYTE OF ZEROS
           ITT = 00
           IF(ITTPRE .NE. 1) GO TO 430
C
C          ... PREVIOUS TYPE WAS RUNNING BLACKS, SO CLOSE THAT OFF
           ASSIGN 430 TO KRETRUN
           GO TO 700
C
  430      CONTINUE
C          ... START NEW RUN OF WHITES
           IRUNCT = IRUNCT + 1
           GO TO 488
C
  440      CONTINUE
           IF(IDIBYT .NE. KDIBLAK) GO TO 470
C          ... OTHERWISE, THIS IS RUN OF 1-DIBYTE OF BLACK
           ITT = 01
           IF(ITTPRE .NE. 0) GO TO 460
C          ... PREVIOUS TYPE=00, SO CLOSE RUN OF WHITES FIRST
           ASSIGN 460 TO KRETRUN
           GO TO 700
C
  460      CONTINUE
C          ... START NEW RUN OF BLACKS
           IRUNCT = IRUNCT + 1
           GO TO 488
C
  470      CONTINUE
C          ... CURRENT TYPE IS "AS-IS"
           ITT = 2
           IF(IRUNCT .LE. 0) GO TO 475
C          ... OTHERWISE, AN ACTIVE RUNNER PRECEDES THIS "AS-IS"
C          ...   SO STOP THE RUNNER
           ASSIGN 475 TO KRETRUN
           GO TO 700
C
  475      CONTINUE
           IDIBYT = IOR(KASIS,IDIBYT)
           IBY = IBY + 1
           CLINEWRK(IBY) = CHAR(IDIBYT)
C
  488    CONTINUE
         GO TO 411    		!... 488 ENDDO WAS CHGD FOR UNIX
  489    CONTINUE
C
  499  CONTINUE
       GO TO 333  		!... 499 ENDDO WAS CHGD FOR UNIX
  500  CONTINUE
C
C      ... WHEN IT FALLS THRU DO_499 LOOP, WE HAVE REACHED THE
C      ...   END OF THE GIVEN SCAN_LINE.
C      ... IF STILL RUNNING BLACKS AT END, THEN CLOSE IT DOWN;
C      ...   (IGNORE RUNNING WHITES AT END OF SCAN LINE).
       IF(ITT .NE. 1) GO TO 510
C      ... OTHERWISE, (ITT.EQ.1), SO ...
C      ...   STILL RUNNING BLACKS, SO ...
         ITTPRE = ITT
         ASSIGN 510 TO KRETRUN
         GO TO 700

  510  CONTINUE
C      ... NOW ONE SCAN LINE HAS BEEN CODED INTO CLINEWRK;
C
C      ... ADD END-OF-SCANLINE TO THIS ONE SCANLINE CLINEWRK(1:IBY) 
C
       IBY = IBY + 1
       CLINEWRK(IBY) = CHAR(KEOS)
C      ... THEN MOVE ONE ENCODED SCANLINE IN CLINEWRK INTO OUTPUT BUFFER
       GO TO 600
C
C
  700  CONTINUE
C      ... COMES HERE TO SHUT OF A RUN
C      ... ITTPRE CONTAINS TYPE OF RUN
C      ... IRUNCT CONTAINS COUNT OF DIBYTES
C
       NDIBS = IRUNCT
       IF(NDIBS .LE. 0) GO TO 777
       IF(NDIBS .GT. MXRUNC) NDIBS = MXRUNC
       IBORW = 0
       IF(ITTPRE .NE. 0) IBORW = KBLKRUN 	!... = X'0010'
       NDIGM2 = 1
       IF(NDIBS .LT. 16) GO TO 750
       IF(NDIBS .GE. 256) THEN
         NDIGM2 = 3
       ELSE
         NDIGM2 = 2
       ENDIF
  750  CONTINUE
       DO  IHEX = 1,NDIGM2
         ICX = IAND(NDIBS,K2LOW4)     	!... where K2LOW4=X'000F'
         ICX = IOR (IBORW,ICX)
         IBY = IBY + 1
         CLINEWRK(IBY) = CHAR(ICX)
         NDIBS = ISHFT(NDIBS,-4)
       ENDDO
C
  777  CONTINUE
       IRUNCT = 0
       GO TO KRETRUN,(340,370,430,460,475,510)
C
  600  continue
       return
       end

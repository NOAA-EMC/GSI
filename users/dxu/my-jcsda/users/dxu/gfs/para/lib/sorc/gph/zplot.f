      SUBROUTINE ZPLOT(XX,YY,IPEN)
C     ... this version seems to be the one in grlib     26-Mar-1996/dss

C     ... in member=~/label/v2/zplot.f                   9-Nov-1995/dss
C     ...   for a version which calls on PUTLAB()
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  ZPLOT         DRAWS LINES ON FAX AND VARIAN MAPS.
C   PRGMMR: KRISHNA KUMAR        ORG: NP12      DATE:1999-07-01
C
C ABSTRACT: THIS SUBROUTINE ENCODES VECTOR DATA INTO AN INTERMEDIATE 
C   FORM WHICH WE REFER TO AS THE "LABEL-ARRAY" FORMAT
C   (VIA SUBR PUTLAB(), WHICH WRITES TO UNIT=55).  
C   THAT FILE IS SUBSEQUENTLY READ BACK IN FOR USE BY SUBROUTINE 
C   PRTITL/CNTR, WHICH PROCESSES THE LABEL-ARRAY FORMATTED DATA
C   AND ACTUALLY PLACES THE GRAPHIC ELEMENTS INTO THE RASTER IMAGE.
C   THE CALL SEQUENCE FOR ZPLOT() IS SIMILAR TO THE PEN COMMANDS OF
C   CALCOMP PLOT COMMANDS.
C
C PROGRAM HISTORY LOG:
C   75-06-25  DENT        ORIGINAL PROGRAMMER.
C   86-05-28  HENRICHSEN    REMOVE ASYNCHRONOUS I/O.
C   87-06-16  HENRICHSEN    CONVERT TO FORTRAN 77.
C   91-02-15  HENRICHSEN    TURN OFF SOME PRINT.
C   93-04-20  LILLY         CONVERT TO FORTRAN 77.
C   95-01-30  LILLY --      THIS LATEST VERSION WAS SO DATED.
C   95-10-11  SHIMOMURA --  CONVERT TO CRAY FORTRAN
C   95-11-09  SHIMOMURA --  MODIFY TERMINATOR FROM "$" TO NULL
C   96-03-26  SHIMOMURA --  MOD /PUTWHERE/ TO INSERT LBLTAP SO THAT
C                           THIS WILL BE CONSISTENT WITH PUTLAB
C 1999-07-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO
C                           IBM RS/6000
C
C USAGE:    CALL ZPLOT(XX,YY,IPEN)
C   INPUT ARGUMENT LIST:
C     XX       - X COORDINATE IN INCHES.
C     YY       - Y COORDINATE IN INCHES.
C     IPEN     - INTEGER FLAG WHERE:
C                IPEN =  3, PEN UP
C                IPEN =  2, PEN DOWN
C                IPEN = -3 CHANGE TO ORIGIN SPECIFIED IN X Y GIVEN
C                       IN ARGUMENTS.
C                IPEN = -7 CLOSE THE FILE ON FT 55.
C                IPEN = 999 END ALL MAPS
C
C
C   INPUT VIA COMMON:
C     COMMON /MRGARG/ YDIS, DUMMY(2), IRET_ZPL, XO,YO
C     YDIS     - Y-DISPLACEMENT (IN INCHES)
C              = 0.0;            THEN, DO NOT SWITCH COORDINATES
C              = NON-ZERO VALUE; THEN SWITCH COORDINATES BY:
C                      XHOLD = X
C                      X     = YDIS - Y
C                      Y     = XHOLD
C
C     COMMON /XZPLAX/ XZPLOT,YZPLOT, IORITY
C     XZPLOT,YZPLOT - MULTIPLICATIVE SCALE FACTORS TO BE APPLIED TO X,Y
C                     USER WILL USUALLY SET TO =1.0; 
C     IORITY   - PRIORITY FOR THE PLOT
C                ACCEPTABLE VALUES FOR PRIORITY = 0, 1, OR 2
C
C   OUTPUT ARGUMENT LIST:
C
C   OUTPUT VIA COMMON:
C     COMMON /KPLOT/  LABEL(2,1024),LABIX,NOBUF,IDRA(50)
C       THE RESULTS ZPLOT() ARE LINE-ELEMENTS PUT INTO THE LABEL-ARRAY
C         VIA SUBR PUTLAB();
C
C     COMMON /MRGARG/ YDIS, DUMMY(2), IRET_ZPL, XO,YO
C     IRET_ZPL  - RETURN CODE FROM ZPLOT()
C               = 0;   NORMAL RETURN
C               = 21;  THE GIVEN LINE WAS TOO LONG
C               = 22;  ZPLOT FAILED ON WITHIN-FONT POINTER VALUE
C               = OTHER NON-ZERO VALUES; ERROR RETURN CODES FROM PUTLAB
C                                       (SEE PUTLAB'S DOCBLOCK)
C
C     XO,YO     - TO SAVE THE COORDINATES OF ORIGIN (IN PIXELS)
C                    AS OBTAINED FROM IPEN=-3 CALLS;
C
C
C   OUTPUT FILES:
C     FT06F001 - PRINT MESSAGES.
C     FT55F001 - THE END RESULT OF ZPLOT IS THIS FILE WHICH CONTAINS
C                   THE LINE ELEMENTS.
C
C REMARKS: 
C     THIS VERSION OF ZPLOT() IS A CHANGE FROM PREVIOUS VERSIONS
C     IN THAT THIS CALLS ON SUBR PUTLAB() INSTEAD OF HANDLING THE
C     LABEL ARRAY DIRECTLY.
C     ...INITIALIZES THE LABEL-ARRAY'S DEFAULT CHARACTER-SET TO BE 
C     .. CHAR SET=12, WHICH MUST BE THE (-4X4) PIXEL LINE-ELEMENT.
C     THIS REFERENCES CHAR SET=13; FOR THE 30-PIXEL HORIZONTAL LINE
C     ...(SEE ISYMBA(27)) 
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C     ----------------------------------------------------------------
C
C     ... Some notes about CRAY conversion ...
C
C     ... This has Z-format DATA statements which contain EBCDIC for
C     ...   characters like the "$"; which must be changed to ASCII
C     ...   or to characters.
C
C     ... Since this initializes the LABEL array to the default char
C     ...   set = 12;  the char set access codes must be set for
C     ...   that index to be the (-4X4) line elements.
C     ...   Also, this ZPLOT() routine must be called before any
C     ...   other routine has PUT anything into the LABEL array.
C
C     ... How does this convert given x,y into i,j (in pixels)?
C     ...   i = NINT((x*XZPLOT)*100.0)
C     ----------------------------------------------------------------
      INTEGER      LMAX
      PARAMETER   (LMAX = 1024)
C ...      PARAMETER (LMAX = 64)  ... FOR CHECKOUT ONLY FOR SMALLER BIN

      INTEGER      NWD_ID
      PARAMETER   (NWD_ID=50)

      INTEGER      MAXFONT
      PARAMETER   (MAXFONT=63)

      INTEGER    MAXIPEL
      PARAMETER (MAXIPEL=4100)   	!... WINDOW BOUND ON I (PIXELS)
      INTEGER    MAXJPEL
      PARAMETER (MAXJPEL=7300)  	!... WINDOW BOUND ON J (PIXELS)

      INTEGER    MXLINE
      PARAMETER (MXLINE=4800)   	!... LIMIT ON LINE LENGTH

      INTEGER    KFNT4X4
      PARAMETER (KFNT4X4=12)    	!... FONT-INDEX LINE-ELEM(-4X4)

      INTEGER    KFNT30X1
      PARAMETER (KFNT30X1=13)    	!... FONT-INDEX HORIZ LNE(-30X1)

C
      COMMON   /MRGARG/YDIS,DUMMY(2),IRET_ZPL,XO,YO
      REAL     YDIS
      INTEGER  IRET_ZPL
      REAL     XO,YO

      COMMON   /XZPLAX/ XZPLOT, YZPLOT, IORITY
      REAL     XZPLOT,YZPLOT     	!... MULTIPLICATIVE FOR X,Y
      INTEGER  IORITY
C     -----------------------------------------------------------------
C
      COMMON /KPLOT/  LABEL(2,LMAX),LABIX,NOBUF,IDRA(NWD_ID)
C
      COMMON /PUTWHERE/ LBLTAP,IERPUT,LCKPT_PUT,LCKPRNQQ,
     1                  IJPXL_GIVN(2),IJPXL_LBL(2),IJPXL_NEXCH(2),
     2                  NCALLS_PUT,ICOUNT_FONT(MAXFONT)

      INTEGER      IERPUT
      INTEGER      LCKPT_PUT
      LOGICAL      LCKPRNQQ

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C     . . . . . . . . . . . . . . . . . . . . . 
C USAGE:    CALL ZPLOT(XX,YY,IPEN)
      REAL         XX
      REAL         YY
      INTEGER      IPEN
C     . . . . . . . . . . . . . . . . . . . . . 
      REAL         AKLN 		!...  width/hgt in pels floated 
      DATA         AKLN        / 4.0 /

      REAL         RADIC2  
      DATA         RADIC2      / 1.414214 /

      REAL         TANA(6) 
      DATA         TANA        / 0.250, 0.500, 0.750, 1.333, 2.0, 4.0 /

C
C     . . . .   CALL SEQUENCE FOR PUTLAB()   . . . . . . .
      INTEGER       IPT,JPT
      REAL          HEIGHT

      INTEGER       JTEXT(10)   	!... 10 CRAY_WD * 8BYTES/WD=80
      CHARACTER*80  CTEXT
      EQUIVALENCE  (JTEXT,CTEXT)

      REAL          ANGLE
      INTEGER       NCHAR

      INTEGER       KROT_PRI(2)
      INTEGER       KROTAT
      EQUIVALENCE  (KROT_PRI(1),KROTAT)
      INTEGER       KPRIOR
      EQUIVALENCE  (KROT_PRI(2),KPRIOR)

      INTEGER       ICMD

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      CHARACTER*1   CFNT4X4

      INTEGER       IFONTXX       
      CHARACTER*1   CHKTH


      INTEGER      IFIRST
      DATA         IFIRST         /1/
C
C     ...INITIALIZE LABEL-ARRAY TO THE 12TH (X'0C') CHAR SET 
C     ...   WHICH IS (-4X4) PIXEL LINE-ELEMENTS
C
      INTEGER    INIT(2)
      DATA       INIT          / 1, X'3F0C0000' /   
C                                ... '?'//X'0C'//NULL
C ... DATA       INIT          / 1, X'3F0C2400' /   !... '?'//X'0C'//'$'
C ... DATA       INIT          / 1, X'6F0C5B00' /


C     ... THE FOLLOWING (K) CONSTANTS ARE FOR the Kth character within
C     ...    the (-4X4) default line-element font12
      INTEGER    K00
      DATA       K00         /1/    	!... HORIZ RIGHTWARD
      INTEGER    K09 
      DATA       K09         /7/  	!... VERT UP- OR DOWN-WARD
      INTEGER    K18
      DATA       K18         /13/   	!... HORIZ LEFTWARD
      INTEGER    KTWO
      DATA       KTWO        /13/
C     ...WHERE KTWO IS DISPLACEMENT IN ELEMENT TABLE TO 2 DOT ELEMENTS
C     ...   ARE THERE SOME 2X2 ELEMENTS IN THE SAME CHAR SET?


      INTEGER      MSKIX
      DATA         MSKIX       / X'1FFF' /
      INTEGER      MSKJY
      DATA         MSKJY       / X'7FFF' /
      INTEGER      NJPW(2)
      CHARACTER*1  NULL
C
      SAVE         IFIRST, X1,Y1, X2,Y2, DELXO,DELYO, SAVX2,SAVY2

C     . . . . . .   S T A R T   . . . . . . . . . . . . . . . . . 

      IRET_ZPL = 0
      NULL = CHAR(0)

      X = XX
      Y = YY

      IF(IFIRST .NE. 0) THEN
C       ... ONLY ON FIRST CALL TO ZPLOT W/I A LOGICAL MAP ...
        IF(IPEN .EQ. 999) GO TO 930
C
        IFIRST = 0    		!... RESET SO IT WON'T COME THIS WAY AGN
        XO = X*100.
        YO = Y*100.
        X1 = 0.
        Y1 = 0.
        DELXO = XO
        DELYO = YO
C
C       ... TO ZERO THE LABEL-ARRAY, AND INITIALIZE PUTLAB COUNTS,
        JTEXT(1) = 0
        CFNT4X4 = CHAR(KFNT4X4)
        CTEXT(1:3) = '?'//CFNT4X4//NULL 
        NCHAR = 2
        ANGLE = 0.0
        IPT = 1
        JPT = 0
        HEIGHT = FLOAT(KFNT4X4)
        KROTAT = 0
        KPRIOR = 0
        ICMD = -2   		!... NEW ICMD TO INITIALIZE LABEL ARRAY

        CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KROT_PRI,ICMD)
C
        IRET_ZPL = IERPUT
        GO TO 999
      ENDIF
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
   10 CONTINUE
      IF ((IPEN .EQ. 999) .OR.
     1    (IPEN .EQ.  -7)) THEN
C       ... TO CLEANUP AND CLOSE OUT THE LABEL ARRAY,
         GO TO 911
      ENDIF

C     ... OTHERWISE, WAS NOT A CLOSE OUTPUT COMMAND, SO
C
      IF(IPEN .EQ. -3) THEN
C       ... CHANGE TO ORIGIN SPECIFIED
C       ... BY THE X AND Y GIVEN IN ARG IN GLORIA,S COORDINATE SYSTEM
        XO = X * 100.0
        YO = Y * 100.0
        X1 = 0.0
        Y1 = 0.0
        DELXO = XO
        DELYO = YO
        GO TO 999    			!... TO EXIT AFTER CHGG ORIGIN
      ENDIF
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
   11 CONTINUE
      KROTAT = 0
      IF((IORITY .GE. 0) .AND.
     1   (IORITY .LE. 2)) THEN
C       ... THE GIVEN IORITY WAS WITHIN BOUNDS, 
        KPRIOR = IORITY
      ELSE
C       ... THE GIVEN IORITY WAS OUT-OF-BOUNDS, SO USE PRIORITY=0
        KPRIOR = 0
      ENDIF

      X = X * XZPLOT
      Y = Y * YZPLOT

      IF(YDIS .NE. 0.0) THEN
C        ... DISPLACE Y AND FLIP COORDINATES ***
        XE = X
        X =  YDIS-Y
        Y = XE
      ENDIF

      X2 = X * 100.0
      Y2 = Y * 100.0

      IX2 = NINT(X2)
      X2  = IX2
C     ...NOW X2 HAS BEEN REPLACED BY ITS ROUNDED TO NEAREST DOT VALUE

      IY2 = NINT(Y2)
      Y2  = IY2
C     ...NOW Y2 HAS BEEN REPLACED BY ITS ROUNDED TO NEAREST DOT VALUE

      IF (IPEN .EQ. 2) GO TO 20
      IF (IPEN .EQ. 3) GO TO 18
      IF(IPEN .EQ.  -7) THEN
C       ... TO LOGICALLY END A PRODUCT ON LABEL ARRAY,
        GO TO 911
      ENDIF
C     ... OTHER VALUED IPEN,
      GO TO 18

   18 CONTINUE
C     ... COMES HERE ON IPEN=3  ....
      X1 = X2
      Y1 = Y2
      GO TO 999    		!... TO EXIT AFTER MOVE ON IPEN=3

   20 CONTINUE
C     ... COMES HERE ON IPEN=2  ....
      DELX = X2 - X1
      DELY = Y2 - Y1
      SAVX2 = X2
      SAVY2 = Y2
      IF((ABS(DELX) + ABS(DELY)) .LT. 1.0) THEN
C       ...WHICH TESTS FOR TWO SUCCESSIVE POINTS TOO CLOSE.....
        GO TO 999   		!... TO EXIT ON END PTS TOO CLOSE
      ENDIF

      GO TO 21

  500 CONTINUE
      PRINT  98, IP, JP, IPEN
   98 FORMAT(1H ,'ZPLOT: IP OR JP OUT OF BOUNDS.  ',
     1          'IP=', I6, '; JP=', I6,'; IPEN=', I4)
      GO TO 36
C
   21 CONTINUE
      LLICOR = 0

      IF(DELX) 206,200,210
  200 CONTINUE
C     ...COMES TO 200 IF VERTICAL LINE, GOING UP OR DOWN.
      IF(DELY) 202,36,204
  202 CONTINUE
C     ...COMES TO 202 IF VERTICAL AND GOING DOWNWARD.
      YE = Y1
      Y1 = Y2
      Y2 = YE
  204 CONTINUE
      K = K09
      DX = 0.0
      DY = AKLN
      ALINC = AKLN
      ALINE = ABS(DELY)
      GO TO 29

  206 CONTINUE
C     ...COMES TO 206 IF LEFT OF VERTICAL
      IF(DELY) 221,208,221
  208 CONTINUE
C     ...COMES TO 208 IF HORIZONTAL TO THE LEFT...
      LLICOR = -3
      K = K18
      DX = -AKLN
      DY = 0.0
      ALINC = AKLN
      ALINE = ABS(DELX)
      IF(ALINE .LT. 30.0) GO TO 29
C     ...SET TO HORIZONTAL 30DOT ELEMENT
      LLICOR = -29
      K = 27
      DX = -30.0
      ALINC = 30.0
      GO TO 29

  210 CONTINUE
C     ...COMES TO 210 IF RIGHT OF VERTICAL
      IF(DELY) 221,212,221

  212 CONTINUE
C     ...COMES TO 212 IF HORIZONTAL TO THE RIGHT
      K = K00
      DX = AKLN
      DY = 0.0
      ALINC = AKLN
      ALINE = DELX
      IF(ALINE .LT. 30.0) GO TO 29
C     ...SET TO HORIZONTAL 30 DOT ELEMENT
      K = 27
      DX = 30.0
      ALINC = 30.0
      GO TO 29

  221 CONTINUE
C     ...COMES TO 221 FOR GENL CASE -- NON-VERTICAL AND NON-HORIZONTAL
C     ...TO FIND (K) FOR WHICH LINE ELEMENT WITHIN THE (-4X4) FONT
C     ...   WHERE (K) IS USED TO INDEX INTO ISYMBA(K)
      TANGEN = DELY/DELX
      ALOOK = ABS(TANGEN)
      DO  22  I = 1,6
        K = I
        IF(ALOOK .LE. TANA(I)) GO TO 24
   22 CONTINUE
C     ...IF IT FALLS OUT HERE, ALOOK WAS .GT. LARGEST TANA SO SET VERT
      K = K09
   24 IF((DELX*DELY) .LT. 0.0) K = 14 - K
      IF(K .GT. K09) LLICOR = -3
C     ...FOR THOSE ELEMENTS IN QUAD II WITH ORIGINS IN LOWER RIGHT CORNR
      IF(DELY .GE.0.0) GO TO 27
C     ...OTHERWISE THE VECTOR IS HEADED DOWNWARD, SO INTERCHG FIRST PT
      YE = Y1
      Y1 = Y2
      Y2 = YE
      XE = X1
      X1 = X2
      X2 = XE
   27 CONTINUE
C     ...TO FIND DX, DY, ALINC, ALINE FOR GENL CASE
      IF(TANGEN - 1.0) 272,274,276
  272 CONTINUE
      IF(TANGEN .LT. 0.0) GO TO 278
C     ...FALLS THRU TO HERE IF 01 - 44 DEGREE ANGLES
      DX = AKLN
  273 DY = DX * TANGEN
      GO TO 280
  274 CONTINUE
C     ...COMES HERE IF EXACTLY 45 DEGREES
      DX = AKLN
  275 DY = AKLN
      ALINC = RADIC2 * AKLN
      ALINE = RADIC2 * ABS(DELX)
      GO TO 29
  276 CONTINUE
C     ...COMES HERE IF 46 - 89 DEGREES
      DY = AKLN
      DX = DY * (DELX/DELY)
      GO TO 280
  278 CONTINUE
C     ...COMES HERE IF TANGEN IS NEGATIVE (QUAD II OR IV)
      DX = -AKLN
      IF(TANGEN + 1.0) 276,275,273
C
  280 CONTINUE
      ALINC = SQRT(DX*DX + DY*DY)
      ALINE = SQRT(DELX*DELX + DELY*DELY)
C     ...WHERE ALINE IS THE LENGTH OF THE HYPOTENUSE..........
   29 CONTINUE
      IF(ALINE .LT. MXLINE) GO TO 300
      PRINT  110
  110 FORMAT(1H ,'ZPLOT: IGNORED LINE WHICH EXCEEDED 4 FEET' )
      IRET_ZPL = 21
      GO TO 999
C
  300 CONTINUE
      COV1 = (ALINC/2.0) - 0.8
      COV2 = (ALINC/2.0) + 1.0
C
  301 CONTINUE
C     ...TEST WHETHER ENOUGH ALINE TO COVER WITH LINE ELEMENT...
      KIPPEN = 0
C     ...WHICH IS A SWITCH TO INDICATE LAST ELEMENT FOR THIS ALINE
      ALINE = ALINE - ALINC
C     ...NOW ALINE HAS REMAINDER IF THIS LINE ELEMENT IS PLOTTED
      IF(ALINE) 302,304,310
  302 CONTINUE
C     ...COMES TO 302 IF THIS ELEMENT WOULD EXTEND BEYOND END OF LINE
C     ...TEST FOR LAST OF 30 DOT ELEMENT
      IF(K .GT. 26) GO TO 20
      KIPPEN = 1
      OVER = ABS(ALINE)
      IF(OVER .LE. COV1) GO TO 310
      IF(OVER .GT. COV2) GO TO 36
      K = K + KTWO
C     ...WHICH RESETS (K) TO 2 DOT ELEMENT TABLE...
      GO TO 310
  304 CONTINUE
      KIPPEN = 1
      GO TO 310
C
  310 CONTINUE
      IP = X1 + DELXO + 0.5
      JP = Y1 + DELYO + 0.5
      IP = IP + LLICOR

      IF(IP .LT. 0) GO TO 500
      IF(IP .GT. MAXIPEL) GO TO 500

      IF(JP.LT.0) GO TO 500
      IF(JP.GT.MAXJPEL) GO TO 500
C     ...WHICH BOUNDS (IP,JP) TO A WINDOW OF APPROX 6FT IN J_LENGTH 
C     ...   AND 41 INCHES ALONG SCAN LINE

      JP = IAND(JP,MSKJY)
      JPT = JP
      IP = IAND(IP,MSKIX)
      IPT = IP

      JTEXT(1) = 0
      IF((K .GT. 0) .AND.
     1   (K .LT. 27)) THEN
C       ... WITHIN BOUNDS OF FONT12 ... THE DEFAULT CHAR SET (4X4)
        ICMD = 1     		!... PUTLAB: USE DEFAULT CHAR SET
        CHKTH = CHAR(K)
        CTEXT(1:2) = CHKTH//NULL
        HEIGHT = FLOAT(KFNT4X4) 	!... CHAR SET (4X4)

      ELSE IF(K .EQ. 27) THEN
C       ... THE CASE OF THE HORIZ LINE (30X1) OF FONT13 ...
        ICMD = 0    		!... PUTLAB: USE EXPLICIT CHAR SET 
        CHKTH = CHAR(1)
        CTEXT(1:2) = CHKTH//NULL
        HEIGHT = FLOAT(KFNT30X1) 	!... CHAR SET (30X1)

      ELSE
C       ... INVALID VALUE FOR (K) POINTER TO THE ONE CHAR DEF
        IRET_ZPL = 22
        GO TO 999
      ENDIF
C

      
      ANGLE = 0.0
      NCHAR = 2

      CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT(1:2),ANGLE,NCHAR,KROT_PRI,ICMD)
      IRET_ZPL = IERPUT
      IF(IERPUT .NE. 0) GO TO 999

      IF(KIPPEN .EQ. 0) THEN
        X1 = X1 + DX
        Y1 = Y1 + DY
        GO TO 301
      ENDIF
C     ... OTHERWISE, KIPPEN .NE. 0, SO THAT WAS LAST ELEM; JUMP OUT:
      GO TO 36

C     ...GOES OUT TO 36 IF THIS WAS LAST ELEMENT TO BE PLOTTED
   36 CONTINUE
      Y1 = SAVY2
      X1 = SAVX2
      GO TO 999            		!... TO EXIT
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C
  911 CONTINUE
C     ...COMES TO 911 AT END OF MAP TO CLEANUP LABEL ARRAY 
C     ...   FILE ON LBLTAP...
      LCKPT = 911
      ICMD = -7

      JTEXT(1) = 0 
      NCHAR = 0
      IPT = 0
      JPT = 0
      HEIGHT = FLOAT(KFNT4X4)
      KROTAT = 0
      KPRIOR = 0
      
      CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT(1:8),ANGLE,NCHAR,KROT_PRI,ICMD)
      IRET_ZPL = IERPUT
C
      IFIRST = 1
C     ...WHICH WILL LET IT INITIALIZE FOR FOLLOWING MAP...
      IF(IPEN .NE. 999) GO TO 999   		!... TO EXIT ON -7

C     ...OTHERWISE, IPEN IS 999 SO END ALL MAPS W/ EXTRA EOF MARK...
  930 CONTINUE
      LCKPT = 933
      ICMD = 999

      JTEXT(1) = 0 
      NCHAR = 0
      IPT = 0
      JPT = 0
      HEIGHT = FLOAT(KFNT4X4)
      KROTAT = 0
      KPRIOR = 0

      CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT(1:8),ANGLE,NCHAR,KROT_PRI,ICMD)
C     ...WHICH TERMINATES THE MAPS FILE
      IRET_ZPL = IERPUT
      GO TO 999

  999 CONTINUE
      RETURN
      END

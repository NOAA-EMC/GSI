      SUBROUTINE W3FP10(RDATA,KTBL,CNST,TITLE,KRECT,KCONTR,
     & LINEV,IWIDTH)
C$$$  SUBROUTINE DOCUMENTATION BLOCK  ***
C
C SUBROUTINE:  W3FP10          PRINTER CONTOUR SUBROUTINE
C   PRGMMR: JONES            ORG: NMC421      DATE:89-09-13
C
C ABSTRACT:  PRINTS A TWO-DIMENSIONAL GRID OF ANY SHAPE, WITH
C   CONTOURING, IF DESIRED. GRID VALUES ARE SCALED ACCORDING TO
C   TO CONSTANTS SPECIFIED BY THE PROGRAMER, ROUNDED, AND PRINTED
C   AS 4,3, OR 2 DIGIT INTEGERS WITH SIGN, THE SIGN MARKING THE
C   GRID POSITION OF THE PRINTED NUMBER. IF CONTOURING IS REQUESTED,
C   BESSEL'S INTERPOLATION FORMULA IS USED TO OPTAIN THE CONTOUR LINES.
C   CONTOURS ARE INDICATED BY ALPHABETIC CHARACTERS RANGING FROM A TO
C   H OR NUMERIC CHARACTERS FROM 0 TO 9. CONTOUR ORIGIN AND INTERVAL
C   ARE SPECIFIED BY THE PROGRAMMER IN TERMS OF PRINTED VALUES.
C
C PROGRAM HISTORY LOG:
C   89-09-08  R.E.JONES
C   92-05-02  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN, ADD SAVE.
C
C USAGE: CALL W3FP10 (RDATA,KTBL,CNST,TITLE,KRECT,KCONTR,LINEV,IWIDTH)
C   INPUT ARGUMENTS:  RDATA  = REAL ARRAY OF GRID DATA TO BE PRINTED.
C                     KTBL   = INTEGER ARRAY WITH SHAPE OF ARRAY.
C                     CNST   = REAL ARRAY OF FOUR ELEMENTS, USED IN
C                              SCALING FOR PRINTING AND CONTOURING.
C                     TITLE  = IS A ARRAY OF 132 CHARACTERS OR LESS OF
C                              HOLLERITH DATA, 1ST CHAR. MUST BE BLANK.
C                              PRINTED AT BOTTOM OF THE MAP.
C                     KRECT  = 1 IF GRID IS RECTANGULAR, 0 OTHERWISE.
C                     KCONTR = 1 FOR CONTOURING , 0 OTHERWISE.
C                     LINEV  = 0 IS FOR 6 LINES PER VERTICAL INCH,
C                              NON-ZERO 8 LINES PER VERTICAL INCH.
C                     IWIDTH = NUMBER OF CHARACTERS IN PRINT LINE,
C                              132 IS STANDARD PRINTER.
C
C   INPUT FILES:  NONE
C
C
C   OUTPUT ARGUMENTS:  NONE
C
C   OUTPUT FILES: STANDARD FORTRAN PRINT FILE
C
C
C   RETURN CONDITIONS: NORMAL SUBROUTINE RETURN, UNLESS NUMBER
C                      OF ROWS IS GREATER THAN 200, PRINTS ERROR
C                      MESSAGE AND EXITS.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE :  NONE
C
C     LIBRARY:  MIN0 , FORTRAN FUNCTION , RETURNS SMALLER VALUE
C               IABS , MOD
C
C REMARKS: SPECIAL VERSION OF W3FP05, 1ST POINT IS UPPER LEFT HAND
C          CORNER. WRITTEN ON REQUEST OF PETER CHASE BECAUSE SOME
C          GRIB FIELDS CAN START WITH THE UPPER LEFT HAND CORNER
C          AS THE 1ST POINT OF A GRID.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
       REAL            CNST(4)
       REAL            RDATA(*)
       REAL            RWA(28)
       REAL            RWB(28)
       REAL            RWC(28)
       REAL            RWD(28)
       REAL            VDJA(29)
       REAL            VDJB(28)
       REAL            VDJC(28)
C
       INTEGER         TITLE(33)
       INTEGER         KRLOC(200)
       INTEGER         KTBL(*)
       INTEGER         OUTPUT
       INTEGER         PAGNL
       INTEGER         PAGNR
       INTEGER         PAGN3
       INTEGER         PCCNT
       INTEGER         PCFST
       INTEGER         PGCNT
       INTEGER         PGCNTA
       INTEGER         PGFST
       INTEGER         PGFSTA
       INTEGER         PGMAX
C
       LOGICAL         DONE
       LOGICAL         LCNTR
       LOGICAL         RECT
C
       CHARACTER*1     KALFA(16)
       CHARACTER*1     KALPH(20)
       CHARACTER*1     KHASTR
       CHARACTER*1     KHBLNK
       CHARACTER*1     KHDOLR
       CHARACTER*1     KHMNS
       CHARACTER*1     KHPLUS
       CHARACTER*1     KHRSTR
       CHARACTER*1     KHTBL(10)
       CHARACTER*1     KLINE(126)
       CHARACTER*1     KLINES(132)
       CHARACTER*1     KNUMB(20)
C
       EQUIVALENCE     (CRMX,VDJA(29))
       EQUIVALENCE     (KLINE(1),KLINES(8))
       EQUIVALENCE     (VDJC(1),RWA(1))
C
C      ... THE VALUE CRMX IS MACHINE DEPENDENT, IT SHOULD BE
C      ... SET TO A VALUE A LITTLE LESS THAN THE LARGEST POSITIVE
C      ... FLOATING  POINT NUMBER FOR THE COMPUTER.
C
       SAVE
C
       DATA  CRMX  /10.E70/
       DATA  KALFA /'A',' ','B',' ','C',' ','D',' ','E',' ','F',
     &              ' ','G',' ','H',' '/
       DATA  KHASTR/'*'/
       DATA  KHBLNK/' '/
       DATA  KHDOLR/'$'/
       DATA  KHMNS /'-'/
       DATA  KHPLUS/'+'/
       DATA  KHRSTR/'1'/
       DATA  KHTBL /'0','1','2','3','4','5','6','7','8','9'/
C
C     ... LIMNRW IS LIMIT ON NUMBER OF ROWS ALLOWED
C     ...   AND IS DIMENSION OF KRLOC ...
C
       DATA  LIMNRW/200/
       DATA  KNUMB /'0',' ','1',' ','2',' ','3',' ','4',' ',
     &              '5',' ','6',' ','7',' ','8',' ','9',' '/
       DATA  OUTPUT/6/
       DATA  R5    /.2/
       DATA  R50   /.02/
C
 8000  FORMAT (1H0,10X,44HERROR FROM W3FP10 ... NUMBER OF ROWS IN YOUR,
     &         9H ARRAY = ,I4,24H WHICH EXCEEDS LIMIT OF ,I4)
 8100  FORMAT ( 1HT)
 8200  FORMAT ( 1HS)
 8300  FORMAT ( 1H ,/,1H ,/,1H )
 8400  FORMAT ( 1H ,/,1H )
 8500  FORMAT ( 132A1)
 8600  FORMAT ( 33A4)
C
C           COMPUTE VALUES FOR PRINTER WIDTH
C
         IF (IWIDTH.GE.132.OR.IWIDTH.LE.0) PGMAX = 25
         IF (IWIDTH.GE.1.AND.IWIDTH.LE.22) PGMAX = 3
         IF (IWIDTH.GT.22.AND.IWIDTH.LT.132) PGMAX = (IWIDTH-7) / 5
         LW            = (PGMAX * 5 + 7) / 4
         PAGN3         =  PGMAX + 3
         VDJA(PAGN3+1) =  CRMX
         MXPG          =  PGMAX * 5 + 7
C
         IF (LINEV .NE. 0) THEN
C
C     ...OTHERWISE LINEV IS NON-ZERO, SO 8 LINES/INCH IS DESIRED...
C
           LINATE = 1
           R4     = 0.250
           R32    = 0.03125
           CON2   = 10.0
           NBTWN  = 3
C
         ELSE
C
           LINATE = 2
           R4     = 0.33333333
           R32    = 1.0 / 18.0
           CON2   = 6.0
           NBTWN  = 2
         ENDIF
C
           PGCNTA = 0
           PGFSTA = 0
           RECT   = .FALSE.
           DONE   = .FALSE.
           KZ     = 0
           KZA    = 1000
           A      = CNST(1)
           KCA    = 2 * (1 - KRECT)
C
C        TO SET NO. OF DIGITS TO BE PRINTED
C        WHICH IS A FUNCTION OF THE TENS POSITION IN KCONTR
C
         NODIG = IABS(KCONTR/10)
         NODIG = 3 - NODIG
C
C        WHERE C(NODIG) + 1 IS NO. OF DIGITS TO BE PRINTED
C
         IF (NODIG.LT.1 .OR. NODIG.GT.3) NODIG = 3
C
C        ANY OUT-OF-RANGE WILL GET 4 DIGITS
C
         LCNTR = .FALSE.
         NCONQ = IABS(MOD(KCONTR,10))
         IF (NCONQ .EQ. 0) GO TO 400
         IF (NCONQ .LE. 2) GO TO 300
C
C        OTHERWISE RESET NCONQ
C
         NCONQ = 0
         GO TO 400
C
 300     CONTINUE
         LCNTR = .TRUE.
C
C        WITH NCONQ = 1 FOR LETTERS,AND = 2 FOR NUMBERS IN CONTOUR BANDS
C
 400     CONTINUE
         IF (NCONQ .NE. 2) THEN
C
C        OTHERWISE SET AS LETTERS
C
           KCOW = 16
           DO 500  J = 1,KCOW
             KALPH(J) = KALFA(J)
 500       CONTINUE
C
         ELSE
C
           KCOW = 20
           DO 700  J = 1,KCOW
             KALPH(J) = KNUMB(J)
 700       CONTINUE
C
         ENDIF
C
           RADJ = 4 * KCOW
           KD   = 1
C
C *** SET UP TABLE OF INDICES CORRESPONDING TO FIRST ITEM IN EACH ROW
C ***     THIS IS KRLOC
C *** PICK OUT SIZE AND ROW NUMBER OF LARGEST ROW (KCMX AND KCLMX)
C *** KZA LEFT-JUSTIFIES MAP IF ALL ROWS HAVE COMMON MINIMAL OFFSET
C
         IF (KTBL(1 ).NE.(-1)) THEN
C
C *** ONE-DIMENSIONAL FORM
C
         KTF  = 3
         KZA  = 0
         IMIN = KTBL(2)
         JMIN = KTBL(3)
         JMAX = KTBL(3) + KTBL(1) - 1
         NRWS = KTBL(1)
         IF (NRWS .GT. LIMNRW) THEN
           WRITE (OUTPUT,8000) NRWS , LIMNRW
           RETURN
         ENDIF
         KC   = 1
C
         DO 1000 J = 1,NRWS
           KRLOC(J) = KD
           IF (KTBL(KC+4) + KTBL(KC+3).LE.KZ ) GO TO 900
             KCLMX = J
             IMAX  = KTBL(KC+4) + KTBL(KC+3)
             KZ    = IMAX
             KCMX  = KRLOC(J) + KTBL(KC+4)
 900       CONTINUE
             KD    = KD + KTBL(KC+4)
             KC    = KC + KCA
 1000    CONTINUE
C
         ELSE
C
C *** TWO-DIMENSIONAL FORM
C *** THE TWO-DIMENSIONAL FORM IS COMPILER-DEPENDENT
C *** IT DEPENDS ON THE TWO-DIMENSIONAL ARRAY BEING STORED COLUMN-WISE
C *** THAT IS, WITH THE FIRST INDEX VARYING THE FASTEST
C
           IMIN = KTBL(6)
           JMIN = KTBL(7)
           NRWS = KTBL(5)
           IF (NRWS .GT. LIMNRW) THEN
             WRITE (OUTPUT,8000) NRWS , LIMNRW
             RETURN
           ENDIF
C
         JMAX  = KTBL(7) + KTBL(5) -1
         KC    = 1
         DO 1500 J = 1,NRWS
           KRLOC(J) = KTBL(2) * (KTBL(4)-NRWS+J-1) + KTBL(KC+7) + 1
           IF (KTBL(KC+7) + KTBL(KC+8).LE.KZ) GO TO 1400
             IMAX  = KTBL(KC+7) + KTBL(KC+8)
             KZ    = IMAX
             KCMX  = KRLOC(J) + KTBL(KC+8)
             KCLMX = J
 1400      CONTINUE
           IF (KTBL(KC+7).LT.KZA) KZA = KTBL(KC+7)
             KC = KC + KCA
 1500    CONTINUE
           IMAX = IMAX - KZA
           KTF  = 7
         ENDIF
C
           PAGNL = 0
           PAGNR = PGMAX
           IF (.NOT.LCNTR) GO TO 1700
           ADC   = (CNST(1) - CNST(4)) / CNST(3) + RADJ
           BC    =  CNST(2) / CNST(3)
C
C *** PRINT I-LABELS ACROSS TOP OF MAP
C
 1700    CONTINUE
C
C ***    WHICH PREPARES CDC512 PRINTER FOR 8 LINES PER INCH
C
         IF (LINATE.EQ.1) WRITE (OUTPUT,8100)
C
C     ...WHICH PREPARES PRINTER FOR 6 LINES PER INCH
C
         IF (LINATE.EQ.2) WRITE (OUTPUT,8200)
           KLINES(1) = KHRSTR
           KBR       = 1
           GO TO 6900
C
 1800    CONTINUE
         IF (.NOT.LCNTR) GO TO 2000
C
C *** INITIALIZE CONTOUR WORKING AREA
C
           DO 1900 J = 1,PAGN3
             RWC(J) = CRMX
             RWD(J) = CRMX
 1900      CONTINUE
C
C *** SET UP CONTOUR DATA AND PAGE LIMITERS FOR FIRST TWO ROWS
C
 2000    CONTINUE
           KRA  = 1
           KC   = KTF + 1
           KBR  = 2
           GO TO 5900
C
 2100    CONTINUE
           KRA = 2
           KC  = KC + KCA
           KBR = 3
           GO TO 5900
C
 2200    CONTINUE
           KR   = 0
C
C *** TEST IF THIS IS LAST PAGE
C
           IF (IMAX.GT.PGMAX-1) GO TO 2300
           LMR  = IMAX * 5 + 2
           DONE = .TRUE.
C
C *** DO LEFT J-LABELS
C
 2300    CONTINUE
            JCURR = JMIN
C
 2400    CONTINUE
           KR  = KR + 1
           KRA = KR + 2
           KC  = KC + KCA
           KTA = MOD(JCURR,10)
           KTB = MOD(JCURR,100)/10
           KTC = MOD(JCURR,1000)/100
           IF (KR .EQ. 1 .OR. (.NOT. LCNTR)) GO TO 2500
           GO TO  2600
C
 2500    CONTINUE
           IF (LINATE.EQ.1) WRITE (OUTPUT,8300)
           IF (LINATE.EQ.2) WRITE (OUTPUT,8400)
C
 2600    CONTINUE
         KLINES(2) = KHPLUS
         KLINES(1) = KHBLNK
         IF (JCURR.LT.0) KLINES(2) = KHMNS
         KTA       = IABS(KTA)
         KTB       = IABS(KTB)
         KTC       = IABS(KTC)
         IF (KTC .EQ. 0) GO TO 2700
         KLINES(3) = KHTBL(KTC+1)
         KLINES(4) = KHTBL(KTB+1)
         KLINES(5) = KHTBL(KTA+1)
         GO TO 2800
C
 2700    CONTINUE
         KLINES(3) = KHTBL(KTB+1)
         KLINES(4) = KHTBL(KTA+1)
         KLINES(5) = KHBLNK
C
 2800    CONTINUE
         DO 2900  J = 6,MXPG
           KLINES(J) = KHBLNK
 2900    CONTINUE
         IF (.NOT.DONE) GO TO 3000
C
C *** DO RIGHT J-LABELS IF LAST PAGE OF MAP
C
           KLINE(LMR)   = KLINES(2)
           KLINE(LMR+1) = KLINES(3)
           KLINE(LMR+2) = KLINES(4)
           KLINE(LMR+3) = KLINES(5)
C
C *** FETCH AND CONVERT GRID VALUES TO A1 FORMAT FOR WHOLE LINE
C
 3000    CONTINUE
         KRX = KRLOC(KR)
         KLX = 5 * PGFST + 1
         IF (PGCNT.EQ.0) GO TO 4000
           DO 3800 KK = 1,PGCNT
             TEMP       = RDATA(KRX) * CNST(2) + A
             KTEMP      = ABS(TEMP) + 0.5
             KLINE(KLX) = KHPLUS
             IF (TEMP.LT.0.0) KLINE(KLX) = KHMNS
             GO TO (3300,3200,3100),NODIG
 3100      CONTINUE
             KTA = MOD(KTEMP,10000)/1000
C
 3200      CONTINUE
             KTB = MOD(KTEMP,1000)/100
C
 3300      CONTINUE
             KTC = MOD(KTEMP,100)/10
             KTD = MOD(KTEMP,10)
             GO TO (3400,3500,3600),NODIG
C
 3400      CONTINUE
             KLINE(KLX+1) = KHTBL(KTC+1)
             KLINE(KLX+2) = KHTBL(KTD+1)
             GO TO  3700
C
 3500      CONTINUE
             KLINE(KLX+1) = KHTBL(KTB+1)
             KLINE(KLX+2) = KHTBL(KTC+1)
             KLINE(KLX+3) = KHTBL(KTD+1)
             GO TO  3700
C
 3600      CONTINUE
             KLINE(KLX+1) = KHTBL(KTA+1)
             KLINE(KLX+2) = KHTBL(KTB+1)
             KLINE(KLX+3) = KHTBL(KTC+1)
             KLINE(KLX+4) = KHTBL(KTD+1)
C
 3700    CONTINUE
           KLX = KLX + 5
           KRX = KRX + 1
 3800    CONTINUE
C
C *** FOLLOWING CHECKS FOR POLE POINT AND INSERTS PROPER CHARACTER.
C
         IF (JCURR.NE.0) GO TO 4000
         IF (IMIN.LT.(-25).OR.IMIN.GT.0) GO TO 4000
         KX = -IMIN
         IF (KX.LT.PGFST.AND.KX.GT.PGCNT+PGFST) GO TO 4000
         KX = 5 * KX
         IF (KLINE(KX+1).EQ.KHMNS) GO TO 3900
         KLINE(KX) = KHDOLR
         GO TO 4000
C
 3900    CONTINUE
         KLINE(KX+1) = KHASTR
C
C *** PRINT LINE OF MAP DATA
C
 4000    CONTINUE
           WRITE (OUTPUT,8500) (KLINES(II),II=1,MXPG)
           KRLOC(KR) = KRX
           JCURR     = JCURR + 1
C           JCURR     = JCURR + JRWMP
C
C *** TEST BOTTOM OF MAP
C
         IF (KR.EQ.NRWS) GO TO 5700
C
C *** SET UP CONTOUR DATA AND PAGE LIMITERS FOR NEXT ROW
C
         KBR = 4
         GO TO 5900
C
 4100    CONTINUE
         IF (.NOT.LCNTR) GO TO 2400
C
C *** DO CONTOURING
C
         DO 4200 JJ = 1,MXPG
             KLINES(JJ) = KHBLNK
 4200    CONTINUE
C
C *** VERTICAL INTERPOLATIONS
C
         DO 4700 KK = 1,PAGN3
           IF (RWB(KK).LT.CRMX.AND.RWC(KK).LT.CRMX) GO TO  4300
           VDJB(KK) = CRMX
           VDJC(KK) = CRMX
           GO TO 4600
C
 4300    CONTINUE
           IF (RWA(KK).LT.CRMX.AND.RWD(KK).LT.CRMX) GO TO  4400
           VDJC(KK) = 0.
           GO TO  4500
C
 4400    CONTINUE
           VDJC(KK) = R32*(RWA(KK)+RWD(KK)-RWB(KK)-RWC(KK))
C
 4500    CONTINUE
           VDJB(KK) = R4*(RWC(KK)-RWB(KK)-CON2*VDJC(KK))
C
 4600    CONTINUE
           VDJA(KK)=RWB(KK)
C
 4700    CONTINUE
C
C     ...DO 2 OR 3 ROWS OF CONTOURING BETWEEN GRID ROWS...
C
         DO 5600 LL = 1,NBTWN
           DO 4800 KK = 1,PAGN3
             VDJB(KK) = VDJC(KK) + VDJB(KK)
             VDJA(KK) = VDJB(KK) + VDJA(KK)
 4800      CONTINUE
C
C     ...WHERE VDJA HAS THE INTERPOLATED VALUE FOR THIS INTER-ROW
C *** HORIZONTAL INTERPOLATIONS
C
           HDC = 0.0
           IF (VDJA(1).GE.CRMX) GO TO 4900
           HDC = R50*(VDJA(4)+VDJA(1)-VDJA(2)-VDJA(3))
C
 4900    CONTINUE
         KXB = 0
         DO 5200 KK = 1,PGMAX
           IF (VDJA(KK+1).GE.CRMX) GO TO 5100
           HDA = VDJA(KK+1)
           IF (VDJA(KK+2).GE.CRMX) GO TO 5500
           IF (VDJA(KK+3).GE.CRMX) HDC = 0.0
           HDB = R5 * (VDJA(KK+2) - VDJA(KK+1) - 15.0 * HDC)
C
C *** COMPUTE AND STORE CONTOUR CHARACTERS, 5 PER POINT
C
           KHDA = HDA
           KDB = IABS(MOD(KHDA,KCOW))
           KLINE(KXB+1) = KALPH(KDB+1)
           DO 5000 JJ = 2,5
             HDB  = HDB + HDC
             HDA  = HDA + HDB
             KHDA = HDA
             KDB  = IABS(MOD(KHDA,KCOW))
             KXA  = KXB + JJ
             KLINE(KXA) = KALPH(KDB+1)
 5000    CONTINUE
         HDC = R50*(VDJA(KK+4)+VDJA(KK+1)-VDJA(KK+2)-VDJA(KK+3))
         IF (VDJA(KK+4).GE.CRMX) HDC = 0.0
C
 5100    CONTINUE
           KXB = KXB + 5
C
 5200    CONTINUE
C
 5300    CONTINUE
         WRITE (OUTPUT,8500) (KLINES(II),II=1,MXPG)
         DO 5400 KK = 1,MXPG
           KLINES(KK) = KHBLNK
 5400    CONTINUE
         GO TO 5600
C
 5500    CONTINUE
           KHDA = HDA
           KDB  = IABS(MOD(KHDA,KCOW))
           KLINE(KXB+1) = KALPH(KDB+1)
           GO TO 5300
C
 5600    CONTINUE
         GO TO 2400
C
 5700    CONTINUE
         IF (LINATE.EQ.1)  WRITE (OUTPUT,8300)
         IF (LINATE.EQ.2)  WRITE (OUTPUT,8400)
         KLINES(1) = KHBLNK
C
C *** PRINT I-LABELS ACROSS BOTTOM OF PAGE
C
         KBR = 5
         GO TO 6900
C
 5800    CONTINUE
         IF (LINATE.EQ.1)  WRITE (OUTPUT,8300)
         IF (LINATE.EQ.2)  WRITE (OUTPUT,8400)
C
C *** PRINT TITLE
C
         WRITE (OUTPUT,8600) (TITLE(II),II=1,LW)
C
C ***    TEST END OF MAP
C
         IF (KRLOC(KCLMX).EQ.KCMX) RETURN
C
C ***    ADJUST PAGE LINE BOUNDARIES
C
         IF (IMAX.GT.PGMAX) IMAX = IMAX - PGMAX
         IMIN  = KA
         PAGNL = PAGNL + PGMAX
         PAGNR = PAGNR + PGMAX
         GO TO   1700
C
C *** ROUTINE TO PRE-STORE ROWS FOR CONTOURING AND COMPUTE LINE LIMITERS
C
 5900    CONTINUE
         PGFST = PGFSTA
         PGCNT = PGCNTA
         IF (KRA.GT.NRWS) GO TO 6800
           KRFST = KTBL(KC) - KZA
           KRCNT = KTBL(KC+1)
           KFX   = KRLOC(KRA)
           IF (RECT) GO TO  6100
           IF (KRFST-PAGNL.LE.(-1)) GO TO 6400
           PCFST  = KRFST - PAGNL + 1
           IF (PCFST.GE.PAGN3) GO TO 6700
           PGFSTA = PCFST-1
           PCCNT  = MIN0(PAGNR-KRFST+2,KRCNT)
           IF (PGFSTA.EQ.0) GO TO 6600
           PGCNTA = MIN0(PAGNR-KRFST,KRCNT)
           IF (PGCNTA.GT.0) GO TO 6000
           PGCNTA = 0
           GO TO 6100
C
 6000    CONTINUE
           RECT = KRECT.EQ.1.AND.PGCNTA.LE.KRCNT
C
 6100    CONTINUE
         IF (.NOT.LCNTR) GO TO (1800,2100,2200,4100,5800) KBR
         DO 6200 KK = 1,PAGN3
           RWA(KK) = RWB(KK)
           RWB(KK) = RWC(KK)
           RWC(KK) = RWD(KK)
           RWD(KK) = CRMX
 6200    CONTINUE
C
         IF (PCCNT.EQ.0) GO TO (1800,2100,2200,4100,5800) KBR
           KPC = PCFST + 1
         DO 6300 KK = 1,PCCNT
           RWD(KPC) = RDATA(KFX) * BC + ADC
           KFX = KFX + 1
           KPC = KPC + 1
 6300    CONTINUE
         GO TO (1800,2100,2200,4100,5800) KBR
C
 6400    CONTINUE
           PCFST  = 0
           PGFSTA = 0
           KFX    = KFX - 1
           PCCNT  = KRFST + KRCNT - PAGNL + 1
           IF (PCCNT.LT.PAGN3) GO TO 6500
           PCCNT  = PAGN3
           PGCNTA = PGMAX
           GO TO 6100
C
 6500    CONTINUE
           IF (PCCNT.GT.0) GO TO 6600
           PGCNTA = 0
           PCCNT  = 0
           GO TO 6100
C
 6600    CONTINUE
           PGCNTA = MIN0(PGMAX,KRCNT+KRFST-PAGNL)
           GO TO 6100
C
 6700    CONTINUE
           PGCNTA = 0
C
 6800    CONTINUE
           PCCNT  = 0
           GO TO 6100
C
C *** ROUTINE TO PRINT I-LABELS
C
 6900    CONTINUE
         DO 7000 KK = 2,MXPG
           KLINES(KK) =  KHBLNK
 7000    CONTINUE
C
         KK  = 1
         KA  = IMIN
         LBL = MIN0(IMAX,PGMAX)
C
         DO 7300 JJ = 1,LBL
           KLINE(KK) = KHPLUS
           IF (KA.LT.0) KLINE(KK) = KHMNS
           KTA = IABS(MOD(KA,100)) / 10
           KTB = IABS(MOD(KA,10))
           KTC = IABS(MOD(KA,1000)) / 100
           IF (KTC .EQ. 0) GO TO 7100
           KLINE(KK+1) = KHTBL(KTC+1)
           KLINE(KK+2) = KHTBL(KTA+1)
           KLINE(KK+3) = KHTBL(KTB+1)
           GO TO  7200
C
 7100      CONTINUE
             KLINE(KK+1) = KHTBL(KTA+1)
             KLINE(KK+2) = KHTBL(KTB+1)
C
 7200    CONTINUE
           KK = KK + 5
           KA = KA + 1
C
 7300    CONTINUE
C
         WRITE (OUTPUT,8500) (KLINES(II),II=1,MXPG)
C
         GO TO (1800,2100,2200,4100,5800) KBR
C
 7400    CONTINUE
           RETURN
C
       END

       SUBROUTINE IROTSYMB(CDEF,NBYTSYMB,LENLINPXL,LENLINBYT,
     1                  KHEIGHT,IQUAD,IROTRA,NEWPXLWID,NEWPXLHGT,IERROR)
C                                                     22-JAN-1996/DSS
C      ... TO ROTATE ONE BIT-MAPPED SYMBOL DEFINITION
C      ...    BY A ROTATION ANGLE OF 0-,90-,180-, OR 270-DEGREES 
C      ...    COUNTERCLOCKWISE 


C      ... GIVEN:
C      ... (1.) C*1 CDEF(NBYTSYMB) -- POINTER TO START OF ONE 
C                                       SYMBOL DEFINITION;
C      ... (3.) LENLINPXL -- WIDTH OF SYMBOL IN PIXELS 
C                            (NOT COUNTING FILL)
C      ... (4.) LENLINBYT -- WIDTH OF SYMBOL IN BYTES (INCLUDING FILL)
C      ... (5.) HEIGHT OF SYMBOL IN SCANLINES
C      ... (6.) IQUAD -- ROTATION ANGLE BY 90-DEGREE QUADRANT
C      ... RESULT:
C      ... (7.) INT IROTRA(*) -- RESULTING ROTATED SYMBOL DEF

C      ... CAUTION:  I AM ASSUMING THE CALLER HAS DIMENSIONED THE
C      ...                RESULTS ARRAY PROPERLY:
C      ...      INT IROTRA(LENLINPXL) -- IF 90-, OR 270-DEGREES
C      ...      INT IROTRA(KHEIGHT)   -- IF  0-, OR 180-DEGREES
C      ... (8.) INT NEWPXLWID -- RESULTING SYMBOL'S WIDTH (IN PIXELS)
C      ... (9.) INT NEWPXLHGT -- RESULTING SYMBOL'S HEIGHT (IN PIXELS)
C      ... (10.) INT IERROR -- RETURN CODE

C      ... REMARKS:
C      ...   I HAVE EXPANDED THE FUNCTION IROT() LOGIC WHICH EXTRACTED
C      ...   ONLY ONE COLUMN OF THE DEFINITION.

       INTEGER      IRVR
       EXTERNAL     IRVR

       CHARACTER*1  CDEF(NBYTSYMB)
       INTEGER      LENLINPXL
       INTEGER      LENLINBYT
       INTEGER      KHEIGHT
       INTEGER      IQUAD
       INTEGER      IROTRA(*)
       INTEGER      NEWPXLWID 
C                              -- RESULTING SYMBOL'S WIDTH (IN PIXELS)
       INTEGER      NEWPXLHGT
C                              -- RESULTING SYMBOL'S HEIGHT (IN PIXELS)
       INTEGER      IERROR

       INTEGER      MAXBYTPWRD
       PARAMETER   (MAXBYTPWRD=8)    	!... CRAY
C ...       PARAMETER   (MAXBYTPWRD=4)    	!... INTERGRAPH

       INTEGER      MAXBIT
       PARAMETER   (MAXBIT=8*MAXBYTPWRD)

       INTEGER      LONELINE
       CHARACTER*1  CONELINE(MAXBYTPWRD)
       EQUIVALENCE (LONELINE,CONELINE)

C      . . . .   S T A R T   . . . . . . . . . . . . . . . . . . . . .

       IERROR = 0
       NEWPXLWID = 0 
       NEWPXLHGT = 0

       IF(LENLINPXL .LE. 0) THEN
         IERROR = 1
         GO TO 999
       ELSE IF(LENLINPXL .GT. (8*LENLINBYT)) THEN
         IERROR = 2
         GO TO 999
       ENDIF

       NBYTGIVN = NBYTSYMB
       IF(NBYTGIVN .LE. 0) THEN
C        ... BAD VALUE FOR DIMENSION OF GIVEN SYMBOL DEF; JUMP OUT.
         IERROR = 3
         GO TO 999
       ENDIF


       IQUADRANT = MOD(IQUAD,4)
       IF(IQUADRANT .EQ. 0) THEN
C        ..............................................................
C        . . .   NO  ROTATION -- SIMPLY BREAK OUT INTO INT PER SCANLINE
C        ... INITIALIZE RESULTS TO ZERO ...
C        ... (7.) INT IROTRA(KHEIGHT)   -- IF  0-, OR 180-DEGREES
         DO  I = 1,KHEIGHT
           IROTRA(I) = 0
         ENDDO

         IACC = 0
         DO  ISCAN = 1,KHEIGHT
C          ... FETCH ONE SCANLINE OF CHAR DEF ...
           ICOFDEF = (ISCAN - 1)*LENLINBYT
           LONELINE = 0
           DO  IC = 1,LENLINBYT
             ICOFDEF = ICOFDEF + 1
             CONELINE(IC) = CDEF(ICOFDEF)
           ENDDO
           IROTRA(ISCAN) = LONELINE
         ENDDO
         NEWPXLWID = LENLINPXL
         NEWPXLHGT = KHEIGHT
         GO TO 999
C      ................................................................
       ELSE IF(IQUADRANT .EQ. 1) THEN
C        ..............................................................
C        . . .   90-DEGREE ROTATION   . . . . . . . . . . . . . . . . .
C        ... INITIALIZE RESULTS TO ZERO ...
C        ... (7.) INT IROTRA(LENLINPXL) -- IF 90-, OR 270-DEGREES
         DO  ICOL = 1,LENLINPXL
           IROTRA(ICOL) = 0
         ENDDO

         IACC = 0
         ITHBIT = MAXBIT
         DO  ISCAN = 1,KHEIGHT
           ITHBIT = ITHBIT - 1
C          ... FETCH ONE SCANLINE OF CHAR DEF ...
           ICOFDEF = (ISCAN - 1)*LENLINBYT
           LONELINE = 0
           DO  IC = 1,LENLINBYT
             ICOFDEF = ICOFDEF + 1
             CONELINE(IC) = CDEF(ICOFDEF)
           ENDDO
           DO  ICOL = 1,LENLINPXL
             ICOLREV = LENLINPXL - ICOL + 1         
             ICOLBIT = MAXBIT - ICOLREV
             IF(BTEST(LONELINE,ICOLBIT)) THEN
               IROTRA(ICOL) = IBSET(IROTRA(ICOL),ITHBIT)
             ENDIF
           ENDDO
         ENDDO
         NEWPXLWID = KHEIGHT
         NEWPXLHGT = LENLINPXL
         GO TO 999
C      ................................................................
       ELSE IF(IQUADRANT .EQ. 2) THEN
C        ..............................................................
C        . . .   180-DEGREE  ROTATION -- 
C        . . .        BY STARTING AT FARTHEST SCANLINE
C        . . .        REVERSING BITS WITHIN EACH SCANLINE
C        ... INITIALIZE RESULTS TO ZERO ...
C        ... (7.) INT IROTRA(KHEIGHT)   -- IF  0-, OR 180-DEGREES
         DO  I = 1,KHEIGHT
           IROTRA(I) = 0
         ENDDO

         IACC = 0
         DO  ISCAN = 1,KHEIGHT
           ISCANREV = KHEIGHT - ISCAN + 1
C          ... FETCH ONE SCANLINE OF CHAR DEF ...
           ICOFDEF = (ISCANREV - 1)*LENLINBYT
           LONELINE = 0
           DO  IC = 1,LENLINBYT
             ICOFDEF = ICOFDEF + 1
             CONELINE(IC) = CDEF(ICOFDEF)
           ENDDO
         
           IROTRA(ISCAN) = IRVR(LONELINE,LENLINPXL)
         ENDDO
         NEWPXLWID = LENLINPXL
         NEWPXLHGT = KHEIGHT
         GO TO 999
C      ................................................................
       ELSE IF(IQUADRANT .EQ. 3) THEN
C        ..............................................................
C        . . .   270-DEGREE ROTATION    . . . . . . . . . . . . . . . .
C        . . .      BY PULLING COLUMN AT A TIME; BUT FROM ORIGIN EDGE;
C        . . .      AND IRVR() THE EXTRACTED COLUMN
C        ... INITIALIZE RESULTS TO ZERO ...
C        ... (7.) INT IROTRA(LENLINPXL) -- IF 90-, OR 270-DEGREES
         DO  ICOL = 1,LENLINPXL
           IROTRA(ICOL) = 0
         ENDDO

         IACC = 0
         ITHBIT = MAXBIT
         DO  ISCAN = 1,KHEIGHT
           ITHBIT = ITHBIT - 1
C          ... FETCH ONE SCANLINE OF CHAR DEF ...
           ICOFDEF = (ISCAN - 1)*LENLINBYT
           LONELINE = 0
           DO  IC = 1,LENLINBYT
             ICOFDEF = ICOFDEF + 1
             CONELINE(IC) = CDEF(ICOFDEF)
           ENDDO
           DO  ICOL = 1,LENLINPXL
             ICOLBIT = MAXBIT - ICOL
             IF(BTEST(LONELINE,ICOLBIT)) THEN
               IROTRA(ICOL) = IBSET(IROTRA(ICOL),ITHBIT)
             ENDIF
           ENDDO
         ENDDO

         DO  ICOL = 1,LENLINPXL
           IROTRA(ICOL) = IRVR(IROTRA(ICOL),KHEIGHT)
         ENDDO

         NEWPXLWID = KHEIGHT
         NEWPXLHGT = LENLINPXL
         GO TO 999
C      ................................................................
       ENDIF
          
  999  CONTINUE
       RETURN
       END           

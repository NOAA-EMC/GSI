       SUBROUTINE CONSTA(MAP,IWINDOW)
C                                                 13-MAR-1996/DSS
C      ... TO CONVERT PRODUCT AND MAP-BGND CONSTANTS
C      ... FROM THE GIVEN MAP-ARRAY INTO THE RESULTING IWINDOW-ARRAY.
C      ... THIS IS A STRIPPED DOWN VERSION OF SUBR CONST();
C
C      ... IN THE OLD CNTR/CNTRI, IWINDOW() IS REFERRED TO AS CADJ()
C      ... I KEPT THE SAME SUBSCRIPTS AS THE CADJ(1) THRU (13);
C      ... ANY NEW ITEMS I ADDED ON STARTING FROM ITEM(15)
C
C  IWINDOW(1) = NO. OF BACKGROUND LINES TO discard BEFORE using any bg
C               (J_BG_SKP)
C  IWINDOW(2) = BOTTOM MARGIN (TOP OF) ... 
C               (j_bg_org)
C  IWINDOW(3) = TOP MARGIN (BOTTOM OF)
C  IWINDOW(4) = FINAL LIMIT (TOP OF TOP MARGIN) ... 
C               (J_FR_MAX)
C  IWINDOW(5) = LATERAL SHIFT IN BITS FOR THE BACKGROUND MAP;
C               NO. OF PIXELS OF BG TO DISCARD BEFORE USING ANY 
C               (IPXL_BG_SKP)
C  IWINDOW(6) = THE FLAGS BLOCK FROM THE IL TABLE -- MAP(2)

C  IWINDOW(7) = WIDTH OF RIGHT (FAX-EDGE) MARGIN
C               (IPXL_BG_ORG)
C  IWINDOW(8) = RIGHT-EDGE OF LEFT-MARGIN; MEASURED FROM FAX-EDGE;
C                  (WIDTH FROM FAX-EDGE TO END OF VSBL GEO; UNDOUBLED)
C                  (RITMARGIN+BGWIDTH)(UNDOUBLED)
C  IWINDOW(9)  = LATERAL  CONTOUR  ireverser & fine_adj
C     ... where IpxlCON(reversed&fine_adj) = iwindow(9) - ipxl(contour)

C  IWINDOW(10) = LATERAL  PRTITL  ireverser & fine_adj
C     ... where IpxlTX(revers&fine_adjusted) = iwindow(10) - IDOT(LABEL)

C  IWINDOW(11) = DELTA_Y = VERTICAL PRTITL  ADJUSTMENT

C  IWINDOW(12) = WIDTH(IN BYTES)  ROUNDED UP IN BYTES(PACKING LIMIT)
C                      (DOUBLE-SCALED IF APPROPRIATE)

C  IWINDOW(13) = DELTA_Y = VERTICAL CONTOUR ADJUSTMENT

      INTEGER     MAP(15)
      INTEGER     IWINDOW(30)
       
      INTEGER     KDUBLBIT
      DATA        KDUBLBIT   / X'0002' /

      DO  I = 1,30
        IWINDOW(I) = 0
      ENDDO

      LSCALE=1
      K=1
      IF(IAND(KDUBLBIT, MAP(2)) .NE. 0) THEN
        LSCALE = 2
      ENDIF

      IWINDOW(1) =  MAP(8)
      IWINDOW(2) = (MAP(8)-MAP(4))*LSCALE

      IWINDOW(3) = (MAP(8)-MAP(4) + MAP(10))*LSCALE

      IWINDOW(4) =  MAP(6)*LSCALE

      IWINDOW(5) =(1800 - MAP(3) - MAP(5))*LSCALE

      IWINDOW(6) = MAP(2)

       MARGINRIT  =  MAP(3) + MAP(5) - (MAP(7)+MAP(9))
      IWINDOW(7)  =  MARGINRIT
      IWINDOW(8)  =  MARGINRIT + MAP(9)
C     ... WHICH IS WIDTH OF EVERYTHING EXCEPT THE LEFT-MARGIN ...

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      IWINDOW(9)  = (MARGINRIT + MAP(9) - MAP(11) + 4)*LSCALE
C                                         DI_CONTOUR
C     ... where IpxlCON(reversed&fine_adj) = iwindow(9) - ipxl(contour)

      iwindow(10) = map(5)*lscale - map(13)	!... L*fr_wid - DItext
C     ... where IpxlTX(revers&fine_adjusted) = iwindow(10) - IDOT(LABEL)

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      IWINDOW(11) =  MAP(14)   		!... chgd to not rescale DJ-text
C ...      IWINDOW(11) =  MAP(14)*LSCALE
C                ... (11): ADDITIVE DELTA-J (RESCALED) --  FOR TEXT ...
C                ... why is this DJ rescaled when DI was not rescaled?
C                ...    This should probably be changed to not rescale.

      IWINDOW(12) = (IWINDOW(10) + 7)/8
C                    ... PACKING PRODUCT-WIDTH LIMIT (IN BYTES) ...

      IWINDOW(13) = (MAP(12)*LSCALE) + IWINDOW(2)
C                    DJ_cntr            BOT_MARG
C                ... (13): ADDITIVE DELTA-J (RESCALED) ... DJ_cntr
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


C ...      IWINDOW(15) = (IWINDOW(12)+7) / 8
C ...               ... (IWRD_FR_maxmax) ... THIS SHOULD BE IMAGE WIDTH

      IWINDOW(15) = (LSCALE*MAP(5) + 63) / 64
C                   ... (IWRD_FR_MAX) ...  USE THIS FOR IMAGE WIDTH
C                   ...  I chngd to use only given product width ...
C                   ...  AND limit line length to given product width
           IPXLFRAMEMAX = 64*IWINDOW(15) 
      IWINDOW(16) =  8*IWINDOW(12)
C                    ... (IPXL_FR_MAX)
      IF(IWINDOW(16) .GT. IPXLFRAMEMAX) THEN
        IWINDOW(16) = IPXLFRAMEMAX      	!... LMT IN PXL
        IWINDOW(12) = (IPXLFRAMEMAX + 7) / 8    !... LMT IN BYTES
      ENDIF

      IWINDOW(17) =  MAP(1)
      IWINDOW(18) =  IWINDOW(3) - IWINDOW(2)
C                    ... (J_BG_SPA)
      IWINDOW(19) =  MAP(9)*LSCALE
C                    ... (IPXL_BG_SPA)
      IWINDOW(20) =  MAP(9)
C                    ... (IPXL_BG_CUT)
      

      RETURN
      END

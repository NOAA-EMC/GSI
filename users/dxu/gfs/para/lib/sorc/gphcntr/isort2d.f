       SUBROUTINE ISORT2D(IDATA,JTM,NOBS,IERR)
C                                                      29-NOV-1995/DSS
C      ... PREPARING FOR CRAY VERSION OF ISORT FOR SORTING THE LABEL
C      ...   ARRAY
C
C      ... STRIPPED DOWN SHLSRT TO I*4 IDATA(2,JTM) 
C      ...   IN WHICH SORT_KEY IS IN I*4 IDATA(1,JTM)
C      ...                             5DEC85  SHIMOMURA
C      ... STRIPPED DOWN LSORT TO COMPARE SIGNED-INTEGER
C      ...   SORT_KEYS INSTEAD OF LSORT'S 32-BIT LOGICAL
C      ...   COMPARE.
C      ... TO SORT, IN PLACE, THE GIVEN ARRAY IDATA
C      ... USING A SIFTING SORT LOGIC BY SHELL
C
C      (1) IDATA ... GIVEN INTEGER    IDATA(2,JTM)
C      (2) JTM   ... J-DIMENSION OF IDATA
C      (3) NOBS   ... NUMBER OF ITEMS WHICH WERE SORTED
C      (4) IERR = 0  IF NORMAL RETURN;
C               = -1; WARNING: the sorting terminated on a zero-
C                              terminator before completing JTM items.
C               = 1; the dimension-of-IDATA, JTM was .LE. 0
C               = 5; zero-terminator was found in first word of IDATA
C
C      CAUTION:  AN ALL ZERO WORD IN IDATA(ISKEY,J) WILL BE
C                    INTERPRETED AS THE END OF ALL DATA.
C
C      ...        COPIED BY HAND FROM IBM VERSION 5DEC85 DSS.
C      ...        DIFFERENCES IN DEC-VAX BYTE-ORDER REMAIN TO
C      ...          BE CORRECTED.
C
       INTEGER     IDATA(2,JTM)   		!... P1,P2
       INTEGER     NOBS   			!... P3
       INTEGER     IERR    			!... P4

       INTEGER    ITM
       PARAMETER (ITM=2)
C      ... WHERE ITM WAS REMOVED FROM ARG LIST FOR SPEED
       INTEGER    ISKEY
       PARAMETER (ISKEY=1)

       INTEGER    MINOBS
       PARAMETER (MINOBS=2)
C      ... WHERE MINOBS IS NO. OF ITEMS LESS THAN WHICH NOT SORT

       COMMON  /STATIS/ NCMPAR,NMOVES(3)


       INTEGER     IUPR(2)
       INTEGER    ISORK, JSORK
       LOGICAL    LSAVED
       LOGICAL    LFOUNDZERO
       LOGICAL    LCHECKOUT
C
C
C      . . .   S T A R T   . . .
C
       IERR = 0
       LCHECKOUT = .TRUE.
       LFOUNDZERO = .FALSE.
       NCMPAR = 0
       NMOVES(1) = 0
       NMOVES(2) = 0
       NMOVES(3) = 0
       IF(JTM .LE. 0) GO TO 900
C
C      ... COUNT THE NO. OF ITEMS IN THE GIVEN IDATA ARRAY,
C      ...   SEARCHING FOR THE FIRST ZERO WORD IN THE SORT-KEY WORD
       NOBS = 0
       IOBS = 0
       DO  J = 1,JTM
         IF(IDATA(ISKEY,J) .EQ. 0) then
           IF(LCHECKOUT) THEN
C            ... CHECKOUT PRINT 
             WRITE(6,163)J,iobs
  163        FORMAT(1H ,'isort2d: found terminating zero word at J=',
     1                     I6,'; iobs=',I6)
           ENDIF
           LFOUNDZERO = .TRUE.
           GO TO 170
         ENDIF
         IOBS = IOBS + 1
       ENDDO
       LFOUNDZERO = .FALSE.
       IF(LCHECKOUT) THEN
C        ... checkout print
         write(6,165)iobs
  165    format(1h ,'isort2d: No zero terminator found. iobs=',I6)
       ENDIF
C
  170  CONTINUE
C      ... WHEN IT COMES TO 170, IOBS CONTAINS COUNT OF NON-ZERO 
C      ...   ITEMS.
       NOBS = IOBS
       IF(NOBS .LE. 0) GO TO 950
       IF(NOBS .LT. MINOBS) GO TO 960
C      ... HERE COMES THE SHELL SORT LOGIC ...
       INTRVL = NOBS
  311  CONTINUE
C      ... THE FRANK & LAZARUS MODIFICATION ...
C      ... TO INSURE ODD-NUMBERED INTERVAL;
C      ... AND ALSO, WHEN INTERVAL IS LARGE, TO DIVIDE BY
C      ... APPROX. 4 INSTEAD OF 2
       IDIV = 4
       IF(INTRVL .GT. 15) IDIV = 8
       ITEMP = INTRVL / IDIV
       INTRVL = 2*ITEMP + 1
       NMI = NOBS - INTRVL
C
       DO  499  JJ = 1,NMI
         LSAVED = .FALSE.
C        ... EXTRACT SORT KEY OF THE UPPER ITEM (,JJ+INTRVL)
C        ... THIS IS COMPARED AGAINST ITEM IN THIS JJ LOOP
         ISORK = IDATA(ISKEY,JJ+INTRVL)
C
C
C        ... INNER DO ON JRUNG GOES BACKWARD FROM JJ TO 1 BY -INTRVL
         JRUNG = JJ
  400    CONTINUE
C        ... COMPARE THE ITEM ON THIS JRUNG AGAINST IUPR
C        ... EXTRACT SORT KEY OF ITEM ON JRUNG
           JSORK = IDATA(ISKEY,JRUNG)
C
C
           NCMPAR = NCMPAR + 1       		!... D_LINE
           IF(JSORK .LT. ISORK) GO TO 450
C          ... COMES HERE TO MOVE THIS ITEM (WHICH IS .GE. IUPR)
C          ... UPWARDS FROM THIS JRUNG TO THE RUNG ABOVE
           IF(LSAVED) GO TO 440
C          ... OTHERWISE, ORIGINAL UPPER ITEM IN THIS JJ LOOP
C          ...   HAS NOT BEEN SAFELY SAVED IN IUPR YET
C          ...   SO SAVE IN IUPR THE (,JJ+INTRVL) ITEM
           NMOVES(1) = NMOVES(1) + 1   		!... D_LINE
C
             IUPR(1) = IDATA(1,JJ+INTRVL)
             IUPR(2) = IDATA(2,JJ+INTRVL)
           LSAVED = .TRUE.
C
  440      CONTINUE
           JRPI = JRUNG + INTRVL
           NMOVES(2) = NMOVES(2) + 1    	!... D_LINE
C
             IDATA(1,JRPI) = IDATA(1,JRUNG)
             IDATA(2,JRPI) = IDATA(2,JRUNG)
C
         JRUNG = JRUNG - INTRVL
         IF(JRUNG .GE. 1) GO TO 400
C        ... WHICH IS END OF INNER DO LOOP ON JRUNG
  450    CONTINUE
C        ... COMES HERE TO STORE THE ORIGINALLY PULLED-OUT ITEM
C        ... (FROM THE TOP OF THE LADDER) DOWN INTO WHERE THE 
C        ... MOVED UP ONES VACATED A RUNG.
         JRPI = JRUNG + INTRVL
         IF(JRUNG .EQ. JJ) GO TO 499
         NMOVES(3) = NMOVES(3) + 1    		!... D_LINE
C
           IDATA(1,JRPI) = IUPR(1)
           IDATA(2,JRPI) = IUPR(2)
C
  499  CONTINUE
C
       IF(INTRVL .GT. 1) GO TO 311
       GO TO 999
C
C      . . .   E R R O R   E X I T S   . . . 
C
  900  CONTINUE
       IERR = 1
       GO TO 999
  950  CONTINUE
       IERR = 5
       GO TO 999
  960  CONTINUE
C      ... COMES HERE IF ONLY ONE ITEM WAS GIVEN TO SORT
       IERR = 0
       GO TO 999
C
  999  CONTINUE
       if((ierr .eq. 0) .and.
     1    (lfoundzero)) then
         ierr = -1   	!... warning: terminated on a zero terminator
       endif
       RETURN
       END

       SUBROUTINE HSORT(IDATA,JTM,NOBS,IERR)
C                                                      29-NOV-1995/DSS
C      ... PREPARING FOR CRAY VERSION OF HSORT FOR SORTING THE LABEL
C      ...   ARRAY
C      ... THIS CRAY VERSION IS FOR A 1-DIMENSIONAL IDATA ARRAY
C      ...   OF 64-BIT LABEL ARRAY ITEMS
C      ...   IN WHICH THE HI-ORDER 32 BITS CONTAINS THE SORT KEY
C
C      ... TO SORT, IN PLACE, THE GIVEN ARRAY IDATA
C      ... USING A HEAP SORT LOGIC
C
C      (1) IDATA ... GIVEN INTEGER    IDATA(JTM)
C      (2) JTM   ... J-DIMENSION OF IDATA
C      (3) NOBS   ... NUMBER OF ITEMS WHICH WERE SORTED
C      (4) IERR = 0  IF NORMAL RETURN;
C               = -1; WARNING: the sorting terminated on a zero-
C                              terminator before completing JTM items.
C               = 1; the dimension-of-IDATA, JTM was .LE. 0
C               = 5; zero-terminator was found in first word of IDATA
C
C      CAUTION:  AN ALL ZERO WORD IN SORT-KEY OF IDATA(J) WILL BE
C                    INTERPRETED AS THE END OF ALL DATA.
C
C
       INTEGER     IDATA(JTM)   		!...<= P1,P2
       INTEGER     NOBS   			!...=> P3
       INTEGER     IERR    			!...=> P4

       INTEGER    ITM
       PARAMETER (ITM=2)
C      ... WHERE ITM WAS REMOVED FROM ARG LIST FOR SPEED

       INTEGER    MINOBS
       PARAMETER (MINOBS=2)
C      ... WHERE MINOBS IS NO. OF ITEMS LESS THAN WHICH NOT SORT

       COMMON  /STATIS/ NCMPAR,NMOVES(3)

       INTEGER    MSKSORK
       DATA       MSKSORK     / X'FFFFFFFF00000000' /

       INTEGER    JSORKZ
       INTEGER    ISORK
       INTEGER    JSORK
       INTEGER    IUPR
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
         JSORKZ = IAND(IDATA(J),MSKSORK)
         IF(JSORKZ .EQ. 0) then
           IF(LCHECKOUT) THEN
C            ... CHECKOUT PRINT 
             WRITE(6,163)J,iobs
  163        FORMAT(1H ,'HSORT: found terminating zero word at J=',
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
  165    format(1h ,'HSORT: No zero terminator found. iobs=',I6)
       ENDIF
C
  170  CONTINUE
C      ... WHEN IT COMES TO 170, IOBS CONTAINS COUNT OF NON-ZERO 
C      ...   ITEMS.
       NOBS = IOBS
       IF(NOBS .LE. 0) GO TO 950
       IF(NOBS .LT. MINOBS) GO TO 960
C      ... HERE COMES THE HEAP SORT LOGIC ...

         CALL HEPSOR(IDATA,NOBS)

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

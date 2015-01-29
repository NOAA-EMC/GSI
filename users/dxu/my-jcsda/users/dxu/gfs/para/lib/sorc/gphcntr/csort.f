       SUBROUTINE CSORT(IDATA,JTM,NOBS,IERR)
C                                                      29-NOV-1995/DSS
C      ... PREPARING FOR CRAY VERSION OF SSORT FOR SORTING THE LABEL
C      ...   ARRAY
C      ... THIS CRAY VERSION IS FOR A 1-DIMENSIONAL IDATA ARRAY
C      ...   OF 64-BIT LABEL ARRAY ITEMS
C      ...   IN WHICH THE HI-ORDER 32 BITS CONTAINS THE SORT KEY
C
C      ... TO SORT, IN PLACE, THE GIVEN ARRAY IDATA
C      ... USING PIKSOR(); or SHLSOR();
C      ...   DEPENDING ON THE COUNT OF ITEMS TO SORT.
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
C      REMARKS: Krishna Kumar changed the name of this routine
C               from SSORT to CSORT on IBM RS/6000 due to a
C               conflict in name on IBM's ESS Library (-lessl has 
C               a routine called SSORT) - 08/17/1999
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

C ...       COMMON  /STATIS/ NCMPAR,NMOVES(3)

       INTEGER    MSKSORK
       DATA       MSKSORK     / X'FFFFFFFF00000000' /

       INTEGER    JSORKZ
       INTEGER    ISORK
       INTEGER    JSORK
       INTEGER    IUPR
       LOGICAL    LSAVED
       LOGICAL    LFOUNDZERO
C
C
C      . . .   S T A R T   . . .
C
ckumar
       print*,'Entered CSORT - JTM  ',JTM
ckumar
       IERR = 0
       LFOUNDZERO = .FALSE.
C ...       NCMPAR = 0
C ...       NMOVES(1) = 0
C ...       NMOVES(2) = 0
C ...       NMOVES(3) = 0
       IF(JTM .LE. 0) GO TO 900
C
C      ... COUNT THE NO. OF ITEMS IN THE GIVEN IDATA ARRAY,
C      ...   SEARCHING FOR THE FIRST ZERO WORD IN THE SORT-KEY WORD
       NOBS = 0
       IOBS = 0
       DO  J = 1,JTM
C ...         JSORKZ = IAND(IDATA(J),MSKSORK)
C ...         IF(JSORKZ .EQ. 0) then
C ...         ... the entire long word == 0; for terminator
         IF(IDATA(J) .EQ. 0) then
C            ... CHECKOUT PRINT 
ckumar
          print*,'In CSORT - J,IOBS,JTM  ',J,IOBS,JTM
ckumar
C          WRITE(6,163)J,iobs,JTM
  163      FORMAT(1H ,'CSORT: found terminating zero word at J=',
     1                     I6,'; iobs=',I6,
     2           /1h ,'   bin-sized I*8  IDATA(JTM), where JTM=',I6)
           LFOUNDZERO = .TRUE.
           GO TO 170
         ENDIF
         IOBS = IOBS + 1
       ENDDO
       LFOUNDZERO = .FALSE.
C        ... checkout print
C ...         write(6,165)iobs
C ...  165    format(1h ,'CSORT: No zero terminator found. iobs=',I6)
C
  170  CONTINUE
C      ... WHEN IT COMES TO 170, IOBS CONTAINS COUNT OF NON-ZERO 
C      ...   ITEMS.
       NOBS = IOBS
       IF(NOBS .LE. 0) GO TO 950
       IF(NOBS .LT. MINOBS) GO TO 960
C      ... HERE COMES THE SORT ...
       IF(NOBS .LT. 50) THEN

         CALL PIKSOR(IDATA,NOBS)

       ELSE 
C        ... [ 50 =< NOBS ]; SO
         CALL SHLSOR(IDATA,NOBS)

       ENDIF
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
ckumar     
         print*,'In CSORT -ierr,lfoundzero  after 999 cont  ',
     &    ierr,lfoundzero
ckumar
       endif
       RETURN
       END
